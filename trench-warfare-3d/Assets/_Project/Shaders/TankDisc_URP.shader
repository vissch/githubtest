// Phase: A5b / C4 (implemented) — what a tank presses on the ground, one quad each drawn instanced by TankRenderer: a
// soft dark contact blob under the hull (the tank sits on the mud, not above it) and a thin ring in its side's colour
// round it, faintly lit, so the side reads at the gameplay zoom and at night (critique 2026-09-22). The shape is
// computed: uv 0..1 across the footprint, which the quad's matrix stretches to the hull plus a margin. A second pass
// draws the ring (not the blob) faintly where the ground or the hull hides it, so a trench lip or a slope never swallows
// the side marker (critique round 2).
//  _Color  per instance: rgb the side's colour, a the ring's strength (0 on a wreck: only the blob).
Shader "TW/TankDisc (URP)"
{
    Properties
    {
        _Blob ("Contact blob strength", Range(0,1)) = 0.5
        [HideInInspector] _Color ("Side colour (per instance)", Vector) = (1,1,1,1)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent-45" "IgnoreProjector"="True" }
        Pass
        {
            Name "TankDisc"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            Offset -2, -2
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            CBUFFER_START(UnityPerMaterial)
                float _Blob;
            CBUFFER_END
            UNITY_INSTANCING_BUFFER_START(DiscProps)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Color)
            UNITY_INSTANCING_BUFFER_END(DiscProps)
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 positionWS : TEXCOORD1; float fog : TEXCOORD2; float4 side : TEXCOORD3; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.uv = v.uv;
                o.fog = ComputeFogFactor(o.positionCS.z);
                o.side = UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Color);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float2 p = i.uv * 2.0 - 1.0;
                // a rounded rectangle, so the blob and the ring follow the hull's long shape
                float2 q = abs(p) - 0.62;
                float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);   // 0 at the rounded edge of the inner box
                half blob = (1.0 - smoothstep(-0.25, 0.16, d)) * _Blob;
                half ring = (smoothstep(0.18, 0.23, d) * (1.0 - smoothstep(0.32, 0.39, d))) * i.side.a;
                half3 color = lerp(half3(0.02, 0.018, 0.016), i.side.rgb * 1.4, ring / max(ring + blob, 1e-3));
                half alpha = max(blob, ring * 0.55);
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                return half4(MixFog(color, i.fog), alpha);
            }
            ENDHLSL
        }
        Pass
        {
            Name "TankDiscHidden"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            ZTest Greater
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            UNITY_INSTANCING_BUFFER_START(DiscProps)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Color)
            UNITY_INSTANCING_BUFFER_END(DiscProps)
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float4 side : TEXCOORD1; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
                o.uv = v.uv;
                o.side = UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Color);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float2 p = i.uv * 2.0 - 1.0;
                float2 q = abs(p) - 0.62;
                float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);
                half ring = (smoothstep(0.18, 0.23, d) * (1.0 - smoothstep(0.32, 0.39, d))) * i.side.a;
                return half4(i.side.rgb * 1.4, ring * 0.3);
            }
            ENDHLSL
        }
    }
}
