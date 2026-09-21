// Phase: B2 (implemented) — points of light at night: the halo round a lantern, the star of a flare, fires burning far
// off in the fog. A glow is built in three layers, the way a flame photographs (owner's reference, 2026-09-21): a small
// white-hot core, a body in the light's own colour, and a wide halo pushed toward the colour's deepest channel (an orange
// lamp ends in red, a cold flare in blue). Additive camera-facing cards; the falloff is computed, so there is no texture read at all, and every
// glow in a mesh is one draw call. They test depth (a lamp behind a wall is hidden) and write none.
// Vertex data: position = the glow's centre (object space), uv0 = corner (-1..1), uv1 = size in metres, flicker 0..1,
// phase, how much the haze dims it 0..1; colour = the light's colour (alpha scales it).
Shader "TW/Glow (URP)"
{
    Properties
    {
        [HDR] _Tint ("Tint", Color) = (1, 1, 1, 1)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+40" "IgnoreProjector"="True" }
        Pass
        {
            Name "Glow"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            CBUFFER_START(UnityPerMaterial)
                half4 _Tint;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; float2 corner : TEXCOORD0; float4 shape : TEXCOORD1; half4 color : COLOR; };
            struct Varyings { float4 positionCS : SV_POSITION; float2 corner : TEXCOORD0; half3 color : TEXCOORD1; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                float3 centre = TransformObjectToWorld(v.positionOS.xyz);
                float3 right = UNITY_MATRIX_V[0].xyz, up = UNITY_MATRIX_V[1].xyz;
                float t = _Time.y * 9.0 + v.shape.z * 40.0;
                float flicker = 1.0 - v.shape.y * (0.5 + 0.5 * sin(t) * sin(t * 0.37 + 1.3) + 0.25 * sin(t * 2.3));
                float3 ws = centre + (right * v.corner.x + up * v.corner.y) * v.shape.x * 0.5 * (0.9 + 0.1 * flicker);
                o.positionCS = TransformWorldToHClip(ws);
                o.corner = v.corner;
                float haze = 1.0 - saturate(ComputeFogIntensity(ComputeFogFactor(o.positionCS.z)));   // 0 clear .. 1 lost in haze
                o.color = v.color.rgb * v.color.a * _Tint.rgb * saturate(flicker) * (1.0 - haze * v.shape.w);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half d = saturate(1.0 - dot(i.corner, i.corner));
                half peak = max(i.color.r, max(i.color.g, i.color.b));
                half3 hot = lerp(i.color, peak.xxx, 0.85);                 // nearly white
                half3 deep = i.color * i.color / max(peak, 1e-3);          // same brightness, hue pulled to the strongest channel
                deep = deep * deep / max(peak, 1e-3);
                half d2 = d * d, d4 = d2 * d2;
                half3 glow = hot * d4 * d4 * 1.3 + i.color * d2 * d * 0.55 + deep * pow(d, 1.3) * 0.30;
                return half4(glow, 1.0);
            }
            ENDHLSL
        }
    }
}
