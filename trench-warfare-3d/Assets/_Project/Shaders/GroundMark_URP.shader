// Phase: B2 (implemented) — what is pressed into the mud and only seen from close by: a boot print or a length of tank
// rut, one quad each, laid on the ground by CombatFx and drawn instanced. The shape is computed (uv: x across, y along
// the direction of travel), the mark darkens the ground and, when the field is wet, holds a film of water that mirrors
// the sky at a low angle. Three materials a shape carry the fade (young, old, going), as the smoke does.
Shader "TW/GroundMark (URP)"
{
    Properties
    {
        _Shape ("Shape (0 boot print, 1 track rut)", Float) = 0
        _Alpha ("Strength", Range(0,1)) = 0.6
        _Color ("Pressed mud", Color) = (0.035, 0.030, 0.026, 1)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent-40" "IgnoreProjector"="True" }
        Pass
        {
            Name "GroundMark"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            Offset -1, -1
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            CBUFFER_START(UnityPerMaterial)
                half4 _Color;
                float _Shape, _Alpha;
            CBUFFER_END
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 positionWS : TEXCOORD1; float fog : TEXCOORD2; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.uv = v.uv;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float2 p = i.uv * 2.0 - 1.0;
                half shape, pooled;   // pooled: the part pressed deepest, where the water stands
                if (_Shape < 0.5)
                {
                    // a boot: the sole ahead, the heel behind, a narrow waist between
                    half sole = 1.0 - smoothstep(0.75, 1.0, length(float2(p.x / 0.92, (p.y - 0.28) / 0.70)));
                    half heel = 1.0 - smoothstep(0.75, 1.0, length(float2(p.x / 0.74, (p.y + 0.66) / 0.33)));
                    shape = max(sole, heel);
                    pooled = smoothstep(0.55, 0.95, shape);
                }
                else
                {
                    // a rut: a soft-edged band with the track's cross bars pressed into it
                    half band = (1.0 - smoothstep(0.62, 1.0, abs(p.x))) * (1.0 - smoothstep(0.86, 1.0, abs(p.y)));
                    half groove = smoothstep(0.42, 0.5, frac(i.uv.y * 4.0)) * (1.0 - smoothstep(0.92, 1.0, frac(i.uv.y * 4.0)));   // the track's cross bars
                    shape = band * (0.72 + 0.28 * groove);
                    half bar = frac(sin(floor(i.uv.y * 4.0 + i.positionWS.z * 1.7) * 12.9898) * 43758.5453);   // no two bars hold the same water
                    pooled = band * groove * (0.25 + 0.55 * bar);
                }
                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                half fresnel = pow(1.0 - saturate(view.y), 3.0);
                // pressed mud is matt and dark (it kills the glitter of the wet ground round it); what stands in the deepest
                // part is water, and mirrors the sky at the low angle a close camera looks from
                half3 color = lerp(_Color.rgb * TWShadeTint() * 3.0, TWSky() * 1.15, pooled * (0.18 + 0.82 * fresnel) * _TWWet.x);
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                half alpha = shape * _Alpha * (1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 30.0) / 12.0));
                return half4(MixFog(color, i.fog), alpha);
            }
            ENDHLSL
        }
    }
}
