// Target-relative authored flame. Transparent scheduling keeps the matte out of
// the global depth-ink pass; stencil4 still excludes the later ground wash.
Shader "TW/Cel/FlameImpact"
{
    Properties
    {
        [NoScaleOffset] _ImpactSheet ("Impact poses", 2D) = "black" {}
        [NoScaleOffset] _ImpactTailSheet ("Terminal impact poses", 2D) = "black" {}
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelFlameImpact"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Off ZWrite Off ZTest LEqual
            Stencil { Ref 4 WriteMask 4 Comp Always Pass Replace Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"
            TEXTURE2D(_ImpactSheet);
            SAMPLER(sampler_ImpactSheet);
            TEXTURE2D(_ImpactTailSheet);
            SAMPLER(sampler_ImpactTailSheet);

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv : TEXCOORD0;
                float fog : TEXCOORD1;
                nointerpolation float alive : TEXCOORD2;
                nointerpolation float terminal : TEXCOORD3;
            };
            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                Varyings o;
                o.alive = CelAlive(r);
                float3 p = CelBillboard(CelPosition(r, max(CelAge(r), 0.0)), (v.uv * 2.0 - 1.0) * (1.45 * r.size.x * o.alive));
                o.positionCS = TransformWorldToHClip(p);
                o.fog = ComputeFogFactor(o.positionCS.z);
                // Authored at 24 fps; lifetime independently preserves the final pose.
                float rawFrame = floor(max(CelAge(r), 0.0) * r.extra.x);
                o.terminal = rawFrame >= 96.0 ? 1.0 : 0.0;
                if (rawFrame >= 100.0) o.alive = 0.0;
                if (o.terminal > 0.5)
                    o.uv = (float2(min(rawFrame - 96.0, 3.0), 0.0) + lerp(0.5 / 320.0, 1.0 - 0.5 / 320.0, v.uv)) / float2(4.0, 1.0);
                else
                {
                    float frame = min(rawFrame, 95.0);
                    float2 cell = float2(fmod(frame, 12.0), 7.0 - floor(frame / 12.0));
                    o.uv = (cell + lerp(0.5 / 320.0, 1.0 - 0.5 / 320.0, v.uv)) / float2(12.0, 8.0);
                }
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                half4 sheet;
                if (i.terminal > 0.5) sheet = SAMPLE_TEXTURE2D(_ImpactTailSheet, sampler_ImpactTailSheet, i.uv);
                else sheet = SAMPLE_TEXTURE2D(_ImpactSheet, sampler_ImpactSheet, i.uv);
                sheet.a *= i.alive;
                clip(sheet.a - 0.01);
                return half4(MixFog(sheet.rgb, i.fog), sheet.a);
            }
            ENDHLSL
        }
    }
}
