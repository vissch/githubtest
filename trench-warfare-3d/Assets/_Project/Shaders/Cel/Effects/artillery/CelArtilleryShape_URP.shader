// Procedural shell silhouettes, inked blast spears and persistent smoke ribbons; no image inputs.
// Extra.x selects shell=0, burst=1, trail=2. Trail Extra.y is flight time and Axis its full path.
// Size.xyz is radius over life; shell Size.w is length, burst Size.w is vertical stretch.
Shader "TW/Cel/ArtilleryShape"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+19" }
        Pass
        {
            Name "ArtilleryShape"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv : TEXCOORD0;
                float3 positionWS : TEXCOORD1;
                nointerpolation float4 tone : TEXCOORD2;
                nointerpolation float4 shape : TEXCOORD3;
                float4 more : TEXCOORD4;
                float2 extra : TEXCOORD5;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float age = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float radius = CelCurve(r.size.xyz, k) * alive;
                float3 p = CelPosition(r, age);
                float3 w;
                float2 uv = v.uv * 2.0 - 1.0;
                float ground = 1.0;
                if (r.extra.x < 0.5)
                {
                    float3 dir = normalize(r.axis);
                    float3 side = normalize(cross(dir, normalize(_WorldSpaceCameraPos - p)));
                    w = p + dir * (uv.x * r.size.w * 0.5 * alive) + side * (uv.y * radius);
                }
                else if (r.extra.x < 1.5)
                {
                    w = CelBillboard(p, uv * radius * float2(1.0, max(r.size.w, 1.0)));
                    ground = w.y - (r.pos.y - 1.2);
                    // The spears sit in front of their own lobes while retaining true-distance fog.
                    w = _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * 0.96;
                }
                else
                {
                    float along = v.uv.x;
                    float3 dir = normalize(r.axis);
                    float3 side = normalize(cross(dir, normalize(_WorldSpaceCameraPos - (r.pos + r.axis * 0.5))));
                    float wave = sin(along * 41.0 + r.seed * 37.0 + age * 0.3) * 0.32
                               + sin(along * 97.0 + r.seed * 13.0) * 0.16;
                    w = r.pos + r.axis * along + side * (uv.y + wave) * radius * alive;
                    uv = float2(along, uv.y);
                }
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.uv = uv;
                o.tone = r.tone;
                o.shape = r.extra;
                o.more = float4(k, CelCurve(r.erode.xyz, k), r.seed, ComputeFogFactor(o.positionCS.z));
                o.extra = float2(age, ground);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                half3 colour;
                float field, mask;
                if (i.shape.x < 0.5)
                {
                    float x = i.uv.x;
                    float width = lerp(0.6, 0.98, smoothstep(-1.0, -0.65, x));
                    width *= sqrt(saturate((1.0 - x) * 3.0));
                    width += 0.18 * step(-0.75, x) * step(x, -0.55);
                    field = width - abs(i.uv.y);
                    mask = CelAAStep(0.02, field);
                    float value = saturate(0.42 + 0.4 * (1.0 - abs(i.uv.y + 0.3)));
                    colour = CelRamp(value, row, 1.0);
                    colour = lerp(colour, CelPal(row, 0), step(0.62, x));
                    colour = lerp(colour, CelPalInk(row).rgb, CelInk(field, 0.02, CelLodInk(i.tone.w)));
                }
                else if (i.shape.x < 1.5)
                {
                    float angle = atan2(i.uv.y, i.uv.x);
                    float sector = (angle / 6.2831853 + 0.5) * i.shape.y;
                    float index = floor(sector);
                    float variation = CelHash21(float2(index, i.more.z * 91.0));
                    float center = lerp(0.28, 0.72, variation);
                    float width = lerp(0.22, 0.47, CelHash21(float2(index + 19.0, i.more.z * 53.0)));
                    float peak = pow(saturate(1.0 - abs(frac(sector) - center) / width), 1.4);
                    float shoulder = 0.42 + 0.10 * sin(angle * 3.0 + i.more.z * 17.0)
                                          + 0.07 * sin(angle * 7.0 - i.more.z * 23.0);
                    float rad = shoulder + peak * lerp(0.18, 0.55, variation);
                    rad *= 1.0 - i.more.y * 0.72;
                    field = 1.0 - length(i.uv) / max(rad, 0.01);
                    mask = CelAAStep(0.015, field) * step(0.0, i.extra.y) * (1.0 - smoothstep(0.7, 1.0, i.more.x));
                    float value = field > 0.12 ? 0.96 : 0.58;
                    colour = CelRamp(value, row, i.tone.y);
                    colour = lerp(colour, CelPalInk(row).rgb, CelInk(field, 0.015, CelLodInk(i.tone.w)));
                }
                else
                {
                    float x = i.uv.x;
                    float width = 0.74 + 0.18 * sin(x * 81.0 + i.more.z * 23.0) + 0.12 * sin(x * 179.0 + i.more.z * 31.0);
                    width *= lerp(0.3, 1.0, sqrt(saturate(x)));
                    field = width - abs(i.uv.y);
                    float breakup = CelValue2(float2(x * 23.0, i.more.z * 61.0));
                    float front = saturate(i.extra.x / max(i.shape.y, 0.01));
                    mask = CelAAStep(0.04, field) * step(x, front) * step(i.more.y, breakup * 0.8 + 0.2);
                    float value = 0.55 + 0.35 * saturate(field);
                    colour = CelRamp(value, row, 1.0);
                    colour = lerp(colour, CelPalInk(row).rgb, CelInk(field, 0.04, CelLodInk(i.tone.w * 0.6)));
                    mask *= i.tone.z;
                }
                clip(mask - 0.002);
                colour = MixFog(ApplyFieldFog(ApplyMist(colour, i.positionWS), i.positionWS), i.more.w);
                return half4(colour * mask, mask);
            }
            ENDHLSL
        }
    }
}
