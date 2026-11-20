// Bullet-impact dust: broad opaque billows with sparse ink curls. Uses the same Icosphere(3), indirect record,
// ballistic motion and palette contracts as CelLobe, but deliberately avoids noisy normals and cutout holes.
// size.xyz = radius curve; size.w = vertical squash; motion = gravity / drag / spin / buoyancy.
// erode.xyz shrinks the whole lobe. Shrink starts after 65% of life; the last 12% closes the silhouette smoothly to a point.
// tone.x = palette; tone.w = outline pixels. Palette c2 is the body, c3 the upper cap, c1 a subtle lower shade.
// extra.x = broad scallop scale (default 1); extra.y = silhouette amplitude (default .14);
// extra.z = upper-cap amount (0..1, default .7); extra.w = cooling start (default .35).
Shader "TW/Cel/BiDust"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+10" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct DustOut { float3 positionWS; float3 normalWS; float k; float radius; };

        float3 DustShape(float3 n, CelRecord r, float t)
        {
            float3 view = normalize(_WorldSpaceCameraPos - CelPosition(r, t));
            float3 right = normalize(cross(UNITY_MATRIX_V[1].xyz, view));
            float3 up = cross(view, right);
            float2 p = float2(dot(n, right), dot(n, up));
            float phase = r.seed * 6.283185;
            float amp = r.extra.y > 0 ? r.extra.y : 0.14;
            float broad = r.extra.x > 0 ? clamp(r.extra.x, 0.65, 1.3) : 1.0;
            float2 ray = p / max(length(p), 1e-5);
            int count = r.seed < 0.58 ? 3 : 4;
            float envelope = 0.64;
            // The silhouette is the union of broad overlapping circles, evaluated along this vertex's eye-plane
            // ray. A circle intersection gives a rounded cap; angular harmonics made the previous caps pointed.
            [unroll] for (int j = 0; j < 4; j++)
            {
                if (j < count)
                {
                    float h0 = CelHash11(r.seed * 71.3 + j * 19.13);
                    float h1 = CelHash11(r.seed * 37.7 + j * 11.29 + 4.3);
                    float a = phase + r.motion.z * t + j * 6.283185 / count + (h0 - 0.5) * 0.38;
                    float sn, cs; sincos(a, sn, cs);
                    float2 center = float2(cs, sn) * (0.37 + h0 * 0.06);
                    float radius = 0.54 + h1 * 0.09;
                    float along = dot(ray, center);
                    float disc = radius * radius - dot(center, center) + along * along;
                    envelope = max(envelope, along + sqrt(max(disc, 0.0)));
                }
            }
            float detail = saturate(amp * broad / 0.088) * (1.0 - smoothstep(0.70, 0.95, CelLife01(r)));
            float scale = lerp(0.95, envelope * 1.035, detail * smoothstep(0.1, 0.8, length(p)));
            return (right * p.x + up * p.y) * scale + view * dot(n, view) * 0.95;
        }

        DustOut Dust(float3 positionOS, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r);
            float e = saturate(CelCurve(r.erode.xyz, k));
            float shrink = lerp(1.0, pow(saturate(1.0 - e), 0.72), smoothstep(0.65, 0.90, k)) * (1.0 - smoothstep(0.88, 1.0, k));
            float radius = CelCurve(r.size.xyz, k) * CelAlive(r) * shrink;
            float squash = r.size.w > 0 ? r.size.w : 1.0;
            float3 n = normalize(positionOS);
            DustOut o;
            o.positionWS = CelPosition(r, t) + DustShape(n, r, t) * float3(1.0, squash, 1.0) * radius;
            o.normalWS = normalize(n / float3(1.0, squash, 1.0));
            o.k = k;
            o.radius = radius;
            return o;
        }

        half3 DustPalette(uint row, float cap, float underside)
        {
            half3 body = CelPal(row, 2);
            half3 col = lerp(body, CelPal(row, 1), underside * 0.16);
            return lerp(col, CelPal(row, 3), cap * 0.60);
        }

        float DustCurl(float2 p, float seed, float age, float px)
        {
            // Most lobes have one short curved crease; a third are left completely clear.
            if (seed < 0.34 || px <= 0.0) return 0.0;
            float a = (seed - 0.5) * 2.1 + age * 0.12;
            float sn, cs; sincos(a, sn, cs);
            p = float2(p.x * cs - p.y * sn, p.x * sn + p.y * cs);
            p.x *= seed > 0.69 ? -1.0 : 1.0;
            float2 q = p - float2(0.23, 0.08);
            float radius = 0.38 + seed * 0.12;
            float d = abs(length(q) - radius);
            // Open lower-left arc with tapered ends; no complete circles or closed dark patches.
            float angle = atan2(q.y, q.x);
            float gate = smoothstep(-3.05, -2.75, angle) * (1.0 - smoothstep(-0.35, -0.05, angle));
            float width = max(fwidth(d), 1e-4);
            return (1.0 - smoothstep(width * px * 0.25, width * (px * 0.5 + 0.7), d)) * gate;
        }
        ENDHLSL

        Pass
        {
            Name "CelBiDust"
            Tags { "LightMode"="UniversalForward" }
            Cull Back ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 normalWS : TEXCOORD1;
                float3 sphere : TEXCOORD2;
                nointerpolation float4 tone : TEXCOORD3;
                nointerpolation float4 more : TEXCOORD4;
                float2 misc : TEXCOORD5;
                nointerpolation float3 facing : TEXCOORD6;
            };
            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                DustOut d = Dust(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                Varyings o;
                o.positionWS = d.positionWS;
                o.positionCS = TransformWorldToHClip(d.positionWS);
                o.normalWS = d.normalWS;
                o.sphere = normalize(v.positionOS.xyz);
                o.tone = r.tone;
                o.more = float4(d.k, CelAge(r), r.extra.z > 0 ? r.extra.z : 0.7, r.extra.w > 0 ? r.extra.w : 0.35);
                o.misc = float2(r.seed, ComputeFogFactor(o.positionCS.z));
                o.facing = normalize(_WorldSpaceCameraPos - CelPosition(r, CelAge(r)));
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float3 n = normalize(i.normalWS);
                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                float lit = dot(n, normalize(view * 0.65 + float3(-0.25, 1.0, 0.0)));
                // Broad cel cap with a single curved boundary, without noise-lit camouflage patches.
                float cap = CelAAStep(0.58, lit) * saturate(i.more.z);
                float underside = 1.0 - CelAAStep(-0.28, n.y);
                uint a = CelRowA(i.tone.x), b = CelRowB(i.tone.x);
                half3 col = DustPalette(a, cap, underside);
                if (b != a && b != 0u) col = lerp(col, DustPalette(b, cap, underside), smoothstep(i.more.w, 1.0, i.more.x));
                float3 sphere = normalize(i.sphere);
                float3 right = normalize(cross(UNITY_MATRIX_V[1].xyz, i.facing));
                float3 up = cross(i.facing, right);
                float2 p = float2(dot(sphere, right), dot(sphere, up));
                float curl = DustCurl(p, i.misc.x, i.more.y, CelLodInk(i.tone.w) * 0.7);
                curl *= smoothstep(0.28, 0.55, dot(n, view));
                // Ink the actual scalloped silhouette in the surface pass too. The hull alone can be hidden by
                // neighbouring opaque lobes; this pixel-width contour keeps their overlapping boundaries readable.
                float rim = CelInk(1.0 - length(p), 0.0, CelLodInk(i.tone.w));
                col = lerp(col, CelInkColor(i.tone.x, i.more.x, i.more.w), max(rim, curl * 0.88));
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.misc.y), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "CelBiDustInk"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Cull Front ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vertInk
            #pragma fragment fragInk
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; };
            struct InkVaryings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; nointerpolation float3 tone : TEXCOORD1; float fog : TEXCOORD2; };
            InkVaryings vertInk(Attributes v, uint instanceID : SV_InstanceID)
            {
                DustOut d = Dust(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                float4 cs = TransformWorldToHClip(d.positionWS);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, d.normalWS).xy;
                dir /= max(length(dir), 1e-4);
                float px = CelLodInk(r.tone.w) * saturate(d.radius * 20.0);
                cs.xy += dir * px * 2.0 / _ScreenParams.xy * cs.w;
                InkVaryings o;
                o.positionCS = cs;
                o.positionWS = d.positionWS;
                o.tone = float3(r.tone.x, d.k, r.extra.w > 0 ? r.extra.w : 0.35);
                o.fog = ComputeFogFactor(cs.z);
                return o;
            }
            half4 fragInk(InkVaryings i) : SV_Target
            {
                half3 col = CelInkColor(i.tone.x, i.tone.y, i.tone.z);
                col = ApplyMist(col, i.positionWS);
                return half4(MixFog(ApplyFieldFog(col, i.positionWS), i.fog), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Back
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct Attributes { float4 positionOS : POSITION; };
            struct DepthVaryings { float4 positionCS : SV_POSITION; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                DustOut d = Dust(v.positionOS.xyz, instanceID);
                DepthVaryings o; o.positionCS = TransformWorldToHClip(d.positionWS); return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
