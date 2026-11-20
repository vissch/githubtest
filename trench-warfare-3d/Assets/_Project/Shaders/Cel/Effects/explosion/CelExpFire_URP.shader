// Prototype 2026-11-14: reference f32/f48/f64/f86 has connected orange billows behind the dummy and a
// narrower vertical yellow-white impact to its left. No sphere-normal ramp, concentric rings, texture or flipbook.
// One immutable CelRecord expands into three smoothly merged warped ellipses. An independently advected heat
// field grades yellow islands through the orange body. The main Heart clears the target face while preserving world fog.
// size.xyz radius curve; size.w screen vertical aspect (0=>1). motion.z boil speed, other motion unchanged.
// extra.x 0 body / 1 vertical heart; extra.y heat contrast (0=>1); extra.z warm outer band width (0=>.1);
// extra.w horizontal lean with height. erode.xyz late dissolution; erode.w edge softness in local radii.
// tone.x palette, tone.y heart HDR multiplier, tone.z opacity. tone.w deliberately unused: no black ink.
// Premultiplied alpha, ZWrite Off. No per-instance transparent sorting; assembled capture remains required.
Shader "TW/Cel/ExpFire"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+14" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelExpFire"
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
                float2 q : TEXCOORD0;
                float3 positionWS : TEXCOORD1;
                nointerpolation float4 life : TEXCOORD2; // k, age, seed, alive
                nointerpolation float4 tone : TEXCOORD3; // palette, hdr, opacity, erode
                nointerpolation float4 shape : TEXCOORD4; // mode, contrast, edge band, lean
                nointerpolation float2 more : TEXCOORD5; // boil speed, edge feather
                float fog : TEXCOORD6;
            };

            // CONTINUOUS HEART EXPERIMENT: one record; exact-time envelope keys, no birth-seed jumps.
            // Keys are (radius, vertical aspect, camera-plane offset x, camera-plane offset y).
            // Hold begins .8s; this record begins Hold+.25s. Reference frames32/64/76/86 occur at
            // record ages .283333/1.616667/2.116667/2.533333. Units are world/billboard units.
            float4 HeartKeyBlend(float t, float a, float b, float4 ka, float4 kb)
            {
                float u = saturate((t - a) / max(b - a, 0.001));
                return lerp(ka, kb, u * u * (3.0 - 2.0 * u));
            }

            float4 AuthoredHeartKey(float t)
            {
                // Begin at Hold+.05; preserve all previously measured keys from frame32 onward.
                t -= 0.20;
                const float4 k0 = float4(1.20, 1.02, -0.35, 0.10);
                // EARLY_ENVELOPE_34: widen and shorten the original Heart field; no new anatomy or gain.
                const float4 k22 = float4(2.20, 0.75, -0.40, 0.36);
                const float4 k26 = float4(2.20, 0.80, -0.34, 0.42);
                const float4 k32 = float4(2.30, 0.84, -0.089, 0.535);
                const float4 k64 = float4(1.50, 1.02, -0.282, 0.239);
                const float4 k76 = float4(1.45, 1.02, -0.438, 0.300);
                const float4 k86 = float4(1.95, 1.10, -0.146, 0.284);
                const float4 kEnd = float4(1.20, 1.02, -0.221, 0.334);
                if (t < -0.133333) return HeartKeyBlend(t, -0.20, -0.133333, k0, k22);
                if (t < 0.033333) return HeartKeyBlend(t, -0.133333, 0.033333, k22, k26);
                if (t < 0.283333) return HeartKeyBlend(t, 0.033333, 0.283333, k26, k32);
                if (t < 1.616667) return HeartKeyBlend(t, 0.283333, 1.616667, k32, k64);
                if (t < 2.116667) return HeartKeyBlend(t, 1.616667, 2.116667, k64, k76);
                if (t < 2.533333) return HeartKeyBlend(t, 2.116667, 2.533333, k76, k86);
                return HeartKeyBlend(t, 2.533333, 3.15, k86, kEnd);
            }

            // BODY UNDERLAY EXPERIMENT: body age starts Hold+.25, independent of the Heart start.
            // Keys describe radius/aspect and camera-plane center. Main orange mass strengthens late.
            float4 AuthoredBodyKey(float t)
            {
                const float4 k0 = float4(1.05, 1.05, 0.010, -0.015);
                const float4 k32 = float4(1.65, 1.05, 0.010, -0.015);
                const float4 k64 = float4(1.47, 1.15, 0.006, 0.045);
                const float4 k76 = float4(1.65, 1.02, -0.120, 0.059);
                const float4 k86 = float4(1.90, 0.90, 0.025, 0.002);
                const float4 kEnd = float4(1.55, 0.95, 0.025, 0.060);
                if (t < 0.283333) return HeartKeyBlend(t, 0.0, 0.283333, k0, k32);
                if (t < 1.616667) return HeartKeyBlend(t, 0.283333, 1.616667, k32, k64);
                if (t < 2.116667) return HeartKeyBlend(t, 1.616667, 2.116667, k64, k76);
                if (t < 2.533333) return HeartKeyBlend(t, 2.116667, 2.533333, k76, k86);
                float4 key = HeartKeyBlend(t, 2.533333, 3.20, k86, kEnd);
                // TAIL_DURATION_65: reference100->112 orange remnants migrate upper-right.
                key.zw += float2(0.52, 0.13) * smoothstep(2.866667, 3.616667, t);
                return key;
            }

            float AuthoredBodyOpacity(float t)
            {
                if (t < 0.283333) return lerp(0.0, 0.45, smoothstep(0.0, 0.283333, t));
                if (t < 1.616667) return lerp(0.45, 0.48, smoothstep(0.283333, 1.616667, t));
                if (t < 2.116667) return lerp(0.48, 0.85, smoothstep(1.616667, 2.116667, t));
                if (t < 2.533333) return lerp(0.85, 0.98, smoothstep(2.116667, 2.533333, t));
                return 0.98 * (1.0 - smoothstep(2.82, 3.20, t));
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float k = CelLife01(r), t = CelAge(r), alive = CelAlive(r);
                float radius = max(0.0, CelCurve(r.size.xyz, k)) * alive;
                float aspect = r.size.w > 0.0 ? r.size.w : 1.0;
                float reach = r.extra.x > 2.5 && r.extra.x < 3.5 ? 2.65 : 1.45;
                float2 q = (v.uv * 2.0 - 1.0) * reach;
                float2 authoredOffset = float2(0.0, 0.0);
                if (r.extra.x > 0.5 && r.extra.x < 1.5 && r.extra.y < 0.0)
                {
                    float4 key = AuthoredHeartKey(max(0.0, t));
                    radius = key.x * alive;
                    aspect = key.y;
                    authoredOffset = key.zw;
                }
                else if (r.extra.x < 0.5 && r.extra.y < 0.0)
                {
                    float4 bodyKey = AuthoredBodyKey(max(0.0, t));
                    radius = bodyKey.x * alive;
                    aspect = bodyKey.y;
                    authoredOffset = bodyKey.zw;
                }
                float3 w = CelBillboard(CelPosition(r, t), q * radius * float2(1.0, aspect) + authoredOffset);
                // SiteBody shares body anatomy, but its explicitly authored card clears the trench lip.
                if (r.extra.x > 1.5 && r.extra.x < 2.5) w = _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * 0.55;
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                // HEART_DRAW_DEPTH_38: projected size and physical fog remain unchanged.
                // At the captured 19.03m camera distance this moves only the impact draw plane
                // 1.14m forward, clearing the roughly 1m front half of the 2m-deep target.
                float4 physicalPositionCS = o.positionCS;
                if (r.extra.x > 0.5 && r.extra.x < 1.5 && r.extra.y < -1.5)
                {
                    // PEAK_TRANSLATE_49: translate the intact Heart only during its brief white pulse.
                    // Captured Heart-plane projection: this is +5.7px right and -7.2px up at86.
                    // Moving the draw plane keeps the heat and alpha fields together; no ellipse narrowing.
                    float peakShift = exp(-pow((max(0.0, t) - 2.733333) / 0.085, 2.0));
                    float opening22 = exp(-pow((max(0.0, t) - 0.066667) / 0.028, 2.0));
                    float opening26 = exp(-pow((max(0.0, t) - 0.233333) / 0.030, 2.0));
                    float2 drawShift = float2(0.079198, 0.100092) * peakShift
                        + float2(0.009818, -0.075076) * opening22
                        + float2(-0.087707, 0.054230) * opening26;
                    float3 peakDrawW = CelBillboard(w, drawShift);
                    float3 drawW = _WorldSpaceCameraPos + (peakDrawW - _WorldSpaceCameraPos) * 0.94;
                    o.positionCS = TransformWorldToHClip(drawW);
                }
                o.q = q;
                o.life = float4(k, max(0.0, t), r.seed, alive);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = r.extra;
                o.more = float2(r.motion.z, max(r.erode.w, 0.006));
                o.fog = ComputeFogFactor(physicalPositionCS.z);
                return o;
            }

            float FireEllipse(float2 p, float2 radius)
            {
                return (1.0 - length(p / radius)) * min(radius.x, radius.y);
            }

            float FireMerge(float a, float b, float softness)
            {
                float h = max(softness - abs(a - b), 0.0) / softness;
                return max(a, b) + h * h * softness * 0.25;
            }

            // UNCAPTURED anatomy proposal: smaller unequal tongues and open edge cuts.
            // No full rings: cuts intersect the outside, rather than enclosing holes.
            float2 FireRotate(float2 p, float angle)
            {
                float sn, cs; sincos(angle, sn, cs);
                return float2(p.x * cs - p.y * sn, p.x * sn + p.y * cs);
            }

            // Positive inside a tapered slit. Applied after fringe so every slit stays edge-open.
            float FireTaperSlot(float2 q, float2 a, float2 b, float widthA, float widthB)
            {
                float2 ab = b - a;
                float t = saturate(dot(q - a, ab) / max(dot(ab, ab), 1e-5));
                return lerp(widthA, widthB, t) - length(q - (a + t * ab));
            }

            // Authored Heart anatomy only: same approximate area/envelope as the old four caps.
            // Nine unequal pockets, a fine moving fringe and seven unequal edge-open slits.
            float AuthoredHeartField(float2 q, float phase)
            {
                q -= float2(0.010, 0.015); // preserve the original field centroid to about one pixel
                float f = FireEllipse(q - float2(-0.03, 0.00), float2(0.22, 0.36));
                f = FireMerge(f, FireEllipse(q - float2(-0.11, 0.35), float2(0.14, 0.25)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(-0.18, 0.46), float2(0.07, 0.14)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(-0.02, 0.39), float2(0.09, 0.16)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(-0.26, 0.12), float2(0.13, 0.17)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(-0.24, -0.16), float2(0.13, 0.14)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(0.14, 0.15), float2(0.12, 0.14)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(0.06, -0.35), float2(0.15, 0.22)), 0.018);
                f = FireMerge(f, FireEllipse(q - float2(-0.02, -0.47), float2(0.11, 0.10)), 0.018);
                f += 0.012 * sin(q.y * 39.0 + q.x * 11.0 + phase * 0.7)
                    + 0.010 * sin(q.x * 47.0 - q.y * 9.0 - phase * 0.55) - 0.004;
                f = min(f, -FireTaperSlot(q, float2(-0.43, 0.11), float2(-0.20, 0.15), 0.035, 0.003));
                f = min(f, -FireTaperSlot(q, float2(0.31, 0.16), float2(0.14, 0.06), 0.030, 0.003));
                f = min(f, -FireTaperSlot(q, float2(-0.055, 0.68), float2(-0.12, 0.39), 0.026, 0.003));
                f = min(f, -FireTaperSlot(q, float2(0.015, -0.65), float2(0.025, -0.39), 0.026, 0.003));
                f = min(f, -FireTaperSlot(q, float2(-0.31, 0.45), float2(-0.17, 0.28), 0.022, 0.003));
                f = min(f, -FireTaperSlot(q, float2(-0.38, -0.26), float2(-0.17, -0.17), 0.025, 0.003));
                return min(f, -FireTaperSlot(q, float2(0.28, -0.32), float2(0.10, -0.26), 0.025, 0.003));
            }

            float FireBody(float2 q, float seed, float phase, float mode)
            {
                float hand = seed < 0.5 ? -1.0 : 1.0;
                q = FireRotate(q, mode > 1.5 ? 0.0 : (seed - 0.5) * (mode > 0.5 ? 0.20 : 0.70));
                q.x += (mode > 1.5 ? 0.022 : 0.060) * sin(q.y * 6.0 + phase * 1.5 + seed * 17.0);
                q.y += (mode > 1.5 ? 0.016 : 0.045) * sin(q.x * 7.0 - phase * 1.2 + seed * 11.0);
                if (mode > 1.5) return AuthoredHeartField(q, phase);
                if (mode > 0.5)
                {
                    // Compact impact pocket; short curled lower tongue replaces the hanging stem.
                    float f = FireEllipse(q - float2(0.00, 0.03), float2(0.26, 0.34));
                    f = FireMerge(f, FireEllipse(q - float2(-0.11, 0.35), float2(0.17, 0.25)), 0.025);
                    f = FireMerge(f, FireEllipse(q - float2(-0.21, -0.10), float2(0.18, 0.19)), 0.025);
                    f = FireMerge(f, FireEllipse(q - float2(0.09, -0.32), float2(0.16, 0.25)), 0.025);
                    // Edge-open side bite gives a hook instead of an elongated oval tip.
                    float cut = FireEllipse(q - float2(0.25, -0.27), float2(0.12, 0.15));
                    return min(f, -cut);
                }
                q.x *= hand;
                float2 drift = float2(0.025 * sin(phase + seed * 9.0), 0.025 * cos(phase * 0.8 + seed * 13.0));
                // A connected orange body with unequal 20-35px perimeter tongues at current record sizes.
                float f = FireEllipse(q - float2(-0.06, -0.04), float2(0.48, 0.46));
                f = FireMerge(f, FireEllipse(q - float2(0.25, 0.43) - drift, float2(0.25, 0.31)), 0.025);
                f = FireMerge(f, FireEllipse(q - float2(0.43, 0.03), float2(0.26, 0.23)), 0.025);
                f = FireMerge(f, FireEllipse(q - float2(-0.43, 0.17), float2(0.22, 0.25)), 0.025);
                f = FireMerge(f, FireEllipse(q - float2(0.10, -0.43), float2(0.27, 0.24)), 0.025);
                float upperCut = FireEllipse(q - float2(0.50, 0.48) - drift, float2(0.16, 0.18));
                float sideCut = FireEllipse(q - float2(-0.57, -0.03), float2(0.12, 0.16));
                f = min(f, -upperCut);
                return min(f, -sideCut);
            }

            // A connected field with ten unequal perimeter tongues. Open edge cuts do not create rings.
            float AuthoredBodyField(float2 q, float phase)
            {
                q.x += 0.025 * sin(q.y * 7.0 + phase * 0.8);
                q.y += 0.025 * sin(q.x * 6.0 - phase * 0.9);
                float2 d = float2(0.025 * sin(phase), 0.020 * cos(phase * 0.83));
                float f = FireEllipse(q, float2(0.61, 0.59));
                f = FireMerge(f, FireEllipse(q - float2(-0.13, 0.61) - d, float2(0.20, 0.24)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(0.30, 0.58) + d.yx, float2(0.22, 0.25)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(0.58, 0.35) - d * 0.7, float2(0.22, 0.22)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(0.67, 0.00) + d, float2(0.20, 0.24)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(0.49, -0.44) - d.yx, float2(0.24, 0.20)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(0.13, -0.64) + d * 0.6, float2(0.21, 0.23)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(-0.28, -0.59) - d, float2(0.25, 0.20)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(-0.60, -0.33) + d.yx, float2(0.23, 0.25)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(-0.66, 0.12) - d * 0.8, float2(0.23, 0.21)), 0.035);
                f = FireMerge(f, FireEllipse(q - float2(-0.46, 0.50) + d, float2(0.23, 0.23)), 0.035);
                f = min(f, -FireEllipse(q - float2(0.82, 0.50) - d, float2(0.12, 0.14)));
                f = min(f, -FireEllipse(q - float2(-0.78, -0.60) + d, float2(0.15, 0.12)));
                return min(f, -FireEllipse(q - float2(0.36, -0.83), float2(0.13, 0.14)));
            }


            // MAIN_RADIANT_MATERIAL_35: main close Body/Heart opt in with extra.y = -2.
            // Advected continuous density replaces hard cutout tongues and palette steps.
            // Existing exact-time envelope keys remain the control; sites and other LODs retain old shading.
            float2 MainRadiantWarp(float2 q, float phase)
            {
                float2 broad = float2(
                    CelValue2(q * 3.2 + float2(1.3, -phase * 0.34)),
                    CelValue2(q * 3.4 + float2(9.7 + phase * 0.22, 3.1))) - 0.5;
                float2 fine = float2(
                    CelValue2(q * 8.5 + float2(-phase * 0.45, 7.2)),
                    CelValue2(q * 8.1 + float2(2.3, -phase * 0.50))) - 0.5;
                return q + broad * 0.12 + fine * 0.045;
            }

            float MainRadiantGaussian(float2 q, float2 centre, float2 radius)
            {
                float2 p = (q - centre) / radius;
                return exp(-2.0 * dot(p, p));
            }

            float MainHeartDensity(float2 q, float phase)
            {
                q = MainRadiantWarp(q, phase);
                // Unequal overlapping soft pockets, without a trunk, cut slits or cross arms.
                float density = MainRadiantGaussian(q, float2(-0.06, 0.00), float2(0.34, 0.43));
                density += 0.72 * MainRadiantGaussian(q, float2(-0.13, 0.29), float2(0.25, 0.30));
                density += 0.82 * MainRadiantGaussian(q, float2(0.035, -0.28), float2(0.28, 0.32));
                density += 0.42 * MainRadiantGaussian(q, float2(-0.255, -0.10), float2(0.21, 0.24));
                density += 0.32 * MainRadiantGaussian(q, float2(0.15, 0.12), float2(0.19, 0.23));
                return density * (0.92 + 0.08 * CelValue2(q * 5.3 + float2(2.1, -phase * 0.28)));
            }


            // CONNECTED_MAIN_FOLDS: a few unequal analytic sheets, not repeated outlined cells.
            // Positions use the existing camera-plane phase keys. No image/flipbook is sampled.
            // ORANGE_FRAMING_60: observed right orange silhouette, camera-plane vectors.
            static const float2 OrangeOutline32[48] = {
                float2(0.074178, 1.705514),
                float2(0.117549, 1.705509),
                float2(0.406757, 1.640951),
                float2(0.522480, 1.576410),
                float2(0.563028, 1.511878),
                float2(0.634416, 1.447342),
                float2(0.725081, 1.382804),
                float2(0.898633, 1.318258),
                float2(0.927614, 1.253726),
                float2(1.006713, 1.189190),
                float2(1.101233, 1.124652),
                float2(1.139853, 1.060119),
                float2(1.205459, 0.995584),
                float2(1.115893, 0.931065),
                float2(1.115960, 0.866537),
                float2(1.130485, 0.802007),
                float2(1.173924, 0.737474),
                float2(1.271335, 0.672936),
                float2(1.360072, 0.608398),
                float2(1.390981, 0.543866),
                float2(1.419963, 0.479335),
                float2(1.448944, 0.414804),
                float2(1.477926, 0.350272),
                float2(1.506907, 0.285741),
                float2(1.506975, 0.221213),
                float2(1.507042, 0.156684),
                float2(1.507110, 0.092156),
                float2(1.507178, 0.027628),
                float2(1.478331, -0.036898),
                float2(1.449485, -0.101423),
                float2(1.449552, -0.165951),
                float2(1.391792, -0.230474),
                float2(1.391859, -0.295002),
                float2(1.305185, -0.359521),
                float2(1.235858, -0.424042),
                float2(1.218578, -0.488569),
                float2(1.189731, -0.553094),
                float2(0.813916, -0.617583),
                float2(0.806274, -0.682111),
                float2(0.669481, -0.746625),
                float2(0.669549, -0.811153),
                float2(0.640702, -0.875678),
                float2(0.640770, -0.940207),
                float2(0.640837, -1.004735),
                float2(0.640905, -1.069263),
                float2(0.756629, -1.133804),
                float2(0.901266, -1.198347),
                float2(0.077217, -1.198261)
            };
            static const float2 OrangeOutline56[48] = {
                float2(0.074480, 1.416581),
                float2(0.406992, 1.416546),
                float2(0.407054, 1.356513),
                float2(0.493859, 1.296470),
                float2(0.869804, 1.236397),
                float2(1.037890, 1.176345),
                float2(1.101242, 1.116305),
                float2(1.159133, 1.056265),
                float2(1.188110, 0.996228),
                float2(1.188173, 0.936194),
                float2(1.217149, 0.876157),
                float2(1.217212, 0.816123),
                float2(1.227556, 0.756089),
                float2(1.275166, 0.696050),
                float2(1.304143, 0.636013),
                float2(1.333120, 0.575976),
                float2(1.333182, 0.515942),
                float2(1.362159, 0.455905),
                float2(1.362222, 0.395872),
                float2(1.391199, 0.335835),
                float2(1.391262, 0.275801),
                float2(1.535895, 0.215752),
                float2(1.535957, 0.155718),
                float2(1.536020, 0.095684),
                float2(1.552789, 0.035649),
                float2(1.651802, -0.024395),
                float2(1.651865, -0.084429),
                float2(1.651928, -0.144463),
                float2(1.623076, -0.204494),
                float2(1.623139, -0.264528),
                float2(1.623202, -0.324562),
                float2(1.652179, -0.384598),
                float2(1.652242, -0.444632),
                float2(1.652305, -0.504666),
                float2(1.623453, -0.564697),
                float2(1.615163, -0.624730),
                float2(1.565751, -0.684758),
                float2(1.513769, -0.744787),
                float2(1.428374, -0.804812),
                float2(1.366112, -0.864839),
                float2(0.900980, -0.924824),
                float2(0.872129, -0.984855),
                float2(0.843278, -1.044886),
                float2(0.814426, -1.104917),
                float2(0.698833, -1.164938),
                float2(0.657774, -1.224968),
                float2(0.525475, -1.284988),
                float2(0.077307, -1.284941)
            };
            static const float2 OrangeOutline86[48] = {
                float2(0.074329, 1.561048),
                float2(1.071863, 1.560943),
                float2(1.361065, 1.501521),
                float2(1.447869, 1.442120),
                float2(1.505759, 1.382723),
                float2(1.563650, 1.323325),
                float2(1.592626, 1.263930),
                float2(1.592688, 1.204538),
                float2(1.592750, 1.145147),
                float2(1.592812, 1.085755),
                float2(1.621788, 1.026360),
                float2(1.628276, 0.966968),
                float2(1.694198, 0.907569),
                float2(1.694260, 0.848177),
                float2(1.700747, 0.788785),
                float2(1.708841, 0.729392),
                float2(1.718541, 0.669999),
                float2(1.737880, 0.610606),
                float2(1.737942, 0.551214),
                float2(1.680176, 0.491828),
                float2(1.651324, 0.432440),
                float2(1.651386, 0.373048),
                float2(1.651448, 0.313656),
                float2(1.709338, 0.254258),
                float2(1.709401, 0.194866),
                float2(1.738377, 0.135472),
                float2(1.767353, 0.076077),
                float2(1.709587, 0.016691),
                float2(1.709649, -0.042701),
                float2(1.680797, -0.102089),
                float2(1.680859, -0.161481),
                float2(1.680922, -0.220873),
                float2(1.680984, -0.280265),
                float2(1.664983, -0.339655),
                float2(1.623280, -0.399042),
                float2(1.623342, -0.458434),
                float2(1.366391, -0.517799),
                float2(1.305412, -0.577184),
                float2(1.247646, -0.636570),
                float2(1.103139, -0.695946),
                float2(1.103201, -0.755338),
                float2(1.103263, -0.814730),
                float2(1.074411, -0.874119),
                float2(0.698591, -0.933471),
                float2(0.669739, -0.992860),
                float2(0.669801, -1.052252),
                float2(0.640949, -1.111640),
                float2(0.077126, -1.111581)
            };

            float2 OrangeOutlinePoint(float age, int vertex)
            {
                vertex = clamp(vertex, 0, 47);
                float2 p32 = OrangeOutline32[vertex];
                float2 p56 = OrangeOutline56[vertex];
                float2 p86 = OrangeOutline86[vertex];
                float2 early = lerp(p32, p56, smoothstep(0.283333, 1.283333, age));
                return lerp(early, p86, smoothstep(1.283333, 2.533333, age));
            }

            // ORANGE_LIMIT_62: observed curved cap; prevent the hidden left continuation forming a flat roof.
            static const float2 OrangeTop32[7] = {
                float2(0.074768, 1.142095),
                float2(0.291245, 1.503238),
                float2(0.508100, 1.503216),
                float2(0.797482, 1.272039),
                float2(1.086894, 1.011969),
                float2(1.376352, 0.708560),
                float2(1.665749, 0.462936)
            };
            static const float2 OrangeTop56[7] = {
                float2(0.074979, 0.939842),
                float2(0.291729, 1.040946),
                float2(0.508433, 1.185390),
                float2(0.797512, 1.243146),
                float2(1.086728, 1.170882),
                float2(1.376518, 0.549647),
                float2(1.666172, 0.058430)
            };
            static const float2 OrangeTop86[7] = {
                float2(0.075025, 0.896502),
                float2(0.291577, 1.185412),
                float2(0.508160, 1.445429),
                float2(0.797210, 1.532079),
                float2(1.086380, 1.503155),
                float2(1.375732, 1.300872),
                float2(1.665613, 0.592956)
            };

            float2 OrangeTopPoint(float age, int vertex)
            {
                vertex = clamp(vertex, 0, 6);
                float2 early = lerp(OrangeTop32[vertex], OrangeTop56[vertex], smoothstep(0.283333, 1.283333, age));
                return lerp(early, OrangeTop86[vertex], smoothstep(1.283333, 2.533333, age));
            }

            float OrangeTopHeight(float x, float age)
            {
                float2 a = OrangeTopPoint(age, 0);
                if (x <= a.x) return a.y;
                [unroll] for (int vertex = 1; vertex < 7; ++vertex)
                {
                    float2 b = OrangeTopPoint(age, vertex);
                    if (x <= b.x) return lerp(a.y, b.y, saturate((x - a.x) / max(b.x - a.x, 0.0001)));
                    a = b;
                }
                return a.y;
            }

            float MainOrangeReferenceField(float2 p, float age)
            {
                float nearestSq = 100.0;
                float inside = 0.0;
                float2 a = OrangeOutlinePoint(age, 47);
                [loop] for (int vertex = 0; vertex < 48; ++vertex)
                {
                    float2 b = OrangeOutlinePoint(age, vertex);
                    float2 edge = b - a;
                    float u = saturate(dot(p - a, edge) / max(dot(edge, edge), 0.0000001));
                    float2 delta = p - a - edge * u;
                    nearestSq = min(nearestSq, dot(delta, delta));
                    bool crossesY = (a.y > p.y) != (b.y > p.y);
                    float dy = abs(edge.y) < 0.0000001 ? 0.0000001 : edge.y;
                    if (crossesY && p.x < a.x + (p.y - a.y) * edge.x / dy)
                        inside = 1.0 - inside;
                    a = b;
                }
                return min(sqrt(nearestSq) * (inside > 0.5 ? 1.0 : -1.0), OrangeTopHeight(p.x, age) - p.y);
            }

            // TAIL_ISLANDS_67: measured orange/yellow vector silhouettes; no image sampled at runtime.
            static const float2 TailOrangeVertices67[1000] = {
                float2(0.435618, 1.691030),
                float2(0.508370, 1.609031),
                float2(0.578964, 1.513782),
                float2(0.568794, 1.379851),
                float2(0.508342, 1.272239),
                float2(0.581872, 1.164149),
                float2(0.633058, 1.046539),
                float2(0.702145, 0.939776),
                float2(0.785570, 1.020364),
                float2(0.841161, 0.977950),
                float2(0.765467, 0.910876),
                float2(0.651355, 0.864918),
                float2(0.635258, 0.795317),
                float2(0.762020, 0.762725),
                float2(0.870348, 0.717331),
                float2(0.934690, 0.612420),
                float2(0.987376, 0.491759),
                float2(1.107942, 0.459417),
                float2(1.217619, 0.427574),
                float2(1.292681, 0.328322),
                float2(1.241809, 0.243005),
                float2(1.119849, 0.206149),
                float2(0.987384, 0.215884),
                float2(0.885252, 0.289986),
                float2(0.805587, 0.354593),
                float2(0.698818, 0.275977),
                float2(0.596094, 0.306775),
                float2(0.567052, 0.429138),
                float2(0.558668, 0.560720),
                float2(0.449508, 0.615599),
                float2(0.436067, 0.730050),
                float2(0.465413, 0.849118),
                float2(0.501634, 0.964007),
                float2(0.457637, 1.052856),
                float2(0.360516, 1.115450),
                float2(0.326764, 1.226925),
                float2(0.404265, 1.300974),
                float2(0.403940, 1.401372),
                float2(0.347388, 1.510580),
                float2(0.336938, 1.644513),
                float2(0.616084, 0.936691),
                float2(0.696035, 0.925330),
                float2(0.760756, 0.954217),
                float2(0.843377, 0.954208),
                float2(0.870158, 0.898347),
                float2(0.831687, 0.849449),
                float2(0.901105, 0.809736),
                float2(0.965826, 0.838622),
                float2(1.012052, 0.791974),
                float2(1.043827, 0.721720),
                float2(1.087305, 0.654369),
                float2(1.101807, 0.576861),
                float2(1.132882, 0.512410),
                float2(1.153004, 0.432311),
                float2(1.202527, 0.376305),
                float2(1.267248, 0.405192),
                float2(1.335387, 0.372185),
                float2(1.362327, 0.295956),
                float2(1.309379, 0.243388),
                float2(1.229974, 0.231836),
                float2(1.153326, 0.246290),
                float2(1.079275, 0.277332),
                float2(1.042555, 0.346028),
                float2(1.019774, 0.425416),
                float2(0.987267, 0.491901),
                float2(0.928445, 0.459696),
                float2(0.904910, 0.384281),
                float2(0.849809, 0.333002),
                float2(0.769548, 0.335371),
                float2(0.740566, 0.400079),
                float2(0.740480, 0.482641),
                float2(0.721222, 0.558266),
                float2(0.668263, 0.621697),
                float2(0.614288, 0.679753),
                float2(0.565161, 0.737538),
                float2(0.482540, 0.737546),
                float2(0.422124, 0.770813),
                float2(0.450971, 0.835515),
                float2(0.468891, 0.908450),
                float2(0.537588, 0.954240),
                float2(0.689417, 0.907244),
                float2(0.739987, 0.954219),
                float2(0.811443, 0.954212),
                float2(0.882899, 0.954204),
                float2(0.954355, 0.954197),
                float2(1.007971, 0.925298),
                float2(1.075663, 0.909286),
                float2(1.101517, 0.853810),
                float2(1.150811, 0.806547),
                float2(1.173478, 0.738804),
                float2(1.218138, 0.685454),
                float2(1.224682, 0.614342),
                float2(1.231226, 0.543230),
                float2(1.217566, 0.477844),
                float2(1.232092, 0.412421),
                float2(1.272273, 0.355076),
                float2(1.333114, 0.332951),
                float2(1.291122, 0.304063),
                float2(1.219666, 0.304070),
                float2(1.174338, 0.341259),
                float2(1.174263, 0.412664),
                float2(1.152864, 0.476965),
                float2(1.109931, 0.534094),
                float2(1.066998, 0.591222),
                float2(1.012573, 0.611469),
                float2(0.993343, 0.549164),
                float2(0.990587, 0.478908),
                float2(0.965501, 0.456981),
                float2(0.926792, 0.515916),
                float2(0.914971, 0.586350),
                float2(0.867264, 0.637435),
                float2(0.827016, 0.679887),
                float2(0.826941, 0.751292),
                float2(0.787169, 0.794087),
                float2(0.739075, 0.838646),
                float2(0.711248, 0.786643),
                float2(0.688999, 0.737525),
                float2(0.682318, 0.802322),
                float2(0.653153, 0.853102),
                float2(0.624392, 0.895813),
                float2(1.078982, 0.925290),
                float2(1.101065, 0.915915),
                float2(1.121808, 0.902104),
                float2(1.130402, 0.881793),
                float2(1.130428, 0.856898),
                float2(1.145522, 0.838604),
                float2(1.170435, 0.838601),
                float2(1.190225, 0.831803),
                float2(1.197094, 0.807866),
                float2(1.203963, 0.783928),
                float2(1.210832, 0.759991),
                float2(1.218614, 0.736811),
                float2(1.240908, 0.725675),
                float2(1.246244, 0.704061),
                float2(1.246270, 0.679166),
                float2(1.246296, 0.654272),
                float2(1.246322, 0.629377),
                float2(1.230753, 0.614108),
                float2(1.217441, 0.597447),
                float2(1.217467, 0.572553),
                float2(1.201852, 0.569314),
                float2(1.178209, 0.577189),
                float2(1.174068, 0.599118),
                float2(1.174042, 0.624013),
                float2(1.162781, 0.641966),
                float2(1.144084, 0.655848),
                float2(1.139173, 0.680260),
                float2(1.134261, 0.704672),
                float2(1.125269, 0.726208),
                float2(1.103893, 0.739018),
                float2(1.082517, 0.751829),
                float2(1.061141, 0.764639),
                float2(1.058215, 0.787892),
                float2(1.058189, 0.812787),
                float2(1.058163, 0.837682),
                float2(1.041187, 0.855559),
                float2(1.029209, 0.875506),
                float2(1.029183, 0.900401),
                float2(1.029157, 0.925296),
                float2(1.054070, 0.925293),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(1.145743, 0.755269),
                float2(-1.182448, 0.622147),
                float2(-1.103682, 0.583846),
                float2(-1.031603, 0.549879),
                float2(-0.977309, 0.477659),
                float2(-0.902765, 0.535438),
                float2(-0.889340, 0.477650),
                float2(-0.978515, 0.434988),
                float2(-1.044636, 0.400240),
                float2(-1.095408, 0.337881),
                float2(-1.095293, 0.227616),
                float2(-1.153057, 0.166681),
                float2(-1.160064, 0.087619),
                float2(-1.049720, 0.087608),
                float2(-0.979431, 0.030589),
                float2(-0.948044, -0.056869),
                float2(-0.849308, -0.095323),
                float2(-0.744804, -0.114677),
                float2(-0.652300, -0.143580),
                float2(-0.592447, -0.186926),
                float2(-0.702791, -0.186915),
                float2(-0.799703, -0.231083),
                float2(-0.888010, -0.273575),
                float2(-0.941998, -0.192409),
                float2(-1.015318, -0.114854),
                float2(-1.052875, -0.027965),
                float2(-1.157216, -0.042401),
                float2(-1.250854, -0.082592),
                float2(-1.284929, -0.162164),
                float2(-1.347832, -0.237553),
                float2(-1.427880, -0.274722),
                float2(-1.503230, -0.273511),
                float2(-1.514126, -0.174028),
                float2(-1.514241, -0.063763),
                float2(-1.457117, 0.019719),
                float2(-1.427687, 0.116159),
                float2(-1.413865, 0.221782),
                float2(-1.372513, 0.321048),
                float2(-1.295436, 0.376566),
                float2(-1.205383, 0.411204),
                float2(-1.196796, 0.517868),
                float2(-0.964872, -0.066078),
                float2(-0.958816, -0.036339),
                float2(-0.936013, -0.013531),
                float2(-0.903731, -0.013534),
                float2(-0.892620, -0.034713),
                float2(-0.883555, -0.061392),
                float2(-0.858079, -0.079689),
                float2(-0.840144, -0.106532),
                float2(-0.812210, -0.114670),
                float2(-0.779928, -0.114673),
                float2(-0.747646, -0.114677),
                float2(-0.715364, -0.114680),
                float2(-0.683082, -0.114684),
                float2(-0.661214, -0.125103),
                float2(-0.661180, -0.157362),
                float2(-0.662646, -0.189167),
                float2(-0.680525, -0.216006),
                float2(-0.698404, -0.242846),
                float2(-0.716283, -0.269685),
                float2(-0.746468, -0.273590),
                float2(-0.778750, -0.273587),
                float2(-0.811032, -0.273583),
                float2(-0.843314, -0.273580),
                float2(-0.875596, -0.273576),
                float2(-0.904092, -0.288115),
                float2(-0.932296, -0.303779),
                float2(-0.960499, -0.319442),
                float2(-0.988702, -0.335106),
                float2(-1.018200, -0.345795),
                float2(-1.050482, -0.345791),
                float2(-1.051355, -0.314371),
                float2(-1.054658, -0.283101),
                float2(-1.072593, -0.256258),
                float2(-1.080361, -0.226339),
                float2(-1.080395, -0.194080),
                float2(-1.070928, -0.167686),
                float2(-1.051529, -0.147422),
                float2(-1.041703, -0.119244),
                float2(-1.017900, -0.097820),
                float2(-0.989041, -0.083396),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(-0.886905, -0.184526),
                float2(0.060175, 1.272116),
                float2(0.119524, 1.269076),
                float2(0.163134, 1.233009),
                float2(0.175926, 1.181003),
                float2(0.125977, 1.159886),
                float2(0.066227, 1.146621),
                float2(0.021679, 1.109845),
                float2(-0.026742, 1.084319),
                float2(-0.076018, 1.113217),
                float2(-0.131902, 1.104607),
                float2(-0.179807, 1.076783),
                float2(-0.231224, 1.050135),
                float2(-0.271018, 1.003689),
                float2(-0.314521, 0.961983),
                float2(-0.336022, 0.910711),
                float2(-0.347244, 0.853643),
                float2(-0.407860, 0.862301),
                float2(-0.455844, 0.888498),
                float2(-0.508376, 0.915938),
                float2(-0.557381, 0.950598),
                float2(-0.611077, 0.968807),
                float2(-0.662390, 0.998892),
                float2(-0.662454, 1.060070),
                float2(-0.662518, 1.121249),
                float2(-0.662582, 1.182427),
                float2(-0.633394, 1.214402),
                float2(-0.581700, 1.191350),
                float2(-0.522895, 1.185497),
                float2(-0.461672, 1.185491),
                float2(-0.406595, 1.167005),
                float2(-0.357579, 1.130292),
                float2(-0.306828, 1.098795),
                float2(-0.245606, 1.098789),
                float2(-0.228839, 1.151618),
                float2(-0.201525, 1.185464),
                float2(-0.158203, 1.214353),
                float2(-0.105958, 1.199806),
                float2(-0.054124, 1.186716),
                float2(-0.040993, 1.242434),
                float2(0.004956, 1.257675),
                float2(-0.106254, 1.192933),
                float2(-0.093069, 1.185452),
                float2(-0.076792, 1.185451),
                float2(-0.060516, 1.185449),
                float2(-0.055379, 1.174305),
                float2(-0.055362, 1.158039),
                float2(-0.055677, 1.142156),
                float2(-0.071792, 1.144458),
                float2(-0.087908, 1.146760),
                float2(-0.104023, 1.149062),
                float2(-0.120139, 1.151364),
                float2(-0.136255, 1.153665),
                float2(-0.152370, 1.155967),
                float2(-0.165068, 1.148055),
                float2(-0.178879, 1.142121),
                float2(-0.195155, 1.142123),
                float2(-0.211432, 1.142125),
                float2(-0.227709, 1.142127),
                float2(-0.243986, 1.142128),
                float2(-0.258102, 1.144622),
                float2(-0.260421, 1.160724),
                float2(-0.262740, 1.176826),
                float2(-0.265059, 1.192927),
                float2(-0.267377, 1.209029),
                float2(-0.269696, 1.225131),
                float2(-0.272015, 1.241233),
                float2(-0.259585, 1.249616),
                float2(-0.245034, 1.256888),
                float2(-0.243423, 1.272148),
                float2(-0.227146, 1.272146),
                float2(-0.210869, 1.272145),
                float2(-0.194592, 1.272143),
                float2(-0.178315, 1.272141),
                float2(-0.162038, 1.272139),
                float2(-0.145762, 1.272138),
                float2(-0.133207, 1.263135),
                float2(-0.121685, 1.251633),
                float2(-0.113275, 1.238843),
                float2(-0.113258, 1.222578),
                float2(-0.113241, 1.206312),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.167865, 1.198206),
                float2(-0.575495, 0.853230),
                float2(-0.527833, 0.853225),
                float2(-0.480170, 0.853220),
                float2(-0.444654, 0.830491),
                float2(-0.408011, 0.809873),
                float2(-0.360348, 0.809868),
                float2(-0.326664, 0.783713),
                float2(-0.288189, 0.766520),
                float2(-0.257333, 0.749705),
                float2(-0.237557, 0.708044),
                float2(-0.200520, 0.680375),
                float2(-0.173628, 0.641211),
                float2(-0.183433, 0.609038),
                float2(-0.230810, 0.614303),
                float2(-0.278186, 0.619568),
                float2(-0.322144, 0.634984),
                float2(-0.363040, 0.659493),
                float2(-0.409072, 0.665406),
                float2(-0.456734, 0.665411),
                float2(-0.473751, 0.622986),
                float2(-0.459555, 0.581389),
                float2(-0.459505, 0.533761),
                float2(-0.488412, 0.526868),
                float2(-0.488462, 0.574497),
                float2(-0.529673, 0.592460),
                float2(-0.559379, 0.622082),
                float2(-0.607042, 0.622087),
                float2(-0.652601, 0.617028),
                float2(-0.661944, 0.573287),
                float2(-0.661895, 0.525659),
                float2(-0.675912, 0.492075),
                float2(-0.690808, 0.524854),
                float2(-0.690858, 0.572483),
                float2(-0.713856, 0.613168),
                float2(-0.719862, 0.658995),
                float2(-0.719912, 0.706623),
                float2(-0.719962, 0.754252),
                float2(-0.691725, 0.787751),
                float2(-0.651684, 0.811999),
                float2(-0.604957, 0.821334),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(-0.486139, 0.673583),
                float2(0.016169, 1.878880),
                float2(0.047104, 1.878877),
                float2(0.076425, 1.875642),
                float2(0.095012, 1.850910),
                float2(0.113599, 1.826178),
                float2(0.141989, 1.821080),
                float2(0.172924, 1.821077),
                float2(0.196053, 1.808431),
                float2(0.215614, 1.786479),
                float2(0.234551, 1.766464),
                float2(0.238420, 1.735790),
                float2(0.242289, 1.705116),
                float2(0.246158, 1.674441),
                float2(0.264304, 1.653867),
                float2(0.274948, 1.630651),
                float2(0.271902, 1.599892),
                float2(0.268856, 1.569133),
                float2(0.265810, 1.538374),
                float2(0.262764, 1.507615),
                float2(0.235871, 1.505278),
                float2(0.205024, 1.507652),
                float2(0.174178, 1.510026),
                float2(0.143332, 1.512400),
                float2(0.112486, 1.514774),
                float2(0.081640, 1.517149),
                float2(0.074350, 1.541340),
                float2(0.064295, 1.566060),
                float2(0.040999, 1.584276),
                float2(0.023084, 1.607521),
                float2(-0.005651, 1.619005),
                float2(-0.034385, 1.630488),
                float2(-0.051880, 1.654191),
                float2(-0.070354, 1.669715),
                float2(-0.070387, 1.700628),
                float2(-0.070419, 1.731540),
                float2(-0.045346, 1.746955),
                float2(-0.041551, 1.775506),
                float2(-0.041584, 1.806418),
                float2(-0.022430, 1.830606),
                float2(-0.003131, 1.854743),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879),
                float2(0.108576, 1.682879)
            };
            static const float2 TailYellowVertices67[160] = {
                float2(-0.589665, 0.578745),
                float2(-0.552551, 0.520349),
                float2(-0.553141, 0.463691),
                float2(-0.603018, 0.423922),
                float2(-0.596344, 0.351399),
                float2(-0.589671, 0.278875),
                float2(-0.632670, 0.229268),
                float2(-0.606684, 0.164285),
                float2(-0.559738, 0.113123),
                float2(-0.488583, 0.097318),
                float2(-0.442571, 0.059826),
                float2(-0.437303, -0.012811),
                float2(-0.432035, -0.085448),
                float2(-0.470700, -0.123187),
                float2(-0.529956, -0.165508),
                float2(-0.595937, -0.186926),
                float2(-0.656604, -0.167150),
                float2(-0.722982, -0.168314),
                float2(-0.783561, -0.143566),
                float2(-0.836667, -0.106470),
                float2(-0.894070, -0.064532),
                float2(-0.941012, -0.016316),
                float2(-0.969788, 0.050621),
                float2(-1.024413, 0.068445),
                float2(-1.069386, 0.029823),
                float2(-1.130229, 0.059755),
                float2(-1.202124, 0.071735),
                float2(-1.193508, 0.102069),
                float2(-1.122089, 0.109107),
                float2(-1.055137, 0.137786),
                float2(-1.030793, 0.198485),
                float2(-1.030024, 0.267934),
                float2(-1.037580, 0.337659),
                float2(-0.992470, 0.372902),
                float2(-0.941230, 0.423159),
                float2(-0.883303, 0.461664),
                float2(-0.814193, 0.484685),
                float2(-0.742826, 0.486625),
                float2(-0.670389, 0.478576),
                float2(-0.641582, 0.538559),
                float2(-0.732805, 0.409677),
                float2(-0.723827, 0.357783),
                float2(-0.733935, 0.292341),
                float2(-0.733862, 0.222686),
                float2(-0.740579, 0.159809),
                float2(-0.800560, 0.136415),
                float2(-0.788975, 0.071415),
                float2(-0.756617, 0.015344),
                float2(-0.686912, 0.015337),
                float2(-0.617378, 0.014841),
                float2(-0.562901, -0.028678),
                float2(-0.502268, -0.046719),
                float2(-0.526678, -0.104145),
                float2(-0.573545, -0.143588),
                float2(-0.641414, -0.137500),
                float2(-0.704231, -0.114681),
                float2(-0.773937, -0.114674),
                float2(-0.843642, -0.114667),
                float2(-0.897970, -0.074992),
                float2(-0.950431, -0.050810),
                float2(-0.950504, 0.018846),
                float2(-0.985804, 0.076767),
                float2(-1.049544, 0.095543),
                float2(-1.117572, 0.094594),
                float2(-1.183776, 0.073175),
                float2(-1.253481, 0.073182),
                float2(-1.323186, 0.073190),
                float2(-1.277895, 0.102078),
                float2(-1.208190, 0.102071),
                float2(-1.139980, 0.111216),
                float2(-1.102446, 0.159846),
                float2(-1.050158, 0.195650),
                float2(-1.049682, 0.258696),
                float2(-1.095378, 0.309464),
                float2(-1.078401, 0.362097),
                float2(-1.026596, 0.390985),
                float2(-0.960282, 0.397741),
                float2(-0.916189, 0.449345),
                float2(-0.846837, 0.456268),
                float2(-0.777485, 0.463192),
                float2(-0.747357, -0.013550),
                float2(-0.728150, -0.013552),
                float2(-0.708943, -0.013554),
                float2(-0.689736, -0.013556),
                float2(-0.670529, -0.013558),
                float2(-0.651322, -0.013560),
                float2(-0.636427, -0.023990),
                float2(-0.622831, -0.037563),
                float2(-0.617916, -0.054729),
                float2(-0.617896, -0.073923),
                float2(-0.617876, -0.093116),
                float2(-0.629942, -0.100242),
                float2(-0.649149, -0.100240),
                float2(-0.668356, -0.100238),
                float2(-0.687563, -0.100236),
                float2(-0.706770, -0.100234),
                float2(-0.725977, -0.100232),
                float2(-0.743958, -0.095020),
                float2(-0.761147, -0.086435),
                float2(-0.770407, -0.069920),
                float2(-0.781567, -0.056887),
                float2(-0.800774, -0.056885),
                float2(-0.819981, -0.056883),
                float2(-0.839188, -0.056881),
                float2(-0.858395, -0.056879),
                float2(-0.877602, -0.056877),
                float2(-0.893684, -0.046527),
                float2(-0.909676, -0.035879),
                float2(-0.921546, -0.023025),
                float2(-0.921567, -0.003832),
                float2(-0.921587, 0.015361),
                float2(-0.902380, 0.015359),
                float2(-0.883173, 0.015357),
                float2(-0.863966, 0.015355),
                float2(-0.844759, 0.015353),
                float2(-0.825552, 0.015351),
                float2(-0.806345, 0.015349),
                float2(-0.789531, 0.011472),
                float2(-0.780923, -0.005695),
                float2(-0.766564, -0.013548),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207),
                float2(-0.766525, -0.039207)
            };

            float TailOrangePolygon67(float2 p, int island, int key0, float blend)
            {
                int first = (island * 5 + key0) * 40;
                float2 a = lerp(TailOrangeVertices67[first + 39], TailOrangeVertices67[first + 79], blend);
                float nearestSq = 100.0, inside = 0.0;
                [loop] for (int v = 0; v < 40; ++v)
                {
                    float2 b = lerp(TailOrangeVertices67[first + v], TailOrangeVertices67[first + 40 + v], blend);
                    float2 e = b - a;
                    float2 d = p - a - e * saturate(dot(p - a, e) / max(dot(e, e), 0.0000001));
                    nearestSq = min(nearestSq, dot(d, d));
                    bool crossing = (a.y > p.y) != (b.y > p.y);
                    float dy = abs(e.y) < 0.0000001 ? 0.0000001 : e.y;
                    if (crossing && p.x < a.x + (p.y - a.y) * e.x / dy) inside = 1.0 - inside;
                    a = b;
                }
                return sqrt(nearestSq) * (inside > 0.5 ? 1.0 : -1.0);
            }

            float TailOrangeField67(float2 p, float age)
            {
                if (age >= 3.700000) return -100.0;
                int key0 = 0; float blend = smoothstep(3.116667, 3.283333, age);
                if (age > 3.283333) { key0 = 1; blend = smoothstep(3.283333, 3.450000, age); }
                if (age > 3.450000) { key0 = 2; blend = smoothstep(3.450000, 3.616667, age); }
                if (age > 3.616667) { key0 = 3; blend = smoothstep(3.616667, 3.700000, age); }
                float field = -100.0;
                [loop] for (int island = 0; island < 5; ++island)
                    field = max(field, TailOrangePolygon67(p, island, key0, blend));
                return field;
            }

            float TailYellowField67(float2 p, float age)
            {
                if (age >= 3.283333) return -100.0;
                int key0 = 0; float blend = smoothstep(3.033333, 3.116667, age);
                if (age > 3.116667) { key0 = 1; blend = smoothstep(3.116667, 3.200000, age); }
                if (age > 3.200000) { key0 = 2; blend = smoothstep(3.200000, 3.283333, age); }
                int first = key0 * 40;
                float2 a = lerp(TailYellowVertices67[first + 39], TailYellowVertices67[first + 79], blend);
                float nearestSq = 100.0, inside = 0.0;
                [loop] for (int v = 0; v < 40; ++v)
                {
                    float2 b = lerp(TailYellowVertices67[first + v], TailYellowVertices67[first + 40 + v], blend);
                    float2 e = b - a;
                    float2 d = p - a - e * saturate(dot(p - a, e) / max(dot(e, e), 0.0000001));
                    nearestSq = min(nearestSq, dot(d, d));
                    bool crossing = (a.y > p.y) != (b.y > p.y);
                    float dy = abs(e.y) < 0.0000001 ? 0.0000001 : e.y;
                    if (crossing && p.x < a.x + (p.y - a.y) * e.x / dy) inside = 1.0 - inside;
                    a = b;
                }
                return sqrt(nearestSq) * (inside > 0.5 ? 1.0 : -1.0);
            }

            float MainConnectedBodyField(float2 q, float age, float phase)
            {
                float late = smoothstep(1.616667, 2.533333, age);
                float2 ripple = float2(0.015 * sin(phase * 0.8), 0.012 * cos(phase * 1.1));
                float f = FireEllipse(q - float2(-0.04, -0.015), float2(0.58, 0.59));
                // One large right sheet, a lower return, and an uneven left/upward shoulder.
                float2 rightCentre = lerp(float2(0.42, 0.34), float2(0.35, 0.40), late);
                float2 rightRadius = lerp(float2(0.49, 0.50), float2(0.43, 0.46), late);
                f = FireMerge(f, FireEllipse(q - rightCentre - ripple, rightRadius), 0.065);
                f = FireMerge(f, FireEllipse(q - float2(0.35, -0.45) + ripple.yx,
                    float2(0.40, 0.43)), 0.065);
                float2 leftCentre = lerp(float2(-0.45, -0.33), float2(-0.39, -0.33), late);
                float2 leftRadius = lerp(float2(0.40, 0.42), float2(0.33, 0.42), late);
                f = FireMerge(f, FireEllipse(q - leftCentre, leftRadius), 0.055);
                f = FireMerge(f, FireEllipse(q - float2(-0.22, 0.50), float2(0.31, 0.34)), 0.055);
                // Unequal edge-open curls separate large sheets without outlining every lobe.
                f = min(f, -FireEllipse(q - float2(0.80, -0.035), float2(0.17, 0.16)));
                f = min(f, -FireEllipse(q - float2(-0.77, 0.13), float2(0.13, 0.20)));
                return min(f, -FireEllipse(q - float2(-0.025, -0.86), float2(0.13, 0.16)));
            }

            // WARM_VECTOR_57: six measured vector outlines, no reference image texture.
            // Only the two right cream fields use these; other fields and all color keys stay intact.
            static const float2 WarmContour32_0[48] = {
                float2(-0.501145, 1.093801),
                float2(-0.343893, 0.963594),
                float2(-0.327824, 0.766762),
                float2(-0.424916, 0.638296),
                float2(-0.563014, 0.490217),
                float2(-0.429116, 0.375044),
                float2(-0.275402, 0.250028),
                float2(-0.070042, 0.250007),
                float2(0.060816, 0.404668),
                float2(0.224621, 0.528231),
                float2(0.376799, 0.624959),
                float2(0.518941, 0.494146),
                float2(0.692550, 0.406177),
                float2(0.868860, 0.487615),
                float2(0.797656, 0.387524),
                float2(0.781050, 0.190572),
                float2(0.781265, -0.014788),
                float2(0.871599, -0.155154),
                float2(0.969053, -0.290385),
                float2(1.050985, -0.406360),
                float2(1.201632, -0.512910),
                float2(1.281974, -0.692592),
                float2(1.371128, -0.856074),
                float2(1.438569, -1.022227),
                float2(1.305790, -1.108649),
                float2(1.132405, -1.175265),
                float2(0.934693, -1.187597),
                float2(0.729333, -1.187576),
                float2(0.523973, -1.187554),
                float2(0.318612, -1.187533),
                float2(0.113252, -1.187511),
                float2(-0.061291, -1.156644),
                float2(0.013914, -0.990721),
                float2(0.000327, -0.796118),
                float2(-0.124303, -0.667626),
                float2(-0.129091, -0.464161),
                float2(-0.323430, -0.437466),
                float2(-0.528790, -0.437445),
                float2(-0.734151, -0.437423),
                float2(-0.717732, -0.270513),
                float2(-0.533004, -0.189396),
                float2(-0.500967, 0.001980),
                float2(-0.610597, 0.172978),
                float2(-0.718116, 0.327710),
                float2(-0.750538, 0.513155),
                float2(-0.750752, 0.718515),
                float2(-0.750967, 0.923875),
                float2(-0.685735, 1.049497)
            };
            static const float2 WarmContour32_1[48] = {
                float2(-0.367678, 0.690504),
                float2(-0.176067, 0.690489),
                float2(0.015544, 0.690475),
                float2(0.207155, 0.690461),
                float2(0.398766, 0.690446),
                float2(0.590377, 0.690432),
                float2(0.781988, 0.690418),
                float2(0.973599, 0.690403),
                float2(1.083997, 0.623231),
                float2(1.132606, 0.496835),
                float2(1.037761, 0.418804),
                float2(0.866114, 0.377537),
                float2(0.930614, 0.252867),
                float2(0.999811, 0.129089),
                float2(0.993540, -0.006235),
                float2(0.871581, -0.110614),
                float2(0.766974, -0.210060),
                float2(0.852151, -0.325081),
                float2(0.827779, -0.442753),
                float2(0.716154, -0.530276),
                float2(0.531395, -0.559810),
                float2(0.367552, -0.604465),
                float2(0.366332, -0.741165),
                float2(0.250881, -0.821677),
                float2(0.107584, -0.904770),
                float2(-0.032063, -0.867603),
                float2(-0.156987, -0.831306),
                float2(-0.338702, -0.874573),
                float2(-0.387004, -0.984415),
                float2(-0.239053, -1.051728),
                float2(-0.316828, -1.095214),
                float2(-0.494667, -1.071391),
                float2(-0.680099, -1.044006),
                float2(-0.821643, -1.084352),
                float2(-0.931767, -1.070415),
                float2(-0.931967, -0.933550),
                float2(-0.915971, -0.798563),
                float2(-0.855568, -0.668726),
                float2(-0.819674, -0.538363),
                float2(-0.766056, -0.417419),
                float2(-0.718328, -0.288637),
                float2(-0.670415, -0.157013),
                float2(-0.633033, -0.022809),
                float2(-0.589874, 0.109979),
                float2(-0.521955, 0.236577),
                float2(-0.487876, 0.371232),
                float2(-0.399935, 0.429593),
                float2(-0.340382, 0.555012)
            };
            static const float2 WarmContour56_0[48] = {
                float2(-0.801413, 0.713893),
                float2(-0.674583, 0.791123),
                float2(-0.520190, 0.834941),
                float2(-0.387374, 0.932597),
                float2(-0.234553, 1.000022),
                float2(-0.059968, 0.988496),
                float2(0.114617, 0.976970),
                float2(0.271389, 0.924787),
                float2(0.411568, 0.834730),
                float2(0.486799, 0.712812),
                float2(0.565958, 0.581486),
                float2(0.626142, 0.468502),
                float2(0.606767, 0.355146),
                float2(0.566407, 0.213277),
                float2(0.566590, 0.063203),
                float2(0.670144, -0.045361),
                float2(0.788931, -0.152206),
                float2(0.867396, -0.286443),
                float2(0.901301, -0.429651),
                float2(0.967354, -0.562631),
                float2(1.066335, -0.684477),
                float2(1.117429, -0.827963),
                float2(1.168007, -0.971666),
                float2(1.242635, -1.096066),
                float2(1.232172, -1.171540),
                float2(1.057085, -1.171524),
                float2(0.881999, -1.171509),
                float2(0.706912, -1.171493),
                float2(0.531825, -1.171477),
                float2(0.356739, -1.171462),
                float2(0.181652, -1.171446),
                float2(0.006565, -1.171430),
                float2(-0.164202, -1.168745),
                float2(-0.080471, -1.041487),
                float2(-0.110284, -0.917284),
                float2(-0.273818, -0.877258),
                float2(-0.448203, -0.863655),
                float2(-0.593621, -0.799805),
                float2(-0.712066, -0.689167),
                float2(-0.830511, -0.578528),
                float2(-0.899449, -0.451381),
                float2(-0.899633, -0.301307),
                float2(-0.899816, -0.151232),
                float2(-0.899999, -0.001158),
                float2(-0.900182, 0.148916),
                float2(-0.900365, 0.298991),
                float2(-0.900548, 0.449065),
                float2(-0.900731, 0.599139)
            };
            static const float2 WarmContour56_1[48] = {
                float2(-0.261914, 0.950018),
                float2(-0.064013, 0.950004),
                float2(0.133888, 0.949989),
                float2(0.331789, 0.949975),
                float2(0.459680, 0.881873),
                float2(0.522458, 0.750447),
                float2(0.603433, 0.626897),
                float2(0.682061, 0.507990),
                float2(0.735863, 0.381859),
                float2(0.864812, 0.276669),
                float2(0.928342, 0.154538),
                float2(0.928549, 0.016007),
                float2(0.989352, -0.096295),
                float2(1.036053, -0.226103),
                float2(1.036260, -0.364634),
                float2(0.951866, -0.486886),
                float2(0.824534, -0.550061),
                float2(0.750975, -0.651633),
                float2(0.738227, -0.787141),
                float2(0.603253, -0.875044),
                float2(0.455066, -0.941253),
                float2(0.268751, -0.978514),
                float2(0.071167, -0.986184),
                float2(-0.126418, -0.993854),
                float2(-0.312425, -0.980254),
                float2(-0.320167, -0.843908),
                float2(-0.349044, -0.715936),
                float2(-0.332816, -0.577885),
                float2(-0.356378, -0.450038),
                float2(-0.533727, -0.399961),
                float2(-0.725989, -0.383150),
                float2(-0.783288, -0.257090),
                float2(-0.745086, -0.161908),
                float2(-0.918453, -0.101572),
                float2(-1.115522, -0.114099),
                float2(-1.301857, -0.109685),
                float2(-1.392868, 0.007657),
                float2(-1.393075, 0.146187),
                float2(-1.355731, 0.274326),
                float2(-1.345536, 0.412665),
                float2(-1.335341, 0.551003),
                float2(-1.325146, 0.689341),
                float2(-1.286935, 0.817464),
                float2(-1.251419, 0.950091),
                float2(-1.053518, 0.950076),
                float2(-0.855617, 0.950062),
                float2(-0.657716, 0.950047),
                float2(-0.459815, 0.950033)
            };
            static const float2 WarmContour86_0[48] = {
                float2(-0.794297, 0.241866),
                float2(-0.727944, 0.375107),
                float2(-0.574656, 0.411887),
                float2(-0.432229, 0.511039),
                float2(-0.274868, 0.555611),
                float2(-0.128786, 0.620596),
                float2(0.029402, 0.582731),
                float2(0.165783, 0.499975),
                float2(0.281122, 0.535657),
                float2(0.426085, 0.499936),
                float2(0.586596, 0.499912),
                float2(0.646566, 0.666570),
                float2(0.803147, 0.653072),
                float2(0.951135, 0.610111),
                float2(1.077998, 0.482506),
                float2(1.205612, 0.365608),
                float2(1.205780, 0.138217),
                float2(1.126519, -0.035569),
                float2(1.161929, -0.229334),
                float2(1.264944, -0.323108),
                float2(1.265112, -0.550499),
                float2(1.250558, -0.768428),
                float2(1.129871, -0.918148),
                float2(1.002257, -1.041818),
                float2(0.877664, -0.935604),
                float2(0.826747, -0.719875),
                float2(0.716286, -0.574265),
                float2(0.619547, -0.423511),
                float2(0.507500, -0.270761),
                float2(0.373746, -0.166723),
                float2(0.281660, -0.012046),
                float2(0.136965, 0.083312),
                float2(-0.012934, 0.038373),
                float2(-0.122282, -0.122248),
                float2(-0.176229, -0.328402),
                float2(-0.027185, -0.403687),
                float2(0.088613, -0.512938),
                float2(0.088781, -0.740329),
                float2(0.072833, -0.962329),
                float2(-0.071210, -0.999992),
                float2(-0.219310, -1.044989),
                float2(-0.377609, -1.082348),
                float2(-0.431203, -0.872926),
                float2(-0.440699, -0.647679),
                float2(-0.440867, -0.420288),
                float2(-0.470434, -0.210152),
                float2(-0.568294, -0.030248),
                float2(-0.666970, 0.149258)
            };
            static const float2 WarmContour86_1[48] = {
                float2(0.425298, 0.333294),
                float2(0.541670, 0.444395),
                float2(0.707768, 0.481416),
                float2(0.891169, 0.481399),
                float2(1.072864, 0.471975),
                float2(1.226388, 0.390838),
                float2(1.332386, 0.260279),
                float2(1.374877, 0.106131),
                float2(1.375069, -0.056892),
                float2(1.344848, -0.206926),
                float2(1.201756, -0.308752),
                float2(1.083817, -0.432230),
                float2(0.930426, -0.461695),
                float2(0.806913, -0.372891),
                float2(0.748734, -0.218228),
                float2(0.668502, -0.075687),
                float2(0.520253, -0.000048),
                float2(0.416777, -0.092682),
                float2(0.476964, -0.241544),
                float2(0.568123, -0.383096),
                float2(0.583973, -0.542423),
                float2(0.583222, -0.705224),
                float2(0.492396, -0.846759),
                float2(0.386721, -0.972212),
                float2(0.212792, -1.023748),
                float2(0.080899, -1.129950),
                float2(-0.036364, -1.255176),
                float2(-0.192291, -1.333316),
                float2(-0.320487, -1.420931),
                float2(-0.496877, -1.432195),
                float2(-0.679177, -1.414176),
                float2(-0.810745, -1.333259),
                float2(-0.903159, -1.263802),
                float2(-0.875461, -1.102678),
                float2(-0.847764, -0.941553),
                float2(-0.802248, -0.784753),
                float2(-0.749257, -0.630838),
                float2(-0.749449, -0.467815),
                float2(-0.698035, -0.315623),
                float2(-0.572752, -0.314640),
                float2(-0.447094, -0.407366),
                float2(-0.301989, -0.352394),
                float2(-0.196758, -0.223923),
                float2(-0.093214, -0.112534),
                float2(-0.087637, 0.033254),
                float2(0.093993, 0.037029),
                float2(0.166483, 0.157179),
                float2(0.260881, 0.272616)
            };

            float2 WarmContourPoint(float age, int index, int vertex)
            {
                float2 p32 = index == 0 ? WarmContour32_0[vertex] : WarmContour32_1[vertex];
                float2 p56 = index == 0 ? WarmContour56_0[vertex] : WarmContour56_1[vertex];
                float2 p86 = index == 0 ? WarmContour86_0[vertex] : WarmContour86_1[vertex];
                if (age <= 1.283333)
                    return lerp(p32, p56, smoothstep(0.283333, 1.283333, age));
                return lerp(p56, p86, smoothstep(1.283333, 2.533333, age));
            }

            float MainWarmVectorField(float2 p, int index, float age)
            {
                float nearestSq = 100.0;
                float inside = 0.0;
                float2 a = WarmContourPoint(age, index, 47);
                [loop] for (int vertex = 0; vertex < 48; ++vertex)
                {
                    float2 b = WarmContourPoint(age, index, vertex);
                    float2 edge = b - a;
                    float u = saturate(dot(p - a, edge) / max(dot(edge, edge), 0.0000001));
                    float2 delta = p - a - edge * u;
                    nearestSq = min(nearestSq, dot(delta, delta));
                    bool crossesY = (a.y > p.y) != (b.y > p.y);
                    float dy = abs(edge.y) < 0.0000001 ? 0.0000001 : edge.y;
                    float crossingX = a.x + (p.y - a.y) * edge.x / dy;
                    if (crossesY && p.x < crossingX) inside = 1.0 - inside;
                    a = b;
                }
                return sqrt(nearestSq) * (inside > 0.5 ? 1.0 : -1.0);
            }

            float MainWarmFoldField(float2 p, int index, float age, float phase)
            {
                // Gentle advection, smaller than a fold: detail must not dissolve the broad anatomy.
                p.x += 0.035 * sin(p.y * 5.2 + phase * 0.8 + index * 1.4);
                p.y += 0.025 * sin(p.x * 6.1 - phase * 0.65 + index * 2.1);
                float f;
                if (index == 0)
                {
                    // Upper-right cap curls open below as the late blast develops.
                    f = FireEllipse(p - float2(-0.10, 0.10), float2(0.88, 0.78));
                    f = FireMerge(f, FireEllipse(p - float2(-0.48, 0.43), float2(0.48, 0.45)), 0.12);
                    f = FireMerge(f, FireEllipse(p - float2(0.46, -0.16), float2(0.49, 0.66)), 0.12);
                    float curlY = lerp(-0.91, -0.54, smoothstep(1.90, 2.533333, age));
                    f = min(f, -FireEllipse(p - float2(0.03, curlY), float2(0.33, 0.40)));
                    return min(f, -FireEllipse(p - float2(0.89, 0.46), float2(0.18, 0.22)));
                }
                if (index == 1)
                {
                    // Lower-right return is tall, with unequal upper and lower lobes.
                    f = FireEllipse(p - float2(-0.13, 0.04), float2(0.78, 0.83));
                    f = FireMerge(f, FireEllipse(p - float2(0.35, 0.42), float2(0.52, 0.48)), 0.11);
                    f = FireMerge(f, FireEllipse(p - float2(0.26, -0.53), float2(0.57, 0.41)), 0.11);
                    f = min(f, -FireEllipse(p - float2(0.87, -0.05), float2(0.31, 0.28)));
                    return min(f, -FireEllipse(p - float2(-0.83, 0.62), float2(0.20, 0.20)));
                }
                if (index == 2)
                {
                    // The lower-left sheet has a broad outer shoulder and a narrow inward return.
                    p = FireRotate(p, -0.16);
                    f = FireEllipse(p - float2(-0.05, -0.04), float2(0.70, 0.83));
                    f = FireMerge(f, FireEllipse(p - float2(-0.43, 0.29), float2(0.52, 0.48)), 0.12);
                    f = FireMerge(f, FireEllipse(p - float2(0.34, -0.42), float2(0.46, 0.50)), 0.12);
                    return min(f, -FireEllipse(p - float2(0.15, 0.78), float2(0.31, 0.30)));
                }
                // Broad upper-left fold: only two rounded notches, not a uniform seven-slit crown.
                f = FireEllipse(p - float2(-0.05, -0.13), float2(0.85, 0.67));
                f = FireMerge(f, FireEllipse(p - float2(-0.48, 0.39), float2(0.43, 0.50)), 0.11);
                f = FireMerge(f, FireEllipse(p - float2(0.08, 0.48), float2(0.43, 0.46)), 0.11);
                f = FireMerge(f, FireEllipse(p - float2(0.52, 0.12), float2(0.40, 0.48)), 0.11);
                f = min(f, -FireEllipse(p - float2(-0.19, 0.86), float2(0.15, 0.32)));
                return min(f, -FireEllipse(p - float2(0.47, 0.71), float2(0.14, 0.29)));
            }

            // EARLY_CONTROL_52: recover measured47 anatomy only at the two early white impacts.
            float MainEarlyPulseControl(float2 q, float phase)
            {
                q = MainRadiantWarp(q, phase);
                q /= 1.025; // both measured47 early white areas were about5% below reference
                float f = FireEllipse(q - float2(-0.04, 0.005), float2(0.26, 0.31));
                f = FireMerge(f, FireEllipse(q - float2(-0.12, 0.30), float2(0.16, 0.23)), 0.055);
                f = FireMerge(f, FireEllipse(q - float2(0.02, -0.25), float2(0.16, 0.24)), 0.055);
                f = FireMerge(f, FireEllipse(q - float2(-0.23, -0.08), float2(0.14, 0.18)), 0.045);
                f = FireMerge(f, FireEllipse(q - float2(0.13, 0.10), float2(0.12, 0.14)), 0.045);
                // Two broad, edge-open concavities preserve a connected folded sheet.
                f = min(f, -FireEllipse(q - float2(0.24, 0.29), float2(0.10, 0.12)));
                return min(f, -FireEllipse(q - float2(-0.22, -0.42), float2(0.10, 0.12)));
            }

            // HEART_IMPACT_46_SHAPE: one irregular connected impact, not a vertical stack of caps.
            float MainConnectedHeartField(float2 q, float phase, float age)
            {
                float2 centre;
                if (age < 0.483333)
                    centre = HeartKeyBlend(age, 0.233333, 0.483333,
                        float4(-0.04, 0.005, 0, 0), float4(-0.15, -0.15, 0, 0)).xy;
                else if (age < 1.816667)
                    centre = HeartKeyBlend(age, 0.483333, 1.816667,
                        float4(-0.15, -0.15, 0, 0), float4(-0.08, 0.02, 0, 0)).xy;
                else
                    centre = HeartKeyBlend(age, 2.316667, 2.733333,
                        float4(-0.08, 0.02, 0, 0), float4(-0.085, 0.03, 0, 0)).xy;
                // EARLY_CONNECTED_64: keep the measured52 draw shifts, recover48's single irregular field.
                // Area-only compensation is restricted to the two early impacts; no stacked ellipse caps.
                float connected22 = exp(-pow((age - 0.066667) / 0.028, 2.0));
                float connected26 = exp(-pow((age - 0.233333) / 0.030, 2.0));
                float earlyScale = 1.0 + 0.01807264 * connected22 + 0.09874369 * connected26;
                q = centre + (q - centre) / earlyScale;
                float2 warped = MainRadiantWarp(q, phase);
                float2 p = (warped - centre) / float2(0.30, 0.40);
                float theta = atan2(p.y, p.x);
                // Unequal broad folds carry smaller concave notches. There are no circular sub-lobes.
                float boundary = 1.0 + 0.12 * sin(theta * 3.0 + 0.45)
                    + 0.095 * sin(theta * 5.0 - 1.8)
                    + 0.050 * sin(theta * 9.0 + phase * 0.55);
                float f = (boundary - length(p)) * 0.30;
                f += 0.010 * sin(q.y * 29.0 + q.x * 11.0 + phase * 0.65)
                    + 0.006 * sin(q.x * 37.0 - q.y * 8.0 - phase * 0.45);
                // Two edge-open pinches vary the larger folds without cutting detached holes.
                f = min(f, -FireEllipse(warped - centre - float2(0.27, 0.19), float2(0.085, 0.12)));
                f = min(f, -FireEllipse(warped - centre - float2(-0.25, -0.32), float2(0.085, 0.10)));
                return f;
            }

            half4 MainRadiantFire(Varyings i)
            {
                bool heart = i.shape.x > 0.5;
                float age = i.life.y;
                float phase = heart ? max(0.0, age - 0.20) * 1.6 + 2.13 : age * 1.35 + 1.8;
                float2 q = i.q;
                uint row = CelRowA(i.tone.x);
                float alpha;
                half3 col;
                if (heart)
                {
                    float foldField = MainConnectedHeartField(q, phase, age);
                    float density = 0.12 + 1.18 * smoothstep(-0.005, 0.15, foldField);
                    float edge = max(fwidth(foldField), lerp(0.028, 0.016, smoothstep(0.24, 0.48, age)));
                    float coverage = smoothstep(-edge, edge, foldField);
                    float fade = smoothstep(0.0, 0.04, age) * (1.0 - smoothstep(3.02, 3.34, age));
                    alpha = coverage * fade * i.tone.z;
                    // HEART_IMPACT_46_PEAK: reference white peaks briefly near86, not throughout82-90.
                    float heartNoise = CelValue2(q * 3.6 + float2(1.7, -phase * 0.24));
                    float heartHeat = 0.50 + 0.12 * sin(q.y * 4.7 + q.x * 3.0 + phase * 0.6)
                        + 0.22 * (heartNoise - 0.5);
                    float pulse22 = exp(-pow((age - 0.066667) / 0.028, 2.0));
                    float pulse26 = exp(-pow((age - 0.233333) / 0.030, 2.0));
                    heartHeat += 0.04 * pulse22 + 0.10 * pulse26;
                    heartHeat -= max(q.y, 0.0) * 0.45 * pulse22;
                    float earlyHeat = max(0.85 * pulse22, 0.95 * pulse26);
                    float ordinaryHot = smoothstep(0.47, 0.64, heartHeat) * max(0.12, earlyHeat);
                    // At86, current radius/aspect/offset project this centre to about(813,367).
                    // Radii project to about34x46px before the soft boundary and other scene layers.
                    float2 peakQ = q + float2(
                        0.010 * sin(q.y * 19.0 + phase * 0.7),
                        0.010 * sin(q.x * 21.0 - phase * 0.6));
                    float peakField = FireEllipse(peakQ - float2(-0.085, 0.10), float2(0.25, 0.31));
                    float peakPocket = smoothstep(-0.018, 0.045, peakField);
                    float pulse86 = exp(-pow((age - 2.733333) / 0.085, 2.0));
                    float hot = max(ordinaryHot, peakPocket * pulse86)
                        * smoothstep(0.12, 0.75, density);
                    half3 pale = lerp(CelPal(row, 1), CelPal(row, 3),
                        (half)smoothstep(0.08, 1.10, density));
                    col = lerp(pale, CelPalCore(row).rgb * (half)max(0.0, i.tone.y), (half)hot);
                    // LATE_HEART_COLOR_63: the reference94 upper fold is cream;
                    // lower pixels at(788,400)/(808,390) are orange, not a new white flash.
                    // Keep the existing irregular alpha envelope, every position and the86 hot pulse.
                    float lateRecolor = smoothstep(2.900000, 3.066667, age)
                        * (1.0 - smoothstep(3.150000, 3.300000, age));
                    float upperFold = smoothstep(-0.120, 0.045,
                        q.y + 0.10 * (-q.x - 0.18));
                    half3 lateOrange = CelPal(row, 0) * half3(1.10, 1.08, 1.80);
                    half3 lateCream = CelPal(row, 3) * half3(1.0, 1.0, 1.18);
                    half3 lateFoldColor = lerp(lateOrange, lateCream, (half)upperFold);
                    col = lerp(col, lateFoldColor, (half)lateRecolor);
                }
                else
                {
                    float2 warped = MainRadiantWarp(q, phase);
                    float field = MainConnectedBodyField(warped, age, phase);
                    // BODY_HEM_48: end the sheet above the ground plane with an uneven orange hem.
                    // Use unwarped q so domain advection cannot push the whole card back below ground.
                    float lowerHem = q.y + 0.70 + 0.035 * sin(q.x * 9.1 + phase * 0.8)
                        + 0.020 * sin(q.x * 18.7 - phase * 0.7);
                    field = min(field, lowerHem);
                    float4 outlineKey = AuthoredBodyKey(age);
                    float2 bodyPlane = q * outlineKey.x * float2(1.0, outlineKey.y) + outlineKey.zw;
                    float rightOutline = MainOrangeReferenceField(bodyPlane, age) / max(outlineKey.x, 0.01);
                    // The narrow join is behind the dummy; keep the existing left body intact.
                    float measuredEnvelope = 1.0 - smoothstep(2.533333, 2.783333, age);
                    field = lerp(field, rightOutline, smoothstep(0.16, 0.36, bodyPlane.x) * measuredEnvelope);
                    // Existing-color retreat breaks the late mass; it is not a longer opaque wall.
                    float tailRetreat = smoothstep(2.866667, 3.700000, age);
                    float retreatBias = 0.16 + 0.24 * (1.0 - smoothstep(0.0, 0.50, q.x))
                        + 0.10 * (1.0 - smoothstep(-0.05, 0.45, q.y));
                    field -= tailRetreat * retreatBias;
                    float measuredTail = smoothstep(2.866667, 3.116667, age);
                    [branch] if (age > 2.866667)
                        field = lerp(field, TailOrangeField67(bodyPlane, age) / max(outlineKey.x, 0.01), measuredTail);
                    float edge = max(fwidth(field), 0.020); // MATERIAL_37: restore a readable orange boundary
                    float coverage = smoothstep(-edge, edge * 1.25, field);
                    // This now carries the orange mass formerly provided by many short opaque cells.
                    float opacity = lerp(0.86, 0.98, smoothstep(1.616667, 2.533333, age));
                    float bodyTailFade = 1.0 - smoothstep(2.82, 3.20, age);
                    if (age > 2.866667)
                        bodyTailFade = (1.0 - smoothstep(2.82, 3.20, 2.866667))
                            * (1.0 - smoothstep(2.866667, 3.750000, age));
                    float fade = smoothstep(0.0, 0.283333, age) * bodyTailFade;
                    float thickness = lerp(0.76, 1.0, smoothstep(-0.02, 0.24, field));
                    // Tail islands shrink geometrically; their surviving interiors do not become translucent disks.
                    float materialOpacity = lerp(opacity * fade * thickness, 0.98, measuredTail);
                    alpha = coverage * materialOpacity * i.tone.z;
                    float n0 = CelValue2(warped * 3.8 + float2(2.1 + phase * 0.24, -phase * 0.44));
                    float n1 = CelValue2(warped * 7.2 + float2(-phase * 0.34, 7.7));
                    float heat = 0.43 + 0.24 * (n0 - 0.5) + 0.10 * (n1 - 0.5)
                        + 0.065 * sin(warped.y * 4.5 - warped.x * 2.6 + phase * 0.65);
                    // Continuous orange-to-gold, with no C0 boundary or brown interior cut.
                    col = lerp(CelPal(row, 1) * (half)1.08, CelPal(row, 2),
                        (half)smoothstep(0.38, 0.73, heat));
                    // TAIL_RESPONSE_69: opaque68 orange was220/78/5 vs reference253/143/65.
                    // Lift the missing warm channels without reinstating a glow card or changing silhouettes.
                    float lateTailColor = smoothstep(3.450000, 3.616667, age);
                    half3 tailColor;
                    // TAIL_CALIBRATION_72: measured71 orange retained too little red and too much blue.
                    tailColor.r = col.r * (half)lerp(1.30, 1.38, lateTailColor);
                    tailColor.g = col.g * (half)0.65 + (half)lerp(0.33, 0.32, lateTailColor);
                    tailColor.b = col.b * (half)0.65 + (half)lerp(0.17, 0.23, lateTailColor);
                    col = lerp(col, tailColor, (half)measuredTail);
                }
                alpha = saturate(alpha);
                clip(alpha - 0.002);
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                col = MixFog(col, i.fog);
                return half4(col * (half)alpha, (half)alpha);
            }

            // WARM_BODY_ISLANDS_30: four independent procedural warm pockets above orange.
            // Their keys are camera-plane world units. No reference pixels, flipbook or HDR core gain.
            float4 WarmBodyInterpolate(float age, float4 k32, float4 k48, float4 k56, float4 k64, float4 k76, float4 k86)
            {
                if (age < 0.283333) return k32;
                if (age < 0.95) return HeartKeyBlend(age, 0.283333, 0.95, k32, k48);
                if (age < 1.283333) return HeartKeyBlend(age, 0.95, 1.283333, k48, k56);
                if (age < 1.616667) return HeartKeyBlend(age, 1.283333, 1.616667, k56, k64);
                if (age < 2.116667) return HeartKeyBlend(age, 1.616667, 2.116667, k64, k76);
                if (age < 2.533333) return HeartKeyBlend(age, 2.116667, 2.533333, k76, k86);
                return k86;
            }

            float4 WarmBodyPocketKey(float age, int index)
            {
                float4 k32 = index == 0 ? float4(0.754580, 0.824198, 0.462624, 0.462293) : index == 1 ? float4(0.914408, 0.058509, 0.433710, 0.606759) : index == 2 ? float4(-0.949911, -0.548055, 0.390339, 0.346719) : float4(-0.344531, 1.185479, 0.332511, 0.288933);
                float4 k48 = index == 0 ? float4(0.610282, 0.564173, 0.433710, 0.274486) : index == 1 ? float4(0.958278, -0.418235, 0.462624, 0.404506) : index == 2 ? float4(-0.848757, -0.504726, 0.404796, 0.346719) : float4(-0.358625, 0.838761, 0.260226, 0.332273);
                float4 k56 = index == 0 ? float4(0.870417, 0.650826, 0.433710, 0.505632) : index == 1 ? float4(1.146007, -0.216001, 0.404796, 0.577866) : index == 2 ? float4(-0.848727, -0.533619, 0.419253, 0.361166) : float4(-0.402162, 0.997678, 0.361425, 0.332273);
                float4 k64 = index == 0 ? float4(0.653607, 0.607509, 0.260226, 0.288933) : index == 1 ? float4(0.553875, -0.793805, 0.346968, 0.418953) : index == 2 ? float4(-0.892038, -0.591401, 0.346968, 0.404506) : float4(-0.503618, 1.243282, 0.318054, 0.288933);
                float4 k76 = index == 0 ? float4(0.451119, 0.694210, 0.318054, 0.303379) : index == 1 ? float4(0.611053, -0.172605, 0.245769, 0.303379) : index == 2 ? float4(-1.051488, -0.186878, 0.332511, 0.274486) : float4(-0.936905, 0.838821, 0.505995, 0.491186);
                float4 k86 = index == 0 ? float4(0.725530, 0.954221, 0.491538, 0.346719) : index == 1 ? float4(0.842183, 0.000730, 0.346968, 0.390059) : index == 2 ? float4(-0.979233, -0.157993, 0.375882, 0.332273) : float4(-0.648022, 1.084384, 0.303597, 0.346719);
                return WarmBodyInterpolate(age, k32, k48, k56, k64, k76, k86);
            }

            float4 WarmBodyPocketOpacity(float age)
            {
                return WarmBodyInterpolate(age,
                    float4(0.900000, 0.940000, 0.700000, 0.700000),
                    float4(0.850000, 0.950000, 0.750000, 0.350000),
                    float4(0.950000, 0.950000, 0.800000, 0.600000),
                    float4(0.050000, 0.900000, 0.800000, 0.700000),
                    float4(0.250000, 0.250000, 0.650000, 0.900000),
                    float4(0.850000, 0.700000, 0.650000, 0.650000));
            }

            float WarmBodyCream(float age)
            {
                return WarmBodyInterpolate(age,
                    float4(0.850000, 0.000000, 0.000000, 0.000000),
                    float4(0.800000, 0.000000, 0.000000, 0.000000),
                    float4(0.450000, 0.000000, 0.000000, 0.000000),
                    float4(0.200000, 0.000000, 0.000000, 0.000000),
                    float4(0.800000, 0.000000, 0.000000, 0.000000),
                    float4(0.650000, 0.000000, 0.000000, 0.000000)).x;
            }

            // WARM_BODY_EMISSION_32: compensate the measured assembled warm attenuation only.
            // Ratios are inferred from linearized output colors; native capture remains the test.
            // WARM_BODY_BLUE_33: only blue phase gains refined from valid parts32 capture.
            float3 WarmBodyEmission32(float age)
            {
                return WarmBodyInterpolate(age,
                    float4(1.00, 1.22, 1.90, 0.0),
                    float4(1.00, 1.30, 2.05, 0.0),
                    float4(1.00, 1.32, 1.90, 0.0),
                    float4(1.00, 1.50, 2.85, 0.0),
                    float4(1.00, 1.20, 2.20, 0.0),
                    float4(1.00, 1.12, 1.80, 0.0)).rgb;
            }

            half4 WarmBodyIslands(Varyings i)
            {
                float age = i.life.y;
                float fade = smoothstep(0.12, 0.283333, age) * (1.0 - smoothstep(2.76, 3.10, age));
                // TAIL_DURATION_65_LEFT: only lower-left field2 survives briefly to104.
                float leftTailFade = fade;
                if (age > 2.866667)
                    leftTailFade = (1.0 - smoothstep(2.76, 3.10, 2.866667))
                        * lerp(1.0, 0.80, smoothstep(2.866667, 3.116667, age))
                        * (1.0 - smoothstep(3.116667, 3.283333, age));
                float4 opacities = WarmBodyPocketOpacity(age);
                float fieldAlpha = 0.0;
                float rightAlpha = 0.0;
                float leftAlpha = 0.0; // WARM_RIGHT_62: preserve both unmeasured left fields.
                float tailFlareAlpha = 0.0;
                float tailFlareCore = 0.0;
                float tracedFlare = smoothstep(2.866667, 3.033333, age);
                float phase = age * 1.45;
                [unroll] for (int index = 0; index < 4; ++index)
                {
                    float4 key = WarmBodyPocketKey(age, index);
                    if (index == 2)
                    {
                        float tailMove = smoothstep(2.866667, 3.116667, age);
                        key.xy += float2(0.10, 0.33) * tailMove;
                        key.zw *= lerp(1.0, 0.75, tailMove);
                    }
                    float2 p = (i.q - key.xy) / max(key.zw, float2(0.05, 0.05));
                    // Independent unequal outlines, with small advected bulges rather than shared cross arms.
                    float theta = atan2(p.y, p.x);
                    float outline = 0.93 + 0.10 * sin(theta * 3.0 + index * 1.7 + phase * 0.25)
                        + 0.055 * sin(theta * 7.0 - phase * 0.45 + index * 2.3);
                    float f = outline - length(p);
                    // One edge-open curled bite on alternating sides of each pocket.
                    float hand = index == 0 || index == 3 ? 1.0 : -1.0;
                    float2 biteP = (p - float2(hand * 0.86, -0.28)) / float2(0.28, 0.30);
                    f = min(f, (length(biteP) - 1.0) * 0.28);
                    // The opt-in main material grades each warm pocket across several pixels.
                    if (i.shape.y < -0.5)
                        f = index < 2 ? MainWarmVectorField(p, index, age)
                            : MainWarmFoldField(p, index, age, phase);
                    [branch] if (index == 2 && age > 2.866667)
                    {
                        float flareDistance = TailYellowField67(i.q, age);
                        tailFlareCore = smoothstep(0.06, 0.18, flareDistance) * tracedFlare;
                        f = lerp(f, flareDistance / max(min(key.z, key.w), 0.05), tracedFlare);
                    }
                    float edge = max(fwidth(f), i.shape.y < -0.5 ? 0.045 : 0.065);
                    if (index == 2)
                        edge = max(edge, tracedFlare * 0.035 / max(min(key.z, key.w), 0.05));
                    float coverage = smoothstep(-edge, edge, f);
                    float bodyVariation = 0.92 + 0.08 * CelValue2(p * 2.6 + float2(index * 3.7, -phase * 0.4));
                    float pocketOpacity = opacities[index] * (index == 2 ? leftTailFade : fade);
                    if (index == 2) pocketOpacity = lerp(pocketOpacity, 0.98, tracedFlare);
                    float pocketAlpha = coverage * pocketOpacity * bodyVariation;
                    if (index == 2) tailFlareAlpha = pocketAlpha * tracedFlare;
                    fieldAlpha = max(fieldAlpha, pocketAlpha);
                    if (index < 2) rightAlpha = max(rightAlpha, pocketAlpha);
                    else leftAlpha = max(leftAlpha, pocketAlpha);
                }
                float alpha = saturate(fieldAlpha * i.tone.z);
                clip(alpha - 0.002);
                uint row = CelRowA(i.tone.x);
                float warmNoise = CelValue2(i.q * 3.1 + float2(phase * 0.3, -phase * 0.5));
                float cream = saturate(WarmBodyCream(age) + 0.20 * (warmNoise - 0.5));
                half3 col = lerp(CelPal(row, 1), CelPal(row, 3), (half)cream);
                col *= (half3)WarmBodyEmission32(age);
                float3 measuredRightResponse = WarmBodyInterpolate(age,
                    float4(1.0, 1.0, 0.900000, 0.0),
                    float4(1.0, 1.0, 1.0, 0.0),
                    float4(1.0, 1.0, 0.921053, 0.0),
                    float4(1.0, 1.0, 1.0, 0.0),
                    float4(1.0, 1.0, 1.0, 0.0),
                    float4(1.0, 1.080357, 1.138889, 0.0)).rgb;
                float rightWeight = rightAlpha / max(rightAlpha + leftAlpha, 0.00001);
                col *= (half3)lerp(float3(1.0, 1.0, 1.0), measuredRightResponse, rightWeight);
                // Ref98/100/102 yellow medians are252,249,146 /254,230,65 /255,218,64.
                // Use the existing yellow palette, never a white/Core gain.
                float3 flareGain = lerp(float3(1.14, 1.65, 3.70), float3(1.18, 1.38, 1.52),
                    smoothstep(3.033333, 3.116667, age));
                flareGain = lerp(flareGain, float3(1.45, 1.40, 1.35),
                    smoothstep(3.116667, 3.200000, age));
                half3 flareColor = CelPal(row, 1) * (half3)flareGain;
                // The brighter interior follows distance to the traced edge, not a separate circular hotspot.
                flareColor += half3(0.02, 0.04, 0.14) * (half)tailFlareCore;
                col = lerp(col, flareColor, (half)saturate(tailFlareAlpha / max(fieldAlpha, 0.00001)));
                // Preserve a warm ceiling rather than lifting all RGB into a clipped white field.
                col.b = min(col.b, (half)1.0);
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                col = MixFog(col, i.fog);
                return half4(col * (half)alpha, (half)alpha);
            }

            half4 frag(Varyings i) : SV_Target
            {
                clip(i.life.w - 0.5);
                if (i.shape.x > 2.5 && i.shape.x < 3.5) return WarmBodyIslands(i);
                if (i.shape.x < 1.5 && i.shape.y < -1.5) return MainRadiantFire(i);
                bool authoredHeart = i.shape.x > 0.5 && i.shape.x < 1.5 && i.shape.y < 0.0;
                bool authoredBody = i.shape.x < 0.5 && i.shape.y < 0.0;
                float phase = authoredHeart ? max(0.0, i.life.y - 0.20) * 1.6 + 2.13
                    : i.life.y * max(abs(i.more.x), 0.1) + i.life.z * 6.2831853;
                if (authoredBody) phase = i.life.y * 1.35 + 1.8;
                float mode = i.shape.x > 1.5 ? 0.0 : i.shape.x;
                float2 q = i.q;
                q.x -= q.y * i.shape.w;
                float field = authoredBody ? AuthoredBodyField(q, phase)
                    : FireBody(q, authoredHeart ? 0.5 : i.life.z, phase, authoredHeart ? 2.0 : mode);
                float edgeNoise = CelValue2(q * 7.0 + float2(i.life.z * 29.0, -phase * 0.9)) - 0.5;
                field += edgeNoise * (authoredHeart ? 0.0 : 0.030); // Heart fringe is applied before its open slits
                // Dissolution retreats an irregular boundary; it never selects the isotherms as rings.
                field -= max(i.tone.w - 0.35, 0.0) * 0.18;
                float aa = max(fwidth(field), i.more.y);
                // The first impacts diffuse across several pixels before the drawn fire tongues resolve.
                if (authoredHeart) aa = max(aa, lerp(0.065, 0.010, smoothstep(0.24, 0.48, i.life.y)));
                float coverage = smoothstep(-aa, aa, field);
                float fade = authoredHeart
                    ? smoothstep(0.0, 0.04, i.life.y) * (1.0 - smoothstep(3.02, 3.34, i.life.y))
                    : smoothstep(0.0, 0.045, i.life.x) * (1.0 - smoothstep(0.65, 1.0, i.life.x));
                if (authoredBody) fade = AuthoredBodyOpacity(i.life.y);
                float alpha = saturate(coverage * fade * i.tone.z);
                clip(alpha - 0.002);

                // Heat follows an inclined, travelling tongue. It is not a function of distance from either
                // the silhouette or a lobe centre, so its yellow islands cannot become nested circular bands.
                float n = CelValue2(q * 2.9 + float2(i.life.z * 23.0 + phase * 0.32, -phase * 0.85));
                float heat = 0.53 + 0.22 * sin(q.y * 3.2 - q.x * 2.8 + i.life.z * 8.0 + phase * 0.9)
                    + 0.32 * (n - 0.5) + q.y * 0.10 - q.x * 0.10;
                // UNTESTED: break BODY heat into multiple islands; Heart retains its existing field.
                if (mode < 0.5)
                {
                    float bodyNoise = CelValue2(q * 4.4 + float2(i.life.z * 23.0 + phase * 0.28, -phase * 0.72));
                    float smallNoise = CelValue2(q * 6.9 + float2(-phase * 0.32, i.life.z * 31.0));
                    heat = 0.50 + 0.11 * sin(q.y * 5.2 - q.x * 3.7 + i.life.z * 8.0 + phase * 0.8)
                        + 0.46 * (bodyNoise - 0.5) + 0.15 * (smallNoise - 0.5) + q.y * 0.04;
                }
                if (authoredBody) heat += 0.04 * smoothstep(1.616667, 2.116667, i.life.y);
                float contrast = i.shape.y > 0.0 ? i.shape.y : 1.0;
                heat = saturate((heat - 0.5) * contrast + 0.5);
                uint row = CelRowA(i.tone.x);
                half3 col;
                if (mode > 0.5)
                {
                    if (authoredHeart)
                    {
                        // Keep the pale pocket coverage stable. Only internal white heat rolls/peaks.
                        float heartNoise = CelValue2(q * 3.6 + float2(1.7, -phase * 0.24));
                        float heartHeat = 0.50 + 0.12 * sin(q.y * 4.7 + q.x * 3.0 + phase * 0.6)
                            + 0.22 * (heartNoise - 0.5);
                        heartHeat += q.y * 0.20 * smoothstep(2.316667, 2.733333, i.life.y);
                        float peakHeat = lerp(0.35, 0.90, smoothstep(2.316667, 2.733333, i.life.y));
                        // Separate impacts at frames22 and26, with the reference's quieter frame24 between them.
                        float pulse22 = exp(-pow((i.life.y - 0.066667) / 0.028, 2.0));
                        float pulse26 = exp(-pow((i.life.y - 0.233333) / 0.030, 2.0));
                        peakHeat = max(peakHeat, 0.85 * pulse22 + 0.95 * pulse26);
                        heartHeat += 0.04 * pulse22 + 0.10 * pulse26;
                        // Opening heat sits below the upper pale tongue, matching the impact centroid.
                        heartHeat -= max(q.y, 0.0) * 0.45 * pulse22;
                        float hot = smoothstep(0.47, 0.64, heartHeat) * peakHeat;
                        half3 pale = lerp(CelPal(row, 1), CelPal(row, 3), (half)smoothstep(0.0, 0.025, field));
                        col = lerp(pale, CelPalCore(row).rgb * (half)max(0.0, i.tone.y), (half)hot);
                    }
                    else
                    {
                        col = lerp(CelPal(row, 1), CelPal(row, 3), (half)CelAAStep(0.43, heat));
                        col = lerp(col, CelPalCore(row).rgb * (half)max(0.0, i.tone.y), (half)CelAAStep(0.64, heat));
                    }
                }
                else
                {
                    col = lerp(CelPal(row, 1), CelPal(row, 2), (half)CelAAStep(0.52, heat));
                    // Main fire has broad warm pale islands inside its yellow band. Site fire retains its measured ramp.
                    float paleAt = i.shape.x < 0.5 ? 0.56 : 0.62;
                    col = lerp(col, CelPal(row, 3), (half)CelAAStep(paleAt, heat));
                }
                float band = i.shape.z > 0.0 ? i.shape.z : 0.10;
                float inside = smoothstep(0.0, band, field);
                // A warm broad outer band replaces black ink. A few inner brown-orange cuts follow heat.
                float warmCut = (1.0 - CelAAStep(0.30, heat)) * 0.55;
                if (mode < 0.5)
                    col = lerp(CelPal(row, 0), col, (half)(inside * (1.0 - warmCut)));
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                col = MixFog(col, i.fog);
                return half4(col * (half)alpha, (half)alpha);
            }
            ENDHLSL
        }
    }
}
