// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - FlameJet: the flamethrower's jet as one
// camera-facing ribbon from the nozzle to the target, widening toward the target, its field made of flowing noise
// tongues and banded into flat cel values (red rim, orange body, yellow, a white HDR core) with an ink edge. Opaque
// cutout (no overdraw, no sort). The front grows out of the nozzle, holds while the record lives, and in its last
// seconds the tail leaves the nozzle and runs into the target (the jet is cut off).
// One record per jet (CelFx.hlsl). The far end is pos + vel x life (spawn it with Aim Target, Speed 0, Arrive 1,
// Spread 0, and no gravity, drag or buoyancy).
//   size       x width at the nozzle (m), y width at the target (m), z width ease power along, w reach past the
//              target (share of the length)
//   erode      x seconds the front takes to reach the target, y seconds the cut-off takes at the end of the life,
//              z tongue amplitude (0: 0.45), w the share of the length the front starts at (it is already out)
//   tone       x palette, y HDR core, z unused, w ink px
//   extra      x flow speed (m/s, the tongues run toward the target), y tongue frequency (1/m; 0: 0.9), z core
//              streak amount (0..1), w upper-edge licking (0..1: the side facing up is wider and more ragged, and its
//              tongues run up across the jet; 0 for upright tongues)
// The ribbon's side is turned so +across faces up. One kind draws the jet and the tongues round the target (the same
// field on short upright records: wide base, pointed tip, the base lifting off at the end of the life).
Shader "TW/Cel/FlameJet"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+20" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        #define JET_ENV 2.5
        struct JetOut { float3 positionWS; float along; float across; float metres; float alive; };

        // the reach of the jet at age t, as shares of the nozzle-target length: x the tail, y the front
        float2 JetSpan(CelRecord r, float t)
        {
            float grow = max(r.erode.x, 1e-3), cut = max(r.erode.y, 1e-3);
            float reach = 1.0 + r.size.w;
            float front = lerp(saturate(r.erode.w), reach, saturate(t / grow));
            float tail = saturate((t - (r.life - cut)) / cut);
            tail = tail * tail * reach;
            return float2(tail, front);
        }

        float JetHalfWidth(CelRecord r, float a)
        {
            // the main jet (upper-edge licking on) starts slimmer and broadens over its first 1.3 s, like the clip's
            bool isMain = r.extra.w > 0.0;
            float grow = isMain ? lerp(0.55, 1.0, smoothstep(0.0, 1.3, CelAge(r))) : 1.0;
            float w = 0.5 * grow * lerp(r.size.x, r.size.y, pow(saturate(a), r.size.z > 0.0 ? r.size.z : 1.0));
            if (isMain)
            {
                // r05: a pointed nozzle start (the body grows out of a point over the first 14 % of the length), a flare
                // through the target so the flame spills past and over it, and wider going out (Mid/Far: the cone must
                // still read as a cone at T1, not a line; detail does not rise, only the size)
                w *= lerp(0.08, 1.0, smoothstep(0.0, 0.14, a));
                w *= 1.0 + 0.1 * smoothstep(0.65, 1.2, a);   // b01: 0.3 -> 0.1 (the beach shot: past the dummy the flare rose to the frame top)
                w *= 1.0 + 2.2 * saturate(_CelLod.x);
            }
            return w;
        }

        // The field's coordinates read from the fragment's own world place (not interpolated off the strip, whose
        // quads twist and stair-step the edge): x along (share of the length), y across (half-widths), z metres along
        float3 JetCoords(CelRecord r, float3 p)
        {
            float3 span = r.vel * r.life;
            float len = max(length(span), 1e-3);
            float3 dir = span / len;
            float3 d = p - r.pos;
            float m = dot(d, dir);
            float3 side = cross(dir, normalize(_WorldSpaceCameraPos - p));
            float sl = dot(side, side);
            side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
            if (side.y < 0.0) side = -side;
            float along = m / len;
            float hw = max(JetHalfWidth(r, saturate(along)), 1e-3);
            return float3(along, dot(d - dir * m, side) / hw, m);
        }

        JetOut Jet(float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), alive = CelAlive(r);
            float3 span = r.vel * r.life;
            float len = max(length(span), 1e-3);
            float3 dir = span / len;
            float2 tf = JetSpan(r, t);
            float a = lerp(tf.x, tf.y, uv.x);
            float3 c = r.pos + dir * (a * len);
            float3 toEye = normalize(_WorldSpaceCameraPos - c);
            float3 side = cross(dir, toEye);
            float sl = dot(side, side);
            side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
            if (side.y < 0.0) side = -side;
            // the envelope is wider than the body so the tongues have room; the round front cap reaches past the end
            float hw = JetHalfWidth(r, a) * JET_ENV * alive;
            float cap = (uv.x > 0.999) ? hw * 1.3 : 0.0;
            JetOut o;
            o.positionWS = c + side * (uv.y * 2.0 - 1.0) * hw + dir * cap;
            o.along = a + cap / len;
            o.across = (uv.y * 2.0 - 1.0) * JET_ENV;
            o.metres = (a * len) + cap;
            o.alive = alive;
            return o;
        }

        // r06: the main jet is a union of separate flame tongues (the clip's jet is 10-15 overlapping tongues, not one
        // slab with a noisy rim). Each tongue is born on a cell of the advected flow coordinate s (metres), runs 2-4
        // cells downstream (4-8x longer than it is tall), has a rounded base and a pointed tip, and the outer ones curl
        // away from the axis as they go (the upper ones up). Two lanes (low, high) overlap so the body stays
        // connected and both edges are lobed; a thin spine keeps the white core. Past the target the lanes lift and
        // spread, so the flame spills up and on past the dummy. 1 on a tongue's middle line, 0 on its edge.
        static float g_jetSeam;     // b01: 0..1, a dark seam between two overlapping tongues (set by JetTongues)
        float JetTongues(CelRecord r, float s, float y, float a01, float freq, float sd)
        {
            float P = 1.0 / max(freq, 0.05);
            float cell = floor(s / P);
            float past = smoothstep(0.8, 1.25, a01);
            y += 0.1 * past;                                    // b01: past the target the field drops (it rose off the frame top)
            // b01: a slimmer spine (the white core was a third too big: hot 0.154 vs the clip's 0.101)
            float best = (0.3 - abs(y)) / 0.3;                  // the spine (the white core)
            // b01: the two best tongues are kept apart: where two tongues overlap and neither is clearly in front, a dark
            // ink-brown seam is cut (the clip's jet reads as separate tongues with dark gaps between them)
            float t1 = -1.0, t2 = -1.0;
            // r06b: two lanes (low, high), one tongue per cell each, overlapping ~1.6 deep, heights 0.6-1.3x: a union
            // that stays scalloped (r06's three lanes ~3 deep merged into one smooth slab)
            [unroll] for (int k = -4; k <= 0; k++)
            {
                float ci = cell + k;
                [unroll] for (int lane = 0; lane < 2; lane++)
                {
                    float hb = ci * 2.0 + lane + sd * 13.0;
                    float h1 = CelHash11(hb), h2 = CelHash11(hb + 17.3), h3 = CelHash11(hb + 41.9), h4 = CelHash11(hb + 77.1);
                    float s0 = (ci + h1 * 0.7) * P;
                    float L = P * (2.4 + 1.6 * h2);            // r06: long (the jet runs into depth, so on screen it is foreshortened)
                    float u = (s - s0) / L;
                    if (u <= 0.0 || u >= 1.0) continue;
                    // b01: the lanes scatter wider (the low edge was one straight line: each low tongue now sits at its own height)
                    float c = (lane == 0 ? -0.34 : 0.34) + (h3 - 0.5) * 0.5;
                    float H = (lane == 0 ? 0.55 : 0.5) * (0.5 + 0.8 * h4);   // b01: the high lane no taller than the low one, no growth past the dummy
                    // curl: the low tongues bend down a little, the high ones lick up (more past the target)
                    float lean = (lane == 0 ? -0.2 : 0.12);
                    float cy = c + lean * u * u;
                    float prof = H * 2.6 * sqrt(u) * (1.0 - u);                // a round base, a pointed tip leaning on
                    float f = (1.0 - abs(y - cy) / max(prof, 1e-3)) * 0.9;
                    if (f > t1) { t2 = t1; t1 = f; } else t2 = max(t2, f);
                }
            }
            // the seam: both tongues cover the point and neither leads by much; not on the spine's white core
            g_jetSeam = (t2 > 0.04 && best < 0.25) ? saturate(1.0 - (t1 - t2) / 0.14) : 0.0;
            return max(best, t1);
        }

        // r06: where the cone meets the ground at the target it was cut in one straight line (ZTest); the body now
        // ends above the ground in a row of lobes, so no straight edge shows. The ground is 1.4 m under the aim point.
        float JetGround(CelRecord r, float3 p, float metres)
        {
            float gy = r.pos.y + r.vel.y * r.life - 1.58;      // b01: the beach dummy (x1.31): the foot is 1.58 m under the aim
            float g = CelSimplex3(float3((metres - CelAge(r) * r.extra.x) * 0.7, CelAge(r) * 0.6, r.seed * 9.0));
            return (p.y - gy - 0.25 - 0.45 * abs(g)) / 0.35;
        }

        /// The jet's field at a point: 1 in the middle of the core, 0 on the edge (below 0 outside).
        float JetField(CelRecord r, float along, float across, float metres, out float streak)
        {
            float t = CelAge(r);
            float2 tf = JetSpan(r, t);
            float len = max(length(r.vel * r.life), 1e-3);
            float hw = max(JetHalfWidth(r, saturate(along)), 1e-3);
            float flow = r.extra.x;
            // r05: tongues scale with the LOD widening (Mid/Far), so detail does not rise going out (no comb at T1)
            float freq = (r.extra.y > 0.0 ? r.extra.y : 0.9) / (r.extra.w > 0.0 ? 1.0 + 2.2 * saturate(_CelLod.x) : 1.0);
            float amp = r.erode.z > 0.0 ? r.erode.z : 0.45;
            // the noise runs with the flow (s: metres, advected); the jet is one connected body whose edge is a row of
            // flame tongues: for each (sheared) place along the flow the body reaches out to edge(s), so every tongue
            // is attached and leans downstream as it rises (no islands, no spots, and the edge never meets the strip's)
            float s = metres - t * flow;
            float up = r.extra.w;
            float sd = r.seed * 17.0;
            float a01 = saturate(along);
            // a slow sway of the whole jet across its axis, stretched far along the flow
            float sway = CelSimplex3(float3(s * freq * 0.22, t * 0.5, sd)) * 0.22 * a01;
            float y = across - sway;
            bool upper = y > 0.0;
            float ay = abs(y);
            // r04: both edges are a row of long, low tongues leaning downstream (each about 4-8x longer along the flow
            // than it is tall): a strong shear tips them forward, and two overlapping rows (offset phases) make the edge
            // ragged without needles. The upper edge is only a little taller than the lower one.
            // r05: a much stronger shear (the jet runs away from the eye, so its length is foreshortened on screen and
            // r04's 1.8 still read as near-vertical spikes): each tongue now runs 4-8x longer along the flow than tall
            float shear = ay * hw * (upper ? 2.4 : 1.8);
            float sl = s - shear;
            float g = CelSimplex3(float3(sl * freq, t * 0.7, sd + (upper ? 3.1 : 7.7)));
            float tongue = 1.0 - abs(g); tongue *= sqrt(tongue);                    // crests between pointed and round
            float g2 = CelSimplex3(float3(sl * freq * 1.9 + 4.3, t * 1.1, sd + (upper ? 1.7 : 5.3)));
            float tongue2 = 1.0 - abs(g2); tongue2 *= sqrt(tongue2);
            float rows = max(tongue, tongue2 * 0.8);
            float lick = (upper ? amp * (1.0 + 0.25 * up) : amp) * lerp(0.7, 1.0, a01);
            // r05: the body sits above the axis on the main jet (upper edge base 0.85, lower 0.55), so the lower edge
            // stays off the ground at the target (the ground cut it in a straight line) and the top spills over it
            float base = up > 0.0 ? (upper ? 0.85 : 0.55) : 0.72;
            float edge = min(base + lick * rows * (upper ? 1.0 : 0.8), JET_ENV * 0.95);
            float field = (edge - ay) / edge;
            if (up > 0.0) field = JetTongues(r, s, y, a01, freq, sd);
            // the front end spills past the target ragged (tongues reach on, no blunt cap); the cut-off tail is round
            float toFront = (tf.y - along) * len / (hw * 1.4);
            float toTail = (along - tf.x) * len / (hw * 0.9);
            // r06: the main jet's front is a wavy row of tongue tips (r05's cap cut it on one hard diagonal)
            if (up > 0.0) field = min(field, toFront * 0.6 + 0.45 * CelSimplex3(float3(y * 1.4, t * 1.6, sd + 9.0)) + 0.15);
            else field = min(field, toFront + (rows - 0.6) * 0.6);
            if (tf.x > 0.0) field = min(field, toTail + g * 0.2);
            // the nozzle end tapers to a point (no square cut where the strip starts near the camera)
            else field = min(field, metres / (hw * 1.2) - 0.05);
            // the bands follow overlapping tongues inside the body too (chevrons sheared like the edge), plus long
            // streaks along the flow for the white core
            float3 q2 = float3(s * freq * 0.3, y * hw * freq * 2.2, t * 1.3 + r.seed * 7.0);
            float inner = CelSimplex3(float3((s - ay * hw * 2.3) * freq * 1.3, ay * 1.2, t * 0.9 + sd + 2.9));
            streak = CelSimplex3(q2) * r.extra.z * 0.7 + inner * 0.45;
            return field;
        }
        ENDHLSL

        Pass
        {
            Name "CelFlameJet"
            Tags { "LightMode"="UniversalForward" }
            Cull Off ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 jet : TEXCOORD1;          // x along (share of the length), y across (-1.45..1.45), z metres along
                float fog : TEXCOORD2;
                nointerpolation uint id : TEXCOORD3;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                JetOut j = Jet(v.uv, instanceID);
                Varyings o;
                o.positionWS = j.positionWS;
                o.positionCS = TransformWorldToHClip(j.positionWS);
                o.jet = float3(j.along, j.across, j.metres);
                o.fog = ComputeFogFactor(o.positionCS.z);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float streak;
                g_jetSeam = 0.0;
                float3 jc = JetCoords(r, i.positionWS);
                float f = JetField(r, jc.x, jc.y, jc.z, streak);
                if (r.extra.w > 0.0) f = min(f, JetGround(r, i.positionWS, jc.z) + 8.0 * (1.0 - smoothstep(0.45, 0.7, jc.x)));
                clip(f);
                float seam = g_jetSeam * smoothstep(0.08, 0.2, jc.x);   // b01: no seams in the nozzle's first metres
                // the value the bands are cut from: the field, lifted along the white streaks, a little hotter at the nozzle
                // a thin red rim, then orange and yellow, and the white core down the middle; cooler toward the target
                // (red only on a thin rim: v passes the first band at f ~0.04)
                // r04: the white core is about twice as wide (it shows over more of the field)
                float v = saturate(0.1 + f * 0.8 + streak * 0.3 + 0.06 * (1.0 - saturate(jc.x * 1.5)) - 0.08 * saturate((jc.x - 0.6) * 1.5));
                half3 col = CelRamp(v, CelRowA(r.tone.x), r.tone.y);
                float ink = CelInk(f, 0.0, CelLodInk(r.tone.w));
                col = lerp(col, CelPalInk(CelRowA(r.tone.x)).rgb, max(ink, seam * 0.9));
                return half4(MixFog(col, i.fog), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Off
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct DepthVaryings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; nointerpolation uint id : TEXCOORD1; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                JetOut j = Jet(v.uv, instanceID);
                DepthVaryings o;
                o.positionCS = TransformWorldToHClip(j.positionWS);
                o.positionWS = j.positionWS;
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float streak;
                g_jetSeam = 0.0;
                float3 jc = JetCoords(r, i.positionWS);
                float f = JetField(r, jc.x, jc.y, jc.z, streak);
                if (r.extra.w > 0.0) f = min(f, JetGround(r, i.positionWS, jc.z) + 8.0 * (1.0 - smoothstep(0.45, 0.7, jc.x)));
                clip(f);
                return 0;
            }
            ENDHLSL
        }
    }
}
