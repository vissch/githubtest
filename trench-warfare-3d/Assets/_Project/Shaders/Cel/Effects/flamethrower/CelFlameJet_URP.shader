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
            float g01 = saturate(t / grow);
            // b05: the main jet's front eases in (pow 1.6): its near half is foreshortened less on screen, so a linear front
            // reached the dummy on screen by the clip's f17 (half length there)
            if (r.extra.w > 0.8) g01 = pow(g01, 1.6);
            float front = lerp(saturate(r.erode.w), reach, g01);
            float tail = saturate((t - (r.life - cut)) / cut);
            tail = tail * tail * reach;
            return float2(tail, front);
        }

        float JetHalfWidth(CelRecord r, float a)
        {
            // the main jet (upper-edge licking on) starts slimmer and broadens over its first 1.3 s, like the clip's
            bool isMain = r.extra.w > 0.0;
            // b04: broadens over 0.75 of its life (to 2.6 s), so the main jet keeps growing to the clip's peak (f84) and short tongues open fast
            float grow = isMain ? lerp(0.5, 1.08, smoothstep(0.0, clamp(0.85 * r.life, 0.3, 3.4), CelAge(r))) : 1.0;
            float w = 0.5 * grow * lerp(r.size.x, r.size.y, pow(saturate(a), r.size.z > 0.0 ? r.size.z : 1.0));
            if (isMain)
            {
                // r05: a pointed nozzle start (the body grows out of a point over the first 14 % of the length), a flare
                // through the target so the flame spills past and over it, and wider going out (Mid/Far: the cone must
                // still read as a cone at T1, not a line; detail does not rise, only the size)
                // b03: the nozzle flash shows from the frame edge (the 14 % point taper started the jet 10 % in)
                w *= lerp(0.2, 1.0, smoothstep(0.0, 0.08, a));
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
            // Once disconnected, the back of the stream narrows to a point rather
            // than exposing the first strip segment as a flat cross-section.
            if (tf.x > 0.0) hw *= smoothstep(0.0, max(min(0.12, tf.y - tf.x), 1e-4), a - tf.x);
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
            float best = (0.3 - abs(y)) / 0.3;   // b05: 0.22 -> 0.3 (a wider white core: hot 0.022 vs the clip's 0.101)                // the spine (the white core); b02: 0.3 -> 0.22 (hot 0.20 vs 0.10)
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
                    float H = (lane == 0 ? 0.55 : 0.5 * (1.0 - 0.6 * smoothstep(0.6, 1.0, a01))) * (0.5 + 0.8 * h4);   // b02b: the high lane flattens onto the dummy (a flap rose over it)   // b01: the high lane no taller than the low one, no growth past the dummy
                    // curl: the low tongues bend down a little, the high ones lick up (more past the target)
                    float lean = (lane == 0 ? -0.2 : 0.0);
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
            // b02b: the main jet ends at the dummy (its round front cap spilled up past the target to the frame top)
            if (up > 0.0) field = min(field, (1.0 - along) * len / (hw * 0.8) + 0.25 * CelSimplex3(float3(y * 2.0, t * 1.4, sd + 4.0)));
            if (tf.x > 0.0) field = min(field, toTail + g * 0.2);
            // the nozzle end tapers to a point (no square cut where the strip starts near the camera)
            else field = min(field, metres / (hw * 1.2) - 0.05);
            // b03: ragged, broken tongue edges: fine flowing noise bites notches and gaps out of the outer band only
            // (the critic read r06-b02's edge as one smooth folded fan)
            if (up > 0.0)
            {
                float rag = CelSimplex3(float3(sl * freq * 3.1, y * 3.5, t * 2.3 + sd + 11.0));
                field -= 0.3 * saturate(rag) * (1.0 - smoothstep(0.08, 0.4, field)) * smoothstep(0.06, 0.2, a01);
            }
            // the bands follow overlapping tongues inside the body too (chevrons sheared like the edge), plus long
            // streaks along the flow for the white core
            float3 q2 = float3(s * freq * 0.3, y * hw * freq * 2.2, t * 1.3 + r.seed * 7.0);
            float inner = CelSimplex3(float3((s - ay * hw * 2.3) * freq * 1.3, ay * 1.2, t * 0.9 + sd + 2.9));
            streak = CelSimplex3(q2) * r.extra.z * 0.7 + inner * 0.45;
            return field;
        }

        // d01: PATH RIBBON tongues (records with extra.w < 0). One tongue = a camera-facing strip whose spine follows a
        // curved path from the nozzle toward pos + vel x life: the head runs out along the path (erode.x seconds to its
        // end), the tail follows size.y metres behind and, after the head arrives, runs into the end too. The spine is
        // bent by flowing noise (amplitude size.z metres at the end, frequency extra.y /m, flow extra.x m/s), lifted
        // motion.w metres up at the end (a^2) and its tip curls size.w metres up. Width: size.x at the end, 12 % of it at
        // the nozzle, a round base and a pointed tip leading downstream. Bands from the strip's own cross distance, so
        // the ink line is a real contour on every tongue (overlapping tongues keep their own outlines).
        #define RIB_ENV 1.3
        float2 RibWindow(CelRecord r, float t)
        {
            float len = max(length(r.vel * r.life), 1e-3);
            float hT = clamp(r.erode.x, 0.05, r.life * 0.95);
            float headA = pow(saturate(t / hT), 0.85);
            float tailA = saturate(headA - r.size.y / len);
            float rest = saturate((t - hT) / max(r.life - hT, 1e-3));
            tailA = lerp(tailA, 1.0, rest * rest);
            return float2(tailA, headA);
        }
        // a flame lens: a tapering tail toward the nozzle, widest at ~40 %, a long pointed tip downstream (d01 try4's
        // sqrt base was cut square)
        float RibProfile(float u) { u = saturate(u); return saturate(smoothstep(0.0, 0.4, u) * pow(1.0 - u, 0.5) * 1.5); }
        float3 RibPoint(CelRecord r, float a, float u, float t)
        {
            float3 span = r.vel * r.life;
            float len = max(length(span), 1e-3);
            float3 dir = span / len;
            float3 up = float3(0.0, 1.0, 0.0);
            float3 side = cross(dir, up); side *= rsqrt(max(dot(side, side), 1e-6));
            float3 up2 = cross(side, dir);
            float m = a * len, sd = r.seed * 31.0;
            float fs = (m - t * r.extra.x) * r.extra.y;
            float n1 = CelSimplex3(float3(fs, sd, t * 0.9));
            float n2 = CelSimplex3(float3(fs * 0.8 + 5.1, sd + 3.3, t * 0.7));
            float amp = r.size.z * a;
            float3 p = r.pos + dir * m + side * (n1 * amp) + up2 * (n2 * amp * 0.7);
            p += up * (r.motion.w * a * a);
            float tip = smoothstep(0.55, 1.0, u);
            p += up2 * (r.size.w * tip * tip);
            return p;
        }
        struct RibOut { float3 positionWS; float u; float across; float a; };
        RibOut Rib(float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), alive = CelAlive(r);
            float2 w = RibWindow(r, t);
            float a = lerp(w.x, w.y, uv.x);
            float da = max((w.y - w.x) * 0.02, 1e-3);
            float3 c = RibPoint(r, a, uv.x, t);
            float3 c2 = RibPoint(r, a + da, min(uv.x + 0.02, 1.0), t);
            float3 tang = c2 - c; tang *= rsqrt(max(dot(tang, tang), 1e-8));
            // d40: one side vector per tongue (its straight span x the view to its middle), not per vertex: where the
            // curved spine ran along the view ray the per-vertex side spun round and folded the strip into fans of
            // slivers (the thin straight grey "seam lines"); the strip is now a flat, untwisted band facing the eye
            float3 span = r.vel * r.life;
            float3 mid = r.pos + span * 0.5;
            float3 side = cross(span, normalize(_WorldSpaceCameraPos - mid));
            float sl = dot(side, side);
            side = sl > 1e-8 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
            side = normalize(side - tang * dot(side, tang) * 0.0);
            // no per-vertex up-flip: on a curved spine it flipped the strip between vertices (plank facets)
            float hw = 0.5 * r.size.x * lerp(0.3, 1.0, sqrt(saturate(a))) * (1.0 + 1.2 * saturate(_CelLod.x));
            // a tongue shrinking into the target thins with its visible length (no wide stub at the end)
            hw *= saturate((w.y - w.x) * length(r.vel * r.life) / max(r.size.y, 0.1));
            // d37: the build-up: tongues born later in the burn are wider (0.7x at 0.6 s -> 1.15x by 3.0 s), so the jet
            // keeps growing to REF's peak (~f84) instead of peaking 22 frames early
            hw *= lerp(0.7, 1.15, smoothstep(0.6, 3.0, r.born - r.tone.z));
            float prof = max(RibProfile(uv.x), 0.0001);
            RibOut o;
            o.positionWS = c + side * (uv.y * 2.0 - 1.0) * hw * prof * RIB_ENV * alive;
            o.u = uv.x; o.across = (uv.y * 2.0 - 1.0) * RIB_ENV; o.a = a;
            return o;
        }
        // 1 on the tongue's mid line, 0 on its edge; ragged by flowing noise, the upper edge licking more
        float RibField(CelRecord r, float u, float across, float a, out float streak)
        {
            float t = CelAge(r), sd = r.seed * 17.0;
            float rag = CelSimplex3(float3(u * 5.0 - t * 3.0, across > 0.0 ? 1.3 : 4.7, sd));
            float edge = 1.0 + 0.22 * rag * (across > 0.0 ? 1.0 : 0.6);
            float field = (edge - abs(across)) / edge;
            streak = CelSimplex3(float3(u * 3.0 - t * 2.0, across * 1.5, sd + 2.0)) * r.extra.z;
            return field;
        }
        // Every overlapping tongue samples one flowing field. Individual ribbon borders must
        // not paint stripes through the body of a continuous flame.
        float3 FlameHeat(CelRecord r, float3 p)
        {
            float3 ab = r.axis - r.motion.xyz;
            float len = max(length(ab), 1e-3);
            float3 dir = ab / len;
            float3 mid = (r.motion.xyz + r.axis) * 0.5;
            float3 view = normalize(_WorldSpaceCameraPos - mid);
            float3 side = cross(dir, view);
            float sideLength = dot(side, side);
            side = sideLength > 1e-6 ? side * rsqrt(sideLength) : UNITY_MATRIX_V[0].xyz;
            if (side.y < 0.0) side = -side;
            // Project all overlapping records onto one plane before sampling color.
            float3 normal = cross(dir, side);
            float3 ray = p - _WorldSpaceCameraPos;
            float denominator = dot(ray, normal);
            float3 projected = abs(denominator) > 1e-5
                ? _WorldSpaceCameraPos + ray * (dot(mid - _WorldSpaceCameraPos, normal) / denominator)
                : p;
            float3 rel = projected - r.motion.xyz;
            float m = dot(rel, dir);
            float s = saturate(m / len);
            float y = dot(rel, side);
            float t = _CelNow - r.tone.z;
            float radius = lerp(0.3, 2.2, pow(s, 0.8));
            float2 q = float2(m * 0.65 - t * 4.0, y * 1.4);
            float curl = CelSimplex3(float3(q * 0.65, 9.4));
            q += float2(curl * 1.1, sin(q.x * 1.2 + curl * 2.2) * 0.65);
            float n = CelSimplex3(float3(q * float2(0.68, 1.0), 3.7));
            float fine = CelSimplex3(float3(q * float2(1.4, 1.7), 11.2));
            float heat = 1.0 - abs(y) / radius;
            float islands = n * 0.92 + fine * 0.08;
            float core = saturate(1.0 - abs(y + 0.3 * sin(m * 1.6 - t * 5.0)) / (radius * 0.52));
            core *= 0.55 + 0.45 * smoothstep(-0.4, 0.4, n);
            return float3(heat, islands, core);
        }

        half3 FlameBody(CelRecord r, float3 heat)
        {
            float value = saturate(0.38 + heat.x * 0.16 + heat.y * 0.46 + heat.z * 0.32);
            return CelRamp(value, CelRowA(r.tone.x), r.tone.y);
        }
        // d47 (NEW PRIMITIVE, fracture 9): FIREBALL RING - one camera-facing disc per record (extra.w <= -1.5) drawn on the
        // same strip mesh as a quad. Polar field round its centre: a ring of flame tongues whose angle swirls with the
        // radius (they curl round the middle), licking up more on top; late in the life a dark smoke hole opens inside
        // (REF f101-f111: a round ring of curling inked flame wrapping the dummy, dark smoke in its middle). Bands from the
        // distance into the ring, cooling into the smoke row (tone.x packed) from 70 % of the life.
        //   size  x radius at birth, y at death, z ease; erode x = the hole's final share of the radius
        //   extra x swirl (rad per radius), y tongue count, z spin (rad/s)
        #define BALL_Q 1.45
        float3 BallCentre(CelRecord r)
        {
            float age = max(CelAge(r), 0.0);
            // The opening front stores its emitter frame in motion.xyz; its
            // authored travel is constant velocity. Ordinary rings use physics.
            return r.extra.w < -2.5 ? r.pos + r.vel * age : CelPosition(r, age);
        }
        float BallR(CelRecord r)
        {
            float k = CelLife01(r);
            return CelCurve(r.size.xyz, k) * lerp(1.0, 0.15, smoothstep(0.55, 1.0, k));
        }
        // x the fire field (>0 inside the flame), y the hole field (>0 inside the smoke hole), z polar noise for the bands
        float3 BallField(CelRecord r, float2 q)
        {
            float t = CelAge(r), k = CelLife01(r), sd = r.seed * 23.0;
            // k01: erode.z (the layer's Erode.Pow, 1 on every ring before k01) squashes the ball vertically about a point
            // low in the disc, so a catch can be a wide low billow sitting on the ground (REF f30-f36) instead of a tall drop
            float squash = max(r.erode.z, 1.0);
            q.y = (q.y + 0.55 * (squash - 1.0)) * squash;
            q.x += 0.13 * sin(q.y * 3.0 + t * 2.0 + sd);
            q.y -= 0.11 * sin(q.x * 3.0 - t * 1.6 + sd);
            float rho = length(q);
            float th = atan2(q.y, q.x) + r.extra.x * rho * 2.1 - t * r.extra.z;
            float n1 = CelSimplex3(float3(cos(th) * 1.35, sin(th) * 1.35, t * 0.65 + sd));
            float n2 = CelSimplex3(float3(q * 2.0 + float2(sin(th), cos(th)), t * 0.8 + sd));
            float up = saturate(q.y / max(rho, 1e-3));
            float outer = (0.83 + 0.27 * n1 + 0.12 * n2 + 0.17 * up) * (1.0 - 0.28 * smoothstep(0.78, 1.0, k));
            float hole = r.erode.x * smoothstep(0.32, 0.9, k) * (0.9 + 0.3 * n2);
            float holeDistance = length(q - float2(0.13 * sin(sd), 0.12));
            float fo = (outer - rho) / max(outer, 1e-3);
            float fi = hole > 0.01 ? (holeDistance - hole) / max(outer - hole, 0.05) : 1.0;
            return float3(min(fo, fi), -fi, n1 * 0.5 + n2 * 0.5);
        }
        void FlameGeometry(float2 uv, uint instanceID, out float3 world, out float4 screen, out float3 coords)
        {
            CelRecord r = CelFetch(instanceID);
            if (r.extra.w < -1.5)
            {
                float2 q = (uv * 2.0 - 1.0) * BALL_Q;
                world = CelBillboard(BallCentre(r), q * BallR(r) * CelAlive(r));
                screen = TransformWorldToHClip(world);
                coords = float3(q, 0.0);
            }
            else if (r.extra.w < 0.0)
            {
                RibOut rb = Rib(uv, instanceID);
                world = rb.positionWS;
                screen = TransformWorldToHClip(world);
                coords = float3(rb.u, rb.across, rb.a);
            }
            else
            {
                JetOut j = Jet(uv, instanceID);
                world = j.positionWS;
                screen = TransformWorldToHClip(world);
                coords = float3(j.along, j.across, j.metres);
            }
            if (r.extra.w >= -1.5 || r.extra.w < -2.5)
            {
                // The continuous stream shares one depth plane as well as one
                // colour field. Per-ribbon depth offsets made the scene ink pass
                // outline internal triangles through otherwise identical flame.
                float3 axis = r.axis - r.motion.xyz;
                float3 mid = (r.motion.xyz + r.axis) * 0.5;
                float3 view = normalize(_WorldSpaceCameraPos - mid);
                float3 side = cross(axis, view);
                side = dot(side, side) > 1e-6 ? normalize(side) : UNITY_MATRIX_V[0].xyz;
                float3 normal = cross(axis, side);
                float3 ray = world - _WorldSpaceCameraPos;
                float den = dot(ray, normal);
                float3 planePoint = abs(den) > 1e-5
                    ? _WorldSpaceCameraPos + ray * (dot(mid - _WorldSpaceCameraPos, normal) / den)
                    : world;
                float4 planeCS = TransformWorldToHClip(planePoint);
                screen.z = planeCS.z / planeCS.w * screen.w;
            }
        }

        float FlameCoverage(CelRecord r, float3 world, float3 coords)
        {
            float streak;
            if (r.extra.w < -1.5)
            {
                float3 ball = BallField(r, coords.xy);
                return r.extra.w < -2.5 ? ball.x : max(ball.x, ball.y);
            }
            if (r.extra.w < 0.0)
            {
                float field = RibField(r, coords.x, coords.y, coords.z, streak);
                float3 heat = FlameHeat(r, world);
                return min(field, (heat.x + 1.35 * heat.y - 0.02) * 1.6);
            }
            g_jetSeam = 0.0;
            float3 jc = JetCoords(r, world);
            float field = JetField(r, jc.x, jc.y, jc.z, streak);
            if (r.extra.w > 0.0) field = min(field, JetGround(r, world, jc.z) + 8.0 * (1.0 - smoothstep(0.45, 0.7, jc.x)));
            field -= g_jetSeam * smoothstep(0.08, 0.2, jc.x) * 0.45 * (1.0 - smoothstep(0.25, 0.55, field));
            return field;
        }
        ENDHLSL

        Pass
        {
            Name "CelFlameJet"
            Tags { "LightMode"="UniversalForward" }
            Cull Off ZWrite On ZTest LEqual
            Stencil { Ref 4 WriteMask 4 Comp Always Pass Replace Fail Keep ZFail Keep }
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
                Varyings o;
                FlameGeometry(v.uv, instanceID, o.positionWS, o.positionCS, o.jet);
                o.fog = ComputeFogFactor(TransformWorldToHClip(o.positionWS).z);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float streak;
                g_jetSeam = 0.0;
                if (r.extra.w < -1.5)
                {
                    float3 bf = BallField(r, i.jet.xy);
                    if (r.extra.w < -2.5)
                    {
                        clip(bf.x);
                        half3 front = FlameBody(r, FlameHeat(r, i.positionWS));
                        return half4(MixFog(front, i.fog), 1.0);
                    }
                    float k = CelLife01(r);
                    if (bf.y > 0.0)
                    {
                        // the smoke hole: dark smoke, ink at its rim
                        half3 sc = CelRamp(saturate(0.15 + 0.2 * bf.z), CelRowB(r.tone.x), 1.0);
                        return half4(MixFog(sc, i.fog), 1.0);
                    }
                    clip(bf.x);
                    float bv = saturate(0.15 + 1.3 * bf.x + 0.15 * bf.z);
                    half3 bc = CelRamp2(bv, r.tone.x, r.tone.y, k, 0.88);
                    float bink = max(CelInk(bf.x, 0.0, CelLodInk(r.tone.w)), CelInk(-bf.y, 0.0, CelLodInk(r.tone.w)));
                    bc = lerp(bc, CelInkColor(r.tone.x, k, 0.88), bink);
                    return half4(MixFog(bc, i.fog), 1.0);
                }
                if (r.extra.w < 0.0)
                {
                    float rf = FlameCoverage(r, i.positionWS, i.jet);
                    float3 fh = FlameHeat(r, i.positionWS);
                    float tear = fh.x + 1.35 * fh.y - 0.02;
                    clip(rf);
                    half3 rc = FlameBody(r, fh);
                    float rim = CelInk(tear, 0.0, CelLodInk(r.tone.w) * 0.5);
                    rc = lerp(rc, CelPalInk(CelRowA(r.tone.x)).rgb, rim);
                    return half4(MixFog(rc, i.fog), 1.0);
                }
                float f = FlameCoverage(r, i.positionWS, i.jet);
                clip(f);
                float3 heat = FlameHeat(r, i.positionWS);
                half3 col = FlameBody(r, heat);
                float ink = CelInk(f, 0.0, CelLodInk(r.tone.w)) * 0.2;
                col = lerp(col, CelPalInk(CelRowA(r.tone.x)).rgb, ink);
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
            struct DepthVaryings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                nointerpolation uint id : TEXCOORD1;
                float3 jet : TEXCOORD2;
            };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                DepthVaryings o;
                FlameGeometry(v.uv, instanceID, o.positionWS, o.positionCS, o.jet);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target
            {
                clip(FlameCoverage(_CelRecords[i.id], i.positionWS, i.jet));
                return 0;
            }
            ENDHLSL
        }
    }
}
