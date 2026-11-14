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
        #include "Assets/_Project/Shaders/Cel/CelImpactFire.hlsl"

        // +2 is the connecting stream; -1.1 is a separately shaded sustained tongue.
        // Legacy +1/-1 records retain their opening behavior.
        bool RibOwnBody(CelRecord r) { return r.extra.w < -1.05 && r.extra.w >= -1.5; }
        // Existing sustained ribbons develop a broader fold after birth. A normalized
        // particle age keeps this local evolution consistent under TimeScaled.
        float RibFoldMaturity(CelRecord r) { return smoothstep(0.18, 0.38, CelLife01(r)); }

        #define JET_ENV 2.5
        struct JetOut { float3 positionWS; float along; float across; float metres; float alive; };

        // the reach of the jet at age t, as shares of the nozzle-target length: x the tail, y the front
        float2 JetSpan(CelRecord r, float t)
        {
            float grow = max(r.erode.x, 1e-3), cut = max(r.erode.y, 1e-3);
            float reach = 1.0 + r.size.w;
            float g01 = saturate(t / grow);
            // Advance from the short muzzle ignition toward the target. The
            // perspective projection supplies the screen-space easing.
            float front = lerp(saturate(r.erode.w), reach, g01);
            float tail = saturate((t - (r.life - cut)) / cut);
            tail = tail * tail * reach;
            return float2(tail, front);
        }

        // Early hot-fold and onset motion share an emitter-age gate. The shared
        // heat frame keeps its ordinary shape; only tagged onset geometry lowers.
        float OpeningPulseBlend(float t)
        {
            return smoothstep(0.10, 0.18, t) * (1.0 - smoothstep(0.58, 0.85, t));
        }
        bool IsOnsetRibbon(CelRecord r) { return r.extra.w < -1.015 && r.extra.w > -1.025; }

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
                w *= 1.0 + 0.5 * (1.0 - saturate(_CelLod.x)) * (1.0 - smoothstep(0.18, 0.52, a));
                if (r.extra.w > 1.5)
                {
                    // The ribbons take over the broad body after ignition. Keep the nozzle
                    // connected and the full Far stream, where the recipe has no ribbons.
                    float handoff = smoothstep(0.60, 0.95, _CelNow - r.tone.z)
                        * smoothstep(0.08, 0.24, a);
                    float connector = lerp(0.48, 1.0, saturate(_CelLod.x * 0.5));
                    // Fill the mature Close body under the ribbons without changing
                    // ignition, world-space heat, or the restored cutoff endpoint.
                    connector += 0.12 * (1.0 - saturate(_CelLod.x)) * smoothstep(0.8, 1.0, handoff);
                    // Restore the body as the last independent masses expire.
                    // The existing tail cutoff remains owned by JetSpan.
                    float cut = max(r.erode.y, 1e-3), cutStart = r.life - cut;
                    float restoredWidth = lerp(0.80, 1.0, saturate(_CelLod.x * 0.5));
                    connector = lerp(connector, restoredWidth, smoothstep(cutStart - 0.66 * cut,
                        cutStart + 0.54 * cut, CelAge(r)));
                    w *= lerp(1.0, connector, handoff);
                }
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
            if (r.extra.w > 0.0)
                c += side * (0.45 * sin(saturate(a) * 3.14159265) + 0.08 * saturate(a));
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
            float up = r.extra.w > 1.5 ? 1.0 : r.extra.w; // +2 changes ownership, not upper amplitude
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
            if (IsOnsetRibbon(r))
            {
                // f12/f16's highest contour is outside the -3 front. Move this
                // actual onset geometry down about 40 px in the captured view,
                // tapering to zero at the nozzle instead of offsetting its root.
                float opening = OpeningPulseBlend(_CelNow - r.tone.z);
                p -= up * (0.44 * smoothstep(0.0, 0.42, a) * opening);
            }
            if (RibOwnBody(r)) p += up2 * (0.30 * sin(saturate(a) * 3.14159265));
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
            // Constant over this record: the lower orange side never flips between segments.
            if (RibOwnBody(r) && side.y < 0.0) side = -side;
            // no per-vertex up-flip: on a curved spine it flipped the strip between vertices (plank facets)
            float hw = 0.5 * r.size.x * lerp(0.3, 1.0, sqrt(saturate(a))) * (1.0 + 1.2 * saturate(_CelLod.x));
            // a tongue shrinking into the target thins with its visible length (no wide stub at the end)
            hw *= saturate((w.y - w.x) * length(r.vel * r.life) / max(r.size.y, 0.1));
            // d37: the build-up: tongues born later in the burn are wider (0.7x at 0.6 s -> 1.15x by 3.0 s), so the jet
            // keeps growing to REF's peak (~f84) instead of peaking 22 frames early
            hw *= lerp(0.7, 1.15, smoothstep(0.6, 3.0, r.born - r.tone.z));
            if (RibOwnBody(r)) hw *= 1.0 + lerp(0.50, 0.22, saturate(_CelLod.x)) * RibFoldMaturity(r);
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
        // A tongue keeps a connected body, but its outer shell and hot runs roll independently
        // of the geometric spine. No global stream noise is allowed to perforate this mass.
        // x coverage; y the folded heat field; z the slow roll; w small outer-edge detail.
        float4 RibMassField(CelRecord r, float u, float across)
        {
            float age = CelAge(r), sd = r.seed * 17.0;
            float flow = u * 3.6 - age * 1.8;
            float roll = CelSimplex3(float3(flow * 0.72, sd + 1.7, age * 0.60));
            // The contour, heat sample and hot core share this transverse bend.
            // It lives inside the existing proxy, so it cannot fold a ribbon quad.
            float transverse = 0.22 * roll * RibFoldMaturity(r);
            across -= transverse;
            float2 q = float2(flow - across * 0.9, across * 1.15);
            q += float2(0.40 * roll, 0.24 * sin(flow * 1.6 + roll * 2.4 + age * 0.5));
            float fold = CelSimplex3(float3(q, sd + 3.1 + age * 0.40));
            float rag = CelSimplex3(float3(u * 10.5 - age * 3.2 - abs(across) * 1.7,
                across * 2.0, sd + 9.2));
            // Recover some of the lost union area. The guaranteed inner .84 remains filled;
            // only the outer shell is notched, and the largest edge stays inside RIB_ENV1.3.
            float edge = clamp(1.14 + 0.16 * roll + (across > 0.0 ? 0.24 : 0.16) * rag, 0.84, 1.28);
            edge = min(edge, 1.28 - abs(transverse)); // leave a margin inside RIB_ENV1.3 on both sides
            return float4((edge - abs(across)) / edge, fold, roll, rag);
        }

        // Only mature Close sustained ribbons soften their internal temperature bands.
        // Palette, HDR peak, outer cutout, depth and all other flame modes are unchanged.
        half3 RibRamp(float value, uint row, float hdr, float soften)
        {
            half4 ink = CelPalInk(row);
            half4 core = CelPalCore(row);
            int bands = clamp((int)round(ink.a), 1, 4);
            int first = 4 - bands;
            float derivative = max(fwidth(value), 1e-5);
            float aaWidth = derivative * 0.75;
            // Keep a modest temperature-space transition on broad folds, with a
            // bounded screen-space contribution. Never narrow the existing AA.
            float softWidth = max(aaWidth, min(0.10, max(0.055, derivative * 1.25)));
            float width = lerp(aaWidth, softWidth, saturate(soften));
            half3 col = CelPal(row, first);
            [unroll] for (int j = 1; j < 4; j++)
                if (j < bands)
                {
                    float edge = (float)j / bands;
                    col = lerp(col, CelPal(row, first + j), smoothstep(edge - width, edge + width, value));
                }
            if (core.a < 1.0)
                col = lerp(col, core.rgb * max(hdr, 0.0), smoothstep(core.a - width, core.a + width, value));
            return col;
        }

        half3 RibBody(CelRecord r, float u, float across, float4 mass)
        {
            float mature = RibFoldMaturity(r);
            across -= 0.22 * mass.z * mature;
            float ridge = 0.10 + 0.22 * mass.z + 0.10 * mass.y;
            float hotWidth = 0.40 + 0.06 * mature + 0.14 * mass.z;
            float core = sqrt(saturate(1.0 - abs(across - ridge) / hotWidth));
            // Broad hot folds retain their connected center through moderate cooling.
            core *= lerp(0.25, 1.0, smoothstep(-0.45, 0.30, mass.y + 0.25 * mass.z));
            float fork = saturate(1.0 - abs(across - (ridge + 0.38 + 0.12 * mass.z)) / 0.18);
            fork *= smoothstep(0.10, 0.60, mass.y) * smoothstep(0.20, 0.55, u)
                * (1.0 - smoothstep(0.72, 0.95, u));
            // The initial fork remains unchanged, then merges into one broader
            // hot fold rather than painting a second thin parallel white strand.
            core = max(core, fork * 0.8 * (1.0 - mature));
            core *= smoothstep(0.04, 0.16, u) * (1.0 - smoothstep(0.80, 1.0, u));
            float underside = 1.0 - smoothstep(-0.65, 0.0, across + 0.18 * mass.y);
            float value = saturate(0.34 + 0.40 * saturate(mass.x) + 0.20 * mass.y
                + 0.40 * core - 0.12 * underside + 0.025 * mass.w);
            uint row = CelRowA(r.tone.x);
            float soften = 0.75 * mature * (1.0 - saturate(_CelLod.x));
            half3 col = RibRamp(value, row, r.tone.y, soften);
            float rim = CelInk(mass.x, 0.0, CelLodInk(r.tone.w) * 0.45);
            return lerp(col, CelPal(row, 0), rim);
        }

        // The connecting stream and legacy opening ribbons share one flowing field. Their
        // overlapping borders must not paint seams through the initial continuous push.
        float3 FlameCoordinates(CelRecord r, float3 p)
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
            return float3(m, s, dot(rel, side));
        }

        // One advected bulk profile bends the silhouette and heat together. It does not add another
        // layer of analytic flame lenses: irregular swells move downstream, with pinches between them.
        float2 FlameBulkProfile(float m, float s, float t)
        {
            // Preserve ignition, then let the strongest rolls develop downstream. The reference
            // keeps a compact middle and gains upper volume in the final third of the stream.
            float sustained = smoothstep(0.45, 0.85, t);
            float gate = lerp(smoothstep(0.12, 0.34, s), smoothstep(0.22, 0.55, s), sustained);
            float spread = smoothstep(0.45, 0.75, s) * sustained;
            float swell = clamp(CelSimplex3(float3(m * 0.46 - t * 2.8, t * 0.28, 17.6)) * 1.6, -1.0, 1.0);
            return float2((1.0 + 0.34 * swell * gate) * (1.0 + 0.18 * spread),
                0.22 * swell * gate + 0.14 * spread);
        }

        float4 FlameHeat(CelRecord r, float3 p)
        {
            float3 frame = FlameCoordinates(r, p);
            float m = frame.x, s = frame.y, y = frame.z;
            float t = _CelNow - r.tone.z;
            float2 bulk = FlameBulkProfile(m, s, t);
            // Candidate 15's irregular flowing field, tightened and lifted. The
            // core and silhouette retain the same emitter-space coordinate system.
            float opening = OpeningPulseBlend(t);
            y -= 0.45 * sin(s * 3.14159265) + 0.08 * s + bulk.y;
            y *= y < 0.0 ? 1.25 : 1.0;
            float radius = lerp(0.30, 1.90, pow(s, 0.75)) * bulk.x;
            // Broaden rolling patches after the anchored nozzle without enlarging the jet envelope.
            float roll = smoothstep(0.12, 0.32, s);
            float2 q = float2(m * 0.84 - t * 5.2, y * lerp(1.9, 1.25, roll));
            // Evolve the advected field slowly in its third dimension. A parcel
            // deforms as it travels instead of carrying one frozen noise pattern.
            float curl = CelSimplex3(float3(q * 0.65, 9.4 + t * 0.55));
            q += float2(curl * lerp(0.55, 0.75, roll),
                sin(q.x * 1.2 + curl * 2.2 + t * 0.70) * lerp(0.32, 0.45, roll));
            float n = CelSimplex3(float3(q * float2(0.68, 1.0), 3.7 + t * 0.80));
            float fine = CelSimplex3(float3(q * float2(1.4, 1.7), 11.2 + t * 1.15));
            float heat = 1.0 - abs(y) / radius;
            // Fewer fine slits at approximately unchanged noise variance.
            float islands = n * lerp(0.78, 0.81, roll) + fine * lerp(0.22, 0.08, roll);
            float core = saturate(1.0 - abs(y + 0.20 * sin(m * 1.8 - t * 6.0) * smoothstep(0.05, 0.35, s)) / (radius * 0.50));
            core *= 0.30 + 0.70 * smoothstep(-0.28, 0.48, n);
            // Keep the white nozzle boost short and narrow; the rest of the body stays yellow-hot.
            float nozzle = 2.2 * (1.0 - smoothstep(0.03, 0.20, s))
                * saturate(1.0 - abs(y) / (radius * 0.44));
            core = max(core, nozzle);
            // A continuous longitudinal white-hot fold matches f12/f16; it is
            // confined to the inner 56 percent and retains a slow flowing bend.
            float hotFold = 1.35 * sqrt(saturate(1.0 - abs(y + 0.09 * sin(m * 1.2 - t * 3.0)) / (radius * 0.56)));
            hotFold *= 0.90 + 0.10 * sin(m * 0.8 - t * 2.8);
            core = max(core, hotFold * opening);
            return float4(heat, islands, core, s);
        }

        float FlameTear(float4 heat)
        {
            // Retain the previous nozzle coverage while narrowing its white color boost. Heat.x
            // stores 1-abs(y)/radius, so this reconstructs the old .70-wide coverage protection.
            float coverageNozzle = 2.2 * (1.0 - smoothstep(0.12, 0.42, heat.w))
                * saturate(1.0 - (1.0 - heat.x) / 0.70);
            float coverageCore = max(heat.z, coverageNozzle);
            return max(heat.x + 1.20 * heat.y - 0.10, max(coverageCore - 0.75, heat.x - 0.72)) - 1e-5;
        }

        float FlameRibbonTear(CelRecord r, float4 heat)
        {
            float tear = FlameTear(heat);
            // The opening fan is filled around its central fuel stream. Preserve
            // the true lowered geometry while closing cold holes inside that fan.
            if (IsOnsetRibbon(r)) tear = lerp(tear, max(tear, heat.x - 0.45),
                OpeningPulseBlend(_CelNow - r.tone.z));
            return tear;
        }

        half3 FlameBody(CelRecord r, float4 heat)
        {
            uint row = CelRowA(r.tone.x);
            float value = saturate(0.35 + heat.x * 0.18 + heat.y * 0.50 + heat.z * 0.38);
            // Negative advected noise may carve a hook, but should not cool most of its intact body
            // to orange. This floor only reaches yellow (.64), below the .83 HDR-core threshold.
            float yellowFloor = 0.48 + 0.16 * smoothstep(0.10, 0.68, heat.x);
            float yellowInterior = smoothstep(0.45, 0.85, heat.x) * smoothstep(0.06, 0.18, heat.w)
                * smoothstep(-0.38, -0.05, heat.y);
            value = max(value, lerp(value, yellowFloor, yellowInterior));
            half3 sustained = CelRamp(value, row, r.tone.y);
            // Most distal hot material is bright yellow, with sparse hotter white folds retained.
            // Change the core's emitted color instead of dimming the whole far end of the stream.
            float warmDistal = smoothstep(0.50, 0.72, heat.w)
                * (1.0 - smoothstep(0.45, 0.64, heat.y));
            float coreMask = CelAAStep(CelPalCore(row).a, value);
            sustained = lerp(sustained, half3(1.85, 1.25, 0.45), warmDistal * coreMask);
            // Blend emitted color, not the ramp coordinate: a quantized ramp held the entire first
            // two frames dark orange, then pale yellow, before allowing any HDR core to appear.
            float ignition = smoothstep(0.045, 0.20, _CelNow - r.tone.z);
            return lerp(half3(1.60, 0.86, 0.34), sustained, ignition);
        }
        // Target curls use open curved flame arcs. Their holes reveal the actual
        // smoke/scene behind them rather than painting opaque dark discs over it.
        // The travelling front (-3) retains its filled leading cap.
        //   size  x radius at birth, y at death, z ease; erode x = the hole's final share of the radius
        //   extra x swirl (rad per radius), y tongue count, z spin (rad/s)
        #define BALL_Q 1.45
        float OpeningNubBlend(CelRecord r)
        {
            return r.extra.w < -2.5 && r.extra.w >= -3.5
                ? 1.0 - smoothstep(0.06, 0.14, _CelNow - r.tone.z) : 0.0;
        }
        // After the matched ignition nub, retain a rounded lower body while its leading
        // contour rolls into the stream. Only the opening front uses this local protection.
        float OpeningPushBlend(CelRecord r)
        {
            float t = _CelNow - r.tone.z;
            return r.extra.w < -2.5 && r.extra.w >= -3.5
                ? smoothstep(0.14, 0.20, t) * (1.0 - smoothstep(0.62, 0.82, t)) : 0.0;
        }
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
            return CelCurve(r.size.xyz, k) * lerp(1.0, 0.15, smoothstep(r.extra.w < -2.5 ? 0.55 : 0.75, 1.0, k));
        }
        // Target-only mode (-2.2): open curling geometry supports the shared
        // smoke heat beyond the billow surface. Extra.y is phase scale (1 default),
        // Extra.z is impact world height, Tone.z is emitter start plus delay.
        // Axis and Motion retain their normal physical meanings.
        bool IsTargetTongue(CelRecord r) { return r.extra.w < -2.15 && r.extra.w > -2.25; }
        float4 TargetImpactFire(CelRecord r, float3 world)
        {
            float scale = max(r.extra.y, 1e-3);
            float elapsed = max(0.0, _CelNow - r.tone.z) / scale;
            // Geometry can rise above the hottest smoke skin. Its raw world heat,
            // phase and palette are identical; only the upper support gate differs.
            return CelImpactFire(world, r.extra.z, elapsed, CelAge(r) / scale, float2(1.8, 4.2));
        }
        float3 TargetTongueField(CelRecord r, float2 q)
        {
            float t = CelAge(r) / max(r.extra.y, 1e-3), k = CelLife01(r), sd = r.seed * 23.0;
            float h = CelHash11(sd + 2.7), h2 = CelHash11(sd + 18.3);
            float h3 = CelHash11(sd + 31.9);
            q.x *= h > 0.5 ? 1.0 : -1.0;
            float angle = (h2 - 0.5) * 0.45;
            float sn = sin(angle), cs = cos(angle);
            q = float2(cs * q.x - sn * q.y, sn * q.x + cs * q.y);
            // A slender upright envelope keeps the same open hook topology.
            // Coarse advected folds break its otherwise polished outline.
            q *= float2(1.20, 0.90);
            float rise = q.y - t * (0.85 + 0.20 * r.extra.x);
            float n = CelSimplex3(float3(q.x * 3.0, rise * 1.8, sd + t * 0.35));
            float fine = CelSimplex3(float3(q.x * 4.2, rise * 2.4, sd + 7.1 + t * 0.35));
            float2 p = q + float2(0.08 * n, 0.045 * fine);
            float bend = 0.10 * sin(t * 1.2 + sd);
            float2 p0 = float2(-0.22 + 0.14 * (h - 0.5), -1.0);
            float2 p1 = float2(-1.10 + 0.32 * (h2 - 0.5) + bend * 0.4,
                0.10 + 0.25 * (h3 - 0.5));
            float2 p2 = float2(1.35 + 0.40 * (h3 - 0.5) + bend,
                1.80 + 0.35 * (h2 - 0.5));
            // The inward-pointing tip stays well above the root. It cannot
            // return to the lower base and close the curve into a hoop.
            float2 p3 = float2(0.55 + 0.30 * (h2 - 0.5) + bend * 0.5,
                0.45 + 0.24 * h3 + 0.04 * cos(t * 1.2 + sd));
            float2 a = -p0 + 3.0 * p1 - 3.0 * p2 + p3;
            float2 b = 3.0 * p0 - 6.0 * p1 + 3.0 * p2;
            float2 c = -3.0 * p0 + 3.0 * p1;
            float best = -1e4, core = 0.0, bestU = 0.0, bestSide = 0.0, bestWidth = 1.0;
            float2 previous = p0;
            [unroll] for (int segment = 0; segment < 20; segment++)
            {
                float u = (segment + 1.0) / 20.0;
                float2 next = ((a * u + b) * u + c) * u + p0;
                float2 delta = next - previous;
                float along = saturate(dot(p - previous, delta) / max(dot(delta, delta), 1e-5));
                float pathU = (segment + along) / 20.0;
                float distanceToCurve = length(p - lerp(previous, next, along));
                // The former capsule-wide base becomes a narrow root that can
                // merge into the hot body. Width pinches travel up the envelope.
                float width = lerp(0.14, 0.010, pow(pathU, 0.85)) * (0.82 + 0.24 * n);
                float field = (width - distanceToCurve) / max(width, 0.015);
                if (field > best)
                {
                    best = field;
                    bestU = pathU;
                    float2 inward = float2(delta.y, -delta.x) * rsqrt(max(dot(delta, delta), 1e-5));
                    bestSide = dot(p - lerp(previous, next, along), inward);
                    bestWidth = width;
                    // A continuous hotter inner edge follows the curvature.
                    // World heat may modulate it slightly, but cannot make spots.
                    core = saturate(1.0 - abs(bestSide / max(width, 0.01) - 0.35) / 0.45);
                }
                previous = next;
            }
            best -= 0.26 * saturate(fine) * smoothstep(0.20, 0.65, bestU);
            // Occasional upper-neck notches split a tip into two short tongues,
            // without carving repeated holes all the way down to the root.
            float split = smoothstep(0.60, 0.82, bestU) * (1.0 - smoothstep(0.93, 1.0, bestU))
                * smoothstep(0.10, 0.60, n);
            best -= 1.25 * split * saturate(1.0 - abs(bestSide) / max(bestWidth * 0.35, 0.005));
            float elapsed = max(0.0, _CelNow - r.tone.z) / max(r.extra.y, 1e-3);
            float late = smoothstep(4.18, 4.52, elapsed);
            float tear = CelSimplex3(float3(bestU * 7.0 - t * 0.8, sd + 5.3, t * 0.35));
            // The f108 envelope survives; by f112 its upper length breaks into
            // irregular short remnants while the lowest hot roots linger.
            best -= late * (0.55 + 1.25 * smoothstep(-0.20, 0.60, tear))
                * smoothstep(0.05, 0.50, bestU);
            float decay = smoothstep(0.58, 1.0, k);
            best -= decay * (1.25 + 0.70 * n + 0.45 * fine);
            return float3(best, core, n * 0.75 + fine * 0.25);
        }

        // x the shared forward/depth fire field, y the inner gap, z curl detail.
        float3 BallField(CelRecord r, float2 q)
        {
            if (IsTargetTongue(r)) return TargetTongueField(r, q);
            float t = CelAge(r), k = CelLife01(r), sd = r.seed * 23.0;
            bool risingCurl = r.extra.w < -2.05 && r.extra.w >= -2.5;
            if (r.extra.w >= -2.5)
            {
                // Upright, leaning crescent rather than a circular emblem. This
                // transforms only the target field, not the travelling front.
                q.x = (q.x + q.y * 0.08 + q.y * q.y * 0.04) * 0.98;
                q.y *= 0.96;
            }
            q.x += (r.extra.w >= -2.5 ? 0.06 : 0.13) * sin(q.y * 3.0 + t * 2.0 + sd);
            q.y -= (r.extra.w >= -2.5 ? 0.05 : 0.11) * sin(q.x * 3.0 - t * 1.6 + sd);
            float rho = length(q);
            float th = atan2(q.y, q.x) + r.extra.x * rho * 2.1 - t * r.extra.z;
            float n1 = CelSimplex3(float3(cos(th) * 1.35, sin(th) * 1.35, t * 0.65 + sd));
            // Heat rises through each curl in irregular elongated patches. A
            // Cartesian field breaks the concentric color rings of radial distance.
            float2 flameUV = float2(q.x * 2.7 + 0.28 * sin(q.y * 4.0 - t * 2.0 + sd),
                q.y * 1.6 - t * 0.95);
            float n2 = CelSimplex3(float3(flameUV, t * 0.55 + sd));
            float up = saturate(q.y / max(rho, 1e-3));
            float outer = (0.83 + (risingCurl ? 0.08 : 0.27) * n1
                + (risingCurl ? 0.025 : 0.12) * n2 + 0.17 * up)
                * (1.0 - 0.28 * smoothstep(0.78, 1.0, k));
            // Rising fire starts as an open lick; a filled noisy disc produces a rosette.
            float holeGrowth = risingCurl ? lerp(0.82, 1.0, smoothstep(0.0, 0.65, k))
                : lerp(0.68, 1.0, smoothstep(0.0, 0.62, k));
            float hole = (r.erode.x + 0.22) * holeGrowth * (0.95 + 0.22 * n2);
            if (r.extra.w < -2.5) hole = 0.0;
            float holeDistance = length(q - float2(0.16 * sin(sd), 0.12));
            float fo = (outer - rho) / max(outer, 1e-3);
            float fi = hole > 0.01 ? (holeDistance - hole) / max(outer - hole, 0.05) : 1.0;
            float field = min(fo, fi);
            if (r.extra.w >= -2.5)
            {
                // A curved slot joins the inner opening to the outer silhouette,
                // turning each cooling ring into a tapering C-shaped lick.
                float slot = 0.58 + n2 * 0.18 - cos(th + sd * 0.7);
                field = min(field, slot);
            }
            return float3(field, -fi, n1 * 0.25 + n2 * 0.75);
        }
        void FlameGeometry(float2 uv, uint instanceID, out float3 world, out float4 screen, out float3 coords)
        {
            CelRecord r = CelFetch(instanceID);
            if (r.extra.w < -1.5)
            {
                float2 q = (uv * 2.0 - 1.0) * BALL_Q;
                // The initial nub reaches x=-1.62; support that contour without changing later bounds.
                q.x *= 1.0 + 0.16 * OpeningNubBlend(r);
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
                // The shutdown parcel has its own heat pattern, so give its shared forward/depth
                // surface a deterministic lead over the stream rather than a coplanar depth tie.
                if (RibOwnBody(r))
                    planePoint += view * lerp(0.002, 0.007, saturate(r.seed));
                else if (r.extra.w < -3.5) planePoint += view * 0.01;
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
                if (IsTargetTongue(r))
                    return CelImpactFireCutout(ball.x, TargetImpactFire(r, world));
                if (r.extra.w < -2.5)
                {
                    float tear = FlameTear(FlameHeat(r, world)) + 0.08;
                    // A late filled parcel must retain its rounded body outside the steady jet band.
                    // Explicit shutdown mode remains stable when the recipe is time-scaled.
                    if (r.extra.w < -3.5) tear = max(tear, ball.x - 0.22);
                    float nubBlend = OpeningNubBlend(r);
                    float nub = 1.0 - length(float2((coords.x + 0.30) / 1.32, coords.y / 0.86));
                    // During the first visible frame the leading gold nub owns the complete contour.
                    // It rejoins the ordinary shared tear before the sustained stream appears.
                    float covered = min(lerp(ball.x, nub, nubBlend), lerp(tear, 1.0, nubBlend));
                    float pushBlend = OpeningPushBlend(r);
                    if (pushBlend > 0.0)
                    {
                        // Keep the bottom connected without a full unbounded disc. The wider
                        // envelope also rounds off the tall upper hook left by the noisy cap.
                        float2 pushQ = coords.xy + float2(0.06, 0.22);
                        float body = 1.0 - length(pushQ / float2(1.08, 1.02));
                        float envelope = 1.0 - length(pushQ / float2(1.40, 1.18));
                        float rounded = max(min(covered, envelope), body - 0.08);
                        covered = lerp(covered, rounded, pushBlend);
                    }
                    return covered;
                }
                return ball.x;
            }
            if (r.extra.w < 0.0)
            {
                if (RibOwnBody(r)) return RibMassField(r, coords.x, coords.y).x;
                float field = RibField(r, coords.x, coords.y, coords.z, streak);
                return min(field, FlameRibbonTear(r, FlameHeat(r, world)) * 1.6);
            }
            float ignitionAge = _CelNow - r.tone.z;
            if (ignitionAge < 0.055) return -1.0; // the rounded front owns the first visible ignition frame
            g_jetSeam = 0.0;
            float3 jc = JetCoords(r, world);
            float a = saturate(jc.x);
            float hw = max(JetHalfWidth(r, a), 0.05);
            float lift = 0.45 * sin(a * 3.14159265) + 0.08 * a;
            float3 frame = FlameCoordinates(r, world);
            float2 bulk = FlameBulkProfile(frame.x, frame.y, _CelNow - r.tone.z);
            float shiftedAcross = (jc.y - (lift + bulk.y) / hw) / bulk.x;
            // Keep candidate 15's irregular tongue silhouette, not the regularly
            // spaced analytic lenses. The same signed field owns forward/depth.
            float field = JetField(r, jc.x, shiftedAcross, jc.z, streak);
            field = min(field, FlameTear(FlameHeat(r, world)) + 0.12);
            if (r.extra.w > 0.0) field = min(field, JetGround(r, world, jc.z) + 8.0 * (1.0 - smoothstep(0.45, 0.7, jc.x)));
            field -= g_jetSeam * smoothstep(0.08, 0.2, jc.x) * 0.18 * (1.0 - smoothstep(0.25, 0.55, field));
            float2 span = JetSpan(r, CelAge(r));
            if (span.x > 0.0)
            {
                float run = max(jc.x - span.x, 0.0);
                float taper = smoothstep(0.0, 0.18, run);
                float radius = lerp(0.15, 1.65, pow(a, 0.9));
                float crossTail = (jc.y * hw - lift) / max(radius, 0.05);
                field = min(field, taper * 1.25 - abs(crossTail - 0.18));
                field = min(field, run * max(length(r.vel * r.life), 1e-3) / (hw * 0.7) - 1e-5);
            }
            field -= 0.45 * (1.0 - smoothstep(0.055, 0.13, ignitionAge));
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
                        clip(FlameCoverage(r, i.positionWS, i.jet));
                        half3 front = FlameBody(r, FlameHeat(r, i.positionWS));
                        if (r.extra.w < -3.5)
                        {
                            float life = CelLife01(r);
                            float localHeat = saturate(0.20 + 0.65 * bf.x + 0.35 * bf.z);
                            half3 rolling = CelRamp(localHeat, CelRowA(r.tone.x), min(r.tone.y, 1.8));
                            float emerge = smoothstep(0.0, 0.22, life)
                                * (1.0 - smoothstep(0.88, 1.0, life));
                            front = lerp(front, rolling, emerge);
                        }
                        return half4(MixFog(front, i.fog), 1.0);
                    }
                    float k = CelLife01(r);
                    clip(bf.x);
                    if (IsTargetTongue(r))
                    {
                        float4 fire = TargetImpactFire(r, i.positionWS);
                        clip(CelImpactFireCutout(bf.x, fire));
                        // Local signed contour supplies the inner hot edge;
                        // the shared field retains phase/cooling and subtle variation.
                        half3 tongue = CelImpactFireColor(0.075 + 0.34 * bf.y + 0.025 * fire.x, fire.w);
                        tongue = ApplyMist(tongue, i.positionWS);
                        tongue = ApplyFieldFog(tongue, i.positionWS);
                        return half4(MixFog(tongue, i.fog), 1.0);
                    }
                    float bv = saturate(0.24 + 1.65 * bf.x + 0.16 * bf.z
                        + 0.10 * (1.0 - k));
                    half3 bc = CelRamp2(bv, r.tone.x, r.tone.y, k, 0.88);
                    if (r.extra.w < -2.05)
                    {
                        uint row = CelRowA(r.tone.x);
                        // A broad orange contour and one luminous yellow interior avoid concentric badges.
                        half3 arc = lerp(CelPal(row, 0), half3(1.85, 1.25, 0.45), CelAAStep(0.12, bf.x));
                        bc = lerp(bc, arc, 1.0 - smoothstep(0.88, 1.0, k));
                    }
                    float bink = CelInk(bf.x, 0.0, CelLodInk(r.tone.w) * 0.35);
                    bc = lerp(bc, CelPal(CelRowA(r.tone.x), 0), bink);
                    return half4(MixFog(bc, i.fog), 1.0);
                }
                if (r.extra.w < 0.0)
                {
                    if (RibOwnBody(r))
                    {
                        float4 mass = RibMassField(r, i.jet.x, i.jet.y);
                        clip(mass.x);
                        half3 ownColor = RibBody(r, i.jet.x, i.jet.y, mass);
                        return half4(MixFog(ownColor, i.fog), 1.0);
                    }
                    float rf = FlameCoverage(r, i.positionWS, i.jet);
                    float4 fh = FlameHeat(r, i.positionWS);
                    float tear = FlameRibbonTear(r, fh);
                    clip(rf);
                    half3 rc = FlameBody(r, fh);
                    float rim = CelInk(tear, 0.0, CelLodInk(r.tone.w) * 0.5);
                    rc = lerp(rc, CelPal(CelRowA(r.tone.x), 0), rim);
                    return half4(MixFog(rc, i.fog), 1.0);
                }
                float f = FlameCoverage(r, i.positionWS, i.jet);
                clip(f);
                float4 heat = FlameHeat(r, i.positionWS);
                half3 col = FlameBody(r, heat);
                float ink = CelInk(f, 0.0, CelLodInk(r.tone.w)) * 0.2;
                col = lerp(col, CelPalInk(CelRowA(r.tone.x)).rgb, ink);
                return half4(MixFog(col, i.fog), 1.0);
            }
            ENDHLSL
        }

        // HDR scene bloom supplies the fire glow without derivative-based halo artifacts.
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
