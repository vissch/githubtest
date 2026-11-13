// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - LightningBeam: a camera-facing ribbon laid
// along a straight beam, with a pulse window that runs down it. Inside the window the beam is a spindle (pointed at
// both ends) whose centre line zig-zags a little and whose outline is ragged, re-drawn at 16 Hz (the flicker of the cel
// clip); short branch arcs leave it at random cells (Close only: they fade out going to Mid). Banded with CelRamp:
// a cyan body with a thin white core and a dark ink edge (premultiplied: the body and the ink cover what is behind),
// and a dim blue halo around it that adds light. One record per pulse; the beam is the record's own velocity x life
// (CelAim.Target, Speed 0, Arrive 1, no drag or gravity: vel x life = target - start).
// r03: a tighter, dimmer halo. r02: a record with size.w < 0 is an impact DOT instead: a camera-facing star at the record's place, additive.
// Going out (_CelLod.x toward 2) the steady thread and the dot keep a minimum size in screen pixels and see through
// most of the fog, so the Far tier (thread + dot, one draw) still reads at O120 and O240.
//   size.xyz   beam: width (m, at the target's distance) of the spindle's widest part at birth, at death, ease.  dot: radius (m)
//   size.w     beam: the pulse window's length (share of the beam, 0..1; 1 and extra.z = extra.w = 1: the whole beam,
//              the steady thread, which shows only going out to Mid/Far).  dot: minus the radius in px at Far
//   erode.xyz  the threshold the cross-section (dot: the radial field) is cut at over the life (it thins in steps)
//   tone       x palette, y HDR core, z opacity, w beam: clip, the window never reaches past this share of the beam
//              (1: stops at the target; above 1: the pulse runs on through it)
//   extra      beam: x zig-zag amplitude (share of the width, 0..1.5), y branch arcs per metre (0: none),
//              z the window's head (share of the beam) at birth, w at death.
//              dot: x mode: 0 star (banded, additive), 1 soft bloom facing the eye (additive, unbanded),
//                   2 soft pool lying on the ground (additive), 3 puff facing the eye (soft lumpy disc that covers:
//                   dust, or with a hard edge a black fleck);
//                   y softness (1/2: falloff, 0 = 5; 3: edge width in field units);
//                   z star points (0: star round; 3: lump amplitude), w star sharpness (1: white core radius share)
//   erode.w    beam (r04): the window's head is jittered by +- this share of the beam per record (seeded), so the
//              dashes re-draw at new places; the steady thread ignores it.  dot: unused
// b01: dot mode 4 = ray star (white-hot core + uneven thin spike rays, additive); y ray root half-width, z rays, w core share.
// r04: dots move (CelPosition), so puffs and flecks drift; soft bloom, pool and puffs added (no new draw: one kind).
Shader "TW/Cel/LightningBeam"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+21" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelLightningBeam"
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

            // the ribbon is this many half-widths to each side, room for the zig-zag, the halo and the branches
            // b06: 8 (the bolts are half as wide, so forks and sparks need more half-widths of room)
            #define BEAM_PAD 8.0

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;                         // beam: x 0..1 along the window, y across in half-widths, z metres along, w half-width (m). dot: xy -1..1, w < 0
                nointerpolation float4 tone : TEXCOORD1;         // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;        // x zig amp, y branches per m, z seed, w window length (m) | dot: z points, w sharpness
                float4 more : TEXCOORD3;                         // x fog, y far (0..1: how much the fog is skipped)
                float3 positionWS : TEXCOORD4;
            };

            // metres per screen pixel at a world point (perspective)
            float PixelMetres(float3 p)
            {
                // |P[1][1]|: the projection is flipped when URP renders to a texture; never more than 1/200 of the distance
                float d = distance(_WorldSpaceCameraPos, p);
                return d * 2.0 / max(abs(UNITY_MATRIX_P[1][1]) * _ScreenParams.y, 400.0);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float k = CelLife01(r), alive = CelAlive(r);
                float far = saturate(_CelLod.x - 1.0);
                Varyings o;
                if (r.size.w < 0.0)
                {
                    // the impact dot: a camera-facing quad, at least -size.w px in radius at Far (half that at Mid)
                    float mode = r.extra.x;
                    float3 c = CelPosition(r, CelAge(r));
                    float minPx = mode < 0.5 ? -r.size.w * saturate(_CelLod.x * 0.5) : 0.0;
                    float rad = max(CelCurve(r.size.xyz, k), minPx * PixelMetres(c)) * alive;
                    float2 corner = float2(v.uv.x * 2.0 - 1.0, v.uv.y * 2.0 - 1.0);
                    float3 w = (mode > 1.5 && mode < 2.5) ? c + float3(corner.x, 0.0, corner.y) * rad + float3(0.0, 0.05, 0.0)
                                                          : CelBillboard(c, corner * rad);
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.beam = float4(corner, r.seed, -1.0);
                    o.tone = float4(r.tone.x, r.tone.y, r.tone.z * (mode < 0.5 ? 1.0 + 1.5 * far : 1.0), CelCurve(r.erode.xyz, k));
                    o.shape = float4(mode, r.extra.y, r.extra.z, r.extra.w);
                    o.more = float4(ComputeFogFactor(o.positionCS.z), far, 0, 0);
                    return o;
                }
                float3 span = r.vel * r.life;                     // start -> target
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                bool thread = r.size.w > 0.999;
                // r04: each dash's head is jittered by its seed (erode.w), so successive dashes land at new places
                float head = lerp(r.extra.z, r.extra.w, k) + (thread ? 0.0 : (r.seed * 2.0 - 1.0) * r.erode.w);
                float s0 = max(head - r.size.w, 0.0);
                float s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                float s = lerp(s0, s1, v.uv.x);
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float across = v.uv.y * 2.0 - 1.0;
                float halfW = width * 0.5;
                // r03: size.x is the width AT THE TARGET; nearer the eye the spindle is thinner in metres (painted
                // dashes keep a readable screen width down the beam: about 2.5x the target's at 10x nearer)
                float scaleW = thread ? 1.0 : clamp(pow(PixelMetres(p) / max(PixelMetres(r.pos + span), 1e-6), 0.1), 0.15, 2.0);   // c02: the clip's near stretch is ~2x wider on screen than at the dummy
                halfW *= scaleW;
                // going out the line keeps at least ~1.3 px of half-width (2.5 px wide at Far)
                halfW = max(halfW, 1.3 * saturate(_CelLod.x * 0.5) * PixelMetres(p) * alive * open);
                float3 w = p + side * across * halfW * BEAM_PAD;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                // the window's own 0..1 is measured against its full length, so a window clipped at the target keeps its head shape
                float full = max(r.size.w, 1e-4);
                float a = saturate((s - (head - r.size.w)) / full);
                // the steady thread is for going out: hidden at Close, it fades in toward Mid and brightens at Far
                float op = r.tone.z * (thread ? saturate(_CelLod.x * 1.4 - 0.4) * (1.0 + 2.0 * far) : 1.0);
                o.beam = float4(a, across * BEAM_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, op, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(1.0 - _CelLod.x), r.seed, full * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), far, thread ? 1.0 : 0.0, scaleW);
                return o;
            }

            // a zig-zag: random offsets -1..1 at cell corners, joined by straight lines (cells of `cell` metres)
            float Zig(float u, float cell, float salt)
            {
                float x = u / cell;
                float i = floor(x), f = x - i;
                float a = CelHash21(float2(i, salt)) * 2.0 - 1.0;
                float b = CelHash21(float2(i + 1.0, salt)) * 2.0 - 1.0;
                return lerp(a, b, f);
            }

            // an additive colour through the fog; at Far most of the fog is skipped so the line and the dot read
            half3 AddFog(half3 c, float3 ws, float fog, float far)
            {
                return lerp(CelAdditiveFog(c, ws, fog), c * 0.9, far * 0.85);
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                if (i.beam.w < 0.0 && i.shape.x > 0.5)
                {
                    float2 q = i.beam.xy;
                    float rr = length(q);
                    float thrS = saturate(i.tone.w);
                    if (i.shape.x < 2.5)
                    {
                        // r04: soft bloom (1) or soft pool (2): a smooth falloff, no bands, no rim; the bloom has a white-hot
                        // round core (w: its radius share); erosion dims it
                        float soft = i.shape.y > 0.0 ? i.shape.y : 5.0;
                        float g = exp(-rr * rr * soft) * saturate(1.0 - rr) * (1.0 - thrS);
                        half3 cs = CelPal(row, 2) * g + CelPal(row, 3) * g * g;
                        float coreR = i.shape.w * (1.0 - thrS * 0.6);
                        if (coreR > 0.0)
                        {
                            float core = 1.0 - smoothstep(coreR * 0.7, coreR, rr);
                            cs = lerp(cs, CelPalCore(row).rgb * i.tone.y, core);
                        }
                        cs *= i.tone.z;
                        return half4(AddFog(cs, i.positionWS, i.more.x, i.more.y), 0.0);
                    }
                    if (i.shape.x > 3.5)
                    {
                        // b01: a ray star (4): a small white-hot core and z thin spike rays of uneven length, each record at
                        // its own turn (additive); y the rays' half-width at the root (quad units), w the core's radius share
                        float nR = max(i.shape.z, 3.0);
                        float sdR = i.beam.z;
                        float turn = frac(atan2(q.y, q.x) * 0.159155 + 0.5 + sdR * 3.7);
                        float sec = turn * nR;
                        float idR = floor(sec);
                        float off = (CelHash21(float2(idR, sdR * 91.0)) - 0.5) * 0.5;
                        float lenR = 0.35 + 0.65 * CelHash21(float2(idR, sdR * 53.0 + 7.0));
                        float lat = abs(sec - idR - 0.5 - off) * 6.28318 / nR * rr;
                        float wR = max(i.shape.y, 0.005) * saturate(1.0 - rr / lenR);
                        float ray = CelAAStep(0.35, saturate(1.0 - lat / max(wR, 1e-4))) * step(rr, lenR);
                        float coreR = max(i.shape.w, 0.02);
                        float core = 1.0 - smoothstep(coreR * 0.75, coreR, rr);
                        float haze = exp(-rr * rr * 40.0) * 0.35;
                        half3 white = CelPalCore(row).rgb * i.tone.y;
                        half3 cr = lerp(CelPal(row, 3), white, saturate(1.2 - rr / lenR)) * ray + CelPal(row, 2) * haze;
                        cr = lerp(cr, white, core) * i.tone.z * (1.0 - thrS);
                        return half4(AddFog(cr, i.positionWS, i.more.x, i.more.y), 0.0);
                    }
                    // r04: a puff (3): a lumpy disc with a soft edge that covers (dust, c2..c3), or a hard-edged fleck
                    float sd = i.beam.z * 37.0;
                    float ang = atan2(q.y, q.x);
                    float lump = 0.5 * sin(ang * 3.0 + sd) + 0.3 * sin(ang * 5.0 + sd * 1.7 + 1.3) + 0.2 * sin(ang * 8.0 + sd * 2.3);
                    float reach = 1.0 - i.shape.z * (0.5 + 0.5 * lump);
                    float fieldP = saturate(1.0 - rr / max(reach, 0.2));
                    float edge = max(i.shape.y, 0.01);
                    float aP = smoothstep(thrS, thrS + edge, fieldP) * saturate(i.tone.z);
                    half3 cp = lerp(CelPal(row, 2), CelPal(row, 3), saturate(fieldP * 1.6));
                    cp = MixFog(cp, i.more.x);
                    return half4(cp * aP, aP);
                }
                if (i.beam.w < 0.0)
                {
                    // the dot: a small star, radial field banded, additive
                    float2 q = i.beam.xy;
                    float rr = length(q);
                    float pts = i.shape.z;
                    float spike = pts > 0.5 ? pow(abs(cos(atan2(q.y, q.x) * pts * 0.5)), max(i.shape.w, 1.0)) : 1.0;
                    float reach = lerp(0.45, 1.0, spike);
                    float fieldD = saturate(1.0 - rr / reach);
                    float maskD = CelAAStep(max(i.tone.w, 0.02), fieldD);
                    half3 cd = CelRamp(fieldD, row, i.tone.y) * maskD * i.tone.z;
                    return half4(AddFog(cd, i.positionWS, i.more.x, i.more.y), 0.0);
                }
                float a = i.beam.x, y = i.beam.y, u = i.beam.z;
                float tick = floor(_CelNow * 16.0);
                float salt = i.shape.z * 97.0 + tick * 13.7;
                // c01 (by eye against the clip, 2026-11-13): each bolt is a COMET, not a spindle. A small round white-hot
                // head (a = 1), a thin pale body behind it and a wider, fainter, softer tail; the brightness falls from
                // the head to the tail so the ramp's white core sits at the head and the tail stays in the dim cyan bands.
                // r03: the cells scale with the width (more.w), or the foreshortened far half aliases away
                float cs = max(i.more.w, 0.15);
                float tail = 1.0 - a;
                float headB = exp(-tail * tail * 70.0);
                // c03: a round tip (a quarter circle over the last 1/12 of the window), not a chevron
                float tipT = saturate(tail * 16.0);
                float tip = sqrt(saturate(1.0 - (1.0 - tipT) * (1.0 - tipT)));
                float w = (0.34 + 0.22 * pow(tail, 1.4) + 0.55 * headB) * tip * sqrt(saturate(a * 5.0));   // c04: a smaller head bulb; c05: a thinner tail
                float glow = lerp(0.26, 1.0, a * a) * (1.0 + 0.6 * headB);   // c05: the tail fades more
                if (i.more.z > 0.5) { w = 0.6; glow = 1.0; }   // the far thread stays an even line
                float prof = w;
                // the centre line carries one gentle long wave and a smaller one (the clip's bolts are smooth, not braided)
                float zig = (0.5 * Zig(u, 3.0 * cs, salt) + 0.25 * Zig(u, 1.1 * cs, salt + 5.1)) * i.shape.x;
                float dy = abs(y - zig);
                float field = saturate(1.0 - dy / max(w, 1e-3)) * glow;
                // a thin crackle hair weaving about the TAIL only (the clip's tails fray into a fine electric wisp)
                float wz = (Zig(u, 0.25 * cs, salt + 17.0) + 0.6 * Zig(u, 0.08 * cs, salt + 23.0)) * 2.0 + zig;
                float wisp = saturate(1.0 - abs(y - wz) / 0.3) * (1.0 - smoothstep(0.2, 0.5, a)) * 0.32 * step(0.15, i.shape.x) * step(0.05, prof);
                wisp *= step(0.7, CelHash21(float2(floor(u / (0.9 * cs)), salt + 61.0)));
                field = max(field, wisp);
                // b06: half-width in screen px, so hairs and sparks keep a minimum size down the beam
                float hwPx = i.beam.w / max(PixelMetres(i.positionWS), 1e-6);
                float onePx = 1.0 / max(hwPx, 0.5);
                // branch arcs: in some cells along the beam a thin crooked arc leaves the core and runs out to one side
                float br = 0.0;
                if (i.shape.y > 0.0)
                {
                    float cell = 0.42 * cs;   // b05: more cells (3-6 forked arcs per bolt)
                    float ci = floor(u / cell);
                    [unroll] for (int c = 0; c < 2; c++)
                    {
                        float id = ci - c;
                        float h = CelHash21(float2(id, salt + 31.0));
                        // b04: a share of the cells (y x 0.05: b03's y x cell saturated, every cell had an arc, but each was a
                        // ~1.5 px inked hair that read as nothing)
                        if (h > saturate(i.shape.y * 0.05)) continue;
                        float x0 = id * cell + CelHash21(float2(id, salt + 2.0)) * cell * 0.5;
                        float lenA = cell * (0.35 + 0.5 * CelHash21(float2(id, salt + 3.0)));
                        float t = (u - x0) / lenA;
                        if (t < 0.0 || t > 1.0) continue;
                        float sgn = CelHash21(float2(id, salt + 4.0)) > 0.5 ? 1.0 : -1.0;
                        // b02: reach further (10-30 px off the beam) and brighter
                        // b06: 2.5-6.5 half-widths (10-30 px off the thinner bolt near the frame edge)
                        float reach = BEAM_PAD * (0.32 + 0.5 * CelHash21(float2(id, salt + 6.0)));
                        float kink = 0.9 * Zig(u, 0.06, salt + id) + 0.5 * Zig(u, 0.025, salt + id + 3.3);
                        float yb = sgn * (pow(t, 0.8) * reach + kink) + zig;
                        // b04: wider and white at the root (b03's 0.5-half-width arcs were all ink), and forked: a second,
                        // flatter arm leaves the arc at 0.35-0.6 of its length
                        // b06: hair-thin forks (~1.5-2 px), never under ~1.2 px
                        float bw = max(0.5 * (1.0 - t * 0.5), 1.2 * onePx);
                        float bf = saturate(1.0 - abs(y - yb) / bw) * lerp(1.0, 0.7, t);
                        float tf = 0.35 + 0.25 * CelHash21(float2(id, salt + 8.0));
                        if (t > tf)
                        {
                            float ybf = sgn * (pow(tf, 0.8) * reach + (t - tf) * reach * 0.3 + 0.7 * Zig(u, 0.04, salt + id + 9.1)) + zig;
                            bf = max(bf, saturate(1.0 - abs(y - ybf) / (bw * 0.8)) * 0.8);
                        }
                        br = max(br, bf * step(0.15, prof));
                    }
                }
                // b06: small white sparks along the bolt, off the core by 1.5-5.5 half-widths, re-cut each tick
                // c01: a few per bolt (the clip shows 2-4 pale dots beside a bolt), drawn with or without branch arcs
                {
                    float scell = 0.3 * cs;
                    float si = floor(u / scell);
                    float hs = CelHash21(float2(si, salt + 51.0));
                    if (hs > 0.78)
                    {
                        float sx = (si + 0.2 + 0.6 * CelHash21(float2(si, salt + 52.0))) * scell;
                        float sg = CelHash21(float2(si, salt + 53.0)) > 0.5 ? 1.0 : -1.0;
                        float sy = sg * (1.5 + 4.0 * CelHash21(float2(si, salt + 54.0))) + zig;
                        float sr = max(0.3 + 0.3 * hs, 1.3 * onePx);
                        float dS = length(float2((u - sx) / max(i.beam.w, 1e-4), y - sy));
                        br = max(br, saturate(1.0 - dS / sr) * step(0.2, prof) * 0.8);
                    }
                }
                field = max(field, br);
                float thr = max(i.tone.w, 0.02);
                // c01: a crisp cel edge at the head, a soft airbrushed edge down the tail (the clip's tails feather out)
                float maskHard = CelAAStep(thr, field);
                float maskSoft = smoothstep(thr, thr + 0.3, field);
                float mask = lerp(maskSoft, maskHard, smoothstep(0.35, 0.75, a));
                // the ink: only at the head's half (thinner going out; none on the far thread); the palette's ink is pale
                float inkPx = CelLodInk(0.7) * (1.0 - i.more.z);
                float ink = CelInk(field, thr, inkPx) * mask * smoothstep(0.4, 0.8, a);
                half3 body = CelRamp(field, row, i.tone.y);
                half3 inkC = CelPalInk(row).rgb;
                half3 straight = lerp(body, inkC, ink);
                straight = MixFog(straight, i.more.x);
                float cover = mask * saturate(i.tone.z * 1.4);
                // the halo: dim blue light a few half-widths around the spindle (not around the arcs)
                // r03: tighter and softer (the clip's beam is pale cyan, not ringed in saturated blue)
                // b07: a tight rim (2 half-widths, was 4: the wide halo made the bolt 20-30 px fat)
                // c01: a wide soft pale-cyan bloom (no bands), fuller round the head, that fades with the tail
                // c03: a gaussian fringe that dies well inside the quad (BEAM_PAD), so it never shows a cut edge
                float hd = max(dy - w, 0.0) / max(3.2 * prof + 0.8, 1e-3);   // c05: a wider fringe round a thinner core
                float halo = exp(-hd * hd * 2.0) * step(0.05, prof) * (1.0 - mask) * saturate((BEAM_PAD - 0.5 - dy) * 2.0);
                halo *= lerp(0.4, 1.0, saturate(glow));
                half3 add = CelPal(row, 0) * halo * 0.85 * i.tone.z * (1.0 - i.more.z);
                // the far thread is all light: no cover, it adds
                if (i.more.z > 0.5) { add += body * mask * i.tone.z; cover = 0.0; straight = 0; }
                add = AddFog(add, i.positionWS, i.more.x, i.more.y);
                return half4(straight * cover + add, cover);
            }
            ENDHLSL
        }
    }
}
