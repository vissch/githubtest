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
                float2 near : TEXCOORD5;                         // c14: x = how near the muzzle this point is (1 at the muzzle, 0 past a third of the beam)
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
                    o.near = float2(0.0, 0.0);
                    return o;
                }
                float3 span = r.vel * r.life;                     // start -> target
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                bool thread = r.size.w > 0.999;
                // r04: each dash's head is jittered by its seed (erode.w), so successive dashes land at new places
                float head = lerp(r.extra.z, r.extra.w, k) + (thread ? 0.0 : (r.seed * 2.0 - 1.0) * r.erode.w);
                // c19 (clip: every frame is a different drawing): each bolt has its own window length (0.45..1.4x) and the
                // window is re-cut every frame (0.6..1.2x), so bolt lengths differ per bolt and per frame
                float tickV = floor(_CelNow * 24.0);
                float winK = thread ? 1.0 : lerp(0.7, 1.1, CelHash21(float2(r.seed * 7.3, 1.0))) * lerp(0.85, 1.1, CelHash21(float2(r.seed * 3.7, tickV)));   // c35: all about the same length (the clip's are)   // c33: never the whole corridor (c32's longest spanned it)   // c32: less spread (the clip's bolts are all long)
                float winLen = r.size.w * winK;
                float s0 = max(head - winLen, 0.0);
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
                float scaleW = thread ? 1.0 : clamp(pow(PixelMetres(p) / max(PixelMetres(r.pos + span), 1e-6), 0.45), 0.15, 2.0);   // c24: mostly constant on screen   // c09: a constant width in metres, so the near stretch is ~2x wider on screen   // c02: the clip's near stretch is ~2x wider on screen than at the dummy
                halfW *= scaleW;
                // c10: wider toward the muzzle (the clip's near stretch is a fat frayed bundle, its far half a hair)
                if (!thread) halfW *= lerp(1.15, 1.0, saturate(s * 1.4));   // c24: barely (the clip's near bolt is still a 4-6 px line)
                // going out the line keeps at least ~1.3 px of half-width (2.5 px wide at Far)
                halfW = max(halfW, 1.3 * saturate(_CelLod.x * 0.5) * PixelMetres(p) * alive * open);
                float3 w = p + side * across * halfW * BEAM_PAD;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                // the window's own 0..1 is measured against its full length, so a window clipped at the target keeps its head shape
                float full = max(winLen, 1e-4);
                float a = saturate((s - (head - winLen)) / full);
                // the steady thread is for going out: hidden at Close, it fades in toward Mid and brightens at Far
                float op = r.tone.z * (thread ? saturate(_CelLod.x * 1.4 - 0.4) * (1.0 + 2.0 * far) : 1.0);
                o.beam = float4(a, across * BEAM_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, op, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(2.0 - _CelLod.x), r.seed, full * lenB);   // c25: the Range films at zoom 30 = tier 1.0, which zeroed every branch arc; now full to Mid, gone at Far
                o.more = float4(ComputeFogFactor(o.positionCS.z), far, thread ? 1.0 : 0.0, scaleW);
                o.near = float2(saturate(1.0 - s * 2.5), 0.0);
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
                    if (i.shape.x > 5.5)
                    {
                        // c22: bullseye rings (6): two thin additive circles (the board's painted rings glowing) with a soft
                        // hot centre; y = ring width, z = outer ring radius, w = inner ring radius (quad units)
                        float rw = max(i.shape.y, 0.02);
                        float r1 = max(i.shape.z, 0.2), r2 = max(i.shape.w, 0.1);
                        float ring = exp(-pow((rr - r1) / rw, 2.0)) + 0.6 * exp(-pow((rr - r2) / rw, 2.0));
                        float centre = exp(-rr * rr * 40.0) * 0.5;   // c23: a small hot point
                        half3 cr6 = (CelPal(row, 3) * ring + CelPalCore(row).rgb * i.tone.y * centre * 0.8) * (1.0 - thrS) * i.tone.z;
                        return half4(AddFog(cr6, i.positionWS, i.more.x, i.more.y), 0.0);
                    }
                    if (i.shape.x > 4.5)
                    {
                        // c21: a board wash (5): a soft rounded rectangle of additive light the size of the dummy's board, so
                        // the board itself reads lit (the clip's hit is a lit board, not a ball); y = soft edge, z = half
                        // width, w = half height (quad units); erosion dims it
                        float2 hx = float2(max(i.shape.z, 0.2), max(i.shape.w, 0.2));
                        float2 d = abs(q) - hx + 0.15;
                        float sdf = length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - 0.15;
                        float edgeB = max(i.shape.y, 0.02);
                        float wash = (1.0 - smoothstep(-edgeB, edgeB, sdf)) * (1.0 - thrS);
                        float inner = exp(-rr * rr * 3.0);
                        half3 cw = (CelPal(row, 2) * 0.6 + CelPal(row, 3) * inner * 0.8) * wash * i.tone.z;
                        return half4(AddFog(cw, i.positionWS, i.more.x, i.more.y), 0.0);
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
                    float lump = 0.5 * sin(ang * 3.0 + sd) + 0.3 * sin(ang * 5.0 + sd * 1.7 + 1.3) + 0.2 * sin(ang * 8.0 + sd * 2.3) + 0.15 * sin(ang * 13.0 + sd * 3.1);   // c43: a finer scallop
                    // c43: a ragged rim (w: amplitude; 0 on the flecks): value noise over the lobe eats into its edge
                    // and mottles its body, and the lobe THINS as it erodes instead of shrinking as a solid disc
                    // (the clip's lobes are ragged at the rim and dissolve into mist)
                    float rag = i.shape.w;
                    float n2 = CelValue2(q * 4.5 + float2(sd, sd * 0.37)) * 0.6 + CelValue2(q * 9.0 + float2(sd * 1.3, sd)) * 0.4;
                    float reach = 1.0 - i.shape.z * (0.5 + 0.5 * lump) - rag * (n2 - 0.5);
                    float fieldP = saturate(1.0 - rr / max(reach, 0.2)) * (1.0 - rag * 0.8 * (n2 - 0.5));
                    float edge = max(i.shape.y, 0.01);
                    float aP = smoothstep(thrS, thrS + edge, fieldP) * saturate(i.tone.z) * (1.0 - 0.6 * thrS * saturate(rag * 4.0));
                    half3 cp = lerp(CelPal(row, 2), CelPal(row, 3), saturate(fieldP * 1.6));
                    cp *= lerp(0.6, 1.28, saturate(q.y * 0.9 + 0.5)) * lerp(0.8, 1.1, saturate(fieldP * 2.0)) * lerp(1.0 - 0.3 * rag, 1.0 + 0.3 * rag, n2);   // c43: mottled   // c38: each lobe lit on top, dark underneath (a cartoon cloud)   // c35/c30
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
                float tick = floor(_CelNow * 24.0);   // c19: a new drawing every frame (was 16 Hz)
                float salt = i.shape.z * 97.0 + tick * 13.7;
                // c19: per bolt per frame, the bolt blinks off in some frames and its brightness jumps (the clip's bolts flicker)
                float blinkH = CelHash21(float2(salt, 0.33));
                if (i.more.z < 0.5 && blinkH < 0.08) discard;   // c30: the clip's bolts persist frame to frame in flight; only a rare gap
                float flick = lerp(0.8, 1.2, CelHash21(float2(salt, 0.77)));   // c30: narrower
                // c01 (by eye against the clip, 2026-11-13): each bolt is a COMET, not a spindle. A small round white-hot
                // head (a = 1), a thin pale body behind it and a wider, fainter, softer tail; the brightness falls from
                // the head to the tail so the ramp's white core sits at the head and the tail stays in the dim cyan bands.
                // r03: the cells scale with the width (more.w), or the foreshortened far half aliases away
                // c50: on a short arc the cell is a quarter of the arc's length, or a 0.6 m board arc got one 2.6 m kink
                // cell and drew as a straight spike (the clip's arcs on the board are crooked scribbles)
                float cs = max(min(i.more.w, i.shape.w * 0.25), 0.05);
                float tail = 1.0 - a;
                float headB = exp(-tail * tail * 28.0);   // c21: a longer white head (the clip's comets are white over their front quarter)
                // c43: the point is in the width profile now (blade); tip only fades the halo toward the front
                float tip = saturate(tail * 4.0);
                // c19: a bright white comet head again (the clip's bolts mostly have one) on a thin beaded strand
                float bead = 0.82 + 0.25 * Zig(u, 0.35 * cs, salt + 31.0);
                // c30: a SPINDLE (the clip's f19-f22: each bolt is a long stripe, thick in the middle, pointed at both ends,
                // its white core along the thick middle, cyan at the thin ends); no comet bulb at the head
                // c38: PROJECTILES - the thickest point sits at 0.7 of the length (a short pointed front, a long thinner
                // tail) and the front quarter is white-hot (the clip's bolts carry a bright head: f60/f76/f84)
                // c43: a BLADE, not a comet (owner: "some look round with a tail"): the tail widens over the back 0.45,
                // a thick plateau from 0.45 to 0.72, then a straight spear point; no round head bulge (c38's headW
                // on a thin tail read as a ball with a tail)
                float blade = min(pow(saturate(a / 0.45), 0.7), pow(saturate((1.0 - a) / 0.28), 0.85));
                float spin = blade;
                float w = 1.6 * blade * bead;
                float longW = step(1.5, i.shape.w);
                // c31: a cyan body with the white core only along the thick middle (c30 was a white rod); the short
                // clinging scribbles on the board are white-hot all through
                float glow = min((0.45 + 0.4 * spin + 0.55 * (1.0 - longW)) * (i.more.z > 0.5 ? 1.0 : flick), 0.95) + 0.2 * headB * blade;   // c44: a cyan BODY with a thin pale core (c43 was a white line with cyan edges)   // c43: no white ball   // c33: the scribbles whiter
                if (i.more.z > 0.5) { w = 0.6; glow = 1.0; }   // the far thread stays an even line
                float prof = w;
                // the centre line carries one gentle long wave and a smaller one (the clip's bolts are smooth, not braided)
                // c08: three scales of crooked lightning kinks (the clip's strands are thin and crooked, not smooth)
                // c11: the fine kinks come and go along the strand (an amplitude on its own slow zig-zag), so the edge is irregular, not a sawtooth
                float fineA = saturate(0.5 + 0.7 * Zig(u, 1.1 * cs, salt + 13.0));
                float zig = (0.65 * Zig(u, 2.6 * cs, salt) + 0.35 * Zig(u, 0.6 * cs, salt + 5.1) + 0.3 * fineA * Zig(u, 0.2 * cs, salt + 9.7)) * i.shape.x;   // c31: a bigger slow wobble
                float dy = abs(y - zig);
                float field = saturate(1.0 - dy / max(w, 1e-3)) * glow;
                // c12: two more thin strands twist round the main one, spread apart toward the tail and meeting at the head
                // (the clip's near stretch is a frayed bundle, its head a single hot point); dimmer than the main strand
                // c13: only on a long window (a short board arc with a bundle and an aura read as a cyan blob)
                float nearF = saturate(i.near.x) * longW;
                // b06: half-width in screen px, so hairs and sparks keep a minimum size down the beam
                float hwPx = i.beam.w / max(PixelMetres(i.positionWS), 1e-6);
                float onePx = 1.0 / max(hwPx, 0.5);
                if (i.more.z < 0.5 && longW > 0.5 && i.shape.x > 0.15)
                {
                    // c30: two thin strands wind round the main spindle over PART of its length, each on its own slow wave
                    // that crosses the centre line (the clip's f27: a second strand twists across the main one and leaves
                    // it again); which part, and how far they stray, is re-cut every frame
                    float h1 = CelHash21(float2(salt, 0.41)), h2 = CelHash21(float2(salt, 0.43));
                    float a1 = h1 * 0.55, a2 = h2 * 0.55;
                    float g1 = smoothstep(a1, a1 + 0.08, a) * (1.0 - smoothstep(a1 + 0.4, a1 + 0.55, a));   // c31: longer, further out
                    float g2 = smoothstep(a2, a2 + 0.08, a) * (1.0 - smoothstep(a2 + 0.3, a2 + 0.45, a));
                    float amp = saturate(i.shape.x);
                    float z1 = zig * 0.4 + (3.6 * Zig(u, 2.2 * cs, salt + 101.0) + 0.6 * Zig(u, 0.45 * cs, salt + 103.0)) * amp;
                    float z2 = zig * 0.4 + (3.0 * Zig(u, 1.8 * cs, salt + 107.0) + 0.6 * Zig(u, 0.4 * cs, salt + 109.0)) * amp;
                    float wS = max(0.5, 1.3 * onePx);
                    float f1 = saturate(1.0 - abs(y - z1) / wS) * glow * 0.85 * g1;
                    float f2 = saturate(1.0 - abs(y - z2) / wS) * glow * 0.75 * g2;
                    field = max(field, max(f1, f2));
                    dy = min(dy, min(abs(y - z1) + 2.0 * (1.0 - g1), abs(y - z2) + 2.0 * (1.0 - g2)));
                }
                // a thin crackle hair weaving about the TAIL only (the clip's tails fray into a fine electric wisp)
                float wz = (Zig(u, 0.25 * cs, salt + 17.0) + 0.6 * Zig(u, 0.08 * cs, salt + 23.0)) * 2.0 + zig;
                float wisp = saturate(1.0 - abs(y - wz) / 0.3) * (1.0 - smoothstep(0.2, 0.5, a)) * 0.32 * step(0.15, i.shape.x) * step(0.05, prof);
                wisp *= step(0.7, CelHash21(float2(floor(u / (0.9 * cs)), salt + 61.0)));
                field = max(field, wisp);
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
                        if (h > saturate(i.shape.y * 0.05 * (0.4 + 1.2 * spin))) continue;   // c30: most forks leave the thick middle
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
                        float bw = max(0.75 * (1.0 - t * 0.6), 1.2 * onePx);   // c30: a little wider at the root
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
                float maskSoft = smoothstep(thr, thr + 0.12, field);   // c14: crisper (the clip's tails are dim, not blurred)
                float mask = lerp(maskSoft, maskHard, smoothstep(0.1, 0.4, a));   // c43: hard nearly all along (the clip's tails are jagged, not airbrushed)
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
                float hd = max(dy - w, 0.0) / max(2.0 * prof + 0.6, 1e-3);   // c24: tight
                float halo = exp(-hd * hd * 2.0) * step(0.05, prof) * (1.0 - mask) * saturate((BEAM_PAD - 1.5 - dy) * 0.5) * saturate(a * 7.0) * tip;   // c23: fades with the tail and the tip (c22: the quad's start edge cut it square)
                halo *= lerp(0.4, 1.0, saturate(glow)) * (1.0 + 0.2 * nearF) * (i.more.z > 0.5 ? 1.0 : flick) * lerp(0.25, 1.0, longW);   // c32: short board arcs barely glow (a dozen halos summed into a cyan blob)   // c24: faint
                // c26: a second, wide and faint airbrush glow (the clip's bolts have presence at full frame without a fat core)
                float hd2 = max(dy - w, 0.0) / max(5.0 * prof + 2.0, 1e-3);
                float halo2 = exp(-hd2 * hd2) * step(0.05, prof) * (1.0 - mask) * saturate((BEAM_PAD - 1.0 - dy) * 0.7) * saturate(a * 7.0) * tip * lerp(0.5, 1.0, saturate(glow)) * (i.more.z > 0.5 ? 0.0 : flick);
                half3 add = CelPal(row, 0) * (halo * 0.22 + halo2 * 0.06 * longW) * i.tone.z * (1.0 - i.more.z);   // c45: the clip's rods have hardly any halo (c44 read as a wire in a glow)   // c36: less again (two bolts converging on the board still summed into a glowing rope)
                // the far thread is all light: no cover, it adds
                if (i.more.z > 0.5) { add += body * mask * i.tone.z; cover = 0.0; straight = 0; }
                add = AddFog(add, i.positionWS, i.more.x, i.more.y);
                return half4(straight * cover + add, cover);
            }
            ENDHLSL
        }
    }
}
