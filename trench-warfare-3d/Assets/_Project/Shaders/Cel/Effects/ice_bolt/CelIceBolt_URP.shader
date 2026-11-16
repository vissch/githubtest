// Phase: CelFx ice_bolt (2026-11-15, lane/show/cel-ice-bolt) - IceBolt: the rapid ice bolt of the owner's clip
// ice_bolt_nolora (H3 round 6): a volley of ice javelins down the blue_laser corridor (bottom-left corner to the
// dummy's chest) over a braid of pale mist ribbons, a frosty white bloom on the board with crisp starbursts, shards
// that glance off to the right, white chips, and frost mist that lingers. One kind, one indirect draw.
// Built from blue_laser's LightningBeam (the straight-span window strip and the camera-facing dots); every shape here
// is the ice clip's (celfx job folder: spec/eye/ice_bolt.md).
//
// A STRIP record (size.w > 0) lies along the straight span r.pos -> r.pos + r.vel x r.life (CelAim.Target, Speed 0,
// Arrive 1), facing the eye:
//   extra.x 0  DART: a hard-edged frosty sky-blue crystal javelin (two mottled facets, white glints, jags that
//              ride with it) in a window that runs down the span
//              size.xyz the width at its barbs (m) at birth, death, ease; size.w the window's length (share of the span)
//              extra.z / extra.w the window's head at birth / death (shares); tone.w the clip (1: it stops in the target)
//   extra.x 1  RIBBON: a soft flat pale mist ribbon (a brighter twisting edge; it fades in over its last 0.7 m to the
//              head) from a tail share (erode.w) to a head that flies at extra.w shares a
//              second from extra.z, its centre line waving across the corridor as a function of the metres along the
//              span (so it stays where it was drawn), drifting at motion.z rad/s, pulled into the target at the end
//              size.xyz the strand's width (m); size.w the wavelength (m); extra.y the wave's amplitude (m)
// A DOT record (size.w < 0) is a quad at CelPosition (it moves), radius size.xyz (m), pulled erode.w metres toward
// the eye (so the board does not cut it):
//   extra.x 2  BLOOM: an opaque white core in a frosty cloud that falls off as a power to a lumpy boiling rim,
//              glitter, a faint additive halo; y the fall's power, z the core's radius (share of the quad; 0 none),
//              w rim glitter; it fades evenly as it erodes while the core shrinks
//           3  STAR: a crisp ice daisy of fat rounded petals; y a petal's half-width (quad units), z petals, w core
//           4  FLECK: a small white flake (a rounded drop) along its screen velocity; y length over width
//           5  SHARD: a crystal shard (a short barbed dart) along its screen velocity, tumbling at motion.z; y length
//              over width
//           6  FROST: a cluster of soft bright clumps with dark gaps (snow on glass) over a faint mist; y clump cells
//              per quad radius, z density
//           7  SPECK: a tiny twinkling white dot
//           8  PUFF: a soft translucent mist puff; y lumpiness
// Colours: c0 deep rim, c1 lower facet / halo, c2 body, c3 light facet; core = the highlight (x tone.y). The palette is
// written as the frame buffer has to hold it for the Range's night grade to show the clip's colours (IceBoltRecipe.W).
// _TWIceLens (global; IceBoltRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0 none.
Shader "TW/Cel/IceBolt"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+22" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelIceBolt"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            // the quad is this many half-widths to each side of a dart (its barbs reach 1) and of a ribbon (room for
            // its specks and puffs)
            #define DART_PAD 1.3
            #define RIBBON_PAD 5.0
            // a ribbon's half-width at most this many screen px (i20: the clip's strands stay 12-16 px all along the
            // corridor; i19's widened with the perspective into blurry bands near the camera)
            // (i21 24: i20's 13 with the twist's pinch made thin wires, ribbons px 0.69 of the clip's)
            #define RIBBON_MAX_PX 24.0
            // how much of its core a bloom loses as it erodes (fitted with the bloom, i03)
            #define BLOOM_SHRINK 1.000

            float4 _TWIceLens;

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 q : TEXCOORD0;                     // strip: x 0..1 tail to head, y across (half-widths), z metres along the span, w half-width (m). dot: xy -1..1, z seed, w -1
                nointerpolation float4 tone : TEXCOORD1;  // x palette, y hdr, z opacity, w erosion
                nointerpolation float4 shape : TEXCOORD2; // x mode, y z w the mode's parameters
                float4 more : TEXCOORD3;                  // x fog, y life 0..1, z share along the span, w seed
                float3 positionWS : TEXCOORD4;
            };

            // metres per screen pixel at a world point
            float PixelMetres(float3 p)
            {
                float d = distance(_WorldSpaceCameraPos, p);
                return d * 2.0 / max(abs(UNITY_MATRIX_P[1][1]) * _ScreenParams.y, 400.0);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float k = CelLife01(r), alive = CelAlive(r);
                float age = max(CelAge(r), 0.0);
                float mode = r.extra.x;
                Varyings o = (Varyings)0;
                o.more = float4(0.0, k, 0.0, r.seed);
                if (r.size.w < 0.0)
                {
                    float3 c = CelPosition(r, age);
                    c += normalize(_WorldSpaceCameraPos - c) * r.erode.w;
                    float rad = CelCurve(r.size.xyz, k) * alive;
                    float2 corner = v.uv * 2.0 - 1.0;
                    float3 vx = UNITY_MATRIX_V[0].xyz;
                    float3 vy = UNITY_MATRIX_V[1].xyz;
                    float2 ext = float2(rad, rad);
                    if (mode > 3.5 && mode < 5.5)
                    {
                        // a fleck or a shard: drawn along its own velocity as the eye sees it, a shard tumbling
                        float3 vel = CelVelocity(r, age);
                        float3 vf = -UNITY_MATRIX_V[2].xyz;
                        float3 dv = vel - dot(vel, vf) * vf;
                        float lv = length(dv);
                        float3 ax = lv > 1e-3 ? dv / lv : vx;
                        float3 ay = normalize(cross(vf, ax));
                        float turn = r.motion.z * age + (lv > 1e-3 ? 0.0 : r.seed * 6.28318);
                        float sn, cs;
                        sincos(turn, sn, cs);
                        vx = ax * cs + ay * sn;
                        vy = ay * cs - ax * sn;
                        ext = float2(rad * max(r.extra.y, 1.0), rad);
                    }
                    float3 wd = c + vx * corner.x * ext.x + vy * corner.y * ext.y;
                    o.positionWS = wd;
                    o.positionCS = TransformWorldToHClip(wd);
                    o.q = float4(corner, r.seed, -1.0);
                    o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                    o.shape = float4(mode, r.extra.y, r.extra.z, r.extra.w);
                    o.more.x = ComputeFogFactor(o.positionCS.z);
                    return o;
                }
                float3 span = r.vel * r.life;
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                float clipS = r.tone.w > 0.0 ? r.tone.w : 1.0;
                float head, s0, winS;
                if (mode < 0.5)
                {
                    // each dart its own length (0.7..1.3 of the window)
                    winS = r.size.w * lerp(0.7, 1.3, CelHash21(float2(r.seed * 7.3, 1.7)));
                    head = lerp(r.extra.z, r.extra.w, k);
                    s0 = max(head - winS, 0.0);
                    o.shape = float4(mode, r.extra.y, winS * lenB, 0.0);
                }
                else
                {
                    head = r.extra.z + r.extra.w * age;
                    s0 = max(r.erode.w, 0.0);
                    winS = max(head - s0, 1e-4);
                    o.shape = float4(mode, r.size.w, r.extra.y, r.motion.z);
                }
                float s1 = min(head, clipS);
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float s = lerp(s0, s1, v.uv.x);
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                // +across is up on the screen, so the light facet is the upper one on any shot
                if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;
                float across = v.uv.y * 2.0 - 1.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                float halfW = max(width * 0.5, 1e-5);
                if (mode > 0.5) halfW = min(halfW, RIBBON_MAX_PX * PixelMetres(p));
                float pad = mode < 0.5 ? DART_PAD : RIBBON_PAD;
                float u = s * lenB;
                float off = 0.0;
                if (mode > 0.5)
                {
                    // the strand's centre line: one long wave and a shorter one over the metres along the span, drifting
                    // slowly, its amplitude pulled in over the last stretch so every strand meets in the target
                    float wl = max(r.size.w, 0.3);
                    float sd = r.seed * 81.7;
                    float drift = r.motion.z * age;
                    float w1 = sin(6.28318 * u / wl + sd + drift);
                    float w2 = sin(6.28318 * u / (wl * 0.55) + sd * 1.7 + 1.3 - drift * 0.6);
                    float taper = pow(saturate((clipS - s) / 0.22), 0.9);
                    off = (0.88 * w1 + 0.12 * w2) * r.extra.y * taper;
                }
                float3 wp = p + side * (off + across * halfW * pad);
                o.positionWS = wp;
                o.positionCS = TransformWorldToHClip(wp);
                // a: 0 at the window's tail, 1 at its head (a window clipped at the target keeps its head's shape)
                float a = saturate((s - (head - winS)) / max(winS, 1e-4));
                o.q = float4(a, across * pad, u, halfW);
                float op = r.tone.z;
                if (mode > 0.5) op *= smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(0.55, 1.0, k));
                o.tone = float4(r.tone.x, r.tone.y, op, CelCurve(r.erode.xyz, k));
                o.more = float4(ComputeFogFactor(o.positionCS.z), k, s, r.seed);
                return o;
            }

            // the colour as it has to be written for the lens vignette to leave it as it is (NeonAcid's, fitted to a
            // film of flat swatches on this Range)
            half3 Unvignette(half3 col, float2 pixel)
            {
                if (_TWIceLens.x <= 0.0) return col;
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWIceLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWIceLens.y, 0.01)));
                float lift = 1.0 / keep;
                float3 lin = SRGBToLinear(max((float3)col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float gain = max(min(lift, 1.0 / max(top, 1e-3)), 1.0);
                return (half3)LinearToSRGB(min(lin * min(lift, gain * 1.2), 1.0));
            }

            // one edge of an ICICLE dart (the clip at 4x), in half-widths of its widest point, at a (0 tail .. 1 head): a
            // needle point straight from the tip (a = 1) back to the widest point hb, then a short broken stub that eases
            // in to `back` and comes to a ragged end at a = 0
            float IceEdge(float a, float hb, float w0, float back)
            {
                float tip = w0 * saturate((1.0 - a) / max(1.0 - hb, 0.05));
                float stub = lerp(back, w0, smoothstep(hb * 0.35, hb, a)) * saturate(a / max(hb * 0.45, 0.02));
                return a >= hb ? tip : stub;
            }

            // inside a hook that sweeps back from the widest point (y >= 0 measured on its own side): its outer edge
            // runs from (hb, w0) back to its point at (hb - len, 0.92 w0), its inner edge from the point forward into
            // the stub, so a gap stands between the hook and the stub (a barb, not a notch); > 0 inside
            float IceHook(float a, float y, float hb, float w0, float back, float len)
            {
                float t = (a - (hb - len)) / max(len, 1e-3);
                float outer = lerp(0.92 * w0, w0, saturate(t));
                float inner = lerp(0.92 * w0, back, saturate(t / 0.8));
                float inA = min(t, 1.0 - t + 0.02);
                return min(min(outer - y, y - inner), inA * len * 6.0);
            }

            // one strand's cross-section (i17/i18, the clip's measured over ~1900 crossings: from the peak the light
            // halves within 3-4 px on the hard side and over ~8 px on the soft, 15 % at 24 px), in half-widths t from
            // its hard side (+1 the hard edge): full from 0.3 to 0.55 (i19: a bright body by the hard edge - i18's
            // spike of a peak read as thin lines at the film's size beside the clip's bold ribbons), eased down over
            // the last 0.45 to a faint halo, the soft side falling off as exp(-d / 0.8)
            // i22: cel-flat, not airbrushed (the clip's ribbons at 1:1 are painted: a bright line along the hard edge,
            // a flat semi-opaque face across the ribbon, a defined dimmer far edge; i21's exponential soft side glowed
            // like a neon tube): the line 0.75-0.9, rising from the face over 0.55-0.75 and cut over 0.9-1.0; the face
            // 0.5 from -0.75 to 0.55; the far edge falling over -1.0..-0.75
            float StrandProfile(float t)
            {
                if (t >= 0.9) return 1.0 - smoothstep(0.9, 1.0, t);
                // (i23: the face 0.7 - i22's 0.5 read as see-through grey cellophane beside the clip's white faces)
                if (t >= 0.55) return lerp(0.7, 1.0, smoothstep(0.55, 0.75, t));
                return 0.7 * smoothstep(-1.0, -0.75, t);
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                float mode = i.shape.x;
                float thr = saturate(i.tone.w);
                float tick = floor(_CelNow * 24.0);
                half3 col = CelPal(row, 2);
                half3 add = half3(0.0, 0.0, 0.0);
                float cover = 0.0;
                if (i.q.w < 0.0)
                {
                    float2 qv = i.q.xy;
                    float rr = length(qv);
                    float sd = i.q.z;
                    if (mode < 2.5)
                    {
                        // BLOOM (i03, fitted to the clip's vertical opacity profile f16-f132 in a model of this very
                        // fragment over the clip's own plate): opaque at the core's edge (z: its radius, a share of the
                        // quad, so it grows with the cloud), falling as (1 - t)^y out to a lumpy edge that boils frame
                        // to frame; the core white, frost grain toward the edge, glitter in the rim, a faint blue
                        // halo; it fades evenly (x (1 - erosion)) while the opaque core shrinks, holes opening late
                        float n1 = CelValue2(qv * 2.1 + float2(sd * 37.0, sd * 11.0));
                        float n2 = CelValue2(qv * 4.7 + float2(sd * 13.0 + tick * 0.37, sd * 29.0 - tick * 0.23));
                        float n3 = CelValue2(qv * 11.0 + float2(sd * 7.0 - tick * 0.11, sd * 3.0 + tick * 0.17));
                        float lumpR = rr * (1.0 + 0.34 * (n1 - 0.5) + 0.2 * (n2 - 0.5));
                        float zc = max(i.shape.z, 0.0) * (1.0 - BLOOM_SHRINK * thr);
                        float t = saturate((lumpR - zc) / max(1.0 - zc, 1e-3));
                        float cloud = pow(1.0 - t, max(i.shape.y, 0.2));
                        float shown = saturate(cloud * 5.0);
                        // frost grain (two scales, ~8 and ~4 px at the hold) and an outer half broken into clumps with
                        // small dark gaps between them
                        float n4 = CelValue2(qv * 23.0 + float2(sd * 17.0 + tick * 0.21, sd * 5.0 - tick * 0.13));
                        float grain = 0.6 * n3 + 0.4 * n4;
                        cloud *= lerp(1.0, lerp(0.55, 1.25, grain), smoothstep(0.0, 0.4, t));
                        cloud *= saturate((grain + 0.55 - t * 0.75) * 3.0);
                        float holes = saturate((0.6 * n2 + 0.4 * n3 + 0.75 - thr * 1.3) * 4.0);
                        cloud = saturate(cloud) * (1.0 - thr) * holes;
                        float coreM = (1.0 - smoothstep(zc * 0.55, zc * 1.05, rr * (1.0 + 0.16 * (n2 - 0.5)))) * (1.0 - thr);
                        float edgeIn = max(zc, 0.3), edgeOut = 1.0 - 0.25 * thr;
                        // glitter: a share of small cells in the rim holds a white point, dealt again every other frame
                        float2 gc = qv * 18.0;
                        float2 gi = floor(gc);
                        float deal = floor(tick * 0.5);
                        float gh = CelHash21(gi + sd * 71.0 + deal * 0.731);
                        float2 gp = gi + 0.25 + 0.5 * float2(CelHash21(gi + 3.1 + sd + deal), CelHash21(gi + 5.7 + sd - deal));
                        float rim = smoothstep(edgeIn * 0.5, edgeIn, rr) * (1.0 - smoothstep(edgeOut * 0.9, edgeOut * 1.1, lumpR));
                        // (i14 sparser: i13's read as a field of blue dots round the cloud)
                        float glit = (gh > 0.9 ? 1.0 : 0.0) * saturate(1.0 - length(gc - gp) / 0.32) * rim * i.shape.w * (1.0 - thr) * shown;
                        col = lerp(CelPal(row, 2), CelPal(row, 3), saturate(n3 * 0.7 + (1.0 - rr) * 0.3));
                        col = lerp(col, CelPalCore(row).rgb * i.tone.y, saturate(max(coreM, glit)));
                        cover = saturate(max(cloud, glit)) * i.tone.z;
                        float halo = exp(-rr * rr * 2.2) * saturate(1.0 - rr * 0.75) * (1.0 - cover) * 0.08 * (1.0 - thr);
                        add = CelPal(row, 1) * halo * i.tone.z;
                    }
                    else if (mode < 3.5)
                    {
                        // STAR (the clip at 3x f16-f17, f87-f94): z fat rounded PETALS of a little uneven length round a
                        // white core, like a daisy of ice; y a petal's half-width at its widest (quad units), w the core
                        float nS = max(i.shape.z, 5.0);
                        float turn = frac(atan2(qv.y, qv.x) * 0.159155 + 0.5 + sd * 3.7);
                        float sec = turn * nS;
                        float idS = floor(sec);
                        float lenS = lerp(0.62, 1.0, CelHash21(float2(idS, sd * 53.0 + 7.0)));
                        float offS = (CelHash21(float2(idS, sd * 91.0)) - 0.5) * 0.3;
                        float lat = abs(sec - idS - 0.5 - offS) * 6.28318 / nS * rr;
                        // an elliptical petal from near the centre to lenS, widest a little past its middle
                        float pc = lenS * 0.56, ph = lenS * 0.46;
                        float wS = max(i.shape.y, 0.01) * sqrt(saturate(1.0 - (rr - pc) * (rr - pc) / max(ph * ph, 1e-4)));
                        float coreS = max(i.shape.w, 0.05);
                        float dS = max(wS - lat, coreS - rr);
                        float m = CelAAStep(0.0, dS) * step(rr, 1.0);
                        col = lerp(CelPal(row, 3), CelPalCore(row).rgb * i.tone.y, saturate(1.0 - rr / max(coreS * 2.5, 0.05)));
                        cover = m * i.tone.z * (1.0 - thr);
                    }
                    else if (mode < 4.5)
                    {
                        // FLECK (the clip at 3x): a small white chip, a teardrop along its motion (round ahead, drawn
                        // to a point behind), its tail a little crooked per chip, whitest on its upper side
                        float bend = (CelHash21(float2(sd * 11.3, 3.7)) - 0.5) * 0.5;
                        float dF = qv.x > 0.0 ? 1.0 - length(qv) : 1.0 - (abs(qv.x) * 0.9 + abs(qv.y + bend * qv.x) * 1.6);
                        float m = CelAAStep(0.0, dF);
                        col = lerp(CelPal(row, 3), CelPalCore(row).rgb * i.tone.y, saturate(0.65 + qv.y));
                        cover = m * i.tone.z * (1.0 - thr);
                    }
                    else if (mode < 5.5)
                    {
                        // SHARD: a short barbed crystal along its motion, two-tone like the darts
                        float a = qv.x * 0.5 + 0.5;
                        float y = qv.y;
                        float h1 = CelHash21(float2(sd * 13.1, 2.3)), h2 = CelHash21(float2(sd * 17.9, 4.1));
                        float hb = lerp(0.35, 0.6, h1);
                        float upW = IceEdge(a, hb, 1.0, lerp(0.35, 0.55, h2));
                        float dnW = IceEdge(a, hb - 0.08, lerp(0.6, 0.95, h2), lerp(0.3, 0.5, h1));
                        float eW = y >= 0.0 ? upW : dnW;
                        float m = CelAAStep(0.0, eW - abs(y));
                        col = y >= 0.1 * eW ? CelPal(row, 3) : CelPal(row, 1);
                        cover = m * i.tone.z * (1.0 - thr);
                    }
                    else if (mode < 6.5)
                    {
                        // FROST (i03, the clip at 2-3x f98-f156): fluffy irregular snow clumps of many sizes with dark
                        // gaps (three octaves of value noise cut at a level that is lower toward the middle), a
                        // crystalline grain, white specks, over a faint grey mist, in a lumpy envelope that thins from
                        // its edge as it erodes; y the clumps' scale (per quad radius), z density
                        float sc = max(i.shape.y, 1.0);
                        float f1 = CelValue2(qv * sc + float2(sd * 19.0, sd * 7.0));
                        float f2 = CelValue2(qv * sc * 2.3 + float2(sd * 23.0 + 5.1, sd * 3.0));
                        float f3 = CelValue2(qv * sc * 5.3 + float2(sd * 29.0, sd * 13.0 + 2.7));
                        float fb = f1 * 0.52 + f2 * 0.3 + f3 * 0.18;
                        float lumpR = rr * (1.0 + 0.34 * (CelValue2(qv * 2.0 + sd * 7.0) - 0.5));
                        float env = 1.0 - smoothstep(0.3 * (1.0 - 0.4 * thr), 1.0 - 0.3 * thr, lumpR);
                        float cut = lerp(0.6, 0.38, env) + thr * 0.12;
                        float clump = smoothstep(cut, cut + 0.22, fb) * env;
                        float mist = env * env * 0.34;
                        // white specks: a share of small cells holds a 1-2 px point
                        float2 kc = qv * sc * 4.0;
                        float2 ki = floor(kc);
                        float kh = CelHash21(ki + sd * 37.0);
                        float2 kp = ki + 0.2 + 0.6 * float2(CelHash21(ki + sd * 41.0 + 1.3), CelHash21(ki + sd * 43.0 + 2.9));
                        float speck = (kh > 0.8 ? 1.0 : 0.0) * saturate(1.0 - length(kc - kp) / 0.3) * env;
                        cover = saturate(max(max(clump * 0.85, mist), speck) * max(i.shape.z, 0.1)) * (1.0 - thr) * i.tone.z;
                        col = lerp(CelPal(row, 2), CelPal(row, 3), saturate((fb - cut) * 3.0 + (f3 - 0.5) * 0.6));
                        col = lerp(col, CelPalCore(row).rgb * i.tone.y, saturate(speck));
                    }
                    else if (mode < 7.5)
                    {
                        // SPECK: a tiny white dot that twinkles
                        float tw = CelHash21(float2(sd * 41.0, tick)) > 0.3 ? 1.0 : 0.35;
                        cover = CelAAStep(0.0, 1.0 - rr) * tw * i.tone.z * (1.0 - thr);
                        col = CelPalCore(row).rgb * i.tone.y;
                    }
                    else
                    {
                        // PUFF: a soft translucent mist puff with a lumpy rim
                        float ang = atan2(qv.y, qv.x);
                        float lump = 1.0 - i.shape.y * (0.5 + 0.5 * sin(ang * 5.0 + sd * 37.0)) * 0.3;
                        float g = exp(-rr * rr * 3.0 / max(lump * lump, 0.2)) * saturate(1.0 - rr);
                        cover = g * i.tone.z * (1.0 - thr);
                        col = lerp(CelPal(row, 2), CelPal(row, 3), g);
                    }
                }
                else if (mode < 0.5)
                {
                    // DART (i16, the clip's darts at 2x f12-f108 against i15's): a HARPOON - a long straight needle
                    // point, swept wings at the widest point 0.3-0.45 from the back (i15: 0.22-0.42 on a stub 0.33-0.55
                    // wide, a thick notched back that read as a paper dart), a thin shaft behind them that fades into
                    // the stream, a smaller second wing behind the first on the lower edge of a quarter (the clip's
                    // f84); clean cut edges; two-tone, a deep blue line along the lower edge
                    float a = i.q.x, y = i.q.y;
                    float sd = i.more.w;
                    float h1 = CelHash21(float2(sd * 13.1, 2.3)), h2 = CelHash21(float2(sd * 17.9, 4.1)), h3 = CelHash21(float2(sd * 5.3, 7.7));
                    float h4 = CelHash21(float2(sd * 23.7, 9.1));
                    float hb = lerp(0.3, 0.45, h1);
                    float w0u = lerp(0.75, 1.0, h3), w0d = lerp(0.85, 1.0, h2);
                    // i20: half are broad blades - the back a solid body, not a thin shaft (the clip's near the camera
                    // at 2x: fat lumpy ice chunks, a long straight top, a lumpy underside)
                    // (i22: on 30 % - the clip's are mostly crisp barbed arrowheads at 1:1)
                    bool blade = h4 >= 0.7;
                    float shU = blade ? lerp(0.6, 0.85, h2) : lerp(0.16, 0.26, h2), shD = blade ? lerp(0.55, 0.8, h3) : lerp(0.16, 0.26, h3);
                    // the needle straight (on some a little hollow) from the tip back to the widest point; behind it
                    // the body steps in to the shaft at once, and the shaft thins over its last 12 %
                    float tip = pow(saturate((1.0 - a) / max(1.0 - hb, 0.05)), lerp(1.0, 1.15, h2));
                    float bodyT = smoothstep(hb - lerp(0.04, 0.08, h3), hb, a);
                    float endT = lerp(0.35, 1.0, saturate(a / (blade ? 0.3 : 0.12)));
                    float upW = a >= hb ? w0u * tip : lerp(shU, w0u, bodyT) * endT;
                    float dnW = a >= hb ? w0d * tip : lerp(shD, w0d, bodyT) * endT;
                    // i09: in its own frame (metres from its tail), so its jags and frost ride with it instead of
                    // crawling through it as it flies
                    float ud = a * i.shape.z;
                    // (i17 0.12 and 0.05: the clip's edges are broken at 1:1; i15 0.16/0.06, i16 0.06/0.03)
                    float cellL = max(i.q.w * 0.9, 1e-3);
                    float rag = (CelValue2(float2(ud / cellL, sd * 31.0 + step(0.0, y) * 7.0)) - 0.5) * 0.08
                              + (CelHash21(float2(floor(ud / (cellL * 0.6)), sd * 37.0 + step(0.0, y))) - 0.5) * 0.04
                              // i18: fine bumps ~0.36 half-widths long (the clip's lumpy frosty edges at 2x)
                              + (CelValue2(float2(ud / (cellL * 0.4), sd * 43.0 + step(0.0, y) * 5.0)) - 0.5) * 0.04
                              // i20: long lumps ~2 half-widths (the clip's lumpy ice chunks), more on the underside
                              + (CelValue2(float2(ud / (cellL * 2.2), sd * 47.0 + step(0.0, y) * 3.0)) - 0.5) * (y >= 0.0 ? 0.06 : 0.1);
                    // (i23: the outline cut back - i22's lumps made blobby snow shards, the clip's are crisp arrowheads)
                    float eW = y >= 0.0 ? upW : dnW;
                    float dE = eW + rag * step(0.05, eW) - abs(y);
                    // the wings: swept back from the widest point, on the lower edge of 60 % and the upper of 30 %
                    // (i17: i16's on nearly all read as paper planes at 1:1; the clip's long shards mostly have none)
                    float hookLen = min(lerp(0.1, 0.22, h3), hb * 0.7);
                    float hookOn = y >= 0.0 ? step(0.7, h2) : step(h1, 0.6);
                    if (hookOn > 0.5 && a < hb)
                        dE = max(dE, IceHook(a, abs(y), hb, y >= 0.0 ? w0u : w0d, eW, hookLen));
                    float hb2 = hb - hookLen * 1.15;
                    if (h4 < 0.25 && y < 0.0 && a < hb2)
                        dE = max(dE, IceHook(a, abs(y), hb2, w0d * 0.72, eW, hookLen * 0.75));
                    float inside = CelAAStep(0.0, dE);
                    float hwPx = i.q.w / max(PixelMetres(i.positionWS), 1e-6);
                    float onePx = 1.0 / max(hwPx, 0.5);
                    // a frosty sky-blue crystal (the clip at 3x, f11-f48): its two facets mottled lighter and deeper
                    // (value noise ~1.6 half-widths along by ~0.8 across), the boundary between them broken
                    // (i17: i15's broken boundary back - i16's clean fold read as folded paper at 1:1)
                    float2 mq = float2(ud / max(i.q.w * 1.6, 1e-3), y * 1.2) + float2(sd * 41.0, sd * 17.0);
                    float mn = 0.65 * CelValue2(mq) + 0.35 * CelValue2(mq * 2.3 + float2(5.1, 1.7));
                    float ridge = (0.12 + (mn - 0.5) * 0.8) * eW;
                    bool upper = y >= ridge;
                    // (i17: the upper facet 0.86 -> 0.8: i16 filmed it (165,193,221) for the clip's (155,180,211))
                    // i18: frost in it, snowy patches ~0.55 half-widths across (the clip's darts at 2x)
                    float fr = CelValue2(float2(ud / max(i.q.w * 0.55, 1e-3), y * 2.2) + float2(sd * 59.0, sd * 23.0));
                    // (i19: the upper facet 0.74: i18 filmed the darts p50 (159,190,227) for the clip's (153,180,213))
                    // (i20 frost 0.5: the clip's chunks are white mottled over deeper blue at 2x)
                    // (i22 frost 0.3 in whole-band steps: i21's 0.7 in half bands made blotchy lumps; the clip's are
                    // two flat tones with a few white patches)
                    // (i24: the lower facet 0.6 and the frost 0.15: i23's dark frost patches dulled the darts grey-blue,
                    // the clip's are bright white to light blue)
                    // (i25 0.85 / 0.42: i24's 0.74 / 0.6 rounded both facets into one flat white band; the clip's are
                    // white over light blue)
                    float shade = saturate((upper ? 0.85 : 0.42) + (mn - 0.5) * 0.5 + (fr - 0.5) * 0.15) * 3.0;
                    // i21: in hard half-band steps - the clip's chunks are hard white snow patches over blue-grey shadow
                    shade = min(floor(shade + 0.5), 3.0);
                    col = shade < 1.0 ? lerp(CelPal(row, 0), CelPal(row, 1), shade) : shade < 2.0 ? lerp(CelPal(row, 1), CelPal(row, 2), shade - 1.0) : lerp(CelPal(row, 2), CelPal(row, 3), shade - 2.0);
                    // a few white glints in it (~2 px)
                    float2 gq = float2(ud / max(i.q.w * 0.8, 1e-3), y * 1.5);
                    float2 gcl = floor(gq);
                    float gh = CelHash21(gcl + float2(sd * 7.0, sd * 3.0));
                    float2 gp = gcl + 0.5 + (float2(CelHash21(gcl + float2(sd * 3.1, 1.3)), CelHash21(gcl + float2(sd * 5.3, 2.9))) - 0.5) * 0.5;
                    float glint = (gh > 0.88 ? 1.0 : 0.0) * saturate(1.4 - length((gq - gp) * float2(hwPx * 0.8, hwPx / 1.5)));
                    // a bright line just inside the upper edge, a deeper rim on the lower
                    // (i18: no white line inside the upper edge - it drew the darts as clean paper at 1:1)
                    if (!upper && dE < onePx * 1.0) col = CelPal(row, 0);
                    col = lerp(col, CelPalCore(row).rgb * i.tone.y, glint);
                    // the shaft fades into the stream
                    float tailFade = lerp(0.45, 1.0, saturate(a / 0.35));
                    cover = inside * i.tone.z * tailFade * (1.0 - thr);
                }
                else
                {
                    // RIBBON: a soft pale strand that twists (narrow edge-on, wide flat), a brighter line down its middle,
                    // freshest by the head; soft mist puffs along it and frost specks beside it
                    float y = i.q.y, u = i.q.z;
                    float sd = i.more.w;
                    float wl = max(i.shape.y, 0.3);
                    // i12: it twists every ~3.4 m from a thin edge-on line to a wide flat sheet (the clip's streaks)
                    float twc = cos(6.28318 * u / 3.4 + sd * 23.0);
                    // (i21: pinched to 0.55 at the twist, not 0.35: the clip's are flat ribbons, i20's wires)
                    float tw = 0.55 + 0.45 * abs(twc);
                    float hw = tw * (1.0 - 0.6 * thr);
                    // i17: a HARD edge and a SOFT one (the clip's strands at 3x: twisted ribbons lit from one side, a
                    // firm bright fold along one edge, the body fading off from it across the strand and on past its
                    // other edge as a glow; i16's peaked strand was soft on both sides). The hard edge is the side the
                    // twist turns toward the eye. i18: the soft side narrower (i17's exp scale 1.35 glowed out to 15 %
                    // at 45-56 px for the clip's 24, hazing the corridor into glassy sheets), the hard edge eased over
                    // 0.45 (i17's 0.3 at full contrast read as glass), and the two sides blend across the twist where
                    // the strand is edge-on (i17's flip kinked it like a folded strip)
                    float tA = y / max(hw, 1e-3);
                    float flip = smoothstep(-0.3, 0.3, twc);
                    float g = lerp(StrandProfile(-tA), StrandProfile(tA), flip);
                    // i24: dry-brush streaks along the face (the clip's ribbons at 1:1: light and dark streaks running
                    // along the stroke; i23's smooth faces read as tape), long along the strand, ~1/6 of it across
                    float streak = CelValue2(float2(u / 0.9, tA * 3.0) + float2(sd * 29.0, sd * 13.0));
                    g *= lerp(0.55, 1.15, streak);
                    // i25: thinner over the muzzle end (the first 15 % of the span): near the camera i24 drew twice the
                    // clip's px
                    g *= lerp(0.5, 1.0, smoothstep(0.0, 0.15, i.more.z));
                    // (i22: the edge line at 0.82, on the profile's bright line)
                    float eP = (tA - 0.82) / 0.1, eM = (-tA - 0.82) / 0.1;
                    float edge = lerp(exp(-eM * eM), exp(-eP * eP), flip) * (1.0 - tw * 0.6);
                    float fresh = lerp(0.7, 1.0, smoothstep(0.6, 1.0, i.q.x));
                    float alpha = (g * 0.9 + edge * 0.2) * fresh * lerp(1.1, 0.9, tw);
                    // (i13: its lit edge no longer whitened - i12's read as the darts' white at the film's size)
                    // (i18 0.3 again: i17's 0.5 made the brighter cores dart-blue, merging them with the darts)
                    // (i22: the line 0.4 toward c3, the face c2)
                    col = lerp(CelPal(row, 2), CelPal(row, 3), saturate(edge * 0.4 + (1.0 - tw) * 0.2));
                    // its head fades in over the last 0.7 m (i09's wakes ended in a hard cut behind their darts);
                    // metres behind the head = u (1/a - 1), a running 0..1 from the tail (share 0) to the head
                    float behindM = i.q.z * (1.0 / max(i.q.x, 1e-3) - 1.0);
                    float headFade = smoothstep(0.0, 0.7, behindM);
                    // i11: the white spray just behind its dart (the clip at 3x f20-f48: 20-30 1-3 px points round and
                    // behind every dart) - denser, bigger and closer in over the first 2.5 m, then i09's sparse frost
                    float spray = 1.0 - smoothstep(0.6, 2.5, behindM);
                    float pm = PixelMetres(i.positionWS);
                    float hwPx = i.q.w / max(pm, 1e-6);
                    // a soft mist puff in some cells along the strand
                    float pc = 0.9;
                    float pcell = floor(u / pc);
                    float ph = CelHash21(float2(pcell, sd * 43.0));
                    if (ph > 0.62)
                    {
                        float px0 = (pcell + 0.2 + 0.6 * CelHash21(float2(pcell, sd * 47.0))) * pc;
                        float py0 = (CelHash21(float2(pcell, sd * 53.0)) - 0.5) * 2.0;
                        float pr = lerp(2.0, 3.5, CelHash21(float2(pcell, sd * 59.0)));
                        float2 dP = float2((u - px0) / max(i.q.w, 1e-5), y - py0) / pr;
                        float puff = exp(-dot(dP, dP) * 2.2) * 0.2;
                        alpha = max(alpha, puff);
                    }
                    // frost specks: a share of 0.22 m cells holds a 1-2 px white dot off the strand, twinkling
                    // (i12 denser and bigger: the clip shows dozens round the darts and along the streaks)
                    float sc = 0.16;
                    float si = floor(u / sc);
                    float sh = CelHash21(float2(si, sd * 61.0));
                    float speck = 0.0;
                    // (i25: more by the darts - the clip throws dozens of white specks round them)
                    if (sh > lerp(0.55, 0.05, spray))
                    {
                        float sx = (si + 0.2 + 0.6 * CelHash21(float2(si, sd * 67.0))) * sc;
                        float sgn = CelHash21(float2(si, sd * 71.0)) > 0.5 ? 1.0 : -1.0;
                        float sy = sgn * lerp(lerp(1.2, 0.5, spray), 3.5, CelHash21(float2(si, sd * 73.0)));
                        float spx = length(float2((u - sx) / max(pm, 1e-6), (y - sy) * hwPx));
                        float rad = lerp(1.0, 2.0, CelHash21(float2(si, sd * 79.0))) * (1.0 + 0.5 * spray);
                        float twk = CelHash21(float2(si + sd * 83.0, tick)) > 0.35 ? 1.0 : 0.3;
                        speck = saturate(rad + 0.5 - spx) * twk;
                    }
                    alpha *= headFade;
                    speck *= headFade;
                    cover = saturate(alpha * i.tone.z);
                    col = lerp(col, CelPalCore(row).rgb * i.tone.y, speck);
                    cover = max(cover, speck * saturate(i.tone.z * 1.6));
                }
                col = Unvignette(col, i.positionCS.xy);
                col = MixFog(col, i.more.x);
                add = (half3)CelAdditiveFog(add, i.positionWS, i.more.x);
                return half4(col * cover + add, cover);
            }
            ENDHLSL
        }
    }
}
