// Phase: CelFx ice_bolt (2026-11-15, lane/show/cel-ice-bolt) - IceBolt: the rapid ice bolt of the owner's clip
// ice_bolt_nolora (H3 round 6): a volley of ice javelins down the blue_laser corridor (bottom-left corner to the
// dummy's chest) over a braid of pale mist ribbons, a frosty white bloom on the board with crisp starbursts, shards
// that glance off to the right, white chips, and frost mist that lingers. One kind, one indirect draw.
// Built from blue_laser's LightningBeam (the straight-span window strip and the camera-facing dots); every shape here
// is the ice clip's (celfx job folder: spec/eye/ice_bolt.md).
//
// A STRIP record (size.w > 0) lies along the straight span r.pos -> r.pos + r.vel x r.life (CelAim.Target, Speed 0,
// Arrive 1), facing the eye:
//   extra.x 0  DART: a hard-edged two-tone crystal javelin in a window that runs down the span
//              size.xyz the width at its barbs (m) at birth, death, ease; size.w the window's length (share of the span)
//              extra.z / extra.w the window's head at birth / death (shares); tone.w the clip (1: it stops in the target)
//   extra.x 1  RIBBON: a soft pale mist strand from a tail share (erode.w) to a head that flies at extra.w shares a
//              second from extra.z, its centre line waving across the corridor as a function of the metres along the
//              span (so it stays where it was drawn), drifting at motion.z rad/s, pulled into the target at the end
//              size.xyz the strand's width (m); size.w the wavelength (m); extra.y the wave's amplitude (m)
// A DOT record (size.w < 0) is a quad at CelPosition (it moves), radius size.xyz (m), pulled erode.w metres toward
// the eye (so the board does not cut it):
//   extra.x 2  BLOOM: a soft frosty white disc that covers, a white core, an additive halo; y edge softness,
//              z core share, w rim glitter
//           3  STAR: a crisp ice starburst; y spike root half-width (quad units), z spikes, w core share
//           4  FLECK: a small hard chip stretched along its screen velocity; y length over width
//           5  SHARD: a crystal shard (a short barbed dart) along its screen velocity, tumbling at motion.z; y length
//              over width
//           6  FROST: a soft frosty mist patch with crystalline veins; y vein scale, z density
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
            #define RIBBON_PAD 7.0

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
                    float w2 = sin(6.28318 * u / (wl * 0.41) + sd * 1.7 + 1.3 - drift * 0.6);
                    float taper = pow(saturate((clipS - s) / 0.55), 0.9);
                    off = (0.75 * w1 + 0.25 * w2) * r.extra.y * taper;
                }
                float3 wp = p + side * (off + across * halfW * pad);
                o.positionWS = wp;
                o.positionCS = TransformWorldToHClip(wp);
                // a: 0 at the window's tail, 1 at its head (a window clipped at the target keeps its head's shape)
                float a = saturate((s - (head - winS)) / max(winS, 1e-4));
                o.q = float4(a, across * pad, u, halfW);
                float op = r.tone.z;
                if (mode > 0.5) op *= smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(0.4, 1.0, k));
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
                        // BLOOM: a frosty white disc with a lumpy soft rim, a white core, glitter in the rim, a halo
                        float n1 = CelValue2(qv * 3.1 + float2(sd * 37.0, sd * 11.0));
                        float n2 = CelValue2(qv * 7.3 + float2(sd * 13.0 + tick * 0.37, sd * 29.0));
                        float edgeR = (0.74 + 0.32 * (n1 - 0.5) + 0.12 * (n2 - 0.5)) * (1.0 - 0.35 * thr);
                        float soft = max(i.shape.y, 0.04);
                        float body = 1.0 - smoothstep(edgeR - soft, edgeR + soft * 0.5, rr);
                        float coreR = i.shape.z * (1.0 - 0.6 * thr);
                        float coreM = 1.0 - smoothstep(coreR * 0.6, coreR, rr + (n2 - 0.5) * 0.08);
                        col = lerp(CelPal(row, 2), CelPal(row, 3), saturate((edgeR - rr) / max(edgeR, 1e-3) * 1.6));
                        col = lerp(col, CelPalCore(row).rgb * i.tone.y, coreM);
                        float2 gc = qv * 16.0;
                        float2 gi = floor(gc);
                        float gh = CelHash21(gi + sd * 71.0);
                        float2 gp = gi + 0.25 + 0.5 * float2(CelHash21(gi + 3.1 + sd), CelHash21(gi + 5.7 + sd));
                        float rim = smoothstep(edgeR - 0.35, edgeR - 0.05, rr) * (1.0 - smoothstep(edgeR + 0.1, edgeR + 0.25, rr));
                        float glit = (gh > 0.86 ? 1.0 : 0.0) * saturate(1.0 - length(gc - gp) / 0.22) * rim * i.shape.w;
                        cover = saturate(body * i.tone.z * (1.0 - thr * 0.7));
                        col = lerp(col, CelPalCore(row).rgb * i.tone.y, saturate(glit));
                        cover = max(cover, saturate(glit) * i.tone.z * (1.0 - thr));
                        float halo = exp(-rr * rr * 4.0) * saturate(1.0 - rr) * (1.0 - body) * 0.35 * (1.0 - thr);
                        add = CelPal(row, 1) * halo * i.tone.z;
                    }
                    else if (mode < 3.5)
                    {
                        // STAR: z sharp spikes of uneven length round a small core, crisp
                        float nS = max(i.shape.z, 5.0);
                        float turn = frac(atan2(qv.y, qv.x) * 0.159155 + 0.5 + sd * 3.7);
                        float sec = turn * nS;
                        float idS = floor(sec);
                        float lenS = lerp(0.42, 1.0, CelHash21(float2(idS, sd * 53.0 + 7.0)));
                        float offS = (CelHash21(float2(idS, sd * 91.0)) - 0.5) * 0.4;
                        float lat = abs(sec - idS - 0.5 - offS) * 6.28318 / nS * rr;
                        float wS = max(i.shape.y, 0.01) * saturate(1.0 - rr / lenS);
                        float coreS = max(i.shape.w, 0.05);
                        float dS = max(wS - lat, coreS - rr);
                        float m = CelAAStep(0.0, dS) * step(rr, 1.0);
                        col = lerp(CelPal(row, 3), CelPalCore(row).rgb * i.tone.y, saturate(1.0 - rr / max(coreS * 2.5, 0.05)));
                        cover = m * i.tone.z * (1.0 - thr);
                    }
                    else if (mode < 4.5)
                    {
                        // FLECK: a small hard kite of a chip, along its motion
                        float kite = qv.x > 0.0 ? 1.0 : 0.7;
                        float dF = 1.0 - (abs(qv.x) / kite + abs(qv.y));
                        float m = CelAAStep(0.0, dF);
                        col = lerp(CelPal(row, 3), CelPalCore(row).rgb * i.tone.y, step(0.0, qv.y));
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
                        // FROST: a soft mist patch laced with crystalline veins
                        float base = exp(-rr * rr * 2.5) * saturate(1.0 - rr);
                        float sc = max(i.shape.y, 1.0);
                        float n1 = CelValue2(qv * sc + sd * 19.0);
                        float n2 = CelValue2(qv * sc * 2.3 + sd * 7.0 + 3.3);
                        float vein = pow(saturate(1.0 - abs(n1 * 2.0 - 1.0)), 6.0);
                        float vein2 = pow(saturate(1.0 - abs(n2 * 2.0 - 1.0)), 6.0);
                        float crys = saturate(vein * 0.8 + vein2 * 0.6);
                        float dens = base * lerp(0.35, 1.0, crys) * (1.0 - thr) * max(i.shape.z, 0.1);
                        cover = smoothstep(0.05, 0.45, dens) * i.tone.z;
                        col = lerp(CelPal(row, 2), CelPal(row, 3), crys);
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
                    // DART: an ICICLE javelin (the clip at 4x): a long needle point, the widest part a quarter to two
                    // fifths from the back, a short broken stub behind it, a swept-back hook on the lower edge of most
                    // (on the upper of a few); two-tone, hard-edged, a deep blue line along the lower edge
                    float a = i.q.x, y = i.q.y;
                    float sd = i.more.w;
                    float h1 = CelHash21(float2(sd * 13.1, 2.3)), h2 = CelHash21(float2(sd * 17.9, 4.1)), h3 = CelHash21(float2(sd * 5.3, 7.7));
                    float hb = lerp(0.22, 0.42, h1);
                    float w0u = lerp(0.8, 1.0, h3), w0d = lerp(0.85, 1.0, h2);
                    float backU = lerp(0.4, 0.6, h2), backD = lerp(0.35, 0.55, h3);
                    float upW = IceEdge(a, hb + (h2 - 0.5) * 0.06, w0u, backU);
                    float dnW = IceEdge(a, hb - (h2 - 0.5) * 0.06, w0d, backD);
                    float hookLen = lerp(0.1, 0.2, h3);
                    float hookUp = h2 > 0.8 ? 1.0 : 0.0;
                    float hookDn = h1 < 0.72 ? 1.0 : 0.0;
                    float cellL = max(i.q.w * 0.9, 1e-3);
                    float rag = (CelHash21(float2(floor(i.q.z / cellL), sd * 31.0 + step(0.0, y))) - 0.5) * 0.14;
                    float eW = y >= 0.0 ? upW : dnW;
                    float dE = eW + rag * step(0.05, eW) - abs(y);
                    // the hook on its side: behind the widest point the stub is narrower and the hook stands off it
                    float hookOn = y >= 0.0 ? hookUp : hookDn;
                    if (hookOn > 0.5 && a < hb)
                    {
                        float stubW = (y >= 0.0 ? backU : backD) * saturate(a / max(hb * 0.45, 0.02));
                        float w0h = y >= 0.0 ? w0u : w0d;
                        dE = max(stubW + rag * 0.5 - abs(y), IceHook(a, abs(y), hb, w0h, stubW, hookLen));
                    }
                    float inside = CelAAStep(0.0, dE);
                    float hwPx = i.q.w / max(PixelMetres(i.positionWS), 1e-6);
                    float onePx = 1.0 / max(hwPx, 0.5);
                    float ridge = 0.12 * eW;
                    bool upper = y >= ridge;
                    col = upper ? CelPal(row, 3) : CelPal(row, 1);
                    // the ridge between the facets reads as a thin pale line toward the head
                    col = lerp(col, CelPal(row, 2), (1.0 - saturate(abs(y - ridge) / max(onePx * 1.2, 1e-3))) * smoothstep(0.3, 0.6, a));
                    // a bright line just inside the upper edge, a deeper rim on the lower
                    if (upper && dE < onePx * 1.6) col = CelPalCore(row).rgb * i.tone.y;
                    if (!upper && dE < onePx * 1.2) col = CelPal(row, 0);
                    float tailFade = lerp(0.55, 1.0, saturate(a / 0.3));
                    cover = inside * i.tone.z * tailFade * (1.0 - thr);
                }
                else
                {
                    // RIBBON: a soft pale strand that twists (narrow edge-on, wide flat), a brighter line down its middle,
                    // freshest by the head; soft mist puffs along it and frost specks beside it
                    float y = i.q.y, u = i.q.z;
                    float sd = i.more.w;
                    float wl = max(i.shape.y, 0.3);
                    float tw = 0.3 + 0.7 * abs(cos(6.28318 * u / (wl * 0.63) + sd * 23.0));
                    float hw = tw * (1.0 - 0.6 * thr);
                    float g = exp(-(y * y) / max(hw * hw, 1e-3) * 1.6);
                    float line0 = exp(-(y * y) / max(hw * hw * 0.12, 1e-4) * 1.6);
                    float fresh = lerp(0.7, 1.0, smoothstep(0.6, 1.0, i.q.x));
                    float alpha = g * fresh * lerp(1.15, 0.85, tw);
                    col = lerp(CelPal(row, 2), CelPal(row, 3), saturate(line0 * 1.2 + (1.0 - tw) * 0.5));
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
                        float pr = lerp(2.5, 4.5, CelHash21(float2(pcell, sd * 59.0)));
                        float2 dP = float2((u - px0) / max(i.q.w, 1e-5), y - py0) / pr;
                        float puff = exp(-dot(dP, dP) * 2.2) * 0.45;
                        alpha = max(alpha, puff);
                    }
                    // frost specks: a share of 0.22 m cells holds a 1-2 px white dot off the strand, twinkling
                    float sc = 0.22;
                    float si = floor(u / sc);
                    float sh = CelHash21(float2(si, sd * 61.0));
                    float speck = 0.0;
                    if (sh > 0.7)
                    {
                        float sx = (si + 0.2 + 0.6 * CelHash21(float2(si, sd * 67.0))) * sc;
                        float sgn = CelHash21(float2(si, sd * 71.0)) > 0.5 ? 1.0 : -1.0;
                        float sy = sgn * lerp(1.5, 6.0, CelHash21(float2(si, sd * 73.0)));
                        float spx = length(float2((u - sx) / max(pm, 1e-6), (y - sy) * hwPx));
                        float rad = lerp(0.8, 1.7, CelHash21(float2(si, sd * 79.0)));
                        float twk = CelHash21(float2(si + sd * 83.0, tick)) > 0.35 ? 1.0 : 0.3;
                        speck = saturate(rad + 0.5 - spx) * twk;
                    }
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
