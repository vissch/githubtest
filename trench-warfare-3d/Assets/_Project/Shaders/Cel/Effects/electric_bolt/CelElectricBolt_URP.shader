// Phase: CelFx electric bolt (2026-11-15, lane/show/cel-electric-bolt) - ElectricBolt: the rapid-fire electric bolt of
// the owner's clip electric_bolt_cel_00001_ (what it is: celfx spec\eye\electric_bolt.md). Built from blue_laser's
// LightningBeam (a camera-facing ribbon laid along a straight span, and its soft dots) and neon_acid's light (the lit
// point found from the camera's depth and multiplied into the scene by a second pass). One kind, one draw, two passes.
//
// BEAM records (size.w >= 0): a ribbon along the span pos -> pos + vel x life (CelAim.Target, Speed 0, Arrive 1, no
// gravity or drag: vel x life is the span). motion.w is the profile:
//   0 COMET (a bolt in flight): a round white-hot head at the front, a body that waves in a gentle S and runs to a
//     point at the tail, a white core in pale cyan bands, a wide soft blue bloom round it (no ink).
//     size.xyz  the head's full width (m) at birth, at death, ease.   size.w the bolt's length (share of the span).
//     extra     x the wave (half-widths), y bends along the bolt, z the head's share of the span at birth, w at death.
//     tone.w    the clip: the bolt never reaches past this share of the span (1: it stops at the target).
//   1 ARC (lightning): the whole span as a jagged line, kinked at sharp corners (2-3 scales; the big ones bend the
//     ribbon itself in the vertex shader), re-drawn several times a second, with short forks, thick at its root.
//     size.xyz  the width (m) at the root over the life.   size.w the share of the life it takes to reach its tip.
//     extra     x the kinks' reach (m), y forks per metre, z kinks along the arc, w drawings per second (0: 24).
//   both: tone x palette (c0 the bloom's light, c2..c3 the bands, core white), y HDR core, z opacity;
//         erode.xyz the threshold the cross-section is cut at over the life (it thins in steps).
// DOT records (size.w < 0): a camera-facing quad at CelPosition; extra.x the mode:
//   1 bloom: soft additive light, y falloff (0: 5), w the white-hot core's radius share.
//   2 pool: the same lying on the ground.
//   4 ray star: a white-hot core and z thin spikes of uneven length; y the spikes' root half-width, w the core share.
//   5 board wash: a soft rounded rectangle of light; y edge softness, z half width, w half height (quad units).
//   7 light: the bolts' light on whatever stands behind its card (the ground, the target): the lit point is found
//     from the camera's depth, so the card is never hidden. Pos the light, Axis a level axis of its own; size x its
//     reach along that axis (m), y up and down (m), z its far-side reach over its near (square to the axis);
//     motion.x its near reach square to the axis over x; vel how much of what it lights it multiplies in at its
//     heart (rgb: the ground keeps its own pattern), erode.xyz what it adds on top; extra y seconds to come up,
//     z seconds to go; tone y 1: level ground only, z its strength.
// _TWBoltLens (global; ElectricBoltRecipe.Lens): the scene's lens vignette (x its strength x 3, y its falloff); 0 none.
// The vignette takes most of the light from the corners, and the clip's bolts are as bright there as in the middle:
// colours are lifted by what the vignette will take (neon_acid's Unvignette).
Shader "TW/Cel/ElectricBolt"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+22" "IgnoreProjector"="True" }
        ZWrite Off ZTest LEqual Cull Off
        HLSLINCLUDE
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            // the ribbon is this many half-widths to each side: room for the wave, the kinks' small jags and the bloom
            #define BOLT_PAD 7.0

            float4 _TWBoltLens;

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;                         // beam: x 0..1 along (tail..head, root..tip), y across (half-widths), z metres along, w half-width (m). dot: xy -1..1, z seed, w -1
                nointerpolation float4 tone : TEXCOORD1;         // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;        // comet: x wave, y bends, z seed, w length (m) | arc: x -, y forks per m, z seed, w length (m) | dot: mode, y, z, w
                float4 more : TEXCOORD3;                         // x fog, y far, z kind (0 comet, 1 arc, -1 dot), w drawings per second
                float3 positionWS : TEXCOORD4;
                nointerpolation float4 g0 : TEXCOORD5;           // light: axis x, axis z, far reach ratio, up reach
                nointerpolation float4 g1 : TEXCOORD6;           // light: reach along, near reach across, up x down, into.r
                nointerpolation float4 g2 : TEXCOORD7;           // light: add rgb, into.g
                nointerpolation float4 g3 : TEXCOORD8;           // light: pos, into.b
            };

            // metres per screen pixel at a world point (perspective)
            float PixelMetres(float3 p)
            {
                float d = distance(_WorldSpaceCameraPos, p);
                return d * 2.0 / max(abs(UNITY_MATRIX_P[1][1]) * _ScreenParams.y, 400.0);
            }

            // a zig-zag: random offsets -1..1 at cell corners, joined by straight lines (cells of `cell` units)
            float Zig(float u, float cell, float salt)
            {
                float x = u / cell;
                float i = floor(x), f = x - i;
                float a = CelHash21(float2(i, salt)) * 2.0 - 1.0;
                float b = CelHash21(float2(i + 1.0, salt)) * 2.0 - 1.0;
                return lerp(a, b, f);
            }

            // the arc's big kinks: offsets at n cell corners joined by straight lines and a finer set at three times
            // the cells, pinned at the root and the tip (0 there)
            float ArcKink(float s, float n, float salt)
            {
                float pin = sqrt(saturate(sin(saturate(s) * 3.14159265)));
                return (Zig(s, 1.0 / n, salt) + 0.35 * Zig(s, 1.0 / (3.0 * n), salt + 7.1)) * pin;
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float age = CelAge(r);
                float k = CelLife01(r), alive = CelAlive(r);
                float farK = saturate(_CelLod.x - 1.0);
                float2 corner = float2(v.uv.x * 2.0 - 1.0, v.uv.y * 2.0 - 1.0);
                Varyings o = (Varyings)0;
                if (r.size.w < 0.0)
                {
                    float mode = r.extra.x;
                    if (mode > 6.5)
                    {
                        // the light: a card over the ball that holds it, never hidden (the light falls on what is behind it)
                        float reachX = r.size.x * max(r.motion.x, 0.01);
                        float R = max(max(r.size.x, r.size.y), reachX);
                        float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                        float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                        float3 wl = lerp(r.pos, CelBillboard(r.pos, corner * R * cover), alive);
                        o.positionWS = wl;
                        o.positionCS = TransformWorldToHClip(wl);
                        if (step(dist, R * 1.02) * alive > 0.5) o.positionCS = float4(corner, 0.0, 1.0);   // the eye is inside the light
                        o.positionCS.z = o.positionCS.w * lerp(UNITY_NEAR_CLIP_VALUE, UNITY_RAW_FAR_CLIP_VALUE, 0.0002);
                        float3 ax = float3(r.axis.x, 0.0, r.axis.z);
                        ax = dot(ax, ax) > 1e-6 ? normalize(ax) : float3(1.0, 0.0, 0.0);
                        float rise = saturate(age / max(r.extra.y, 0.01)), fall = saturate((r.life - age) / max(r.extra.z, 0.01));
                        o.g0 = float4(ax.x, ax.z, clamp(r.size.z, 0.05, 1.0), max(r.size.y, 0.05));
                        o.g1 = float4(max(r.size.x, 0.05), max(reachX, 0.05), rise * fall * alive, r.vel.x);
                        o.g2 = float4(r.erode.xyz, r.vel.y);
                        o.g3 = float4(r.pos, r.vel.z);
                        o.tone = r.tone;
                        o.beam = float4(corner, r.seed, -1.0);
                        o.shape = float4(7.0, 0.0, 0.0, 0.0);
                        o.more = float4(0.0, farK, -1.0, 0.0);
                        return o;
                    }
                    float3 c = CelPosition(r, age);
                    float rad = CelCurve(r.size.xyz, k) * alive;
                    float3 wd = (mode > 1.5 && mode < 2.5) ? c + float3(corner.x, 0.0, corner.y) * rad + float3(0.0, 0.05, 0.0)
                                                         : CelBillboard(c, corner * rad);
                    o.positionWS = wd;
                    o.positionCS = TransformWorldToHClip(wd);
                    o.beam = float4(corner, r.seed, -1.0);
                    o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                    o.shape = float4(mode, r.extra.y, r.extra.z, r.extra.w);
                    o.more = float4(ComputeFogFactor(o.positionCS.z), farK, -1.0, 0.0);
                    return o;
                }
                float3 span = r.vel * r.life;                     // start -> target
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                bool arc = r.motion.w > 0.5;
                float s0, s1, head = 1.0, winLen = 1.0;
                if (arc)
                {
                    // the arc shoots out from its root to its tip over the first size.w of its life
                    float grow = r.size.w > 1e-3 ? saturate(k / r.size.w) : 1.0;
                    s0 = 0.0; s1 = grow;
                }
                else
                {
                    head = lerp(r.extra.z, r.extra.w, k);
                    winLen = max(r.size.w, 1e-4);
                    s0 = max(head - winLen, 0.0);
                    s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                }
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                float s = lerp(s0, s1, v.uv.x);
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float halfW = width * 0.5;
                float3 sideW = side;
                float rate = (arc && r.extra.w > 0.0) ? r.extra.w : 24.0;
                if (arc)
                {
                    float salt = r.seed * 97.0 + floor(_CelNow * rate) * 13.7;
                    float n = max(r.extra.z, 1.0);
                    float amp = r.extra.x;
                    float J = ArcKink(s, n, salt);
                    float Jd = (ArcKink(s + 0.004, n, salt) - ArcKink(s - 0.004, n, salt)) / 0.008;
                    p += side * J * amp;
                    // the ribbon's own side, square to the kinked line as the eye sees it
                    float3 t = dir * lenB + side * Jd * amp;
                    float3 sd = cross(normalize(t), toEye);
                    sideW = dot(sd, sd) > 1e-6 ? normalize(sd) : side;
                    halfW *= lerp(1.0, 0.55, s);                   // thick at the root, thinner to its tip
                }
                // going out the line keeps at least ~1.3 px of half-width (2.5 px wide at Far)
                halfW = max(halfW, 1.3 * saturate(_CelLod.x * 0.5) * PixelMetres(p) * alive * open);
                float across = v.uv.y * 2.0 - 1.0;
                float3 wb = p + sideW * across * halfW * BOLT_PAD;
                o.positionWS = wb;
                o.positionCS = TransformWorldToHClip(wb);
                // the comet's own 0..1 is measured against its full length, so a bolt clipped at the target keeps its head
                float a = arc ? s : saturate((s - (head - winLen)) / winLen);
                o.beam = float4(a, across * BOLT_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(2.0 - _CelLod.x), r.seed, arc ? lenB : winLen * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), farK, arc ? 1.0 : 0.0, rate);
                return o;
            }

            // an additive colour through the fog; at Far most of the fog is skipped so the line and the dot read
            half3 AddFog(half3 c, float3 ws, float fog, float farK)
            {
                return lerp(CelAdditiveFog(c, ws, fog), c * 0.9, farK * 0.85);
            }

            // the colour as it has to be written for the lens vignette to leave it as it is (neon_acid's; share: how
            // much of what the lens takes is made up, 1 all of it)
            half3 Unvignette(half3 col, float2 pixel, float share)
            {
                if (_TWBoltLens.x <= 0.0) return col;
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWBoltLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWBoltLens.y, 0.01)));
                float lift = pow(1.0 / keep, share);
                float3 lin = SRGBToLinear(max(col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float gain = max(min(lift, 1.0 / max(top, 1e-3)), 1.0);
                // the bolts' own colours (share 1) take the whole lift, each channel clipped: a white-hot core in a dark
                // corner goes on to white as the clip's does (capped at 1.2 x its top, it went cyan there); the ground
                // light's added part keeps its hue
                float k = share >= 0.999 ? lift : min(lift, gain * 1.2);
                return (half3)LinearToSRGB(min(lin * k, 1.0));
            }

            // the point of the opaque scene behind this pixel (from the camera's depth)
            float3 SceneSpot(Varyings i)
            {
                float2 uv = i.positionCS.xy / _ScaledScreenParams.xy;
                float2 suv = ClampAndScaleUVForBilinear(UnityStereoTransformScreenSpaceTex(uv), _CameraDepthTexture_TexelSize.xy);
                float depth = SAMPLE_TEXTURE2D_X_LOD(_CameraDepthTexture, sampler_PointClamp, suv, 0).r;
                #if !UNITY_REVERSED_Z
                depth = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, depth);
                #endif
                return ComputeWorldSpacePosition(uv, depth, UNITY_MATRIX_I_VP);
            }

            // a light's strength (0..1) on a point of the opaque scene that faces `facing`
            float GlowOn(Varyings i, float3 spot, float3 facing)
            {
                float3 d = spot - i.g3.xyz;
                float al = d.x * i.g0.x + d.z * i.g0.y, back = d.z * i.g0.x - d.x * i.g0.y;
                float t = length(float3(al / i.g1.x, back / (i.g1.y * (back > 0.0 ? i.g0.z : 1.0)), d.y / i.g0.w));
                float level = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                return exp(-3.0 * t * t) * (1.0 - smoothstep(0.8, 1.0, t)) * i.g1.z * i.tone.z * lerp(1.0, level, i.tone.y);
            }

            // what the scene's fog and mist leave of a light that falls on a point
            half3 GlowSeen(float3 spot)
            {
                return CelAdditiveFog(half3(1.0, 1.0, 1.0), spot, ComputeFogFactor(TransformWorldToHClip(spot).z));
            }

            // the light pass: only the lights draw, and what they write is multiplied into what is there
            Varyings vertLight(Attributes v, uint instanceID : SV_InstanceID)
            {
                Varyings o = vert(v, instanceID);
                if (o.more.z > -0.5 || o.shape.x < 6.5) o.positionCS = float4(2.0, 2.0, 2.0, 1.0);
                return o;
            }

            half4 fragLight(Varyings i) : SV_Target
            {
                float3 spot = SceneSpot(i);
                float f = GlowOn(i, spot, cross(ddy(spot), ddx(spot)));
                return half4(float3(i.g1.w, i.g2.w, i.g3.w) * f * GlowSeen(spot), 0.0);
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                // the scene behind this pixel and which way it faces (the derivatives are taken once, outside every branch)
                float3 spot = SceneSpot(i);
                float3 facing = cross(ddy(spot), ddx(spot));
                if (i.more.z < -0.5)
                {
                    float2 q = i.beam.xy;
                    float rr = length(q);
                    float thrS = saturate(i.tone.w);
                    float mode = i.shape.x;
                    if (mode > 6.5)
                    {
                        // a light, after the light pass has multiplied it in: the colour it adds on top
                        float f = GlowOn(i, spot, facing);
                        return half4(Unvignette((half3)(i.g2.rgb * f * GlowSeen(spot)), i.positionCS.xy, 0.35), 0.0);
                    }
                    if (mode < 2.5)
                    {
                        // a soft bloom (1) or pool (2): a smooth falloff, no bands, no rim; a white-hot round core (w)
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
                        return half4(Unvignette(AddFog(cs, i.positionWS, i.more.x, i.more.y), i.positionCS.xy, 1.0), 0.0);
                    }
                    if (mode > 4.5)
                    {
                        // a board wash (5): a soft rounded rectangle of additive light the size of the dummy's board
                        float2 hx = float2(max(i.shape.z, 0.2), max(i.shape.w, 0.2));
                        float2 d = abs(q) - hx + 0.15;
                        float sdf = length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - 0.15;
                        float edgeB = max(i.shape.y, 0.02);
                        float wash = (1.0 - smoothstep(-edgeB, edgeB, sdf)) * (1.0 - thrS);
                        float inner = exp(-rr * rr * 3.0);
                        half3 cw = (CelPal(row, 2) * 0.6 + CelPal(row, 3) * inner * 0.8) * wash * i.tone.z;
                        return half4(Unvignette(AddFog(cw, i.positionWS, i.more.x, i.more.y), i.positionCS.xy, 1.0), 0.0);
                    }
                    // a ray star (4): a small white-hot core and z thin spikes of uneven length, each record at its own turn
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
                    float coreS = max(i.shape.w, 0.02);
                    float coreD = 1.0 - smoothstep(coreS * 0.75, coreS, rr);
                    float haze = exp(-rr * rr * 40.0) * 0.35;
                    half3 white = CelPalCore(row).rgb * i.tone.y;
                    half3 cr = lerp(CelPal(row, 3), white, saturate(1.2 - rr / lenR)) * ray + CelPal(row, 2) * haze;
                    cr = lerp(cr, white, coreD) * i.tone.z * (1.0 - thrS);
                    return half4(Unvignette(AddFog(cr, i.positionWS, i.more.x, i.more.y), i.positionCS.xy, 1.0), 0.0);
                }

                // ---- a bolt or an arc
                float a = i.beam.x, y = i.beam.y, u = i.beam.z;
                bool arc = i.more.z > 0.5;
                float tick = floor(_CelNow * i.more.w);
                float salt = i.shape.z * 97.0 + tick * 13.7;
                float hwPx = i.beam.w / max(PixelMetres(i.positionWS), 1e-6);
                float onePx = 1.0 / max(hwPx, 0.5);
                float centre, prof, bright;
                if (arc)
                {
                    // small jags on the kinked line (the vertex shader bent the ribbon for the big ones); the width
                    // wanders along it and narrows over the last of it into the flare
                    centre = 0.55 * Zig(u, 0.09, salt + 3.0) + 0.3 * Zig(u, 0.03, salt + 5.0);
                    prof = (0.7 + 0.6 * CelHash21(float2(floor(u / 0.3), salt + 9.0))) * lerp(1.0, 0.6, smoothstep(0.8, 1.0, a));
                    bright = 0.85 + 0.15 * CelHash21(float2(salt, 0.71));
                }
                else
                {
                    // the comet: a round cap one half-width long at the head (a = 1), tapering to a point at the tail
                    float L = max(i.shape.w, 1e-3);
                    float capA = saturate(i.beam.w / L);
                    float neck = 1.0 - capA;
                    float capT = saturate((a - neck) / max(capA, 1e-4));
                    float cap = sqrt(saturate(1.0 - capT * capT));
                    float taper = pow(saturate(a / max(neck, 1e-3)), 0.85);
                    prof = a > neck ? cap : taper;
                    // the body waves in a gentle S (more toward the tail), re-drawn a little every frame
                    float bends = max(i.shape.y, 0.3);
                    float ph = i.shape.z * 6.2832 + tick * 0.45;
                    centre = i.shape.x * sin(a * bends * 6.2832 + ph) * lerp(1.0, 0.2, a) + 0.18 * Zig(u, 0.07, salt);
                    // hot at the head, the tail in the cyan bands
                    bright = lerp(0.55, 1.0, smoothstep(0.05, 0.75, a));
                }
                float dy = abs(y - centre);
                float field = saturate(1.0 - dy / max(prof, 1e-3));
                // forks: in some cells along an arc a thin crooked arm leaves the line and runs out to one side
                float br = 0.0;
                if (arc && i.shape.y > 0.0)
                {
                    float cell = 0.45;
                    float ci = floor(u / cell);
                    [unroll] for (int c = 0; c < 2; c++)
                    {
                        float id = ci - c;
                        float h = CelHash21(float2(id, salt + 31.0));
                        if (h > saturate(i.shape.y * cell * 0.6)) continue;
                        float x0 = id * cell + CelHash21(float2(id, salt + 2.0)) * cell * 0.5;
                        float lenA = cell * (0.35 + 0.6 * CelHash21(float2(id, salt + 3.0)));
                        float t = (u - x0) / lenA;
                        if (t < 0.0 || t > 1.0) continue;
                        float sgn = CelHash21(float2(id, salt + 4.0)) > 0.5 ? 1.0 : -1.0;
                        float reach = (BOLT_PAD - 1.5) * (0.35 + 0.55 * CelHash21(float2(id, salt + 6.0)));
                        float kink = 0.8 * Zig(u, 0.05, salt + id) + 0.4 * Zig(u, 0.02, salt + id + 3.3);
                        float yb = sgn * (pow(t, 0.8) * reach + kink) + centre;
                        float bw = max(0.55 * (1.0 - t * 0.6), 1.2 * onePx);
                        br = max(br, saturate(1.0 - abs(y - yb) / bw) * lerp(0.9, 0.55, t));
                    }
                }
                float thr = max(i.tone.w, 0.02);
                float mask = max(CelAAStep(thr, field), CelAAStep(0.3, br));
                float v = max(field * bright, br * 0.8);
                half3 body = CelRamp(v, row, i.tone.y);
                body = MixFog(body, i.more.x);
                float cover = mask * saturate(i.tone.z * 1.4);
                // the bloom: saturated blue light round it, a slow fall over ~3 half-widths (the clip's lift at 1, 5 and
                // 12 px out of a near bolt: (38,86,131), (16,47,89), (10,24,50)), fading before the ribbon's edge
                float outside = max(dy - prof, 0.0);
                float bloom = exp(-outside / 2.0) * (1.0 - mask) * saturate((BOLT_PAD - 0.5 - abs(y)) * 0.5) * step(0.02, prof);
                if (!arc) bloom *= smoothstep(0.0, 0.3, a);
                half3 add = CelPal(row, 0) * bloom * i.tone.z;
                add = AddFog(add, i.positionWS, i.more.x, i.more.y);
                half3 col = body * cover + add;
                return half4(Unvignette(col, i.positionCS.xy, 1.0), cover);
            }
        ENDHLSL
        Pass
        {
            Name "CelElectricBoltLight"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend DstColor One, Zero One
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vertLight
            #pragma fragment fragLight
            #pragma multi_compile_fog
            ENDHLSL
        }
        Pass
        {
            Name "CelElectricBolt"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            ENDHLSL
        }
    }
}
