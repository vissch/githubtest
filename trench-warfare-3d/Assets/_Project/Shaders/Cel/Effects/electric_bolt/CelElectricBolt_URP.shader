// Phase: CelFx electric bolt (2026-11-15, lane/show/cel-electric-bolt) - ElectricBolt: the rapid-fire electric bolt of
// the owner's clip electric_bolt_cel_00001_ (what it is: celfx spec\eye\electric_bolt.md). Built from blue_laser's
// LightningBeam (a camera-facing ribbon laid along a straight span, and its soft dots) and neon_acid's light (the lit
// point found from the camera's depth and multiplied into the scene by a second pass). One kind, one draw, two passes.
//
// BEAM records (size.w >= 0): a ribbon along the span pos -> pos + vel x life (CelAim.Target, Speed 0, Arrive 1, no
// gravity or drag: vel x life is the span). motion.w is the profile:
//   0 COMET (a bolt in flight): the clip's teardrop, widest at 0.8 of its length and rounded to the head, running to
//     a thin tail; a body that waves in a gentle S with a tendril or two, a white core in pale cyan bands, a wide soft
//     blue bloom round it (no ink).
//     size.xyz  the head's full width (m) at birth, at death, ease.   size.w the bolt's length at birth (share of the
//     span); motion.x its length at death over that (0: the same); motion.y tendrils per metre.
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
//   3 burst: a white-hot core with a ragged edge (z its radius share) and y tapered shards of uneven length thrown out
//     of it (w their root half-width share), in a soft glow (c2); the shards' edge c3, their middle and the core white.
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
            // and runs on this many half-widths past each end of the body, for the bloom to close round the head and
            // the tail (cut off where the body ended, it drew a lit box round each bolt)
            #define BOLT_PADL 4.0

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
                nointerpolation float2 ends : TEXCOORD9;         // beam: metres along where the body begins and ends
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
                    // the bolt grows as it goes (motion.x: its length at the end over its length at the start; the
                    // clip's are 120 px long near the eye and 76 by the dummy, longer in metres the further they are)
                    winLen = max(r.size.w * lerp(1.0, r.motion.x > 0.0 ? r.motion.x : 1.0, k), 1e-4);
                    s0 = max(head - winLen, 0.0);
                    s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                }
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                // the ribbon runs on BOLT_PADL half-widths past both ends for the bloom; the fragment cuts the body at them
                float padS = width * 0.5 * BOLT_PADL / lenB;
                float s = lerp(s0 - padS, s1 + padS, v.uv.x);
                o.ends = float2(s0, s1) * lenB;
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
                // (not clamped here: the pads run past 0 and 1, the fragment clamps it)
                float a = arc ? s : (s - (head - winLen)) / winLen;
                o.beam = float4(a, across * BOLT_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(2.0 - _CelLod.x), r.seed, arc ? lenB : winLen * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), farK, arc ? 1.0 : 0.0, rate);
                o.g0 = float4(arc ? 0.0 : max(r.motion.y, 0.0) * saturate(2.0 - _CelLod.x), 0.0, 0.0, 0.0);   // a comet's tendrils per metre
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
                    if (mode < 3.5)
                    {
                        // a burst (3): the hit's white-hot splash as the clip draws it (f40-f104 at 4x): a white core with
                        // a ragged edge (z its radius), y tapered shards of uneven length thrown out of it (w their half-
                        // width at the root), white along the middle and pale cyan at the edge, in a soft cyan glow; each
                        // record lays them out anew (a record lives a frame or two, so it flickers as the clip's does)
                        float sdB = i.beam.z;
                        float turnB = atan2(q.y, q.x) * 0.159155 + 0.5;
                        float lx = turnB * 9.0;
                        float l0 = floor(lx);
                        float rag = lerp(CelHash21(float2(l0, sdB * 17.0)), CelHash21(float2(l0 + 1.0 - 9.0 * step(8.0, l0), sdB * 17.0)),
                                         smoothstep(0.0, 1.0, lx - l0));
                        float coreRB = max(i.shape.z, 0.02) * (0.75 + 0.5 * rag) * (1.0 - thrS * 0.5);
                        float coreB = 1.0 - smoothstep(coreRB * 0.3, coreRB * 1.1, rr);
                        // (e07: the clip's splash is a fan, f30-f104 at 3x: most feathers fly back toward the shooter, the
                        // eye's left and down-left, a few up and right; the fan turns a little from record to record)
                        float fanA = 3.6 + (CelHash21(float2(sdB, 4.1)) - 0.5) * 1.0;
                        float nB = max(i.shape.y, 3.0);
                        float idB0 = floor(turnB * nB);
                        float fieldB = 0.0, alongB = 1.0;
                        [unroll] for (int kB = -1; kB <= 1; kB++)
                        {
                            float idB = idB0 + kB;
                            float idW = idB - nB * floor(idB / nB);                 // the sector, wrapped round
                            float hA = CelHash21(float2(idW, sdB * 91.0));
                            float hL = CelHash21(float2(idW, sdB * 53.0 + 7.0));
                            float hW = CelHash21(float2(idW, sdB * 29.0 + 3.0));
                            float angB = ((idB + 0.5 + (hA - 0.5) * 0.7) / nB - 0.5) * 6.28318;
                            float2 dB = float2(cos(angB), sin(angB));
                            float alB = dot(q, dB);
                            // (e06: crooked slivers, as the clip's: the shard's line jags sideways along its length)
                            float hK = CelHash21(float2(idW, sdB * 13.0 + 1.0));
                            float latB = abs(q.x * dB.y - q.y * dB.x + (hK - 0.5) * 0.3 * alB * alB);
                            // (e04: the clip's shards are loose flakes, most start well out of the core, not a star)
                            float wFan = 0.5 + 0.5 * cos(angB - fanA);
                            float keepF = step(CelHash21(float2(idW, sdB * 37.0 + 9.0)), 0.15 + 0.85 * wFan * wFan);
                            float r0 = coreRB * 0.3 + 0.2 * CelHash21(float2(idW, sdB * 71.0 + 5.0)) * step(0.5, hL);
                            float tB = (alB - r0) / max(lerp(0.3, 1.0, hL) * lerp(0.4, 1.0, wFan) - r0, 1e-3);
                            float hwB = max(i.shape.w, 0.005) * lerp(0.5, 1.5, hW) * pow(saturate(1.0 - tB), 0.9) * saturate(tB * 4.0 + 0.3);
                            float fB = saturate(1.0 - latB / max(hwB, 1e-4)) * step(0.0, tB) * step(tB, 1.0) * keepF;
                            alongB = lerp(alongB, saturate(tB), step(fieldB, fB));
                            fieldB = max(fieldB, fB);
                        }
                        float shardB = CelAAStep(0.3, fieldB);
                        float shardW = CelAAStep(0.5, fieldB) * saturate(1.4 - alongB);
                        float glowB = exp(-rr * rr * 2.5) * 1.0;
                        half3 whiteB = CelPalCore(row).rgb * i.tone.y;
                        half3 cb = CelPal(row, 2) * glowB;
                        cb = lerp(cb, CelPal(row, 3), shardB);
                        cb = lerp(cb, whiteB, max(shardW, coreB));
                        cb *= i.tone.z * (1.0 - thrS * 0.6);
                        // (e12: the shards and core go on OVER what is there, the glow alone is added: stacked additive
                        // whites of overlapping records summed past 1 and the grade showed them cream (255,255,229))
                        float covB = max(shardB, coreB) * saturate(i.tone.z) * (1.0 - thrS * 0.6);
                        return half4(Unvignette(AddFog(cb, i.positionWS, i.more.x, i.more.y), i.positionCS.xy, 1.0), covB);
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
                float a = saturate(i.beam.x), y = i.beam.y, u = i.beam.z;
                bool arc = i.more.z > 0.5;
                // half-widths past either end of the body (0 on it): the pads only carry the bloom
                float beyond = max(max(i.ends.x - u, u - i.ends.y), 0.0) / max(i.beam.w, 1e-4);
                float inside = step(beyond, 0.0);
                float tick = floor(_CelNow * i.more.w);
                float salt = i.shape.z * 97.0 + tick * 13.7;
                float hwPx = i.beam.w / max(PixelMetres(i.positionWS), 1e-6);
                float onePx = 1.0 / max(hwPx, 0.5);
                float centre, prof, bright;
                if (arc)
                {
                    // small jags on the kinked line (the vertex shader bent the ribbon for the big ones); the width
                    // wanders along it and narrows over the last of it into the flare. Half-widths: kept small, the
                    // clip's arcs are thick lines with sharp corners, not a scribble
                    centre = 0.1 * Zig(u, 0.12, salt + 3.0) + 0.04 * Zig(u, 0.04, salt + 5.0);
                    prof = (0.7 + 0.6 * CelHash21(float2(floor(u / 0.3), salt + 9.0))) * lerp(1.0, 0.6, smoothstep(0.8, 1.0, a));
                    bright = 0.85 + 0.15 * CelHash21(float2(salt, 0.71));
                }
                else
                {
                    // the comet: the clip's teardrop (cometprof.py over 377 of its comets: the width rises about straight
                    // from 0.15 of its most at the tail to the most at 0.8 of the length, then rounds off to the head)
                    // (e03: the clip's at 2x is a needle: a hair-thin tail swelling late into a bright head, then a
                    // pointed nose; straight, not a wave)
                    float capT = saturate((a - 0.82) / 0.18);
                    prof = a > 0.82 ? pow(saturate(1.0 - capT), 0.55) : lerp(0.04, 1.0, pow(a / 0.82, 1.6));
                    float bends = max(i.shape.y, 0.3);
                    float ph = i.shape.z * 6.2832 + tick * 0.45;
                    centre = i.shape.x * sin(a * bends * 6.2832 + ph) * lerp(1.0, 0.2, a) + 0.06 * Zig(u, 0.07, salt);
                    // white-hot all along: the core's share of the width is set by the bands below
                    bright = 1.0;
                }
                float dy = abs(y - centre);
                float field = saturate(1.0 - dy / max(prof, 1e-3)) * inside;
                // forks: in some cells along an arc a thin crooked arm leaves the line and runs out to one side; a comet
                // has a tendril or two off its body the same way, short and hair-thin (the clip's f33-f35 at 3x)
                float br = 0.0;
                float forks = arc ? i.shape.y : i.g0.x;
                if (forks > 0.0)
                {
                    float cell = arc ? 0.45 : 0.3;
                    float ci = floor(u / cell);
                    [unroll] for (int c = 0; c < 2; c++)
                    {
                        float id = ci - c;
                        float h = CelHash21(float2(id, salt + 31.0));
                        if (h > saturate(forks * cell * 0.6)) continue;
                        float x0 = id * cell + CelHash21(float2(id, salt + 2.0)) * cell * 0.5;
                        float lenA = cell * (0.35 + 0.6 * CelHash21(float2(id, salt + 3.0)));
                        float t = (u - x0) / lenA;
                        if (t < 0.0 || t > 1.0) continue;
                        float sgn = CelHash21(float2(id, salt + 4.0)) > 0.5 ? 1.0 : -1.0;
                        float h6 = CelHash21(float2(id, salt + 6.0));
                        float reach = (BOLT_PAD - 1.5) * (arc ? 0.35 + 0.55 * h6 : 0.25 + 0.45 * h6);
                        float kink = arc ? 0.8 * Zig(u, 0.05, salt + id) + 0.4 * Zig(u, 0.02, salt + id + 3.3)
                                         : 0.35 * Zig(u, 0.03, salt + id);
                        float yb = sgn * (pow(t, 0.8) * reach + kink) + centre;
                        float bw = arc ? max(0.55 * (1.0 - t * 0.6), 1.2 * onePx) : max(0.16 * (1.0 - t * 0.5), onePx);
                        float onBody = arc ? 1.0 : step(0.12, a) * step(a, 0.72);
                        br = max(br, saturate(1.0 - abs(y - yb) / bw) * lerp(0.9, 0.55, t) * onBody);
                    }
                }
                br *= inside;
                float thr = max(i.tone.w, 0.02);
                float mask = max(CelAAStep(thr, field), CelAAStep(0.3, br));
                // the clip's cross-section: a white core over most of the width with a pale cyan band and a cyan-blue
                // band about a pixel each at its edge (f36-f100: core, 1 px (174,221,242), 2 px (122,173,209)). The
                // ramp's two bands split at 0.5 and the core comes at 0.62, so the field is remapped for them to fall on
                // the outer 1.2 and 2.4 px of the local half-width, however wide the bolt is there
                // (a comet's: the clip's white core is 0.4-0.5 of its width from the tail to the head, cometprof.py; the
                // bands keep a pixel each where it is thin)
                float px = onePx / max(prof, 1e-3);
                float e1 = arc ? min(px * 1.2, 0.3) : clamp(px, 0.3, 0.5);
                float e2 = arc ? min(px * 2.4, 0.5) : clamp(px * 2.0, 0.52, 0.85);
                float fr = field < e1 ? 0.5 * field / e1
                         : field < e2 ? lerp(0.5, 0.62, (field - e1) / max(e2 - e1, 1e-4))
                         : lerp(0.62, 1.0, (field - e2) / max(1.0 - e2, 1e-4));
                float v = max(fr * bright, br * 0.8);
                half3 body = CelRamp(v, row, i.tone.y);
                body = MixFog(body, i.more.x);
                float cover = mask * saturate(i.tone.z * 1.4);
                // the bloom: a pale rim right at the body (c1, falling over 1.2 px) and the wide saturated blue light round
                // it (c0, over 8.5 px for a bolt, 10 for an arc), fading before the ribbon's edge. Fitted to a near bolt
                // of the clip, 1, 2, 6 and 13 px out of its body: (85,139,187), (54,101,153), (32,62,111), (26,39,72), and
                // to an arc's (86,142,178), (61,110,145), (46,77,104), (37,57,80): in pixels, whatever the width
                // (past an end of the body the distance is taken to that end, so the light closes round it)
                float outside = length(float2(beyond, max(dy - prof * inside, 0.0)));
                float dpx = outside * hwPx;
                float keepB = (1.0 - mask) * saturate((BOLT_PAD - 0.5 - abs(y)) * 0.5) * saturate((BOLT_PADL - beyond) * 0.6);
                if (!arc) keepB *= smoothstep(0.0, 0.3, a);
                half3 add = (CelPal(row, 1) * exp(-dpx / 1.2) + CelPal(row, 0) * exp(-dpx / (arc ? 6.0 : 11.0))) * keepB * i.tone.z;
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
