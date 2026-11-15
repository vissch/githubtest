// Phase: CelFx red_lasers (2026-11-15, lane/show/cel-red-lasers) - RedLasers: the rapid-fire red laser bolts of the
// owner's clip red_lasers_nolora_00001_ (what it is: celfx spec\eye\red_lasers.md). Built from electric_bolt's
// ElectricBolt shader (the camera-facing ribbon laid along a straight span with a body and an added glow, its soft
// dots, and neon_acid's light found from the camera's depth and multiplied into the scene by a second pass) and
// blue_laser's puff (the smoke). One kind, one draw, two passes.
//
// BEAM records (size.w >= 0): a ribbon along the span pos -> pos + vel x life (vel x life is the span; no gravity or
// drag). A LASER bolt: a thin straight line of one width that tapers to a fine point at its head (a short taper) and
// at its tail (a long one); across it a pink-white core, two red bands and the red body to its edge, then a glow of
// almost pure red light added round it in three falls, fitted to the clip's bolts (red light added over the plate at
// 1, 4, 8, 10, 18, 30, 46 and 62 px beyond a 4 px half-width: halo e-fold 1.08 half-widths, glow 4.6, and a faint
// skirt that reaches the ribbon's edge).
//   size.xyz  the body's full width (m) at birth, at death, ease.   size.w the bolt's length (share of the span).
//   extra     x the skirt's gain (1: the clip's), y the head's taper (share of the length; 0: 0.06),
//             z the head's share of the span at birth, w at death.
//   motion.w  the tail's taper (share of the length; 0: 0.35).
//   tone      x palette (c0 the glow, c1 the halo, c2 the outer band, c3 the inner band, core the centre, ink the
//             core's edge), y the core's gain, z opacity, w the clip: the bolt never reaches past this share of the
//             span (1: it stops at the target, its tail runs in after it).
//   erode.xyz the bolt's fade over its life (0: none, 1: gone): a miss fades as it goes off into the distance.
// DOT records (size.w < 0): a camera-facing quad at CelPosition (it moves: vel, gravity, drag, buoyancy); extra.x mode:
//   1 bloom: soft additive light, y falloff (0: 5), w the white-hot core's radius share (c2, c3, core).
//   2 pool: the same lying on the ground.
//   3 puff (smoke): a soft lumpy disc that covers, lit on top and dark underneath (blue_laser's); y edge softness,
//     z lump amplitude, w ragged rim (0: smooth); c2 its shadow, c3 its light; erode.xyz thins it over the life.
//   4 ray star: a white-hot core and z thin spikes of uneven length; y the spikes' root half-width, w the core share.
//   5 board wash: a soft rounded rectangle of light; y edge softness, z half width, w half height (quad units).
//   7 light: the light on whatever stands behind its card (the ground, the target): the lit point is found from the
//     camera's depth, so the card is never hidden. Pos the light, Axis a level axis of its own; size x its reach along
//     that axis (m), y up and down (m), z its far-side reach over its near (square to the axis); motion.x its near
//     reach square to the axis over x; vel how much of what it lights it multiplies in at its heart (rgb: the ground
//     and the dummy keep their own pattern), erode.xyz what it adds on top; extra y seconds to come up, z seconds to
//     go; tone y 1: level ground only, z its strength.
// _TWRedLaserLens (global; RedLasersRecipe.Lens): the scene's lens vignette (x its strength x 3, y its falloff); 0 none.
// The vignette takes most of the light from the corners, and the clip's bolts are as bright there as in the middle:
// colours are lifted by what the vignette will take (neon_acid's Unvignette).
Shader "TW/Cel/RedLasers"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+23" "IgnoreProjector"="True" }
        ZWrite Off ZTest LEqual Cull Off
        HLSLINCLUDE
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            // the ribbon is this many half-widths to each side: room for the glow and its skirt (the clip's red light is
            // still +10 over the ground 60 px from a bolt whose half-width is 4 px)
            #define LASER_PAD 17.0
            // and runs on this many half-widths past each end of the body, for the glow to close round the head and the tail
            #define LASER_PADL 8.0

            float4 _TWRedLaserLens;

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;                         // beam: x 0..1 along (tail..head), y across (half-widths), z metres along, w half-width (m). dot: xy -1..1, z seed, w -1
                nointerpolation float4 tone : TEXCOORD1;         // x palette, y core gain, z opacity, w erosion / fade
                nointerpolation float4 shape : TEXCOORD2;        // beam: x skirt gain, y head taper, z seed, w length (m) | dot: mode, y, z, w
                float4 more : TEXCOORD3;                         // x fog, y far, z kind (0 beam, -1 dot), w beam: tail taper
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
                        float rise = saturate(age / max(r.extra.y, 0.005)), fall = saturate((r.life - age) / max(r.extra.z, 0.005));
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
                // ---- a laser bolt: the window [head - length, head] of the span, clipped at tone.w
                float3 span = r.vel * r.life;                     // start -> end
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                float head = lerp(r.extra.z, r.extra.w, k);
                float winLen = max(r.size.w, 1e-4);
                float s0 = max(head - winLen, 0.0);
                float s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                // the ribbon runs on LASER_PADL half-widths past both ends for the glow; the fragment cuts the body at them
                float padS = width * 0.5 * LASER_PADL / lenB;
                float s = lerp(s0 - padS, s1 + padS, v.uv.x);
                o.ends = float2(s0, s1) * lenB;
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float halfW = width * 0.5;
                // going out the line keeps at least ~1.3 px of half-width (2.5 px wide at Far)
                halfW = max(halfW, 1.3 * saturate(_CelLod.x * 0.5) * PixelMetres(p) * alive * open);
                float across = v.uv.y * 2.0 - 1.0;
                float3 wb = p + side * across * halfW * LASER_PAD;
                o.positionWS = wb;
                o.positionCS = TransformWorldToHClip(wb);
                // the bolt's own 0..1 is measured against its full length, so a bolt clipped at the target keeps its tail
                // taper and loses its head into the board (not clamped here: the pads run past 0 and 1)
                float a = (s - (head - winLen)) / winLen;
                o.beam = float4(a, across * LASER_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y, r.seed, winLen * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), farK, 0.0, r.motion.w);
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
                if (_TWRedLaserLens.x <= 0.0) return col;
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWRedLaserLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWRedLaserLens.y, 0.01)));
                float lift = pow(1.0 / keep, share);
                float3 lin = SRGBToLinear(max(col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float gain = max(min(lift, 1.0 / max(top, 1e-3)), 1.0);
                // the bolts' own colours (share 1) take the whole lift, each channel clipped (a white-hot core in a dark
                // corner goes on to white); the ground light's added part keeps its hue
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
                    if (mode > 2.5 && mode < 3.5)
                    {
                        // a puff of smoke (blue_laser's): a lumpy disc with a soft edge that covers, each lobe lit on top
                        // and dark underneath (a cartoon cloud); with a ragged rim (w) it mottles and thins as it erodes
                        float sd = i.beam.z * 37.0;
                        float ang = atan2(q.y, q.x);
                        float lump = 0.5 * sin(ang * 3.0 + sd) + 0.3 * sin(ang * 5.0 + sd * 1.7 + 1.3) + 0.2 * sin(ang * 8.0 + sd * 2.3) + 0.15 * sin(ang * 13.0 + sd * 3.1);
                        float rag = i.shape.w;
                        float n2 = CelValue2(q * 4.5 + float2(sd, sd * 0.37)) * 0.6 + CelValue2(q * 9.0 + float2(sd * 1.3, sd)) * 0.4;
                        float reach = 1.0 - i.shape.z * (0.5 + 0.5 * lump) - rag * (n2 - 0.5);
                        float fieldP = saturate(1.0 - rr / max(reach, 0.2)) * (1.0 - rag * 0.8 * (n2 - 0.5));
                        float edge = max(i.shape.y, 0.01);
                        // the clip's smoke fades translucent rather than shrinking: the erosion is mostly a fade, a little a shrink
                        float aP = smoothstep(thrS * 0.35, thrS * 0.35 + edge, fieldP) * saturate(i.tone.z) * (1.0 - thrS);
                        half3 cp = lerp(CelPal(row, 2), CelPal(row, 3), saturate(fieldP * 1.6));
                        cp *= lerp(0.78, 1.12, saturate(q.y * 0.9 + 0.5)) * lerp(0.92, 1.04, saturate(fieldP * 2.0)) * lerp(1.0 - 0.3 * rag, 1.0 + 0.3 * rag, n2);
                        cp = MixFog(cp, i.more.x);
                        cp = Unvignette(cp, i.positionCS.xy, 1.0);
                        return half4(cp * aP, aP);
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

                // ---- a laser bolt
                float a = i.beam.x, y = i.beam.y, u = i.beam.z;
                // half-widths past either end of the body (0 on it): the pads only carry the glow
                float beyond = max(max(i.ends.x - u, u - i.ends.y), 0.0) / max(i.beam.w, 1e-4);
                float inside = step(beyond, 0.0);
                float hwPx = i.beam.w / max(PixelMetres(i.positionWS), 1e-6);
                float onePx = 1.0 / max(hwPx, 0.5);
                // the width along the bolt: one width over its middle, a fine point at the head (a short taper) and at
                // the tail (a long one), measured on the bolt's full length so a bolt in the board keeps its tail
                float headT = i.shape.y > 0.0 ? i.shape.y : 0.06;
                float tailT = i.more.w > 0.0 ? i.more.w : 0.35;
                float aa = saturate(a);
                float prof = min(pow(saturate(aa / tailT), 0.75), pow(saturate((1.0 - aa) / headT), 0.6));
                float dy = abs(y);
                float ub = dy / max(prof, 1e-3);                     // 0 on the centre line, 1 at the body's edge
                float pxU = onePx / max(prof, 1e-3);
                float cover = (1.0 - smoothstep(1.0 - pxU * 0.7, 1.0 + pxU * 0.7, ub)) * inside * step(1e-3, prof);
                float fade = 1.0 - saturate(i.tone.w);
                // across the body (the clip's, from the core out): the centre (core), the core's edge (ink) to 0.45 of
                // the half-width, the inner band (c3) at 0.75, the outer band (c2) at the edge
                half3 cc = CelPalCore(row).rgb * i.tone.y;
                half3 ce = CelPalInk(row).rgb * i.tone.y;
                half3 b1 = CelPal(row, 3), b2 = CelPal(row, 2);
                half3 body = ub < 0.45 ? lerp(cc, ce, ub / 0.45)
                           : ub < 0.75 ? lerp(ce, b1, (ub - 0.45) / 0.3)
                           : lerp(b1, b2, saturate((ub - 0.75) / 0.25));
                body = MixFog(body, i.more.x);
                cover *= saturate(i.tone.z * 1.4) * fade;
                // the glow: half-widths out from the body's edge (past an end of the body, to that end), a halo and a glow
                // of red light and the faint skirt; it thins with the body toward the ends
                float xo = length(float2(beyond, max(dy - prof * inside, 0.0)));
                float skirt = i.shape.x * saturate((LASER_PAD - 1.0 - dy) / 4.0) * saturate((LASER_PADL - beyond) / 3.0);
                float keepG = (1.0 - cover) * saturate((LASER_PAD - 0.5 - dy) * 0.5) * saturate((LASER_PADL - beyond) * 0.5) * sqrt(saturate(prof + 0.15 * inside));
                half3 add = (CelPal(row, 1) * exp(-xo / 1.08) + CelPal(row, 0) * (exp(-xo / 4.6) + 0.17 * skirt)) * keepG * i.tone.z * fade;
                add = AddFog(add, i.positionWS, i.more.x, i.more.y);
                half3 col = body * cover + add;
                return half4(Unvignette(col, i.positionCS.xy, 1.0), cover);
            }
        ENDHLSL
        Pass
        {
            Name "CelRedLasersLight"
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
            Name "CelRedLasers"
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
