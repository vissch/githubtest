// Phase: CelFx swirling rockets (2026-11-15, r01: built by eye against swirling_rockets_nolora) - one GPU pool for the
// whole effect: six rockets on looping paths (body, flame, beaded smoke trail, spark dashes), the hit's star, the
// fireball they burst into on the dummy, its embers and debris, and the light on the ground. Every shape is a field in
// the fragment; nothing is a texture (the light reads the camera's depth). The clip is drawn on ONES: the age a record
// is drawn at is stepped 24 times a second on the EFFECT's clock (Extra.w carries the seconds from the effect's start
// to the record's birth, so every record of one effect steps on the same frames). Built from CelFireball_URP: the
// fleck, star, cloud and light modes are its own (the cloud runs on this clip's faster frame clock); the trail and
// the body are new.
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 trail    one stretch of one rocket's path, the stretch the head flies in Size.x seconds: a Catmull-Rom spline
//             from Pos (u 0) to Vel (u 1), Axis the point before and Erode.xyz the point after. Born = when the head
//             is at Pos. It draws what lies on that stretch: the FLAME while the head is less than its length past
//             (a white core at the nozzle, a yellow-orange jet tapering back along the path, bending with it) and
//             the SMOKE TRAIL laid where it flew (round cel puffs, one every CellL metres of path, swelling, then
//             thinning into wisps and gone, all in place). Size x the stretch's seconds, y its length (m), z the
//             strip's half-width (m), w the age (s) by which its smoke is gone (the clip clears the swirl's old
//             loops; 99 for none). Erode.w the path length at Pos (m: the puffs
//             are laid on the whole path's grid, so stretches join). Motion xyzw the path length the head has
//             reached 2, 4, 6 and 8 frames after the stretch's end (the flame reaches back over it then). Extra y
//             1 if the stretch's end is the rocket's end (its round cap is drawn) + 2 x how much its smoke swells
//             as it ages (0..255: the swirl's none, the final approach's to 1.45x), z the seconds from Born to the
//             rocket's end (a hit or flying into another's flame: the flame dies there).
//             Tone x the flame row + 256 x the smoke row, w the glints' row (the spark dashes') + 256 x the
//             smoke's second row (its warm belly, the glow behind the jet, the warm ink); y and z the
//             light the flames throw on its smoke over its life (six samples 0.42 s apart, three 8-bit values
//             to a float: z is not the opacity on a trail). It also draws
//             GLINTS: short spark dashes that light up along the path and round it once the flame has gone by,
//             three frames each, fewer as the trail ages (the clip's rings of dashes round its old loops).
//  1 drop     a ballistic fleck. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at
//             death, w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf
//             (a point at both ends), 3 a stick (a thin bar, the burst's debris). Two tones: body and its light.
//  2 splat    the hit: a star of spikes facing the eye. Size x its half-extent (m). Extra y metres toward the eye.
//  3 cloud    the burst, on one card: a mound of round lobes that pops out of the hit, burns white and yellow with an
//             orange rim, cools to char with glowing cracks, turns to brown smoke, rises and breaks up. Pos its
//             middle, Size x its half-width at its peak (m), y its half-height, z metres it is drawn toward the eye,
//             w how many lobes (up to 28). Tone x the fire row + 256 x the smoke row. Extra y metres it rises by the
//             end of its life. Its frame clock is the fireball clip's, reached through this clip's (Remap below).
//  4 body     a rocket's body for one stretch of its path (the trail's spline, Size.x seconds): a straight cel
//             missile facing along the path at the head, the nozzle a little ahead of the flame's core. Size y its
//             length (m), z its radius (m). Palette: c0 shadow, c1 mid, c2 lit, c3 highlight, core the nose, ink.
//  7 glow     the light on whatever stands behind it (CelFireball_URP's, unchanged).
// Palette rows: c0 rim, c1 shade, c2 body, c3 light, core pale, ink.
//
// Two passes, one draw: the light pass first (only the glows draw in it), then everything, in the records' order.
// _TWAcidLens (global; SwirlingRocketsRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff
// power; 0 none. Every colour is lifted by what the vignette will take from it, as far as white allows.
Shader "TW/Cel/SwirlingRockets"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+17" "IgnoreProjector"="True" }
        ZWrite Off ZTest LEqual Cull Off
        HLSLINCLUDE
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            float4 _TWAcidLens;

            // metres of path a smoke puff is laid every
            #define SR_CELL 0.17                                    // r22: the clip's beads ~0.2 m apart (0.15)
            // metres the nozzle sits ahead of the path point (the tracked hot core is a little behind it)
            #define SR_NOZZLE 0.16
            // metres the flame runs back along the path from the nozzle (SwirlingRocketsRecipe.FlameLength)
            #define SR_FLAME 2.2                                    // r27: the clip's 1.7-2.6 m (1.8)
            // r19: how soft every smoke edge is, as a share of the body's radius (the owner on r18: "i barely see any
            // soft edges")
            #define SR_SOFT 0.34
            // r20: how soft the burst's lobes' edges are, as a share of their radius
            #define SR_CLOUD_SOFT 0.20

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float2 q : TEXCOORD1;                       // the card's own coordinates (per mode)
                float4 flow : TEXCOORD2;                    // per mode, interpolated
                nointerpolation float4 tone : TEXCOORD3;
                nointerpolation float4 state : TEXCOORD4;   // mode, age (stepped), life fraction, seed
                nointerpolation float4 shape : TEXCOORD5;   // per mode
                nointerpolation float4 shape2 : TEXCOORD6;  // per mode
                float fog : TEXCOORD7;
                nointerpolation float4 shape3 : TEXCOORD8;  // per mode
                nointerpolation float4 p0 : TEXCOORD9;      // trail: the spline's four points (w: per mode)
                nointerpolation float4 p1 : TEXCOORD10;
                nointerpolation float4 p2 : TEXCOORD11;
                nointerpolation float4 p3 : TEXCOORD12;
            };

            // the uniform Catmull-Rom spline through a, b, c, d: b at u 0, c at u 1
            float3 Spline(float3 a, float3 b, float3 c, float3 d, float u)
            {
                float u2 = u * u, u3 = u2 * u;
                return 0.5 * (2.0 * b + (c - a) * u + (2.0 * a - 5.0 * b + 4.0 * c - d) * u2 + (3.0 * b - a - 3.0 * c + d) * u3);
            }
            float3 SplineD(float3 a, float3 b, float3 c, float3 d, float u)
            {
                return 0.5 * ((c - a) + 2.0 * (2.0 * a - 5.0 * b + 4.0 * c - d) * u + 3.0 * (3.0 * b - a - 3.0 * c + d) * u * u);
            }

            // the cloud's frame clock: the fireball clip's frame (whose cloud this is) at this clip's frame. This clip's
            // burst is hot only to f105, charred by f110 and smoke by f112: it cools three times as fast, then lingers.
            float Remap(float f)
            {
                if (f < 98.0) return lerp(28.0, 32.0, saturate((f - 96.0) / 2.0));
                if (f < 104.0) return lerp(32.0, 44.0, (f - 98.0) / 6.0);
                if (f < 107.0) return lerp(44.0, 62.0, (f - 104.0) / 3.0);
                if (f < 110.0) return lerp(62.0, 70.0, (f - 107.0) / 3.0);
                if (f < 116.0) return lerp(70.0, 82.0, (f - 110.0) / 6.0);
                if (f < 136.0) return lerp(82.0, 102.0, (f - 116.0) / 20.0);
                return lerp(102.0, 118.0, saturate((f - 136.0) / 14.0));
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), alive = CelAlive(r);
                float mode = r.extra.x, delay = r.extra.w;
                float tick = floor((t + delay) * 24.0 + 0.01);          // the drawing: a new one every frame
                float a = max(tick / 24.0 - delay, 0.0);                // the record's age at that drawing
                float k = saturate(a / max(r.life, 1e-4));
                float3 vr = UNITY_MATRIX_V[0].xyz, vu = UNITY_MATRIX_V[1].xyz;
                float2 cq = v.uv * 2.0 - 1.0;
                float3 w = r.pos;
                float2 q = cq;
                float4 flow = 0.0, shape = 0.0, shape2 = 0.0, shape3 = 0.0;
                float4 p0 = 0.0, p1 = 0.0, p2 = 0.0, p3 = 0.0;
                float wholeFrame = 0.0;
                if (mode < 0.5 || (mode > 3.5 && mode < 4.5))
                {
                    p0 = float4(r.axis, 0.0); p1 = float4(r.pos, 0.0); p2 = float4(r.vel, 0.0); p3 = float4(r.erode.xyz, 0.0);
                }
                if (mode < 0.5)
                {
                    // ---- a stretch of trail: a strip along the spline, widened at both ends by its half-width so a
                    // round cap and a puff's overhang are covered; nothing ahead of the head is laid out
                    float T = max(r.size.x, 1e-3), len = max(r.size.y, 1e-4), W = r.size.z;
                    float uHead = a / T + SR_NOZZLE / len;
                    float uu = min(v.uv.x, max(uHead, 0.0));
                    float3 c = Spline(r.axis, r.pos, r.vel, r.erode.xyz, uu);
                    float3 tg = SplineD(r.axis, r.pos, r.vel, r.erode.xyz, saturate(uu));
                    tg = tg / max(length(tg), 1e-5);
                    float3 toEye = normalize(_WorldSpaceCameraPos - c);
                    float3 side = cross(tg, toEye);
                    float sl = length(side);
                    side = sl > 1e-3 ? side / sl : vr;
                    float3 tgS = tg - toEye * dot(tg, toEye);
                    float tl = length(tgS);
                    tgS = tl > 1e-3 ? tgS / tl : vu;
                    float endPush = v.uv.x < 0.001 ? -1.0 : (v.uv.x > 0.999 || uu < v.uv.x ? 1.0 : 0.0);
                    w = c + side * cq.y * W + tgS * endPush * W;
                    q = float2(uu, cq.y);
                    shape = float4(T, len, W, r.size.w);
                    shape2 = float4(r.erode.w, r.extra.y, r.extra.z, 0.0);
                    shape3 = r.motion;
                }
                else if (mode < 1.5)
                {
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float3 p = r.pos + r.vel * CelDragInt(r.motion.y, a) + 0.5 * G * a * a;
                    float3 vel = r.vel * exp(-r.motion.y * a) + G * a;
                    float3 vV = mul((float3x3)UNITY_MATRIX_V, vel);
                    float sp = length(vV.xy);
                    float2 d2 = sp > 1e-3 ? vV.xy / sp : float2(0.0, -1.0);
                    float rad = CelCurve(r.size.xyz, k);
                    float len = max(rad, sp * r.size.w);
                    w = p + (vr * d2.x + vu * d2.y) * cq.x * len + (vu * d2.x - vr * d2.y) * cq.y * rad;
                    shape = float4(len / max(rad, 1e-4), r.extra.y, 0.0, 0.0);
                }
                else if (mode < 2.5)
                {
                    float3 p = r.pos + normalize(_WorldSpaceCameraPos - r.pos) * r.extra.y;
                    w = CelBillboard(p, cq * r.size.x);
                    shape = float4(tick, 0.0, 0.0, 0.0);
                }
                else if (mode < 3.5)
                {
                    // the cloud: one card, drawn toward the eye at the size it would have where it stands; it rises
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(r.size.z, dist * 0.5);
                    const float Card = 1.7;
                    float F = Remap(96.0 + a * 24.0);
                    float3 mid = r.pos + float3(0.0, r.extra.y * smoothstep(80.0, 118.0, F), 0.0);
                    w = CelBillboard(mid + toEye / dist * pull, cq * r.size.xy * Card * ((dist - pull) / dist));
                    float aspect = r.size.y / max(r.size.x, 1e-3);
                    q = cq * Card * float2(1.0, aspect);                // in the cloud's half-widths, both ways
                    shape = float4(tick, a, aspect, r.size.w);
                }
                else if (mode < 4.5)
                {
                    // ---- a rocket's body at the head: a straight card along the path's tangent as seen
                    float T = max(r.size.x, 1e-3), L = r.size.y, R = r.size.z;
                    float u = saturate(a / T);
                    float3 c = Spline(r.axis, r.pos, r.vel, r.erode.xyz, u);
                    float3 tg = SplineD(r.axis, r.pos, r.vel, r.erode.xyz, u);
                    tg = tg / max(length(tg), 1e-5);
                    float3 nozzle = c + tg * SR_NOZZLE;
                    float3 toEye = normalize(_WorldSpaceCameraPos - nozzle);
                    float3 tgS = tg - toEye * dot(tg, toEye);
                    float tl = length(tgS);
                    float3 ax = tl > 1e-3 ? tgS / tl : vu;
                    float3 up = normalize(cross(toEye, ax));
                    up *= dot(up, vu) >= 0.0 ? 1.0 : -1.0;             // the card's +y is up the screen
                    // end-on it is foreshortened, but less than true: the clip draws its rockets side-on
                    float seenF = max(pow(max(tl, 1e-4), 0.45), 0.30);
                    float seenL = L * seenF;
                    float x = lerp(-0.22, 1.08, v.uv.x);                // body lengths from the nozzle forward
                    w = nozzle + ax * x * seenL + up * cq.y * R * 2.6;       // r21: room for the bigger fins
                    q = float2(x, cq.y * 2.6);
                    // r14: its motion blur, a quarter of the path it flies in a frame each way, in body lengths
                    float spd = distance(r.pos, r.vel) / T;
                    shape = float4(seenL / max(R, 1e-4), seenF, 0.25 * spd / 24.0 / max(seenL, 0.05), 0.0);
                }
                else
                {
                    float reachZ = r.size.x * max(r.size.w, 0.01);
                    float R = max(max(r.size.x, r.size.y), reachZ);     // the ball that holds the light
                    float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                    w = CelBillboard(r.pos, cq * R * cover);
                    wholeFrame = step(dist, R * 1.02) * alive;
                    float3 right = normalize(float3(r.axis.x, 0.0, r.axis.z));
                    float up = saturate(a / max(r.extra.y, 0.01)), down = saturate((r.life - a) / max(r.extra.z, 0.01));
                    flow = float4(right.x, right.z, clamp(r.size.z, 0.05, 1.0), max(r.size.y, 0.05));
                    shape = float4(r.size.x, reachZ, up * down, r.vel.x);
                    shape2 = float4(r.erode.xyz, r.vel.y);
                    shape3 = float4(r.pos, r.vel.z);
                }
                w = lerp(r.pos, w, alive);
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                if (mode > 6.5)
                {
                    if (wholeFrame > 0.5) o.positionCS = float4(cq, 0.0, 1.0);
                    o.positionCS.z = o.positionCS.w * lerp(UNITY_NEAR_CLIP_VALUE, UNITY_RAW_FAR_CLIP_VALUE, 0.0002);
                }
                o.q = q;
                o.flow = flow;
                o.tone = r.tone;
                o.state = float4(mode, a, k, r.seed);
                o.shape = shape;
                o.shape2 = shape2;
                o.shape3 = shape3;
                o.p0 = p0; o.p1 = p1; o.p2 = p2; o.p3 = p3;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            // a step at edge, w wide
            float Step(float edge, float v, float w) { return smoothstep(edge - w, edge + w, v); }

            // the colour as it has to be written for the lens vignette to leave it as it is (the header)
            half3 Unvignette(half3 col, float2 pixel, float share)
            {
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWAcidLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWAcidLens.y, 0.01)));
                float lift = pow(1.0 / keep, share);
                float3 lin = SRGBToLinear(max(col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float gain = max(min(lift, 1.0 / max(top, 1e-3)), 1.0);
                float3 lifted = LinearToSRGB(min(lin * min(lift, gain * 1.2), 1.0));
                return _TWAcidLens.x > 0.0 ? lifted : col;
            }

            // a star of spikes round a ragged lump (CelFireball_URP)
            float Star(float2 q, float seed, float heart, float reach, float thin, float w)
            {
                float ang = atan2(q.y, q.x);
                float lump = 0.80 + 0.20 * sin(ang * 2.0 + seed * 33.0) + 0.16 * sin(ang * 3.0 + seed * 40.0) + 0.10 * sin(ang * 5.0 + seed * 57.0) + 0.07 * sin(ang * 9.0 + seed * 91.0);
                float m = 1.0 - Step(heart * lump, length(q * float2(1.18, 0.92)), w);
                [unroll] for (int j = 0; j < 16; j++)
                {
                    float h = CelHash21(float2(j + 1.0, seed * 77.0 + 3.0));
                    float h2 = CelHash21(float2(j + 31.0, seed * 131.0 + 5.0));
                    float an = (j + 0.25 + 0.5 * h) * 0.3926991;
                    float2 d = float2(cos(an), sin(an));
                    float L = lerp(heart * 0.8, reach, h2 * h2 * h2);
                    float al = dot(q, d), ac = abs(q.x * d.y - q.y * d.x);
                    float wd = thin * pow(saturate(1.0 - al / max(L, 1e-3)), 0.7);
                    float spike = (1.0 - Step(wd, ac, w)) * step(0.0, al) * (1.0 - Step(L, al, w)) * step(0.33, h);
                    float2 b = q - d * L * (1.12 + 0.22 * h);
                    float bead = (1.0 - Step(0.014 + 0.014 * h, length(float2(dot(b, d) * 0.45, b.x * d.y - b.y * d.x)), w)) * step(0.4, h2);
                    m = max(m, max(spike, bead));
                }
                return m;
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

            float GlowOn(Varyings i, float3 spot, float3 facing)
            {
                float3 d = spot - i.shape3.xyz;
                float across = d.x * i.flow.x + d.z * i.flow.y, back = d.z * i.flow.x - d.x * i.flow.y;
                float t = length(float3(across / i.shape.x, back / (i.shape.y * (back > 0.0 ? i.flow.z : 1.0)), d.y / i.flow.w));
                float level = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                // r25: no rim (the clip's light is a broad soft wash): sinks smoothly to nothing at its reach
                float t2 = saturate(t * t);
                return exp(-2.2 * t2) * (1.0 - t2) * (1.0 - t2) * i.shape.z * i.tone.z * lerp(1.0, level, i.tone.y);
            }

            half3 GlowSeen(float3 spot)
            {
                return CelAdditiveFog(half3(1.0, 1.0, 1.0), spot, ComputeFogFactor(TransformWorldToHClip(spot).z));
            }

            Varyings vertLight(Attributes v, uint instanceID : SV_InstanceID)
            {
                Varyings o = vert(v, instanceID);
                if (o.state.x < 6.5) o.positionCS = float4(2.0, 2.0, 2.0, 1.0);
                return o;
            }

            half4 fragLight(Varyings i) : SV_Target
            {
                float3 spot = SceneSpot(i);
                float f = GlowOn(i, spot, cross(ddy(spot), ddx(spot)));
                return half4(float3(i.shape.w, i.shape2.w, i.shape3.w) * f * GlowSeen(spot), 0.0);
            }

            // ---- the trail
            // the flame's half-width (m) at `back` metres behind the nozzle, for a flame `fl` long: a blunt white-hot
            // head as wide as the body right at the nozzle (the clip: 25 px across at the dummy), fullest a fifth of
            // a metre back, tapering to a point at its end
            float FlameHalf(float back, float fl, float mb)
            {
                // r03: a round white-hot bulb at the tracked core, then a narrow jet (the clip, f80-f88: 0.33-0.37 m
                // across at the core, 0.10-0.17 m from 0.25 m back on); r14: the bulb smeared along the path by its
                // motion, mb metres each way (a capsule)
                float bc = back - SR_NOZZLE;
                bc = (bc < 0.0 ? -1.0 : 1.0) * max(abs(bc) - mb, 0.0);
                float bulb = 0.18 * sqrt(saturate(1.0 - (bc / 0.21) * (bc / 0.21)));
                float jet = 0.140 * pow(saturate(1.0 - back / max(fl, 0.05)), 0.4) * smoothstep(-0.05, 0.10, back);   // r27: ^0.4
                return max(bulb, jet);
            }

            // r19: one smoke body's colour (worked out for the front body and the one behind it, so the crease between two
            // lumps is blended soft): n the pixel's place in the body (unit at its outline), tubeN its place across the
            // trail, rr the body's radius, inkv / gv / agev its ink, glow and age, W the flames' light on it, rimD how
            // deep inside the trail's whole outline the pixel is (m)
            half3 SmokeShade(float2 n, float2 tubeN, float rr, float inkv, float gv, float agev, float W, float mpp, float sd,
                float rimD, half3 cBase, half3 wBase, half3 cDisc, half3 wDisc, half3 cBelly, half3 wBelly, half3 sHot,
                half3 fOrange, half3 sInk, half3 wInk)
            {
                // shaded as one tube with bumps: half the puff's own normal, half the trail's
                float2 mixN = lerp(n, tubeN, 0.5);
                // up the rope as the light sees it (from above, a little from the eye's left), wavy
                float up = mixN.y * 0.94 - mixN.x * 0.22 + 0.12 * (CelValue2(n * 2.3 + sd) - 0.5);
                float belly = 1.0 - Step(-0.40, up, 0.16);              // r19: soft (r16 0.09)
                half3 base = lerp(cBase, wBase, W), disc = lerp(cDisc, wDisc, W), bel = lerp(cBelly, wBelly, W);
                // r16/r17: the body 0.95x, a little darker with age; the lighter disc x1.06 fading into the body
                float ageS = smoothstep(0.45, 1.6, agev);
                half dk = 0.95 * lerp(1.0, 0.95, ageS);
                base *= dk; bel *= dk;
                disc = lerp(disc * 1.06, base, 0.55 * ageS);
                // the top of each puff a big soft lighter disc (r19: softer still, 0.24)
                float dsc = 1.0 - Step(0.54, length(n - float2(-0.12, 0.22)), 0.24 + 2.5 * mpp / max(rr, 1e-4));
                half3 col = lerp(base, disc, dsc * (1.0 - belly));
                col = lerp(col, bel, belly);
                // r17: 15% less saturated (the clip's smoke is more neutral)
                col = lerp(col, dot(col, half3(0.299, 0.587, 0.114)), 0.15);
                // r09-r18: the glow on the top band; an old trail's tops glow again as a flame passes
                half3 glowC = lerp(sHot, fOrange, saturate(gv * 3.0 - 2.0));
                float gLit = max(gv, 0.45 * W * smoothstep(0.3, 0.7, agev));
                col = lerp(col, glowC, gLit * (0.06 + 0.94 * smoothstep(0.40, 0.78, up)));
                // r19: the ink a soft dark rim just inside the outline, strongest underneath (r07-r18 a hard 2 px band)
                half3 inkC = lerp(sInk, wInk, W * saturate(up * 1.5));
                float rim = 1.0 - smoothstep(0.0, 0.30 * rr + 1.5 * mpp, rimD);
                col = lerp(col, inkC, rim * inkv * lerp(0.70, 0.20, smoothstep(-0.5, 0.6, up)));
                return col;
            }

            half4 Trail(Varyings i, float a, float seed, float pix)
            {
                float T = i.shape.x, len = i.shape.y, fl = SR_FLAME, wipe = i.shape.w;
                // r23: Extra.y + 512 when the rocket's last trail keeps no end rule (the clip's R1)
                float keepEnd = step(511.5, i.shape2.y), yLo = fmod(i.shape2.y, 512.0);
                float s1 = i.shape2.x, isEnd = fmod(yLo, 2.0), endAt = i.shape2.z;
                // r12: how much this stretch's smoke swells late in its life (the clip's swirl trails thin a little,
                // its final approach's swell to 1.45x)
                float swA = floor(yLo / 2.0) / 255.0;            // 0 the swirl's trails, 1 the final approach's
                float lateSwell = lerp(-0.15, 0.45, swA);
                // r17: the lit tops' glow lasts to 1.1 s on the swirl's trails, 0.75 s on the approach's (the clip's
                // tops at 10-20 frames)
                float glowEnd = lerp(1.10, 0.65, swA);                  // r18: the approach's 0.65 (0.75)
                float3 A = i.p0.xyz, B = i.p1.xyz, C = i.p2.xyz, D = i.p3.xyz;
                float3 O = _WorldSpaceCameraPos;
                float3 dir = normalize(i.positionWS - O);
                // the head's path length now: along this stretch while it is in it, then from the four samples after
                float aa = min(a, endAt);
                float sH;
                if (aa <= T) sH = s1 + len * aa / T;
                else
                {
                    float f2 = (aa - T) * 12.0;                         // in steps of two frames after the end
                    float4 m = i.shape3;
                    float lo = f2 < 1.0 ? s1 + len : (f2 < 2.0 ? m.x : (f2 < 3.0 ? m.y : m.z));
                    float hi = f2 < 1.0 ? m.x : (f2 < 2.0 ? m.y : (f2 < 3.0 ? m.z : m.w));
                    sH = f2 >= 4.0 ? m.w + (m.w - m.z) * (f2 - 4.0) : lerp(lo, hi, frac(f2));
                }
                float dead = saturate((a - endAt) * 10.0);              // the flame dies a tenth of a second after the end

                // the point of the path nearest this pixel's ray (two Gauss-Newton steps from the strip's own place)
                float u = saturate(i.q.x);
                [unroll] for (int it = 0; it < 2; it++)
                {
                    float3 X = Spline(A, B, C, D, u) - O, Xd = SplineD(A, B, C, D, u);
                    float3 g = X - dir * dot(dir, X), gd = Xd - dir * dot(dir, Xd);
                    u = saturate(u - dot(g, gd) / max(dot(gd, gd), 1e-8));
                }
                float3 Xu = Spline(A, B, C, D, u) - O;
                float across = length(Xu - dir * dot(dir, Xu));         // metres from the path, as seen
                float depthU = dot(dir, Xu);
                float mpp = pix * max(depthU, 0.1);                     // metres a pixel spans there
                float sU = s1 + len * u;
                float back = sH + SR_NOZZLE - sU;                       // metres behind the nozzle
                // a stretch draws its own end cap only when it is the rocket's end (else the next one draws it)
                float capEnd = (u > 0.999 && isEnd < 0.5) ? 0.0 : 1.0;

                // ---- the smoke: puffs on the path's grid, two a cell (a core puff and a small bump on its top or bottom
                // edge). A puff belongs to the stretch that holds its middle.
                // r07 (the clip's trail profile top to bottom over every level stretch, and at 6x f96): a tan rope,
                // darker along its bottom third, its puffs' tops big soft lighter discs, all of it inside a 2 px
                // near-black line; warmer while flames are near (a rocket's own smoke pale right behind its jet), cool
                // grey-tan once none is. A section goes at once: it deflates into a sliver in a third of a second (the
                // clip f72-f76); the swirl's old loops are cleared by frame ~84-92 (the recipe's wipe age, Size.w).
                half3 col = 0.0;
                float al = 0.0;
                uint rowS = CelRowB(i.tone.x), rowS2 = CelRowB(i.tone.w);
                half3 cBelly = CelPal(rowS, 0), cBase = CelPal(rowS, 1), cDisc = CelPal(rowS, 2);
                half3 wBase = CelPal(rowS, 3), wDisc = CelPalCore(rowS).rgb, sInk = CelPalInk(rowS).rgb;
                half3 wBelly = CelPal(rowS2, 0), sHot = CelPal(rowS2, 1), wInk = CelPal(rowS2, 2);
                uint rowF = CelRowA(i.tone.x);
                float sd = seed * 37.0;
                float3 vR = UNITY_MATRIX_V[0].xyz, vU = UNITY_MATRIX_V[1].xyz;
                float inkM = 2.0 * mpp;                                 // the ink line's width (m) at the path's depth
                float bestDepth = 1e9, bestD = 2.0, bestBack = 0.0, bestR = 0.1, bestInk = 0.0, bestG = 0.0, bestAge = 0.0;
                float2 bestN = 0.0;
                // r19: the body behind the front one (its crease with the front one is blended)
                float secDepth = 1e9, secD = 2.0, secR = 0.1, secInk = 0.0, secG = 0.0, secAge = 0.0;
                float2 secN = 0.0;
                float cover = 0.0;                                      // the own puffs' cover, ink bands included
                // how deep inside the trail's whole outline this pixel is (m): every puff near it counts, the next
                // stretch's too, so the ink runs round the trail and never across it where two stretches meet
                float inUnion = -1e5;
                // r14: the ink of the element whose outline is nearest (0 when it is another stretch's: the veil is
                // drawn once), its glow (the colour of a soft edge), and the glow wake over the young smoke
                float uInk = 0.0, uG = 0.0, wake = 0.0, wakeAge = 0.0;
                // r08: the rope's core, a tube along the path (the clip's young smoke is one smooth rope with gentle
                // bulges; the puffs swell out of it as it ages). Where this pixel is past the stretch's end its
                // distance is to the path's tangent line, so two stretches' tubes join with no ink between them; the
                // part past the end is the next stretch's to draw.
                {
                    float3 gT = Xu - dir * dot(dir, Xu);
                    float3 XdU = SplineD(A, B, C, D, u);
                    float3 gdT = XdU - dir * dot(dir, XdU);
                    float gg = max(dot(gdT, gdT), 1e-8);
                    float along = -dot(gT, gdT);
                    float3 gPerp = gT - gdT * (dot(gT, gdT) / gg);
                    float dlT = length(gPerp);
                    bool past = (u > 0.9999 && along > 0.0) || (u < 0.0001 && along < 0.0);
                    float ageU = a - u * T;
                    float backU = sH + SR_NOZZLE - sU;
                    if (ageU >= 0.035 && backU >= 0.22)
                    {
                        float lifeU = 1.70 + 0.45 * CelValue2(float2(sU / 1.6, sd * 3.1 + 7.0));
                        lifeU = lerp(lerp(1.05, lifeU, saturate((endAt - u * T) / 0.35)), lifeU, keepEnd);
                        lifeU = min(lifeU, max(wipe - u * T, 0.30));
                        float thinU = smoothstep(lifeU - 0.35, lifeU, ageU);
                        if (thinU < 1.0)
                        {
                            // r09: it only fills the necks between the lumps (0.62 of a middling one)
                            float swellU = 0.70 + 0.30 * smoothstep(0.0, 0.5, ageU) + lateSwell * smoothstep(0.5, 1.7, ageU);
                            float R0 = 0.040 * swellU * (1.0 - 0.72 * thinU) * (1.0 + 0.16 * (CelValue2(float2(sU * 3.3, sd + 5.0)) - 0.5));
                            float gU = (1.0 - smoothstep(0.15, glowEnd, ageU)) * (1.0 - dead);
                            // r14: motion blur: the young smoke's edge is soft, the hard cel edge by 0.3 s
                            float mbU = 1.0 - smoothstep(0.06, 0.30, ageU);
                            float inkT = smoothstep(0.30, 0.55, ageU) * (1.0 - thinU);
                            if (R0 - dlT > inUnion) { inUnion = R0 - dlT; uInk = past ? 0.0 : inkT; uG = gU; }
                            if (!past)
                            {
                                float inkP = inkT;
                                // r19: a soft edge, SR_SOFT of the radius (the young softer); no hard ink band
                                cover = max(cover, saturate((R0 - dlT) / max(mpp * (1.0 + 2.5 * gU) + R0 * (0.30 * mbU + SR_SOFT), 1e-5) + 0.5) * (1.0 - 0.55 * thinU * thinU));
                                // r14: the hot gas's motion blur: a soft streaky glow along the path over the young smoke
                                // r15: gone by 0.32 s (the clip's glow is on the tops by 8 frames), a little wider
                                float mbW = (1.0 - smoothstep(0.04, 0.42, ageU)) * (1.0 - dead);   // r28: to 0.42 s
                                float wW = 0.13 + 0.07 * smoothstep(0.0, 0.3, ageU);
                                wake = mbW * exp(-(dlT * dlT) / (wW * wW)) * (0.80 + 0.45 * (CelValue2(float2(sU * 1.7, dlT / wW * 2.2 + sd * 1.3)) - 0.5));
                                wakeAge = mbW;
                                float front = depthU - R0 * sqrt(saturate(1.0 - (dlT / R0) * (dlT / R0)));
                                float2 nT = float2(dot(-gPerp, vR), dot(-gPerp, vU)) / max(R0, 1e-4);
                                // r19: the two front bodies (the crease between them is blended soft)
                                if (dlT < R0 && front < secDepth)
                                {
                                    if (front < bestDepth)
                                    {
                                        secDepth = bestDepth; secD = bestD; secR = bestR; secInk = bestInk; secG = bestG; secAge = bestAge; secN = bestN;
                                        bestDepth = front;
                                        bestD = dlT / R0; bestR = R0; bestBack = backU; bestInk = inkP; bestG = gU; bestAge = ageU;
                                        bestN = nT;
                                    }
                                    else
                                    {
                                        secDepth = front; secD = dlT / R0; secR = R0; secInk = inkP; secG = gU; secAge = ageU; secN = nT;
                                    }
                                }
                            }
                        }
                    }
                }
                float sMid0 = s1 + len * saturate(i.q.x);
                float ci = floor(sMid0 / SR_CELL);
                [unroll] for (int n = -2; n <= 2; n++)
                {
                    float id = ci + n;
                    float h1 = CelHash21(float2(id, 17.0)), h2 = CelHash21(float2(id + 7.0, 29.0)), h3 = CelHash21(float2(id + 13.0, 41.0 + sd));
                    float h4 = CelHash21(float2(id + 19.0, 53.0 + sd)), h5 = CelHash21(float2(id + 23.0, 67.0 + sd));
                    float sc = (id + 0.30 + 0.4 * h1) * SR_CELL;
                    bool own = sc >= s1 && sc < s1 + len;
                    if (sc < s1 - 0.5 || sc >= s1 + len + 0.5 || sc < 0.0) continue;
                    float uc = (sc - s1) / len;
                    float ageC = a - uc * T;                            // seconds since the head passed it
                    float backC = sH + SR_NOZZLE - sc;
                    if (ageC < 0.035 || backC < 0.22) continue;
                    // a section of trail lives 0.55-2.65 s (the clip's die from 0.6 s to 2.6 s old, a metre or two of
                    // path at a time); the last half second of path before a rocket's end is gone within 0.55 s; and
                    // nothing outlives the stretch's wipe
                    // r11: 1.45-1.90 s (the clip's final-approach trails live 38-50 frames; the swirl's are cleared
                    // by the wipe, a rocket's last stretch by the end rules)
                    float life = 1.70 + 0.45 * CelValue2(float2(sc / 1.6, sd * 3.1 + 7.0));
                    // r13: the last 0.35 s of path before a hit lives 1.05 s (the clip's trails laid f96-103: ~26
                    // frames); a rocket gone in the air has the recipe's wipe
                    life = lerp(lerp(1.05, life, saturate((endAt - uc * T) / 0.35)), life, keepEnd);
                    life = min(life, max(wipe - uc * T, 0.30));
                    // r08: it swells out of the tube: a quarter of its size behind the flame, full by 0.9 s, 1.3x by 2.2 s
                    // r09: near full size from the start (the clip's lumps right behind the jet), 1.3x by 2.2 s
                    // r11: and it keeps swelling, 1.45x by 1.7 s (the clip's 0.25-0.30 m at 10 frames, 0.33-0.41 at 30-40)
                    float swell = 0.70 + 0.30 * smoothstep(0.0, 0.5, ageC) + lateSwell * smoothstep(0.5, 1.7, ageC);
                    float thin = smoothstep(life - 0.35, life, ageC);
                    if (thin >= 1.0) continue;
                    float squash = 1.0 - 0.72 * thin;                   // it deflates across, not along
                    // its ink comes in as it leaves the flame (the clip's young smoke has none) and goes as it thins
                    float inkP = smoothstep(0.30, 0.55, ageC) * (1.0 - thin);
                    // r09: the young smoke glows (by its age; gone with the flame when the rocket ends); r10: to 0.75 s
                    float gC = (1.0 - smoothstep(0.15, glowEnd, ageC)) * (1.0 - dead);
                    // r14 (the owner on r13: more motion blur and soft shapes, with the hard ones): a young lump is
                    // smeared along the path, up to 2.4x as long as across with a soft edge, the round hard lump by 0.3 s
                    // (r15: the clip's old lumps are round: sr/age/softness.py)
                    float mbC = 1.0 - smoothstep(0.06, 0.30, ageC);
                    float stretch = 1.0 + 1.40 * mbC;
                    float3 P0 = Spline(A, B, C, D, uc);
                    float3 tgc = normalize(SplineD(A, B, C, D, uc) + 1e-6);
                    float3 off = normalize(cross(tgc, float3(0.0, 1.0, 0.0)) + 1e-5);
                    float3 upP = normalize(cross(off, tgc) + 1e-5);      // up across the path (a level path: world up)
                    upP *= dot(upP, float3(0.0, 1.0, 0.0)) >= 0.0 ? 1.0 : -1.0;
                    float3 tgS = tgc - dir * dot(dir, tgc);
                    tgS *= rsqrt(max(dot(tgS, tgS), 1e-8));
                    // r21: 0.75x (the clip's trail 0.13-0.18 m at half its contrast, r20's 0.20-0.22: sr/age/fwhm.py)
                    // r22: round beads of nearly one size (the clip's bead chain)
                    float rCore = (0.090 + 0.034 * h3) * swell * squash;     // r23: 1.12x (the clip's 0.16-0.18 m)
                    // the bigger it is the higher it sits (the bottoms line up, and a dying puff sinks onto them); it
                    // rises a little as it ages
                    float3 Pc = P0 + off * (h2 - 0.5) * 0.06
                        + float3(0.0, (h1 - 0.5) * 0.06 + 0.4 * (rCore - 0.10) + 0.05 * smoothstep(0.3, 1.6, ageC), 0.0);
                    [unroll] for (int k = 0; k < 2; k++)
                    {
                        // k 0 the core puff; k 1 a small bump on its top or bottom edge (the clip's scalloped outline)
                        // r10: a bump on 55% of the cells only, a little bigger (the clip's mid-life lumps are fewer)
                        float rb = k == 0 ? rCore : (0.038 + 0.033 * h4) * swell * squash * step(0.85, frac(h4 * 7.13 + h5 * 3.7));   // r21: 30% of cells; r22 15%
                        if (rb < 0.008) continue;
                        float3 P = Pc;
                        if (k == 1) P += upP * ((h5 < 0.55 ? 1.0 : -0.8) * (0.62 + 0.25 * h4) * rCore) + tgc * (h5 - 0.5) * 0.10;
                        float3 Y = P - O;
                        float dz = dot(dir, Y);
                        float3 perp = Y - dir * dz;
                        // deflating: across it shrinks to 0.28, along it keeps its length (a sliver, not an ellipse)
                        perp += tgS * (dot(perp, tgS) * (squash - 1.0));
                        perp += tgS * (dot(perp, tgS) * (1.0 / stretch - 1.0));
                        // a little rounder above than below
                        perp += vU * (max(dot(perp, vU), 0.0) * 0.18);
                        // a ragged outline as it thins
                        float rag = 1.0 + (0.08 + 0.30 * thin) * (CelValue2(float2(dot(perp, vR), dot(perp, vU)) / max(rb, 1e-3) * 1.7 + id * 3.7 + k * 5.3) - 0.5);
                        float R0 = rb * rag;
                        float dl = length(perp);
                        if (R0 - dl > inUnion) { inUnion = R0 - dl; uInk = own ? inkP : 0.0; uG = gC; }
                        if (!own) continue;
                        // r19: a soft edge, SR_SOFT of the radius (the young softer); no hard ink band
                        cover = max(cover, saturate((R0 - dl) / max(mpp * (1.0 + 2.5 * gC) + R0 * (0.30 * mbC + SR_SOFT), 1e-5) + 0.5) * (1.0 - 0.55 * thin * thin));
                        float front = dz - R0 * sqrt(saturate(1.0 - (dl / R0) * (dl / R0)));
                        // the pixel's place in the puff, as seen: +x right, +y up the screen (unit at its outline)
                        float2 nP = float2(dot(-perp, vR), dot(-perp, vU)) / max(R0, 1e-4);
                        if (dl < R0 && front < secDepth)
                        {
                            if (front < bestDepth)
                            {
                                secDepth = bestDepth; secD = bestD; secR = bestR; secInk = bestInk; secG = bestG; secAge = bestAge; secN = bestN;
                                bestDepth = front;
                                bestD = dl / R0; bestR = R0; bestBack = backC; bestInk = inkP; bestG = gC; bestAge = ageC;
                                bestN = nP;
                            }
                            else
                            {
                                secDepth = front; secD = dl / R0; secR = R0; secInk = inkP; secG = gC; secAge = ageC; secN = nP;
                            }
                        }
                    }
                }
                // the light the flames near this stretch throw on it now: the recipe's six samples of the record's
                // life, 0.42 s apart, three 8-bit values to a float in Tone y and z (r14: for the veil too)
                float lt = clamp(a / 0.42, 0.0, 4.999);
                float lkI = floor(lt);
                float ty = i.tone.y, tz = i.tone.z;
                float q0 = fmod(ty, 256.0), q1 = fmod(floor(ty / 256.0), 256.0), q2 = floor(ty / 65536.0);
                float q3 = fmod(tz, 256.0), q4 = fmod(floor(tz / 256.0), 256.0), q5 = floor(tz / 65536.0);
                float lLo = lkI < 0.5 ? q0 : (lkI < 1.5 ? q1 : (lkI < 2.5 ? q2 : (lkI < 3.5 ? q3 : q4)));
                float lHi = lkI < 0.5 ? q1 : (lkI < 1.5 ? q2 : (lkI < 2.5 ? q3 : (lkI < 3.5 ? q4 : q5)));
                float W = lerp(lLo, lHi, smoothstep(0.0, 1.0, lt - lkI)) / 255.0;
                if (cover > 0.0)
                {
                    // where this pixel sits across the path, as seen, over the trail's half-width (+y up the screen)
                    float3 rayAt = dir * dot(dir, Xu) - Xu;
                    float2 tubeN = float2(dot(rayAt, vR), dot(rayAt, vU)) / 0.17;
                    tubeN *= rsqrt(max(1.0, dot(tubeN, tubeN)));
                    float upT = tubeN.y * 0.94 - tubeN.x * 0.22;
                    if (bestD < 1.0)
                    {
                        half3 fOr = CelPal(rowF, 2);
                        col = SmokeShade(bestN, tubeN, bestR, bestInk, bestG, bestAge, W, mpp, sd, inUnion,
                            cBase, wBase, cDisc, wDisc, cBelly, wBelly, sHot, fOr, sInk, wInk);
                        // r19: the crease between two lumps soft: over the front one's outer 40% its colour goes to the
                        // one behind's
                        if (secD < 1.0)
                        {
                            half3 col2 = SmokeShade(secN, tubeN, secR, secInk, secG, secAge, W, mpp, sd, inUnion,
                                cBase, wBase, cDisc, wDisc, cBelly, wBelly, sHot, fOr, sInk, wInk);
                            col = lerp(col, col2, smoothstep(0.60, 1.0, bestD));
                        }
                    }
                    else
                    {
                        // outside every body of its own: the soft outer edge; inside a neighbour stretch's body it is
                        // left to that stretch. r19: the smoke's colour (going to its glow's on the young), darkened
                        // underneath where it is inked (a soft shadow edge below, a soft light edge above)
                        half3 edgeC = lerp(lerp(cBase, wBase, W) * 0.95, lerp(sHot, CelPal(rowF, 2), saturate(uG * 3.0 - 2.0)), uG * 0.6);
                        col = lerp(edgeC, lerp(sInk, wInk, W * saturate(upT * 1.5)), uInk * lerp(0.70, 0.15, smoothstep(-0.5, 0.6, upT)));
                        if (inUnion > 0.0) cover = 0.0;
                    }
                    al = cover;
                }
                // r14 (the owner on r13: more motion blur and soft shapes, with the hard ones we have): a soft veil
                // under the hard rope's line, out to ~4 px past it (the clip's trail has a 2.5 px soft fringe, r13's
                // 0.8-1.3: sr/age/softness.py), drawn by the stretch that owns the nearest puff
                {
                    float vd = max(-inUnion, 0.0);
                    // r15: fainter and wider (the clip's is a light haze 2.5 px wide; r14's 0.30 over 3.5 px reached the
                    // width threshold)
                    // r16: 0.14 over 10 px (the clip's haze +3-4 luma above its lit ground; r15's 0.26 gave +10-12)
                    // r19: 0.18 over 14 px + 0.02 m, round the soft edge
                    float veil = 0.18 * uInk * (1.0 - smoothstep(0.0, 14.0 * mpp + 0.020, vd));
                    half3 vc = lerp(cBase, wBase, W);
                    float aV = saturate(al + veil - al * veil);
                    col = aV > 1e-4 ? (col * al + vc * veil * (1.0 - al)) / aV : col;
                    al = aV;
                }
                // r14: the hot gas's motion blur over the young smoke: a soft streaky glow along the path behind the
                // flame, light orange going cream (the clip's first ~0.5 s of trail glows as one smeared streak)
                {
                    float wa = 0.45 * saturate(wake);
                    half3 wc = lerp(sHot, CelPal(rowF, 2), saturate(wakeAge * 2.0 - 1.0));
                    float aW = saturate(al + wa - al * wa);
                    col = lerp(col, wc, aW > 1e-4 ? wa / aW : 0.0);
                    al = aW;
                }

                // ---- glints over the smoke: short spark dashes that light up along the trail and round it once the
                // flame has gone by, each for three frames, ever fewer as it ages. The clip's (f40-f96): 0.1-0.3 m from
                // the path, half within 15 degrees of it, 6-18 px long, a density that halves every ten frames and is
                // gone by forty. Two chances a cell of path every three frames, each cell on its own phase.
                {
                    uint rowG = CelRowA(i.tone.w);
                    half3 gEdge = CelPal(rowG, 2), gCore = CelPal(rowG, 3);
                    float ga = 0.0;
                    half3 gc = gEdge;
                    [unroll] for (int gn = -2; gn <= 2; gn++)
                    {
                        float gid = ci + gn;
                        float gs = (gid + 0.5) * SR_CELL;
                        if (gs < s1 || gs >= s1 + len || gs < 0.0) continue;
                        float gAge = a - (gs - s1) / len * T;
                        if (gAge < 0.10 || sH + SR_NOZZLE - gs < 0.6) continue;
                        float gOn = 0.34 * exp(-gAge / 0.60) * (1.0 - smoothstep(1.2, 1.7, gAge));
                        [unroll] for (int gsl = 0; gsl < 2; gsl++)
                        {
                            float gk = gid * 2.0 + gsl;
                            float gGen = floor(gAge * 8.0 + CelHash21(float2(gk, 53.0 + sd)));
                            float g1 = CelHash21(float2(gk, gGen * 1.37 + 11.0 + sd));
                            if (g1 > gOn) continue;
                            float g2 = CelHash21(float2(gk + 5.0, gGen * 2.11 + 23.0 + sd));
                            float g3 = CelHash21(float2(gk + 9.0, gGen * 3.07 + 31.0 + sd));
                            float g4 = CelHash21(float2(gk + 13.0, gGen * 1.73 + 43.0 + sd));
                            float gu = (gs + (g2 - 0.5) * SR_CELL - s1) / len;
                            float3 gP = Spline(A, B, C, D, gu);
                            float3 gT = SplineD(A, B, C, D, gu);
                            float3 toG = normalize(gP - O);
                            float3 gS = gT - toG * dot(gT, toG);
                            float gSl = length(gS);
                            if (gSl < 1e-4) continue;
                            gS /= gSl;
                            float3 gN = cross(toG, gS);
                            // r22: along the path, 0.10-0.42 m off it (the clip's rings of streaks round its loops)
                            float gOff = (frac(g3 * 7.31) < 0.5 ? -1.0 : 1.0) * (0.10 + 0.32 * pow(g3, 1.1));
                            float gTilt = (g4 - 0.5) * 0.20;
                            float3 gX = gS * cos(gTilt) + gN * sin(gTilt);
                            float gL = frac(g4 * 5.17);
                            float gHl = (0.035 + 0.065 * gL * gL) * 1.35;   // r14: drawn out (r15: 1.35x; the clip's p90 12-19 px)
                            float3 gY = gP + gN * gOff - O;
                            float gZ = dot(dir, gY);
                            float3 gYp = gY - dir * gZ;
                            float3 gXp = gX - dir * dot(dir, gX);
                            float gSt = clamp(-dot(gYp, gXp) / max(dot(gXp, gXp), 1e-8), -gHl, gHl);
                            float gD = length(gYp + gXp * gSt);
                            float gM = pix * max(gZ, 0.1);
                            // r14: a streak as wide along most of it, fading at both ends (motion blur), whiter
                            float gW = max(0.006, 0.50 * gM) * pow(saturate(1.0 - (gSt / gHl) * (gSt / gHl)), 0.35);
                            float gCov = saturate((gW - gD) / max(gM, 1e-5) + 0.5) * step(1e-5, gW) * (1.0 - smoothstep(0.55, 1.0, abs(gSt) / gHl));
                            if (gCov > ga) { ga = gCov; gc = lerp(lerp(gEdge, gCore, saturate(frac(g2 * 3.71) * 1.4 + 0.2)), half3(1.0, 0.96, 0.88), 0.45); }   // r22: whiter
                        }
                    }
                    float aG = saturate(al + ga - al * ga);
                    col = lerp(col, gc, aG > 1e-4 ? ga / aG : 0.0);
                    al = aG;
                }

                // ---- the flame over it (worked out on every pixel: no derivative inside a branch)
                // r14: the flame's motion blur, a quarter of the path it flies in a frame each way
                float mbF = clamp(0.25 * len / max(T, 1e-3) / 24.0, 0.0, 0.12);
                float hf = FlameHalf(back, fl, mbF) * (1.0 - dead);
                float flameOn = step(-0.01, back) * step(back, fl) * step(0.002, hf) * capEnd;
                // r21: ragged (the clip's flame tongue): its width wobbles at two scales
                float wob = 1.0 + (0.28 * (CelValue2(float2(sU * 3.1 - a * 9.0, sd)) - 0.5) + 0.22 * (CelValue2(float2(sU * 9.0 - a * 21.0, sd + 3.0)) - 0.5)) * smoothstep(0.05, 0.4, back);
                float hw = max(hf * wob, 1e-4);
                float dd = across / hw;
                // a soft glowing edge (the clip's flame is painterly at its rim, not a hard cel cut)
                float edge = max(mpp / hw * 0.8, 0.28);                // r21: a solid tongue (r19 0.50)
                float inF = (1.0 - Step(0.92, dd, edge)) * flameOn;
                // a soft orange halo round the jet (the clip's flame glows into the dark), under the jet
                // r14: wider, and further back along it (0.55-1.75 x its half-width, 0.34)
                // r16: out to 3.2 (the clip's flame glow 0.67-0.89 m across 2-4 frames behind, r15's 0.47-0.50)
                // r21: out to 1.9x, faded before the strip's edge (r19's 3.2x was cut square by it)
                float halo = (1.0 - smoothstep(0.45, 1.90, dd)) * (1.0 - smoothstep(0.36, 0.50, across)) * step(-0.05, back) * step(back, fl) * capEnd * (1.0 - dead)
                    * sqrt(1.0 - saturate(back / max(fl, 0.05))) * step(0.002, hf);
                float ha = 0.32 * halo;
                half3 hcol = lerp(CelPal(rowF, 1), CelPal(rowF, 3), saturate(1.6 - dd));
                float aH = saturate(al + ha - al * ha);
                col = lerp(col, hcol, aH > 1e-4 ? ha / aH : 0.0);
                al = aH;
                // the bulb burns white, the jet pale yellow going orange toward its end
                float bulbIn = saturate(1.0 - max(abs(back - SR_NOZZLE) - mbF, 0.0) / 0.21);
                float hJet = 0.95 - 0.55 * pow(saturate(back / max(fl, 0.05)), 1.3);   // r27: hot for longer
                float heat = (1.0 - min(dd * dd, 1.0)) * max(hJet, bulbIn);
                float eh = max(fwidth(heat) * 0.8, 0.06);                // r19: its heat bands blended (a pixel)
                half3 fc = CelPal(rowF, 0);
                fc = lerp(fc, CelPal(rowF, 1), Step(0.10, heat, eh));
                fc = lerp(fc, CelPal(rowF, 2), Step(0.24, heat, eh));
                fc = lerp(fc, CelPal(rowF, 3), Step(0.42, heat, eh));
                fc = lerp(fc, CelPalCore(rowF).rgb, Step(0.60, heat, eh));
                float fa = inF * lerp(1.0, 0.80, saturate(back / max(fl, 0.05)));
                float aOut = saturate(al + fa - al * fa);
                col = lerp(col, fc, aOut > 1e-4 ? fa / aOut : 0.0);
                al = aOut;
                return half4(col, al);
            }

            // r14: the body's outline with its fins at x body lengths from the nozzle (the motion blur samples it)
            float BodyCover(float x, float ay, float pxX, float pxY)
            {
                const float NoseStart = 0.68;                           // r21: the clip's long pointed nose (0.80)
                float wd = x < 0.0 ? 0.55 + 0.45 * saturate(1.0 + x / 0.07)
                    : (x < NoseStart ? 1.0 : sqrt(saturate(1.0 - pow((x - NoseStart) / (1.0 - NoseStart), 2.0))));
                float inLen = step(-0.07, x) * (1.0 - Step(1.0, x, pxX));
                float body = (1.0 - Step(wd, ay, pxY)) * inLen;
                float finW = 1.0 + 1.4 * saturate(1.0 - x / 0.30);       // r21: the clip's big swept fins (1.9, 0.22)
                float fins = (1.0 - Step(finW, ay, pxY)) * step(0.0, x) * (1.0 - Step(0.30, x, pxX));
                return max(body, fins);
            }

            // ---- a rocket's body: a cylinder with a pointed nose, a short tail cone and two swept fins, cel-banded,
            // inked. q.x body lengths from the nozzle forward, q.y radii across; pxX / pxY a pixel in those units.
            half4 Body(Varyings i, float pxX, float pxY)
            {
                float x = i.q.x, y = i.q.y, ay = abs(y);
                uint row = CelRowA(i.tone.x);
                half3 cSh = CelPal(row, 0), cMid = CelPal(row, 1), cLit = CelPal(row, 2), cHi = CelPal(row, 3);
                half3 nose = CelPalCore(row).rgb, ink = CelPalInk(row).rgb;
                const float NoseStart = 0.68;                           // r21: the clip's long pointed nose (0.80)
                // half-width in radii along it: the tail cone (behind the nozzle), the tube, the ogive nose
                float wd = x < 0.0 ? 0.55 + 0.45 * saturate(1.0 + x / 0.07)
                    : (x < NoseStart ? 1.0 : sqrt(saturate(1.0 - pow((x - NoseStart) / (1.0 - NoseStart), 2.0))));
                float inLen = step(-0.07, x) * (1.0 - Step(1.0, x, pxX));
                float body = (1.0 - Step(wd, ay, pxY)) * inLen;
                // two fins swept back from the tail: out to 1.9 radii at the nozzle, back into the tube 0.22 along
                float finW = 1.0 + 1.4 * saturate(1.0 - x / 0.30);       // r21: the clip's big swept fins (1.9, 0.22)
                float fins = (1.0 - Step(finW, ay, pxY)) * step(0.0, x) * (1.0 - Step(0.30, x, pxX));
                // r14: motion blur along it (Shape.z: a quarter of a frame's path each way, in body lengths): its outline
                // averaged over three places along the path; the smear outside it is the body's middle colour
                float mbB = min(i.shape.z, 0.08);
                float m = 0.5 * max(body, fins) + 0.25 * (BodyCover(x - mbB, ay, pxX, pxY) + BodyCover(x + mbB, ay, pxX, pxY));
                float inkY = 2.0 * pxY, inkX = 2.0 * pxX;
                float innerB = (1.0 - Step(wd - inkY, ay, pxY)) * step(-0.07 + inkX, x) * (1.0 - Step(1.0 - inkX * 2.0, x, pxX));
                float innerF = (1.0 - Step(finW - inkY, ay, pxY)) * step(inkX, x) * (1.0 - Step(0.22 - inkX, x, pxX)) * step(wd + inkY, ay);
                // bands across, lit from BELOW by the fire (the clip, r04): c0 the warm underside, c1 the warm
                // middle, c2 the greyish mauve top, c3 its dark top edge
                float yy = y / max(wd, 0.05);
                half3 c = cMid;
                c = lerp(c, cSh, Step(0.25, -yy, 0.08));
                c = lerp(c, cLit, Step(0.05, yy, 0.08));
                c = lerp(c, cHi, Step(0.62, yy, 0.08));
                // the nose: its top dark mauve, its underside lit like the body; a dark ring at its base; the tail
                // cone dark
                float isNose = Step(NoseStart + 0.02, x, pxX);
                c = lerp(c, nose, isNose * Step(-0.20, yy, 0.08));
                c = lerp(c, ink, (1.0 - Step(1.2 * pxX, abs(x - NoseStart), 0.5 * pxX)) * 0.85);
                c = lerp(c, cHi, (1.0 - step(0.0, x)) * 0.8);
                half3 finC = lerp(cSh, cLit, step(0.0, y) * 0.8);
                c = lerp(c, finC, fins * (1.0 - body));
                c = lerp(ink, c, max(innerB, innerF));
                c = lerp(c, cMid, step(max(body, fins), 0.01));
                return half4(c, m);
            }

            half4 frag(Varyings i) : SV_Target
            {
                float mode = i.state.x, a = i.state.y, k = i.state.z, seed = i.state.w;
                float2 q = i.q;
                float3 spot = 0.0;
                if (mode > 6.5) spot = SceneSpot(i);
                float3 facing = cross(ddy(spot), ddx(spot));
                float2 fw = fwidth(q);
                float pix = max(max(fw.x, fw.y) * 0.75, 1e-5);
                float pixY = fw.y * 0.75;
                uint row = CelRowA(i.tone.x);
                half3 rim = CelPal(row, 0), shade = CelPal(row, 1), body = CelPal(row, 2), light = CelPal(row, 3);
                half3 pale = CelPalCore(row).rgb;
                half3 ink = CelPalInk(row).rgb;
                half3 col = body;
                float al = 0.0;

                if (mode < 0.5)
                {
                    // a pixel's angular size (radians), for the AA widths in metres
                    // (P[1][1] is negative when Unity renders into a texture: the film. r01 had ink on every puff)
                    float pixAng = 2.0 / (_ScaledScreenParams.y * abs(UNITY_MATRIX_P[1][1]));
                    half4 t = Trail(i, a, seed, pixAng);
                    col = t.rgb; al = t.a;
                }
                else if (mode < 1.5)
                {
                    float core = 0.0;
                    if (i.shape.y > 2.5)
                    {
                        al = (1.0 - Step(0.55, abs(q.y), pixY)) * (1.0 - Step(0.97, abs(q.x), fw.x));   // a stick
                        core = 0.0;
                    }
                    else if (i.shape.y > 1.5)
                    {
                        float wd = pow(saturate(1.0 - q.x * q.x), 0.75);    // a leaf: a point at both ends
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        core = (1.0 - Step(wd * 0.42, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    else if (i.shape.y > 0.5)
                    {
                        al = 1.0 - Step(0.9, length(q), pix);
                        core = 1.0 - Step(0.52, length(q), pix);
                    }
                    else
                    {
                        float aspect = max(i.shape.x, 1.0);
                        float xm = q.x * aspect, hc = aspect - 1.0;
                        float wd = xm >= hc ? sqrt(saturate(1.0 - (xm - hc) * (xm - hc)))
                            : pow(saturate((xm + aspect) / max(2.0 * aspect - 1.0, 1e-3)), 0.8);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        core = (1.0 - Step(wd * 0.42, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    col = lerp(body, light, core);
                    al *= 1.0 - smoothstep(0.85, 1.0, k);
                }
                else if (mode < 2.5)
                {
                    float tk = i.shape.x;
                    float starA = Star(q, seed + tk * 0.137, 0.30, 0.92, 0.050, pix);
                    float heartGlow = 0.70 * (1.0 - smoothstep(0.12, 0.80, length(q)));
                    al = max(starA, heartGlow);
                    col = lerp(lerp(rim, light, saturate(1.3 - length(q) * 1.4)), pale, starA);
                }
                else if (mode < 3.5)
                {
                    // ---- the cloud (CelFireball_URP's), on the fireball clip's frame clock reached through this one's
                    float tk = i.shape.x, aCl = i.shape.y, aspect = i.shape.z;
                    float N = clamp(floor(i.shape.w + 0.5), 1.0, 28.0);
                    float F = Remap(96.0 + aCl * 24.0);
                    float S = (1.0 - 0.55 * exp(-(F - 28.0) / 3.6)) * (1.0 + 0.06 * saturate((F - 44.0) / 44.0));
                    float brk = saturate((F - 80.0) / 24.0);
                    float smoky = saturate((F - 62.0) / 12.0);
                    float bestKey = -1e5, bd = 2.0, br = 1.0, bid = 0.0, bcy = 0.0, inside = 0.0, puffDepth = -1e5, puffUnder = 0.0;
                    float outD = -1e5;
                    // r29: the hot spots, the mass's own outline, lobes soft while white-hot
                    float hot = -1e5, massD = -1e5;
                    float softC = lerp(0.35, SR_CLOUD_SOFT, saturate((F - 34.0) / 10.0));   // r30: 0.35 (0.55)
                    float2 brel = 0.0;
                    [loop] for (int j = 0; j < 28; j++)
                    {
                        float fj = j;
                        if (fj >= N) break;
                        float h1 = CelHash21(float2(fj + 1.0, seed * 91.0 + 3.0)), h2 = CelHash21(float2(fj + 17.0, seed * 53.0 + 5.0));
                        float h3 = CelHash21(float2(fj + 41.0, seed * 29.0 + 7.0));
                        float fr = (fj + 0.5) / N, tt, y0, hw;
                        // r29: a column of fewer, bigger, looser lobes (the clip's; r28 a pyramid of small ones)
                        // r31: narrower and taller (the clip's column)
                        if (fr < 0.30) { tt = fr / 0.30; y0 = -0.46; hw = 0.46; }
                        else if (fr < 0.60) { tt = (fr - 0.30) / 0.30; y0 = -0.08; hw = 0.66; }
                        else if (fr < 0.84) { tt = (fr - 0.60) / 0.24; y0 = 0.30; hw = 0.66; }
                        else { tt = (fr - 0.84) / 0.16; y0 = 0.70; hw = 0.44; }
                        float2 c = float2((tt * 2.0 - 1.0) * hw + (h2 - 0.5) * 0.22, (y0 + (h3 - 0.5) * 0.30) * aspect);
                        float r0 = 0.29 + 0.12 * h1;
                        float born = 28.0 + length(c - float2(0.0, -0.1)) * 4.0 + h2 * 1.5;
                        float pop = saturate((F - born) / 2.0);
                        pop = pop * (2.0 - pop);
                        float2 jit = float2(CelHash21(float2(fj, tk * 1.7 + 2.0)), CelHash21(float2(fj + 9.0, tk * 2.3 + 5.0))) - 0.5;
                        // the smoke rises as a column, the upper lobes first, and thins
                        c = c * S * (1.0 + 0.04 * brk * (0.4 + h3)) + jit * 0.022 + float2(0.0, (0.05 + 0.12 * (0.5 + c.y)) * brk * (0.5 + 0.5 * h2));
                        float rFull = r0 * S * lerp(0.55, 1.0, pop) * step(0.001, pop) * (1.0 + 0.04 * sin(tk * 1.3 + fj * 2.1));
                        rFull *= 1.0 - 0.20 * brk * (0.5 - c.y * 0.5);
                        float rMain = rFull * (1.0 - saturate((F - (96.0 + h1 * 10.0)) / 10.0));   // r31: stay (83-90)
                        rMain *= 1.0 - 0.25 * saturate((F - 70.0) / 14.0);   // r32: so the smoke's bumps show
                        // r29: the lobe's white-hot spot (up a little), shrinking as it cools: the whole lobe while white
                        // r34: on the lobe's own clock, the middle's ahead (its spots go out with its char)
                        float outerJ = saturate(length(c * float2(1.0, 1.0 / max(aspect, 0.1))) / 0.75);
                        float Fj = F - (h1 - 0.5) * 5.0 - 10.0 * (outerJ - 0.5) * saturate((F - 44.0) / 6.0);
                        // r30: bigger, uneven, and a second smaller one (the clip's 2-3 to a lobe by f108)
                        // r32: in yellow-orange by f104, bigger later
                        float hs = Fj < 40.0 ? 1.35 : (Fj < 48.0 ? lerp(1.35, 0.85, (Fj - 40.0) / 8.0)
                            : (Fj < 58.0 ? lerp(0.85, 0.60, (Fj - 48.0) / 10.0) : lerp(0.60, 0.0, saturate((Fj - 58.0) / 18.0))));   // r33: gone by 76
                        float sR = rFull * hs * (0.75 + 0.5 * h3);
                        float2 so = c + rFull * float2((h2 - 0.5) * 0.30, 0.12 + 0.16 * h1);
                        float2 so2 = c + rFull * float2((0.5 - h2) * 0.55, -0.22 + 0.12 * h3);
                        float hvj = 1.0 - length(q - so) / max(sR, 1e-4) + 0.55 * (CelValue2((q - so) / max(rFull, 1e-3) * 2.2 + fj * 5.1) - 0.5);
                        float hvj2 = 1.0 - length(q - so2) / max(sR * 0.55, 1e-4) + 0.40 * (CelValue2((q - so2) / max(rFull, 1e-3) * 3.4 - fj * 3.3) - 0.5);
                        if (rMain > 0.004 && sR > 0.002) hot = max(hot, max(hvj, hvj2 * step(46.0, Fj)));
                        float2 rel = q - c;
                        float ang = atan2(rel.y, rel.x);
                        float lump = 1.0 + 0.07 * sin(ang * 3.0 + h1 * 40.0 + tk * 0.4) + 0.05 * sin(ang * 5.0 + h2 * 60.0)
                            + 0.02 * smoky * sin(ang * 9.0 + h3 * 50.0 + tk * 0.3)
                            + 0.10 * (CelValue2(rel / max(r0, 1e-3) * 2.6 + fj * 7.3) - 0.5);
                        float rl = max(rFull * lump, 1e-4);
                        float len = length(rel);
                        float d = len / rl;
                        float rm = rMain * lump;
                        // r20: a soft edge (SR_CLOUD_SOFT of the lobe); caH the hard inside, which alone claims a lobe
                        float ca = saturate((rm - len) / (pix + softC * rMain) + 0.5) * step(0.004, rMain);
                        if (rMain > 0.004) massD = max(massD, rm - len);
                        float caH = step(len, rm) * step(0.004, rMain);
                        if (F > 56.0)
                        [unroll] for (int n = 0; n < 3; n++)
                        {
                            float hk = CelHash21(float2(fj * 3.0 + n + 7.0, seed * 37.0 + 11.0)), hk2 = frac(hk * 7.31), hk3 = frac(hk * 13.7);
                            float ak = h2 * 6.2831853 + n * 2.0943951 + (hk - 0.5) * 0.9;
                            float gone = saturate((F - (82.0 + hk * 19.0)) / (8.0 + 22.0 * hk2 * hk2));
                            float2 cc = float2(cos(ak), sin(ak)) * rFull * (0.42 + 0.22 * brk * hk) + float2(0.0, 0.30 * brk * hk * r0);
                            float rk = rFull * (0.38 + 0.24 * hk3) * (1.0 - gone) * saturate((F - 56.0) / 8.0);
                            float2 rc = rel - cc;
                            float at = ak * 1.7 + hk3 * 6.2831853;
                            float dk = max(rk - length(rc), rk * 0.62 - length(rc - float2(cos(at), sin(at)) * rk * 0.62))
                                - 0.15 * rk * (CelValue2(rc / max(rFull, 1e-3) * 4.2 + hk * 40.0) - 0.5);   // r31: round
                            ca = max(ca, saturate(dk / (pix + SR_CLOUD_SOFT * rk) + 0.5) * step(0.004, rk));
                            caH = max(caH, step(0.0, dk) * step(0.004, rk));
                            if (rk > 0.004) massD = max(massD, dk);
                            if (dk > puffDepth) { puffDepth = dk; puffUnder = length(rc - rk * float2(-0.16, 0.40)) / max(rk, 1e-4); }
                        }
                        al = max(al, ca);
                        inside = max(inside, (1.0 - d) * rl * step(0.004, rFull));
                        if (rFull > 0.004) outD = max(outD, rl - len);
                        float key = c.y * 0.6 + 0.5 * h3;
                        if (caH > 0.0 && key > bestKey) { bestKey = key; bd = d; br = rl; brel = rel; bid = fj; bcy = c.y; }
                    }
                    // r29: ONE mass (the clip's fireball): white-hot lobes merging, then an orange mass with a white-hot
                    // spot in each lobe, the middle charring first and the spots shrinking to glowing holes, then smoke;
                    // no ink between lobes, only a thin rim on the mass's outline (dark brown underneath)
                    float hb = CelHash21(float2(bid + 3.0, seed * 17.0 + 1.0));
                    float e = max(pix / max(br, 1e-4), 0.07);
                    float dOut = 1.0 - bd;
                    uint rowB = CelRowB(i.tone.x);
                    half3 charC = CelPal(rowB, 0), crease = CelPal(rowB, 1), smokeA = CelPal(rowB, 2), smokeB = CelPal(rowB, 3);
                    float2 cb = q - brel;
                    float chT = 52.0 + 11.0 * saturate(length(cb * float2(1.0, 1.0 / max(aspect, 0.1))) / 0.75) + 3.0 * hb;   // r33: 11 (r31 8, r32 15)   // r31: 5 earlier; r32: middle first
                    float charA = smoothstep(chT, chT + 6.0, F);
                    half3 charCol = lerp(charC, smokeA, saturate((F - 72.0) / 12.0));
                    charCol = lerp(charCol, smokeB, saturate((F - 86.0) / 14.0));
                    half3 massC = lerp(light, body, saturate((F - 40.0) / 6.0));
                    massC = lerp(massC, shade, saturate((F - 46.0) / 6.0));
                    massC = lerp(massC, charCol, charA);
                    float eh = max(pix / max(br, 1e-4) * 0.8, 0.04);
                    // r32: a wider orange ring round each spot (the clip's glowing holes)
                    half3 hotC = lerp(lerp(shade, rim, 0.7 * charA), body, Step(0.24, hot, eh));   // r34: red-orange ring in the char
                    hotC = lerp(hotC, light, Step(0.44, hot, eh));
                    hotC = lerp(hotC, pale, Step(0.60 + 0.32 * saturate((F - 58.0) / 10.0), hot, eh));   // r31: orange as they shrink
                    float up = saturate((q.y / max(aspect, 0.1) + 0.6) / 1.3);
                    // r30: a dark orange-brown band inside the outline while orange (the clip f106-f108 (161,83,35))
                    float bandA = 0.75 * saturate((F - 46.0) / 6.0) * (1.0 - charA);
                    massC = lerp(massC, rim, bandA * (1.0 - Step(0.10 + 0.05 * (1.0 - up), massD, 0.035)));
                    // r30: the charred lobes lit on top, creased between (the clip f116: 44-107)
                    float2 nq = brel / max(br, 1e-4);
                    float litT = Step(0.30, nq.y + 0.35 * (CelValue2(nq * 2.4 + bid * 7.7) - 0.5), e * 2.0);
                    massC = lerp(massC, CelPalCore(rowB).rgb, 0.75 * charA * litT * (0.4 + 0.6 * up));
                    col = lerp(massC, hotC, Step(0.0, hot, eh));
                    col = lerp(col, crease, charA * (1.0 - Step(0.16, dOut, e)) * (1.0 - Step(0.0, hot, eh)));   // r33: wider, full
                    // the rim: on the mass's outline only, orange-red on top, dark brown underneath, from f103
                    // r30: wider underneath (the clip's 8-10 px), gone once it is smoke
                    float rimW = (0.030 + 0.070 * (1.0 - up)) * saturate((F - 44.0) / 6.0) * (1.0 - saturate((F - 72.0) / 8.0));
                    half3 rimC = lerp(lerp(ink, rim, up), charC, charA * 0.6);
                    col = lerp(col, rimC, (1.0 - Step(rimW, massD, max(pix, 0.010))) * step(0.001, rimW));
                    // while it is hot it glows into the dark round it (the clip, f98-f105): a soft yellow-orange haze
                    float hotF = (1.0 - saturate((F - 40.0) / 8.0)) * step(29.0, F);
                    float gK = saturate(1.0 + outD / 0.32);
                    float gA = 0.50 * hotF * gK * gK * (1.0 - al);
                    float aNew = al + gA;
                    col = aNew > 1e-4 ? (col * al + lerp(shade, light, gK) * gA) / aNew : col;
                    al = aNew;
                    // the last of the smoke thins as one mass
                    al *= 1.0 - smoothstep(86.0, 112.0, F) * 0.85;   // r32: thins from f122 (the clip's)
                }
                else if (mode < 4.5)
                {
                    half4 b = Body(i, max(fw.x * 0.75, 1e-5), max(fw.y * 0.75, 1e-5));
                    col = b.rgb; al = b.a;
                }
                else
                {
                    float f = GlowOn(i, spot, facing);
                    return half4(Unvignette(i.shape2.rgb * f * GlowSeen(spot), i.positionCS.xy, 0.35), 0.0);
                }
                al *= mode < 0.5 ? 1.0 : i.tone.z;
                col = Unvignette(col, i.positionCS.xy, 1.0);
                return half4(col * al, al);
            }
        ENDHLSL
        Pass
        {
            Name "CelSwirlingRocketsLight"
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
            Name "CelSwirlingRockets"
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
