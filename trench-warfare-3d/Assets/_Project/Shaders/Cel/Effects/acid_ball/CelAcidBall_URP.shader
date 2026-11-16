// Phase: CelFx acid ball (2026-11-15) - one GPU pool for the whole acid ball (reference: acid_ball_cel_00001_): the
// ball and its tail, the drops it sheds, the burst where it lands, the crown of goo that stands up out of it, the ring
// that runs out over the ground, the puddle, its bubbles, the smoke and the glowing cloud, and the light it throws.
// Every shape is a field in the fragment; nothing is a texture (the light, the puddle and the ring read the camera's
// depth). The clip is drawn on ONES (it changes every frame), so the age a record is drawn at is stepped 24 times a
// second on the EFFECT's clock (Extra.w carries the seconds from the effect's start to the record's birth).
// Built from CelNeonAcid_URP (drop, puddle, bubble, puff, glow, the light pass) and CelFireball_URP (the comet strip).
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 comet    the ball and its tail, one strip along the path it flies. Pos where it starts, Vel the launch velocity,
//             Motion.x gravity. Size x the ball's radius (m), y the tail's length (m, as seen), z the strip's
//             half-width in ball radii (room for the glow round it), w the seconds of flight to the hit. Life = the
//             flight + the seconds the tail takes to go after the hit. The ball is a thick ring of bright acid with
//             a ragged outer edge round a dark, cracked inside (veins of a lighter green between dark cells), a soft
//             glow round it; the tail streams back off it in flame ribbons that thin to points.
//  1 drop     a ballistic drop. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at death,
//             w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf (a point at
//             both ends). Two tones: the palette's body, its light in the middle. Extra z: its light share (0 none).
//  2 burst    the hit, on one card facing the eye: a white-hot core in a yellow body, thin white rays, big
//             translucent green blades thrown out of it (the longest up past the top of the frame), and the body
//             turning into a spiky lime splash that flattens on the ground. Pos the hit, Size x the card's half-width
//             (m), y its half-height, z the core's half-width (m), w the blades' reach (m). Extra y metres toward the eye.
//  3 crown    the goo that comes out of the burst: a splash of goo streams thrown out of a low wide foot (thin
//             straight spikes on its first frames, then flame tongues, then thinning strands with drops on their tips,
//             tied into loops by thin strands that tear), the front ones lime lit from the upper left, the back ones a
//             darker green, yellow-lime on its first frames (Tone.x's second palette row); its foot fades into the
//             ground on the scene's depth. Pos its foot. Size x the card's half-width (m), y its height (m), z the
//             reach of its streams (m), w how many streams (up to 96). Motion z metres toward the eye.
//  4 puddle   a splat lying on the ground: painted on whatever level ground the scene shows behind its card (found
//             from the scene's depth). Pos its middle, Axis the eye's level right. Size x half-extent (m), w depth /
//             width. Motion x, y the ground's slope there, z the side its longest fingers run to. Extra y seconds it
//             takes to spread, z the age at which it drains. Erode x the age at which it turns to a bright rim round
//             a dark inside, y the seconds that takes. Erode w 1: the burst's flat splash instead (long streaks out to
//             the two sides in torn sheets, the palette's body and light, turning its shade as it drains from Extra z).
//  5 bubble   Pos on the ground. Size x radius it swells to (m). Extra y seconds of the pop at the end of its life.
//  6 puff     smoke: no line round it, a ragged edge, seen through. Pos, Vel, Motion (gravity, drag, -, buoyancy).
//             Size x, y, z its size over the life (m). Extra y the life fraction at which it starts to thin, z the
//             share of its life it is lit (from 1 up it stays light all its life).
//  7 glow     the acid's light on whatever stands behind it (from the scene's depth). Pos the light, Axis a level
//             axis. Size x its reach along that axis (m), y up and down, w its reach square to it on the near side
//             over x, z its reach on the far side over that. Vel what it multiplies in at its heart (rgb), Erode xyz
//             the colour it adds on top. Extra y seconds it takes to come up, z seconds it takes to go.
//  8 ring     the wave that runs out over the ground from the hit: a band of goo whose radius grows fast and
//             slows, thinning, with ripples inside it, breaking into arcs as it goes. Painted on the level ground
//             behind its card (as the puddle). Pos its middle, Axis the eye's level right. Size x the radius it
//             runs out to (m), y the band's width at its start (m), z its width at the end (m), w the e-folding seconds of its
//             radius (it runs 63 % of the way in that time). Motion x, y the ground slope there, z its depth over its
//             width (the Range's ground falls toward the eye: 0.62 as the puddle).
//  9 coat     the goo on the target: wherever the scene's own surface lies in a box round it, painted over - a dark
//             green coat, a lime band along its outline and on what faces up, lime runs sliding down it - darkening
//             as it ages. Pos the target's foot, Axis the eye's level right. Size x the box's half-width (m), y its
//             top over the foot (m), z its bottom over the foot (m), w its half-depth (m). Extra y seconds it takes
//             to come, z seconds it takes to go.
// Tone: x the palette row, y (a glow: 1 if it falls on level ground only), z opacity (a glow: its strength),
// w (a puff) 0 a cel edge, 1 a soft glowing edge, brighter in its middle while it is young.
// Palette rows: c0 rim, c1 shade, c2 body, c3 light, core pale.
//
// Two passes, one draw: the light pass first (only the glows draw in it: lit = unlit x (1 + what a glow multiplies
// in)), then everything, in the records' order.
//
// _TWAcidLens (global; AcidBallRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0
// none. Every colour is lifted by what the vignette will take from it, as far as white allows.
Shader "TW/Cel/AcidBall"
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
            #define AB_TICKS 24.0

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float2 q : TEXCOORD1;
                float4 flow : TEXCOORD2;
                nointerpolation float4 tone : TEXCOORD3;
                nointerpolation float4 state : TEXCOORD4;
                nointerpolation float4 shape : TEXCOORD5;
                nointerpolation float4 shape2 : TEXCOORD6;
                float fog : TEXCOORD7;
                nointerpolation float4 shape3 : TEXCOORD8;
            };

            bool IsDecal(float mode) { return abs(mode - 4.0) < 0.5 || abs(mode - 8.0) < 0.5; }
            bool IsGlow(float mode) { return abs(mode - 7.0) < 0.5; }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), alive = CelAlive(r);
                float mode = r.extra.x, delay = r.extra.w;
                float tick = floor((t + delay) * AB_TICKS + 0.01);
                float a = max(tick / AB_TICKS - delay, 0.0);
                float k = saturate(a / max(r.life, 1e-4));
                float3 vr = UNITY_MATRIX_V[0].xyz, vu = UNITY_MATRIX_V[1].xyz;
                float2 cq = v.uv * 2.0 - 1.0;
                float3 w = r.pos;
                float2 q = cq;
                float4 flow = 0.0, shape = 0.0, shape2 = 0.0, shape3 = 0.0;
                float wholeFrame = 0.0;
                if (mode < 0.5)
                {
                    // the comet: a strip along the path, from ahead of the ball back to the end of the tail, laid out
                    // in metres AS SEEN so the ball stays round from any side (CelFireball_URP)
                    float T = max(r.size.w, 0.01), R = r.size.x, L = r.size.y;
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float sH = min(a, T);
                    float3 head = r.pos + r.vel * sH + 0.5 * G * sH * sH;
                    float3 tanH = normalize(r.vel + G * sH);
                    float sinT = max(length(cross(tanH, normalize(_WorldSpaceCameraPos - head))), 0.25);
                    float speed = max(length(r.vel), 0.01);
                    float X = lerp(-1.6 * R, L, v.uv.x);
                    float s = max(sH - X / (sinT * speed), -0.30);
                    float3 c = r.pos + r.vel * s + 0.5 * G * s * s;
                    float3 tangent = normalize(r.vel + G * s);
                    float3 side = normalize(cross(tangent, normalize(_WorldSpaceCameraPos - c)));
                    side *= side.y >= 0.0 ? 1.0 : -1.0;
                    float halfW = R * max(r.size.z, 1.0);
                    w = c + side * cq.y * halfW;
                    q = float2(X, cq.y * halfW);
                    flow = float4(s * speed * sinT, 0.0, 0.0, 0.0);
                    shape = float4(R, L, tick, max(a - T, 0.0));
                    shape2 = float4(a, T, 0.0, 0.0);
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
                    shape = float4(len / max(rad, 1e-4), r.extra.y, r.extra.z, 0.0);
                }
                else if (mode < 2.5)
                {
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.1);
                    float pull = min(r.extra.y, dist * 0.8);
                    float keep = (dist - pull) / dist;
                    float3 p = r.pos + toEye / dist * pull;
                    w = p + (vr * cq.x * r.size.x + vu * cq.y * r.size.y) * keep;
                    q = cq * r.size.xy;
                    shape = float4(tick, a, r.size.z, r.size.w);
                }
                else if (mode < 3.5)
                {
                    float yy = lerp(-0.7, r.size.y, v.uv.y);
                    w = r.pos + vr * cq.x * r.size.x + float3(0.0, 1.0, 0.0) * yy;
                    w += normalize(_WorldSpaceCameraPos - w) * r.motion.z;
                    q = float2(cq.x * r.size.x, yy);
                    float rise = saturate(a / max(r.motion.x, 0.01));
                    float sink = saturate((a - (r.life - r.motion.y)) / max(r.motion.y, 0.01));
                    shape = float4(tick, rise, sink, r.size.z);
                    shape2 = float4(r.size.w, r.size.y, a, r.life);
                }
                else if (mode < 4.5 || (mode > 7.5 && mode < 8.5))
                {
                    // a card that hides the ball the decal lies in: the fragment finds the ground behind it
                    float R = r.size.x * 1.15;
                    float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                    w = CelBillboard(r.pos, cq * R * cover);
                    wholeFrame = step(dist, R * 1.02) * alive;
                    float3 right = normalize(float3(r.axis.x, 0.0, r.axis.z));
                    if (mode < 4.5)
                    {
                        float grow = 0.35 + 0.65 * (1.0 - exp(-a / max(r.extra.y, 0.01) * 3.4));
                        grow *= 1.0 - 0.15 * smoothstep(0.55, 1.0, a / max(r.extra.z, 0.01));
                        float drain = saturate((a - r.extra.z) / max(r.life - r.extra.z, 0.01));
                        float late = saturate((a - r.erode.x) / max(r.erode.y, 0.01));
                        flow = float4(right.x, right.z, max(r.size.w, 0.01), max(r.size.x, 0.01));
                        shape = float4(tick, grow, drain, r.motion.z);
                        shape2 = float4(r.motion.xy, late, 0.0);
                    }
                    else
                    {
                        float run = 1.0 - exp(-a / max(r.size.w, 0.01));
                        flow = float4(right.x, right.z, r.motion.z > 0.01 ? r.motion.z : 1.0, max(r.size.x, 0.01));
                        shape = float4(tick, run, k, 0.0);
                        shape2 = float4(r.motion.xy, r.size.y, r.size.z);
                    }
                    shape3 = float4(r.pos, r.erode.w);
                }
                else if (mode < 5.5)
                {
                    float popAt = r.life - r.extra.y;
                    float swell = saturate(a / max(popAt * 0.35, 0.01));
                    float rad = r.size.x * (0.18 + 0.82 * pow(swell, 0.7));
                    float pop = saturate((a - popAt) / max(r.extra.y, 0.01));
                    float card = rad * (1.4 + 0.9 * step(0.0001, pop));
                    w = CelBillboard(r.pos + float3(0.0, rad * 0.92, 0.0), cq * card);
                    q = cq * (card / max(rad, 1e-4));
                    shape = float4(tick, pop, swell, 0.0);
                }
                else if (mode < 6.5)
                {
                    float3 p = r.pos + r.vel * CelDragInt(r.motion.y, a) + float3(0.0, 0.5 * (r.motion.w - r.motion.x) * a * a, 0.0);
                    float size = CelCurve(r.size.xyz, k);
                    w = CelBillboard(p, cq * size);
                    float fray = saturate((k - r.extra.y) / max(1.0 - r.extra.y, 0.01));
                    shape = float4(tick, fray, max(r.extra.z, 0.06), 0.0);
                }
                else if (mode > 8.5)
                {
                    // the coat: a card over the target's box; the fragment finds the target's surface in the depth
                    float R = max(r.size.x, r.size.y * 0.6) * 1.3;
                    float3 mid = r.pos + float3(0.0, r.size.y * 0.5, 0.0);
                    float dist = max(distance(_WorldSpaceCameraPos, mid), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                    w = CelBillboard(mid, cq * R * cover);
                    float3 right = normalize(float3(r.axis.x, 0.0, r.axis.z));
                    float up = saturate(a / max(r.extra.y, 0.01)), down = saturate((r.life - a) / max(r.extra.z, 0.01));
                    flow = float4(right.x, right.z, 0.0, 0.0);
                    shape = float4(tick, up * down, a, k);
                    shape2 = r.size;
                    shape3 = float4(r.pos, 0.0);
                }
                else
                {
                    float reachZ = r.size.x * max(r.size.w, 0.01);
                    float R = max(max(r.size.x, r.size.y), reachZ);
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
                if (IsGlow(mode) || IsDecal(mode) || mode > 8.5)
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
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            float Step(float edge, float v, float w) { return smoothstep(edge - w, edge + w, v); }

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

            // cell noise: the distance to the nearest and the second nearest of a jittered point per cell
            float2 Cells(float2 p, float seed)
            {
                float2 c = floor(p), f = frac(p);
                float d1 = 8.0, d2 = 8.0;
                [unroll] for (int y = -1; y <= 1; y++)
                [unroll] for (int x = -1; x <= 1; x++)
                {
                    float2 o = float2(x, y);
                    float2 h = float2(CelHash21(c + o + seed * 7.0), CelHash21(c + o + seed * 13.0 + 5.0));
                    float d = length(o + h * 0.85 + 0.075 - f);
                    if (d < d1) { d2 = d1; d1 = d; } else if (d < d2) d2 = d;
                }
                return float2(d1, d2);
            }

            // a splat: a ragged lump and up to 24 spikes that run out of it to a point (most short, a few long),
            // some throwing a bead off their tip; `flat` squashes it toward the ground below its middle
            float Star(float2 q, float seed, float heart, float reach, float thin, float w, float flat)
            {
                float2 p = q;
                p.y *= p.y < 0.0 ? flat : 1.0;
                float ang = atan2(p.y, p.x);
                float lump = 0.80 + 0.20 * sin(ang * 2.0 + seed * 33.0) + 0.16 * sin(ang * 3.0 + seed * 40.0) + 0.10 * sin(ang * 5.0 + seed * 57.0) + 0.07 * sin(ang * 9.0 + seed * 91.0);
                float m = 1.0 - Step(heart * lump, length(p * float2(0.85, 1.15)), w);
                [unroll] for (int j = 0; j < 24; j++)
                {
                    float h = CelHash21(float2(j + 1.0, seed * 77.0 + 3.0));
                    float h2 = CelHash21(float2(j + 31.0, seed * 131.0 + 5.0));
                    float an = (j + 0.25 + 0.5 * h) * 0.2617994;
                    float2 d = float2(cos(an), sin(an));
                    float L = lerp(heart * 0.9, reach, h2 * h2);
                    float al = dot(p, d), ac = abs(p.x * d.y - p.y * d.x);
                    float wd = thin * pow(saturate(1.0 - al / max(L, 1e-3)), 0.8);
                    float spike = (1.0 - Step(wd, ac, w)) * step(0.0, al) * (1.0 - Step(L, al, w)) * step(0.25, h);
                    float2 b = p - d * L * (1.10 + 0.25 * h);
                    float bead = (1.0 - Step(0.018 + 0.018 * h, length(b), w)) * step(0.55, h2);
                    m = max(m, max(spike, bead));
                }
                return m;
            }

            float Puff(float2 q, float seed, float tick)
            {
                q += 0.07 * (float2(CelValue2(q * 2.3 + seed * 17.0), CelValue2(q * 2.3 + seed * 29.0 + 7.0)) - 0.5);
                q += 0.07 * (float2(CelValue2(q * 6.1 + seed * 11.0 + tick * 0.065), CelValue2(q * 6.1 + seed * 23.0 + 3.0)) - 0.5);
                q += 0.035 * (float2(CelValue2(q * 13.0 + seed * 7.0), CelValue2(q * 13.0 + seed * 31.0 + 5.0 - tick * 0.085)) - 0.5);
                float f = 0.40 - length(q);
                [unroll] for (int j = 0; j < 5; j++)
                {
                    float h = CelHash21(float2(j + 1.0, seed * 63.0 + 1.0)), h2 = CelHash21(float2(j + 17.0, seed * 29.0 + 2.0));
                    float an = CelHash21(float2(j + 29.0, seed * 41.0 + 3.0)) * 6.2831853 + j * 0.9;
                    float2 c = float2(cos(an), sin(an)) * (0.16 + 0.30 * h);
                    c += 0.02 * float2(sin(tick * 0.65 + j), cos(tick * 0.85 + j * 2.0));
                    f = max(f, (0.20 + 0.22 * h2) - length(q - c));
                }
                [unroll] for (int n = 0; n < 7; n++)
                {
                    float h = CelHash21(float2(n + 41.0, seed * 57.0 + 5.0)), h2 = CelHash21(float2(n + 53.0, seed * 33.0 + 6.0));
                    float an = (n + h) * 0.8975979 + seed * 20.0;
                    float2 c = float2(cos(an), sin(an)) * (0.44 + 0.20 * h2);
                    c += 0.025 * float2(sin(tick * 0.95 + n * 1.3), cos(tick * 0.75 + n));
                    f = max(f, (0.07 + 0.08 * h) - length(q - c));
                }
                return f;
            }

            float Streaks(float2 p, float seed, float share, float thick, float w)
            {
                p.y += CelHash21(float2(floor(p.x), seed * 19.0 + 4.0));
                float2 cell = floor(p), f = frac(p);
                float h = CelHash21(cell + seed * 3.0), h2 = CelHash21(cell + seed * 5.0 + 31.0);
                float lens = saturate(1.0 - abs(f.y * 2.0 - 1.0));
                float at = 0.3 + 0.4 * h2;
                return (1.0 - Step(thick * sqrt(lens), abs(f.x - at), w)) * step(1.0 - share, h) * step(0.03, lens);
            }

            float Fingers(float turn, float cells, float seed, float tk, float lo, float hi, float share, float w0, float w1, float power, float blunt)
            {
                float af = turn * cells;
                float c0 = floor(af);
                float reach = 0.0;
                [unroll] for (int j = -1; j <= 1; j++)
                {
                    float cid = c0 + j;
                    float wid = cid - cells * floor(cid / cells);
                    float h = CelHash21(float2(wid + 1.0, seed * 71.0 + 4.0)), h2 = CelHash21(float2(wid + 23.0, seed * 37.0 + 6.0));
                    float h3 = CelHash21(float2(wid + 47.0, seed * 19.0 + 8.0));
                    float boil = CelHash21(float2(wid + 5.0, tk * 3.1 + seed)) - 0.5;
                    float da = abs(af - (cid + 0.5 + (h - 0.5) * 0.7)) / lerp(w0, w1, h2);
                    float amp = (lo + (hi - lo) * h3 * h3 + 0.015 * boil) * step(1.0 - share, h);
                    reach = max(reach, amp * pow(saturate(1.0 - lerp(da, da * da, blunt)), power));
                }
                return reach;
            }

            // lay colour c over what is there with coverage m (premultiplied accumulation; divide by pa at the end)
            void Over(inout float3 pc, inout float pa, float3 c, float m)
            {
                pc = pc * (1.0 - m) + c * m;
                pa = pa + m * (1.0 - pa);
            }

            // the field of a capsule from a to b whose radius runs ra to rb (pinched by `neck` in its middle): 1 on its
            // line, 0.42 at its radius, 0 at twice it. Fields that add melt into each other (goo).
            float CapField(float2 p, float2 a, float2 b, float ra, float rb, float neck)
            {
                float2 pa = p - a, ba = b - a;
                float hh = saturate(dot(pa, ba) / max(dot(ba, ba), 1e-8));
                float2 dd = pa - ba * hh;
                float rr = lerp(ra, rb, hh) * (1.0 - neck * sin(3.1416 * hh));
                float w = saturate(1.0 - dot(dd, dd) / max(4.0 * rr * rr, 1e-8));
                return w * w * w;
            }

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

            // how far behind this fragment the scene lies, in metres of eye depth (soft edges where a card meets the
            // ground in front of it)
            float SceneGap(Varyings i)
            {
                float2 uv = i.positionCS.xy / _ScaledScreenParams.xy;
                float2 suv = ClampAndScaleUVForBilinear(UnityStereoTransformScreenSpaceTex(uv), _CameraDepthTexture_TexelSize.xy);
                float raw = SAMPLE_TEXTURE2D_X_LOD(_CameraDepthTexture, sampler_PointClamp, suv, 0).r;
                return LinearEyeDepth(raw, _ZBufferParams) - LinearEyeDepth(i.positionCS.z, _ZBufferParams);
            }

            // the scene's surface under another pixel (the coat looks round itself for the target's outline)
            float3 SpotAt(float2 px)
            {
                float2 uv = px / _ScaledScreenParams.xy;
                float2 suv = ClampAndScaleUVForBilinear(UnityStereoTransformScreenSpaceTex(uv), _CameraDepthTexture_TexelSize.xy);
                float depth = SAMPLE_TEXTURE2D_X_LOD(_CameraDepthTexture, sampler_PointClamp, suv, 0).r;
                #if !UNITY_REVERSED_Z
                depth = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, depth);
                #endif
                return ComputeWorldSpacePosition(uv, depth, UNITY_MATRIX_I_VP);
            }

            // 1 where a scene point lies in the coat's box round the target (Size: x half-width, y top, z bottom over
            // the foot, w half-depth)
            float CoatIn(float3 spot, Varyings i)
            {
                float3 d = spot - i.shape3.xyz;
                float across = d.x * i.flow.x + d.z * i.flow.y, deep = d.z * i.flow.x - d.x * i.flow.y;
                return step(abs(across), i.shape2.x) * step(d.y, i.shape2.y) * step(i.shape2.z, d.y) * step(abs(deep), i.shape2.w);
            }

            float GlowOn(Varyings i, float3 spot, float3 facing)
            {
                float3 d = spot - i.shape3.xyz;
                float across = d.x * i.flow.x + d.z * i.flow.y, back = d.z * i.flow.x - d.x * i.flow.y;
                float t = length(float3(across / i.shape.x, back / (i.shape.y * (back > 0.0 ? i.flow.z : 1.0)), d.y / i.flow.w));
                float level = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                return exp(-3.0 * t * t) * (1.0 - smoothstep(0.8, 1.0, t)) * i.shape.z * i.tone.z * lerp(1.0, level, i.tone.y);
            }

            half3 GlowSeen(float3 spot)
            {
                return CelAdditiveFog(half3(1.0, 1.0, 1.0), spot, ComputeFogFactor(TransformWorldToHClip(spot).z));
            }

            Varyings vertLight(Attributes v, uint instanceID : SV_InstanceID)
            {
                Varyings o = vert(v, instanceID);
                if (!IsGlow(o.state.x)) o.positionCS = float4(2.0, 2.0, 2.0, 1.0);
                return o;
            }

            half4 fragLight(Varyings i) : SV_Target
            {
                float3 spot = SceneSpot(i);
                float f = GlowOn(i, spot, cross(ddy(spot), ddx(spot)));
                return half4(float3(i.shape.w, i.shape2.w, i.shape3.w) * f * GlowSeen(spot), 0.0);
            }

            half4 frag(Varyings i) : SV_Target
            {
                float mode = i.state.x, k = i.state.z, seed = i.state.w;
                float2 q = i.q;
                float3 spot = 0.0;
                bool decal = IsDecal(mode);
                if (decal || IsGlow(mode) || mode > 8.5) spot = SceneSpot(i);
                if (decal)
                {
                    float3 off = spot - i.shape3.xyz;
                    q = float2(off.x * i.flow.x + off.z * i.flow.y, (off.z * i.flow.x - off.x * i.flow.y) / i.flow.z) / i.flow.w;
                }
                float3 facing = cross(ddy(spot), ddx(spot));
                float2 fw = fwidth(q);
                float pix = max(max(fw.x, fw.y) * 0.75, 1e-5);
                float pixX = fw.x * 0.75, pixY = fw.y * 0.75;
                uint row = CelRowA(i.tone.x);
                half3 rim = CelPal(row, 0), shade = CelPal(row, 1), body = CelPal(row, 2), light = CelPal(row, 3);
                half3 pale = CelPalCore(row).rgb;
                half3 col = body;
                float al = 0.0;

                if (mode < 0.5)
                {
                    // ---- the ball and its tail. X metres behind the ball's middle, Y metres across (up the screen)
                    float R = i.shape.x, L = i.shape.y, tk = i.shape.z, after = i.shape.w;
                    float a = i.shape2.x, T = i.shape2.y;
                    float X = q.x, Y = q.y, P = i.flow.x;
                    float u = saturate(X / L);
                    float ay = abs(Y);
                    float sd = seed * 17.0;
                    float up = Y > 0.0 ? 1.0 : 0.0;
                    // the tail (a03): as wide as the ball where it leaves it, thinning to a long point; its edges are
                    // flame wisps streaming back, a light streak along its middle, its far end seen through
                    // a64: the clip's profile - the ball's width at the neck, 0.45 R at 1.5 R behind, then as X^-1.1
                    float h = R * 1.2 * clamp(0.45 * pow(max(X, 0.3 * R) / (1.5 * R), -1.1), 0.14, 1.0) * sqrt(saturate(1.0 - u));
                    float sl = X - a * 9.0 - ay * 0.9;
                    float g1 = CelSimplex3(float3(sl * 0.8, tk * 0.10, sd + lerp(7.7, 3.1, up)));
                    float g2 = CelSimplex3(float3(sl * 2.2 + 4.3, tk * 0.16, sd + lerp(5.3, 1.7, up)));
                    float rows = 0.5 + 0.5 * (g1 * 0.6 + g2 * 0.4);
                    float grow = smoothstep(0.3 * R, 1.5 * R, X);
                    float wisp = CelSimplex3(float3(sl * 2.2, tk * 0.2, sd + 2.2));
                    float edge = h * (0.75 + (rows - 0.45) * 1.7 * grow) * (1.0 + 0.40 * wisp * grow);
                    // a63: flame tongues grown out of its top edge near the ball (the clip's licks are joined to it)
                    edge *= 1.0 + 0.9 * up * pow(saturate(CelSimplex3(float3(sl * 1.6, tk * 0.25, sd + 13.0)) * 1.2 + 0.1), 2.0)
                        * (1.0 - smoothstep(0.0, 0.45, u)) * smoothstep(-0.2 * R, 0.4 * R, X);
                    float fTail = X > 0.0 ? (edge - ay) / max(edge, 1e-3) : -1.0;
                    float lane = Y / max(edge, 1e-3) * 2.2 + 0.55 * CelSimplex3(float3(sl * 0.6, tk * 0.08, sd + 9.0));
                    float slit = 1.0 - abs(frac(lane + 0.5) - 0.5) * 2.0;
                    fTail -= 0.0 * slit;
                    // the ball: a lumpy round outline, no teeth
                    float ang = atan2(Y, X);
                    float2 ca = float2(cos(ang), sin(ang));
                    float flickB = saturate(CelSimplex3(float3(ca * 5.0, tk * 0.15 + sd * 2.0)));
                    float wob = 0.07 * CelSimplex3(float3(ca * 1.3, tk * 0.05 + sd)) + 0.04 * CelSimplex3(float3(ca * 2.6, tk * 0.08 - sd)) + 0.0 * flickB;
                    // flame licks off its upper back, flickering (the clip's f10-22)
                    float lickB = pow(saturate(CelSimplex3(float3(ca * 3.2, tk * 0.31 + sd * 5.0)) * 1.4 + 0.20), 2.0);
                    wob += 0.12 * lickB * smoothstep(0.05, 0.60, ca.y) * smoothstep(-0.40, 0.30, ca.x);
                    float rr = length(float2(X, Y)) / R;
                    float fHead = 1.0 - rr / (1.0 + wob);
                    float f2 = max(fHead, fTail);
                    float solid = saturate(f2 / max(fwidth(f2), 1e-4) + 0.5);
                    float halo = saturate(1.0 - (rr - 1.0) / 0.55) * step(1.0, rr);
                    halo = max(halo * halo * 0.45, saturate(fTail + 0.5) * 0.25 * (1.0 - u));
                    float fade = 1.0 - 0.65 * smoothstep(0.45, 1.0, u);
                    al = max(solid * fade, halo);
                    // small flame licks thrown off just outside the tail's edges
                    float lk = CelSimplex3(float3(sl * 3.5, ay / max(R, 1e-3) * 2.5, tk * 0.22 + sd));
                    float lickGrow = smoothstep(-0.2 * R, 0.6 * R, X);   // a62: from right at the ball, swept back with the tail
                    float lick = step(0.0, X) * step(edge, ay) * step(ay, edge * 1.5) * Step(0.45, lk, 0.04) * lickGrow * (1.0 - u);
                    al = max(al, lick * fade);
                    float heat = saturate(f2 * 1.6) * (1.0 - 0.5 * pow(u, 0.8));
                    float streak = CelValue2(float2((X - a * 11.0) * 0.30 + sd, Y / R * 4.5 + tk * 0.05)) - 0.5;
                    half3 tailC = lerp(shade, body, Step(0.10, heat * 0.7 + 0.80 * streak, 0.03));
                    tailC = lerp(tailC, light, Step(0.34, heat * 0.7 + 0.90 * streak, 0.03));
                    tailC = lerp(tailC, pale, Step(0.58, heat * 0.7 + 1.00 * streak, 0.03));
                    // the ball (a55): a thick bright rim, lit along its lower front, round a dark green foam - round holes
                    // parted by lime webs - that opens into a dark hollow as it comes in (the clip's f10-16, f19-24)
                    float2 bp = float2(X, Y) / R;
                    float inner = step(rr, 0.70 + 0.08 * CelSimplex3(float3(ca * 2.0, tk * 0.09 - sd * 2.0)));
                    float2 wbp = bp + (float2(CelValue2(bp * 2.3 + sd), CelValue2(bp * 2.3 - sd + 4.1)) - 0.5) * 0.35;
                    float2 cellB = Cells(wbp * 3.2 + float2(a * 1.6, 0.0) + sd, seed);
                    float ageB = smoothstep(0.3, 1.0, a / max(T, 1e-3));
                    float holeR = lerp(0.40, 0.50, ageB) + 0.12 * (CelValue2(bp * 4.0 + sd) - 0.5);
                    float foam = 1.0 - Step(holeR, cellB.x, 0.03);
                    float hollow = 1.0 - Step(lerp(0.15, 0.55, ageB), length(bp - float2(0.08, 0.05)), 0.04);
                    float hole = max(foam, hollow) * inner;
                    float holeEdge = (1.0 - Step(holeR + 0.08, cellB.x, 0.03)) * inner * (1.0 - hole);
                    float lower = smoothstep(0.35, 0.95, dot(bp, normalize(float2(-0.55, -0.83))));
                    half3 headC = lerp(lerp(body, light, 0.6), light, saturate(lower * 1.2 + Step(0.82, rr, 0.03) * 0.6));
                    headC = lerp(headC, pale, Step(0.80, rr, 0.03) * lower * 0.8);
                    headC = lerp(headC, lerp(body, light, 0.7), inner * (1.0 - hole));
                    headC = lerp(headC, lerp(shade, body, 0.3), holeEdge);
                    headC = lerp(headC, lerp(lerp(rim, shade, 0.7), lerp(CelPalInk(row).rgb, rim, 0.4), hollow * ageB), hole);   // a64: mid-green holes
                    float onHead = saturate(fHead / max(fwidth(fHead), 1e-4) + 0.5);
                    col = lerp(tailC, headC, onHead);
                    col = lerp(lerp(shade, body, 0.6), col, solid);
                    al *= lerp(1.0, 0.85, onHead * hole);
                }
                else if (mode < 1.5)
                {
                    float core = 0.0;
                    if (i.shape.y > 1.5)
                    {
                        float wd = pow(saturate(1.0 - q.x * q.x), 0.75);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        core = (1.0 - Step(wd * 0.40, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    else if (i.shape.y > 0.5)
                    {
                        al = 1.0 - Step(0.9, length(q), pix);
                        core = 1.0 - Step(0.50, length(q - float2(-0.1, 0.15)), pix);
                    }
                    else
                    {
                        float aspect = max(i.shape.x, 1.0);
                        float xm = q.x * aspect, hc = aspect - 1.0;
                        float wd = xm >= hc ? sqrt(saturate(1.0 - (xm - hc) * (xm - hc)))
                            : pow(saturate((xm + aspect) / max(2.0 * aspect - 1.0, 1e-3)), 0.8);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        core = (1.0 - Step(wd * 0.40, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    col = lerp(body, light, core * saturate(i.shape.z));
                    col = lerp(col, pale, core * saturate(i.shape.z - 1.0));
                    al *= 1.0 - smoothstep(0.85, 1.0, k);
                }
                else if (mode < 2.5)
                {
                    // ---- the burst (a54), after the clip's frames 26-31: a soft green glow; broad soft green rays fanned
                    // up and out to both sides (lime with a darker half, some all dark green), tapering to points; two
                    // long blades up; a soft ragged yellow halo; a white core of flame tongues with a cream edge.
                    // q in metres on the card (as at the hit's depth), the hit at its middle. Fitted offline (ab/burst.py).
                    float tk = i.shape.x, a = i.shape.y, B = i.shape.w;
                    float2 m = q;
                    float out0 = smoothstep(-0.025, 0.02, a);
                    float gap = saturate(SceneGap(i) / 1.2);   // soft where the ground in front comes up to the card
                    half3 dkg = CelPalInk(row).rgb;
                    // the glow
                    float2 hm = float2(m.x / 5.0, (m.y - 0.3) / 3.4);
                    float glow = 0.72 * exp(-dot(hm, hm) * 1.6) * (1.0 - smoothstep(0.16, 0.34, a)) * out0;
                    // the rays: up and out to both sides, broad and soft, tapering to points
                    float ray = 0.0, dark = 0.0, lineR = 0.0;
                    float thinR = 1.0 - 0.55 * smoothstep(0.12, 0.30, a), longR = 1.0 + 0.35 * smoothstep(0.06, 0.28, a);
                    [unroll] for (int j = 0; j < 20; j++)
                    {
                        float h0 = CelHash21(float2(j + 3.0, seed * 17.0)), h1 = CelHash21(float2(j + 19.0, seed * 29.0 + 1.0));
                        float h2 = CelHash21(float2(j + 47.0, seed * 41.0 + 2.0)), h3 = CelHash21(float2(j + 61.0, seed * 53.0 + 3.0));
                        float u = (j + 0.5 * h0 + 0.25) / 20.0;
                        float an = radians(-18.0 + 216.0 * u) + (h3 - 0.5) * 1.10;
                        float L = lerp(2.4, 7.5, pow(h1, 1.4)) * out0 * longR;
                        float Wd = lerp(0.25, 1.40, pow(h2, 1.8)) * thinR;
                        float2 d = float2(cos(an), sin(an));
                        float al2 = dot(m, d), cr = m.x * d.y - m.y * d.x, ac = abs(cr);
                        float t = (al2 - 0.4) / max(L, 1e-3);
                        float wd = Wd * pow(saturate(1.0 - t), 0.9);
                        float soft = 0.30 + 0.25 * saturate(t);
                        float mm = saturate((wd - ac) / soft + 0.5) * step(0.0, t) * saturate((1.0 - t) / 0.05) * saturate(t / 0.05) * lerp(0.45, 1.0, h0);
                        if (h3 < 0.25) dark = max(dark, mm);
                        else
                        {
                            ray = max(ray, cr > 0.0 ? 0.0 : mm);
                            dark = max(dark, cr > 0.0 ? mm * 0.45 : 0.0);
                            lineR = max(lineR, mm * saturate((wd * 0.18 - ac) / 0.10 + 0.5) * step(0.72, h1));
                        }
                    }
                    float fadeR = 1.0 - smoothstep(0.14, 0.27, a);
                    ray *= fadeR; dark *= fadeR; lineR *= fadeR;
                    // two long blades up: one broad and seen through, one thin and bright
                    float blades = 0.0, bright = 0.0;
                    [unroll] for (int b = 0; b < 2; b++)
                    {
                        float hb = CelHash21(float2(b + 7.0, seed * 37.0 + 2.0));
                        float an = b == 0 ? 1.82 + 0.12 * hb : 1.36 + 0.10 * hb;
                        float wide = b == 0 ? 0.42 : 0.13;
                        float2 d = float2(cos(an), sin(an));
                        float al2 = dot(m, d), ac = abs(m.x * d.y - m.y * d.x);
                        float t = (al2 - 0.6) / max(B * out0, 1e-3);
                        float wd = wide * sin(3.1416 * saturate(t) * 0.85 + 0.15) * (1.0 - 0.6 * smoothstep(0.18, 0.40, a));
                        float in2 = saturate((wd - ac) / max(pix * 1.5, 1e-4) + 0.5) * step(0.0, t) * step(t, 1.0);
                        blades = max(blades, b == 0 ? in2 : 0.0);
                        bright = max(bright, b == 1 ? in2 : 0.0);
                    }
                    blades *= 1.0 - smoothstep(0.26, 0.42, a);
                    bright *= 1.0 - smoothstep(0.22, 0.36, a);
                    // the yellow halo: soft and ragged round the core, a little above the hit, flame points on its top
                    float ang = atan2(m.y, m.x);
                    float2 cs = float2(cos(ang), sin(ang));
                    float rag = 0.70 * (CelValue2(float2(cs.x * 1.6 + seed * 3.0, cs.y * 1.6 + tk * 0.05)) - 0.5) + 0.50 * (CelValue2(float2(cs.x * 4.0 - seed, cs.y * 4.0 + tk * 0.09)) - 0.5);
                    float spk = pow(saturate(CelValue2(cs * 3.0 + float2(seed * 7.0, tk * 0.4))), 3.0) * 0.55 * step(-0.2, cs.y);
                    float bgrow = 1.0 + 0.25 * smoothstep(0.0, 0.14, a);
                    float bm = length(float2(m.x / (2.10 * bgrow), (m.y - 0.45) / (1.75 * bgrow)));   // a66: a bigger yellow
                    float yel = saturate((1.0 + rag + spk - bm) / 0.28 + 0.5) * out0 * (1.0 - smoothstep(0.20, 0.32, a));
                    // the white core: flame tongues up and out off a low wide base, gaps between them
                    float cw = 1.70 * (1.0 + 0.2 * smoothstep(0.0, 0.05, a)) * (1.0 - smoothstep(0.12, 0.26, a));
                    float2 cq = float2(m.x * 0.80, (m.y - 0.15) * 1.25);   // a66: wider than tall
                    float cr2 = length(cq), cang = atan2(cq.y, cq.x);
                    float2 ccs = float2(cos(cang), sin(cang));
                    float upw = clamp(ccs.y + 0.35, 0.0, 1.35);
                    float s1 = CelValue2(ccs * 12.0 + float2(seed * 11.0, tk * 0.7)), s2 = CelValue2(ccs * 26.0 + float2(-seed * 5.0, tk * 1.1));   // a66: many thin tongues
                    float spikes = pow(saturate(s1), 2.5) * 1.2 + pow(saturate(s2), 3.0) * 0.7;
                    float coreF = cw * (0.46 + 0.45 * pow(upw, 1.5) * spikes + 0.08 * spikes) - cr2;   // a67: short tongues, mostly up
                    float core = saturate(coreF / 0.12 + 0.5) * step(0.02, cw);   // a66: white fire, soft-edged
                    float cream = saturate((coreF + 0.18) / 0.18) * step(0.02, cw);
                    // back to front
                    float3 pc = 0.0; float pa = 0.0;
                    Over(pc, pa, lerp(rim, dkg, 0.3), glow * gap);
                    Over(pc, pa, dkg, dark * 0.75 * gap);
                    Over(pc, pa, lerp(rim, dkg, 0.4), blades * 0.62);
                    Over(pc, pa, shade, ray * 0.85 * gap);
                    Over(pc, pa, lerp(shade, light, 0.6), lineR * 0.8 * gap);
                    Over(pc, pa, shade, bright);
                    Over(pc, pa, lerp(body, shade, saturate((bm - 0.85) * 1.2)), yel);
                    Over(pc, pa, light, cream * 0.9);
                    Over(pc, pa, pale, core);
                    col = pc / max(pa, 1e-4);
                    al = pa * (1.0 - smoothstep(0.36, 0.42, a));
                }
                else if (mode < 3.5)
                {
                    // ---- the crown (a51): a splash of goo, after the clip's frames 31-52. Streams thrown out of a low
                    // wide foot, each a chain of capsules round a centre line whose direction turns along it (a wiggle,
                    // then a curl into a hook) and droops at the tip as it ages: thin straight spikes on its first
                    // frames, flame tongues by the clip's 36 (a wide foot, an S, a curled point), thinning strands with a
                    // drop on a neck at half the tips by 40, gone by 52. Neighbours melt together (their fields add) and
                    // about half are tied to the one before by a thin strand that tears (loops, webs). The front streams
                    // lime lit from the upper left, the back ones a darker green; yellow-lime on the first frames.
                    // q in metres: x across the view from the foot, y up from the ground. Fitted offline (ab/splash.py).
                    float tk = i.shape.x, reach = i.shape.w;
                    float count = clamp(i.shape2.x, 1.0, 96.0), a = i.shape2.z, life = max(i.shape2.w, 0.01);
                    uint rowB = CelRowB(i.tone.x);
                    half3 hotLime = CelPal(rowB, 1), hotYel = CelPal(rowB, 2), hotCream = CelPal(rowB, 3);
                    float ext = 1.0 - exp(-a / 0.045);
                    float wave = smoothstep(0.08, 0.30, a), opn = smoothstep(0.10, 0.55, a);
                    float tear = smoothstep(0.28, 0.62, a), droop = smoothstep(0.50, 1.0, a);
                    float gone = smoothstep(0.78, 1.0, a / life);
                    float thin = (1.0 - 0.38 * tear - 0.12 * droop) * (1.0 - 0.45 * gone);
                    float fall = 2.2 * smoothstep(0.66, 0.92, a);   // it collapses at its end (the clip's top drops 1.5 m f44-48)
                    float lowGo = smoothstep(0.18, 0.36, a);
                    float rootF = lerp(0.40, 0.25, tear), tipF = lerp(0.18, 0.40, tear);   // a66: between twigs and tubes
                    float2 o = float2(0.55, 0.30);
                    const float T = 0.421875;    // a lone capsule's edge: (1 - 1/4)^3
                    float FF = 0.0, FB = 0.0, MF = 0.0, MB = 0.0, TF = 0.0, TB = 0.0;   // each layer's field; its strongest stream's, and that one's tone
                    float2 lastB = o;
                    float lastBR = 0.0, lastBack = -1.0;
                    [loop] for (int j = 0; j < 96; j++)
                    {
                        if (j + 0.5 > count) break;
                        float h0 = CelHash21(float2(j + 1.0, seed * 31.0 + 2.0)), h1 = CelHash21(float2(j + 11.0, seed * 47.0 + 3.0));
                        float h2 = CelHash21(float2(j + 23.0, seed * 59.0 + 4.0)), h3 = CelHash21(float2(j + 37.0, seed * 71.0 + 5.0));
                        float h4 = CelHash21(float2(j + 53.0, seed * 83.0 + 6.0)), h5 = CelHash21(float2(j + 67.0, seed * 97.0 + 7.0));
                        float h6 = CelHash21(float2(j + 79.0, seed * 13.0 + 8.0)), h7 = CelHash21(float2(j + 89.0, seed * 17.0 + 9.0));
                        float h8 = CelHash21(float2(j + 97.0, seed * 23.0 + 1.0)), h9 = CelHash21(float2(j + 103.0, seed * 29.0 + 3.0));
                        float h10 = CelHash21(float2(j + 113.0, seed * 37.0 + 4.0));
                        float tone = CelHash21(float2(j + 127.0, seed * 41.0 + 6.0));   // its own flat green
                        float side = (j + 0.5 + (h0 - 0.5) * 0.9) / count * 2.0 - 1.0;
                        float back = step(h6, 0.45);
                        float early = 1.0 - smoothstep(0.15, 0.40, a);   // wide and low on its first frames
                        float th0 = sign(side) * pow(abs(side), 1.2) * lerp(1.15, 1.75, early) + (h7 - 0.5) * 0.20;
                        float sg = th0 >= 0.0 ? 1.0 : -1.0;
                        float low = smoothstep(1.05, 1.30, abs(th0));
                        float L = reach * (0.22 + 0.78 * pow(h1, 1.6)) * (1.0 - lerp(0.60, 0.20, early) * side * side) * ext;   // a64: mostly short
                        float R = lerp(0.18, 0.44, pow(h2, 1.5)) * (back > 0.5 ? 1.25 : 1.0);
                        float kappa = (h9 > 0.35 ? sg : -sg) * lerp(0.3, 2.6, pow(h8, 1.6)) * opn;
                        float A = lerp(0.30, 0.85, h3) * smoothstep(0.03, 0.20, a);   // a64: wavy
                        float fr = lerp(0.9, 1.8, h4), ph = h5 * 6.2832 + tk * 0.05;
                        float ds = L / 9.0 * (1.0 + 0.10 * opn);
                        float dropTip = step(0.5, h10);
                        float ka = 3.0 + floor(CelHash21(float2(j + 223.0, seed * 59.0 + 1.0)) * 6.0);
                        float kaN = 3.0 + floor(CelHash21(float2(j + 224.0, seed * 59.0 + 1.0)) * 6.0);
                        float kbN = clamp(kaN + floor(CelHash21(float2(j + 228.0, seed * 61.0 + 2.0)) * 3.0) - 1.0, 2.0, 9.0);
                        float2 p = o + float2(side * lerp(1.40, 0.45, early), -0.20 * side * side + 0.75 * early);   // a64: a raised centre at first
                        float2 pPrev = p, tipDir = float2(0.0, 1.0), bA = p, nxtB = p;
                        float rPrev = 0.0, brA = 0.0, nxtBR = 0.0, fs = 0.0;
                        [unroll] for (int k = 0; k < 10; k++)
                        {
                            float u = k / 9.0;
                            float spike = max(1.0 - 0.92 * u, 0.05);
                            float flame = (rootF + (1.0 - rootF) * smoothstep(0.0, 0.25, u)) * max(1.0 - lerp(0.95, 0.80, tear) * pow(u, 1.2), tipF);
                            float prof = lerp(spike, flame, wave);
                            prof = dropTip > 0.5 ? prof * (1.0 - 0.30 * tear * smoothstep(0.70, 0.86, u)) + 0.26 * max(tear, 0.7 * wave) * smoothstep(0.88, 1.0, u) : prof;
                            float bead = 1.0 + 0.45 * wave * (0.6 * sin(u * 9.0 + h0 * 19.0 + tk * 0.11) + 0.4 * sin(u * 15.0 + h1 * 23.0 - tk * 0.07));
                            float r = max(R * prof * bead * thin * (1.0 - lowGo * low), 0.0);
                            float2 pe = p - float2(0.0, fall * pow(u, 1.3));
                            if (k > 0)
                            {
                                fs = max(fs, CapField(q, pPrev, pe, rPrev, r, 0.0));
                                tipDir = pe - pPrev;
                            }
                            if (abs(k - ka) < 0.5) { bA = pe; brA = r; }
                            if (abs(k - kbN) < 0.5) { nxtB = pe; nxtBR = r; }
                            pPrev = pe; rPrev = r;
                            float um = (k + 0.5) / 9.0;
                            float th = th0 + A * sin(6.2832 * fr * um + ph) + kappa * um * um + sg * droop * 0.6 * um * abs(side);
                            float wx = 0.25 * (sin(p.x * 1.805 + p.y * 0.665 + seed * 13.0 + tk * 0.03) + sin(p.y * 1.235 - p.x * 1.045 + seed * 7.0));
                            float wy = 0.25 * (sin(p.y * 1.615 - p.x * 0.76 + seed * 29.0 - tk * 0.03) + sin(p.x * 1.425 + p.y * 1.14 + seed * 11.0));
                            p += float2(sin(th), cos(th)) * ds + float2(wx, wy) * (0.70 * wave * ds);
                        }
                        // a drop off the tip of about half of them, flying on along it
                        float hm = CelHash21(float2(j + 101.0, seed * 29.0 + 3.0));
                        if (hm > 0.60)
                        {
                            float2 dp = pPrev + normalize(tipDir + 1e-6) * ((0.15 + 0.8 * hm) * opn + 0.10);
                            dp.y -= 1.2 * smoothstep(0.3, 0.9, a) * hm;
                            float rd = lerp(0.03, 0.06, CelHash21(float2(j + 131.0, seed * 37.0 + 5.0))) * smoothstep(0.06, 0.16, a) * (1.0 - gone);
                            fs += CapField(q, dp, dp, rd, rd, 0.0);
                        }
                        // the thin strand to the stream before it (in the same layer): loops and webs, torn by the clip's 46
                        float hb = CelHash21(float2(j + 211.0, seed * 53.0 + 9.0));
                        if (hb < 0.65 && abs(lastBack - back) < 0.5)
                        {
                            float span = length(lastB - bA);
                            float br = 0.55 * min(brA, lastBR) * (1.0 - smoothstep(0.55, 0.85, a + 0.2 * hb)) * (1.0 - smoothstep(0.6, 1.6, span));
                            if (br > 1e-3) fs = max(fs, CapField(q, bA, lastB, br, br, 0.45));
                        }
                        lastB = nxtB; lastBR = nxtBR; lastBack = back;
                        if (back > 0.5) { TB = fs > MB ? tone : TB; MB = max(MB, fs); FB += fs; }
                        else { TF = fs > MF ? tone : TF; MF = max(MF, fs); FF += fs; }
                    }
                    // a64: the yellow core mass the spikes come out of on its first frames (the clip's f30-34)
                    float cr = 0.80 * (1.0 - smoothstep(0.10, 0.32, a)) * ext;
                    if (cr > 1e-3)
                    {
                        float cf = CapField(q, o + float2(-0.70, 0.70), o + float2(0.70, 0.80), cr, cr * 0.85, 0.0);
                        TF = cf > MF ? 0.9 : TF; MF = max(MF, cf); FF += cf;
                    }
                    // small round holes torn in its thick parts as it ages (warped value noise): a net of strands
                    float2 wq = q + (float2(CelValue2(q * 0.9 + float2(seed * 5.0, 1.3)), CelValue2(q * 0.9 + float2(-2.1, seed * 3.0))) - 0.5) * 0.8;
                    float nh = 0.6 * CelValue2(wq * 2.2 + float2(seed * 9.0, 0.0)) + 0.4 * CelValue2(wq * 4.62 + float2(-4.0, seed));
                    float hcut = lerp(0.12, 0.42, smoothstep(0.15, 0.60, a));
                    // only where streams overlap (the field beyond the strongest one's): never inside one strand or drop
                    float hF = nh - hcut * smoothstep(0.0, 0.35, FF - MF - 0.10), hB = nh - (hcut + 0.05) * smoothstep(0.0, 0.35, FB - MB - 0.10);
                    float front = saturate((FF - T) / max(fwidth(FF), 1e-5) + 0.5) * saturate(hF / max(fwidth(hF), 1e-5) + 0.5);
                    float backA = saturate((FB - T) / max(fwidth(FB), 1e-5) + 0.5) * saturate(hB / max(fwidth(hB), 1e-5) + 0.5);
                    // each strand one flat green of its own: light, lime or body in front, shade or dark behind
                    half3 fc = TF > 0.55 ? light : (TF > 0.20 ? lerp(body, light, 0.4) : body);
                    half3 bc = TB > 0.50 ? shade : lerp(rim, shade, 0.45);
                    float lit = TF > 0.55 ? 1.0 : 0.0;
                    // yellow-lime on its first frames, a hot heart low in the middle
                    float hot = 1.0 - smoothstep(0.10, 0.30, a);
                    float heartF = saturate(1.0 - length((q - float2(0.55, 0.9)) * float2(1.0, 1.3)) / 1.6);
                    half3 hf = lit > 0.5 ? lerp(hotLime, hotYel, 0.65) : lerp(light, hotLime, 0.6);
                    hf = lerp(hf, hotYel, heartF * 0.7);
                    fc = lerp(fc, lerp(light, hotYel, 0.5), (1.0 - smoothstep(0.25, 0.50, a)) * heartF * 0.6);   // the core lingers
                    fc = lerp(fc, hf, hot);
                    bc = lerp(bc, lerp(CelPal(rowB, 0), CelPalInk(rowB).rgb, 0.45), hot);   // darker spikes among the lime ones
                    col = lerp(bc, fc, front);
                    al = max(backA * 0.92, front);
                    al *= saturate(SceneGap(i) / 0.6) * smoothstep(-0.30, 0.35, q.y);
                    al *= saturate((CelValue2(q * 1.4 + seed * 9.0) - gone * 1.05) / 0.10);   // its end: it breaks up and goes
                }
                else if (mode < 4.5 && i.shape3.w > 0.5)
                {
                    // ---- the flat splash the burst throws on the ground (a07; the clip's frames 29-36): long
                    // streaks lying on the ground, out to the two sides most, in torn sheets; they turn the palette's
                    // shade and thin away as the ring runs out. q in units of its reach, the depth squashed
                    float rad = length(q), ang = atan2(q.y, q.x);
                    float2 spoke = float2(cos(ang), sin(ang));
                    float tk = i.shape.x, grow = i.shape.y, drain = i.shape.z;
                    float turn = ang / 6.2831853 + 0.5;
                    float aside = spoke.x * spoke.x;
                    float streaks = Fingers(turn, 34.0, seed, tk, 0.10, 0.85, 0.75, 0.10, 0.30, 0.8, 0.0) * lerp(0.35, 1.0, aside);
                    float blobs = Fingers(turn, 12.0, seed + 0.47, tk, 0.08, 0.26, 0.8, 0.25, 0.45, 0.6, 1.0);
                    float edge = (0.12 + max(streaks, blobs)) * grow * (1.0 - 0.3 * drain);
                    al = 1.0 - Step(edge, rad, pix);
                    float tornS = CelValue2(float2(turn * 70.0 + seed * 3.0, rad * 4.0 - tk * 0.05));
                    al *= Step(0.18 + 0.55 * drain, tornS, 0.06);
                    float rr = rad / max(edge, 1e-3);
                    col = lerp(body, light, Step(0.62, CelValue2(float2(turn * 48.0 + seed, rad * 3.0)), 0.05));
                    col = lerp(col, shade, saturate(smoothstep(0.45, 1.0, rr) * 0.7 + drain * 0.8));
                    al *= 1.0 - smoothstep(0.55, 1.0, drain);
                    float3 lie = spot - i.shape3.xyz;
                    al *= (1.0 - smoothstep(0.45, 0.7, abs(lie.y - dot(lie.xz, i.shape2.xy)))) * smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                }
                else if (mode < 4.5)
                {
                    float rad = length(q), ang = atan2(q.y, q.x);
                    float2 spoke = float2(cos(ang), sin(ang));
                    float tk = i.shape.x, grow = i.shape.y, drain = i.shape.z, late = i.shape2.z;
                    float R = 0.46 + 0.048 * sin(ang * 2.0 + seed * 20.0) + 0.040 * sin(ang * 3.0 + seed * 47.0) + 0.028 * sin(ang * 5.0 + seed * 61.0)
                        + 0.016 * sin(ang * 8.0 + seed * 83.0) + 0.010 * sin(ang * 13.0 + seed * 29.0 + tk * 0.45);
                    float aside = smoothstep(0.35, 1.0, spoke.x * i.shape.w);
                    float turn = ang / 6.2831853 + 0.5;
                    float tear = smoothstep(0.0, 0.08, drain) * (1.0 - smoothstep(0.14, 0.22, drain));
                    float fingers = max(Fingers(turn, 26.0, seed, tk, 0.025, 0.12, 0.60, 0.22, 0.48, 0.6, 1.0),
                        Fingers(turn, 14.0, seed + 0.31, tk, 0.04, 0.10, 0.40, 0.10, 0.20, 0.8, 1.0) * (0.45 + 0.4 * aside));
                    float shrink = pow(saturate(1.0 - drain), 0.85);
                    float keep = pow(saturate(1.0 - drain), 0.7);
                    float inner = R * shrink * grow;
                    float edge = inner + fingers * keep * grow;
                    al = 1.0 - Step(edge, rad, pix);
                    float2 satG = q * 9.0 + seed * 7.0;
                    float2 satC = floor(satG), satD = frac(satG) - 0.5;
                    float sh1 = CelHash21(satC + 3.1), sh2 = CelHash21(satC + 17.7);
                    float sat = (1.0 - Step(0.10 + 0.16 * sh2, length(satD - (float2(sh1, sh2) - 0.5) * 0.5), pix * 9.0))
                        * step(0.45, sh1) * step(edge, rad) * step(rad, edge * 1.4 + 0.04) * grow;
                    al = max(al, sat);
                    float rr = rad / max(edge, 1e-3), rb = rad / max(inner, 1e-3);
                    float wob = CelValue2(spoke * 1.6 + seed * 3.0 + tk * 0.1);
                    float ringF = rb * 2.6 + wob * 0.6 - tk * 0.035;
                    float ring = 1.0 - Step(0.030, abs(frac(ringF) - 0.5), pix * 2.6 / max(inner, 0.05));
                    ring *= step(0.50, CelValue2(spoke * 2.2 + floor(ringF) * 3.7 + seed * 5.0)) * step(0.22, rb) * step(rb, 0.90);
                    col = lerp(body, light, Step(0.48, CelValue2(q * 3.4 + seed * 5.0 + tk * 0.025), pix * 3.4));
                    col = lerp(col, lerp(shade, rim, 0.5), ring * 0.9);
                    float lip = Step(0.84, rr, pix / max(edge, 0.05)) * (1.0 - smoothstep(-0.35, 0.25, spoke.y));
                    col = lerp(col, lerp(light, pale, 0.5), lip);
                    // late on it is a bright rim round a dark inside: the goo has soaked in and left its edge
                    float band = Step(lerp(0.62, 0.80, drain), rr, pix / max(edge, 0.05));   // a62: a broad bright rim
                    half3 lateC = lerp(rim, shade, 0.25 * Step(0.6, CelValue2(q * 5.0 + seed), 0.05));
                    lateC = lerp(lateC, lerp(light, pale, 0.6), band);
                    lateC = lerp(lateC, lerp(rim, shade, 0.6), ring * 0.5);   // a56: no bright arcs (they read as a smile)
                    col = lerp(col, lateC, late);
                    al *= lerp(1.0, lerp(0.70, 1.0, band), late);
                    float3 lie = spot - i.shape3.xyz;
                    al *= (1.0 - smoothstep(0.45, 0.7, abs(lie.y - dot(lie.xz, i.shape2.xy)))) * smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                }
                else if (mode < 5.5)
                {
                    float ang = atan2(q.y, q.x);
                    float tk = i.shape.x, pop = i.shape.y;
                    float sq = 0.05 * sin(tk * 1.05 + seed * 30.0);
                    float2 p = q / float2(1.0 + sq, 1.0 - sq);
                    float r1 = length(p);
                    if (pop > 0.0001)
                    {
                        float ringR = 0.9 + 0.45 * pop;   // a61: a small ring, not a big thin C
                        float ring = 1.0 - Step(0.07 * (1.0 - pop) + 0.02, abs(r1 - ringR), pix);
                        ring *= step(0.35, CelHash21(float2(floor(ang * 2.2 + seed * 9.0), 3.0)));
                        float fl = (1.0 - Step(0.10 * (1.0 - pop), length(frac(p * 1.6 + seed) - 0.5), pix * 1.6))
                            * step(0.82, CelHash21(floor(p * 1.6 + seed) + 13.0)) * step(r1, ringR);
                        al = max(ring, fl) * (1.0 - pop);
                        col = light;
                    }
                    else
                    {
                        // a glossy bubble: seen through, a bright rim, a highlight
                        al = (1.0 - Step(0.90, r1, pix)) * 0.92;   // a65: glossy marbles, nearly solid
                        col = lerp(lerp(rim, shade, 0.6), lerp(shade, body, 0.5), 1.0 - Step(0.52, length(p - float2(-0.16, 0.22)), pix));   // a65: a darker green body
                        float rimB = Step(0.78, r1, pix);
                        col = lerp(col, lerp(body, light, 0.6), (1.0 - Step(0.34, length((p - float2(0.18, -0.42)) * float2(0.8, 1.6)), pix)) * 0.8);   // the light caught low inside
                        col = lerp(col, lerp(light, pale, 0.3), rimB);
                        al = max(al, rimB * (1.0 - Step(0.90, r1, pix)));
                        col = lerp(col, pale, 1.0 - Step(0.17, length((p - float2(-0.34, 0.38)) * float2(1.0, 1.25)), pix));
                        al = max(al, (1.0 - Step(0.17, length((p - float2(-0.34, 0.38)) * float2(1.0, 1.25)), pix)));
                        float2 g = (q - float2(0.0, -0.92)) / float2(1.22, 0.30);
                        float arc = (1.0 - Step(0.10, abs(length(g) - 1.0), pix * 3.0)) * step(g.y, 0.25) * step(0.94, r1);
                        col = lerp(col, rim, arc);
                        al = max(al, arc * 0.8);
                    }
                }
                else if (mode < 6.5)
                {
                    float tk = i.shape.x, fray = i.shape.y, lit = i.shape.z;
                    float f = Puff(q, seed, tk);
                    float n = CelValue2(q * 2.6 + seed * 13.0 + tk * 0.055) * 0.6 + CelValue2(q * 6.5 + seed * 5.0 - tk * 0.035) * 0.4 - 0.5;
                    float strand = CelValue2(float2(q.x * 8.0 + seed * 19.0, q.y * 1.6 - tk * 0.045)) - 0.5;
                    float cut = f + 0.20 * n + (0.05 + 0.22 * fray) * strand - fray * 0.34;
                    float softP = i.tone.w;
                    al = lerp(smoothstep(0.0, 0.06, cut), smoothstep(-0.10, 0.32, cut), softP);
                    float glow = saturate(lit);
                    float age = k + 0.25 * n;
                    col = lerp(pale, light, smoothstep(0.0, lit * 0.6, age));
                    col = lerp(col, body, smoothstep(lit * 0.5, lit * 1.4, age));
                    col = lerp(col, shade, smoothstep(0.5, 1.0, k + 0.20 * n) * 0.8 * (1.0 - 0.6 * glow));
                    // the glowing cloud goes a deep green as it ages (the clip's frames 84-110)
                    col = lerp(col, lerp(rim, shade, 0.6), step(0.65, softP) * smoothstep(0.50, 0.92, k + 0.20 * n) * 0.85);
                    float under = smoothstep(-0.15, 0.55, dot(q, float2(0.45, -0.85)) + 0.5 * n);
                    col = lerp(col, lerp(shade, body, glow), under * 0.8);
                    col = lerp(col, rim, smoothstep(0.55, 0.95, under + 0.6 * n) * 0.5 * (1.0 - 0.7 * glow));
                    // the glowing cloud's puffs shaded as lumps: dark below, light on top toward the upper left
                    float deepP = step(0.65, softP);
                    float topL = smoothstep(0.1, 0.6, dot(q, float2(-0.35, 0.94)) + 0.4 * n);
                    col = lerp(col, lerp(rim, shade, 0.5), deepP * under * 0.55);
                    col = lerp(col, lerp(light, pale, 0.3), deepP * topL * (1.0 - k) * 0.6);
                    col = lerp(col, lerp(light, pale, 0.4), softP * smoothstep(0.05, 0.45, f) * (1.0 - k));
                    al *= (1.0 - smoothstep(0.35, 1.0, fray)) * smoothstep(0.0, 0.08, k);
                    al *= saturate(SceneGap(i) / 0.5);
                    col = MixFog(col, i.fog);
                }
                else if (mode > 8.5)
                {
                    // ---- the goo on the target (a08; the clip's frames 46-104): wherever the scene's own surface lies
                    // in the target's box it is painted over - a dark green coat, a lime band along its outline and on
                    // what faces up, lime runs sliding down it - and it darkens as it ages, then goes
                    float tk = i.shape.x, on = i.shape.y, a = i.shape.z, kk = i.shape.w;
                    float inside = CoatIn(spot, i);
                    float e1 = 0.0, e2 = 0.0;
                    [unroll] for (int e = 0; e < 8; e++)
                    {
                        float2 dir = float2(cos(e * 0.7853982), sin(e * 0.7853982));
                        e1 = max(e1, 1.0 - CoatIn(SpotAt(i.positionCS.xy + dir * 2.5), i));
                        e2 = max(e2, 1.0 - CoatIn(SpotAt(i.positionCS.xy + dir * 6.0), i));
                    }
                    float3 dd = spot - i.shape3.xyz;
                    float across = dd.x * i.flow.x + dd.z * i.flow.y;
                    float topF = smoothstep(0.80, 0.97, abs(facing.y) / max(length(facing), 1e-20));
                    float fromTop = i.shape2.y - dd.y;
                    float lane = CelValue2(float2(across * 3.2 + seed * 3.0, 0.5));
                    float reachD = 0.25 + (0.35 + 1.5 * CelValue2(float2(across * 4.5 - seed, 2.5))) * smoothstep(0.0, 0.7, a);
                    float runs = Step(0.62, lane, 0.04) * (1.0 - Step(reachD, fromTop, 0.03));
                    float cap = 1.0 - Step(0.25 + 0.10 * CelValue2(float2(across * 5.0, 9.0)), fromTop, 0.03);
                    float blot = Step(0.62, CelValue2(float2(across * 4.0 - seed, dd.y * 3.0)), 0.04) * (1.0 - runs);
                    col = lerp(CelPalInk(row).rgb, rim, 0.35);
                    col = lerp(col, lerp(shade, body, 0.4), blot * 0.6);
                    col = lerp(col, lerp(body, light, 0.45), max(runs, cap));
                    col = lerp(col, lerp(body, light, 0.5), e2 * 0.35);
                    col = lerp(col, lerp(body, light, 0.5), max(e1, topF));
                    col = lerp(col, lerp(CelPalInk(row).rgb, rim, 0.5), smoothstep(0.20, 0.85, kk) * 0.55);
                    al = inside * on;
                }
                else if (mode < 7.5)
                {
                    float f = GlowOn(i, spot, facing);
                    return half4(Unvignette(i.shape2.rgb * f * GlowSeen(spot), i.positionCS.xy, 0.35), 0.0);
                }
                else
                {
                    // ---- the ring (a52): a sheet of goo running out over the ground from the crown's foot, streaked
                    // round the ring (a slight spiral): filled at first, its inner edge running out after it, holes
                    // opening between the streaks until it is torn arcs; the palette's light, body and shade in streaks,
                    // a bright line on its outer edge. q in units of the radius it runs out to. Fitted offline (ab/ring.py).
                    float rad = length(q), ang = atan2(q.y, q.x);
                    float tk = i.shape.x, run = i.shape.y, kk = i.shape.z;
                    float2 cs = float2(cos(ang), sin(ang));
                    float wob = 0.12 * (CelValue2(float2(cs.x * 2.0 + seed * 7.0, cs.y * 2.0 + tk * 0.02)) - 0.5);
                    float Rr = run * (1.0 + wob);
                    float Ri = Rr * lerp(0.25, 0.78, smoothstep(0.048, 0.524, kk));
                    float sb = (rad - Ri) / max(Rr - Ri, 1e-4);
                    float phi = ang + 0.9 * rad;
                    float2 cp = float2(cos(phi), sin(phi));
                    float t1 = CelValue2(float2(cp.x * 3.2 + seed * 11.0, cp.y * 3.2 + rad * 14.0));
                    float t2 = CelValue2(float2(cp.x * 5.44 - seed * 5.0, cp.y * 5.44 + rad * 22.4 + 3.0));
                    float n = 0.65 * t1 + 0.35 * t2;
                    float hole = lerp(0.18, 0.52, smoothstep(0.10, 0.80, kk));
                    float m = saturate((n - hole) / 0.05) * step(0.0, sb) * step(sb, 1.0);
                    m *= saturate((1.0 - sb) / 0.22) * saturate(sb / 0.35) * (1.0 - smoothstep(0.75, 1.0, kk));
                    float tone = CelValue2(float2(cp.x * 5.0 + seed * 3.0, cp.y * 5.0 + rad * 22.0 + 9.0));
                    col = tone > 0.62 ? light : (tone < 0.40 ? lerp(shade, body, 0.35) : body);
                    al = m;
                    float3 lie = spot - i.shape3.xyz;
                    al *= (1.0 - smoothstep(1.0, 1.6, abs(lie.y - dot(lie.xz, i.shape2.xy)))) * smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                }
                al *= i.tone.z;
                col = Unvignette(col, i.positionCS.xy, 1.0);
                return half4(col * al, al);
            }
        ENDHLSL
        Pass
        {
            Name "CelAcidBallLight"
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
            Name "CelAcidBall"
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
