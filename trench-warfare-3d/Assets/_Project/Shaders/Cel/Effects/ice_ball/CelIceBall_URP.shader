// Phase: CelFx ice_ball (2026-11-15, i02: every part redrawn against the clip) - one GPU pool for the whole ice ball:
// the ball and its tail, the chips, the crystal it bursts into, the swirl it throws over the ground, the mist the
// crystal breaks down into and the light on the ground. Every shape is a field in the fragment; nothing is a
// texture (the swirl and the light read the camera's depth). The clip is drawn on ONES: the age a record is drawn
// at is stepped 24 times a second on the EFFECT's clock (Extra.w carries the seconds from the effect's start to
// the record's birth, so every record of one effect steps on the same frames).
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 comet    the ball and its tail, one strip along the path it flies. Pos where it starts, Vel the launch
//             velocity, Motion.x gravity. Size x the ball's radius (m), y the tail's length (m, as seen: across
//             the view), z the strip's half-width in ball radii, w the seconds of flight to where it HOLDS.
//             Extra y the seconds it holds there, z the seconds of its lunge into the target; Axis the lunge
//             (world metres from where it holds to where it hits). Life = flight + hold + lunge (+ a frame).
//             The ball is a round head of pale ice with a dark crescent inside its nose, shards swept back off
//             it (BallShards) and thin arcs ahead of its nose; the tail a band of grey-blue streaks round a white
//             middle, drawn lines along it, its upper edge mist. As it lunges it softens into a white star.
//             Tone x the ball's row + 256 x the tail's row.
//  1 drop     a ballistic fleck. Pos, Vel, Motion.x gravity, Motion.y drag, Motion.z spin (a shard). Size x radius
//             at birth (m), y at death, w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a
//             round fleck, 2 a leaf (a point at both ends), 3 a shard (a kite of ice, two faces, a line round it).
//  2 crystal  the burst: a cluster of faceted ice spikes round a white heart, on one card facing the eye. Pos its
//             heart, Size x the card's half-extent (m), z metres it is drawn toward the eye. It shoots out in two
//             frames, stands, and breaks up from Extra.y frames on (each spike a little later than that by its
//             own delay: pieces drop out between slanted cracks, tip first). Spikes / SpikeFaces are the clip's.
//  3 mist     what the crystal breaks down into, on one card: a cloud (soft lobes summed and broken up by
//             noise) that gathers at its heart, bright inside and grey at its rim, parts round the target and
//             thins away in holes. Pos its middle, Size x its half-width (m), y its half-height, z metres toward
//             the eye, w how many lobes (up to 24). Tone x the early row + 256 x the late row.
//  4 swirl    the ring of spray the burst throws over the ground: a disc, then a ring, then spiral strands that
//             thin as they run out. It is laid out on the LEVEL plane through its foot as the eye sees it (the
//             clip's is a circle on level ground whatever the ground under it does) and painted wherever the
//             scene's depth shows ground near that plane. Pos the foot of the burst, Axis a level axis of its
//             own, Size x its last reach (m), y how far above or below the plane the ground may be (m).
//  5 chart    lab only: a card of flat colours for measuring what the scene's grade makes of a written colour
//             (IceBallRange's "ice_ball_swatch"). Pos its middle, Size x its half-extent (m), z metres toward the
//             eye. Eight blocks of 7 x 7 cells, one block every three frames: red across, green down (0, 48, 96,
//             144, 192, 224, 255), blue by block; the eighth block is a grey ramp. Extra y 1: the palette
//             instead, as the recipe wrote it: 8 rows from Tone.x's down, c0 c1 c2 c3 pale ink across.
//  7 glow     the ice's light on whatever stands behind it (the ground, the target, a crate): the lit point is
//             found from the scene's depth, so the card itself is never hidden. Pos the light (a metre over the
//             ground), Axis a level axis of its own. Size x its reach along that axis (m), y up and down, w its
//             reach square to it on the near side over x, z its reach on the far side over that. Vel how much of
//             what it lights it multiplies into it at its heart (rgb), Erode xyz the colour it adds on top of
//             that. Extra y seconds it takes to come up, z seconds it takes to go at the end of its life.
//             Tone y 1 if it falls on level ground only, z its strength. Needs the camera's depth texture.
// Palette rows: c0 rim, c1 shade, c2 body, c3 light, core pale, ink.
//
// Two passes, one draw: the light pass first (only the glows draw in it: lit = unlit x (1 + what a glow multiplies
// in)), then everything, in the records' order.
//
// _TWAcidLens (global; IceBallRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0
// none. Every colour is lifted by what the vignette will take from it, as far as white allows.
Shader "TW/Cel/IceBall"
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

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float2 q : TEXCOORD1;                       // the card's own coordinates (per mode)
                float4 flow : TEXCOORD2;                    // per mode, interpolated
                nointerpolation float4 tone : TEXCOORD3;
                nointerpolation float4 state : TEXCOORD4;   // mode, age (held on the frame), life fraction, seed
                nointerpolation float4 shape : TEXCOORD5;   // per mode
                nointerpolation float4 shape2 : TEXCOORD6;  // per mode
                float fog : TEXCOORD7;
                nointerpolation float4 shape3 : TEXCOORD8;  // per mode
            };

            // The crystal's spikes as the clip's frame 46 shows them (celfx/runs/ice_ball/_ref/look/polar46_*.jpg),
            // the ones behind first: the angle up from the eye's right (degrees), the length (m), the half-width
            // at its widest (m), and how far along it that is (0..1).
            #define SPIKES 33
            static const float4 Spikes[SPIKES] =
            {
                float4(105.0, 3.0, 0.3, 0.35), float4(130.5, 3.4, 0.3, 0.35), float4(88.0, 2.8, 0.28, 0.35),
                float4(50.0, 2.7, 0.26, 0.35), float4(16.0, 3.2, 0.3, 0.4), float4(-18.0, 3.0, 0.28, 0.4),
                float4(75.9, 5.0, 0.34, 0.35), float4(68.7, 6.3, 0.62, 0.42), float4(84.4, 3.5, 0.12, 0.3),
                float4(98.0, 3.9, 0.4, 0.3), float4(112.0, 2.5, 0.25, 0.35), float4(123.2, 5.3, 0.74, 0.42),
                float4(119.5, 4.4, 0.16, 0.4), float4(138.7, 4.05, 0.6, 0.42), float4(136.5, 3.3, 0.13, 0.4),
                float4(59.5, 3.7, 0.36, 0.35), float4(41.0, 3.0, 0.2, 0.35), float4(25.4, 4.3, 0.34, 0.4),
                float4(33.0, 2.6, 0.18, 0.35), float4(-9.5, 3.7, 0.34, 0.45), float4(10.1, 5.95, 0.56, 0.55),
                float4(-26.0, 4.8, 0.42, 0.45), float4(-38.2, 3.57, 0.38, 0.45), float4(-47.6, 2.2, 0.22, 0.4),
                float4(179.0, 2.83, 0.22, 0.4), float4(156.8, 2.0, 0.24, 0.4), float4(-63.0, 1.3, 0.2, 0.4),
                float4(-78.0, 1.5, 0.2, 0.4), float4(-95.0, 1.3, 0.2, 0.4), float4(-112.0, 1.5, 0.22, 0.4),
                float4(-130.0, 1.3, 0.2, 0.4), float4(-150.0, 1.7, 0.22, 0.4), float4(-166.0, 1.5, 0.2, 0.4)
            };
            // their faces: x the tone of the face on its counter-clockwise side + 8 x the tone of its clockwise
            // one (0 rim, 1 shade, 2 body, 3 light, 4 pale, 5 between shade and body); y where the ridge between
            // them runs (-1 its counter-clockwise edge .. 1 its clockwise edge); z how much wider its clockwise
            // side is (-1..1); w the frames after the crystal starts to break at which this one does.
            static const float4 SpikeFaces[SPIKES] =
            {
                float4(1.0 + 8.0 * 5.0, 0.0, 0.0, 2.0), float4(5.0 + 8.0 * 1.0, 0.0, 0.0, 0.5), float4(5.0 + 8.0 * 2.0, 0.0, 0.0, 3.0),
                float4(5.0 + 8.0 * 2.0, 0.0, 0.0, 2.5), float4(1.0 + 8.0 * 5.0, 0.0, 0.0, 2.0), float4(5.0 + 8.0 * 2.0, 0.0, 0.0, 2.5),
                float4(3.0 + 8.0 * 2.0, 0.1, 0.0, 4.5), float4(3.0 + 8.0 * 2.0, -0.1, 0.0, 4.0), float4(4.0 + 8.0 * 4.0, 0.0, 0.0, 3.0),
                float4(3.0 + 8.0 * 5.0, 0.15, 0.0, 2.5), float4(5.0 + 8.0 * 1.0, -0.2, 0.0, 1.5), float4(1.0 + 8.0 * 2.0, -0.55, 0.0, -1.0),
                float4(3.0 + 8.0 * 3.0, 0.0, 0.0, -1.0), float4(1.0 + 8.0 * 2.0, -0.5, 0.0, 0.5), float4(3.0 + 8.0 * 3.0, 0.0, 0.0, 0.5),
                float4(2.0 + 8.0 * 5.0, 0.0, 0.0, 3.0), float4(4.0 + 8.0 * 3.0, 0.0, 0.0, 2.0), float4(2.0 + 8.0 * 5.0, 0.0, 0.0, 2.0),
                float4(3.0 + 8.0 * 2.0, 0.0, 0.0, 2.5), float4(2.0 + 8.0 * 3.0, 0.0, 0.0, 2.5), float4(2.0 + 8.0 * 5.0, 0.35, 0.6, 2.0),
                float4(4.0 + 8.0 * 2.0, 0.1, 0.0, 3.0), float4(4.0 + 8.0 * 2.0, 0.0, 0.0, 3.0), float4(4.0 + 8.0 * 3.0, 0.0, 0.0, 2.0),
                float4(1.0 + 8.0 * 2.0, 0.0, 0.0, 1.0), float4(2.0 + 8.0 * 5.0, 0.0, 0.0, 1.0), float4(4.0 + 8.0 * 3.0, 0.0, 0.0, 0.0),
                float4(3.0 + 8.0 * 2.0, 0.0, 0.0, 1.0), float4(4.0 + 8.0 * 2.0, 0.0, 0.0, 0.5), float4(3.0 + 8.0 * 2.0, 0.0, 0.0, 1.5),
                float4(2.0 + 8.0 * 1.0, 0.0, 0.0, 0.0), float4(3.0 + 8.0 * 2.0, 0.0, 0.0, 1.0), float4(2.0 + 8.0 * 1.0, 0.0, 0.0, 0.5)
            };
            // the soft white streaks thrown out past the spikes to the upper right: angle (degrees), half-width (m)
            static const float2 Needles[5] =
            {
                float2(26.1, 0.09), float2(19.8, 0.08), float2(30.3, 0.10), float2(36.0, 0.06), float2(14.5, 0.06)
            };
            // The swirl's strands (celfx/runs/ice_ball/_ref/swirl_preview.py against the clip's frames 43-50): where
            // round the foot it starts (turns, counter-clockwise from the eye's right as seen from above), how far
            // round it runs (turns), its radius at its start and at its end (of the swirl's reach).
            static const float4 SwirlArcs[5] =
            {
                float4(0.42, 0.60, 0.80, 1.00), float4(0.46, 0.62, 0.64, 0.90), float4(0.56, 0.36, 0.54, 0.66),
                float4(0.90, 0.32, 0.86, 1.00), float4(0.10, 0.45, 0.70, 0.92)
            };
            static const float SwirlThick[5] = { 1.0, 0.9, 0.6, 0.9, 0.7 };   // how thick each is
            // The ball's shards as the clip's frame 30 shows them (look/ball_f30_zoom.jpg), in the ball's own
            // frame (0 degrees its nose, counter-clockwise up the screen), lengths in ball radii: where on the
            // ball it is rooted (degrees), which way it points (degrees), its length past its root, its half-width.
            #define SHARDS 22
            #define OUTER_SHARDS 14     // the first 14 stand off its round; the rest lie over it, swept back
            static const float4 BallShards[SHARDS] =
            {
                float4(107.0, 139.0, 1.05, 0.17), float4(120.0, 186.0, 1.15, 0.15), float4(78.0, 100.0, 0.45, 0.12),
                float4(60.0, 78.0, 0.35, 0.10), float4(45.0, 60.0, 0.28, 0.09), float4(92.0, 122.0, 0.70, 0.14),
                float4(135.0, 160.0, 0.80, 0.14), float4(-125.0, -148.0, 1.00, 0.15), float4(-95.0, -113.0, 0.62, 0.12),
                float4(-110.0, -135.0, 0.70, 0.12), float4(-70.0, -85.0, 0.30, 0.09), float4(-45.0, -55.0, 0.20, 0.08),
                float4(-145.0, -165.0, 0.90, 0.14), float4(-160.0, -178.0, 1.00, 0.13),
                float4(150.0, 165.0, 1.10, 0.12), float4(170.0, 182.0, 1.30, 0.12), float4(-165.0, -172.0, 1.20, 0.12),
                float4(120.0, 150.0, 0.75, 0.11), float4(-130.0, -155.0, 0.80, 0.11), float4(100.0, 135.0, 0.60, 0.10),
                float4(-100.0, -130.0, 0.60, 0.10), float4(180.0, 176.0, 1.50, 0.10)
            };
            // x how far from the ball's middle it is rooted (radii), y the tone of its counter-clockwise face,
            // z the tone of its clockwise one (the crystal's tones)
            static const float3 BallShardFaces[SHARDS] =
            {
                float3(0.85, 3.0, 2.0), float3(0.80, 4.0, 3.0), float3(0.85, 4.0, 3.0), float3(0.88, 4.0, 3.0),
                float3(0.90, 4.0, 4.0), float3(0.85, 3.0, 2.0), float3(0.80, 2.0, 5.0), float3(0.80, 3.0, 2.0),
                float3(0.85, 2.0, 5.0), float3(0.82, 3.0, 2.0), float3(0.88, 4.0, 3.0), float3(0.90, 4.0, 4.0),
                float3(0.80, 2.0, 3.0), float3(0.75, 4.0, 3.0),
                float3(0.45, 3.0, 2.0), float3(0.35, 4.0, 3.0), float3(0.40, 3.0, 2.0), float3(0.50, 2.0, 5.0),
                float3(0.50, 3.0, 2.0), float3(0.60, 4.0, 3.0), float3(0.60, 2.0, 5.0), float3(0.15, 4.0, 3.0)
            };

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
                float wholeFrame = 0.0;
                bool decal = (mode > 3.5 && mode < 4.5) || mode > 6.5;
                if (mode < 0.5)
                {
                    // the comet: a strip along the path, from ahead of the ball back to the end of the tail
                    float T = max(r.size.w, 0.01), R = r.size.x, L = r.size.y;
                    float holdS = r.extra.y, lungeS = max(r.extra.z, 0.01);
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float sH = min(a, T);                               // the ball stops where it holds
                    float lunge = saturate((a - T - holdS) / lungeS);
                    lunge = lerp(lunge, lunge * (2.0 - lunge), 0.6);    // it goes in at once and eases onto the target
                    float3 head = r.pos + r.vel * sH + 0.5 * G * sH * sH + r.axis * lunge;
                    float3 tanH = normalize(r.vel + G * sH);
                    // the path runs away from the eye, so a metre along it is less than a metre across the view:
                    // the strip is laid out in metres AS SEEN, so the ball stays round from any side
                    float sinT = max(length(cross(tanH, normalize(_WorldSpaceCameraPos - head))), 0.25);
                    float speed = max(length(r.vel), 0.01);
                    float X = lerp(-2.6 * R, L, v.uv.x);                // metres behind the ball's middle, as seen
                    float s = max(sH - X / (sinT * speed), -0.12);      // (never back to behind the eye)
                    float follow = saturate(1.0 - X / (4.0 * R));       // the tail's near end goes in with the ball
                    float3 c = r.pos + r.vel * s + 0.5 * G * s * s + r.axis * lunge * follow;
                    float3 tangent = normalize(r.vel + G * s);
                    float3 side = normalize(cross(tangent, normalize(_WorldSpaceCameraPos - c)));
                    side *= side.y >= 0.0 ? 1.0 : -1.0;                 // up the screen
                    float halfW = R * max(r.size.z, 1.0);
                    w = c + side * cq.y * halfW;
                    float3 seen = w - _WorldSpaceCameraPos;
                    w = _WorldSpaceCameraPos + seen * (1.0 - min(2.5 / max(length(seen), 0.01), 0.5));   // 2.5 m nearer the eye, on its own line of sight: over what it strikes
                    q = float2(X, cq.y * halfW);
                    flow = float4(s * speed * sinT, speed * sinT * T, 0.0, 0.0);   // metres along the path, as seen (stays put); the whole flight's
                    shape = float4(R, L, tick, lunge);
                    shape2 = float4(a, T, holdS, lungeS);
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
                    if (r.extra.y > 2.5)
                    {
                        float turn = r.seed * 6.2831853 + r.motion.z * a;   // a shard tumbles
                        d2 = float2(cos(turn), sin(turn));
                        len = rad * 1.7;
                    }
                    w = p + (vr * d2.x + vu * d2.y) * cq.x * len + (vu * d2.x - vr * d2.y) * cq.y * rad;
                    shape = float4(len / max(rad, 1e-4), r.extra.y, 0.0, 0.0);
                }
                else if (mode < 2.5)
                {
                    // the crystal: one card, drawn toward the eye at the size it would have where it stands
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(r.size.z, dist * 0.5);
                    w = CelBillboard(r.pos + toEye / dist * pull, cq * r.size.x * ((dist - pull) / dist));
                    q = cq * r.size.x;                                  // metres from its heart
                    shape = float4(tick, a, r.size.x, r.extra.y);
                }
                else if (mode < 3.5)
                {
                    // the mist: one card, drawn toward the eye at the size it would have where it stands
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(r.size.z, dist * 0.5);
                    const float Card = 1.8;
                    w = CelBillboard(r.pos + toEye / dist * pull, cq * r.size.x * Card * ((dist - pull) / dist));
                    float aspect = r.size.y / max(r.size.x, 1e-3);
                    q = cq * Card;                                      // in the cloud's half-widths, both ways
                    shape = float4(r.size.x, a, aspect, r.size.w);
                    shape3 = float4(r.pos, 0.0);
                }
                else if (mode < 4.5)
                {
                    // the swirl: a card that covers the ball its reach makes; painted on the scene's depth
                    float R = max(r.size.x, 0.1);
                    float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                    w = CelBillboard(r.pos, cq * R * cover);
                    wholeFrame = step(dist, R * 1.02) * alive;
                    float3 right = normalize(float3(r.axis.x, 0.0, r.axis.z));
                    flow = float4(right.x, right.z, 0.0, max(r.size.y, 0.05));
                    shape = float4(R, a, tick, 0.0);
                    shape3 = float4(r.pos, 0.0);
                }
                else if (mode < 5.5)
                {
                    // the chart: a square card facing the eye, drawn toward it
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(r.size.z, dist * 0.5);
                    w = CelBillboard(r.pos + toEye / dist * pull, cq * r.size.x * ((dist - pull) / dist));
                    shape = float4(tick, r.extra.y, 0.0, 0.0);
                }
                else
                {
                    float reachZ = r.size.x * max(r.size.w, 0.01);
                    float R = max(max(r.size.x, r.size.y), reachZ);     // the ball that holds the light
                    float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));   // a card through its middle hides all of it from here
                    w = CelBillboard(r.pos, cq * R * cover);
                    wholeFrame = step(dist, R * 1.02) * alive;          // the eye is inside the light: the whole frame
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
                if (decal)
                {
                    if (wholeFrame > 0.5) o.positionCS = float4(cq, 0.0, 1.0);
                    o.positionCS.z = o.positionCS.w * lerp(UNITY_NEAR_CLIP_VALUE, UNITY_RAW_FAR_CLIP_VALUE, 0.0002);   // nothing hides a decal's card
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

            // how far to the nearest of the points scattered one to a cell (each drifts round slowly), and the way to it
            float IceWorley(float2 u, float t, float k, out float2 way)
            {
                float2 ip = floor(u), fp = u - ip;
                float best = 9.0;
                way = 0.0;
                for (int yy = -1; yy <= 1; yy++)
                    for (int xx = -1; xx <= 1; xx++)
                    {
                        float2 g = float2(xx, yy);
                        float hx = CelHash21(ip + g + float2(13.0 + k, 57.0)), hy = CelHash21(ip + g + float2(71.0, 29.0 + k));
                        float2 o = 0.5 + 0.38 * float2(sin(6.2831853 * hx + t * (0.6 + hy)), cos(6.2831853 * hy + t * (0.6 + hx)));
                        float2 r = g + o - fp;
                        float d = dot(r, r);
                        if (d < best) { best = d; way = r; }
                    }
                return sqrt(best);
            }

            // a step at edge, w wide
            float Step(float edge, float v, float w) { return smoothstep(edge - w, edge + w, v); }

            // the colour as it has to be written for the lens vignette to leave it as it is (the header)
            // (share: how much of what the lens takes is made up, 1 all of it)
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

            // a glow's strength (0..1) on a point of the opaque scene that faces `facing` (a light that falls on level
            // ground only leaves whatever stands on it alone)
            float GlowOn(Varyings i, float3 spot, float3 facing)
            {
                float3 d = spot - i.shape3.xyz;
                float across = d.x * i.flow.x + d.z * i.flow.y, back = d.z * i.flow.x - d.x * i.flow.y;   // metres along its axis, and square to it on the far side
                float t = length(float3(across / i.shape.x, back / (i.shape.y * (back > 0.0 ? i.flow.z : 1.0)), d.y / i.flow.w));
                float lvl = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                return exp(-3.0 * t * t) * (1.0 - smoothstep(0.8, 1.0, t)) * i.shape.z * i.tone.z * lerp(1.0, lvl, i.tone.y);
            }

            // what the scene's fog and mist leave of a light that falls on a point
            half3 GlowSeen(float3 spot)
            {
                return CelAdditiveFog(half3(1.0, 1.0, 1.0), spot, ComputeFogFactor(TransformWorldToHClip(spot).z));
            }

            // the light pass: only the glows draw, and what they write is multiplied into what is there
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

            half4 frag(Varyings i) : SV_Target
            {
                float mode = i.state.x, k = i.state.z, seed = i.state.w;
                float2 q = i.q;
                float3 spot = 0.0;
                if (mode > 2.5) spot = SceneSpot(i);                    // a decal falls on what the scene shows behind this pixel (the mist asks what stands in it)
                float3 facing = cross(ddy(spot), ddx(spot));            // which way it faces
                float2 fw = fwidth(q);
                float pix = max(max(fw.x, fw.y) * 0.75, 1e-5);
                float pixY = max(fw.y * 0.75, 1e-5);
                uint row = CelRowA(i.tone.x);
                uint rowB = CelRowB(i.tone.x);
                half3 rim = CelPal(row, 0), shade = CelPal(row, 1), body = CelPal(row, 2), light = CelPal(row, 3);
                half3 pale = CelPalCore(row).rgb;
                half3 ink = CelPalInk(row).rgb;
                half3 col = body;
                half3 extra = 0.0;                                      // light added on top of everything
                float al = 0.0;

                if (mode < 0.5)
                {
                    // ---- the comet. X metres behind the ball's middle, Y metres across (up the screen)
                    float R = i.shape.x, L = i.shape.y, tk = i.shape.z, lunge = i.shape.w;
                    float a = i.shape2.x, T = i.shape2.y;
                    float X = q.x, Y = q.y, P = i.flow.x;
                    float sd = seed * 17.0;
                    float settled = saturate((a - T + 0.04) / 0.12);    // 0 in flight .. 1 held at the target: the drawn look
                    float star = smoothstep(0.85, 1.0, lunge);          // 0 the ball .. 1 the soft white star it is as it goes in
                    float blur = smoothstep(0.25, 0.75, lunge);         // it loses its lines and its edge on the way in
                    float inky = settled * (1.0 - blur);
                    float Rh = R * (1.0 - 0.12 * lunge) * lerp(1.12, 1.0, settled);
                    half3 mid = lerp(shade, body, 0.5);
                    half3 tRim = CelPal(rowB, 0), tShade = CelPal(rowB, 1), tBody = CelPal(rowB, 2), tLight = CelPal(rowB, 3);
                    half3 tPale = CelPalCore(rowB).rgb, tInk = CelPalInk(rowB).rgb;

                    // -- the tail: grey-blue streaks round a thin white middle, its upper edge mist, its lower edge
                    // a cut. It narrows back along the path (the clip's hardly widens toward the eye).
                    float backF = saturate(X / max(i.flow.y, 1e-3));    // 0 at the ball .. 1 where it was launched
                    float hT = R * 0.90 * lerp(1.0, 0.36, smoothstep(0.0, 0.8, backF)) * (1.0 - 0.65 * lunge);
                    float n1 = CelValue2(float2(P * 0.25 + sd, 2.0)), n2 = CelValue2(float2(P * 0.9 - sd, 7.0));
                    float yn = Y / max(hT, 1e-3) + 0.10 * (n1 - 0.5) + 0.05 * (n2 - 0.5);
                    float upA = 1.0 - smoothstep(lerp(0.60, 0.30, smoothstep(0.0, 0.3, backF)), 1.05, yn + 0.25 * (n2 - 0.5));
                    float dnA = smoothstep(-1.05, lerp(-0.75, -0.55, smoothstep(0.0, 0.3, backF)) + 0.20 * (n1 - 0.5), yn);    // its lower edge: a feathered cut
                    float endFade = saturate((L - X) / (2.0 * R));
                    float aTail = upA * dnA * step(0.3 * Rh, X) * endFade * (1.0 - 0.3 * blur) * lerp(1.0, 0.75, backF) * (0.72 + 0.28 * CelValue2(float2(P * 0.5 + sd, yn * 3.0)));   // froth, not a ribbon
                    float st = CelValue2(float2(P * 0.08 + sd * 0.3, yn * 14.0 + 3.0));   // long streaks
                    aTail *= lerp(1.0, lerp(0.40, 1.0, smoothstep(0.25, 0.55, CelValue2(float2(P * 0.22 + sd, yn * 8.0 + 3.0)))), smoothstep(0.08, 0.45, backF));   // away from the ball it thins into strands, the night showing between them
                    float st2 = CelValue2(float2(P * 0.30 - sd, yn * 30.0));              // and thin ones
                    float cy = yn + 0.08;                               // its bright middle sits a little low
                    float lum = 0.34 + 0.36 * (1.0 - smoothstep(0.0, 0.85, abs(cy))) + 0.50 * (st - 0.5) + 0.25 * (st2 - 0.5) + 0.30 * (1.0 - smoothstep(0.0, 0.5, backF));   // whitest at the ball
                    lum += 0.25 * (1.0 - smoothstep(0.06, 0.12, abs(cy) + 0.08 * (st - 0.5))) * (1.0 - 0.7 * backF);   // the white streaks, strongest at the ball
                    float eb = 0.05;                                    // brushed, not cut
                    half3 cTail = tRim;
                    cTail = lerp(cTail, tShade, Step(0.14, lum, eb));
                    cTail = lerp(cTail, tBody, Step(0.32, lum, eb));
                    cTail = lerp(cTail, tLight, Step(0.54, lum, eb));
                    cTail = lerp(cTail, tPale, Step(0.76, lum, eb));
                    // a stripe of the ball's own ice under the white middle, and thin drawn lines along it, broken
                    float stripe = (1.0 - Step(0.05, abs(yn + 0.36), 0.015)) * step(0.45, CelValue2(float2(P * 0.15 + 4.0, sd)));
                    cTail = lerp(cTail, body, 0.55 * stripe);
                    float strokes = 0.0;
                    float lw = 1.2 * pixY / max(hT, 1e-3);
                    [unroll] for (int m = 0; m < 5; m++)
                    {
                        float yl = m == 0 ? -0.90 : m == 1 ? -0.52 : m == 2 ? -0.27 : m == 3 ? 0.22 : 0.50;
                        float gate = step(0.38, CelValue2(float2(P * 0.20 + m * 7.3, sd + m)));
                        strokes = max(strokes, (1.0 - Step(lw, abs(yn - yl), 0.4 * lw)) * gate);
                    }
                    cTail = lerp(cTail, tInk, strokes * 0.45 * inky);

                    // -- the ball, in its own frame: forward +x, up the screen +y, in radii
                    float2 hp = float2(-X, Y) / max(Rh, 1e-3);
                    float rho = length(hp);
                    float phi = atan2(hp.y, hp.x);                      // 0 its nose
                    float pixR = pix / max(Rh, 1e-3);
                    // its round, with a fringe of short teeth swept back all round it but its nose
                    float back = smoothstep(0.55, 1.25, abs(phi));
                    float ps = phi - 0.9 * (rho - 1.0) * sign(phi);      // a tooth leans back as it goes out
                    float sawA = 1.0 - abs(frac(ps * 19.0 / 6.2831853 + seed) - 0.5) * 2.0;
                    float sawB = 1.0 - abs(frac(ps * 31.0 / 6.2831853 + seed * 5.0) - 0.5) * 2.0;
                    float fringe = back * (0.36 * sawA * sawA * (0.5 + CelHash21(float2(floor(ps * 19.0 / 6.2831853 + seed), 3.0))) + 0.16 * sawB * sawB) * (1.0 - 0.7 * lunge);   // (each tooth its own length)
                    float inBody = 1.0 + 0.04 * sin(phi * 5.0 + 1.0 + sd) + fringe - rho;   // radii inside its round
                    // its shards: the outer ones stand off its round, the inner ones lie over it
                    float inSp = -1.0, spTone = 2.0, spLine = 1.0, inIn = -1.0, inTone = 2.0, inLine = 1.0;
                    [unroll] for (int j = 0; j < SHARDS; j++)
                    {
                        float4 A = BallShards[j];
                        float3 B = BallShardFaces[j];
                        float boil = sin(tk * 1.7 + j * 2.3);           // in flight they thrash; held, they hardly stir
                        float rootA = radians(A.x), dirA = radians(A.y + 8.0 * boil * (1.0 - settled));
                        float2 root = float2(cos(rootA), sin(rootA)) * B.x, dv = float2(cos(dirA), sin(dirA));
                        float len = A.z * (j < OUTER_SHARDS ? 1.0 : 0.7) * (1.0 + (0.18 - 0.15 * settled) * boil) * (1.0 - 0.6 * lunge);
                        float2 rel = hp - root;
                        float al2 = dot(rel, dv), sc2 = rel.x * dv.y - rel.y * dv.x;   // sc2 > 0 its clockwise side
                        float uu = (al2 + 0.30) / (len + 0.30);         // it is rooted 0.3 radii further in
                        float wd = A.w * 0.62 * min(uu / 0.30, (1.0 - uu) / 0.70);
                        float f = wd - abs(sc2);
                        float tone = sc2 > 0.15 * wd ? B.z : B.y, ridge = abs(sc2 - 0.15 * wd);
                        if (j < OUTER_SHARDS) { if (f > inSp) { inSp = f; spTone = tone; spLine = ridge; } }
                        else if (f > inIn) { inIn = f; inTone = tone; inLine = ridge; }
                    }
                    // the soft white star it is as it goes in
                    float t9 = abs(frac(phi * 9.0 / 6.2831853 + seed) - 0.5) * 2.0, t5 = abs(frac(phi * 5.0 / 6.2831853 + seed * 3.0) - 0.5) * 2.0;
                    float inStar = 0.50 + 0.55 * t9 * t9 * t9 + 0.35 * t5 * t5 * t5 + 0.15 * abs(frac(phi * 13.0 / 6.2831853 + seed * 7.0) - 0.5) * 2.0 - rho;   // thin sharp rays
                    float inDrawn = max(max(inBody, inSp), inIn);
                    float inHead = lerp(inDrawn, inStar, star);
                    float edge = lerp(pixR, 0.18, max(star, 0.4 * blur));
                    float aHead = saturate(inHead / edge + 0.5 * (1.0 - star));
                    // its ice: froth turning round a dark eye, brightest in a ring round it, pale above and ahead
                    float sw = phi + 2.6 * rho - tk * 0.9;              // a spiral, turning
                    float fro = CelValue2(float2(sw * 1.6, rho * 4.0 + sd)), fro2 = CelValue2(hp * 5.0 + float2(sd, tk * 0.3));
                    float cr1 = min(0.62 - length(hp - float2(0.10, 0.08)), length(hp - float2(-0.14, 0.18)) - 0.56);   // > 0 in the dark crescent inside its nose
                    float ring = (rho - 0.62) / 0.26;
                    float glare = 0.47 + 0.06 * exp(-ring * ring) + 0.18 * hp.y + 0.16 * hp.x
                        + 0.56 * (fro - 0.5) + 0.24 * (fro2 - 0.5) - 0.16 * smoothstep(-0.03, 0.06, cr1) - 0.20 * smoothstep(1.0, 1.3, rho) - 0.14 * (1.0 - settled);   // (its teeth a tone down)
                    half3 cHead = rim;
                    cHead = lerp(cHead, shade, smoothstep(0.08, 0.16, glare));
                    cHead = lerp(cHead, mid, smoothstep(0.24, 0.32, glare));
                    cHead = lerp(cHead, body, smoothstep(0.38, 0.46, glare));
                    cHead = lerp(cHead, light, smoothstep(0.56, 0.64, glare));
                    cHead = lerp(cHead, pale, smoothstep(0.76, 0.84, glare));
                    float sl = abs(frac(sw * 0.45 + 0.35 * fro2) - 0.5);
                    float stroke = (1.0 - saturate(sl / (1.6 * pixR))) * step(0.45, CelValue2(float2(phi * 2.0 + sd, rho * 3.0))) * step(0.25, rho) * step(rho, 0.95);
                    cHead = lerp(cHead, ink, stroke * 0.55 * inky);
                    cHead = lerp(cHead, pale, Step(0.87, rho, 0.015) * (1.0 - smoothstep(0.75, 1.15, abs(phi))));   // the bright rim on its nose
                    // a shard: two faces either side of its ridge, a line round it
                    half3 cSp = spTone < 2.5 ? body : (spTone < 3.5 ? light : (spTone < 4.5 ? pale : mid));
                    cSp = lerp(cSp, ink, (1.0 - saturate(spLine / (1.1 * pixR))) * 0.6 * inky);
                    cHead = lerp(cHead, cSp, step(inBody, inSp));
                    half3 cIn = inTone < 2.5 ? body : (inTone < 3.5 ? light : (inTone < 4.5 ? pale : mid));
                    cIn = lerp(cIn, ink, (1.0 - saturate(min(inIn, inLine) / (1.2 * pixR))) * 0.7 * inky);
                    cHead = lerp(cHead, cIn, saturate(inIn / pixR + 0.5));
                    cHead = lerp(cHead, ink, (1.0 - saturate(inDrawn / (1.5 * pixR))) * 0.8 * inky);   // the line round it
                    cHead = lerp(cHead, pale, 0.35 * blur);
                    cHead = lerp(cHead, lerp(pale, lerp(light, body, saturate((rho - 0.6) / 0.5)), saturate((rho - 0.25) / 0.35)), star);
                    // thin arcs ahead of its nose
                    float aArc = 0.0;
                    [unroll] for (int n = 0; n < 3; n++)
                    {
                        float rr = n == 0 ? 1.22 : (n == 1 ? 1.30 : 1.42);
                        float pc = n == 0 ? 0.87 : (n == 1 ? -0.87 : 0.61);     // radians round from its nose
                        float da = abs(phi - pc) / (n == 2 ? 0.40 : 0.56);      // 0 its middle .. 1 its ends
                        float th = 0.040 * (1.0 - da * da);
                        aArc = max(aArc, saturate((th - abs(rho - rr - 0.10 * da * da)) / pixR + 0.5) * step(da, 1.0));
                    }
                    aArc *= inky * 0.85;

                    // the tail, the arcs over it, the ball over both
                    float na = aTail + aArc * (1.0 - aTail);
                    col = (cTail * aTail + pale * aArc * (1.0 - aTail)) / max(na, 1e-4);
                    al = aHead + na * (1.0 - aHead);
                    col = (cHead * aHead + col * na * (1.0 - aHead)) / max(al, 1e-4);
                    // the cold glow round the ball
                    float halo = exp(-pow(max(rho - 0.85, 0.0) / 0.60, 2.0));
                    extra = lerp(body, pale, 0.35) * (0.30 + 0.15 * star) * halo * (1.0 - al);
                }
                else if (mode < 1.5)
                {
                    float core = 0.0, edgeInk = 0.0;
                    if (i.shape.y > 2.5)
                    {
                        float wd = 0.9 * min((q.x + 1.0) / 0.7, (1.0 - q.x) / 1.3);   // a shard: a kite, its long point ahead
                        float e1 = wd - abs(q.y);
                        al = saturate(e1 / pixY + 0.5);
                        core = step(0.15 * wd, q.y);
                        edgeInk = 1.0 - saturate(min(e1, abs(q.y - 0.15 * wd)) / (1.4 * pixY));
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
                        float xm = q.x * aspect, hc = aspect - 1.0;     // the head's centre; the point trails behind
                        float wd = xm >= hc ? sqrt(saturate(1.0 - (xm - hc) * (xm - hc)))
                            : pow(saturate((xm + aspect) / max(2.0 * aspect - 1.0, 1e-3)), 0.8);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        core = (1.0 - Step(wd * 0.42, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    col = lerp(body, light, core);
                    col = lerp(col, ink, edgeInk * 0.85);
                    al *= 1.0 - smoothstep(0.9, 1.0, k);
                }
                else if (mode < 2.5)
                {
                    // ---- the crystal. q metres from its heart; Fb the frames since it was born
                    float tk = i.shape.x, a = i.shape.y, breakAt = i.shape.w;
                    float Fb = a * 24.0 - 1.0;                          // frames since the clip's frame 41.5; it is born a frame before, small
                    float grow = Fb < 0.5 ? 0.28 + 0.27 * saturate(Fb + 0.5) : 1.0 - 0.45 * exp(-(Fb - 0.5) / 0.75);  // 0.55 of its reach on its first frame, all of it on its third
                    float hot = exp(-max(Fb - 0.5, 0.0) / 2.2);         // 1 as it bursts .. 0: how far out its spikes are white
                    float sd = seed * 23.0;
                    half3 mid = lerp(shade, body, 0.5);
                    half3 cCol = body;
                    float cAl = 0.0;
                    [loop] for (int j = 0; j < SPIKES; j++)
                    {
                        float4 S = Spikes[j], Fa = SpikeFaces[j];
                        float h1 = CelHash21(float2(j + 1.0, sd + 3.0)), h2 = CelHash21(float2(j + 19.0, sd + 5.0));
                        float an = radians(S.x);
                        float2 d = float2(cos(an), sin(an));
                        float Lg = S.y * grow;
                        float along = dot(q, d), sc = q.x * d.y - q.y * d.x;   // sc > 0 its clockwise side
                        float uu = along / max(Lg, 1e-3);
                        float prof = min(0.55 + 0.45 * uu / S.w, (1.0 - uu) / (1.0 - S.w));   // wide where it leaves the heart, widest at S.w, a long point
                        float wCW = S.z * 1.12 * (1.0 + Fa.z) * prof, wCC = S.z * 1.12 * (1.0 - Fa.z) * prof;
                        float inS = sc >= 0.0 ? wCW - sc : wCC + sc;    // metres inside the spike
                        float rs = lerp(-wCC, wCW, Fa.y * 0.5 + 0.5);   // its ridge
                        // it breaks up: tip first, pieces dropping out between slanted cracks (the ridge splits each piece)
                        float tb = breakAt + Fa.w;
                        float pr = saturate((Fb - tb) / 7.0);
                        float cc = along / 0.9 + h2 + 2.2 * (h1 - 0.5) * sc / max(S.z, 1e-3) + 0.25 * sin(along * 3.1 + j);
                        float hc = CelHash21(float2(floor(cc) + 17.0 * step(rs, sc), j * 7.0 + sd));
                        float crack = 0.5 - abs(frac(cc) - 0.5);        // 0 on a cut .. 0.5 the middle of a piece
                        float held = step(pr * 1.2, hc + 0.10) * step(0.22 * pr, crack) * step(along, Lg * (1.0 - 0.85 * pr) + 0.4);
                        float cov = saturate(inS / pix + 0.5) * step(0.0, along) * lerp(1.0, held, step(0.001, pr));
                        float tcw = floor(Fa.x / 8.0 + 0.01), tcc = Fa.x - 8.0 * tcw;
                        float tn = sc >= rs ? tcw : tcc;
                        half3 sCol = tn < 0.5 ? rim : (tn < 1.5 ? shade : (tn < 2.5 ? body : (tn < 3.5 ? light : (tn < 4.5 ? pale : mid))));
                        // a wide blade has a darker strip down its clockwise edge and a pale one beside its ridge
                        float wide = step(0.24, S.z);
                        float dkAt = rs + 0.60 * (wCW - rs), dkOn = wide * step(0.25, h1);
                        sCol = lerp(sCol, shade, 0.45 * dkOn * step(dkAt, sc));
                        sCol = lerp(sCol, light, 0.75 * wide * step(0.35, h2) * step(rs - 0.16 * (rs + wCC), sc) * step(sc, rs));
                        // white at the heart: a hard tooth of it up each blade, long as it bursts
                        float wh = 0.45 + 0.7 * h2 + 1.0 * hot;
                        float white = 1.0 - smoothstep(wh - 0.12, wh + 0.12, along + 0.5 * abs(sc));
                        sCol = lerp(sCol, pale, white);
                        // the drawn line: brushed round it (heaviest on the edges that face up and left), thin along
                        // its ridge and its strip, and across it where it has cracked
                        float cutOn = step(0.45, CelHash21(float2(floor(cc + 0.5), j * 3.0 + sd)));   // the cuts that show before it goes
                        float cracked = saturate(Fb - (tb - 1.5)) * lerp(cutOn, 1.0, step(0.001, pr));
                        float up = saturate(dot(sc >= 0.0 ? float2(d.y, -d.x) : float2(-d.y, d.x), float2(-0.6, 0.8)));
                        float wInk = (2.4 + 3.6 * up) * (0.65 + 0.7 * CelValue2(float2(along * 1.9 + j * 5.0, sd))) * pix;
                        float lineIn = min(min(inS / wInk, abs(sc - rs) / (1.8 * pix)), lerp(1e3, crack * 2.0 / (3.2 * pix), cracked));
                        lineIn = min(lineIn, lerp(1e3, abs(sc - dkAt) / (1.5 * pix), dkOn));
                        sCol = lerp(sCol, ink, (1.0 - smoothstep(0.55, 1.0, lineIn)) * (1.0 - 0.8 * white));
                        cCol = lerp(cCol, sCol, cov);
                        cAl = max(cAl, cov);
                    }
                    // the soft white streaks thrown out past them to the upper right
                    float needleOn = saturate((Fb - 1.5) / 1.0) * (1.0 - saturate((Fb - 6.5) / 4.0));
                    [unroll] for (int n = 0; n < 3; n++)
                    {
                        float an = radians(Needles[n].x);
                        float2 d = float2(cos(an), sin(an));
                        float along = dot(q, d), sc = abs(q.x * d.y - q.y * d.x);
                        float tip = (3.0 + 0.72 * Fb) * (1.0 - 0.10 * n), tailEnd = 1.5 + 0.35 * Fb;
                        float wd = Needles[n].y * 0.6 * saturate((tip - along) / (0.6 * (tip - tailEnd) + 1e-3)) * saturate((along - tailEnd) / 0.6);
                        float cov = saturate((wd - sc) / (3.0 * pix) + 0.5) * step(along, tip) * step(tailEnd, along) * needleOn * 0.6;
                        cCol = lerp(cCol, lerp(light, pale, 0.5), cov * (1.0 - cAl));
                        cAl = max(cAl, cov);
                    }
                    // its heart: white-hot, a ragged star of short teeth; the mist takes it
                    float ang = atan2(q.y, q.x);
                    float t1 = abs(frac(ang * 11.0 / 6.2831853 + seed) - 0.5) * 2.0, t2 = abs(frac(ang * 17.0 / 6.2831853 + seed * 3.0) - 0.5) * 2.0;
                    float rag = 0.80 + 0.35 * pow(t1, 2.5) + 0.20 * pow(t2, 2.5);
                    float rc = 0.60 + 0.45 * exp(-max(Fb - 0.5, 0.0) / 1.6);
                    float rr = length(q * float2(1.0, 1.08));
                    float heartOn = 1.0 - saturate((Fb - 9.0) / 6.0);
                    float heartCov = saturate((rc * rag - rr) / pix + 0.5) * heartOn;
                    half3 heartCol = lerp(pale, light, smoothstep(0.55, 1.0, rr / max(rc * rag, 1e-3)) * 0.6);
                    cCol = lerp(cCol, heartCol, heartCov);
                    cAl = max(cAl, heartCov);
                    // the mist takes it from its heart out: what is inside is given up to the cloud behind it
                    // (from a little under and left of it: the cloud gathers at its foot first)
                    float rIn = 2.0 * saturate((Fb - 5.0) / 9.0) + 2.0 * saturate((Fb - 14.0) / 8.0);
                    float take = 1.0 - smoothstep(rIn - 0.5, rIn + 0.5, length((q + float2(0.2, 0.45)) * float2(0.75, 1.3)) + 0.8 * (CelValue2(q * 2.3 + float2(sd, 0.0)) - 0.5));
                    cAl *= 1.0 - 0.72 * take * saturate((Fb - 5.0) / 3.0);
                    col = cCol;
                    al = cAl;
                    // the flash as it bursts, and the glow its heart keeps
                    extra = pale * (0.22 * hot + 0.04 * heartOn) * exp(-dot(q, q) / (1.6 * 1.6)) * (1.0 - al);
                }
                else if (mode < 3.5)
                {
                    // ---- the mist. p metres from the crystal's heart; Fc the frames since it was born. One billowing
                    // cloud: its lumps are two layers of scattered points, bright at its heart, grey where it is thin.
                    float halfW = i.shape.x, a = i.shape.y;
                    float Fc = a * 24.0;
                    float2 p = q * halfW;
                    float grow = 1.0 - exp(-Fc / 4.5);
                    float thin = saturate((Fc - 22.0) / 30.0);          // 0 full .. 1 thin and sinking
                    float late = saturate((Fc - 14.0) / 20.0);          // its shadows go grey first
                    float lateHi = saturate((Fc - 20.0) / 28.0);        // its lights after
                    half3 t0 = lerp(rim, CelPal(rowB, 0), late), t1 = lerp(shade, CelPal(rowB, 1), late);
                    half3 t2 = lerp(body, CelPal(rowB, 2), late), t3 = lerp(light, CelPal(rowB, 3), lateHi);
                    half3 t4 = lerp(pale, CelPalCore(rowB).rgb, lateHi);
                    // the clip's cloud: its foot stays on the ground, its top climbs and then sinks, it drifts right over the target
                    float Rx = (0.9 + 3.1 * grow) * (1.0 - 0.06 * thin);
                    float yb = -1.3 - 1.0 * saturate(Fc / 8.0);
                    float yt = 0.9 + 2.4 * smoothstep(4.0, 20.0, Fc) - 1.8 * smoothstep(28.0, 58.0, Fc);
                    float2 c = float2(-0.1 + 0.8 * smoothstep(8.0, 34.0, Fc) - 0.2 * thin, yb + (yt - yb) * 0.42);
                    float2 e = float2((p.x - c.x) / Rx, p.y > c.y ? (p.y - c.y) / (yt - c.y) : (p.y - c.y) / (c.y - yb));
                    float E = 1.0 - length(e) + 0.30 * (CelValue2(p * 0.45 + float2(seed * 11.0, Fc * 0.01)) - 0.5);   // 1 its middle .. 0 its rim, in lobes
                    float sc = 0.70 + 0.30 * grow;                      // its lumps swell as it grows
                    float sd = seed * 37.0;
                    float2 wv = float2(CelValue2(p * 0.8 + float2(sd, 3.0)), CelValue2(p * 0.8 + float2(-7.0, sd))) - 0.5;
                    float2 u = (p - c) / sc + 0.55 * wv;
                    float2 wa, wb;
                    float w1 = IceWorley(u / 1.25 + float2(sd, -sd), Fc * 0.05, 0.0, wa);
                    float w2 = IceWorley(u / 0.55 + float2(-sd, sd), Fc * 0.08, 5.0, wb);
                    float b1 = 1.0 - smoothstep(0.0, 0.85, w1), b2 = 1.0 - smoothstep(0.0, 0.85, w2);
                    float relief = 0.60 * b1 + 0.40 * b2;               // 1 a lump's top .. 0 the crease between two
                    float erode = 0.50 * smoothstep(40.0, 84.0, Fc);    // it thins: its creases open and it comes apart
                    float dens = 1.5 * E + (0.75 * b1 + 0.25 * b2 - 0.5) - erode;
                    float am = smoothstep(0.0, 0.32, dens) * saturate(Fc / 2.0) * (0.94 - 0.34 * thin);
                    // the target shows through it, then stands clear of it (the target: what stands within reach of the heart)
                    float3 off = spot - i.shape3.xyz;
                    float standing = step(length(off.xz), 1.3) * step(-1.0, off.y);
                    am *= 1.0 - standing * smoothstep(18.0, 34.0, Fc);
                    float core = smoothstep(0.05, 0.75, E) * (1.0 - 0.55 * lateHi);
                    float lit = -dot(wa, float2(-0.53, 0.85)) * 0.6 - dot(wb, float2(-0.53, 0.85)) * 0.4;   // lit from above left
                    float fine = CelValue2(p * 6.0 + float2(sd, -Fc * 0.03));
                    float lum = 0.16 + 0.36 * relief + 0.52 * core + 0.10 * lit + 0.10 * (fine - 0.5)
                        - 0.18 * (1.0 - smoothstep(0.0, 0.55, dens));   // where it is thin it is grey
                    col = lerp(t0, t1, smoothstep(0.17, 0.43, lum));
                    col = lerp(col, t2, smoothstep(0.35, 0.61, lum));
                    col = lerp(col, t3, smoothstep(0.55, 0.81, lum));
                    col = lerp(col, t4, smoothstep(0.79, 1.05, lum));
                    al = am * (1.0 - smoothstep(57.0, 76.0, Fc));
                }
                else if (mode < 4.5)
                {
                    // ---- the swirl on the ground, on the level plane through its foot. Fr the frames since it was born
                    float3 cam = _WorldSpaceCameraPos;
                    float3 ray = spot - cam;
                    float3 hit = cam + ray * ((i.shape3.y - cam.y) / min(ray.y, -1e-3));   // where the eye's line meets that plane
                    float3 d = hit - i.shape3.xyz;
                    float across = d.x * i.flow.x + d.z * i.flow.y, back = d.z * i.flow.x - d.x * i.flow.y;
                    float lvl = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                    float onGround = (1.0 - smoothstep(0.6, 1.0, abs(spot.y - i.shape3.y) / i.flow.w)) * step(ray.y, -1e-3);
                    float Fr = i.shape.y * 24.0;
                    float rho = length(float2(across, back));
                    float turns = atan2(back, across) / 6.2831853;
                    float soft = max(fwidth(rho), 0.12);                // a brushed edge
                    float fore = clamp(fwidth(rho) / max(length(ddx(hit)), 1e-4), 1.0, 4.0);   // how much the eye foreshortens a strand's width here
                    float fat = lerp(1.0, fore, 0.3);                   // the clip draws its ring nearly as thick in front as at its sides
                    float Ro = 3.5 + 0.9 * min(Fr, 4.5) + 1.6 * (1.0 - exp(-max(Fr - 4.5, 0.0) / 2.2));   // its reach (m): the clip's frames 42-50
                    float open = saturate((Fr - 1.5) / 2.5);            // 0 one ring .. 1 strands with ground between
                    float wT = 0.85 + 1.5 * exp(-max(Fr - 1.0, 0.0) / 5.0);   // how thick a strand is (m)
                    float m = 0.0, lumS = 0.0;
                    [unroll] for (int k = 0; k < 5; k++)
                    {
                        float4 A = SwirlArcs[k];
                        float u = frac(turns - A.x - 0.012 * Fr) / A.y;     // 0 its start .. 1 its end
                        float fr = lerp(A.z, A.w, u);
                        fr = lerp(0.80 + 0.20 * (fr - 0.54) / 0.46, fr, open);   // packed into one ring at first
                        float lump = 0.75 + 0.5 * CelValue2(float2(u * 6.0 + k * 3.0, seed * 9.0));
                        float hk = 0.5 * wT * SwirlThick[k] * lump * fat * pow(sin(3.14159265 * saturate(u)), 0.7);   // a point at both ends
                        float rk = min(fr * Ro, Ro - 0.9 * hk);
                        float c = smoothstep(-1.0, 1.0, (hk - abs(rho - rk)) / soft) * step(u, 1.0);
                        float br = CelValue2(float2(u * A.y * 22.0 + k * 7.0, (rho - rk) * 2.5 + seed * 5.0 + k * 3.0));   // brushed along it
                        float lk = 0.46 + 0.30 * (rho - rk) / max(hk, 1e-3) + 0.55 * (br - 0.5);
                        if (c > m) { m = c; lumS = lk; }
                    }
                    // its first frame: a whole pale disc
                    float disc = (1.0 - saturate(Fr - 0.5)) * (1.0 - smoothstep(Ro - soft, Ro + soft, rho));
                    float ld = 0.72 + 0.25 * (CelValue2(float2(across, back) * 0.8) - 0.5) - 0.45 * smoothstep(0.8, 1.0, rho / Ro);
                    if (disc > m) { m = disc; lumS = ld; }
                    lumS -= 0.15 * saturate((Fr - 5.0) / 5.0);          // greyer as it runs out
                    col = rim;
                    col = lerp(col, shade, smoothstep(0.10, 0.14, lumS));
                    col = lerp(col, body, smoothstep(0.28, 0.32, lumS));
                    col = lerp(col, light, smoothstep(0.70, 0.74, lumS));
                    al = m * lvl * onGround * 0.95 * (1.0 - saturate((Fr - 6.5) / 6.5));
                }
                else if (mode < 5.5)
                {
                    float2 g = float2(q.x * 0.5 + 0.5, 0.5 - q.y * 0.5);
                    if (i.shape.y > 0.5)
                    {
                        // ---- the chart of the palette: 8 rows from this one down, c0 c1 c2 c3 pale ink across
                        float cx = min(floor(g.x * 6.0), 5.0), cy = min(floor(g.y * 8.0), 7.0);
                        uint pr8 = row + (uint)cy;
                        half3 pc = cx < 3.5 ? CelPal(pr8, (uint)cx) : (cx < 4.5 ? CelPalCore(pr8).rgb : CelPalInk(pr8).rgb);
                        return half4(Unvignette(pc, i.positionCS.xy, 1.0), 1.0);
                    }
                    // ---- the chart: one block of 7 x 7 flat colours every three frames, written as they are
                    float2 ci = min(floor(g * 7.0), 6.0);
                    float bi = fmod(floor(i.shape.x / 3.0), 8.0);
                    float3 lv = float3(ci.x, ci.y, bi);
                    float3 c = float3(lv.x < 4.5 ? lv.x * 48.0 : (lv.x < 5.5 ? 224.0 : 255.0),
                                      lv.y < 4.5 ? lv.y * 48.0 : (lv.y < 5.5 ? 224.0 : 255.0),
                                      lv.z < 4.5 ? lv.z * 48.0 : (lv.z < 5.5 ? 224.0 : 255.0));
                    if (bi > 6.5) c = (ci.x + ci.y * 7.0) * 255.0 / 48.0;
                    return half4(Unvignette(c / 255.0, i.positionCS.xy, 1.0), 1.0);
                }
                else
                {
                    // a light, after the light pass has multiplied it in: the colour it adds on top (lifted against
                    // the lens only a third of the way: the clip's light falls off into the corners of the frame too)
                    float f = GlowOn(i, spot, facing);
                    return half4(Unvignette(i.shape2.rgb * f * GlowSeen(spot), i.positionCS.xy, 0.35), 0.0);
                }
                al *= i.tone.z;
                col = Unvignette(col, i.positionCS.xy, 1.0);
                return half4(col * al + Unvignette(extra, i.positionCS.xy, 0.35) * i.tone.z, al);
            }
        ENDHLSL
        Pass
        {
            Name "CelIceBallLight"
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
            Name "CelIceBall"
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
