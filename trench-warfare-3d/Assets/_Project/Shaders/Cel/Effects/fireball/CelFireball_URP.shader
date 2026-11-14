// Phase: CelFx fireball (2026-11-14) - one GPU pool for the whole acid jet (reference: fireball_nolora): the
// strands of the jet, the drops it sheds, the splat where it lands, the mound of goo over the target, the puddle on
// the ground, the bubbles that swell and pop on it, the green smoke it leaves and the light it throws on the ground.
// Every shape is a field in the fragment; nothing is a texture (the light reads the camera's depth). The clip is
// drawn on twos, so every shape here is held for two frames of a 24 fps film: the age a record is drawn at is
// stepped 12 times a second on the EFFECT's clock (Extra.w carries the seconds from the effect's start to the
// record's birth, so every record of one effect steps on the same frames).
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 strand   one ribbon of the jet. Pos the nozzle, Vel the launch velocity, Motion.x gravity (m/s2): the fluid that
//             left s seconds ago is at Pos + Vel s - g s2 / 2. Life = seconds the jet is on + the flight.
//             Size x half-width at the nozzle (m), y at the target, z the half-width it holds through the middle
//             of the flight (it opens to that at once, and on to y over the last of the way), w the flight seconds.
//             Axis x, y the strand's place across the jet (-1..1: up the screen, toward the eye), z its weave phase.
//             Erode x the band's half-width at the nozzle (m), y at the target (a place of 1 is its edge), z dash
//             duty 0..1, w dash seconds.
//             Extra y seconds the jet is on, z what it is: 0 a lane (a ribbon broken into long dashes, inside the
//             band until it nears the target), 1 a lane with light streaks, 2 the band: one sheet of five lanes
//             with long lens-shaped tears between its ribbons (a fifth of it where it leaves, half of it where it
//             lands), thinner in its first quarter-second, breaking into leaves over its last half-second
//             (Erode z is unused, w the seconds one thin slit takes).
//             Motion y the head's taper (seconds of fluid), z the tail's, w metres it is lifted before it comes
//             down (the clip's arc is fuller than a parabola over its last third).
//  1 drop     a ballistic drop. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at death,
//             w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf (a point
//             at both ends).
//  2 splat    the first hit: a star of spikes facing the eye. Size x its half-extent (m). Extra y metres toward the eye.
//  3 mound    the goo standing over the target (a pillar pushed over the way the jet goes, thick tongues at its
//             top), and the last splash at its foot. Pos its foot. Size x width (m), w height / width. Axis the
//             jet's level direction. Motion x seconds it takes to stand up, y the share of its height it starts
//             at, z metres it is drawn toward the eye (it stands on the target's foot and hides the target).
//             Extra y its heat (0 the heap: it keeps its height and thins to hanging strands as it goes, its
//             middle first; 1 the last splash: a broad flame torn as it comes up, hot at its heart), z seconds
//             before its end at which it starts to go.
//  4 puddle   a splat lying on the ground: painted on whatever level ground the scene shows behind its card (found
//             from the scene's depth, like the glow), so uneven ground cannot cut it and nothing that stands in
//             it is painted. Pos its middle, Axis the eye's level right. Size x half-extent across (m), w depth /
//             width. Motion x, y the ground's slope there, z the side its longest fingers run to (1 the eye's
//             right, -1 its left). Extra y seconds it takes to spread, z the age at which it drains.
//  5 lobe     a07: one lobe of the burst. A round cel ball facing the eye, hard-cut, with a lumpy edge, the
//             palette's bands off a highlight high and to the eye's left (core, c3, c2, c1 at the edge), an ink
//             contour, and a cooling into the palette's second row. Pos where it sits, Vel its drift, Motion
//             (gravity, drag, -, buoyancy). Size x radius at birth (m), y at death, z the ease, w its squash
//             (0 or 1 round, 0.8 a little flattened). Tone y the core's HDR multiplier, w the ink's width in
//             screen pixels (0 no contour). Extra y the lumps' amplitude (0: 0.10), z the life fraction at which
//             it starts to char into the cool row (0: 0.75). It replaced the acid's bubble, which the fireball
//             never spawned; a lobe is the one thing the drop and the puff modes cannot draw.
//  6 puff     smoke: no line round it, a ragged edge, seen through; it frays into upright tatters. Pos, Vel,
//             Motion (gravity, drag, -, buoyancy). Size x, y, z its size over the life (m). Extra y the life
//             fraction at which it starts to thin, z the share of its life it is lit by the acid under it (core,
//             c3, then its own c2, and c1 as it ages; from 1 up it stays light all its life, its underside c2).
//  7 glow     the acid's light on whatever stands behind it (the ground, the target, a crate): the lit point is
//             found from the scene's depth, so the card itself is never hidden. Pos the light (a metre over the
//             ground), Axis a level axis of its own (the recipe: square to the jet's track). Size x its reach
//             along that axis (m), y up and down, w its reach square to it on the near side (against
//             cross(Axis, up): back toward the nozzle) over x, z its reach on the far side over that.
//             Vel how much of what it lights it multiplies into it at its heart (rgb, 0..1: the ground keeps its
//             own pattern under the light), Erode xyz the colour it adds on top of that (as written, before the
//             lens). Extra y seconds it takes to come up, z seconds it takes to go at the end of its life.
//             Needs the camera's depth texture.
// Tone: x the palette row, y unused (a glow: 1 if it falls on level ground only and leaves what stands on it
// alone), z opacity (a glow: its strength), w unused (a strand: the band's half-width through the middle of the
// flight, m: the strands keep their places in the band).
// Palette rows: c0 rim, c1 shade, c2 body, c3 light, core pale. Mode 5 reads them through the kit's CelRamp2
// instead, which cuts a 0..1 field into the row's bands (the lightest n of c0..c3) and its HDR core, and blends the
// whole ramp into the row the record cools to (Tone.x = row + 256 x cool row), so one record can go fire to char.
//
// Two passes, one draw: the light pass first (only the glows draw in it: lit = unlit x (1 + what a glow multiplies
// in), which a blend can do and an added colour cannot), then everything, in the records' order.
//
// _TWAcidLens (global; FireballRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0 none.
// The vignette takes most of the light from the corners of the frame, and the clip's acid is as bright there as in
// the middle: every colour is lifted by what the vignette will take from it, as far as white allows (its brightest
// channel stops there, the others go on a fifth further: it keeps its hue and pales only a little).
Shader "TW/Cel/Fireball"
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
                nointerpolation float4 state : TEXCOORD4;   // mode, age (held on twos), life fraction, seed
                nointerpolation float4 shape : TEXCOORD5;   // per mode
                nointerpolation float4 shape2 : TEXCOORD6;  // per mode
                float fog : TEXCOORD7;
                nointerpolation float4 shape3 : TEXCOORD8;  // per mode
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), alive = CelAlive(r);
                float mode = r.extra.x, delay = r.extra.w;
                float tick = floor((t + delay) * 12.0 + 0.01);          // the drawing: a new one every two frames
                float a = max(tick / 12.0 - delay, 0.0);                // the record's age at that drawing
                float k = saturate(a / max(r.life, 1e-4));
                float3 vr = UNITY_MATRIX_V[0].xyz, vu = UNITY_MATRIX_V[1].xyz;
                float2 cq = v.uv * 2.0 - 1.0;
                float3 w = r.pos;
                float2 q = cq;
                float4 flow = 0.0, shape = 0.0, shape2 = 0.0, shape3 = 0.0;
                float wholeFrame = 0.0;
                if (mode < 0.5)
                {
                    float T = max(r.size.w, 0.01), D = r.extra.y;
                    float sHead = min(a, T), sTail = clamp(a - D, 0.0, T);
                    float s = lerp(sTail, sHead, v.uv.x);
                    float e = a - s;                                    // when this fluid left the nozzle
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float3 c = r.pos + r.vel * s + 0.5 * G * s * s;
                    float3 tangent = normalize(r.vel + G * s);
                    float3 view = normalize(_WorldSpaceCameraPos - c);
                    float3 side = normalize(cross(tangent, view));
                    side *= side.y >= 0.0 ? 1.0 : -1.0;                 // up the screen
                    float3 nrm = normalize(cross(side, tangent));
                    float kS = s / T;
                    float starve = smoothstep(D - 0.55, D - 0.05, e);   // the jet starves before it is shut off:
                    float last = r.extra.z > 1.5 ? 1.0 + 0.7 * starve : 1.0 - 0.3 * starve;   // the band opens to a wide fan of ribbons, its lanes thin
                    float land = 1.0 - (r.extra.z > 1.5 ? 0.45 : 0.85) * smoothstep(0.90, 1.0, kS);   // it runs to a point where it lands
                    float head = smoothstep(0.05, 0.45, e);            // the first of the jet is one tight stream: its lanes inside it
                    float first = lerp(r.extra.z > 1.5 ? 0.85 : 0.75, 1.0, head);
                    float early = smoothstep(0.0, 0.14, kS), late = pow(saturate((kS - 0.4) / 0.6), 1.6);   // it opens at once, holds that width, and opens again as it comes down
                    float hw = (lerp(r.size.x, r.size.z, early) + (r.size.y - r.size.z) * late) * last * land * first;
                    float spread = (lerp(r.erode.x, r.tone.w, early) + (r.erode.y - r.tone.w) * late) * lerp(0.45, 1.0, head);   // the lanes keep their places in the band
                    float2 place = r.axis.xy + 0.22 * float2(sin(e * 9.0 + r.axis.z), sin(e * 6.1 + r.axis.z * 1.7));
                    place.x *= 1.0 + 0.8 * starve;                      // and the lanes stray further from it
                    c += (side * place.x + nrm * place.y * 0.6) * spread;
                    c += side * (r.motion.w * smoothstep(0.42, 0.72, kS) * (1.0 - smoothstep(0.86, 1.0, kS)));
                    w = c + side * cq.y * hw;
                    q = float2(cq.y, v.uv.x);
                    flow = float4(e, kS, 0.0, 0.0);
                    shape = float4(D, r.erode.z, max(r.erode.w, 0.02), r.extra.z);
                    shape2 = float4(max(r.motion.y, 0.004), max(r.motion.z, 0.004), 0.0, 0.0);
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
                    float left = max(r.life - a, 0.0);
                    float rise = saturate(r.motion.y + a / max(r.motion.x, 0.01) * (1.0 - r.motion.y));
                    float sink = saturate(left / max(r.extra.z, 0.01));
                    w = r.pos + vr * cq.x * r.size.x * 0.5 + float3(0.0, 1.0, 0.0) * v.uv.y * r.size.x * r.size.w;
                    w += normalize(_WorldSpaceCameraPos - w) * r.motion.z;   // where it stands, but in front of the target
                    q = float2(cq.x, v.uv.y);
                    shape = float4(tick, rise, sink, r.extra.y);
                    shape2 = float4(dot(vr, r.axis), 2.0 * r.size.w, 0.0, 0.0);   // the way the jet pushes it, across the view; the card's height over its half-width
                }
                else if (mode < 4.5)
                {
                    // a card that hides the ball the puddle lies in: the fragment finds the ground behind it
                    float R = r.size.x * 1.1;
                    float dist = max(distance(_WorldSpaceCameraPos, r.pos), 0.01);
                    float cover = dist / sqrt(max(dist * dist - R * R, 0.04 * dist * dist));
                    w = CelBillboard(r.pos, cq * R * cover);
                    wholeFrame = step(dist, R * 1.02) * alive;
                    float3 right = normalize(float3(r.axis.x, 0.0, r.axis.z));
                    float grow = 0.35 + 0.65 * (1.0 - exp(-a / max(r.extra.y, 0.01) * 3.4));
                    grow *= 1.0 - 0.15 * smoothstep(0.55, 1.0, a / max(r.extra.z, 0.01));   // it ebbs a little before it drains
                    float drain = saturate((a - r.extra.z) / max(r.life - r.extra.z, 0.01));
                    flow = float4(right.x, right.z, max(r.size.w, 0.01), max(r.size.x, 0.01));
                    shape = float4(tick, grow, drain, r.motion.z);
                    shape2 = float4(r.motion.xy, 0.0, 0.0);
                    shape3 = float4(r.pos, 0.0);
                }
                else if (mode < 5.5)
                {
                    // a07: the burst's lobe. One round cel ball facing the eye, carried by its own velocity with
                    // drag, gravity and buoyancy like a puff. The card is wider than the lobe by Card: its lumps
                    // reach 1 + amp and the ink line is drawn just inside its edge, so neither may touch the card's.
                    float3 p = r.pos + r.vel * CelDragInt(r.motion.y, a) + float3(0.0, 0.5 * (r.motion.w - r.motion.x) * a * a, 0.0);
                    float rad = CelCurve(r.size.xyz, k);
                    float squash = r.size.w > 0.0 ? r.size.w : 1.0;     // 1 round, 0.8 a little flattened
                    const float Card = 1.22;
                    w = CelBillboard(p, cq * rad * Card * float2(1.0, squash));
                    q = cq * Card;                                      // |q| = 1 is the lobe's own edge
                    // shape.z the life fraction at which it begins to char, shape.w the one at which the char is
                    // COMPLETE (a12: a window, not a ramp that only finishes when the lobe dies)
                    shape = float4(tick, r.extra.y > 0.0 ? r.extra.y : 0.10, r.extra.z > 0.0 ? r.extra.z : 0.75,
                        r.erode.x > 0.0 ? r.erode.x : 1.0);
                    // a22: where this pixel is in the WHOLE CLOUD, so the fragment can band the burst in the
                    // cloud's frame instead of the lobe's. Axis is the lobe's offset from the burst's heart at
                    // birth, so axis + (p - pos) is its offset NOW, drift and all. Projected on the billboard's
                    // own basis (the same vr/vu CelBillboard builds its corners from) and divided by the cloud's
                    // half-extents, so the fragment has nothing left to do but add its own q.
                    // The card maps q to rad * (1, squash) metres from the lobe's middle (w = CelBillboard(p,
                    // cq * rad * Card * (1, squash)) with q = cq * Card), which is the zw pair below.
                    float3 coff = r.axis + (p - r.pos);
                    float2 cloudR = float2(max(r.erode.y, 0.01), max(r.erode.z, 0.01));
                    shape2 = float4(dot(coff, vr) / cloudR.x, dot(coff, vu) / cloudR.y,
                        rad / cloudR.x, rad * squash / cloudR.y);
                }
                else if (mode < 6.5)
                {
                    float3 p = r.pos + r.vel * CelDragInt(r.motion.y, a) + float3(0.0, 0.5 * (r.motion.w - r.motion.x) * a * a, 0.0);
                    p += vr * (0.22 * sin(delay * 1.7 + a * 2.3) * saturate(a * 1.2));   // the column sways as one
                    float size = CelCurve(r.size.xyz, k);
                    w = CelBillboard(p, cq * size);
                    float fray = saturate((k - r.extra.y) / max(1.0 - r.extra.y, 0.01));
                    shape = float4(tick, fray, max(r.extra.z, 0.06), 0.0);
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
                if (mode > 6.5 || abs(mode - 4.0) < 0.5)
                {
                    if (wholeFrame > 0.5) o.positionCS = float4(cq, 0.0, 1.0);
                    o.positionCS.z = o.positionCS.w * lerp(UNITY_NEAR_CLIP_VALUE, UNITY_RAW_FAR_CLIP_VALUE, 0.0002);   // nothing hides a light's card, or the puddle's
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

            // a step at edge, w wide (the derivative is taken once, outside every branch)
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

            // a splat: a ragged lump taller than it is wide, and up to sixteen spikes that run out of it to a point (a
            // third of them missing, most of them short, a few long), some throwing a small leaf off their tip
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

            // a puff of smoke: a heart, five lumps of every size set anywhere round it and seven small knobs on its
            // rim (some of them loose), its outline pulled about, rolling a little on every drawing; above 0 inside
            float Puff(float2 q, float seed, float tick)
            {
                q += 0.07 * (float2(CelValue2(q * 2.3 + seed * 17.0), CelValue2(q * 2.3 + seed * 29.0 + 7.0)) - 0.5);
                q += 0.07 * (float2(CelValue2(q * 6.1 + seed * 11.0 + tick * 0.13), CelValue2(q * 6.1 + seed * 23.0 + 3.0)) - 0.5);
                q += 0.035 * (float2(CelValue2(q * 13.0 + seed * 7.0), CelValue2(q * 13.0 + seed * 31.0 + 5.0 - tick * 0.17)) - 0.5);
                float f = 0.40 - length(q);
                [unroll] for (int j = 0; j < 5; j++)
                {
                    float h = CelHash21(float2(j + 1.0, seed * 63.0 + 1.0)), h2 = CelHash21(float2(j + 17.0, seed * 29.0 + 2.0));
                    float an = CelHash21(float2(j + 29.0, seed * 41.0 + 3.0)) * 6.2831853 + j * 0.9;
                    float2 c = float2(cos(an), sin(an)) * (0.16 + 0.30 * h);
                    c += 0.02 * float2(sin(tick * 1.3 + j), cos(tick * 1.7 + j * 2.0));
                    f = max(f, (0.20 + 0.22 * h2) - length(q - c));
                }
                [unroll] for (int n = 0; n < 7; n++)
                {
                    float h = CelHash21(float2(n + 41.0, seed * 57.0 + 5.0)), h2 = CelHash21(float2(n + 53.0, seed * 33.0 + 6.0));
                    float an = (n + h) * 0.8975979 + seed * 20.0;
                    float2 c = float2(cos(an), sin(an)) * (0.44 + 0.20 * h2);
                    c += 0.025 * float2(sin(tick * 1.9 + n * 1.3), cos(tick * 1.5 + n));
                    f = max(f, (0.07 + 0.08 * h) - length(q - c));
                }
                return f;
            }

            // thin lens-shaped streaks standing in a grid of cells (x across them, y along them), each column of
            // cells set at its own height: 1 inside a streak. share of the cells hold one; thick its half-width.
            float Streaks(float2 p, float seed, float share, float thick, float w)
            {
                p.y += CelHash21(float2(floor(p.x), seed * 19.0 + 4.0));
                float2 cell = floor(p), f = frac(p);
                float h = CelHash21(cell + seed * 3.0), h2 = CelHash21(cell + seed * 5.0 + 31.0);
                float lens = saturate(1.0 - abs(f.y * 2.0 - 1.0));
                float at = 0.3 + 0.4 * h2;
                return (1.0 - Step(thick * sqrt(lens), abs(f.x - at), w)) * step(1.0 - share, h) * step(0.03, lens);
            }

            // long tears in a sheet, in cells (x across it, y along it): each cell holds one, its own width (none
            // in one cell of seven), `open` of the cell at its widest, a point at both ends: 1 inside a tear
            float Tears(float2 p, float seed, float open, float w)
            {
                p.y += CelHash21(float2(floor(p.x), seed * 19.0 + 4.0));
                float2 cell = floor(p), f = frac(p);
                float h = CelHash21(cell + seed * 3.0), h2 = CelHash21(cell + seed * 5.0 + 31.0);
                float lens = saturate(1.0 - abs(f.y * 2.0 - 1.0));
                float halfWide = open * saturate(h * 2.0 - 0.3) * sqrt(lens);
                return (1.0 - Step(halfWide, abs(f.x - (0.35 + 0.3 * h2)), w)) * step(0.02, halfWide);
            }

            // a row of tongues, one to a cell of u, each with its own height and lean, new on every drawing: 0..1
            float Tongues(float u, float seed, float tk, float power)
            {
                float c = floor(u), f = u - c;
                float h = CelHash21(float2(c + seed * 31.0, tk * 1.9 + seed * 7.0));
                float at = 0.35 + 0.3 * CelHash21(float2(c + seed * 13.0, tk * 2.7 + 3.0));
                float t = f < at ? f / at : (1.0 - f) / (1.0 - at);
                return h * pow(saturate(t), power);
            }

            // fingers run out of a puddle's edge: `cells` of them round it (turn 0..1), `share` of the cells hold
            // one, lo..hi long, w0..w1 of a cell wide, with a round end (blunt 1) or a point (0)
            float Fingers(float turn, float cells, float seed, float tk, float lo, float hi, float share, float w0, float w1, float power, float blunt)
            {
                float af = turn * cells;
                float c0 = floor(af);
                float reach = 0.0;
                [unroll] for (int j = -1; j <= 1; j++)
                {
                    float cid = c0 + j;
                    float wid = cid - cells * floor(cid / cells);       // the same finger either side of the seam
                    float h = CelHash21(float2(wid + 1.0, seed * 71.0 + 4.0)), h2 = CelHash21(float2(wid + 23.0, seed * 37.0 + 6.0));
                    float h3 = CelHash21(float2(wid + 47.0, seed * 19.0 + 8.0));
                    float boil = CelHash21(float2(wid + 5.0, tk * 3.1 + seed)) - 0.5;
                    float da = abs(af - (cid + 0.5 + (h - 0.5) * 0.7)) / lerp(w0, w1, h2);
                    float amp = (lo + (hi - lo) * h3 * h3 + 0.015 * boil) * step(1.0 - share, h);
                    reach = max(reach, amp * pow(saturate(1.0 - lerp(da, da * da, blunt)), power));
                }
                return reach;
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
                float level = smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                return exp(-3.0 * t * t) * (1.0 - smoothstep(0.8, 1.0, t)) * i.shape.z * i.tone.z * lerp(1.0, level, i.tone.y);
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
                if (abs(mode - 4.0) < 0.5 || mode > 6.5) spot = SceneSpot(i);   // the puddle and the lights fall on what the scene shows behind this pixel
                if (abs(mode - 4.0) < 0.5)
                {
                    // the puddle's own coordinates on that ground
                    float3 off = spot - i.shape3.xyz;
                    q = float2(off.x * i.flow.x + off.z * i.flow.y, (off.z * i.flow.x - off.x * i.flow.y) / i.flow.z) / i.flow.w;
                }
                float3 facing = cross(ddy(spot), ddx(spot));            // which way it faces
                float2 fw = fwidth(q);
                float pix = max(fw.x, fw.y) * 0.75;
                float pixX = fw.x * 0.75, pixY = fw.y * 0.75;
                uint row = CelRowA(i.tone.x);
                half3 rim = CelPal(row, 0), shade = CelPal(row, 1), body = CelPal(row, 2), light = CelPal(row, 3);
                half3 pale = CelPalCore(row).rgb;
                half3 col = body;
                float al = 0.0;

                if (mode < 0.5)
                {
                    float e = i.flow.x, kS = i.flow.y;
                    float D = i.shape.x, duty = i.shape.y, period = i.shape.z, kind = i.shape.w;
                    float starve = smoothstep(D - 0.55, D - 0.05, e);   // the jet starves before it is shut off
                    if (kind > 1.5)
                    {
                        // the band: five wandering lanes side by side, each with its own head and tail
                        float u = (q.x * 0.5 + 0.5 + 0.035 * sin(e * 23.0 + seed * 40.0) + 0.02 * sin(q.x * 5.0 + e * 11.0)) * 5.0;
                        float lag = 0.02 * CelHash21(float2(floor(u) + 2.0, seed * 7.0));
                        float ends = pow(saturate((e - lag) / i.shape2.x), 0.65) * pow(saturate((D - lag - e) / i.shape2.y), 0.65);
                        float top = 1.0 - 0.20 * CelValue2(float2(e * 11.0 + seed * 31.0, 0.5));
                        float bottom = 1.0 - 0.26 * CelValue2(float2(e * 9.0 + seed * 17.0, 7.5));
                        al = 1.0 - Step(q.x > 0.0 ? top : bottom, abs(q.x), pixX);
                        al *= (1.0 - Step(ends * 1.2, abs(frac(u) - 0.5) * 2.0, pixX * 5.0)) * step(0.02, ends);
                        // long tears along it, three abreast: hardly any where it leaves (one sheet), broad ribbons with
                        // gaps as wide between them where it lands (not in its head, which is one sheet)
                        float open = lerp(0.03, 0.30, smoothstep(0.35, 0.85, kS)) * smoothstep(0.10, 0.50, e);
                        al *= 1.0 - Tears(float2(u * 0.6 + seed * 7.0, e / 0.13 + seed * 3.0), seed + 0.8, open, pixX * 1.5);
                        // starving, each lane breaks into leaves, thinner and fewer to the end
                        float2 lp = float2(u, e / 0.075 + seed * 9.0);
                        lp.y += CelHash21(float2(floor(lp.x), seed * 23.0 + 2.0));
                        float2 lf = frac(lp);
                        float lh = CelHash21(floor(lp) + seed * 7.0);
                        float leaf = (0.26 + 0.10 * lh) / max(starve * starve, 1e-4) * sqrt(saturate(1.0 - abs(lf.y * 2.0 - 1.0)));
                        al *= (1.0 - Step(leaf, abs(lf.x - 0.5), pixX * 2.5)) * step(saturate(starve * 1.2 - 0.8), lh);
                        // thin slits along it too: none in the first of it (the head is one sheet), few where it
                        // leaves, more where it lands
                        float share = saturate(lerp(0.01, 0.22, pow(kS, 1.5)) * smoothstep(0.05, 0.45, e)) * (1.0 - starve), wide = lerp(0.07, 0.22, kS);
                        al *= 1.0 - Streaks(float2(u + seed * 3.0, e / (period * 1.8) + seed * 5.0), seed, share, wide * 0.7, pixX * 2.5);
                        // light lies along it in broad streaks, a paler one inside some of them, the palest inside that
                        col = lerp(body, light, max(Streaks(float2(u * 0.45 + seed * 11.0, e / (period * 2.2) + seed * 2.0), seed + 0.21, 0.65, 0.44, pixX * 1.2),
                            Streaks(float2(u * 0.8 + seed * 17.0 + 5.0, e / (period * 3.1) + seed * 6.0), seed + 0.67, 0.50, 0.38, pixX * 2.0)));
                        col = lerp(col, pale, Streaks(float2(u * 0.9 + seed * 23.0, e / (period * 1.4) + seed * 9.0), seed + 0.53, 0.50, 0.32, pixX * 2.3));
                        col = lerp(col, lerp(pale, half3(1.0, 1.0, 0.80), 0.14), Streaks(float2(u * 0.9 + seed * 23.0, e / (period * 1.4) + seed * 9.0), seed + 0.53, 0.50, 0.13, pixX * 2.3));
                    }
                    else
                    {
                        float cellF = e / period + seed * 37.0;
                        float cell = floor(cellF), p = frac(cellF);
                        float h1 = CelHash21(float2(cell, seed * 91.7 + 1.0)), h2 = CelHash21(float2(cell + 7.0, seed * 53.3 + 2.0));
                        float d = min(duty * (0.80 + 0.40 * h1), 0.985) * (1.0 - 0.5 * starve);
                        float along = p / max(d, 1e-3);                 // 0 its leading point .. 1 its trailing point
                        float env = pow(saturate(min(along / 0.20, (1.0 - along) / 0.24)), 0.62);
                        env *= 0.74 + 0.26 * h2;
                        env *= pow(saturate(e / i.shape2.x), 0.65) * pow(saturate((D - e) / i.shape2.y), 0.65);
                        al = (1.0 - Step(env, abs(q.x), pixX)) * step(0.03, env);
                        float sx = q.x / max(env, 0.05);                // -1 its underside .. 1 its top
                        float cellL = e / (period * 0.37) + seed * 11.0;
                        float pl = frac(cellL), hl = CelHash21(float2(floor(cellL), seed * 23.0 + 9.0));
                        float lens = saturate(min(pl / 0.3, (0.8 - pl) / 0.3)) * step(0.4, hl);
                        float streak = (1.0 - Step(0.30 * lens, abs(sx - 0.25), pixX / max(env, 0.05))) * step(0.02, lens);
                        col = lerp(body, light, streak * step(0.5, kind));
                    }
                }
                else if (mode < 1.5)
                {
                    if (i.shape.y > 1.5)
                    {
                        float wd = pow(saturate(1.0 - q.x * q.x), 0.75);    // a leaf: a point at both ends
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    else if (i.shape.y > 0.5) al = 1.0 - Step(0.9, length(q), pix);
                    else
                    {
                        float aspect = max(i.shape.x, 1.0);
                        float xm = q.x * aspect, hc = aspect - 1.0;     // the head's centre; the point trails behind
                        float wd = xm >= hc ? sqrt(saturate(1.0 - (xm - hc) * (xm - hc)))
                            : pow(saturate((xm + aspect) / max(2.0 * aspect - 1.0, 1e-3)), 0.8);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                    }
                    al *= 1.0 - smoothstep(0.9, 1.0, k);
                }
                else if (mode < 2.5)
                {
                    float tk = i.shape.x;
                    al = Star(q, seed + tk * 0.137, 0.44, 0.90, 0.055, pix);
                    col = lerp(body, light, Streaks(float2(q.x * 9.0 + seed * 7.0, q.y * 3.1 + seed * 3.0), seed, 0.5, 0.15, pix * 9.0));
                    col = lerp(col, pale, Streaks(float2(q.x * 11.0 + seed * 5.0, q.y * 3.7 + seed * 9.0), seed + 0.4, 0.35, 0.22, pix * 11.0));
                }
                else if (mode < 3.5)
                {
                    float tk = i.shape.x, rise = i.shape.y, sink = i.shape.z, heat = i.shape.w;
                    float h = max(rise * lerp(0.75 + 0.25 * sink, 0.22 + 0.78 * sink, heat), 0.05);   // its height now, of the card's: the heap keeps it and thins, the last splash sinks
                    float y = q.y / h;
                    float py = pixY / h;
                    // a pillar of goo pushed over the way the jet goes, more toward its top, a little differently on
                    // every drawing
                    float x = q.x - i.shape2.x * (0.02 + 0.05 * CelHash21(float2(tk * 1.3 + 2.0, seed * 17.0))) * y * y;
                    // its flanks: each side with its own lumps, drawn anew every time, flaring out into the puddle
                    // where it stands (the last splash is a broader one, and spreads as it comes down); in its first
                    // moments the heap is a broad mass
                    float young = (1.0 - heat) * (1.0 - smoothstep(0.15, 0.6, i.state.y));
                    float sd = seed + step(0.0, x) * 0.37;
                    float n1 = CelValue2(float2(y * 2.6 + sd * 9.0, tk * 0.71)) - 0.5;
                    float n2 = CelValue2(float2(y * 6.5 + sd * 3.0, tk * 0.93 + 5.0)) - 0.5;
                    float wave = CelValue2(float2(x * 8.0 + seed * 19.0, y * 9.0 + tk * 0.8)) - 0.5;   // its outline is wavy all round
                    float halfW = lerp(0.37, 0.50, heat) + 0.20 * exp(-y * 8.0) * (1.0 - 0.6 * young) + 0.22 * n1 + 0.09 * n2 + 0.07 * wave;
                    halfW *= (1.25 - 0.25 * sink) * (1.0 + 0.6 * young);
                    // its top: round lumps side by side and two or three thick thorns standing on them, the whole of
                    // it swaying, thrown a little higher on the side the jet goes to
                    float xs = x + (0.09 * sin(y * 6.0 + seed * 40.0 + tk * 1.3) + 0.04 * sin(y * 15.0 + seed * 17.0 + tk * 2.1)) * smoothstep(0.35, 0.9, y);
                    float sky = 0.54 + 0.12 * (CelValue2(float2(xs * 3.0 + seed * 5.0, tk * 1.3)) - 0.5) - 0.5 * xs * xs;
                    sky += 0.08 * i.shape2.x * clamp(xs / 0.35, -1.0, 1.0);
                    sky += 0.15 * Tongues(xs * 3.1 + seed * 3.0, seed, tk, 0.6) + 0.03 * Tongues(xs * 8.0 + seed * 11.0, seed + 0.5, tk, 1.4) + 0.10 * wave;
                    [unroll] for (int p = 0; p < 2; p++)
                    {
                        float pa = CelHash21(float2(p + 4.0, tk * 1.1 + seed * 23.0)), pb = CelHash21(float2(p + 9.0, tk * 2.3 + seed * 61.0));
                        float pc = CelHash21(float2(p + 15.0, tk * 3.3 + seed * 43.0));
                        sky += (0.03 + 0.17 * pb * pb) * pow(saturate(1.0 - abs(xs - (pa - 0.5) * 0.8) / (0.06 + 0.09 * pc)), 1.6);
                    }
                    float heap = (1.0 - Step(halfW, abs(x), pix)) * (1.0 - Step(sky, y, py));
                    // holes open in its upper half once it stands
                    float hole = Step(0.70, CelValue2(float2(x * 7.0 + seed * 3.0 + tk * 0.9, y * 4.0 - tk * 0.5)), pix * 7.0);
                    heap *= 1.0 - hole * smoothstep(0.30, 0.45, y) * (1.0 - heat) * (1.0 - young);
                    // one or two thin ones flung high, curling outward
                    float crown = 0.0;
                    [unroll] for (int j = 0; j < 1; j++)
                    {
                        float hh = CelHash21(float2(j + 3.0, tk * 1.7 + seed * 50.0)), h2 = CelHash21(float2(j + 11.0, tk * 2.3 + seed * 20.0));
                        float h3 = CelHash21(float2(j + 19.0, tk * 3.1 + seed * 70.0));
                        float cx = (hh - 0.5) * 0.7;
                        float y0 = 0.50 - 0.5 * cx * cx;
                        float tt = (y - y0) / (0.16 + 0.30 * h2);
                        float dx = abs(x - cx - (cx * 2.2 + (h3 - 0.5) * 1.6) * (y - y0) * (0.5 + 0.9 * tt));
                        crown = max(crown, (1.0 - Step((0.020 + 0.022 * h3) * pow(saturate(1.0 - tt), 0.7), dx, pix)) * step(0.0, tt) * step(tt, 1.0));
                    }
                    // and leaves of it thrown out of the heap to either side: each goes up and over and comes down
                    // in the puddle, over three drawings
                    float ropes = 0.0;
                    [unroll] for (int n = 0; n < 3; n++)
                    {
                        float turn = floor((tk + n) / 3.0), ph = tk + n - turn * 3.0;
                        float ha = CelHash21(float2(n + 5.0, turn * 1.7 + seed * 30.0)), hb = CelHash21(float2(n + 13.0, turn * 2.9 + seed * 60.0));
                        float hc = CelHash21(float2(n + 23.0, turn * 3.7 + seed * 80.0));
                        float reach = 0.40 + 0.42 * hb, over = 0.06 + 0.10 * hc, from = 0.25 + 0.25 * frac(ha * 2.0);
                        float u = ((ha < 0.5 ? -x : x) - 0.12) / reach;
                        float ya = from * (1.0 - u) + 4.0 * over * u * (1.0 - u);
                        float slope = (4.0 * over * (1.0 - 2.0 * u) - from) / reach * h * i.shape2.y;
                        float lo = ph * 0.34, hi = lo + 0.32;
                        float th = (0.030 + 0.025 * hc) * pow(saturate(1.0 - abs((u - lo) / (hi - lo) * 2.0 - 1.0)), 0.6);
                        ropes = max(ropes, (1.0 - Step(th, abs(y - ya) * h * i.shape2.y / sqrt(1.0 + slope * slope), pix)) * step(lo, u) * step(u, hi) * step(0.004, th));
                    }
                    // (it stands in the puddle on a round foot)
                    al = max(heap, max(crown, ropes * (1.0 - heat))) * step(0.10 * saturate(x * x * 3.0), y) * step(0.0, q.y);
                    // as it goes it thins to strands that hang from its top, its middle (where the target stands)
                    // first; the last splash is torn into flames as it comes up, and again as it sinks
                    float torn = max(pow(1.0 - sink, 1.6) * lerp(1.25, 0.70, heat) - 0.10, heat * (0.5 - i.state.y * 5.0));
                    torn += (1.0 - heat) * (1.0 - sink) * 0.35 * (1.0 - smoothstep(0.06, 0.24, abs(x))) * smoothstep(0.04, 0.16, y) * (1.0 - smoothstep(0.45, 0.62, y));
                    al *= Step(torn, CelValue2(float2(x * lerp(11.0, 4.0, heat) + seed * 13.0, y * lerp(0.9, 2.4, heat) + tk * 0.5)), pix * 9.0);
                    al *= smoothstep(0.0, 0.2, sink);
                    // the goo pours down it: its own green with streaks of light running down it, thin pale ones in
                    // them and the palest low in its middle, its flanks a tone down; all of it stepping down on every
                    // drawing (the last splash is the light tone all over)
                    float flank = smoothstep(0.55, 1.0, abs(x) / max(halfW, 0.05));
                    col = lerp(body, light, max(Streaks(float2(x * 5.5 + seed * 7.0 + 0.5 * sin(y * 7.0 + seed * 9.0), y * 2.6 + tk * 0.37), seed, 0.42, 0.31, pix * 5.5), heat));
                    col = lerp(col, shade, max(flank * 0.3, Streaks(float2(x * 7.0 + seed * 7.0 + 17.0, y * 2.8 + tk * 0.29 + 17.0), seed, 0.12, 0.24, pix * 7.0) * 0.7));
                    float2 ps = float2(x * 8.0 + seed * 7.0 + 0.7 * sin(y * 8.0 + seed * 21.0), y * 3.2 + tk * 0.41);
                    col = lerp(col, pale, Streaks(ps, seed + 0.3, 0.42, 0.26, pix * 8.0) * (1.0 - flank));
                    half3 palest = lerp(pale, half3(1.0, 1.0, 0.80), 0.35);
                    col = lerp(col, palest, Streaks(ps, seed + 0.3, 0.42, 0.15, pix * 8.0) * (1.0 - flank) * (1.0 - smoothstep(0.35, 0.85, y)));
                    // the last splash is hot at its heart: a clump of flames of the palest tone standing in its foot
                    float hx = x / 0.24;
                    float hotTop = (0.38 + 0.36 * Tongues(x * 9.0 + seed * 5.0, seed + 0.7, tk, 1.0)) * saturate(1.0 - hx * hx);
                    float hot = heat * (1.0 - Step(hotTop, y, py)) * step(0.03, y) * smoothstep(0.6, 0.9, rise);
                    col = lerp(col, lerp(pale, half3(1.0, 1.0, 0.80), 0.35), hot);
                    col = lerp(body, col, saturate(heat + smoothstep(0.0, 0.08, y)));   // where it stands in the puddle it is the puddle's green
                }
                else if (mode < 4.5)
                {
                    float rad = length(q), ang = atan2(q.y, q.x);
                    float2 spoke = float2(cos(ang), sin(ang));
                    float tk = i.shape.x, grow = i.shape.y, drain = i.shape.z;
                    // its edge: slow round lobes; blunt drips and a few small points run out of it, and long spikes
                    // to the side the jet came from
                    float R = 0.46 + 0.048 * sin(ang * 2.0 + seed * 20.0) + 0.040 * sin(ang * 3.0 + seed * 47.0) + 0.028 * sin(ang * 5.0 + seed * 61.0)
                        + 0.016 * sin(ang * 8.0 + seed * 83.0) + 0.010 * sin(ang * 13.0 + seed * 29.0 + tk * 0.9);
                    float aside = smoothstep(0.35, 1.0, spoke.x * i.shape.w);
                    float turn = ang / 6.2831853 + 0.5;
                    // draining: the body shrinks fast under its fingers, which shoot out to a star of spikes for a
                    // drawing or two as the heap comes down, and leaves a small pool that dries away
                    float tear = smoothstep(0.0, 0.08, drain) * (1.0 - smoothstep(0.14, 0.22, drain));
                    float fingers = max(Fingers(turn, 26.0, seed, tk, 0.025, 0.12, 0.60, 0.22, 0.48, 0.6, 1.0),
                        max(Fingers(turn, 31.0, seed + 0.17, tk, 0.016, 0.07, 0.28 + 0.5 * tear, 0.10, 0.22, 1.3, 0.0) * (1.0 + 2.0 * tear),
                            Fingers(turn, 11.0, seed + 0.31, tk, 0.08, 0.18, 0.30 + 0.5 * tear, 0.05, 0.11, 1.0, 0.0) * (0.45 + max(2.1 * aside, 1.75 * tear))));
                    float shrink = lerp(1.0, 0.40, smoothstep(0.0, 0.16, drain)) * (1.0 - smoothstep(0.55, 1.0, drain));
                    float keep = lerp(1.0, 0.45, smoothstep(0.12, 0.3, drain)) * (1.0 - smoothstep(0.8, 1.0, drain));
                    float inner = R * shrink * grow;
                    float edge = inner + fingers * keep * grow;
                    al = 1.0 - Step(edge, rad, pix);
                    float rr = rad / max(edge, 1e-3), rb = rad / max(inner, 1e-3);
                    // light lying on it in a few patches; thin ripples two tones down running round the heap's foot
                    // (round, whatever its edge does)
                    float wob = CelValue2(spoke * 1.6 + seed * 3.0 + tk * 0.21);
                    float ringF = rb * 2.6 + wob * 0.6 - tk * 0.07;
                    float ring = 1.0 - Step(0.030, abs(frac(ringF) - 0.5), pix * 2.6 / max(inner, 0.05));
                    ring *= step(0.50, CelValue2(spoke * 2.2 + floor(ringF) * 3.7 + seed * 5.0)) * step(0.22, rb) * step(rb, 0.90);
                    col = lerp(body, light, Step(0.72, CelValue2(q * 3.4 + seed * 5.0 + tk * 0.05), pix * 3.4));
                    col = lerp(col, lerp(shade, rim, 0.5), ring * 0.9);
                    // and a band of light along its near edge, out to the edge itself
                    float lip = Step(0.84, rr, pix / max(edge, 0.05)) * (1.0 - smoothstep(-0.35, 0.25, spoke.y));
                    col = lerp(col, lerp(light, pale, 0.5), lip);
                    // only where the ground is: level, and at the height the puddle lies at
                    float3 lie = spot - i.shape3.xyz;
                    al *= (1.0 - smoothstep(0.45, 0.7, abs(lie.y - dot(lie.xz, i.shape2.xy)))) * smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                }
                else if (mode < 5.5)
                {
                    // a07: the burst's lobe (the focus part), the mode the clip's lobe needs and neither the drop
                    // (one flat palette row, lines 488-505: it only ever computes al) nor the puff (no line round it
                    // by design) can give it. One hard-cut round cel ball, lumpy at its edge, filled with the
                    // palette's bands off a highlight high and to the eye's left - white core, pale yellow, yellow,
                    // orange-red at the far edge - cooling into the row it chars to from shape.z of its life, and
                    // contoured with a hard ink line tone.w screen pixels wide just inside its edge. Everything
                    // steps on the effect's twos clock (shape.x = tick), like every other mode here.
                    float tk = i.shape.x, amp = i.shape.y, charStart = i.shape.z, charEnd = i.shape.w;
                    // a12: the char runs over its own window instead of to the lobe's death. CelRamp2 blends on
                    // smoothstep(coolFrom, 1, k), so passing it the remapped progress with coolFrom 0 lets the
                    // char START at charStart of the life and be COMPLETE at charEnd - the clip chars while the
                    // cloud is still big (f48 to about f65), not as it dies.
                    float ch = saturate((k - charStart) / max(charEnd - charStart, 1e-3));
                    float d = length(q);
                    float2 dir = d > 1e-4 ? q / d : float2(1.0, 0.0);   // read on the direction: no seam round the circle
                    float lump = CelValue2(dir * 2.3 + seed * 17.0 + tk * 0.021) - 0.5;
                    float lump2 = CelValue2(dir * 5.1 - seed * 9.0 - tk * 0.013) - 0.5;
                    float rr = 1.0 + amp * (1.6 * lump + 0.8 * lump2);   // its own edge, in q
                    al = 1.0 - Step(rr, d, pix);                        // one hard cel edge, nothing soft
                    // a22, the THIRD and last budgeted round of the burst's recorded diagnosis: THE BAND FIELD IS
                    // MEASURED IN THE CLOUD, NOT IN THE LOBE. a20 took the per-lobe ink contour off and the cloud
                    // still read as a heap of countable balls; a21 squeezed the per-lobe annulus into a thin rind
                    // and it still did (and lost the clip's yellow body and its whole orange-red cooling stage).
                    // So what fences the lobes is not the band's WIDTH but its FRAME: a field measured from each
                    // lobe's own highlight in each lobe's own card gives two overlapping lobes DIFFERENT values in
                    // the pixels they share, and a seam is drawn wherever they meet - the shingled roof of tiles
                    // the reader saw at 3x in a21, each tile wearing its own yellow and orange-red arc through the
                    // middle of the mass. The clip at f40 is one YELLOW FIELD carrying large irregular creamy
                    // white blobs that run across several lobes and ignore lobe centres, with the only orange-red
                    // a hard line on the mass's OUTER bottom edge.
                    // cloudQ is this pixel's place in the whole cloud, in the cloud's half-extents (the vertex did
                    // the projection and the divide). It is CONTINUOUS ACROSS LOBES, so two overlapping lobes
                    // compute the same band value where they overlap and no seam between them can be drawn; and
                    // the orange-red, which the ramp puts beyond u = 1.15, can only reach pixels far from the
                    // cloud's middle - its outer silhouette, the one place the clip has it.
                    float2 cloudQ = i.shape2.xy + q * i.shape2.zw;
                    // the cloud's own highlight, high and to the eye's left, the way the lobe's used to be
                    float un = length(cloudQ - float2(-0.20, 0.26));
                    // ...and made IRREGULAR in the same cloud frame, because a purely radial cloud field is only
                    // the same mistake at cloud scale - one big bullseye - while the clip's whites are blobs. This
                    // noise is in CLOUD coordinates with NO per-lobe seed, so its blobs span lobes and ignore
                    // lobe centres exactly as the clip's do (about 1.4 m across at this frequency).
                    // a25: this noise is now the CORE-BREAKER as well. a22-a24 only let it wobble the band
                    // BOUNDARIES, and it could not break the heart, for a measurable reason: at k = 0.28 the core
                    // threshold sits at cloud u 0.99 while un at the heart is about 0.3, and the amplitude was
                    // only 0.50 (swing +-0.375), so the whole heart stayed inside the core band whatever the
                    // noise did - one giant cream lens, which a24's reader marked [worse] against a16 ('a flat
                    // cream-and-yellow poster with no blob or lobe edge inside it'). The clip has about TWENTY
                    // small irregular creamy blobs with saturated yellow as the MORTAR between them, each smaller
                    // than a lobe and crossing two or three of them; ours has 4.7 of ~1400 px and the white AREA
                    // is nearly right, so this is purely DISTRIBUTION.
                    // Frequency 1.9 -> 4.8 (and 4.3 -> 9.5) makes a feature about 2.60 / 4.8 = 0.54 m, roughly a
                    // lobe radius, which is the clip's blob size; it is still in CLOUD coordinates with no
                    // per-lobe seed, so the blobs run across lobes and ignore lobe centres.
                    // a26: the band-boundary wobble goes back to a24's low frequency and amplitude - a25 proved
                    // the high frequency belongs in the CUT below, not here.
                    float blob = CelValue2(cloudQ * 1.9 + 31.7) + 0.5 * CelValue2(cloudQ * 4.3 - 11.3) - 0.75;
                    // ...and the core-breaker is its own HIGH-frequency field. A feature is about 2.60 / 4.8 =
                    // 0.54 m, roughly a lobe radius: the clip's blob size. Still cloud coordinates, no per-lobe
                    // seed, so the blobs run across lobes and ignore lobe centres, as the clip's do.
                    float blobHF = CelValue2(cloudQ * 4.8 + 7.3) + 0.5 * CelValue2(cloudQ * 9.5 - 3.1) - 0.75;
                    // ...and the amplitude gets an ENVELOPE so it breaks only the heart. At un ~ 0.3 it is 1.045,
                    // so u swings about +-0.78 around 0.3: it crosses the core threshold 0.99 and breaks the lens
                    // into blobs with yellow mortar, while staying under the orange threshold 1.24 so no warm
                    // colour can appear in the heart. At un >= 1.25 it is ZERO, so a23's fitted bands and a24's
                    // time ramp keep the clean warm rind and the cooling ramp they earned, untouched.
                    // a26 keeps a25's ENVELOPE, which was the half of it that worked: zero at the rim, so the
                    // warm rind and a24's f54-f76 cooling ramp are untouched, strongest only in the heart.
                    // a27: the cut becomes a WALL instead of a PEAK, and that is the whole round. a26's
                    // max(blobHF, 0.0) cuts hardest where the noise is HIGHEST, so it digs a few deep BAYS into
                    // the cream and leaves the rest of the sheet connected behind them: measured on the repaired
                    // core counter, a26's cream at f40 is 5 blobs whose largest holds 0.68 of the core area,
                    // against the clip's 11 blobs whose largest holds 0.27. Raising that peak-cut's frequency or
                    // amplitude cannot fix it - simulated on this exact CelValue2, the 2x frequency the a26
                    // reader's NEXT asked for leaves ONE component of 59711 px (max/total 1.00), a finer stipple
                    // of holes in the same sheet. The clip does not spend its interior differently from us: at
                    // f40 the clip is 31302 core / 33653 mortar px (48/52) and a26 is 30932 / 32574 (49/51), the
                    // SAME budget. The clip spends it as ONE CONNECTED NET of narrow lanes (3 mortar components,
                    // 99 % of the mortar in one of them, lane half-width 3.8 px) and a26 spends it as a few fat
                    // bays (12 components, the largest only 0.37, half-width 8.2 px, p90 26.7). So the mortar
                    // must be a THIN CONTINUOUS WALL at the noise's ZERO CROSSINGS, not a pit at its peaks:
                    // saturate(1.0 - abs(blobHF) / 0.20) is 1 where blobHF = 0 and falls to 0 by |blobHF| = 0.20,
                    // a lane whose width is set by that 0.20 and whose topology is a net by construction, because
                    // the zero set of a smooth 2D field is a set of CLOSED CURVES. Simulated: 15 blobs, the
                    // largest holding 0.35 of the core, the mortar one component holding 0.88. The coefficient
                    // comes DOWN 0.50 -> 0.45, so in the hot phase (v = 1) the floor is v = 0.55 and still no
                    // orange can enter the mass: strictly safer than a26 on its own stated bound. The
                    // frequencies, the envelope, A/B, the time ramp, the char cap and every recipe number are
                    // UNTOUCHED, and v = saturate(v - coreCut) below keeps the cut one-sided by construction, so
                    // a25's falsification still stands and this cut can never enlarge the core either.
                    float coreCut = 0.45 * saturate(1.25 - un) * saturate(1.0 - abs(blobHF) / 0.20);
                    // a08: the band field. u WAS the distance from the highlight in units of the lobe's own
                    // radius (a22 measures it in the cloud instead; the proportions below are a08's and stand);
                    // v holds above the ramp's core threshold (0.88) out to u = 0.5 and only falls under its first
                    // threshold (0.25) beyond u = 1.15, so the mass is mostly core and yellow with the orange-red
                    // left as its outermost sliver - the clip's proportions. a07's 1 - u/1.42 reached the core
                    // inside u = 0.17 (a dot) and put c1/c0 over most of the lobe.
                    // NOT saturate(): u must be free to pass 1.15, which is where the ramp hands over to the
                    // orange-red. Clamping it at 1 would leave v at 0.398, above the first threshold, and the
                    // orange-red rim this whole round is about could never be drawn anywhere.
                    float u = max(un + 0.50 * blob, 0.0);
                    // a21, the second round of the burst's recorded diagnosis. a20 took the ink contour off every
                    // lobe and the cloud STILL read as a heap of countable balls, so the line was not what fences
                    // them: the independent reader named the band field instead - "the full-circle ORANGE RIM
                    // drawn round every lobe plus each lobe's own concentric yellow band and cream core, which
                    // fence even the interior lobes the clip leaves open", against "the clip's whites are large
                    // irregular blobs that run across several lobes and ignore lobe centres". a08's field put the
                    // core inside u = 0.50 and the first threshold at u = 1.15, so every lobe wore a wide yellow
                    // and orange-red annulus - a bullseye, drawn in the middle of the mass as well as at its edge.
                    // a21 pushed the core out to u = 0.90 and the first threshold to u = 1.22, leaving the warm
                    // bands as a thin RIND on the last tenth of the radius. FALSIFIED: the cores did merge, the
                    // cloud still read as a heap, and the price was the clip's yellow body (ours went to paper
                    // white) and its whole orange-red cooling stage (f54-f60 went white bellies under black caps).
                    // a22 puts a08's PROPORTIONS back - core to u = 0.50, yellow to 1.15, orange-red beyond - now
                    // that u is measured in the cloud: at cloud scale a wide band IS the clip, one yellow field
                    // with a warm rim, and the same width that drew a bullseye on every lobe draws it once, on the
                    // whole mass. Reverting a21 is not a second change; it is what the new frame must be tested on.
                    // a23: and those proportions are now FITTED to the cloud, which is the one thing a22 got
                    // wrong. a08's A = 1.41, B = 1.03 were written for the LOBE's radius, where u = 1.15 is a
                    // lobe's outermost sliver; carried over to the cloud unchanged, u = 1.15 is HALF THE MASS, so
                    // a22 came out as a target - cream centre, yellow annulus, orange annulus, flat poster red
                    // over the whole outer and lower half - where the clip's body is yellow with the orange-red
                    // only a thin rind on the outer silhouette. The same overshoot put flat red lobes on screen
                    // at f30-f32, eighteen frames before the clip has any red at all.
                    // CelRamp's thresholds are fixed (0.25 / 0.50 / 0.75, core at 0.88), so A and B place the
                    // bands in u directly: core out to A - 0.88 B, orange-red beyond A - 0.25 B. A = 2.16,
                    // B = 1.83 put the core at u = 0.55 (a22's was 0.50: the white heart deliberately does NOT
                    // grow), light yellow to 0.79, the yellow BODY to 1.24, orange to 1.70, and the orange-red
                    // rim only beyond 1.70 - the outermost rind of the whole cloud, which is where the clip
                    // rims it. Nothing else of a22 moves: cloudQ, the cloud highlight, the cloud-space noise,
                    // the half-extents, the per-lobe alpha, the ink (still off) are all exactly as it built them.
                    // a24: and A gets a TIME AXIS, which is the one thing this field has never had. An
                    // independent analyst at the a23 diagnosis point measured the red-rim-class fraction
                    // (g < 110) of the burst body per frame: the CLIP ramps .043 .076 .179 .464 .661 .621 at
                    // f36 f44 f48 f60 f68 f76, while a23 sits FLAT at about 0.25 for its whole life (a16: .186
                    // .158 .160 .299 .420 .000). a23 is five times too red while hot and seven times too cool
                    // while cooling, and a16 beats it at BOTH ends. The reason was one line: v = (A - u) / B had
                    // no k in it at all, so every band boundary was fixed in space for the lobe's whole life and
                    // the only way the burst could show warm colour was to show it in a PLACE, from its first
                    // frame - hence flat red lobes at f30-f33, eighteen frames before the clip has any red, and
                    // a pale creamy heart at f54-f76 where the clip is orange-red on grey-brown charcoal.
                    // THE CLIP'S WARM BAND IS A CLOCK. At k = 0.28 the core reaches cloud u = 0.99 and the
                    // orange-red only beyond 2.14: a white-and-yellow mass with a thin rim, the clip's f36-f48,
                    // whose interior is white-dominant with yellow as the mortar between the blobs. At k = 0.80
                    // the core is gone and the orange-red starts at u = 0.94: red-orange over the whole field,
                    // the clip's f68-f76. k 0.28..0.80 is about f44..f68 at this lobe's 1.7-2.1 s life, so the
                    // warm front LEADS charStart 0.38 and finishes just before charEnd 0.86 - fire that cools
                    // under a cap already closing over it, which is what the clip shows.
                    // This also records a struck conclusion: a13 claimed the char SHELL is independent of this
                    // ramp. Three falsifiers have now fired on that (a21, a23, and the table above). The cool's
                    // colour comes from the band field; no round may assume the shell is independent, and every
                    // band-field round is judged at f54-f76 as well as at f36-f48.
                    // B, cloudQ, the highlight, the blob noise, the alpha, the ink (off) and the char cap are
                    // exactly as a22 and a23 left them. No geometry moves.
                    float A = lerp(2.60, 1.40, saturate((k - 0.28) / 0.52));
                    float v = saturate((A - u) / 1.83);
                    // a26: CUT the cream heart into the clip's many small blobs - and only ever CUT it. a25 put
                    // the breaking term inside u, where `u = max(un + amp*blob, 0)` RECTIFIES: the noise is
                    // symmetric about zero but the clamp removes its negative tail, so raising the amplitude
                    // biased u DOWNWARD on average and made the cream heart BIGGER (one blob, 72494 px -> 78762)
                    // instead of breaking it. Here the term is one-sided by construction - max(blobHF, 0) can
                    // only ever REDUCE v - so it can carve the cream field into blobs and can never grow it.
                    // Sized so it cannot overshoot: at the heart v is 1.0 and the cut reaches at most 0.50, so v
                    // lands in 0.50..1.0 - below the ramp's core threshold 0.88 wherever the noise is high (the
                    // clip's yellow MORTAR between blobs) and never below 0.50, so no orange can appear inside
                    // the mass. The clip at f36-f48 is about twenty small irregular creamy blobs in a yellow
                    // field, each smaller than a lobe and crossing two or three of them.
                    v = saturate(v - coreCut);
                    // a13: the char is a SHELL, not a tone change. A dark cap closes over the lobe from its TOP
                    // down as ch runs 0..1, its edge as ragged as the lobe's own, and the band under it goes on
                    // burning: the clip's cooling is charcoal closing over fire, never a ball that dims all over.
                    // Under the cap the lobe still darkens a little (0.22 ch) the way the clip's unburnt part
                    // goes from yellow toward orange before the cap reaches it.
                    // a31: the cap was written to CLOSE COMPLETELY. q.y spans about -1..1, so at ch = 1 the
                    // front sat at -1.25 +- 0.17, BELOW the whole lobe: every pixel charred, chPix = 1,
                    // and CelRamp2 returned the smoke row everywhere regardless of v - one flat tone with
                    // no fire in it at all, from about f69 on. The clip's cooling mass is NEVER that: it is
                    // grey-brown char with a dozen orange CRACKS still burning in it at f76-f80 (red over
                    // char 0.101 against our 0.0016). Two terms, one expression: the front now STOPS at
                    // -0.25 so a band of each lobe never chars, and the noise amplitude GROWS 0.34 -> 1.14
                    // as ch passes 0.5 so the front frays into cracks instead of sweeping clean. Identical
                    // at ch = 0 by construction, so every frame before the cap moves is untouched.
                    float capY = lerp(1.25, -0.25, ch)
                        + (0.34 + 0.80 * smoothstep(0.50, 1.0, ch))
                          * (CelValue2(dir * 3.1 + seed * 5.0 + tk * 0.017) - 0.5);
                    float charred = Step(capY, q.y, pix);               // 1 above the front, 0 below it
                    float chPix = max(0.22 * ch, charred);
                    col = CelRamp2(v, i.tone.x, max(i.tone.y, 0.0), chPix, 0.0);
                    col = lerp(col, CelInkColor(i.tone.x, chPix, 0.0), CelInk(rr - d, 0.0, i.tone.w));
                }
                else if (mode < 6.5)
                {
                    float tk = i.shape.x, fray = i.shape.y, lit = i.shape.z;
                    float f = Puff(q, seed, tk);
                    float n = CelValue2(q * 2.6 + seed * 13.0 + tk * 0.11) * 0.6 + CelValue2(q * 6.5 + seed * 5.0 - tk * 0.07) * 0.4 - 0.5;
                    float strand = CelValue2(float2(q.x * 8.0 + seed * 19.0, q.y * 1.6 - tk * 0.09)) - 0.5;   // upright tatters
                    float cut = f + 0.20 * n + (0.05 + 0.22 * fray) * strand - fray * 0.34;   // it thins from its rim inward as it ages
                    al = smoothstep(0.0, 0.06, cut);                    // smoke: no line round it, a ragged edge
                    // lit from below in its first moments (the acid under it), then its own green, a tone down as it
                    // ages; one that stays lit keeps the light tone, its underside its own green
                    float glow = saturate(lit);
                    float age = k + 0.25 * n;
                    col = lerp(pale, light, smoothstep(0.0, lit * 0.6, age));
                    col = lerp(col, body, smoothstep(lit * 0.5, lit * 1.4, age));
                    col = lerp(col, shade, smoothstep(0.5, 1.0, k + 0.20 * n) * 0.8 * (1.0 - 0.6 * glow));
                    float under = smoothstep(-0.15, 0.55, dot(q, float2(0.45, -0.85)) + 0.5 * n);
                    col = lerp(col, lerp(shade, body, glow), under * 0.8);   // its underside a tone down,
                    col = lerp(col, rim, smoothstep(0.55, 0.95, under + 0.6 * n) * 0.5 * (1.0 - 0.7 * glow));   // its hollows two
                    al *= (1.0 - smoothstep(0.35, 1.0, fray)) * smoothstep(0.0, 0.08, k);
                    col = MixFog(col, i.fog);
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
                return half4(col * al, al);
            }
        ENDHLSL
        Pass
        {
            Name "CelFireballLight"
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
            Name "CelFireball"
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
