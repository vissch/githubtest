// Phase: CelFx acid beam (2026-11-15) - one GPU pool for the whole acid beam (reference: acid_beam_cel_00001_): the
// beam itself and the thin lines beside it, the glossy blobs it throws, the sparks and leaves, the starbursts where
// it lands, the mass of goo over the target, the puddle on the ground, the green smoke it leaves and the light it
// throws on the ground. Built from the neon acid shader (CelNeonAcid_URP): the puddle decal, the light pass, the
// smoke and the mound are that shader's. Every shape is a field in the fragment; nothing is a texture (the light
// reads the camera's depth). The clip is drawn on ONES (every frame changes), so the age a record is drawn at is
// stepped 24 times a second on the EFFECT's clock (Extra.w carries the seconds from the effect's start to the
// record's birth, so every record of one effect steps on the same frames).
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 strand   the beam, or a thin line beside it. Pos the nozzle, Vel the launch velocity, Motion.x gravity (m/s2):
//             the fluid that left s seconds ago is at Pos + Vel s - g s2 / 2. Life = seconds the beam is on + the
//             flight. Size x half-width at the nozzle (m), y at the target (linear between), z unused, w the flight
//             seconds. Axis x, y a line's place across the beam (1 is the beam's edge: up the screen, toward the
//             eye), z its weave phase. Erode x the beam's half-width at the nozzle (m), y at the target (a line is
//             placed in those units), z dash duty 0..1, w dash seconds.
//             Extra y seconds the beam is on, z what it is: 0 a line (a ribbon broken into long dashes), 1 a line
//             with light streaks, 2 the beam: lumpy edges that flow with it, a yellow core with pale streaks, lime
//             and green round it, a spear point at its head; its last Motion.w seconds of fluid break into a
//             wavy rope of beads and necks.
//             Motion y the head's taper (seconds of fluid), z the tail's, w (the beam) the seconds of fluid at its
//             end that break up.
//  1 drop     a ballistic drop. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at death,
//             w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf (a point
//             at both ends), 3 a glossy blob (drawn like a bubble: its light side, a glint, a dark rim), 4 a spark
//             (a teardrop, hot along its middle).
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
//  5 bubble   Pos on the ground. Size x radius it swells to (m). Extra y seconds of the pop at the end of its life.
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
// alone), z opacity (a glow: its strength), w unused.
// Palette rows: c0 rim, c1 shade, c2 body, c3 light, core hot, ink pale (the beam: c3 its yellow, core its hot
// yellow, ink the pale streaks; the puddle: core its yellow rim).
// A splat (mode 2): Extra z the spikes' half-width (0: 0.055), Axis x its turn per drawing.
//
// Two passes, one draw: the light pass first (only the glows draw in it: lit = unlit x (1 + what a glow multiplies
// in), which a blend can do and an added colour cannot), then everything, in the records' order.
//
// _TWAcidBeamLens (global; AcidBeamRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0 none.
// The vignette takes most of the light from the corners of the frame, and the clip's acid is as bright there as in
// the middle: every colour is lifted by what the vignette will take from it, as far as white allows (its brightest
// channel stops there, the others go on a fifth further: it keeps its hue and pales only a little).
Shader "TW/Cel/AcidBeam"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+16" "IgnoreProjector"="True" }
        ZWrite Off ZTest LEqual Cull Off
        HLSLINCLUDE
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            float4 _TWAcidBeamLens;

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
                float tick = floor((t + delay) * 24.0 + 0.01);          // the drawing: a new one every frame (on ones)
                float a = max(tick / 24.0 - delay, 0.0);                // the record's age at that drawing
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
                    float win = max(r.motion.w, 0.01);
                    float starve = smoothstep(D - win, D - 0.35 * win, e);   // the last of the beam breaks up
                    float beam = lerp(r.erode.x, r.erode.y, kS);        // the beam's half-width here (a line is placed in it)
                    float hw;
                    if (r.extra.z > 1.5)
                    {
                        hw = lerp(r.size.x, r.size.y, kS) * lerp(1.0, 1.50, starve);
                        hw *= 1.0 + 0.6 * smoothstep(0.88, 1.0, kS);   // it fans out into the splash where it lands
                        // its broken end is a torn net that spreads a little wider and swings across where the beam was
                        c += side * (beam * 0.30 * starve * sin(e * 23.0 + r.axis.z)) + nrm * (beam * 0.25 * starve * sin(e * 17.0 + r.axis.z * 1.3));
                    }
                    else
                    {
                        hw = lerp(r.size.x, r.size.y, kS) * (1.0 - starve);
                        float2 place = r.axis.xy + 0.10 * float2(sin(e * 9.0 + r.axis.z), sin(e * 6.1 + r.axis.z * 1.7));
                        c += (side * place.x + nrm * place.y * 0.6) * beam;
                    }
                    w = c + side * cq.y * hw;
                    q = float2(cq.y, v.uv.x);
                    flow = float4(e, kS, 0.0, 0.0);
                    shape = float4(D, r.erode.z, max(r.erode.w, 0.02), r.extra.z);
                    shape2 = float4(max(r.motion.y, 0.004), max(r.motion.z, 0.004), win, 0.0);
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
                    w = CelBillboard(p, cq * CelCurve(r.size.xyz, k));   // it grows over its life
                    shape = float4(floor(tick / max(r.axis.x, 1.0)), r.extra.z > 0.0 ? r.extra.z : 0.055, r.axis.y > 0.0 ? r.axis.y : 0.90, 0.0);   // a new star every Axis.x drawings; its spikes' half-width; their reach
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
                    float popAt = r.life - r.extra.y;
                    float swell = saturate(a / max(popAt * 0.35, 0.01));
                    float rad = r.size.x * (0.18 + 0.82 * pow(swell, 0.7));
                    float pop = saturate((a - popAt) / max(r.extra.y, 0.01));
                    float card = rad * (1.4 + 0.9 * step(0.0001, pop));  // room for the ripple it stands in, and for the pop's ring
                    w = CelBillboard(r.pos + float3(0.0, rad * 0.92, 0.0), cq * card);
                    q = cq * (card / max(rad, 1e-4));
                    shape = float4(tick, pop, swell, 0.0);
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
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWAcidBeamLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWAcidBeamLens.y, 0.01)));
                float lift = pow(1.0 / keep, share);
                float3 lin = SRGBToLinear(max(col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float gain = max(min(lift, 1.0 / max(top, 1e-3)), 1.0);
                float3 lifted = LinearToSRGB(min(lin * min(lift, gain * 1.2), 1.0));
                return _TWAcidBeamLens.x > 0.0 ? lifted : col;
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
                    float L = lerp(heart * 1.1, reach, h2);             // (the clip's starbursts: many long spikes)
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
                    float win = i.shape2.z;
                    float starve = smoothstep(D - win, D - 0.35 * win, e);   // the last of the beam breaks up
                    if (kind > 1.5)
                    {
                        // the beam: lumpy edges that ride with the fluid (a lump keeps its place in the fluid as it
                        // flies), a spear point at its head, a blunt end at its tail
                        half3 hot = CelPalCore(row).rgb, streak = CelPalInk(row).rgb;
                        // (the head: a long cone - the first fluid is thin and it widens over its first 0.45 s, steeply
                        // at the point: the clip's beam is 0.36 of its full width 0.02 s behind the point, 0.78 at 0.21 s)
                        float ends = pow(saturate(e / i.shape2.x), 0.33) * pow(saturate((D - e) / i.shape2.y), 0.5);
                        float lt = CelValue2(float2(e * 24.0 + seed * 31.0, 0.5)) * 0.65 + CelValue2(float2(e * 61.0 + seed * 13.0, 2.5)) * 0.35;
                        float lb = CelValue2(float2(e * 21.0 + seed * 17.0, 7.5)) * 0.65 + CelValue2(float2(e * 55.0 + seed * 7.0, 9.5)) * 0.35;
                        float edge = (q.x > 0.0 ? 0.68 + 0.32 * lt : 0.70 + 0.30 * lb) * ends;
                        al = (1.0 - Step(edge, abs(q.x), pixX)) * step(0.02, edge);
                        // breaking up: ROUND holes open in the sheet, each its own size, and grow until they meet, so
                        // the fluid between them is blobby ropes of every thickness that tear apart (cells of the
                        // fluid, so the holes fly with it; b02's cell borders drew a honeycomb of straight ropes)
                        float border = 1.0;
                        if (starve > 0.001)
                        {
                            float2 wp = float2(q.x * 2.0, e / 0.040) + seed * 7.0;
                            float2 wc = floor(wp), wf = frac(wp);
                            float gap = 8.0;                             // how far this point is outside every hole
                            [unroll] for (int yy = -1; yy <= 1; yy++)
                            [unroll] for (int xx = -1; xx <= 1; xx++)
                            {
                                float2 o2 = float2(xx, yy);
                                float2 cid = wc + o2;
                                float2 pt = o2 + float2(CelHash21(cid + 3.1), CelHash21(cid + 17.7)) * 0.8 + 0.1 - wf;
                                float hr = (0.18 + 0.42 * CelHash21(cid + 29.3)) * starve * 1.15;   // (b03's 1.5 left thin strands)
                                gap = min(gap, length(pt) - hr);
                            }
                            al *= Step(0.0, gap, pixX * 2.0);
                            border = saturate(1.0 - gap / 0.30);         // 1 at a hole's rim, 0 deep in the fluid
                        }
                        // across it: a yellow core with a hot heart, lime round it, then green, a dark lip at its edge;
                        // the bands waver along it (in the net: each rope green with a yellow middle)
                        float pd = pixX / max(edge, 0.05);
                        float wob = CelValue2(float2(e * 17.0 + seed * 5.0, q.x * 2.0 + 4.5)) - 0.5;
                        float d = abs(q.x) / max(edge, 0.02) + 0.16 * wob;
                        d = max(d, lerp(d, 0.35 + 0.65 * border, smoothstep(0.0, 0.5, starve)));   // (round a hole: green, then lime)
                        col = lerp(rim, shade, 1.0 - Step(0.92, d, pd));
                        col = lerp(col, body, 1.0 - Step(0.74, d, pd));
                        col = lerp(col, light, 1.0 - Step(0.56, d, pd));
                        col = lerp(col, hot, 1.0 - Step(0.30, d, pd));
                        // long pale streaks in the core and lime ones in the green, flowing with it
                        float u = q.x * 0.5 + 0.5;
                        float sk = Streaks(float2(u * 7.0 + seed * 3.0, e / 0.045 + seed * 5.0), seed, 0.55, 0.30, pixX * 3.5);
                        col = lerp(col, streak, sk * (1.0 - smoothstep(0.40, 0.58, d)));
                        float sk2 = Streaks(float2(u * 10.0 + seed * 7.0, e / 0.032 + seed * 9.0), seed + 0.4, 0.45, 0.26, pixX * 5.0);
                        col = lerp(col, lerp(body, light, 0.5), sk2 * smoothstep(0.56, 0.62, d) * (1.0 - smoothstep(0.86, 0.92, d)));
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
                    if (i.shape.y > 3.5)
                    {
                        // a spark: a teardrop (its head ahead), hot along its middle and at its head
                        float aspect = max(i.shape.x, 1.0);
                        float xm = q.x * aspect, hc = aspect - 1.0;
                        float wd = xm >= hc ? sqrt(saturate(1.0 - (xm - hc) * (xm - hc)))
                            : pow(saturate((xm + aspect) / max(2.0 * aspect - 1.0, 1e-3)), 1.2);
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        float inner = 1.0 - abs(q.y) / max(wd * 0.92, 0.02);
                        col = lerp(shade, body, smoothstep(0.0, 0.35, inner));
                        col = lerp(col, light, smoothstep(0.35, 0.7, inner) * smoothstep(-0.6, 0.4, q.x));
                        col = lerp(col, pale, smoothstep(0.75, 0.95, inner) * smoothstep(0.2, 0.8, q.x));
                    }
                    else if (i.shape.y > 2.5)
                    {
                        // a glossy blob: round (stretched a little along its flight), its lit side up and to the
                        // left, a glint, a shade low on its right and a dark rim
                        float2 p = float2(q.x * max(i.shape.x, 1.0) / max(i.shape.x * 0.5 + 0.5, 1.0), q.y);
                        float r1 = length(p);
                        al = 1.0 - Step(0.92, r1, pix);
                        col = lerp(shade, body, 1.0 - Step(0.80, length(p - float2(-0.10, 0.10)), pix));
                        col = lerp(col, light, 1.0 - Step(0.50, length(p - float2(-0.18, 0.24)), pix));
                        col = lerp(col, rim, Step(0.84, r1, pix) * 0.85);
                        col = lerp(col, pale, 1.0 - Step(0.20, length((p - float2(-0.34, 0.40)) * float2(1.0, 1.3)), pix));
                    }
                    else if (i.shape.y > 1.5)
                    {
                        float wd = pow(saturate(1.0 - q.x * q.x), 0.75);    // a leaf: a point at both ends
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        // its tone (Tone.w: 0 body, 1 shade, 2 light, 3 the row's ink, 4 rim), a lighter streak down
                        // its middle, a little off to one side (pale on the light ones, a tone up on the dark ones)
                        float ts = i.tone.w;
                        half3 base = ts < 0.5 ? body : ts < 1.5 ? shade : ts < 2.5 ? light : ts < 3.5 ? CelPalInk(row).rgb : rim;
                        half3 lit = ts < 1.5 ? lerp(base, light, 0.5) : ts > 3.5 ? shade : lerp(base, pale, 0.65);
                        col = lerp(base, lit, 1.0 - Step(0.28 * wd, abs(q.y - 0.18 * wd), pixY));
                    }
                    else if (i.shape.y > 0.5)
                    {
                        al = 1.0 - Step(0.9, length(q), pix);
                        float ts = i.tone.w;                            // its tone, as a leaf's
                        col = ts < 0.5 ? body : ts < 1.5 ? shade : ts < 2.5 ? light : CelPalInk(row).rgb;
                        col = lerp(col, pale, 1.0 - Step(0.35, length(q - float2(-0.25, 0.3)), pix));
                    }
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
                    float an = tk * 0.61 + seed * 6.2831853;            // each new star turned
                    float2 qr = float2(q.x * cos(an) - q.y * sin(an), q.x * sin(an) + q.y * cos(an));
                    al = Star(qr, seed + tk * 0.137, 0.30, i.shape.z, i.shape.y, pix);
                    // a pale-yellow heart, light-yellow streaks out along its spikes, lime at their ends
                    float rq = length(q);
                    float2 radial = float2(atan2(qr.y, qr.x) * 2.546 + seed * 7.0, rq * 4.0 - seed * 3.0);
                    half3 yellow = CelPalInk(row).rgb;
                    col = lerp(light, yellow, saturate(Streaks(radial, seed, 0.55, 0.30, pix * 9.0) + (1.0 - smoothstep(0.20, 0.50, rq))));
                    col = lerp(col, pale, saturate(Streaks(radial + float2(0.5, 0.3), seed + 0.4, 0.30, 0.20, pix * 9.0) * (1.0 - smoothstep(0.3, 0.7, rq)) + (1.0 - smoothstep(0.10, 0.26, rq))));
                    col = lerp(col, lerp(body, shade, 0.5), smoothstep(0.55, 0.95, rq) * 0.8);
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
                    // its edge: slow round lobes, blunt drips and a few small points (the clip's pool: a broad slab
                    // round the target's foot, its outline wavy, no long fingers)
                    float R = 0.46 + 0.048 * sin(ang * 2.0 + seed * 20.0) + 0.040 * sin(ang * 3.0 + seed * 47.0) + 0.028 * sin(ang * 5.0 + seed * 61.0)
                        + 0.020 * sin(ang * 8.0 + seed * 83.0) + 0.014 * sin(ang * 13.0 + seed * 29.0 + tk * 0.3);
                    float turn = ang / 6.2831853 + 0.5;
                    float fingers = max(Fingers(turn, 26.0, seed, floor(tk / 3.0), 0.015, 0.06, 0.50, 0.22, 0.48, 0.6, 1.0),
                        Fingers(turn, 31.0, seed + 0.17, floor(tk / 3.0), 0.010, 0.04, 0.25, 0.10, 0.22, 1.3, 0.0));
                    // draining: it shrinks a little and holes open in it, which grow and join until it is islands,
                    // then beads, then nothing
                    float shrink = lerp(1.0, 0.75, smoothstep(0.0, 1.0, drain));
                    float inner = R * shrink * grow;
                    float edge = inner + fingers * grow;
                    float n = CelValue2(q * 5.5 + seed * 3.0) * 0.7 + CelValue2(q * 13.0 + seed * 9.0) * 0.3;
                    float thr = 1.05 * smoothstep(0.0, 1.0, drain);
                    float inside = min((edge - rad) / max(edge, 0.05) * 3.0, (n - thr) * 2.5);   // > 0 on the acid; 0 its edge (the pool's or a hole's)
                    al = Step(0.0, inside, pix * 3.0 / max(edge, 0.05));
                    float rb = rad / max(inner, 1e-3);
                    // its own green with patches of a lighter one, thin yellow ripples round the target's foot, and a
                    // broad yellow rim at every edge, a thin pale line just inside it
                    float wob = CelValue2(spoke * 1.6 + seed * 3.0 + tk * 0.07);
                    float ringF = rb * 2.4 + wob * 0.7 - tk * 0.02;
                    float ring = 1.0 - Step(0.045, abs(frac(ringF) - 0.5), pix * 2.4 / max(inner, 0.05));
                    ring *= step(0.45, CelValue2(spoke * 2.2 + floor(ringF) * 3.7 + seed * 5.0)) * step(0.18, rb) * step(rb, 0.88);
                    col = lerp(shade, body, Step(0.68, CelValue2(q * 3.4 + seed * 5.0 + tk * 0.02), pix * 3.4));
                    col = lerp(col, pale, ring);
                    float band = 1.0 - Step(0.34, inside, pix * 3.0 / max(edge, 0.05));   // (b01's 0.16 and b03's 0.24 drew it thin)
                    col = lerp(col, CelPalInk(row).rgb, (1.0 - Step(0.46, inside, pix * 3.0 / max(edge, 0.05))) * (1.0 - band));
                    col = lerp(col, pale, band);
                    // only where the ground is: level, and at the height the puddle lies at
                    float3 lie = spot - i.shape3.xyz;
                    al *= (1.0 - smoothstep(0.45, 0.7, abs(lie.y - dot(lie.xz, i.shape2.xy)))) * smoothstep(0.30, 0.55, abs(facing.y) / max(length(facing), 1e-20));
                }
                else if (mode < 5.5)
                {
                    float ang = atan2(q.y, q.x);
                    float tk = i.shape.x, pop = i.shape.y;
                    float sq = 0.05 * sin(tk * 2.1 + seed * 30.0);
                    float2 p = q / float2(1.0 + sq, 1.0 - sq);          // it wobbles on every drawing
                    float r1 = length(p);
                    if (pop > 0.0001)
                    {
                        float ringR = 0.9 + 1.1 * pop;
                        float ring = 1.0 - Step(0.07 * (1.0 - pop) + 0.02, abs(r1 - ringR), pix);
                        ring *= step(0.35, CelHash21(float2(floor(ang * 2.2 + seed * 9.0), 3.0)));
                        float fl = (1.0 - Step(0.10 * (1.0 - pop), length(frac(p * 1.6 + seed) - 0.5), pix * 1.6))
                            * step(0.82, CelHash21(floor(p * 1.6 + seed) + 13.0)) * step(r1, ringR);
                        al = max(ring, fl) * (1.0 - pop);
                        col = light;
                    }
                    else
                    {
                        al = (1.0 - Step(0.90, r1, pix)) * 0.94;
                        col = lerp(body, light, 1.0 - Step(0.52, length(p - float2(-0.16, 0.22)), pix));
                        col = lerp(col, shade, Step(0.80, length(p - float2(-0.10, 0.14)), pix));
                        col = lerp(col, rim, Step(0.85, r1, pix) * 0.8);
                        col = lerp(col, pale, 1.0 - Step(0.17, length((p - float2(-0.34, 0.38)) * float2(1.0, 1.25)), pix));
                        // the ripple it stands in: a thin arc on the ground round its foot
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
            Name "CelAcidBeamLight"
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
            Name "CelAcidBeam"
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
