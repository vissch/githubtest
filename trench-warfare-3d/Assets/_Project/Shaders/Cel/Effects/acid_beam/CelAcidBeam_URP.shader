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
//             flight. Size x half-width at the nozzle (m), y at the target (linear between), z the age at which
//             the whole of it breaks up (s; 0: the fluid of its last Motion.w seconds breaks up as it flies), w the
//             flight seconds. Axis x, y a line's place across the beam (1 is the beam's edge: up the screen, toward the
//             eye), z its weave phase. Erode x the beam's half-width at the nozzle (m), y at the target (a line is
//             placed in those units), z dash duty 0..1, w dash seconds.
//             Extra y seconds the beam is on, z what it is: 0 a line (a ribbon broken into long dashes), 1 a line
//             with light streaks, 2 the beam: lumpy edges that flow with it, a yellow core with pale streaks, lime
//             and green round it, a spear point at its head; from the age Size.z the whole of it slows to a stop
//             and tears into a lace (yellow round its holes, green between) that thins away over Motion.w seconds.
//             Motion y the head's taper (seconds of fluid), z the tail's, w the seconds it takes to break up.
//  1 drop     a ballistic drop. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at death,
//             w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf (a point
//             at both ends), 3 a glossy blob (a ring of the light tone round the shade, a glint of the core; Erode x
//             a dark line on its shade side, y a big one opening into a C late in its life), 4 a spark
//             (a teardrop, hot along its middle), 5 a blade of goo (a round root and a long point: the root holds
//             where it was thrown from for Motion.z seconds, then follows the point; no stretch), 6 a sheet of goo
//             (round lobes along its flight run into one body with a tail behind; stretched like any drop).
//             A blade and a sheet: Tone.w their tone on the row's ladder (0 rim, 1 shade, 2 body, 3 light, 4 core,
//             5 ink), Erode x a dark line round it (0..1), y a blade's bend (-1..1), z a blade's paleness toward its
//             root / a sheet's holes (0..1).
//  2 splat    the first hit: a star of spikes facing the eye. Size x its half-extent (m). Extra y metres toward the eye.
//  3 lump     the bubbly lump the goo sinks to over the target after the beam: a round mass on a low foot, lobes
//             that boil round its outline, bays torn in it, bubbles set in it (Tone.y their palette row), yellow
//             flows down its flanks. Pos its foot. Size x width (m), w height / width. Axis the jet's level
//             direction. Motion x seconds it takes to swell up, y the share of its size it starts at, z metres it
//             is drawn toward the eye (it stands on the target's foot and hides the target). Extra z seconds
//             before its end at which it starts to go (it flattens, opens into a C, its bubbles shrink away).
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
// alone; a lump: its bubbles' palette row), z opacity (a glow: its strength), w a drop's tone (its kind's).
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
                    float T = max(r.size.w, 0.01), D = r.extra.y, B0 = r.size.z;
                    float win = max(r.motion.w, 0.01);
                    float s, e, starve;                                 // e: when this fluid left the nozzle
                    if (B0 > 0.0)
                    {
                        // (b06) the whole of it breaks up at once from the age B0, and its fluid stops flowing as it
                        // does: what left before B0 flies on 0.1 s more, then drifts at 4 % of its speed (the clip's
                        // beam turns to a lace end to end on frames 85-88 and hangs there as blobs; b02-b05 broke the
                        // last of its fluid up as it flew on, so its holed end swept into the target until frame 100)
                        float g = a > B0 ? 0.10 * (1.0 - exp(-(a - B0) / 0.10)) + 0.04 * (a - B0) : 0.0;
                        float eT = min(a, D);                           // the newest fluid
                        float eH = clamp(a > B0 ? B0 + g - T : a - T, 0.0, eT);   // the oldest still short of the target
                        e = lerp(eT, eH, v.uv.x);
                        s = a <= B0 ? a - e : e < B0 ? B0 - e + g : 0.10 * (1.0 - exp(-(a - e) / 0.10)) + 0.04 * (a - e);
                        starve = smoothstep(B0, B0 + win, a + 0.05 * sin(e * 13.0 + r.axis.z) - 0.03);
                    }
                    else
                    {
                        float sHead = min(a, T), sTail = clamp(a - D, 0.0, T);
                        s = lerp(sTail, sHead, v.uv.x);
                        e = a - s;
                        starve = smoothstep(D - win, D - 0.35 * win, e);   // the last of the beam breaks up
                    }
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float3 c = r.pos + r.vel * s + 0.5 * G * s * s;
                    float3 tangent = normalize(r.vel + G * s);
                    float3 view = normalize(_WorldSpaceCameraPos - c);
                    float3 side = normalize(cross(tangent, view));
                    side *= side.y >= 0.0 ? 1.0 : -1.0;                 // up the screen
                    float3 nrm = normalize(cross(side, tangent));
                    float kS = s / T;
                    float beam = lerp(r.erode.x, r.erode.y, kS);        // the beam's half-width here (a line is placed in it)
                    float hw;
                    if (r.extra.z > 1.5)
                    {
                        hw = lerp(r.size.x, r.size.y, kS) * lerp(1.0, 2.10, starve);   // (the clip's lace is broad; b07's 1.7 narrow)
                        hw *= 1.0 + 1.0 * smoothstep(0.86, 1.0, kS);   // it fans out into the splash where it lands (b07: wider)
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
                    flow = float4(e, kS, starve, 0.0);
                    shape = float4(D, r.erode.z, max(r.erode.w, 0.02), r.extra.z);
                    shape2 = float4(max(r.motion.y, 0.004), max(r.motion.z, 0.004), win, 0.0);
                }
                else if (mode < 1.5)
                {
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float3 p = r.pos + r.vel * CelDragInt(r.motion.y, a) + 0.5 * G * a * a;
                    float3 vel = r.vel * exp(-r.motion.y * a) + G * a;
                    float rad = CelCurve(r.size.xyz, k);
                    float2 span;
                    float len;
                    if (abs(r.extra.y - 5.0) < 0.5)
                    {
                        // a blade: its root holds where it was thrown from, then lets go and follows its point
                        float b = max(a - r.motion.z, 0.0);
                        float3 root = r.pos + r.vel * CelDragInt(r.motion.y, b) + 0.5 * G * b * b;
                        span = mul((float3x3)UNITY_MATRIX_V, p - root).xy;
                        len = max(0.5 * length(span), rad);
                        p = 0.5 * (p + root);
                    }
                    else
                    {
                        span = mul((float3x3)UNITY_MATRIX_V, vel).xy;
                        len = max(rad, length(span) * r.size.w);
                    }
                    float sp = length(span);
                    float2 d2 = sp > 1e-4 ? span / sp : float2(0.0, -1.0);
                    w = p + (vr * d2.x + vu * d2.y) * cq.x * len + (vu * d2.x - vr * d2.y) * cq.y * rad;
                    float2 ls = float2(-0.6, 0.8);                      // the light falls from the frame's upper left
                    shape = float4(len / max(rad, 1e-4), r.extra.y, dot(ls, d2), dot(ls, float2(-d2.y, d2.x)));   // (in the card's own axes)
                    shape2 = r.erode;
                    shape3 = float4(tick, 0.0, 0.0, 0.0);
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

            // a tone of a palette row's ladder, dark to light: 0 rim, 1 shade, 2 body, 3 light, 4 core; 5 the row's ink
            half3 Ladder(uint row, float t)
            {
                return t < 0.5 ? CelPal(row, 0) : t < 1.5 ? CelPal(row, 1) : t < 2.5 ? CelPal(row, 2) : t < 3.5 ? CelPal(row, 3)
                    : t < 4.5 ? CelPalCore(row).rgb : CelPalInk(row).rgb;
            }

            // a smooth union of two fields that are above 0 inside (k: how far they run together)
            float SMax(float a, float b, float k)
            {
                float h = saturate(0.5 + 0.5 * (b - a) / k);
                return lerp(a, b, h) + k * h * (1.0 - h);
            }

            // a sheet of goo in its own half-widths (x along its flight; the card runs from -aspect to aspect): three
            // round lobes run together from its head back, a thick tail from the last of them to the card's back end,
            // two horns trailing off its flanks (three in four), a hole when `holes` > 0; above 0 inside
            // (b05: the tail stops at the body - run through it, it cut the head flat at the card's edge)
            float GooSheet(float2 m, float aspect, float seed, float holes)
            {
                float f = -1.0;
                float head = aspect - 0.95, back = head - 0.55 * max(aspect - 0.6, 0.0);
                [unroll] for (int j = 0; j < 3; j++)
                {
                    float h = CelHash21(float2(j + 1.0, seed * 91.0 + 7.0)), h2 = CelHash21(float2(j + 9.0, seed * 37.0 + 3.0));
                    float2 c = float2(lerp(head, back, j * 0.5), (h2 - 0.5) * 0.30);
                    f = SMax(f, (0.60 - 0.10 * j) * (0.82 + 0.30 * h) - length(m - c), 0.25);
                }
                float tx = saturate((m.x + aspect) / max(back + aspect, 0.3));
                f = SMax(f, 0.42 * pow(tx, 0.8) - abs(m.y - 0.20 * sin(tx * 3.5 + seed * 19.0) * (1.0 - tx)) - 2.0 * max(m.x - back, 0.0), 0.2);
                [unroll] for (int n = 0; n < 2; n++)
                {
                    float h = CelHash21(float2(n + 13.0, seed * 47.0 + 1.0)), h2 = CelHash21(float2(n + 17.0, seed * 23.0 + 2.0));
                    float h3 = CelHash21(float2(n + 19.0, seed * 61.0 + 3.0));
                    float sd = n == 0 ? 1.0 : -1.0;
                    float2 p = m - float2(back + 0.15, sd * (0.30 + 0.20 * h));
                    float ang = 2.7 + 0.45 * h2;                         // backward and out
                    float2 d = float2(cos(ang), sd * sin(ang));
                    float t = dot(p, d) / max(min(0.5 + 0.9 * h3, (back + 0.15 + aspect) * 0.9), 1e-3);
                    float horn = t >= 0.0 && t <= 1.0 && h3 > 0.25 ? (0.16 + 0.10 * h) * pow(saturate(1.0 - t), 0.9) - abs(p.x * d.y - p.y * d.x) : -1.0;
                    f = max(f, horn);
                }
                if (holes > 0.01)
                {
                    float h = CelHash21(float2(3.0, seed * 77.0 + 1.0)), h2 = CelHash21(float2(5.0, seed * 59.0 + 2.0));
                    float2 c = float2(lerp(head, back, 0.3 + 0.5 * h), (h2 - 0.5) * 0.35);
                    f = min(f, length(m - c) - (0.10 + 0.12 * CelHash21(float2(7.0, seed * 31.0))) * holes);
                }
                return f;
            }

            // the lump's mass of goo in its half-widths (x across, y up from its foot): a round body, eight lobes
            // round its outline that boil (new on every drawing), a low foot, bays torn in its outline (two on its far
            // side; never a round hole inside it: one reads as an eye), and a hollow off its middle as it goes (`opn`
            // 0..1: it opens into a C); above 0 inside
            float Lump(float2 m, float seed, float tk, float opn)
            {
                float f = (1.0 - length(float2(m.x / 0.80, (m.y - 0.80) / 0.74))) * 0.74;
                [unroll] for (int n = 0; n < 8; n++)
                {
                    float h1 = CelHash21(float2(n + 31.0, seed * 13.0 + 2.0)), h2 = CelHash21(float2(n + 37.0, seed * 19.0 + 6.0));
                    float an = 6.2831853 * (n + 0.25 + 0.5 * h1) / 8.0;
                    float r = (0.16 + 0.18 * h2) * (1.0 + 0.18 * (CelValue2(float2(tk * 0.5 + n * 2.3, seed * 5.0)) - 0.5));
                    f = SMax(f, r - length(m - float2(0.704 * cos(an), 0.80 + 0.651 * sin(an))), 0.10);
                }
                f = SMax(f, (1.0 - length(float2(m.x / 0.72, (m.y - 0.16) / 0.18))) * 0.18, 0.12);   // (b07: inside its card)
                [unroll] for (int b = 0; b < 3; b++)
                {
                    float an = 0.9 * (CelHash21(float2(b + 41.0, seed * 23.0 + 1.0)) - 0.5) + (b < 2 ? 0.0 : 3.1415927);
                    float2 c = float2(0.816 * cos(an), 0.80 + 0.755 * sin(an));
                    f = min(f, length((m - c) * float2(1.0, 0.8)) - (0.10 + 0.10 * CelHash21(float2(b + 47.0, seed * 7.0 + 3.0))));
                }
                return min(f, length((m - float2(0.62, 0.95)) * float2(1.0, 0.85)) - 0.58 * opn);   // (b07: a bay, b06's read as a donut)
            }

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
                level *= 1.0 - smoothstep(0.35, 0.70, d.y);       // and low: not the top of the target's head (b04 lit it from frame 30)
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
                    float e = i.flow.x, kS = i.flow.y, starve = i.flow.z;   // (the vertex's: how far it has broken up)
                    float D = i.shape.x, duty = i.shape.y, period = i.shape.z, kind = i.shape.w;
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
                            // (b09: warped and fewer - b08's round holes in rows read as a leopard band)
                            float2 wp = float2(q.x * 1.6, e / 0.055) + seed * 7.0;
                            wp += 0.45 * (float2(CelValue2(wp * 0.9 + seed * 3.0), CelValue2(wp * 0.9 + seed * 5.0 + 7.0)) - 0.5);
                            float2 wc = floor(wp), wf = frac(wp);
                            float gap = 8.0;                             // how far this point is outside every hole
                            [unroll] for (int yy = -1; yy <= 1; yy++)
                            [unroll] for (int xx = -1; xx <= 1; xx++)
                            {
                                float2 o2 = float2(xx, yy);
                                float2 cid = wc + o2;
                                float2 pt = o2 + float2(CelHash21(cid + 3.1), CelHash21(cid + 17.7)) * 0.8 + 0.1 - wf;
                                float hr = (0.18 + 0.42 * CelHash21(cid + 29.3)) * (0.25 + 1.25 * starve);   // (they grow until they meet)
                                gap = min(gap, length(pt) - hr);
                            }
                            // its ropes thin and part, then its islands go (b06: it is gone when it has broken up)
                            al *= Step(0.18 * smoothstep(0.5, 1.0, starve), gap, pixX * 2.0) * (1.0 - smoothstep(0.85, 1.0, starve));
                            border = saturate(1.0 - gap / 0.30);         // 1 at a hole's rim, 0 deep in the fluid
                        }
                        // across it (b09): long thin streaks of yellow, lime, green and dark green that flow with it,
                        // 2-4 m long and slanted a little, the yellow low in it and the green high (the clip's beam is a
                        // bundle of streaks about a fifth yellow; b02-b08 drew concentric bands, 57 % yellow)
                        float pd = pixX / max(edge, 0.05) * 1.6;
                        float u = q.x * 0.5 + 0.5;
                        float st = 0.55 * CelValue2(float2(u * 9.0 + e * 3.0 + seed * 3.0, e * 6.0 + seed * 5.0))
                            + 0.45 * CelValue2(float2(u * 24.0 - e * 5.0 + seed * 7.0, e * 11.0 + seed * 11.0));
                        float d = abs(q.x + 0.20) / max(edge + 0.10, 0.02) + 1.10 * (st - 0.5);
                        // (the lace: yellow round its holes, then green deep in it - the clip's; b03-b05 drew
                        // green-rimmed holes in a lime sheet, a leopard)
                        d = lerp(d, 0.80 - 0.62 * border, smoothstep(0.0, 0.45, starve));
                        // (b07) its end pales into a fan of yellow where it goes in (the clip's pale-yellow heart, f40-84)
                        d *= 1.0 - 0.55 * smoothstep(0.86, 1.0, kS) * (1.0 - starve);
                        // (b10: a thin lime band only - the clip's streaks are yellow against green; b09's broad lime
                        // band took the contrast out)
                        col = lerp(rim, shade, 1.0 - Step(0.72, d, pd));
                        col = lerp(col, body, 1.0 - Step(0.33, d, pd));
                        col = lerp(col, light, 1.0 - Step(0.26, d, pd));
                        col = lerp(col, hot, 1.0 - Step(0.08, d, pd));
                        // pale streaks in its yellow, flowing with it
                        float sk = Streaks(float2(u * 7.0 + seed * 3.0, e / 0.045 + seed * 5.0), seed, 0.55, 0.30, pixX * 3.5);
                        col = lerp(col, streak, sk * (1.0 - smoothstep(0.18, 0.32, d)));
                        // its edges fray into strands with the night between them (b09)
                        float fray = CelValue2(float2(u * 18.0 + seed * 9.0, e * 10.0 + seed * 2.0));
                        al *= Step(0.0, 0.40 * fray + 0.80 - abs(q.x) / max(edge, 0.02), pixX * 2.0 / max(edge, 0.05));
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
                    float ts = i.tone.w;
                    if (i.shape.y > 4.5)
                    {
                        float aspect = max(i.shape.x, 1.0), tk = i.shape3.x;
                        float2 L = i.shape.zw;                          // the light, in the card's axes (x along it, y across)
                        half3 base = Ladder(row, ts), up1 = Ladder(row, min(ts + 1.0, 4.0)), dn1 = Ladder(row, max(min(ts, 4.0) - 1.0, 0.0));
                        float f;
                        if (i.shape.y > 5.5)
                        {
                            // a sheet of goo flying (GooSheet); its outline wobbles as it flies (a little new on every
                            // drawing)
                            float2 m = float2(q.x * aspect, q.y);       // in its half-widths
                            m += 0.16 * (float2(CelValue2(m * 1.6 + seed * 23.0 + tk * 0.23), CelValue2(m * 1.6 + seed * 41.0 - tk * 0.19)) - 0.5);
                            f = GooSheet(m, aspect, seed, i.shape2.z);
                            al = Step(0.0, f, pixY);
                            // its tone, a tone up where it faces the light and a tone down where it faces away (its own
                            // outline moved toward the light and away from it: the bands follow its shape), a pale
                            // glint on its head on one in two (b05's first try split the tones along straight lines)
                            float fl = GooSheet(m + L * 0.38, aspect, seed, i.shape2.z), fs = GooSheet(m - L * 0.26, aspect, seed, i.shape2.z);
                            col = base;
                            col = lerp(col, up1, 1.0 - Step(0.0, fl, pixY));
                            col = lerp(col, dn1, (1.0 - Step(0.0, fs, pixY)) * 0.9);
                            float2 g = m - float2(aspect - 0.95, (CelHash21(float2(9.0, seed * 37.0 + 3.0)) - 0.5) * 0.30) - L * 0.30;
                            col = lerp(col, pale, (1.0 - Step(0.13, length(g * float2(1.0, 1.5)), pixY)) * step(0.5, CelHash21(float2(seed * 13.0, 5.0))));
                        }
                        else
                        {
                            // a blade of goo: a round root, a long point, bent a little to one side, its flanks a
                            // little ragged (new every second drawing), one in two with a short spike off a flank;
                            // pale toward its root where it leaves the hot heart (Erode.z), a lighter streak along
                            // its lit flank, its other flank a tone down
                            float u = q.x * 0.5 + 0.5;                  // 0 its root .. 1 its point
                            float yb = q.y - i.shape2.y * 0.36 * u * u;
                            float side = yb >= 0.0 ? 1.0 : -1.0;
                            float rag = CelValue2(float2(u * aspect * 2.2 + seed * 17.0, side * 3.0 + seed * 5.0 + floor(tk / 2.0) * 0.61)) - 0.5;
                            float wd = 0.62 * pow(saturate(1.0 - u), 0.8) * sqrt(saturate((q.x + 1.0) * aspect / 0.55)) * (1.0 + 0.35 * rag);   // (a round root whatever its length)
                            f = wd - abs(yb);
                            float hs = CelHash21(float2(seed * 53.0, 7.0));
                            float u0 = 0.25 + 0.3 * CelHash21(float2(seed * 29.0, 3.0));
                            float sideS = hs < 0.25 ? -1.0 : 1.0;
                            float w0 = 0.62 * pow(1.0 - u0, 0.8);
                            float2 pS = float2(q.x * aspect, yb) - float2((u0 * 2.0 - 1.0) * aspect, sideS * w0 * 0.6);
                            float2 dS = float2(0.876, sideS * 0.482);
                            float tS = dot(pS, dS) / max(min((1.0 - u0) * aspect * 0.55, 0.9), 1e-3);
                            float spike = tS >= 0.0 && tS <= 1.0 && hs < 0.5 ? w0 * 0.45 * pow(saturate(1.0 - tS), 0.9) - abs(pS.x * dS.y - pS.y * dS.x) : -1.0;
                            f = max(f, spike);
                            al = Step(0.0, f, pixY);
                            float lit = L.y >= 0.0 ? 1.0 : -1.0;        // the flank the light falls on
                            float yn = yb / max(wd, 0.05), pn = pixY / max(wd, 0.05);
                            col = base;
                            col = lerp(col, dn1, Step(0.35, -yn * lit, pn) * 0.9);
                            col = lerp(col, up1, (1.0 - Step(0.28, abs(yn - 0.30 * lit), pn)) * step(0.12, u) * (1.0 - step(0.9, u)));
                            col = lerp(col, pale, (1.0 - smoothstep(0.08, 0.45, u)) * i.shape2.z);
                        }
                        col = lerp(col, rim * 0.55, (1.0 - Step(1.6 * pixY, f, pixY)) * i.shape2.x);   // a dark line round it
                    }
                    else if (i.shape.y > 3.5)
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
                        // a glossy blob (the clip's, on the beam's top f22-48 and where it breaks up f86-110): a ring
                        // of the light tone, thicker toward the light and the ink on its shade side, the shade inside
                        // it, a glint of the core a little toward the light and a pale arc inside the ring on its lit
                        // side, a dark line on its shade side (Erode.x), an outline that wobbles a little; a big one
                        // opens into a C on its shade side late in its life (Erode.y) (b02-b05: a pale ball with a dark
                        // ring, lit from the card's own axes, which turn with its flight)
                        float2 L = i.shape.zw;
                        float2 p = float2(q.x * max(i.shape.x, 1.0) / max(i.shape.x * 0.5 + 0.5, 1.0), q.y);
                        float tk = i.shape3.x, an = atan2(p.y, p.x);
                        float r1 = length(p) * (1.0 + 0.045 * sin(an * 3.0 + seed * 40.0 + tk * 0.35) + 0.025 * sin(an * 5.0 + seed * 23.0 - tk * 0.5));
                        float cl = dot(p, L) / max(length(p), 1e-4);   // 1 toward the light, -1 away
                        al = 1.0 - Step(0.92, r1, pix);
                        float ringIn = 0.77 - 0.07 * cl;
                        float ring = Step(ringIn, r1, pix);
                        col = lerp(shade, light, ring);
                        col = lerp(col, CelPalInk(row).rgb, ring * (1.0 - smoothstep(-0.65, -0.15, cl)));
                        col = lerp(col, pale, (1.0 - Step(0.04, abs(r1 - ringIn + 0.085), pix)) * smoothstep(0.56, 0.68, cl));
                        col = lerp(col, pale, 1.0 - Step(0.21, length(p - L * 0.16), pix));
                        col = lerp(col, rim, Step(0.86, r1, pix) * (1.0 - smoothstep(-0.6, -0.25, cl)) * i.shape2.x);
                        if (i.shape2.y > 0.01)
                            al *= Step(0.72 * smoothstep(0.55, 0.95, k) * i.shape2.y, length((p + L * 0.28) * float2(1.0, 1.15)), pix);
                    }
                    else if (i.shape.y > 1.5)
                    {
                        float wd = pow(saturate(1.0 - q.x * q.x), 0.75);    // a leaf: a point at both ends
                        al = (1.0 - Step(wd * 0.92, abs(q.y), pixY)) * step(0.03, wd);
                        // its tone (Tone.w: 0 body, 1 shade, 2 light, 3 the row's ink, 4 rim), a lighter streak down
                        // its middle, a little off to one side (pale on the light ones, a tone up on the dark ones)
                        half3 base = ts < 0.5 ? body : ts < 1.5 ? shade : ts < 2.5 ? light : ts < 3.5 ? CelPalInk(row).rgb : rim;
                        half3 lit = ts < 1.5 ? lerp(base, light, 0.5) : ts > 3.5 ? shade : lerp(base, pale, 0.65);
                        col = lerp(base, lit, 1.0 - Step(0.28 * wd, abs(q.y - 0.18 * wd), pixY));
                    }
                    else if (i.shape.y > 0.5)
                    {
                        al = 1.0 - Step(0.9, length(q), pix);           // (its tone, as a leaf's)
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
                    // the bubbly lump the goo sinks to after the beam (the clip's frames 94-110; Lump): a tone up
                    // toward the light and down away from it, patches, the bubbles set in it each over the last
                    // (their own row, Tone.y: a light inside, a pale crescent toward the light, a glint, a dark line
                    // on the shade side of some; each shrinks away at its own time as it goes), yellow flows down its
                    // lower flanks, a dark line round it (b02-b05: a pillar of goo with tongues at its top - it read
                    // as a tuft of grass and a tent, a third of the clip's size)
                    float tk = i.shape.x, rise = i.shape.y, sink = i.shape.z, Hh = i.shape2.y;
                    float2 L = float2(-0.6, 0.8);                       // the light, from the frame's upper left (the card faces the eye)
                    float grow = 0.45 + 0.55 * smoothstep(0.0, 1.0, rise), flt = 0.40 + 0.60 * smoothstep(0.0, 1.0, sink);
                    float sx = grow * (0.85 + 0.15 * flt), sy = grow * flt;
                    // in its half-widths: it swells up from its foot, flattens as it goes, and keeps inside its card
                    // (b06's lobes ran out of its card's side and foot: straight cuts)
                    float2 m = float2(q.x / sx, q.y * Hh / sy) / 0.86;
                    float mp = max(pixX / sx, pixY * Hh / sy) / 0.86;
                    float opn = 1.0 - smoothstep(0.15, 0.75, sink);
                    float wob = 0.03 * (CelValue2(float2(atan2(m.y - 0.80, m.x) * 2.5 + seed * 9.0, tk * 0.7)) - 0.5);
                    float f = Lump(m, seed, tk, opn) + wob;
                    al = Step(0.0, f, mp) * step(0.0, q.y);
                    float fl = Lump(m + L * 0.10, seed, tk, opn) + wob, fs = Lump(m - L * 0.22, seed, tk, opn) + wob;   // (b07: deeper)
                    col = lerp(body, light, 1.0 - Step(0.0, fl, mp));
                    col = lerp(col, shade, (1.0 - Step(0.0, fs, mp)) * 0.9);
                    float pn = CelValue2(float2(m.x * 6.5 + seed * 5.0 + tk * 0.05, m.y * 6.5 + seed * 3.0));
                    float deep = Step(0.05, f, mp);
                    col = lerp(col, light, Step(0.68, pn, mp * 6.5) * deep);
                    col = lerp(col, lerp(light, pale, 0.6), Step(0.80, pn, mp * 6.5) * deep);
                    col = lerp(col, shade, Step(0.60, 1.0 - pn, mp * 6.5) * deep * 0.9);
                    col = lerp(col, rim, Step(0.80, 1.0 - pn, mp * 6.5) * deep * 0.5);   // (b08: the clip's lump has pale and dark patches)
                    uint row2 = CelRowA(i.tone.y);
                    half3 bRim = CelPal(row2, 0), bShade = CelPal(row2, 1), bIn = CelPal(row2, 2), bLit = CelPal(row2, 3), bGlint = CelPalCore(row2).rgb;
                    [loop] for (int j = 0; j < 20; j++)
                    {
                        float h1 = CelHash21(float2(j + 1.0, seed * 91.0 + 3.0)), h2 = CelHash21(float2(j + 7.0, seed * 37.0 + 5.0));
                        float h3 = CelHash21(float2(j + 13.0, seed * 53.0 + 1.0));
                        float rb = j < 2 ? 0.20 + 0.10 * h3 : 0.05 + 0.11 * h3 * h3;
                        float rho = j < 2 ? 0.10 + 0.35 * h2 : 0.20 + 0.75 * h2;
                        float2 c = float2(0.80 * rho * cos(6.2831853 * h1), max(0.80 + 0.74 * rho * sin(6.2831853 * h1), 0.25));
                        c += 0.035 * (float2(CelValue2(float2(tk * 0.45 + j * 3.1, seed * 7.0)), CelValue2(float2(tk * 0.45 + j * 5.3, seed * 11.0 + 4.0))) - 0.5);
                        rb *= (1.0 + 0.12 * (CelValue2(float2(tk * 0.6 + j * 1.7, seed * 3.0 + 8.0)) - 0.5))
                            * (1.0 - smoothstep(0.0, 1.0, (1.0 - sink) * 1.7 - CelHash21(float2(j + 19.0, seed * 29.0))));
                        float2 dp = m - c;
                        float d = length(dp);
                        half3 c2 = lerp(bIn, bLit, Step(rb * 0.78, length(dp + L * rb * 0.18), mp));
                        c2 = lerp(c2, bShade, Step(rb * 0.86, length(dp - L * rb * 0.16), mp) * 0.8);
                        c2 = lerp(c2, bGlint, 1.0 - Step(rb * 0.17, length(dp - L * rb * 0.42), mp));
                        c2 = lerp(c2, bRim, Step(rb * 0.94 - 1.6 * mp, d, mp) * step(dot(dp, L), 0.15 * rb) * step(CelHash21(float2(j + 23.0, seed * 17.0)), 0.55));
                        col = lerp(col, c2, (1.0 - Step(rb * 0.94, d, mp)) * step(0.02, rb) * Step(0.0, f, mp));
                    }
                    float fy = CelValue2(float2(m.x * 7.0 + seed * 13.0, m.y * 1.5 - tk * 0.2));
                    col = lerp(col, CelPalInk(row).rgb, Step(0.50, fy, mp * 7.0) * smoothstep(0.20, 0.60, abs(m.x)) * (1.0 - smoothstep(0.45, 1.25, m.y)) * Step(0.02, f, mp));
                    col = lerp(col, rim, 1.0 - Step(1.8 * mp, f, mp));
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
                    // smoke: no line round it, a soft ragged edge, a veil the background shows through as it ages (b08:
                    // the clip's goes from a soft lumpy band on the beam at f50-60 to see-through wisps by f84; b02-b07's
                    // were opaque cotton puffs to the end)
                    al = smoothstep(0.0, 0.14, cut) * lerp(1.0, 0.45, smoothstep(0.35, 1.0, k));
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
                    col = lerp(col, rim, (1.0 - smoothstep(0.0, 0.045, abs(n + 0.10 * strand - 0.10))) * smoothstep(0.08, 0.20, cut) * 0.45);   // dark curls in it
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
