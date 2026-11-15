// Phase: CelFx fireball (2026-11-15, s01: rebuilt by eye against fireball_nolora) - one GPU pool for the whole
// fireball: the comet, the cloud it bursts into, the sparks and embers, the hit star and the light on the ground.
// Every shape is a field in the fragment; nothing is a texture (the light reads the camera's depth). The clip is
// drawn on twos, so every shape is held for two frames of a 24 fps film: the age a record is drawn at is stepped
// 12 times a second on the EFFECT's clock (Extra.w carries the seconds from the effect's start to the record's
// birth, so every record of one effect steps on the same frames).
//
// Extra.x is the mode, Extra.w the seconds from the effect's start to Born (all modes). Per mode:
//  0 comet    the head and its tail, one strip along the path it flies. Pos where it starts, Vel the launch
//             velocity, Motion.x gravity. Size x the head's radius (m), y the tail's length (m, as seen: across
//             the view), z the strip's half-width in head radii, w the seconds of flight to the hit. Life = the
//             flight + the seconds the tail takes to burn out after the hit. The head is a round banded ball
//             (red rim, orange, yellow, cream streaks, white) with a ragged red splat on its nose in its first
//             third of a second; the tail is a ribbon whose edges are flame tongues leaning back (the
//             flamethrower jet's technique), cooler toward its end, breaking into round blobs that stay where the
//             comet flew and shrink away after the hit.
//  1 drop     a ballistic fleck. Pos, Vel, Motion.x gravity, Motion.y drag. Size x radius at birth (m), y at
//             death, w its stretch (seconds of its own motion). Extra y 0 a teardrop, 1 a round fleck, 2 a leaf
//             (a point at both ends). Two tones: the palette's body, its light in the middle.
//  2 splat    the hit: a star of spikes facing the eye. Size x its half-extent (m). Extra y metres toward the eye.
//  3 cloud    the burst, on one card: a mound of round lobes, the lower ones in front. It pops out of the hit,
//             burns white and yellow with an orange rim, cools from each lobe's rim and top (red-brown line, then
//             char closing over the fire until only a crescent burns at each lobe's foot), turns to brown smoke
//             and breaks up into flecks that shrink away - all in place, on the clip's own frame clock.
//             Pos its middle, Size x its half-width at its peak (m), y its half-height, z metres it is drawn
//             toward the eye (so the ground does not cut its foot; its size on screen is kept), w how many lobes
//             (up to 28). Tone x the fire row + 256 x the smoke row.
//  7 glow     the fire's light on whatever stands behind it (the ground, the target, a crate): the lit point is
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
// _TWAcidLens (global; FireballRecipe.Lens): the scene's lens vignette, x its strength x 3, y its falloff power; 0
// none. Every colour is lifted by what the vignette will take from it, as far as white allows.
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
                    // the comet: a strip along the path, from ahead of the head back to the end of the tail
                    float T = max(r.size.w, 0.01), R = r.size.x, L = r.size.y;
                    float3 G = float3(0.0, -r.motion.x, 0.0);
                    float sH = min(a, T);                               // the head stops where it hits
                    float3 head = r.pos + r.vel * sH + 0.5 * G * sH * sH;
                    float3 tanH = normalize(r.vel + G * sH);
                    // the path runs away from the eye, so a metre along it is less than a metre across the view:
                    // the strip is laid out in metres AS SEEN, so the head stays a round ball from any side
                    float sinT = max(length(cross(tanH, normalize(_WorldSpaceCameraPos - head))), 0.25);
                    float speed = max(length(r.vel), 0.01);
                    float X = lerp(-1.7 * R, L, v.uv.x);                // metres behind the head's middle, as seen
                    float s = max(sH - X / (sinT * speed), -0.30);      // (never back to behind the eye)
                    float3 c = r.pos + r.vel * s + 0.5 * G * s * s;
                    float3 tangent = normalize(r.vel + G * s);
                    float3 side = normalize(cross(tangent, normalize(_WorldSpaceCameraPos - c)));
                    side *= side.y >= 0.0 ? 1.0 : -1.0;                 // up the screen
                    float halfW = R * max(r.size.z, 1.0);
                    w = c + side * cq.y * halfW;
                    q = float2(X, cq.y * halfW);
                    flow = float4(s * speed * sinT, 0.0, 0.0, 0.0);     // metres along the path, as seen: stays put
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
                    shape = float4(len / max(rad, 1e-4), r.extra.y, 0.0, 0.0);
                }
                else if (mode < 2.5)
                {
                    float3 p = r.pos + normalize(_WorldSpaceCameraPos - r.pos) * r.extra.y;
                    w = CelBillboard(p, cq * r.size.x);
                    shape = float4(tick, 0.0, 0.0, 0.0);
                }
                else if (mode < 6.5)
                {
                    // the cloud: one card, drawn toward the eye at the size it would have where it stands
                    float3 toEye = _WorldSpaceCameraPos - r.pos;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(r.size.z, dist * 0.5);
                    const float Card = 1.7;
                    w = CelBillboard(r.pos + toEye / dist * pull, cq * r.size.xy * Card * ((dist - pull) / dist));
                    float aspect = r.size.y / max(r.size.x, 1e-3);
                    q = cq * Card * float2(1.0, aspect);                // in the cloud's half-widths, both ways
                    shape = float4(tick, a, aspect, r.size.w);
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
                if (mode > 6.5)
                {
                    if (wholeFrame > 0.5) o.positionCS = float4(cq, 0.0, 1.0);
                    o.positionCS.z = o.positionCS.w * lerp(UNITY_NEAR_CLIP_VALUE, UNITY_RAW_FAR_CLIP_VALUE, 0.0002);   // nothing hides a light's card
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
                if (mode > 6.5) spot = SceneSpot(i);                    // a light falls on what the scene shows behind this pixel
                float3 facing = cross(ddy(spot), ddx(spot));            // which way it faces
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
                    // ---- the comet. X metres behind the head's middle, Y metres across (up the screen)
                    float R = i.shape.x, L = i.shape.y, tk = i.shape.z, after = i.shape.w;
                    float a = i.shape2.x, T = i.shape2.y;
                    float X = q.x, Y = q.y, P = i.flow.x;
                    float u = saturate(X / L);
                    float ay = abs(Y);
                    float sd = seed * 17.0;
                    float up = Y > 0.0 ? 1.0 : 0.0;
                    // the tail's half-width: the head's, narrowing to a third at its end
                    float h = R * (1.0 - 0.70 * pow(u, 1.25));
                    // its edges are rows of flame tongues that lean back and stream back off the head
                    float sl = X - a * 6.0 - ay * lerp(0.5, 0.8, up);
                    float g1 = CelSimplex3(float3(sl * 0.55, tk * 0.31, sd + lerp(7.7, 3.1, up)));
                    float g2 = CelSimplex3(float3(sl * 1.4 + 4.3, tk * 0.47, sd + lerp(5.3, 1.7, up)));
                    float g3 = CelSimplex3(float3(sl * 3.1 - 2.0, tk * 0.71, sd + lerp(2.9, 6.1, up)));
                    float rows = 0.5 + 0.5 * (g1 * 0.52 + g2 * 0.33 + g3 * 0.15);    // a wavy edge of round lobes, not spikes
                    float grow = smoothstep(0.2 * R, 2.2 * R, X);       // the head is smooth, the tail licks
                    float edge = h * (0.86 + (lerp(0.84, 1.00, up) * rows - 0.42) * grow);
                    float fTail = X > 0.0 ? (edge - ay) / max(edge, 1e-3) : -1.0;
                    fTail = min(fTail, (L - X) / (1.5 * R) + (rows - 0.6) * 0.5);
                    float fHead = 1.0 - length(float2(X, Y)) / R;
                    float f = max(fHead, fTail);
                    // it breaks into round blobs from its far end; they stay where the comet flew, and after the
                    // hit the break runs up to the head and every blob shrinks away
                    float breakX = lerp(0.46, 0.20, saturate(a / T)) * L - after * 20.0;
                    float past = (X - breakX) / L;
                    float thr = saturate(past / 0.5) * 0.55 + after * 1.5;   // 0 whole .. 1 gone
                    const float CellL = 0.36;
                    float ci = floor(P / CellL), fb = -1.0;
                    [unroll] for (int n = -1; n <= 1; n++)
                    {
                        float id = ci + n;
                        float b1 = CelHash21(float2(id, sd + 1.0)), b2 = CelHash21(float2(id + 13.0, sd + 5.0)), b3 = CelHash21(float2(id + 29.0, sd + 9.0));
                        float2 bc = float2((id + 0.3 + 0.4 * b1) * CellL, (b2 - 0.5) * 1.5 * h);
                        float rb = CellL * (0.86 - 0.76 * saturate(thr)) * (0.60 + 0.55 * b3);
                        fb = max(fb, 1.0 - length(float2(P, Y) - bc) / max(rb, 1e-3) - 0.22 * (CelValue2(float2(P, Y) * 7.0 + id * 3.1) - 0.5));
                    }
                    float mixB = saturate(past / 0.10 + after * 4.0);
                    float f2 = max(f - mixB * 1.5, min(f + 0.25, fb));
                    al = saturate(f2 / max(fwidth(f2), 1e-4) + 0.5);
                    // bands: red rim, orange, yellow, and cream and white patches along it; cooler toward the end
                    float heat = f2 * (1.0 - 0.80 * pow(u, 0.65)) * (1.0 - 0.50 * mixB);
                    float streak = CelValue2(float2((X - a * 9.0) * 0.50 + sd, Y / R * 1.9 + tk * 0.13)) * 0.7
                        + CelValue2(float2((X - a * 9.0) * 1.3 - sd, Y / R * 3.1 - tk * 0.21)) * 0.3 - 0.5;
                    float behind = exp(-pow((X - 0.9 * R) / (0.8 * R), 2.0)) * saturate(1.0 - ay / max(h, 1e-3));   // white behind the nose
                    float hot = heat * 0.75 + 0.80 * streak + 0.25 * behind;
                    float eh = max(fwidth(heat), 1e-4) * 0.75, eo = max(fwidth(hot), 1e-4) * 0.75;
                    col = rim;
                    col = lerp(col, shade, Step(0.10, heat, eh));
                    col = lerp(col, body, Step(0.24, heat, eh));
                    col = lerp(col, light, Step(0.50, hot, eo) * Step(0.34, heat, eh));
                    col = lerp(col, pale, Step(0.62, hot, eo) * Step(0.34, heat, eh));
                    // the red splat on its nose, ragged, with an orange ring; gone half a second in
                    float rs = R * 0.62 * (1.0 - smoothstep(0.08, 0.50, a));
                    float2 sp = float2(X + 0.22 * R, Y - 0.02 * R);
                    float ds = length(sp) / max(rs, 1e-3) + 0.55 * (CelValue2(sp / R * 5.0 + tk * 0.71 + sd) - 0.5) + 0.25 * (CelValue2(sp / R * 11.0 - tk * 0.37) - 0.5);
                    float es = max(fwidth(ds), 1e-4) * 0.75;
                    float inS = step(0.03 * R, rs) * Step(0.16, fHead, eh);
                    col = lerp(col, shade, inS * (1.0 - Step(1.14, ds, es)));
                    col = lerp(col, ink, inS * (1.0 - Step(0.94, ds, es)));
                }
                else if (mode < 1.5)
                {
                    float core = 0.0;
                    if (i.shape.y > 1.5)
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
                    al *= 1.0 - smoothstep(0.9, 1.0, k);
                }
                else if (mode < 2.5)
                {
                    float tk = i.shape.x;
                    al = Star(q, seed + tk * 0.137, 0.20, 0.92, 0.050, pix);
                    col = pale;
                }
                else if (mode < 6.5)
                {
                    // ---- the cloud. q in the cloud's half-widths; F is the clip's frame (the hit is frame 28)
                    float tk = i.shape.x, a = i.shape.y, aspect = i.shape.z;
                    float N = clamp(floor(i.shape.w + 0.5), 1.0, 28.0);
                    float F = 28.0 + a * 24.0;
                    float S = (1.0 - 0.55 * exp(-(F - 28.0) / 3.6)) * (1.0 + 0.06 * saturate((F - 44.0) / 44.0));
                    float brk = saturate((F - 80.0) / 24.0);            // it breaks up: 0 whole .. 1
                    float smoky = saturate((F - 62.0) / 12.0);
                    float bestKey = -1e5, bd = 2.0, br = 1.0, bid = 0.0, bcy = 0.0, inside = 0.0;
                    float2 brel = 0.0;
                    [loop] for (int j = 0; j < 28; j++)
                    {
                        float fj = j;
                        if (fj >= N) break;
                        float h1 = CelHash21(float2(fj + 1.0, seed * 91.0 + 3.0)), h2 = CelHash21(float2(fj + 17.0, seed * 53.0 + 5.0));
                        float h3 = CelHash21(float2(fj + 41.0, seed * 29.0 + 7.0));
                        // laid out in the rows of a mound, the foot first: a broad foot, a broad waist, narrowing
                        // shoulders and a head (the clip's cloud is a mound on the ground, not a ball)
                        float fr = (fj + 0.5) / N, t, y0, hw;
                        if (fr < 0.32) { t = fr / 0.32; y0 = -0.50; hw = 0.80; }
                        else if (fr < 0.62) { t = (fr - 0.32) / 0.30; y0 = -0.13; hw = 0.86; }
                        else if (fr < 0.86) { t = (fr - 0.62) / 0.24; y0 = 0.21; hw = 0.62; }
                        else { t = (fr - 0.86) / 0.14; y0 = 0.64; hw = 0.22; }
                        float2 c = float2((t * 2.0 - 1.0) * hw + (h2 - 0.5) * 0.16, (y0 + (h3 - 0.5) * 0.24) * aspect);
                        float r0 = 0.225 + 0.13 * h1 * h1;
                        float born = 28.0 + length(c - float2(0.0, -0.1)) * 4.0 + h2 * 1.5;   // the middle pops first
                        float pop = saturate((F - born) / 2.0);
                        pop = pop * (2.0 - pop);
                        // it boils: drawn a little differently on every drawing
                        float2 jit = float2(CelHash21(float2(fj, tk * 1.7 + 2.0)), CelHash21(float2(fj + 9.0, tk * 2.3 + 5.0))) - 0.5;
                        c = c * S * (1.0 + 0.30 * brk * (0.4 + h3)) + jit * 0.022 + float2(0.0, 0.10 * brk * h2);
                        float shrink = saturate((F - (78.0 + h1 * 11.0)) / (15.0 + 20.0 * h3));   // 0 whole .. 1 gone
                        float r = r0 * S * lerp(0.55, 1.0, pop) * step(0.001, pop) * (1.0 + 0.04 * sin(tk * 1.3 + fj * 2.1)) * (1.0 - shrink);
                        float2 rel = q - c;
                        float ang = atan2(rel.y, rel.x);
                        float lump = 1.0 + 0.07 * sin(ang * 3.0 + h1 * 40.0 + tk * 0.4) + (0.05 + 0.06 * smoky) * sin(ang * 5.0 + h2 * 60.0)
                            + 0.07 * smoky * sin(ang * 9.0 + h3 * 50.0 + tk * 0.3)
                            + (0.10 + 0.55 * brk) * (CelValue2(rel / max(r0, 1e-3) * 2.6 + fj * 7.3) - 0.5);
                        float rl = max(r * lump, 1e-4);
                        float d = length(rel) / rl;
                        float ca = saturate((1.0 - d) * rl / pix + 0.5) * step(0.004, r);
                        al = max(al, ca);
                        inside = max(inside, (1.0 - d) * rl * step(0.004, r));   // how deep inside the whole cloud
                        float key = c.y * 0.6 + 0.5 * h3;               // the upper ones stand in front: every lobe shows its foot
                        if (ca > 0.0 && key > bestKey) { bestKey = key; bd = d; br = rl; brel = rel; bid = fj; bcy = c.y; }
                    }
                    // the lobe in front here. Its own clock runs a little early or late; its top cools last
                    float hb = CelHash21(float2(bid + 3.0, seed * 17.0 + 1.0));
                    float lag = (hb - 0.5) * 3.0 + bcy / max(aspect, 0.1) * 1.5;
                    float Fl = F - lag * saturate((F - 40.0) / 8.0);
                    float e = pix / max(br, 1e-4);
                    float dOut = 1.0 - bd;                              // 0 on its edge .. 1 in its middle
                    float2 nq = brel / max(br, 1e-4);
                    // while it is hot only the CLOUD's edge has a warm rim: lobe on lobe there is no line at all
                    // (the clip at f36-f44); as it cools every lobe gets its own (f48 on)
                    float dRim = lerp(inside / max(br, 1e-4), dOut, saturate((Fl - 42.0) / 6.0));
                    float pop2 = (CelValue2(nq * 3.2 + bid * 13.1 + tk * 0.21) - 0.5) * 0.44 + (CelValue2(nq * 7.0 - bid * 5.7 - tk * 0.13) - 0.5) * 0.20;
                    // the fire in it: its whole face at first. As it cools a dark rim grows in from its edge and a
                    // cap of char closes over it from one side (above, a little to the left or right), its edge
                    // ragged, until only a crescent burns inside its foot; then that goes out too
                    float cool = saturate((Fl - 50.0) / 8.0);
                    float capT = smoothstep(45.0, 74.0, Fl);
                    float capA = (hb - 0.5) * 1.5;
                    float dCap = length(nq - 0.45 * float2(sin(capA), cos(capA))) - 0.96 * capT + 0.36 * pop2 * cool;
                    float gain = lerp(1.0, 3.2, smoothstep(0.50, 1.0, capT));   // the crescent keeps its bands as it thins
                    float open = 1.0 - capT; open *= open; open *= open;
                    float hv = min(dRim - 0.10 * cool, 1.4 * dCap + 3.0 * open) * gain + pop2 * smoothstep(0.0, 0.30, dRim) * 0.6;
                    float hw2 = min(dOut, hv) + pop2 * smoothstep(0.0, 0.25, dOut);   // the white: each lobe's own middle, ragged
                    float ends = saturate((Fl - 80.0) / 9.0);                    // the last of the fire goes out
                    float tw = lerp(0.55, 0.26, saturate((Fl - 30.0) / 6.0)) + 0.74 * saturate((Fl - 45.0) / 12.0);
                    float ty = 0.09 + 0.36 * saturate((Fl - 45.0) / 11.0) + 0.17 * saturate((Fl - 56.0) / 10.0) + 0.50 * saturate((Fl - 63.0) / 8.0) + ends;   // the yellow licks are gone by f70: the crescents burn red
                    float to = 0.02 + 0.22 * saturate((Fl - 45.0) / 10.0) + 0.32 * saturate((Fl - 64.0) / 20.0) + ends;
                    float tr = min(to - 0.11 * saturate((Fl - 45.0) / 5.0) - 0.09 * saturate((Fl - 55.0) / 6.0), 0.24) + ends;
                    // what is not burning: a red-brown line while it is hot, then char, then brown smoke
                    uint rowB = CelRowB(i.tone.x);
                    half3 charC = CelPal(rowB, 0), crease = CelPal(rowB, 1), smokeA = CelPal(rowB, 2), smokeB = CelPal(rowB, 3);
                    half3 shell = lerp(ink, charC, saturate((Fl - 50.0) / 6.0));
                    shell = lerp(shell, smokeA, saturate((Fl - 63.0) / 10.0));
                    shell = lerp(shell, smokeB, saturate((Fl - 76.0) / 12.0));
                    // smoke is lumpy: soft darker hollows in each lobe, and a crease of uneven width where it lies
                    // on the next
                    half3 line3 = lerp(charC, crease, 0.5 * saturate((Fl - 70.0) / 14.0));
                    float lineA = saturate((Fl - 52.0) / 5.0) * (1.0 - saturate((Fl - 86.0) / 10.0));
                    float lineW = (0.03 + 0.10 * CelValue2(nq * 2.1 + bid * 3.7)) * (1.0 - 0.5 * saturate((Fl - 66.0) / 12.0));
                    float hollow = Step(0.62, CelValue2(nq * 2.6 + bid * 9.1 + 4.0) + 0.25 * (1.0 - dOut), e * 2.6);
                    shell = lerp(shell, crease, hollow * saturate((Fl - 58.0) / 8.0) * (1.0 - saturate((Fl - 88.0) / 10.0)) * 0.8);
                    shell = lerp(shell, line3, lineA * (1.0 - Step(lineW, dOut, e)));
                    col = shell;
                    col = lerp(col, rim, Step(tr, hv, e));
                    col = lerp(col, shade, Step(to, hv, e));
                    col = lerp(col, body, Step(ty, hv, e));
                    col = lerp(col, pale, Step(tw, hw2, e));
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
