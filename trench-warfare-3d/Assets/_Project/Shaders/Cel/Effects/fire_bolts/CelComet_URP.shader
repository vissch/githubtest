// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; c05: the flame is a noise-warped teardrop SDF coloured by heat distance; b04: a small round head tapering into a long thin red-orange flame streak; b03: flame body twice as wide, wider yellow band) - Comet: a fire bolt drawn as one camera-facing quad along
// its velocity, an SDF in the fragment: a white-yellow head with a soft yellow bloom, a flame tail banded yellow to
// orange to deep red that tapers smoothly to a point, a dark ink line on the screen-lower edge, and behind it a thin
// wavy smoke ribbon with an ink edge that thins to a point and breaks up in steps. With no head and no flame it is a
// curling smoke wisp rising from a fire, drawn as a chain of inked grey cel puffs. Premultiplied alpha: the fire is
// nearly opaque colour, the ink darkens what is behind it, the ribbon is part covering, the bloom is additive.
// Mode extra.y < 0 (no head): a thin curling smoke strand (the ribbon alone, no ink), widening toward its newest end
// and breaking up in pieces over its life.
// Mode extra.x = -4 (p12): a smooth camera-facing additive flash (a gaussian, no bands); size = radius curve,
// tone.z = brightness, tone.y = the white-hot middle.
// Mode extra.x < 0: a soft warm light pool lying on the ground (additive, a smooth radial gradient, no bands), whose
// brightness rises to a peak and fades smoothly over the life.
//   size.xyz   ribbon width (m) at birth, at death, ease        size.w  ribbon length in seconds of its speed
//              (b02: negative = the same length, drawn behind the birth too: a bolt already in flight)
//              (pool: radius at birth, at death, ease)
//   motion     gravity, drag, z: the ribbon's wave speed (rad/s, the curl travels along it), buoyancy
//   erode.xyz  the ribbon's break-up threshold over the life (0 whole .. 1 gone): it breaks into pieces in steps
//   tone       x flame palette (+ 256 x the ribbon palette), y HDR core, z ribbon opacity (pool: brightness), w ink px
//   extra      x head radius (m, 0: no head; < 0: pool), y flame tail length (m, 0: no flame; pool: falloff power),
//              z ribbon wave amplitude (m; pool: life fraction of the peak), w ribbon wavelength (m; pool: life
//              fraction where the fade starts)
Shader "TW/Cel/Comet"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+18" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelComet"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 local : TEXCOORD0;                         // x metres behind the head, y metres across (+ screen up); pool: -1..1
                nointerpolation float4 dims : TEXCOORD1;          // x length, y head radius, z flame length, w wave amplitude
                nointerpolation float4 tone : TEXCOORD2;          // x palettes, y hdr, z ribbon opacity, w ink px
                nointerpolation float4 more : TEXCOORD3;          // x ribbon width, y erosion threshold, z seed, w wavelength
                float4 misc : TEXCOORD4;                          // x fog, y wave speed, z age, w 1 = pool
                float3 positionWS : TEXCOORD5;
            };

            // d01 path ribbon: the wisp's offset at height h (m) and age t: x along the camera's right, y up, z depth.
            // A drift right that grows with height and time, plus two travelling sines whose amplitude grows with
            // height (an S that widens as it rises), plus a slow depth wobble.
            float3 WispPath(CelRecord r, float h, float t)
            {
                float amp = r.extra.y, drift = r.extra.z, f = max(r.extra.w, 0.5), ws = r.motion.z;
                float ph = r.seed * 37.0;
                float g = amp * (0.25 + h);
                float x = drift * h * (0.6 + 0.25 * t)
                        + g * sin(h * f + ph - t * ws)
                        + g * 0.3 * sin(h * f * 1.6 + ph * 1.7 - t * ws * 1.4);
                float z = g * 0.5 * sin(h * f * 0.7 + ph * 2.3 - t * ws * 0.8);
                return float3(x, h, z);
            }

            // p21: a bolt's path: its ballistic arc plus a swirl across it (amplitude A m, rate motion.z rad/s, phase from
            // its seed) that is zero at the launch and at the hit, so it still lands where it is aimed. The strip's
            // vertices sit on this path at the time the head passed them, so the tail bends with it.
            #define BOLT_HOLD 0.3   // p27j: a bolt lives BOLT_HOLD s past its hit (FireBoltsRecipe.Hold), its head frozen there while the tail streams in
            float3 BoltPath(CelRecord r, float t, float A)
            {
                float3 q = t >= 0.0 ? CelPosition(r, t) : r.pos + r.vel * t;   // p26: straight back before the birth (CelPosition clamps at 0: the rear columns collapsed into a hard end)
                float k = t / max(r.life - BOLT_HOLD, 1e-3);   // p27j: the swirl is zero at the hit, not at the end of the hold
                float env = saturate(4.0 * k * (1.0 - k));
                float3 v = t >= 0.0 ? CelVelocity(r, t) : r.vel;   // p26
                float vl = dot(v, v);
                float3 d = vl > 1e-6 ? v * rsqrt(vl) : float3(1, 0, 0);
                float3 sd = cross(d, float3(0, 1, 0));
                float sl = dot(sd, sd);
                sd = sl > 1e-6 ? sd * rsqrt(sl) : float3(0, 0, 1);
                float3 ud = cross(sd, d);
                float ph = r.motion.z * t + r.seed * 6.2832;
                return q + A * env * (sin(ph) * ud + 0.4 * cos(ph) * sd);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                Varyings o;
                if (r.extra.x < -3.5)
                {
                    // p12 soft flash: a camera-facing quad, a smooth radial glow in the fragment
                    float R = CelCurve(r.size.xyz, k) * alive;
                    float2 c = v.uv * 2.0 - 1.0;
                    float3 w = CelBillboard(p, c * R);
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.local = c;
                    o.dims = float4(k, r.extra.y, 0.0, 0.0);   // p22: extra.y = 1 -> a hard ragged splash
                    o.tone = r.tone;
                    o.more = float4(R, 0.0, r.seed, 0.0);
                    o.misc = float4(ComputeFogFactor(o.positionCS.z), 0.0, t, 4.0);
                    return o;
                }
                if (r.extra.x < -2.5)
                {
                    // d25 cel blob (mode extra.x = -3): a camera-facing quad holding a metaball SDF of five balls
                    // that drift apart, curl and grow over the life; the outline is eaten by travelling noise, bands
                    // come from the field's own gradient (lit upper-left), the ink is a real px line on the iso edge.
                    // size.xyz = radius curve (m); erode.xyz = iso rise over the life (tears into pieces);
                    // extra.y = ball spread at death (radii), extra.z = curl (rad/s), extra.w = outline noise amount.
                    float R = CelCurve(r.size.xyz, k) * alive;
                    float2 c = v.uv * 2.0 - 1.0;
                    float3 w = CelBillboard(p, c * R * 1.9);
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.local = c * 1.9;
                    o.dims = float4(k, max(r.extra.y, 0.3), r.extra.z, max(r.extra.w, 0.0));
                    o.tone = r.tone;
                    o.more = float4(R, CelCurve(r.erode.xyz, k), r.seed, 0.0);
                    o.misc = float4(ComputeFogFactor(o.positionCS.z), 0.0, t, 3.0);
                    return o;
                }
                if (r.extra.x < -1.5)
                {
                    // d01 path ribbon (smoke wisp): a strip whose every vertex sits on a curling path rising from the
                    // record's position. uv.x 0 = the base, 1 = the newest top; the top climbs over the life, the S
                    // travels up it and grows with height, the whole wisp drifts along the camera's right.
                    float hMax = max(r.size.w, 0.1);
                    float top = hMax * (1.0 - exp(-t * 3.6 / hMax));
                    float3 R = float3(UNITY_MATRIX_V[0].x, 0.0, UNITY_MATRIX_V[0].z);
                    R = dot(R, R) > 1e-6 ? normalize(R) : float3(1, 0, 0);
                    float3 F = float3(-R.z, 0.0, R.x);
                    float u0 = v.uv.x;
                    float3 qa = WispPath(r, u0 * top, t), qb = WispPath(r, u0 * top + 0.35, t);   // d07: a smoothed tangent (less folding)
                    float3 pa = p + R * qa.x + float3(0, qa.y, 0) + F * qa.z;
                    float3 pb = p + R * qb.x + float3(0, qb.y, 0) + F * qb.z;
                    float3 tng = pb - pa;
                    tng = dot(tng, tng) > 1e-8 ? normalize(tng) : float3(0, 1, 0);
                    float3 toEye = normalize(_WorldSpaceCameraPos - pa);
                    float3 side = cross(tng, toEye);
                    float sl = dot(side, side);
                    side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                    if (dot(side, UNITY_MATRIX_V[0].xyz) < 0.0) side = -side;   // +across = screen right
                    // width: thin at the base, widening up the wisp and over the life
                    float wid = lerp(r.size.x, r.size.y, pow(saturate(k), max(r.size.z, 0.1))) * lerp(0.45, 1.0, u0);
                    float across = (v.uv.y * 2.0 - 1.0) * 2.0;   // d07: room for the puffs
                    float3 w = pa + side * across * wid * 0.5 * alive;
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.local = float2(u0, across);
                    o.dims = float4(top, wid, k, 0.0);
                    o.tone = r.tone;
                    o.more = float4(wid, CelCurve(r.erode.xyz, k), r.seed, 0.0);
                    o.misc = float4(ComputeFogFactor(o.positionCS.z), 0.0, t, 2.0);
                    return o;
                }
                if (r.extra.x < 0.0)
                {
                    // the pool: a level quad on the ground, nudged toward the eye so small bumps do not cut it
                    float radius = CelCurve(r.size.xyz, k) * alive;
                    float2 c = v.uv * 2.0 - 1.0;
                    float3 w = p + float3(c.x, 0.0, c.y) * radius + float3(0.0, 0.04, 0.0);
                    float3 toEye = _WorldSpaceCameraPos - w;
                    w += normalize(toEye) * min(0.7, length(toEye) * 0.1);
                    // brightness over the life: a soft start, a rise to the peak, a smooth fade to nothing
                    float env = smoothstep(0.03, 0.06, k) * lerp(0.4, 1.0,   /* d06: dark until the first hit (f17) */ smoothstep(0.02, max(r.extra.z, 0.03), k))
                              * (1.0 - smoothstep(max(r.extra.w, r.extra.z), 1.0, k));
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.local = c;
                    o.dims = float4(0.0, 0.0, max(r.extra.y, 0.2), 0.0);
                    o.tone = float4(r.tone.x, r.tone.y, r.tone.z * env, 0.0);
                    o.more = float4(0.0, 0.0, r.seed, 1.0);
                    o.misc = float4(ComputeFogFactor(o.positionCS.z), 0.0, t, 1.0);
                    return o;
                }
                float headR = max(r.extra.x, 0.0), flameL = max(r.extra.y, 0.0), amp = abs(r.extra.z);
                float tHit = (headR > 0.0 && r.extra.w >= 0.0) ? r.life - BOLT_HOLD : 1e9;   // p27j: a bolt's head stops at its hit
                float tc = min(t, tHit);
                p = CelPosition(r, tc);
                float3 vel = CelVelocity(r, tc);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : float3(0, 1, 0);
                float rw = CelCurve(r.size.xyz, k);
                float adv = max(t - tHit, 0.0) * speed;   // p27j: metres of tail already swallowed by the hit
                float swirlA = (headR > 0.0 && r.extra.w >= 0.0 && speed > 1.0) ? amp : 0.0;   // p21: a bolt's extra.z is its swirl amplitude
                if (headR > 0.0) amp = 0.08;   // p22: the smear waves a little
                float halfW = max(max(headR * 3.5, amp * 1.2 + rw), 0.01) * alive;   // c01: room for the 3.4x bloom
                float len = max(speed * abs(r.size.w), flameL);
                if (r.size.w >= 0.0) len = min(len, distance(p, r.pos));   // no ribbon behind its birth (b02: a negative
                                                                           // size.w is a bolt already in flight: whole tail)
                float front = max(headR * 5.0, rw) * alive;   // p24b: more room ahead of the ball
                len *= alive;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;   // +y is up on the screen: the ink goes below
                float back = (headR + flameL) > 0.0 ? 0.0 : rw * alive;   // a wisp's last puff is round past its end
                float along = lerp(-len - back, front, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * halfW;
                float3 pa = p + dir * along;
                if (swirlA > 0.0)
                {
                    // p21: on the bolt's own path, at the time its head was `along` metres back
                    float ta = min(tc + (along - adv) / speed, tHit);   // p27j: past the hit the strip shows the tail that is still arriving
                    pa = BoltPath(r, ta, swirlA);
                    float3 pb = BoltPath(r, min(ta + 0.02, tHit), swirlA);
                    float3 tg = pb - pa;
                    dir = dot(tg, tg) > 1e-10 ? normalize(tg) : dir;
                    toEye = normalize(_WorldSpaceCameraPos - pa);
                    side = cross(dir, toEye);
                    sl = dot(side, side);
                    side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                    if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;
                }
                float3 w = pa + side * across;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.local = float2(-along + adv, across);   // p27j: the fragment shades the tail by its distance behind the (frozen) head
                o.dims = float4(len, headR, flameL, amp);
                o.tone = r.tone;
                o.more = float4(rw, CelCurve(r.erode.xyz, k), r.seed, max(abs(r.extra.w), 0.05));
                // p16: extra.w < 0 = a standing flame (a ground fire): misc.w = 0.25 + 0.5 k so the fragment can fade it
                o.misc = float4(ComputeFogFactor(o.positionCS.z), r.motion.z, t, r.extra.w < 0.0 ? -2.0 - k : ((r.extra.y < 0.0 && headR <= 0.0) ? -1.0 : 0.0));   // p17: -2..-3 (p16's 0.25+0.5k fell into the pool branch past half life: a tan box)
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint rowA = CelRowA(i.tone.x), rowB = CelRowB(i.tone.x);
                if (rowB == 0u) rowB = rowA;

                if (i.misc.w > 3.5)
                {
                    // ---- p12 soft flash: a gaussian glow, white-hot in the middle, the palette's light colour outside,
                    //      additive (alpha 0), a quick rise and a smooth fade over the life
                    float rr = length(i.local), k = i.dims.x;
                    float env = smoothstep(0.0, 0.12, k) * (1.0 - smoothstep(0.25, 1.0, k));
                    float fa = atan2(i.local.y, i.local.x);
                    float fn = CelValue2(float2(fa * 1.6 + i.more.z * 7.0, i.misc.z * 9.0)) + 0.5 * CelValue2(float2(fa * 4.0 - i.more.z * 3.0, i.misc.z * 14.0));   // p19: a ragged splash, not a disc
                    if (i.dims.y > 0.5)
                    {
                        // p26 fireball (was p22's spiky cel star): a lumpy soft-edged ball, cream in the middle, yellow,
                        // orange, a red rim; pops in over 2 frames, holds, fades over the last 45% of its life (REF f29-36)
                        // p28 (REF f33-36 at 8x): a compact white blob inside a FLAT orange splat with 12 thin triangular
                        //      fingers of random length, longer toward the shooter, hard-edged; the fingers grow out over
                        //      4 frames and draw back; a thin dark rim, sparse dark flecks beyond
                        float fn2 = CelValue2(float2(fa * 3.0 + i.more.z * 5.0, i.misc.z * 12.0)) + 0.6 * CelValue2(float2(fa * 7.0 - i.more.z * 2.0, i.misc.z * 20.0));
                        float lb = saturate(-i.local.x / max(rr, 1e-4)), rb = saturate(i.local.x / max(rr, 1e-4));
                        float grow = smoothstep(0.0, 0.12, k) * (1.0 - smoothstep(0.45, 0.85, k));
                        float coreR = (0.12 + 0.04 * fn2) * (1.0 - 0.5 * smoothstep(0.4, 1.0, k));   // p28b
                        float star = (0.28 + 0.05 * fn2) * (1.0 - 0.15 * rb);   // p28d: the orange star a little bigger round the blob (REF 30 / 18 px)   // p28b
                        float fing = 0.0;
                        [unroll] for (int fj = 0; fj < 12; fj++)
                        {
                            float h1 = CelHash11(fj * 7.0 + i.more.z * 31.0), h2 = CelHash11(fj * 13.0 + i.more.z * 17.0), h3 = CelHash11(fj * 3.0 + i.more.z * 53.0);
                            float aj = (fj + h1 * 0.8) * 0.5236 + i.more.z * 6.2832;
                            float dj = fa - aj; dj -= 6.2832 * round(dj / 6.2832);
                            float wj = 0.07 + 0.1 * h2;   // p28e: thinner (REF)   // p28b: 6-14 deg half-angle (REF fingers 4-8 px at the base)
                            float dirL = 0.35 + 0.65 * saturate(-cos(aj));   // p28e   // p28d: the fingers lean toward the shooter (REF)
                            float Lj = (0.45 + 0.5 * h3) * dirL * grow;   // p28e: every finger long, none past the quad (0.5-1.4 were clipped)   // p28d   // p28c: longer (REF: fingers to 2.5x the star)
                            fing = max(fing, Lj * saturate(1.0 - abs(dj) / wj));
                        }
                        float R = star + fing;
                        float env2 = smoothstep(0.0, 0.03, k) * (1.0 - smoothstep(0.55, 1.0, k));
                        float ef = 1.0 - smoothstep(0.78, 0.98, max(abs(i.local.x), abs(i.local.y)));
                        float cov = (1.0 - smoothstep(R - 0.012, R + 0.012, rr)) * env2 * ef;
                        float wcov = 1.0 - smoothstep(coreR - 0.02, coreR + 0.02, rr);
                        half3 body = lerp(CelPal(rowA, 2), CelPal(rowA, 3), 0.55);
                        half3 ch = lerp(body, CelPal(rowA, 3), 1.0 - smoothstep(coreR, coreR + 0.1, rr));
                        ch = lerp(ch, CelPalCore(rowA).rgb * i.tone.y, wcov);
                        ch = lerp(ch, CelPal(rowA, 1), 0.6 * smoothstep(R - 0.035, R - 0.01, rr) * (1.0 - wcov));
                        float m2 = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x).r;
                        float fn4 = CelValue2(float2(fa * 17.0 + i.more.z * 11.0, rr * 8.0 + i.misc.z * 5.0)) * 0.6 + CelValue2(float2(fa * 5.0 - i.more.z * 7.0, rr * 3.0 - i.misc.z * 3.0)) * 0.6;
                        float fleck = smoothstep(R, R + 0.03, rr) * (1.0 - smoothstep(R + 0.08, R + 0.18, rr)) * step(0.66, fn4) * env2 * ef * 0.4;
                        float aa = saturate(cov + fleck * (1.0 - cov)) * m2;
                        return half4(lerp(CelPal(rowA, 0), ch, cov / max(cov + fleck * (1.0 - cov), 1e-4)) * aa, aa);
                    }
                    float rq = rr / (0.75 + 0.45 * fn);
                    float g = exp(-rq * rq * 3.0) * (1.0 - smoothstep(0.6, 1.0, rq));
                    half3 c = lerp(CelPal(rowA, 3), CelPalCore(rowA).rgb * i.tone.y, g * g) * (g * saturate(i.tone.z) * env);
                    float m = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x).r;
                    return half4(c * m, 0.0);
                }
                if (i.misc.w > 2.5)
                {
                    // ---- d25 cel blob: sum of five gaussian balls (units of the radius), outline displaced by two
                    //      octaves of travelling value noise, iso raised by erosion so it tears late in the life
                    float2 q = i.local;
                    float k = i.dims.x, t = i.misc.z, seed = i.more.z;
                    float spread = lerp(0.45, i.dims.y, smoothstep(0.0, 1.0, k));
                    float F = 0.0; float2 G = 0.0;
                    [unroll] for (int j = 0; j < 5; j++)
                    {
                        float h1 = CelHash21(float2(j, seed * 71.0)), h2 = CelHash21(float2(j + 9.0, seed * 37.0)), h3 = CelHash21(float2(j + 23.0, seed * 13.0));
                        float2 o0 = float2((h1 - 0.5) * 1.7, (h2 - 0.5) * 0.7 + 0.1 * j - 0.2) * spread;   // wider than tall: a billow
                        float ang = (h3 - 0.5) * 2.0 * i.dims.z * t;
                        float2 cj = float2(o0.x * cos(ang) - o0.y * sin(ang), o0.x * sin(ang) + o0.y * cos(ang));
                        cj.y += 0.18 * k * (h2 - 0.3);
                        float rj = (0.32 + 0.22 * h3) * lerp(0.85, 1.1, k);
                        float2 dv = q - cj;
                        float e = exp(-dot(dv, dv) / (rj * rj));
                        F += e; G += e * (-2.0 * dv / (rj * rj));
                    }
                    float nz = CelValue2(q * 2.3 + float2(seed * 17.0, -t * 0.9)) + 0.5 * CelValue2(q * 5.1 + float2(-t * 1.4, seed * 29.0));
                    float iso = 0.42 + i.more.y * 0.7;
                    float sdf = F - iso + (nz - 0.75) * i.dims.w;
                    float mask = CelAAStep(0.0, sdf);
                    float2 nOut = normalize(-G + 1e-5);
                    float rim = saturate(1.0 - sdf * 2.2);
                    float lit = saturate(0.55 + 0.5 * dot(nOut, float2(-0.45, 0.89)) * rim + 0.12 * (q.y / 1.9));
                    lit = max(lit, saturate(sdf * 1.6) * step(1.05, i.tone.y));   // d27: a fire blob (Core > 1) is hottest deep inside
                    half3 col = CelRamp(lit, rowA, max(i.tone.y, 1.0));
                    float ink = CelInk(sdf, 0.0, CelLodInk(i.tone.w)) * mask;
                    col = lerp(col, CelPalInk(rowA).rgb, ink * 0.85);
                    float op = saturate(i.tone.z) * smoothstep(0.0, 0.12, k) * (1.0 - smoothstep(0.72, 1.0, k));
                    float a = mask * op;
                    float m = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x).r;
                    return half4(col * a * m, a * m);
                }
                if (i.misc.w > 1.5)
                {
                    // ---- d01 path ribbon: a soft grey smoke wisp along its curling path. Its edges are eaten by
                    //      noise that travels up it (fray), it breaks into pieces as it ages, the top thins out, the
                    //      base lets go late; lit on its screen-left side, a mid-grey shade band, a thin ink line.
                    float u = i.local.x, y = i.local.y;
                    float top = i.dims.x, k = i.dims.z, t = i.misc.z, seed = i.more.z;
                    float sm = u * top;                                    // metres up the wisp
                    float wid = max(i.more.x, 1e-3);
                    float n1 = CelValue2(float2(sm / (wid * 1.6 + 0.05) - t * 1.3, seed * 53.0 + step(0.0, y) * 11.0));
                    float n2 = CelValue2(float2(sm / (wid * 0.7 + 0.03) - t * 2.1, seed * 29.0 + 5.0));
                    float yc = (n2 - 0.5) * 0.5;
                    float edge = 0.55 + 0.7 * n1;                          // frayed half width (1 = the nominal)
                    float field = saturate(1.0 - abs(y - yc) / edge);
                    // d07 (fracture 2: width profile): from mid-life the wisp is a chain of separate overlapping
                    // round puffs along its path (metres: sm along, ym across), each its own size and offset,
                    // growing up the wisp and over the life; the strip stays only as the young thread at the base
                    float ym = (y - yc) * wid * 0.5;
                    float pitch = wid * 0.55 + 0.05;
                    float puffF = 0.0, puffLit = 0.5;
                    float grow = (0.55 + 0.9 * u) * lerp(0.7, 1.15, k);
                    [unroll] for (int q = -1; q <= 1; q++)
                    {
                        float ci = floor(sm / pitch) + q;
                        float h1 = CelHash21(float2(ci, seed * 131.0)), h2 = CelHash21(float2(ci + 17.0, seed * 59.0));
                        float sc = (ci + 0.5 + (h1 - 0.5) * 0.4) * pitch;
                        float cy = (h2 - 0.5) * wid * 0.8;   // p03: puffs scattered across the column (REF: a broken cloud, not a sausage)
                        float R = wid * 0.7 * (0.5 + 0.7 * h1) * grow;   // p02: puffs overlap into a soft column
                        float2 dv = float2(sm - sc, ym - cy) / max(R, 1e-3);
                        float f = (sc > 0.0 && sc < top) ? 1.0 - length(dv) : 0.0;
                        if (f > puffF) { puffF = f; puffLit = saturate(0.62 + 0.3 * dv.x - 0.15 * dv.y); }
                    }
                    float billow = smoothstep(0.12, 0.45, k) * smoothstep(0.0, 0.25, u);
                    field = lerp(field, saturate(puffF * 1.4), billow);
                    // pieces: the wisp tears into chunks as it ages, the top first
                    float brk = CelValue2(float2(sm / (wid * 2.5 + 0.1) - t * 0.4, seed * 91.0));
                    float thr = i.more.y + 0.35 * u * k;
                    float cut = smoothstep(thr, thr + 0.08, brk * 0.85 + 0.15);
                    // the top thins to nothing, the base fades in from the fire and lets go late in the life
                    float ends = saturate((1.0 - u) * 5.0) * saturate(u * 12.0 + 0.2 - k * 3.0 * saturate(k * 2.0 - 0.6));
                    float fm = field * cut * ends;
                    float mask = CelAAStep(0.06, fm);
                    float lit = saturate(0.5 - 0.5 * (y - yc) / edge + 0.25 * (n2 - 0.5));   // screen-left side lit
                    lit = lerp(lit, puffLit, billow);   // d07: each puff lit on its upper side
                    // p01 (by eye): REF's smoke is a soft mottled grey, not two flat stripes: a smooth shade between
                    // the palette's dark and light greys, broken by slow travelling noise, a faint ink, a soft edge
                    float mott = CelValue2(float2(sm / (wid * 0.9 + 0.04) + t * 0.5, y * 1.3 + seed * 71.0));
                    float hole = CelValue2(float2(sm / (wid * 0.55 + 0.03) - t * 0.7, y * 2.2 + seed * 13.0));
                    fm *= smoothstep(0.22, 0.5, hole + 0.25 * (1.0 - k));   // p04: holes open as it ages
                    lit = saturate(lit * 0.9 + 0.08 + 0.7 * (mott - 0.5));
                    half3 c = lerp(CelPal(rowA, 0), CelPalCore(rowA).rgb, smoothstep(0.15, 0.8, lit));   // p17: cream lit side, dark grey shade (REF)   // p03: cream lit side, dark grey shade
                    mask = saturate((fm - 0.03) / 0.12);   // p17: defined lumps
                    float ink = CelInk(fm, 0.06, CelLodInk(i.tone.w)) * mask;
                    c = lerp(c, CelPalInk(rowA).rgb, ink * 0.35);
                    float op = saturate(i.tone.z) * (1.0 - smoothstep(0.55, 0.88, k));   // d37: the wisp is gone by ~f128 (REF clear at f131)
                    float a = mask * op;
                    float m = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x).r;
                    return half4(c * a * m, a * m);
                }
                if (i.misc.w > 0.5)
                {
                    // ---- the pool: a smooth radial gradient of the palette's lightest colour, the rim a little uneven
                    float2 c = i.local;
                    float ang = atan2(c.y, c.x);
                    float wob = 1.0 + 0.10 * sin(ang * 3.0 + i.more.z * 40.0) + 0.06 * sin(ang * 5.0 + i.more.z * 17.0);
                    float rr = length(c) / wob;
                    float g = pow(saturate(1.0 - rr * rr), i.dims.z);
                    // part cover, part light: the blue ground is pulled toward the warm colour (an additive orange
                    // alone turns it mauve), the middle a little brighter than the colour
                    float o = g * max(i.tone.z, 0.0);
                    half3 pf = CelAdditiveFog(CelPal(rowA, 3) * o, i.positionWS, i.misc.x);
                    float m = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x).r;
                    return half4(pf, saturate(o * o * 0.45) * m);   // cover only toward the middle: the rim adds light, never darkens
                }

                float s = i.local.x, y = i.local.y;
                float len = i.dims.x, hr = i.dims.y, fl = i.dims.z, amp = i.dims.w;
                float seed = i.more.z, wl = i.more.w;
                float inkPx = CelLodInk(i.tone.w);
                float fireMode = step(i.misc.w, -1.5);                // p16: a standing flame (misc.w = -2 - k)
                float fireK = saturate(-(i.misc.w + 2.0));
                float strand = step(i.misc.w, -0.5) * (1.0 - fireMode);   // a smoke strand: the ribbon alone
                float fireEnv = lerp(1.0, smoothstep(0.0, 0.1, fireK) * (1.0 - smoothstep(0.6, 1.0, fireK)), fireMode);   // p23f: full for longer
                float puffs = step(fl + hr, 1e-5) * (1.0 - strand);   // a wisp: a chain of round puffs

                // ---- the smoke ribbon: a thin wavy strand behind the flame that fades and thins to nothing
                //      (a wisp instead: a cluster of round inked puffs along a curling path, lit from above)
                float ribbonStart = (strand > 0.5 || puffs > 0.5) ? fl * 0.55 : hr * 3.0;   // p23   // p13: the smear begins just behind the head   // c01: the bolt's smoke starts inside its flame (it frays out of it)
                float grow = saturate((s - ribbonStart) / max(wl * 0.6, 1e-3));
                float yc = amp * grow * sin(6.2832 * s / wl + seed * 40.0 - i.misc.z * i.misc.y)
                         + amp * 0.15 * grow * sin(6.2832 * s / (wl * 0.45) + seed * 17.0 - i.misc.z * i.misc.y * 1.7);
                yc += step(1e-5, hr) * hr * lerp(1.6, 0.2, smoothstep(hr * 8.0, hr * 24.0, s));   // p27l: rides the top edge near the head, settles onto the axis far back (REF)   // p27: higher (REF)   // p23c: the REF's smear rides on the tail's upper edge
                float taper = saturate(1.0 - s / max(len, 1e-3));
                float rwid = i.more.x * 0.5 * lerp(lerp(lerp(0.8, 1.0, pow(taper, 1.5)), 0.35 + 0.65 * taper, strand), 1.0, puffs) * saturate((s - ribbonStart) / max(wl * 0.25, 1e-3) + lerp(0.15, 0.7, step(1e-5, hr)));   // p27l: a bolt's smear stays broad far back and starts wide (the ramp from a point drew a grey dart)   // p21: head-wide, then a thin line (REF)
                float rField = 0.0;
                if (puffs > 0.5)
                {
                    // round puffs on a chain: each cell along s holds one circle, neighbours overlap
                    float pitch = max(i.more.x * 0.5, 1e-3);   // constant along the wisp (a varying pitch cut the puffs straight)
                    float cell = floor(s / pitch);
                    [unroll] for (int q = -1; q <= 1; q++)
                    {
                        float cq = cell + q;
                        float h = CelHash21(float2(cq, seed * 113.0));
                        float sc = (cq + 0.5) * pitch;
                        float rad = pitch * (0.7 + 0.4 * h) * saturate(1.0 - sc / max(len, 1e-3) * 0.4);
                        float yq = yc + pitch * 0.3 * (h - 0.5);
                        float d = length(float2(s - sc, y - yq)) / max(rad, 1e-4);
                        float alivePuff = step(i.more.y, h * 0.9 + 0.08);   // whole puffs drop out as it breaks up
                        rField = max(rField, (sc > 0.0 && sc < len) ? saturate(1.0 - d) * alivePuff : 0.0);
                    }
                }
                else
                {
                    rField = (s > ribbonStart) ? saturate(1.0 - abs(y - yc) / max(rwid, 1e-4)) : 0.0;
                    float rg = CelValue2(float2(s / max(rwid * 2.5, 1e-3) - i.misc.z * 2.0, (y - yc) / max(rwid * 0.3, 1e-4) + seed * 5.0));   // p13: streaky, blurred
                    rField = pow(rField, 1.4) * (0.55 + 0.7 * rg);
                }
                float brk = CelValue2(float2(s / (wl * 0.5), seed * 91.0));   // pieces along the ribbon
                float rCut = puffs > 0.5 ? 1.0 : step(i.more.y, brk * 0.9 + 0.1 * taper);
                float rMask = lerp(pow(rField, 0.6), CelAAStep(0.04, rField), max(puffs, strand)) * rCut * lerp(1.0, smoothstep(ribbonStart, ribbonStart + wl * 0.6, s), step(1e-5, hr));   // p27l: fades in by opacity, not width   // p27e: nearly opaque (REF smear A58872 at the edge, F2D6BB core)   // p06: the bolt's trail is a soft smear
                // a puff is lit from above: light on its upper side, darker below
                // b05: the strand is a smoke ribbon: a light band on its upper/left side, a mid-grey shade band, an ink edge
                float sLit = saturate(0.5 + 0.5 * (y - yc) / max(rwid, 1e-4));
                float rVal = lerp(lerp(0.40 + 0.35 * rField, 0.3 + 0.6 * sLit, strand), saturate(0.6 + 0.4 * (y - yc) / max(rwid, 1e-4)) * lerp(0.75, 1.0, rField), puffs);
                half3 rCol = lerp(CelRamp(rVal, rowB, 1.0), lerp(CelPal(rowB, 1), CelPal(rowB, 3), saturate(rField * 1.6)), 1.0 - max(puffs, strand));
                float rInk = CelInk(rField, 0.04, inkPx * max(puffs, strand)) * rMask;    // b05: the strand is inked too
                half3 col = lerp(rCol, CelPalInk(rowB).rgb, rInk) * rMask;
                // the strand fades along its length and thins out at far zoom (the comet carries the read there)
                float lodY = _CelLod.y > 0.0 ? _CelLod.y : 1.0;
                float rFade = lerp(saturate(taper * lerp(2.2, 5.0, step(1e-5, hr))) * saturate(lodY * 4.0 - 2.0), 1.0, puffs);   // p27l: a bolt's smear holds to the last fifth of the quad (REF: past the body's end)   // p27k: full at lod <= 1 (0.7 at the close film read as a grey cloud), gone at lod 2
                rFade = lerp(rFade, saturate(taper * 6.0) * saturate(lodY * 2.0 - 0.6), strand);   // b05: fades only near its base
                float rOp = saturate(i.tone.z) * rFade;
                col *= lerp(rFade, 1.0, rInk);
                float a = rMask * lerp(rOp, 1.0, rInk);

                // ---- the flame (p23, rebuilt to 6x crops of the reference, in head radii hr): a cream CORE band
                //      0.4 hr thick, a yellow band to 0.65, orange to 0.85, a RED edge to 1.0; the body keeps
                //      0.78 hr half-thickness for 12 hr and tapers out; the head is an ellipse a little bigger with
                //      2-3 pointed tongues at its back and a dark loop inside; a translucent DARK red-brown halo
                //      1-2 hr wide lies below and behind; the pink smear rides ABOVE the tail; 3 thin red lines
                //      run on behind. The outline wobbles slowly and boils; the inner shapes change every frame.
                {
                    float has = step(1e-5, fl + hr);
                    float t = i.misc.z;
                    float grain = 0.5 * CelValue2(float2(s / max(hr * 3.0, 1e-3) - t * 4.0, y / max(hr * 0.2, 1e-4) + seed * 7.0))
                                + 0.5 * CelValue2(float2(s / max(hr * 1.2, 1e-3) + t * 7.0, y / max(hr * 0.45, 1e-4) + seed * 3.0));   // p23d: long streaks 4-9 px apart
                    float bodyL = max(min(fl * 0.55, hr * 12.0), 1e-3);   // p27b: 12 hr (REF ~13 hr)
                    float sb = saturate(s / bodyL);
                    float sway = fireMode * hr * 0.35 * sin(t * 6.0 + seed * 9.0) * sb;   // p18: a standing flame leans and flickers
                    float yy = y - sway;
                    float wob = 0.6 + 0.45 * CelValue2(float2(s / max(hr * 2.5, 1e-3) - t * 8.0, seed * 3.0 + step(0.0, yy) * 2.0))   // p26: faster   // p23d: bigger bulges
                              + 0.3 * CelValue2(float2(s / max(hr * 0.7, 1e-3) + t * 21.0, seed * 7.0 + step(0.0, yy) * 11.0))   // p26: a stronger, faster boil
                              + 0.35 * pow(CelValue2(float2(s / max(hr * 1.5, 1e-3) - t * 13.0, seed * 13.0 + step(0.0, yy) * 5.0)), 3.0);   // p26: flares that come and go
                    float u = s / max(hr, 1e-4);
                    float neck = 1.0 - lerp(0.08, 0.28, fireMode) * (1.0 + 0.3 * sin(t * 9.0 + seed * 4.0))   // p28: a mild pinch on a bolt (REF: a smooth teardrop)
                                  * exp(-pow((u - 1.9 - 0.3 * sin(t * 6.0 + seed * 8.0)) / 1.4, 2.0));   // p26: the pinch breathes and drifts   // p24d: a tighter pinch just behind the ball
                    float bulge = 1.0 + lerp(0.05, 0.18, fireMode) * (1.0 + 0.4 * sin(t * 5.0 + seed * 3.0))   // p28: a mild swell on a bolt
                                   * exp(-pow((u - 6.0 - 1.5 * sin(t * 7.0 + seed * 9.0)) / 3.2, 2.0));  // p26: the swell drifts along the tail   // p24: then swells
                    float halfW = hr * 1.15 * neck * bulge * lerp(pow(1.0 - sb, 0.5) * (0.85 + 0.35 * smoothstep(0.0, 0.3, sb) * (1.0 - smoothstep(0.3, 0.9, sb))), 1.0 - pow(sb, 2.0), fireMode)   /* p28d: a cigar, widest a third back (REF) */ * lerp(1.0, wob, lerp(0.3, 1.0, fireMode)) * lerp(1.0, 1.5, fireMode);   // p28: a bolt is a smooth teardrop, round at the front, a point at the end (REF); the boil stays on a standing flame   // p27: 1.15 hr (REF 35 px = 2.7 hr wide for 100 px)   // p24: ball, neck, bulge, taper (owner)
                    // HEAD
                    float ang = atan2(yy, s);
                    float back = saturate(s / max(hr, 1e-4) + 0.3);
                    float tips = CelValue2(float2(ang * 2.2 + seed * 9.0, t * 20.0 + seed * 3.0)) + 0.6 * CelValue2(float2(ang * 6.0 - seed * 5.0, t * 31.0));   // p26: livelier
                    float hrad = 1.0 + pow(saturate(tips / 1.5), 1.6) * 0.7 * back;   // p26: longer tongues again   // p24: shorter tongues, the neck shows
                    float brA = 0.9 + 0.2 * CelValue2(float2(t * 9.0, seed * 13.0)), brB = 0.9 + 0.2 * CelValue2(float2(t * 11.0 + 3.0, seed * 29.0));
                    float ax = hr * 1.15 * brA;   // p24d: the ball a little bigger than the body
                    float headD = length(float2(s / max(ax, 1e-4), yy / max(hr * 1.15 * brB, 1e-4))) / max(hrad, 0.3);
                    float2 cq = float2(s - hr * 0.05, yy);
                    float mR = hr * (0.5 + 0.2 * CelValue2(float2(ang * 2.0 + seed * 11.0, t * 6.0)));   // p23e: the REF's loop is half the head
                    float ringD = abs(length(cq) - mR) / max(hr * 0.2, 1e-4);
                    float mark = saturate(1.0 - ringD) * saturate(0.2 + s / max(hr * 0.4, 1e-4)) * saturate((1.0 - headD) * 6.0) * (0.5 + 0.5 * CelValue2(float2(ang * 4.0 - t * 8.0, seed * 23.0)));
                    // q: 0 on the axis, 1 at the silhouette's edge
                    float qBody = (s > 0.0 && sb < 1.0) ? abs(yy) / max(halfW, 1e-4) : 10.0;
                    float q = min(qBody, headD) + 0.2 * (grain - 0.5);   // p23c: streaky
                    q *= lerp(1.0, 0.65 + 0.7 * CelValue2(float2(yy / max(hr * 0.45, 1e-4) + seed * 5.0, s / max(hr * 0.7, 1e-3) - t * 9.0)), fireMode);   // p23g: a standing flame is torn into tongues (multiplicative: the axis stays opaque)
                    float capF = lerp(0.5, 1.0, smoothstep(-hr * 0.8, hr * 1.2, s));   // p25: the cap's heat blends into the body's over 2 hr (no line at s = 0)
                    float qc = min(qBody, max(headD, abs(yy) / max(hr * 0.95 * brB, 1e-4))) * capF + 0.3 * (grain - 0.5);   // p25: continuous   // p24c: the cap's edge stays yellow-orange   // p23b: colour by the cigar's thickness
                    float qe = qc + lerp(0.12, 0.3, fireMode) * pow(sb, 2.0);   // p28: the cream runs to the point (REF)   // p23b: cools slowly   // the tail cools: the core narrows, the edges redden
                    // LINES: three thin red lines running on behind the body, braiding
                    float streak = 0.0, streakC = 0.0;
                    [unroll] for (int k3 = 0; k3 < 4; k3++)
                    {
                        float hk = CelHash21(float2(k3, seed * 57.0));
                        float hk2 = CelHash21(float2(k3 + 31.0, seed * 19.0));
                        float Lk = fl * (1.0 + 0.4 * hk) * (0.95 + 0.05 * sin(t * 6.0 + hk * 31.0)) * lerp(1.0, 0.45, fireMode);   // p28: 300+ px (REF)   // p27k: the tail's whole length
                        float uk = saturate(s / max(Lk, 1e-3));
                        float side = k3 < 2 ? -1.0 : 1.0;   // p28: two stripes below the body, two above (the fourth faint)
                        float yk = sway + side * hr * ((k3 == 0 ? 1.0 : k3 == 1 ? 1.3 : k3 == 2 ? 0.9 : 1.6) + (k3 == 0 ? 1.4 : k3 == 1 ? 1.8 : k3 == 2 ? 0.9 : 1.4) * uk)   // p28: they diverge from the body's rear edges (REF)
                                 + hr * 0.05 * uk * sin(s * 0.6 / max(hr, 1e-3) + hk * 20.0 - t * 6.0)   // p27k: straight (REF: motion streaks)   // p26c: straighter (REF)
                                 + hr * 0.02 * uk * sin(s * 2.4 / max(hr, 1e-3) - t * 11.0 + hk2 * 9.0);
                        float wk = hr * (0.2 + 0.08 * hk2) * (1.0 - 0.2 * uk);   // p27k: 3 px (REF)   // p26c: thinner (REF 1-2 px cores)   // p25c: the blur's width
                        float dk = (y - yk) / max(wk, 1e-4);
                        float ek = (1.0 - smoothstep(0.7, 1.0, uk)) * smoothstep(hr * 3.5, hr * 7.0, s) * (k3 == 3 ? 0.4 : 1.0)   // p28: they emerge from under the body's rear (REF ~6 hr)
                                 * (0.55 + 0.45 * CelValue2(float2(s / max(hr * 5.0, 1e-3) - t * 3.0, hk * 40.0 + seed * 9.0)));   // p25: soft ends, long soft dashes (motion blur)
                        float fk = exp(-dk * dk) * ek;   // p25: gaussian across
                        streak = max(streak, fk);
                        streakC = max(streakC, exp(-dk * dk * 5.0) * ek);   // p25c: a 1-2 px bright core (p25b's was under a pixel: invisible)   // p25b: REF: cream hair lines in a red blur
                    }
                    // the opaque silhouette (anti-aliased), the lines, a softer edge on a standing flame
                    float rear = lerp(1.0 - smoothstep(max(len - hr * 8.0, len * 0.5), len, s), 1.0, fireMode);   // p23e: never on a standing flame (it faded the ground fires)   // p23c: the body fades out before the quad's rear (no cut at the birth)
                    float cov = (1.0 - smoothstep(lerp(0.9, 0.85, fireMode), lerp(1.15, 1.25, fireMode), q)) * rear;   // p28: a 4 px feathered edge on a bolt (REF)   // p27: the red edge shows   // p23c: a soft blurred edge (REF), the dark halo outside
                    float strandN = CelValue2(float2(yy / max(hr * 0.35, 1e-4) + seed * 3.0, s / max(hr * 7.0, 1e-3) - t * 2.5));   // p26: strands across the tail
                    float fray = smoothstep(lerp(0.75, 0.55, fireMode), 1.0, sb);   // p28: the teardrop stays whole longer   // p26b: the body stays whole for ~1.5 m (REF ~180 px), then frays
                    cov *= lerp(1.0, 0.05 + 0.95 * smoothstep(0.45, 0.65, strandN), fray);   // p26: the tail frays into parallel strands (REF)
                    float capD = length(float2(min(s, 0.0) / max(ax * 1.25, 1e-4) + max(s, 0.0) / max(ax * 0.55, 1e-4), yy / max(hr * 1.15 * brB, 1e-4)));   // p25: long ahead, short behind, no step
                    cov = max(cov, 1.0 - smoothstep(0.5, 1.7, capD));   // p24c: the ball's own long soft edge (REF: a 20 px orange glow ahead)
                    float lineCov = saturate(streak * 0.7 + streakC * 0.5) * rear;   // p25d: the blur 80%, the core opaque   // p25: no AA step: the line IS its blur
                    float coreR = streakC / max(streak, 1e-4);   // p25d: 1 on the line's axis, 0 in its blur, whatever the line's strength
                    float alphaF = max(cov, lineCov);
                    alphaF = lerp(alphaF, 1.0 - smoothstep(0.85, 1.2, q), fireMode) * has * fireEnv;   // p23g: a firm ragged flame
                    // heat by q: core to 0.45, yellow to 0.65, orange to 0.85, red to 1.0
                    float heatF = qe < 0.45 ? 1.0 - 0.15 * (qe / 0.45) : qe < 0.7 ? lerp(0.85, 0.25, (qe - 0.45) / 0.25) : lerp(0.25, 0.0, saturate((qe - 0.7) / 0.5));
                    float heatB = qe < 0.7 ? 1.0 - 0.12 * (qe / 0.7) : qe < 0.86 ? lerp(0.88, 0.42, (qe - 0.7) / 0.16) : lerp(0.42, 0.12, saturate((qe - 0.86) / 0.3));   // p28: cream fills the teardrop, a thin yellow then orange band, red at the edge (REF)
                    float heat = lerp(heatB, heatF, fireMode);   // p27: a wide red edge (REF 7-17 px)
                    heat -= lerp(0.12, 0.3, fireMode) * (grain - 0.5) * step(qe, 0.9);   // p28: the REF's cream is smooth   // p23d: the streak texture shows in the colour, not just the edge
                    heat = lerp(heat, 0.1 + 0.9 * coreR, saturate(lineCov * 3.0) * (1.0 - cov));   // p25d: the colour switches fully to the line's wherever there is one (the alpha carries the blur; p25b/c diluted the heat twice and the core never showed)   // p25b: a cream core on a red blur
                    heat = saturate(heat + 0.25 * smoothstep(0.5, 0.75, strandN) * fray * cov);   // p26: the strands are bright hair lines (REF)
                    heat = saturate(heat - 0.5 * mark * (1.0 - fireMode) + 0.05 * fireMode);   // p24b: the loop is a red line, not a bite   // p23g: no loop mark on a fire, bright
                    half3 c0 = CelPal(rowA, 0), c1 = CelPal(rowA, 1), c2 = CelPal(rowA, 2), c3 = CelPal(rowA, 3);
                    half3 cc = CelPalCore(rowA).rgb * i.tone.y;
                    half3 fCol = heat < 0.25 ? lerp(c0, c1, heat / 0.25)
                               : heat < 0.4 ? lerp(c1, c2, (heat - 0.25) / 0.15)
                               : heat < 0.8 ? lerp(c2, c3, (heat - 0.4) / 0.4)   // p27b: a thinner orange band (REF)
                               : lerp(c3, cc, (heat - 0.8) / 0.2);
                    // the DARK halo: translucent red-brown, wider below and behind, under the body
                    float yq = yy + hr * 0.5;   // p27: the REF's halo sits lower
                    float haloHalf = hr * (2.8 - 2.4 * saturate(s / max(hr * 20.0, 1e-3)));   // p27c: narrows faster   // p27b: the gaussian's 1-sigma: dark to +-1.9 hr at the head (REF 56 px), +-1.1 hr at 250 px (REF 27)
                    float qH = min(abs(yq) / max(haloHalf, 1e-4) + max(-s, 0.0) / max(hr * 1.5, 1e-4), length(float2(s / max(ax * 2.2, 1e-4), yq / max(hr * 2.4, 1e-4))) / max(hrad, 0.3));   // p27: a broad soft cloud, not a band   // p24: rounds off ahead of the head (no cut)
                    qH += 0.15 * (grain - 0.5);
                    float haloA = exp(-pow(qH, 3.0) * 1.2) * (1.0 - 0.25 * sb)   // p27c: fuller to +-0.85 of the width, then off (REF: dark right up to the rim)
                                  * has * fireEnv * (1.0 - fireMode) * rear;   // p27b: a soft gradient cloud, no plateau (the plateau read as a flat bar)   // p27: stays dark down the tail   // p23h: no dark halo on a standing flame (it filled the quad: a red box)
                    haloA *= (1.0 - saturate(a)) * (1.0 - smoothstep(hr * 2.6, hr * 3.4, abs(yy)));   // p27k: none under the smear, none at the quad's edge (a box over the board)   // p27d: the smear stays on top of the dark cloud (it went grey-brown under it)
                    col = lerp(col, c0, haloA);
                    a = lerp(a, 1.0, haloA);
                    col = lerp(col, fCol, alphaF);
                    a = lerp(a, 1.0, alphaF);
                    // a faint warm bloom around the head
                    float bl = saturate(1.0 - length(float2(s / max(hr * 2.4, 1e-4), yy / max(hr * 2.2, 1e-4))));   // p27b: tighter still (REF: dark up to the rim)
                    bl *= bl;
                    col += lerp(c2, c3, bl) * (bl * 0.25 * has * fireEnv);   // p27b: 0.25   // p26c: 0.5   // p24c: a wide orange glow around the ball
                }

                half3 fogged = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x);
                float m = fogged.r;
                return half4(col * m, a * m);
            }
            ENDHLSL
        }
    }
}
