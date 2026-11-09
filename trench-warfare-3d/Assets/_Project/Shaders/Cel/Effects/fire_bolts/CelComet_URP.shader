// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; b04: a small round head tapering into a long thin red-orange flame streak; b03: flame body twice as wide, wider yellow band) - Comet: a fire bolt drawn as one camera-facing quad along
// its velocity, an SDF in the fragment: a white-yellow head with a soft yellow bloom, a flame tail banded yellow to
// orange to deep red that tapers smoothly to a point, a dark ink line on the screen-lower edge, and behind it a thin
// wavy smoke ribbon with an ink edge that thins to a point and breaks up in steps. With no head and no flame it is a
// curling smoke wisp rising from a fire, drawn as a chain of inked grey cel puffs. Premultiplied alpha: the fire is
// nearly opaque colour, the ink darkens what is behind it, the ribbon is part covering, the bloom is additive.
// Mode extra.y < 0 (no head): a thin curling smoke strand (the ribbon alone, no ink), widening toward its newest end
// and breaking up in pieces over its life.
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

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                Varyings o;
                if (r.extra.x < 0.0)
                {
                    // the pool: a level quad on the ground, nudged toward the eye so small bumps do not cut it
                    float radius = CelCurve(r.size.xyz, k) * alive;
                    float2 c = v.uv * 2.0 - 1.0;
                    float3 w = p + float3(c.x, 0.0, c.y) * radius + float3(0.0, 0.04, 0.0);
                    float3 toEye = _WorldSpaceCameraPos - w;
                    w += normalize(toEye) * min(0.7, length(toEye) * 0.1);
                    // brightness over the life: a soft start, a rise to the peak, a smooth fade to nothing
                    float env = smoothstep(0.0, 0.025, k) * lerp(0.4, 1.0, smoothstep(0.02, max(r.extra.z, 0.03), k))
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
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : float3(0, 1, 0);
                float rw = CelCurve(r.size.xyz, k);
                float headR = max(r.extra.x, 0.0), flameL = max(r.extra.y, 0.0), amp = abs(r.extra.z);
                float halfW = max(max(headR * 2.5, amp * 1.2 + rw), 0.01) * alive;
                float len = max(speed * abs(r.size.w), flameL);
                if (r.size.w >= 0.0) len = min(len, distance(p, r.pos));   // no ribbon behind its birth (b02: a negative
                                                                           // size.w is a bolt already in flight: whole tail)
                float front = max(headR * 2.5, rw) * alive;
                len *= alive;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;   // +y is up on the screen: the ink goes below
                float back = (headR + flameL) > 0.0 ? 0.0 : rw * alive;   // a wisp's last puff is round past its end
                float along = lerp(-len - back, front, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * halfW;
                float3 w = p + dir * along + side * across;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.local = float2(-along, across);
                o.dims = float4(len, headR, flameL, amp);
                o.tone = r.tone;
                o.more = float4(rw, CelCurve(r.erode.xyz, k), r.seed, max(r.extra.w, 0.05));
                o.misc = float4(ComputeFogFactor(o.positionCS.z), r.motion.z, t, (r.extra.y < 0.0 && headR <= 0.0) ? -1.0 : 0.0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint rowA = CelRowA(i.tone.x), rowB = CelRowB(i.tone.x);
                if (rowB == 0u) rowB = rowA;

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
                float strand = step(i.misc.w, -0.5);                  // a smoke strand: the ribbon alone
                float puffs = step(fl + hr, 1e-5) * (1.0 - strand);   // a wisp: a chain of round puffs

                // ---- the smoke ribbon: a thin wavy strand behind the flame that fades and thins to nothing
                //      (a wisp instead: a cluster of round inked puffs along a curling path, lit from above)
                float ribbonStart = fl * 0.55;
                float grow = saturate((s - ribbonStart) / max(wl * 0.6, 1e-3));
                float yc = amp * grow * sin(6.2832 * s / wl + seed * 40.0 - i.misc.z * i.misc.y)
                         + amp * 0.15 * grow * sin(6.2832 * s / (wl * 0.45) + seed * 17.0 - i.misc.z * i.misc.y * 1.7);
                float taper = saturate(1.0 - s / max(len, 1e-3));
                float rwid = i.more.x * 0.5 * lerp(lerp(sqrt(taper), 0.35 + 0.65 * taper, strand), 1.0, puffs) * saturate((s - ribbonStart) / max(wl * 0.25, 1e-3) + 0.15);
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
                else rField = (s > ribbonStart) ? saturate(1.0 - abs(y - yc) / max(rwid, 1e-4)) : 0.0;
                float brk = CelValue2(float2(s / (wl * 0.5), seed * 91.0));   // pieces along the ribbon
                float rCut = puffs > 0.5 ? 1.0 : step(i.more.y, brk * 0.9 + 0.1 * taper);
                float rMask = CelAAStep(0.04, rField) * rCut;
                // a puff is lit from above: light on its upper side, darker below
                // b05: the strand is a smoke ribbon: a light band on its upper/left side, a mid-grey shade band, an ink edge
                float sLit = saturate(0.5 + 0.5 * (y - yc) / max(rwid, 1e-4));
                float rVal = lerp(lerp(0.62 + 0.38 * rField, 0.3 + 0.6 * sLit, strand), saturate(0.6 + 0.4 * (y - yc) / max(rwid, 1e-4)) * lerp(0.75, 1.0, rField), puffs);
                half3 rCol = CelRamp(rVal, rowB, 1.0);
                float rInk = CelInk(rField, 0.04, inkPx * max(puffs, strand)) * rMask;    // b05: the strand is inked too
                half3 col = lerp(rCol, CelPalInk(rowB).rgb, rInk) * rMask;
                // the strand fades along its length and thins out at far zoom (the comet carries the read there)
                float lodY = _CelLod.y > 0.0 ? _CelLod.y : 1.0;
                float rFade = lerp(saturate(taper * 2.2) * saturate(lodY * 2.0 - 0.8), 1.0, puffs);
                rFade = lerp(rFade, saturate(taper * 6.0) * saturate(lodY * 2.0 - 0.6), strand);   // b05: fades only near its base
                float rOp = saturate(i.tone.z) * rFade;
                col *= lerp(rFade, 1.0, rInk);
                float a = rMask * lerp(rOp, 1.0, rInk);

                // ---- the flame (b02): a round white-yellow head, a yellow flame body as wide as the head behind it
                //      that flares out and then tapers to a point, and thin red streaks that run on behind it and fade
                {
                    float has = step(1e-5, fl + hr);
                    float hd = length(float2(s, y));
                    float headF = 1.0 - hd / max(hr, 1e-4);
                    // b04: the body is a long thin flame streak, 6x the head's length, never wider than the head
                    // b05: 7x the head's length (b04: 5x)
                    float bodyL = max(min(fl * 0.85, hr * 14.0), 1e-3);
                    float sb = saturate(s / bodyL);
                    // a lick that travels down the body (the flame flickers, never a rigid capsule)
                    float lick = hr * 0.12 * sin(s * 5.0 / max(hr * 4.0, 1e-3) + seed * 30.0 - i.misc.z * 14.0) * sb;
                    // b05: the back half is ragged: its width flickers in tongues that travel down the body
                    float rag = 1.0 - smoothstep(0.35, 1.0, sb) * (0.55 + 0.45 * sin(s * 9.0 / max(hr * 4.0, 1e-3) + seed * 21.0 - i.misc.z * 22.0));
                    float wBody = hr * (1.0 - 0.15 * smoothstep(0.0, 0.2, sb)) * pow(saturate(1.0 - sb), 0.6) * rag;   // b04: as wide as the head, tapering to a point
                    float tailF = (s >= 0.0 && s <= bodyL) ? 1.0 - abs(y - lick) / max(wBody, 1e-4) : 0.0;
                    float streakF = 0.0, streakU = 1.0;
                    [unroll] for (int k5 = 0; k5 < 5; k5++)
                    {
                        float hk = CelHash21(float2(k5, seed * 57.0));
                        float Lk = fl * (k5 == 2 ? 1.0 : 0.35 + 0.5 * hk);
                        float uk = saturate(s / max(Lk, 1e-3));
                        float yk = hr * 0.3 * (k5 - 2) * (1.0 - 0.7 * uk) + hr * 0.1 * sin(s * 2.5 + hk * 20.0 - i.misc.z * 9.0) * uk;
                        float wk = hr * (k5 == 2 ? 0.26 : 0.16) * pow(saturate(1.0 - uk), 0.8);   // b04: thinner
                        float fk = (s >= bodyL * 0.3 && s <= Lk) ? 1.0 - abs(y - yk) / max(wk, 1e-4) : 0.0;
                        if (fk > streakF) { streakF = fk; streakU = uk; }
                    }
                    float f = max(max(headF, tailF), streakF);
                    float fMask = CelAAStep(0.03, f) * has;
                    // colour: the head white-hot to yellow; the body yellow in the middle, orange round it, red at its
                    // edge and toward its end; the streaks red to deep red
                    float vHead = headF > 0.0 ? 0.78 + 0.22 * saturate((headF - 0.35) * 2.0) : 0.0;   // b04: white-yellow   // yellow, white only at the middle (b03: a smaller white)
                    // b04: yellow just behind the head, orange, then red-orange down the streak (b03's yellow-white inner band read as a white rectangle)
                    // b05: a white-yellow core band down the front two thirds, orange round it, red-orange at the edges
                    float vBody = tailF > 0.0 ? (0.2 + 0.38 * saturate(tailF * 1.6)) + 0.3 * saturate(1.0 - sb * 1.5) * step(0.45, tailF) : 0.0;
                    float vStreak = streakF > 0.0 ? 0.08 + 0.3 * (1.0 - streakU) : 0.0;
                    float val = max(max(vHead, vBody), vStreak);
                    half3 fCol = CelRamp(saturate(val), rowA, i.tone.y);
                    // no ink on the flame (b02: an ink ring made the head read as a ball)
                    float fInk = 0.0;
                    fCol = lerp(fCol, CelPalInk(rowA).rgb, fInk);
                    // the streaks fade out along their length; the head and body are opaque
                    float fOp = max(step(1e-4, max(headF, tailF)), saturate(1.25 - streakU));
                    col = lerp(col, fCol, fMask * fOp);
                    a = lerp(a, lerp(0.92, 1.0, fInk), fMask * fOp);
                    // the bloom: a soft yellow-orange light about 2.4x the head, added around it
                    float bl = saturate(1.0 - hd / max(hr * 2.4, 1e-4));
                    col += lerp(CelPal(rowA, 2), CelPal(rowA, 3), bl) * (bl * bl * 0.7 * has * (1.0 - fMask * 0.7));
                }

                half3 fogged = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x);
                float m = fogged.r;
                return half4(col * m, a * m);
            }
            ENDHLSL
        }
    }
}
