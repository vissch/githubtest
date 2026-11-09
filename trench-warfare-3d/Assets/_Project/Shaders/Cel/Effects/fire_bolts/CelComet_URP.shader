// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - Comet: a fire bolt drawn as one camera-facing quad along
// its velocity, an SDF in the fragment: a white-yellow head with a soft yellow bloom, a flame tail banded yellow to
// orange to deep red that tapers smoothly to a point, a dark ink line on the screen-lower edge, and behind it a thin
// wavy smoke ribbon with an ink edge that thins to a point and breaks up in steps. With no head and no flame it is a
// curling smoke wisp rising from a fire, drawn as a chain of inked grey cel puffs. Premultiplied alpha: the fire is
// nearly opaque colour, the ink darkens what is behind it, the ribbon is part covering, the bloom is additive.
// Mode extra.x < 0: a soft warm light pool lying on the ground (additive, a smooth radial gradient, no bands), whose
// brightness rises to a peak and fades smoothly over the life.
//   size.xyz   ribbon width (m) at birth, at death, ease        size.w  ribbon length in seconds of its speed
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
                    float env = smoothstep(0.0, 0.025, k) * lerp(0.7, 1.0, smoothstep(0.02, max(r.extra.z, 0.03), k))
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
                float halfW = max(max(headR * 1.75, amp + rw), 0.01) * alive;
                float len = max(speed * r.size.w, flameL);
                len = min(len, distance(p, r.pos));                    // no ribbon behind its birth
                float front = max(headR * 1.75, rw);
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;   // +y is up on the screen: the ink goes below
                float along = lerp(-len, front, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * halfW;
                float3 w = p + dir * along + side * across;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.local = float2(-along, across);
                o.dims = float4(len, headR, flameL, amp);
                o.tone = r.tone;
                o.more = float4(rw, CelCurve(r.erode.xyz, k), r.seed, max(r.extra.w, 0.05));
                o.misc = float4(ComputeFogFactor(o.positionCS.z), r.motion.z, t, 0.0);
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
                float puffs = step(fl + hr, 1e-5);                    // a wisp: a chain of round puffs

                // ---- the smoke ribbon: a wavy line behind the flame, thinning to a point at its end
                float ribbonStart = fl * 0.3;
                float grow = saturate((s - ribbonStart) / max(wl * 0.6, 1e-3));
                float yc = amp * grow * sin(6.2832 * s / wl + seed * 40.0 - i.misc.z * i.misc.y)
                         + amp * 0.15 * grow * sin(6.2832 * s / (wl * 0.45) + seed * 17.0 - i.misc.z * i.misc.y * 1.7);
                float taper = saturate(1.0 - s / max(len, 1e-3));
                float bead = lerp(1.0, 0.8 + 0.3 * sin(6.2832 * s / (wl * 0.5) + seed * 23.0), puffs);
                float rwid = i.more.x * 0.5 * sqrt(taper) * bead * saturate((s - ribbonStart) / max(wl * 0.25, 1e-3) + 0.15);
                float rField = (s > ribbonStart) ? saturate(1.0 - abs(y - yc) / max(rwid, 1e-4)) : 0.0;
                float brk = CelValue2(float2(s / (wl * 0.5), seed * 91.0));   // pieces along the ribbon
                float rCut = step(i.more.y, brk * 0.9 + 0.1 * taper);
                float rMask = CelAAStep(0.04, rField) * rCut;
                // a puff is lit from above: light on its upper side, darker below
                float rVal = lerp(rField, saturate(0.45 + 0.55 * (y - yc) / max(rwid, 1e-4)), puffs * 0.7);
                half3 rCol = CelRamp(rVal, rowB, 1.0);
                float rInk = CelInk(rField, 0.04, inkPx * lerp(0.6, 1.0, puffs)) * rMask;
                half3 col = lerp(rCol, CelPalInk(rowB).rgb, rInk) * rMask;
                float a = rMask * lerp(saturate(i.tone.z), 1.0, rInk);

                // ---- the flame: a round head and a tail that tapers smoothly to a point
                {
                    float has = step(1e-5, fl + hr);
                    float hd = length(float2(s, y));
                    float headF = 1.0 - hd / max(hr, 1e-4);
                    float u = saturate(s / max(fl, 1e-3));
                    float lick = 1.0 + 0.05 * sin(s * 9.0 + seed * 50.0 - i.misc.z * 14.0) * u;
                    float hw = hr * 0.95 * pow(saturate(1.0 - u), 1.1) * lick;
                    float tailF = (s >= 0.0 && s <= fl) ? 1.0 - abs(y) / max(hw, 1e-4) : 0.0;
                    float f = max(headF, tailF);
                    float fMask = CelAAStep(0.03, f) * has;
                    // banded along its length: yellow behind the head, orange, deep red at the tip; the head white-yellow
                    // (a yellow streak runs down the middle of the first half; the white core is only the head's middle)
                    float valTail = lerp(0.8, 0.1, pow(u, 1.2)) * lerp(0.8, 1.0, saturate(tailF * 3.0)) + 0.2 * step(0.55, tailF) * (1.0 - u);
                    float val = max(min(valTail, 0.82), headF > 0.0 ? 0.74 + 0.6 * headF : 0.0);
                    half3 fCol = CelRamp(saturate(val), rowA, i.tone.y);
                    // ink: a line on the screen-lower edge of the head and the tail, a thin one above, and the tip
                    float below = saturate(-y / max(hr * 0.15, 1e-4) + 0.5);
                    float fInk = CelInk(f, 0.03, inkPx) * fMask * max(max(below, 0.3 * u), u * u);
                    fCol = lerp(fCol, CelPalInk(rowA).rgb, fInk);
                    col = lerp(col, fCol, fMask);
                    a = lerp(a, lerp(0.85, 1.0, fInk), fMask);
                    // the bloom: a soft yellow light about 1.5x the head, added around it (no alpha: additive)
                    float bl = saturate(1.0 - hd / max(hr * 1.7, 1e-4));
                    col += CelPal(rowA, 3) * (bl * bl * 0.55 * has * (1.0 - fMask));
                }

                half3 fogged = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x);
                float m = fogged.r;
                return half4(col * m, a * m);
            }
            ENDHLSL
        }
    }
}
