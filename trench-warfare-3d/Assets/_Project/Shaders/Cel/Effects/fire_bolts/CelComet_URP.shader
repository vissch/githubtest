// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - Comet: a fire bolt drawn as one camera-facing quad along
// its velocity, an SDF in the fragment: a small round head, a banded flame tail that tapers to a point (yellow to
// orange to deep red), a dark ink edge on the screen-lower side, and behind it a thin wavy smoke ribbon with an ink
// edge that thins to a point and breaks up in steps. With no head and no flame it is a curling smoke wisp rising from
// a fire (the ribbon alone, from where it was born up to where it is now). Premultiplied alpha: the fire is nearly
// opaque colour, the ink darkens what is behind it, the ribbon is part covering.
//   size.xyz   ribbon width (m) at birth, at death, ease        size.w  ribbon length in seconds of its speed
//   motion     gravity, drag, z: the ribbon's wave speed (rad/s, the curl travels along it), buoyancy
//   erode.xyz  the ribbon's break-up threshold over the life (0 whole .. 1 gone): it breaks into pieces in steps
//   tone       x flame palette (+ 256 x the ribbon palette), y HDR core, z ribbon opacity, w ink px
//   extra      x head radius (m, 0: no head), y flame tail length (m, 0: no flame), z ribbon wave amplitude (m),
//              w ribbon wavelength (m)
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
                float2 local : TEXCOORD0;                         // x metres behind the head, y metres across (+ screen up)
                nointerpolation float4 dims : TEXCOORD1;          // x length, y head radius, z flame length, w wave amplitude
                nointerpolation float4 tone : TEXCOORD2;          // x palettes, y hdr, z ribbon opacity, w ink px
                nointerpolation float4 more : TEXCOORD3;          // x ribbon width, y erosion threshold, z seed, w wavelength
                float4 misc : TEXCOORD4;                          // x fog, y wave speed, z age
                float3 positionWS : TEXCOORD5;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : float3(0, 1, 0);
                float rw = CelCurve(r.size.xyz, k);
                float headR = max(r.extra.x, 0.0), flameL = max(r.extra.y, 0.0), amp = abs(r.extra.z);
                float halfW = max(max(headR * 1.35, amp + rw), 0.01) * alive;
                float len = max(speed * r.size.w, flameL);
                len = min(len, distance(p, r.pos));                    // no ribbon behind its birth
                float front = max(headR * 1.35, rw);
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                if (dot(side, UNITY_MATRIX_V[1].xyz) < 0.0) side = -side;   // +y is up on the screen: the ink goes below
                float along = lerp(-len, front, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * halfW;
                float3 w = p + dir * along + side * across;
                Varyings o;
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
                float s = i.local.x, y = i.local.y;
                float len = i.dims.x, hr = i.dims.y, fl = i.dims.z, amp = i.dims.w;
                float seed = i.more.z, wl = i.more.w;
                uint rowA = CelRowA(i.tone.x), rowB = CelRowB(i.tone.x);
                if (rowB == 0u) rowB = rowA;
                float inkPx = CelLodInk(i.tone.w);

                // ---- the smoke ribbon: a wavy line behind the flame, thinning to a point at its end
                float ribbonStart = fl * 0.3;
                float grow = saturate((s - ribbonStart) / max(wl * 0.6, 1e-3));
                float yc = amp * grow * sin(6.2832 * s / wl + seed * 40.0 - i.misc.z * i.misc.y)
                         + amp * 0.15 * grow * sin(6.2832 * s / (wl * 0.45) + seed * 17.0 - i.misc.z * i.misc.y * 1.7);
                float taper = saturate(1.0 - s / max(len, 1e-3));
                float rwid = i.more.x * 0.5 * sqrt(taper) * saturate((s - ribbonStart) / max(wl * 0.25, 1e-3) + 0.15);
                float rField = (s > ribbonStart) ? saturate(1.0 - abs(y - yc) / max(rwid, 1e-4)) : 0.0;
                float brk = CelValue2(float2(s / (wl * 0.5), seed * 91.0));   // pieces along the ribbon
                float rCut = step(i.more.y, brk * 0.9 + 0.1 * taper);
                float rMask = CelAAStep(0.04, rField) * rCut;
                half3 rCol = CelRamp(rField, rowB, 1.0);
                float rInk = CelInk(rField, 0.04, inkPx * 0.6) * rMask;
                half3 col = lerp(rCol, CelPalInk(rowB).rgb, rInk) * rMask;
                float a = rMask * lerp(saturate(i.tone.z), 1.0, rInk);

                // ---- the flame: a round head and a tail that tapers to a point, licking at its edges
                {
                    float has = step(1e-5, fl + hr);
                    float headF = 1.0 - length(float2(s, y)) / max(hr, 1e-4);
                    float u = saturate(s / max(fl, 1e-3));
                    float lick = 1.0 + 0.22 * sin(s * 23.0 + seed * 50.0 - i.misc.z * 31.0) * u;
                    float hw = hr * 1.08 * pow(saturate(1.0 - u), 0.75) * lick;
                    float tailF = (s >= 0.0 && s <= fl) ? 1.0 - abs(y) / max(hw, 1e-4) : 0.0;
                    float f = max(headF, tailF);
                    float fMask = CelAAStep(0.03, f) * has;
                    float val = saturate(f) * lerp(1.0, 0.45, u) + 0.25 * saturate(headF);
                    half3 fCol = CelRamp(saturate(val), rowA, i.tone.y);
                    // ink: the screen-lower edge of the head and the tail, and the tail's tip
                    float below = saturate(-y / max(hr * 0.35, 1e-4) + 0.25);
                    float fInk = CelInk(f, 0.03, inkPx) * fMask * max(below, u * u);
                    fCol = lerp(fCol, CelPalInk(rowA).rgb, fInk);
                    col = lerp(col, fCol, fMask);
                    a = lerp(a, lerp(0.8, 1.0, fInk), fMask);
                }

                half3 fogged = CelAdditiveFog(half3(1.0, 1.0, 1.0), i.positionWS, i.misc.x);
                float m = fogged.r;
                return half4(col * m, a * m);
            }
            ENDHLSL
        }
    }
}
