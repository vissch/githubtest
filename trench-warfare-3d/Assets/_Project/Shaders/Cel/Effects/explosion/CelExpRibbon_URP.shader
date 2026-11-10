// Phase: CelFx job (2026-11-10, lane/show/cel-explosion, round d20) - ExpRibbon: a path-ribbon spark streak.
// A thin camera-facing strip (CelMeshes.Strip(16)) that follows the record's own past path (CelPosition sampled back
// in time, so gravity and drag bend it) plus a per-record bend in the screen plane that grows toward the tail (a C or
// hook curve, sign and size from the seed) and a small S wiggle. It tapers from a round head to a pointed tail, a white
// HDR core with a faint warm edge (CelRamp of the palette), and is cut by erosion like the kit's Streak. Additive, one
// indirect draw for every record. Drop-in for a Streak layer: the same size/erode/tone fields.
//   size.xyz   width (m) at birth, at death, ease          size.w  trail length in seconds of its path (0: a dot)
//   erode.xyz  the cross-section cut threshold over the life
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      x tail width share (0 pointed .. 1 square), y max length (m, 0: none), z flicker 0..1,
//              w bend: the tail's sideways offset as a share of the length (signed per record by the seed)
// Kept for a build by Resources/ShaderKeep/KeepCelExpRibbon.mat. Written to lift into the kit unchanged.
Shader "TW/Cel/ExpRibbon"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+21" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelExpRibbon"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
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
                float2 uv : TEXCOORD0;                          // x 0 head .. 1 tail, y -1..1 across
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;       // x tail width share, y unused, z flicker, w seed
                float4 more : TEXCOORD3;                        // x fog
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float s = v.uv.x;                                  // 0 head .. 1 tail
                float width = CelCurve(r.size.xyz, k) * alive;
                float3 head = CelPosition(r, t);
                float3 hv = CelVelocity(r, t);
                float hs = length(hv);
                float3 hdir = hs > 1e-3 ? hv / hs : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                // the time the trail spans: size.w seconds, cut so the path is no longer than extra.y metres
                float span = max(r.size.w, 0.0);
                float fullLen = distance(head, CelPosition(r, max(t - span, 0.0)));
                if (r.extra.y > 0.0 && fullLen > r.extra.y) span *= r.extra.y / fullLen;
                float ts = max(t - s * span, 0.0);
                float3 p = CelPosition(r, ts);
                float len = distance(head, CelPosition(r, max(t - span, 0.0)));
                // the bend: a sideways offset in the screen plane growing as s^2 (tangent to the motion at the head)
                float3 toEye = normalize(_WorldSpaceCameraPos - head);
                float3 bside = cross(hdir, toEye);
                float bl = dot(bside, bside);
                bside = bl > 1e-6 ? bside * rsqrt(bl) : UNITY_MATRIX_V[0].xyz;
                float sgn = CelHash11(r.seed * 71.3) < 0.5 ? -1.0 : 1.0;
                float bend = r.extra.w * sgn * lerp(0.4, 1.0, CelHash11(r.seed * 23.9));
                float wig = 0.06 * sin(s * 6.2831853 + r.seed * 40.0);
                p += bside * len * (bend * s * s + wig * s);
                // the path's tangent at s (d/ds), for the strip's side
                float3 tg = -CelVelocity(r, ts) * span + bside * len * (bend * 2.0 * s + 0.06 * (sin(s * 6.2831853 + r.seed * 40.0) + s * 6.2831853 * cos(s * 6.2831853 + r.seed * 40.0)));
                float tl = dot(tg, tg);
                float3 dir = tl > 1e-8 ? -tg * rsqrt(tl) : hdir;
                float3 side = cross(dir, normalize(_WorldSpaceCameraPos - p));
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                // taper: full at the head, extra.x share at the tail; the head end rounded over its first 6%
                float taper = lerp(1.0, saturate(r.extra.x), s) * sqrt(saturate(s / 0.06 + 0.15));
                float halfW = width * 0.5 * max(taper, 0.0);
                // a little cap ahead of the head so it reads round, not cut
                float3 w = p + dir * (s < 1e-3 ? width * 0.35 : 0.0) + side * (v.uv.y * 2.0 - 1.0) * halfW;
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.uv = float2(s, v.uv.y * 2.0 - 1.0);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, 0.0, r.extra.z, r.seed);
                o.more = float4(ComputeFogFactor(o.positionCS.z), k, 0.0, 0.0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float s = i.uv.x;
                float across = saturate(1.0 - abs(i.uv.y));
                // white along the core near the head, warm toward the edge and the tail
                float field = across * lerp(1.0, 0.45, s);
                float flick = 1.0 - i.shape.z * step(0.5, CelHash11(floor(_CelNow * 24.0) + i.shape.w * 37.0));
                float mask = CelAAStep(max(i.tone.w, 0.02), field);
                half3 col = CelRamp(field, CelRowA(i.tone.x), i.tone.y);
                col *= (half)(mask * i.tone.z * flick);
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
