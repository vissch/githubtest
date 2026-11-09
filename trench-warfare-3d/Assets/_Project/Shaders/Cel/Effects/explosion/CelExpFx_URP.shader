// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - ExpFx: the explosion's own additive kind, one strip mesh
// (CelMeshes.Strip(8)) and one draw for three looks, picked per record by extra.x:
//   0 soft ground pool   a smooth radial falloff lying on the ground, white-yellow middle to nothing (no bands)
//   1 soft flash         the same field turned to the eye: a dome of light, a fireball's white heart, a bloom halo
//   2 jagged arc         a white spark arc from the record outward along its velocity, kinked at three knots that
//                        re-jump 12 times a second (the reference's lightning-like spark arcs)
// The glows fade smoothly (light, not a shape); the arc is banded and cut like a streak.
//   size.xyz   glows: radius (m) at birth, at death, ease        arc: width (m) at birth, at death, ease
//   size.w     arc: length (m) (x 0.45..1 by the record's seed)
//   erode.xyz  glows: how far it has faded (0 full .. 1 gone) over the life; arc: the cross-section cut threshold
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      x mode, y glows: falloff power (0: 1.5), z arc: kink size (x length); flash: the ground cut (m below
//              the centre; 0 none: a dome of light with a flat foot, as the clip's), w unused
// A flash is drawn pulled toward the eye (same screen size, ExpPull of its distance), so a ruin or bank between it
// and the eye does not hide a far dome; its fog is taken where it really is.
// Palette use: glows blend c1 (edge) to c3 (middle) and to core x hdr above the core threshold; arcs use CelRamp.
Shader "TW/Cel/ExpFx"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+18" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelExpFx"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off ZTest LEqual Cull Off
            Offset -1, -1
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
                float2 uv : TEXCOORD0;                          // glows: -1..1 corner; arc: x 0 root .. 1 tip, y -1..1
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w fade / cut threshold
                nointerpolation float4 shape : TEXCOORD2;       // extra
                float4 more : TEXCOORD3;                        // x fog
                float3 positionWS : TEXCOORD4;
            };

            // the arc's sideways offset at knot i of 4 sections (0 at the root and the tip), -1..1
            #define ExpPull 0.55

            float ExpKnot(float i, float seed, float tick)
            {
                if (i < 0.5 || i > 3.5) return 0.0;
                return CelHash21(float2(seed * 97.13 + i * 1.7, tick + seed * 13.0)) * 2.0 - 1.0;
            }

            float ExpZig(float a, float seed, float tick)
            {
                float s = a * 4.0;
                float i = min(floor(s), 3.0);
                return lerp(ExpKnot(i, seed, tick), ExpKnot(i + 1.0, seed, tick), s - i);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 c = CelPosition(r, t);
                float mode = r.extra.x;
                float2 corner = v.uv * 2.0 - 1.0;
                float3 w, wf;
                float2 uv;
                float cut = 1.0;
                if (mode < 1.5)
                {
                    float radius = CelCurve(r.size.xyz, k) * alive;
                    if (mode < 0.5) { w = c + float3(corner.x, 0.0, corner.y) * radius + float3(0.0, 0.03, 0.0); wf = w; }
                    else
                    {
                        wf = CelBillboard(c, corner * radius);
                        w = _WorldSpaceCameraPos + (wf - _WorldSpaceCameraPos) * ExpPull;
                        cut = r.extra.z > 0.0 ? wf.y - (c.y - r.extra.z) : 1.0;
                    }
                    uv = corner;
                }
                else
                {
                    float3 vel = CelVelocity(r, t);
                    float sp = length(vel);
                    float3 dir = sp > 1e-4 ? vel / sp : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                    float width = CelCurve(r.size.xyz, k) * alive;
                    float len = r.size.w * lerp(0.45, 1.0, r.seed) * alive;
                    float3 toEye = normalize(_WorldSpaceCameraPos - c);
                    float3 side = cross(dir, toEye);
                    float sl = dot(side, side);
                    side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                    float a = v.uv.x;
                    float tick = floor(_CelNow * 12.0);
                    float zig = ExpZig(a, r.seed, tick) * r.extra.z * len;
                    float taper = lerp(1.0, 0.4, a);
                    w = c + dir * (a * len) + side * (zig + corner.y * width * 0.5 * taper);
                    wf = w;
                    uv = float2(a, corner.y);
                }
                Varyings o;
                o.positionWS = wf;
                o.positionCS = TransformWorldToHClip(w);
                o.uv = uv;
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = r.extra;
                o.more = float4(ComputeFogFactor(TransformWorldToHClip(wf).z), k, cut, 0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                half3 col;
                if (i.shape.x < 1.5)
                {
                    float field = saturate(1.0 - length(i.uv));
                    float p = i.shape.y > 0.0 ? i.shape.y : 1.5;
                    float env = pow(field, p);
                    half4 core = CelPalCore(row);
                    col = lerp(CelPal(row, 1), CelPal(row, 3), (half)smoothstep(0.0, 0.45, field));
                    col = lerp(col, core.rgb * (half)max(i.tone.y, 0.0), (half)smoothstep(min(core.a, 0.99), 1.0, field));
                    col *= (half)(env * i.tone.z * saturate(1.0 - i.tone.w) * saturate(i.more.z * 40.0));
                }
                else
                {
                    float across = saturate(1.0 - abs(i.uv.y));
                    float field = across * lerp(1.0, 0.7, i.uv.x) * saturate((1.0 - i.uv.x) * 8.0 + 0.2);
                    float mask = CelAAStep(max(i.tone.w, 0.02), field);
                    col = CelRamp(field, row, i.tone.y) * (half)(mask * i.tone.z);
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
