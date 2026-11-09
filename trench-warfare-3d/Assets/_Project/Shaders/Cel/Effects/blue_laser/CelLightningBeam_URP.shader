// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - LightningBeam: a camera-facing ribbon laid
// along a straight beam, with a pulse window that runs down it. Inside the window the beam is a spindle (pointed at
// both ends) whose centre line zig-zags and whose outline is ragged, re-drawn at 16 Hz (the flicker of the cel clip);
// short branch arcs leave it at random cells (Close only: they fade out going to Mid). Banded with CelRamp, a white
// HDR core, additive. One record per pulse; the beam is the record's own velocity x life (CelAim.Target, Speed 0,
// Arrive 1, no drag or gravity: vel x life = target - start).
//   size.xyz   width (m) of the spindle's widest part at birth, at death, ease
//   size.w     the pulse window's length (share of the beam, 0..1; 1 and extra.z = extra.w = 1: the whole beam)
//   erode.xyz  the threshold the cross-section is cut at over the life (it thins in steps)
//   tone       x palette, y HDR core, z opacity, w clip: the window never reaches past this share of the beam (1: stops
//              at the target; above 1: the pulse runs on through it)
//   extra      x zig-zag amplitude (share of the width, 0..1.5), y branch arcs per metre (0: none),
//              z the window's head (share of the beam) at birth, w at death
Shader "TW/Cel/LightningBeam"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+21" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelLightningBeam"
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

            // the ribbon is this many half-widths to each side, room for the zig-zag and the branches
            #define BEAM_PAD 5.0

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;                         // x 0..1 along the window, y across in half-widths, z metres along the beam, w half-width (m)
                nointerpolation float4 tone : TEXCOORD1;         // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;        // x zig amp, y branches per m, z seed, w window length (m)
                float4 more : TEXCOORD3;                         // x fog
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float k = CelLife01(r), alive = CelAlive(r);
                float3 span = r.vel * r.life;                     // start -> target
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                float head = lerp(r.extra.z, r.extra.w, k);
                float s0 = max(head - r.size.w, 0.0);
                float s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                float s = lerp(s0, s1, v.uv.x);
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float across = v.uv.y * 2.0 - 1.0;
                float halfW = width * 0.5;
                float3 w = p + side * across * halfW * BEAM_PAD;
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                // the window's own 0..1 is measured against its full length, so a window clipped at the target keeps its head shape
                float full = max(r.size.w, 1e-4);
                float a = saturate((s - (head - r.size.w)) / full);
                o.beam = float4(a, across * BEAM_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(1.0 - _CelLod.x), r.seed, full * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), 0, 0, 0);
                return o;
            }

            // a zig-zag: random offsets -1..1 at cell corners, joined by straight lines (cells of `cell` metres)
            float Zig(float u, float cell, float salt)
            {
                float x = u / cell;
                float i = floor(x), f = x - i;
                float a = CelHash21(float2(i, salt)) * 2.0 - 1.0;
                float b = CelHash21(float2(i + 1.0, salt)) * 2.0 - 1.0;
                return lerp(a, b, f);
            }

            half4 frag(Varyings i) : SV_Target
            {
                float a = i.beam.x, y = i.beam.y, u = i.beam.z, hw = i.beam.w;
                float tick = floor(_CelNow * 16.0);
                float salt = i.shape.z * 97.0 + tick * 13.7;
                // the spindle: pointed at both ends, fullest a little behind the head
                float prof = pow(saturate(sin(3.14159 * pow(saturate(a), 1.6))), 0.55);
                // the centre line zig-zags (two scales, in half-widths) and the outline is ragged
                float zig = (Zig(u, 0.42, salt) + 0.45 * Zig(u, 0.13, salt + 5.1)) * i.shape.x * 1.6;
                float rag = 1.0 + 0.4 * Zig(u, 0.07, salt + 9.3);
                float w = prof * rag;
                float field = saturate(1.0 - abs(y - zig * prof) / max(w, 1e-3));
                field = field * lerp(0.75, 1.0, saturate(a * 1.5));
                // branch arcs: in some cells along the beam a thin crooked arc leaves the core and runs out to one side
                float br = 0.0;
                if (i.shape.y > 0.0)
                {
                    const float cell = 0.55;
                    float ci = floor(u / cell);
                    [unroll] for (int c = 0; c < 2; c++)
                    {
                        float id = ci - c;
                        float h = CelHash21(float2(id, salt + 31.0));
                        if (h > i.shape.y * cell) continue;
                        float x0 = id * cell + CelHash21(float2(id, salt + 2.0)) * cell * 0.5;
                        float lenA = cell * (0.6 + 0.9 * CelHash21(float2(id, salt + 3.0)));
                        float t = (u - x0) / lenA;
                        if (t < 0.0 || t > 1.0) continue;
                        float sgn = CelHash21(float2(id, salt + 4.0)) > 0.5 ? 1.0 : -1.0;
                        float reach = BEAM_PAD * (0.45 + 0.45 * CelHash21(float2(id, salt + 6.0)));
                        float yb = sgn * (pow(t, 0.8) * reach + 0.6 * Zig(u, 0.06, salt + id)) + zig * prof;
                        float bw = 0.32 * (1.0 - t * 0.7);
                        br = max(br, saturate(1.0 - abs(y - yb) / bw) * 0.62 * step(0.15, prof));
                    }
                }
                field = max(field, br);
                float mask = CelAAStep(max(i.tone.w, 0.02), field);
                half3 col = CelRamp(field, CelRowA(i.tone.x), i.tone.y);
                col *= mask * i.tone.z;
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
