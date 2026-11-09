// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - LightningBeam: a camera-facing ribbon laid
// along a straight beam, with a pulse window that runs down it. Inside the window the beam is a spindle (pointed at
// both ends) whose centre line zig-zags a little and whose outline is ragged, re-drawn at 16 Hz (the flicker of the cel
// clip); short branch arcs leave it at random cells (Close only: they fade out going to Mid). Banded with CelRamp:
// a cyan body with a thin white core and a dark ink edge (premultiplied: the body and the ink cover what is behind),
// and a dim blue halo around it that adds light. One record per pulse; the beam is the record's own velocity x life
// (CelAim.Target, Speed 0, Arrive 1, no drag or gravity: vel x life = target - start).
// r02: a record with size.w < 0 is an impact DOT instead: a camera-facing star at the record's place, additive.
// Going out (_CelLod.x toward 2) the steady thread and the dot keep a minimum size in screen pixels and see through
// most of the fog, so the Far tier (thread + dot, one draw) still reads at O120 and O240.
//   size.xyz   beam: width (m) of the spindle's widest part at birth, at death, ease.  dot: radius (m)
//   size.w     beam: the pulse window's length (share of the beam, 0..1; 1 and extra.z = extra.w = 1: the whole beam,
//              the steady thread, which shows only going out to Mid/Far).  dot: minus the radius in px at Far
//   erode.xyz  the threshold the cross-section (dot: the radial field) is cut at over the life (it thins in steps)
//   tone       x palette, y HDR core, z opacity, w beam: clip, the window never reaches past this share of the beam
//              (1: stops at the target; above 1: the pulse runs on through it)
//   extra      beam: x zig-zag amplitude (share of the width, 0..1.5), y branch arcs per metre (0: none),
//              z the window's head (share of the beam) at birth, w at death.  dot: x unused, y unused,
//              z star points (0: round), w star sharpness
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
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            // the ribbon is this many half-widths to each side, room for the zig-zag, the halo and the branches
            #define BEAM_PAD 6.0

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;                         // beam: x 0..1 along the window, y across in half-widths, z metres along, w half-width (m). dot: xy -1..1, w < 0
                nointerpolation float4 tone : TEXCOORD1;         // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;        // x zig amp, y branches per m, z seed, w window length (m) | dot: z points, w sharpness
                float4 more : TEXCOORD3;                         // x fog, y far (0..1: how much the fog is skipped)
                float3 positionWS : TEXCOORD4;
            };

            // metres per screen pixel at a world point (perspective)
            float PixelMetres(float3 p)
            {
                // |P[1][1]|: the projection is flipped when URP renders to a texture; never more than 1/200 of the distance
                float d = distance(_WorldSpaceCameraPos, p);
                return d * 2.0 / max(abs(UNITY_MATRIX_P[1][1]) * _ScreenParams.y, 400.0);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float k = CelLife01(r), alive = CelAlive(r);
                float far = saturate(_CelLod.x - 1.0);
                Varyings o;
                if (r.size.w < 0.0)
                {
                    // the impact dot: a camera-facing quad, at least -size.w px in radius at Far (half that at Mid)
                    float3 c = r.pos;
                    float minPx = -r.size.w * saturate(_CelLod.x * 0.5);
                    float rad = max(CelCurve(r.size.xyz, k), minPx * PixelMetres(c)) * alive;
                    float2 corner = float2(v.uv.x * 2.0 - 1.0, v.uv.y * 2.0 - 1.0);
                    float3 w = CelBillboard(c, corner * rad);
                    o.positionWS = w;
                    o.positionCS = TransformWorldToHClip(w);
                    o.beam = float4(corner, 0.0, -1.0);
                    o.tone = float4(r.tone.x, r.tone.y, r.tone.z * (1.0 + 1.5 * far), CelCurve(r.erode.xyz, k));
                    o.shape = float4(0.0, 0.0, r.extra.z, r.extra.w);
                    o.more = float4(ComputeFogFactor(o.positionCS.z), far, 0, 0);
                    return o;
                }
                float3 span = r.vel * r.life;                     // start -> target
                float lenB = max(length(span), 1e-3);
                float3 dir = span / lenB;
                float head = lerp(r.extra.z, r.extra.w, k);
                float s0 = max(head - r.size.w, 0.0);
                float s1 = min(head, r.tone.w > 0.0 ? r.tone.w : 1.0);
                float open = (s1 > s0 + 1e-4) ? 1.0 : 0.0;
                bool thread = r.size.w > 0.999;
                float width = CelCurve(r.size.xyz, k) * alive * open;
                float s = lerp(s0, s1, v.uv.x);
                float3 p = r.pos + span * s;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float across = v.uv.y * 2.0 - 1.0;
                float halfW = width * 0.5;
                // going out the line keeps at least ~1.3 px of half-width (2.5 px wide at Far)
                halfW = max(halfW, 1.3 * saturate(_CelLod.x * 0.5) * PixelMetres(p) * alive * open);
                float3 w = p + side * across * halfW * BEAM_PAD;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                // the window's own 0..1 is measured against its full length, so a window clipped at the target keeps its head shape
                float full = max(r.size.w, 1e-4);
                float a = saturate((s - (head - r.size.w)) / full);
                // the steady thread is for going out: hidden at Close, it fades in toward Mid and brightens at Far
                float op = r.tone.z * (thread ? saturate(_CelLod.x * 1.4 - 0.4) * (1.0 + 2.0 * far) : 1.0);
                o.beam = float4(a, across * BEAM_PAD, s * lenB, max(halfW, 1e-4));
                o.tone = float4(r.tone.x, r.tone.y, op, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, r.extra.y * saturate(1.0 - _CelLod.x), r.seed, full * lenB);
                o.more = float4(ComputeFogFactor(o.positionCS.z), far, thread ? 1.0 : 0.0, 0);
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

            // an additive colour through the fog; at Far most of the fog is skipped so the line and the dot read
            half3 AddFog(half3 c, float3 ws, float fog, float far)
            {
                return lerp(CelAdditiveFog(c, ws, fog), c * 0.9, far * 0.85);
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                if (i.beam.w < 0.0)
                {
                    // the dot: a small star, radial field banded, additive
                    float2 q = i.beam.xy;
                    float rr = length(q);
                    float pts = i.shape.z;
                    float spike = pts > 0.5 ? pow(abs(cos(atan2(q.y, q.x) * pts * 0.5)), max(i.shape.w, 1.0)) : 1.0;
                    float reach = lerp(0.45, 1.0, spike);
                    float fieldD = saturate(1.0 - rr / reach);
                    float maskD = CelAAStep(max(i.tone.w, 0.02), fieldD);
                    half3 cd = CelRamp(fieldD, row, i.tone.y) * maskD * i.tone.z;
                    return half4(AddFog(cd, i.positionWS, i.more.x, i.more.y), 0.0);
                }
                float a = i.beam.x, y = i.beam.y, u = i.beam.z;
                float tick = floor(_CelNow * 16.0);
                float salt = i.shape.z * 97.0 + tick * 13.7;
                // the spindle: pointed at both ends, fullest a little behind the head
                float prof = pow(saturate(sin(3.14159 * pow(saturate(a), 1.6))), 0.55);
                // the centre line zig-zags (two scales, in half-widths) and the outline is ragged
                float zig = (Zig(u, 0.42, salt) + 0.45 * Zig(u, 0.13, salt + 5.1)) * i.shape.x * 1.6;
                float rag = 1.0 + 0.35 * Zig(u, 0.07, salt + 9.3);
                float w = prof * rag;
                float dy = abs(y - zig * prof);
                float field = saturate(1.0 - dy / max(w, 1e-3));
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
                        float lenA = cell * (0.35 + 0.5 * CelHash21(float2(id, salt + 3.0)));
                        float t = (u - x0) / lenA;
                        if (t < 0.0 || t > 1.0) continue;
                        float sgn = CelHash21(float2(id, salt + 4.0)) > 0.5 ? 1.0 : -1.0;
                        float reach = BEAM_PAD * (0.4 + 0.4 * CelHash21(float2(id, salt + 6.0)));
                        float yb = sgn * (pow(t, 0.8) * reach + 0.6 * Zig(u, 0.06, salt + id)) + zig * prof;
                        float bw = 0.4 * (1.0 - t * 0.6);
                        br = max(br, saturate(1.0 - abs(y - yb) / bw) * 0.55 * step(0.15, prof));
                    }
                }
                field = max(field, br);
                float thr = max(i.tone.w, 0.02);
                float mask = CelAAStep(thr, field);
                // the ink: a dark line just inside the edge (thinner going out; none on the far thread)
                float inkPx = CelLodInk(0.9) * (1.0 - i.more.z);
                float ink = CelInk(field, thr, inkPx) * mask;
                half3 body = CelRamp(field, row, i.tone.y);
                half3 inkC = CelPalInk(row).rgb;
                half3 straight = lerp(body, inkC, ink);
                straight = MixFog(straight, i.more.x);
                float cover = mask * saturate(i.tone.z * 1.4);
                // the halo: dim blue light a few half-widths around the spindle (not around the arcs)
                float halo = saturate(1.0 - (dy - w) / max(4.8 * prof, 1e-3)) * step(0.05, prof) * (1.0 - mask);
                halo = CelAAStep(0.55, halo) * 0.35 + CelAAStep(0.02, halo) * 0.65;
                half3 add = CelPal(row, 0) * halo * 0.8 * i.tone.z * (1.0 - i.more.z);
                // the far thread is all light: no cover, it adds
                if (i.more.z > 0.5) { add += body * mask * i.tone.z; cover = 0.0; straight = 0; }
                add = AddFog(add, i.positionWS, i.more.x, i.more.y);
                return half4(straight * cover + add, cover);
            }
            ENDHLSL
        }
    }
}
