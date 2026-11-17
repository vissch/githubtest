// Phase: CelFx kit (2026-11-09) - Lobe: a noise-displaced icosphere, cut out (opaque, alpha-tested, so a smoke wall
// costs no overdraw), banded with the camera as the light, an ink rim of N screen pixels (an inverted hull) and ink on
// the edges its erosion opens. Fireballs, puffs, smoke walls and columns are many lobes.
//   size.xyz   radius (m) at birth, at death, ease          size.w  vertical squash (0 or 1: round; 0.6 flattened)
//   motion     gravity, drag, spin (rad/s: the noise rolls over the lobe, it boils), buoyancy
//   erode.xyz  the erosion threshold at birth, at death, ease (0 whole .. 1 gone)   erode.w feather (an ink band that
//              wide in field units on the eroded edge, on top of the ink px)
//   tone       x palette (+ 256 x the palette it cools to), y HDR core, z unused for ordinary lobes, w ink px
//   extra      x lump frequency (0: 1.3), y lump amplitude (0: 0.14), z light up-bias (0: the eye), w when the cooling
//              starts (life 0..1). Negative w is reserved for prepared FlameSmoke records: -w is time scale,
//              tone.z is emitter start plus phase delay. Body transitions warm -> dark cool -> lighter cool;
//              ink follows the warm/cool mix without the body's temporary cool-brightness reduction.
Shader "TW/Cel/FlameSmokeSoft"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct LobeOut
        {
            float3 positionWS; float3 normalWS; float3 field; float k; float radius;
        };

        float3 LobeShape(float3 n, CelRecord r, float t, float freq, float amp)
        {
            // the noise domain turns about the record's axis: the lumps roll over the surface (it boils), the lobe stays put
            float3 axis = dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0);
            float3 q = CelRotate(n, axis, r.motion.z * t);
            float lump = CelSimplex3(q * freq + r.seed * 31.7);
            return n * (1.0 + amp * lump);
        }

        void LobeParams(CelRecord r, out float freq, out float amp, out float squash)
        {
            freq = r.extra.x > 0.0 ? r.extra.x : 1.3; amp = r.extra.y > 0.0 ? r.extra.y : 0.14;
            squash = r.size.w > 0.0 ? r.size.w : 1.0;
        }

        /// The world normal of the displaced, squashed surface over the sphere direction n, from two neighbours.
        float3 LobeNormal(float3 n, float3 s, CelRecord r, float t, float freq, float amp, float squash)
        {
            float3 t1 = normalize(cross(n, abs(n.y) < 0.9 ? float3(0, 1, 0) : float3(1, 0, 0)));
            float3 t2 = cross(n, t1);
            const float e = 0.06;
            float3 a = LobeShape(normalize(n + t1 * e), r, t, freq, amp), b = LobeShape(normalize(n + t2 * e), r, t, freq, amp);
            float3 nrm = normalize(cross(a - s, b - s));
            if (dot(nrm, n) < 0.0) nrm = -nrm;
            return normalize(nrm / float3(1.0, squash, 1.0));
        }

        // Prepared bridge direction uses oct11x11 in negative size.w; original spin/noise fields are untouched.
        float3 BridgeCenter(CelRecord r, float3 original)
        {
            if (r.extra.w >= 0.0 || r.size.w > -1024.0 || r.size.w < -4195327.0) return original;
            float elapsed = (_CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
            if (elapsed <= 4.0) return original; // exact original geometry through frame103 in the fixed capture
            uint bits = (uint)(-r.size.w - 1024.0);
            float2 q = float2(bits & 2047u, (bits >> 11) & 2047u) * (2.0 / 2047.0) - 1.0;
            float3 n = float3(q, 1.0 - abs(q.x) - abs(q.y));
            if (n.z < 0.0)
                n.xy = (1.0 - abs(q.yx)) * float2(q.x < 0.0 ? -1.0 : 1.0, q.y < 0.0 ? -1.0 : 1.0);
            return original + normalize(n) * (1.0228514 * smoothstep(4.0, 4.7, elapsed));
        }

        LobeOut Lobe(float3 positionOS, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float freq, amp, squash; LobeParams(r, freq, amp, squash);
            float radius = CelCurve(r.size.xyz, k) * alive;
            float3 n = normalize(positionOS);
            float3 s = LobeShape(n, r, t, freq, amp);
            float3 sq = float3(1.0, squash, 1.0);
            float3 center = BridgeCenter(r, CelPosition(r, t));
            LobeOut o;
            o.positionWS = center + s * sq * radius;
            o.normalWS = LobeNormal(n, s, r, t, freq, amp, squash);
            // the erosion field is read where the eye ray crosses the lobe's middle plane, so a hole goes through
            // the front and the back together and the background shows through it
            float3 view = normalize(_WorldSpaceCameraPos - center);
            o.field = (s - view * dot(s, view)) + r.seed * 13.0;
            o.k = k;
            o.radius = radius;
            return o;
        }

        /// The lobe's erosion field: noise, biased so the rim goes before the middle. 0..1, 1 = most solid.
        float LobeField(float3 field, float seed, float k)
        {
            float3 q = field - seed * 13.0;
            float rim = 1.0 - saturate(length(q));
            return saturate(CelFbm3(field * 1.7 + k * 0.6) * 0.62 + rim * 0.5);
        }
        // Shared by the body and ink hull. Ordinary lobes retain their per-particle cooling curve.
        float2 LobePalettePhase(CelRecord r, float k)
        {
            float coolFrom = r.extra.w > 0.0 ? r.extra.w : 0.35;
            if (r.extra.w >= 0.0) return float2(smoothstep(coolFrom, 1.0, k), 1.0);
            float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
            return float2(smoothstep(3.5, 4.8, elapsed), lerp(0.5, 1.0, smoothstep(5.2, 5.75, elapsed)));
        }

        half3 LobeBodyPalette(float lit, float packedRow, float hdr, float2 phase)
        {
            uint a = CelRowA(packedRow), b = CelRowB(packedRow);
            half3 warm = CelRamp(lit, a, hdr);
            if (b == a || b == 0u) return warm;
            return lerp(warm, CelRamp(lit, b, hdr) * phase.y, phase.x);
        }

        half3 LobeInkPalette(float packedRow, float mix)
        {
            uint a = CelRowA(packedRow), b = CelRowB(packedRow);
            half3 warm = CelPalInk(a);
            return b != a && b != 0u ? lerp(warm, CelPalInk(b), mix) : warm;
        }

        float SmokeSoftPhase(CelRecord r)
        {
            if (r.extra.w >= 0.0) return 0.0;
            float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
            return smoothstep(5.2, 5.75, elapsed);
        }
        float SmokeCoverage(CelRecord r, float distance)
        {
            float phase = SmokeSoftPhase(r);
            if (phase <= 0.0) return 1.0;
            return lerp(1.0, smoothstep(0.0, 0.12, distance), phase);
        }

        ENDHLSL

        Pass
        {
            Name "CelLobe"
            Tags { "LightMode"="UniversalForward" }
            Cull Back ZWrite Off ZTest LEqual
            Blend SrcAlpha OneMinusSrcAlpha
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                nointerpolation float2 palettePhase : TEXCOORD1; // warm/cool mix, cool brightness
                float3 field : TEXCOORD2;
                nointerpolation float4 tone : TEXCOORD3;        // x palette, y hdr, z erosion threshold, w ink px
                nointerpolation float4 more : TEXCOORD4;        // x life 0..1, y feather, z up-bias, w cool from
                float2 misc : TEXCOORD5;        // x seed, y fog
                float3 sphere : TEXCOORD6;      // the undisplaced direction: the normal is worked out again per pixel
                nointerpolation uint id : TEXCOORD7;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                Varyings o;
                o.positionWS = l.positionWS;
                o.positionCS = TransformWorldToHClip(l.positionWS);
                o.palettePhase = LobePalettePhase(r, l.k);
                o.field = l.field;
                o.tone = float4(r.tone.x, r.tone.y, CelCurve(r.erode.xyz, l.k), r.tone.w);
                o.more = float4(l.k, r.erode.w, r.extra.z, r.extra.w > 0.0 ? r.extra.w : 0.35);
                o.misc = float2(r.seed, ComputeFogFactor(o.positionCS.z));
                o.sphere = normalize(v.positionOS.xyz);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float f = LobeField(i.field, i.misc.x, i.more.x);
                float e = i.tone.z;
                clip(f - e);
                CelRecord smokeRecord = _CelRecords[i.id];
                float coverage = SmokeCoverage(smokeRecord, f - e);
                if (coverage >= 0.999 || coverage <= 0.005) discard;
                float3 viewWS = normalize(_WorldSpaceCameraPos - i.positionWS);
                // per pixel, so the bands follow the lumps and not the triangles of the mesh
                CelRecord r = _CelRecords[i.id];
                float freq, amp, squash; LobeParams(r, freq, amp, squash);
                float t = CelAge(r);
                float3 n = normalize(i.sphere);
                float3 nrm = LobeNormal(n, LobeShape(n, r, t, freq, amp), r, t, freq, amp, squash);
                float lit;
                if (r.extra.w < 0.0)
                {
                    float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
                    if (elapsed < 3.5)
                    {
                        // Warm smoke: broaden upper-face fill without changing palette endpoints.
                        float shadeUp = i.more.z * lerp(0.5, 1.0, smoothstep(3.0, 3.5, elapsed));
                        lit = CelLobeLight(nrm, viewWS, shadeUp);
                    }
                    else lit = CelLobeLight(nrm, viewWS, i.more.z);
                }
                else lit = CelLobeLight(nrm, viewWS, i.more.z);
                lit *= lit;   // n.v squared: the bands come out of even width across the disc (n.v alone is mostly light)
                // the eroded edge darkens a band before the ink: the lobe thins toward its holes
                if (e > 0.001) lit *= lerp(0.55, 1.0, saturate((f - e) / max(i.more.y, 1e-3)));
                half3 col = LobeBodyPalette(lit, i.tone.x, i.tone.y, i.palettePhase);
                // FlameSmoke fire stays at broken lower edges, with dark lobe centres.
                // Emitter time prevents newly born bridge smoke relighting after burnout.
                if (r.extra.w < 0.0)
                {
                    float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
                    float rise = CelPosition(r, t).y - r.pos.y;
                    float heat = (1.0 - smoothstep(0.7, 1.6, t))
                        * (1.0 - smoothstep(0.6, 1.8, rise))
                        * (1.0 - smoothstep(3.9, 4.5, elapsed));
                    float3 fireDomain = n * 3.2 + float3(t * 0.65, -t * 1.4, r.seed * 19.0);
                    float rolling = CelSimplex3(fireDomain);
                    float broken = smoothstep(-0.15, 0.18, rolling);
                    float lower = 1.0 - smoothstep(-0.1, 0.6, n.y);
                    float width = (0.22 + 0.18 * saturate(rolling + 0.3)) * heat * lower * broken;
                    float nv = saturate(dot(nrm, viewWS));
                    float flame = CelAAStep(nv, width) * step(0.015, width);
                    float hot = CelAAStep(nv, width * 0.35) * step(0.015, width);
                    col = lerp(col, half3(1.0, 0.30, 0.03), flame);
                    col = lerp(col, half3(1.0, 0.72, 0.14), hot);
                }
                float ink = e > 0.001 ? CelInk(f, e, CelLodInk(i.tone.w * 0.8)) : 0.0;
                col = lerp(col, LobeInkPalette(i.tone.x, i.palettePhase.x), ink * coverage * (1.0 - SmokeSoftPhase(r)));
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.misc.y), coverage);
            }
            ENDHLSL
        }

    }
}
