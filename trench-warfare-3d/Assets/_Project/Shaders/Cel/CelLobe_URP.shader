// Phase: CelFx kit (2026-11-09) - Lobe: a noise-displaced icosphere, cut out (opaque, alpha-tested, so a smoke wall
// costs no overdraw), banded with the camera as the light, an ink rim of N screen pixels (an inverted hull) and ink on
// the edges its erosion opens. Fireballs, puffs, smoke walls and columns are many lobes.
//   size.xyz   radius (m) at birth, at death, ease          size.w  vertical squash (0 or 1: round; 0.6 flattened)
//   motion     gravity, drag, spin (rad/s: the noise rolls over the lobe, it boils), buoyancy
//   erode.xyz  the erosion threshold at birth, at death, ease (0 whole .. 1 gone)   erode.w feather (an ink band that
//              wide in field units on the eroded edge, on top of the ink px)
//   tone       x palette (+ 256 x the palette it cools to), y HDR core, z unused (cutout), w ink px (0: no rim)
//   extra      x lump frequency (0: 1.3), y lump amplitude (0: 0.14), z light up-bias (0: the eye), w when the cooling
//              starts (life 0..1)
Shader "TW/Cel/Lobe"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+10" "IgnoreProjector"="True" }

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

        LobeOut Lobe(float3 positionOS, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float freq, amp, squash; LobeParams(r, freq, amp, squash);
            float radius = CelCurve(r.size.xyz, k) * alive;
            float3 n = normalize(positionOS);
            float3 s = LobeShape(n, r, t, freq, amp);
            float3 sq = float3(1.0, squash, 1.0);
            float3 center = CelPosition(r, t);
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
        ENDHLSL

        Pass
        {
            Name "CelLobe"
            Tags { "LightMode"="UniversalForward" }
            Cull Back ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 normalWS : TEXCOORD1;
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
                o.normalWS = l.normalWS;
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
                float3 viewWS = normalize(_WorldSpaceCameraPos - i.positionWS);
                // per pixel, so the bands follow the lumps and not the triangles of the mesh
                CelRecord r = _CelRecords[i.id];
                float freq, amp, squash; LobeParams(r, freq, amp, squash);
                float t = CelAge(r);
                float3 n = normalize(i.sphere);
                float3 nrm = LobeNormal(n, LobeShape(n, r, t, freq, amp), r, t, freq, amp, squash);
                float lit = CelLobeLight(nrm, viewWS, i.more.z);
                lit *= lit;   // n.v squared: the bands come out of even width across the disc (n.v alone is mostly light)
                // the eroded edge darkens a band before the ink: the lobe thins toward its holes
                if (e > 0.001) lit *= lerp(0.55, 1.0, saturate((f - e) / max(i.more.y, 1e-3)));
                half3 col = CelRamp2(lit, i.tone.x, i.tone.y, i.more.x, i.more.w);
                float ink = e > 0.001 ? CelInk(f, e, CelLodInk(i.tone.w * 0.8)) : 0.0;
                col = lerp(col, CelInkColor(i.tone.x, i.more.x, i.more.w), ink);
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.misc.y), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "CelLobeInk"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Cull Front ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vertInk
            #pragma fragment fragInk
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; };
            struct InkVaryings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; float3 field : TEXCOORD1; nointerpolation float4 tone : TEXCOORD2; float2 misc : TEXCOORD3; };
            InkVaryings vertInk(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                float4 cs = TransformWorldToHClip(l.positionWS);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, l.normalWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = CelLodInk(r.tone.w) * (l.radius > 0.0 ? 1.0 : 0.0);
                cs.xy += dir * px * 2.0 / _ScreenParams.xy * cs.w;
                InkVaryings o;
                o.positionCS = cs;
                o.positionWS = l.positionWS;
                o.field = l.field;
                o.tone = float4(r.tone.x, CelCurve(r.erode.xyz, l.k), l.k, r.extra.w > 0.0 ? r.extra.w : 0.35);
                o.misc = float2(r.seed, ComputeFogFactor(cs.z));
                return o;
            }
            half4 fragInk(InkVaryings i) : SV_Target
            {
                clip(LobeField(i.field, i.misc.x, i.tone.z) - i.tone.y);
                half3 c = CelInkColor(i.tone.x, i.tone.z, i.tone.w);
                return half4(MixFog(ApplyFieldFog(c, i.positionWS), i.misc.y), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Back
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct Attributes { float4 positionOS : POSITION; };
            struct DepthVaryings { float4 positionCS : SV_POSITION; float3 field : TEXCOORD0; float3 cut : TEXCOORD1; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                DepthVaryings o;
                o.positionCS = TransformWorldToHClip(l.positionWS);
                o.field = l.field;
                o.cut = float3(r.seed, CelCurve(r.erode.xyz, l.k), l.k);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target
            {
                clip(LobeField(i.field, i.cut.x, i.cut.z) - i.cut.y);
                return 0;
            }
            ENDHLSL
        }
    }
}
