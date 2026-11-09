// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - FlameJet: the flamethrower's jet as one
// camera-facing ribbon from the nozzle to the target, widening toward the target, its field made of flowing noise
// tongues and banded into flat cel values (red rim, orange body, yellow, a white HDR core) with an ink edge. Opaque
// cutout (no overdraw, no sort). The front grows out of the nozzle, holds while the record lives, and in its last
// seconds the tail leaves the nozzle and runs into the target (the jet is cut off).
// One record per jet (CelFx.hlsl). The far end is pos + vel x life (spawn it with Aim Target, Speed 0, Arrive 1,
// Spread 0, and no gravity, drag or buoyancy).
//   size       x width at the nozzle (m), y width at the target (m), z width ease power along, w reach past the
//              target (share of the length)
//   erode      x seconds the front takes to reach the target, y seconds the cut-off takes at the end of the life,
//              z tongue amplitude (0: 0.45), w the share of the length the front starts at (it is already out)
//   tone       x palette, y HDR core, z unused, w ink px
//   extra      x flow speed (m/s, the tongues run toward the target), y tongue frequency (1/m; 0: 0.9), z core
//              streak amount (0..1), w unused
Shader "TW/Cel/FlameJet"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+20" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct JetOut { float3 positionWS; float along; float across; float metres; float alive; };

        // the reach of the jet at age t, as shares of the nozzle-target length: x the tail, y the front
        float2 JetSpan(CelRecord r, float t)
        {
            float grow = max(r.erode.x, 1e-3), cut = max(r.erode.y, 1e-3);
            float reach = 1.0 + r.size.w;
            float front = lerp(saturate(r.erode.w), reach, saturate(t / grow));
            float tail = saturate((t - (r.life - cut)) / cut);
            tail = tail * tail * reach;
            return float2(tail, front);
        }

        float JetHalfWidth(CelRecord r, float a)
        {
            return 0.5 * lerp(r.size.x, r.size.y, pow(saturate(a), r.size.z > 0.0 ? r.size.z : 1.0));
        }

        JetOut Jet(float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), alive = CelAlive(r);
            float3 span = r.vel * r.life;
            float len = max(length(span), 1e-3);
            float3 dir = span / len;
            float2 tf = JetSpan(r, t);
            float a = lerp(tf.x, tf.y, uv.x);
            float3 c = r.pos + dir * (a * len);
            float3 toEye = normalize(_WorldSpaceCameraPos - c);
            float3 side = cross(dir, toEye);
            float sl = dot(side, side);
            side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
            // the envelope is wider than the body so the tongues have room; the round front cap reaches past the end
            float hw = JetHalfWidth(r, a) * 1.45 * alive;
            float cap = (uv.x > 0.999) ? hw * 0.6 : 0.0;
            JetOut o;
            o.positionWS = c + side * (uv.y * 2.0 - 1.0) * hw + dir * cap;
            o.along = a + cap / len;
            o.across = (uv.y * 2.0 - 1.0) * 1.45;
            o.metres = (a * len) + cap;
            o.alive = alive;
            return o;
        }

        /// The jet's field at a point: 1 in the middle of the core, 0 on the edge (below 0 outside).
        float JetField(CelRecord r, float along, float across, float metres, out float streak)
        {
            float t = CelAge(r);
            float2 tf = JetSpan(r, t);
            float len = max(length(r.vel * r.life), 1e-3);
            float hw = max(JetHalfWidth(r, saturate(along)), 1e-3);
            float flow = r.extra.x;
            float freq = r.extra.y > 0.0 ? r.extra.y : 0.9;
            float amp = r.erode.z > 0.0 ? r.erode.z : 0.45;
            // the noise runs with the flow; across it is scaled by the local width so the tongues widen with the jet
            float s = metres - t * flow;
            float y = across * hw;
            float3 q = float3(s * freq * 0.55, y * freq * 1.2, t * 0.9 + r.seed * 17.0);
            float n = CelSimplex3(q) * 0.65 + CelSimplex3(q * 2.1 + 5.3) * 0.35;
            float body = 1.0 - abs(across);
            // more ragged toward the target, where the jet breaks into tongues
            float rag = amp * lerp(0.45, 1.0, saturate(along));
            float field = body + n * rag;
            // the front end and the cut-off tail are round, eaten by the same noise
            float toFront = (tf.y - along) * len / (hw * 1.1);
            float toTail = (along - tf.x) * len / (hw * 0.9);
            field = min(field, toFront + n * 0.35);
            if (tf.x > 0.0) field = min(field, toTail + n * 0.35);
            // the white core: long streaks along the flow
            float3 q2 = float3(s * freq * 0.35, y * freq * 3.2, t * 1.3 + r.seed * 7.0);
            streak = CelSimplex3(q2) * (r.extra.z);
            return field;
        }
        ENDHLSL

        Pass
        {
            Name "CelFlameJet"
            Tags { "LightMode"="UniversalForward" }
            Cull Off ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 jet : TEXCOORD1;          // x along (share of the length), y across (-1.45..1.45), z metres along
                float fog : TEXCOORD2;
                nointerpolation uint id : TEXCOORD3;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                JetOut j = Jet(v.uv, instanceID);
                Varyings o;
                o.positionWS = j.positionWS;
                o.positionCS = TransformWorldToHClip(j.positionWS);
                o.jet = float3(j.along, j.across, j.metres);
                o.fog = ComputeFogFactor(o.positionCS.z);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float streak;
                float f = JetField(r, i.jet.x, i.jet.y, i.jet.z, streak);
                clip(f);
                // the value the bands are cut from: the field, lifted along the white streaks, a little hotter at the nozzle
                float v = saturate(f * 1.05 + streak * 0.35 + 0.12 * (1.0 - saturate(i.jet.x * 1.5)));
                half3 col = CelRamp(v, CelRowA(r.tone.x), r.tone.y);
                float ink = CelInk(f, 0.0, CelLodInk(r.tone.w));
                col = lerp(col, CelPalInk(CelRowA(r.tone.x)).rgb, ink);
                return half4(MixFog(col, i.fog), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Off
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct DepthVaryings { float4 positionCS : SV_POSITION; float3 jet : TEXCOORD0; nointerpolation uint id : TEXCOORD1; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                JetOut j = Jet(v.uv, instanceID);
                DepthVaryings o;
                o.positionCS = TransformWorldToHClip(j.positionWS);
                o.jet = float3(j.along, j.across, j.metres);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float streak;
                clip(JetField(r, i.jet.x, i.jet.y, i.jet.z, streak));
                return 0;
            }
            ENDHLSL
        }
    }
}
