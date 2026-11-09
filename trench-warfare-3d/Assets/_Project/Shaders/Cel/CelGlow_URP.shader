// Phase: CelFx kit (2026-11-09) - Glow: one quad, additive, banded. Lying on the ground it is a glow pool or a
// widening shock ring; turned to the eye it is a flash, a star burst or a shock ring in the air. The far LOD of most
// effects is one of these. One kind (one draw) for all four, picked per record by extra.x.
//   size.xyz   radius (m) at birth, at death, ease          size.w  unused
//   motion     gravity, drag, spin (rad/s: a star turns), buoyancy
//   erode.xyz  the threshold the radial field is cut at over the life: rising, the glow shrinks in flat steps
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      x mode: 0 ground pool, 1 ground ring, 2 flash (faces the eye), 3 ring facing the eye
//              y falloff power (pool and flash; 0: 1) or ring half-width (rings, 0..1 of the radius; 0: 0.18)
//              z star points (0: round)   w star sharpness (0: 6)
Shader "TW/Cel/Glow"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+15" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelGlow"
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
                float2 uv : TEXCOORD0;          // -1..1, turned by the spin
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;       // x mode, y falloff / ring width, z points, w sharpness
                float4 more : TEXCOORD3;        // x fog, y life 0..1
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float radius = CelCurve(r.size.xyz, k) * alive;
                float3 c = CelPosition(r, t);
                float2 corner = v.uv * 2.0 - 1.0;
                float mode = r.extra.x;
                float3 w;
                if (mode < 1.5) w = c + float3(corner.x, 0.0, corner.y) * radius + float3(0.0, 0.03, 0.0);   // lying on the ground
                else w = CelBillboard(c, corner * radius);
                float s, co; sincos(r.motion.z * t + r.seed * 6.2832, s, co);
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.uv = float2(corner.x * co - corner.y * s, corner.x * s + corner.y * co);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = r.extra;
                o.more = float4(ComputeFogFactor(o.positionCS.z), k, 0, 0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float rr = length(i.uv);
                float mode = i.shape.x;
                float field;
                if (mode > 0.5 && mode < 1.5 || mode > 2.5)
                {
                    float hw = i.shape.y > 0.0 ? i.shape.y : 0.18;
                    field = saturate(1.0 - abs(rr - (1.0 - hw)) / hw);        // 1 on the ring's middle line
                }
                else
                {
                    float r2 = rr;
                    if (i.shape.z > 0.5)
                    {
                        // a star: the radius reaches out along the spikes
                        float a = atan2(i.uv.y, i.uv.x);
                        float spike = pow(abs(cos(a * i.shape.z * 0.5)), i.shape.w > 0.0 ? i.shape.w : 6.0);
                        r2 = rr / lerp(0.32, 1.0, spike);
                    }
                    float p = i.shape.y > 0.0 ? i.shape.y : 1.0;
                    field = pow(saturate(1.0 - r2), p);
                }
                float mask = CelAAStep(max(i.tone.w, 0.01), field);
                half3 col = CelRamp(field, CelRowA(i.tone.x), i.tone.y) * mask * i.tone.z;
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
