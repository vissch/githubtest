// Artillery Close smoke: one joined surface, cavity shadows and broad connected hot paint.
// Record motion, size, erosion and palette cooling retain the Lobe conventions. UV0 stores concavity/convexity.
Shader "TW/Cel/ArtilleryBillow"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+10" "IgnoreProjector"="True" }
        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 shape : TEXCOORD0; };
        struct BillowSurface { float3 positionWS; float3 normalWS; float3 local; float3 field; float k; };

        float3 BillowWarp(float3 p, CelRecord r, float age)
        {
            float3 axis = dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0);
            float3 q = CelRotate(p, axis, r.motion.z * age);
            float freq = r.extra.x > 0.0 ? r.extra.x : 1.3;
            float amp = r.extra.y > 0.0 ? r.extra.y : 0.14;
            return p * (1.0 + amp * CelSimplex3(q * freq + r.seed * 31.7));
        }

        BillowSurface Billow(Attributes v, CelRecord r)
        {
            float age = CelAge(r), k = CelLife01(r);
            float radius = CelCurve(r.size.xyz, k) * CelAlive(r);
            float squash = r.size.w > 0.0 ? r.size.w : 1.0;
            float3 p = v.positionOS.xyz;
            float3 n = normalize(v.normalOS);
            float3 tangent = normalize(cross(n, abs(n.y) < 0.9 ? float3(0, 1, 0) : float3(1, 0, 0)));
            float3 bitangent = cross(n, tangent);
            float3 s = BillowWarp(p, r, age);
            float3 normal = normalize(cross(BillowWarp(p + tangent * 0.015, r, age) - s,
                                           BillowWarp(p + bitangent * 0.015, r, age) - s));
            float angle = r.seed * 6.2831853;
            s = CelRotate(s, float3(0, 1, 0), angle);
            normal = CelRotate(normal, float3(0, 1, 0), angle);
            float3 center = CelPosition(r, age);
            float3 view = normalize(_WorldSpaceCameraPos - center);
            BillowSurface o;
            o.positionWS = center + s * float3(1, squash, 1) * radius;
            o.normalWS = normalize(normal / float3(1, squash, 1));
            o.local = p;
            // As in Lobe, front/back use the same middle-plane erosion so the holes remain open.
            o.field = (s - view * dot(s, view)) + r.seed * 13.0;
            o.k = k;
            return o;
        }

        float BillowField(float3 field, float seed, float k)
        {
            float rim = 1.0 - saturate(length(field - seed * 13.0));
            return saturate(CelFbm3(field * 1.7 + k * 0.6) * 0.62 + rim * 0.5);
        }
        ENDHLSL

        Pass
        {
            Name "ArtilleryBillow"
            Tags { "LightMode"="UniversalForward" }
            Cull Back ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 field : TEXCOORD1;
                float3 local : TEXCOORD2;
                float3 normalWS : TEXCOORD3;
                float2 shape : TEXCOORD4;
                float fog : TEXCOORD5;
                nointerpolation uint id : TEXCOORD6;
            };
            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                BillowSurface b = Billow(v, r);
                Varyings o;
                o.positionWS = b.positionWS;
                o.positionCS = TransformWorldToHClip(b.positionWS);
                o.field = b.field; o.local = b.local; o.normalWS = b.normalWS; o.shape = v.shape;
                o.fog = ComputeFogFactor(o.positionCS.z);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half3 BillowPaint(uint row, float cavity, float cream, float core, float heat, float hdr)
            {
                half3 colour = lerp(CelPal(row, 1), CelPal(row, 2), cream);
                colour = lerp(colour, CelPal(row, 3), core);
                half4 glow = CelPalCore(row);
                if (glow.a < 1.0) colour = lerp(colour, glow.rgb * max(hdr, 0.0), CelAAStep(glow.a, heat));
                return lerp(colour, CelPal(row, 0), cavity);
            }

            half4 frag(Varyings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float k = CelLife01(r), e = CelCurve(r.erode.xyz, k);
                float field = BillowField(i.field, r.seed, k);
                clip(field - e);
                float3 axis = dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0);
                float3 q = CelRotate(i.local, axis, r.motion.z * CelAge(r));
                float broad = CelSimplex3(q * 1.25 + r.seed * 19.7);
                float underlight = -normalize(i.normalWS).y * 0.10;
                // Concave joins carry the shadow; broad paint only breaks its boundary, never draws a disc center.
                float pocket = i.shape.x * 1.25 + broad * 0.12;
                float cavity = CelAAStep(0.24, pocket);
                float cream = CelAAStep(-0.08, broad + i.shape.y * 0.24 - i.shape.x * 0.32 + underlight);
                float core = CelAAStep(0.23, broad + i.shape.y * 0.27 - i.shape.x * 0.60 + underlight);
                float heat = saturate(0.52 + broad * 0.5 + i.shape.y * 0.18 - i.shape.x * 0.4);
                float coolFrom = r.extra.w > 0.0 ? r.extra.w : 0.35;
                uint hot = CelRowA(r.tone.x), cool = CelRowB(r.tone.x);
                half3 colour = BillowPaint(hot, cavity, cream, core, heat, r.tone.y);
                if (cool != hot && cool != 0u)
                    colour = lerp(colour, BillowPaint(cool, cavity, cream, core, heat, r.tone.y), smoothstep(coolFrom, 1.0, k));
                float gate = CelAAStep(0.12, broad);
                float crease = 1.0 - smoothstep(0.5, 1.0 + CelLodInk(r.tone.w * 0.5), abs(pocket - 0.24) / max(fwidth(pocket), 1e-5));
                float holeInk = e > 0.001 ? CelInk(field, e, CelLodInk(r.tone.w * 0.65)) : 0.0;
                colour = lerp(colour, CelInkColor(r.tone.x, k, coolFrom), max(crease * gate * 0.55, holeInk * gate));
                colour = ApplyMist(colour, i.positionWS);
                colour = ApplyFieldFog(colour, i.positionWS);
                return half4(MixFog(colour, i.fog), 1);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            Cull Back ZWrite On ColorMask R
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct DepthVaryings { float4 positionCS : SV_POSITION; float3 field : TEXCOORD0; nointerpolation float3 cut : TEXCOORD1; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                BillowSurface b = Billow(v, r);
                DepthVaryings o;
                o.positionCS = TransformWorldToHClip(b.positionWS);
                o.field = b.field; o.cut = float3(r.seed, CelCurve(r.erode.xyz, b.k), b.k);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target { clip(BillowField(i.field, i.cut.x, i.cut.z) - i.cut.y); return 0; }
            ENDHLSL
        }
    }
}
