// Phase: B3 (skeleton) — Vertex Animation Texture shader for URP.
// Atlas layout (from VATBaker): U = vertex index / VertexCount, V = (rowStart + frame) / TotalFrames.
// Per-instance data comes from a StructuredBuffer filled by VATRenderer (RenderMeshIndirect).
// Near tier lerps two frames; the distant tier sets _Lerp = 0 for nearest-frame sampling.
Shader "TW/VAT Infantry (URP)"
{
    Properties
    {
        _BaseMap ("Albedo Atlas", 2D) = "white" {}
        _PosTex ("VAT Position (RGBAHalf)", 2D) = "black" {}
        _NrmTex ("VAT Normal (RGBAHalf)", 2D) = "gray" {}
        _VertexCount ("Vertex Count", Float) = 1024
        _TotalFrames ("Total Frames", Float) = 512
        _Lerp ("Frame Lerp (1 near, 0 far)", Range(0,1)) = 1
        _WoundCenter ("Wound Ellipsoid Center", Vector) = (0,0,0,0)
        _WoundRadii ("Wound Ellipsoid Radii", Vector) = (0,0,0,0)
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry" }
        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma target 4.5
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            struct VatInstance { float4x4 objectToWorld; float animRow; float animT; float tint; float lod; };
            StructuredBuffer<VatInstance> _Instances;
            StructuredBuffer<float2> _RowTable;   // x = row start frame, y = row frame count (AnimRow order)

            TEXTURE2D(_BaseMap); SAMPLER(sampler_BaseMap);
            TEXTURE2D(_PosTex);  SAMPLER(sampler_PosTex);
            TEXTURE2D(_NrmTex);  SAMPLER(sampler_NrmTex);
            CBUFFER_START(UnityPerMaterial)
                float _VertexCount, _TotalFrames, _Lerp;
                float4 _WoundCenter, _WoundRadii;
            CBUFFER_END

            struct Attributes { uint vertexID : SV_VertexID; float2 uv : TEXCOORD0; };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2; float tint : TEXCOORD3; float3 positionOS : TEXCOORD4; };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                VatInstance inst = _Instances[instanceID];
                float2 row = _RowTable[(uint)inst.animRow];
                float frame = row.x + frac(inst.animT) * row.y;
                float f0 = floor(frame), f1 = min(f0 + 1.0, row.x + row.y - 1.0);
                float u = (v.vertexID + 0.5) / _VertexCount;
                float3 p0 = SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, (f0 + 0.5) / _TotalFrames), 0).xyz;
                float3 p1 = SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, (f1 + 0.5) / _TotalFrames), 0).xyz;
                float3 n0 = SAMPLE_TEXTURE2D_LOD(_NrmTex, sampler_NrmTex, float2(u, (f0 + 0.5) / _TotalFrames), 0).xyz * 2.0 - 1.0;
                float3 pos = lerp(p0, p1, frac(frame) * _Lerp);
                Varyings o;
                o.positionOS = pos;
                o.positionWS = mul(inst.objectToWorld, float4(pos, 1.0)).xyz;
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.normalWS = normalize(mul((float3x3)inst.objectToWorld, n0));
                o.uv = v.uv;
                o.tint = inst.tint;
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                // B4: ellipsoid wound clip exposes embedded gore geometry
                if (_WoundRadii.x > 0.0)
                {
                    float3 d = (i.positionOS - _WoundCenter.xyz) / _WoundRadii.xyz;
                    clip(dot(d, d) - 1.0);
                }
                half3 albedo = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, i.uv).rgb;
                albedo = lerp(albedo, albedo * half3(0.8, 0.85, 1.0), i.tint);
                Light mainLight = GetMainLight();
                half ndl = saturate(dot(i.normalWS, mainLight.direction));
                half3 color = albedo * (mainLight.color * ndl + half3(0.25, 0.27, 0.3));
                return half4(color, 1.0);
            }
            ENDHLSL
        }
    }
}
