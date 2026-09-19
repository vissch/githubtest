// Phase: B2 (skeleton) — chunked terrain with GPU vertex displacement from an R16 height texture (centimetres).
// Crater stamps update the texture region for dirty chunks; the mesh itself never changes.
Shader "TW/Terrain Displace (URP)"
{
    Properties
    {
        _HeightTex ("Height (R16, cm)", 2D) = "black" {}
        _MapSize ("Map Size (m)", Vector) = (300, 800, 0, 0)
        _MudTex ("Mud Albedo", 2D) = "gray" {}
        _GroundTex ("Ground Albedo", 2D) = "gray" {}
        _CraterMask ("Crater Mask", 2D) = "black" {}
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" }
        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            TEXTURE2D(_HeightTex); SAMPLER(sampler_HeightTex);
            TEXTURE2D(_MudTex); SAMPLER(sampler_MudTex);
            TEXTURE2D(_GroundTex); SAMPLER(sampler_GroundTex);
            TEXTURE2D(_CraterMask); SAMPLER(sampler_CraterMask);
            CBUFFER_START(UnityPerMaterial)
                float4 _MapSize;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; float2 uv : TEXCOORD1; float3 normalWS : TEXCOORD2; };

            float HeightAt(float2 uv) { return SAMPLE_TEXTURE2D_LOD(_HeightTex, sampler_HeightTex, uv, 0).r * 655.35 - 327.68; }

            Varyings vert(Attributes v)
            {
                float3 ws = TransformObjectToWorld(v.positionOS.xyz);
                float2 uv = ws.xz / _MapSize.xy;
                ws.y = HeightAt(uv);
                float2 e = 1.0 / _MapSize.xy;
                float hx = HeightAt(uv + float2(e.x, 0)) - HeightAt(uv - float2(e.x, 0));
                float hz = HeightAt(uv + float2(0, e.y)) - HeightAt(uv - float2(0, e.y));
                Varyings o;
                o.positionWS = ws;
                o.positionCS = TransformWorldToHClip(ws);
                o.uv = uv;
                o.normalWS = normalize(float3(-hx, 2.0, -hz));
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half crater = SAMPLE_TEXTURE2D(_CraterMask, sampler_CraterMask, i.uv).r;
                half3 ground = SAMPLE_TEXTURE2D(_GroundTex, sampler_GroundTex, i.positionWS.xz * 0.15).rgb;
                half3 mud = SAMPLE_TEXTURE2D(_MudTex, sampler_MudTex, i.positionWS.xz * 0.2).rgb;
                half3 albedo = lerp(ground, mud, crater);
                Light mainLight = GetMainLight();
                half ndl = saturate(dot(i.normalWS, mainLight.direction));
                return half4(albedo * (mainLight.color * ndl + half3(0.22, 0.24, 0.27)), 1.0);
            }
            ENDHLSL
        }
    }
}
