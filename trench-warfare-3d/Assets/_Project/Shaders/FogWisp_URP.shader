// Phase: B2 (implemented) — drifting fog in front of things: a few large soft cards along the fog bank and low over no
// man's land, all in ONE mesh and one draw call (FogWisps.cs builds it). The vertex shader does the work: each card
// slides along its own line and wraps round (fading out at both ends of the run, so the wrap never shows), and turns
// about the vertical to face the camera. The fragment is one texture read and one depth read: the card melts into
// whatever it meets instead of cutting it, fades out close to the camera so it never fills the screen, and thins where
// the player's men are (TWAtmosphere's presence map), so the fight stays clear.
// Vertex data: position = the card's home (world), uv0 = corner (-1..1), uv1 = drift direction xz, run length, phase,
// uv2 = width, height, opacity, speed (m/s).
Shader "TW/Fog Wisp (URP)"
{
    Properties
    {
        _MainTex ("Cloud (R)", 2D) = "white" {}
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+20" "IgnoreProjector"="True" }
        Pass
        {
            Name "Wisp"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            TEXTURE2D(_MainTex); SAMPLER(sampler_MainTex);
            CBUFFER_START(UnityPerMaterial)
                float4 _MainTex_ST;
            CBUFFER_END
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 corner : TEXCOORD0; float4 drift : TEXCOORD1; float4 shape : TEXCOORD2; };
            struct Varyings { float4 positionCS : SV_POSITION; float4 uv : TEXCOORD0; float3 data : TEXCOORD1; float4 screen : TEXCOORD2; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                float cycle = frac(_Time.y * v.shape.w / max(v.drift.z, 1.0) + v.drift.w);
                float3 centre = v.positionOS.xyz + float3(v.drift.x, 0, v.drift.y) * (cycle - 0.5) * v.drift.z;
                float3 right = normalize(float3(UNITY_MATRIX_V[0].x, 0, UNITY_MATRIX_V[0].z));
                float3 ws = centre + right * v.corner.x * v.shape.x * 0.5 + float3(0, (v.corner.y * 0.5 + 0.5) * v.shape.y, 0);
                o.positionCS = TransformWorldToHClip(ws);
                float2 uv = v.corner * 0.5 + 0.5;
                o.uv = float4(uv, uv * float2(0.6, 1.0) + float2(_Time.y * 0.006 + v.drift.w * 3.7, v.drift.w));   // the cloud inside the card creeps too
                half presence = 0;
                if (_TWQuiet.x > 0.0)
                {
                    float2 puv = (centre.xz - _TWField.xy) / max(_TWField.zw - _TWField.xy, 1.0);
                    presence = SAMPLE_TEXTURE2D_LOD(_TWPresence, sampler_TWPresence, puv, 0).r * step(0.0, min(puv.x, puv.y)) * step(max(puv.x, puv.y), 1.0);
                }
                o.data = float3(v.shape.z * sin(cycle * 3.14159) * (1.0 - presence * 0.85), ComputeFogFactor(o.positionCS.z), o.positionCS.w);
                o.screen = ComputeScreenPos(o.positionCS);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half cloud = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv.zw).r;
                float2 c = i.uv.xy * 2.0 - 1.0;
                half body = saturate(1.0 - dot(c, c)) * smoothstep(0.0, 0.35, i.uv.y);   // round, and rooted softly in the ground
                float eye = i.data.z;
                float scene = LinearEyeDepth(SampleSceneDepth(i.screen.xy / i.screen.w), _ZBufferParams);
                half soft = saturate((scene - eye) / 4.0) * saturate((eye - 14.0) / 26.0);
                half alpha = saturate(cloud * 1.6 - 0.25) * body * soft * i.data.x * _TWFieldFogColor.a;
                return half4(MixFog(_TWFieldFogColor.rgb * 1.04, i.data.y), alpha);
            }
            ENDHLSL
        }
    }
}
