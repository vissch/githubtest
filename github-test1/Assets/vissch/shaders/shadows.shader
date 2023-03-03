Shader "URP Unlit Shadow masked"
{
    Properties
    {
        _BaseMap("Base Map", 2D) = "white"
        _ShadowMap("Shadow Map", 2D) = "white"
        _ShadowRange("Shadow Range", float) = 50.0
    }
    SubShader
    {

        Tags { "RenderType" = "Opaque" "RenderPipeline" = "UniversalRenderPipeline" }

        Pass
        {
            //Name "ForwardLit"
            //Tags { "LightMode" = "UniversalForward" }
            //Blend DstColor Zero, One One   // multiply alpha source value by One instead of Zero to preserve alpha info
            //Cull Back
            //ZTest LEqual
            //ZWrite Off
 
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            #pragma target 2.0
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
            #pragma multi_compile _ _SHADOWS_SOFT
            #pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile _ LIGHTMAP_SHADOW_MIXING // v10+ only, renamed from "_MIXED_LIGHTING_SUBTRACTIVE"
            #pragma multi_compile _ SHADOWS_SHADOWMASK // v10+ only
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"            

            struct Attributes
            {
                float4 positionOS : POSITION;
                float2 uv         : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };
            struct Varyings
            {
                float4 positionCS               : SV_POSITION;
                float3 positionWS               : TEXCOORD3;
                //float fogCoord                  : TEXCOORD1;
                float2 uv                       : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
            };

            // This macro declares _BaseMap as a Texture2D object.
            TEXTURE2D(_BaseMap);
            TEXTURE2D(_ShadowMap);
            // This macro declares the sampler for the _BaseMap texture.
            SAMPLER(sampler_BaseMap);
            SAMPLER(sampler_ShadowMap);

            CBUFFER_START(UnityPerMaterial)
                // The following line declares the _BaseMap_ST variable, so that you
                // can use the _BaseMap variable in the fragment shader. The _ST 
                // suffix is necessary for the tiling and offset function to work.
                float4 _BaseMap_ST;
                float4 _ShadowMap_ST;
                float _ShadowRange;
            CBUFFER_END

            Varyings vert (Attributes input)
            {
                Varyings output;
                UNITY_SETUP_INSTANCE_ID(input);
                UNITY_TRANSFER_INSTANCE_ID(input, output);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);
                VertexPositionInputs vertexInput = GetVertexPositionInputs(input.positionOS.xyz);
                //output.positionCS = vertexInput.positionCS;
                output.positionCS = TransformObjectToHClip(input.positionOS.xyz);

                output.positionWS = vertexInput.positionWS;
                //output.fogCoord = ComputeFogFactor(vertexInput.positionCS.z);
                output.uv = TRANSFORM_TEX(input.uv, _BaseMap);
                return output;
            }
            half4 frag (Varyings input) : SV_Target
            {
                UNITY_SETUP_INSTANCE_ID(input);
                UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(input);
                half4 color = half4(1,1,1,1);
                half4 mainColor = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, input.uv);
                half4 shadowColor = SAMPLE_TEXTURE2D(_ShadowMap, sampler_ShadowMap, input.uv);

            /*#ifdef _MAIN_LIGHT_SHADOWS
                VertexPositionInputs vertexInput = (VertexPositionInputs)0;
                vertexInput.positionWS = input.positionWS;
                float4 shadowCoord = GetShadowCoord(vertexInput);
                half shadowAttenutation = MainLightRealtimeShadow(shadowCoord);
                // lerp from alpha 0 instead of 1 to have the mesh surface be fully transparent:
                color = lerp(half4(1,1,1,0), _ShadowColor, (1.0 - shadowAttenutation) * _ShadowColor.a);
                color.rgb = MixFogColor(color.rgb, half3(1,1,1), input.fogCoord);
            #endif*/

            #ifdef _ADDITIONAL_LIGHT_SHADOWS
                VertexPositionInputs vertexInput = (VertexPositionInputs)0;
                vertexInput.positionWS = input.positionWS;
                
                float4 shadowCoord = GetShadowCoord(vertexInput);

                int lightAmount = GetAdditionalLightsCount();
                half shadowAttenutation = 1;
                half product = 0;
                half combined = 1;
                half radius = 1;
                half shadow = 1;

                for (int i = 0; i < lightAmount; i++) {
                Light light = GetAdditionalLight(i, vertexInput.positionWS);
                //shadowAttenutation = AdditionalLightRealtimeShadow(i, vertexInput.positionWS, light.direction);
                shadowAttenutation = (1 - AdditionalLightRealtimeShadow(i, vertexInput.positionWS, light.direction))*clamp(_ShadowRange * light.distanceAttenuation, 0, 1);
                //shadow = clamp(1-shadowAttenutation,0,1);
                //radius = clamp(50 * light.distanceAttenuation, 0, 1);
                //combined = clamp(radius*shadow,0,1);
                product = clamp(product + shadowAttenutation,0,1);
                }

                // lerp from alpha 0 instead of 1 to have the mesh surface be fully transparent:
                color = lerp(mainColor, shadowColor, product);
                //color.rgb = MixFogColor(color.rgb, half3(1, 1, 1), input.fogCoord);

            #endif
                return color;
            }
            ENDHLSL
        }
    }
}