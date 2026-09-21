// Phase: B2 (implemented) — the environment's look (owner's visual target, 2026-09-21): flat cartoon shading in two
// steps, a dark ink outline, distance haze. Used by the ground, the water's bed and every prop.
// Light: half-lambert cut into lit / half / shade, the main light's shadow pushes a pixel into shade.
// Detail: an optional tiling texture in world XZ (brush strokes on the ground), centred on 0.5 so grey = no change.
// Outline: an inverted hull pushed out along the smoothed normal stored in TEXCOORD3 (BattlefieldProps.Combine writes
// it; hard-edged meshes would split at the corners otherwise). Its width is set in pixels and shrinks with distance.
// Water: _Gloss, or a base-map alpha below 1 (the ground's puddles), mirrors the sky with a fresnel and takes a hard sun
// glint. Mist: _TWMist / _TWMistColor (set by Atmosphere) lay a pale layer over low, distant ground.
// The ground switches the pass off (SetShaderPassEnabled("SRPDefaultUnlit", false)): its ink is in its texture.
Shader "TW/Toon (URP)"
{
    Properties
    {
        _BaseColor ("Color", Color) = (1,1,1,1)
        _BaseMap ("Base Map", 2D) = "white" {}
        _DetailMap ("World Detail (grey = none)", 2D) = "gray" {}
        _DetailScale ("Detail tiles per metre", Float) = 0.125
        _DetailStrength ("Detail strength", Range(0,1)) = 0
        _Gloss ("Gloss (1 = standing water)", Range(0,1)) = 0
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _OutlineColor ("Outline", Color) = (0.13, 0.10, 0.08, 1)
        _OutlineWidth ("Outline width (pixels up close)", Float) = 2.6
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        TEXTURE2D(_BaseMap); SAMPLER(sampler_BaseMap);
        TEXTURE2D(_DetailMap); SAMPLER(sampler_DetailMap);
        CBUFFER_START(UnityPerMaterial)
            half4 _BaseColor, _ShadeColor, _OutlineColor;
            float4 _BaseMap_ST;
            float _DetailScale, _DetailStrength, _OutlineWidth, _Gloss;
        CBUFFER_END
        float4 _TWMist;        // x top height, y 1/depth, z start distance, w 1/range
        float4 _TWMistColor;   // rgb, a = density (0 when no Atmosphere is present)
        half3 ApplyMist(half3 color, float3 positionWS)
        {
            float far = saturate((distance(_WorldSpaceCameraPos, positionWS) - _TWMist.z) * _TWMist.w);
            float low = saturate((_TWMist.x - positionWS.y) * _TWMist.y);
            return lerp(color, _TWMistColor.rgb, low * far * _TWMistColor.a);
        }
        ENDHLSL

        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 uv : TEXCOORD0; half4 color : COLOR; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2; half4 color : COLOR; float fog : TEXCOORD3; };

            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.uv = TRANSFORM_TEX(v.uv, _BaseMap);
                o.color = v.color;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half4 base = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, i.uv);
                half3 albedo = base.rgb * _BaseColor.rgb * i.color.rgb;
                half gloss = max(_Gloss, 1.0 - base.a);
                half detail = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, i.positionWS.xz * _DetailScale).r;
                albedo *= 1.0 + (detail - 0.5) * 2.0 * _DetailStrength;
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half wrap = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half lit = wrap;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;   // broad lit top planes, readable cool side planes
                half3 color = albedo * lerp(_ShadeColor.rgb, mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation); // contact shadows must survive the toon thresholds
                if (gloss > 0.01)
                {
                    float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                    float3 n = normalize(lerp(normalize(i.normalWS), float3(0, 1, 0), 0.8));   // water lies flat whatever the ground does
                    float3 r = reflect(-view, n);
                    half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                    half3 sky = unity_FogColor.rgb * lerp(1.08, 0.62, saturate(r.y * 1.4));   // bright at the horizon, darker overhead
                    color = lerp(color, sky, gloss * (0.22 + 0.70 * fresnel));
                    half glint = smoothstep(0.990, 0.994, dot(r, mainLight.direction));
                    color += glint * gloss * mainLight.color * 0.55 * mainLight.shadowAttenuation;
                }
                color = ApplyMist(color, i.positionWS);
                color = MixFog(color, i.fog);
                return half4(color, 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "Outline"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Cull Front
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float fog : TEXCOORD0; };

            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                float3 n = dot(v.smoothOS, v.smoothOS) > 0.01 ? v.smoothOS : v.normalOS;
                float4 cs = TransformObjectToHClip(v.positionOS.xyz);
                float3 nWS = TransformObjectToWorldNormal(n);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, nWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = clamp(_OutlineWidth * 70.0 / max(cs.w, 1.0), 0.0, _OutlineWidth);   // full width within 70 m, thinner beyond
                cs.xy += dir * px * 2.0 / _ScreenParams.xy * cs.w;
                o.positionCS = cs;
                o.fog = ComputeFogFactor(cs.z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target { return half4(MixFog(_OutlineColor.rgb, i.fog), 1.0); }
            ENDHLSL
        }

        Pass
        {
            Name "ShadowCaster"
            Tags { "LightMode"="ShadowCaster" }
            ZWrite On ZTest LEqual ColorMask 0
            HLSLPROGRAM
            #pragma vertex vertShadow
            #pragma fragment fragNull
            #pragma multi_compile_instancing
            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
            float3 _LightDirection;
            float3 _LightPosition;
            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; UNITY_VERTEX_INPUT_INSTANCE_ID };
            float4 vertShadow(Attributes v) : SV_POSITION
            {
                UNITY_SETUP_INSTANCE_ID(v);
                float3 ws = TransformObjectToWorld(v.positionOS.xyz);
                float3 n = TransformObjectToWorldNormal(v.normalOS);
            #if _CASTING_PUNCTUAL_LIGHT_SHADOW
                float3 lightDir = normalize(_LightPosition - ws);
            #else
                float3 lightDir = _LightDirection;
            #endif
                float4 cs = TransformWorldToHClip(ApplyShadowBias(ws, n, lightDir));
            #if UNITY_REVERSED_Z
                cs.z = min(cs.z, UNITY_NEAR_CLIP_VALUE);
            #else
                cs.z = max(cs.z, UNITY_NEAR_CLIP_VALUE);
            #endif
                return cs;
            }
            half4 fragNull() : SV_Target { return 0; }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            #pragma multi_compile_instancing
            struct Attributes { float4 positionOS : POSITION; UNITY_VERTEX_INPUT_INSTANCE_ID };
            float4 vertDepth(Attributes v) : SV_POSITION { UNITY_SETUP_INSTANCE_ID(v); return TransformObjectToHClip(v.positionOS.xyz); }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
