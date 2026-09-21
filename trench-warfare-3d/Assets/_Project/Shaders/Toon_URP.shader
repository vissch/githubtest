// Phase: B2 (implemented) — the environment's look (owner's visual target, 2026-09-21): flat cartoon shading in two
// steps, a dark ink outline, distance haze. Used by the ground, the water's bed and every prop.
// Light: half-lambert cut into lit / half / shade, the main light's shadow pushes a pixel into shade.
// Detail: an optional tiling texture in world XZ. R is tone centred on 0.5 (grey = no change); A is a broader tone, read
// a second time turned and 3.5x larger so the repeat never shows; G and B are the surface slope, which _DetailBump
// turns into a painted relief: a pale edge on the side of each clod that faces the light and a dark one on the far
// side, both hard-edged like the rest of the look.
// Outline: an inverted hull pushed out along the smoothed normal stored in TEXCOORD3 (BattlefieldProps.Combine writes
// it; hard-edged meshes would split at the corners otherwise). Its width is set in pixels and shrinks with distance.
// Base-map alpha (the ground writes it): 1 dry, down to 0.7 a slick sheen, 0.5 liquid mud, 0.4 .. 0 standing water from
// its edge to 0.7 m deep, which is painted with the river's depth bands, shore line and rings (TWWater.hlsl).
// Water: _Gloss, or a base-map alpha below 1 (the ground's puddles), mirrors the sky with a fresnel and takes a hard sun
// glint; still water takes a slow ripple from the same slopes. Mist and the fog bank round the battlefield come from
// TWAtmosphere.hlsl (set by Atmosphere).
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
        _DetailBump ("Detail relief", Range(0,1)) = 0
        _Gloss ("Gloss (1 = standing water)", Range(0,1)) = 0
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        [HDR] _Emission ("Emission (lamp glass, embers)", Color) = (0, 0, 0, 0)
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
            half4 _BaseColor, _ShadeColor, _OutlineColor, _Emission;
            float4 _BaseMap_ST;
            float _DetailScale, _DetailStrength, _DetailBump, _OutlineWidth, _Gloss;
        CBUFFER_END
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
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _FORWARD_PLUS
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            #include "Assets/_Project/Shaders/TWWater.hlsl"
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"

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
                half shore = 0;
                if (base.a < 0.45)
                {
                    gloss = 1.0;
                    half lapNoise = frac(dot(floor(i.positionWS.xz * 0.5), float2(0.37, 0.61)));
                    albedo = TWWaterAlbedo((1.0 - base.a / 0.4) * 0.7, i.positionWS.xz, lapNoise, half3(0.36, 0.34, 0.27), half3(0.235, 0.255, 0.215), half3(0.135, 0.165, 0.16), half3(0.70, 0.71, 0.66), 0.45, shore) * i.color.rgb;
                }
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half2 slope = 0;
                if (_DetailStrength > 0.0)
                {
                    float2 uv1 = i.positionWS.xz * _DetailScale;
                    float2 uv2 = float2(uv1.x * 0.259 - uv1.y * 0.117, uv1.x * 0.117 + uv1.y * 0.259) + 0.37;   // turned and 3.5x larger: the broad blotches
                    half3 d1 = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uv1).rgb;
                    half near = 1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 90.0) / 90.0);   // from the overview only the broad tone is left, so the tile never shows
                    half tone = (d1.r - 0.5) * near + (SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uv2).a - 0.5);
                    slope = (d1.gb - 0.5) * near;
                    gloss = max(gloss, _TWWet.x * _DetailBump * (0.30 + 0.25 * saturate(0.5 - d1.r * 1.0 + 0.3)));   // soaked ground: every surface with relief shines a little, the dark crevices most
                    half dry = 1.0 - saturate(gloss * 2.0 - 1.0);   // only standing water is smooth
                    albedo *= 1.0 + tone * 2.0 * _DetailStrength * dry;
                    half relief = dot(slope, mainLight.direction.xz) * _DetailBump * dry;
                    albedo *= 1.0 + smoothstep(0.035, 0.06, relief) * 0.13 - smoothstep(0.03, 0.055, -relief) * 0.20;
                }
                half wrap = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half lit = wrap;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;   // broad lit top planes, readable cool side planes
                half3 color = albedo * lerp(_ShadeColor.rgb * TWShadeTint(), mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation); // contact shadows must survive the toon thresholds
                if (gloss > 0.01)
                {
                    float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                    float3 n = normalize(lerp(normalize(i.normalWS), float3(0, 1, 0), 0.8));   // water lies flat whatever the ground does
                    if (_DetailStrength > 0.0)
                    {
                        // liquid mud keeps the clods' broken surface; still water breathes with a slow ripple
                        float2 uvr = i.positionWS.xz * 0.19 + float2(_Time.y * 0.021, _Time.y * 0.013);
                        half2 ripple = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uvr).gb - 0.5;
                        half still = saturate(gloss * 2.0 - 1.0);
                        n = normalize(n + float3(lerp(slope * 0.35, ripple * 0.22, still), 0).xzy);
                    }
                    float3 r = reflect(-view, n);
                    half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                    half3 sky = TWSky() * lerp(1.08, 0.62, saturate(r.y * 1.4));   // bright at the horizon, darker overhead
                    color = lerp(color, sky, gloss * (0.22 + 0.70 * fresnel) * (1.0 - shore * 0.7));
                    half glint = smoothstep(0.990, 0.994, dot(r, mainLight.direction));
                    color += glint * gloss * mainLight.color * 0.55 * mainLight.shadowAttenuation;
                    // wet sheen: a broad soft highlight toward the light, on top of the hard glint (the moon on soaked mud)
                    color += pow(saturate(dot(r, mainLight.direction)), 14.0) * gloss * _TWWet.y * mainLight.color * mainLight.shadowAttenuation;
                }
                color += albedo * TWLocalLights(i.positionWS, normalize(i.normalWS + float3(slope.x, 0, slope.y) * _DetailBump), i.positionCS);
                color += _Emission.rgb;
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
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
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float fog : TEXCOORD0; float3 positionWS : TEXCOORD1; };

            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
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
            half4 frag(Varyings i) : SV_Target { return half4(MixFog(ApplyFieldFog(_OutlineColor.rgb, i.positionWS), i.fog), 1.0); }
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
