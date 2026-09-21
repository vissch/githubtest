// Phase: B2 (implemented) — the river and every flooded shell hole: one flat sheet at the water table.
// Budget first: the sheet is OPAQUE. No blending, no grab pass, no scene-depth read, no reflection camera; one draw
// call and three texture reads a pixel (depth map, ripple map twice). Everything a see-through water shader would
// learn from the depth buffer comes instead from _DepthMap, which GreyboxTerrainView bakes from the ground
// (R = (depth + 0.4 m) / 2 m, so a little of the bank is in it and the shoreline interpolates cleanly) and repaints
// where a shell lands.
// Look: depth cut into three painted bands (silty margin, olive body, dark channel) whose edges wobble with the
// ripples; a pale wet line at the shore and slow contour rings lapping toward it; long pale streaks drifting with the
// current; the sky mirrored with a fresnel through the rippled normal; a hard sun glint that breaks into sparkles.
// It takes the main light's shadows, the ground mist, the fog bank and the distance haze like the ground around it.
Shader "TW/Water (URP)"
{
    Properties
    {
        _DepthMap ("Depth map (R = (depth + 0.4) / 2)", 2D) = "white" {}
        _RippleMap ("Ripple map (RG slope, B streaks)", 2D) = "gray" {}
        _DepthST ("World XZ to depth map UV (scale xy, offset zw)", Vector) = (0.011, 0.004, 0, 0)
        _Flow ("Flow (m/s, world XZ)", Vector) = (0.35, 0.05, 0, 0)
        _Shallow ("Silty margin", Color) = (0.36, 0.34, 0.27, 1)
        _Body ("Body", Color) = (0.235, 0.255, 0.215, 1)
        _Deep ("Channel", Color) = (0.135, 0.165, 0.16, 1)
        _Foam ("Shore line", Color) = (0.70, 0.71, 0.66, 1)
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _Ripple ("Ripple strength", Range(0, 1)) = 0.35
        _Rings ("Shore rings", Range(0, 1)) = 0.45
        _Streaks ("Current streaks", Range(0, 1)) = 0.5
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry+10" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        TEXTURE2D(_DepthMap); SAMPLER(sampler_DepthMap);
        TEXTURE2D(_RippleMap); SAMPLER(sampler_RippleMap);
        CBUFFER_START(UnityPerMaterial)
            float4 _DepthST, _Flow;
            half4 _Shallow, _Body, _Deep, _Foam, _ShadeColor;
            float _Ripple, _Rings, _Streaks;
        CBUFFER_END
        ENDHLSL

        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _FORWARD_PLUS
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            #include "Assets/_Project/Shaders/TWWater.hlsl"
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"

            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; float fog : TEXCOORD1; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float2 xz = i.positionWS.xz;
                float2 drift = _Flow.xy * _Time.y;
                // two ripple layers riding the current at different speeds; the second runs a little across it
                half4 r1 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, (xz - drift) / 6.0);
                half4 r2 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, (xz - drift * 0.55 + float2(-drift.y, drift.x) * 0.3) / 15.0 + 0.41);
                half2 slope = ((r1.rg - 0.5) + (r2.rg - 0.5) * 0.8) * _Ripple;

                // depth in metres, read a little off-centre so every band edge wobbles with the ripples
                float depth = SAMPLE_TEXTURE2D(_DepthMap, sampler_DepthMap, (xz + slope * 1.1) * _DepthST.xy + _DepthST.zw).r * 2.0 - 0.4;
                // depth bands, the shore line, lapping rings and the rings men and shells throw (TWWater.hlsl)
                half shore;
                half3 albedo = TWWaterAlbedo(depth, xz, r2.b, _Shallow.rgb, _Body.rgb, _Deep.rgb, _Foam.rgb, _Rings, shore);

                // current streaks: long pale strokes where two drifting layers agree, none over the margin
                half streak = smoothstep(0.63, 0.68, r1.b * 0.6 + r2.b * 0.4) * _Streaks * smoothstep(0.10, 0.30, depth);
                albedo = lerp(albedo, albedo * 1.35 + 0.035, streak);

                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half3 color = albedo * lerp(_ShadeColor.rgb * TWShadeTint(), mainLight.color, 0.5 + 0.5 * mainLight.shadowAttenuation);

                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                float3 n = normalize(float3(slope.x * 0.6, 1.0, slope.y * 0.6));
                float3 r = reflect(-view, n);
                half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                half3 sky = TWSky() * half3(0.93, 0.98, 1.05) * lerp(1.08, 0.58, saturate(r.y * 1.4));   // bright at the horizon, darker overhead, a little colder than the haze
                half mirror = (0.14 + 0.62 * fresnel) * (1.0 - shore * 0.7);
                color = lerp(color, sky, mirror);
                half glint = smoothstep(0.988, 0.994, dot(r, mainLight.direction)) * 0.6;
                color += glint * mainLight.color * 0.6 * mainLight.shadowAttenuation;
                color += pow(saturate(dot(r, mainLight.direction)), 14.0) * _TWWet.y * 0.2 * mainLight.color * mainLight.shadowAttenuation;
                // lanterns and muzzle flashes lie on the water as a warm pool: lit body plus a mirrored core
                half3 local = TWLocalLights(i.positionWS, n, i.positionCS);
                color += (albedo + 0.25) * local;

                color = lerp(color, ApplyMist(color, i.positionWS), 0.55);   // the sheet lies below the mist's top everywhere: full mist would paint the whole river white
                color = ApplyFieldFog(color, i.positionWS);
                color = MixFog(color, i.fog);
                return half4(color, 1.0);
            }
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
            float4 vertDepth(float4 positionOS : POSITION) : SV_POSITION { return TransformObjectToHClip(positionOS.xyz); }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
