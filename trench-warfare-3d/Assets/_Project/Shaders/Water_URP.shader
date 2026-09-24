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
        _Body ("Body", Color) = (0.17, 0.185, 0.155, 1)
        _Deep ("Channel", Color) = (0.10, 0.12, 0.115, 1)
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
                // Ice does not flow. Killing the scroll at its source stops BOTH ripple layers, which is the
                // half cycle 12 could not reach from a colour: it gave winter the right hue and left the sheet
                // drifting downstream. Zero on every field but winter, so nothing else changes by a bit.
                half ice = saturate(_TWLiquidIce);
                // metres between the taps the buckle gradient is read from, and how hard the sheet heaves
                // along a break. IceBuckle is REASONED, NOT MEASURED: the gradient across a 0.42 m tap at
                // domain scale 0.32 is about 0.13 in distance-field units, and 0.9 puts the perturbed normal
                // in the range the ripple map used to supply. It wants a capture before anyone trusts it.
                const float IceTap = 0.42, IceBuckle = 0.9;
                float2 drift = _Flow.xy * _Time.y * (1.0 - ice);
                // two ripple layers riding the current at different speeds; the second runs a little across it
                half4 r1 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, (xz - drift) / 6.0);
                half4 r2 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, (xz - drift * 0.55 + float2(-drift.y, drift.x) * 0.3) / 15.0 + 0.41);
                half2 slope = ((r1.rg - 0.5) + (r2.rg - 0.5) * 0.8) * _Ripple * (1.0 - ice);   // ice is flat: no rippled normal, so no wobbling mirror

                // depth in metres, read a little off-centre so every band edge wobbles with the ripples
                float depth = SAMPLE_TEXTURE2D(_DepthMap, sampler_DepthMap, (xz + slope * 1.1) * _DepthST.xy + _DepthST.zw).r * 2.0 - 0.4;
                // depth bands, the shore line, lapping rings and the rings men and shells throw (TWWater.hlsl)
                half shore;
                // Rings are water being displaced - lapping at the shore, and the ones men and shells throw. A
                // man does not ripple a frozen puddle. The shore LINE is separate and is left alone.
                half3 albedo = TWWaterAlbedo(depth, xz, r2.b, _Shallow.rgb, _Body.rgb, _Deep.rgb, _Foam.rgb, _Rings * (1.0 - ice), shore);

                // current streaks: long pale strokes where two drifting layers agree, none over the margin
                half streak = smoothstep(0.63, 0.68, r1.b * 0.6 + r2.b * 0.4) * _Streaks * smoothstep(0.10, 0.30, depth) * (1.0 - ice);   // long pale strokes are a current's signature
                albedo = lerp(albedo, albedo * 1.35 + 0.035, streak);
                // The liquid belongs to the biome. _Shallow/_Body/_Deep are this MATERIAL's colours - the night
                // mud river - and until now no field could change them, so the lava river came out a cream band
                // at saturation 0.18 across a picture sitting at 0.63. Zero alpha leaves the night field exactly
                // as it was.
                albedo = lerp(albedo, _TWLiquid.rgb, _TWLiquid.a);
                if (ice > 0.0)
                {
                    // Cracks, from the SAME Worley border the lava crust reads for its plates: a frozen sheet
                    // and a cooling one break the same way, so this needs no new noise and no new texture.
                    //
                    // MEASURED AND WIDENED, 2026-09-24. The first version used one 0.085 smoothstep, which at
                    // domain scale 0.32 is a feathered band about 0.4 m across on the ground, and it measured
                    // FLATTER than the snow beside it (luma spread 0.061 against 0.161). A fracture is a line:
                    // a pale shoulder where the sheet has shattered, and a dark water line down the centre.
                    half c0 = TWPlateEdge(xz * 0.32);
                    half cx = TWPlateEdge((xz + float2(IceTap, 0.0)) * 0.32);
                    half cz = TWPlateEdge((xz + float2(0.0, IceTap)) * 0.32);
                    half lip  = 1.0 - smoothstep(0.0, 0.030, c0);                          // the white shoulder
                    half fine = 1.0 - smoothstep(0.0, 0.018, TWPlateEdge(xz * 0.85 + 31.7));  // the finer set, ~1.2 m
                    half core = 1.0 - smoothstep(0.0, 0.007, c0);                          // the dark line at the break
                    // The crack colour is not a new global: it is this surface's own albedo taken white, the
                    // way the current streaks above already do it, so the ice keeps the biome's colour and a
                    // crack is simply where the sheet has gone white - and, at its centre, where it has opened.
                    albedo = lerp(albedo, min(half3(1, 1, 1), albedo * 2.2 + 0.30), saturate(lip + fine * 0.55) * ice);
                    albedo = lerp(albedo, albedo * 0.45, core * 0.7 * ice);

                    // THE SHEET IS NOT FLAT, and that is why there was no glare to measure at all. W2 forces
                    // slope to zero for ice, correctly - a frozen pan must not ripple - but `n` is built from
                    // slope below, so the whole sheet had the normal (0,1,0) and the glint's very tight cone was
                    // off everywhere under a low moon. Ice heaves where it broke, so the GRADIENT of the crack
                    // field is the buckle, read from the two taps this already needed. No ripple map, no flow.
                    slope += half2(c0 - cx, c0 - cz) * (IceBuckle * ice);
                }

                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half3 color = albedo * lerp(_ShadeColor.rgb * TWShadeTint(), mainLight.color, 0.5 + 0.5 * mainLight.shadowAttenuation);

                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                float3 n = normalize(float3(slope.x * 0.6, 1.0, slope.y * 0.6));
                float3 r = reflect(-view, n);
                half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                half3 sky = TWSky() * half3(0.93, 0.98, 1.05) * lerp(1.08, 0.58, saturate(r.y * 1.4));   // bright at the horizon, darker overhead, a little colder than the haze
                // Molten rock does not mirror the sky. Leaving this in was most of why the river read as a sheet
                // of something rather than a flow of anything.
                half mirror = min(0.14 + 0.62 * fresnel, 0.34) * (1.0 - shore * 0.7) * saturate(1.0 - _TWLiquidHeat);
                color = lerp(color, sky, mirror);
                half glint = smoothstep(0.988, 0.994, dot(r, mainLight.direction)) * 0.6;
                color += glint * mainLight.color * 0.6 * mainLight.shadowAttenuation;
                // W10, the glare. Fresh snow is deliberately matt (SnowSparkle 0.15) so that this is the one
                // hard highlight in a winter frame. Broader than the glint above on purpose: a sheet lit at a
                // glancing angle should read as a sheen across the whole pan, not as one hot pixel, and the
                // buckled normal above is what gives it anything to vary over. Zero on every other field.
                half iceGlare = pow(saturate(dot(r, mainLight.direction)), 42.0) * ice;
                color += iceGlare * mainLight.color * 1.9 * mainLight.shadowAttenuation;
                color += pow(saturate(dot(r, mainLight.direction)), 14.0) * _TWWet.y * 0.2 * mainLight.color * mainLight.shadowAttenuation;
                // lanterns and muzzle flashes lie on the water as a warm pool: lit body plus a mirrored core
                half3 lampGlint;
                half3 local = TWLocalLights(i.positionWS, n, i.positionCS, view, 1.0, lampGlint);
                color += (albedo + 0.25) * local + lampGlint * 1.2;

                // Melt, computed BEFORE the fog and added AFTER it - the reason the ground gives at
                // Toon_URP.shader:230: emission is seen THROUGH haze, and lerping it toward the fog colour turns
                // distant melt pale pink instead of orange behind pink. TWMolten is the same field the ground
                // reads for its open pools, so the river and the pools agree about where the heat is rather than
                // being two unrelated oranges. The distance term is TWHeatGlow's, so the far river dims the same
                // way the far ground does instead of blooming into a band.
                half3 melt = 0.0;
                if (_TWLiquidHeat > 0.0)
                {
                    half flow = TWMolten(xz * 0.032);
                    half hot = saturate(0.30 + 1.30 * flow + streak * 0.45);
                    half far = saturate(1.22 - length(_WorldSpaceCameraPos - i.positionWS) / 230.0);
                    melt = _TWHeatColor.rgb * (hot * hot) * _TWLiquidHeat * far;
                }
                color = lerp(color, ApplyMist(color, i.positionWS), 0.38);   // the sheet lies below the mist's top everywhere: full mist would paint the whole river white (and .55 still read as a pale sheet)
                color = ApplyFieldFog(color, i.positionWS);
                color = MixFog(color, i.fog);
                color += melt;
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
