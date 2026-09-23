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
        _Pigment ("Painted surface layer (-1 = use Base Map)", Float) = -1
        _DetailScale ("Detail tiles per metre", Float) = 0.125
        _DetailStrength ("Detail strength", Range(0,1)) = 0
        _DetailBump ("Detail relief", Range(0,1)) = 0
        _Gloss ("Gloss (1 = standing water)", Range(0,1)) = 0
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        [HDR] _Emission ("Emission (lamp glass, embers)", Color) = (0, 0, 0, 0)
        _Sway ("Sway in the wind (reeds, grass, scrub)", Range(0,1)) = 0
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
        // The kit's painted surfaces as one array, set globally by BattlefieldKit (BattlefieldPigment.Sheet).
        // An array and not an atlas because these tile across a plank and an atlas tile bleeds when it wraps.
        TEXTURE2D_ARRAY(_PigmentSheet); SAMPLER(sampler_PigmentSheet);
        CBUFFER_START(UnityPerMaterial)
            half4 _BaseColor, _ShadeColor, _OutlineColor, _Emission;
            float4 _BaseMap_ST;
            float _DetailScale, _DetailStrength, _DetailBump, _OutlineWidth, _Gloss, _Sway, _Pigment;
        CBUFFER_END
        float4 _TWWind;   // xz: the wind (Atmosphere.WindNow, scaled), w: 1 when set
        /// Reeds, grass and scrub lean with the wind and shiver in the gusts: the bend grows with the square of the height
        /// above the root (object space), so the foot stays planted. Same in every pass.
        float3 TWSway(float3 positionWS, float heightOS)
        {
            if (_Sway <= 0.0) return positionWS;
            float phase = dot(positionWS.xz, float2(0.35, 0.27));
            float gust = 0.55 + sin(_Time.y * 1.7 + phase) * 0.6 + sin(_Time.y * 3.9 + phase * 2.3) * 0.25;
            positionWS.xz += _TWWind.xy * (gust * _Sway * heightOS * heightOS);
            return positionWS;
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
                o.positionWS = TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.uv = TRANSFORM_TEX(v.uv, _BaseMap);
                o.color = v.color;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                // A painted surface is one grey channel of the shared sheet; the ground and the imported sheets
                // keep their own base map, which also carries the water depth in its alpha, so they pass -1.
                half4 base;
                if (_Pigment >= 0.0)
                {
                    half paint = SAMPLE_TEXTURE2D_ARRAY(_PigmentSheet, sampler_PigmentSheet, i.uv, _Pigment).r;
                    base = half4(paint, paint, paint, 1.0);
                }
                else base = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, i.uv);
                half3 albedo = base.rgb * _BaseColor.rgb * i.color.rgb;
                half gloss = max(_Gloss, 1.0 - base.a);
                half shore = 0;
                if (base.a < 0.45)
                {
                    gloss = 1.0;
                    half lapNoise = frac(dot(floor(i.positionWS.xz * 0.5), float2(0.37, 0.61)));
                    albedo = TWWaterAlbedo((1.0 - base.a / 0.4) * 0.7, i.positionWS.xz, lapNoise, half3(0.36, 0.34, 0.27), half3(0.165, 0.18, 0.15), half3(0.095, 0.115, 0.11), half3(0.44, 0.45, 0.42), 0.45, shore) * i.color.rgb;
                }
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half2 slope = 0;
                half near = 1;
                if (_DetailBump < 0.5 && _Gloss < 0.5 && _TWWet.x > 0.0)
                {
                    // everything standing in the rain is wet: bags, timber, trunks, wrecks. Wettest on top.
                    half soaked = _TWWet.x * saturate(normalize(i.normalWS).y * 0.55 + 0.5);
                    gloss = max(gloss, soaked * 0.46);
                    albedo *= 1.0 - 0.28 * soaked;
                }
                if (_DetailStrength > 0.0)
                {
                    float2 uv1 = i.positionWS.xz * _DetailScale;
                    // the clods are cells of a tiling noise: bent by a slow sine field (about three cells a wave, a quarter
                    // of a cell deep) their edges wander, so up close the mud no longer reads as laid paving (critique round 1)
                    uv1 += 0.014 * float2(sin(uv1.y * 37.0 + sin(uv1.x * 23.0)), sin(uv1.x * 41.0 + 1.7 + sin(uv1.y * 19.0)));
                    float2 uv2 = float2(uv1.x * 0.259 - uv1.y * 0.117, uv1.x * 0.117 + uv1.y * 0.259) + 0.37;   // turned and 3.5x larger: the broad blotches
                    half3 d1 = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uv1).rgb;
                    float eyeDistance = distance(_WorldSpaceCameraPos, i.positionWS);
                    near = 1.0 - saturate((eyeDistance - 90.0) / 90.0);   // from the overview only the broad tone is left, so the tile never shows
                    half closeDetail = 0.0;
                    half tone = (d1.r - 0.5) * near + (SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uv2).a - 0.5);
                    slope = (d1.gb - 0.5) * near;
                    if (_TWClose > 0.0)
                    {
                        // grit: with the camera among the men the same map is read once more, five times finer and turned, so
                        // the mud under their boots breaks into pebbles and hairline cracks (and the wet glints into sparkle)
                        half grit = _TWClose * (1.0 - saturate((eyeDistance - 12.0) / 16.0));
                        float2 uv3 = float2(uv1.x * 4.55 + uv1.y * 2.10, uv1.y * 4.55 - uv1.x * 2.10) + 0.61;
                        half3 d3 = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uv3).rgb;
                        tone += (d3.r - 0.5) * 0.95 * grit;
                        slope += (d3.gb - 0.5) * 1.10 * grit;
                        closeDetail = grit;
                    }
                    gloss = max(gloss, _TWWet.x * _DetailBump * (0.30 + 0.25 * saturate(0.5 - d1.r * 1.0 + 0.3)));   // soaked ground: every surface with relief shines a little, the dark crevices most
                    half dry = 1.0 - saturate(gloss * 2.0 - 1.0);   // only standing water is smooth
                    albedo *= 1.0 - 0.55 * _TWWet.x * _DetailBump * dry;   // soaked earth is darker: black mud under the moon
                    albedo *= 1.0 + tone * 2.0 * (_DetailStrength * (1.0 + 1.75 * closeDetail)) * dry;   // 0.20 out at the standard view, about 0.55 among the men
                    half relief = dot(slope, mainLight.direction.xz) * _DetailBump * dry;
                    albedo *= 1.0 + smoothstep(0.035, 0.08, relief) * 0.10 - smoothstep(0.03, 0.08, -relief) * 0.12;   // soft clod edges, not grouted cells
                }
                // Snow lies before anything is lit, so it takes the light the surface under it would have taken and
                // the toon bands break over it the same way. Zero on every biome but winter (uniform branch).
                albedo = TWWorldPaint(albedo);   // basalt, or cold rock, or on the night field untouched
                albedo *= TWHeatCrust(i.positionWS, normalize(i.normalWS), saturate(_DetailStrength * 8.0));   // black between the plates
                half snow = TWSnowAmount(i.normalWS, i.positionWS);
                if (snow > 0.0)
                {
                    albedo = lerp(albedo, _TWSnowColor.rgb, snow);
                    gloss = max(gloss, _TWSnowColor.a * snow);
                }
                half wrap = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half lit = wrap;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;   // broad lit top planes, readable cool side planes
                // the shaded half is a hemisphere, not a flat tint: what falls from the sky above, what comes back
                // up off the ground below. On the night field _TWGroundLight is unset and this is the old constant.
                half3 shade = TWHemisphere(_ShadeColor.rgb * TWShadeTint(), normalize(i.normalWS));
                half3 color = albedo * lerp(shade, mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation); // contact shadows must survive the toon thresholds
                if (gloss > 0.01)
                {
                    float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                    // the fine wet sparkle is for the camera among the men: from the standard view (30 m and more) it is
                    // toned down to a third, so lanterns, not the mud, are the brightest things on screen
                    half close = 1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 14.0) / 20.0);
                    float3 n = normalize(lerp(normalize(i.normalWS), float3(0, 1, 0), _DetailBump > 0.5 || _Gloss > 0.5 ? 0.8 : 0.0));   // water lies flat whatever the ground does; a wet prop keeps its own planes
                    if (_DetailStrength > 0.0)
                    {
                        // liquid mud keeps the clods' broken surface; still water breathes with a slow ripple
                        float2 uvr = i.positionWS.xz * 0.19 + float2(_Time.y * 0.021, _Time.y * 0.013);
                        half2 ripple = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, uvr).gb - 0.5;
                        half still = saturate(gloss * 2.0 - 1.0);
                        n = normalize(n + float3(lerp(slope * (0.35 + 0.45 * _TWWet.x) * lerp(0.5, 1.0, close), ripple * 0.22, still), 0).xzy);   // soaked: every clod throws its own highlight
                    }
                    float3 r = reflect(-view, n);
                    half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                    half3 sky = TWSky() * lerp(1.08, 0.62, saturate(r.y * 1.4));   // bright at the horizon, darker overhead
                    color = lerp(color, sky, min(gloss * (0.22 + 0.70 * fresnel), lerp(1.0, 0.35, saturate(gloss * 2.0 - 1.0))) * (1.0 - shore * 0.7));   // a puddle mirrors at most a third of the sky: dark water, not paper
                    half glint = smoothstep(0.990, 0.994, dot(r, mainLight.direction));
                    color += glint * gloss * mainLight.color * 0.55 * mainLight.shadowAttenuation;
                    // wet sheen: a broad soft highlight toward the light, on top of the hard glint (the moon on soaked mud)
                    half toLight = saturate(dot(r, mainLight.direction));
                    half mudOnly = 1.0 - saturate(gloss * 2.0 - 1.0) * 0.72;   // the sheen is the mud's; still water only mirrors
                    color += (pow(toLight, 14.0) * 0.20 + smoothstep(0.93, 0.96, toLight) * lerp(0.26, 0.40, close)) * gloss * mudOnly * _TWWet.y * mainLight.color * mainLight.shadowAttenuation;
                    if (_DetailStrength > 0.0 && _TWWet.y > 0.0)
                    {
                        // hard wet glints: a second, much finer read of the slopes tilts tiny facets into the moon, so the
                        // highlight breaks into sharp specks instead of one soft patch. Close range only (the mips flatten it).
                        half2 fine = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, i.positionWS.xz * _DetailScale * 4.3 + 0.17).gb - 0.5;
                        float3 facet = normalize(n + float3(fine.x, 0, fine.y) * 1.25);
                        half spark = smoothstep(0.972, 0.984, dot(reflect(-view, facet), mainLight.direction));
                        color += spark * 1.7 * near * lerp(0.3, 1.0, close) * gloss * mudOnly * _TWWet.y * mainLight.color * mainLight.shadowAttenuation;
                    }
                }
                if (_TWWet.z > 0.0)
                {
                    // raindrops burst on everything that faces the sky: mud, duckboards, sandbags, wrecks. Close range only.
                    half open = saturate(normalize(i.normalWS).y * 2.0 - 0.7) * (1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 8.0) / 14.0));
                    // the burst takes the colour of the ground it lands on and of whatever lights that ground, so it is
                    // a wet glint inside a lamp pool and almost nothing out in the dark, instead of a white ring lying
                    // on black mud at the same strength everywhere
                    if (open > 0.0) color += TWRainSplash(i.positionWS.xz) * open * (color * 2.4 + half3(0.05, 0.055, 0.06)) * (TWSky() * 0.9 + mainLight.color * 0.25);
                }
                half3 lampGlint;
                color += max(albedo, 0.16) * TWLocalLights(i.positionWS, normalize(i.normalWS + float3(slope.x, 0, slope.y) * _DetailBump), i.positionCS, normalize(_WorldSpaceCameraPos - i.positionWS), gloss, lampGlint);
                color += lampGlint;
                color += _Emission.rgb;
                // molten ground burns up out of its own cracks. Dimmed by whatever is lying on top of it, because
                // snow and lava never share a field but a mask that ignores the other one is a bug waiting to happen.
                // _DetailStrength is set only by the terrain material, so it is what tells molten ground from a
                // sandbag standing on it. Passing it as the exposure means props are LIT by the lava and never cracked.
                color += TWHeatGlow(i.positionWS, normalize(i.normalWS), (1.0 - snow) * saturate(_DetailStrength * 8.0));
                color += TWGroundBounce(normalize(i.normalWS), albedo);
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
                o.positionWS = TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y);
                float3 n = dot(v.smoothOS, v.smoothOS) > 0.01 ? v.smoothOS : v.normalOS;
                float4 cs = TransformWorldToHClip(o.positionWS);
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
                float3 ws = TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y);
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
            float4 vertDepth(Attributes v) : SV_POSITION { UNITY_SETUP_INSTANCE_ID(v); return TransformWorldToHClip(TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y)); }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
