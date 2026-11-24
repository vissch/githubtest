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
// _CHUNKMASK (the village houses only, HouseKit): a house is one mesh, each vertex tagged with its chunk in TEXCOORD1.x,
// and a set bit in the per-instance _ChunkMask hides that chunk in every pass. Other materials never compile it in.
// _KITJITTER (the trench lining only, BattlefieldKit.Handmade): every part of a panel (a board, a post, a sack) carries
// its pivot and a seed in TEXCOORD2, and each instance turns, shifts, sinks and tones its parts by a hash of its own
// matrix, so one mesh is a different panel at every place. Same in every pass; Resources/Env/KitJitter.mat keeps it in a build.
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
        // How much of the ground's own relief survives under lying snow. 0 is what this did before: one flat
        // colour that threw away everything the close-up grit branch had just computed.
        _SnowRelief ("Relief kept under snow", Range(0,2)) = 1.6
        // A blast site holds no snow until it has filled in. The threshold is a VALUE no natural ground reaches:
        // GreyboxTerrainView burns a crater toward (.10, .09, .08) and the darkest honest mud texel is near 0.30.
        _SnowBareLuma ("Burn darker than this holds no snow", Range(0,0.5)) = 0.06
        // SWEPT, and 0.17 was badly wrong. The guess assumed the darkest honest ground texel was near 0.30,
        // but albedo here has already been multiplied by crustRelief, which drives clod shadows well below
        // 0.10. Measured share of the near ground stripped of snow: 58.5% at both 0.17 and 0.10 - more than
        // half the field - against 22.4% with the feature off entirely and 25.8% at 0.06. The threshold has
        // to sit BELOW the natural minimum, not below a guess at it.
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        [HDR] _Emission ("Emission (lamp glass, embers)", Color) = (0, 0, 0, 0)
        _Sway ("Sway in the wind (reeds, grass, scrub)", Range(0,1)) = 0
        _OutlineColor ("Outline", Color) = (0.13, 0.10, 0.08, 1)
        _OutlineWidth ("Outline width (pixels up close)", Float) = 2.6
        _KitJitter ("Lining parts: turn (deg), shift (m), tone, sink (m)", Vector) = (0, 0, 0, 0)
        _KitJitterAxes ("Lining turn per axis (x, y, z), w: share of loose parts left out", Vector) = (1, 1, 1, 0)
        // Hessian soaks dark in the rain but does not shine: 0 (every material so far) keeps the wet gloss as it was
        _Matte ("Wet without the shine (cloth)", Range(0,1)) = 0
        _KitGrime ("Lining grime from its joints: 0 none, 1 on", Float) = 0
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
            float _DetailScale, _DetailStrength, _DetailBump, _OutlineWidth, _Gloss, _Sway, _Pigment, _SnowRelief, _SnowBareLuma;
            float4 _KitJitter, _KitJitterAxes;   // in every variant: the SRP Batcher wants one layout
            float _Matte, _KitGrime;
        CBUFFER_END
        float4 _TWWind;   // xz: the wind (Atmosphere.WindNow, scaled), w: 1 when set
        #if defined(_CHUNKMASK)
        UNITY_INSTANCING_BUFFER_START(TWChunks)
            UNITY_DEFINE_INSTANCED_PROP(float4, _ChunkMask)
        UNITY_INSTANCING_BUFFER_END(TWChunks)
        #endif
        /// A hidden chunk's vertices all go to the object's origin, so its triangles have no area and draw nothing.
        /// The mask is four words of 24 bits rather than one, because a float holds a whole number exactly only that
        /// far: chunk i is bit (i % 24) of word (i / 24). The word is picked by compare rather than by indexing the
        /// vector, which on some targets spills it to memory for a dynamic index.
        bool TWChunkHidden(float chunk)
        {
        #if defined(_CHUNKMASK)
            float4 packed = UNITY_ACCESS_INSTANCED_PROP(TWChunks, _ChunkMask);
            uint i = (uint)(chunk + 0.5);
            uint word = i / 24u;
            uint mask = (uint)(word == 0u ? packed.x : word == 1u ? packed.y : word == 2u ? packed.z : packed.w);
            if ((mask >> (i % 24u)) & 1u) return true;
        #endif
            return false;
        }
        float3 TWChunk(float3 positionOS, float chunk)
        {
            return TWChunkHidden(chunk) ? float3(0, 0, 0) : positionOS;
        }
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

        /// The trench lining's grime (_KitGrime 1, BattlefieldKit.Handmade: knob props.grime; 0, every other material, does
        /// nothing; the owner's pick of six, 2026-11-24, "lived in"). Laid out in world space, so a mark runs on across the
        /// next board and the next sack, it reads the part's height in its panel (kit.x), its seed (kit.y) and how far the
        /// point lies above the nearest joint (kit.z, BattlefieldKit.Joints: 0 where its part meets another part or the
        /// ground, 0.6 where nothing touches). The water slows in a joint and drops its silt there, thick on the shelves
        /// that face up and thin on the upright faces, and carries some down the faces below in streaks. Mud dries from its
        /// edge, so the rim is thicker than the middle, and it cracks in plates that grow with how thick it lies. The wet is
        /// held dark round it and climbs a little further in tongues; moss keeps to the deepest crevices. Men lean and climb
        /// on the top of a heap and of a wall and rub it clean and dark; timber rots along its grain out of the joints;
        /// hessian wears through at a seam to the earth inside; lichen spots the bare dry timber. Close up the cloth shows
        /// its weave and the board its grain, both faded out before they would shimmer.
        float TWGrimeHash(float3 p) { p = frac(p * 0.3183099 + 0.1); p *= 17.0; return frac(p.x * p.y * p.z * (p.x + p.y + p.z)); }
        float TWGrimeNoise(float3 x)
        {
            float3 i = floor(x), f = frac(x); f = f * f * (3.0 - 2.0 * f);
            return lerp(lerp(lerp(TWGrimeHash(i), TWGrimeHash(i + float3(1, 0, 0)), f.x), lerp(TWGrimeHash(i + float3(0, 1, 0)), TWGrimeHash(i + float3(1, 1, 0)), f.x), f.y),
                        lerp(lerp(TWGrimeHash(i + float3(0, 0, 1)), TWGrimeHash(i + float3(1, 0, 1)), f.x), lerp(TWGrimeHash(i + float3(0, 1, 1)), TWGrimeHash(i + float3(1, 1, 1)), f.x), f.y), f.z);
        }
        /// Mud plates: the distance to the nearest and the next nearest of one jittered point per cell (d2 - d1 is small
        /// on a crack). 18 hashes.
        float2 TWCells(float2 p)
        {
            float2 i = floor(p), f = frac(p); float d1 = 8.0, d2 = 8.0;
            for (int y = -1; y <= 1; y++)
            for (int x = -1; x <= 1; x++)
            {
                float2 g = float2(x, y), c = i + g;
                float2 o = float2(TWGrimeHash(float3(c, 1.7)), TWGrimeHash(float3(c, 9.1)));
                float d = length(g + o - f);
                if (d < d1) { d2 = d1; d1 = d; } else if (d < d2) d2 = d;
            }
            return float2(d1, d2);
        }
        half3 TWKitGrime(half3 albedoIn, float3 ws, float3 nWS, float2 uv, float3 kit)
        {
            bool sack = _Pigment > 6.5 && _Pigment < 7.5;   // BattlefieldPigment.Surface.Sacking; the rest is timber
            float3 albedo = albedoIn;
            float h = kit.x, seed = kit.y, wick = kit.z;
            float big = TWGrimeNoise(ws * 0.85 + 11.0), mid = TWGrimeNoise(ws * 3.1), fine = TWGrimeNoise(ws * 9.7 + 5.3);
            float upward = saturate(nWS.y), away = saturate(-nWS.y), upright = 1.0 - abs(nWS.y);
            if (sack)
            {
                float2 w = uv * float2(70.0, 110.0);
                float fade = saturate(1.6 - max(fwidth(w.x), fwidth(w.y)) * 1.6);
                albedo *= 1.0 + (abs(sin(w.x * 3.14159)) + abs(sin(w.y * 3.14159)) - 1.27) * 0.07 * fade;   // warp and weft
            }
            else
            {
                float g = TWGrimeNoise(float3(uv.x * 4.0, uv.y * 20.0, seed * 29.0));
                float rings = abs(frac(uv.y * 7.0 + g * 1.6) - 0.5) * 2.0;
                float fade = saturate(1.6 - fwidth(uv.y * 7.0) * 3.0);
                albedo *= lerp(1.0, lerp(0.86, 1.04, smoothstep(0.08, 0.4, rings)), fade);
            }
            // the damp wicks along what it can: the threads of the cloth, the grain of the board
            float along = sack ? TWGrimeNoise(float3(uv.x * 38.0, uv.y * 5.0, seed * 11.0)) : TWGrimeNoise(float3(uv.x * 2.5, uv.y * 26.0, seed * 11.0));
            const float reach = 0.26;
            float near = 1.0 - saturate(wick / reach);
            // how thick the silt lies: in the joint, on what faces up, more where the ground threw more
            float thick = near * lerp(0.6, 1.25, upward) * (0.6 + 0.8 * mid);
            float edge = thick + 0.16 * (fine - 0.5);
            float silt = smoothstep(0.30, 0.40, edge);
            float rim = smoothstep(0.32, 0.38, edge) * (1.0 - smoothstep(0.38, 0.5, edge));
            // dry pale where thin, darker and still wet in the thick of it, the rim darker than the middle
            float dry = saturate(1.6 - thick * 1.1);
            float3 clay = lerp(float3(0.34, 0.29, 0.22), float3(0.66, 0.60, 0.48), dry * (0.85 + 0.3 * fine));
            clay *= 1.0 - 0.18 * rim;
            // plates on the plane the face looks along, cracks wider where it lies thick
            float3 an = abs(nWS);
            float2 plane = an.y > max(an.x, an.z) ? ws.xz : (an.x > an.z ? ws.zy : ws.xy);
            // The plates are only ever read through silt (the lerp below weighs the clay, cracks and all, by silt * 0.9), so
            // where there is no silt the 3x3 Worley is not paid for: most of a panel is clear of any joint's silt.
            if (silt > 0.0)
            {
                float2 cells = TWCells(plane * 24.0 + seed * 3.0);
                float crack = (1.0 - smoothstep(0.0, 0.05 + 0.10 * saturate(thick), cells.y - cells.x)) * dry;
                clay *= 1.0 - 0.5 * crack;
            }
            albedo = lerp(albedo, clay, silt * 0.9);
            // the wet held round it, dark, climbing a little further in tongues
            float finger = TWGrimeNoise(float3(ws.x * 11.0, ws.y * 1.8, ws.z * 11.0) + seed * 7.0);
            float wet = (1.0 - smoothstep(0.0, reach * (0.8 + 0.5 * finger), wick)) * (1.0 - silt);
            albedo *= 1.0 - 0.34 * wet;
            // silt carried down the upright faces below a shelf, in streaks along the fall
            float fall = TWGrimeNoise(float3(ws.x * 17.0, ws.y * 1.1, ws.z * 17.0) + 3.0);
            float streak = smoothstep(0.6, 0.82, fall) * upright * (1.0 - smoothstep(reach, reach * 2.6, wick)) * (1.0 - silt);
            albedo = lerp(albedo, albedo * float3(0.82, 0.74, 0.62) + float3(0.08, 0.07, 0.05), streak * 0.65);
            // moss only in the deepest crevices, out of the sky
            float moss = smoothstep(0.6, 0.8, mid * 0.6 + away * 0.4) * (1.0 - smoothstep(0.0, 0.08, wick)) * (1.0 - silt * 0.6);
            albedo = lerp(albedo, float3(0.16, 0.21, 0.08), moss * 0.6);
            // rubbed clean and dark where men lean and climb: the top of a heap, the top of a wall
            float top = sack ? upward * smoothstep(0.32, 0.5, wick) : smoothstep(1.65, 1.95, h) * (0.4 + 0.6 * upward);
            float rub = top * smoothstep(0.35, 0.6, big + 0.3 * (fine - 0.5));
            albedo = lerp(albedo, albedoIn * 0.74, rub * 0.8);
            float bare = smoothstep(reach * 1.2, reach * 2.4, wick);
            if (sack)
            {
                // worn through at a seam: the weave thins, then gives, and the earth inside shows
                float thin = near * smoothstep(0.62, 0.8, TWGrimeNoise(ws * 6.3 + seed * 17.0));
                float hole = smoothstep(0.55, 0.65, thin);
                albedo = lerp(albedo, albedo * 0.72 + 0.05, smoothstep(0.3, 0.55, thin) * (1.0 - hole));
                albedo = lerp(albedo, float3(0.20, 0.16, 0.12), hole);
                albedo *= lerp(1.0, 1.1, bare * upward);
            }
            else
            {
                // rot follows the grain out of the joint: dark, soft, in streaks along the board
                float rot = near * smoothstep(0.45, 0.7, along + 0.25 * mid);
                albedo = lerp(albedo, albedo * float3(0.42, 0.38, 0.33), rot * 0.8);
                float lichen = smoothstep(0.8, 0.86, TWGrimeNoise(ws * 21.0 + seed * 5.0)) * bare * (0.5 + 0.5 * upward);
                albedo = lerp(albedo, float3(0.62, 0.66, 0.52), lichen * 0.5);
            }
            return (half3)albedo;
        }
        #if defined(_KITJITTER)
        float TWKitHash(float3 p)
        {
            p = frac(p * float3(0.1031, 0.1030, 0.0973));
            p += dot(p, p.yzx + 33.33);
            return frac((p.x + p.y) * p.z);
        }
        /// The panel's own dice. Its matrix's turn and scale are set per trench edge by the composer and a bullet's knock
        /// only pushes a panel (PropWear), so they never change under fire; its place to the nearest 2 m (x and z only)
        /// tells apart two panels turned alike.
        float3 TWKitPanel()
        {
            float2 at = floor(float2(UNITY_MATRIX_M._m03, UNITY_MATRIX_M._m23) * 0.5 + 0.5);
            float3 a = float3(UNITY_MATRIX_M._m00, UNITY_MATRIX_M._m20, UNITY_MATRIX_M._m11);
            float3 b = float3(UNITY_MATRIX_M._m02, UNITY_MATRIX_M._m22, UNITY_MATRIX_M._m21);
            return float3(at.x * 0.173, at.y * 0.311, 0) + a * 41.7 + b * 23.9;
        }
        float3x3 TWKitTurn(float3 a)
        {
            float3 s, c; sincos(a, s, c);
            float3x3 rx = float3x3(1, 0, 0, 0, c.x, -s.x, 0, s.x, c.x);
            float3x3 ry = float3x3(c.y, 0, s.y, 0, 1, 0, -s.y, 0, c.y);
            float3x3 rz = float3x3(c.z, -s.z, 0, s.z, c.z, 0, 0, 0, 1);
            return mul(rz, mul(ry, rx));
        }
        /// One part of a lining panel turned about its own pivot, shifted, sunk, and its tone; a part whose seed is
        /// positive (a loose board, never a post, rail or sack) is now and then left out, collapsed onto its pivot.
        /// Returns true when it is left out, so the outline does not ink the collapsed point.
        bool TWKitPart(inout float3 positionOS, inout float3 normalOS, inout float3 smoothOS, float4 part, out half3 tone)
        {
            float3 panel = TWKitPanel();
            float seed = abs(part.w);
            float h1 = TWKitHash(panel + seed * 17.13), h2 = TWKitHash(panel.zxy + seed * 9.71 + 3.1);
            float h3 = TWKitHash(panel.yzx + seed * 5.37 + 7.7), h4 = TWKitHash(panel + seed * 2.91 + 11.3);
            float h5 = TWKitHash(panel * 1.7 + seed * 13.9 + 1.9), h6 = TWKitHash(panel * 2.3 + seed * 7.1 + 4.4);
            float hp = TWKitHash(panel + 5.5);
            float3x3 r = TWKitTurn((float3(h1, h2, h3) * 2.0 - 1.0) * _KitJitterAxes.xyz * radians(_KitJitter.x));
            float3 shift = float3((h4 * 2.0 - 1.0) * _KitJitter.y, -h5 * _KitJitter.w, (h2 * 2.0 - 1.0) * _KitJitter.y * 0.5);
            positionOS = part.xyz + mul(r, positionOS - part.xyz) + shift;
            normalOS = mul(r, normalOS); smoothOS = mul(r, smoothOS);
            // value first (a part and the whole panel), then a little warm or cold: fresh timber, grey timber, wet sacking
            tone = (half)((1.0 + (h3 - 0.5) * 2.0 * _KitJitter.z) * (1.0 + (hp - 0.5) * _KitJitter.z)) * lerp(half3(0.96, 1.0, 1.05), half3(1.05, 1.0, 0.93), (half)h4);
            bool gone = part.w > 0.0 && h6 < _KitJitterAxes.w;
            if (gone) positionOS = part.xyz;
            return gone;
        }
        #endif
        ENDHLSL

        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma shader_feature_local_vertex _ _CHUNKMASK
            #pragma shader_feature_local_vertex _ _KITJITTER
            #pragma multi_compile_fog
            // [Hs7] the screen-space shadow variant is left out on purpose: this pass builds its shadow coord with
            // TransformWorldToShadowCoord (light-space), and the renderer has no Screen Space Shadows feature.
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _FORWARD_PLUS
            #define TW_PIGMENT_RANGE 1.25   // BattlefieldPigment.Range: the R8 sheet holds value/Range
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            #include "Assets/_Project/Shaders/TWWater.hlsl"
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"
            #include "Assets/_Project/Shaders/TWLightPools.hlsl"

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 uv : TEXCOORD0; float2 chunk : TEXCOORD1; float4 part : TEXCOORD2; half4 color : COLOR; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2; half4 color : COLOR; float fog : TEXCOORD3; float3 kit : TEXCOORD4; };

            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                v.positionOS.xyz = TWChunk(v.positionOS.xyz, v.chunk.x);
                half3 kitTone = 1.0;
                float3 kit = float3(v.positionOS.y, abs(v.part.w), v.chunk.y);   // the grime's height in the panel, the part's seed, its joints
            #if defined(_KITJITTER)
                float3 unusedSmooth = 0;
                TWKitPart(v.positionOS.xyz, v.normalOS, unusedSmooth, v.part, kitTone);
            #endif
                Varyings o;
                o.positionWS = TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.uv = TRANSFORM_TEX(v.uv, _BaseMap);
                o.color = v.color;
                o.color.rgb *= kitTone;   // before the albedo step, so the biome's tint, snow and the night still win
                o.kit = kit;
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
                    // The sheet stores value/Range (R8 clamps at 1, the paint peaks above it), so scale it back.
                    half paint = SAMPLE_TEXTURE2D_ARRAY(_PigmentSheet, sampler_PigmentSheet, i.uv, _Pigment).r * TW_PIGMENT_RANGE;
                    base = half4(paint, paint, paint, 1.0);
                }
                else base = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, i.uv);
                half3 albedo = base.rgb * _BaseColor.rgb * i.color.rgb;
                float3 nWS = normalize(i.normalWS);   // once: every use below wants the same unit normal
                if (_KitGrime > 0.5) albedo = TWKitGrime(albedo, i.positionWS, nWS, i.uv, i.kit);   // a per-material branch: the lining only
                half gloss = max(_Gloss, 1.0 - base.a);
                half shore = 0;
                if (base.a < 0.45)
                {
                    gloss = 1.0;
                    half lapNoise = frac(dot(floor(i.positionWS.xz * 0.5), float2(0.37, 0.61)));
                    half puddleThrown, puddleTrough;   // a painted puddle keeps the pale ring it always had (r7's value step is the river sheet's)
                    albedo = TWWaterAlbedo((1.0 - base.a / 0.4) * 0.7, i.positionWS.xz, lapNoise, half3(0.36, 0.34, 0.27), half3(0.165, 0.18, 0.15), half3(0.095, 0.115, 0.11), half3(0.44, 0.45, 0.42), 0.45, shore, puddleThrown, puddleTrough) * i.color.rgb;
                }
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half2 slope = 0;
                half near = 1;
                // How much the crust's own grain brightens or darkens this pixel. It is hoisted out of the
                // detail block because the MOLTEN GLOW needs it too: the glow is added after lighting and the
                // grain multiplies albedo, so without this the hottest ground is mathematically the flattest.
                // Zero on anything with no detail map - props, sandbags, men - so only the terrain changes.
                half crustRelief = 0;
                if (_DetailBump < 0.5 && _Gloss < 0.5 && _TWWet.x > 0.0)
                {
                    // everything standing in the rain is wet: bags, timber, trunks, wrecks. Wettest on top.
                    half soaked = _TWWet.x * saturate(nWS.y * 0.55 + 0.5);
                    gloss = max(gloss, soaked * 0.46 * (1.0 - _Matte));
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
                    crustRelief = tone * 2.0 * (_DetailStrength * (1.0 + 1.75 * closeDetail)) * dry;   // 0.20 out at the standard view, about 0.55 among the men
                    albedo *= 1.0 + crustRelief;
                    half relief = dot(slope, mainLight.direction.xz) * _DetailBump * dry;
                    albedo *= 1.0 + smoothstep(0.035, 0.08, relief) * 0.10 - smoothstep(0.03, 0.08, -relief) * 0.12;   // soft clod edges, not grouted cells
                }
                // Snow lies before anything is lit, so it takes the light the surface under it would have taken and
                // the toon bands break over it the same way. Zero on every biome but winter (uniform branch).
                albedo = TWWorldPaint(albedo);   // basalt, or cold rock, or on the night field untouched
                // The crust (black between the plates) and the molten glow share one plate edge and one molten field: asked
                // for together, they are computed once. The glow's exposure waits on the snow (below), which waits on this
                // albedo, and the glow is linear in its exposure, so it is taken here at the terrain's own exposure and
                // scaled by (1 - snow) where the snow is known.
                half heatExposure = saturate(_DetailStrength * 8.0);
                half heatCrust; half3 heatGlow;
                TWHeatCrustAndGlow(i.positionWS, nWS, heatExposure, heatExposure, heatCrust, heatGlow);
                albedo *= heatCrust;   // black between the plates
                half snow = TWSnowAmount(i.normalWS, i.positionWS);
                // A CRATER IS BARE EARTH WHEN IT LANDS. The blast throws the snow off and burns what is under it,
                // and the ground texture already carries that as a burn far darker than any natural texel - so the
                // snow reads the albedo it has already computed rather than needing a second map or a per-pixel
                // loop over crater positions. The fill comes free: GreyboxTerrainView fades the burn and repaints
                // it, the ground lightens, and the snow returns with it. One number, both halves.
                if (snow > 0.0 && _SnowBareLuma > 0.0)
                    snow *= saturate(dot(albedo, half3(0.299, 0.587, 0.114)) / _SnowBareLuma);
                if (snow > 0.0)
                {
                    // SNOW KEEPS THE GROUND IT LIES ON. A flat lerp here replaced everything the close-up grit
                    // branch computed - crustRelief is about 0.55 among the men - with one constant, which is
                    // why the winter ground read as a featureless plane at zoom 8 while the same code made mud
                    // break into pebbles and cracks. Snow lying on a rutted, cratered field is not flat; a
                    // covering softens what is under it, so the relief is scaled rather than dropped. Same
                    // argument the comment above makes for light, applied to form.
                    half3 lying = _TWSnowColor.rgb * max(0.0, 1.0 + crustRelief * _SnowRelief);
                    albedo = lerp(albedo, lying, snow);
                    gloss = max(gloss, _TWSnowColor.a * snow);
                }
                half wrap = dot(nWS, mainLight.direction) * 0.5 + 0.5;
                half lit = wrap;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;   // broad lit top planes, readable cool side planes
                // the shaded half is a hemisphere, not a flat tint: what falls from the sky above, what comes back
                // up off the ground below. On the night field _TWGroundLight is unset and this is the old constant.
                half3 shade = TWHemisphere(_ShadeColor.rgb * TWShadeTint(), nWS);
                half3 color = albedo * lerp(shade, mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation); // contact shadows must survive the toon thresholds
                float3 glintR = float3(0, 1, 0); half glintW = 0;   // look.wet: the flames' glints on wet ground (TWLightPools.hlsl), orange near a fire
                if (gloss > 0.01)
                {
                    float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                    // the fine wet sparkle is for the camera among the men: from the standard view (30 m and more) it is
                    // toned down to a third, so lanterns, not the mud, are the brightest things on screen
                    half close = 1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 14.0) / 20.0);
                    float3 n = normalize(lerp(nWS, float3(0, 1, 0), _DetailBump > 0.5 || _Gloss > 0.5 ? 0.8 : 0.0));   // water lies flat whatever the ground does; a wet prop keeps its own planes
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
                    color = lerp(color, sky, min(gloss * (0.22 + 0.70 * fresnel), lerp(1.0, 0.35, saturate(gloss * 2.0 - 1.0))) * (1.0 - shore * 0.7) * (1.0 - _TWWetLook * lerp(_TWMudSky > 0.0 ? _TWMudSky : 0.55, 0.55, saturate(gloss * 2.0 - 1.0))));   // look.wet: less of the pale sky in every puddle; look.mudSky: the MUD loses more of it still (its blue was the sky's: B:R 1.77), while still water keeps today's 0.55 (its slab was tuned in rounds 7-10)   // a puddle mirrors at most a third of the sky: dark water, not paper
                    half glint = smoothstep(0.990, 0.994, dot(r, mainLight.direction));
                    color += glint * gloss * mainLight.color * 0.55 * mainLight.shadowAttenuation;
                    // wet sheen: a broad soft highlight toward the light, on top of the hard glint (the moon on soaked mud)
                    half toLight = saturate(dot(r, mainLight.direction));
                    half mudOnly = 1.0 - saturate(gloss * 2.0 - 1.0) * 0.72;   // the sheen is the mud's; still water only mirrors
                    // look.mudSheen: the mud's own share of the moon. lerp(_TWMoonSheen, 1.0, close) below goes to 1 among the
                    // men, so at zoom 12 that knob cannot touch the near mud - this one can, and it is what the near mud's p95 is.
                    half mudSheen = lerp(1.0, _TWMudSheen > 0.0 ? _TWMudSheen : 1.0, _TWWetLook);
                    color += (pow(toLight, 14.0) * 0.20 + smoothstep(0.93, 0.96, toLight) * lerp(0.26, 0.40, close)) * gloss * mudOnly * mudSheen * _TWWet.y * mainLight.color * mainLight.shadowAttenuation;
                    // look.wet: the flames' own glints on the wet ground (TWLightPools.hlsl), orange near a fire
                    glintR = r; glintW = gloss * mudOnly * _TWWetLook * 0.6;   // applied with the pools below, in one pass (TWPoolsAndGlints)
                    if (_DetailStrength > 0.0 && _TWWet.y > 0.0)
                    {
                        // hard wet glints: a second, much finer read of the slopes tilts tiny facets into the moon, so the
                        // highlight breaks into sharp specks instead of one soft patch. Close range only (the mips flatten it).
                        half2 fine = SAMPLE_TEXTURE2D(_DetailMap, sampler_DetailMap, i.positionWS.xz * _DetailScale * 4.3 + 0.17).gb - 0.5;
                        float3 facet = normalize(n + float3(fine.x, 0, fine.y) * 1.25);
                        half spark = smoothstep(0.972, 0.984, dot(reflect(-view, facet), mainLight.direction));
                        color += spark * 1.7 * near * lerp(_TWMoonSheen > 0.0 ? _TWMoonSheen : 0.3, 1.0, close) * (1.0 - _TWMenSheenCut * close) * gloss * mudOnly * mudSheen * _TWWet.y * mainLight.color * mainLight.shadowAttenuation;
                    }
                }
                if (_TWWet.z > 0.0)
                {
                    // raindrops burst on everything that faces the sky: mud, duckboards, sandbags, wrecks. Close range only.
                    half open = saturate(nWS.y * 2.0 - 0.7) * (1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 8.0) / 14.0));
                    // the burst takes the colour of the ground it lands on and of whatever lights that ground, so it is
                    // a wet glint inside a lamp pool and almost nothing out in the dark, instead of a white ring lying
                    // on black mud at the same strength everywhere
                    if (open > 0.0) color += TWRainSplash(i.positionWS.xz) * open * (color * 2.4 + half3(0.05, 0.055, 0.06)) * (TWSky() * 0.9 + mainLight.color * 0.25);
                }
                half3 lampGlint;
                color += max(albedo, 0.16) * TWLocalLights(i.positionWS, normalize(i.normalWS + float3(slope.x, 0, slope.y) * _DetailBump), i.positionCS, normalize(_WorldSpaceCameraPos - i.positionWS), gloss, lampGlint);
                color += lampGlint;
                // look.pools: the nearest flames' painted pools, past the eight real lights an object may take (TWLightPools.hlsl)
                // look.poolsThroughHaze: that share of it goes on after the fog below, like the molten glow, so the haze
                // that lifts the distance does not also put out the lamps in it (the owner's edit keeps far fires bright)
                half3 poolLight = 0;
                if (_TWPoolCount > 0.0)
                {
                    half3 glints, propRim;
                    // look.propRim: a stump, a post, a sandbag beside a fire catches its edge warm, as the men do (critique round 13:
                    // "foreground stumps solid black cutouts"); props only, the ground has _DetailStrength set. Taken in the pools'
                    // own walk (TWPoolsGlintsAndRim), not a second one. A narrower band than the men's (0.52, not 0.30): at 0.30 a
                    // big flat face seen at a slant was all "edge" and a concrete slab by a fire lit whole (critique round 15)
                    bool rimOn = _TWPropRim > 0.0 && _DetailStrength <= 0.0;
                    poolLight = max(albedo, 0.16) * TWPoolsGlintsAndRim(i.positionWS, nWS, glintR, glintW > 0.0, rimOn, rimOn ? normalize(_WorldSpaceCameraPos - i.positionWS) : half3(0, 0, 0), 0.52, glints, propRim) * (_TWLampScale > 0.0 ? _TWLampScale : 1.0);
                    color += glints * glintW;
                    // look.glintUnblue: a flame's orange glint laid over the moonlit blue mud read pink-lilac (critique
                    // round 23; violet pixels halved with the glints off), so a glint takes that much blue out from under it,
                    // and some green: blue alone turned the white-lilac sheen lemon yellow, not the fire's orange
                    if (_TWGlintUnblue > 0.0)
                    {
                        half take = dot(glints * glintW, half3(0.6, 0.4, 0.0)) * _TWGlintUnblue;
                        color.gb -= min(color.gb * half2(0.3, 0.7), take * half2(0.45, 1.0));
                    }
                    if (rimOn) poolLight += propRim * _TWPropRim;
                }
                // look.poolUnblue: amber at the pool's edge, not violet. Only the faint edge: where the pool is strong it would strip the
                // lit sandbags of all their blue and leave acid yellow, so the take fades out as the pool's light grows
                half poolWarm = poolLight.r * 0.6 + poolLight.g * 0.4;
                half unblue = poolWarm * _TWPoolUnblue * (1.0 - smoothstep(0.08, 0.30, poolWarm));
                if (_TWPoolUnblue > 0.0) color.b -= min(color.b * 0.6, unblue);
                color += poolLight * (1.0 - _TWPoolsThroughHaze);
                color += _Emission.rgb;
                // molten ground burns up out of its own cracks. Dimmed by whatever is lying on top of it, because
                // snow and lava never share a field but a mask that ignores the other one is a bug waiting to happen.
                // _DetailStrength is set only by the terrain material, so it is what tells molten ground from a
                // sandbag standing on it. Passing it as the exposure means props are LIT by the lava and never cracked.
                color += TWGroundBounce(nWS, albedo);
                // Molten rock is emissive, so it is seen THROUGH the haze rather than repainted by it. Added after
                // the fog for that reason: while it was added before, ApplyMist and MixFog lerped the glow itself
                // toward the fog colour and the distant pools came out pale pink instead of orange behind pink.
                half3 heat = (1.0 - snow) * heatExposure > 0.002 ? heatGlow * (1.0 - snow) : half3(0, 0, 0);
                // The glow takes the crust's relief, for the reason crustRelief is declared where it is. The
                // grain multiplies ALBEDO and this is ADDED, so the final pixel is albedo*light*(1+relief) + heat:
                // as heat grows the relief becomes a vanishing fraction of it, and the nearest, brightest, most
                // looked-at ground on the field comes out an airbrush. MEASURED on the lava capture, relative
                // adjacent-pixel texture: 0.0222 in the hot foreground against 0.0347 in the mid-field trough -
                // 36% LESS grain in the band where the detail map runs at full strength. No amount of tuning
                // _DetailStrength could reach it; it was on the wrong side of a plus sign.
                // Same factor as the albedo takes, so there is no second number to keep in step, and clamped so
                // that relief can never drive an emission negative.
                heat *= max(0.0, 1.0 + crustRelief);
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                color = TWSilhouetteFog(color, MixFog(color, i.fog), _DetailStrength);
                color += heat;
                if (_TWPoolUnblue > 0.0) color.b -= min(color.b * 0.6, unblue * _TWPoolsThroughHaze);   // and over the haze the fog put back
                color += poolLight * _TWPoolsThroughHaze;
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
            #pragma shader_feature_local_vertex _ _CHUNKMASK
            #pragma shader_feature_local_vertex _ _KITJITTER
            #pragma multi_compile_fog
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 chunk : TEXCOORD1; float4 part : TEXCOORD2; float3 smoothOS : TEXCOORD3; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float fog : TEXCOORD0; float3 positionWS : TEXCOORD1; };

            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                bool hidden = TWChunkHidden(v.chunk.x);                 // a hidden chunk is collapsed below; the rim must not pull it apart again
                v.positionOS.xyz = TWChunk(v.positionOS.xyz, v.chunk.x);
            #if defined(_KITJITTER)
                half3 kitTone;
                hidden = TWKitPart(v.positionOS.xyz, v.normalOS, v.smoothOS, v.part, kitTone) || hidden;
            #endif
                Varyings o;
                o.positionWS = TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y);
                float3 n = dot(v.smoothOS, v.smoothOS) > 0.01 ? v.smoothOS : v.normalOS;
                float4 cs = TransformWorldToHClip(o.positionWS);
                float3 nWS = TransformObjectToWorldNormal(n);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, nWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = clamp(_OutlineWidth * 70.0 / max(cs.w, 1.0), 0.0, _OutlineWidth);   // full width within 70 m, thinner beyond
                cs.xy += hidden ? 0.0 : dir * px * 2.0 / _ScreenParams.xy * cs.w;
                o.positionCS = cs;
                o.fog = ComputeFogFactor(cs.z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target { half3 c = ApplyFieldFog(ApplyMist(_OutlineColor.rgb, i.positionWS), i.positionWS); return half4(TWSilhouetteFog(c, MixFog(c, i.fog), _DetailStrength), 1.0); }
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
            #pragma shader_feature_local_vertex _ _CHUNKMASK
            #pragma shader_feature_local_vertex _ _KITJITTER
            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
            float3 _LightDirection;
            float3 _LightPosition;
            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 chunk : TEXCOORD1; float4 part : TEXCOORD2; UNITY_VERTEX_INPUT_INSTANCE_ID };
            float4 vertShadow(Attributes v) : SV_POSITION
            {
                UNITY_SETUP_INSTANCE_ID(v);
                v.positionOS.xyz = TWChunk(v.positionOS.xyz, v.chunk.x);
            #if defined(_KITJITTER)
                half3 kitTone; float3 unusedSmooth = 0;
                TWKitPart(v.positionOS.xyz, v.normalOS, unusedSmooth, v.part, kitTone);
            #endif
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
            #pragma shader_feature_local_vertex _ _CHUNKMASK
            #pragma shader_feature_local_vertex _ _KITJITTER
            struct Attributes { float4 positionOS : POSITION; float2 chunk : TEXCOORD1; float4 part : TEXCOORD2; UNITY_VERTEX_INPUT_INSTANCE_ID };
            float4 vertDepth(Attributes v) : SV_POSITION
            {
                UNITY_SETUP_INSTANCE_ID(v);
                v.positionOS.xyz = TWChunk(v.positionOS.xyz, v.chunk.x);
            #if defined(_KITJITTER)
                half3 kitTone; float3 unusedNormal = float3(0, 1, 0), unusedSmooth = 0;
                TWKitPart(v.positionOS.xyz, unusedNormal, unusedSmooth, v.part, kitTone);
            #endif
                return TransformWorldToHClip(TWSway(TransformObjectToWorld(v.positionOS.xyz), v.positionOS.y));
            }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
