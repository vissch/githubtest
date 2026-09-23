// Phase: A5b / C4 (implemented) — the tanks (Resources/Vehicles, drawn by TankRenderer). TW/Toon's look (two-step cartoon
// light, the main light's shadow, ink outline, mist and field fog, lamp light, the rain's wet sheen) on the owner's Tripo
// atlas, plus what a tank in a fight needs, per instance (TankRenderer's arrays, one instanced draw per part):
//  _Tread  links the track has rolled; the tread band (mask UV2.x) carries grouser bars at UV1.x (links round the loop,
//          seamless), so the tracks run while the atlas stays still;
//  _Damage x scorch: soot creeps over the paint in patches as the hull is hurt, all of it on a wreck;
//          y burn: embers glow in the soot and the exhaust outlets (UV2.z) run red;
//          z hit flash: the plate lights up for an instant where a round struck;
//          w furnace: the painted fire in the Maw's mouth (region UV2.y, lit only where the atlas is painted in fire
//          colours) glows, brighter as the engine works;
//  _Tint   rgb, strength: team 1's field-grey over the olive paint (only olive: bone, rust, iron and fire keep theirs).
//  _Team   rgb the side's colour, a how much of this part wears it: the horns are painted in it and glow faintly, so
//          the two sides tell apart at the gameplay zoom and at night (plain paint dies in the night grade).
// Night readability (critique 2026-09-22): a cool moonlight fill and rim keep a live hull off the mud (only soot, a
// wreck, goes near black, and even that keeps its plates); a trench lamp adds at most 0.6 so it never reads as fire.
// Mesh data (TankImport): UV0 atlas, UV1 tread coordinate, UV2 masks (tread, furnace, exhaust), UV3 smoothed normal for the
// ink, vertex colour the painted form (darker at the foot).
Shader "TW/Tank (URP)"
{
    Properties
    {
        _BaseColor ("Color", Color) = (1,1,1,1)
        _BaseMap ("Atlas", 2D) = "white" {}
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _OutlineColor ("Outline", Color) = (0.13, 0.10, 0.08, 1)
        _OutlineWidth ("Outline width (pixels up close)", Float) = 2.2
        _GrouserDark ("Grouser shade", Range(0,1)) = 0.62
        _MoonFill ("Moonlight fill", Color) = (0.55, 0.64, 0.85, 1)
        [HideInInspector] _Tread ("Tread (per instance)", Float) = 0
        [HideInInspector] _Damage ("Damage (per instance)", Vector) = (0,0,0,0)
        [HideInInspector] _Tint ("Team tint (per instance)", Vector) = (1,1,1,0)
        [HideInInspector] _Team ("Team colour band (per instance)", Vector) = (0,0,0,0)
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        TEXTURE2D(_BaseMap); SAMPLER(sampler_BaseMap);
        CBUFFER_START(UnityPerMaterial)
            half4 _BaseColor, _ShadeColor, _OutlineColor, _MoonFill;
            float4 _BaseMap_ST;
            float _OutlineWidth, _GrouserDark;
        CBUFFER_END
        UNITY_INSTANCING_BUFFER_START(TankProps)
            UNITY_DEFINE_INSTANCED_PROP(float, _Tread)
            UNITY_DEFINE_INSTANCED_PROP(float4, _Damage)
            UNITY_DEFINE_INSTANCED_PROP(float4, _Tint)
            UNITY_DEFINE_INSTANCED_PROP(float4, _Team)
        UNITY_INSTANCING_BUFFER_END(TankProps)

        float TankHash(float3 p) { p = frac(p * 0.3183099 + 0.1); p *= 17.0; return frac(p.x * p.y * p.z * (p.x + p.y + p.z)); }
        /// Value noise in object space: soot patches stay put on a moving hull.
        float TankNoise(float3 x)
        {
            float3 i = floor(x), f = frac(x); f = f * f * (3.0 - 2.0 * f);
            return lerp(lerp(lerp(TankHash(i), TankHash(i + float3(1,0,0)), f.x), lerp(TankHash(i + float3(0,1,0)), TankHash(i + float3(1,1,0)), f.x), f.y),
                        lerp(lerp(TankHash(i + float3(0,0,1)), TankHash(i + float3(1,0,1)), f.x), lerp(TankHash(i + float3(0,1,1)), TankHash(i + float3(1,1,1)), f.x), f.y), f.z);
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
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"

            struct Attributes
            {
                float4 positionOS : POSITION; float3 normalOS : NORMAL; float2 uv : TEXCOORD0; float2 tread : TEXCOORD1; float4 mask : TEXCOORD2;
                half4 color : COLOR; UNITY_VERTEX_INPUT_INSTANCE_ID
            };
            struct Varyings
            {
                float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2;
                half4 color : COLOR; float fog : TEXCOORD3; float4 mask : TEXCOORD4; float3 positionOS : TEXCOORD5;
                float4 damage : TEXCOORD6; float4 tint : TEXCOORD7;   // mask.w carries the tread coordinate
                float4 team : TEXCOORD8;
            };

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
                o.mask = float4(v.mask.xyz, v.tread.x - UNITY_ACCESS_INSTANCED_PROP(TankProps, _Tread));
                o.positionOS = v.positionOS.xyz;
                o.damage = UNITY_ACCESS_INSTANCED_PROP(TankProps, _Damage);
                o.tint = UNITY_ACCESS_INSTANCED_PROP(TankProps, _Tint);
                o.team = UNITY_ACCESS_INSTANCED_PROP(TankProps, _Team);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half3 paint = SAMPLE_TEXTURE2D(_BaseMap, sampler_BaseMap, i.uv).rgb * _BaseColor.rgb;
                half3 albedo = paint * i.color.rgb;
                // the atlas' painted fire (the Maw's mouth): warm, bright, not rust
                half fire = saturate((paint.r - 0.55) * 3.0) * saturate((paint.g - 0.28) * 3.0) * saturate((0.5 - paint.b) * 3.0);

                // team 1: field grey over the olive paint only
                half olive = saturate((paint.g - 0.88 * paint.r) * 12.0) * saturate((paint.g - 1.25 * paint.b) * 6.0) * (1.0 - fire);
                half lum = dot(albedo, half3(0.3, 0.59, 0.11));
                albedo = lerp(albedo, lum * i.tint.rgb * 1.75, i.tint.a * olive);
                // the side's colour on the parts that wear it (the horns)
                albedo = lerp(albedo, i.team.rgb, i.team.a * 0.75 * (1.0 - fire));

                // the tracks run: dark grouser bars travel round the loop on the tread band
                if (i.mask.x > 0.5)
                {
                    albedo = albedo * 1.3 + 0.03;   // the tread plate a shade lighter than the hull, so the bars read
                    half s = frac(i.mask.w);
                    half bar = smoothstep(0.0, 0.07, s) * (1.0 - smoothstep(0.34, 0.42, s));
                    half lip = smoothstep(0.34, 0.40, s) * (1.0 - smoothstep(0.40, 0.48, s));
                    albedo *= lerp(1.0, 1.0 - _GrouserDark, bar) + lip * 0.35;
                }

                // soot: patches that grow from the low, shadowed places up as the hull is hurt
                float scorch = i.damage.x;
                half n = TankNoise(i.positionOS * 1.7) * 0.65 + TankNoise(i.positionOS * 5.3) * 0.35;
                half soot = saturate((scorch * 1.35 - n) * 4.0);
                albedo = lerp(albedo, half3(0.22, 0.14, 0.10), soot * 0.85);   // charred warm (burnt metal, not a grey prop), plates still show

                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half wrap = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half band = smoothstep(0.32, 0.36, wrap) * 0.5 + smoothstep(0.69, 0.74, wrap) * 0.5;
                half gloss = 0;
                if (_TWWet.x > 0.0)
                {
                    half soaked = _TWWet.x * saturate(normalize(i.normalWS).y * 0.55 + 0.5);
                    gloss = soaked * 0.5;
                    albedo *= 1.0 - 0.22 * soaked;
                }
                // the battlefield reaches this too: the shaded half is a hemisphere, and the floor lights it from below
                half3 color = albedo * lerp(TWHemisphere(_ShadeColor.rgb * TWShadeTint(), normalize(i.normalWS)), mainLight.color, band);
                color += TWGroundBounce(normalize(i.normalWS), albedo);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation);
                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                if (gloss > 0.01)
                {
                    float3 r = reflect(-view, normalize(i.normalWS));
                    half glint = smoothstep(0.985, 0.992, dot(r, mainLight.direction));
                    color += glint * gloss * mainLight.color * 0.5 * mainLight.shadowAttenuation;
                }
                half3 lampGlint;
                half3 lamp = TWLocalLights(i.positionWS, normalize(i.normalWS), i.positionCS, view, gloss, lampGlint);
                // a trench lamp lights the plate, it does not set it on fire: most of its colour taken out, and capped
                half lampLum = dot(lamp, half3(0.3, 0.59, 0.11));
                lamp = lerp(lampLum.xxx, lamp, 0.4);
                color += albedo * min(lamp * 0.45, 0.3);
                color += lampGlint * 0.35;
                // moonlight: a cool fill and a rim, so a live hull stands off the mud at night (less on soot)
                half rim = pow(1.0 - saturate(dot(normalize(i.normalWS), view)), 3.0);
                color += albedo * _MoonFill.rgb * (0.22 + 0.9 * rim) * (1.0 - soot * 0.5);
                // light thrown back up off the mud: the underside, tracks and lower hull do not crush to black
                color += albedo * half3(0.10, 0.12, 0.18) * saturate(0.5 - normalize(i.normalWS).y * 0.5) * 1.6;

                // light of its own: embers in the soot, hot exhaust outlets, the furnace mouth, a round striking
                half burn = i.damage.y;
                half flicker = 0.65 + 0.35 * sin(_Time.y * 9.0 + n * 23.0) * sin(_Time.y * 3.7 + n * 11.0);
                half ember = soot * pow(saturate(1.0 - n * 1.25), 3.0) * burn * flicker;
                color += half3(1.0, 0.33, 0.07) * ember * 2.4;
                color += half3(1.0, 0.36, 0.09) * i.mask.z * saturate(burn + i.damage.w * 0.35) * flicker * 1.6;
                color += paint * fire * i.mask.y * i.damage.w * (1.4 + 0.6 * flicker) * 1.8;
                color += half3(1.0, 0.86, 0.62) * i.damage.z * 0.75;
                color += i.team.rgb * i.team.a * 0.45 * (1.0 - soot * 0.8);   // the side's colour glows a little

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
                float4 cs = TransformWorldToHClip(o.positionWS);
                float3 nWS = TransformObjectToWorldNormal(n);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, nWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = clamp(_OutlineWidth * 70.0 / max(cs.w, 1.0), 0.0, _OutlineWidth);
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
            float4 vertDepth(Attributes v) : SV_POSITION { UNITY_SETUP_INSTANCE_ID(v); return TransformWorldToHClip(TransformObjectToWorld(v.positionOS.xyz)); }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
