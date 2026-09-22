// Phase: B2 (implemented) — the sea beyond the beach (owner, 2026-09-22: "we're also adding an ocean with boats
// arriving"). The river's sheet (TW/Water) is a flat plane reading a baked depth map; open water is not flat, so
// this one moves.
//
// Budget first, like the river: OPAQUE, no grab pass, no depth read, no reflection camera, no extra texture. Depth
// and the distance to the waterline are baked into the mesh's vertex colours by Ocean.cs (they only change when the
// ground does), so a pixel costs two ripple reads and arithmetic.
//
// Water: two Gerstner swells running at the beach, shortening and steepening as they shoal, plus a chop; the crests
// throw whitecaps, and in the shallows the same wave phase becomes the line of breakers, the foam behind it and the
// last thin wash sliding up the sand. It takes the sky, the sun's glint, the mist, the field fog and the haze the
// way the river does, so the two read as one body of water where they meet.
//
// The depth pass displaces with exactly the same code as the lit pass: a crest written to the depth buffer from an
// undisplaced plane would be clipped away by depth priming, and every effect that reads scene depth would put the
// water surface half a metre under its own foam.
Shader "TW/Sea (URP)"
{
    Properties
    {
        _RippleMap ("Ripple map (RG slope, B streaks)", 2D) = "gray" {}
        _Shallow ("Over the sand", Color) = (0.32, 0.38, 0.36, 1)
        _Body ("Body", Color) = (0.13, 0.18, 0.19, 1)
        _Deep ("Deep", Color) = (0.075, 0.105, 0.125, 1)
        _Foam ("Foam", Color) = (0.78, 0.80, 0.78, 1)
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _Swell ("Swell height (m)", Range(0, 2)) = 0.62
        _SwellLength ("Swell length (m)", Range(4, 120)) = 34
        _SwellSpeed ("Swell speed (m/s)", Range(0, 12)) = 4.2
        _Inbound ("Direction the swell runs (world XZ)", Vector) = (0, -1, 0, 0)
        _Surf ("Breakers", Range(0, 2)) = 1.0
        _Chop ("Chop", Range(0, 1)) = 0.5
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry+10" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        CBUFFER_START(UnityPerMaterial)
            half4 _Shallow, _Body, _Deep, _Foam, _ShadeColor;
            float4 _Inbound;
            float _Swell, _SwellLength, _SwellSpeed, _Surf, _Chop;
        CBUFFER_END

        // Vertex colour, baked by Ocean.cs: r = depth / 20 m under the surface, g = 1 at the waterline falling to
        // 0 eighty metres out, b = how sheltered the water is (0 in the open, 1 close under the land).
        struct SeaVertex { float4 positionOS : POSITION; float4 color : COLOR; };

        /// One swell: a Gerstner wave that shortens, steepens and leans forward as the water shallows.
        float Swell(float2 xz, float2 dir, float wavelength, float amp, float speed, float shoal, out float2 shift, out float phase)
        {
            float k = 6.2831853 / max(2.0, wavelength * lerp(0.45, 1.0, shoal));
            phase = dot(dir, xz) * k - _Time.y * speed * k;
            shift = -dir * cos(phase) * amp * lerp(1.7, 0.6, shoal) * 0.45;
            return sin(phase) * amp;
        }

        /// The crest of the leading swell at a point, read per pixel: the triangles far out to sea are a hundred
        /// metres across, and a crest interpolated over one of those is a smear instead of a wave.
        float SeaCrest(float2 xz, float depth)
        {
            float k = 6.2831853 / max(2.0, _SwellLength * lerp(0.45, 1.0, saturate(depth / 3.5)));
            return saturate(sin(dot(normalize(_Inbound.xy + 1e-4), xz) * k - _Time.y * _SwellSpeed * k) * 0.5 + 0.5);
        }

        /// The surface at a vertex: where it ends up, the slope it leaves, and the phase the foam is cut from.
        float3 SeaSurface(float3 ws, float4 color, out float2 slope, out float crest, out float shoalOut)
        {
            float depth = color.r * 20.0, inshore = color.g, shelter = color.b;
            float shoal = saturate(depth / 3.5);
            float amp = _Swell * lerp(0.15, 1.0, shoal) * saturate(depth / 0.55) * (1.0 - 0.75 * shelter);
            float2 dir = normalize(_Inbound.xy + 1e-4);
            float2 across = float2(-dir.y, dir.x);
            float2 s1, s2; float p1, p2;
            float h = Swell(ws.xz, dir, _SwellLength, amp, _SwellSpeed, shoal, s1, p1)
                    + Swell(ws.xz, normalize(dir * 0.92 + across * 0.38), _SwellLength * 0.61, amp * 0.42, _SwellSpeed * 0.8, shoal, s2, p2);
            h += sin(dot(across, ws.xz) * 0.9 - _Time.y * 2.7) * cos(dot(dir, ws.xz) * 1.31 + _Time.y * 1.9) * _Chop * 0.11 * amp;
            ws.xz += (s1 + s2) * (1.0 - inshore * 0.6);
            ws.y += h;
            slope = dir * (cos(p1) * amp + cos(p2) * amp * 0.42) * 0.09;
            crest = saturate(sin(p1) * 0.5 + 0.5);
            shoalOut = shoal;
            return ws;
        }
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

            TEXTURE2D(_RippleMap); SAMPLER(sampler_RippleMap);

            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float3 wave : TEXCOORD1;   // xy surface slope, z crest 0..1
                float2 shore : TEXCOORD2;  // x depth (m), y inshore 0..1
                float fog : TEXCOORD3;
            };

            Varyings vert(SeaVertex v)
            {
                Varyings o;
                float2 slope; float crest, shoal;
                float3 ws = SeaSurface(TransformObjectToWorld(v.positionOS.xyz), v.color, slope, crest, shoal);
                o.positionWS = ws;
                o.positionCS = TransformWorldToHClip(ws);
                o.wave = float3(slope, crest);
                o.shore = float2(v.color.r * 20.0, v.color.g);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float2 xz = i.positionWS.xz;
                float depth = i.shore.x;
                half4 r1 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, xz / 7.0 + float2(0.03, -0.05) * _Time.y);
                half4 r2 = SAMPLE_TEXTURE2D(_RippleMap, sampler_RippleMap, xz / 19.0 - float2(0.01, 0.02) * _Time.y + 0.37);
                half2 slope = i.wave.xy + ((r1.rg - 0.5) + (r2.rg - 0.5) * 0.7) * 0.4;
                half crest = SeaCrest(xz, depth);

                // colour by depth: the sand shows through the last metre, then the body, then the deep
                half sand = 1.0 - smoothstep(0.25, 1.6, depth);
                half deep = smoothstep(2.5, 9.0, depth);
                half3 albedo = lerp(lerp(_Body.rgb, _Deep.rgb, deep), _Shallow.rgb, sand);

                // The surf. One wave phase does all of it: the breaker where the crest trips over the bar, a tail of
                // spent foam behind it, and the thin wash that runs up the sand and drains back.
                half breaker = smoothstep(0.55, 0.95, crest) * saturate(1.0 - depth / 2.2) * _Surf;
                half spent = smoothstep(0.25, 0.9, crest * 0.6 + 0.4) * saturate(1.0 - depth / 1.1) * 0.7;
                half wash = saturate(1.0 - depth / 0.35) * (0.45 + 0.55 * sin(_Time.y * 0.9 + xz.x * 0.02));
                half whitecap = smoothstep(0.80, 0.97, crest) * smoothstep(2.5, 6.0, depth) * 0.75 * _Surf;   // the open sea is not glass in this weather
                half foam = saturate(max(max(breaker, spent), max(wash, whitecap)) * (0.75 + 0.5 * r1.b));
                // and what is put into the water: a boat's wake, men wading ashore, shells (WaterRings), and the rain
                half stir; half rings = TWRings(xz, stir);
                foam = saturate(max(foam, rings * 0.85) + TWRainRings(xz) * 0.45);
                albedo = lerp(albedo, _Foam.rgb, foam);

                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half3 color = albedo * lerp(_ShadeColor.rgb * TWShadeTint(), mainLight.color, 0.5 + 0.5 * mainLight.shadowAttenuation);

                float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                float3 n = normalize(float3(slope.x, 1.0, slope.y));
                float3 r = reflect(-view, n);
                half fresnel = pow(1.0 - saturate(dot(n, view)), 3.0);
                half3 sky = TWSky() * half3(0.93, 0.98, 1.05) * lerp(1.12, 0.55, saturate(r.y * 1.4));
                half mirror = min(0.16 + 0.70 * fresnel, 0.42) * (1.0 - foam * 0.8);
                color = lerp(color, sky, mirror);
                color += smoothstep(0.985, 0.993, dot(r, mainLight.direction)) * 0.7 * mainLight.color * mainLight.shadowAttenuation;
                half3 lampGlint;
                half3 local = TWLocalLights(i.positionWS, n, i.positionCS, view, 1.0, lampGlint);
                color += (albedo + 0.25) * local + lampGlint * 1.2;

                color = lerp(color, ApplyMist(color, i.positionWS), 0.30);
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
            float4 vertDepth(SeaVertex v) : SV_POSITION
            {
                float2 slope; float crest, shoal;
                return TransformWorldToHClip(SeaSurface(TransformObjectToWorld(v.positionOS.xyz), v.color, slope, crest, shoal));
            }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
