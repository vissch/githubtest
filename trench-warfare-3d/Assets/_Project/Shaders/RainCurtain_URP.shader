// Phase: B2 (implemented) — rain in the distance. Close to the lens rain is streaks (TW/Rain); far off it is a grey
// veil that hangs in curtains, denser in some places than others, and drifts across the field with the wind. The ground
// is divided into 70 m cells that travel with the wind; a hash decides which cells hold a curtain (more of them the
// harder it rains), where in the cell it hangs and how wide it is. The mesh (Rain.cs) is a 7 x 7 block of cards round
// the camera's cell; a card whose cell is empty collapses to nothing in the vertex shader, so only real curtains are
// rasterised. They fade in beyond 60 m (nothing ever hangs in front of the fight) and lean with the wind.
// Vertex data: position.xy = the card's cell offset from the camera's cell, uv0 = corner (-1..1).
Shader "TW/Rain Curtain (URP)"
{
    Properties
    {
        _Noise ("Streak noise (R)", 2D) = "gray" {}
        _Drift ("Drift so far (xz), cell size, height", Vector) = (0, 0, 70, 55)
        _Weather ("Rain now, lean x, lean z, base height", Vector) = (0.6, 0, 0, 0)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+25" "IgnoreProjector"="True" }
        Pass
        {
            Name "Curtain"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            TEXTURE2D(_Noise); SAMPLER(sampler_Noise);
            CBUFFER_START(UnityPerMaterial)
                float4 _Noise_ST, _Drift, _Weather;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; float2 corner : TEXCOORD0; };
            struct Varyings { float4 positionCS : SV_POSITION; float4 uv : TEXCOORD0; half2 fade : TEXCOORD1; };

            float Hash(float2 p) { return frac(sin(dot(p, float2(127.1, 311.7))) * 43758.5453); }

            Varyings vert(Attributes v)
            {
                Varyings o;
                float cell = _Drift.z;
                float2 id = floor((_WorldSpaceCameraPos.xz - _Drift.xy) / cell) + v.positionOS.xy;
                float h = Hash(id), h2 = Hash(id + 17.7);
                half present = step(1.0 - (0.18 + 0.5 * _Weather.x), h);            // harder rain, more curtains
                float2 centre = (id + 0.2 + 0.6 * float2(h2, frac(h2 * 7.3))) * cell + _Drift.xy;
                float width = cell * (0.75 + 0.6 * h2) * present, height = _Drift.w * (0.8 + 0.4 * h);
                float3 right = normalize(float3(UNITY_MATRIX_V[0].x, 0, UNITY_MATRIX_V[0].z));
                float up01 = v.corner.y * 0.5 + 0.5;
                float3 ws = float3(centre.x, _Weather.w, centre.y) + right * v.corner.x * width * 0.5 + float3(0, up01 * height, 0);
                ws.xz -= _Weather.yz * up01 * height;                                // the top hangs upwind of the foot
                o.positionCS = TransformWorldToHClip(ws);
                float away = distance(_WorldSpaceCameraPos.xz, centre);
                o.fade = half2(saturate((away - 60.0) / 55.0) * (1.0 - saturate((away - 175.0) / 60.0)) * present * (0.35 + 0.65 * _Weather.x), ComputeFogFactor(o.positionCS.z));
                o.uv = float4(v.corner, h * 7.0, up01);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float t = _Time.y;
                half streaks = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(i.uv.x * 2.2 + i.uv.z, i.uv.w * 0.9 + t * 0.55)).r;
                half veil = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(i.uv.x * 0.11 + i.uv.z * 0.3, i.uv.w * 0.25 + t * 0.07)).r;
                half body = saturate(1.0 - i.uv.x * i.uv.x) * smoothstep(0.0, 0.10, i.uv.w) * (1.0 - smoothstep(0.55, 1.0, i.uv.w));
                half alpha = body * (0.30 + 0.70 * streaks) * (0.5 + 0.8 * veil) * i.fade.x * 0.42;
                half3 colour = unity_FogColor.rgb * 1.9 + half3(0.03, 0.04, 0.06);   // a little paler than the haze it hangs in
                return half4(MixFog(colour, i.fade.y), alpha);
            }
            ENDHLSL
        }
    }
}
