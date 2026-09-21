// Phase: B2 (implemented) — fire you can look at: torches, burning stumps. An upright card that turns to the camera; the
// flame itself is computed: two reads of a small noise texture scroll upward at different speeds, one swells and pinches
// the flame's width, the other bends it sideways more and more toward the tip, so it licks and tears instead of
// pulsing. Heat falls from the base to the tip and from the middle to the edge, and the colour is read off the heat:
// white at the heart, then yellow, orange, and a deep red fringe (the same layering as TW/Glow and the light pools).
// Additive and over-bright, so the bloom takes it. Every flame in a mesh is one draw call.
// Vertex data: position = the flame's foot (object space), uv0 = corner (-1..1), uv1 = width, height, phase, unused.
Shader "TW/Flame (URP)"
{
    Properties
    {
        _Noise ("Noise (R)", 2D) = "gray" {}
        _Strength ("Strength", Float) = 2.4
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+35" "IgnoreProjector"="True" }
        Pass
        {
            Name "Flame"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            TEXTURE2D(_Noise); SAMPLER(sampler_Noise);
            CBUFFER_START(UnityPerMaterial)
                float4 _Noise_ST;
                float _Strength;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; float2 corner : TEXCOORD0; float4 shape : TEXCOORD1; };
            struct Varyings { float4 positionCS : SV_POSITION; float3 uvp : TEXCOORD0; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                float3 foot = TransformObjectToWorld(v.positionOS.xyz);
                float3 right = normalize(float3(UNITY_MATRIX_V[0].x, 0, UNITY_MATRIX_V[0].z));
                float3 ws = foot + right * v.corner.x * v.shape.x * 0.5 + float3(0, (v.corner.y * 0.5 + 0.5) * v.shape.y, 0);
                o.positionCS = TransformWorldToHClip(ws);
                o.uvp = float3(v.corner.x, v.corner.y * 0.5 + 0.5, v.shape.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float y = i.uvp.y, t = _Time.y, phase = i.uvp.z;
                half swell = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(i.uvp.x * 0.35 + phase, y * 0.55 - t * 1.25)).r;
                half bend = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(i.uvp.x * 0.20 + phase * 2.3, y * 0.9 - t * 2.1)).r;
                float x = i.uvp.x + (bend - 0.5) * 1.1 * y;                       // the tip wanders, the base stays on the wick
                float width = (1.0 - y) * (0.45 + 0.75 * swell) * saturate(y * 9.0 + 0.25);
                half body = saturate((width - abs(x)) * 3.5);
                half heat = body * (1.0 - y * 0.75) * (0.55 + 0.9 * swell);
                half3 colour = lerp(half3(1.0, 0.16, 0.03), half3(1.0, 0.48, 0.10), smoothstep(0.10, 0.35, heat));
                colour = lerp(colour, half3(1.0, 0.85, 0.42), smoothstep(0.35, 0.65, heat));
                colour = lerp(colour, half3(1.0, 0.98, 0.90), smoothstep(0.70, 0.95, heat));
                return half4(colour * heat * _Strength, 1.0);
            }
            ENDHLSL
        }
    }
}
