// Phase: B2 (implemented) — a lightning bolt: ribbons built by Storm.cs for each strike, additive and far over-bright so
// the bloom spreads them. Across its width a ribbon is a thin white-hot core inside a soft blue sheath (uv.x = -1..1);
// vertex alpha carries how bright a limb is (branches are dimmer than the trunk). _Tint is the flicker of the strike.
Shader "TW/Bolt (URP)"
{
    Properties
    {
        [HDR] _Tint ("Tint (the strike's flicker)", Color) = (0, 0, 0, 1)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+45" "IgnoreProjector"="True" }
        Pass
        {
            Name "Bolt"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            CBUFFER_START(UnityPerMaterial)
                half4 _Tint;
            CBUFFER_END
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; half4 color : COLOR; };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; half4 color : TEXCOORD1; };
            Varyings vert(Attributes v)
            {
                Varyings o;
                o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
                o.uv = v.uv; o.color = v.color;
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                half across = saturate(1.0 - abs(i.uv.x));
                half core = pow(across, 7.0), sheath = across * across;
                half3 colour = half3(1.0, 1.0, 1.0) * core * 4.0 + half3(0.45, 0.62, 1.0) * sheath * 0.8;
                return half4(colour * i.color.a * _Tint.rgb, 1.0);
            }
            ENDHLSL
        }
    }
}
