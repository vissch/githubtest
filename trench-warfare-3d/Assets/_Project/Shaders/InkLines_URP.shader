// Phase: B2 (implemented) — screen-space ink for the lines an inverted hull cannot draw: the creases inside a shape
// (board edges, bag seams, the rim of a shell hole, a trench lip). Full-screen pass, run by URP's Full Screen Pass
// renderer feature before the transparents, so smoke, tracers and markers stay clean.
// Method: device depth is linear in screen space across any flat surface, so its second difference is zero on a plane
// and jumps at a crease or a silhouette. Five depth taps a pixel, no normals prepass, nothing drawn twice: that is the
// whole cost, which is why it fits the minimum GPU. The line fades with distance so the far field stays quiet, and
// inside the fog bank round the battlefield (TWAtmosphere.hlsl), where a crisp line would cut through the fog.
Shader "TW/Ink Lines (URP)"
{
    Properties
    {
        _InkColor ("Ink", Color) = (0.13, 0.10, 0.08, 1)
        _Thickness ("Thickness (pixels)", Range(0.5, 3)) = 1.0
        _Sensitivity ("Sensitivity", Float) = 900
        _Threshold ("Threshold", Range(0, 2)) = 0.55
        _Strength ("Strength", Range(0, 1)) = 0.7
        _FadeStart ("Fade start (m)", Float) = 70
        _FadeEnd ("Fade end (m)", Float) = 170
    }
    SubShader
    {
        Tags { "RenderPipeline"="UniversalPipeline" }
        ZWrite Off ZTest Always Cull Off Blend Off
        Pass
        {
            Name "InkLines"
            HLSLPROGRAM
            #pragma vertex Vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.core/Runtime/Utilities/Blit.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

            half4 _InkColor;
            float _Thickness, _Sensitivity, _Threshold, _Strength, _FadeStart, _FadeEnd;

            half4 frag(Varyings input) : SV_Target
            {
                float2 uv = input.texcoord;
                half4 color = SAMPLE_TEXTURE2D_X(_BlitTexture, sampler_LinearClamp, uv);
                float2 px = _Thickness / _ScreenParams.xy;
                float c = SampleSceneDepth(uv);
                float l = SampleSceneDepth(uv - float2(px.x, 0)), r = SampleSceneDepth(uv + float2(px.x, 0));
                float d = SampleSceneDepth(uv - float2(0, px.y)), u = SampleSceneDepth(uv + float2(0, px.y));
                // second difference, relative to the depth itself so near and far creases weigh the same
                float bend = (abs(l + r - 2.0 * c) + abs(d + u - 2.0 * c)) / max(c, 1e-6);
                float eye = LinearEyeDepth(c, _ZBufferParams);
                float fade = 1.0 - saturate((eye - _FadeStart) / max(1.0, _FadeEnd - _FadeStart));
                half ink = smoothstep(_Threshold, _Threshold * 1.6, bend * _Sensitivity) * _Strength * fade;
                if (ink > 0.001 && _TWFieldFogColor.a > 0.0) ink *= saturate(1.0 - 1.15 * FieldFogAmount(ComputeWorldSpacePosition(uv, c, UNITY_MATRIX_I_VP)));
                color.rgb = lerp(color.rgb, _InkColor.rgb, ink);
                return color;
            }
            ENDHLSL
        }
    }
}
