// Phase: C4 (implemented) — hand-drawn flipbook sprites for the fight: shell bursts, earth and water columns, the dust a
// round kicks up, the puff and star where a man is hit, the muzzle flare. One quad each, drawn instanced by FlipbookFx.
// Nothing about the sprite is in the mesh: the instance matrix is not a transform but a packed record (see FlipbookFx.Pack)
//   m03 m13 m23  world position          m00 width (m)      m11 height (m)     m22 frame (fractional: blends to the next)
//   m01 alpha    m10 brightness           m02 roll (rad)     m12 1 = upright (turns to the view about Y only, stays vertical)
//   m20 1 = anchored at its bottom edge   m21 the card's opacity, negative to mirror it   (m01 is the fade, 1 at birth)
// and the vertex shader builds the card facing the camera. The books are greyscale drawings with alpha: their own light and
// dark reads as the toon's lit and shade bands (_Lit), so a tinted cloud sits under the same moon as the men. Additive books
// (flash, star, muzzle) ignore the scene light and only dim with the fog.
Shader "TW/Flipbook (URP)"
{
    Properties
    {
        _MainTex ("Book", 2D) = "white" {}
        _Grid ("Columns, rows, frames, unused", Vector) = (4, 4, 16, 0)
        _Tint ("Tint", Color) = (1, 1, 1, 1)
        _Shade ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _Levels ("Ink levels: the drawing's value that is full shade (x) and full light (y)", Vector) = (0, 1, 0, 0)
        _Lit ("Lit by the scene (0 additive/unlit, 1 toon lit)", Range(0, 1)) = 1
        _MaskOnly ("Use alpha only (a drawing in black)", Float) = 0
        _Erode ("Tears apart as it fades (0 fades evenly)", Range(0, 1)) = 0
        _ShadeMood ("How much the mood tints the shade (smoke keeps more of its own grey)", Range(0, 1)) = 1
        [Enum(UnityEngine.Rendering.BlendMode)] _SrcBlend ("Src", Float) = 5
        [Enum(UnityEngine.Rendering.BlendMode)] _DstBlend ("Dst", Float) = 10
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+10" "IgnoreProjector"="True" }
        Pass
        {
            Name "Flipbook"
            Tags { "LightMode"="UniversalForward" }
            Blend [_SrcBlend] [_DstBlend]
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            TEXTURE2D(_MainTex); SAMPLER(sampler_MainTex);
            CBUFFER_START(UnityPerMaterial)
                float4 _Grid, _Levels;
                half4 _Tint, _Shade;
                float _Lit, _MaskOnly, _Erode, _SrcBlend, _DstBlend, _ShadeMood;
            CBUFFER_END
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 uv : TEXCOORD0;        // xy this frame's cell, zw the next frame's
                float4 tone : TEXCOORD1;      // x fade (1 at birth), y brightness, z blend to the next frame, w fog
                float opacity : TEXCOORD3;
                float2 local : TEXCOORD4;     // the card's own uv, for the erode's edge bias and the underside
                float3 positionWS : TEXCOORD2;
                float4 screen : TEXCOORD5;    // where it lands on the screen, for the scene depth behind it
                float2 extra : TEXCOORD6;     // x the card's width (m), y its depth from the eye (m)
            };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                float4x4 m = UNITY_MATRIX_M;
                float3 at = float3(m._m03, m._m13, m._m23);
                float width = m._m00, height = m._m11, frame = m._m22;
                float alpha = m._m01, bright = m._m10, roll = m._m02, upright = m._m12, anchored = m._m20, mirror = m._m21 < 0 ? -1.0 : 1.0, opacity = abs(m._m21);
                // the card: centred, or standing on its bottom edge; mirrored; rolled in its own plane
                float2 c = float2((v.uv.x - 0.5) * mirror, v.uv.y - 0.5 * (1.0 - anchored));
                float sr, cr; sincos(roll, sr, cr);
                c = float2(c.x * cr - c.y * sr, c.x * sr + c.y * cr) * float2(width, height);
                float3 viewRight = UNITY_MATRIX_V[0].xyz, viewUp = UNITY_MATRIX_V[1].xyz;
                float3 toEye = normalize(_WorldSpaceCameraPos - at);
                float3 flatRight = normalize(cross(float3(0, 1, 0), toEye));
                float3 right = lerp(viewRight, flatRight, upright), up = lerp(viewUp, float3(0, 1, 0), upright);
                o.positionWS = at + right * c.x + up * c.y;
                o.positionCS = TransformWorldToHClip(o.positionWS);
                // the book: frames left to right, top row first; a fractional frame blends into the next
                float frames = max(1.0, _Grid.z), cols = max(1.0, _Grid.x), rows = max(1.0, _Grid.y);
                float f0 = clamp(floor(frame), 0.0, frames - 1.0), f1 = min(f0 + 1.0, frames - 1.0);
                float2 cell = float2(1.0 / cols, 1.0 / rows);
                float2 uv0 = (float2(fmod(f0, cols), rows - 1.0 - floor(f0 / cols)) + v.uv) * cell;
                float2 uv1 = (float2(fmod(f1, cols), rows - 1.0 - floor(f1 / cols)) + v.uv) * cell;
                o.uv = float4(uv0, uv1);
                o.tone = float4(alpha, bright, frame - floor(frame), ComputeFogFactor(o.positionCS.z));
                o.opacity = opacity;
                o.local = v.uv;
                o.screen = ComputeScreenPos(o.positionCS);
                o.extra = float2(width, -TransformWorldToView(o.positionWS).z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                half4 a = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv.xy), b = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv.zw);
                half4 tex = lerp(a, b, i.tone.z);
                // a cloud tears apart as it goes: the edges go first and the dense heart last, and what is left thins
                half gone = 1.0 - i.tone.x;
                half edge = saturate(length(i.local - 0.5) * 2.0);
                half threshold = gone * (0.45 + 0.55 * edge);
                half torn = saturate((tex.a - threshold) / 0.45) * pow(1.0 - gone, 1.5);
                half alpha = lerp(tex.a * i.tone.x, torn, _Erode) * i.opacity;
                // soft where it meets the ground or a wall (the scene's depth behind it, over 0.8 m), and gone as it comes
                // through the lens: up close a card wider than the picture was a wall of smoke for seconds
                float scene = LinearEyeDepth(SampleSceneDepth(i.screen.xy / i.screen.w), _ZBufferParams);
                alpha *= saturate((scene - i.extra.y) / 0.8) * saturate((i.extra.y - 0.25 * i.extra.x) / (0.5 * i.extra.x + 0.01));
                if (alpha < 0.004) discard;
                // the packs keep black under their transparent pixels, so the small mips of a thin wisp go dark: read the
                // drawing's value per unit of coverage
                half ink = _MaskOnly > 0.5 ? 1.0 : saturate(dot(tex.rgb, half3(0.299, 0.587, 0.114)) / max(tex.a, 0.06));
                // the drawing's light and dark are the toon's two bands (each book's own range stretched to them, so a
                // drawing done in mid greys still catches the light); an unlit book keeps its own values
                half band = saturate((ink - _Levels.x) / max(0.01, _Levels.y - _Levels.x));
                half3 lit = lerp(_Shade.rgb * lerp(half3(1, 1, 1), TWShadeTint(), _ShadeMood), _MainLightColor.rgb, band);   // smoke keeps more of its own grey under a blue moon
                half3 color = _Tint.rgb * lerp(ink.xxx, lit, _Lit) * i.tone.y;
                if (_Erode > 0.5) color *= lerp(0.78, 1.0, smoothstep(0.0, 0.6, i.local.y));   // a cloud's underside is in its own shadow
                // the shell's own flash lights the earth it threw up and the smoke rolling off it: the drawing's light
                // parts catch it most, its dark parts least, so the column is modelled by its own burst and not flooded
                color += lerp(ink, 1.0, 0.3) * TWBurstLight(i.positionWS) * _Lit;
                half fog = ComputeFogIntensity(i.tone.w);
                if (_Lit < 0.5)
                {
                    // additive: dims into the fog, adds nothing where the mist would have hidden it
                    half mist = 1.0 - saturate(FieldFogAmount(i.positionWS) + _TWMistColor.a * saturate((_TWMist.x - i.positionWS.y) * _TWMist.y) * saturate((distance(_WorldSpaceCameraPos, i.positionWS) - _TWMist.z) * _TWMist.w));
                    return half4(color * alpha * fog * mist, 1.0);
                }
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                return half4(MixFog(color, i.tone.w), alpha);
            }
            ENDHLSL
        }
    }
}
