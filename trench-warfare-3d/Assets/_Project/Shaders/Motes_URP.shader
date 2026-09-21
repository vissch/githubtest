// Phase: B2 (implemented) — the smallest moving things, seen only by a close camera (_TWClose): moths round the lamps,
// ash rising off the fires, water dripping from roof edges and branches. SmallLife.cs builds one mesh of four-vertex
// cards; each vertex carries its anchor (POSITION), its corner (uv0) and what it is (uv1: x kind, y phase 0..1, z size in
// metres, w reach in metres). All motion is computed here from _Time, so a thousand motes cost the CPU nothing. Cards
// beyond 45 m, or any card at the standard view, collapse to nothing in the vertex shader.
//   kind 0 moth: circles its lamp at "reach", bobbing and darting; a warm pale speck, additive
//   kind 1 ash:  rises "reach" metres off the fire, drifting with the wind, glowing orange low and dying to grey
//   kind 2 drip: falls "reach" metres from its anchor and starts again; only while the field is wet, more in rain
Shader "TW/Motes (URP)"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+30" "IgnoreProjector"="True" }
        Pass
        {
            Name "Motes"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            float4 _TWWind;
            struct Attributes { float4 positionOS : POSITION; float2 corner : TEXCOORD0; float4 what : TEXCOORD1; };
            struct Varyings { float4 positionCS : SV_POSITION; float2 corner : TEXCOORD0; half4 colour : TEXCOORD1; };
            Varyings vert(Attributes v)
            {
                Varyings o;
                float3 anchor = v.positionOS.xyz;
                float kind = v.what.x, phase = v.what.y, size = v.what.z, reach = v.what.w;
                float t = _Time.y;
                float3 p = anchor; half3 colour = 0; float2 stretch = float2(1, 1);
                if (kind < 0.5)
                {
                    float a = t * (1.6 + phase * 1.7) + phase * 40.0, dart = sin(t * 7.3 + phase * 90.0) * 0.12;
                    float r = reach * (0.55 + 0.45 * sin(t * 0.9 + phase * 17.0));
                    p += float3(cos(a) * r + dart, sin(t * 2.3 + phase * 23.0) * 0.22 + dart, sin(a * 1.13) * r);
                    colour = half3(1.0, 0.82, 0.55) * (0.55 + 0.45 * sin(t * 31.0 + phase * 50.0));   // wings catch the lamp and lose it
                }
                else if (kind < 1.5)
                {
                    float k = frac(t * (0.10 + 0.08 * phase) + phase * 7.0);
                    p += float3(sin(k * 9.0 + phase * 30.0) * 0.35, k * reach, cos(k * 7.0 + phase * 19.0) * 0.35) + float3(_TWWind.x, 0, _TWWind.y) * (k * k * 22.0);
                    colour = lerp(half3(1.6, 0.62, 0.16), half3(0.20, 0.20, 0.22), smoothstep(0.15, 0.6, k)) * (1.0 - smoothstep(0.7, 1.0, k)) * smoothstep(0.0, 0.05, k);
                }
                else
                {
                    float wet = max(_TWWet.z, _TWWet.x * 0.35);
                    float k = frac(t * (0.55 + 0.5 * phase) + phase * 13.0);
                    float fall = saturate(k * 1.6);   // falls, then a pause while the next drop gathers
                    p.y -= fall * fall * reach;
                    stretch = float2(0.35, 1.0 + fall * 5.0);
                    colour = (TWSky() * 0.9 + 0.05) * step(phase, wet * 1.1) * step(k, 0.625);
                }
                float seen = _TWClose * (1.0 - saturate((distance(_WorldSpaceCameraPos, p) - 32.0) / 13.0));
                float3 right = UNITY_MATRIX_V[0].xyz, up = kind > 1.5 ? float3(0, 1, 0) : UNITY_MATRIX_V[1].xyz;
                p += (right * v.corner.x * stretch.x + up * v.corner.y * stretch.y) * size * step(0.001, seen);
                o.positionCS = TransformWorldToHClip(p);
                o.corner = v.corner;
                o.colour = half4(colour * seen, 1);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                half soft = saturate(1.0 - dot(i.corner, i.corner));
                return half4(i.colour.rgb * soft * soft, 1.0);
            }
            ENDHLSL
        }
    }
}
