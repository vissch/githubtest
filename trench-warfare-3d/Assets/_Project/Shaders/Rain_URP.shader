// Phase: B2 (implemented) — rain as streaks: one mesh of thin quads (Rain.cs), one draw call, no texture. Each streak
// owns a point in a unit box; the vertex shader scales the box round the ground the camera looks at, lets every point
// fall with the wind and wraps it back in at the top, so the rain follows the camera with no CPU work and no popping
// (streaks fade out toward the box's faces, close to the lens and far away). The rain comes in squalls: Rain.cs hands
// over how far the rain has fallen and blown so far (_Offset, integrated on the CPU so a change of wind bends the
// streaks instead of teleporting them) and how hard it rains (_Level): each streak has its own threshold, so a drizzle
// is a few thin short streaks and a downpour is all of them, longer, brighter and slanted by the wind. Additive, so it only ever adds a little
// light to a dark frame. Vertex data: position = the point (0..1), uv0 = side (-1/1) and end (0/1), uv1.x = a random.
Shader "TW/Rain (URP)"
{
    Properties
    {
        _Color ("Colour (a = strength)", Color) = (0.62, 0.72, 0.92, 0.12)
        _Centre ("Box centre", Vector) = (0, 0, 0, 0)
        _Size ("Box size", Vector) = (90, 46, 90, 0)
        _Fall ("Wind x, fall speed, wind z, streak length", Vector) = (3, 19, -2, 0.9)
        _Width ("Streak width (m)", Float) = 0.017
        _Offset ("Fallen so far", Vector) = (0, 0, 0, 0)
        _Level ("Rain now 0..1", Float) = 0.6
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+30" "IgnoreProjector"="True" }
        Pass
        {
            Name "Rain"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            CBUFFER_START(UnityPerMaterial)
                half4 _Color;
                float4 _Centre, _Size, _Fall, _Offset;
                float _Width, _Level;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; float2 quad : TEXCOORD0; float2 random : TEXCOORD1; };
            struct Varyings { float4 positionCS : SV_POSITION; half alpha : TEXCOORD0; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                float3 velocity = float3(_Fall.x, -_Fall.y * (0.8 + 0.4 * v.random.x), _Fall.z);
                float3 p = v.positionOS.xyz * _Size.xyz + _Offset.xyz * float3(1.0, 0.8 + 0.4 * v.random.x, 1.0);
                float3 cell = frac((p - _Centre.xyz) / _Size.xyz + 0.5);          // where in the box, 0..1
                p = _Centre.xyz + (cell - 0.5) * _Size.xyz;
                float3 along = normalize(velocity);
                float3 end = p + along * _Fall.w * (0.7 + 0.6 * v.random.x) * v.quad.y;
                float3 side = normalize(cross(along, _WorldSpaceCameraPos - end));
                float reach = distance(_WorldSpaceCameraPos, end);
                float3 ws = end + side * v.quad.x * _Width * max(1.0, reach / 45.0);   // never thinner than a pixel's worth far off
                o.positionCS = TransformWorldToHClip(ws);
                float3 face = saturate((0.5 - abs(cell - 0.5)) * 10.0);
                o.alpha = _Color.a * face.x * face.y * face.z * saturate((reach - 3.0) / 8.0) * (1.0 - saturate((reach - 95.0) / 40.0)) * (0.55 + 0.45 * v.random.x)
                    * saturate((_Level - v.random.y * 0.9) * 7.0) * (0.6 + 0.6 * _Level);   // this streak only falls when it rains hard enough
                return o;
            }

            half4 frag(Varyings i) : SV_Target { return half4(_Color.rgb, i.alpha); }
            ENDHLSL
        }
    }
}
