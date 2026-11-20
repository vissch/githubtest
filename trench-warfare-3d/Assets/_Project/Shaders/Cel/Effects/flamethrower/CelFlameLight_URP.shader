// Phase: CelFx flamethrower reference-first rebuild (2026-11-18) - the fire's light on the scene. Full-screen passes
// multiply the destination: the ground's own texture and the scene's ink stay and brighten, which is what the
// reference clip shows (texture contrast x3.3 while lit; the old FlameWash covered the ground with a flat tint).
// Drawn before every other transparent of the effect and after the scene's depth ink; the effect's own smoke
// (stencil bit 4) is skipped. Receivers come from the scene depth.
// The light is a finite LINE under the jet plus a broad POINT at the aim (the fire on the target):
//   E = (sin to the front end - sin to the tail end) / sqrt(d^2 + 1)  +  0.92 x 9 / (r^2 + 9) x the target's fire
// with d the level distance (m) to the line and r to the aim. Measured on the clip (REFERENCE_ATLAS.md) the SHOWN gain
// is R 1 + 6.5 E, G 1 + 1.9 E^1.25, B 1 - 0.75 E + 0.56 E^2 (blue drops to 0.75 at 3-5 m). The beach grade shows a
// written gain to the power 1.85 (R), 2.1 (G), 1.75 (B) (n18_04b), so the inverse powers are written.
// The lit part of the line follows the jet's front out and the tail in when the jet is cut; the point comes on with
// the catch and fades after the cut.
// The colour target may be 8-bit (the Range's close film is): a source above 1 is clamped before the blend, so one
// multiply cannot brighten (n18_04). Blend DstColor SrcAlpha gives destination x (rgb + alpha), a factor of 0..2 a
// channel; the layer spawns N records and each multiplies by the N-th root of the gain (4 records: gains up to 16).
//   pos        the nozzle                       axis       the aim point (world)
//   born/life  the shot
//   extra      x film frames per second (24), y overall strength (0: 1), z N, the layer's record count
//   erode      x frames after the shot the front leaves the nozzle, y frames it takes to reach the aim (ease 0.8)
//   size       x frame the tail leaves the nozzle, y frames it takes to reach the aim,
//              z frame the fade at the target ends, w frames the fade takes (linear)
Shader "TW/Cel/FlameLight"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent-10" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelFlameLight"
            Tags { "LightMode"="UniversalForward" }
            Blend DstColor SrcAlpha
            ColorMask RGB
            ZWrite Off ZTest Always Cull Off
            // bit 4 marks the effect's own opaque smoke: it has its own fire-lit shading
            Stencil { Ref 4 ReadMask 4 WriteMask 0 Comp NotEqual Pass Keep }
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            #define FL_LINE_HEIGHT 1.0                       // m: the jet above the ground (softens the near field)
            #define FL_POINT float2(0.92, 14.0)              // the aim's point light: weight, core radius squared (m2)
            #define FL_SHOWN_POWER float3(1.85, 2.1, 1.75)   // the grade: shown gain = written gain ^ this

            /// The gain the clip SHOWS at irradiance e. The model's far field shows too strong through the grade (n18_05:
            /// 0.59/0.36/0.21/0.15 where the clip has 0.35/0.19/0.12/0.05), so e is remapped first.
            float3 ShownGain(float e)
            {
                e = lerp(0.86 * pow(max(e, 1e-5), 1.48), 1.08 * e, smoothstep(0.6, 1.1, e));
                e = min(e, 1.6);
                return float3(1.0 + 7.6 * e, 1.0 + 1.9 * pow(e, 1.25), min(0.86, 1.0 - 0.75 * e + 0.56 * e * e));
            }

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                nointerpolation float4 a : TEXCOORD0;      // xyz the nozzle, w strength
                nointerpolation float4 b : TEXCOORD1;      // xyz the aim point, w the point light's level 0..1
                nointerpolation float3 lit : TEXCOORD2;    // the lit part of the line: from (tail) .. to (front), 0..1; z 1/N
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float alive = CelAlive(r);
                float f = max(CelAge(r), 0.0) * r.extra.x;                 // film frames since the shot
                float front = pow(saturate((f - r.erode.x) / max(r.erode.y, 1e-3)), 0.8);
                float tail = saturate((f - r.size.x) / max(r.size.y, 1e-3));
                float fade = saturate((r.size.z - f) / max(r.size.w, 1e-3));
                float strength = (r.extra.y > 0.0 ? r.extra.y : 1.0) * step(r.erode.x, f) * alive * step(1e-4, fade);
                Varyings o;
                // the whole screen, whatever the record's place (the receivers are found by depth)
                o.positionCS = strength > 1e-4 ? float4(v.uv * 2.0 - 1.0, 0.5, 1.0) : float4(0.0, 0.0, 0.0, 0.0);
                o.a = float4(r.pos, strength);
                // the target's fire: on over the 8 frames round the front's arrival, off with the fade
                o.b = float4(r.axis, saturate((f - (r.erode.x + r.erode.y - 3.0)) / 8.0) * fade);
                o.lit = float3(min(tail, 0.999), max(front, tail + 1e-3), 1.0 / max(r.extra.z, 1.0));
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float2 uv = GetNormalizedScreenSpaceUV(i.positionCS);
                #if UNITY_REVERSED_Z
                    float d = SampleSceneDepth(uv);
                #else
                    float d = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, SampleSceneDepth(uv));
                #endif
                float3 wp = ComputeWorldSpacePosition(uv, d, UNITY_MATRIX_I_VP);
                // the line under the jet, level
                float2 ab = i.b.xz - i.a.xz, ap = wp.xz - i.a.xz;
                float len = max(length(ab), 1e-3);
                float s0 = dot(ap, ab) / (len * len);                      // the receiver's place along the line, unclamped
                float dl = length(ap - ab * s0);                           // its level distance to the line
                float x1 = (i.lit.x - s0) * len, x2 = (i.lit.y - s0) * len;
                float e = (x2 / sqrt(x2 * x2 + dl * dl + 1e-4) - x1 / sqrt(x1 * x1 + dl * dl + 1e-4))
                    / sqrt(dl * dl + FL_LINE_HEIGHT * FL_LINE_HEIGHT);
                float2 rp = wp.xz - i.b.xz;
                e += FL_POINT.x * FL_POINT.y / (dot(rp, rp) + FL_POINT.y) * i.b.w;
                // nothing far above the jet (trees and banks behind it keep their night)
                float lift = wp.y - lerp(i.a.y, i.b.y, saturate(s0));
                e *= i.a.w * (1.0 - smoothstep(3.0, 7.0, lift));
                float3 g = pow(max(ShownGain(e), 1e-3), 1.0 / FL_SHOWN_POWER);      // what to write
                float3 m = pow(g, i.lit.z);                                         // this record's share
                clip(max(max(abs(m.r - 1.0), abs(m.g - 1.0)), abs(m.b - 1.0)) - 0.003);
                float a = min(1.0, min(m.r, min(m.g, m.b)));
                return half4(saturate(m - a), a);
            }
            ENDHLSL
        }
    }
}
