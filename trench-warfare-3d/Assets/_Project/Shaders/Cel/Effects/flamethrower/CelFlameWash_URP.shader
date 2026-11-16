// Phase: CelFx flamethrower reference pass (2026-11-13) - a smooth depth-projected ground wash: premultiplied warm
// tint plus emission, drawn after opaque effects. One quad per record covers a widening capsule with gaussian
// falloff and animated variation. Scene depth reconstructs each receiver's position; height above the ground
// line fades its contribution. There is no terrain-normal gate. User stencil bit 4 excludes visible FlameJet
// forward and CelLobe body/ink fragments, preserving their colors; DepthOnly does not write that bit.
// The recipe uses this kind for the sustained wash and a separate target afterglow.
//   pos        the ground point below the nozzle (spawn Aim Dir, Speed ~0.001, Spread 0: vel gives the jet's heading)
//   size       x radius (m) at the nozzle end, y at the target end, z unused, w the line's length (m) (layer Stretch)
//   erode      x seconds the wash takes to run out along the line and fade in, y seconds it fades out at the end
//   tone       x palette (its lightest colour is the tint/light), y HDR multiplier, z emission strength multiplier;
//              y*z scales emission, while tint alpha follows the projected field and life envelope
//   extra      x falloff (gaussian k; 0: 2.2), y height (m) above the ground line at which the light is gone (0: 2.5),
//              z the ground line's rise from nozzle to target end (m), w unused
Shader "TW/Cel/FlameWash"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+12" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelFlameWash"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest Always Cull Off
            // User stencil bit 4 marks opaque Cel flame/smoke; bit 8 belongs to units.
            Stencil { Ref 4 ReadMask 4 WriteMask 0 Comp NotEqual Pass Keep }
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 screen : TEXCOORD0;
                nointerpolation float4 a : TEXCOORD1;      // xyz line start (ground below the nozzle), w strength
                nointerpolation float4 b : TEXCOORD2;      // xyz line end, w the share at the nozzle end
                nointerpolation float4 rr : TEXCOORD3;     // x radius at start, y at end, z falloff k, w fade height
                nointerpolation float4 tone : TEXCOORD4;   // x palette, y hdr x opacity
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), alive = CelAlive(r);
                float grow = max(r.erode.x, 1e-3), fade = max(r.erode.y, 1e-3);
                float g = smoothstep(0.0, 1.0, saturate(t / grow));
                float strength = g * (1.0 - smoothstep(r.life - fade, r.life, t)) * alive;
                float3 h = float3(r.vel.x, 0.0, r.vel.z);
                h = dot(h, h) > 1e-12 ? normalize(h) : float3(0.0, 0.0, 1.0);
                float3 side = float3(h.z, 0.0, -h.x);
                float len = max(r.size.w, 0.1) * lerp(0.35, 1.0, g);
                float3 p0 = r.pos;
                float3 p1 = r.pos + h * len + float3(0.0, r.extra.z * lerp(0.35, 1.0, g), 0.0);
                float r0 = r.size.x, r1 = lerp(r.size.x, r.size.y, lerp(0.35, 1.0, g));
                float m = 2.0 * max(r0, r1);
                float2 c = v.uv * 2.0 - 1.0;
                float3 mid = 0.5 * (p0 + p1);
                float3 w = mid + h * c.y * (0.5 * len + m) + side * c.x * m + float3(0.0, 0.05, 0.0);
                Varyings o;
                o.positionCS = TransformWorldToHClip(w) * (alive > 0.5 ? 1.0 : 0.0);
                o.screen = ComputeScreenPos(o.positionCS);
                o.a = float4(p0, strength);
                o.b = float4(p1, 1.0);
                o.rr = float4(r0, r1, r.extra.x > 0.0 ? r.extra.x : 2.2, r.extra.y > 0.0 ? r.extra.y : 2.5);
                o.tone = float4(r.tone.x, r.tone.y * r.tone.z, 0.0, 0.0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float2 uv = i.screen.xy / i.screen.w;
                #if UNITY_REVERSED_Z
                    float d = SampleSceneDepth(uv);
                #else
                    float d = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, SampleSceneDepth(uv));
                #endif
                float3 wp = ComputeWorldSpacePosition(uv, d, UNITY_MATRIX_I_VP);
                // distance to the ground line, measured level (the height is handled apart)
                float3 ab = i.b.xyz - i.a.xyz;
                float2 abh = ab.xz, ap = wp.xz - i.a.xz;
                float s = saturate(dot(ap, abh) / max(dot(abh, abh), 1e-4));
                float2 q = ap - abh * s;
                float rad = lerp(i.rr.x, i.rr.y, s);
                float field = exp(-i.rr.z * dot(q, q) / max(rad * rad, 1e-4));
                // beyond the line's ends the field already falls off with q; lift above the ground line fades it
                float gy = lerp(i.a.y, i.b.y, s);
                float lift = wp.y - gy;
                field *= 1.0 - smoothstep(0.25 * i.rr.w, i.rr.w, lift);
                field *= 1.0 - smoothstep(2.0, 4.0, abs(lift));   // nothing far under the line (the sky, holes)
                field *= lerp(i.b.w, 1.0, smoothstep(0.0, 0.7, s));
                // Uneven moving illumination follows the burning stream without a disc boundary.
                field *= 1.0 + 0.2 * CelSimplex3(float3(wp.xz * 0.8, _CelNow * 1.5));
                // A broad warm tint suppresses the cold blue plate; the smaller bright
                // center adds illumination without a blown-out ground hotspot.
                float alpha = saturate(sqrt(max(field, 0.0)) * 0.65) * i.a.w;
                half3 col = CelPal(CelRowA(i.tone.x), 3)
                    * (0.4 * alpha + field * i.tone.y * 0.9 * i.a.w);
                float fog = ComputeFogFactor(TransformWorldToHClip(wp).z);
                float visibility = CelAdditiveFog(half3(1.0, 1.0, 1.0), wp, fog).r;
                return half4(col * visibility, alpha * visibility);
            }
            ENDHLSL
        }
    }
}
