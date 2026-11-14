// Phase: CelFx flamethrower reference pass (2026-11-14) - depth-projected receiver illumination.
// Four bounded multiplies preserve the receiver grooves on HDR and LDR targets. Drawn after opaque effects. Four coincident proxy quads per record cover a widening capsule with gaussian
// falloff and animated variation. Scene depth reconstructs each receiver's position; height above the ground
// line fades its contribution. Depth normals dim vertical faces away from the stream while preserving ground gain.
// User stencil bit 4 excludes visible FlameJet
// forward and CelLobe body/ink fragments, preserving their colors; DepthOnly does not write that bit.
// The recipe uses this kind for the sustained wash and a separate target afterglow.
//   pos        the ground point below the nozzle (spawn Aim Dir, Speed ~0.001, Spread 0: vel gives the jet's heading)
//   size       x radius (m) at the nozzle end, y at the target end, z unused, w the line's length (m) (layer Stretch)
//   erode      x seconds the wash takes to run out along the line and fade in, y seconds it fades out at the end
//   tone       x reserved palette, y strength multiplier, z recipe opacity;
//              y*z scales receiver gain (3.8 is the sustained reference strength)
//   extra      x falloff (gaussian k; 0: 2.2), y height (m) above the ground line at which the light is gone (0: 2.5),
//              z the ground line's rise from nozzle to target end (m), w 1 = radial target afterglow; legacy .6 = stream
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
            Blend DstColor SrcColor, Zero One
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
                nointerpolation float4 tone : TEXCOORD4;   // x reserved palette, y strength x opacity, z reach phase, w target mode
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), alive = CelAlive(r);
                float grow = max(r.erode.x, 1e-3), fade = max(r.erode.y, 1e-3);
                float g = smoothstep(0.0, 1.0, saturate(t / grow));
                // Light is strong near the nozzle immediately; g still owns its growing reach.
                // Target afterglow uses its authored overlap ramp. The stream
                // keeps its anchored nozzle ignition exactly as before.
                float rise = r.extra.w > 0.9 ? grow : 0.22;
                float strength = smoothstep(0.0, rise, t) * (1.0 - smoothstep(r.life - fade, r.life, t)) * alive;
                float3 h = float3(r.vel.x, 0.0, r.vel.z);
                h = dot(h, h) > 1e-12 ? normalize(h) : float3(0.0, 0.0, 1.0);
                float3 side = float3(h.z, 0.0, -h.x);
                float len = max(r.size.w, 0.1) * lerp(0.35, 1.0, g);
                float3 p0 = r.pos;
                float3 p1 = r.pos + h * len + float3(0.0, r.extra.z * lerp(0.35, 1.0, g), 0.0);
                float r0 = r.size.x, r1 = lerp(r.size.x, r.size.y, lerp(0.35, 1.0, g));
                // The first advancing pool is narrower; it opens with the stream.
                r1 *= lerp(0.65, 1.0, g);
                // Raster bounds need to cover receivers below this proxy plane.
                // At low camera pitch, height displacement moves their projected
                // footprint beyond a two-radius ground quad. Four radii keeps the
                // quad edge beyond the visible Gaussian without changing light size.
                float m = 4.0 * max(r0, r1);
                float2 c = v.uv * 2.0 - 1.0;
                float3 mid = 0.5 * (p0 + p1);
                float3 w = mid + h * c.y * (0.5 * len + m) + side * c.x * m + float3(0.0, 0.05, 0.0);
                Varyings o;
                o.positionCS = TransformWorldToHClip(w) * (alive > 0.5 ? 1.0 : 0.0);
                o.screen = ComputeScreenPos(o.positionCS);
                o.a = float4(p0, strength);
                o.b = float4(p1, 1.0);
                o.rr = float4(r0, r1, r.extra.x > 0.0 ? r.extra.x : 2.2, r.extra.y > 0.0 ? r.extra.y : 2.5);
                o.tone = float4(r.tone.x, r.tone.y * r.tone.z, g, r.extra.w);
                return o;
            }

            // The renderer guarantees depth, not a camera normals texture.
            float3 WashReceiverWorld(float2 uv)
            {
                float rawDepth = SampleSceneDepth(uv);
                #if !UNITY_REVERSED_Z
                    rawDepth = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, rawDepth);
                #endif
                return ComputeWorldSpacePosition(uv, rawDepth, UNITY_MATRIX_I_VP);
            }

            float3 WashReceiverNormal(float2 uv, float3 centre)
            {
                // Use explicit neighboring samples, not ddx/ddy quad derivatives.
                // As in URP's five-tap reconstruction, choose the neighbor on each
                // axis with the smaller eye-depth discontinuity. This keeps a
                // crate edge from borrowing the ground/sky normal next to it.
                float2 texel = abs(_CameraDepthTexture_TexelSize.xy);
                float3 left = centre - WashReceiverWorld(uv - float2(texel.x, 0.0));
                float3 right = WashReceiverWorld(uv + float2(texel.x, 0.0)) - centre;
                float3 down = centre - WashReceiverWorld(uv - float2(0.0, texel.y));
                float3 up = WashReceiverWorld(uv + float2(0.0, texel.y)) - centre;
                float3 eyeForward = UNITY_MATRIX_V[2].xyz;
                float3 dx = abs(dot(left, eyeForward)) < abs(dot(right, eyeForward)) ? left : right;
                float3 dy = abs(dot(down, eyeForward)) < abs(dot(up, eyeForward)) ? down : up;
                float3 normal = cross(dy, dx);
                float normalLength2 = dot(normal, normal);
                normal = normalLength2 > 1e-12 ? normal * rsqrt(normalLength2) : float3(0.0, 1.0, 0.0);
                if (dot(normal, _WorldSpaceCameraPos - centre) < 0.0) normal = -normal;
                return normal;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float2 uv = i.screen.xy / i.screen.w;
                float3 wp = WashReceiverWorld(uv);
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
                // Light comes from the closest stream point above its ground
                // footprint. Preserve the measured horizontal-ground gain while
                // dimming steep receiver faces turned away from that light.
                float3 receiverNormal = WashReceiverNormal(uv, wp);
                float3 toLight = lerp(i.a.xyz, i.b.xyz, s) + float3(0.0, 1.2, 0.0) - wp;
                toLight *= rsqrt(max(dot(toLight, toLight), 1e-6));
                float direct = saturate(dot(receiverNormal, toLight) / max(toLight.y, 0.25));
                float steep = 1.0 - smoothstep(0.35, 0.80, saturate(receiverNormal.y));
                field *= lerp(1.0, 0.15 + 0.85 * direct, steep);
                field *= lerp(i.b.w, 1.0, smoothstep(0.0, 0.7, s));
                // Receiver light builds just beyond the nozzle rather than making
                // the source's whole foreground a bright orange disc. Ground
                // measurements put the stronger near patch at s ~= 0.14.
                bool targetGlow = i.tone.w > 0.9;
                if (!targetGlow) field *= lerp(0.34, 1.0, smoothstep(0.0, 0.25, s));
                // The nozzle is already bright during ignition, but illumination
                // farther down the path catches up as the stream reaches it.
                // The steady-state field is unchanged once reach phase is one.
                if (!targetGlow) field *= lerp(1.0, lerp(0.18, 1.0, i.tone.z), smoothstep(0.10, 0.85, s));
                // Uneven moving illumination follows the burning stream without a disc boundary.
                field *= 1.0 + 0.2 * CelSimplex3(float3(wp.xz * 0.8, _CelNow * 1.5));
                float fog = ComputeFogFactor(TransformWorldToHClip(wp).z);
                float visibility = CelAdditiveFog(half3(1.0, 1.0, 1.0), wp, fog).r;
                // The project renders into a gamma framebuffer. These are direct
                // dimensionless receiver gains, not sRGB colors or palette values.
                // Multiplication boosts the receiver's own texture; no emissive
                // orange layer fills its cracks and no terrain specular is invoked.
                float middle = targetGlow ? 0.0 : smoothstep(0.18, 0.43, s) * (1.0 - smoothstep(0.65, 0.90, s));
                float amount = saturate(field * i.a.w * max(i.tone.y, 0.0) / 3.8
                    * visibility * (1.0 - 0.42 * middle));
                half3 peakGain = targetGlow ? half3(12.6, 3.9, 0.78)
                    : lerp(half3(12.6, 3.9, 1.15), half3(8.1, 2.475, 0.78), s);
                half3 gain = lerp(half3(1.0, 1.0, 1.0), peakGain, amount);
                // Four coincident quads in FlameWashKind each apply the fourth
                // root of the requested gain. Blend DstColor SrcColor means
                // dst *= 2 * src. The bounded source also works on UNORM targets,
                // which clamp shader output before the blend (HDR is not needed).
                // max gain 12.6 gives src < 1; neutral gain 1 gives src = 0.5.
                half3 blendSource = 0.5 * sqrt(sqrt(max(gain, 0.0)));
                return half4(blendSource, 0.0);
            }
            ENDHLSL
        }
    }
}
