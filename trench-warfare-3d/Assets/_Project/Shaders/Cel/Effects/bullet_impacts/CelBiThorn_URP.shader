// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - BiThorn: an OPAQUE, inked, velocity-aligned spike for
// the ground-hit crowns and the dirt chips of bullet_impacts_cel (b05). The Streak is additive, so a dark-brown thorn
// cannot be drawn with it, and a Lobe squashes only vertically (b04's crowns read as a palisade of sticks). A thorn is a
// camera-facing quad from its base (where it was born, it stays on the ground until it has flown its minimum length) to
// its tip (where the record is now): wide at the base, pointed at the tip, cel-banded, with an ink edge, cut out (no
// alpha) and depth-written like a Lobe.
//   size.xyz   base width (m) at birth, at death, ease      size.w  length in seconds of its speed
//   erode.xyz  the cut-out threshold across the section, rising over the life (thins the thorn to nothing in steps)
//   tone       x palette, y HDR core, z unused, w ink px at Close
//   extra      x shape 0 torn dirt splash (broad base, bent tip) .. 1 chip (a diamond, pointed both ends), y min length (m),
//              z jag 0..1 (two asymmetric tears on a splash, one notch on a chip), w unused
Shader "TW/Cel/BiThorn"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+11" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelBiThorn"
            Tags { "LightMode"="UniversalForward" }
            ZWrite On ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv : TEXCOORD0;                      // x 0 base .. 1 tip, y -1..1 across
                nointerpolation float4 tone : TEXCOORD1;    // x palette, y hdr, z cut threshold, w ink px
                nointerpolation float4 shape : TEXCOORD2;   // x shape, y jag, z seed, w unused
                float fog : TEXCOORD3;
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                float width = CelCurve(r.size.xyz, k) * alive;
                float len = max(speed * r.size.w, max(r.extra.y, width));
                len = min(len, distance(p, r.pos) + width * 0.5);            // the base stays where it was born
                len *= alive;
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float along = lerp(-len, 0.0, v.uv.x);
                float3 w = p + dir * along + side * (v.uv.y * 2.0 - 1.0) * width * 0.5;
                Varyings o;
                o.positionCS = TransformWorldToHClip(w);
                o.positionWS = w;
                o.uv = float2(v.uv.x, v.uv.y * 2.0 - 1.0);
                o.tone = float4(r.tone.x, r.tone.y, CelCurve(r.erode.xyz, k), r.tone.w);
                o.shape = float4(saturate(r.extra.x), saturate(r.extra.z), r.seed, 0.0);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float a = i.uv.x;
                float sideSign = i.shape.z > 0.5 ? 1.0 : -1.0;
                float across = i.uv.y;
                float halfW;
                if (i.shape.x < 0.5)
                {
                    // Broad torn sheets of dirt, rather than perfect needle triangles. The coherent bend and
                    // two unequal edge cuts are fixed per particle, so they never shimmer during the flight.
                    float seedA = frac(i.shape.z * 17.371 + 0.219);
                    float seedB = frac(i.shape.z * 43.113 + 0.617);
                    float bend = sideSign * (0.13 + 0.07 * seedA) * sin(a * 3.14159265) * i.shape.y;
                    across -= bend;
                    halfW = pow(saturate(1.0 - a), 0.60);
                    // Keep a wide grounded foot, a slightly lopsided shoulder, and a tapered bent tip.
                    halfW *= 1.0 - i.shape.y * (0.10 + 0.09 * step(0.0, -across * sideSign))
                        * sin(a * 3.14159265);
                    float tearA = saturate(1.0 - abs(a - (0.29 + 0.13 * seedA)) / (0.09 + 0.04 * seedB));
                    float tearB = saturate(1.0 - abs(a - (0.66 + 0.10 * seedB)) / (0.07 + 0.03 * seedA));
                    float edgeA = step(0.0, across * sideSign);
                    halfW *= 1.0 - i.shape.y * (tearA * edgeA * 0.60 + tearB * (1.0 - edgeA) * 0.70);
                }
                else
                {
                    // Keep the ballistic chip's existing diamond and single angular notch unchanged.
                    halfW = 1.0 - abs(a * 2.0 - 1.0);
                    float notch = i.shape.y * saturate(1.0 - abs(a - 0.4) * 5.0) * step(0.0, i.uv.y * sideSign);
                    halfW *= 1.0 - notch * 0.6;
                }
                float field = saturate(1.0 - abs(across) / max(halfW, 1e-3));
                float edge = max(i.tone.z, 0.02);
                clip(field - edge);
                // lighter along the spine and toward the base (lit from above), cel-banded
                half3 col = CelRamp(saturate(field * 0.75 + (1.0 - a) * 0.25), CelRowA(i.tone.x), i.tone.y);
                float ink = CelInk(field, edge, CelLodInk(i.tone.w));
                col = lerp(col, CelPalInk(CelRowA(i.tone.x)).rgb, ink);
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.fog), 1.0);
            }
            ENDHLSL
        }
    }
}
