// Phase: CelFx job (2026-11-10, lane/show/cel-explosion, round e01) - ExpPuff: a cel smoke puff whose silhouette is a
// cluster of sub-lobes that churns over its life, built as a drop-in for a kit Lobe layer.
//
// Why: four blind critics all called the smoke wall "a row of outlined boulders": a kit Lobe is a noise-displaced
// sphere whose lumps are fixed at birth, so one record reads as ONE round rock with a closed ink loop round it, and
// the bank never churns (measured: the bank's top edge did not move over f100-f144). The reference is ~20-25 small
// soft puffs per bank that roll, lift and break up. More kit Lobes is not available: the Lobe pool is full at Close
// (979/998). So one record here draws EIGHT gaussian sub-balls summed into one metaball field: they start packed, then
// spread, curl round the middle and rise relative to each other, so the SAME record count reads as ~8x the puff count
// at a third of the radius, and the silhouette changes shape every frame instead of being frozen at birth.
//
// Opaque and alpha-tested like the kit Lobe (ZWrite + DepthOnly, so a smoke wall costs no overdraw), banded with the
// same CelLobeLight / CelRamp2 as the Lobe (the field's own analytic gradient stands in for the surface normal), and
// inked with CelInk on the iso edge - a real screen-pixel line that breaks where the puff is lit and where the
// boiling noise cuts it, instead of the Lobe's inverted-hull loop round every rock.
//
// Every record field keeps its kit Lobe meaning, so a Lobe layer becomes an ExpPuff layer by changing Kind alone:
//   size.xyz   radius (m) at birth, at death, ease          size.w  sub-ball spread at death, in radii (0: 0.55)
//   motion     gravity, drag, spin (rad/s: the sub-balls curl round the middle - the puff churns), buoyancy
//   erode.xyz  the erosion threshold over the life (0 whole .. 1 gone)  erode.w feather (the band before a hole)
//   tone       x palette (+ 256 x the palette it cools to), y HDR core, z unused, w ink px (0: no ink)
//   extra      x boil frequency (0: 1.3), y iso noise amount (0: 0.16), z light up-bias (0: the eye), w cool from
// Kept for a build by Resources/ShaderKeep/KeepCelExpPuff.mat. Written to lift into the kit unchanged.
Shader "TW/Cel/ExpPuff"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+10" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        #define EXP_PUFF_BALLS 8
        // a sub-ball's own contour radius, in units of its gaussian radius
        #define EXP_PUFF_RING 0.92

        struct PuffV2F
        {
            float4 positionCS : SV_POSITION;
            float2 local : TEXCOORD0;                      // the quad in radii: the nominal puff edge is at |q| = 1
            nointerpolation float4 life : TEXCOORD1;       // x life 0..1, y spread now, z curl rad/s, w age s
            nointerpolation float4 shape : TEXCOORD2;      // x seed, y iso now, z boil freq, w iso noise amount
            nointerpolation float4 tone : TEXCOORD3;       // x palette, y hdr, z ink px, w cool from
            nointerpolation float2 more : TEXCOORD4;       // x feather, y light up-bias
            float3 positionWS : TEXCOORD5;
            float fog : TEXCOORD6;
        };

        PuffV2F PuffVert(float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float radius = CelCurve(r.size.xyz, k) * alive;
            float spreadMax = r.size.w > 0.0 ? r.size.w : 0.55;
            // the quad must hold the spread cluster plus the boiling noise, so it is wider than the nominal radius
            const float reach = 1.6;
            float2 c = uv * 2.0 - 1.0;
            float3 p = CelPosition(r, t);
            float3 w = CelBillboard(p, c * radius * reach);
            PuffV2F o;
            o.positionWS = w;
            o.positionCS = TransformWorldToHClip(w);
            o.local = c * reach;
            o.life = float4(k, lerp(0.38, spreadMax, smoothstep(0.0, 1.0, k)), r.motion.z, t);
            o.shape = float4(r.seed, CelCurve(r.erode.xyz, k),
                             r.extra.x > 0.0 ? r.extra.x : 1.3, r.extra.y > 0.0 ? r.extra.y : 0.16);
            o.tone = float4(r.tone.x, r.tone.y, CelLodInk(r.tone.w * 0.8), r.extra.w > 0.0 ? r.extra.w : 0.35);
            o.more = float2(max(r.erode.w, 1e-3), r.extra.z);
            o.fog = ComputeFogFactor(o.positionCS.z);
            return o;
        }

        /// The metaball field of the record's sub-balls at q (radii), and its gradient. The balls start packed round
        /// the middle, then drift apart, turn about it and climb, each one shrinking a little, so the union opens into
        /// separate billows over the life.
        float PuffField(float2 q, float seed, float k, float spread, float curl, float t, out float2 grad, out float ring)
        {
            float F = 0.0; float2 G = 0.0;
            float near = 1e3, front = -1.0;
            [unroll] for (int j = 0; j < EXP_PUFF_BALLS; j++)
            {
                float h1 = CelHash21(float2(j, seed * 71.0));
                float h2 = CelHash21(float2(j + 11.0, seed * 37.0));
                float h3 = CelHash21(float2(j + 27.0, seed * 13.0));
                float h4 = CelHash21(float2(j + 41.0, seed * 53.0));   // depth order, uncorrelated with the place
                float2 o0 = float2((h1 - 0.5) * 1.9, (h2 - 0.5) * 1.05) * spread;  // wider than tall: a billow
                float ang = (h3 - 0.5) * 2.0 * curl * t;
                float sa, ca; sincos(ang, sa, ca);
                float2 cj = float2(o0.x * ca - o0.y * sa, o0.x * sa + o0.y * ca);
                cj.y += 0.30 * spread * k * (0.35 + h2);                           // the cluster lifts within itself
                float rj = (0.40 + 0.20 * h3) * lerp(1.0, 0.95, k);
                float2 dv = q - cj;
                float e = exp(-dot(dv, dv) / (rj * rj));
                F += e;
                G += e * (-2.0 * dv / (rj * rj));
                // e02: a sub-ball's own iso contour is exactly a circle of this radius, so it can be drawn as a
                // line INSIDE the union. But drawing every circle gives a wire frame (try3): in the reference a
                // puff HIDES the ones behind it, and only the near arc of each circle is drawn. So keep the
                // circle of the FRONTMOST sub-ball covering this pixel (the seed orders them in depth) and
                // report how far inside its own rim the pixel is; everything behind it draws nothing.
                float inside = rj * EXP_PUFF_RING - length(dv);
                if (inside > 0.0 && h4 > front) { front = h4; near = inside; }
            }
            grad = G;
            ring = near;
            return F;
        }

        /// The iso-surface distance: positive inside. The edge is displaced by two octaves of noise that travel over
        /// the puff, so the outline boils and frays instead of standing still.
        float PuffSdf(PuffV2F i, out float2 grad, out float ring)
        {
            float2 q = i.local;
            float F = PuffField(q, i.shape.x, i.life.x, i.life.y, i.life.z, i.life.w, grad, ring);
            float f = i.shape.z;
            float nz = CelValue2(q * f * 1.8 + float2(i.shape.x * 17.0, -i.life.w * 0.6))
                     + 0.5 * CelValue2(q * f * 4.0 + float2(-i.life.w * 1.0, i.shape.x * 29.0));
            // the inner contours are left as clean circles. Displacing them by the boil noise as well (try2) makes
            // the line cross itself several times inside one wavelength and it comes out as hair, not as puffs.
            return F - 0.30 + (nz - 0.75) * (i.shape.w * 2.2);
        }

        /// The erosion field, the kit Lobe's rule in two dimensions: noise biased so the rim opens before the middle,
        /// 0..1 with 1 the most solid. The layer's Erode curve is a threshold on THIS, so a puff tears into pieces
        /// late exactly as a Lobe does, instead of the whole shape shrinking (which costs the bank its mass).
        float PuffErodeField(float2 q, float seed, float k)
        {
            float rim = 1.0 - saturate(length(q));
            float n = CelValue2(q * 1.7 + float2(seed * 13.0, k * 0.6))
                    + 0.5 * CelValue2(q * 3.4 + float2(-k * 0.4, seed * 7.0));
            return saturate(n * 0.42 + rim * 0.5);
        }
        ENDHLSL

        Pass
        {
            Name "CelExpPuff"
            Tags { "LightMode"="UniversalForward" }
            Cull Off ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            PuffV2F vert(Attributes v, uint instanceID : SV_InstanceID) { return PuffVert(v.uv, instanceID); }

            half4 frag(PuffV2F i) : SV_Target
            {
                float2 g; float ring;
                float sdf = PuffSdf(i, g, ring);
                clip(sdf);
                float ef = PuffErodeField(i.local, i.shape.x, i.life.x);
                clip(ef - i.shape.y);
                // a sphere-like normal from the field: the first-order distance to the iso edge (sdf over the
                // gradient's length) is read as the depth into a sub-lobe of about this radius, so EVERY sub-lobe
                // gets its own round n.v falloff - a bank of small shaded puffs, not one flat lit slab. Raw sdf
                // cannot be used: a metaball's field runs to 3 or 4 in the middle, which saturates the whole puff
                // into the lightest band and lightens the palette.
                float gl = max(length(g), 1e-4);
                float2 n2 = -g / gl;
                float depth = saturate((sdf / gl) / 0.40);
                float2 nxy = n2 * sqrt(saturate(1.0 - depth * depth));
                float3 right = UNITY_MATRIX_V[0].xyz, up = UNITY_MATRIX_V[1].xyz;
                float3 toEye = normalize(_WorldSpaceCameraPos - i.positionWS);
                float3 nrm = normalize(right * nxy.x + up * nxy.y + toEye * depth);
                float lit = CelLobeLight(nrm, toEye, i.more.y);
                lit *= lit;
                // the puff thins toward a hole its erosion opened, as the Lobe's feather band does
                lit *= lerp(0.55, 1.0, saturate((ef - i.shape.y) / i.more.x));
                half3 col = CelRamp2(lit, i.tone.x, i.tone.y, i.life.x, i.tone.w);
                // the ink is a real px line on the iso edge, dropped out where the puff faces the light: a broken
                // contour of varying weight, not the closed loop an inverted hull draws round every lobe
                // the outer contour, and the edges the erosion opens: dropped out where the puff faces the light,
                // so it is a broken line of varying weight, not the closed loop an inverted hull draws
                float ink = max(CelInk(sdf, 0.0, i.tone.z),
                                i.shape.y > 0.001 ? CelInk(ef, i.shape.y, i.tone.z) : 0.0)
                          * lerp(1.0, 0.3, saturate(lit * 1.4));
                // e02: the line round every sub-puff INSIDE the mass. The reference's bank is a pile of small
                // circles, each with its own thin contour; a union silhouette alone can never read that way,
                // however soft it is made (that was e01's dead end). These lines live in the lit interior, so
                // they must NOT take the lit-side dropout the silhouette takes, or they vanish.
                float inner = CelInk(ring, 0.0, i.tone.z * 0.6);   // ring >= 0, so this is 1 on the contour
                ink = max(ink, inner * 0.6);
                col = lerp(col, CelInkColor(i.tone.x, i.life.x, i.tone.w), ink);
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.fog), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            PuffV2F vert(Attributes v, uint instanceID : SV_InstanceID) { return PuffVert(v.uv, instanceID); }
            half4 frag(PuffV2F i) : SV_Target
            {
                float2 g; float ring;
                clip(PuffSdf(i, g, ring));
                clip(PuffErodeField(i.local, i.shape.x, i.life.x) - i.shape.y);
                return 0;
            }
            ENDHLSL
        }
    }
}
