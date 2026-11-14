// Phase: CelFx kit (2026-11-09) - Lobe: a noise-displaced icosphere, cut out (opaque, alpha-tested, so a smoke wall
// costs no overdraw), banded with the camera as the light, an ink rim of N screen pixels (an inverted hull) and ink on
// the edges its erosion opens. Fireballs, puffs, smoke walls and columns are many lobes.
//   size.xyz   radius (m) at birth, at death, ease          size.w  vertical squash (0 or 1: round; 0.6 flattened)
//   motion     gravity, drag, spin (rad/s: the noise rolls over the lobe, it boils), buoyancy
//   erode.xyz  the erosion threshold at birth, at death, ease (0 whole .. 1 gone)   erode.w feather (an ink band that
//              wide in field units on the eroded edge, on top of the ink px)
//   tone       x palette (+ 256 x the palette it cools to), y HDR core, z unused for ordinary lobes, w ink px
//   extra      x lump frequency (0: 1.3), y lump amplitude (0: 0.14), z light up-bias (0: the eye), w when the cooling
//              starts (life 0..1). Negative w is reserved for prepared FlameSmoke records: -w is time scale,
//              tone.z is emitter start plus phase delay, extra.z is impact height (world y), and light up-bias is -0.8.
//              With negative w only, negative y opts into fire-only opaque cutouts: abs(y) remains lump amplitude.
//              Cold/fuel-depleted surface is discarded; it never leaves a smoke body or ink rim.
//              Body transitions warm -> dark cool -> lighter cool;
//              ink follows the warm/cool mix without the body's temporary cool-brightness reduction.
Shader "TW/Cel/Lobe"
{
    Properties
    {
        [HideInInspector] _SrcBlend ("Source blend", Float) = 1
        [HideInInspector] _DstBlend ("Destination blend", Float) = 0
        [HideInInspector] _ZWrite ("Depth write", Float) = 1
    }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+10" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelImpactFire.hlsl"

        struct LobeOut
        {
            float3 positionWS; float3 normalWS; float3 field; float k; float radius;
        };

        float3 LobeShape(float3 n, CelRecord r, float t, float freq, float amp)
        {
            // the noise domain turns about the record's axis: the lumps roll over the surface (it boils), the lobe stays put
            float3 axis = dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0);
            float3 q = CelRotate(n, axis, r.motion.z * t);
            if (r.extra.w < 0.0)
            {
                // Flamethrower smoke: rolling unequal billows, not a smooth coin face. Ordinary lobes are unchanged.
                float smokeTime = t / max(-r.extra.w, 1e-3);
                float3 flow = float3(0.13 * sin(smokeTime * 0.5 + r.seed * 7.0), -smokeTime * 0.16, 0.0);
                float broad = CelSimplex3(q * freq + r.seed * 31.7 + flow);
                float folds = CelSimplex3(q * (freq * 2.15) + r.seed * 19.3 + flow * 0.65);
                // Two broad unequal shoulders make a compound billow without high-frequency surface noise.
                float3 qb = CelRotate(q, float3(0, 1, 0), r.seed * 6.2831853);
                float capA = saturate(dot(qb, normalize(float3(0.88, 0.18, 0.44))));
                float capB = saturate(dot(qb, normalize(float3(-0.76, -0.12, 0.64))));
                float neck = saturate(qb.z);
                capA *= capA; capA *= capA;
                capB *= capB; capB *= capB;
                neck *= neck; neck *= neck;
                float displacement = amp * (0.92 * broad + 0.08 * folds)
                    + 0.14 * capA + 0.10 * capB - 0.06 * neck;
                float3 shaped = n * (1.0 + displacement);
                // Fade displacement at the vertical poles: the authored plume height is retained.
                shaped.y = n.y * (1.0 + displacement * (1.0 - n.y * n.y));
                return shaped;
            }
            float lump = CelSimplex3(q * freq + r.seed * 31.7);
            return n * (1.0 + amp * lump);
        }

        void LobeParams(CelRecord r, out float freq, out float amp, out float squash)
        {
            freq = r.extra.x > 0.0 ? r.extra.x : 1.3;
            float lumpAmplitude = r.extra.w < 0.0 && r.extra.y < 0.0 ? abs(r.extra.y) : r.extra.y;
            amp = lumpAmplitude > 0.0 ? lumpAmplitude : 0.14;
            squash = r.size.w > 0.0 ? r.size.w : 1.0;
        }

        /// The world normal of the displaced, squashed surface over the sphere direction n, from two neighbours.
        float3 LobeNormal(float3 n, float3 s, CelRecord r, float t, float freq, float amp, float squash)
        {
            float3 t1 = normalize(cross(n, abs(n.y) < 0.9 ? float3(0, 1, 0) : float3(1, 0, 0)));
            float3 t2 = cross(n, t1);
            const float e = 0.06;
            float3 a = LobeShape(normalize(n + t1 * e), r, t, freq, amp), b = LobeShape(normalize(n + t2 * e), r, t, freq, amp);
            float3 nrm = normalize(cross(a - s, b - s));
            if (dot(nrm, n) < 0.0) nrm = -nrm;
            return normalize(nrm / float3(1.0, squash, 1.0));
        }

        LobeOut Lobe(float3 positionOS, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float freq, amp, squash; LobeParams(r, freq, amp, squash);
            float radius = CelCurve(r.size.xyz, k) * alive;
            float3 n = normalize(positionOS);
            float3 s = LobeShape(n, r, t, freq, amp);
            float3 sq = float3(1.0, squash, 1.0);
            float3 center = CelPosition(r, t);
            LobeOut o;
            o.positionWS = center + s * sq * radius;
            o.normalWS = LobeNormal(n, s, r, t, freq, amp, squash);
            // the erosion field is read where the eye ray crosses the lobe's middle plane, so a hole goes through
            // the front and the back together and the background shows through it
            float3 view = normalize(_WorldSpaceCameraPos - center);
            o.field = (s - view * dot(s, view)) + r.seed * 13.0;
            o.k = k;
            o.radius = radius;
            return o;
        }

        /// The lobe's erosion field: noise, biased so the rim goes before the middle. 0..1, 1 = most solid.
        float LobeField(float3 field, float seed, float k, float smokeMode)
        {
            float3 q = field - seed * 13.0;
            float rim = 1.0 - saturate(length(q));
            if (smokeMode < 0.0)
            {
                // Smoke loses broad pieces instead of fine crumbs; ordinary lobes retain their field.
                float3 p = field * 1.3 + k * 0.6;
                float noise = saturate(0.5 + 0.5 * (CelSimplex3(p) * 0.88
                    + CelSimplex3(p * 2.03 + 17.1) * 0.12));
                return saturate(noise * 0.62 + rim * 0.5);
            }
            return saturate(CelFbm3(field * 1.7 + k * 0.6) * 0.62 + rim * 0.5);
        }
        // Shared by the body and ink hull. Ordinary lobes retain their per-particle cooling curve.
        float2 LobePalettePhase(CelRecord r, float k)
        {
            float coolFrom = r.extra.w > 0.0 ? r.extra.w : 0.35;
            if (r.extra.w >= 0.0) return float2(smoothstep(coolFrom, 1.0, k), 1.0);
            float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
            return float2(smoothstep(3.9, 5.0, elapsed), lerp(0.85, 1.0, smoothstep(5.2, 5.75, elapsed)));
        }

        half3 LobeBodyPalette(float lit, float packedRow, float hdr, float2 phase)
        {
            uint a = CelRowA(packedRow), b = CelRowB(packedRow);
            half3 warm = CelRamp(lit, a, hdr);
            if (b == a || b == 0u) return warm;
            return lerp(warm, CelRamp(lit, b, hdr) * phase.y, phase.x);
        }

        half3 LobeInkPalette(float packedRow, float mix)
        {
            uint a = CelRowA(packedRow), b = CelRowB(packedRow);
            half3 warm = CelPalInk(a);
            return b != a && b != 0u ? lerp(warm, CelPalInk(b), mix) : warm;
        }

        float LobeSmokeOpacity(CelRecord r)
        {
            if (r.extra.w >= 0.0) return 1.0;
            float elapsed = max(0.0, _CelNow - r.tone.z) / max(-r.extra.w, 1e-3);
            return 1.0 - smoothstep(5.30, 6.50, elapsed);
        }

        ENDHLSL

        Pass
        {
            Name "CelLobe"
            Tags { "LightMode"="UniversalForward" }
            Blend [_SrcBlend] [_DstBlend]
            Cull Back ZWrite [_ZWrite] ZTest LEqual
            Stencil { Ref 4 WriteMask 4 Comp Always Pass Replace Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog

            struct Attributes { float4 positionOS : POSITION; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                nointerpolation float2 palettePhase : TEXCOORD1; // warm/cool mix, cool brightness
                float3 field : TEXCOORD2;
                nointerpolation float4 tone : TEXCOORD3;        // x palette, y hdr, z erosion threshold, w ink px
                nointerpolation float4 more : TEXCOORD4;        // x life 0..1, y feather, z up-bias, w cool from
                float2 misc : TEXCOORD5;        // x seed, y fog
                float3 sphere : TEXCOORD6;      // the undisplaced direction: the normal is worked out again per pixel
                nointerpolation uint id : TEXCOORD7;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                Varyings o;
                o.positionWS = l.positionWS;
                o.positionCS = TransformWorldToHClip(l.positionWS);
                o.palettePhase = LobePalettePhase(r, l.k);
                o.field = l.field;
                o.tone = float4(r.tone.x, r.tone.y, CelCurve(r.erode.xyz, l.k), r.tone.w);
                o.more = float4(l.k, r.erode.w, r.extra.w < 0.0 ? -0.8 : r.extra.z, r.extra.w > 0.0 ? r.extra.w : 0.35);
                o.misc = float2(r.seed, ComputeFogFactor(o.positionCS.z));
                o.sphere = normalize(v.positionOS.xyz);
                o.id = GetIndirectInstanceID_Base(instanceID);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                CelRecord r = _CelRecords[i.id];
                float opacity = LobeSmokeOpacity(r);
                clip(opacity - 0.0001);
                float f = LobeField(i.field, i.misc.x, i.more.x, r.extra.w);
                float e = i.tone.z;
                clip(f - e);
                float3 viewWS = normalize(_WorldSpaceCameraPos - i.positionWS);
                // per pixel, so the bands follow the lumps and not the triangles of the mesh
                float freq, amp, squash; LobeParams(r, freq, amp, squash);
                float t = CelAge(r);
                float3 n = normalize(i.sphere);
                float3 nrm = LobeNormal(n, LobeShape(n, r, t, freq, amp), r, t, freq, amp, squash);
                bool flameSmoke = r.extra.w < 0.0;
                if (flameSmoke) nrm = normalize(lerp(nrm, normalize(n / float3(1.0, squash, 1.0)), 0.45));
                float lit = CelLobeLight(nrm, viewWS, i.more.z);
                // Warm billows retain broad middle bands; cooled smoke has limited slate highlights.
                if (flameSmoke) lit = pow(saturate(lit), lerp(1.65, 2.8, i.palettePhase.x));
                else lit *= lit;
                if (flameSmoke) lit = saturate(0.18 + 0.74 * lit + 0.025 * (f - 0.5));
                // the eroded edge darkens a band before the ink: the lobe thins toward its holes
                if (e > 0.001) lit *= lerp(flameSmoke ? 0.85 : 0.55, 1.0, saturate((f - e) / max(i.more.y, 1e-3)));
                half3 col = LobeBodyPalette(lit, i.tone.x, i.tone.y, i.palettePhase);
                float ink = e > 0.001 ? CelInk(f, e, CelLodInk(i.tone.w * (flameSmoke ? 0.28 : 0.8))) : 0.0;
                if (flameSmoke)
                {
                    // The transparent material omits the inverted ink hull; its own rim fades with the body.
                    float facing = saturate(dot(nrm, viewWS));
                    float rim = 1.0 - smoothstep(0.12, 0.32, facing);
                    ink = max(ink, rim);
                }
                col = lerp(col, LobeInkPalette(i.tone.x, i.palettePhase.x), ink);
                if (flameSmoke)
                {
                    // Keep the existing emitter-local world heat and fuel. Only
                    // its upper surface support becomes a bounded wrapping band.
                    float scale = max(-r.extra.w, 1e-3);
                    float elapsed = max(0.0, _CelNow - r.tone.z) / scale;
                    float age = t / scale;
                    float4 fire = CelImpactFire(i.positionWS, r.extra.z, elapsed, age, float2(1.35, 3.15));
                    float height = i.positionWS.y - r.extra.z;
                    // Below +0.60 m retain the filled R48 body. Between +0.60
                    // and +1.50 m, broad islands turn into shared contour bands.
                    float wrap = smoothstep(0.60, 1.50, height);
                    float upperLimit = 0.35 - 0.50 * fire.w;
                    float band = min(fire.x + 0.02, upperLimit - fire.x);
                    float skin = lerp(fire.x, band, wrap);
                    float facing = saturate(dot(nrm, viewWS));
                    float edge = min(0.18 * (1.0 - facing) - 0.12, skin + 0.06);
                    skin = max(skin, edge);
                    float coverage = CelAAStep(0.0, skin) * fire.y * fire.z;
                    // The upper heat peaks inside the band rather than at the
                    // noise maximum that used to fill a whole billow yellow.
                    // Both band boundaries contract during cooling; a dark
                    // center cannot relight merely because the raw field fell.
                    float bandCentre = 0.5 * (upperLimit - 0.02);
                    float bandHalfWidth = max(0.5 * (upperLimit + 0.02), 0.02);
                    float innerHeat = saturate(1.0 - abs(fire.x - bandCentre) / bandHalfWidth);
                    float colorSkin = lerp(skin, lerp(0.08, 0.34, innerHeat), wrap);
                    half3 flame = CelImpactFireColor(colorSkin, fire.w);
                    if (r.extra.y < 0.0)
                    {
                        // Fire-only support in the existing ZWrite-on pool: fuel
                        // contracts opaque hot cutouts instead of fading a solid
                        // sphere that would still mask transparent smoke behind it.
                        clip(CelImpactFireCutout(skin, fire, 0.35));
                        col = flame;
                        opacity = 1.0;
                    }
                    else col = lerp(col, flame, coverage);
                }
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.misc.y), opacity);
            }
            ENDHLSL
        }

        Pass
        {
            Name "CelLobeInk"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Cull Front ZWrite On ZTest LEqual
            Stencil { Ref 4 WriteMask 4 Comp Always Pass Replace Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma vertex vertInk
            #pragma fragment fragInk
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; };
            struct InkVaryings { float4 positionCS : SV_POSITION; float3 positionWS : TEXCOORD0; float3 field : TEXCOORD1; nointerpolation float4 tone : TEXCOORD2; float4 misc : TEXCOORD3; };
            InkVaryings vertInk(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                float4 cs = TransformWorldToHClip(l.positionWS);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, l.normalWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = CelLodInk(r.tone.w) * (l.radius > 0.0 ? 1.0 : 0.0);
                cs.xy += dir * px * 2.0 / _ScreenParams.xy * cs.w;
                InkVaryings o;
                o.positionCS = cs;
                o.positionWS = l.positionWS;
                o.field = l.field;
                o.tone = float4(r.tone.x, CelCurve(r.erode.xyz, l.k), l.k, LobePalettePhase(r, l.k).x);
                o.misc = float4(r.seed, ComputeFogFactor(cs.z), r.extra.w, 0.0);
                return o;
            }
            half4 fragInk(InkVaryings i) : SV_Target
            {
                clip(LobeField(i.field, i.misc.x, i.tone.z, i.misc.z) - i.tone.y);
                half3 c = LobeInkPalette(i.tone.x, i.tone.w);
                return half4(MixFog(ApplyFieldFog(c, i.positionWS), i.misc.y), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Back
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            struct Attributes { float4 positionOS : POSITION; };
            struct DepthVaryings { float4 positionCS : SV_POSITION; float3 field : TEXCOORD0; float4 cut : TEXCOORD1; };
            DepthVaryings vertDepth(Attributes v, uint instanceID : SV_InstanceID)
            {
                LobeOut l = Lobe(v.positionOS.xyz, instanceID);
                CelRecord r = _CelRecords[GetIndirectInstanceID_Base(instanceID)];
                DepthVaryings o;
                o.positionCS = TransformWorldToHClip(l.positionWS);
                o.field = l.field;
                o.cut = float4(r.seed, CelCurve(r.erode.xyz, l.k), l.k, r.extra.w);
                return o;
            }
            half4 fragDepth(DepthVaryings i) : SV_Target
            {
                clip(LobeField(i.field, i.cut.x, i.cut.z, i.cut.w) - i.cut.y);
                return 0;
            }
            ENDHLSL
        }
    }
}
