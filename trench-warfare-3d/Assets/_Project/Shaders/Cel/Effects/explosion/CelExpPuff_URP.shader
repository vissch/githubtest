// Proposed smoke decomposition (2026-11-14): one irregular dominant lobe and two shoulders per record.
// A smooth union connects the silhouette; partial contact shadows describe billows without radial dark rims.
// The full palette supplies pale tan body, soft mids, and sparse ink. No sphere-normal band rings.
// Premultiplied alpha lets the late cloud become haze without ordered-dither stipple. No depth is written: a
// disappearing puff cannot leave an invisible blocker. The tradeoff is blended overdraw within this one pool.
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
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+10" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        #define EXP_PUFF_BALLS 3

        struct PuffV2F
        {
            float4 positionCS : SV_POSITION;
            float2 local : TEXCOORD0;                      // the quad in radii: the nominal puff edge is at |q| = 1
            nointerpolation float4 life : TEXCOORD1;       // x life 0..1, y spread now, z curl rad/s, w age s
            nointerpolation float4 shape : TEXCOORD2;      // x seed, y iso now, z boil freq, w iso noise amount
            nointerpolation float4 tone : TEXCOORD3;       // x palette, y hdr, z ink px, w cool from
            nointerpolation float4 more : TEXCOORD4;       // feather, light up-bias, ground plane, draw pull
            float3 positionWS : TEXCOORD5;
            float fog : TEXCOORD6;
            // Only opted-in Main .81 consumes this; data support alone never changes color or alpha.
            nointerpolation float4 bankFrame : TEXCOORD7; // common Play target xyz, nominal bank birth
        };

        PuffV2F PuffVert(float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float radius = CelCurve(r.size.xyz, k) * alive;
            bool farSoot = r.extra.z > 0.635 && r.extra.z < 0.645;
            if (farSoot) radius = lerp(r.size.x, r.size.y, smoothstep(0.05, 0.25, t)) * alive;
            float spreadMax = r.size.w > 0.0 ? r.size.w : 0.55;
            // the quad must hold the spread cluster plus the boiling noise, so it is wider than the nominal radius
            const float reach = 1.6;
            float2 c = uv * 2.0 - 1.0;
            float3 p = CelPosition(r, t);
            if (farSoot) p.y += -0.12 * (1.0 - smoothstep(0.05, 0.25, t))
                + 0.55 * smoothstep(0.35, 0.85, t);
            // Far-site tag1.6: newborn gray wraps the fire base, then older large lobes rise clear of the ground.
            bool liftedSite = r.extra.z > 1.55 && r.extra.z < 1.65;
            if (liftedSite) p.y += 0.65 * smoothstep(0.08, 0.45, t);
            float3 w = CelBillboard(p, c * radius * reach);
            PuffV2F o;
            o.positionWS = w;
            float pull = r.extra.w < 0.0 ? -r.extra.w : 1.0;
            o.positionCS = TransformWorldToHClip(_WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * pull);
            o.local = c * reach;
            o.life = float4(k, lerp(0.65, max(spreadMax, 0.85), smoothstep(0.0, 1.0, k)), r.motion.z, t);
            o.shape = float4(r.seed, CelCurve(r.erode.xyz, k),
                             r.extra.x > 0.0 ? r.extra.x : 1.3, r.extra.y > 0.0 ? r.extra.y : 0.16);
            o.tone = float4(r.tone.x, r.tone.y, CelLodInk(r.tone.w * 0.72), r.extra.w > 0.0 ? r.extra.w : 0.35);
            o.more = float4(max(r.erode.w, 1e-3), liftedSite ? 0.6 : r.extra.z, r.pos.y - 1.1, pull);
            o.fog = ComputeFogFactor(o.positionCS.z);
            o.bankFrame = float4(r.axis, r.tone.y);
            return o;
        }

        // Main .81 only: five soft contact fields traced in reference96/112/128.
        // R1a+R1b form one connected right channel; R2/R3 are unobserved and inactive at96.
        // These points are metres on the common target's camera plane, not fixed screen/world positions.
        static const float2 MainContactPoints[90] = {
            float2(0.6664745,-0.6666601), float2(0.9352717,-0.6199659), float2(1.2054286,-0.5821436), float2(1.4624433,-0.5012382), float2(1.6907096,-0.3586157), float2(1.8834643,-0.1658179),
            float2(1.0244320,-0.5378768), float2(1.2721522,-0.4435633), float2(1.5245123,-0.3627583), float2(1.7592132,-0.2475873), float2(1.9761634,-0.0955493), float2(2.1983610,0.0488504),
            float2(1.0960206,-0.5092575), float2(1.3332926,-0.4302208), float2(1.5681936,-0.3444498), float2(1.7867787,-0.2248888), float2(1.9897100,-0.0798095), float2(2.1840074,0.0774787),
            float2(1.9264803,-0.2087626), float2(1.9968909,-0.0589221), float2(2.0535947,0.0955131), float2(2.0875673,0.2575681), float2(2.0682675,0.4183021), float2(2.0259228,0.5784647),
            float2(1.9979940,-0.1085762), float2(2.0890560,0.0156777), float2(2.1506509,0.1477988), float2(2.1334615,0.3009537), float2(2.1156891,0.4540420), float2(2.0975113,0.6070840),
            float2(1.9836553,-0.0942613), float2(2.1113034,0.0254268), float2(2.2085810,0.1563123), float2(2.1890578,0.3302612), float2(2.1715186,0.5044144), float2(2.1547313,0.6786451),
            float2(-1.3963617,-0.4517432), float2(-1.3041050,-0.5208415), float2(-1.2118483,-0.5899399), float2(-1.1084312,-0.6323123), float2(-0.9944577,-0.6494053), float2(-0.8804842,-0.6664983),
            float2(-1.3963617,-0.4517432), float2(-1.3041050,-0.5208415), float2(-1.2118483,-0.5899399), float2(-1.1084312,-0.6323123), float2(-0.9944577,-0.6494053), float2(-0.8804842,-0.6664983),
            float2(-1.8262221,-0.3085641), float2(-1.6204252,-0.3542749), float2(-1.4274611,-0.4362095), float2(-1.2514983,-0.5511595), float2(-1.0680582,-0.6488986), float2(-0.8661156,-0.7094401),
            float2(0.5488012,2.2819152), float2(0.5779804,2.2121074), float2(0.6071596,2.1422996), float2(0.6571292,2.0919006), float2(0.7248513,2.0580744), float2(0.7925734,2.0242483),
            float2(0.5488012,2.2819152), float2(0.5779804,2.2121074), float2(0.6071596,2.1422996), float2(0.6571292,2.0919006), float2(0.7248513,2.0580744), float2(0.7925734,2.0242483),
            float2(0.7062720,2.3677792), float2(0.8046204,2.1856065), float2(0.9089311,2.0067751), float2(0.9343081,1.8364617), float2(0.7732676,1.7949539), float2(0.5921765,1.8954485),
            float2(2.0681005,1.3370711), float2(2.1045617,1.2522590), float2(2.1410229,1.1674469), float2(2.1819925,1.0847973), float2(2.2255294,1.0033792), float2(2.2690663,0.9219611),
            float2(2.0249797,1.4802097), float2(2.1201862,1.4024260), float2(2.2016035,1.3136198), float2(2.2566843,1.2037611), float2(2.3174866,1.0971410), float2(2.3835810,0.9935162),
            float2(2.0249498,1.5088366), float2(2.1404279,1.4319479), float2(2.2490551,1.3488234), float2(2.3149360,1.2267909), float2(2.3949328,1.1142972), float2(2.4838319,1.0078191)
        };
        static const float3 MainContactWidths[5] = { float3(0.1288670,0.1431855,0.1575041), float3(0.1431855,0.1431855,0.1575041), float3(0.1002299,0.1002299,0.1288670), float3(0.0859113,0.0859113,0.1145484), float3(0.0859113,0.1002299,0.1145484) };
        static const float3 MainContactVisibility[5] = { float3(0.3000000,1.0000000,1.0000000), float3(0.3000000,1.0000000,1.0000000), float3(0.0000000,0.6500000,1.0000000), float3(0.0000000,0.6500000,1.0000000), float3(0.3000000,1.0000000,1.0000000) };
        float MainContactField(PuffV2F i)
        {
            float age = _CelNow - i.bankFrame.w;
            // Sample one common plane so overlapping cards evaluate the same field at a pixel.
            float3 ray = i.positionWS - _WorldSpaceCameraPos;
            float3 toAnchor = i.bankFrame.xyz - _WorldSpaceCameraPos;
            float rayDepth = dot(ray, UNITY_MATRIX_V[2].xyz);
            float scale = dot(toAnchor, UNITY_MATRIX_V[2].xyz)
                / (sign(rayDepth) * max(abs(rayDepth), 1e-3));
            float3 q3 = ray * scale - toAnchor;
            float2 q = float2(dot(q3, UNITY_MATRIX_V[0].xyz), dot(q3, UNITY_MATRIX_V[1].xyz));
            // Common age96=.25,112=.916667,128=1.583333. Per-record births do not desynchronize contacts.
            float a = smoothstep(0.25, 0.9166667, age);
            float b = smoothstep(0.9166667, 1.5833333, age);
            float field = 0.0;
            [unroll] for (int f = 0; f < 5; f++)
            {
                float d = 1e3;
                [unroll] for (int j = 0; j < 5; j++)
                {
                    int n = f * 18 + j;
                    float2 pa = lerp(lerp(MainContactPoints[n], MainContactPoints[n+6], a), MainContactPoints[n+12], b);
                    float2 pb = lerp(lerp(MainContactPoints[n+1], MainContactPoints[n+7], a), MainContactPoints[n+13], b);
                    float2 ab = pb - pa;
                    float along = saturate(dot(q-pa, ab) / max(dot(ab,ab), 1e-6));
                    d = min(d, length(q-pa-ab*along));
                }
                float3 w = MainContactWidths[f], v = MainContactVisibility[f];
                float width = lerp(lerp(w.x,w.y,a),w.z,b);
                float visible = lerp(lerp(v.x,v.y,a),v.z,b);
                // A broad soft trough, never a radial ring or a hard stamp.
                float contact = (1.0-smoothstep(width*0.35,width*1.35,d))*visible;
                field = max(field,contact); // overlaps do not double-darken the branching junction
            }
            // Preserve the early fire handoff and late dim-haze tail, outside the measured window.
            return field * smoothstep(0.12,0.25,age) * (1.0-smoothstep(1.70,2.08,age));
        }

        // A hierarchical, irregular drawn lobe with two shoulders. Equal spheres in a ring produced
        // hundreds of beads; a dominant lobe gives the bank larger connected masses with small accents.
        // Shading touches only the shadow-side contact arc; no complete shaded rim around each lobe.
        float PuffField(float2 q, float seed, float k, float spread, float curl, float t, float contactWidth, float foldStrength, float roundMain,
            out float crescent, out float contour, out float arcSide, out float variation)
        {
            float silhouette = -1e3, front = -1e3;
            crescent = 1.0; contour = 1e3; arcSide = 0.0; variation = 1.0;
            float2 warp = float2(
                CelValue2(q * 2.7 + float2(seed * 17.0, -t * 0.14)),
                CelValue2(q * 2.2 + float2(t * 0.12, seed * 31.0))) - 0.5;
            q += warp * 0.095;
            [unroll] for (int j = 0; j < EXP_PUFF_BALLS; j++)
            {
                float h1 = CelHash21(float2(j + 1.0, seed * 71.0));
                float h2 = CelHash21(float2(j + 11.0, seed * 37.0));
                float h3 = CelHash21(float2(j + 27.0, seed * 13.0));
                float theta = seed * 6.2831853 + j * 2.45;
                float reach = j == 0 ? 0.08 : lerp(0.50, 0.73, h1);
                float2 centre = float2(cos(theta), sin(theta) * 0.82) * reach;
                centre *= lerp(0.85, 1.12, k);
                centre += float2(sin(t * curl * 0.25 + h1 * 6.0), cos(t * curl * 0.2 + h2 * 6.0)) * 0.035;
                float radius = j == 0 ? lerp(0.70, 0.88, h1) : lerp(0.33, 0.52, h3);
                // Main reference caps are rounder than the pointed stretched folds in the other banks.
                float aspect = lerp(lerp(0.78, 1.23, h2), lerp(0.90, 1.11, h2), roundMain);
                float angle = (h3 - 0.5) * 2.1 + curl * t * 0.07;
                float sa, ca; sincos(angle, sa, ca);
                float2 v = q - centre;
                float2 dv = float2(v.x * ca - v.y * sa, v.x * sa + v.y * ca);
                dv *= float2(aspect, 1.0 / aspect);
                // Low harmonics deform the contour at a scale that reads as a hand-drawn billow,
                // not granular noise. The silhouette remains coherent while the rim slowly rolls.
                float phi = atan2(dv.y, dv.x);
                float rimAmount = lerp(1.0, 0.45, roundMain);
                float rim = radius * (1.0 + rimAmount * (0.065 * sin(phi * 3.0 + h2 * 9.0 + t * 0.22)
                    + 0.035 * sin(phi * 5.0 + h1 * 13.0 - t * 0.17)
                    + 0.018 * sin(phi * 7.0 + h3 * 11.0 + t * 0.12)));
                float inside = rim - length(dv);
                // Smooth union only at the joins; this fills necks without inflating a Gaussian cloud.
                float underlaySdf = silhouette;
                float join = max(0.07 - abs(silhouette - inside), 0.0) / 0.07;
                silhouette = max(silhouette, inside) + join * join * 0.0175;
                // The shoulder's contribution feathers across its boundary. A hard front-rank switch
                // made the prior native capture read as a pile of outlined rubber balls.
                float order = j == 0 ? 0.0 : 0.25 + h3 * 0.65;
                if (inside >= -0.015 && order > front)
                {
                    front = order;
                    float2 light = normalize(float2(-0.65, 0.76));
                    // Main lobe shapes rotate, but their apparent light shares one view-space direction.
                    // Other banks retain the original shape-local lighting for an isolated comparison.
                    float2 lightingV = roundMain > 0.5 ? v : dv;
                    float shadowSide = smoothstep(-0.10, 0.80, -dot(normalize(lightingV + 1e-5), light));
                    // The late neutral banks need broad curled contact folds. Direction and arc breaks keep the rim partial.
                    float contact = 1.0 - smoothstep(0.01, contactWidth, inside / max(radius, 0.1));
                    // Main exposed caps stay matte; strong creases form where a foreground shoulder overlaps a lobe.
                    if (roundMain > 0.5)
                        contact *= lerp(0.18, 1.0, smoothstep(-0.02, 0.08, underlaySdf));
                    float arcBreak = smoothstep(-0.4, 0.25, sin(phi * 2.0 + h2 * 7.0));
                    // A broad directional face fold supplies volume between the pale upper face and its narrow contact.
                    // It is not a radial rim: the lower-right interior receives the shade, including the centerward fold.
                    float2 planeQ = lightingV / max(radius, 0.1);
                    float plane = smoothstep(0.10, 0.65, -dot(planeQ, light)
                        + 0.075 * sin(planeQ.x * 3.0 + h1 * 6.0));
                    float lobeLight = 1.0 - max(shadowSide * contact * lerp(0.35, 0.90, arcBreak), plane * foldStrength);
                    float coverage = smoothstep(-0.025, 0.07, inside);
                    crescent = lerp(crescent, lobeLight, coverage);
                    contour = inside;
                    arcSide = shadowSide * coverage * lerp(0.4, 1.0, arcBreak);
                    variation = lerp(variation, lerp(0.94, 1.035, h2), coverage);
                }
            }
            return silhouette;
        }

        // The main brown fragments use two unequal folded faces, not three round smoke beads.
        float DebrisTaperCut(float2 p, float2 a, float2 b, float ra, float rb)
        {
            float2 ab = b - a;
            float u = saturate(dot(p - a, ab) / max(dot(ab, ab), 1e-5));
            return lerp(ra, rb, u) - length(p - (a + ab * u));
        }

        // Three reference-derived fragment contours (f112); no universal fork or enclosed hole.
        static const float2 BrownContour0[12] = {
            float2(0.44236, 0.94835),
            float2(-0.07158, 1.00000),
            float2(-0.48701, 0.65017),
            float2(-0.90244, 0.30033),
            float2(-0.99867, -0.20892),
            float2(-0.97139, -0.73175),
            float2(-0.56456, -1.00000),
            float2(-0.04176, -0.89407),
            float2(0.28410, -0.45958),
            float2(0.64859, -0.07868),
            float2(0.99867, 0.21210),
            float2(0.68921, 0.48681)
        };
        static const float2 BrownContour1[12] = {
            float2(-1.00000, 0.69611),
            float2(-0.95865, 0.15080),
            float2(-0.88595, -0.19348),
            float2(-0.56871, -0.22128),
            float2(-0.26353, -0.50978),
            float2(0.11783, -0.72770),
            float2(0.68140, -0.60997),
            float2(1.00000, -0.10021),
            float2(0.71819, 0.11279),
            float2(0.55531, 0.11911),
            float2(0.11938, 0.52281),
            float2(-0.39971, 0.72770)
        };
        static const float2 BrownContour2[12] = {
            float2(-0.22203, 1.00000),
            float2(-0.47387, 0.62223),
            float2(-0.49806, 0.17554),
            float2(-0.14502, -0.02279),
            float2(-0.06057, -0.45569),
            float2(-0.03037, -0.90870),
            float2(0.15909, -1.00000),
            float2(0.49806, -0.85724),
            float2(0.46615, -0.50325),
            float2(0.23066, -0.16752),
            float2(0.16645, 0.28194),
            float2(0.05731, 0.71736)
        };

        float2 BrownContourPoint(int j, int variant)
        {
            j = (j + 12) % 12;
            return variant == 0 ? BrownContour0[j] : (variant == 1 ? BrownContour1[j] : BrownContour2[j]);
        }

        // Rounded quadratic joins retain the three traced silhouettes without hard polygon corners.
        float2 BrownRoundedPoint(int j, float t, int variant)
        {
            float2 p = BrownContourPoint(j, variant);
            float2 a = (BrownContourPoint(j - 1, variant) + p) * 0.5;
            float2 b = (p + BrownContourPoint(j + 1, variant)) * 0.5;
            return lerp(lerp(a, p, t), lerp(p, b, t), t);
        }

        float BrownReferenceField(float2 p, int variant)
        {
            float distanceToEdge = 1e3;
            bool inside = false;
            [unroll] for (int j = 0; j < 24; j++)
            {
                int corner = j / 2;
                float t = (j % 2) * 0.5;
                float2 a = BrownRoundedPoint(corner, t, variant);
                float2 b = BrownRoundedPoint(corner, t + 0.5, variant);
                float2 edge = b - a;
                float u = saturate(dot(p - a, edge) / max(dot(edge, edge), 1e-5));
                distanceToEdge = min(distanceToEdge, length(p - (a + edge * u)));
                if ((a.y > p.y) != (b.y > p.y))
                {
                    float hitX = a.x + (p.y - a.y) * edge.x / edge.y;
                    if (p.x < hitX) inside = !inside;
                }
            }
            return (inside ? distanceToEdge : -distanceToEdge) + 0.025;
        }

        float DebrisFoldField(PuffV2F i, out float crescent, out float contour, out float arcSide, out float variation)
        {
            float seed = i.shape.x;
            float angle = seed * 6.2831853 + i.life.w * i.life.z * 0.13;
            float sn, cs; sincos(angle, sn, cs);
            float2 p = float2(i.local.x * cs - i.local.y * sn, i.local.x * sn + i.local.y * cs) / 1.12;
            p.x *= seed < 0.5 ? -1.0 : 1.0;
            int variant = seed < 0.55 ? 0 : (seed < 0.85 ? 1 : 2);
            float sdf = BrownReferenceField(p, variant);
            float fold = p.y + 0.20 * p.x;
            float crease = exp(-pow((p.y - (0.10 + 0.20 * sin(p.x * 2.8 + seed * 5.0))) / 0.18, 2.0))
                * smoothstep(-0.9, -0.5, p.x) * (1.0 - smoothstep(0.25, 0.75, p.x));
            crescent = max(0.10, smoothstep(-0.55, 0.35, fold) - crease * 0.30);
            contour = sdf;
            arcSide = lerp(0.20, 0.55, 1.0 - crescent);
            variation = lerp(0.96, 1.04, CelHash21(float2(seed * 29.0, 13.0)));
            return sdf;
        }
        // Unequal blunt folds with open edge clefts; no enclosed hole or ring.
        float FarSootField(PuffV2F i, out float crescent, out float contour, out float arcSide, out float variation)
        {
            float2 p = i.local / float2(0.85, 1.20);
            p += float2(sin(p.y * 7.0 + i.shape.x * 5.0), sin(p.x * 6.0 + i.life.w * 0.5)) * 0.025;
            float2 qa = abs((p - float2(-0.10, -0.20)) / float2(0.82, 0.80));
            float2 qb = abs((p - float2(0.10, 0.40)) / float2(0.92, 0.60));
            float2 qc = abs((p - float2(0.78, 0.40)) / float2(0.36, 0.28));
            float a = (1.0 - pow(pow(qa.x, 3.0) + pow(qa.y, 3.0), 1.0 / 3.0)) * 0.80;
            float b = (1.0 - pow(pow(qb.x, 2.6) + pow(qb.y, 2.6), 1.0 / 2.6)) * 0.60;
            float c = (1.0 - qc.x - qc.y) * 0.28;
            float ab = max(0.10 - abs(a - b), 0.0) / 0.10;
            float sdf = max(a, b) + ab * ab * 0.025;
            float bc = max(0.08 - abs(sdf - c), 0.0) / 0.08;
            sdf = max(sdf, c) + bc * bc * 0.020;
            sdf = min(sdf, 0.96 - p.y - 0.10 * p.x);
            float cleft = DebrisTaperCut(p, float2(0.05, -1.35), float2(-0.02, -0.62), 0.12, 0.045);
            float shoulder = DebrisTaperCut(p, float2(1.42, 0.34), float2(0.78, 0.18), 0.12, 0.045);
            sdf = min(sdf, -max(cleft, shoulder));
            // Reference84/86 has two high dark faces separated by a narrow orange gap; they join by88.
            float earlyGap = lerp(0.11, -0.12, smoothstep(0.12, 0.28, i.life.w));
            sdf = min(sdf, abs(p.y - (0.03 + 0.04 * p.x)) - earlyGap);
            // A broad curved crease crosses only part of the face; it cannot form a radial rim.
            float crease = exp(-pow((p.y - (0.05 + 0.18 * sin(p.x * 2.6 + 0.4))) / 0.16, 2.0))
                * smoothstep(-0.80, -0.30, p.x) * (1.0 - smoothstep(0.35, 0.80, p.x));
            crescent = smoothstep(-0.55, 0.30, p.y + 0.24 * p.x) * (1.0 - 0.75 * crease);
            contour = sdf; arcSide = 0.0; variation = 1.0;
            return sdf;
        }

        float PuffSdf(PuffV2F i, out float crescent, out float contour, out float arcSide, out float variation)
        {
            if (i.more.y > 0.635 && i.more.y < 0.645)
                return FarSootField(i, crescent, contour, arcSide, variation);
            if (i.more.y > 0.605 && i.more.y < 0.615)
                return DebrisFoldField(i, crescent, contour, arcSide, variation);
            float contactWidth = i.more.y >= 0.75 ? 0.28 : 0.14;
            float foldStrength = i.more.y > 0.805 && i.more.y < 0.815 ? 0.22 : 0.0;
            float roundMain = i.more.y > 0.805 && i.more.y < 0.815 ? 1.0 : 0.0;
            return PuffField(i.local, i.shape.x, i.life.x, i.life.y, i.life.z, i.life.w, contactWidth, foldStrength, roundMain,
                crescent, contour, arcSide, variation);
        }

        // Smoke needs the complete dark-to-pale authored palette. CelRamp with two bands uses only c2/c3,
        // which erased every shadow in the rejected bead version even when dark c0/c1 were specified.
        half3 PuffColor(float packedRow, uint band, float k, float coolFrom)
        {
            uint a = CelRowA(packedRow), b = CelRowB(packedRow);
            half3 col = CelPal(a, band);
            if (b != a && b != 0u) col = lerp(col, CelPal(b, band), smoothstep(coolFrom, 1.0, k));
            return col;
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

        // Retain the full bank for most of the life, then lose opacity over its last quarter. Edges also become
        // softer as the smoke thins, rather than leaving brown, hard-edged fragments behind.
        float PuffAlpha(PuffV2F i, float sdf, float ef)
        {
            if (i.more.y > 0.635 && i.more.y < 0.645)
                return smoothstep(-0.05, 0.07, sdf) * smoothstep(0.0, 0.04, i.life.w)
                    * (1.0 - smoothstep(0.80, 1.35, i.life.w)) * 0.96;
            float late = smoothstep(0.70, 1.0, i.life.x);
            float edge = smoothstep(-0.04, lerp(0.05, 0.14, late), sdf);
            float erosion = smoothstep(-0.025, max(i.more.x, 0.10), ef - i.shape.y);
            float fade = 1.0 - smoothstep(0.60, 0.98, i.life.x);
            // Late banks retain a dark haze; fire wraps and site puffs keep their shorter fade.
            float tail = abs(i.more.y) >= 0.75 ? fade * sqrt(fade) : fade * fade * fade;
            return edge * erosion * tail;
        }
        ENDHLSL

        Pass
        {
            Name "CelExpPuff"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            Cull Off ZWrite Off ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            PuffV2F vert(Attributes v, uint instanceID : SV_InstanceID) { return PuffVert(v.uv, instanceID); }

            half4 frag(PuffV2F i) : SV_Target
            {
                float crescent, contour, arcSide, variation;
                float sdf = PuffSdf(i, crescent, contour, arcSide, variation);
                float ef = PuffErodeField(i.local, i.shape.x, i.life.x);
                // RearPeak is fragmented soot, not a pale cell containing a dark bean-shaped hole.
                float sootGrain = CelValue2(i.local * 5.4 + float2(i.shape.x * 37.0, i.life.w * 0.22));
                if (i.more.y < 0.0)
                {
                    sdf += (sootGrain - 0.5) * 0.10;
                    ef -= (1.0 - sootGrain) * 0.08;
                }
                float alpha = PuffAlpha(i, sdf, ef);
                if (i.more.y < 0.0)
                {
                    // Per-record transparency must survive overlaps; open the interior as soot disperses.
                    float porousAt = lerp(0.28, 0.46, smoothstep(0.20, 0.78, i.life.x));
                    float porous = smoothstep(porousAt, porousAt + 0.10, sootGrain);
                    alpha *= lerp(0.35, 0.85, smoothstep(0.40, 0.62, i.shape.x)) * porous;
                }
                if (i.more.w < 0.99) alpha *= smoothstep(0.0, 0.12, i.positionWS.y - i.more.z);
                if (i.more.y > 0.805 && i.more.y < 0.815)
                {
                    // Feather the Main bank's intersection with opaque terrain instead of exposing straight clipped cards.
                    float2 screenUV = i.positionCS.xy / _ScaledScreenParams.xy;
                    float sceneEye = LinearEyeDepth(SampleSceneDepth(screenUV), _ZBufferParams);
                    float puffEye = LinearEyeDepth(i.positionCS.z, _ZBufferParams);
                    alpha *= smoothstep(0.0, 0.35, max(sceneEye - puffEye, 0.0));
                }
                clip(alpha - 0.002);
                half3 body = PuffColor(i.tone.x, 2u, i.life.x, i.tone.w);
                half3 light = PuffColor(i.tone.x, 3u, i.life.x, i.tone.w);
                half3 shadow = PuffColor(i.tone.x, 0u, i.life.x, i.tone.w);
                half3 mid = PuffColor(i.tone.x, 1u, i.life.x, i.tone.w);
                float thinning = smoothstep(0.65, 0.95, i.life.x);
                half3 pale = lerp(body, light, 0.20);
                // Broader neutral-bank folds use more of c0. Short fire wraps, debris, and rear soot retain their fill.
                half3 softShadow = lerp(shadow, mid, i.more.y >= 0.75 ? 0.55 : 0.72);
                half3 col = lerp(softShadow, pale, crescent) * lerp(variation, 1.0, thinning);
                // Soot is an irregular flat patch that may touch a billow boundary. The previous
                // deep inset made every dark patch look like a hole inside a glowing spherical rim.
                float2 sootQ = i.local * 2.1 + float2(i.shape.x * 19.0, i.life.w * 0.12);
                float pocketNoise = CelValue2(sootQ) * 0.72
                    + CelValue2(sootQ * 2.3 + 7.1) * 0.28;
                float pocketShare = i.more.y < 0.0 ? 0.78 : 0.28;
                float pocket = smoothstep(0.48, 0.68, pocketNoise)
                    * (1.0 - smoothstep(pocketShare - 0.04, pocketShare, i.shape.x));
                pocket *= smoothstep(-0.015, 0.035, sdf);
                half3 soot = i.more.y < 0.0 ? lerp(shadow, mid, 0.42) : lerp(shadow, mid, 0.66);
                if (i.more.y < 0.0)
                {
                    // Mix small dark and pale fragments across records. No inset dark center remains.
                    float paleShare = smoothstep(0.40, 0.62, i.shape.x);
                    col = lerp(lerp(shadow, mid, 0.40) * half3(1.18, 1.02, 0.80), lerp(mid, pale, 0.82), (half)paleShare);
                    col *= (half)lerp(0.95, 1.04, sootGrain);
                }
                else if (i.more.y > 0.635 && i.more.y < 0.645)
                {
                    half3 darkFace = lerp(shadow, mid, 0.38);
                    half3 litFace = lerp(mid, body, 0.30);
                    col = lerp(darkFace, litFace, (half)(crescent * 0.80));
                    float heat = 1.0 - smoothstep(0.08, 0.18, i.life.w);
                    col = lerp(col, body * half3(1.50, 0.85, 0.60) * (half)lerp(0.65, 1.0, crescent), (half)heat);
                }
                else if (i.more.y > 0.605 && i.more.y < 0.615)
                {
                    // Measured reference fragments have warm brown faces and narrow dark undersides.
                    half3 darkFace = lerp(mid, body, 0.18);
                    half3 litFace = lerp(body, light, 0.65);
                    col = lerp(darkFace, litFace, (half)crescent) * (half)variation;
                }
                else
                {
                    // Normal-bank soot clears while the gray curled contacts remain visible.
                    float pocketFade = i.more.y >= 0.75 ? 1.0 - smoothstep(0.35, 0.65, i.life.x) : 1.0;
                    col = lerp(col, soot, (half)(pocket * pocketFade * (1.0 - thinning * 0.3)));
                }
                if (i.more.y > 0.805 && i.more.y < 0.815)
                {
                    // Only existing Main RGB: alpha, silhouette, brown .61 and all other tags stay untouched.
                    half3 contactShade = min(col, lerp(shadow, mid, 0.35));
                    col = lerp(col, contactShade, (half)(MainContactField(i) * 0.26));
                }
                float outside = CelInk(sdf, 0.0, i.tone.z * 0.7) * arcSide * 0.34;
                float inner = CelInk(contour, 0.0, i.tone.z * 0.60) * arcSide * 0.34;
                float ink = max(outside, inner) * (1.0 - thinning);
                col = lerp(col, CelInkColor(i.tone.x, i.life.x, i.tone.w), ink);
                // At the end the connected bank becomes dim haze; no bright isolated beads remain.
                // Main-bank tag0.81 preserves112 color and begins losing illumination before128.
                float dimFrom = i.more.y > 0.805 && i.more.y < 0.815 ? 0.50 : 0.65;
                col *= lerp(1.0, 0.45, smoothstep(dimFrom, 0.96, i.life.x));
                col = ApplyMist(col, i.positionWS);
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.fog) * alpha, alpha);
            }
            ENDHLSL
        }
    }
}
