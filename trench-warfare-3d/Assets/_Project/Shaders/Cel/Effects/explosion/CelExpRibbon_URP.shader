// Phase: CelFx job (2026-11-10, lane/show/cel-explosion, round d20) - ExpRibbon: a path-ribbon spark streak.
// A thin camera-facing strip (CelMeshes.Strip(16)) that follows the record's own past path (CelPosition sampled back
// in time, so gravity and drag bend it) plus a per-record bend in the screen plane that grows toward the tail (a C or
// hook curve, sign and size from the seed) and a small S wiggle. It tapers from a round head to a pointed tail, a white
// HDR core with a faint warm edge (CelRamp of the palette), and is cut by erosion like the kit's Streak. Additive, one
// indirect draw for every record. Drop-in for a Streak layer: the same size/erode/tone fields.
//   size.xyz   width (m) at birth, at death, ease          size.w  trail length in seconds of its path (0: a dot)
//   erode.xyz  the cross-section cut threshold over the life
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      x tail width share (0 pointed .. 1 square), y max length (m, 0: none), z flicker 0..1,
//              w bend: the tail's sideways offset as a share of the length (signed per record by the seed)
// Kept for a build by Resources/ShaderKeep/KeepCelExpRibbon.mat. Written to lift into the kit unchanged.
Shader "TW/Cel/ExpRibbon"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+21" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelExpRibbon"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off ZTest LEqual Cull Off
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
                float2 uv : TEXCOORD0;                          // x 0 head .. 1 tail, y -1..1 across
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w erosion threshold
                nointerpolation float4 shape : TEXCOORD2;       // x tail width share, y unused, z flicker, w seed
                float4 more : TEXCOORD3;                        // x fog
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                bool fanStrand = r.extra.x < -0.5 && r.extra.x > -2.5 && v.uv.y > 1.5;
                float fanStrandId = fanStrand ? floor(v.uv.y * 0.5) - 1.0 : 0.0;
                float fanLocalU = v.uv.x;
                float fanAcross = fanStrand ? (v.uv.y - 2.0 * (fanStrandId + 1.0)) * 2.0 - 1.0 : v.uv.y * 2.0 - 1.0;
                float2 fanRange = float2(0.0, 1.0);
                float fanOffset = 0.0, fanWidth = 1.0;
                float s = v.uv.x;                                  // 0 head .. 1 tail
                if (fanStrand)
                {
                    bool fanMiddle = r.extra.x < -1.5;
                    if (fanStrandId < 0.5)
                    {
                        fanRange = fanMiddle ? float2(0.11, 0.29) : float2(0.10, 0.23);
                        fanOffset = fanMiddle ? 0.72 : -0.72;
                        fanWidth = 0.16;
                    }
                    else if (fanStrandId < 1.5)
                    {
                        fanRange = fanMiddle ? float2(0.32, 0.55) : float2(0.29, 0.45);
                        fanOffset = fanMiddle ? -0.65 : -0.46;
                        fanWidth = 0.12;
                    }
                    else if (fanStrandId < 2.5)
                    {
                        fanRange = fanMiddle ? float2(0.52, 0.67) : float2(0.44, 0.62);
                        fanOffset = fanMiddle ? 0.58 : 0.54;
                        fanWidth = 0.11;
                    }
                    else
                    {
                        fanRange = fanMiddle ? float2(0.06, 0.17) : float2(0.15, 0.26);
                        fanOffset = fanMiddle ? -0.72 : 0.65;
                        fanWidth = 0.14;
                    }
                    s = lerp(fanRange.x, fanRange.y, fanLocalU);
                }
                float width = CelCurve(r.size.xyz, k) * alive;
                float3 head = CelPosition(r, t);
                float3 hv = CelVelocity(r, t);
                float hs = length(hv);
                float3 hdir = hs > 1e-3 ? hv / hs : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                // the time the trail spans: size.w seconds, cut so the path is no longer than extra.y metres
                float span = max(r.size.w, 0.0);
                float fullLen = distance(head, CelPosition(r, max(t - span, 0.0)));
                if (r.extra.y > 0.0 && fullLen > r.extra.y) span *= r.extra.y / fullLen;
                float ts = max(t - s * span, 0.0);
                float3 p = CelPosition(r, ts);
                float len = distance(head, CelPosition(r, max(t - span, 0.0)));
                // the bend: a sideways offset in the screen plane growing as s^2 (tangent to the motion at the head)
                float3 toEye = normalize(_WorldSpaceCameraPos - head);
                float3 bside = cross(hdir, toEye);
                float bl = dot(bside, bside);
                bside = bl > 1e-6 ? bside * rsqrt(bl) : UNITY_MATRIX_V[0].xyz;
                float sgn = CelHash11(r.seed * 71.3) < 0.5 ? -1.0 : 1.0;
                float bend = r.extra.w * sgn * lerp(0.4, 1.0, CelHash11(r.seed * 23.9));
                float wig = 0.06 * sin(s * 6.2831853 + r.seed * 40.0);
                if (r.extra.x < 0.0) { bend = 0.0; wig *= 0.12; }
                p += bside * len * (bend * s * s + wig * s);
                // the path's tangent at s (d/ds), for the strip's side
                float3 tg = -CelVelocity(r, ts) * span + bside * len * (bend * 2.0 * s + (r.extra.x < 0.0 ? 0.0072 : 0.06) * (sin(s * 6.2831853 + r.seed * 40.0) + s * 6.2831853 * cos(s * 6.2831853 + r.seed * 40.0)));
                float tl = dot(tg, tg);
                float3 dir = tl > 1e-8 ? -tg * rsqrt(tl) : hdir;
                float3 side = cross(dir, normalize(_WorldSpaceCameraPos - p));
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                // taper: full at the head, extra.x share at the tail; the head end rounded over its first 6%
                float taper = lerp(1.0, saturate(r.extra.x), s) * sqrt(saturate(s / 0.06 + 0.15));
                if (r.extra.x < 0.0)
                {
                    // Give the fragment field room for independent upper/lower lobes. The
                    // visible body is cut inside this envelope, not a thin noisy outline on a leaf.
                    // The luminous mass lags behind the leading point as the sheet cools.
                    // A fixed leaf profile moved the late body too far left by 20-90 pixels.
                    float shapeA = 1.7;
                    if (r.extra.x > -1.5)
                        shapeA += -0.50 * exp(-pow((k - 0.40) / 0.075, 2.0))
                            + 2.05 * smoothstep(0.49, 0.73, k) + 3.50 * smoothstep(0.78, 0.94, k);
                    else if (r.extra.x > -2.5)
                        shapeA += 1.55 * smoothstep(0.35, 0.56, k)
                            + 0.45 * smoothstep(0.60, 0.76, k) + 2.30 * smoothstep(0.78, 0.94, k);
                    float peakS = shapeA / (shapeA + 6.5);
                    float norm = pow(peakS, shapeA) * pow(1.0 - peakS, 6.5);
                    taper = pow(s, shapeA) * pow(1.0 - s, 6.5) / max(norm, 0.0001);
                    taper = max(taper, 0.09 * smoothstep(0.03, 0.10, s) * (1.0 - smoothstep(0.92, 1.0, s)));
                    taper *= 1.15;
                }
                if (fanStrand)
                {
                    // Follow the parent rim rather than placing four detached pale leaflets
                    // at a constant distance from the centreline. Each tongue has one attached
                    // end and one free tip, so it cannot form a closed leaf-vein loop.
                    float parentFanTaper = taper;
                    float strandBell = max(0.0, sin(3.14159265 * fanLocalU));
                    taper = pow(strandBell, 0.55 + 0.10 * fanStrandId)
                        * fanWidth * (0.82 + 0.18 * sin(fanLocalU * (7.0 + fanStrandId) + r.seed * 13.0 + fanStrandId));
                    float strandSide = fanOffset < 0.0 ? -1.0 : 1.0;
                    float strandFree = (fanStrandId < 0.5 || fanStrandId > 2.5) ? (1.0 - fanLocalU) : fanLocalU;
                    float strandOffset = strandSide * (0.25 * parentFanTaper
                        + (0.17 + 0.030 * fanStrandId) * pow(saturate(strandFree), 1.1))
                        + 0.015 * strandBell * sin(fanLocalU * (5.0 + fanStrandId) + r.seed * 11.0 + fanStrandId);
                    p += side * width * strandOffset;
                }
                float halfW = width * 0.5 * max(taper, 0.0);
                // a little cap ahead of the head so it reads round, not cut
                float3 w = p + dir * (s < 1e-3 ? width * 0.35 : 0.0) + side * fanAcross * halfW;
                // Only the upper sheet gradually drops in the reference. Middle/lower
                // already travel on their authored diagonals and retain those paths.
                if (r.extra.x < -0.5 && r.extra.x > -1.5)
                    w -= UNITY_MATRIX_V[1].xyz * (0.40 * smoothstep(0.40, 0.95, k) * pow(1.0 - s, 1.3));
                Varyings o;
                o.positionWS = w;
                // Fan sheets sweep toward the eye over the ground; retain their projected path while
                // placing the luminous volume above terrain instead of burying its lower branch.
                float3 draw = r.extra.x < 0.0 ? _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * 0.65 : w;
                o.positionCS = TransformWorldToHClip(draw);
                o.uv = float2(fanStrand ? fanLocalU : s, fanAcross);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, fanStrand ? fanStrandId + 1.0 : 0.0, r.extra.z, r.seed);
                o.more = float4(ComputeFogFactor(o.positionCS.z), k, r.tone.w, 0.0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float s = i.uv.x;
                float across = saturate(1.0 - abs(i.uv.y));
                // white along the core near the head, warm toward the edge and the tail
                float field = across * lerp(1.0, 0.45, s);
                if (i.shape.x < 0.0)
                {
                    uint row = CelRowA(i.tone.x);
                    float k = i.more.y;
                    float phase = i.shape.w * 19.0;
                    bool middle = i.shape.x < -1.5 && i.shape.x > -2.5;
                    bool lower = i.shape.x < -2.5;
                    if (i.shape.y > 0.5)
                    {
                        // Independent short ribbons outside the main blade, with unequal soft breaks.
                        float strandId = i.shape.y - 1.0;
                        float strandPhase = phase + strandId * 3.7;
                        float strandNoise = CelValue2(float2(s * (11.0 + strandId * 2.0) + strandPhase, strandPhase + 7.3));
                        float strandContour = 0.68 + 0.26 * strandNoise
                            - abs(i.uv.y - 0.08 * sin(s * 17.0 + strandPhase));
                        float strandAlpha = smoothstep(0.0, 0.18, strandContour)
                            * smoothstep(0.32 + 0.012 * strandId, 0.50 + 0.012 * strandId, k)
                            * (1.0 - smoothstep(0.75 - 0.012 * strandId, 0.95 - 0.012 * strandId, k));
                        if ((strandId > 0.5 && strandId < 1.5) || strandId > 2.5)
                            strandAlpha *= smoothstep(0.006, 0.028, abs(s - (0.39 + 0.055 * strandId)));
                        // A tongue belongs to the same cooling sheet. The old separate cooling
                        // window left white shards around a sheet that was already amber.
                        float strandCooling = middle ? smoothstep(0.40, 0.76, k) : smoothstep(0.32, 0.62, k);
                        half3 strandColour = lerp(CelPal(row, 2), CelPal(row, 3), (half)(0.25 * (1.0 - strandCooling)));
                        half3 strandAmber = middle
                            ? lerp(CelPal(row, 1), CelPal(row, 2), (half)0.90) * half3(1.0, 1.08, 1.18)
                            : CelPal(row, 1) * half3(1.0, 0.77, 1.35);
                        strandColour = lerp(strandColour, strandAmber, (half)strandCooling);
                        if (middle) strandColour.b *= (half)lerp(1.0, 1.20, smoothstep(0.60, 0.76, k));
                        strandColour *= (half)(strandAlpha * i.tone.z * 0.85 * (1.0 - i.tone.w));
                        return half4(CelAdditiveFog(strandColour, i.positionWS, i.more.x), 1.0);
                    }
                    // A few large, unequal lobes read as torn flame sheets. Independent
                    // contours avoid the mirror-symmetric cigar of the original strip.
                    float nTop = CelValue2(float2(s * 9.0 - k * 0.45, phase));
                    float nBottom = CelValue2(float2(s * 11.0 - k * 0.35, phase + 7.1));
                    float top = 0.64 + 0.30 * nTop;
                    float bottom = 0.63 + 0.32 * nBottom;
                    float tear = 0.35 + 0.65 * smoothstep(0.30, 0.80, k);
                    float cTop = 0.25 + 0.05 * sin(phase);
                    float cBottom = 0.43 + 0.06 * cos(phase * 1.3);
                    // Broad opposite-side bites pinched the shifted upper/middle body into an S.
                    // Keep their uneven edge cuts, but narrow them and retain a straighter hot spine.
                    // The lower sheet keeps its existing contour and separate fragmentation.
                    float biteRelief = lower ? 0.0 : (1.0 - smoothstep(0.76, 0.86, k));
                    float topBite = lerp(0.10 + 0.52 * tear, 0.04 + 0.18 * tear, biteRelief);
                    float bottomBite = lerp(0.10 + 0.56 * tear, 0.04 + 0.20 * tear, biteRelief);
                    top -= topBite * exp(-pow((s - cTop) / lerp(0.045, 0.026, biteRelief), 2.0));
                    bottom -= bottomBite * exp(-pow((s - cBottom) / lerp(0.060, 0.035, biteRelief), 2.0));
                    top -= lerp(0.30, 0.20, biteRelief) * tear * exp(-pow((s - 0.57) / lerp(0.040, 0.022, biteRelief), 2.0));
                    float center = 0.055 * sin(s * 18.0 + phase - k * 0.6);
                    float y = i.uv.y - center;
                    float contour = min(top - y, bottom + y);
                    float soft = smoothstep(0.0, 0.075, contour);
                    float ending = middle ? (1.0 - smoothstep(0.90, 1.0, k)) : (1.0 - smoothstep(0.82, 1.0, k));
                    soft *= smoothstep(0.18, 0.40, k) * ending;
                    // Actual full-width ruptures, not partial-alpha surface noise: the lower
                    // sheet is separate tongues by frame90. Three unequal diagonal gaps
                    // cut the longitudinal body as it cools. Middle remnants last longer.
                    float splitPhase = lower ? smoothstep(0.64, 0.84, k)
                        : (middle ? smoothstep(0.85, 1.05, k) : smoothstep(0.80, 1.05, k));
                    float gapWidth = lerp(-0.012, 0.043, splitPhase);
                    float longitudinal = s + y * 0.014 + 0.010 * sin(y * 5.1 + phase + s * 9.0);
                    float gapA = abs(longitudinal - (0.16 + 0.025 * sin(phase))) - gapWidth;
                    float gapB = abs(longitudinal - (0.38 + 0.025 * cos(phase * 1.3))) - gapWidth * 0.72;
                    float gapC = abs(longitudinal - (0.61 + 0.030 * sin(phase * 0.7))) - gapWidth * 0.56;
                    soft *= smoothstep(0.0, 0.010, min(gapA, min(gapB, gapC)));
                    if (lower)
                    {
                        // Divide the broad foreground body once more; its previous1114px
                        // surviving island was still three times the reference's largest.
                        float bodyGap = lerp(-0.012, 0.020, smoothstep(0.66, 0.84, k));
                        soft *= smoothstep(0.0, 0.010, abs(longitudinal - 0.25) - bodyGap);
                    }
                    else
                    {
                        // Late fragments remain behind the moving leading point.
                        float frontCut = (middle ? 0.28 : 0.20) * smoothstep(0.80, 0.94, k);
                        soft *= smoothstep(frontCut, frontCut + 0.025, s);
                    }
                    // Disconnected surviving islands also have irregular edges, not three
                    // perfectly straight saw cuts. The material motion remains coherent.
                    float island = CelValue2(float2(s * 12.0 - k * 0.40, phase + 11.0));
                    float islandMask = smoothstep(lerp(-0.15, 0.35, splitPhase), lerp(-0.08, 0.46, splitPhase), island);
                    soft *= lower ? lerp(1.0, islandMask, 0.30) : islandMask;
                    float heat = smoothstep(0.025, 0.30, contour) * lerp(1.0, 0.82, s);
                    half3 sheet = lerp(CelPal(row, 0), CelPal(row, 2), smoothstep(0.08, 0.68, heat));
                    float whiteHighlight = smoothstep(0.85, 0.99, heat);
                    if (middle) whiteHighlight *= 1.0 - smoothstep(0.35, 0.58, k);
                    sheet = lerp(sheet, CelPal(row, 3), whiteHighlight);
                    // Ref: white at 82, cream at 86, amber at 90, torn orange remnants at 94.
                    float cooling = (!middle && !lower) ? smoothstep(0.32, 0.62, k) : smoothstep(0.40, 0.76, k);
                    half3 amberCore = middle ? lerp(CelPal(row, 1), CelPal(row, 2), (half)0.90) * half3(1.0, 1.08, 1.18)
                        : CelPal(row, 1) * half3(1.0, lower ? 1.0 : 0.77, lower ? 0.90 : 1.35);
                    half3 amber = lerp(CelPal(row, 0), amberCore, smoothstep(0.03, 0.32, contour));
                    sheet = lerp(sheet, amber, (half)cooling);
                    if (middle) sheet.b *= (half)lerp(1.0, 1.20, smoothstep(0.60, 0.76, k));
                    sheet *= (half)(soft * i.tone.z * (1.0 - i.tone.w));
                    return half4(CelAdditiveFog(sheet, i.positionWS, i.more.x), 1.0);
                }
                float flick = 1.0 - i.shape.z * step(0.5, CelHash11(floor(_CelNow * 24.0) + i.shape.w * 37.0));
                float mask = CelAAStep(max(i.tone.w, 0.02), field);
                half3 col = CelRamp(field, CelRowA(i.tone.x), i.tone.y);
                col *= (half)(mask * i.tone.z * flick);
                // Negative ink is an opt-in warm-energy cap on closing sparks; fan and ordinary ribbons are unchanged.
                if (i.more.z < -0.5) col = min(col, half3(2.0, 1.6, 0.45));
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
