// Axis is the eye's level right. Extra: mode, timeline offset, ribbon phase, unused.
// Motion.xy is the local ground slope for stationary layers; particles use ballistic motion.
// Size.w is shaft/ribbon height, or the ground card's depth-to-width ratio.
// All motion is evaluated from records; the crescent survives until scene teardown.
Shader "TW/Cel/OrbitalLaser"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+16" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelOrbitalLaser"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
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
                float3 positionWS : TEXCOORD0;
                float2 q : TEXCOORD1;
                nointerpolation float4 tone : TEXCOORD2;
                nointerpolation float4 state : TEXCOORD3; // mode, age, life fraction, seed
                nointerpolation float4 shape : TEXCOORD4; // size, timeline age, phase, far
                float fog : TEXCOORD5;
            };

            float3 Sweep(float t, float3 right)
            {
                float k = saturate((t - 0.2) / 3.16);
                float angle = lerp(0.45, 3.22, pow(k, 0.88));
                float terminal = lerp(3.18, 3.06, smoothstep(3.20, 3.36, t));
                float correction = lerp(0.55, 0.75, smoothstep(3.30, 3.50, t));
                angle = lerp(angle, terminal, smoothstep(2.92, 3.17, t) * correction);
                return right * (0.45 - sin(angle) * 3.0) + cross(right, float3(0, 1, 0)) * (cos(angle) * 2.24);
            }

            float PixelMetres(float3 p)
            {
                float4 clipP = TransformWorldToHClip(p);
                return 2.0 * abs(clipP.w) / max(abs(UNITY_MATRIX_P._m11) * _ScreenParams.y, 1.0);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float mode = r.extra.x;
                float3 right = normalize(r.axis);
                float3 forward = cross(right, float3(0, 1, 0));
                float size = CelCurve(r.size.xyz, k);
                float age = t + r.extra.y;
                float3 p = r.pos;
                if (mode > 2.5 && mode < 3.5)
                {
                    float3 shift = -forward * 0.60 + right * 0.12;
                    p += shift + float3(0, dot(shift.xz, r.motion.xy), 0);
                }
                float2 q = v.uv * 2.0 - 1.0;
                float far = saturate(_CelLod.x - 1.0);
                float3 w;
                if (mode < 0.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.18, 0);
                    float3 side = normalize(cross(float3(0, 1, 0), _WorldSpaceCameraPos - p));
                    float arriving = 1.0 - smoothstep(0.025, 0.20, t);
                    float arrival = lerp(1.0, 10.0, arriving);
                    arrival = lerp(arrival, 1.0, far * 0.7);
                    float h = v.uv.x * v.uv.x * r.size.w;
                    float foot = 1.0 + exp(-h * 1.35) * lerp(0.5, 1.8, far) * (1.0 - arriving);
                    float width = max(size * arrival * foot, PixelMetres(p) * far * 3.0);
                    width *= lerp(1.0, 0.025, smoothstep(3.20, 3.53, t));
                    q = float2((v.uv.y * 2.0 - 1.0) * 1.5, h);
                    float lean = smoothstep(1.70, 3.20, age) * 0.19;
                    lean -= smoothstep(3.25, 3.45, age) * 0.06 * lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
                    w = p + (float3(0, h + arriving * 1.05, 0) + side * q.x * width - right * h * lean) * alive;
                    w += (-forward * 3.0 - right * 0.25 + float3(0, -q.x * 2.0 - 1.2, 0)) * arriving * alive;
                }
                else if (mode < 1.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.2, 0);
                    float h = v.uv.x * r.size.w;
                    float a = h * 0.68 + 0.9 * sin(h * 0.61 + age * 1.8) - age * 2.3 + r.extra.z;
                    float rad = 1.15 + 0.85 * sin(h * 0.57 - age * 1.6 + r.extra.z) + h * 0.04;
                    float3 radial = right * cos(a) + forward * sin(a);
                    float3 tangent = normalize(float3(0, 1, 0) + rad * (-right * sin(a) + forward * cos(a)));
                    float lean = smoothstep(1.70, 3.20, age) * 0.19;
                    lean -= smoothstep(3.25, 3.45, age) * 0.06 * lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
                    float3 centre = p + float3(0, h, 0) + radial * rad - right * h * lean;
                    float3 side = normalize(cross(tangent, _WorldSpaceCameraPos - centre));
                    float taper = (0.3 + 0.7 * pow(abs(sin(h * 0.63 + age + r.extra.z)), 1.4))
                        * (1.0 - smoothstep(0.65, 1.0, v.uv.x));
                    q = float2(v.uv.y * 2.0 - 1.0, v.uv.x);
                    w = p + (centre - p + side * q.x * size * taper * 2.05) * alive;
                }
                else if (mode > 3.5 && mode < 4.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.35, 0);
                    float collapse = lerp(1.0, 0.20, smoothstep(3.26, 3.45, age));
                    w = CelBillboard(p, float2(q.x, q.y * 1.30 * collapse) * size * alive);
                }
                else if (mode > 5.5 && mode < 6.5)
                {
                    float a = v.uv.x * 6.283185307;
                    float ringDepth = r.extra.z > 0.5 ? 0.85 : 1.0;
                    float3 radial = right * cos(a) + forward * (sin(a) * ringDepth);
                    float3 tangent = -right * sin(a) + forward * (cos(a) * ringDepth);
                    float expansionDrag = r.motion.z > 0.0 ? r.motion.z : 0.8;
                    float radius = r.size.x + r.size.y * CelDragInt(expansionDrag, t);
                    float wave = sin(a * 7.0 + r.seed * 43.0) * 0.20 + sin(a * 13.0 + r.seed * 19.0) * 0.10;
                    float3 centre = p + radial * (radius + wave * 1.6);
                    centre.y += dot((centre - p).xz, r.motion.xy) + 0.40 + wave;
                    float3 side = normalize(cross(tangent, _WorldSpaceCameraPos - centre));
                    float width = lerp(0.15, 0.40, smoothstep(0.0, 0.22, t))
                        * (1.0 - smoothstep(0.25, 0.80, k) * 0.85);
                    q = float2(a, v.uv.y * 2.0 - 1.0);
                    w = p + (centre - p + side * q.y * width) * alive;
                }
                else if (mode > 6.5)
                {
                    p = CelPosition(r, t);
                    float angle = mode < 7.5 && r.extra.z > 0.5 ? 0.0 : t * r.motion.z;
                    q = float2(q.x * cos(angle) - q.y * sin(angle), q.x * sin(angle) + q.y * cos(angle));
                    float2 corner = v.uv * 2.0 - 1.0;
                    if (mode > 8.5) { corner.x += corner.y * r.extra.z; corner.y *= r.size.w; }
                    else if (mode > 7.5)
                    {
                        corner.y *= r.size.w * lerp(0.85, 1.05, smoothstep(0.25, 0.55, t));
                        corner.x *= 1.15;
                        if (r.extra.z > 0.5) corner *= 1.0 + 0.25 * (1.0 - smoothstep(0.35, 0.65, t));
                        float3 tangent = normalize(cross(float3(r.vel.x, 0, r.vel.z), float3(0, 1, 0)));
                        float2 screenTangent = float2(dot(tangent, UNITY_MATRIX_V[0].xyz), dot(tangent, UNITY_MATRIX_V[1].xyz));
                        screenTangent = normalize(screenTangent);
                        if (screenTangent.x < 0.0) screenTangent = -screenTangent;
                        corner = screenTangent * corner.x + float2(-screenTangent.y, screenTangent.x) * corner.y;
                    }
                    w = CelBillboard(p, corner * size * alive);
                    // The terminal burst sits at contact in screen space, with its lower rays clear of the terrain.
                    if (mode > 9.5) w += normalize(_WorldSpaceCameraPos - w) * 3.0;
                }
                else
                {
                    // The illuminated crater stays fixed while the beam traces its left rim.
                    if (mode > 2.5 && mode < 3.5 && far > 0.5)
                    {
                        float shutdown = smoothstep(3.17, 3.35, t) * (1.0 - smoothstep(4.5, 4.8, t));
                        size = lerp(size, 12.0, shutdown);
                    }
                    float stretch = r.size.w > 0.0 ? r.size.w : 1.0;
                    float widthScale = mode > 2.5 && mode < 3.5 ? 0.95 : 1.0;
                    w = p + (right * q.x * widthScale + forward * q.y * stretch) * size * alive;
                    w.y += dot((w - p).xz, r.motion.xy) + (mode > 5.5 ? 0.45 : 0.2);
                    // Grid vertices lift the broken rim above the terrain, while the inner bed stays low.
                    if ((mode > 2.5 && mode < 3.5) || (mode > 4.5 && mode < 5.5))
                    {
                        float2 groundQ = mode > 4.5 ? q * size / 6.7 : q;
                        float rad = length(groundQ), a = atan2(groundQ.y, groundQ.x);
                        float n = CelValue2(groundQ * 8.0 + r.seed * 17.0);
                        float at = mode < 3.5 ? 0.75 : 0.52;
                        float offset = (n - 0.5) * 0.07;
                        float lip = exp(-pow((abs(rad - at - offset) - (mode < 3.5 ? 0.14 : 0.045)) * 24.0, 2.0));
                        float rise = smoothstep(0.05, 0.30, t) * (0.08 + n * 0.22);
                        w.y += lip * rise * alive;
                    }
                    w += normalize(_WorldSpaceCameraPos - w) * 0.06;
                }
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.q = q;
                o.tone = r.tone;
                o.state = float4(mode, t, k, r.seed);
                o.shape = float4(size, age, r.extra.z, far);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            float Zig(float x, float seed)
            {
                float cell = floor(x), f = frac(x);
                return lerp(CelHash21(float2(cell, seed)), CelHash21(float2(cell + 1, seed)), f) * 2.0 - 1.0;
            }

            half4 Energy(half3 colour, float alpha, Varyings i)
            {
                return half4(lerp(CelAdditiveFog(colour, i.positionWS, i.fog), colour * 0.9, i.shape.w * 0.8), alpha);
            }

            half4 frag(Varyings i) : SV_Target
            {
                float mode = i.state.x, t = i.state.y, k = i.state.z, seed = i.state.w;
                float2 q = i.q;
                uint row = CelRowA(i.tone.x);
                if (mode < 0.5)
                {
                    float tick = floor(t * 12.0);
                    float bend = sin(q.y * 1.1 + tick) * 0.02;
                    float ignition = 1.0 - smoothstep(0.025, 0.20, t);
                    bend *= 1.0 - ignition;
                    float edge = 0.58 + (0.06 * Zig(q.y * 0.7 - t * 1.3, seed * 31.0) + 0.03 * Zig(q.y * 3.3, tick * 0.2)) * (1.0 - ignition);
                    float x = abs(q.x - bend);
                    float body = 1.0 - CelAAStep(edge, x);
                    float cyan = 1.0 - CelAAStep(edge * 0.74, x);
                    float core = 1.0 - CelAAStep(0.16 + 0.025 * Zig(q.y * 1.7 - t * 4.0, 71.0) * (1.0 - ignition), x);
                    half3 c = lerp(half3(0.08, 0.25, 1.0), half3(0.20, 0.85, 1.6), cyan);
                    float seam = (1.0 - CelAAStep(0.04, abs(q.x - 0.32 - Zig(q.y * 1.2 - t * 3.0, seed) * 0.11)))
                        * smoothstep(0.3, 0.65, CelValue2(float2(q.y * 1.4 - t * 3.0, seed * 31.0))) * (1.0 - ignition);
                    c = lerp(c, half3(0.08, 0.24, 0.65), seam);
                    c = lerp(c, half3(2.0, 4.0, 6.0), core);
                    float halo = exp(-x * x * 1.8) * 0.38 * (1.0 - body);
                    float arrival = 1.0 - smoothstep(0.025, 0.20, t);
                    float bottom = smoothstep(0.0, 0.12, q.y - arrival * (0.3 + x * x * 0.7));
                    float fade = 1.0 - smoothstep(0.975, 1.0, k);
                    return Energy((c * body + CelPal(row, 1) * halo) * i.tone.z * fade * bottom,
                        body * i.tone.z * fade * bottom, i);
                }
                if (mode < 1.5)
                {
                    float segment = smoothstep(-0.35, 0.15, sin(q.y * 18.0 - t * 4.0 + seed * 7.0));
                    float mask = (1.0 - CelAAStep(0.9, abs(q.x))) * segment;
                    float fade = smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(3.10, 3.36, i.shape.y));
                    half3 col = half3(0.86, 0.89, 0.91) * (0.9 + 0.1 * sin(q.y * 38.0 + seed));
                    float alpha = mask * i.tone.z * fade * 0.88;
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                float radius = length(q), angle = atan2(q.y, q.x);
                if (mode < 2.5)
                {
                    // A few broken accents inside the smoke, never a complete wire hoop.
                    float phase = seed * 17.0;
                    float rim = 0.87 + 0.018 * sin(angle * 5.0 + phase);
                    float width = 0.010 + 0.007 * saturate(0.5 + 0.5 * sin(angle * 11.0 - phase));
                    float stroke = 1.0 - smoothstep(width, width + 0.008, abs(radius - rim));
                    float arcs = smoothstep(-0.10, 0.40,
                        sin(angle * 3.0 + phase) + 0.35 * sin(angle * 7.0 - phase));
                    float a = stroke * arcs * (1.0 - k) * i.tone.z * 0.35;
                    return Energy(CelPal(row, 3) * a * i.tone.y, 0.0, i);
                }
                if (mode < 3.5)
                {
                    float worldRadius = radius * i.shape.x;
                    q *= i.shape.x / 3.7;
                    radius = length(q);
                    float a = atan2(-q.x, q.y);
                    if (a < 0.0) a += 6.283185;
                    float grown = lerp(0.30, 3.36, pow(saturate((t - 0.07) / 3.0), 0.88));
                    float arc = smoothstep(0.17, 0.36, a) * (1.0 - smoothstep(grown, grown + 0.16, a));
                    float rough = 0.025 * Zig(a * 5.0, seed * 81.0) + 0.016 * Zig(a * 12.0, seed * 43.0);
                    float ringAt = 0.75 + rough;
                    float distanceR = abs(radius - ringAt);
                    float bedWidth = i.shape.w < 0.5 && radius < ringAt ? 0.24 : 0.18;
                    float ring = 1.0 - CelAAStep(bedWidth + rough * 0.8 + i.shape.w * 0.025, distanceR);
                    float crackAngle = frac(a * 13.0 + 0.2 * Zig(radius * 17.0, seed)) - 0.5;
                    float cracks = (1.0 - CelAAStep(0.035, abs(crackAngle))) * step(ringAt, radius)
                        * (1.0 - smoothstep(0.92, 1.0, radius)) * (1.0 - smoothstep(3.2, 3.6, t));
                    float cover = max(ring, cracks) * arc;
                    float cooling = smoothstep(3.45, 6.45, t);
                    float heat = smoothstep(2.92, 3.18, t) * (1.0 - cooling);
                    float hot = (1.0 - CelAAStep(0.13 + rough * 0.3, distanceR)) * arc;
                    float noise = CelValue2(q * 8.0 + seed * 17.0) * 0.65 + CelValue2(q * 23.0 + seed) * 0.35;
                    float hotIslands = smoothstep(cooling * 0.98 - 0.06, cooling * 0.98 + 0.06, noise);
                    half3 black = half3(0.002, 0.003, 0.005);
                    half3 fire = lerp(half3(1.0, 0.004, 0.001), half3(4.2, 0.7, 0.025), smoothstep(0.3, 0.85, heat));
                    float whiteCore = (1.0 - CelAAStep(0.04 + noise * 0.03, distanceR)) * smoothstep(0.75, 0.95, heat);
                    fire = lerp(fire, half3(12.0, 14.0, 16.0), whiteCore);
                    fire *= heat * hot * hotIslands;
                    float fissure = 1.0 - CelAAStep(0.008, abs(radius - ringAt + Zig(a * 9.0, seed) * 0.07));
                    fissure *= arc * smoothstep(0.1, 0.4, noise) * smoothstep(0.4, 0.7, cooling)
                        * (1.0 - smoothstep(5.9, 6.45, t));
                    float halo = exp(-distanceR * distanceR * 350.0) * arc * heat * 0.18;
                    float rim = (1.0 - hot) * cover;
                    half3 c = black * cover + half3(0.012, 0.014, 0.02) * rim + fire
                        + half3(0.8, 0.08, 0.001) * halo + half3(0.75, 0.003, 0.001) * fissure;
                    // Close/Mid: the cooling surface becomes black crust with a connected network
                    // of molten seams. The field is stationary; heat retreats through it over time.
                    // Far retains its original low-frequency representation below.
                    [branch] if (i.shape.w < 0.5)
                    {
                        // Earlier parts of the sweep cool first; the most recent front tip stays hot longer.
                        float surfaceAge = t + lerp(0.15, -0.10, saturate((a - 0.30) / 3.0));
                        float crossR = radius - ringAt;
                        float2 crustUV = float2(a * 8.5, crossR * 35.0);
                        crustUV += float2(Zig(crossR * 13.0, seed * 73.0),
                            Zig(a * 13.0, seed * 29.0)) * 0.32;
                        float2 crustCell = floor(crustUV), crustF = frac(crustUV);
                        float nearest = 100.0, second = 100.0;
                        float2 nearestCell = 0.0;
                        [unroll] for (int cy = -1; cy <= 1; cy++)
                        [unroll] for (int cx = -1; cx <= 1; cx++)
                        {
                            float2 cell = float2(cx, cy);
                            float2 id = crustCell + cell;
                            float2 jitter = float2(CelHash21(id + seed * 41.0),
                                CelHash21(id + 17.3 + seed * 67.0));
                            float2 delta = cell + 0.18 + jitter * 0.64 - crustF;
                            float d = dot(delta, delta);
                            if (d < nearest) { second = nearest; nearest = d; nearestCell = id; }
                            else second = min(second, d);
                        }
                        float cellHeat = CelHash21(nearestCell + seed * 113.0);
                        float seamDistance = sqrt(second) - sqrt(nearest);
                        float coolingFront = smoothstep(4.25, 6.48, surfaceAge);
                        float seamWidth = lerp(0.08, 0.020, coolingFront);
                        float seams = 1.0 - CelAAStep(seamWidth, seamDistance);
                        float moltenBand = 1.0 - CelAAStep(0.10 + rough * 0.3, abs(crossR));
                        float spine = crossR + Zig(a * 14.0, seed * 19.0) * 0.038;
                        float coreWidth = lerp(0.045, 0.010, smoothstep(4.05, 4.95, surfaceAge));
                        float hotSpine = 1.0 - CelAAStep(coreWidth, abs(spine));
                        float broadHeat = 1.0 - smoothstep(4.35, 5.35, surfaceAge);
                        float litCells = 1.0 - smoothstep(0.60 + cellHeat * 0.35,
                            0.73 + cellHeat * 0.27, coolingFront);
                        float endHeat = 1.0 - smoothstep(5.35, 6.65, surfaceAge);
                        float hotStart = smoothstep(2.92, 3.18, t);
                        float patch = CelValue2(q * 23.0 + seed * 13.0) * 0.7 + CelValue2(q * 61.0) * 0.3;
                        float hotPatch = smoothstep(0.35 + coolingFront * 0.18, 0.52 + coolingFront * 0.18, patch);
                        float channel = 1.0 - CelAAStep(0.012 + broadHeat * 0.010,
                            abs(crossR + Zig(a * 9.0, seed * 39.0) * 0.060));
                        float islands = smoothstep(0.38, 0.62, CelValue2(float2(a * 6.0, crossR * 24.0) + seed));
                        // Keep the raised black tips, but inset the molten material from their caps.
                        float moltenArc = smoothstep(0.30, 0.46, a)
                            * (1.0 - smoothstep(grown - 0.14, grown - 0.02, a));
                        float molten = max(broadHeat * islands * 0.7,
                            max(seams * litCells * hotPatch, channel * litCells * smoothstep(0.20, 0.45, CelValue2(float2(a * 15.0, seed * 19.0))) * 0.65)) * moltenBand * moltenArc;
                        float core = hotSpine * moltenBand * moltenArc * (1.0 - smoothstep(4.40, 5.25, surfaceAge));
                        float fireNoise = 0.78 + 0.22 * CelValue2(q * 58.0 + seed * 19.0);
                        half3 warm = lerp(half3(4.5, 0.85, 0.22), half3(0.85, 0.14, 0.07),
                            smoothstep(5.15, 6.10, surfaceAge));
                        half3 hotColour = lerp(warm, half3(6.0, 3.0, 0.8), core * broadHeat);
                        float raisedEdge = (1.0 - moltenBand) * cover;
                        float facet = CelHash21(nearestCell + 9.7);
                        half3 crust = lerp(half3(0.0015, 0.0020, 0.0035),
                            half3(0.018, 0.022, 0.028), (0.35 + facet * 0.65) * raisedEdge);
                        float rimCap = smoothstep(0.865, 0.88, radius)
                            * (1.0 - smoothstep(0.895, 0.91, radius)) * cover * step(0.55, facet);
                        crust = lerp(crust, half3(0.22, 0.24, 0.27) * (0.50 + facet * 0.50), rimCap);
                        float glowStrength = hotStart * endHeat;
                        float glow = exp(-crossR * crossR * 95.0) * moltenArc
                            * (broadHeat * 0.11 + seams * litCells * moltenBand * 0.035);
                        c = crust * cover + hotColour * molten * fireNoise * glowStrength
                            + half3(1.0, 0.16, 0.008) * glow * glowStrength;
                    }

                    float active = (1.0 - smoothstep(3.17, 3.28, t)) * i.shape.w;
                    float pool = exp(-radius * radius * 2.3) * saturate(1.0 - radius);
                    float shock = (1.0 - CelAAStep(0.045, abs(radius - 0.8))) * saturate(1.0 - t);
                    c += (half3(0.12, 0.75, 2.3) * pool + half3(1.4, 2.4, 3.2) * shock) * active;
                    cover *= 1.0 - active;
                    float shutdown = smoothstep(3.17, 3.32, t) * (1.0 - smoothstep(4.0, 4.65, t)) * i.shape.w;
                    float sweepR = lerp(4.8, 10.5, saturate((t - 3.17) / 1.3));
                    float sweepBand = 1.0 - CelAAStep(max(0.16, fwidth(worldRadius) * 1.4), abs(worldRadius - sweepR));
                    float broken = smoothstep(-0.45, -0.1, sin(angle * 4.0 + t));
                    float sweepAlpha = sweepBand * broken * shutdown * 0.75;
                    c = c * (1.0 - sweepAlpha) + half3(0.55, 0.65, 0.75) * sweepAlpha;
                    cover = max(cover, sweepAlpha);
                    return half4(c, cover);
                }
                // Uses the existing mode-4 billboard and existing Cel/Zig helpers only.
                // All silhouettes are mathematical; no image/flipbook/reference sampling.
                // Deliberately unequal cubic sheets, with ash occupying OPEN parts of them,
                // replace both the thin radial fountain and the disconnected round ash lobes.
                if (mode < 4.5)
                {
                    float impactAge = i.shape.y;
                    float clock = impactAge * 1.85 + seed * 11.0;
                    half3 painted = 0.0;
                    float coverage = 0.0;
                    // One low torn ash mass, not five repeated circular knobs.
                    // The white core stays open; existing sheet ash faces still supply rising gray.
                    float ashSpan = 0.60;
                    float ashFloor = 0.014 + 0.006 * sin(q.x * 13.0 + clock);
                    float ashHeight = 0.13
                        + (CelValue2(float2(q.x * 5.0 + seed * 13.0, clock * 0.33)) - 0.5) * 0.10
                        + (CelValue2(float2(q.x * 11.0 - seed * 7.0, clock * 0.47)) - 0.5) * 0.06;
                    float ashTaper = sqrt(saturate(1.0 - (q.x / ashSpan) * (q.x / ashSpan)));
                    float ashTop = ashFloor + ashHeight * ashTaper;
                    float notchA = 0.23 + CelHash11(seed * 43.0) * 0.16;
                    float notchB = -0.23 - CelHash11(seed * 59.0) * 0.16;
                    ashTop -= 0.055 * (1.0 - smoothstep(0.012, 0.055, abs(q.x - notchA)));
                    ashTop -= 0.045 * (1.0 - smoothstep(0.012, 0.050, abs(q.x - notchB)));
                    float ashField = min(ashTop - q.y, q.y - ashFloor);
                    float ashMask = CelAAStep(0.0, ashField)
                        * (1.0 - CelAAStep(ashSpan, abs(q.x))) * smoothstep(0.07, 0.19, abs(q.x));
                    float ashLight = smoothstep(0.025, 0.15, q.y + 0.020 * sin(q.x * 11.0 + seed));
                    half3 lowAsh = lerp(half3(0.23, 0.24, 0.25), half3(0.38, 0.39, 0.38), ashLight);
                    painted = MixFog(lowAsh, i.fog) * ashMask;
                    coverage = ashMask;

                    // Seven rooted, torn curtains replace seven detached closed ellipses.
                    // Existing cycle, flight, envelope, roots and maximum tops/widths are retained.
                    float foamField = -10.0;
                    float foamHeight01 = 0.0;
                    float foamAcross = 0.0;
                    [unroll] for (int plume = 0; plume < 7; plume++)
                    {
                        float id = seed * 83.0 + plume * 13.7;
                        float cycle = frac(impactAge * (2.3 + plume * 0.09) + CelHash11(id));
                        float flight = smoothstep(0.0, 0.85, cycle);
                        float envelope = sqrt(smoothstep(0.0, 0.10, cycle)
                            * (1.0 - smoothstep(0.65, 1.0, cycle)));
                        float root = (plume - 3.0) * 0.14;
                        float2 centre = float2(root + sign(root) * sin(flight * 3.14159265) * 0.07,
                            0.07 + flight * (0.18 + CelHash11(id + 3.0) * 0.10));
                        centre.x += 0.025 * sin(clock * 2.0 + id);
                        float2 radii = float2(0.12 + CelHash11(id + 5.0) * 0.05,
                            0.10 + CelHash11(id + 7.0) * 0.08) * max(envelope, 0.01);

                        float rootY = 0.018;
                        float topY = centre.y + radii.y;
                        float height = max(topY - rootY, 0.03);
                        float u = saturate((q.y - rootY) / height);
                        float rootX = root + 0.025 * sin(clock * 2.0 + id);
                        float tipX = centre.x + radii.y * sign(root) * 0.22;
                        float spine = lerp(rootX, tipX, u)
                            + 0.025 * sin(u * 3.14159265) * sin(id + flight * 4.0);
                        float taper = sqrt(saturate((1.0 - u) / 0.18));
                        float halfWidth = radii.x * (0.45 + 0.55 * sin(u * 3.14159265)) * taper;
                        halfWidth *= 0.90 + 0.10 * Zig(u * 9.0 + flight, id);
                        float across = q.x - spine;
                        float d = min(halfWidth - abs(across), min(q.y - rootY, topY - q.y));

                        // A narrow cut opens to the crest, producing unequal ragged tongues.
                        // Its bottom stays above the connected lower fold, never a closed hole.
                        float cutStart = 0.46 + CelHash11(id + 23.0) * 0.15;
                        float cutSide = CelHash11(id + 29.0) < 0.5 ? -1.0 : 1.0;
                        float cutCentre = spine + cutSide * halfWidth
                            * (0.20 + 0.90 * smoothstep(0.65, 1.0, u));
                        float cutWidth = (0.012 + CelHash11(id + 37.0) * 0.010) * envelope;
                        float cut = min(cutWidth - abs(q.x - cutCentre), q.y - (rootY + cutStart * height));
                        d = min(d, -cut);
                        if (d > foamField)
                        {
                            foamHeight01 = u;
                            foamAcross = across / max(halfWidth, 0.008);
                        }
                        foamField = max(foamField, d);
                    }
                    float foam = CelAAStep(0.0, foamField);
                    // Pale crests and broad lower ash folds, without ellipse-normal banding.
                    float foamRim = (1.0 - smoothstep(0.004, 0.014, foamField))
                        * smoothstep(0.50, 0.92, foamHeight01);
                    half3 foamColour = lerp(half3(0.48, 1.05, 1.30), half3(1.0, 1.6, 2.0), foamRim);
                    float foldHeight = foamHeight01 + 0.08 * sin(foamAcross * 4.0 + clock);
                    float foamAsh = (1.0 - smoothstep(0.18, 0.40, foldHeight))
                        * smoothstep(0.10, 0.24, abs(q.x));
                    foamColour = lerp(foamColour, half3(0.32, 0.35, 0.36), foamAsh);
                    painted = painted * (1.0 - foam) + CelAdditiveFog(foamColour, i.positionWS, i.fog) * foam;
                    coverage = coverage + foam * (1.0 - coverage);

                    // Five principal curls plus two secondary paths lift out of the rounded body.
                    // The outer ones curl across the base while two others break upward. Their
                    // roots are spread over a contact edgeVector instead of converging at one point.
                    [unroll] for (int sheet = 0; sheet < 7; sheet++)
                    {
                        float id = seed * 137.0 + sheet * 19.37;
                        float phase = clock * (0.78 + 0.11 * sheet) + id;
                        float2 p0, p1, p2, p3;
                        float rootWidth, bellyWidth;
                        if (sheet == 0)
                        {
                            p0 = float2(-0.30, 0.015);
                            p1 = float2(-0.64, 0.045);
                            p2 = float2(-0.82, 0.30);
                            p3 = float2(-0.59, 0.46);
                            rootWidth = 0.075; bellyWidth = 0.052;
                        }
                        else if (sheet == 1)
                        {
                            p0 = float2(0.13, 0.025);
                            p1 = float2(0.30, 0.18);
                            p2 = float2(0.58, 0.56);
                            p3 = float2(0.38, 0.73);
                            rootWidth = 0.075; bellyWidth = 0.065;
                        }
                        else if (sheet == 2)
                        {
                            p0 = float2(-0.11, 0.005);
                            p1 = float2(-0.18, 0.14);
                            p2 = float2(-0.23, 0.48);
                            p3 = float2(-0.43, 0.66);
                            rootWidth = 0.095; bellyWidth = 0.085;
                        }
                        else if (sheet == 3)
                        {
                            p0 = float2(0.27, 0.005);
                            p1 = float2(0.49, 0.055);
                            p2 = float2(0.83, 0.15);
                            p3 = float2(0.77, 0.39);
                            rootWidth = 0.080; bellyWidth = 0.045;
                        }
                        else
                        {
                            p0 = float2(-0.035, 0.020);
                            p1 = float2(0.08, 0.13);
                            p2 = float2(-0.09, 0.34);
                            p3 = float2(0.08, 0.55);
                            rootWidth = 0.070; bellyWidth = 0.052;
                            if (sheet > 4)
                            {
                                // Spread the secondary sheets over the contact.
                                // Each has its own shoulder; none inherits the common p2.x=-0.09.
                                p0.x = (CelHash11(id + 7.0) - 0.5) * 1.08;
                                float rootSide = p0.x < 0.0 ? -1.0 : 1.0;
                                p3.x = clamp(p0.x + rootSide * (0.12 + CelHash11(id + 19.0) * 0.30), -0.86, 0.86);
                                p3.y = 0.22 + CelHash11(id + 31.0) * 0.42;
                                p1.x = lerp(p0.x, p3.x, 0.25) + (CelHash11(id + 11.0) - 0.5) * 0.10;
                                p1.y = 0.06 + p3.y * 0.18;
                                p2.x = lerp(p0.x, p3.x, 0.72) + (CelHash11(id + 23.0) - 0.5) * 0.18;
                                p2.y = p3.y * 0.66;
                                rootWidth = 0.045; bellyWidth = 0.035;
                            }
                        }

                        // Different smooth frequencies bend the surfaces rather than moving a
                        // rigid fan. No floor(time) pose changes or visibility discontinuities.
                        float lift = 0.86 + 0.14 * sin(phase * 2.71);
                        float cycle = frac(impactAge * (3.0 + sheet * 0.23) + CelHash11(id + 61.0));
                        float flight = smoothstep(0.0, 0.75, cycle);
                        float breath = (0.60 + 0.20 * sin(phase * 3.43 + 1.7))
                            * smoothstep(0.0, 0.12, cycle) * (1.0 - smoothstep(0.70, 1.0, cycle));
                        p2.y *= 0.45 + flight * 0.85;
                        p3.y *= 0.45 + flight * 0.85;
                        p3.x += sin(phase * 2.6) * flight * 0.12;
                        p0.x += 0.025 * sin(phase * 2.1);
                        p1.x += 0.045 * sin(phase * 2.4 + 0.7);
                        p2 += float2(0.065 * sin(phase * 2.2 + 1.8),
                                      0.040 * sin(phase * 2.9));
                        p3 += float2(0.075 * sin(phase * 2.6),
                                      0.045 * sin(phase * 3.1 + 0.9));
                        p2.y *= 0.82; p3.y *= 0.82;
                        p1.y *= lift; p2.y *= lift; p3.y *= lift;

                        float field = -10.0;
                        float winningU = 0.0, winningCross = 0.0;
                        float2 previous = p0;
                        // Five segments keep the broader splash surfaces curved.
                        // The best field retains intrinsic coordinates for coherent slab paint.
                        [unroll] for (int segment = 0; segment < 5; segment++)
                        {
                            float u1 = (segment + 1.0) / 5.0;
                            float v1 = 1.0 - u1;
                            float2 next = p0 * (v1 * v1 * v1)
                                + p1 * (3.0 * v1 * v1 * u1)
                                + p2 * (3.0 * v1 * u1 * u1)
                                + p3 * (u1 * u1 * u1);
                            float2 edgeVector = next - previous;
                            float line2 = max(dot(edgeVector, edgeVector), 0.00001);
                            float along = saturate(dot(q - previous, edgeVector) / line2);
                            float u = (segment + along) / 5.0;
                            float2 delta = q - lerp(previous, next, along);
                            float signedAcross = (edgeVector.x * delta.y - edgeVector.y * delta.x)
                                * rsqrt(line2);
                            float body = rootWidth * lerp(1.0, 0.65, u)
                                + bellyWidth * pow(max(sin(u * 3.14159265), 0.0), 0.80);
                            body *= breath * 0.85;
                            // Retain the broad fold, but let its final quarter end as a tongue.
                            body *= 1.0 - smoothstep(0.70, 1.0, u);
                            // Unequal torn margins and a broad belly. The end is a fine torn
                            // tongue, but most of the path stays filled and visibly substantial.
                            float tearPhase = 0.18 * sin(phase * 1.7);
                            float leftWidth = body * (0.78 + 0.22 * Zig(u * 9.0 + tearPhase, id));
                            float rightWidth = body * (1.08 + 0.27 * Zig(u * 7.0 - tearPhase, id + 31.0));
                            float width = signedAcross < 0.0 ? leftWidth : rightWidth;
                            float candidate = width - length(delta);
                            if (candidate > field)
                            {
                                field = candidate;
                                winningU = u;
                                winningCross = signedAcross / max(body, 0.008);
                            }
                            previous = next;
                        }

                        // A bite opens from ONE edge. It is not an interior oval hole and never
                        // turns the sheet into a closed outlined petal. Each bite has its own
                        // position and side, so the five silhouettes cannot form a radial star.
                        float biteSide = (sheet == 0 || sheet == 3) ? -1.0 : 1.0;
                        float biteAt = 0.38 + 0.22 * CelHash11(id + 8.0)
                            + 0.045 * sin(phase * 2.3);
                        float biteWindow = 1.0 - smoothstep(0.035, 0.105, abs(winningU - biteAt));
                        float biteBoundary = lerp(1.5, 0.03, biteWindow);
                        float bite = CelAAStep(biteBoundary, winningCross * biteSide);
                        float mask = CelAAStep(0.0, field) * (1.0 - bite);

                        // Gray-brown slabs share the strip silhouette and reach an open edge.
                        // Only the longer two sheets are predominantly white; low curling sheets
                        // carry larger ash faces. Paint follows the curve, not global screen y.
                        float ashSide = (sheet == 0 || sheet == 2) ? -1.0 : 1.0;
                        float ashBoundary = (sheet == 1 || sheet == 4) ? 0.62 : 0.30;
                        ashBoundary += 0.18 * sin(winningU * 9.0 + id)
                            + 0.10 * Zig(winningU * 12.0, id + 4.0);
                        float ashEnd = 0.60 + 0.14 * CelHash11(id + 17.0);
                        float ashFace = CelAAStep(ashBoundary, winningCross * ashSide)
                            * CelAAStep(0.06, winningU)
                            * (1.0 - CelAAStep(ashEnd, winningU)) * smoothstep(0.10, 0.28, abs(q.x));
                        float facet = CelAAStep(0.12, winningCross * ashSide
                            + winningU * 0.65 + 0.13 * sin(winningU * 12.0 + id));
                        half3 ash = lerp(half3(0.17, 0.16, 0.17),
                                         half3(0.37, 0.36, 0.34), facet);
                        float reflected = (1.0 - smoothstep(0.08, 0.27, abs(q.x)))
                            * (1.0 - smoothstep(0.10, 0.48, q.y));
                        ash += half3(0.035, 0.095, 0.13) * reflected;

                        // Mostly white paint with a restrained cold edge; don't make the whole
                        // material blue, and don't multiply by the beam's large tone.y gain.
                        float interior = CelAAStep(0.020, field);
                        half3 ejecta = lerp(half3(0.60, 1.8, 2.8),
                                            half3(2.5, 4.0, 5.0), interior);
                        half3 surface = lerp(CelAdditiveFog(ejecta, i.positionWS, i.fog),
                                            MixFog(ash, i.fog), ashFace);
                        painted = painted * (1.0 - mask) + surface * mask;
                        coverage = coverage + mask * (1.0 - coverage);
                    }

                    // Low irregular contact fill: a shallow white shelf, not a glowing ellipse.
                    float shelfSpan = 0.49 + 0.035 * sin(clock * 4.1);
                    float shelfY = 0.021 + 0.010 * Zig(q.x * 13.0, seed * 53.0)
                        + 0.008 * sin(q.x * 11.0 + clock * 4.7);
                    float shelfWidth = 0.022 * saturate(1.0 - pow(abs(q.x) / shelfSpan, 2.0));
                    float shelfField = min(shelfSpan - abs(q.x), shelfWidth - abs(q.y - shelfY));
                    float shelf = CelAAStep(0.0, shelfField);

                    // Open white lip: one low front arc plus a short unequal side curl.
                    // Neither path closes behind the beam, and both taper to zero at their ends.
                    float lipField = -10.0;
                    [unroll] for (int arc = 0; arc < 2; arc++)
                    {
                        float2 a0, a1, a2, a3;
                        if (arc == 0)
                        {
                            a0 = float2(-0.84, 0.16);
                            a1 = float2(-0.90, -0.080);
                            a2 = float2(0.25, -0.105);
                            a3 = float2(0.77, 0.105);
                        }
                        else
                        {
                            a0 = float2(0.44, 0.055);
                            a1 = float2(0.84, 0.080);
                            a2 = float2(0.91, 0.250);
                            a3 = float2(0.70, 0.285);
                        }
                        float curl = sin(clock * 3.3 + arc * 2.7);
                        a0.y += curl * 0.018;
                        a2 += float2(0.030 * curl, 0.025 * sin(clock * 4.1 + arc));
                        a3.y += 0.038 * sin(clock * 2.9 + arc * 3.7);
                        float2 previous = a0;
                        [unroll] for (int segment = 0; segment < 8; segment++)
                        {
                            float u1 = (segment + 1.0) * 0.125;
                            float v1 = 1.0 - u1;
                            float2 next = a0 * (v1 * v1 * v1)
                                + a1 * (3.0 * v1 * v1 * u1)
                                + a2 * (3.0 * v1 * u1 * u1) + a3 * (u1 * u1 * u1);
                            float2 edgeVector = next - previous;
                            float along = saturate(dot(q - previous, edgeVector)
                                / max(dot(edgeVector, edgeVector), 0.00001));
                            float u = (segment + along) * 0.125;
                            float uneven = smoothstep(-0.70, 0.60,
                                Zig(u * 9.0 + clock * 0.10, seed * 29.0 + arc));
                            float width = (arc == 0 ? 0.018 : 0.012)
                                * pow(max(sin(u * 3.14159265), 0.0), 0.6)
                                * (0.38 + 0.82 * uneven);
                            // One open interruption per existing arc; no extra path or loop.
                            float breakAt = 0.16 + CelHash11(seed * 17.0 + arc * 11.0) * 0.16;
                            float breakProfile = 1.0 - smoothstep(0.010, 0.030, abs(u - breakAt));
                            width -= breakProfile * (arc == 0 ? 0.040 : 0.025);
                            float d = width - length(q - lerp(previous, next, along));
                            lipField = max(lipField, d);
                            previous = next;
                        }
                    }
                    float front = max(shelf, CelAAStep(0.0, lipField));
                    half3 frontColour = CelAdditiveFog(half3(2.6, 3.8, 4.8), i.positionWS, i.fog);
                    painted = painted * (1.0 - front) + frontColour * front;
                    coverage = coverage + front * (1.0 - coverage);

                    // Hold the splash as the shaft narrows, then clear before the terminal star.
                    float fade = smoothstep(0.075, 0.18, impactAge)
                        * (1.0 - smoothstep(3.31, 3.45, impactAge)) * i.tone.z;
                    float glow = exp(-dot(q * float2(1.0, 1.7), q * float2(1.0, 1.7)) * 7.0);
                    painted += CelAdditiveFog(half3(0.025, 0.10, 0.19) * glow,
                        i.positionWS, i.fog) * (1.0 - coverage);
                    return half4(painted * fade, coverage * fade);
                }
                if (mode < 5.5)
                {
                    // Expand support for long rim projections without enlarging the crater or blue pool.
                    q *= i.shape.x / 6.7;
                    radius = length(q);
                    angle = atan2(q.y, q.x);
                    float fade = smoothstep(0.0, 0.035, k) * (1.0 - smoothstep(3.40, 3.51, i.shape.y));
                    float grain = 0.72 + 0.28 * CelValue2(q * 35.0 + CelValue2(q * 8.0));
                    float f = exp(-radius * radius * 1.8) * (1.0 - smoothstep(0.65, 1.0, radius));
                    float jagged = Zig(angle * 7.0, seed * 31.0) * 0.035 + Zig(angle * 21.0, seed * 17.0) * 0.018;
                    float rimAt = 0.52 + jagged;
                    float nearSide = 1.0 - smoothstep(0.04, 0.22, q.y);
                    float rimWidth = lerp(0.018, 0.060, smoothstep(0.40, 0.55, CelValue2(q * 14.0 + seed)));
                    float rim = (1.0 - CelAAStep(rimWidth + jagged * 0.3, abs(radius - rimAt))) * nearSide;
                    float edge = (1.0 - CelAAStep(0.008, abs(radius - rimAt + 0.022))) * nearSide;
                    float fissure = 0.0;
                    [unroll] for (int crack = 0; crack < 9; crack++)
                    {
                        float ca = crack < 3 ? -2.72 + crack * 0.16 : lerp(-2.25, -0.15, (crack - 3.0) / 5.0);
                        ca += (CelHash11(crack + seed * 43.0) - 0.5) * 0.16;
                        float pulse = 0.5 + 0.5 * sin((t - 2.0) * 6.2831853 + 0.30
                            + (CelHash11(crack * 7.0 + seed) - 0.5) * 0.80);
                        float activation = smoothstep(1.65, 2.05, t) * lerp(0.22, 1.0, pulse);
                        activation *= crack < 3 ? 1.0 - smoothstep(2.78, 3.12, t)
                            : lerp(0.22, 1.0, smoothstep(2.80, 3.10, t));
                        float targetSpread = crack < 3 ? 0.98 + CelHash11(crack * 11.0 + seed) * 0.14
                            : 0.88 + CelHash11(crack * 11.0 + seed) * 0.20;
                        float spread = lerp(0.58, targetSpread, activation);
                        float cr = saturate((radius - 0.50) / (spread - 0.50));
                        float bend = (CelHash11(crack + seed * 29.0) - 0.5) * 0.45 * cr;
                        bend += Zig(cr * 4.0, crack + seed * 13.0) * 0.11 * sin(cr * 3.14159265);
                        float da = abs(atan2(sin(angle - ca - bend), cos(angle - ca - bend)));
                        float width = (0.035 + CelHash11(crack + seed * 31.0) * 0.070)
                            * activation * pow(1.0 - cr, 0.80)
                            * (0.65 + 0.35 * CelValue2(float2(cr * 7.0, crack + seed * 19.0)));
                        fissure = max(fissure, (1.0 - CelAAStep(width, da)) * step(0.50, radius) * step(radius, spread));
                    }
                    float opaque = max(rim, fissure * nearSide) * smoothstep(0.06, 0.22, t);
                    half3 c = half3(0.035, 0.14, 0.32) * f * grain * (1.0 - opaque)
                        + half3(0.18, 0.45, 0.7) * edge
                        + half3(0.003, 0.006, 0.014) * opaque;
                    return half4(CelAdditiveFog(c, i.positionWS, i.fog) * fade, opaque * fade);
                }
                if (mode < 6.5)
                {
                    // Long periodic wind bands on the existing ring strip; no round-cell chain.
                    float a = q.x;
                    float phase = seed * 17.0;
                    float spine = 0.13 * sin(a * 3.0 + phase)
                        + 0.07 * sin(a * 7.0 - phase);
                    float crossBand = q.y - spine;
                    float halfWidth = 0.72 + 0.13 * sin(a * 5.0 + phase)
                        + 0.05 * sin(a * 11.0 - phase);
                    float mask = 1.0 - smoothstep(-0.12, 0.08, abs(crossBand) - halfWidth);
                    float arcField = sin(a * 3.0 + phase) + 0.32 * sin(a * 7.0 - phase);
                    float arcs = smoothstep(-0.62, -0.16, arcField);
                    mask *= arcs;

                    // Mostly cream with a shallow lower band, without spherical shadowing.
                    float lightBand = smoothstep(-0.50, 0.42, crossBand);
                    half3 col = lerp(CelPal(row, 2), CelPal(row, 3), lightBand * 0.65);
                    col = lerp(CelPal(row, 1), col, smoothstep(-0.78, -0.38, crossBand));
                    float accentAt = -0.24 + 0.04 * sin(a * 9.0 + phase);
                    float accent = (1.0 - smoothstep(0.055, 0.16, abs(crossBand - accentAt)))
                        * smoothstep(-0.10, 0.45, sin(a * 5.0 - phase));
                    col += half3(0.10, 0.105, 0.08) * accent;
                    float attack = i.shape.z > 0.5 ? smoothstep(0.0, 0.012, t) : smoothstep(0.0, 0.08, k);
                    float fade = attack * (1.0 - smoothstep(0.48, 0.72, k));
                    float alpha = mask * 0.82 * i.tone.z * fade;
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                if (mode > 9.5)
                {
                    float r = length(q);
                    float a = atan2(q.y, q.x);
                    float spoke = pow(abs(cos(a * 3.0 + seed * 3.0)), 18.0);
                    float reach = 0.18 + spoke * 0.72;
                    float mask = 1.0 - CelAAStep(reach, r);
                    float core = 1.0 - smoothstep(0.10, 0.40, r);
                    half3 c = lerp(half3(0.80, 2.4, 5.0), half3(4.0, 5.0, 6.0), core);
                    return Energy(c * mask, mask, i);
                }
                if (mode > 8.5)
                {
                    float curve = q.x + sin(q.y * 3.4 + seed * 13.0) * 0.24;
                    float width = 0.40 * pow(saturate(1.0 - q.y * q.y), 0.8);
                    float mask = (1.0 - CelAAStep(width, abs(curve))) * (1.0 - smoothstep(0.7, 1.0, k));
                    float core = 1.0 - CelAAStep(width * 0.45, abs(curve));
                    half3 c = lerp(half3(0.35, 1.5, 3.0), half3(2.4, 4.5, 6.0), core);
                    return Energy(c * mask, mask, i);
                }
                if (mode > 7.5)
                {
                    q *= 1.10;
                    // First shock: a low rolling billow grows into torn irregular puffs.
                    // The second ring retains its authored silhouette/lifecycle exactly.
                    bool firstRing = i.shape.z < 0.5;
                    float firstTear = smoothstep(0.45, 0.95, t);
                    if (firstRing)
                    {
                        float billowHeight = lerp(0.78, 0.92, smoothstep(0.08, 0.55, t));
                        q.x /= 1.08;
                        q.y = (q.y + 0.35) / billowHeight - 0.35;
                    }
                    float distanceField = 10.0;
                    float frontDepth = -1000.0;
                    float3 puffNormal = float3(0.0, 0.0, 1.0);
                    float breakup = smoothstep(0.45, 0.90, t);
                    float separation = smoothstep(0.30, 0.75, t);
                    float clearance = smoothstep(0.90, 1.18, t);
                    float expiry = smoothstep(0.90, 1.0, k);
                    float fragmentScale = lerp(1.0, 0.55, breakup) * (1.0 - max(clearance, expiry));
                    [unroll] for (int j = 0; j < 6; j++)
                    {
                        float id = seed * 127.0 + j * 17.0;
                        float2 centre = float2(CelHash11(id), CelHash11(id + 5.0)) * float2(0.95, 0.65)
                            - float2(0.475, 0.12);
                        centre += (float2(CelHash11(id + 13.0), CelHash11(id + 21.0)) - 0.5) * separation * 0.18;
                        float r = (0.28 + CelHash11(id + 9.0) * 0.28) * fragmentScale;
                        if (firstRing)
                        {
                            centre = float2(0.0, 0.18) + (centre - float2(0.0, 0.18))
                                * (1.0 + firstTear * 0.30);
                            r *= lerp(0.96, 0.87, smoothstep(0.20, 0.55, t));
                        }
                        float2 local = q - centre;
                        if (firstRing)
                        {
                            // Low-frequency rounded bites keep each fragment unequal; no holes
                            // or high-frequency noise, and no late replacement with identical hooks.
                            float a = atan2(local.y, local.x);
                            float roll = id + t * (0.45 + CelHash11(id + 37.0) * 0.60);
                            float contour = sin(a * 3.0 + roll) * 0.11
                                + sin(a * 5.0 - roll * 1.31) * 0.07
                                + sin(a * 8.0 + id * 1.73) * 0.035;
                            r *= 1.0 + contour * lerp(0.70, 1.35, firstTear);
                        }
                        float d = length(local) - r;
                        float unionWidth = firstRing ? lerp(0.055, 0.020, firstTear) : 0.10;
                        float blend = saturate(0.5 + 0.5 * (d - distanceField) / unionWidth);
                        distanceField = lerp(d, distanceField, blend) - unionWidth * blend * (1.0 - blend);
                        float frontZ2 = r * r - dot(local, local);
                        if (frontZ2 > 0.0)
                        {
                            float frontZ = sqrt(frontZ2);
                            float depth = frontZ + (CelHash11(id + 31.0) - 0.5) * r * 0.18;
                            float smoothFront = max(r * 0.42, 0.001);
                            float join = saturate(0.5 + 0.5 * (depth - frontDepth) / smoothFront);
                            float3 lobeNormal = float3(local, frontZ) / max(r, 0.0001);
                            puffNormal = normalize(lerp(puffNormal, lobeNormal, join));
                            frontDepth = lerp(frontDepth, depth, join) + smoothFront * join * (1.0 - join);
                        }
                    }
                    [branch] if (t > 0.65 && !firstRing)
                    {
                        // Positive rounded masses join at a neck, then separate along an open spine.
                        float handover = smoothstep(0.65, 0.86, t);
                        float neck = smoothstep(0.78, 1.08, t);
                        float lateScale = lerp(1.0, 0.46, smoothstep(0.92, 1.155, t))
                            * (1.0 - smoothstep(1.155, 1.18, t)) * sqrt(1.0 - smoothstep(0.94, 1.0, k));
                        float curveDistance = 10.0;
                        float curveFront = -1000.0;
                        float3 curveNormal = puffNormal;
                        [branch] if (lateScale > 0.0001)
                        {
                            float curveSeed = seed * 191.0;
                            float curveAngle = CelHash11(curveSeed + 1.0) * 6.283185307;
                            float2 axis = float2(cos(curveAngle), sin(curveAngle));
                            float2 offset = q - float2(0.0, 0.205);
                            float2 curvePoint = float2(dot(offset, axis), dot(offset, float2(-axis.y, axis.x))) / lateScale;
                            float halfSpan = lerp(0.36, 0.48, CelHash11(curveSeed + 3.0));
                            float bend = lerp(-0.34, 0.34, CelHash11(curveSeed + 5.0));
                            float roll = lerp(-0.30, 0.30, CelHash11(curveSeed + 7.0));
                            float tilt = lerp(-0.16, 0.16, CelHash11(curveSeed + 9.0));
                            float split = lerp(-0.20, 0.25, CelHash11(curveSeed + 11.0)) * halfSpan;
                            float gap = 0.14 * neck;
                            float leftMass = lerp(0.12, 0.20, CelHash11(curveSeed + 13.0));
                            float rightMass = lerp(0.09, 0.17, CelHash11(curveSeed + 19.0));
                            float taperFlip = CelHash11(curveSeed + 17.0) > 0.5 ? -1.0 : 1.0;
                            [unroll] for (int part = 0; part < 2; part++)
                            {
                                float lo = part == 0 ? -halfSpan : split + gap;
                                float hi = part == 0 ? split - gap : halfSpan;
                                [unroll] for (int lobe = 0; lobe < 5; lobe++)
                                {
                                    float along = lerp(lo, hi, lobe * 0.25);
                                    float u = along / halfSpan;
                                    float spine = bend * (1.0 - u * u) + roll * u * (1.0 - u * u) + tilt * u;
                                    float leftLobe = (u + 0.52) / 0.42;
                                    float rightLobe = (u - 0.38) / 0.36;
                                    float middleLobe = (u + 0.02) / 0.18;
                                    float width = (0.07 + leftMass * exp(-leftLobe * leftLobe)
                                        + rightMass * exp(-rightLobe * rightLobe) + 0.045 * exp(-middleLobe * middleLobe))
                                        * lerp(1.0, 0.70, (u * taperFlip + 1.0) * 0.5);
                                    float neckZone = 1.0 - smoothstep(0.0, 0.25, abs(along - split));
                                    width *= 1.0 - neckZone * neck * 0.75;
                                    float d = (length(curvePoint - float2(along, spine)) - width) * lateScale;
                                    curveDistance = min(curveDistance, d);
                                    float2 lobeLocal = curvePoint - float2(along, spine);
                                    float lobeZ2 = width * width - dot(lobeLocal, lobeLocal);
                                    if (lobeZ2 > 0.0)
                                    {
                                        float lobeZ = sqrt(lobeZ2);
                                        float smoothFront = max(width * 0.42, 0.001);
                                        float join = saturate(0.5 + 0.5 * (lobeZ - curveFront) / smoothFront);
                                        float2 localNormal = lobeLocal / max(width, 0.0001);
                                        float2 screenNormal = axis * localNormal.x
                                            + float2(-axis.y, axis.x) * localNormal.y;
                                        float3 lobeNormal = float3(screenNormal, lobeZ / max(width, 0.0001));
                                        curveNormal = normalize(lerp(curveNormal, lobeNormal, join));
                                        curveFront = lerp(curveFront, lobeZ, join) + smoothFront * join * (1.0 - join);
                                    }
                                }
                            }
                        }
                        distanceField = lerp(distanceField, curveDistance, handover);
                        puffNormal = normalize(lerp(puffNormal, curveNormal, handover));
                    }
                    float mask = 1.0 - smoothstep(-0.025, 0.025, distanceField);
                    // Broad cel patches follow individual cloud lobes, including the late curls.
                    float light = dot(puffNormal, normalize(float3(-0.36, 0.82, 0.45)));
                    light += (CelHash11(seed * 67.0 + 7.0) - 0.5) * 0.12
                        + sin(q.x * 6.0 + q.y * 3.0 + seed * 19.0) * 0.045;
                    float shadeAA = max(fwidth(light) * 0.9, 0.025);
                    half3 col = CelPal(row, 1);
                    col = lerp(col, CelPal(row, 2), smoothstep(0.12 - shadeAA, 0.12 + shadeAA, light));
                    col = lerp(col, CelPal(row, 3), smoothstep(0.66 - shadeAA, 0.66 + shadeAA, light));
                    float outline = smoothstep(-0.020, 0.0, distanceField);
                    col = lerp(col, half3(0.12, 0.14, 0.16), outline * 0.20) * half3(0.796, 0.780, 0.754);
                    float fade = (1.0 - smoothstep(0.985, 1.0, k))
                        * (1.0 - smoothstep(1.16, 1.18, t));
                    float alpha = mask * fade * smoothstep(0.0, i.shape.z > 0.5 ? 0.02 : 0.12, t);
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                float polygon = max(abs(q.x) * 0.85 + abs(q.y) * 0.45, abs(q.y));
                float maskRock = 1.0 - CelAAStep(0.78, polygon);
                half3 rock = lerp(CelPal(row, 0), CelPal(row, 2), step(0.1, q.x + q.y * 0.5));
                if (i.shape.z > 0.5)
                {
                    float stoneShape = max(length(q * float2(1.0, 1.15)) - 0.80, -q.y - 0.45);
                    maskRock = 1.0 - CelAAStep(0.0, stoneShape);
                    rock = lerp(CelPal(row, 0), CelPal(row, 1), step(-0.18, q.y + q.x * 0.2));
                    rock = lerp(rock, CelPal(row, 2), step(0.22, q.y - q.x * 0.45));
                    rock *= i.tone.y;
                }
                if (i.shape.z < -0.5)
                {
                    half3 glow = lerp(half3(0.25, 2.0, 4.0), half3(2.0, 4.0, 6.0),
                        1.0 - CelAAStep(0.38, abs(q.x) + abs(q.y)));
                    return half4(CelAdditiveFog(glow, i.positionWS, i.fog) * maskRock, maskRock);
                }
                return half4(MixFog(rock, i.fog) * maskRock, maskRock);
            }
            ENDHLSL
        }
    }
}
