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
                return -right * (sin(angle) * 3.0) + cross(right, float3(0, 1, 0)) * (cos(angle) * 2.24);
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
                    float foot = 1.0 + exp(-h * 1.35) * lerp(0.5, 1.8, far);
                    float width = max(size * arrival * foot, PixelMetres(p) * far * 3.0);
                    width *= lerp(1.0, 0.025, smoothstep(3.20, 3.53, t));
                    q = float2((v.uv.y * 2.0 - 1.0) * 1.5, h);
                    w = p + (float3(0, h + arriving * 1.05, 0) + side * q.x * width) * alive;
                }
                else if (mode < 1.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.2, 0);
                    float h = v.uv.x * r.size.w;
                    float a = h * 0.85 + 0.6 * sin(h * 0.73 + age * 1.8) - age * 3.4 + r.extra.z;
                    float rad = 0.85 + 0.65 * sin(h * 0.57 - age * 2.3 + r.extra.z) + h * 0.035;
                    float3 radial = right * cos(a) + forward * sin(a);
                    float3 tangent = normalize(float3(0, 1, 0) + rad * (-right * sin(a) + forward * cos(a)));
                    float3 centre = p + float3(0, h, 0) + radial * rad;
                    float3 side = normalize(cross(tangent, _WorldSpaceCameraPos - centre));
                    float taper = (0.3 + 0.7 * pow(abs(sin(h * 0.63 + age + r.extra.z)), 1.4))
                        * (1.0 - smoothstep(0.65, 1.0, v.uv.x));
                    q = float2(v.uv.y * 2.0 - 1.0, v.uv.x);
                    w = p + (centre - p + side * q.x * size * taper) * alive;
                }
                else if (mode > 3.5 && mode < 4.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.35, 0);
                    w = CelBillboard(p, float2(q.x, q.y * 0.85) * size * alive);
                }
                else if (mode > 6.5)
                {
                    p = CelPosition(r, t);
                    float angle = t * r.motion.z;
                    q = float2(q.x * cos(angle) - q.y * sin(angle), q.x * sin(angle) + q.y * cos(angle));
                    float2 corner = v.uv * 2.0 - 1.0;
                    if (mode > 7.5) corner.y *= r.size.w;
                    w = CelBillboard(p, corner * size * alive);
                }
                else
                {
                    if (mode > 4.5 && mode < 5.5)
                    {
                        float3 sweep = Sweep(age, right);
                        p += sweep + float3(0, dot(sweep.xz, r.motion.xy), 0);
                    }
                    if (mode > 2.5 && mode < 3.5 && far > 0.5)
                    {
                        float shutdown = smoothstep(3.17, 3.35, t) * (1.0 - smoothstep(4.5, 4.8, t));
                        size = lerp(size, 12.0, shutdown);
                    }
                    float stretch = r.size.w > 0.0 ? r.size.w : 1.0;
                    w = p + (right * q.x + forward * q.y * stretch) * size * alive;
                    w.y += dot((w - p).xz, r.motion.xy) + (mode > 5.5 ? 0.45 : 0.2);
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
                    float edge = 0.58 + 0.035 * Zig(q.y * 2.0, seed * 31.0 + tick * 0.33);
                    float x = abs(q.x - bend);
                    float body = 1.0 - CelAAStep(edge, x);
                    float cyan = 1.0 - CelAAStep(edge * 0.74, x);
                    float core = 1.0 - CelAAStep(0.19 + 0.025 * Zig(q.y * 1.7, tick + 71.0), x);
                    half3 c = lerp(CelPal(row, 1), CelPal(row, 2), cyan);
                    c = lerp(c, half3(8.0, 16.0, 32.0), core);
                    float halo = exp(-x * x * 1.8) * 0.38 * (1.0 - body);
                    float arrival = 1.0 - smoothstep(0.025, 0.20, t);
                    float bottom = smoothstep(0.0, 0.12, q.y - arrival * (0.3 + x * x * 0.7));
                    float fade = 1.0 - smoothstep(0.975, 1.0, k);
                    return Energy((c * body + CelPal(row, 1) * halo) * i.tone.z * fade * bottom,
                        body * i.tone.z * fade * bottom, i);
                }
                if (mode < 1.5)
                {
                    float mask = 1.0 - CelAAStep(0.9, abs(q.x));
                    float fade = smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(0.92, 1.0, k));
                    half3 col = lerp(half3(0.4, 1.2, 2.4), half3(6.0, 12.0, 24.0), 1.0 - abs(q.x));
                    return Energy(col * mask * i.tone.y * i.tone.z * fade, mask * 0.6 * fade, i);
                }
                float radius = length(q), angle = atan2(q.y, q.x);
                if (mode < 2.5)
                {
                    float rim = 0.87 + 0.012 * sin(angle * 9.0 + t * 7.0);
                    float ring = 1.0 - CelAAStep(0.012, abs(radius - rim));
                    float holes = step(0.04, CelHash21(float2(floor(angle * 18.0), floor(t * 9.0))));
                    float a = ring * holes * (1.0 - k) * i.tone.z;
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
                    float ring = 1.0 - CelAAStep(0.18 + rough * 0.8 + i.shape.w * 0.025, distanceR);
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
                if (mode < 4.5)
                {
                    float tick = floor(i.shape.y * 12.0);
                    float rays = 9.0;
                    float s = (angle * 0.159155 + 0.5) * rays;
                    float id = floor(s);
                    float lengthR = 0.5 + 0.5 * CelHash21(float2(id, tick + seed * 71.0));
                    float widthR = 0.2 + 0.24 * CelHash21(float2(id, tick + seed * 41.0));
                    float ray = saturate(1.0 - abs(frac(s) - 0.5) / widthR);
                    float reach = lerp(0.24, lengthR, pow(ray, 1.3));
                    reach *= q.y < 0.0 ? 0.26 : 1.0;
                    float f = saturate(1.0 - radius / max(reach, 0.01));
                    float baseSplash = saturate(1.0 - length(float2(q.x / 0.72, (q.y - 0.02) / 0.22)));
                    float tongues = 0.0;
                    [unroll] for (int j = 0; j < 5; j++)
                    {
                        float idTongue = j + seed * 71.0 + tick * 0.15;
                        float side = (j - 2.0) * 0.25;
                        float top = 0.4 + CelHash11(idTongue) * 0.5;
                        float curve = side * (0.45 + q.y * 1.2) + sin(q.y * 5.0 + idTongue) * q.y * 0.1;
                        float width = 0.11 * saturate(1.0 - q.y / top);
                        tongues = max(tongues, (1.0 - CelAAStep(width, abs(q.x - curve)))
                            * step(0.0, q.y) * (1.0 - smoothstep(top - 0.1, top, q.y)));
                    }
                    f = max(f, max(baseSplash, tongues * 0.75));
                    float mask = CelAAStep(0.02, f);
                    float halo = exp(-dot(q, q) * 13.0) * 0.2;
                    half3 c = lerp(half3(0.3, 1.5, 3.0), half3(8, 16, 24), CelAAStep(0.12, f)) * mask
                        + CelPal(row, 2) * halo;
                    float fade = i.shape.y > 3.4 ? 1.0 : 1.0 - smoothstep(3.05, 3.36, i.shape.y);
                    return Energy(c * i.tone.z * fade, mask * fade, i);
                }
                if (mode < 5.5)
                {
                    float f = exp(-radius * radius * 3.5) * saturate(1.0 - radius);
                    float grain = 0.88 + 0.12 * CelValue2(q * 16.0);
                    float fade = smoothstep(0.0, 0.045, k) * (1.0 - smoothstep(0.91, 1.0, k));
                    float id = floor((angle + 3.14159) * 1.7507);
                    float sector = (angle + 3.14159) * 1.7507 + Zig(radius * 9.0, seed + id * 7.0) * 0.35;
                    float broken = step(0.35, CelHash21(float2(id, floor(radius * 18.0))));
                    float cracks = (1.0 - CelAAStep(0.018, abs(frac(sector) - 0.5))) * broken
                        * smoothstep(0.25, 0.4, radius) * (1.0 - smoothstep(0.65, 0.85, radius));
                    float innerRim = 0.23 + 0.025 * Zig(angle * 3.0, seed + floor(t * 8.0));
                    float splashRing = 1.0 - CelAAStep(0.025, abs(radius - innerRim));
                    half3 c = half3(0.04, 0.18, 1.4) * f + half3(0.1, 0.4, 1.8) * f * f
                        + half3(4, 8, 14) * splashRing
                        + half3(0.3, 0.8, 1.7) * cracks * 0.6;
                    return Energy(c * i.tone.y * i.tone.z * grain * fade, 0.0, i);
                }
                if (mode < 6.5)
                {
                    float band = 0.0;
                    [unroll] for (int j = 0; j < 2; j++)
                    {
                        float along = frac(angle * 0.318309886 + seed * 0.17 + j * 0.31 - t * 0.16);
                        float taper = smoothstep(0.0, 0.12, along) * (1.0 - smoothstep(0.64, 1.0, along));
                        float centreR = 0.88 - j * 0.07 + sin(angle * 3.0 + j * 1.8 + seed * 3.0) * 0.015;
                        float width = lerp(0.075, 0.018, k) * taper * (1.0 - j * 0.45);
                        float stroke = (1.0 - CelAAStep(max(width, 0.0005), abs(radius - centreR))) * step(0.01, taper);
                        band = max(band, stroke);
                    }
                    float breakup = smoothstep(0.42, 0.92, k);
                    float fragments = CelAAStep(breakup * 0.98, CelValue2(q * 20.0 + seed * 17.0));
                    float alpha = band * fragments * i.tone.z * smoothstep(0.0, 0.06, k)
                        * (1.0 - smoothstep(0.90, 1.0, k));
                    return half4(MixFog(CelPal(row, 2), i.fog) * alpha, alpha);
                }
                if (mode > 7.5)
                {
                    float distanceField = 10.0;
                    float breakup = smoothstep(0.18, 0.60, t);
                    float separation = smoothstep(0.30, 0.75, t);
                    float clearance = smoothstep(0.90, 1.18, t);
                    float expiry = smoothstep(0.90, 1.0, k);
                    float fragmentScale = lerp(1.0, 0.48, breakup) * (1.0 - max(clearance, expiry));
                    [unroll] for (int j = 0; j < 6; j++)
                    {
                        float id = seed * 127.0 + j * 17.0;
                        float2 centre = float2(CelHash11(id), CelHash11(id + 5.0)) * float2(0.95, 0.65)
                            - float2(0.475, 0.12);
                        centre += (float2(CelHash11(id + 13.0), CelHash11(id + 21.0)) - 0.5) * separation * 0.18;
                        float r = (0.28 + CelHash11(id + 9.0) * 0.28) * fragmentScale;
                        float2 local = q - centre;
                        float d = length(local) - r;
                        distanceField = min(distanceField, d);
                    }
                    [branch] if (t > 0.65)
                    {
                        // Positive rounded masses join at a neck, then separate along an open spine.
                        float handover = smoothstep(0.65, 0.86, t);
                        float neck = smoothstep(0.78, 1.08, t);
                        float lateScale = lerp(1.0, 0.46, smoothstep(0.92, 1.155, t))
                            * (1.0 - smoothstep(1.155, 1.18, t)) * sqrt(1.0 - smoothstep(0.94, 1.0, k));
                        float curveDistance = 10.0;
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
                                }
                            }
                        }
                        distanceField = lerp(distanceField, curveDistance, handover);
                    }
                    float mask = 1.0 - smoothstep(-0.025, 0.025, distanceField);
                    float bandHeight = q.y + q.x * 0.16
                        + sin(q.x * 4.0 + seed * 17.0) * 0.12
                        + sin(q.x * 8.0 + seed * 31.0) * 0.05;
                    half3 col = CelPal(row, 1);
                    col = lerp(col, CelPal(row, 2), CelAAStep(-0.12, bandHeight));
                    col = lerp(col, CelPal(row, 3), CelAAStep(0.36, bandHeight));
                    float fade = (1.0 - smoothstep(0.985, 1.0, k))
                        * (1.0 - smoothstep(1.16, 1.18, t));
                    float alpha = mask * fade;
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                float polygon = max(abs(q.x) * 0.85 + abs(q.y) * 0.45, abs(q.y));
                float maskRock = 1.0 - CelAAStep(0.78, polygon);
                half3 rock = lerp(CelPal(row, 0), CelPal(row, 2), step(0.1, q.x + q.y * 0.5));
                return half4(MixFog(rock, i.fog) * maskRock, maskRock);
            }
            ENDHLSL
        }
    }
}
