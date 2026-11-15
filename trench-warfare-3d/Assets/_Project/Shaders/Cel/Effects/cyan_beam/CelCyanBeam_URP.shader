// Phase: CelFx cyan-beam lab effect, adapted from the blue-laser lane's ribbon and billboard modes.
// size.xyz: beam width / dot radius over life. size.w >= 0: beam; < 0: dot, magnitude is minimum far radius in px.
// extra for beams: x arc strength, y style (1 bands, 2 braid: e01), z/w leading end at birth/death (fraction of the span).
// _TWCyanLens (global; CyanBeamRecipe.Lens, the Range only): the lens vignette, x strength x 3, y falloff power; 0 none.
// extra for dots: x mode (0 star, 1 bloom, 2 ground pool, 3 atmosphere, 4 spike burst, 5 fleck), y softness/root width, z points or pulse, w core.
Shader "TW/Cel/CyanBeam"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+21" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelCyanBeam"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            #define BEAM_PAD 8.0

            float4 _TWCyanLens;

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 beam : TEXCOORD0;
                nointerpolation float4 tone : TEXCOORD1;
                nointerpolation float4 shape : TEXCOORD2;
                float4 more : TEXCOORD3;
                float3 positionWS : TEXCOORD4;
                nointerpolation float4 centerRadius : TEXCOORD5;
            };

            float PixelMetres(float3 samplePosition)
            {
                return distance(_WorldSpaceCameraPos, samplePosition) * 2.0 / max(abs(UNITY_MATRIX_P[1][1]) * _ScreenParams.y, 400.0);
            }

            Varyings vert(Attributes input, uint instanceID : SV_InstanceID)
            {
                CelRecord record = CelFetch(instanceID);
                float age = CelAge(record), life = CelLife01(record), alive = CelAlive(record);
                float far = saturate(_CelLod.x - 1.0);
                Varyings output;
                if (record.size.w < 0.0)
                {
                    float3 center = CelPosition(record, age);
                    float mode = record.extra.x;
                    if (mode > 4.5) center += record.axis * (0.3 + age * 1.3);
                    float minPx = mode < 1.5 ? -record.size.w * saturate(_CelLod.x * 0.5) : 0.0;
                    float radius = max(CelCurve(record.size.xyz, life), minPx * PixelMetres(center)) * alive;
                    float2 corner = input.uv * 2.0 - 1.0;
                    // Energy sits just in front of the contact surface, instead of being cut out by the dummy.
                    bool groundPool = mode > 1.5 && mode < 2.5;
                    float3 litCenter = center + normalize(_WorldSpaceCameraPos - center) * (groundPool ? radius + 0.6 : 0.7);
                    float quadRadius = groundPool && record.extra.z < 0.0 ? radius * 1.35 : radius;
                    float3 world = CelBillboard(litCenter, corner * quadRadius);
                    output.positionCS = TransformWorldToHClip(world);
                    output.positionWS = world;
                    output.beam = float4(corner, record.seed, -1.0);
                    output.tone = float4(record.tone.xyz, CelCurve(record.erode.xyz, life));
                    output.shape = record.extra;
                    output.more = float4(ComputeFogFactor(output.positionCS.z), far, life, age);
                    output.centerRadius = float4(center, radius);
                    return output;
                }
                float3 span = record.vel * record.life;
                float lengthB = max(length(span), 1e-4);
                float3 direction = span / lengthB;
                float along = input.uv.x * lerp(record.extra.z, record.extra.w, life);
                float3 centerB = record.pos + span * along;
                float3 side = cross(direction, normalize(_WorldSpaceCameraPos - centerB));
                float sideLength = dot(side, side);
                side = sideLength > 1e-6 ? side * rsqrt(sideLength) : UNITY_MATRIX_V[0].xyz;
                float envelope = min(saturate(age / 0.16), saturate((record.life - age) / 0.27));
                float width = CelCurve(record.size.xyz, life) * envelope;
                // A little convergence preserves the near shaft's broad screen silhouette.
                float scale = pow(clamp(PixelMetres(centerB) / max(PixelMetres(record.pos + span), 1e-5), 0.15, 2.0), 0.22);
                float halfWidth = max(width * scale * 0.5, PixelMetres(centerB) * 1.3 * far) * alive;
                float across = input.uv.y * 2.0 - 1.0;
                float3 worldB = centerB + side * across * halfWidth * BEAM_PAD;
                output.positionCS = TransformWorldToHClip(worldB);
                output.positionWS = worldB;
                output.beam = float4(along, across * BEAM_PAD, along * lengthB, max(halfWidth, 1e-4));
                output.tone = float4(record.tone.xyz, CelCurve(record.erode.xyz, life));
                output.shape = float4(record.extra.x * max(0.18, 1.0 - _CelLod.x * 0.65), lengthB, record.seed, record.extra.y);
                output.more = float4(ComputeFogFactor(output.positionCS.z), far, life, age);
                // e01: x = +1 where +across is up on the screen (the braid's deep-blue edge is the LOWER one)
                output.centerRadius = float4(dot(side, UNITY_MATRIX_V[1].xyz) >= 0.0 ? 1.0 : -1.0, 0.0, 0.0, 0.0);
                return output;
            }

            float Zig(float along, float cells, float salt)
            {
                float cell = along * cells;
                float index = floor(cell), part = frac(cell);
                return lerp(CelHash21(float2(index, salt)), CelHash21(float2(index + 1.0, salt)), part) * 2.0 - 1.0;
            }

            // e01: the colour as it has to be written for the Range's lens vignette to leave it as the clip shows it
            // (neon_acid's Unvignette, a01-a29: as far as white allows, the hue kept).
            half3 Unvignette(half3 col, float2 pixel)
            {
                if (_TWCyanLens.x <= 0.0) return col;
                float2 d = abs(pixel / _ScaledScreenParams.xy - 0.5) * _TWCyanLens.x;
                float keep = lerp(0.08, 1.0, pow(saturate(1.0 - dot(d, d)), max(_TWCyanLens.y, 0.01)));
                float3 lin = SRGBToLinear(max((float3)col, 0.0));
                float top = max(lin.r, max(lin.g, lin.b));
                float lift = min(1.0 / keep, 1.0 / max(top, 1e-3));
                return (half3)LinearToSRGB(min(lin * max(lift, 1.0), 1.0));
            }

            half3 AddFog(half3 color, Varyings input)
            {
                return lerp(CelAdditiveFog(color, input.positionWS, input.more.x), color * 0.9, input.more.y * 0.85);
            }

            float ImpactCuts(float2 samplePoint, float seed)
            {
                float shift = (seed - 0.5) * 0.15;
                float firstCut = 1.0 - CelAAStep(0.095, abs(samplePoint.y + samplePoint.x * 0.65 - 0.12 - shift));
                float secondCut = (1.0 - CelAAStep(0.075, abs(samplePoint.y - samplePoint.x * 0.4 + 0.24))) * step(samplePoint.x, 0.25);
                float lower = 1.0 - smoothstep(0.18, 0.72, -samplePoint.y) * 0.65;
                return (1.0 - max(firstCut, secondCut) * 0.94) * lower;
            }

            float OpeningReveal(float ageFromBeamStart)
            {
                return smoothstep(0.08333333, 0.20, ageFromBeamStart);
            }

            half4 Dot(Varyings input, uint row)
            {
                float2 samplePosition = input.beam.xy;
                float radius = length(samplePosition), mode = input.shape.x;
                float erosion = saturate(input.tone.w);
                if (mode > 1.5 && mode < 2.5)
                {
                    float2 uv = input.positionCS.xy / _ScaledScreenParams.xy;
                    float depth = SampleSceneDepth(uv);
                    #if !UNITY_REVERSED_Z
                    depth = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, depth);
                    #endif
                    float3 surface = ComputeWorldSpacePosition(uv, depth, UNITY_MATRIX_I_VP);
                    float3 delta = surface - input.centerRadius.xyz;
                    float groundRadius = length(delta.xz) / max(input.centerRadius.w, 0.01);
                    float glow = exp(-groundRadius * groundRadius * input.shape.y) * saturate(1.0 - groundRadius);
                    if (input.shape.z < 0.0)
                    {
                        glow = exp(-groundRadius * groundRadius * input.shape.y) * (1.0 - smoothstep(0.0, 1.0, groundRadius));
                        float quadEdge = 1.0 - smoothstep(0.82, 1.0, max(abs(samplePosition.x), abs(samplePosition.y)));
                        glow *= quadEdge * smoothstep(0.0, 0.10, input.more.w);
                    }
                    glow *= exp(-delta.y * delta.y * 1.3) * (1.0 - erosion) * saturate((1.0 - input.more.z) * 6.0);
                    half3 color = CelPal(row, 2) * glow + CelPal(row, 3) * glow * glow;
                    if (input.shape.z >= 0.0)
                    {
                        // Ground records carry their birth offset so both pools share one effect clock.
                        float groundTime = input.more.w + input.shape.z;
                        if (groundTime < 0.42)
                            color *= smoothstep(0.29166667, 0.41666667, groundTime);
                        if (groundTime > 5.416667)
                            color *= 1.0 - smoothstep(5.416667, 5.95, groundTime);
                    }
                    return half4(CelAdditiveFog(color * input.tone.z, surface, input.more.x), 0.0);
                }
                if (mode > 2.5 && mode < 3.5)
                {
                    float phase = floor(_CelNow * 12.0);
                    float clouds = 0.75 + 0.28 * CelValue2(samplePosition * 6.0 + phase * 0.17);
                    float glow = exp(-radius * radius * input.shape.y) * saturate((1.0 - radius) * 4.0);
                    float pulse = input.shape.z > 0.5 ? pow(sin(input.more.z * 3.14159), 1.3) : 0.24;
                    pulse *= saturate(input.more.w / 0.12) * saturate((1.0 - input.more.z) * 6.0);
                    half3 color = CelPal(row, 2) * glow * clouds * pulse * 1.35;
                    float openingAge = input.more.w + (input.shape.z > 0.5 ? 0.03 : 0.05);
                    if (input.more.y < 0.5 && openingAge < 0.20) color *= OpeningReveal(openingAge);
                    return half4(AddFog(color * input.tone.z, input), 0.0);
                }
                if (mode > 0.5 && mode < 1.5)
                {
                    float glow = exp(-radius * radius * max(input.shape.y, 1.0)) * saturate(1.0 - radius) * (1.0 - erosion);
                    glow *= saturate((1.0 - input.more.z) * 6.0);
                    half3 color = CelPal(row, 2) * glow + CelPal(row, 3) * glow * glow;
                    float coreRadius = input.shape.w * (1.0 - erosion * 0.6);
                    if (coreRadius > 0.0)
                    {
                        bool impactBloom = input.more.y < 0.5 && abs(input.shape.y - 2.0) < 0.01;
                        if (impactBloom) coreRadius *= 0.64;
                        float core = 1.0 - smoothstep(coreRadius * 0.65, coreRadius, radius);
                        if (impactBloom) core *= ImpactCuts(samplePosition / max(coreRadius, 0.01), input.beam.z);
                        color = lerp(color, CelPalCore(row).rgb * input.tone.y, core);
                    }
                    if (input.more.y < 0.5 && abs(input.shape.y - 2.0) < 0.01 && input.more.w < 0.15)
                        color *= OpeningReveal(input.more.w + 0.05);
                    return half4(AddFog(color * input.tone.z, input), 0.0);
                }
                if (mode > 3.5 && mode < 4.5)
                {
                    float rays = max(input.shape.z, 3.0);
                    float salt = input.beam.z * 93.0;
                    float angle = frac(atan2(samplePosition.y, samplePosition.x) * 0.159155 + 0.5 + input.beam.z * 3.7);
                    float sector = angle * rays, index = floor(sector);
                    float offset = (CelHash21(float2(index, salt)) - 0.5) * 0.45;
                    float rayLength = 0.2 + 0.8 * CelHash21(float2(index, salt + 7.0));
                    bool impactBlades = input.more.y < 0.5 && rays < 8.5;
                    if (impactBlades)
                        rayLength *= lerp(0.42, 1.0, smoothstep(-0.65, 0.15, samplePosition.y / max(radius, 0.001)));
                    float bend = max(radius - 0.35, 0.0) * (CelHash21(float2(index, salt + 3.0)) - 0.5) * 0.25;
                    float lateral = abs(sector - index - 0.5 - offset + bend) * 6.28318 / rays * radius;
                    float rayWidth = max(input.shape.y, 0.008) * saturate(1.0 - radius / rayLength);
                    if (impactBlades)
                        rayWidth *= 0.65 + 1.05 * CelHash21(float2(index, salt + 11.0));
                    float ray = (1.0 - CelAAStep(rayWidth, lateral)) * step(radius, rayLength);
                    float heart = 1.0 - smoothstep(input.shape.w * 0.7, input.shape.w, radius);
                    if (impactBlades) heart *= ImpactCuts(samplePosition / max(input.shape.w, 0.01), input.beam.z);
                    float white = saturate(1.0 - radius / max(rayLength * 0.43, 1e-4));
                    if (impactBlades)
                        white = saturate(1.0 - radius / max(rayLength * (0.16 + 0.12 * CelHash21(float2(index, salt + 15.0))), 1e-4));
                    half3 color = lerp(CelPal(row, 2), CelPalCore(row).rgb * input.tone.y, CelAAStep(0.4, white));
                    color *= max(ray, heart) * input.tone.z * (1.0 - erosion);
                    float rayGlow = (1.0 - smoothstep(rayWidth, rayWidth + 0.028, lateral)) * step(radius, rayLength);
                    if (impactBlades)
                    {
                        float tipFade = saturate((1.0 - radius / rayLength) * 12.0);
                        rayGlow = (1.0 - CelAAStep(rayWidth * 1.8, lateral)) * tipFade * step(radius, rayLength);
                    }
                    color += CelPal(row, 2) * rayGlow * 0.65 * (1.0 - erosion);
                    return half4(AddFog(color, input), 0.0);
                }
                if (mode > 4.5)
                {
                    float angle = input.beam.z * 39.0 + input.more.w * (input.beam.z - 0.5) * 3.0;
                    float2 rotated = float2(samplePosition.x * cos(angle) - samplePosition.y * sin(angle),
                        samplePosition.x * sin(angle) + samplePosition.y * cos(angle));
                    float shard = abs(rotated.x) * (0.75 + input.beam.z * 0.3) + abs(rotated.y) * 2.3;
                    float field = saturate(1.0 - shard);
                    float mask = CelAAStep(max(erosion * 0.55, 0.03), field) * saturate((1.0 - input.more.z) * 5.0);
                    half3 color = lerp(CelPal(row, 2), CelPalCore(row).rgb * input.tone.y, CelAAStep(0.62, field));
                    return half4(AddFog(color * mask * input.tone.z, input), 0.0);
                }
                if (mode < 0.5 && input.more.y < 0.5)
                {
                    samplePosition *= 1.35;
                    radius = length(samplePosition);
                }
                float spikes = input.shape.z > 0.5
                    ? pow(abs(cos(atan2(samplePosition.y, samplePosition.x) * input.shape.z * 0.5)), max(input.shape.w, 1.0)) : 1.0;
                float field = saturate(1.0 - radius / lerp(0.3, 1.0, spikes));
                float mask = CelAAStep(max(erosion, 0.02), field);
                if (mode < 0.5 && input.more.y < 0.5)
                    mask *= ImpactCuts(samplePosition, input.beam.z);
                if (mode < 0.5 && input.more.y < 0.5 && input.more.w < 0.20)
                    mask *= OpeningReveal(input.more.w);
                return half4(AddFog(CelRamp(field, row, input.tone.y) * mask * input.tone.z, input), 0.0);
            }

            half4 frag(Varyings input) : SV_Target
            {
                uint row = CelRowA(input.tone.x);
                if (input.beam.w < 0.0) return Dot(input, row);
                float along = input.beam.x, across = input.beam.y;
                float tick = floor(_CelNow * 12.0);
                float salt = input.shape.z * 97.0 + tick * 13.7;
                float onePixel = PixelMetres(input.positionWS) / max(input.beam.w, 1e-5);
                float ragged = 0.14 * Zig(along, 33.0, salt + 2.0) + 0.055 * Zig(along, 91.0, salt + 5.0);
                float halfWidth = 1.0 + ragged;
                float centerline = Zig(along, 13.0, salt + 6.0) * 0.12 * saturate((1.0 - along) * 15.0);
                float distanceAxis = abs(across - centerline);
                float outerWidth = halfWidth * (1.16 + 0.13 * Zig(along, 19.0, salt + 9.0));
                float outer = 1.0 - CelAAStep(outerWidth, distanceAxis);
                float cyan = 1.0 - CelAAStep(halfWidth, distanceAxis);
                float core = 1.0 - CelAAStep(halfWidth * (0.52 + 0.09 * Zig(along, 24.0, salt)), distanceAxis);
                half3 body = lerp(CelPal(row, 0), CelPal(row, 2), cyan);
                body = lerp(body, CelPalCore(row).rgb * input.tone.y, core);
                float cover = outer * input.tone.z;
                half3 added = 0.0;
                // e01: the clip's held beam is a BRAID (spec/eye/cyan_beam.md): 3-6 white strands, each a soft tube
                // with a pale-cyan sheen, weaving smoothly (wavelength 1-2 beam widths, amplitude ~ the half width)
                // and crossing, over a cyan fill with deep-blue patches between them and a deep-blue lower edge. They
                // drift coherently (on twos), they do not re-randomise. The old bands drew one flat cream core 0.52
                // of the width wide: 'too white, hides the detail' (owner 2026-11-13).
                bool braid = input.shape.w > 1.5;
                if (braid)
                {
                    float metres = input.beam.z;
                    float t = tick / 12.0;
                    float up = (across - centerline) * input.centerRadius.x;
                    float braidSalt = input.shape.z * 61.0;
                    // e02: e01's strands twisted like a tight rope (a crossing every ~150 px near the eye, kinks from the
                    // second harmonic) and looped into rings near the dummy. The clip's run long and smooth across the
                    // whole width and MERGE into one white core over the last stretch toward the dummy.
                    float merge = smoothstep(0.50, 0.90, along);
                    float strand = 0.0, sheen = 0.0;
                    [unroll] for (int k = 0; k < 4; k++)
                    {
                        float kk = k + 1.0;
                        float wavelength = 4.0 + 4.0 * CelHash21(float2(kk, braidSalt + 3.0));
                        float amp = (0.45 + 0.40 * CelHash21(float2(kk, braidSalt + 7.0))) * (1.0 - 0.92 * merge);
                        float phase = 6.28318 * CelHash21(float2(kk, braidSalt + 11.0));
                        float drift = (0.6 + 0.9 * CelHash21(float2(kk, braidSalt + 13.0))) * ((k % 2) == 0 ? 1.0 : -1.0);
                        float wave = sin(6.28318 * metres / wavelength + phase + t * drift * 1.6)
                            + 0.12 * sin(6.28318 * metres / (wavelength * 0.37) + phase * 1.7 - t * 2.4);
                        float track = amp * wave / 1.12;
                        float pinch = 0.5 + 0.5 * sin(6.28318 * metres / (wavelength * 0.61) + phase * 2.3 + t * 1.1);
                        float thick = 0.10 + 0.10 * pinch;
                        float strandDistance = abs(across - centerline - track);
                        strand = max(strand, 1.0 - CelAAStep(thick, strandDistance));
                        sheen = max(sheen, 1.0 - CelAAStep(thick + 0.10 + 0.06 * pinch, strandDistance));
                    }
                    // the merged core toward the dummy: one white band about a third of the width, a pale sheen round it
                    strand = max(strand, (1.0 - CelAAStep(0.34 * merge, distanceAxis)) * step(0.02, merge));
                    sheen = max(sheen, (1.0 - CelAAStep(0.55 * merge, distanceAxis)) * step(0.02, merge));
                    float patchNoise = CelValue2(float2(metres * 0.45 - t * 1.1, up * 2.2 + braidSalt));
                    float lowEdge = smoothstep(-0.30, -0.85, up);
                    float deep = max(CelAAStep(0.55, lowEdge + 0.35 * patchNoise), CelAAStep(0.70, patchNoise)) * (1.0 - sheen);
                    float fill = 1.0 - CelAAStep(halfWidth, distanceAxis);
                    body = lerp(CelPal(row, 1), CelPal(row, 0), deep);
                    body = lerp(body, CelPal(row, 2), sheen);
                    body = lerp(body, CelPal(row, 3), strand);
                    body = Unvignette(body, input.positionCS.xy);
                    core = strand;
                    outer = fill;
                    cover = fill * input.tone.z;
                }

                if (input.shape.x > 0.0)
                {
                    bool refineStyle = input.shape.w > 0.5 && input.more.y < 0.5 && input.more.w >= 0.20;
                    float refineWeight = 1.0 - outer;
                    float arcMask = 0.0;
                    float arcGlow = 0.0;
                    [unroll] for (int arc = 0; arc < 4; arc++)
                    {
                        float arcSalt = salt + arc * 31.0;
                        float start = 0.03 + 0.77 * CelHash21(float2(arcSalt, 4.0));
                        float end = min(0.99, start + 0.14 + 0.25 * CelHash21(float2(arcSalt, 5.0)));
                        float progress = saturate((along - start) / max(end - start, 0.1));
                        float rise = saturate(min(progress * 3.2, (1.0 - progress) * 1.6));
                        float side = (arc % 2) == 0 ? 1.0 : -1.0;
                        float track = side * (0.55 + (1.6 + CelHash21(float2(arcSalt, 12.0)) * 1.8) * rise);
                        track += Zig(along, 39.0, arcSalt + 8.0) * 0.38;
                        float broken = step(0.12, CelHash21(float2(floor(progress * 8.0), arcSalt + 21.0)));
                        float thickness = max((0.14 + 0.33 * CelHash21(float2(floor(progress * 6.0), arcSalt))) * saturate(min(progress, 1.0 - progress) * 12.0), onePixel * 1.3);
                        float arcLine = 1.0 - CelAAStep(thickness, abs(across - track));
                        arcLine *= step(start, along) * step(along, end) * broken;
                        float branchTrack = track + side * progress * 2.8;
                        float branchMask = 1.0 - CelAAStep(thickness * 0.55, abs(across - branchTrack));
                        branchMask *= step(0.35, progress) * step(progress, 0.75) * step(start, along) * step(along, end);
                        float diffuse = exp(-pow(abs(across - track) / max(thickness * 2.2, 0.1), 2.0));
                        float glowPiece = diffuse * step(start, along) * step(along, end) * broken;
                        if (refineStyle)
                        {
                            float widthCell = progress * 6.0;
                            float widthBlend = smoothstep(0.0, 1.0, frac(widthCell));
                            float widthNoise = lerp(CelHash21(float2(floor(widthCell), arcSalt)),
                                CelHash21(float2(floor(widthCell) + 1.0, arcSalt)), widthBlend);
                            float endTaper = saturate(min(progress * 6.0, (1.0 - progress) * 5.0));
                            float arcTaper = endTaper;
                            float narrowWidth = max((0.045 + 0.055 * widthNoise) * arcTaper, onePixel * 0.65);
                            float narrowArc = (1.0 - CelAAStep(narrowWidth, abs(across - track))) * arcTaper;
                            float branchProgress = saturate((progress - 0.35) / 0.40);
                            float attachedTrack = track + side * branchProgress * 2.1;
                            float branchStop = 0.75;
                            [unroll] for (int branchCell = 2; branchCell < 6; branchCell++)
                            {
                                float segmentSolid = step(0.12, CelHash21(float2(branchCell, arcSalt + 21.0)));
                                branchStop = min(branchStop, lerp(branchCell * 0.125, 0.75, segmentSolid));
                            }
                            // A branch starts inside its parent at full coverage and never resumes beyond a gap.
                            float branchStart = step(0.35, progress) * step(0.08333333, branchStop - 0.35);
                            float branchTaper = branchStart * saturate((branchStop - progress) * 12.0);
                            float narrowBranchWidth = max(narrowWidth * 0.55 * branchTaper, onePixel * 0.45);
                            float narrowBranch = (1.0 - CelAAStep(narrowBranchWidth, abs(across - attachedTrack))) * branchTaper;
                            float narrowGlow = exp(-pow(abs(across - track) / max(narrowWidth * 2.2, onePixel), 2.0)) * arcTaper;
                            float branchGlow = exp(-pow(abs(across - attachedTrack) / max(narrowBranchWidth * 2.2, onePixel), 2.0)) * branchTaper;
                            arcLine = lerp(arcLine, narrowArc, refineWeight);
                            branchMask = lerp(branchMask, narrowBranch, refineWeight);
                            glowPiece = lerp(glowPiece, max(narrowGlow, branchGlow), refineWeight);
                        }
                        arcMask = max(arcMask, max(arcLine, branchMask));
                        arcGlow = max(arcGlow, glowPiece);
                    }
                    added += (CelPalCore(row).rgb * arcMask * 1.8 + CelPal(row, 2) * arcGlow * 1.0) * input.shape.x;
                    float halo = exp(-pow(max(distanceAxis - 1.0, 0.0), 2.0) * 0.85) * (1.0 - outer);
                    added += CelPal(row, 2) * halo * 0.4;

                    float ink = 0.0;
                    [unroll] for (int inkIndex = 0; inkIndex < 12; inkIndex++)
                    {
                        float side = (inkIndex % 2) == 0 ? -1.0 : 1.0;
                        float offset = side * (0.9 + inkIndex * 0.27 + Zig(along, 7.0, salt + inkIndex) * 0.14);
                        float lineStart = CelHash21(float2(inkIndex, salt + 11.0)) * 0.65;
                        float lineEnd = min(0.95, lineStart + 0.16 + CelHash21(float2(inkIndex, salt + 41.0)) * 0.34);
                        float taper = saturate((lineEnd - along) * 18.0);
                        float lineMask = 1.0 - CelAAStep(max(onePixel * 1.4, 0.055) * taper, abs(across - offset));
                        float inkPiece = lineMask * step(lineStart, along) * step(along, lineEnd);
                        if (refineStyle)
                        {
                            float shortEnd = min(0.95, lineStart + 0.07 + CelHash21(float2(inkIndex, salt + 41.0)) * 0.13);
                            float inkProgress = saturate((along - lineStart) / max(shortEnd - lineStart, 0.01));
                            float inkTaper = saturate(min(inkProgress * 5.0, (1.0 - inkProgress) * 4.0));
                            float fineWidth = max(onePixel * 0.65, 0.022) * inkTaper;
                            float fineInk = (1.0 - CelAAStep(fineWidth, abs(across - offset))) * inkTaper;
                            inkPiece = lerp(inkPiece, fineInk, refineWeight);
                        }
                        ink = max(ink, inkPiece);
                    }
                    ink *= (1.0 - core) * (1.0 - arcMask) * (braid ? 0.0 : 1.0);   // e01: the clip's ink lies on the ground, not on the beam
                    body = lerp(body, CelPalInk(row).rgb, ink);
                    cover = max(cover, ink);
                    added *= 1.0 - ink;
                }
                body = MixFog(body, input.more.x);
                if (input.more.y > 0.5)
                {
                    added += body * outer * input.tone.z;
                    cover = 0.0;
                }
                half4 result = half4(body * cover + AddFog(added, input), cover);
                if (input.more.y < 0.5 && input.shape.w > 0.5 && input.more.w < 0.20)
                    result *= OpeningReveal(input.more.w);
                return result;
            }
            ENDHLSL
        }
    }
}
