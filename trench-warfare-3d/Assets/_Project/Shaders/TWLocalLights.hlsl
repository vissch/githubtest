// Phase: B2 (implemented) - lanterns, muzzle flashes and flares on the painted world. They are ordinary URP additional
// lights (NightLights.cs owns them); this adds each one in hard steps, so a lamp throws a pool with an edge, not a gradient.
// Include after URP's Lighting.hlsl. Without _ADDITIONAL_LIGHTS it returns 0.
#ifndef TW_LOCAL_LIGHTS_INCLUDED
#define TW_LOCAL_LIGHTS_INCLUDED

/// Lanterns, muzzle flashes, flares: each adds its colour in two hard steps (a bright core, a dim reach).
/// view and gloss add the wet highlight: the same light mirrored off the surface toward the eye, in one hard step,
/// so lamps and muzzle flashes streak warm across soaked mud, puddles, bags and helmets. It rides in the same loop as the
/// diffuse steps, so it costs one reflect and one pow a light. Pass gloss 0 to skip it.
half3 TWLocalLights(float3 positionWS, float3 normalWS, float4 positionCS, float3 view, half gloss, out half3 highlight)
{
    half3 sum = 0;
    highlight = 0;
    half lamp = _TWLampScale > 0.0 ? _TWLampScale : 1.0;
    float3 mirrored = reflect(-view, normalWS);
#if defined(_ADDITIONAL_LIGHTS)
    InputData inputData = (InputData)0;
    inputData.positionWS = positionWS;
    inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(positionCS);
    uint count = GetAdditionalLightsCount();
    LIGHT_LOOP_BEGIN(count)
        Light l = GetAdditionalLight(lightIndex, positionWS);
        half facing = saturate(dot(normalWS, l.direction) * 0.6 + 0.4);   // wrapped: a lantern still shows on the wall beside it
        half peak = max(l.color.r, max(l.color.g, l.color.b));
        half e = l.distanceAttenuation * facing * peak;                    // how much light arrives, whatever its colour
        // the pool of light is layered like the glow: a deep-coloured reach, the light's own colour, a hotter heart
        half3 tint = l.color / max(peak, 1e-4);
        half3 deep = tint * tint * tint;
        half glint = pow(saturate(dot(mirrored, l.direction)), 22.0) * l.distanceAttenuation * peak;
        highlight += lerp(tint, half3(1, 1, 1), 0.35) * (smoothstep(0.05, 0.11, glint) * 0.55 + smoothstep(0.6, 0.9, glint) * 0.6) * gloss;
        // The three bands are scaled UNEVENLY by the biome. On snow the widest, dimmest band - the deep-coloured
        // reach - is what turns a lantern into a salmon stain twenty metres across, because it is multiplied by an
        // albedo three times what it was tuned against. The core is not the problem and must not be dimmed with it:
        // on a single-hue field the lamps are the only warm things left and their job is to be precious.
        half reach = lamp * lamp, mid = lamp, core = sqrt(lamp);
        sum += deep * smoothstep(0.02, 0.04, e) * 0.36 * reach + tint * smoothstep(0.12, 0.20, e) * 0.38 * mid + lerp(tint, half3(1, 1, 1), 0.45) * smoothstep(0.55, 0.75, e) * 0.44 * core;
    LIGHT_LOOP_END
#endif
    highlight *= lamp;
    return sum;
}

#endif
