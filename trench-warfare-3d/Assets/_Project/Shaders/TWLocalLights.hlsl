// Phase: B2 (implemented) - lanterns, muzzle flashes and flares on the painted world. They are ordinary URP additional
// lights (NightLights.cs owns them); this adds each one in steps. At look.poolSoft 0 they are the old hard steps, so a
// lamp throws a pool with an edge; at 1 (the default) the same three steps are widened into ramps, keeping their levels,
// because the outer one at 0.02 -> 0.04 of the arriving light is a few centimetres of ground under inverse-square
// attenuation: a hard disc, and several lamps' discs read as one scalloped line (the look of 2026-10-17). No new knob.
// Include after URP's Lighting.hlsl. Without _ADDITIONAL_LIGHTS it returns 0.
#ifndef TW_LOCAL_LIGHTS_INCLUDED
#define TW_LOCAL_LIGHTS_INCLUDED

#ifndef TW_POOLSOFT_DECLARED
#define TW_POOLSOFT_DECLARED
float _TWPoolSoft;   // look.poolSoft (NightLights.Pools.cs), declared here too: this header is included BEFORE
                     // TWLightPools.hlsl (Toon_URP, Tank_URP, VAT_URP) and alone (Debris, Sea, Water).
#endif

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
        // wrapped: a lantern still shows on the wall beside it. look.poolSoft wraps it further, so a lamp's light
        // carries over the crest of a shell hole instead of stopping on it and cutting its pool with the terminator.
        half spF = saturate(_TWPoolSoft);
        half facing = saturate(dot(normalWS, l.direction) * lerp(0.6, 0.45, spF) + lerp(0.4, 0.55, spF));
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
        // look.poolSoft widens the three bands, never their levels: the levels are what the biome scaling above and
        // the muzzle flashes, flares and winter/lava lamp scale were tuned against.
        half sp = spF;
        half2 bR = lerp(half2(0.02, 0.04), half2(0.004, 0.09), sp);
        half2 bM = lerp(half2(0.12, 0.20), half2(0.05, 0.38), sp);
        half2 bC = lerp(half2(0.55, 0.75), half2(0.35, 1.10), sp);
        sum += deep * smoothstep(bR.x, bR.y, e) * 0.36 * reach + tint * smoothstep(bM.x, bM.y, e) * 0.38 * mid + lerp(tint, half3(1, 1, 1), 0.45) * smoothstep(bC.x, bC.y, e) * 0.44 * core;
    LIGHT_LOOP_END
#endif
    highlight *= lamp;
    // Beyond arm's reach of the lamp itself, a lantern on snow should make it BRIGHTER, not PINKER. Scaling the
    // brightness was not enough: measured, 22% of a winter frame came back warm against the reference's 0.2%, and
    // 46% of those pixels were lit snow rather than fire. So the far half of the pool is desaturated toward the
    // biome's own colour and only the near half keeps its flame hue. Costs three mads.
    if (_TWLampScale > 0.0 && _TWLampScale < 0.999)
    {
        half near = saturate(length(sum) * 2.2);              // bright = close to a lamp
        // WHAT the far half fades toward has to be the colour of THIS field. _TWSnowColor is not that: it is
        // pushed on every biome (the night field's own default is 0.90,0.93,0.97) and it only MEANS anything
        // where snow lies. This branch is live wherever LampScale < 1, which is winter and LAVA - and
        // BiomeProfile.Lava() never assigns SnowColor, so every lantern on a planet lit from below by molten
        // rock has been fading to snow white at the edge of its pool.
        // Ask the question that was meant - is there snow here - and otherwise take the field's own tint,
        // normalised by its luma so it desaturates without DARKENING: basalt is (0.34,0.28,0.27), luma 0.30,
        // and multiplying by that directly would dim the outer pool to a third rather than grey it.
        half3 field = _TWSnow.x > 0.0 ? _TWSnowColor.rgb
                    : (_TWWorldTint.a > 0.0
                        ? _TWWorldTint.rgb / max(dot(_TWWorldTint.rgb, half3(0.299, 0.587, 0.114)), 1e-3)
                        : half3(1, 1, 1));
        half3 flat = dot(sum, half3(0.299, 0.587, 0.114)) * field;
        sum = lerp(flat, sum, near);
    }
    return sum;
}

#endif
