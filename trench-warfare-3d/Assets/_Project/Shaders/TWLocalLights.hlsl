// Phase: B2 (implemented) - lanterns, muzzle flashes and flares on the painted world. They are ordinary URP additional
// lights (NightLights.cs owns them); this adds each one in hard steps, so a lamp throws a pool with an edge, not a gradient.
// Include after URP's Lighting.hlsl. Without _ADDITIONAL_LIGHTS it returns 0.
#ifndef TW_LOCAL_LIGHTS_INCLUDED
#define TW_LOCAL_LIGHTS_INCLUDED

/// Lanterns, muzzle flashes, flares: each adds its colour in two hard steps (a bright core, a dim reach).
half3 TWLocalLights(float3 positionWS, float3 normalWS, float4 positionCS)
{
    half3 sum = 0;
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
        half steps = smoothstep(0.02, 0.04, e) * 0.22 + smoothstep(0.12, 0.20, e) * 0.38 + smoothstep(0.55, 0.75, e) * 0.40;
        sum += l.color / max(peak, 1e-4) * steps;
    LIGHT_LOOP_END
#endif
    return sum;
}

#endif
