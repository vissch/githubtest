// R44: one world-space impact heat field shared by smoke skin and support curls.
// Include after CelFx.hlsl. All phase/height inputs belong to the emitter record.
#ifndef TW_CEL_IMPACT_FIRE_INCLUDED
#define TW_CEL_IMPACT_FIRE_INCLUDED

// x signed hot field, y temporal/particle fuel, z height fuel, w cooling phase.
float4 CelImpactFire(float3 world, float impactHeight, float elapsed, float age, float2 upperHeight)
{
    float cool = smoothstep(4.05, 4.65, elapsed);
    float fuel = smoothstep(1.0, 1.5, elapsed)
        * (1.0 - smoothstep(4.12, 4.74, elapsed))
        * (1.0 - smoothstep(1.0, 1.8, age));
    float height = world.y - impactHeight;
    float heightFuel = smoothstep(-1.15, -0.20, height)
        * (1.0 - smoothstep(upperHeight.x, upperHeight.y, height));
    float3 flow = float3(world.x * 0.62,
        height * 0.78 - elapsed * 0.90, world.z * 0.62);
    float bend = CelSimplex3(flow * 0.52 + float3(1.7, elapsed * 0.12, 4.3));
    flow.xz += float2(bend * 0.50, bend * 0.35);
    float roll = CelSimplex3(flow);
    float fold = CelSimplex3(flow * 1.45 + float3(4.1, 0.0, 2.3));
    float skin = 0.12 + 0.74 * roll + 0.16 * fold
        - 0.07 * max(height, 0.0) - 0.42 * cool;
    return float4(skin, fuel, heightFuel, cool);
}

half3 CelImpactFireColor(float skin, float cool)
{
    float hot = saturate((skin + 0.02) / 0.78);
    half3 flame = lerp(half3(0.95, 0.26, 0.075), half3(1.50, 0.95, 0.46), CelAAStep(0.30, hot));
    flame = lerp(flame, half3(2.10, 1.55, 0.60), CelAAStep(0.94, hot));
    return lerp(flame, half3(1.00, 0.30, 0.085), cool * 0.85);
}

// Opaque support contracts through the shared fuel fade. Cooler regions stay
// connected orange folds instead of becoming hard holes in the curl.
float CelImpactFireCutout(float localField, float4 fire, float contraction)
{
    float fuel = fire.y * fire.z;
    // Surface skin has a smaller signed range than the normalized curl field.
    // Callers may tune geometric contraction while sharing fuel and its floor.
    return min(localField - (1.0 - fuel) * contraction, fuel - 0.015);
}

float CelImpactFireCutout(float localField, float4 fire)
{
    // Keep the existing support-curl contour and decay exactly as authored.
    return CelImpactFireCutout(localField, fire, 0.85);
}
#endif
