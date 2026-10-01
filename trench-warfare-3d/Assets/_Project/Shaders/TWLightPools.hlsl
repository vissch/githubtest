// Warm light pools (look.pools, NightLights.Pools.cs; the owner's night look, 2026-09-29): the ground under every lantern,
// trench lamp, torch and fire lit in a painted pool of its own, the way the owner's effects edit has a dozen torches each
// lighting its patch of mud. The real lights cannot do it: the renderer is Forward, eight lights an object at most
// (TW-URP.asset), and a ground chunk sees far more lamps than that. So NightLights hands the nearest TW_MAX_POOLS flames
// to every Toon surface as a global array, and this adds them in the toon's own hard bands.
//   _TWPools[k]     xyz where the flame hangs, w the pool's reach (m)
//   _TWPoolTint[k]  rgb the flame's colour times its strength now (it flickers with the lamp)
//   _TWPoolCount    how many are set; 0 (the default, look.pools 0) adds nothing and costs one branch
#ifndef TW_LIGHT_POOLS_INCLUDED
#define TW_LIGHT_POOLS_INCLUDED

#define TW_MAX_POOLS 32
#define TW_STREAK_REACH 2.6   // a streak runs out to this many of its flame's pool reaches
float4 _TWPools[TW_MAX_POOLS];
float4 _TWPoolTint[TW_MAX_POOLS];
float _TWPoolCount;
float _TWWetLook;   // look.wet (Atmosphere.NightLook.cs): 0 today
float _TWPoolSoft;   // look.poolSoft: 0 the three hard bands, 1 a soft falloff (critique round 5: "cut-out discs")
float _TWPoolShoulder;   // look.poolShoulder: rolls a bright sum off, so a fire's pool never clips the ground to flat orange
float _TWPoolsThroughHaze;
float _TWFireStreak;   // look.fireStreak: each flame's long broken reflection across the wet mud toward the camera; 0 = none
float _TWPropRim;
float _TWMoonSheen;   // look.moonSheen (Atmosphere.NightLook.cs): the standard view's share of the fine wet sparkle (0.3 before); 0 = as before
float _TWPoolUnblue;   // look.poolUnblue: where a pool adds warm light it takes that much blue out of the moonlit ground under it,
                       // or its thin rim of orange over blue read violet (critique round 15: 1.5 % of pixels violet, all at pool edges)   // look.propRim: a prop's (not the ground's) warm rim toward the nearest pools, like the men's; 0 = none   // look.poolsThroughHaze: the share of a pool's light added after the haze, so a lamp far off still lights its mud through it

/// A pool's weight at t (0 under the flame, 1 at its rim): the three hard bands, softened toward a smooth falloff by
/// look.poolSoft (the bands' edges widen and a squared falloff takes over).
half TWPoolBand(float t)
{
    half hard = 0.45 * (1.0 - smoothstep(0.30, 0.34, t)) + 0.33 * (1.0 - smoothstep(0.62, 0.66, t)) + 0.22 * (1.0 - smoothstep(0.94, 1.0, t));
    half s = 1.0 - saturate(t);
    half soft = s * s * (0.55 + 0.45 * (1.0 - smoothstep(0.35, 0.65, t)));
    return lerp(hard, soft * 1.6, _TWPoolSoft);
}

/// look.poolShoulder: a sum of pools rolled off so it approaches, never passes, 1/k of a lantern's full light.
half3 TWPoolRoll(half3 sum)
{
    if (_TWPoolShoulder <= 0.0) return sum;
    half m = max(sum.r, max(sum.g, sum.b));
    return sum / (1.0 + m * _TWPoolShoulder);
}

/// The warm light the pools throw on a surface at positionWS facing normalWS: three hard bands (softened by
/// look.poolSoft), brightest in the middle third of the reach, falling to a faint rim, and only on the side that faces
/// the flame.
half3 TWLightPools(float3 positionWS, half3 normalWS)
{
    half3 sum = 0;
    int count = (int)_TWPoolCount;
    [loop] for (int k = 0; k < count; k++)
    {
        float3 d = _TWPools[k].xyz - positionWS;
        float reach = _TWPools[k].w;
        float dist2 = dot(d, d);
        if (dist2 >= reach * reach) continue;
        float t = sqrt(dist2) / reach;                                            // 0 under the flame, 1 at the rim
        half facing = saturate(dot(normalWS, d * rsqrt(max(dist2, 1e-4))) * 0.6 + 0.4);
        sum += _TWPoolTint[k].rgb * TWPoolBand(t) * facing;
    }
    return TWPoolRoll(sum);
}

/// For a figure (a man, a machine): the pools' light and a warm rim together in one pass. The rim: the edge of a figure
/// turned toward a flame catches it, hard and warm, strongest near it, as the owner's colour edit rims every man on the
/// trench's lip orange from the wreck burning behind him; it shows where the surface turns away from the eye AND toward
/// the flame, so a man between the camera and a fire is outlined in it and one lit face-on is not. Both over the first
/// TW_FIGURE_POOLS pools only. NightLights sorts them best first for the camera, so these are the ones that light the
/// most of what it sees; looping all 32 twice over a thousand men cost the GPU 0.8 to 1.6 ms (bench, 2026-09-30).
#define TW_FIGURE_POOLS 12
half3 TWPoolsOnFigure(float3 positionWS, half3 normalWS, half3 viewWS, out half3 rimOut, half edgeLo = 0.30)
{
    half3 sum = 0; rimOut = 0;
    half edge = 1.0 - saturate(dot(normalWS, viewWS));
    int count = min((int)_TWPoolCount, TW_FIGURE_POOLS);
    [loop] for (int k = 0; k < count; k++)
    {
        float3 d = _TWPools[k].xyz - positionWS;
        float reach = _TWPools[k].w * 1.25;
        float dist2 = dot(d, d);
        if (dist2 >= reach * reach) continue;
        float t = sqrt(dist2) / reach;
        half toward = saturate(dot(normalWS, d * rsqrt(max(dist2, 1e-4))));
        float tp = t * 1.25;                                                    // the pool's own reach, not the rim's
        half band = tp < 1.0 ? TWPoolBand(tp) : 0.0;
        sum += _TWPoolTint[k].rgb * band * (toward * 0.6 + 0.4);
        rimOut += _TWPoolTint[k].rgb * smoothstep(edgeLo, edgeLo + 0.08, edge * toward) * (1.0 - smoothstep(0.45, 1.0, t));
    }
    return TWPoolRoll(sum);
}

/// The ground's pools and, where it is wet (glintOn), the flames' glints in its reflection r, in ONE pass over the pools:
/// the two separate loops cost the terrain two walks of 32 pools on every wet pixel (round 12: the pool system measured
/// 0.4-0.5 ms). Same bands, same glints, as TWLightPools and TWPoolGlints.
half3 TWPoolsAndGlints(float3 positionWS, half3 normalWS, float3 r, bool glintOn, out half3 glints)
{
    half3 sum = 0; glints = 0;
    int count = (int)_TWPoolCount;
    [loop] for (int k = 0; k < count; k++)
    {
        float3 d = _TWPools[k].xyz - positionWS;
        float reach = _TWPools[k].w;
        float dist2 = dot(d, d);
        float glintReach = reach * 1.6;
        float cull = glintOn && _TWFireStreak > 0.0 ? reach * TW_STREAK_REACH : glintReach;
        if (dist2 >= cull * cull) continue;
        float dist = sqrt(dist2);
        float3 dn = d / max(dist, 1e-2);
        if (dist < reach)
            sum += _TWPoolTint[k].rgb * TWPoolBand(dist / reach) * saturate(dot(normalWS, dn) * 0.6 + 0.4);
        if (glintOn)
        {
            glints += _TWPoolTint[k].rgb * smoothstep(0.86, 0.92, dot(r, dn)) * (1.0 - smoothstep(0.55, 1.0, dist / glintReach));
            if (_TWFireStreak > 0.0)
            {
                // look.fireStreak: wet mud stretches a flame's reflection into a long streak running from under it back
                // toward the viewer (the owner's colour edit: an orange smear under every burning wreck), where a mirror
                // would show one point. Measured in the ground plane: this point lies on the streak when the flame stands
                // ahead of it on the line from the camera through it (along) and close beside that line (across). The
                // line is the flat ground's reflected ray, not this pixel's: the soaked mud's bumps scatter r, and a
                // streak taken from it broke into specks (round 23).
                float2 rh = positionWS.xz - _WorldSpaceCameraPos.xz;
                rh *= rsqrt(max(dot(rh, rh), 1e-4));
                float along = dot(d.xz, rh);
                float across = d.x * rh.y - d.z * rh.x;   // signed: the wobble below bends it either way
                float wob = dot(positionWS.xz, float2(2.3, 1.7));
                across = abs(across + 0.07 * sin(along * 1.9 + wob));   // the column wanders a little, as ripples bend it
                float width = 0.10 + 0.02 * along;   // a thin column, a quarter of a fire's width (round 23's critic: "a flat orange flood")
                half fade = 1.0 - saturate(along / (reach * TW_STREAK_REACH));
                half s = (1.0 - smoothstep(width * 0.3, width, across)) * smoothstep(0.0, 0.8, along) * fade * fade;
                // broken by the wet ground's ripples: uneven dark gaps across the column, about half of it (one even
                // sine read as road markings)
                half ripple = sin(along * 7.0 + 2.3 * sin(dot(positionWS.xz, float2(6.1, 4.7)))) + 0.8 * sin(along * 2.9 + wob * 1.3);
                s *= smoothstep(0.1, 0.9, ripple) * 2.0;
                half3 deep = _TWPoolTint[k].rgb * half3(1.0, 0.62, 0.32);   // deeper orange than the flame: acid yellow on the mud read as paint
                glints += deep * s * _TWFireStreak;
            }
        }
    }
    return TWPoolRoll(sum);
}

/// look.wet: the flames' glints on wet ground. A reflection r that points back at a flame within its pool catches a hard
/// highlight of its colour, so the mud near a fire sparkles orange and, away from it, only the moon's blue remains.
half3 TWPoolGlints(float3 positionWS, float3 r)
{
    half3 sum = 0;
    int count = (int)_TWPoolCount;
    [loop] for (int k = 0; k < count; k++)
    {
        float3 d = _TWPools[k].xyz - positionWS;
        float reach = _TWPools[k].w * 1.6;                                      // a glint is seen from further than the pool
        float dist2 = dot(d, d);
        if (dist2 >= reach * reach) continue;
        half toward = smoothstep(0.86, 0.92, dot(r, d * rsqrt(max(dist2, 1e-4))));
        sum += _TWPoolTint[k].rgb * toward * (1.0 - smoothstep(0.55, 1.0, sqrt(dist2) / reach));
    }
    return sum;
}

#endif
