// Phase: B2 (implemented) — the air shared by every world shader (TW/Toon, TW/Water, the ink pass). Atmosphere.cs sets
// the globals; with no Atmosphere in the scene every density is 0 and these functions return the colour unchanged.
// Ground mist: a pale layer over low, distant ground.
// Fog bank: a wall of fog round the fought-over ground. It is a function of world position only (signed distance to
// the battlefield's rectangle, a slow billow along its edge, thinning with height), so it costs a few instructions a
// pixel, needs no extra geometry or overdraw, and the ink pass can ask the same question to keep lines out of the fog.
// Quiet fog: inside the battlefield the same fog lies, thinner and lower, over ground where nothing is happening.
// _TWPresence (QuietFog.cs: one texel per 4 m, 1 = the player's men, their HQ or a recent shell burst are near) lifts it.
// Mood (Atmosphere.cs): _TWShadeTint darkens and colours every shaded plane (night: deep blue), _TWSky is what water
// and wet mud mirror, _TWWet.x is how wet the open ground is (rain-soaked mud glints under the moon).
// Local lights: lanterns, muzzle flashes and flares are URP additional lights; TWLocalLights adds them in two hard
// steps so they stay in the painted look (TWLocalLights.hlsl, included after URP's Lighting.hlsl).
#ifndef TW_ATMOSPHERE_INCLUDED
#define TW_ATMOSPHERE_INCLUDED

float4 _TWShadeTint;     // rgb multiplies each material's shade colour, a = 1 when Atmosphere has set it
float4 _TWSky;           // rgb mirrored by water and wet ground, a = 1 when set
float _TWClose;          // 0 at the standard view, 1 zoomed in among the men: the small detail only exists above 0 (TacticalCamera)
float4 _TWWet;           // x wetness of open ground 0..1, y glint strength, z rain on the water 0..1

half3 TWShadeTint() { return _TWShadeTint.a > 0.5 ? _TWShadeTint.rgb : half3(1, 1, 1); }
half3 TWSky() { return _TWSky.a > 0.5 ? _TWSky.rgb : unity_FogColor.rgb; }


// ---------------------------------------------------------------------------------------------------------------
// Biome. Snow lies on what faces the sky, molten ground burns between its plates, and the ground itself lights the
// air above it. Set by Atmosphere.cs from a BiomeProfile; every one of these is zero on the night mud field, and
// every function below returns its input unchanged in that case. The `if` on a global is a uniform branch, so the
// base game pays nothing at all for either biome existing.
//
// The reason these live in the SHARED header rather than in TW/Toon: snow that lands on the terrain and the
// sandbags but not on the men, the tanks or the thrown debris is worse than no snow, because the eye reads the
// men as cut out of a different picture. Any shader that draws something standing on the battlefield includes
// this file and asks the same question.
float4 _TWSnow;        // x coverage 0..1, y the normal.y below which a surface sheds it, z sparkle, w drift break-up
half4 _TWSnowColor;    // rgb lying snow, a the gloss it adds (fresh snow is matt; old wind-packed snow is not)
float4 _TWHeat;        // x glow strength, y plates per metre, z crack width, w world Y of the molten level
half4 _TWHeatColor;    // rgb what molten rock throws up out of its cracks
half4 _TWGroundLight;  // rgb light coming UP off the ground into everything above it, a = 1 when set

/// The ambient a surface sees, split into what falls from the sky and what comes back up off the ground.
///
/// A single flat ambient gives an unlit plane no form whatsoever. Night and mud hide that; a flat-lit snowfield
/// will not, and it is the difference between a winter battlefield and a white page. The same split is the whole
/// lighting model of the lava field, where the ground is the BRIGHTER half and every man is lit from beneath -
/// which is why this is one function and not two.
half3 TWHemisphere(half3 skyShade, float3 normalWS)
{
    if (_TWGroundLight.a < 0.5) return skyShade;
    return lerp(_TWGroundLight.rgb, skyShade, saturate(normalWS.y * 0.5 + 0.5));
}

/// How much snow is lying here. Up-facing surfaces keep it, vertical faces shed it, and the line between is broken
/// so it reads as drift rather than as a contour line drawn round every object. Free: normalWS and positionWS are
/// already interpolated for every pixel of every surface, five times over in TW/Toon alone.
half TWSnowAmount(float3 normalWS, float3 positionWS)
{
    if (_TWSnow.x <= 0.0) return 0.0;
    half up = saturate((normalWS.y - _TWSnow.y) / max(1.0 - _TWSnow.y, 1e-3));
    // three incommensurate waves, so the snow line wanders instead of following the geometry. Cheap on purpose:
    // a texture read here would have to be bound by every shader that includes this header.
    float2 q = positionWS.xz;
    half n = sin(q.x * 0.73 + q.y * 0.41) * 0.5 + sin(q.x * 2.17 - q.y * 1.63) * 0.3 + sin(q.x * 5.31 + q.y * 4.11) * 0.2;
    return saturate(up * (1.0 + n * _TWSnow.w) * _TWSnow.x);
}

/// Distance to the nearest border between two crust plates: 0 exactly on a crack, rising into the middle of a
/// plate. A 3x3 Worley, because the reference shows light coming up BETWEEN plates - a noise threshold instead
/// gives glowing dirt, which is the specific failure docs/18 warns about for L2.
half TWPlateEdge(float2 p)
{
    float2 c = floor(p), f = p - c;
    half d1 = 8.0, d2 = 8.0;
    for (int y = -1; y <= 1; y++)
    for (int x = -1; x <= 1; x++)
    {
        float2 g = float2(x, y);
        float2 h = frac(sin(float2(dot(c + g, float2(127.1, 311.7)), dot(c + g, float2(269.5, 183.3)))) * 43758.5453);
        half d = length(g + h - f);
        if (d < d1) { d2 = d1; d1 = d; } else if (d < d2) { d2 = d; }
    }
    return d2 - d1;
}

/// What the cracks in molten ground add to a surface. Strongest on ground near the molten level and fading with
/// height, so a sandbag two metres up glows along its foot and not along its top.
half3 TWHeatGlow(float3 positionWS, half exposure)
{
    if (_TWHeat.x <= 0.0) return half3(0, 0, 0);
    half crack = 1.0 - smoothstep(0.0, max(_TWHeat.z, 1e-3), TWPlateEdge(positionWS.xz * _TWHeat.y));
    half high = saturate(1.0 - (positionWS.y - _TWHeat.w) * 0.55);   // the heat is in the floor, not in the air
    return _TWHeatColor.rgb * (crack * crack * _TWHeat.x * high * exposure);
}


float4 _TWMist;          // x top height, y 1/depth, z start distance, w 1/range
float4 _TWMistColor;     // rgb, a = density
float4 _TWField;         // xz min, xz max of the fought-over ground
float4 _TWFieldFog;      // x metres outside the bounds where the bank begins (negative = inside), y 1/range, z top height at the edge, w top rise per metre out
float4 _TWFieldFogColor; // rgb, a = density
float4 _TWQuiet;         // x density (0 = off), y top height, z 1/height fade
TEXTURE2D(_TWPresence); SAMPLER(sampler_TWPresence);

half3 ApplyMist(half3 color, float3 positionWS)
{
    float far = saturate((distance(_WorldSpaceCameraPos, positionWS) - _TWMist.z) * _TWMist.w);
    float low = saturate((_TWMist.x - positionWS.y) * _TWMist.y);
    return lerp(color, _TWMistColor.rgb, low * far * _TWMistColor.a);
}

/// 0 inside the battlefield, rising to the bank's density beyond its edge.
half FieldFogAmount(float3 positionWS)
{
    float2 d = max(_TWField.xy - positionWS.xz, positionWS.xz - _TWField.zw);
    float outside = length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);   // signed distance to the rectangle, round at the corners
    float2 p = positionWS.xz * 0.045;
    float t = _Time.y;
    // the edge billows: long tongues of fog reach in and draw back, drifting slowly
    float billow = sin(p.x * 1.7 + t * 0.05) * sin(p.y * 1.3 - t * 0.04) + 0.5 * sin((p.x + p.y) * 3.1 + t * 0.08) + 0.25 * sin((p.x - p.y) * 7.3 - t * 0.11);
    float reach = saturate((outside + billow * 6.0 - _TWFieldFog.x) * _TWFieldFog.y);
    reach = reach * reach * (3.0 - 2.0 * reach);
    float top = _TWFieldFog.z + max(outside, 0.0) * _TWFieldFog.w;    // the bank stands taller the further out it lies
    float low = saturate((top - positionWS.y) / max(top * 0.6, 0.5)); // tree tops near the edge show through
    half bank = reach * low * _TWFieldFogColor.a;
    if (_TWQuiet.x <= 0.0) return bank;
    float2 uv = (positionWS.xz - _TWField.xy) / max(_TWField.zw - _TWField.xy, 1.0);
    half presence = SAMPLE_TEXTURE2D_LOD(_TWPresence, sampler_TWPresence, uv, 0).r;
    half quiet = saturate((1.0 - presence) * 1.25 + billow * 0.10 - 0.12);
    quiet *= saturate((_TWQuiet.y - positionWS.y) * _TWQuiet.z) * _TWQuiet.x;   // past the edge the presence map clamps, and the bank takes over
    return max(bank, quiet * _TWFieldFogColor.a);
}

half3 ApplyFieldFog(half3 color, float3 positionWS)
{
    return lerp(color, _TWFieldFogColor.rgb, FieldFogAmount(positionWS));
}

// The shell burning on the field right now: xyz where it is, w how far it reaches; the colour already carries how much
// of it is left, so it is simply added. NightLights writes both every frame and leaves the colour black when nothing is
// burning. The drawn bursts read it: a shell's own flash has to light the column of earth it threw up and the smoke
// coming off it, or the brightest event in the game is a grey drawing standing in front of an orange light.
// Only the strongest burst alive is carried — in a barrage the eye follows the biggest one anyway.
float4 _TWBurst;
half4 _TWBurstColor;

half3 TWBurstLight(float3 positionWS)
{
    return _TWBurstColor.rgb * saturate(1.0 - distance(positionWS, _TWBurst.xyz) / max(0.01, _TWBurst.w));
}

#endif
