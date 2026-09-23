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
half4 _TWWorldTint;    // rgb multiplies the ground and every prop standing on it, a = 1 when set
float _TWLampScale;    // local lights are multiplied by albedo; snow is 3x the albedo mud is, so its lamps need reining in

/// The battlefield's own colour over the top of the painted mud. The ground texture is baked on the CPU from a
/// mud palette and repainted per crater; re-baking it per biome means hoisting six private colours and a dozen
/// literals out of a 160-line static function, which is the right fix and not a cheap one. This is the cheap
/// one, and it is honest about being a tint rather than a repaint: basalt is mud multiplied most of the way to
/// black, old snow is mud multiplied toward cold grey. Unset on the night field, where it is exactly 1.
half3 TWWorldTint() { return _TWWorldTint.a > 0.5 ? _TWWorldTint.rgb : half3(1, 1, 1); }

/// The ambient a surface sees, split into what falls from the sky and what comes back up off the ground.
///
/// A single flat ambient gives an unlit plane no form whatsoever. Night and mud hide that; a flat-lit snowfield
/// will not, and it is the difference between a winter battlefield and a white page. The same split is the whole
/// lighting model of the lava field, where the ground is the BRIGHTER half and every man is lit from beneath -
/// which is why this is one function and not two.
half3 TWHemisphere(half3 skyShade, float3 normalWS)
{
    if (_TWGroundLight.a <= 0.001) return skyShade;
    return lerp(_TWGroundLight.rgb, skyShade, saturate(normalWS.y * 0.5 + 0.5));
}

/// How much snow is lying here. Up-facing surfaces keep it, vertical faces shed it, and the line between is broken
/// so it reads as drift rather than as a contour line drawn round every object. Free: normalWS and positionWS are
/// already interpolated for every pixel of every surface, five times over in TW/Toon alone.
half TWSnowAmount(float3 normalWS, float3 positionWS)
{
    if (_TWSnow.x <= 0.0) return 0.0;
    half up = saturate((normalWS.y - _TWSnow.y) / max(1.0 - _TWSnow.y, 1e-3));
    // Three waves, DOMAIN-WARPED by the lowest of them. Three fixed-frequency sines on the same axes are a plane
    // wave: at 0.73 rad/m that is an 8.6 m period, about seventeen evenly spaced parallel stripes across the field
    // from the standard camera. Warping the higher octaves by the lowest breaks the axis alignment, which is the
    // difference between drift and corduroy, and costs three mads.
    float2 q = positionWS.xz;
    half lo = sin(q.x * 0.073 + q.y * 0.041);
    q += lo * 4.3;
    half n = lo * 0.5 + sin(q.x * 0.217 - q.y * 0.163) * 0.3 + sin(q.x * 0.531 + q.y * 0.411) * 0.2;
    half snow = up * (1.0 + n * _TWSnow.w);
    // A definite edge that WANDERS, not an airbrushed ramp: a linear ramp in normal.y fades smoothly from white top
    // to bare side on a curved sandbag, and the reference has a step there with an overhang.
    snow = saturate((snow - 0.5) * 4.0 + 0.5);
    // A frost floor, so nothing is ever bare. At SnowShedBelow 0.22 anything steeper than 77 degrees shows raw
    // brown sandbag and khaki, and in the reference there is not one warm pixel on any vertical face. This also
    // saves the wire, the bunting and the flags, whose normals point sideways.
    return saturate(max(snow, 0.30) * _TWSnow.x);
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
        // Wrap the cell id into 0..256 before hashing. At 300x800 m the raw id reaches 80, so the sin argument
        // reaches ~35,000, where a sin-hash decorrelates and the cells visibly repeat at the far end of the map.
        // Wrapping repeats honestly every 256 cells, which at ten-metre plates is further away than the field.
        float2 cc = c + g; cc -= floor(cc / 256.0) * 256.0;
        float2 h = frac(sin(float2(dot(cc, float2(127.1, 311.7)), dot(cc, float2(269.5, 183.3)))) * 43758.5453);
        half d = length(g + h - f);
        if (d < d1) { d2 = d1; d1 = d; } else if (d < d2) { d2 = d; }
    }
    return d2 - d1;
}

/// What the cracks in molten ground add to a surface.
///
/// The molten rock is in the FLOOR, and both gates below exist because the first version of this did not say so.
/// A glow drawn from world xz alone asks the same question of every surface in that column, so the cracks climbed
/// the sandbag lines and drew a bright band across the middle of every tree stump: the ground's cracks painted
/// onto whatever happened to be standing in them. So it is gated twice. Only near-horizontal faces, because a
/// vertical face is the SIDE of a rock and no light comes out of it; and only close above the molten level,
/// falling off over about half a metre rather than two.
///
/// The crack itself is a width that varies, with a hotter core inside it. A constant width reads as glowing rope
/// laid over the ground; what the reference shows is molten rock seen between plates, wide in places, pinched in
/// others, and near-white in the middle where it is deepest.
half3 TWHeatGlow(float3 positionWS, float3 normalWS, half exposure)
{
    if (_TWHeat.x <= 0.0 || exposure <= 0.002) return half3(0, 0, 0);
    // Only the floor, and only the parts of it turned upward. NOT gated on absolute world height: the first attempt
    // faded the glow out above MoltenLevel, which silently deleted every crack on ground that happened to lie above
    // 0.6 m - which is most of a battlefield with a parapet on it. The caller says whether this is ground at all.
    half where = saturate(normalWS.y * 2.2 - 0.5);
    if (where <= 0.002) return half3(0, 0, 0);

    // Plates stretched along the flow rather than a honeycomb, and the field CENTRED before scaling: at 300x800 m
    // the raw cell coordinate reaches 80, so the sin-hash argument reaches ~35,000, where it decorrelates and the
    // cells start repeating at the far end of the map. Centring keeps it in a range the hash survives.
    float2 q = positionWS.xz * _TWHeat.y * float2(1.0, 0.78);   // slightly stretched; at 0.45 they became parallel ribbons
    half edge = TWPlateEdge(q);
    half w = max(_TWHeat.z, 1e-3) * (0.5 + 1.0 * (sin(positionWS.x * 0.21 + positionWS.z * 0.17) * 0.5 + 0.5));
    // Widen to at least one pixel. A 0.6 m emissive crack is sub-pixel at 200 m, and sub-pixel emissive detail under
    // bloom is the worst artefact available: bright specks crawling along the horizon as the camera moves.
    w = max(w, fwidth(edge) * 1.5);
    half crack = 1.0 - smoothstep(0.0, w, edge);
    half core = 1.0 - smoothstep(0.0, w * 0.34, edge);
    half3 lit = _TWHeatColor.rgb * (crack * crack) + half3(1.0, 0.86, 0.58) * (core * core * 0.85);
    return lit * (_TWHeat.x * where * exposure);
}

/// How black the rock is at this point. Adding glow without taking the albedo down is the "glowing dirt" failure:
/// the reference's plate interior is essentially black RIGHT BESIDE a crack, and it is that contrast, not the
/// emission on its own, that makes the crack look hot. Returns 1 off the lava fields.
half TWHeatCrust(float3 positionWS, float3 normalWS, half exposure)
{
    if (_TWHeat.x <= 0.0 || exposure <= 0.002) return 1.0;
    half where = saturate(normalWS.y * 2.2 - 0.5);
    if (where <= 0.002) return 1.0;
    float2 q = positionWS.xz * _TWHeat.y * float2(1.0, 0.78);
    half edge = TWPlateEdge(q);
    return lerp(1.0, 0.42, saturate(edge * 2.4) * where * exposure);   // dark in the middle of a plate
}

/// Light thrown UP off the floor onto whatever stands on it: strongest low down and on faces turned toward the
/// ground. Without it the lava field is a set of black cutouts against a bright floor, which is the one thing the
/// reference never does - there, every stump and sandbag catches the fire along its underside and its lower edges.
/// _TWGroundLight.a is how strong the bounce is, so the night field can have a little and the lava field a lot.
half3 TWGroundBounce(float3 normalWS, half3 albedo)
{
    if (_TWGroundLight.a <= 0.001) return half3(0, 0, 0);
    // No height term. It had one, referenced to the molten level, and it made the bounce vanish on any prop
    // standing on high ground - the same absolute-height mistake as the cracks above. Nothing on this battlefield
    // is more than a few metres up, so how far a surface faces the ground is the whole of the answer.
    half down = saturate(-normalWS.y * 0.8 + 0.55);
    return _TWGroundLight.rgb * albedo * (down * _TWGroundLight.a);
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
