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

#endif
