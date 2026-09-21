// Phase: B2 (implemented) — the air shared by every world shader (TW/Toon, TW/Water, the ink pass). Atmosphere.cs sets
// the globals; with no Atmosphere in the scene every density is 0 and these functions return the colour unchanged.
// Ground mist: a pale layer over low, distant ground.
// Fog bank: a wall of fog round the fought-over ground. It is a function of world position only (signed distance to
// the battlefield's rectangle, a slow billow along its edge, thinning with height), so it costs a few instructions a
// pixel, needs no extra geometry or overdraw, and the ink pass can ask the same question to keep lines out of the fog.
#ifndef TW_ATMOSPHERE_INCLUDED
#define TW_ATMOSPHERE_INCLUDED

float4 _TWMist;          // x top height, y 1/depth, z start distance, w 1/range
float4 _TWMistColor;     // rgb, a = density
float4 _TWField;         // xz min, xz max of the fought-over ground
float4 _TWFieldFog;      // x metres outside the bounds where the bank begins (negative = inside), y 1/range, z top height at the edge, w top rise per metre out
float4 _TWFieldFogColor; // rgb, a = density

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
    return reach * low * _TWFieldFogColor.a;
}

half3 ApplyFieldFog(half3 color, float3 positionWS)
{
    return lerp(color, _TWFieldFogColor.rgb, FieldFogAmount(positionWS));
}

#endif
