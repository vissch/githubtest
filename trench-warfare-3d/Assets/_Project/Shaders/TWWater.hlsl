// Phase: B2 (implemented) — what the river sheet (TW/Water) and the puddles painted on the ground (TW/Toon) share: the
// painted depth bands with a pale shore and lapping rings, and the rings thrown by wading men and shell bursts.
// WaterRings.cs owns _TWRings / _TWNow; with none in the scene every ring is long dead and the loop adds nothing.
#ifndef TW_WATER_INCLUDED
#define TW_WATER_INCLUDED

#define TW_RING_COUNT 16
float4 _TWRings[TW_RING_COUNT];   // x, z, start time, size in metres
float _TWNow;

/// Pale expanding rings at a point on the water: two lines a ring, fading as they spread.
half TWRings(float2 xz)
{
    half sum = 0;
    for (int k = 0; k < TW_RING_COUNT; k++)
    {
        float4 r = _TWRings[k];
        float age = (_TWNow - r.z) / (0.9 + r.w * 0.4);
        half alive = step(0.0, age) * step(age, 1.0) * step(0.01, r.w);
        float radius = r.w * sqrt(saturate(age));
        float width = 0.05 + r.w * 0.035;
        float d = distance(xz, r.xy);
        half lines = (1.0 - smoothstep(width * 0.6, width, abs(d - radius))) + 0.6 * (1.0 - smoothstep(width * 0.6, width, abs(d - radius * 0.55)));
        sum += lines * (1.0 - age) * (1.0 - age) * alive;
    }
    return saturate(sum);
}

/// Water colour from its depth in metres: silty margin, body, dark channel, a pale wet line at the shore and slow
/// contour rings lapping toward it. noise (0..1) breaks the rings up; shore returns how much of the pixel is shoreline.
half3 TWWaterAlbedo(float depth, float2 xz, half noise, half3 shallow, half3 body, half3 deep, half3 foam, half ringAmount, out half shore)
{
    half margin = 1.0 - smoothstep(0.16, 0.24, depth);
    half channel = smoothstep(0.62, 0.78, depth);
    half3 albedo = lerp(lerp(body, deep, channel), shallow, margin);
    shore = 1.0 - smoothstep(0.015, 0.05, depth);
    float lap = frac(depth * 3.2 + _Time.y * 0.11 + noise * 0.35);
    half ring = smoothstep(0.0, 0.10, lap) * (1.0 - smoothstep(0.10, 0.22, lap)) * (1.0 - smoothstep(0.12, 0.62, depth)) * ringAmount;
    half3 pale = foam * lerp(half3(1, 1, 1), TWShadeTint() * 1.6, 0.7);   // under a night mood the pale lines dim with everything else
    return lerp(albedo, pale, max(max(shore * 0.85, ring * 0.5), TWRings(xz) * 0.75));
}

#endif
