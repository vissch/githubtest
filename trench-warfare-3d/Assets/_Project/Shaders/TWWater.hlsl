// Phase: B2 (implemented) — what the river sheet (TW/Water) and the puddles painted on the ground (TW/Toon) share: the
// painted depth bands with a pale shore and lapping rings, and the rings thrown by wading men and shell bursts.
// WaterRings.cs owns _TWRings / _TWNow; with none in the scene every ring is long dead and the loop adds nothing.
#ifndef TW_WATER_INCLUDED
#define TW_WATER_INCLUDED

#define TW_RING_COUNT 16
float4 _TWRings[TW_RING_COUNT];   // x, z, start time, size in metres
float _TWNow;

/// Pale expanding rings at a point on the water: two lines a ring, fading as they spread. stir returns how much the
/// water inside a young ring is stirred up (a wading man or a shell clouds it with silt for as long as the ring lives).
half TWRings(float2 xz, out half stir)
{
    half sum = 0; stir = 0;
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
        stir += (1.0 - smoothstep(radius * 0.35, radius * 0.95 + 0.05, d)) * (1.0 - age) * alive;
    }
    stir = saturate(stir);
    return saturate(sum);
}

/// Raindrops on the water: in every cell of two offset grids a ring opens at a random point and time (_TWWet.z = how
/// many cells are raining). No loop over drops and no state: a pixel only ever looks at its own two cells.
half TWRainRings(float2 xz)
{
    half sum = 0;
    if (_TWWet.z > 0.0)
    {
        for (int k = 0; k < 2; k++)
        {
            float2 p = xz * (1.15 + 0.55 * k) + k * 17.3;
            float2 cell = floor(p), f = frac(p);
            float h = frac(sin(dot(cell, float2(127.1, 311.7))) * 43758.5453);
            float2 c = float2(frac(h * 13.7), frac(h * 7.3)) * 0.5 + 0.25;
            float age = frac(_Time.y * (0.75 + 0.5 * h) + h * 9.0);
            float d = distance(f, c);
            sum += (1.0 - smoothstep(0.018, 0.04, abs(d - age * 0.24))) * (1.0 - age) * (1.0 - age) * saturate((_TWWet.z - h * 0.9) * 6.0);
        }
        if (_TWClose > 0.0)
        {
            // up close the small drops show too: a third, finer grid (gone by 30 m, where it would only shimmer)
            float2 p = xz * 3.7 + 41.9;
            float2 cell = floor(p), f = frac(p);
            float h = frac(sin(dot(cell, float2(74.7, 173.3))) * 43758.5453);
            float2 c = float2(frac(h * 11.3), frac(h * 5.9)) * 0.5 + 0.25;
            float age = frac(_Time.y * (1.3 + 0.8 * h) + h * 7.0);
            half fine = _TWClose * (1.0 - saturate((distance(_WorldSpaceCameraPos.xz, xz) - 14.0) / 16.0));
            sum += (1.0 - smoothstep(0.03, 0.07, abs(distance(f, c) - age * 0.30))) * (1.0 - age) * saturate((_TWWet.z - h * 0.9) * 6.0) * fine * 0.8;
        }
    }
    return saturate(sum);
}

/// Raindrops bursting on ground, boards, bags and steel: a grid of 0.3 m cells, and in each cell a run of short time
/// slots. A hash of cell and slot decides whether a drop lands in this slot and where, so splashes never repeat in
/// place or in step; how many slots are live follows the rain (_TWWet.z). A splash is a bright speck that opens into a
/// tiny ring in a seventh of a second. One cell a pixel, no loop, no texture.
half TWRainSplash(float2 xz)
{
    float2 p = xz * 3.3;
    float2 cell = floor(p), f = frac(p);
    float h = frac(sin(dot(cell, float2(269.5, 183.3))) * 43758.5453);
    float slotTime = _Time.y * (2.6 + 1.4 * h) + h * 31.0;
    float slot = floor(slotTime), age = frac(slotTime);
    float r = frac(sin(dot(cell + slot * 0.618, float2(12.9898, 78.233))) * 43758.5453);
    float2 c = float2(frac(r * 17.3), frac(r * 5.71)) * 0.6 + 0.2;
    half lands = step(r, _TWWet.z * 0.55);
    float d = distance(f, c);
    half speck = (1.0 - smoothstep(0.03, 0.09, d)) * (1.0 - smoothstep(0.0, 0.22, age));
    half ring = (1.0 - smoothstep(0.02, 0.05, abs(d - age * 0.34))) * (1.0 - smoothstep(0.15, 0.55, age));
    return lands * saturate(speck * 1.4 + ring * 0.7);
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
    half stir;
    half thrown = TWRings(xz, stir);
    albedo = lerp(albedo, shallow * half3(1.02, 0.86, 0.66), stir * 0.7);   // stirred-up silt clouds the water brown
    half flecks = 0;
    if (_TWClose > 0.0)
    {
        // scum and chaff gathered at the rim of a puddle: pale flecks in the first hand of water, close camera only
        float2 sp = xz * 13.0;
        float fh = frac(sin(dot(floor(sp), float2(39.3, 91.7))) * 43758.5453);
        half fleck = step(0.83, fh) * (1.0 - smoothstep(0.18, 0.42, distance(frac(sp), float2(frac(fh * 7.1), frac(fh * 3.3)) * 0.4 + 0.3)));
        flecks = fleck * smoothstep(0.02, 0.05, depth) * (1.0 - smoothstep(0.10, 0.20, depth)) * _TWClose * (1.0 - saturate((distance(_WorldSpaceCameraPos.xz, xz) - 12.0) / 14.0));
    }
    return lerp(albedo, pale, max(max(max(shore * 0.85, flecks * 0.6), ring * 0.5), max(thrown * 0.75, TWRainRings(xz) * 0.55)));
}

#endif
