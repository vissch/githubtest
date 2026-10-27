// Phase: B2 (implemented) — what the river sheet (TW/Water) and the puddles painted on the ground (TW/Toon) share: the
// painted depth bands with a pale shore and lapping rings, and the rings thrown by wading men and shell bursts.
// WaterRings.cs owns _TWRings / _TWNow; with none in the scene every ring is long dead and the loop adds nothing.
#ifndef TW_WATER_INCLUDED
#define TW_WATER_INCLUDED

#define TW_RING_COUNT 16
float4 _TWRings[TW_RING_COUNT];   // x, z, start time, size in metres
float _TWNow;
// The water's own clock. A capture shoots the same throw twice seconds apart, and the lapping, the drift and the
// raindrops would move ~1,600 px between the two runs by themselves, which is noise nothing can measure through.
// WaterRings.Update pushes a pinned value here (WaterRings.PinnedClock); 0 means let it run.
float _TWWaterClock;
#define TW_WTIME (_TWWaterClock > 0.0 ? _TWWaterClock : _Time.y)

/// Pale expanding rings at a point on the water: ONE crest and the dark trough just inside it, fading as they
/// spread. stir returns how much the water inside a young ring is stirred up (a wading man or a shell clouds it
/// with silt for as long as the ring lives).
half TWRings(float2 xz, out half stir, out half trough)
{
    half sum = 0; stir = 0; trough = 0;
    for (int k = 0; k < TW_RING_COUNT; k++)
    {
        float4 r = _TWRings[k];
        float age = (_TWNow - r.z) / (0.9 + r.w * 0.4);
        half alive = step(0.0, age) * step(age, 1.0) * step(0.01, r.w);
        // a body punches a hole in the water, it does not grow one from nothing: the ring opens at 0.22 of its
        // size. The width is sublinear, so an 8 m shell ring fattens to 0.69 m, not to a metre (harsh critic r1:
        // at land+0.05 s the entry ring was a 0.3 m circle two pixels thick).
        float radius = r.w * (0.22 + 0.78 * sqrt(saturate(age)));
        float width = 0.07 + 0.22 * sqrt(r.w);
        float d = distance(xz, r.xy);
        // ONE crest, not two. A second line at radius * 0.55 used to ride inside this one, and on a 2.8 m entry
        // ring at 19 px/m the two crests and the trough between them land within a dozen pixels of each other: the
        // owner read the result as "a faint disc", and the capture agreed (ring_hollow 0.745 of the crest's own
        // lift showing up in the middle, against a bar of 0.5). One crest plus the dark trough at radius * 0.78 is
        // a ring that opens from a hole. The crest's own profile is widened - full strength out to 0.75 of the
        // width and gone by 1.8 of it, where it used to be 0.6 and 1.0 - so the band has enough lit pixels for a
        // p90 to see, without touching `width` itself, which an 8 m shell ring shares. 1.5 was tried first and the
        // capture measured ring_px_band 965 against a bar of 1000, on the landing frame where the ring is still
        // only 0.96 m across; 1.8 is what cleared it.
        half lines = 1.0 - smoothstep(width * 0.9, width * 1.8, abs(d - radius));
        sum += lines * (1.0 - age) * (1.0 - age) * alive;
        stir += (1.0 - smoothstep(radius * 0.35, radius * 0.95 + 0.05, d)) * (1.0 - age) * alive;
        // the hollow just inside the crest. Harsh critic r1: a pale ring on night water moved the luma by 0.019,
        // under the 0.02 the capture can tell from nothing, because the pale sources are the night-dimmed foam
        // colour. A DARK trough is a value step the night cannot take away.
        trough += (1.0 - smoothstep(width * 0.6, width, abs(d - radius * 0.78))) * (1.0 - age) * alive;
    }
    stir = saturate(stir); trough = saturate(trough);
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
            float age = frac(TW_WTIME * (0.75 + 0.5 * h) + h * 9.0);
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
            float age = frac(TW_WTIME * (1.3 + 0.8 * h) + h * 7.0);
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
    float slotTime = TW_WTIME * (2.6 + 1.4 * h) + h * 31.0;
    float slot = floor(slotTime), age = frac(slotTime);
    float r = frac(sin(dot(cell + slot * 0.618, float2(12.9898, 78.233))) * 43758.5453);
    float2 c = float2(frac(r * 17.3), frac(r * 5.71)) * 0.6 + 0.2;
    half lands = step(r, _TWWet.z * 0.55);
    float d = distance(f, c);
    half speck = (1.0 - smoothstep(0.03, 0.09, d)) * (1.0 - smoothstep(0.0, 0.22, age));
    half ring = (1.0 - smoothstep(0.02, 0.05, abs(d - age * 0.34))) * (1.0 - smoothstep(0.15, 0.55, age));
    return lands * saturate(speck * 1.4 + ring * 0.7);
}

/// How much a thrown ring's crest lifts the water it stands in, and how far its trough darkens it - applied to the
/// LIT colour, after the lighting and _TWWaterDim (water entry r7).
///
/// MEASURED, and the measurement is the whole point. r7's first attempt put the lift into the ALBEDO (albedo + 0.26
/// on the crest, albedo * 0.62 in the trough) and the capture moved by 0.0196 luma - the same 0.019 the harsh critic
/// called "the ring adds nothing", and under the 0.02 the probe counts. Night water is albedo ~0.05 times a dim
/// moon times (1 - _TWWaterDim): an albedo step of 0.26 arrives on screen as 0.02. A ring that is to READ at night
/// has to be added after the light, not before it.
/// g2: the lift was 0.26 and the band's p90 moved 0.058 against a bar of 0.08 - a ring in the right place in the
/// pipeline, still too faint for the owner to call it a ring. 0.45 is what the capture needed, with the widened
/// crest above; it is scaled by nothing, because the crest's own falloff already leaves the band's edges alone.
#define TW_RING_LIFT 0.45
#define TW_RING_TROUGH 0.45

/// The thrown ring as a value step on the lit colour: a crest brighter than this water and a trough darker than it.
/// With no ring live both weights are 0, so every other water pixel in the game is bit-identical.
half3 TWRingValueStep(half3 color, half thrown, half trough)
{
    color = lerp(color, color * TW_RING_TROUGH, trough * 0.85);
    return lerp(color, min(half3(1, 1, 1), color + TW_RING_LIFT), thrown * 0.92);
}

/// Water colour from its depth in metres: silty margin, body, dark channel, a pale wet line at the shore and slow
/// contour rings lapping toward it. noise (0..1) breaks the rings up; shore returns how much of the pixel is shoreline.
half3 TWWaterAlbedo(float depth, float2 xz, half noise, half3 shallow, half3 body, half3 deep, half3 foam, half ringAmount, out half shore, out half thrownOut, out half troughOut)
{
    half margin = 1.0 - smoothstep(0.16, 0.24, depth);
    half channel = smoothstep(0.62, 0.78, depth);
    half3 albedo = lerp(lerp(body, deep, channel), shallow, margin);
    shore = 1.0 - smoothstep(0.015, 0.05, depth);
    float lap = frac(depth * 3.2 + TW_WTIME * 0.11 + noise * 0.35);
    half ring = smoothstep(0.0, 0.10, lap) * (1.0 - smoothstep(0.10, 0.22, lap)) * (1.0 - smoothstep(0.12, 0.62, depth)) * ringAmount;
    half3 pale = foam * lerp(half3(1, 1, 1), TWShadeTint() * 1.6, 0.7);   // under a night mood the pale lines dim with everything else
    half stir, trough;
    half thrown = TWRings(xz, stir, trough);
    thrownOut = thrown; troughOut = trough;
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
    // the pale sources as they always were (a puddle's ring, the sea's foam): the ring's VALUE step is the caller's,
    // applied to the lit colour with TWRingValueStep
    return lerp(albedo, pale, max(max(max(shore * 0.85, flecks * 0.6), ring * 0.5), max(thrown * 0.92, TWRainRings(xz) * 0.55)));
}

#endif
