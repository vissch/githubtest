// Phase: CelFx kit (2026-11-09) - the shared functions of every cel effect shader (TW/Cel/*). The look the owner picked
// (the six H3 reference clips): bold cel shading in 3-4 flat value bands, a white HDR core that blooms, dark ink contours
// on smoke and fire lobes, saturated energy colours. No flipbooks: every shape is a mesh or a field in the fragment.
//
// A record (CelRecord, 128 bytes, CelFxRenderer.cs) is written ONCE when it is spawned; the vertex shader works out where
// it is from the record and the clock (_CelNow), like TW/Debris. Include this after URP's Core.hlsl.
//
// What is here:
//   CelFetch / CelAge / CelLife01 / CelAlive    the record of this instance (GetIndirectInstanceID_Base) and its age
//   CelPosition / CelVelocity                   ballistic motion with drag, gravity and buoyancy
//   CelCurve                                    a value from-to with a power ease over the life
//   CelPal / CelRamp / CelRamp2                 N-step band ramp from a palette row (4 colours + an HDR core)
//   CelAAStep / CelInk / CelErode               anti-aliased steps, an ink line N screen px wide on a field's edge,
//                                               step erosion with a feather
//   CelLobeLight                                camera-as-light shading (n . v, biased up), to be banded
//   CelHash* / CelValue2/3 / CelSimplex3 / CelFbm3   cheap procedural noise
//   CelSd*                                      SDF helpers: capsule, ring, star/spike, cone, sphere
//   CelLodInk / CelDistanceFade / CelAdditiveFog    LOD and fog
#ifndef TW_CELFX_INCLUDED
#define TW_CELFX_INCLUDED

#define UNITY_INDIRECT_DRAW_ARGS IndirectDrawIndexedArgs
#include "UnityIndirect.cginc"
#include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

struct CelRecord
{
    float3 pos;  float born;     // where it was spawned (world) and when (_CelNow seconds; may be in the future)
    float3 vel;  float life;     // its speed (m/s) at birth, and how long it lives (s)
    float3 axis; float seed;     // a direction of its own (kind-specific: a spin axis, a beam's end offset), a seed 0..1
    float4 size;                 // x size at birth (m), y at death, z the ease power, w kind-specific (stretch, squash)
    float4 motion;               // x gravity (m/s2, down), y drag (1/s), z spin (rad/s), w buoyancy (m/s2, up)
    float4 erode;                // x erosion threshold at birth, y at death, z ease power, w feather
    float4 tone;                 // x palette row (+ 256 x the row it cools to), y HDR core multiplier, z opacity, w ink px
    float4 extra;                // kind-specific (see each shader's header)
};

StructuredBuffer<CelRecord> _CelRecords;   // every pool's records, one buffer (set on each kind's material)
StructuredBuffer<float4> _CelPalette;      // CelPalette: 6 float4 a row: c0..c3 dark to light, core (rgb, a threshold), ink (rgb, a bands)
float _CelNow;                             // the clock the records were spawned on (CelFx.Now)
float4 _CelLod;                            // x the zoom tier 0 close .. 1 mid .. 2 far (continuous), y the ink width scale

#define CEL_PAL_STRIDE 6

// ------------------------------------------------------------------ the record
CelRecord CelFetch(uint svInstanceID)
{
    InitIndirectDrawArgs(0);
    // _Base: on D3D SV_InstanceID starts at 0 for every command, so the pool's startInstance must be added (see TW/Debris)
    return _CelRecords[GetIndirectInstanceID_Base(svInstanceID)];
}

float CelAge(CelRecord r) { return _CelNow - r.born; }
float CelLife01(CelRecord r) { return saturate((_CelNow - r.born) / max(r.life, 1e-4)); }
/// 1 while the record is born and not yet dead, else 0: multiply a size by it to collapse the instance to a point.
float CelAlive(CelRecord r) { float t = _CelNow - r.born; return (t >= 0.0 && t <= r.life) ? 1.0 : 0.0; }

/// from (c.x) to (c.y) over k in 0..1, eased by k^c.z (0 or 1 = linear).
float CelCurve(float3 c, float k) { float p = c.z > 0.0 ? c.z : 1.0; return lerp(c.x, c.y, pow(saturate(k), p)); }

float CelDragInt(float d, float t) { return d > 1e-4 ? (1.0 - exp(-d * t)) / d : t; }

/// Where the record is t seconds after its birth: its speed slowed by drag, gravity down, buoyancy up.
float3 CelPosition(CelRecord r, float t)
{
    t = max(t, 0.0);
    float a = r.motion.w - r.motion.x;
    return r.pos + r.vel * CelDragInt(r.motion.y, t) + float3(0.0, 0.5 * a * t * t, 0.0);
}

float3 CelVelocity(CelRecord r, float t)
{
    t = max(t, 0.0);
    return r.vel * exp(-r.motion.y * t) + float3(0.0, (r.motion.w - r.motion.x) * t, 0.0);
}

float3 CelRotate(float3 v, float3 axis, float angle)
{
    float s, c; sincos(angle, s, c);
    return v * c + cross(axis, v) * s + axis * dot(axis, v) * (1.0 - c);
}

// ------------------------------------------------------------------ palette and bands
/// The palette rows packed in tone.x (row + 256 x coolRow), rounded: a float that went through a varying can arrive
/// as 2569.9998, and truncating it picks the row before (pass tone in a nointerpolation varying as well).
uint CelRowA(float packedRow) { return ((uint)(packedRow + 0.5)) & 255u; }
uint CelRowB(float packedRow) { return (((uint)(packedRow + 0.5)) >> 8) & 255u; }
half3 CelPal(uint row, uint i) { return (half3)_CelPalette[row * CEL_PAL_STRIDE + i].rgb; }
half4 CelPalCore(uint row) { return (half4)_CelPalette[row * CEL_PAL_STRIDE + 4]; }
half4 CelPalInk(uint row) { return (half4)_CelPalette[row * CEL_PAL_STRIDE + 5]; }

/// A step at edge, anti-aliased over about a pixel of v.
float CelAAStep(float edge, float v)
{
    float w = max(fwidth(v), 1e-5) * 0.75;
    return smoothstep(edge - w, edge + w, v);
}

/// v in 0..1 (0 dark, 1 light) cut into the palette's bands (2..4: the lightest n of c0..c3), then the core (the core
/// colour x hdr) above the row's core threshold. Flat bands with a one-pixel edge: a cel ramp, never a gradient.
half3 CelRamp(float v, uint row, float hdr)
{
    half4 ink = CelPalInk(row);
    half4 core = CelPalCore(row);
    int n = clamp((int)round(ink.a), 1, 4);
    int first = 4 - n;
    half3 col = CelPal(row, first);
    [unroll] for (int j = 1; j < 4; j++)
        if (j < n) col = lerp(col, CelPal(row, first + j), CelAAStep((float)j / n, v));
    if (core.a < 1.0) col = lerp(col, core.rgb * max(hdr, 0.0), CelAAStep(core.a, v));
    return col;
}

/// The ramp of the record's palette, blended over the life into the row it cools to (tone.x = row + 256 x coolRow),
/// from k = coolFrom on. A fireball that turns to smoke.
half3 CelRamp2(float v, float packedRow, float hdr, float k, float coolFrom)
{
    uint a = CelRowA(packedRow), b = CelRowB(packedRow);
    half3 col = CelRamp(v, a, hdr);
    if (b != a && b != 0u)
    {
        float m = smoothstep(coolFrom, 1.0, k);
        col = lerp(col, CelRamp(v, b, hdr), m);
    }
    return col;
}

half3 CelInkColor(float packedRow, float k, float coolFrom)
{
    uint a = CelRowA(packedRow), b = CelRowB(packedRow);
    half3 c = CelPalInk(a).rgb;
    if (b != a && b != 0u) c = lerp(c, CelPalInk(b).rgb, smoothstep(coolFrom, 1.0, k));
    return c;
}

/// An ink line px screen pixels wide just inside the edge where field crosses `edge` (field above edge is inside).
/// 1 on the line, 0 inside; 0 when px is 0. Width is measured with fwidth, so it is the same at any distance.
float CelInk(float field, float edge, float px)
{
    if (px <= 0.0) return 0.0;
    float d = (field - edge) / max(fwidth(field), 1e-5);   // pixels inside the edge
    return 1.0 - saturate(d / px);
}

/// Step erosion with a feather: 0 below threshold, 1 above threshold + feather (feather 0: a hard step, anti-aliased).
float CelErode(float field, float threshold, float feather)
{
    if (feather <= 1e-4) return CelAAStep(threshold, field);
    return smoothstep(threshold, threshold + feather, field);
}

/// The camera is the light: 1 facing the eye, 0 at the silhouette. up tilts the light toward the sky (smoke lit from
/// above), 0 is the eye exactly. Band the result with CelRamp.
float CelLobeLight(float3 normalWS, float3 viewWS, float up)
{
    float3 l = normalize(viewWS + float3(0.0, up, 0.0));
    return saturate(dot(normalWS, l));
}

// ------------------------------------------------------------------ noise
float CelHash11(float p) { p = frac(p * 0.1031); p *= p + 33.33; p *= p + p; return frac(p); }
float CelHash21(float2 p) { float3 p3 = frac(float3(p.xyx) * 0.1031); p3 += dot(p3, p3.yzx + 33.33); return frac((p3.x + p3.y) * p3.z); }
float CelHash31(float3 p3) { p3 = frac(p3 * 0.1031); p3 += dot(p3, p3.zyx + 31.32); return frac((p3.x + p3.y) * p3.z); }

/// Value noise 0..1.
float CelValue2(float2 p)
{
    float2 i = floor(p), f = frac(p);
    float2 u = f * f * (3.0 - 2.0 * f);
    float a = CelHash21(i), b = CelHash21(i + float2(1, 0)), c = CelHash21(i + float2(0, 1)), d = CelHash21(i + float2(1, 1));
    return lerp(lerp(a, b, u.x), lerp(c, d, u.x), u.y);
}

float CelValue3(float3 p)
{
    float3 i = floor(p), f = frac(p);
    float3 u = f * f * (3.0 - 2.0 * f);
    float n000 = CelHash31(i), n100 = CelHash31(i + float3(1, 0, 0)), n010 = CelHash31(i + float3(0, 1, 0)), n110 = CelHash31(i + float3(1, 1, 0));
    float n001 = CelHash31(i + float3(0, 0, 1)), n101 = CelHash31(i + float3(1, 0, 1)), n011 = CelHash31(i + float3(0, 1, 1)), n111 = CelHash31(i + float3(1, 1, 1));
    return lerp(lerp(lerp(n000, n100, u.x), lerp(n010, n110, u.x), u.y), lerp(lerp(n001, n101, u.x), lerp(n011, n111, u.x), u.y), u.z);
}

// Simplex 3D (after Ashima Arts / Stefan Gustavson, MIT), -1..1.
float3 CelMod289(float3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
float4 CelMod289(float4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
float4 CelPermute(float4 x) { return CelMod289(((x * 34.0) + 1.0) * x); }
float CelSimplex3(float3 v)
{
    const float2 C = float2(1.0 / 6.0, 1.0 / 3.0);
    float3 i = floor(v + dot(v, C.yyy));
    float3 x0 = v - i + dot(i, C.xxx);
    float3 g = step(x0.yzx, x0.xyz);
    float3 l = 1.0 - g;
    float3 i1 = min(g.xyz, l.zxy), i2 = max(g.xyz, l.zxy);
    float3 x1 = x0 - i1 + C.xxx, x2 = x0 - i2 + C.yyy, x3 = x0 - 0.5;
    i = CelMod289(i);
    float4 p = CelPermute(CelPermute(CelPermute(i.z + float4(0.0, i1.z, i2.z, 1.0)) + i.y + float4(0.0, i1.y, i2.y, 1.0)) + i.x + float4(0.0, i1.x, i2.x, 1.0));
    float4 j = p - 49.0 * floor(p * (1.0 / 49.0));
    float4 x_ = floor(j * (1.0 / 7.0));
    float4 y_ = floor(j - 7.0 * x_);
    float4 x = (x_ * 2.0 + 0.5) / 7.0 - 1.0, y = (y_ * 2.0 + 0.5) / 7.0 - 1.0;
    float4 h = 1.0 - abs(x) - abs(y);
    float4 b0 = float4(x.xy, y.xy), b1 = float4(x.zw, y.zw);
    float4 s0 = floor(b0) * 2.0 + 1.0, s1 = floor(b1) * 2.0 + 1.0;
    float4 sh = -step(h, 0.0);
    float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy, a1 = b1.xzyw + s1.xzyw * sh.zzww;
    float3 p0 = float3(a0.xy, h.x), p1 = float3(a0.zw, h.y), p2 = float3(a1.xy, h.z), p3 = float3(a1.zw, h.w);
    float4 norm = rsqrt(float4(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));
    p0 *= norm.x; p1 *= norm.y; p2 *= norm.z; p3 *= norm.w;
    float4 m = max(0.6 - float4(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), 0.0);
    m = m * m;
    return 42.0 * dot(m * m, float4(dot(p0, x0), dot(p1, x1), dot(p2, x2), dot(p3, x3)));
}

/// Two-octave simplex, 0..1: the erosion field of a lobe.
float CelFbm3(float3 p)
{
    return saturate(0.5 + 0.5 * (CelSimplex3(p) * 0.68 + CelSimplex3(p * 2.03 + 17.1) * 0.32));
}

// ------------------------------------------------------------------ SDF helpers (negative inside)
float CelSdSphere(float3 p, float r) { return length(p) - r; }
float CelSdCapsule(float2 p, float2 a, float2 b, float r)
{
    float2 pa = p - a, ba = b - a;
    float h = saturate(dot(pa, ba) / max(dot(ba, ba), 1e-8));
    return length(pa - ba * h) - r;
}
float CelSdCapsule3(float3 p, float3 a, float3 b, float r)
{
    float3 pa = p - a, ba = b - a;
    float h = saturate(dot(pa, ba) / max(dot(ba, ba), 1e-8));
    return length(pa - ba * h) - r;
}
/// A ring of radius r and half-width w.
float CelSdRing(float2 p, float r, float w) { return abs(length(p) - r) - w; }
/// A star of `points` spikes: inner radius rIn, tips at rOut; sharp > 1 thins the spikes.
float CelSdStar(float2 p, float points, float rIn, float rOut, float sharp)
{
    float a = atan2(p.y, p.x);
    float spike = pow(abs(cos(a * points * 0.5)), max(sharp, 1.0));
    return length(p) - lerp(rIn, rOut, spike);
}
/// A 2D cone (a wedge) from the origin along +x: half-angle `angle` (rad), length h. Approximate distance.
float CelSdCone(float2 p, float angle, float h)
{
    float s, c; sincos(angle, s, c);
    float side = dot(float2(p.x, abs(p.y)), float2(-s, c));
    return max(max(side, -p.x), p.x - h);
}

// ------------------------------------------------------------------ LOD and fog
/// The ink width to draw for a wanted width in px: thinner going out (CelFx sets _CelLod.y from the zoom tier).
float CelLodInk(float px) { return px * (_CelLod.y > 0.0 ? _CelLod.y : 1.0); }

/// 1 nearer than near metres from the eye, 0 past far.
float CelDistanceFade(float3 positionWS, float near, float far)
{
    return 1.0 - saturate((distance(_WorldSpaceCameraPos, positionWS) - near) / max(far - near, 1e-3));
}

/// An additive colour dimmed by the fog and the mist the way TW/Flipbook's additive books are.
half3 CelAdditiveFog(half3 color, float3 positionWS, float fogFactor)
{
    color = MixFogColor(color, half3(0, 0, 0), fogFactor);   // additive: the fog takes it toward nothing (no fog: unchanged)
    half mist = 1.0 - saturate(FieldFogAmount(positionWS) + _TWMistColor.a * saturate((_TWMist.x - positionWS.y) * _TWMist.y) * saturate((distance(_WorldSpaceCameraPos, positionWS) - _TWMist.z) * _TWMist.w));
    return color * mist;
}

/// A camera-facing corner: right and up of the view.
float3 CelBillboard(float3 center, float2 corner)
{
    return center + UNITY_MATRIX_V[0].xyz * corner.x + UNITY_MATRIX_V[1].xyz * corner.y;
}

#endif
