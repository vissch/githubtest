// Axis is the eye's level right. Extra: mode, timeline offset, ribbon phase, unused.
// Motion.xy is the local ground slope for stationary layers; particles use ballistic motion.
// Size.w is shaft/ribbon height, or the ground card's depth-to-width ratio.
// All motion is evaluated from records; the crescent survives until scene teardown.
Shader "TW/Cel/OrbitalLaser"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+16" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelOrbitalLaser"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float2 q : TEXCOORD1;
                nointerpolation float4 tone : TEXCOORD2;
                nointerpolation float4 state : TEXCOORD3; // mode, age, life fraction, seed
                nointerpolation float4 shape : TEXCOORD4; // size, timeline age, phase, far
                float fog : TEXCOORD5;
            };

            float3 Sweep(float t, float3 right)
            {
                float k = saturate((t - 0.2) / 3.16);
                float angle = lerp(0.45, 3.22, pow(k, 0.88));
                float terminal = lerp(3.18, 3.06, smoothstep(3.20, 3.36, t));
                float correction = lerp(0.55, 0.75, smoothstep(3.30, 3.50, t));
                angle = lerp(angle, terminal, smoothstep(2.92, 3.17, t) * correction);
                return right * (0.45 - sin(angle) * 3.0) + cross(right, float3(0, 1, 0)) * (cos(angle) * 2.24);
            }

            float PixelMetres(float3 p)
            {
                float4 clipP = TransformWorldToHClip(p);
                return 2.0 * abs(clipP.w) / max(abs(UNITY_MATRIX_P._m11) * _ScreenParams.y, 1.0);
            }

            // v21: where an orbit spark is t seconds after its birth - a slow widening swirl round the shaft's axis,
            // clockwise seen from above (a falls; a = 0 is the eye's right, -pi/2 toward the eye), like the ribbons.
            // vel = (start radius, radial speed, rise), motion = (gravity, radial drag, spin, spin drag),
            // erode = (start angle, start height, ground slope x, z).
            float3 OrbitSparkAt(CelRecord r, float3 foot, float3 right, float3 forward, float lean, float t)
            {
                float a = r.erode.x - r.motion.z * CelDragInt(r.motion.w, t);
                float radius = r.vel.x + r.vel.y * CelDragInt(r.motion.y, t);
                float height = r.erode.y + r.vel.z * t - 0.5 * r.motion.x * t * t;
                return foot + (right * cos(a) + forward * sin(a)) * radius + float3(0, height, 0) - right * height * lean;
            }

            // z58: the clip's swirl ribbon traced by hand at f42..f90 (every 4th frame), top end -> bottom end, 10 points
            // by arc length, in the view plane through the origin: (metres right of the shaft, metres up, half width)
            static const float3 SW[130] =
            {
                float3(0.037, 6.273, 0.194), float3(-0.164, 6.050, 0.219), float3(-0.365, 5.826, 0.244), float3(-0.577, 5.613, 0.254), float3(-0.790, 5.400, 0.262), float3(-0.993, 5.178, 0.250), float3(-1.193, 4.953, 0.231), float3(-1.222, 4.661, 0.160), float3(-1.137, 4.388, 0.093), float3(-0.962, 4.144, 0.035), // f42
                float3(-0.537, 6.319, 0.088), float3(-0.875, 6.117, 0.154), float3(-1.205, 5.903, 0.217), float3(-1.525, 5.674, 0.275), float3(-1.783, 5.385, 0.306), float3(-1.944, 5.045, 0.307), float3(-1.868, 4.659, 0.253), float3(-1.671, 4.322, 0.177), float3(-1.449, 3.997, 0.100), float3(-1.214, 3.681, 0.026), // f46
                float3(-1.542, 6.319, 0.141), float3(-1.684, 6.003, 0.191), float3(-1.789, 5.679, 0.231), float3(-1.777, 5.331, 0.242), float3(-1.687, 5.001, 0.233), float3(-1.553, 4.681, 0.213), float3(-1.359, 4.394, 0.170), float3(-1.159, 4.110, 0.125), float3(-0.959, 3.826, 0.076), float3(-0.759, 3.542, 0.026), // f50
                float3(-1.958, 6.319, 0.317), float3(-2.079, 6.018, 0.337), float3(-2.135, 5.713, 0.341), float3(-2.047, 5.401, 0.305), float3(-1.837, 5.162, 0.264), float3(-1.600, 4.940, 0.222), float3(-1.333, 4.758, 0.178), float3(-1.055, 4.591, 0.133), float3(-0.775, 4.426, 0.084), float3(-0.494, 4.264, 0.026), // f54
                float3(-2.304, 6.227, 0.264), float3(-2.149, 5.826, 0.286), float3(-1.965, 5.439, 0.291), float3(-1.736, 5.075, 0.272), float3(-1.470, 4.739, 0.243), float3(-1.181, 4.420, 0.208), float3(-0.892, 4.101, 0.169), float3(-0.604, 3.782, 0.126), float3(-0.299, 3.479, 0.078), float3(0.019, 3.190, 0.026), // f58
                float3(-2.433, 6.227, 0.299), float3(-2.103, 5.817, 0.321), float3(-1.736, 5.444, 0.317), float3(-1.318, 5.125, 0.277), float3(-0.873, 4.846, 0.227), float3(-0.415, 4.586, 0.171), float3(0.041, 4.324, 0.104), float3(0.482, 4.039, 0.065), float3(0.908, 3.730, 0.053), float3(1.152, 3.264, 0.035), // f62
                float3(-2.487, 6.227, 0.317), float3(-1.827, 5.717, 0.268), float3(-1.121, 5.275, 0.163), float3(-0.437, 4.799, 0.093), float3(0.240, 4.314, 0.053), float3(0.964, 3.902, 0.055), float3(1.510, 3.281, 0.201), float3(1.694, 2.496, 0.255), float3(1.351, 1.739, 0.185), float3(0.894, 1.042, 0.053), // f66
                float3(-2.316, 6.597, 0.264), float3(-1.543, 6.128, 0.264), float3(-0.791, 5.625, 0.170), float3(-0.036, 5.127, 0.100), float3(0.724, 4.638, 0.100), float3(1.475, 4.134, 0.191), float3(2.115, 3.527, 0.259), float3(2.174, 2.664, 0.213), float3(1.636, 1.945, 0.123), float3(0.941, 1.366, 0.035), // f70
                float3(0.377, 5.671, 0.053), float3(0.960, 5.117, 0.119), float3(1.522, 4.541, 0.198), float3(1.996, 3.911, 0.264), float3(2.129, 3.151, 0.252), float3(1.741, 2.447, 0.178), float3(1.174, 1.877, 0.097), float3(0.520, 1.419, 0.058), float3(-0.192, 1.049, 0.042), float3(-0.944, 0.764, 0.035), // f74
                float3(2.046, 5.949, 0.088), float3(2.595, 5.303, 0.228), float3(2.632, 4.499, 0.234), float3(2.096, 3.860, 0.166), float3(1.392, 3.388, 0.085), float3(0.677, 2.932, 0.060), float3(-0.047, 2.490, 0.049), float3(-0.774, 2.054, 0.044), float3(-1.487, 1.594, 0.040), float3(-2.199, 1.134, 0.035), // f78
                float3(2.056, 6.088, 0.088), float3(2.633, 5.311, 0.212), float3(2.344, 4.395, 0.207), float3(1.564, 3.750, 0.111), float3(0.713, 3.202, 0.066), float3(-0.161, 2.690, 0.053), float3(-1.083, 2.268, 0.053), float3(-2.035, 1.925, 0.114), float3(-3.036, 1.954, 0.288), float3(-3.914, 1.505, 0.264), // f82
                float3(2.069, 6.505, 0.088), float3(2.272, 5.881, 0.173), float3(2.017, 5.285, 0.191), float3(1.571, 4.811, 0.161), float3(1.080, 4.377, 0.119), float3(0.566, 3.971, 0.072), float3(0.042, 3.577, 0.059), float3(-0.476, 3.176, 0.050), float3(-0.986, 2.764, 0.044), float3(-1.485, 2.338, 0.026), // f86
                float3(2.240, 6.412, 0.070), float3(2.050, 6.003, 0.109), float3(1.794, 5.636, 0.123), float3(1.501, 5.293, 0.123), float3(1.166, 4.994, 0.111), float3(0.817, 4.708, 0.094), float3(0.462, 4.430, 0.077), float3(0.103, 4.157, 0.059), float3(-0.249, 3.875, 0.043), float3(-0.597, 3.588, 0.026), // f90
            };
            float3 SwAt(int k, float u)
            {
                float f = saturate(u) * 9.0;
                int i = min((int)floor(f), 8);
                float t = f - i;
                float3 p0 = SW[k * 10 + max(i - 1, 0)], p1 = SW[k * 10 + i], p2 = SW[k * 10 + i + 1], p3 = SW[k * 10 + min(i + 2, 9)];
                return 0.5 * (2.0 * p1 + (p2 - p0) * t + (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t * t
                    + (3.0 * p1 - p0 - 3.0 * p2 + p3) * t * t * t);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float mode = r.extra.x;
                float3 right = normalize(r.axis);
                float3 forward = cross(right, float3(0, 1, 0));
                float size = CelCurve(r.size.xyz, k);
                float age = t + r.extra.y;
                float3 p = r.pos;
                if (mode > 2.5 && mode < 3.5)
                {
                    float3 shift = -forward * 0.60 + right * 0.12;
                    p += shift + float3(0, dot(shift.xz, r.motion.xy), 0);
                }
                float2 q = v.uv * 2.0 - 1.0;
                float far = saturate(_CelLod.x - 1.0);
                float3 w;
                if (mode < 0.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.18, 0);
                    float3 side = normalize(cross(float3(0, 1, 0), _WorldSpaceCameraPos - p));
                    float arriving = 1.0 - smoothstep(0.075, 0.13, t); // v12: clip f18 still wide, f19 ~1.2x
                    // v26: the clip's wall is half the frame at f18, the shaft ~2x at f19 and 1.2x at f20 (v12-v25: full
                    // width to f18, 1.27x at f19)
                    float arrival = 1.0 + 9.0 * (1.0 - smoothstep(0.06, 0.10, t)) + 1.1 * (1.0 - smoothstep(0.10, 0.19, t))
                        * smoothstep(0.06, 0.10, t);
                    arrival = lerp(arrival, 1.0, far * 0.7);
                    float h = v.uv.x * v.uv.x * r.size.w;
                    float foot = 1.0 + exp(-h * 1.35) * lerp(0.5, 1.8, far) * (1.0 - arriving);
                    float width = max(size * arrival * foot, PixelMetres(p) * far * 3.0);
                    // v22d: the clip's shaft steps down at f91-f92, holds a thin line to f99 and is gone at f100
                    width *= lerp(1.0, lerp(1.50, 1.15, smoothstep(1.9, 2.9, t)), smoothstep(0.10, 0.20, t)); // z09: 0.17-0.40 // z01: clip shaft 145-170 px (ours 91-113, then 130-148)
                    width *= lerp(1.0, 0.88, smoothstep(0.10, 0.20, t)); // z62: owner - the beam a bit smaller
                    width *= lerp(1.0, 0.80, smoothstep(3.30, 3.38, t)) * lerp(1.0, 0.45, smoothstep(3.40, 3.47, t)); // z26 // z16 // z15: the clip's f96-f100 bolt is 10-15 px: the clip's f96 shaft is a ~20 px line
                    width *= lerp(1.0, 0.58, smoothstep(3.15, 3.21, t)) * lerp(1.0, 0.62, smoothstep(3.21, 3.50, t))
                        * lerp(1.0, 0.04, smoothstep(3.505, 3.545, t)); // z30 // z29: stands at f100 (3.50-3.545)
                    q = float2((v.uv.y * 2.0 - 1.0) * 1.5, h);
                    float lean = smoothstep(1.70, 3.20, age) * 0.19;
                    lean -= smoothstep(3.25, 3.45, age) * 0.06 * lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
                    float wallK = saturate((arrival - 1.0) / 9.0); // v26b: the tilt and lift follow the width (the clip's f18 wall stands)
                    w = p + (float3(0, h + wallK * 1.05, 0) + side * q.x * width - right * h * lean) * alive;
                    w += (-forward * 3.0 - right * 0.25 + float3(0, -q.x * 2.0 - 1.2, 0)) * wallK * alive;
                }
                else if (mode < 1.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.2, 0);
                    float h = v.uv.x * r.size.w;
                    // v14 (owner: "the reference is erratic"): the clip's swirl ribbons wrap the shaft 1.5-3 m out and
                    // land somewhere new every frame or two (f60-f67); ours was one thin strip 0.3-2 m out.
                    // z58: the traced ribbon replayed in the camera plane (z54-z57: a helix formula)
                    float3 cr = normalize(UNITY_MATRIX_V[0].xyz); // z59: the view plane (z58: an upright plane)
                    float3 cu = normalize(UNITY_MATRIX_V[1].xyz);
                    float second = step(1.0, r.extra.z);
                    float kf = clamp((age - 1.125) * 6.0, 0.0, 11.999);
                    int k0 = (int)floor(kf);
                    float kt = kf - k0;
                    float u = 1.0 - v.uv.x;
                    float3 a = lerp(SwAt(k0, u), SwAt(k0 + 1, u), kt);
                    float uA = max(u - 0.01, 0.0), uB = min(u + 0.01, 1.0);
                    float3 d = lerp(SwAt(k0, uA), SwAt(k0 + 1, uA), kt) - lerp(SwAt(k0, uB), SwAt(k0 + 1, uB), kt);
                    float lean = smoothstep(1.70, 3.20, age) * 0.19;
                    lean -= smoothstep(3.25, 3.45, age) * 0.06 * lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
                    // the second record is the thinner strand that rides outside the ribbon until ~f60
                    float3 centre = p + cr * (a.x - 0.42 * second) + cu * (a.y + 0.25 * second) - right * (a.y / max(cu.y, 0.3)) * lean;
                    float3 tangent = normalize(cr * d.x + cu * d.y);
                    float3 side = normalize(cross(tangent, _WorldSpaceCameraPos - centre));
                    q = float2(v.uv.y * 2.0 - 1.0, v.uv.x);
                    float wide = max(a.z, 0.012) / 0.47 * lerp(1.0, 0.5 * (1.0 - smoothstep(1.70, 1.95, age)), second);
                    w = p + (centre - p + side * q.x * wide) * alive;
                }
                else if (mode > 3.5 && mode < 4.5)
                {
                    float3 sweep = Sweep(age, right);
                    p += sweep + float3(0, dot(sweep.xz, r.motion.xy) + 0.35, 0);
                    float collapse = lerp(1.0, 0.20, smoothstep(3.26, 3.45, age));
                    w = CelBillboard(p, float2(q.x, q.y * 1.30 * collapse) * size * 0.90 * alive);
                    w += normalize(_WorldSpaceCameraPos - w) * 2.0; // z21: clear of the terrain (its base was cut straight at the ground line)
                }
                else if (mode > 5.5 && mode < 6.5)
                {
                    float a = v.uv.x * 6.283185307;
                    float ringDepth = r.extra.z > 0.5 ? 0.85 : 1.0; // v09: r26's (measured right)
                    float3 radial = right * cos(a) + forward * (sin(a) * ringDepth);
                    float3 tangent = -right * sin(a) + forward * (cos(a) * ringDepth);
                    float expansionDrag = r.motion.z > 0.0 ? r.motion.z : 0.8;
                    float radius = r.size.x + r.size.y * CelDragInt(expansionDrag, t);
                    float wave = sin(a * 7.0 + r.seed * 43.0) * 0.06 + sin(a * 13.0 + r.seed * 19.0) * 0.02; // z17: 0.20 a wobbly outline // v12: was 0.10 (corners)
                    float3 centre = p + radial * (radius + wave * 1.6);
                    centre.y += dot((centre - p).xz, r.motion.xy) + 0.40 + wave;
                    float3 side = normalize(cross(tangent, _WorldSpaceCameraPos - centre));
                    // v13: measured on the clip's centre column - ring 1's front band is 30-50 px (~0.5 m) at
                    // f27-f38, ring 2's opening torus ~0.9 m at f99 (v04's 1.2 m was read off 0.42x sheets).
                    // v15: ring 1's torus read as a thin line (side bands 5-41 px vs the clip's 52-119): 0.42 m.
                    // v16: 0.58 m - the clip's ring-1 torus is a soft dust band 50-60 px thick at f26-f27.
                    // v26b: the clip's tori are thick puffy rolls (40-60 px at f27-f30, f99); ours filmed thin ribbons
                    float width = (r.extra.z > 0.5 ? 1.30 : 1.42) * /* z37 (z36 1.30 / 1.55) */ /* z20: ring 1 1.30 */ /* v26e: 1.40 / 1.05 */ lerp(0.55, 1.0, smoothstep(0.0, 0.20, t)) // v18 / v19: 0.85 / 0.70
                        * (1.0 - smoothstep(0.25, 0.80, k) * 0.35);
                    q = float2(a, v.uv.y * 2.0 - 1.0);
                    w = p + (centre - p + side * q.y * width) * alive;
                }
                else if (mode > 11.5)
                {
                    // v22 lab chart (celfx/runs/orbital_laser/_grade_hdr): a square card facing the eye, drawn toward it.
                    float3 toEye = _WorldSpaceCameraPos - p;
                    float dist = max(length(toEye), 0.01);
                    float pull = min(3.0, dist * 0.5);
                    w = CelBillboard(p + toEye / dist * pull, q * r.size.x * ((dist - pull) / dist) * alive);
                }
                else if (mode > 10.5)
                {
                    // v21 (owner on v20: "the sparks should wrap around the beam"): a spark shed from the shaft rides a
                    // slow widening swirl round it. Clip f40-f95 (_eye_v/sparkmap.py, sparktrack.py): ~27 a frame,
                    // balanced left / right, 0-300 px up the shaft, drifting right to left across the front at ~4 m/s
                    // and rising ~1.5 m/s (v06-v20's streaks flew up and to the right). Size.w is the trail (s); the
                    // strip bends along the orbit. Behind the shaft a spark is hidden where the shaft covers it.
                    // v21b: it circles the point where it was shed (extra.y = its birth on the timeline); the clip's
                    // sparks stay there while the shaft moves on (their drift relative to the foot is the foot's own).
                    float3 sweepBorn = Sweep(r.extra.y, right);
                    float3 born = p + sweepBorn + float3(0, dot(sweepBorn.xz, r.erode.zw), 0);
                    float3 sweep = Sweep(age, right);
                    float3 foot = p + sweep + float3(0, dot(sweep.xz, r.erode.zw), 0);
                    float lean = smoothstep(1.70, 3.20, age) * 0.19;
                    lean -= smoothstep(3.25, 3.45, age) * 0.06 * lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
                    float along = v.uv.x;
                    float th = max(t - r.size.w * (1.0 - along), 0.0);
                    float3 at = OrbitSparkAt(r, born, right, forward, lean, th);
                    float3 dir = at - OrbitSparkAt(r, born, right, forward, lean, max(th - 0.01, 0.0));
                    dir = dot(dir, dir) > 1e-8 ? normalize(dir) : float3(0, 1, 0);
                    float3 side = cross(dir, normalize(_WorldSpaceCameraPos - at));
                    side = dot(side, side) > 1e-6 ? normalize(side) : UNITY_MATRIX_V[0].xyz;
                    float3 flatEye = _WorldSpaceCameraPos - foot;
                    flatEye.y = 0.0;
                    flatEye = normalize(flatEye);
                    float3 off = at - foot;
                    off.y = 0.0;
                    float facing = dot(off, flatEye) / max(length(off), 1e-3);
                    float lateral = abs(dot(off, cross(float3(0, 1, 0), flatEye)));
                    float shaftHalf = 0.84 * (1.0 + 0.5 * exp(-1.35 * max(at.y - foot.y, 0.0)))
                        * lerp(1.0, 0.36, smoothstep(3.15, 3.50, age)) * (1.0 - smoothstep(3.50, 3.545, age)); // v22d: the shaft's shutdown
                    float behind = smoothstep(0.10, -0.30, facing);
                    float hidden = behind * (1.0 - smoothstep(shaftHalf * 0.85, shaftHalf * 1.20, lateral));
                    float width = size * 2.0 * lerp(0.60, 1.0, along) * (1.0 - hidden) * lerp(1.0, 0.65, behind); // v22d: x2 - the glow round the dash
                    q = float2(v.uv.y * 2.0 - 1.0, along);
                    w = alive > 0.5 ? at + side * q.x * width * 0.5 : p;
                }
                else if (mode > 6.5)
                {
                    p = CelPosition(r, t);
                    float angle = mode < 7.5 && r.extra.z > 0.5 ? 0.0 : t * r.motion.z;
                    q = float2(q.x * cos(angle) - q.y * sin(angle), q.x * sin(angle) + q.y * cos(angle));
                    float2 corner = v.uv * 2.0 - 1.0;
                    if (mode > 8.5 && mode < 9.5)
                    {
                        // v22b: a needle points along its flight on screen - the clip's fly out of the splash radially
                        // (v22's sheared quads stood upright: hairlines in the middle of the splash)
                        float3 nv = CelVelocity(r, t);
                        float2 sv = float2(dot(nv, UNITY_MATRIX_V[0].xyz), dot(nv, UNITY_MATRIX_V[1].xyz));
                        sv = dot(sv, sv) > 1e-6 ? normalize(sv) : float2(0.0, 1.0);
                        corner.y *= r.size.w;
                        corner = float2(sv.y, -sv.x) * corner.x + sv * corner.y;
                    }
                    else if (mode > 8.5) { corner.x += corner.y * r.extra.z; corner.y *= r.size.w; }
                    else if (mode > 7.5)
                    {
                        corner.y *= r.size.w * lerp(0.85, 1.05, smoothstep(0.25, 0.55, t));
                        corner.x *= r.extra.z > 0.5 ? 1.0 : 1.15; // z08: ring 2 not widened
                        if (r.extra.z > 1.5) corner *= lerp(0.05, 1.0, smoothstep(0.10, 0.24, t)); // z28 // z20 // z19 (z18): ring-1 puffs swell out of the torus from f28
                        if (r.extra.z > 0.5 && r.extra.z < 1.5) corner *= 1.0 + 0.25 * (1.0 - smoothstep(0.35, 0.65, t));
                        float3 tangent = normalize(cross(float3(r.vel.x, 0, r.vel.z), float3(0, 1, 0)));
                        float2 screenTangent = float2(dot(tangent, UNITY_MATRIX_V[0].xyz), dot(tangent, UNITY_MATRIX_V[1].xyz));
                        screenTangent = normalize(screenTangent);
                        if (screenTangent.x < 0.0) screenTangent = -screenTangent;
                        corner = screenTangent * corner.x + float2(-screenTangent.y, screenTangent.x) * corner.y;
                    }
                    w = CelBillboard(p, corner * size * alive);
                    // The terminal burst sits at contact in screen space, with its lower rays clear of the terrain.
                    if (mode > 9.5) w += normalize(_WorldSpaceCameraPos - w) * 3.0;
                }
                else
                {
                    // The illuminated crater stays fixed while the beam traces its left rim.
                    if (mode > 2.5 && mode < 3.5 && far > 0.5)
                    {
                        float shutdown = smoothstep(3.17, 3.35, t) * (1.0 - smoothstep(4.5, 4.8, t));
                        size = lerp(size, 12.0, shutdown);
                    }
                    float stretch = r.size.w > 0.0 ? r.size.w : 1.0;
                    float widthScale = mode > 2.5 && mode < 3.5 ? 0.95 : 1.0;
                    w = p + (right * q.x * widthScale + forward * q.y * stretch) * size * alive;
                    w.y += dot((w - p).xz, r.motion.xy) + (mode > 5.5 ? 0.45 : 0.2);
                    // Grid vertices lift the broken rim above the terrain, while the inner bed stays low.
                    if ((mode > 2.5 && mode < 3.5) || (mode > 4.5 && mode < 5.5))
                    {
                        float2 groundQ = mode > 4.5 ? q * size / 6.7 : q;
                        float rad = length(groundQ), a = atan2(groundQ.y, groundQ.x);
                        float n = CelValue2(groundQ * 8.0 + r.seed * 17.0);
                        float at = mode < 3.5 ? 0.75 : 0.52;
                        float offset = (n - 0.5) * 0.07;
                        float lip = exp(-pow((abs(rad - at - offset) - (mode < 3.5 ? 0.14 : 0.045)) * 24.0, 2.0));
                        float rise = smoothstep(0.05, 0.30, t) * (0.08 + n * 0.22);
                        w.y += lip * rise * alive;
                    }
                    w += normalize(_WorldSpaceCameraPos - w) * 0.06;
                }
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.q = q;
                o.tone = r.tone;
                o.state = float4(mode, t, k, r.seed);
                o.shape = float4(size, age, r.extra.z, far);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            float Zig(float x, float seed)
            {
                float cell = floor(x), f = frac(x);
                return lerp(CelHash21(float2(cell, seed)), CelHash21(float2(cell + 1, seed)), f) * 2.0 - 1.0;
            }

            half4 Energy(half3 colour, float alpha, Varyings i)
            {
                return half4(lerp(CelAdditiveFog(colour, i.positionWS, i.fog), colour * 0.9, i.shape.w * 0.8), alpha);
            }

            // v22: the splash's five value tiers, written for an hdr=1 film (celfx/runs/orbital_laser/_grade_hdr: there
            // the night grade does not fold at 1 - neutral 1.0 shows 254,254,229, >= 1.33 white). e is a light level:
            // low 146,163,198 | -0.45 mid 159,200,233 | -0.10 pale 182,234,245 | 0.25 white 219,249,251 | 0.55 blown 250,251,254
            half3 SplashTier(float e)
            {
                half3 c = half3(0.447, 0.498, 0.606);
                c = lerp(c, half3(0.467, 0.622, 0.753), CelAAStep(-0.45, e));
                c = lerp(c, half3(0.505, 0.796, 0.963), CelAAStep(-0.10, e));
                c = lerp(c, half3(0.567, 0.897, 1.261), CelAAStep(0.25, e));
                c = lerp(c, half3(0.80, 1.00, 1.60), CelAAStep(0.55, e)); // v25: 0.674,0.897,2.477 bloomed
                c = lerp(c, half3(0.92, 1.14, 1.75), CelAAStep(0.85, e)); // v25: 0.80,1.05,3.0 bloomed // v24c: blown through the splash's fog
                return c;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float mode = i.state.x, t = i.state.y, k = i.state.z, seed = i.state.w;
                float2 q = i.q;
                uint row = CelRowA(i.tone.x);
                if (mode < 0.5)
                {
                    float tick = floor(t * 12.0);
                    float bend = sin(q.y * 1.1 + tick) * 0.006; // z40: 0.02
                    float ignition = 1.0 - smoothstep(0.075, 0.13, t);
                    bend *= 1.0 - ignition;
                    float boltLate = smoothstep(3.38, 3.45, t); // z16: from f97 (z15 3.25-3.36) // z15: the dying shaft is a jagged bolt
                    bend += Zig(q.y * 2.3 + tick * 3.7, seed * 11.0) * 0.55 * boltLate;
                    // v14 (owner: "the reference is erratic"): the clip's shaft bands are re-drawn every frame or two -
                    // its solid body (columns beam-coloured over y0-150) is 42-174 px and its white core 24-50 px,
                    // jumping frame to frame (ours a steady 116-123 / 27-31); its envelope sits off-centre at times.
                    float drawTick = floor(t * 12.0 + 0.25);
                    float settled = 1.0 - ignition;
                    float dj0 = (CelHash11(drawTick * 1.37 + seed * 3.0) - 0.5) * settled;
                    float dj1 = (CelHash11(drawTick * 2.71 + seed * 5.0) - 0.5) * settled;
                    float dj2 = (CelHash11(drawTick * 4.13 + seed * 7.0) - 0.5) * settled;
                    float envShift = dj0 * 0.16;
                    // z40: each side a ruler line of its own per drawing (own width, own slope) with a few slant-cut saw
                    // notches - the clip's edges stray 3-4 px from straight, z39's zig-zag 6
                    float sgnE = (q.x - bend - envShift) < 0.0 ? -1.0 : 1.0;
                    float sideId = drawTick * 1.91 + sgnE * 17.0 + seed * 23.0;
                    float slantE = (CelHash11(sideId) - 0.5) * 0.050 * (min(q.y, 6.0) - 2.0);
                    float nu = q.y * 0.95 + t * 5.0 + CelHash11(sideId + 1.0) * 9.0; // z41: 0.55
                    float nCell = floor(nu);
                    float notch = step(CelHash11(nCell * 3.7 + sideId * 1.3), 0.34)
                        * saturate(min(frac(nu) / 0.80, (1.0 - frac(nu)) / 0.20)) // z41: pointed both ends (a square step)
                        * lerp(0.04, 0.11, CelHash11(nCell * 5.1 + sideId));
                    float edge = (lerp(0.58, 0.64, settled) + ((CelHash11(sideId + 2.0) - 0.5) * 0.07 + slantE - notch
                        + 0.008 * Zig(q.y * 1.9 - t * 11.0, drawTick * 0.7 + seed)) * settled)
                        * (1.0 + dj1 * 0.18);
                    // v26: the dissolving wall's edge torn (clip f18)
                    float tornW = smoothstep(0.055, 0.08, t) * (1.0 - smoothstep(0.095, 0.115, t)); // v26b: f18 only (f19 teeth)
                    edge += tornW * (0.16 * Zig(q.y * 0.45 + seed * 7.0, seed * 13.0) + 0.05 * Zig(q.y * 1.6, seed * 3.0)); // v26b: few big tears
                    float x = abs(q.x - bend);
                    float xe = abs(q.x - bend - envShift);
                    float body = 1.0 - CelAAStep(edge, xe);
                    // z41: loose pointed slivers just outside the edge
                    [unroll] for (int sv = 0; sv < 3; sv++)
                    {
                        float vid = drawTick * 2.3 + sv * 5.7 + seed * 17.0;
                        float vu = q.y * 0.45 + t * 4.0 + CelHash11(vid + 1.0) * 7.0;
                        float vf = frac(vu);
                        float vw = lerp(0.012, 0.030, CelHash11(vid + 2.0)) * saturate(min(vf / 0.70, (1.0 - vf) / 0.30))
                            * step(CelHash11(floor(vu) * 2.9 + vid), 0.45);
                        float vx = envShift + bend + (CelHash11(vid) < 0.5 ? -1.0 : 1.0)
                            * (0.64 + lerp(0.03, 0.11, CelHash11(vid + 3.0)) + (vf - 0.5) * 0.06);
                        body = max(body, (1.0 - CelAAStep(vw, abs(q.x - vx))) * step(1e-4, vw) * settled * (1.0 - boltLate));
                    }
                    float cyan = 1.0 - CelAAStep(edge * (0.64 + dj2 * 0.20), xe); // z40: 0.74
                    float xc = abs(q.x - bend - dj1 * 0.20); // z38: the core off-centre, a new place each drawing
                    float core = 1.0 - CelAAStep((0.16 + (0.025 * Zig(q.y * 1.7 - t * 4.0, 71.0)
                        + 0.028 * Zig(q.y * 0.8 - t * 9.0, drawTick * 1.3 + seed * 3.0)) * (1.0 - ignition)) // z38: torn (z40: 0.060)
                        * (1.0 + settled * 0.30) * (1.0 + dj2 * 0.45)
                        * lerp(1.0, 0.45, smoothstep(3.21, 3.38, t)), lerp(x, xc, settled)); // v22d: the clip's core 26/18/11 px at f92/94/96
                    // v11: from f18 the still-wide wall dissolves - darker, its white core gone (clip f18-f19).
                    // v13: f18 only - the clip's f19 shaft already has its white core back.
                    float dissolve = smoothstep(0.055, 0.08, t) * (1.0 - smoothstep(0.095, 0.115, t));
                    core *= 1.0 - dissolve;
                    // v13: the clip's shaft colours are paler (cyan band R 135-200, outer blue R 68-101).
                    // v25: the clip's crisp bands (blue 90,152,236 | pale cyan 170,247,250 | white | navy 97,113,178 edge)
                    // through the hdr=1 chart, low enough not to bloom (v24e's 1.6 / 2,4,6 / halo filmed a soft glow ramp)
                    half3 c = lerp(half3(0.28, 0.47, 0.82), half3(0.50, 0.93, 1.13), cyan);
                    // v14: the dark seam jumps with the drawing (and swaps sides in about a third of them).
                    float seamAt = (0.32 + dj0 * 0.25) * (CelHash11(drawTick * 5.9 + seed) < 0.30 * settled ? -1.0 : 1.0)
                        + envShift;
                    float seam = (1.0 - CelAAStep(0.04, abs(q.x - seamAt - Zig(q.y * 1.2 - t * 3.0, seed) * 0.11)))
                        * smoothstep(0.3, 0.65, CelValue2(float2(q.y * 1.4 - t * 3.0, seed * 31.0))) * (1.0 - ignition);
                    c = lerp(c, half3(0.33, 0.38, 0.59), seam);
                    // z38: streaks - re-laid every drawing, each its own blue, torn along the shaft and racing down it
                    [unroll] for (int sb = 0; sb < 7; sb++)
                    {
                        float sid = drawTick * 3.17 + sb * 7.31 + seed * 13.0;
                        float sx = (CelHash11(sid) * 2.0 - 1.0) * 0.50 + envShift + bend
                            + (CelHash11(sid + 5.0) - 0.5) * 0.030 * (min(q.y, 6.0) - 2.0); // z40: straight, a slope of its own
                        float sw = lerp(0.020, 0.10, CelHash11(sid + 1.0));
                        float sOn = CelAAStep(0.42, CelValue2(float2(q.y * 0.22 - t * 4.0 + sid
                            + (q.x - sx) * (CelHash11(sid + 6.0) < 0.5 ? 2.2 : -2.2), sid * 1.7))); // z40: slant-cut ends
                        float sm = (1.0 - CelAAStep(sw, abs(q.x - sx))) * sOn * settled * (1.0 - boltLate);
                        float pick = CelHash11(sid + 3.0);
                        half3 sc = pick < 0.30 ? half3(0.11, 0.27, 0.92) : pick < 0.50 ? half3(0.15, 0.22, 0.58)
                            : pick < 0.75 ? half3(0.40, 0.78, 1.12) : half3(0.62, 1.02, 1.22);
                        c = lerp(c, sc, sm);
                    }
                    c = lerp(c, half3(0.20, 0.42, 1.55), boltLate * 0.75); // z15: vivid blue as it dies
                    c = lerp(c, half3(0.83, 1.05, 1.59), core);
                    // v13: while it dissolves the wide wall is one deep translucent blue (clip f18 median 55,103,242).
                    c = lerp(c, half3(0.18, 0.36, 0.98) * (0.50 + 0.85 * CelValue2(float2(q.x * 11.0 + seed, q.y * 0.10))), dissolve); // z29 // z09: streaked
                    float halo = 0.0; // v25: no glow round the shaft (the clip's edge is a hard navy line)
                    float arrival = 1.0 - smoothstep(0.075, 0.13, t);
                    float bottom = smoothstep(0.0, 0.12, q.y - arrival * (0.3 + x * x * 0.7));
                    float fade = (1.0 - smoothstep(0.988, 1.0, k)) * (1.0 - 0.12 * dissolve); // z29: 0.975
                    return Energy((c * body + CelPal(row, 1) * halo) * i.tone.z * fade * bottom,
                        body * i.tone.z * fade * bottom * (1.0 - 0.12 * dissolve), i); // z29: 0.35 // v26: the f18 wall translucent
                }
                if (mode < 1.5)
                {
                    // z44: smoke wisps - the strip is cut into rising spindles with a lumpy outline and fingered ends
                    float rTick = floor(t * 6.0 + 0.25); // the lumps hold for two drawings
                    // z54: one continuous ribbon; its foot comes down to the splash at ~f57
                    float sId = seed * 31.0;
                    // z58: the strip is the traced ribbon end to end; its width comes from the trace
                    float sv = q.y;
                    float present = 1.0;
                    float cx = 0.05 * sin(sv * 9.0 + CelHash11(sId + 3.0) * 6.2831853 + t * 1.6);
                    float xs = q.x - cx;
                    float svx = sv + xs * (CelHash11(sId + 5.0) - 0.5) * 0.06; // slanted ends
                    float prof = pow(min(saturate(svx / 0.05), saturate((1.0 - svx) / 0.05)), 0.7); // pointed ends
                    float hw = 0.60 * prof;
                    float ph = CelHash11(sId + 7.0) * 6.2831853;
                    float boil = rTick * 0.9 - t * 3.0; // z54: the lumps travel up
                    float hwP = hw * (0.78 + 0.14 * abs(sin(sv * 34.0 + ph + boil)) + 0.08 * abs(sin(sv * 61.0 - ph * 1.3)));
                    float hwN = hw * (0.78 + 0.14 * abs(sin(sv * 29.0 - ph * 0.7 + boil)) + 0.08 * abs(sin(sv * 53.0 + ph * 2.1)));
                    float e = max(xs > 0.0 ? hwP : hwN, 1e-3);
                    float d = abs(xs);
                    // each outer edge is cut hard along some stretches and fades out along others
                    float softP = smoothstep(0.54, 0.74, CelValue2(float2(sv * 5.0 + sId, 3.0)));
                    float softN = smoothstep(0.54, 0.74, CelValue2(float2(sv * 4.0 - sId, 9.0)));
                    float soft = xs > 0.0 ? softP : softN;
                    float mask = (1.0 - CelAAStep(e, d)) * lerp(1.0, 1.0 - smoothstep(e * 0.48, e, d), soft);
                    mask *= present;
                    float forked = 1.0;
                    float mirror = 1.0;
                    float xn = mirror * xs / e;
                    float slateOn = 1.0, periOn = 1.0;
                    float b0 = lerp(-1.3, -0.18 + 0.26 * sin(sv * 5.3 + ph * 1.7), slateOn); // z61: -0.18
                    float b1 = lerp(1.3, 0.78 + 0.16 * sin(sv * 4.1 - ph * 2.3), periOn);
                    float r0 = -0.80 + 0.08 * sin(sv * 11.0 + ph);
                    [unroll] for (int sl = 0; sl < 2; sl++)
                    {
                        float lid = sId + sl * 4.7;
                        float lx = mirror * (sl == 0 ? b0 : b1) * e;
                        float lw = 0.0; // z58: no forked end
                        mask *= lerp(1.0, CelAAStep(lw, abs(xs - lx)), step(1e-3, lw) * step(abs(sl == 0 ? b0 : b1), 1.0));
                    }
                    float fade = smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(3.04, 3.22, i.shape.y))
                        * smoothstep(0.95, 1.20, i.shape.y); // z29: the clip has no ribbon before ~f42
                    // lane borders: crisp where the noise is low, a wide gradient where it is high
                    float s0 = lerp(0.03, 0.42, smoothstep(0.35, 0.65, CelValue2(float2(sv * 3.3 + sId, 17.0))));
                    float s1 = lerp(0.03, 0.50, smoothstep(0.30, 0.60, CelValue2(float2(sv * 2.7 - sId, 23.0))));
                    float wA = smoothstep(b0 - s0, b0 + s0, xn);
                    float wC = smoothstep(b1 - s1, b1 + s1, xn);
                    float rim = (1.0 - CelAAStep(r0, xn)) * slateOn;
                    float warm = CelHash11(sId + 15.0);
                    half3 slate = lerp(half3(0.36, 0.35, 0.40), half3(0.41, 0.36, 0.38), warm); // z58: the taupe underside
                    half3 core = lerp(half3(0.60, 0.70, 0.95), half3(0.78, 0.83, 0.99), smoothstep(b0, b1, xn)); // z58: the pale face
                    half3 peri = lerp(half3(0.50, 0.54, 0.82), half3(0.50, 0.47, 0.58), step(0.6, warm));
                    half3 col = lerp(slate, core, wA);
                    col = lerp(col, peri, wC);
                    col = lerp(col, half3(0.55, 0.63, 0.84), rim);
                    float lane = lerp(lerp(0.92, 1.0, wA), 0.70, wC);
                    lane = lerp(lane, 0.92, rim);
                    float alpha = mask * lane * saturate(i.tone.z * 2.0) * fade;
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                float radius = length(q), angle = atan2(q.y, q.x);
                if (mode < 2.5)
                {
                    // A few broken accents inside the smoke, never a complete wire hoop.
                    float phase = seed * 17.0;
                    float rim = 0.87 + 0.018 * sin(angle * 5.0 + phase);
                    float width = 0.010 + 0.007 * saturate(0.5 + 0.5 * sin(angle * 11.0 - phase));
                    float stroke = 1.0 - smoothstep(width, width + 0.008, abs(radius - rim));
                    float arcs = smoothstep(-0.10, 0.40,
                        sin(angle * 3.0 + phase) + 0.35 * sin(angle * 7.0 - phase));
                    float a = stroke * arcs * (1.0 - k) * i.tone.z * 0.35;
                    return Energy(CelPal(row, 3) * a * i.tone.y, 0.0, i);
                }
                if (mode < 3.5)
                {
                    float worldRadius = radius * i.shape.x;
                    q *= i.shape.x / 3.7;
                    radius = length(q);
                    float a = atan2(-q.x, q.y);
                    if (a < 0.0) a += 6.283185;
                    float grown = lerp(0.30, 3.36, pow(saturate((t - 0.07) / 3.0), 0.88));
                    float arc = smoothstep(0.17, 0.36, a) * (1.0 - smoothstep(grown, grown + 0.16, a));
                    float rough = 0.025 * Zig(a * 5.0, seed * 81.0) + 0.016 * Zig(a * 12.0, seed * 43.0);
                    float ringAt = 0.75 + rough;
                    float distanceR = abs(radius - ringAt);
                    float bedWidth = i.shape.w < 0.5 && radius < ringAt ? 0.24 : 0.18;
                    float ring = 1.0 - CelAAStep(bedWidth + rough * 0.8 + i.shape.w * 0.025, distanceR);
                    float crackAngle = frac(a * 13.0 + 0.2 * Zig(radius * 17.0, seed)) - 0.5;
                    float cracks = (1.0 - CelAAStep(0.035, abs(crackAngle))) * step(ringAt, radius)
                        * (1.0 - smoothstep(0.92, 1.0, radius)) * (1.0 - smoothstep(3.2, 3.6, t));
                    float cover = max(ring, cracks) * arc;
                    float cooling = smoothstep(3.45, 6.45, t);
                    float heat = smoothstep(2.92, 3.18, t) * (1.0 - cooling);
                    float hot = (1.0 - CelAAStep(0.13 + rough * 0.3, distanceR)) * arc;
                    float noise = CelValue2(q * 8.0 + seed * 17.0) * 0.65 + CelValue2(q * 23.0 + seed) * 0.35;
                    float hotIslands = smoothstep(cooling * 0.98 - 0.06, cooling * 0.98 + 0.06, noise);
                    half3 black = half3(0.006, 0.007, 0.009); // v17: the clip's band is navy-grey (47,51,67), ours was near black
                    half3 fire = lerp(half3(1.0, 0.004, 0.001), half3(4.2, 0.7, 0.025), smoothstep(0.3, 0.85, heat));
                    float whiteCore = (1.0 - CelAAStep(0.04 + noise * 0.03, distanceR)) * smoothstep(0.75, 0.95, heat);
                    fire = lerp(fire, half3(12.0, 14.0, 16.0), whiteCore);
                    fire *= heat * hot * hotIslands;
                    float fissure = 1.0 - CelAAStep(0.008, abs(radius - ringAt + Zig(a * 9.0, seed) * 0.07));
                    fissure *= arc * smoothstep(0.1, 0.4, noise) * smoothstep(0.4, 0.7, cooling)
                        * (1.0 - smoothstep(5.9, 6.45, t));
                    float halo = exp(-distanceR * distanceR * 350.0) * arc * heat * 0.18;
                    float rim = (1.0 - hot) * cover;
                    half3 c = black * cover + half3(0.020, 0.022, 0.028) * rim + fire
                        + half3(0.8, 0.08, 0.001) * halo + half3(0.75, 0.003, 0.001) * fissure;
                    // Close/Mid: the cooling surface becomes black crust with a connected network
                    // of molten seams. The field is stationary; heat retreats through it over time.
                    // Far retains its original low-frequency representation below.
                    [branch] if (i.shape.w < 0.5)
                    {
                        // Earlier parts of the sweep cool first; the most recent front tip stays hot longer.
                        float surfaceAge = t + lerp(0.15, -0.10, saturate((a - 0.30) / 3.0));
                        float crossR = radius - ringAt;
                        float2 crustUV = float2(a * 8.5, crossR * 35.0);
                        crustUV += float2(Zig(crossR * 13.0, seed * 73.0),
                            Zig(a * 13.0, seed * 29.0)) * 0.32;
                        float2 crustCell = floor(crustUV), crustF = frac(crustUV);
                        float nearest = 100.0, second = 100.0;
                        float2 nearestCell = 0.0;
                        [unroll] for (int cy = -1; cy <= 1; cy++)
                        [unroll] for (int cx = -1; cx <= 1; cx++)
                        {
                            float2 cell = float2(cx, cy);
                            float2 id = crustCell + cell;
                            float2 jitter = float2(CelHash21(id + seed * 41.0),
                                CelHash21(id + 17.3 + seed * 67.0));
                            float2 delta = cell + 0.18 + jitter * 0.64 - crustF;
                            float d = dot(delta, delta);
                            if (d < nearest) { second = nearest; nearest = d; nearestCell = id; }
                            else second = min(second, d);
                        }
                        float cellHeat = CelHash21(nearestCell + seed * 113.0);
                        float seamDistance = sqrt(second) - sqrt(nearest);
                        float coolingFront = smoothstep(4.25, 6.48, surfaceAge);
                        float seamWidth = lerp(0.08, 0.020, coolingFront);
                        float seams = 1.0 - CelAAStep(seamWidth, seamDistance);
                        float crescent = sqrt(saturate(sin(saturate((a - 0.30) / max(grown - 0.30, 0.1)) * 3.14159265))); // z36
                        float moltenBand = 1.0 - CelAAStep(0.045 + 0.115 * crescent + rough * 0.3, abs(crossR)); // z36: 0.10 all along
                        // v07: the clip's hot core is a smooth line, not zig-zag fire teeth.
                        float spine = crossR + Zig(a * 14.0, seed * 19.0) * 0.010;
                        float coreWidth = lerp(0.020 + 0.070 * crescent, 0.010, smoothstep(4.05, 4.95, surfaceAge)); // z36: 0.060
                        float hotSpine = 1.0 - CelAAStep(coreWidth, abs(spine));
                        float broadHeat = 1.0 - smoothstep(4.35, 5.35, surfaceAge);
                        float litCells = 1.0 - smoothstep(0.60 + cellHeat * 0.35,
                            0.73 + cellHeat * 0.27, coolingFront);
                        float endHeat = 1.0 - smoothstep(5.35, 6.65, surfaceAge);
                        float hotStart = smoothstep(2.92, 3.18, t);
                        float patch = CelValue2(q * 23.0 + seed * 13.0) * 0.7 + CelValue2(q * 61.0) * 0.3;
                        float hotPatch = smoothstep(0.35 + coolingFront * 0.18, 0.52 + coolingFront * 0.18, patch);
                        float channel = 1.0 - CelAAStep(0.012 + broadHeat * 0.010,
                            abs(crossR + Zig(a * 9.0, seed * 39.0) * 0.060));
                        float islands = smoothstep(0.38, 0.62, CelValue2(float2(a * 6.0, crossR * 24.0) + seed));
                        // Keep the raised black tips, but inset the molten material from their caps.
                        float moltenArc = smoothstep(0.30, 0.46, a)
                            * (1.0 - smoothstep(grown - 0.14, grown - 0.02, a));
                        // v03: while hot the clip's C is one solid band; islands only open as it cools (f128-f140).
                        islands = lerp(1.0, islands, smoothstep(4.20, 4.80, surfaceAge));
                        float molten = max(broadHeat * islands * 0.7,
                            max(seams * litCells * hotPatch, channel * litCells * smoothstep(0.20, 0.45, CelValue2(float2(a * 15.0, seed * 19.0))) * 0.65)) * moltenBand * moltenArc;
                        float core = hotSpine * moltenBand * moltenArc * (1.0 - smoothstep(4.40, 5.25, surfaceAge));
                        float fireNoise = 0.78 + 0.22 * CelValue2(q * 58.0 + seed * 19.0);
                        half3 warm = lerp(half3(4.5, 0.85, 0.22), half3(0.85, 0.14, 0.07),
                            smoothstep(5.15, 6.10, surfaceAge));
                        half3 hotColour = lerp(warm, half3(6.0, 3.0, 0.8), core * broadHeat);
                        float raisedEdge = (1.0 - moltenBand) * cover;
                        float facet = CelHash21(nearestCell + 9.7);
                        half3 crust = lerp(half3(0.0015, 0.0020, 0.0035),
                            half3(0.018, 0.022, 0.028), (0.35 + facet * 0.65) * raisedEdge);
                        float rimCap = smoothstep(0.865, 0.88, radius)
                            * (1.0 - smoothstep(0.895, 0.91, radius)) * cover * step(0.55, facet);
                        crust = lerp(crust, half3(0.22, 0.24, 0.27) * (0.50 + facet * 0.50), rimCap);
                        float glowStrength = hotStart * endHeat;
                        float glow = exp(-crossR * crossR * 95.0) * moltenArc
                            * (broadHeat * 0.11 + seams * litCells * moltenBand * 0.035);
                        c = crust * cover + hotColour * molten * fireNoise * glowStrength
                            + half3(1.0, 0.16, 0.008) * glow * glowStrength;
                    }

                    float active = (1.0 - smoothstep(3.17, 3.28, t)) * i.shape.w;
                    float pool = exp(-radius * radius * 2.3) * saturate(1.0 - radius);
                    float shock = (1.0 - CelAAStep(0.045, abs(radius - 0.8))) * saturate(1.0 - t);
                    c += (half3(0.12, 0.75, 2.3) * pool + half3(1.4, 2.4, 3.2) * shock) * active;
                    cover *= 1.0 - active;
                    float shutdown = smoothstep(3.17, 3.32, t) * (1.0 - smoothstep(4.0, 4.65, t)) * i.shape.w;
                    float sweepR = lerp(4.8, 10.5, saturate((t - 3.17) / 1.3));
                    float sweepBand = 1.0 - CelAAStep(max(0.16, fwidth(worldRadius) * 1.4), abs(worldRadius - sweepR));
                    float broken = smoothstep(-0.45, -0.1, sin(angle * 4.0 + t));
                    float sweepAlpha = sweepBand * broken * shutdown * 0.75;
                    c = c * (1.0 - sweepAlpha) + half3(0.55, 0.65, 0.75) * sweepAlpha;
                    cover = max(cover, sweepAlpha);
                    return half4(c, cover);
                }
                // Uses the existing mode-4 billboard and existing Cel/Zig helpers only.
                // All silhouettes are mathematical; no image/flipbook/reference sampling.
                // Deliberately unequal cubic sheets, with ash occupying OPEN parts of them,
                // replace both the thin radial fountain and the disconnected round ash lobes.
                if (mode < 4.5)
                {
                    float impactAge = i.shape.y;
                    float clock = impactAge * 1.85 + seed * 11.0;
                    // v13 (2026-11-15, by eye): the clip's splash starts as a small crown of thin, nearly upright
                    // flame tongues (f18 ~120 x 190 px, f21 ~300 x 210 px) and erupts into the broad spray with its
                    // white ground swoosh at f24. The whole splash grows from 0.28x about the ground line (the card
                    // centre is 0.35 m up); after the eruption the tongues lean out and widen into soft sheets.
                    float growth = lerp(0.80, 1.0, smoothstep(0.08, 0.38, impactAge)); // z10: 0.50 (a ~90 px crown at f20; the clip's ~220) // v26: the clip's f18 flames tall (0.28)
                    q = float2(q.x, q.y + 0.063) / growth - float2(0.0, 0.063);
                    float erupt = smoothstep(0.30, 0.55, impactAge);
                    // v22 (owner on v21e: "look closer ... exactly like the reference"): the shaft lights its splash - by
                    // 40 px rings from the foot (_eye_v/radtier.py, f44-f88) the clip's is blown white 53/47/37/17/10 % at
                    // 0-200 px, then white -> pale -> mid toward the tips; v21e's 1-2 % blown, a pale cloud with a dark
                    // outline. Every splash layer takes its tier from this level plus its own shading.
                    // v22c: in the card's own units (q is magnified by 1/growth before the eruption: v22's early tongues
                    // took the mid/low tiers and filmed as wire loops)
                    float footR = length(float2(q.x * 263.0, (q.y + 0.063) * 342.0)) * growth / 100.0; // 100s of px from the foot
                    float footLight = lerp(0.95, -0.55, smoothstep(0.2, 2.2, footR)); // v24d: v24c blown 49-52 % at 80-160 px (clip 37/17)
                    footLight += 0.60 * (1.0 - erupt); // v22d: the arrival's tongues are solid blown white (clip f19-f22)
                    half3 painted = 0.0;
                    float coverage = 0.0;
                    // One low torn ash mass, not five repeated circular knobs.
                    // The white core stays open; existing sheet ash faces still supply rising gray.
                    float ashSpan = 0.60;
                    float ashFloor = 0.014 + 0.006 * sin(q.x * 13.0 + clock);
                    float ashHeight = 0.13
                        + (CelValue2(float2(q.x * 5.0 + seed * 13.0, clock * 0.33)) - 0.5) * 0.10
                        + (CelValue2(float2(q.x * 11.0 - seed * 7.0, clock * 0.47)) - 0.5) * 0.06;
                    float ashTaper = sqrt(saturate(1.0 - (q.x / ashSpan) * (q.x / ashSpan)));
                    float ashTop = ashFloor + ashHeight * ashTaper;
                    float notchA = 0.23 + CelHash11(seed * 43.0) * 0.16;
                    float notchB = -0.23 - CelHash11(seed * 59.0) * 0.16;
                    ashTop -= 0.055 * (1.0 - smoothstep(0.012, 0.055, abs(q.x - notchA)));
                    ashTop -= 0.045 * (1.0 - smoothstep(0.012, 0.050, abs(q.x - notchB)));
                    float ashField = min(ashTop - q.y, q.y - ashFloor);
                    float ashMask = CelAAStep(0.0, ashField)
                        * (1.0 - CelAAStep(ashSpan, abs(q.x))) * smoothstep(0.07, 0.19, abs(q.x));
                    float ashLight = smoothstep(0.025, 0.15, q.y + 0.020 * sin(q.x * 11.0 + seed));
                    half3 lowAsh = lerp(half3(0.23, 0.24, 0.25), half3(0.38, 0.39, 0.38), ashLight);
                    // v02: no low ash disc under the splash (the clip has none; v01 showed it as a dark lump).
                    ashMask = 0.0;
                    painted = MixFog(lowAsh, i.fog) * ashMask;
                    coverage = ashMask;
                    // v02b: the splash's body - a wide dense pale-cyan wall with a spiky torn crown. Its spikes are
                    // re-drawn about every two frames (the clip's crown changes every frame or two).
                    // v13: a low soft white-cyan mound without spikes (v02b's spiky crown read as a picket wall).
                    float wallSpan = 0.50 + 0.04 * sin(impactAge * 3.1 + seed);
                    float wallX = q.x / wallSpan;
                    float wallEnv = sqrt(saturate(1.0 - wallX * wallX));
                    float wallTop = (0.10 + 0.07 * CelValue2(float2(q.x * 4.0 + seed * 7.0, impactAge * 1.6))) * wallEnv;
                    float wallFloor = -0.02 + 0.01 * sin(q.x * 9.0);
                    float wallField = min(wallTop - q.y, q.y - wallFloor);
                    float wall = smoothstep(-0.035, 0.015, wallField) * (1.0 - CelAAStep(1.0, abs(wallX)))
                        * smoothstep(0.04, 0.16, impactAge) * 0.85;
                    float wallLight = saturate((q.y - wallFloor) / max(wallTop - wallFloor, 0.01));
                    half3 wallColour = lerp(half3(0.55, 0.95, 1.25), half3(1.10, 1.30, 1.40), wallLight);
                    // v21: the foam billows below carry the foot; this glowing wall was the flat mid tones (v20's
                    // mass 35 % mid vs the clip's 22 %).
                    wall = 0.0;
                    painted = painted * (1.0 - wall) + CelAdditiveFog(wallColour, i.positionWS, i.fog) * wall;
                    coverage = coverage + wall * (1.0 - coverage);

                    // v01 (2026-11-15, by eye): the clip's splash is a fan of long strokes flung up and OUT of
                    // the foot, leaning right, widest a third of the way out and ending in a sharp splinter,
                    // white with a cyan rim, some with a brown dirt face, soft where they move. The seven
                    // foam curtains and seven curl sheets read as a symmetric crown of ice crystals.
                    // Twelve strokes and eight thin splinters, each on its own throw cycle: flung out,
                    // stretched, then torn off the foot and thinned.
                    // v14 (owner 2026-11-15: "the reference is erratic - look at the different frames"): the clip's
                    // splash is a new drawing every frame or two - a wide low wave to the left (f80-f83), a ring all
                    // round (f84), a burst to the right (f61, f63) - impact-box frame diff 16-31 on the drawing frames,
                    // 9-18 between (ours 5-9). Every throw now gets new geometry (v01-v13 reused one seed per stroke,
                    // so each stroke redrew the same shape every cycle), and each drawing (12 a second, a third of them
                    // split in two) re-lays the strokes: side bias, flatness, reach, lean, bow and root.
                    // v15: +1 so a new drawing lands on the clip's even frames (its big jumps: f56, f58, f60, ...).
                    float frameN = floor(impactAge * 24.0 + 0.5) + 1.0;
                    float drawN = floor(frameN * 0.5);
                    drawN += step(0.40, CelHash11(drawN * 7.31 + seed * 3.0)) * fmod(frameN, 2.0) * 0.5; // v22d: 60 % (was 34 %)
                    float wild = erupt * lerp(0.60, 1.0, smoothstep(1.10, 1.70, impactAge));
                    float drawBias = (CelHash11(floor(drawN * 0.5) * 1.37 + seed * 5.0) - 0.5) * 1.2
                        + (CelHash11(drawN * 2.17 + seed * 6.0) - 0.5) * 0.8;
                    // v18: flatter - the clip's sheets curl low round the shaft (f80-f87), ours rose tall and straight.
                    float drawFlat = lerp(1.0, lerp(0.75, 1.15, CelHash11(drawN * 2.91 + seed * 7.0)), wild); // v22b: was 0.45-1.05
                    // v15: shorter - v14's splash filled the crop and covered the dummy (white 35-70k px vs 20-44k).
                    float drawReach = lerp(1.0, lerp(0.78, 1.12, CelHash11(drawN * 4.73 + seed * 9.0)), wild); // v22c: 0.64-1.00
                    // v21 (owner on v20: "the streaks also need to move around the beam"): the clip's sheets curl round
                    // the shaft (f63, f66, f67, f72, f78: a white arc wraps the foot, in front of the shaft low down and
                    // behind it higher up). Six helical arcs round the shaft's axis, swung clockwise seen from above like
                    // the ribbons, widening and rising as they go; the eye is ~18 deg up, so a ring of radius R is drawn
                    // R x 0.24R in this card. Their back halves are hidden where the shaft covers them and painted in
                    // the sheets' back-face tones; half of them are re-drawn in each drawing.
                    float backField = -10.0, frontField = -10.0, backU = 0.0, frontU = 0.0;
                    float arcOn = 0.0; // z21: no thin hoops (the helix ribbons surround the beam since z11) // smoothstep(0.32, 0.55, impactAge)
                    [unroll] for (int va = 0; va < 4; va++) // v23: 4 (6 in v22b, 9 in v22)
                    {
                        float vid = seed * 97.0 + va * 31.3;
                        float vphase = impactAge * (1.5 + CelHash11(vid) * 1.1) + CelHash11(vid + 1.0);
                        float vcyc = frac(vphase);
                        float vtid = vid + floor(vphase) * 41.3;
                        vtid += step(CelHash11(vtid + drawN * 2.71), 0.5 * wild) * (drawN * 13.1 + 3.0);
                        float R0 = lerp(0.20, 0.36, CelHash11(vtid + 2.0));
                        float R1 = R0 + lerp(0.12, 0.40, CelHash11(vtid + 3.0));
                        float y0 = lerp(0.00, 0.34, CelHash11(vtid + 4.0)); // v21d: above the mound too
                        float rise = lerp(0.04, 0.22, CelHash11(vtid + 5.0));
                        float aHead = CelHash11(vtid + 6.0) * 6.2831853 - vcyc * lerp(2.0, 3.4, CelHash11(vtid + 9.0));
                        float span = lerp(2.2, 3.8, CelHash11(vtid + 7.0)) * smoothstep(0.0, 0.30, vcyc);
                        float vw = lerp(0.022, 0.048, CelHash11(vtid + 8.0)) * smoothstep(0.0, 0.12, vcyc)
                            * (1.0 - smoothstep(0.72, 1.0, vcyc)) * arcOn;
                        float2 prevP = float2(R0 * cos(aHead + span), y0 + R0 * sin(aHead + span) * 0.24);
                        float prevFacing = -sin(aHead + span);
                        [unroll] for (int vs = 1; vs <= 8; vs++)
                        {
                            float u = vs / 8.0; // 0 tail .. 1 head; the tail is where the head was
                            float a = aHead + span * (1.0 - u);
                            float R = lerp(R0, R1, u);
                            float2 P = float2(R * cos(a), y0 + rise * u + R * sin(a) * 0.24);
                            float facing = -sin(a);
                            float2 e = P - prevP;
                            float al = saturate(dot(q - prevP, e) / max(dot(e, e), 1e-6));
                            float uu = (vs - 1.0 + al) / 8.0;
                            float body = vw * pow(saturate(uu + 0.02), 0.6) * (1.0 - smoothstep(0.85, 1.0, uu) * 0.6);
                            float cand = body - length(q - (prevP + e * al));
                            if (lerp(prevFacing, facing, al) < 0.0)
                            {
                                if (cand > backField) { backField = cand; backU = uu; }
                            }
                            else if (cand > frontField) { frontField = cand; frontU = uu; }
                            prevP = P;
                            prevFacing = facing;
                        }
                    }
                    float shaftQ = 0.20 + 0.10 * exp(-max(q.y + 0.063, 0.0) * 7.4);
                    float shaftCol = 1.0 - smoothstep(shaftQ - 0.025, shaftQ + 0.025, abs(q.x));
                    float arcAA = max(fwidth(max(backField, frontField)), 1e-5) * 0.75 + 0.008;
                    float backA = smoothstep(-arcAA, arcAA * 0.3, backField) * (1.0 - shaftCol);
                    half3 backC = lerp(half3(0.300, 0.400, 0.522), half3(0.433, 0.476, 0.559), smoothstep(0.3, 1.0, backU));
                    painted = painted * (1.0 - backA) + MixFog(backC, i.fog) * backA;
                    coverage = coverage + backA * (1.0 - coverage);

                    // v21 (owner on v20: "too flat where it meets the ground, it should have more volume"): the clip's
                    // foot is a mound of round foam billows on both sides of and in front of the shaft (f44, f52,
                    // f70-f78), lit cyan-white toward the shaft and on top, pale blue, lavender-grey and slate away from
                    // it and below (mass value bands dark 17 / shade 20 / mid 22 / lit 41 %, v20 8 / 14 / 35 / 43).
                    // Seven billows, re-laid every other drawing, in front of the sheets' roots.
                    float bGrow = smoothstep(0.30, 0.62, impactAge) * (1.0 - smoothstep(3.22, 3.40, impactAge));
                    float bMask = 0.0;
                    half3 bCol = 0.0;
                    [unroll] for (int bl = 0; bl < 7; bl++)
                    {
                        // v21d: half re-laid in every drawing, half every other (v21c's splash diff 13.6 vs 20.0).
                        float bid = seed * 71.0 + bl * 13.7 + floor(drawN * 2.0) * 5.31; // v22c: all re-laid each drawing (v21d: half)
                        float bSide = fmod(bl, 2.0) < 0.5 ? -1.0 : 1.0;
                        bool frontRow = bl >= 4;
                        float bx = bSide * lerp(frontRow ? 0.05 : 0.18, frontRow ? 0.40 : 0.58, CelHash11(bid));
                        bx = bl == 6 ? (CelHash11(bid) - 0.5) * 0.10 : bx; // v21b: one always on the foot
                        float br = (bl == 6 ? lerp(0.12, 0.16, CelHash11(bid + 1.0)) : lerp(0.075, 0.135, CelHash11(bid + 1.0))) // v23e: x0.75 (a cloud beside the sheets)
                            * (frontRow ? 1.0 : 0.85)
                            * (1.0 + (CelHash11(drawN * 1.9 + bl * 3.7 + seed) - 0.5) * 0.16 * wild) * bGrow;
                        float by = (frontRow ? -0.08 : 0.03) + br * 0.75
                            + lerp(0.0, 0.10, CelHash11(bid + 2.0)) * saturate(1.0 - abs(bx) / 0.6);
                        float2 d = (q - float2(bx, by)) / max(float2(br * 1.35, br), 1e-4);
                        float ang = atan2(d.y, d.x);
                        float lump = 1.0 + 0.10 * sin(ang * 5.0 + bid * 3.0) + 0.06 * sin(ang * 9.0 - bid * 5.0);
                        float rr = length(d) / lump;
                        float inside = (1.0 - CelAAStep(1.0, rr)) * step(frontRow ? -0.13 : -0.07, q.y);
                        float3 n = float3(d / lump, sqrt(saturate(1.0 - rr * rr)));
                        float lightB = dot(n, normalize(float3(-sign(bx) * 0.65, 0.70, 0.45)))
                            + (CelHash11(bid + 4.0) - 0.5) * 0.15;
                        // v22: no dark outline (v21d's rim -0.6 read as a cartoon cloud's): the rim falls a tier on the
                        // side away from the shaft and below, the lump's own light is a third of the shaft's, and the
                        // base sits a tier down on the ground line.
                        float rimB = smoothstep(0.72, 1.0, rr)
                            * saturate(0.5 - 0.5 * dot(d / max(length(d), 1e-4), normalize(float2(-sign(bx) + 1e-3, 0.8))));
                        float eB = footLight + 0.30 * lightB - 0.18 * rimB /* v23e: 0.38 */ - 0.40 * (1.0 - smoothstep(-0.06, 0.02, q.y));
                        half3 cB = SplashTier(eB);
                        bCol = lerp(bCol, cB, inside);
                        bMask = bMask + inside * (1.0 - bMask);
                    }
                    bMask = 0.0; // v24: no foam billows (round puffs: the clip has none, f56-f63)
                    painted = painted * (1.0 - bMask) + MixFog(bCol, i.fog) * bMask;
                    coverage = coverage + bMask * (1.0 - coverage);

                    // v22: the shaft's foot flares into a blown-white fountain (clip f44-f51, f76-f87: ~250 px across on
                    // the ground, ~90 px up; foot column white 0.75 vs v21e's 0.54), its edge torn anew each drawing.
                    // v22e: a new fountain each drawing - its height, width and lean jump (the clip's foot changes as much
                    // as the rest of its splash: frame diff 25.8 within 80 px of the foot, v22c 13.1)
                    // z02: the mound - one solid dome with a torn rim (clip f56-f84: ~475 x 170 px)
                    {
                        float mOn = smoothstep(0.30, 0.55, impactAge) * (1.0 - smoothstep(3.26, 3.42, impactAge)); // z27 (z25 3.20-3.38)
                        float mJ0 = CelHash11(drawN * 5.19 + seed * 3.7), mJ1 = CelHash11(drawN * 7.77 + seed * 1.9);
                        // z03: polar about the foot, in 100s of px (a card x unit is 263 px, a y unit 342)
                        float2 mP = float2((q.x + 0.08 - (mJ0 - 0.5) * 0.10) * 2.63, (q.y + 0.063) * 3.42); // z12: centred on the foot (z04 0.20)
                        float2 mR = float2(1.95 * lerp(0.88, 1.10, mJ0), mP.y > 0.0 ? 2.60 * lerp(0.80, 1.15, mJ1) : 0.45) * max(mOn, 1e-3) // z34: a crown (2.20 x 2.30)
                            * lerp(0.62, 1.0, smoothstep(0.50, 1.30, impactAge)); // z29: the clip's splash ~220 px at f30, ~340 at f44
                        float2 mE = mP / mR;
                        float mD = length(mE);
                        float mAng = atan2(mE.y, mE.x);
                        float mFront = smoothstep(-0.25, 0.10, mE.y / max(mD, 1e-3)); // 0 on the front (lower) half
                        float mUp = smoothstep(0.10, 0.55, mE.y / max(mD, 1e-3)); // z34: teeth and spikes point up, none sideways
                        float mRim = lerp(0.60, 0.72, mUp) + 0.52 * pow(saturate(Zig(mAng * 4.5 + drawN * 4.3, seed * 53.0)), 2.0) * mUp // z36: fewer, bigger (0.36, x7)
                            + (0.24 * Zig(mAng * 5.5 + drawN * 3.1, seed * 17.0)
                            + 0.07 * Zig(mAng * 15.0 + drawN * 5.3, seed * 29.0)
                            + 0.025 * Zig(mAng * 34.0 + drawN * 7.9, seed * 41.0)) * lerp(0.10, 1.0, mUp); // z36: half the fine teeth // z34: mFront // z22 // z21: finer teeth
                        float mound = (1.0 - CelAAStep(mRim, mD)) * step(0.02, mOn);
                        // z21: rays out of the foot - narrow by angle, long by radius
                        float mStripe = CelValue2(float2(mAng * 18.0 + drawN * 1.7, mD * 0.9 - drawN * 0.9 + seed));
                        float mRay = smoothstep(0.54, 0.72, mStripe); // z29: more of them // a thin shade ray now and then
                        half3 mCol = SplashTier(footLight + 0.80 - 0.95 * mD + (mStripe - 0.5) * 0.45 - mRay * 0.95); // z29: +0.75 blown white all through
                        painted = painted * (1.0 - mound) + MixFog(mCol, i.fog) * mound;
                        coverage = coverage + mound * (1.0 - coverage);
                    }
                    float fJ0 = CelHash11(drawN * 6.71 + seed * 2.3), fJ1 = CelHash11(drawN * 9.13 + seed * 4.1);
                    float fJ2 = CelHash11(drawN * 4.37 + seed * 8.9) - 0.5;
                    float fQx = q.x - fJ2 * 0.12 * wild * saturate((q.y + 0.063) / 0.27);
                    float fH = (q.y + 0.063) / (0.27 * lerp(1.0, lerp(0.65, 1.40, fJ0), wild));
                    float fTorn = 0.12 * (CelValue2(float2(q.y * 9.0, drawN * 3.7 + seed)) - 0.5) * wild; // v22c: was 0.06
                    float fHalf = (0.17 + 0.17 * pow(saturate(1.0 - fH), 1.8)) * smoothstep(0.30, 0.55, impactAge) // v22c: 0.34 at the ground (a trapezoid at 0.48)
                        * (1.0 - smoothstep(3.22, 3.40, impactAge)) * lerp(1.0, lerp(0.75, 1.30, fJ1), wild) * 0.70 + fTorn; // z25: v26e's // v24: 0.7x
                    // v22b: its base follows the ground's ellipse round the foot and is torn (v22's step(-0.075) filmed as
                    // a flat white floor, f76-f79)
                    float fBase = -0.063 - 0.055 * sqrt(saturate(1.0 - (q.x / 0.50) * (q.x / 0.50)))
                        + 0.035 * (CelValue2(float2(q.x * 11.0, drawN * 2.3 + seed * 5.0)) - 0.5);
                    float fount = (1.0 - CelAAStep(fHalf, abs(fQx))) * CelAAStep(fBase, q.y) * step(fH, 1.0);
                    half3 fCol = SplashTier(footLight + 0.60 - 0.25 * fH); // v24c: +0.35 filmed pale
                    // v22e: mid-tone shards inside it, re-drawn each drawing (the clip's 0-40 px ring is 18 % mid)
                    float fShard = 0.0;
                    [unroll] for (int fs = 0; fs < 4; fs++)
                    {
                        float fsid = drawN * 3.71 + fs * 17.13 + seed * 9.0;
                        float fsa = lerp(-1.1, 1.1, CelHash11(fsid));
                        float2 fsd = float2(sin(fsa), cos(fsa));
                        float2 fs0 = float2((CelHash11(fsid + 1.0) - 0.5) * 0.36, -0.06);
                        float fAlong = dot(q - fs0, fsd);
                        float fAcross = abs(dot(q - fs0, float2(fsd.y, -fsd.x)));
                        float fsl = lerp(0.10, 0.24, CelHash11(fsid + 2.0));
                        float fsw = lerp(0.008, 0.020, CelHash11(fsid + 3.0)) * saturate(1.0 - fAlong / fsl) * step(0.0, fAlong);
                        fShard = max(fShard, (1.0 - CelAAStep(fsw, fAcross)) * step(0.35, CelHash11(fsid + 4.0)));
                    }
                    fCol = lerp(fCol, SplashTier(footLight - 1.0), fShard * wild);
                    painted = painted * (1.0 - fount) + MixFog(fCol, i.fog) * fount;
                    coverage = coverage + fount * (1.0 - coverage);

                    // v22: the petals (v01's strokes) in front of the foam - v21-v21e drew the billows over them and the
                    // splash read as a round cloud.
                    // v24 (owner on v23e: "the fidelity and the shapes are not there"): the clip's splash is a few long
                    // crescent blades (250-350 px, 30-60 px thick) swept low and out of the foot, curling up at the end,
                    // striped along their length (white / pale blue / taupe) and torn into comb fingers at the tail.
                    [unroll] for (int s = 0; s < 24; s++) // v24e: 13 blades + 11 shards (the clip's spray is broken up)
                    {
                        bool splinter = s >= 13;
                        float id = seed * 151.0 + s * 23.17;
                        float h0 = CelHash11(id), h1 = CelHash11(id + 3.0);
                        float rate = splinter ? 2.4 + h1 * 1.4 : 1.3 + h1 * 0.9;
                        float cycle = frac(impactAge * rate + h0);
                        float tid = id + floor(impactAge * rate + h0) * 37.71;
                        tid += step(CelHash11(tid + drawN * 3.31), 0.85 * wild) * (drawN * 17.93 + 5.0);
                        float h2 = CelHash11(tid + 7.0), h3 = CelHash11(tid + 11.0);
                        float j0 = CelHash11(tid + drawN * 5.71) - 0.5, j1 = CelHash11(tid + drawN * 9.13) - 0.5;
                        float grow = smoothstep(0.0, 0.35, cycle);
                        float tear = smoothstep(0.55, 1.0, cycle);
                        float side = h2 < saturate(0.48 + 0.42 * drawBias * wild) ? 1.0 : -1.0; // z01: 0.62 (the clip's mound reaches 100 px further left)
                        // low throws (7-55 deg above the ground), a quarter upright beside the shaft
                        bool upright = CelHash11(tid + 41.0) < 0.18; // z34: 0.08 // v24d: upper band diff 20.6 (clip 13.8)
                        float lean = upright ? lerp(0.12, 0.45, h3) : lerp(0.55, side < 0.0 ? 1.70 : 1.50, h3); // z34: from 0.55 (0.95: all low) // z01: 0.80-1.50 (a tall fan at y280)
                        lean *= lerp(0.30, 1.0, erupt);
                        lean = clamp(lean + j0 * 0.35 * wild, 0.05, side < 0.0 ? 1.70 : 1.50);
                        float len = (splinter ? lerp(0.28, 0.66, h1) : lerp(0.50, 0.85, CelHash11(tid + 13.0))) // z02: 0.75-1.30 (a fan of feathers)
                            * drawReach * (1.0 + j1 * 0.30 * wild) * lerp(0.55, 1.0, erupt) * (side < 0.0 ? 1.25 : 0.95); // z01
                        float2 dir = float2(sin(lean) * side, cos(lean) * 0.62); // z01: 0.77 // a card y unit is 1.3x the px of an x unit
                        float2 nrm = normalize(float2(-dir.y * side, dir.x * side) + 1e-5); // up-and-in for a low throw
                        float2 root = float2((CelHash11(tid + 17.0) - 0.5) * lerp(0.85, 0.16, erupt), -0.03); // z10: 0.60
                        root += splinter ? dir * lerp(0.10, 0.65, CelHash11(tid + 43.0)) * grow : 0.0; // v24e: flung off the foot
                        float reach = len * grow;
                        float2 p3 = root + dir * reach;
                        float2 p0 = root + dir * reach * tear * 0.70;
                        float curl = lerp(0.05, 0.26, CelHash11(tid + 23.0)) * reach; // v24c: 0.08-0.40 a bouquet // v24b: -0.04-0.22 straight // the end curls up
                        // z11 (owner): the stream splashes back up, not straight - up out of the foot, over, and down outward
                        float arcUp = splinter ? 0.0 : reach * lerp(0.22, 0.40, CelHash11(tid + 47.0)) * erupt;
                        float2 p1 = lerp(p0, p3, 0.35) - nrm * curl * 0.25 + float2(0.0, arcUp);
                        float2 p2 = lerp(p0, p3, 0.72) + nrm * curl * 0.45 + float2(0.0, arcUp * 1.05);
                        p3 += nrm * curl * 1.6 - float2(0.0, arcUp * 0.35);
                        float wide = splinter ? lerp(0.018, 0.040, h3) : /* z34: the clip's spikes ~10 px at the root */ lerp(0.110, 0.190, CelHash11(tid + 19.0)); // z12: 0.085-0.150 // v24b: 0.050-0.095 icicles
                        wide *= lerp(0.70, 1.0, erupt) * (1.0 - tear * 0.30)
                            * smoothstep(0.0, 0.08, cycle) * (1.0 - smoothstep(0.90, 1.0, cycle));

                        float field = -10.0, winningU = 0.0, winningCross = 0.0;
                        float2 previous = p0;
                        [unroll] for (int segment = 0; segment < 5; segment++)
                        {
                            float u1 = (segment + 1.0) * 0.2;
                            float v1 = 1.0 - u1;
                            float2 next = p0 * (v1 * v1 * v1) + p1 * (3.0 * v1 * v1 * u1)
                                + p2 * (3.0 * v1 * u1 * u1) + p3 * (u1 * u1 * u1);
                            float2 edgeVector = next - previous;
                            float line2 = max(dot(edgeVector, edgeVector), 0.00001);
                            float along = saturate(dot(q - previous, edgeVector) / line2);
                            float u = (segment + along) * 0.2;
                            float2 delta = q - lerp(previous, next, along);
                            float signedAcross = (edgeVector.x * delta.y - edgeVector.y * delta.x) * rsqrt(line2);
                            // a crescent: thin at the foot, widest ~0.3 out, a long pointed tail
                            float body = wide * (0.30 + 0.70 * sqrt(saturate(u / 0.30))) * pow(saturate(1.0 - u), splinter ? 0.55 : 0.50); // v24b: 0.75 spiky
                            body *= splinter ? 1.0 : 0.80 + 0.40 * Zig(u * 9.0 + drawN * 1.3, id + (signedAcross < 0.0 ? 0.0 : 31.0)); // v24e: ragged edges
                            float candidate = body - length(delta);
                            if (candidate > field)
                            {
                                field = candidate;
                                winningU = u;
                                winningCross = signedAcross / max(body, 0.004);
                            }
                            previous = next;
                        }
                        float c = clamp(winningCross * side, -1.0, 1.0); // +1 the upper (inner) edge
                        // stripes along the blade: 3-5 bands across it, each its own tone, re-drawn each drawing
                        float stripeN = CelValue2(float2(c * 2.6 + h1 * 13.0, winningU * 1.2 + drawN * 0.37 + h0 * 5.0));
                        float stripeF = CelValue2(float2(c * 6.5 + h0 * 7.0, winningU * 2.0 - drawN * 0.53));
                        // the tail tears into comb fingers
                        float comb = smoothstep(0.50, 1.0, winningU) * (splinter ? 0.0 : 0.75);
                        float soft = splinter ? 0.0 : 0.012 * erupt;
                        float edgeAA = max(fwidth(field), 1e-5) * 0.75 + soft;
                        float solid = smoothstep(-edgeAA, edgeAA * 0.3, field) * CelAAStep(comb * 0.62, stripeF);
                        // a motion smear behind the trailing (lower) edge
                        float smear = splinter ? 0.0
                            : (1.0 - smoothstep(0.0, wide * 0.9 + 0.004, -field)) * 0.10 * step(winningCross * side, 0.0) * (1.0 - tear);
                        float mask = max(solid, smear);
                        float shaftAt = 0.20 + 0.10 * exp(-max(q.y + 0.063, 0.0) * 7.4);
                        mask *= 1.0 - step(CelHash11(tid + 31.0), 0.35)
                            * (1.0 - smoothstep(shaftAt - 0.025, shaftAt + 0.025, abs(q.x)));
                        // lit from the foot and on the upper edge; the stripes step it through the tiers
                        float strokeE = footLight + 0.67 - 0.30 * winningU + 0.18 * c // z34 (z30 0.58, z29 0.48, z28 0.70)
                            + (stripeN - 0.5) * 0.85 + (CelHash11(tid + 29.0) - 0.5) * 0.16;
                        half3 lit = SplashTier(strokeE);
                        half3 streak = lerp(half3(0.80, 1.25, 1.55), lit, erupt);
                        // taupe: one stripe of every third blade, the lower half of it
                        float dirt = (!splinter && fmod(s, 4.0) > 2.5) ? 1.0 : 0.0; // v24e: every fourth (tan 2.4x the clip's)
                        float dirtFace = dirt * CelAAStep(0.46, stripeN) * (1.0 - CelAAStep(0.30, c)) * (1.0 - CelAAStep(0.80, winningU));
                        half3 ash = lerp(half3(0.50, 0.488, 0.502), half3(0.549, 0.461, 0.435), CelAAStep(0.66, stripeN));
                        half3 surface = lerp(CelAdditiveFog(streak, i.positionWS, i.fog), MixFog(ash, i.fog), dirtFace * erupt);
                        painted = painted * (1.0 - mask) + surface * mask;
                        coverage = coverage + mask * (1.0 - coverage);
                    }

                    float frontA = smoothstep(-arcAA, arcAA * 0.3, frontField);
                    half3 frontC = SplashTier(footLight - 0.12 + 0.25 * smoothstep(0.05, 0.25, frontU)); // v22b: a tier below the petals
                    painted = painted * (1.0 - frontA) + MixFog(frontC, i.fog) * frontA;
                    coverage = coverage + frontA * (1.0 - coverage);

                    // v14: black specks fly in some drawings only (clip f58-f59, f66-f67), a new set each drawing.
                    float specks = 0.0;
                    float spLit = 0.0;
                    [unroll] for (int sp = 0; sp < 16; sp++) // v24e: 16, most small (the clip's 5-10 px specks with a few rocks)
                    {
                        float sid = drawN * 3.17 + sp * 11.3 + seed * 5.0;
                        float2 spAt = float2((CelHash11(sid) - 0.5) * 1.2, lerp(0.0, 0.60, CelHash11(sid + 1.0))); // v22e: inside the splash
                        float spR = lerp(0.012, 0.065, pow(CelHash11(sid + 2.0), 2.5)); // v24e: skewed small (v23 0.030-0.065)
                        float spOn = step(0.40, CelHash11(sid + 4.0));
                        // v22: angular blue-black chunks (the clip's shards are 6-15 px), not round black dots
                        float2 dq = (q - spAt) * float2(1.0, 1.4);
                        float spA = CelHash11(sid + 5.0) * 6.2831853;
                        float2 rq = float2(cos(spA) * dq.x - sin(spA) * dq.y, sin(spA) * dq.x + cos(spA) * dq.y);
                        // v23: a lumpy rock with a lit top-left rim (the clip's f68), not a hexagon
                        float rockA = atan2(rq.y, rq.x);
                        float chunk = length(rq * float2(1.0, 1.25))
                            / (1.0 + 0.22 * sin(rockA * 3.0 + sid) + 0.10 * sin(rockA * 7.0 - sid * 2.0));
                        float inS = (1.0 - CelAAStep(spR, chunk)) * spOn;
                        spLit = max(spLit, inS * CelAAStep(spR * 0.62, chunk) * step(0.0, dq.y - dq.x * 0.6));
                        specks = max(specks, inS);
                    }
                    specks *= wild;
                    painted = painted * (1.0 - specks)
                        + MixFog(lerp(half3(0.07, 0.085, 0.15), half3(0.16, 0.20, 0.32), spLit * wild), i.fog) * specks; // v23: lit rim
                    coverage = coverage + specks * (1.0 - coverage);

                    // Low irregular contact fill: a shallow white shelf, not a glowing ellipse.
                    float shelfSpan = 0.49 + 0.035 * sin(clock * 4.1);
                    float shelfY = 0.021 + 0.010 * Zig(q.x * 13.0, seed * 53.0)
                        + 0.008 * sin(q.x * 11.0 + clock * 4.7);
                    float shelfWidth = 0.022 * saturate(1.0 - pow(abs(q.x) / shelfSpan, 2.0));
                    float shelfField = min(shelfSpan - abs(q.x), shelfWidth - abs(q.y - shelfY));
                    float shelf = 0.0 * CelAAStep(0.0, shelfField); // v21: no white floor under the billows

                    // Open white lip: one low front arc plus a short unequal side curl.
                    // Neither path closes behind the beam, and both taper to zero at their ends.
                    float lipField = -10.0;
                    [unroll] for (int arc = 0; arc < 2; arc++)
                    {
                        float2 a0, a1, a2, a3;
                        if (arc == 0)
                        {
                            // v01: the lip is the bowl's front rim, about as wide as the splash.
                            // v21b: narrowed to the billows' base (v21: a flat ellipse round the mound)
                            a0 = float2(-0.60, 0.10);
                            a1 = float2(-0.64, -0.070);
                            a2 = float2(0.20, -0.090);
                            a3 = float2(0.56, 0.080);
                        }
                        else
                        {
                            a0 = float2(0.32, 0.055);
                            a1 = float2(0.60, 0.080);
                            a2 = float2(0.65, 0.240);
                            a3 = float2(0.50, 0.275);
                        }
                        float curl = sin(clock * 3.3 + arc * 2.7);
                        a0.y += curl * 0.018;
                        a2 += float2(0.030 * curl, 0.025 * sin(clock * 4.1 + arc));
                        a3.y += 0.038 * sin(clock * 2.9 + arc * 3.7);
                        // v14: the swoosh is re-drawn with the splash (clip f64, f68, f83: a new low wave each time).
                        a0 += float2(0.10, 0.05) * (CelHash11(drawN * 3.91 + arc * 5.3 + seed) - 0.5) * wild;
                        a2 += float2(-0.14, 0.07) * (CelHash11(drawN * 6.13 + arc * 2.9 + seed) - 0.5) * wild;
                        a3 += float2(0.10, 0.08) * (CelHash11(drawN * 8.37 + arc * 4.1 + seed) - 0.5) * wild;
                        float2 previous = a0;
                        [unroll] for (int segment = 0; segment < 8; segment++)
                        {
                            float u1 = (segment + 1.0) * 0.125;
                            float v1 = 1.0 - u1;
                            float2 next = a0 * (v1 * v1 * v1)
                                + a1 * (3.0 * v1 * v1 * u1)
                                + a2 * (3.0 * v1 * u1 * u1) + a3 * (u1 * u1 * u1);
                            float2 edgeVector = next - previous;
                            float along = saturate(dot(q - previous, edgeVector)
                                / max(dot(edgeVector, edgeVector), 0.00001));
                            float u = (segment + along) * 0.125;
                            float uneven = smoothstep(-0.70, 0.60,
                                Zig(u * 9.0 + clock * 0.10, seed * 29.0 + arc));
                            // v13: the clip's ground swoosh is broad and soft (f24-f52).
                            float width = (arc == 0 ? 0.040 : 0.024)
                                * pow(max(sin(u * 3.14159265), 0.0), 0.6)
                                * (0.38 + 0.82 * uneven);
                            // One open interruption per existing arc; no extra path or loop.
                            float breakAt = 0.16 + CelHash11(seed * 17.0 + arc * 11.0) * 0.16;
                            float breakProfile = 1.0 - smoothstep(0.010, 0.030, abs(u - breakAt));
                            width -= breakProfile * (arc == 0 ? 0.060 : 0.036);
                            float d = width - length(q - lerp(previous, next, along));
                            lipField = max(lipField, d);
                            previous = next;
                        }
                    }
                    // v13: the swoosh appears with the eruption (clip f23-f24), soft-edged and less HDR.
                    float lipAA = max(fwidth(lipField), 1e-5) * 0.75 + 0.012;
                    // v20: the clip's low swoosh shows in some drawings only (f64, f68, f83), not as a standing floor.
                    float lipOn = smoothstep(0.27, 0.36, impactAge)
                        * lerp(1.0, step(0.50, CelHash11(drawN * 5.37 + seed * 2.0)), wild); // v21e: ~half (narrowed in v21b)
                    float front = max(shelf, smoothstep(-lipAA, lipAA * 0.3, lipField) * lipOn) * 0.0; // v24: no lip band
                    half3 frontColour = CelAdditiveFog(SplashTier(footLight + 0.20), i.positionWS, i.fog); // v22: blown at the foot
                    painted = painted * (1.0 - front) + frontColour * front;
                    coverage = coverage + front * (1.0 - coverage);

                    // Hold the splash as the shaft narrows, then clear before the terminal star.
                    float fade = smoothstep(0.05, 0.12, impactAge)
                        * (1.0 - smoothstep(3.31, 3.45, impactAge)) * i.tone.z; // z25: v26e's
                    float glow = exp(-dot(q * float2(1.0, 1.7), q * float2(1.0, 1.7)) * 7.0);
                    painted += CelAdditiveFog(half3(0.025, 0.10, 0.19) * glow,
                        i.positionWS, i.fog) * (1.0 - coverage);
                    return half4(painted * fade, coverage * fade);
                }
                if (mode < 5.5)
                {
                    // Expand support for long rim projections without enlarging the crater or blue pool.
                    q *= i.shape.x / 6.7;
                    radius = length(q);
                    angle = atan2(q.y, q.x);
                    float fade = smoothstep(0.0, 0.035, k) * (1.0 - smoothstep(3.40, 3.51, i.shape.y));
                    float grain = 0.72 + 0.28 * CelValue2(q * 35.0 + CelValue2(q * 8.0));
                    float f = exp(-radius * radius * 1.8) * (1.0 - smoothstep(0.65, 1.0, radius));
                    // v16: 0.6x - the clip's rim band is smooth (ours had jagged tentacles at its left end).
                    // v17: re-drawn every frame or two while the beam is on - the clip's crater-front band is mostly
                    // the splash's moving shadow (ground-band frame diff 23.5 vs ours 6.2).
                    float rimTick = floor(t * 12.0 + 0.25) * step(0.85, t) * (1.0 - step(3.30, t));
                    float jagged = Zig(angle * 7.0, seed * 31.0 + rimTick * 1.7) * 0.022
                        + Zig(angle * 21.0, seed * 17.0 + rimTick * 2.3) * 0.011;
                    float rimAt = 0.52 + jagged;
                    float nearSide = 1.0 - smoothstep(0.04, 0.22, q.y);
                    // v18: narrower (the clip's crater dark is 13.5k px at f40, ours 25k)
                    float rimWidth = lerp(0.024, 0.062, smoothstep(0.40, 0.55, CelValue2(q * 14.0 + seed + rimTick * 3.1)));
                    float rim = (1.0 - CelAAStep(rimWidth + jagged * 0.3, abs(radius - rimAt))) * nearSide;
                    // v16: the clip's rim band has a pale lit lip along its inner (upper) edge (f22, f40).
                    float edge = (1.0 - CelAAStep(0.009, abs(radius - rimAt + rimWidth + 0.006))) * nearSide
                        * smoothstep(0.25, 0.45, t); // v22c: the clip's lit lip shows from ~f22, not at the arrival (f17-f20)
                    float fissure = 0.0;
                    [unroll] for (int crack = 0; crack < 9; crack++)
                    {
                        float ca = crack < 3 ? -2.72 + crack * 0.16 : lerp(-2.25, -0.15, (crack - 3.0) / 5.0);
                        ca += (CelHash11(crack + seed * 43.0) - 0.5) * 0.16;
                        float pulse = 0.5 + 0.5 * sin((t - 2.0) * 6.2831853 + 0.30
                            + (CelHash11(crack * 7.0 + seed) - 0.5) * 0.80);
                        float activation = smoothstep(1.65, 2.05, t) * lerp(0.22, 1.0, pulse);
                        activation *= crack < 3 ? 1.0 - smoothstep(2.78, 3.12, t)
                            : lerp(0.22, 1.0, smoothstep(2.80, 3.10, t));
                        float targetSpread = crack < 3 ? 0.98 + CelHash11(crack * 11.0 + seed) * 0.14
                            : 0.88 + CelHash11(crack * 11.0 + seed) * 0.20;
                        float spread = lerp(0.58, targetSpread, activation);
                        float cr = saturate((radius - 0.50) / (spread - 0.50));
                        float bend = (CelHash11(crack + seed * 29.0) - 0.5) * 0.45 * cr;
                        bend += Zig(cr * 4.0, crack + seed * 13.0) * 0.11 * sin(cr * 3.14159265);
                        float da = abs(atan2(sin(angle - ca - bend), cos(angle - ca - bend)));
                        // v14: the clip's radial cracks are thin lines (~10 px at f70), not 30-40 px tentacles.
                        float width = (0.018 + CelHash11(crack + seed * 31.0) * 0.035)
                            * activation * pow(1.0 - cr, 0.80)
                            * (0.65 + 0.35 * CelValue2(float2(cr * 7.0, crack + seed * 19.0)));
                        fissure = max(fissure, (1.0 - CelAAStep(width, da)) * step(0.50, radius) * step(radius, spread));
                    }
                    float opaque = max(rim, fissure * nearSide) * smoothstep(0.12, 0.24, t); // z09: the clip's band is there at f20 // v22c: was 0.06-0.22 (full by f20)
                    // v14c (owner: "the reference is erratic"): the beam light throws the flying sheets' shadows toward
                    // the camera - dark streaks and fans in front of the crater, a new set every frame or two (clip
                    // f40-f92: 2.4-11k px, ground-box frame diff 6-17; ours 1-2). They start at the moving impact
                    // (the beam's Sweep in this card's right/forward units) and fall outward, mostly toward the camera.
                    float sweepK = saturate((i.shape.y - 0.2) / 3.16);
                    float sweepA = lerp(0.45, 3.22, pow(sweepK, 0.88));
                    sweepA = lerp(sweepA, lerp(3.18, 3.06, smoothstep(3.20, 3.36, i.shape.y)),
                        smoothstep(2.92, 3.17, i.shape.y) * lerp(0.55, 0.75, smoothstep(3.30, 3.50, i.shape.y)));
                    float2 fromHit = q - float2(0.45 - sin(sweepA) * 3.0, cos(sweepA) * 2.24 / 1.05) / 6.7;
                    float hitR = length(fromHit), hitA = atan2(fromHit.y, fromHit.x);
                    float shadowTick = floor(t * 12.0 + 0.25);
                    float shadow = 0.0;
                    [unroll] for (int ss = 0; ss < 5; ss++) // v17: 5 broad wedges (were 4 thin streaks)
                    {
                        float hid = shadowTick * 4.37 + ss * 13.1 + seed * 7.0;
                        float sa = lerp(-2.75, -0.40, CelHash11(hid));
                        float r0 = lerp(0.15, 0.35, CelHash11(hid + 1.0));
                        float r1 = r0 + lerp(0.12, 0.28, CelHash11(hid + 2.0)); // z04: 0.25-0.55 (leaves to the frame's bottom)
                        float halfW = lerp(0.060, 0.130, CelHash11(hid + 3.0));
                        float bendS = (CelHash11(hid + 4.0) - 0.5) * 0.9;
                        float onS = step(0.30, CelHash11(hid + 5.0));
                        float along = saturate((hitR - r0) / max(r1 - r0, 0.01));
                        float across = abs(atan2(sin(hitA - sa - bendS * along), cos(hitA - sa - bendS * along))) * hitR;
                        float w = halfW * sin(along * 3.14159265) * (0.7 + 0.3 * sin(along * 9.0 + hid));
                        shadow = max(shadow, (1.0 - smoothstep(w * 0.55, w + 0.004, across))
                            * step(r0, hitR) * step(hitR, r1) * onS);
                    }
                    opaque = max(opaque, shadow * 0.85 * smoothstep(0.85, 1.05, t) * (1.0 - smoothstep(3.10, 3.30, t)));
                    // v13: no neon line in the crack - the clip's crater edge is a dark navy band.
                    // z38: the lit ground - stepped light with a warped edge, swirl lines, a ragged hot patch under the
                    // moving foot with a pale ring, and rays along the ground; the patch and rays re-drawn each drawing
                    float beamOn = smoothstep(0.30, 0.50, t) * (1.0 - smoothstep(3.25, 3.40, t));
                    float warp = CelValue2(q * 9.0 + seed * 3.0) - 0.5;
                    float lv = f * (1.0 + 0.9 * warp);
                    float tiers = 0.30 * CelAAStep(0.10, lv) + 0.30 * CelAAStep(0.26, lv) + 0.40 * CelAAStep(0.48, lv);
                    float swirlN = CelValue2(q * 21.0 + 3.0 * CelValue2(q * 6.0 + seed));
                    float swirl = smoothstep(0.86, 0.96, 1.0 - abs(2.0 * swirlN - 1.0)); // z39: 0.80-0.94
                    float footEdge = 0.115 * (1.0 + 0.32 * Zig(hitA * 1.6 + shadowTick * 2.3, seed * 5.0) // z39: 0.17
                        + 0.14 * Zig(hitA * 4.8 + shadowTick * 4.1, seed * 9.0));
                    float hitE = length(fromHit * float2(1.0, 1.35));
                    float footHot = (1.0 - CelAAStep(footEdge, hitE)) * beamOn;
                    float footPale = (1.0 - CelAAStep(footEdge * 1.75 + 0.03 * warp, hitE)) * beamOn;
                    float rayN = CelValue2(float2(hitA * 4.2 + shadowTick * 1.3, seed * 7.0 + shadowTick));
                    float gRay = smoothstep(0.60, 0.74, rayN) * (1.0 - smoothstep(0.20, 0.62, hitR)) * step(footEdge, hitE) * beamOn;
                    half3 lit = half3(0.035, 0.14, 0.32) * lerp(f, tiers, 0.65) * grain
                        + half3(0.05, 0.15, 0.24) * swirl * saturate(f * 2.2)
                        + half3(0.10, 0.26, 0.42) * gRay
                        + half3(0.09, 0.25, 0.40) * (footPale - footHot) // z39: 0.16,0.40,0.58
                        + half3(0.75, 0.98, 1.30) * footHot;
                    half3 c = lit * (1.0 - opaque)
                        + half3(0.35, 0.30, 0.10) * edge * (1.0 - opaque)
                        + half3(0.040, 0.046, 0.075) * opaque; // v18: lighter navy again (clip 47,51,67; v17 30,38,66)
                    return half4(CelAdditiveFog(c, i.positionWS, i.fog) * fade, opaque * fade);
                }
                if (mode < 6.5)
                {
                    // v13: the clip's torus is a swirl of several soft smoke strands (f25-f30, f99-f102) with lighter
                    // streaks sliding along them; each strand breaks along the ring and the breaks widen as the lobes
                    // take over. Tone is symmetric across a strand (the strip's side flips round the ring).
                    float a = q.x;
                    float phase = seed * 17.0;
                    float spine = 0.10 * sin(a * 3.0 + phase) + 0.05 * sin(a * 7.0 - phase);
                    float crossBand = q.y - spine;
                    float open = smoothstep(0.22, 0.55, k);
                    // z35: the roll's body - solid, its edge scalloped into lumps, breaking up late
                    float bu1 = a * 2.5465, bu2 = a * 1.4324 + 0.37; // z37: 16 + 9 round (22 + 13)
                    float bf1 = 2.0 * frac(bu1) - 1.0, bf2 = 2.0 * frac(bu2) - 1.0;
                    float lump = max(sqrt(saturate(1.0 - bf1 * bf1)) * lerp(0.45, 1.0, CelHash11(floor(bu1) * 3.1 + seed * 7.0)),
                        sqrt(saturate(1.0 - bf2 * bf2)) * lerp(0.50, 1.0, CelHash11(floor(bu2) * 5.3 + seed * 11.0)));
                    float halfBody = (0.13 + 0.80 * lump * smoothstep(0.02, 0.16, k)) * (1.0 - 0.45 * open); // z37 // z36: deeper lumps (0.26 + 0.52)
                    float bodyD = abs(crossBand) / max(halfBody, 1e-3);
                    float bodyGap = smoothstep(lerp(-1.40, -0.30, open), lerp(-1.00, 0.30, open),
                        sin(a * 3.0 + phase * 2.0) + 0.5 * sin(a * 7.0 - phase));
                    float bodyA = (1.0 - CelAAStep(1.0, bodyD)) * bodyGap * 0.96;
                    half3 bodyC = lerp(CelPal(row, 1), CelPal(row, 2), 1.0 - CelAAStep(0.42, bodyD)); // z36: 0.58, x1.12
                    float alpha = bodyA;
                    half3 col = bodyC * bodyA;
                    [unroll] for (int j = 0; j < 4; j++)
                    {
                        float hj = CelHash11(seed * 37.0 + j * 5.1);
                        // v14: the strands are re-drawn every frame or two (the clip's torus swirl jumps, f24-f30).
                        float dj = CelHash11(floor(t * 12.0 + 0.25) * 1.71 + seed * 3.0 + j * 7.3) - 0.5;
                        float at = lerp(-0.30, 0.30, (j + 0.5) / 4.0) // z34: z28's // v26d: packed into one roll (+-0.45) + 0.10 * sin(a * (2.0 + j) + phase + j * 1.9)
                            + dj * 0.16;
                        // v15: fatter, denser strands - the clip's torus is a thick soft roll (f24-f28, f99-f100).
                        float hw = lerp(0.20, 0.30, hj) * (0.80 + 0.20 * sin(a * 5.0 + j * 2.3 + phase)); // z34: z28's
                        float d = abs(crossBand - at);
                        float band = 1.0 - smoothstep(hw * 0.25, hw + 0.22, d); // z34: z28's // v16: soft, motion-blurred
                        float run = sin(a * (2.0 + fmod(j, 2.0)) + phase * (1.0 + j) - t * (1.8 + 0.6 * j))
                            + 0.45 * sin(a * 5.0 - j * 1.3 + phase) + dj * 1.8;
                        float gap = smoothstep(lerp(-1.10, -0.35, open), lerp(-0.60, 0.25, open), run); // z34: z28's
                        float aj = band * gap * lerp(0.82, 0.96, hj) * 0.55; // z37: over the body // z34: z28's // v17: v16's band read translucent
                        float strandTone = saturate(1.0 - d / max(hw + 0.08, 0.01)) * 0.55 + hj * 0.25;
                        half3 cj = lerp(CelPal(row, 1), CelPal(row, 2), strandTone);
                        cj *= lerp(1.45, 0.80, smoothstep(0.0, 0.62, abs(crossBand))); // z34: z28's // v26d: one rolled tube across the band (v26c per strand = stripes)
                        float slide = smoothstep(0.2, 0.8, sin(a * (3.0 + j) - t * (2.6 + j * 0.7) + phase + j * 1.7));
                        cj = lerp(cj, CelPal(row, 3), (1.0 - smoothstep(0.02, 0.07, d)) * slide * 0.45);
                        col = col * (1.0 - aj) + cj * aj;
                        alpha = alpha * (1.0 - aj) + aj;
                    }
                    col /= max(alpha, 0.001);
                    col *= half3(0.80, 0.78, 0.80) * (i.shape.z > 0.5 ? 0.95 : 0.98); // z36 (z35 1.0 / 1.18 a white tube) // z35: cream-lit (z28 0.85 / 1.0) // z32 (z30's 0.86,0.82,0.78 filmed tan) // v26e: ring 2 0.85x (f99 crest 481 vs the clip's 395); v19: darker, less blue (clip torus median 128,131,143)
                    float attack = i.shape.z > 0.5 ? smoothstep(0.0, 0.012, t) : smoothstep(0.0, 0.08, k);
                    // v10: ring 2's swirl is gone by ~f105 in the clip; ring 1's arcs stay to ~f36.
                    // v16: ring 2's ribbons go by ~f106 so its puffs carry the ring (they were 0.6x the clip's late).
                    float fade = attack * (1.0 - (i.shape.z > 0.5 ? smoothstep(0.18, 0.36, k) : smoothstep(0.48, 0.72, k)));
                    alpha *= 0.94 * i.tone.z * fade;
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                if (mode > 11.5)
                {
                    // v22 lab chart: 7 x 7 HDR swatches written as they are (no fog, no tone); R 0.45 x 1.26^col,
                    // G 0.75 x 1.27^row, B 0.9 x 1.31^block with a new block every 4 frames, block 7 a neutral ramp 0..4.
                    float2 g = float2(q.x * 0.5 + 0.5, 0.5 - q.y * 0.5);
                    float2 ci = min(floor(g * 7.0), 6.0);
                    float2 cf = frac(g * 7.0);
                    float gutter = step(0.10, cf.x) * step(cf.x, 0.90) * step(0.10, cf.y) * step(cf.y, 0.90);
                    float bi = min(floor(t * 6.0), 7.0);
                    float3 c = float3(0.45 * pow(1.26, ci.x), 0.75 * pow(1.27, ci.y), 0.9 * pow(1.31, bi));
                    if (bi > 6.5) c = (ci.x + ci.y * 7.0) / 48.0 * 4.0;
                    return half4(c * gutter, 1.0);
                }
                if (mode > 10.5)
                {
                    // v21: a short salmon dash with a near-white core (clip cores ~255,250,238, median 222,198,196);
                    // q = (across, 0 tail .. 1 head). Additive.
                    // v22: a capsule dash with a small core (clip blobs p50 8.2 x 3.6 px; v21e's triangle with a big
                    // core filmed as a white arrowhead).
                    float endT = saturate(q.y / 0.30), endH = saturate((1.0 - q.y) / 0.22);
                    float halfW = sqrt(saturate(endT * (2.0 - endT))) * sqrt(saturate(endH * (2.0 - endH)));
                    float ax = abs(q.x) * 1.5; // v25d: 2/3 of the strip (the clip's warm area 3.5x ours); v22d: the dash in the strip's inner half, a soft orange glow in the outer
                    float field = saturate(1.0 - ax / max(halfW, 1e-3)) * lerp(0.18, 1.0, q.y) * step(0.001, halfW); // z29: a comet (0.55 a capsule)
                    float mask = CelAAStep(0.10, field);
                    float hot = CelAAStep(0.50, field); // z29 // v25e: 0.50 left the dash pale peach
                    float glowS = pow(saturate(1.0 - abs(q.x)), 2.0) * halfW * lerp(0.35, 1.0, q.y) * (1.0 - mask);
                    float fade = smoothstep(0.0, 0.06, k) * (1.0 - smoothstep(0.70, 1.0, k));
                    // v21c: written through the inverse grade over the dark ground (shows 228,196,186 / 246,230,216);
                    // v21b's HDR 2.2,1.9,1.6 filmed dull cream 222,221,200 and only its rim read warm.
                    // v22: the hdr=1 chart over the ground (0.218,0.245,0.320): body shows 195,164,138, core 241,222,197
                    // (the clip's 249,220,197) - v21e's 0.61,0.565,0.629 core filmed white.
                    half3 c = (lerp(half3(0.62, 0.27, 0.03), half3(1.25, 0.95, 0.48), hot) * mask // z29: orange with a white-hot head (filmed salmon 190,120,120) // v25d: the clip's orange 200,125,105 / 236,191,145 // v22d: core pinker (v22c 254,241,214)
                        + half3(0.24, 0.10, 0.015) * glowS) * i.tone.y;
                    return Energy(c * i.tone.z * fade, 0.0, i);
                }
                if (mode > 9.5)
                {
                    // v03: the clip's last contact is a small burst of jagged spikes leaning up and right,
                    // unequal, thin and tapering, blue with a white core - not a symmetric ray star.
                    float r = length(q);
                    float mask = 0.0, inner = 0.0;
                    [unroll] for (int j = 0; j < 9; j++)
                    {
                        float id = seed * 29.0 + j * 7.31;
                        float ang = lerp(-2.75, 3.53, (j + CelHash11(id)) / 9.0); // z29: all round (clip f16, f100) // v20: fans up
                        float len = lerp(0.40, 1.0, CelHash11(id + 1.7)) * (j == 3 ? 1.0 : 0.85);
                        float2 d = float2(cos(ang), sin(ang));
                        float along = dot(q, d);
                        float across = abs(q.x * d.y - q.y * d.x);
                        float u = saturate(along / len);
                        float w = lerp(0.055, 0.0, pow(u, 0.8)) * (0.7 + 0.6 * CelHash11(id + 3.1));
                        float inside = step(0.0, along) * step(along, len);
                        mask = max(mask, (1.0 - CelAAStep(w, across)) * inside);
                        inner = max(inner, (1.0 - CelAAStep(w * 0.45, across)) * inside);
                    }
                    float core = 1.0 - smoothstep(0.04, 0.16, r);
                    mask = max(mask, core);
                    half3 c = lerp(half3(0.35, 1.0, 3.2), half3(1.6, 2.6, 4.0), max(core, inner * 0.30)); // v20: bluer, white only at the root
                    return Energy(c * mask, mask, i);
                }
                if (mode > 8.5)
                {
                    // v22: the clip's needles - straight white spikes flung out of the splash, pointed at the outer
                    // end (f46, f50, f84-f87: 3-8 px wide, 60-200 px long); v21's wavy cyan HDR lines filmed as electric
                    // crackles in the middle of the mound.
                    float width = 0.32 * pow(saturate((1.0 - q.y) * 0.5), 0.75) * smoothstep(-1.0, -0.80, q.y); // v22b: 0.25
                    float mask = (1.0 - CelAAStep(width, abs(q.x))) * (1.0 - smoothstep(0.7, 1.0, k));
                    float core = 1.0 - CelAAStep(width * 0.40, abs(q.x));
                    half3 c = lerp(half3(0.567, 0.897, 1.261), half3(0.674, 0.897, 2.477), core); // white / blown (hdr=1 chart)
                    return Energy(c * mask, mask, i);
                }
                if (mode > 7.5)
                {
                    q *= 1.10;
                    // First shock: a low rolling billow grows into torn irregular puffs.
                    // The second ring retains its authored silhouette/lifecycle exactly.
                    bool firstRing = i.shape.z < 0.5;
                    float firstTear = smoothstep(0.45, 0.95, t);
                    if (firstRing)
                    {
                        float billowHeight = lerp(0.78, 0.92, smoothstep(0.08, 0.55, t));
                        q.x /= 1.08;
                        q.y = (q.y + 0.35) / billowHeight - 0.35;
                    }
                    float distanceField = 10.0;
                    float frontDepth = -1000.0;
                    float3 puffNormal = float3(0.0, 0.0, 1.0);
                    float breakup = firstRing ? smoothstep(0.55, 1.05, t) : smoothstep(0.58, 1.05, t); // z07 // z06: ring 2 holds to f112
                    float separation = smoothstep(0.30, 0.75, t);
                    float clearance = firstRing ? smoothstep(1.00, 1.35, t) : smoothstep(0.98, 1.20, t); // z07: 0.90-1.18
                    float expiry = smoothstep(0.90, 1.0, k);
                    // v15: was 0.55 - both rings' smoke thinned out too early (f38 37k px vs 93k, f112 65k vs 126k).
                    float fragmentScale = lerp(1.0, firstRing ? 0.62 : 0.45, breakup) * (1.0 - max(clearance, expiry));
                    [unroll] for (int j = 0; j < 6; j++)
                    {
                        float id = seed * 127.0 + j * 17.0;
                        float2 centre = float2(CelHash11(id), CelHash11(id + 5.0)) * (firstRing ? float2(0.95, 0.65) : float2(0.72, 0.95))
                            - (firstRing ? float2(0.475, 0.12) : float2(0.36, 0.22)); // z08: ring 2 tall cauliflowers
                        centre += (float2(CelHash11(id + 13.0), CelHash11(id + 21.0)) - 0.5) * separation * 0.18;
                        float r = (firstRing ? 0.28 + CelHash11(id + 9.0) * 0.28 : 0.22 + CelHash11(id + 9.0) * 0.24) * fragmentScale;
                        if (firstRing)
                        {
                            centre = float2(0.0, 0.18) + (centre - float2(0.0, 0.18))
                                * (1.0 + firstTear * 0.30);
                            r *= lerp(0.96, 0.87, smoothstep(0.20, 0.55, t));
                        }
                        float2 local = q - centre;
                        if (firstRing)
                        {
                            // Low-frequency rounded bites keep each fragment unequal; no holes
                            // or high-frequency noise, and no late replacement with identical hooks.
                            float a = atan2(local.y, local.x);
                            float roll = id + t * (0.45 + CelHash11(id + 37.0) * 0.60);
                            float contour = sin(a * 3.0 + roll) * 0.11
                                + sin(a * 5.0 - roll * 1.31) * 0.07
                                + sin(a * 8.0 + id * 1.73) * 0.035;
                            r *= 1.0 + contour * lerp(0.70, 1.35, firstTear);
                        }
                        float d = length(local) - r;
                        float unionWidth = firstRing ? lerp(0.055, 0.020, firstTear) : 0.10;
                        float blend = saturate(0.5 + 0.5 * (d - distanceField) / unionWidth);
                        distanceField = lerp(d, distanceField, blend) - unionWidth * blend * (1.0 - blend);
                        float frontZ2 = r * r - dot(local, local);
                        if (frontZ2 > 0.0)
                        {
                            float frontZ = sqrt(frontZ2);
                            float depth = frontZ + (CelHash11(id + 31.0) - 0.5) * r * 0.18;
                            float smoothFront = max(r * 0.42, 0.001);
                            float join = saturate(0.5 + 0.5 * (depth - frontDepth) / smoothFront);
                            float3 lobeNormal = float3(local, frontZ) / max(r, 0.0001);
                            puffNormal = normalize(lerp(puffNormal, lobeNormal, join));
                            frontDepth = lerp(frontDepth, depth, join) + smoothFront * join * (1.0 - join);
                        }
                    }
                    float tLate = t - 0.12; // z08: the hand-over to wisps 0.12 s later (the cliff at f116)
                    [branch] if (tLate > 0.65 && !firstRing)
                    {
                        // Positive rounded masses join at a neck, then separate along an open spine.
                        float handover = smoothstep(0.65, 0.86, tLate);
                        float neck = smoothstep(0.78, 1.08, tLate);
                        float lateScale = lerp(1.0, 0.46, smoothstep(0.92, 1.155, tLate))
                            * (1.0 - smoothstep(1.155, 1.18, tLate)) * sqrt(1.0 - smoothstep(0.94, 1.0, k));
                        float curveDistance = 10.0;
                        float curveFront = -1000.0;
                        float3 curveNormal = puffNormal;
                        [branch] if (lateScale > 0.0001)
                        {
                            float curveSeed = seed * 191.0;
                            float curveAngle = CelHash11(curveSeed + 1.0) * 6.283185307;
                            float2 axis = float2(cos(curveAngle), sin(curveAngle));
                            float2 offset = q - float2(0.0, 0.205);
                            float2 curvePoint = float2(dot(offset, axis), dot(offset, float2(-axis.y, axis.x))) / lateScale;
                            float halfSpan = lerp(0.36, 0.48, CelHash11(curveSeed + 3.0));
                            float bend = lerp(-0.34, 0.34, CelHash11(curveSeed + 5.0));
                            float roll = lerp(-0.30, 0.30, CelHash11(curveSeed + 7.0));
                            float tilt = lerp(-0.16, 0.16, CelHash11(curveSeed + 9.0));
                            float split = lerp(-0.20, 0.25, CelHash11(curveSeed + 11.0)) * halfSpan;
                            float gap = 0.14 * neck;
                            float leftMass = lerp(0.12, 0.20, CelHash11(curveSeed + 13.0));
                            float rightMass = lerp(0.09, 0.17, CelHash11(curveSeed + 19.0));
                            float taperFlip = CelHash11(curveSeed + 17.0) > 0.5 ? -1.0 : 1.0;
                            [unroll] for (int part = 0; part < 2; part++)
                            {
                                float lo = part == 0 ? -halfSpan : split + gap;
                                float hi = part == 0 ? split - gap : halfSpan;
                                [unroll] for (int lobe = 0; lobe < 5; lobe++)
                                {
                                    float along = lerp(lo, hi, lobe * 0.25);
                                    float u = along / halfSpan;
                                    float spine = bend * (1.0 - u * u) + roll * u * (1.0 - u * u) + tilt * u;
                                    float leftLobe = (u + 0.52) / 0.42;
                                    float rightLobe = (u - 0.38) / 0.36;
                                    float middleLobe = (u + 0.02) / 0.18;
                                    float width = (0.07 + leftMass * exp(-leftLobe * leftLobe)
                                        + rightMass * exp(-rightLobe * rightLobe) + 0.045 * exp(-middleLobe * middleLobe))
                                        * lerp(1.0, 0.70, (u * taperFlip + 1.0) * 0.5);
                                    float neckZone = 1.0 - smoothstep(0.0, 0.25, abs(along - split));
                                    width *= 1.0 - neckZone * neck * 0.75;
                                    float d = (length(curvePoint - float2(along, spine)) - width) * lateScale;
                                    curveDistance = min(curveDistance, d);
                                    float2 lobeLocal = curvePoint - float2(along, spine);
                                    float lobeZ2 = width * width - dot(lobeLocal, lobeLocal);
                                    if (lobeZ2 > 0.0)
                                    {
                                        float lobeZ = sqrt(lobeZ2);
                                        float smoothFront = max(width * 0.42, 0.001);
                                        float join = saturate(0.5 + 0.5 * (lobeZ - curveFront) / smoothFront);
                                        float2 localNormal = lobeLocal / max(width, 0.0001);
                                        float2 screenNormal = axis * localNormal.x
                                            + float2(-axis.y, axis.x) * localNormal.y;
                                        float3 lobeNormal = float3(screenNormal, lobeZ / max(width, 0.0001));
                                        curveNormal = normalize(lerp(curveNormal, lobeNormal, join));
                                        curveFront = lerp(curveFront, lobeZ, join) + smoothFront * join * (1.0 - join);
                                    }
                                }
                            }
                        }
                        distanceField = lerp(distanceField, curveDistance, handover);
                        puffNormal = normalize(lerp(puffNormal, curveNormal, handover));
                    }
                    // v13: the clip's puffs are soft-edged with soft low-contrast patches (smoke lum p10/p50/p90
                    // 89/101/118 at f104-f112, v12 92/115/150) and no dark outline.
                    float mask = 1.0 - smoothstep(-0.07, 0.03, distanceField);
                    // Broad cel patches follow individual cloud lobes, including the late curls.
                    float light = dot(puffNormal, normalize(float3(-0.36, 0.82, 0.45)));
                    light += (CelHash11(seed * 67.0 + 7.0) - 0.5) * 0.12
                        + sin(q.x * 6.0 + q.y * 3.0 + seed * 19.0) * 0.045;
                    float shadeAA = max(fwidth(light) * 0.9, 0.07);
                    half3 col = CelPal(row, 1);
                    col = lerp(col, lerp(CelPal(row, 1), CelPal(row, 2), 0.45), smoothstep(0.12 - shadeAA, 0.12 + shadeAA, light));
                    col = lerp(col, lerp(CelPal(row, 1), CelPal(row, 2), 0.85), smoothstep(0.66 - shadeAA, 0.66 + shadeAA, light));
                    // v14: the clip's ring-1 puffs are bluish (median 116,119,128); v13's multiply pushed them warm.
                    col *= half3(0.87, 0.85, 0.91); // v15: +10 % (v14 p50 108-129 vs the clip's 118-137)
                    float fade = (1.0 - smoothstep(0.985, 1.0, k))
                        * (1.0 - smoothstep(1.16, 1.18, t));
                    // v04: the lobes form ON the rolling torus (ring 1 from ~f29, ring 2 from ~f101).
                    // v16: ring 1's puffs fade in f25-f28 - the clip's torus is a soft band until f27, lumps from f28.
                    float alpha = mask * fade * (i.shape.z > 0.5 ? smoothstep(0.06, 0.16, t) : smoothstep(0.14, 0.28, t));
                    return half4(MixFog(col, i.fog) * alpha, alpha);
                }
                // v23d: a lumpy rock, not a hexagon (the clip's chunks, f68)
                float rockA = atan2(q.y, q.x);
                float polygon = length(q * float2(0.95, 1.15)) / (1.0 + 0.20 * sin(rockA * 3.0 + seed * 7.0) + 0.09 * sin(rockA * 5.0 - seed * 3.0));
                float maskRock = 1.0 - CelAAStep(0.78, polygon);
                half3 rock = lerp(CelPal(row, 0), CelPal(row, 2), step(0.1, q.x + q.y * 0.5));
                if (i.shape.z > 0.5)
                {
                    float stoneShape = max(length(q * float2(1.0, 1.15)) - 0.80, -q.y - 0.45);
                    maskRock = 1.0 - CelAAStep(0.0, stoneShape);
                    rock = lerp(CelPal(row, 0), CelPal(row, 1), step(-0.18, q.y + q.x * 0.2));
                    rock = lerp(rock, CelPal(row, 2), step(0.22, q.y - q.x * 0.45));
                    rock *= i.tone.y;
                }
                if (i.shape.z < -0.5)
                {
                    // v14: the end glints are pink-red (clip f104-f108 ~ 175,100,105), not blue flakes.
                    half3 glow = lerp(half3(1.5, 0.32, 0.38), half3(2.6, 1.3, 1.35),
                        1.0 - CelAAStep(0.38, abs(q.x) + abs(q.y)));
                    return half4(CelAdditiveFog(glow, i.positionWS, i.fog) * maskRock, maskRock);
                }
                return half4(MixFog(rock, i.fog) * maskRock, maskRock);
            }
            ENDHLSL
        }
    }
}
