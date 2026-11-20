// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - ExpFx: the explosion's own additive kind, one strip mesh
// (CelMeshes.Strip(8)) and one draw for three looks, picked per record by extra.x:
//   0 soft ground pool   a smooth radial falloff lying on the ground, white-yellow middle to nothing (no bands)
//   1 soft flash         the same field turned to the eye: a dome of light, a fireball's white heart, a bloom halo
//   3 ragged star       r04: the soft flash's field cut to an n-pointed star with uneven spikes that re-jump 12
//                        times a second (extra.w points), banded white heart / yellow / orange rim (the clip's
//                        white-yellow star over the fireball)
//   2 jagged arc         a white spark arc from the record outward along its velocity, kinked at three knots that
//                        re-jump 12 times a second (the reference's lightning-like spark arcs)
// The glows fade smoothly (light, not a shape); the arc is banded and cut like a streak.
//   size.xyz   glows: radius (m) at birth, at death, ease        arc: width (m) at birth, at death, ease
//   size.w     arc: length (m) (x 0.45..1 by the record's seed)
//   erode.xyz  glows: how far it has faded (0 full .. 1 gone) over the life; arc: the cross-section cut threshold
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      x mode, y glows: falloff power (0: 1.5), z arc: kink size (x length); flash: the ground cut (m below
//              the centre; 0 none: a dome of light whose foot fades out over 0.18 of its radius), w flash: its own pull toward the
//              eye with fog taken at the drawn place (0: ExpPull, true fog); star: points
// A flash is drawn pulled toward the eye (same screen size, ExpPull of its distance), so a ruin or bank between it
// and the eye does not hide a far dome; its fog is taken where it really is.
// Palette use: glows blend c1 (edge) to c3 (middle) and to core x hdr above the core threshold; arcs use CelRamp.
Shader "TW/Cel/ExpFx"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+18" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelExpFx"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off ZTest LEqual Cull Off
            Offset -1, -1
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv : TEXCOORD0;                          // glows: -1..1 corner; arc: x 0 root .. 1 tip, y -1..1
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w fade / cut threshold
                nointerpolation float4 shape : TEXCOORD2;       // extra
                float4 more : TEXCOORD3;                        // x fog
                float3 positionWS : TEXCOORD4;
                float4 screen : TEXCOORD5;
            };

            // the arc's sideways offset at knot i of 4 sections (0 at the root and the tip), -1..1
            #define ExpPull 0.55

            float ExpKnot(float i, float seed, float tick)
            {
                if (i < 0.5 || i > 3.5) return 0.0;
                return CelHash21(float2(seed * 97.13 + i * 1.7, tick + seed * 13.0)) * 2.0 - 1.0;
            }

            float ExpZig(float a, float seed, float tick)
            {
                float s = a * 4.0;
                float i = min(floor(s), 3.0);
                return lerp(ExpKnot(i, seed, tick), ExpKnot(i + 1.0, seed, tick), s - i);
            }

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 c = CelPosition(r, t);
                float mode = r.extra.x;
                float2 corner = v.uv * 2.0 - 1.0;
                float3 w, wf;
                float2 uv;
                float cut = 1.0;
                if (mode > 7.5)
                {
                    // Short-lived fan sparks travel farther left; dots and longer flakes retain their paths.
                    // Keep the original flare dimensions while orienting them along the corrected velocity.
                    float flareTravel = distance(c, r.pos);
                    float3 flareVelocity = CelVelocity(r, t);
                    float speed = length(flareVelocity);
                    float3 viewRight = UNITY_MATRIX_V[0].xyz;
                    float leftSpeed = -dot(r.vel, viewRight);
                    float upwardSpeed = dot(r.vel, UNITY_MATRIX_V[1].xyz);
                    float shortSpark = (r.extra.y > 0.05 ? 1.0 : 0.0)
                        * (1.0 - smoothstep(0.32, 0.36, r.life))
                        * smoothstep(2.0, 3.0, leftSpeed)
                        * (1.0 - smoothstep(2.0, 2.5, upwardSpeed));
                    float extraLeftSpeed = min(2.0 * leftSpeed, max(0.0, 12.0 - leftSpeed));
                    float3 extraVelocity = -viewRight * extraLeftSpeed * shortSpark;
                    c += extraVelocity * CelDragInt(r.motion.y, max(t, 0.0));
                    float3 velocity = flareVelocity + extraVelocity * exp(-r.motion.y * max(t, 0.0));
                    float directionalSpeed = length(velocity);
                    float3 dir = directionalSpeed > 1e-4 ? velocity / directionalSpeed : viewRight;
                    float width = CelCurve(r.size.xyz, k) * alive * 1.15;
                    float span = max(speed * r.size.w, max(r.extra.y, width)) * 2.30;
                    span = min(span, flareTravel + width);
                    float3 side = cross(dir, normalize(_WorldSpaceCameraPos - c));
                    side = dot(side, side) > 1e-6 ? normalize(side) : UNITY_MATRIX_V[1].xyz;
                    wf = c + dir * lerp(-span, width * 0.5, v.uv.x) + side * corner.y * width * 0.65;
                    w = wf; uv = float2(v.uv.x, corner.y);
                }
                else if (mode > 6.5)
                {
                    float width = CelCurve(r.size.xyz, k) * alive;
                    float span = lerp(r.size.w, r.extra.y, smoothstep(0.0, r.extra.w, k)) * alive;
                    float3 dir = normalize(r.vel);
                    c = r.pos; // Velocity encodes the authored direction; aftertrail root remains anchored.
                    if (r.extra.y > 7.0)
                        c += (UNITY_MATRIX_V[1].xyz * 0.57 - UNITY_MATRIX_V[0].xyz * 0.10)
                            * (1.0 - smoothstep(0.0, 0.15, k));
                    float3 side = normalize(cross(dir, normalize(_WorldSpaceCameraPos - c)));
                    wf = c + dir * (v.uv.x * span) + side * corner.y * width;
                    w = _WorldSpaceCameraPos + (wf - _WorldSpaceCameraPos) * r.extra.z;
                    uv = float2(v.uv.x, corner.y);
                }
                else if (mode > 5.5)
                {
                    // Authored landing age freezes a dying ember at its ground contact.
                    // Zero retains ordinary ballistic motion for all existing ember layers.
                    if (r.extra.w > 0.0) c = CelPosition(r, min(t, r.extra.w));
                    float rad = CelCurve(r.size.xyz, k) * alive;
                    float tilt = (r.seed - 0.5) * 0.7;
                    float2 q = corner * float2(rad, rad * 2.6);
                    q.x += q.y * tilt;
                    w = CelBillboard(c, q); wf = w; uv = corner;
                }
                else if (mode < 1.5 || (mode > 2.5 && mode < 3.5) || mode > 4.5)
                {
                    if (mode > 4.5) c = CelPosition(r, min(t, 0.13));
                    float radius = CelCurve(r.size.xyz, k) * alive;
                    if (mode > 4.5)
                        radius = lerp(r.size.x, r.size.y, smoothstep(0.0, 0.28, k))
                            * (1.0 - 0.2 * smoothstep(0.5, 1.0, k)) * alive;
                    if (mode < 0.5) { w = c + float3(corner.x, 0.0, corner.y) * radius + float3(0.0, 0.03, 0.0); wf = w; }
                    else
                    {
                        wf = CelBillboard(c, corner * float2(radius, radius * (mode > 4.5 || (mode > 1.15 && mode < 1.25) ? 0.82 : 1.0)));
                        // extra.w > 0 on a flash: its own pull, and fog taken where it is drawn (a far dome across the field)
                        float pull = mode > 4.5 ? (r.extra.w > 0.0 ? r.extra.w : 1.0) :
                            ((mode > 2.5 && mode < 3.5 && r.extra.y < 0.0) ? (r.extra.y < -1.5 ? 0.80 : 0.85) : ((mode < 1.5 && r.extra.w > 0.0) ? r.extra.w : ExpPull));
                        w = _WorldSpaceCameraPos + (wf - _WorldSpaceCameraPos) * pull;
                        // e03: the dome's foot. This used to be the raw height above the foot plane in metres,
                        // and the fragment cut it with saturate(cut * 40), i.e. a hard step 2.5 cm above the
                        // plane. Measured on the best take with a straight-step detector (and the same detector
                        // on the reference clip as a control, which finds none): f24 row 586 steps 71 luminance
                        // levels over a dead-straight 421 px run, f33 row 582 68 over 302 px, f59 row 666 58 over
                        // 232 px, f77 row 360 109 over 229 px and row 383 134 over 156 px. That straight line is
                        // what reads as a rectangle of light rather than a dome standing on the ground. It is now
                        // measured as a share of the dome's own radius, so the fragment can feather it.
                        float groundY = (mode > 4.5 ? r.pos.y : c.y) - r.extra.z;
                        cut = r.extra.z > 0.0 ? (wf.y - groundY) / max(radius * 0.06, 1e-3) : 1.0;
                    }
                    uv = corner;
                }
                else if (mode > 3.5)
                {
                    float flight = r.extra.w > 0.0 ? r.extra.w : r.life;
                    float travelTime = t;
                    // Sustained-stream heads were ahead of the measured lower lane at frames22/24.
                    // Keep all opening shots and the separate upper companion on their existing paths.
                    if (mode > 4.25 && mode < 4.35)
                        travelTime = max(0.0, t - 0.027613 + 0.036880 * t);
                    if (mode > 4.05 && mode < 4.15)
                        travelTime += 0.0021 + 0.0142 * sin(3.14159265 * saturate((t - r.extra.z - 0.004) / 0.0833333));
                    else if (mode > 4.15 && mode < 4.25)
                        travelTime += 0.002 - 0.007 * exp(-pow((t - 0.162) / 0.018, 2.0));
                    c = CelPosition(r, min(travelTime, flight));
                    // Sustained stream and its first upper companion have independent view-plane lanes.
                    // Opening modes 4.0/4.1/4.2 retain their measured path and timing.
                    if (mode > 4.25 && mode < 4.45)
                        c += UNITY_MATRIX_V[1].xyz * (mode > 4.35 ? 0.46 : 0.17);
                    float3 vel = CelVelocity(r, min(t, flight));
                    float speed = length(vel);
                    float3 dir = speed > 1e-4 ? vel / speed : float3(1, 0, 0);
                    float width = CelCurve(r.size.xyz, k) * alive;
                    float span = lerp(r.size.w, r.extra.y > 0.0 ? r.extra.y : r.size.w,
                        smoothstep(r.extra.z, r.extra.z + 0.07, t));
                    float len = min(max(speed * span, width), distance(c, r.pos) + width);
                    float3 side = normalize(cross(dir, normalize(_WorldSpaceCameraPos - c)));
                    w = c + dir * lerp(-len, width * 0.5, v.uv.x) + side * corner.y * width;
                    wf = w;
                    // Incoming trails clear the foreground trench while retaining their world fog and projected lane.
                    w = _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * 0.85;
                    uv = float2(v.uv.x, corner.y);
                    // The first visible frame may show a round already in flight with its full tail.
                    cut = smoothstep(r.extra.z, r.extra.z + 0.004, t)
                        * (1.0 - smoothstep(flight, max(flight + 0.001, r.life), t));
                }
                else
                {
                    float3 vel = CelVelocity(r, t);
                    float sp = length(vel);
                    float3 dir = sp > 1e-4 ? vel / sp : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                    bool directedArc = mode > 1.5 && mode < 2.5 && r.extra.y > 0.0;
                    // Only the dedicated directed-arc mesh encodes a daughter strip in uv.y 2..3.
                    bool arcFork = directedArc && v.uv.y > 1.5;
                    float arcAcross = arcFork ? (v.uv.y - 2.0) * 2.0 - 1.0 : corner.y;
                    if (directedArc)
                    {
                        float angle = r.extra.y * 0.0174532925 + (r.seed - 0.5) * 0.08;
                        dir = UNITY_MATRIX_V[0].xyz * cos(angle) + UNITY_MATRIX_V[1].xyz * sin(angle);
                    }
                    float width = CelCurve(r.size.xyz, k) * alive;
                    float len = r.size.w * lerp(directedArc ? 0.90 : 0.45, 1.0, r.seed) * alive;
                    float3 toEye = normalize(_WorldSpaceCameraPos - c);
                    float3 side = cross(dir, toEye);
                    float sl = dot(side, side);
                    side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                    float forkT = v.uv.x;
                    float forkStart = 0.72, forkEnd = 0.94, forkSpan = 0.06;
                    if (arcFork)
                    {
                        // Upper accents carry a backward thorn; the lateral accent is nearly transverse.
                        // Left-going accents keep a short forward fork from the shoulder.
                        if (r.extra.y >= 70.0 && r.extra.y < 120.0)
                        { forkStart = 0.78; forkEnd = 0.67; forkSpan = 0.07; }
                        else if (r.extra.y < 70.0)
                        { forkStart = 0.80; forkEnd = 0.84; forkSpan = 0.09; }
                    }
                    float a = arcFork ? lerp(forkStart, forkEnd, forkT) : v.uv.x;
                    float tick = floor(_CelNow * 12.0);
                    float zig = ExpZig(a, r.seed, tick) * r.extra.z * len;
                    // Authored curved whips: a stable hook moves with the spark rather than
                    // changing into an unrelated three-corner lightning bolt each tick.
                    if (r.extra.w > 0.0)
                    {
                        float signBend = r.seed < 0.5 ? -1.0 : 1.0;
                        zig = signBend * len * (sin(a * 3.14159265) * 0.22
                            + sin(a * 6.2831853 + r.seed * 6.0 + t * 4.0) * a * 0.055);
                    }
                    float taper = lerp(1.0, 0.04, a);
                    if (directedArc)
                    {
                        // A fine shaft flares at an angular shoulder, then hooks into its tip.
                        // Localized bends avoid the uniformly broad parallel bows of the first pass.
                        float shoulder = 0.75 + (r.seed - 0.5) * 0.035;
                        float tooth = saturate(1.0 - abs(a - shoulder) / 0.075)
                            - 0.65 * saturate(1.0 - abs(a - (shoulder + 0.12)) / 0.065);
                        float bendSign = r.extra.z < 0.0 ? -1.0 : 1.0;
                        zig = len * (sin(a * 3.14159265) * r.extra.z + tooth * 0.028 * bendSign
                            + sin(a * 6.2831853 + r.seed * 6.0 + t * 3.0) * a * 0.012);
                        taper = (0.14 + 0.38 * pow(saturate(sin(a * 3.14159265)), 0.55)
                            + 0.72 * exp(-pow((a - shoulder) / 0.065, 2.0)))
                            * smoothstep(0.0, 0.035, a) * (1.0 - smoothstep(0.94, 1.0, a));
                        taper *= 0.88 + 0.12 * sin(a * 23.0 + r.seed * 9.0);
                    }
                    if (arcFork)
                    {
                        // The root lies on the unchanged main centreline. Offset then grows into a pointed spur.
                        zig -= len * forkSpan * (forkT + 0.10 * sin(forkT * 3.14159265));
                        taper = 0.35 * pow(saturate(1.0 - forkT), 0.70);
                    }
                    w = c + dir * (a * len) + side * (zig + arcAcross * width * 0.5 * taper);
                    wf = w;
                    if (directedArc && r.extra.w > 2.5 && r.extra.w < 3.5)
                    {
                        // Only the appended right closing hook: turn its cap outward while
                        // keeping the lower stem nearer the smoke edge. Preserve its original fog.
                        float closingTip = 0.45 * exp(-pow((a - 0.885) / 0.105, 2.0));
                        float closingStem = 0.36 * exp(-pow((a - 0.39) / 0.20, 2.0));
                        float closingShift = (closingTip - closingStem) * smoothstep(0.0, 0.05, a);
                        w += UNITY_MATRIX_V[0].xyz * closingShift;
                    }
                    // View-plane downward hooks otherwise intersect the terrain before their tip.
                    // Preserve their projection and fog position while keeping this authored card above it.
                    if (directedArc) w = _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * 0.90;
                    uv = float2(a, arcAcross);
                }
                Varyings o;
                o.positionCS = TransformWorldToHClip(w);
                o.screen = ComputeScreenPos(o.positionCS);
                o.uv = uv;
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = r.extra;
                // Tag 11 has no use for shape.z; carry local age for its late white-centre fade.
                if (mode > 2.5 && mode < 3.5 && r.extra.y >= 0.0 && r.extra.y < 0.5
                    && r.extra.w > 10.5 && r.extra.w < 11.5)
                    o.shape.z = max(t, 0.0);
                float3 fogAt = (mode > 6.5 || (((mode > 0.5 && mode < 1.5) || (mode > 4.5 && mode < 5.5)) && r.extra.w > 0.0)) ? w : wf;
                o.positionWS = fogAt; // Explicit site cards use the same position for both mist and linear fog.
                o.more = float4(ComputeFogFactor(TransformWorldToHClip(fogAt).z), k, cut, r.seed);
                return o;
            }

            // Three short curved tongues for the Far cooling field only. No radial spoke ring.
            float FarTongueField(float2 p, float2 a, float2 b, float2 c, float width)
            {
                float field = -1e3;
                [unroll] for (int j = 0; j < 4; j++)
                {
                    float t0 = j * 0.25, t1 = (j + 1) * 0.25;
                    float2 q0 = lerp(lerp(a, b, t0), lerp(b, c, t0), t0);
                    float2 q1 = lerp(lerp(a, b, t1), lerp(b, c, t1), t1);
                    float2 span = q1 - q0;
                    float u = saturate(dot(p - q0, span) / max(dot(span, span), 1e-5));
                    float along = lerp(t0, t1, u);
                    float radius = width * lerp(1.0, 0.08, pow(along, 0.7));
                    field = max(field, radius - length(p - (q0 + span * u)));
                }
                return field;
            }

            // Measured connected Far hot-body contours from reference82 and84.
            static const float2 FarBodyContour0[32] = {
                float2(-0.486032, -0.909738),
                float2(-0.494766, -0.716591),
                float2(-0.503500, -0.523444),
                float2(-0.512233, -0.330297),
                float2(-0.567883, -0.156886),
                float2(-0.590790, 0.009611),
                float2(-0.590493, 0.188359),
                float2(-0.652143, 0.366355),
                float2(-0.710865, 0.517928),
                float2(-0.561640, 0.451806),
                float2(-0.458263, 0.569782),
                float2(-0.301859, 0.601881),
                float2(-0.160394, 0.581988),
                float2(-0.039446, 0.624954),
                float2(0.070397, 0.545698),
                float2(0.168752, 0.496773),
                float2(0.274890, 0.369467),
                float2(0.353795, 0.341614),
                float2(0.341475, 0.199017),
                float2(0.476255, 0.299318),
                float2(0.508377, 0.224293),
                float2(0.460503, 0.046585),
                float2(0.554807, -0.105419),
                float2(0.420987, -0.209249),
                float2(0.385205, -0.384893),
                float2(0.372879, -0.577719),
                float2(0.360554, -0.770546),
                float2(0.307589, -0.909840),
                float2(0.148864, -0.909819),
                float2(-0.009860, -0.909799),
                float2(-0.168584, -0.909779),
                float2(-0.327308, -0.909759)
            };
            static const float2 FarBodyContour1[32] = {
                float2(-0.442955, -0.976793),
                float2(-0.438735, -0.800991),
                float2(-0.434515, -0.625190),
                float2(-0.430295, -0.449388),
                float2(-0.467613, -0.293664),
                float2(-0.574696, -0.175990),
                float2(-0.531076, -0.046868),
                float2(-0.595537, 0.098460),
                float2(-0.617598, 0.266260),
                float2(-0.623927, 0.440143),
                float2(-0.651579, 0.557593),
                float2(-0.510452, 0.557210),
                float2(-0.373790, 0.591657),
                float2(-0.232812, 0.604944),
                float2(-0.093147, 0.589122),
                float2(0.047928, 0.596185),
                float2(0.148872, 0.470474),
                float2(0.239150, 0.333121),
                float2(0.322162, 0.395031),
                float2(0.368906, 0.374426),
                float2(0.352367, 0.203833),
                float2(0.324953, 0.039692),
                float2(0.402567, -0.108703),
                float2(0.374429, -0.261429),
                float2(0.321918, -0.422637),
                float2(0.299787, -0.596409),
                float2(0.277656, -0.770181),
                float2(0.255525, -0.943953),
                float2(0.134353, -0.976867),
                float2(-0.009974, -0.976848),
                float2(-0.154301, -0.976830),
                float2(-0.298628, -0.976812)
            };

            float FarMeasuredBody(float2 p, float phase)
            {
                float distanceToEdge = 1e3;
                bool inside = false;
                float blend = smoothstep(0.224138, 0.367816, phase);
                [unroll] for (int j = 0; j < 32; j++)
                {
                    int next = j == 31 ? 0 : j + 1;
                    float2 a = lerp(FarBodyContour0[j], FarBodyContour1[j], blend);
                    float2 b = lerp(FarBodyContour0[next], FarBodyContour1[next], blend);
                    float2 span = b - a;
                    float u = saturate(dot(p - a, span) / max(dot(span, span), 1e-5));
                    distanceToEdge = min(distanceToEdge, length(p - (a + span * u)));
                    if ((a.y > p.y) != (b.y > p.y))
                    {
                        float hitX = a.x + (p.y - a.y) * span.x / span.y;
                        if (p.x < hitX) inside = !inside;
                    }
                }
                return inside ? distanceToEdge : -distanceToEdge;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                half3 col;
                if (i.shape.x > 7.5)
                {
                    float along = i.uv.x;
                    float flank = 0.66 + 0.12 * sin(along * 12.0 + i.more.w * 29.0)
                        + 0.07 * sin(along * 23.0 - i.more.w * 17.0);
                    float side = (i.uv.y + 0.08 * sin(along * 9.0 + i.more.w * 13.0)) / flank;
                    float axial = (along - 0.64) / 0.44;
                    float field = side * side + axial * axial;
                    float warm = exp(-field * 1.5);
                    float hot = exp(-field * 7.0);
                    float edgeFade = smoothstep(0.0, 0.08, along) * (1.0 - smoothstep(0.92, 1.0, along));
                    float ageFade = smoothstep(0.0, 0.06, i.more.y) * (1.0 - smoothstep(0.76, 1.0, i.more.y));
                    float pulse = 0.85 + 0.15 * sin(i.more.y * 18.0 + i.more.w * 37.0);
                    col = (CelPal(row, 1) * (half)(warm * 2.0)
                        + CelPalCore(row).rgb * (half)(hot * i.tone.y * 0.60))
                        * (half)(edgeFade * ageFade * pulse * i.tone.z);
                }
                else if (i.shape.x > 6.5)
                {
                    float crossLine = i.uv.y / max(0.05, pow(sin(3.14159265 * i.uv.x), 0.6));
                    float env = exp(-crossLine * crossLine * 3.0);
                    float core = exp(-crossLine * crossLine * 18.0);
                    float ends = smoothstep(0.0, 0.06, i.uv.x) * (1.0 - smoothstep(0.90, 1.0, i.uv.x));
                    float cool = i.shape.y < 3.0 ? smoothstep(0.15, 0.55, i.more.y) : smoothstep(0.55, 0.94, i.more.y);
                    float fade = 1.0 - smoothstep(0.80, 1.0, i.more.y);
                    half3 hot = CelPal(row, 2) * (half)(env * 1.2) + CelPalCore(row).rgb * (half)(core * i.tone.y);
                    col = lerp(hot, CelPal(row, 0) * (half)(env * 0.8), (half)cool) * (half)(ends * fade * i.tone.z);
                }
                else if (i.shape.x > 5.5)
                {
                    float r2 = dot(i.uv, i.uv);
                    float hot = exp(-r2 * 16.0);
                    float halo = exp(-r2 * 3.4);
                    float fadeFrom = i.shape.y > 0.0 ? saturate(i.shape.y) : 0.65;
                    float fade = smoothstep(0.0, 0.08, i.more.y) * (1.0 - smoothstep(fadeFrom, 1.0, i.more.y));
                    col = (CelPal(row, 0) * (half)(halo * 0.45)
                        + CelPal(row, 2) * (half)(exp(-r2 * 7.0) * 0.8)
                        + CelPalCore(row).rgb * (half)(hot * i.tone.y)) * (half)(fade * i.tone.z);
                }
                else if (i.shape.x > 3.5 && i.shape.x < 4.5)
                {
                    float a = i.uv.x;
                    float width = lerp(0.035, 1.0, pow(a, 0.45));
                    width *= smoothstep(0.0, 0.16, 1.0 - a);
                    float openingEnergy = 1.0;
                    float openingHalo = 1.0;
                    bool firstOpening = i.shape.x > 4.15 && i.shape.x < 4.25;
                    bool secondOpening = i.shape.x > 4.05 && i.shape.x < 4.15;
                    if (firstOpening)
                    {
                        // First shot: broad at entry, then a bright forward flare over a very faint rear trail.
                        // k=.491/.661/.831 at reference frames14/15/16. Interpolate width and energy logarithmically.
                        float maturity = pow(saturate((i.more.y - 0.49) / 0.34), 1.3);
                        float matureWidth = 0.12 + 0.88 * pow(smoothstep(0.08, 0.90, a), 1.4);
                        float matureEnergy = 0.003 + 0.045 * smoothstep(0.12, 0.35, a)
                            + 0.31 * smoothstep(0.34, 0.48, a) + 0.50 * smoothstep(0.48, 0.84, a);
                        openingEnergy = lerp(1.08, 1.0, maturity) * pow(max(matureEnergy, 0.00001), maturity);
                        openingHalo = max(lerp(1.08, 0.018, maturity), openingEnergy);
                        width *= 1.25 * pow(matureWidth / 1.25, maturity);
                        width *= 1.0 + 0.30 * maturity * maturity * maturity * smoothstep(0.87, 0.96, a);
                    }
                    else if (secondOpening)
                    {
                        // The second shot enters with a broad rear shoulder. Its forward end is thinner,
                        // and it reaches the cooling profile sooner than the first shot.
                        float maturity = smoothstep(0.35, 0.55, i.more.y);
                        float forwardTaper = smoothstep(0.55, 0.92, a);
                        float entryWidth = lerp(1.65, 0.60, forwardTaper);
                        float entryEnergy = lerp(1.0, 0.65, forwardTaper);
                        float matureWidth = (0.48 + 0.64 * smoothstep(0.32, 0.68, a))
                            * (1.0 - 0.18 * smoothstep(0.70, 0.93, a));
                        float matureEnergy = (0.03 + 0.80 * smoothstep(0.0, 0.78, a))
                            * (1.0 - 0.48 * smoothstep(0.72, 0.96, a));
                        openingEnergy = lerp(entryEnergy, matureEnergy, maturity);
                        openingHalo = max(0.025, openingEnergy);
                        width *= lerp(entryWidth, matureWidth, maturity);
                    }
                    float across = abs(i.uv.y) / max(width, 0.01);
                    float tip = smoothstep(0.0, 0.035, 1.0 - a);
                    float env = exp(-across * across * 5.0) * tip;
                    float hot = exp(-across * across * 45.0) * tip;
                    float halo = exp(-across * across * 0.65) * tip;
                    env *= openingEnergy;
                    hot *= openingEnergy;
                    halo *= openingHalo;
                    if (firstOpening || secondOpening)
                        halo *= 1.0 - smoothstep(0.83, 1.0, abs(i.uv.y));
                    col = (CelPal(row, 0) * (half)(halo * 0.22) + CelPal(row, 2) * (half)(env * 1.3)
                        + CelPalCore(row).rgb * (half)(hot * i.tone.y)) * (half)(i.tone.z * i.more.z);
                    if (firstOpening || secondOpening)
                    {
                        // Entry ridge red is under target while green is already close. Keep this correction
                        // separate from the measured width/energy profile and fade it before the late trails.
                        float entryRed = firstOpening ? (1.0 - smoothstep(0.60, 0.75, i.more.y))
                            : (1.0 - smoothstep(0.38, 0.55, i.more.y));
                        col.r *= (half)(1.0 + 0.55 * entryRed);
                    }
                }
                else if (i.shape.x > 2.5 && i.shape.x < 3.5)
                {
                    // Soft pressure jets: thin tapered rays around a compact white centre. No concentric palette
                    // steps, circular petal tips or freshly randomized silhouette every other frame.
                    bool onsetWisp = i.shape.y < -1.5;
                    float n = i.shape.y < 0.0 ? 8.0 : clamp(i.shape.w * 0.65, 4.0, 9.0);
                    float radius = length(i.uv);
                    float rotation = i.shape.y < 0.0 ? 0.03 + 0.015 * sin(i.more.y * 3.14159265) : i.more.w * 3.1 + i.more.y * 0.045;
                    float ang = atan2(i.uv.y, i.uv.x) / 6.2831853 + rotation;
                    float sct = ang * n;
                    float idx = floor(sct + 0.5);
                    float raySeed = CelHash21(float2(idx + i.more.w * 31.0, i.more.w * 19.0));
                    float lenI = onsetWisp ? lerp(0.38, 1.0, pow(raySeed, 0.65)) : lerp(0.55, 1.0, raySeed);
                    // Pressure fingers keep approximately constant world width outside the
                    // hidden core; angular width alone made straight triangular wedges.
                    float spread = i.shape.y < 0.0
                        ? lerp(0.080, 0.050, saturate(radius / lenI)) / max(radius, 0.20)
                            * (0.91 + 0.09 * sin(radius * 18.0 + raySeed * 11.0))
                        : lerp(0.16, 0.035, saturate(radius / lenI));
                    if (onsetWisp) spread *= 0.56;
                    else if (i.shape.y < 0.0)
                    {
                        // Pressure is strongest near the body and disperses into a narrow, faint tip.
                        float pressureAlong = saturate(radius / lenI);
                        spread = lerp(0.115, 0.025, pressureAlong) / max(radius, 0.20)
                            * (0.91 + 0.09 * sin(radius * 18.0 + raySeed * 11.0));
                    }
                    float delta = sct - idx + (raySeed - 0.5) * (onsetWisp ? 0.32 : 0.15);
                    if (i.shape.y < 0.0) delta += radius * radius
                        * (onsetWisp ? (0.08 + 0.10 * raySeed) : (0.18 + 0.18 * raySeed))
                        * (raySeed < 0.5 ? -1.0 : 1.0);
                    float jet = exp(-delta * delta / max(spread * spread, 1e-4));
                    if (i.shape.y < 0.0)
                    {
                        float along = saturate(radius / lenI);
                        // Rounded luminous fingers curl away from the opaque body. Keep
                        // their broad part bright, then feather the last quarter to a tip.
                        jet *= smoothstep(0.03, 0.16, along) * (1.0 - smoothstep(0.76, 1.0, along));
                        jet *= 1.00 * smoothstep(0.10, 0.30, radius) * lerp(0.82, 1.18, raySeed);
                        if (onsetWisp)
                        {
                            // Short uneven fingers with broken outer stretches, rather than a uniform spoke wheel.
                            float breakup = CelValue2(float2(along * 7.0 + raySeed * 9.0,
                                delta * 12.0 + raySeed * 31.0 + i.more.y * 0.18));
                            float gaps = smoothstep(0.30, 0.48, breakup);
                            jet *= lerp(0.20, 1.10, smoothstep(0.15, 0.70, raySeed));
                            jet *= lerp(1.0, gaps, smoothstep(0.20, 0.55, along));
                        }
                    }
                    else jet *= pow(saturate(1.0 - radius / lenI), 1.3) * smoothstep(0.0, 0.10, radius);
                    if (onsetWisp)
                    {
                        // Six unequal opening fingers follow the reference's dominant directions.
                        // Each has its own width/length and a softly curved spine; no sector wheel seams.
                        jet = 0.0;
                        [unroll] for (int finger = 0; finger < 6; ++finger)
                        {
                            float4 raySpec = finger == 0 ? float4(4.0, 0.85, 0.065, 0.90)
                                : finger == 1 ? float4(38.0, 1.00, 0.055, 0.85)
                                : finger == 2 ? float4(79.0, 0.92, 0.080, 1.10)
                                : finger == 3 ? float4(118.0, 0.52, 0.045, 0.50)
                                : finger == 4 ? float4(155.0, 0.40, 0.040, 0.40)
                                : float4(235.0, 0.78, 0.065, 0.90);
                            float theta = raySpec.x * 0.01745329252;
                            float2 fingerDir = float2(cos(theta), sin(theta));
                            float alongFinger = dot(i.uv, fingerDir);
                            float acrossFinger = dot(i.uv, float2(-fingerDir.y, fingerDir.x));
                            acrossFinger += 0.035 * sin(saturate(alongFinger / raySpec.y) * 3.14159265)
                                * (finger % 2 == 0 ? -1.0 : 1.0);
                            float fingerGrain = CelValue2(float2(alongFinger * 7.0, finger * 3.7 + i.more.y * 0.3));
                            float fingerWidth = raySpec.z * (0.85 + 0.22 * fingerGrain);
                            float fingerMask = exp(-acrossFinger * acrossFinger / (fingerWidth * fingerWidth));
                            fingerMask *= smoothstep(0.08, 0.22, alongFinger)
                                * (1.0 - smoothstep(raySpec.y * 0.70, raySpec.y, alongFinger));
                            jet = max(jet, fingerMask * raySpec.w * (0.65 + 0.35 * fingerGrain));
                        }
                    }
                    if (onsetWisp)
                        jet *= 1.0 - smoothstep(0.30, 0.75, i.more.y);
                    else if (i.shape.y < 0.0)
                        jet *= 0.85 * exp(-2.1 * pow(saturate(radius / lenI), 2.0));
                    if (i.shape.y < -0.5 && !onsetWisp)
                    {
                        // Continuous pressure fingers: unequal curved paths with a broad shoulder and soft tip.
                        // The old sector field stacked identical thin spokes and lost the cream plume mass.
                        jet = 0.0;
                        [unroll] for (int finger = 0; finger < 10; ++finger)
                        {
                            float4 spec = finger == 0 ? float4(5, 1.18, 0.069, 0.06)
                                : finger == 1 ? float4(31, 1.03, 0.095, -0.07)
                                : finger == 2 ? float4(81, 0.68, 0.060, 0.05)
                                : finger == 3 ? float4(111, 0.68, 0.065, -0.05)
                                : finger == 4 ? float4(145, 0.73, 0.100, 0.06)
                                : finger == 5 ? float4(178, 0.94, 0.065, -0.04)
                                : finger == 6 ? float4(211, 0.80, 0.068, 0.06)
                                : finger == 7 ? float4(245, 0.48, 0.055, -0.06)
                                : finger == 8 ? float4(284, 0.38, 0.041, 0.07)
                                : float4(327, 0.55, 0.060, -0.05);
                            float clock = i.more.y * 2.98;
                            float pulse = 0.5 + 0.5 * sin(clock * (8.0 + finger * 0.31) + finger * 2.37);
                            float theta = (spec.x + 3.0 * sin(clock * 2.2 + finger * 1.7)) * 0.01745329252;
                            float2 direction = float2(cos(theta), sin(theta));
                            float along = dot(i.uv, direction);
                            float across = dot(i.uv, float2(-direction.y, direction.x));
                            float reach = spec.y * lerp(0.78, 1.0, pulse);
                            float u = saturate(along / reach);
                            across -= spec.w * sin(u * 3.14159265) + 0.012 * sin(u * 11.0 + clock * 2.7 + finger);
                            float width = spec.z * (1.65 - 1.40 * u) * lerp(0.75, 1.18, pulse);
                            float crossRatio = across / max(width, 0.001);
                            float body = exp(-crossRatio * crossRatio * (0.90 + 0.15 * sin(along * 34.0 + clock * 4.0 + finger)));
                            body *= smoothstep(0.08, 0.23, along) * (1.0 - smoothstep(reach * 0.72, reach, along));
                            float grain = CelValue2(float2(along * 12.0 - clock * 2.0, across * 15.0 + finger * 6.1));
                            body *= lerp(0.65, 1.0, grain) * lerp(0.48, 1.0, pulse);
                            jet = max(jet, body);
                        }
                        jet *= smoothstep(0.0, 0.055, i.more.y) * (1.0 - smoothstep(0.88, 1.0, i.more.y));
                    }
                    float centre = exp(-radius * radius * 60.0);
                    // The late impact remains yellow; do not renew an anchored white disc after the peak.
                    // Both retained closing records are younger than 0.14 s at reference frame 86.
                    if (i.shape.y >= 0.0 && i.shape.y < 0.5 && i.shape.w > 10.5 && i.shape.w < 11.5)
                        centre *= 1.0 - smoothstep(0.14, 0.18, i.shape.z);
                    float halo = exp(-radius * radius * 12.0) * 0.08;
                    half4 core = CelPalCore(row);
                    float fade = saturate(1.0 - i.tone.w);
                    half3 jetColor = lerp(CelPal(row, 3), max(CelPal(row, 3), core.rgb), 0.55);
                    col = jetColor * (half)(jet * 1.8 + halo)
                        + core.rgb * (half)(centre * max(i.tone.y, 0.0));
                    col *= (half)(i.tone.z * fade * smoothstep(0.0, 1.0, saturate(i.more.z)));
                    if (i.shape.y > 1.5)
                    {
                        // Compact warm splinters around the impact; no added white centre.
                        float impact = 0.0;
                        float clock = i.more.y * 2.90;
                        [unroll] for (int ray = 0; ray < 19; ++ray)
                        {
                            float seed = CelHash21(float2(ray * 3.17, 11.9));
                            float angle = (ray * 0.33069396 + (seed - 0.5) * 0.28)
                                + 0.045 * sin(clock * 5.0 + ray);
                            float2 axis = float2(cos(angle), sin(angle));
                            float x = dot(i.uv, axis);
                            float y = dot(i.uv, float2(-axis.y, axis.x));
                            float pulse = 0.5 + 0.5 * sin(clock * (10.0 + seed * 7.0) + ray * 2.1);
                            float reach = lerp(0.48, 0.98, seed) * lerp(0.72, 1.0, pulse);
                            float u = saturate(x / reach);
                            y += (seed - 0.5) * 0.09 * sin(u * 3.14159265);
                            float width = lerp(0.018, 0.045, seed) * pow(saturate(1.0 - u), 0.85);
                            float field = (1.0 - smoothstep(width * 0.55, width + 0.007, abs(y)))
                                * smoothstep(0.12, 0.23, x) * (1.0 - smoothstep(reach * 0.87, reach, x));
                            impact = max(impact, field * lerp(0.48, 1.0, pulse) * pow(saturate(1.0 - u), 0.45));
                        }
                        float alive = smoothstep(0.0, 0.035, i.more.y) * (1.0 - smoothstep(0.88, 1.0, i.more.y));
                        col = half3(1.0, 0.70, 0.16) * (half)(impact * i.tone.z * alive);
                    }
                }
                else if (i.shape.x > 1.15 && i.shape.x < 1.25)
                {
                    // Far79-92: measured uneven connected body; the existing plume supplies flying splinters.
                    float2 q = i.uv;
                    float field = FarMeasuredBody(q, i.more.y);
                    float eruptionGrain = CelValue2(q * float2(5.0, 7.0) + float2(i.more.w * 19.0, -i.more.y * 1.6));
                    field += (eruptionGrain - 0.5) * 0.010;
                    float edge = smoothstep(-0.025, 0.04, field);
                    float heat = smoothstep(0.0, 0.32, field) * (1.0 - 0.55 * smoothstep(0.25, 0.70, i.more.y));
                    half3 warm = lerp(CelPal(row, 0) * half3(1.0, 0.85, 0.60), CelPal(row, 2), (half)heat);
                    float white = (1.0 - smoothstep(0.15, 0.44, i.more.y)) * smoothstep(0.10, 0.40, field);
                    col = lerp(warm, CelPalCore(row).rgb * 1.80, (half)white) * (half)max(i.tone.y, 0.0);
                    float envelope = edge * lerp(0.55, 1.0, smoothstep(0.0, 0.25, field));
                    float fade = smoothstep(0.0, 0.08, i.more.y) * (1.0 - smoothstep(0.65, 1.0, i.more.y));
                    col *= (half)(envelope * fade * i.tone.z * smoothstep(0.0, 1.0, saturate(i.more.z)));
                }
                else if (i.shape.x < 1.5 || i.shape.x > 4.5)
                {
                    float field = saturate(1.0 - length(i.uv));
                    float p = i.shape.y > 0.0 ? i.shape.y : 1.5;
                    float env = pow(field, p);
                    half4 core = CelPalCore(row);
                    col = lerp(CelPal(row, 1), CelPal(row, 3), (half)smoothstep(0.0, 0.45, field));
                    float coreMix = i.shape.x > 4.5 ? smoothstep(0.12, 0.70, field)
                        : smoothstep(min(core.a, 0.99), 1.0, field);
                    col = lerp(col, core.rgb * (half)max(i.tone.y, 0.0), (half)coreMix);
                    col *= (half)(env * i.tone.z * saturate(1.0 - i.tone.w) * smoothstep(0.0, 1.0, saturate(i.more.z)));
                    // The opening dome gives way to pressure jets by frame22. Secondary domes keep their own timing.
                    if (i.shape.x > 5.05 && i.shape.x < 5.5)
                        col *= (half)(1.0 - smoothstep(0.48, 0.78, i.more.y));
                    if (i.shape.x < 0.5)
                    {
                        // The pool is a horizontal mesh over uneven ground. Feather its actual depth crossing;
                        // a radial eruptionGrain alone cannot hide the straight terrain intersection through its centre.
                        float scene = LinearEyeDepth(SampleSceneDepth(i.screen.xy / i.screen.w), _ZBufferParams);
                        float eye = LinearEyeDepth(i.positionCS.z, _ZBufferParams);
                        col *= (half)smoothstep(0.0, 0.35, max(scene - eye, 0.0));
                    }
                }
                else
                {
                    float across = saturate(1.0 - abs(i.uv.y));
                    float field = across * lerp(1.0, 0.7, i.uv.x) * saturate((1.0 - i.uv.x) * 8.0 + 0.2);
                    float mask = CelAAStep(max(i.tone.w, 0.02), field);
                    col = CelRamp(field, row, i.tone.y) * (half)(mask * i.tone.z);
                    // Late directed arcs carry warm cream energy; the reference white is concentrated at the impact.
                    if (i.shape.w > 1.5) col = min(col, half3(2.0, 1.6, 0.45));
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
