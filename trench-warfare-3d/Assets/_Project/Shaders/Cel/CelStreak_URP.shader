// Phase: CelFx kit (2026-11-09) - Streak: a velocity-stretched, camera-facing quad with a white HDR core and coloured
// cel edges. Tracers, sparks, embers. Additive. One record (CelFx.hlsl) per streak; the quad's head is where the record
// is now, its tail trails back along its velocity (length = speed x size.w seconds, never further back than where it
// was born, so a tracer does not start behind the muzzle).
//   size.xyz   width (m) at birth, at death, ease          size.w  trail length in seconds of its speed (0: a dot)
//   erode.xyz  the threshold the cross-section is cut at, rising over the life (thins it out in steps, no alpha fade)
//   tone       x palette, y HDR core, z opacity, w unused (streaks have no ink)
//   extra      x tail taper (0 pointed, 1 square), y min length (m), z flicker (0..1), w unused
Shader "TW/Cel/Streak"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+20" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelStreak"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
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
                float2 uv : TEXCOORD0;            // x 0 tail .. 1 head, y -1..1 across
                nointerpolation float4 tone : TEXCOORD1;          // x palette, y hdr, z opacity, w erosion threshold
                float4 shape : TEXCOORD2;         // x tail taper, y life 0..1, z flicker, w fog
                float3 positionWS : TEXCOORD3;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : (dot(r.axis, r.axis) > 1e-6 ? normalize(r.axis) : float3(0, 1, 0));
                float width = CelCurve(r.size.xyz, k) * alive;
                float len = max(speed * r.size.w, max(r.extra.y, width));
                len = min(len, distance(p, r.pos) + width);                 // no tail behind its birth
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;   // seen end on: any side will do
                float along = lerp(-len, width * 0.5, v.uv.x);
                float3 w = p + dir * along + side * (v.uv.y * 2.0 - 1.0) * width * 0.5;
                Varyings o;
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.uv = float2(v.uv.x, v.uv.y * 2.0 - 1.0);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, CelCurve(r.erode.xyz, k));
                o.shape = float4(r.extra.x, k, r.extra.z, ComputeFogFactor(o.positionCS.z));
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                // the cross-section narrows to the tail; the head is rounded by the last stretch of the quad
                float a = i.uv.x;
                float halfW = lerp(saturate(i.shape.x), 1.0, a);
                float across = 1.0 - abs(i.uv.y) / max(halfW, 1e-3);
                float head = saturate((1.0 - a) * 6.0 + 0.0);                 // the head end tapers over its last sixth
                float field = saturate(across) * lerp(0.55, 1.0, a) * lerp(0.0, 1.0, sqrt(head));
                float flick = 1.0 - i.shape.z * step(0.5, CelHash11(floor(_CelNow * 24.0) + i.tone.x * 7.0 + a));
                float mask = CelAAStep(max(i.tone.w, 0.02), field);
                half3 col = CelRamp(field, CelRowA(i.tone.x), i.tone.y);
                col *= mask * i.tone.z * flick;
                return half4(CelAdditiveFog(col, i.positionWS, i.shape.w), 1.0);
            }
            ENDHLSL
        }
    }
}
