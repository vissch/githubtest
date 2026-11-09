// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - GlBolt: the green laser's own primitive, one quad,
// additive. Two modes, picked per record by extra.x (one kind = one draw for both):
//   mode 0  dart: a bulbous white-hot head (3x the tail's width) where the record is now, a ragged saturated tail of a
//           fixed length in metres trailing back along its velocity (never behind its birth), and a soft bloom round
//           both (a smooth falloff outside the banded body, not a cel band).
//   mode 1  wash: a ground quad with a smooth radial falloff (no bands, no rings), faded in and out over the life.
//   size.xyz   mode 0: tail width (m) at birth, at death, ease;  mode 1: radius (m)       size.w unused
//   tone       x palette, y HDR core, z opacity, w unused
//   extra      mode 0: x 0, y tail length (m), z bloom radius (m), w head scale (0: 1.5 = 3x tail width)
//              mode 1: x 1, y fade-in (life 0..1), z fade-out start (life 0..1), w falloff (0: 3; higher = tighter)
// Colours: mode 0 bands the body with CelRamp of its palette (core above the row's threshold); the bloom and the wash
// use the palette's c3 (the lightest body colour) times the opacity.
Shader "TW/Cel/GlBolt"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+18" "IgnoreProjector"="True" }
        Pass
        {
            Name "CelGlBolt"
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
                float2 st : TEXCOORD0;                          // mode 0: metres along (0 = head centre, - = tail), across; mode 1: -1..1
                nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w mode
                nointerpolation float4 dims : TEXCOORD2;        // mode 0: x tail width, y tail length, z head radius, w bloom; mode 1: x envelope, w falloff
                float4 more : TEXCOORD3;                        // x fog, y seed
                float3 positionWS : TEXCOORD4;
            };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                CelRecord r = CelFetch(instanceID);
                float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
                float3 p = CelPosition(r, t);
                float mode = r.extra.x;
                float size = CelCurve(r.size.xyz, k) * alive;
                Varyings o;
                float3 w;
                if (mode > 0.5)
                {
                    float2 corner = v.uv * 2.0 - 1.0;
                    w = p + float3(corner.x, 0.0, corner.y) * size + float3(0.0, 0.3, 0.0);   // lifted clear of the bumps: its edge fades to nothing, so it never shows
                    float fin = r.extra.y > 0.0 ? smoothstep(0.0, r.extra.y, k) : 1.0;
                    float fout = 1.0 - smoothstep(saturate(r.extra.z), 1.0, k);
                    o.st = corner;
                    o.dims = float4(fin * fout, 0.0, 0.0, r.extra.w > 0.0 ? r.extra.w : 3.0);
                }
                else
                {
                    float3 vel = CelVelocity(r, t);
                    float speed = length(vel);
                    float3 dir = speed > 1e-3 ? vel / speed : float3(1, 0, 0);
                    float headR = size * 0.5 * (r.extra.w > 0.0 ? r.extra.w * 2.0 : 3.0);
                    float bloom = max(r.extra.z, 0.0) * alive;
                    float len = min(max(r.extra.y, size), distance(p, r.pos) + headR);
                    float3 toEye = normalize(_WorldSpaceCameraPos - p);
                    float3 side = cross(dir, toEye);
                    float sl = dot(side, side);
                    side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                    float along = lerp(-len - bloom, headR * 1.7 + bloom, v.uv.x);
                    float across = (v.uv.y * 2.0 - 1.0) * (headR + bloom);
                    w = p + dir * along + side * across;
                    o.st = float2(along, across);
                    o.dims = float4(size, len, headR, bloom);
                }
                o.positionWS = w;
                o.positionCS = TransformWorldToHClip(w);
                o.tone = float4(r.tone.x, r.tone.y, r.tone.z, mode);
                o.more = float4(ComputeFogFactor(o.positionCS.z), r.seed, 0, 0);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                half3 col;
                if (i.tone.w > 0.5)
                {
                    // the wash: a smooth radial falloff, no band, no ring
                    float rr = length(i.st);
                    float field = exp(-rr * rr * i.dims.w) * (1.0 - smoothstep(0.8, 1.0, rr));
                    col = CelPal(row, 3) * (field * i.dims.x * i.tone.z);
                }
                else
                {
                    float s = i.st.x, y = i.st.y;
                    float tw = i.dims.x, len = max(i.dims.y, 1e-3), headR = max(i.dims.z, 1e-4), bloom = i.dims.w;
                    // the head: an egg, long along the flight
                    float dHead = length(float2(s / 1.6, y)) - headR;
                    // the tail: a teardrop tapering back from the head's width, its edge torn by noise that runs with it
                    float u = saturate(-s / len);
                    float rag = CelValue2(float2(s * 6.0 - _CelNow * 9.0, i.more.y * 91.0 + sign(y) * 3.1));
                    float rT = headR * 0.85 * pow(1.0 - u, 1.1) * lerp(0.6, 1.3, rag);
                    float dTail = (s <= 0.0 && s >= -len) ? abs(y) - rT : 1e3;
                    float d = min(dHead, dTail);
                    // inside: banded (a white core about half the head, saturated green bands to the edge)
                    float v = saturate(-d / max(headR * 0.95, 1e-4));
                    half3 body = CelRamp(v, row, i.tone.y);
                    float inside = CelAAStep(0.0, -d);
                    // outside: a smooth bloom, strongest round the head
                    float x = max(d, 0.0) / max(bloom, 1e-4);
                    float near = exp(-4.0 * x) * (1.0 - smoothstep(0.6, 1.0, x));   // no visible rim at the quad's edge
                    float glow = near * lerp(0.3, 1.0, saturate(1.0 + s / max(headR * 3.0, 1e-4)));
                    col = lerp(CelPal(row, 3) * (glow * 0.6), body, inside) * i.tone.z;
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
