// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - GlBolt: the green laser's own primitive, one quad.
// Three modes, picked per record by extra.x (one kind = one draw for all three; two passes):
//   mode 0  dart (additive): a pointed white-hot head, a chevron of swept-back barbs behind it, and a short ragged,
//           segmented saturated-green tail of a fixed length in metres trailing back along its velocity (never
//           behind its birth); a faint bloom, no ring.
//   mode 1  wash (additive): a ground quad with a smooth radial falloff (no bands, no rings), faded in and out.
//   mode 2  shade (multiply, second pass): a ground quad that darkens the dirt in a soft ring round the glow (the
//           reference's dark surround that makes the glow read); faded in and out like the wash.
//   size.xyz   mode 0: head half-width (m) at birth, at death, ease;  mode 1/2: radius (m)       size.w unused
//   tone       x palette, y HDR core, z opacity (mode 2: darkness 0..1), w unused
//   extra      mode 0: x 0, y tail length (m), z bloom (m), w head length / half-width (0: 4)
//              mode 1: x 1, y fade-in (life 0..1), z fade-out start (life 0..1), w falloff (0: 3; higher = tighter)
//              mode 2: x 2, y fade-in, z fade-out start, w ring centre (0..1 of the radius; 0: 0.7)
// Colours: mode 0 bands the body with CelRamp of its palette (core above the row's threshold); the bloom and the wash
// use the palette's c3 (the lightest body colour) times the opacity.
Shader "TW/Cel/GlBolt"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+18" "IgnoreProjector"="True" }
        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
        struct Varyings
        {
            float4 positionCS : SV_POSITION;
            float2 st : TEXCOORD0;                          // mode 0: metres along (0 = head centre, - = tail), across; mode 1/2: -1..1
            nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w mode
            nointerpolation float4 dims : TEXCOORD2;        // mode 0: x head half-width, y tail length, z head length, w bloom; mode 1/2: x envelope, w falloff / ring
            float4 more : TEXCOORD3;                        // x fog, y seed
            float3 positionWS : TEXCOORD4;
        };

        // darkPass: 1 for the multiply pass (only mode 2 draws), 0 for the additive pass (mode 2 collapses)
        Varyings GlVert(Attributes v, uint instanceID, float darkPass)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float mode = r.extra.x;
            float isDark = mode > 1.5 ? 1.0 : 0.0;
            alive *= (isDark == darkPass) ? 1.0 : 0.0;
            float3 p = CelPosition(r, t);
            float size = CelCurve(r.size.xyz, k) * alive;
            Varyings o;
            float3 w;
            if (mode > 0.5)
            {
                float2 corner = v.uv * 2.0 - 1.0;
                w = p + float3(corner.x, 0.0, corner.y) * size + float3(0.0, 0.3, 0.0);   // lifted clear of the bumps: its edge fades to nothing
                float fin = r.extra.y > 0.0 ? smoothstep(0.0, r.extra.y, k) : 1.0;
                float fout = 1.0 - smoothstep(saturate(r.extra.z), 1.0, k);
                o.st = corner;
                float shapeW = mode > 1.5 ? (r.extra.w > 0.0 ? r.extra.w : 0.7) : (r.extra.w > 0.0 ? r.extra.w : 3.0);
                o.dims = float4(fin * fout * alive, 0.0, 0.0, shapeW);
            }
            else
            {
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : float3(1, 0, 0);
                float hw = size;
                float hl = hw * (r.extra.w > 0.0 ? r.extra.w : 4.0);
                float bloom = max(r.extra.z, 0.0) * alive;
                float len = min(max(r.extra.y, hl), distance(p, r.pos) + hl * 0.6);
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float along = lerp(-len - bloom, hl * 0.6 + bloom, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * (hw * 2.6 + bloom);
                w = p + dir * along + side * across;
                o.st = float2(along, across);
                o.dims = float4(hw, len, hl, bloom);
            }
            o.positionWS = w;
            o.positionCS = TransformWorldToHClip(w);
            o.tone = float4(r.tone.x, r.tone.y, r.tone.z, mode);
            o.more = float4(ComputeFogFactor(o.positionCS.z), r.seed, 0, 0);
            return o;
        }

        // distance from p to the segment a-b, minus a radius tapering from ra at a to rb at b
        float GlBarb(float2 p, float2 a, float2 b, float ra, float rb)
        {
            float2 pa = p - a, ba = b - a;
            float h = saturate(dot(pa, ba) / max(dot(ba, ba), 1e-8));
            return length(pa - ba * h) - lerp(ra, rb, h);
        }
        ENDHLSL

        Pass
        {
            Name "CelGlBoltShade"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend Zero OneMinusSrcColor
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma vertex vertDark
            #pragma fragment fragDark
            Varyings vertDark(Attributes v, uint instanceID : SV_InstanceID) { return GlVert(v, instanceID, 1.0); }
            half4 fragDark(Varyings i) : SV_Target
            {
                // a soft ring of shade round the glow: 0 = leave the dirt, 1 = black
                float rr = length(i.st);
                float c = i.dims.w;
                float x = (rr - c) / max(1.0 - c, 0.05);
                float field = exp(-x * x * 2.5) * (1.0 - smoothstep(0.85, 1.0, rr));
                float inner = smoothstep(c * 0.45, c, rr);
                float d = saturate(field * lerp(0.35, 1.0, inner) * i.dims.x * i.tone.z);
                return half4(d, d, d, 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "CelGlBolt"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            Varyings vert(Attributes v, uint instanceID : SV_InstanceID) { return GlVert(v, instanceID, 0.0); }

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
                    float s = i.st.x, y = abs(i.st.y);
                    float hw = max(i.dims.x, 1e-4), len = max(i.dims.y, 1e-3), hl = max(i.dims.z, 1e-4), bloom = i.dims.w;
                    float seed = i.more.y;
                    // the head: a pointed dart, sharp in front, a blunt back
                    float wHead = s >= 0.0 ? hw * pow(saturate(1.0 - s / (hl * 0.6)), 0.85) : hw * sqrt(saturate(1.0 - (s / (hl * 0.45)) * (s / (hl * 0.45))));
                    float dHead = (s > hl * 0.6 || s < -hl * 0.45) ? 1e3 : y - wHead;
                    // the chevron: swept-back barbs flaring out behind the head, two a side, each its own length
                    float2 q = float2(s, y);
                    float j1 = CelHash11(seed * 37.0 + (i.st.y > 0.0 ? 1.0 : 2.0));
                    float j2 = CelHash11(seed * 53.0 + (i.st.y > 0.0 ? 3.0 : 4.0));
                    float dB1 = GlBarb(q, float2(-hl * 0.05, hw * 0.3), float2(-hl * (0.8 + 0.3 * j1), hw * (1.6 + 0.4 * j1)), hw * 0.45, hw * 0.04);
                    float dB2 = GlBarb(q, float2(-hl * 0.25, hw * 0.25), float2(-hl * (1.15 + 0.3 * j2), hw * (1.0 + 0.3 * j2)), hw * 0.35, hw * 0.03);
                    // the tail: short, ragged and broken into segments, tapering to nothing
                    float u = saturate((-s - hl * 0.2) / max(len - hl * 0.2, 1e-3));
                    float rag = CelValue2(float2(s * 9.0 - _CelNow * 11.0, seed * 91.0 + sign(i.st.y) * 3.1));
                    float rT = hw * 0.75 * pow(1.0 - u, 0.9) * lerp(0.55, 1.35, rag);
                    float seg = frac(-s / max(hl * 0.55, 1e-3) + seed * 7.0);
                    float gap = (u > 0.25 && seg < lerp(0.1, 0.35, u)) ? 1.0 : 0.0;
                    float dTail = (s <= -hl * 0.2 && s >= -len && gap < 0.5) ? y - rT : 1e3;
                    float d = min(min(dHead, dTail), min(dB1, dB2));
                    // inside: banded - white-hot core in the head, pale green barbs, saturated green tail
                    float vHead = saturate(-dHead / hw);
                    float vRest = 0.3 + saturate(-min(min(dB1, dB2), dTail) / hw) * 0.4;
                    float v = max(vHead * 1.0 + (dHead < 0.0 ? 0.25 : 0.0), vRest);
                    half3 body = CelRamp(saturate(v), row, i.tone.y);
                    float inside = CelAAStep(0.0, -d);
                    // outside: a faint smooth bloom round the dart (no ring, no rim at the quad's edge)
                    float x = max(d, 0.0) / max(bloom, 1e-4);
                    float glow = exp(-5.0 * x) * (1.0 - smoothstep(0.5, 1.0, x));
                    col = lerp(CelPal(row, 2) * (glow * 0.35), body, inside) * i.tone.z;
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
