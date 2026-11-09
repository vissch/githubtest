// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - GlBolt: the green laser's own primitive, one quad.
// Four modes, picked per record by extra.x (one kind = one draw for all four; two passes):
//   mode 0  dart (additive): a thin dart - a small pointed pale head, then a long saturated-green tail of a fixed
//           length in metres trailing back along its velocity (never behind its birth), tapering to a point and
//           broken into two or three ragged pieces at the back; a smooth bloom round it, no barbs, no ring.
//   mode 1  wash (additive): a ground quad with a smooth radial falloff that reaches nothing well inside the quad's
//           edge (no bands, no rings, no rim), faded in and out.
//   mode 2  shade (multiply, second pass): a ground quad that darkens the dirt in a soft ring (kept, unused in r04).
//   mode 3  wisp (both passes): an upright S-curling flame wisp facing the eye, tapering to a pointed tip, with a dark
//           ink outline: the multiply pass blacks out the shape and its ink band, the additive pass then lays the
//           flat cel bands on the shape (so it reads opaque). It thins away with its envelope (no alpha fade).
//   size.xyz   mode 0: head half-width (m) at birth, at death, ease;  mode 1/2: radius (m);  mode 3: height (m)
//   tone       x palette, y HDR core, z opacity (mode 2: darkness 0..1), w unused
//   extra      mode 0: x 0, y tail length (m), z bloom (m), w head length / half-width (0: 4)
//              mode 1: x 1, y fade-in (life 0..1), z fade-out start (life 0..1), w falloff (0: 3; higher = tighter)
//              mode 2: x 2, y fade-in, z fade-out start, w ring centre (0..1 of the radius; 0: 0.7)
//              mode 3: x 3, y fade-in, z fade-out start, w curl amplitude (0..0.6 of the half-width; 0: 0.4)
// Colours: mode 0/3 band the body with CelRamp of its palette (core above the row's threshold); the bloom and the
// wash use the palette's c2/c3 times the opacity.
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
            float2 st : TEXCOORD0;                          // mode 0: metres along (0 = head centre, - = tail), across; mode 1/2: -1..1; mode 3: x -1..1, y 0..1
            nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w mode
            nointerpolation float4 dims : TEXCOORD2;        // mode 0: x head half-width, y tail length, z head length, w bloom; mode 1/2/3: x envelope, w falloff / ring / curl
            float4 more : TEXCOORD3;                        // x fog, y seed
            float3 positionWS : TEXCOORD4;
        };

        // darkPass: 1 for the multiply pass (modes 2 and 3 draw), 0 for the additive pass (modes 0, 1 and 3 draw)
        Varyings GlVert(Attributes v, uint instanceID, float darkPass)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float mode = r.extra.x;
            float isWisp = mode > 2.5 ? 1.0 : 0.0;
            float isDark = (mode > 1.5 && mode < 2.5) ? 1.0 : 0.0;
            alive *= (isWisp > 0.5 || isDark == darkPass) ? 1.0 : 0.0;
            float3 p = CelPosition(r, t);
            float size = CelCurve(r.size.xyz, k) * alive;
            Varyings o;
            float3 w;
            float fin = r.extra.y > 0.0 ? smoothstep(0.0, r.extra.y, k) : 1.0;
            float fout = 1.0 - smoothstep(saturate(r.extra.z), 1.0, k);
            if (isWisp > 0.5)
            {
                // upright quad facing the eye: x across (camera right), y up from the base
                float3 right = UNITY_MATRIX_V[0].xyz;
                float x = v.uv.x * 2.0 - 1.0;
                w = p + right * (x * size * 0.5) + float3(0.0, v.uv.y * size, 0.0);
                o.st = float2(x, v.uv.y);
                o.dims = float4(fin * fout * alive, 0.0, 0.0, r.extra.w > 0.0 ? r.extra.w : 0.4);
            }
            else if (mode > 0.5)
            {
                float2 corner = v.uv * 2.0 - 1.0;
                w = p + float3(corner.x, 0.0, corner.y) * size + float3(0.0, 0.3, 0.0);   // lifted clear of the bumps: its edge fades to nothing
                o.st = corner;
                float shapeW = mode > 1.5 ? (r.extra.w > 0.0 ? r.extra.w : 0.7) : (r.extra.w > 0.0 ? r.extra.w : 3.0);
                o.dims = float4(fin * fout * alive, 0.0, 0.0, shapeW);
            }
            else
            {
                float3 vel = CelVelocity(r, t);
                float speed = length(vel);
                float3 dir = speed > 1e-3 ? vel / speed : float3(1, 0, 0);
                // keep every dart about 1/8 of the frame wide on screen and at most ~1/7 as tall as long, however near
                // the eye it flies: the tail is capped by the frame's width at the dart's distance (width = 2 d / P00)
                float dcam = distance(p, _WorldSpaceCameraPos);
                float lenCap = min(r.extra.y, 0.25 * dcam / max(UNITY_MATRIX_P[0][0], 1e-3));
                float hw = min(size, lenCap * 0.075);
                float hl = hw * (r.extra.w > 0.0 ? r.extra.w : 4.0);
                float bloom = min(max(r.extra.z, 0.0), hw * 2.5) * alive;
                float len = min(max(lenCap, hl), distance(p, r.pos) + hl * 0.6);
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float along = lerp(-len - bloom, hl * 0.6 + bloom, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * (hw * 1.3 + bloom);
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

        // the wisp's signed distance in quad-x units (negative inside) and its value across (1 centre, 0 rim)
        float GlWisp(float2 st, float env, float curl, float seed, out float across, out float thickOut)
        {
            float y = st.y;
            float side = CelHash11(seed * 17.0) > 0.5 ? 1.0 : -1.0;
            float phase = CelHash11(seed * 29.0) * 6.2832;
            // S-shaped centre line: still at the base, curling more towards the tip, slowly writhing
            float xc = side * curl * smoothstep(0.0, 0.5, y) * sin(y * 7.5 + phase + _CelNow * 4.0);
            // a rounded base, swelling low, tapering to a sharp tip; thins away with the envelope
            float thick = 0.42 * pow(saturate(1.0 - y), 0.7) * smoothstep(0.0, 0.18, y) * (0.75 + 0.25 * sin(y * 9.0 + phase)) * env;
            float dx = abs(st.x - xc);
            across = saturate(1.0 - dx / max(thick, 1e-4));
            thickOut = thick;
            return dx - thick;
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
                if (i.tone.w > 2.5)
                {
                    // the wisp: black out the shape and an ink band round it (the additive pass colours the inside)
                    float across, thick;
                    float d = GlWisp(i.st, i.dims.x, i.dims.w, i.more.y, across, thick);
                    float px = max(fwidth(d), 1e-5);
                    // the ink band thins with the shape, so a wisp thinning away leaves no bare black line
                    float ink = CelLodInk(3.5) * saturate(thick / max(px * 4.0, 1e-5));
                    float a = (1.0 - smoothstep(ink - 0.5, ink + 0.5, d / px)) * (i.dims.x > 0.02 ? 1.0 : 0.0);
                    return half4(a, a, a, 1.0);
                }
                // a soft ring of shade round the glow: 0 = leave the dirt, 1 = black
                float rr = length(i.st);
                float c = i.dims.w;
                float x = (rr - c) / max(1.0 - c, 0.05);
                float field = exp(-x * x * 2.5) * (1.0 - smoothstep(0.85, 1.0, rr));
                float inner = smoothstep(c * 0.45, c, rr);
                float d2 = saturate(field * lerp(0.35, 1.0, inner) * i.dims.x * i.tone.z);
                return half4(d2, d2, d2, 1.0);
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
                if (i.tone.w > 2.5)
                {
                    // the wisp's flat bands: pale core down the middle, green toward the rim (ink is the black left by the shade pass)
                    float across, thick;
                    float d = GlWisp(i.st, i.dims.x, i.dims.w, i.more.y, across, thick);
                    float inside = CelAAStep(0.0, -d) * (i.dims.x > 0.02 ? 1.0 : 0.0);
                    float v = 0.5 + 0.5 * across * lerp(1.0, 0.8, i.st.y);
                    col = CelRamp(saturate(v), row, i.tone.y) * (inside * i.tone.z);
                }
                else if (i.tone.w > 0.5)
                {
                    // the wash: a smooth gaussian that is already nothing at the radius: no band, no ring, no rim
                    float rr = length(i.st);
                    float field = exp(-rr * rr * i.dims.w) * (1.0 - smoothstep(0.55, 1.0, rr));
                    col = CelPal(row, 3) * (field * i.dims.x * i.tone.z);
                }
                else
                {
                    float s = i.st.x, y = abs(i.st.y);
                    float hw = max(i.dims.x, 1e-4), len = max(i.dims.y, 1e-3), hl = max(i.dims.z, 1e-4), bloom = i.dims.w;
                    float seed = i.more.y;
                    // the head: small and pointed in front (a long sharp tip), a short rounded back that runs into the tail
                    float hh = hw * 0.7;
                    float wHead = s >= 0.0 ? hh * pow(saturate(1.0 - s / (hl * 0.9)), 1.3) : hh * sqrt(saturate(1.0 - (s / (hl * 0.35)) * (s / (hl * 0.35))));
                    float dHead = (s > hl * 0.9 || s < -hl * 0.35) ? 1e3 : y - wHead;
                    // the tail: long, tapering to a point, ragged along its edge, broken into pieces only in its back half
                    float u = saturate((-s - hl * 0.1) / max(len - hl * 0.1, 1e-3));
                    float rag = CelValue2(float2(s * 7.0 / max(len, 0.2) * 1.5 - _CelNow * 9.0, seed * 91.0 + sign(i.st.y) * 3.1));
                    float rT = hw * 0.85 * pow(1.0 - u, 1.4) * lerp(0.6, 1.2, rag);
                    float j1 = CelHash11(seed * 37.0), j2 = CelHash11(seed * 53.0);
                    float gap1 = (u > 0.55 + 0.08 * j1 && u < 0.63 + 0.08 * j1) ? 1.0 : 0.0;
                    float gap2 = (j2 > 0.3 && u > 0.78 + 0.05 * j2 && u < 0.85 + 0.05 * j2) ? 1.0 : 0.0;
                    float dTailC = (s <= 0.0 && s >= -len) ? y - rT : 1e3;
                    float dTail = gap1 + gap2 < 0.5 ? dTailC : 1e3;
                    float d = min(dHead, dTail);
                    // inside: pale white-green core in the head, saturated green tail, paler down its middle
                    // only the head's middle reaches the white core (the palette's core band); its rim is pale green
                    float vHead = dHead < 0.0 ? 0.68 + 0.35 * saturate(-dHead / hh) : 0.0;
                    float vTail = dTail < 0.0 ? 0.42 + 0.4 * saturate(-dTail / max(rT, 1e-4)) * (1.0 - 0.5 * u) : 0.0;
                    float v = max(vHead, vTail);
                    half3 body = CelRamp(saturate(v), row, i.tone.y);
                    float inside = CelAAStep(0.0, -d);
                    // outside: a smooth bloom round the dart, strongest round the head (no ring, no rim at the quad's edge)
                    // the bloom follows the unbroken tail, so the gaps never cut dark rungs across it
                    float x = max(min(dHead, dTailC), 0.0) / max(bloom, 1e-4);
                    float headNear = saturate(1.0 - (-s) / max(len * 0.6, 1e-3));
                    float glow = exp(-3.5 * x) * (1.0 - smoothstep(0.6, 1.0, x)) * lerp(0.6, 1.0, headNear);
                    col = lerp(CelPal(row, 2) * (glow * 0.85), body, inside) * i.tone.z;
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
    }
}
