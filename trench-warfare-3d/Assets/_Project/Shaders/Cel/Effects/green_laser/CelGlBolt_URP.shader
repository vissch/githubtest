// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - GlBolt: the green laser's own primitive, one quad.
// Six modes, picked per record by extra.x (one kind = one draw for all six; two passes):
//   mode 0  dart (additive): a pointed white-hot lozenge head, then one smooth unbroken tail of a fixed length in
//           metres trailing back along its velocity, hot down its middle, green at its rim, tapering to nothing; a soft
//           green bloom round the whole spear. Capped on screen to ~1/6 of the frame.
//   mode 1  wash (additive): a ground quad: a pale gaussian core plus a wide dim deep-green spill that fades to nothing
//           at the quad's edge (no bands, no rings, no rim), faded in and out.
//   mode 2  shade (multiply): a ground quad that darkens the dirt in a smooth disc (w small) or a soft ring.
//   mode 3  wisp (both passes): an upright S-curling flame wisp facing the eye, tapering to a pointed tip, with a dark
//           ink outline; translucent: the multiply pass darkens the inside by its opacity and blacks the ink band,
//           the additive pass lays the flat cel bands times the opacity.
//   mode 4  puff (both passes): a translucent lumpy cel smoke puff facing the eye with a dark ink outline and one inner
//           curl stroke, lit from above in flat bands, eroding into holes as it fades.
//   mode 5  star (both passes): an inked spiky starburst facing the eye: a small white core (about 1/4 of the burst),
//           pale hub, green spikes of uneven length, dark ink round every spike.
//   size.xyz   mode 0: head half-width (m) at birth, at death, ease;  mode 1/2: radius (m);  mode 3: height (m);
//              mode 4/5: radius (m)
//   tone       x palette, y HDR core, z opacity (mode 2: darkness 0..1; modes 3-5: opacity 0..1 of the inside)
//   extra      mode 0: x 0, y tail length (m), z bloom (m), w head length / half-width (0: 4)
//              mode 1: x 1, y fade-in (life 0..1), z fade-out start (life 0..1), w core falloff (0: 3; higher = tighter)
//              mode 2: x 2, y fade-in, z fade-out start, w ring centre (0..1 of the radius; 0: 0.7; 0.01 = a disc)
//              mode 3: x 3, y fade-in, z fade-out start, w curl amplitude (0..0.6 of the half-width; 0: 0.4)
//              mode 4: x 4, y fade-in, z fade-out start, w lump amplitude (0: 0.1)
//              mode 5: x 5, y fade-in, z fade-out start, w spike count (0: 9)
//              mode 7: x 7, y fade-in, z fade-out start: a hot boiling splat (additive, no ink) (d09/d11)
//              mode 6: x 6, y fade-in, z fade-out start, w height / width (0: 0.5): a soft unbanded upright mist (additive)
// Colours: modes 0/3/4/5 band the body with CelRamp of its palette (core above the row's threshold); the bloom and the
// wash use the palette's c1..c3 times the opacity.
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
            float2 st : TEXCOORD0;                          // mode 0: metres along (0 = head centre, - = tail), across; mode 1/2/4/5: -1..1; mode 3: x -1..1, y 0..1
            nointerpolation float4 tone : TEXCOORD1;        // x palette, y hdr, z opacity, w mode
            nointerpolation float4 dims : TEXCOORD2;        // mode 0: x head half-width, y tail length, z head length, w bloom; else: x envelope, w falloff / ring / curl / lump / spikes
            float4 more : TEXCOORD3;                        // x fog, y seed
            float3 positionWS : TEXCOORD4;
        };

        // darkPass: 1 for the multiply pass (modes 2-5 draw), 0 for the additive pass (modes 0, 1, 3, 4, 5 draw)
        Varyings GlVert(Attributes v, uint instanceID, float darkPass)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float mode = r.extra.x;
            float isFace = mode > 2.5 ? 1.0 : 0.0;
            float isDark = (mode > 1.5 && mode < 2.5) ? 1.0 : 0.0;
            // d07: puffs (mode 4) draw only in the third, alpha-blended pass (darkPass 2): in the two shared passes every
            // puff darkened first and then every puff added its colour, so overlapping puffs summed to a self-lit yellow
            float isPuff = (mode > 3.5 && mode < 4.5) ? 1.0 : 0.0;
            if (darkPass > 1.5) alive *= isPuff;
            else alive *= (isPuff < 0.5 && (isFace > 0.5 || isDark == darkPass)) ? 1.0 : 0.0;
            float3 p = CelPosition(r, t);
            float size = CelCurve(r.size.xyz, k) * alive;
            // d14: the soft lights (wash, mist, splat) grow going out so the hit still reads as a green flash at Mid/Far
            // (x1 up to tier 1.2, rising to x2.8 at Far; the beach close-up sits near tier 1); counts and detail do not change
            float isLight = (abs(mode - 1.0) < 0.5 || mode > 5.5) ? 1.0 : 0.0;
            float farT = saturate((_CelLod.x - 1.2) / 0.8) * isLight;
            size *= 1.0 + 1.2 * farT;   // d15: capped so the O120 footprint stays <= T1   // far only: the close film runs near tier 1
            Varyings o;
            float3 w;
            float fin = r.extra.y > 0.0 ? smoothstep(0.0, r.extra.y, k) : 1.0;
            float fout = 1.0 - smoothstep(saturate(r.extra.z), 1.0, k);
            if (isFace > 0.5)
            {
                float3 right = UNITY_MATRIX_V[0].xyz;
                float x = v.uv.x * 2.0 - 1.0;
                if (mode < 3.5)
                {
                    // the wisp: upright quad facing the eye, x across (camera right), y up from the base
                    w = p + right * (x * size * 0.5) + float3(0.0, v.uv.y * size, 0.0);
                    o.st = float2(x, v.uv.y);
                    o.dims = float4(fin * fout * alive, 0.0, 0.0, r.extra.w > 0.0 ? r.extra.w : 0.4);
                }
                else
                {
                    // puff / star: a centred quad facing the eye
                    float y = v.uv.y * 2.0 - 1.0;
                    // mode 6 mist: squashed to extra.w of its width (0: 0.5), upright in the world
                    float sq = (mode > 5.5 && mode < 6.5) ? (r.extra.w > 0.0 ? r.extra.w : 0.5) : 1.0;
                    float3 upv = (mode > 5.5 && mode < 6.5) ? float3(0.0, 1.0, 0.0) : UNITY_MATRIX_V[1].xyz;
                    w = p + right * (x * size) + upv * (y * size * sq);
                    o.st = float2(x, y);
                    float def = mode < 4.5 ? 0.1 : 9.0;
                    o.dims = float4(fin * fout * alive, fin, 0.0, r.extra.w > 0.0 ? r.extra.w : def);
                }
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
                // keep every dart about 1/10 of the frame wide on screen however near the eye it flies: the tail is
                // capped by the frame's width at the dart's distance (width = 2 d / P00)
                float dcam = distance(p, _WorldSpaceCameraPos);
                // the cap is on the length seen on screen: a dart flying partly away from the eye may be longer in the world
                float3 eyeDir = (_WorldSpaceCameraPos - p) / max(dcam, 1e-3);
                float seen = max(length(cross(dir, eyeDir)), 0.35);
                float lenCap = min(r.extra.y, 0.3 * dcam / max(UNITY_MATRIX_P[0][0], 1e-3) / seen);
                lenCap *= lerp(0.7, 1.3, CelHash11(r.seed * 3.1 + 0.7));   // d04: each dart's tail +-30%
                float hw = min(size, lenCap * 0.06);
                float hl = hw * (r.extra.w > 0.0 ? r.extra.w : 4.0);
                float bloom = min(max(r.extra.z, 0.0), hw * 2.0) * alive;
                // the whole spear from its first frame (darts are born at or past the frame's edge)
                float len = max(lenCap, hl);
                float3 toEye = normalize(_WorldSpaceCameraPos - p);
                float3 side = cross(dir, toEye);
                float sl = dot(side, side);
                side = sl > 1e-6 ? side * rsqrt(sl) : UNITY_MATRIX_V[0].xyz;
                float along = lerp(-len - bloom, hl + bloom, v.uv.x);
                float across = (v.uv.y * 2.0 - 1.0) * (hw * 1.9 + bloom);   // d04: room for the flicks
                w = p + dir * along + side * across;
                o.st = float2(along, across);
                o.dims = float4(hw, len, hl, bloom);
            }
            o.positionWS = w;
            o.positionCS = TransformWorldToHClip(w);
            // a record that does not draw in this pass is put outside the clip volume (no stray pixels)
            if (alive <= 0.0 || size <= 0.0) o.positionCS = float4(2.0, 2.0, 2.0, 1.0);
            o.tone = float4(r.tone.x, r.tone.y, r.tone.z * (1.0 + 0.6 * farT), mode);   // d14: brighter at Far too
            o.more = float4(ComputeFogFactor(o.positionCS.z), r.seed, 0, 0);
            // d06: the mist never fails the depth test (the ground cut it with a hard horizontal line); it is additive,
            // so drawn at the near plane it just lights whatever is in front, like the ref's glow on the dirt
            if (mode > 5.5 && mode < 6.5 && alive > 0.0 && size > 0.0) o.positionCS.z = UNITY_NEAR_CLIP_VALUE * o.positionCS.w * 0.999;
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

        // the puff's signed distance (quad units, negative inside), its band value, and the inner curl stroke's distance
        float GlPuff(float2 st, float env, float grow, float lump, float seed, out float v, out float curlD)
        {
            float r = length(st);
            float a = atan2(st.y, st.x);
            float ph = CelHash11(seed * 13.0) * 6.2832;
            float rr = 0.7 + lump * (sin(a * 3.0 + ph + _CelNow * 0.7) + 0.6 * sin(a * 5.0 - ph * 1.7 - _CelNow * 0.9) + 0.35 * sin(a * 8.0 + ph * 2.3));
            rr *= lerp(0.55, 1.0, grow);
            float d = r - rr;
            // erode into holes as it fades (things vanish in steps, never by alpha)
            float n = CelValue2(st * 2.4 + float2(seed * 37.0, seed * 11.0 + _CelNow * 0.3));
            d = max(d, (1.0 - env) * 1.25 - n);
            // lit from above: paler on top
            v = saturate(0.35 + 0.55 * saturate(st.y * 0.6 + 0.45) + 0.15 * (1.0 - r));
            // one curl stroke inside: an arc round a point below the centre, on the upper side only
            float2 c0 = float2((CelHash11(seed * 7.0) - 0.5) * 0.4, -0.15);
            float2 q = st - c0;
            float arc = abs(length(q) - 0.32 * lerp(0.6, 1.0, grow));
            curlD = (q.y > 0.02 && q.x * (CelHash11(seed * 3.0) > 0.5 ? 1.0 : -1.0) > -0.12) ? arc : 1e3;
            return d;
        }

        // the star's signed distance (quad units, negative inside) and its band value
        float GlStar(float2 st, float env, float grow, float n, float seed, out float v)
        {
            float r = length(st);
            float a = atan2(st.y, st.x) + CelHash11(seed * 5.0) * 6.2832 + _CelNow * 0.6;
            float sector = a / 6.2832 * n;
            float idx = floor(sector);
            float f = frac(sector) - 0.5;
            float tip = lerp(0.55, 1.0, CelHash11(idx * 7.13 + seed * 31.0));
            float hub = 0.3;
            float g = lerp(0.35, 1.0, grow);
            // the spikes thin away and the hub shrinks as it fades
            float prof = pow(saturate(1.0 - abs(f) * 2.0), lerp(4.5, 2.2, env));
            float rr = (hub * env + (tip - hub * env) * prof) * g;
            float d = r - rr;
            // white core only in the inner quarter; pale hub; green spikes
            v = r < 0.23 * g ? lerp(1.0, 0.95, r / (0.23 * g)) : lerp(0.9, 0.2, saturate((r / g - 0.23) / 0.77));
            return d;
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
                float mode = i.tone.w;
                float op = saturate(i.tone.z);
                if (mode > 5.5) return half4(0.0, 0.0, 0.0, 1.0);   // mist: additive only
                if (mode > 2.5)
                {
                    float d, inkPx, inner = 0.0, extraInk = 0.0;
                    float px;
                    if (mode < 3.5)
                    {
                        float across, thick;
                        d = GlWisp(i.st, i.dims.x, i.dims.w, i.more.y, across, thick);
                        px = max(fwidth(d), 1e-5);
                        // the ink band thins with the shape, so a wisp thinning away leaves no bare black line
                        inkPx = CelLodInk(3.0) * saturate(thick / max(px * 4.0, 1e-5));
                    }
                    else if (mode < 4.5)
                    {
                        float v, curlD;
                        d = GlPuff(i.st, i.dims.x, i.dims.y, i.dims.w, i.more.y, v, curlD);
                        px = max(fwidth(d), 1e-5);
                        inkPx = CelLodInk(2.5);
                        float cpx = max(fwidth(curlD < 100.0 ? curlD : 0.0), 1e-5);
                        extraInk = (d < 0.0 && curlD < 100.0) ? 1.0 - smoothstep(CelLodInk(1.2) - 0.5, CelLodInk(1.2) + 0.5, curlD / cpx) : 0.0;
                    }
                    else
                    {
                        float v;
                        d = GlStar(i.st, i.dims.x, i.dims.y, i.dims.w, i.more.y, v);
                        px = max(fwidth(d), 1e-5);
                        inkPx = CelLodInk(2.5);
                    }
                    float on = i.dims.x > 0.02 ? 1.0 : 0.0;
                    float inside = 1.0 - smoothstep(-0.5, 0.5, d / px);
                    float band = (1.0 - smoothstep(inkPx - 0.5, inkPx + 0.5, d / px)) - inside;
                    // ink black, the inside darkened by the opacity (the additive pass then adds colour x opacity)
                    float a = saturate(max(band, extraInk) + inside * op) * on;
                    return half4(a, a, a, 1.0);
                }
                // shade: a smooth darkening disc (centre near 0) or a soft ring: 0 = leave the dirt, 1 = black
                float rr = length(i.st);
                float c = i.dims.w;
                float x = (rr - c) / max(1.0 - c, 0.05);
                float field = exp(-x * x * 2.5) * (1.0 - smoothstep(0.75, 1.0, rr));
                float inner2 = smoothstep(c * 0.45, c, rr);
                float d2 = saturate(field * lerp(0.35, 1.0, inner2) * i.dims.x * i.tone.z);
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
                float mode = i.tone.w;
                if (mode > 6.5)
                {
                    // mode 7 splat (d09, ref zoom hit_ref_seq.jpg): the hit is a hot pale lumpy blob that re-shapes every
                    // frame (no star geometry, no ink): radius broken by angular noise boiling fast, white-hot middle,
                    // pale yellow-green rim
                    float2 st = i.st;
                    float rr = length(st);
                    float ang = atan2(st.y, st.x);
                    float tq = floor(_CelNow * 24.0);
                    float n1 = CelValue2(float2(ang * 3.2 + i.more.y * 17.0, tq * 0.37 + i.more.y * 5.0));
                    float n2 = CelValue2(float2(ang * 9.5 - i.more.y * 9.0, tq * 0.71));
                    float g = lerp(0.45, 1.0, i.dims.y);
                    float edge = (0.3 + 0.4 * n1 + 0.3 * n2 * n2) * g * lerp(0.6, 1.0, i.dims.x);
                    float d = rr - edge;
                    float inside = CelAAStep(0.0, -d);
                    float v = lerp(0.98, 0.6, saturate(rr / max(edge, 1e-3)));
                    col = CelRamp(saturate(v), row, i.tone.y) * (inside * saturate(i.tone.z) * step(0.02, i.dims.x));
                    return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
                }
                if (mode > 5.5)
                {
                    // mode 6 mist (d03): a smooth unbanded pale haze standing round the target, its edge broken by slow
                    // drifting noise, fading out towards the bottom so the ground never cuts it with a hard line
                    float2 st = i.st;
                    float rr = length(st);
                    float n = CelValue2(st * 1.8 + float2(i.more.y * 13.0 + _CelNow * 0.25, i.more.y * 7.0 - _CelNow * 0.15));
                    float n2 = CelValue2(st * 4.1 + float2(i.more.y * 5.0 - _CelNow * 0.4, _CelNow * 0.3));
                    float field = exp(-rr * rr * (i.dims.w > 0.0 ? 2.6 : 2.6)) * (0.55 + 0.3 * n + 0.15 * n2);
                    field *= 1.0 - smoothstep(0.55, 1.0, rr);
                    field *= lerp(0.35, 1.0, smoothstep(0.05, 0.4, rr));   // d06: dimmer over the target so it stays readable
                    field *= smoothstep(-0.95, -0.35, st.y);
                    float core = exp(-rr * rr * 7.0);
                    col = (CelPal(row, 3) * (field * 0.8) + CelPal(row, 1) * field * 0.5) * (i.dims.x * i.tone.z);
                    return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
                }
                if (mode > 2.5)
                {
                    float d, v;
                    float on = i.dims.x > 0.02 ? 1.0 : 0.0;
                    if (mode < 3.5)
                    {
                        float across, thick;
                        d = GlWisp(i.st, i.dims.x, i.dims.w, i.more.y, across, thick);
                        v = 0.5 + 0.5 * across * lerp(1.0, 0.8, i.st.y);
                    }
                    else if (mode < 4.5)
                    {
                        float curlD;
                        d = GlPuff(i.st, i.dims.x, i.dims.y, i.dims.w, i.more.y, v, curlD);
                    }
                    else
                    {
                        d = GlStar(i.st, i.dims.x, i.dims.y, i.dims.w, i.more.y, v);
                    }
                    float inside = CelAAStep(0.0, -d) * on;
                    col = CelRamp(saturate(v), row, i.tone.y) * (inside * saturate(i.tone.z));
                }
                else if (mode > 0.5)
                {
                    // the wash: a pale gaussian core plus a wide dim deep-green spill, both nothing at the quad's edge
                    float rr = length(i.st);
                    float edge = 1.0 - smoothstep(0.6, 1.0, rr);
                    float core = exp(-rr * rr * i.dims.w);
                    float spill = exp(-rr * rr * 2.2);
                    col = (CelPal(row, 3) * core + CelPal(row, 1) * spill) * (edge * i.dims.x * i.tone.z);
                }
                else
                {
                    float s = i.st.x, y = abs(i.st.y);
                    float hw = max(i.dims.x, 1e-4), len = max(i.dims.y, 1e-3), hl = max(i.dims.z, 1e-4), bloom = i.dims.w;
                    float seed = i.more.y;
                    float sy = i.st.y;
                    // d04 (ref zoom work/zoom/bolts_ref.jpg): a small pale OVAL head, a collar of swept-back flame flicks
                    // behind it (like fletching), then a thin pale tail that wobbles and tapers to nothing
                    float hx = s / (hl * 0.5);
                    float dHead = (sqrt(hx * hx + (sy / (hw * 0.8)) * (sy / (hw * 0.8))) - 1.0) * hw * 0.8;
                    // flicks: four slanted tapered blades from just behind the head, two each side, uneven, flickering
                    float dFl = 1e3;
                    [unroll] for (int k = 0; k < 4; k++)
                    {
                        float side = (k & 1) == 0 ? 1.0 : -1.0;
                        float hk = CelHash11(seed * 3.7 + k * 1.31);
                        float L = hl * lerp(0.6, 1.3, hk) * (0.85 + 0.15 * sin(_CelNow * 23.0 + k * 2.1 + seed * 9.0));
                        float2 a0 = float2(-hl * (0.25 + 0.35 * (k >> 1)), side * hw * 0.3);
                        float2 a1 = a0 + float2(-L, side * hw * lerp(0.7, 1.5, frac(hk * 7.3)));
                        float2 pa = float2(s, sy) - a0, ba = a1 - a0;
                        float h = saturate(dot(pa, ba) / dot(ba, ba));
                        float th = hw * 0.38 * (1.0 - h);
                        dFl = min(dFl, length(pa - ba * h) - th);
                    }
                    // the tail: thin, wobbling slightly, tapering to nothing
                    float u = saturate((-s - hl * 0.2) / max(len - hl * 0.2, 1e-3));
                    float wob = hw * 0.25 * sin(-s * 6.0 / max(hl, 1e-3) * 0.35 + _CelNow * 18.0 + seed * 11.0) * u;
                    float rT = hw * 0.42 * pow(1.0 - u, 1.1);
                    float dTail = (s <= 0.0 && s >= -len) ? abs(sy - wob) - rT : 1e3;
                    float d = min(min(dHead, dFl), dTail);
                    // pale everywhere (the ref bolt is pale yellow-green, white-hot at the head), green only at the rim
                    float vHead = dHead < 0.0 ? 0.86 + 0.14 * saturate(-dHead / hw) : 0.0;
                    float vFl = dFl < 0.0 ? 0.72 + 0.2 * saturate(-dFl / (hw * 0.3)) : 0.0;
                    float vTail = dTail < 0.0 ? 0.62 + 0.3 * saturate(-dTail / max(rT, 1e-4)) * pow(1.0 - u, 0.8) : 0.0;
                    vHead = max(vHead, vFl);
                    float v = max(vHead, vTail);
                    half3 body = CelRamp(saturate(v), row, i.tone.y);
                    float inside = CelAAStep(0.0, -d);
                    // outside: a soft green bloom round the whole spear, strongest at the head, thinning down the tail
                    float x = max(d, 0.0) / max(bloom * lerp(1.0, 0.45, u), 1e-4);
                    float glow = bloom > 0.0 ? exp(-2.5 * x) * (1.0 - smoothstep(0.6, 1.0, x)) * lerp(1.0, 0.35, u) : 0.0;
                    col = lerp(CelPal(row, 2) * (glow * 0.7), body, inside) * i.tone.z;
                }
                return half4(CelAdditiveFog(col, i.positionWS, i.more.x), 1.0);
            }
            ENDHLSL
        }
        Pass
        {
            // d07: smoke puffs, ordinary alpha blending (each puff covers the ones drawn before it, no summing)
            Name "CelGlBoltPuff"
            Tags { "LightMode"="UniversalForwardOnly" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off ZTest LEqual Cull Off
            HLSLPROGRAM
            #pragma vertex vertPuff
            #pragma fragment fragPuff
            Varyings vertPuff(Attributes v, uint instanceID : SV_InstanceID) { return GlVert(v, instanceID, 2.0); }
            half4 fragPuff(Varyings i) : SV_Target
            {
                uint row = CelRowA(i.tone.x);
                float op = saturate(i.tone.z);
                float on = i.dims.x > 0.02 ? 1.0 : 0.0;
                float v, curlD;
                float d = GlPuff(i.st, i.dims.x, i.dims.y, i.dims.w, i.more.y, v, curlD);
                float px = max(fwidth(d), 1e-5);
                float inkPx = CelLodInk(2.5);
                float inside = 1.0 - smoothstep(-0.5, 0.5, d / px);
                float band = (1.0 - smoothstep(inkPx - 0.5, inkPx + 0.5, d / px)) - inside;
                float cpx = max(fwidth(curlD < 100.0 ? curlD : 0.0), 1e-5);
                float curl = (d < 0.0 && curlD < 100.0) ? 1.0 - smoothstep(CelLodInk(1.2) - 0.5, CelLodInk(1.2) + 0.5, curlD / cpx) : 0.0;
                float ia = saturate(max(band, curl));
                float fa = inside * op;
                float a = saturate(ia + fa * (1.0 - ia)) * on;
                half3 body = CelRamp(saturate(v), row, i.tone.y);
                half3 ink = CelPal(row, 0) * 0.25;
                half3 c = lerp(body, ink, ia);
                return half4(c, a);
            }
            ENDHLSL
        }
    }
}
