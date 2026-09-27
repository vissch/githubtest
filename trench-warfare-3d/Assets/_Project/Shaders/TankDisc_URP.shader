// Phase: A5b / C4 (implemented) — what a tank presses on the ground, one quad each drawn instanced by TankRenderer: a
// soft dark contact blob under the hull (the tank sits on the mud, not above it) and a thin ring in its side's colour
// round it, faintly lit, so the side reads at the gameplay zoom and at night (critique 2026-09-22). The shape is
// computed: uv 0..1 across the footprint, which the quad's matrix stretches to the hull plus a margin. A second pass
// draws the ring (not the blob) faintly where the ground or the hull hides it, so a trench lip or a slope never swallows
// the side marker (critique round 2). Only the ground: tanks and figures set stencil bit 8 (Tank_URP, VAT_URP) and the
// hidden pass skips it, or a tank's ring painted its side's colour over the infantry beside it and across its own hull
// (asset playground, critic round 8: friendly frogs on an enemy tank's ring read as enemy).
//  _Color  per instance: rgb the side's colour, a the ring's strength (0 on a wreck: only the blob).
//          a >= 2 makes the quad a RIDER PIP instead (TankRenderer.Riders): a dot by the ring, one per seat, a - 2 its
//          fill - 1 a man aboard, between a man climbing on or off, 0 an empty seat (a dark disc ringed in the side's
//          colour); a >= 3.5 draws a TRIANGLE pointing down the screen instead (a man flat under a sweeping barrel; the
//          pip's +z is turned away from the camera). a >= 5 is the dark PLATE behind a machine's row (so rows of walkers
//          side by side can be told apart), a > 6.5 a man thrown clear, a > 7.5 a rider's shot. See PipGlyph.
Shader "TW/TankDisc (URP)"
{
    Properties
    {
        _Blob ("Contact blob strength", Range(0,1)) = 0.5
        [HideInInspector] _Color ("Side colour (per instance)", Vector) = (1,1,1,1)
        [HideInInspector] _Pip ("Rider pip offset in view space (per instance)", Vector) = (0,0,0,0)
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent-45" "IgnoreProjector"="True" }
        Pass
        {
            Name "TankDisc"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            Offset -2, -2
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            CBUFFER_START(UnityPerMaterial)
                float _Blob;
            CBUFFER_END
            UNITY_INSTANCING_BUFFER_START(DiscProps)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Color)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Pip)
            UNITY_INSTANCING_BUFFER_END(DiscProps)
            // a rider pip is built HERE, from the camera drawing it: the instance's matrix holds the row's anchor (the ring's
            // point nearest the lens) and the pip's size, _Pip its offset along the view's right and up and a nudge toward
            // the lens. Laid out on the CPU with Camera.main, the row tilted whenever the camera moved after it (critic r5).
            float3 PipCorner(float2 uv, float4 pip)
            {
                float3 anchor = TransformObjectToWorld(float3(0, 0, 0));
                float wide = length(float3(UNITY_MATRIX_M[0].x, UNITY_MATRIX_M[1].x, UNITY_MATRIX_M[2].x));   // x scale
                float tall = length(float3(UNITY_MATRIX_M[0].z, UNITY_MATRIX_M[1].z, UNITY_MATRIX_M[2].z));   // z scale (the row's plate is wider than tall)
                float3 right = UNITY_MATRIX_V[0].xyz, up = UNITY_MATRIX_V[1].xyz, toward = UNITY_MATRIX_V[2].xyz;
                // turned in the view plane by pip.w (a shot's flash lies along its aim on screen; everything else is 0)
                float2 o = float2((uv.x - 0.5) * wide, (uv.y - 0.5) * tall), cs = float2(cos(pip.w), sin(pip.w));
                o = float2(o.x * cs.x - o.y * cs.y, o.x * cs.y + o.y * cs.x);
                return anchor + right * (pip.x + o.x) + up * (pip.y + o.y) + toward * pip.z;
            }

            // every mark of a rider row, one glyph per state, the same in both passes (seen, and through what hides it):
            //   plate (5 < a < 6.5)  a dark tag with rules above and below in the side's colour, behind the row
            //   thrown (a > 6.5)     a dot with its lower half dark - pitched off alive when his machine died
            //   flat (3.5 < a < 5)   a triangle, apex down - lying flat under a barrel swinging over him
            //   aboard (a = 3)       a solid dot
            //   climbing (2 < a < 3) a dot with a dark bar through it, its brightness the pulse
            //   empty (a = 2)        a faint hollow ring only (a dark disc could not be told from a man climbing: critic r10)
            half4 PipGlyph(float2 p, float4 side, half strength)
            {
                half3 dark = half3(0.02, 0.018, 0.016);
                float2 q = abs(p); float r = length(p);
                if (side.a > 7.5)   // a rider's SHOT: a star streaked along the aim, no rim (round dots read as eyes: critic r19)
                {
                    half streak = (1.0 - smoothstep(0.0, 0.14, q.y)) * (1.0 - smoothstep(0.25, 1.0, q.x));
                    half cross = (1.0 - smoothstep(0.0, 0.12, q.x)) * (1.0 - smoothstep(0.08, 0.5, q.y));
                    half hot = 1.0 - smoothstep(0.08, 0.3, r);
                    half fa = saturate(max(max(streak, cross * 0.8), hot));
                    return half4(side.rgb * (1.0 + hot * 0.4), fa * strength);
                }
                if (side.a > 5.0 && side.a < 6.5)
                {
                    half ends = 1.0 - smoothstep(0.93, 1.0, q.x);
                    half fillA = (1.0 - smoothstep(0.80, 0.86, q.y)) * ends;
                    half rule = smoothstep(0.80, 0.86, q.y) * (1.0 - smoothstep(0.92, 0.99, q.y)) * ends;
                    return half4(lerp(dark, side.rgb, rule), max(fillA * 0.8, rule * 0.75) * strength);   // 0.7 all but vanished on snow (critic r11)
                }
                half rim = 1.0 - smoothstep(0.72, 0.90, r);
                half disc = 1.0 - smoothstep(0.50, 0.60, r);
                if (side.a > 6.5)
                {
                    half3 c = lerp(dark, lerp(side.rgb * 1.1, dark, step(p.y, -0.05)), disc);
                    // outlined, so it keeps a full circle and is not taken for a climbing man's bar at the standard view (critic r11)
                    c = lerp(c, side.rgb * 1.1, smoothstep(0.42, 0.48, r) * (1.0 - smoothstep(0.56, 0.62, r)));
                    return half4(c, max(disc * 0.97, rim * 0.85) * strength);
                }
                if (side.a > 3.5)
                {
                    float bd = max(q.x - (p.y + 0.6) * 0.62, max(-0.6 - p.y, p.y - 0.45));
                    half tri = 1.0 - smoothstep(0.0, 0.06, bd);
                    half trim = 1.0 - smoothstep(0.12, 0.26, bd);
                    return half4(lerp(dark, side.rgb, tri), max(tri * 0.97, trim * 0.85) * strength);
                }
                half fill = side.a - 2.0;
                if (fill < 0.001)
                {
                    half band = smoothstep(0.36, 0.44, r) * (1.0 - smoothstep(0.54, 0.62, r));
                    return half4(side.rgb, band * 0.5 * strength);
                }
                half3 col = lerp(dark, side.rgb * (0.9 + 0.35 * fill), disc);   // brighter washed the cyan out to white
                if (fill < 0.999) col = lerp(col, dark, (1.0 - smoothstep(0.22, 0.28, q.y)) * disc);   // a thick bar: at ~20% of the dot it read as a hairline (r11, r12)
                return half4(col, max(disc * 0.97, rim * 0.85) * strength);
            }
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 positionWS : TEXCOORD1; float fog : TEXCOORD2; float4 side : TEXCOORD3; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.side = UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Color);
                o.positionWS = o.side.a > 1.5 ? PipCorner(v.uv, UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Pip)) : TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.uv = v.uv;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float2 p = i.uv * 2.0 - 1.0;
                if (i.side.a > 1.5)   // a rider row's mark (TankRenderer.Riders)
                {
                    half4 g = PipGlyph(p, i.side, 1.0);
                    half3 gc = ApplyMist(g.rgb, i.positionWS);
                    gc = ApplyFieldFog(gc, i.positionWS);
                    return half4(MixFog(gc, i.fog), g.a);
                }
                // a rounded rectangle, so the blob and the ring follow the hull's long shape
                float2 q = abs(p) - 0.62;
                float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);   // 0 at the rounded edge of the inner box
                half blob = (1.0 - smoothstep(-0.25, 0.16, d)) * _Blob;
                half ring = (smoothstep(0.18, 0.23, d) * (1.0 - smoothstep(0.32, 0.39, d))) * i.side.a;
                half3 color = lerp(half3(0.02, 0.018, 0.016), i.side.rgb * 1.4, ring / max(ring + blob, 1e-3));
                half alpha = max(blob, ring * 0.55);
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                return half4(MixFog(color, i.fog), alpha);
            }
            ENDHLSL
        }
        Pass
        {
            Name "TankDiscHidden"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            ZTest Greater
            Stencil { Ref 8 ReadMask 8 Comp NotEqual }
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            UNITY_INSTANCING_BUFFER_START(DiscProps)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Color)
                UNITY_DEFINE_INSTANCED_PROP(float4, _Pip)
            UNITY_INSTANCING_BUFFER_END(DiscProps)
            // a rider pip is built HERE, from the camera drawing it: the instance's matrix holds the row's anchor (the ring's
            // point nearest the lens) and the pip's size, _Pip its offset along the view's right and up and a nudge toward
            // the lens. Laid out on the CPU with Camera.main, the row tilted whenever the camera moved after it (critic r5).
            float3 PipCorner(float2 uv, float4 pip)
            {
                float3 anchor = TransformObjectToWorld(float3(0, 0, 0));
                float wide = length(float3(UNITY_MATRIX_M[0].x, UNITY_MATRIX_M[1].x, UNITY_MATRIX_M[2].x));   // x scale
                float tall = length(float3(UNITY_MATRIX_M[0].z, UNITY_MATRIX_M[1].z, UNITY_MATRIX_M[2].z));   // z scale (the row's plate is wider than tall)
                float3 right = UNITY_MATRIX_V[0].xyz, up = UNITY_MATRIX_V[1].xyz, toward = UNITY_MATRIX_V[2].xyz;
                // turned in the view plane by pip.w (a shot's flash lies along its aim on screen; everything else is 0)
                float2 o = float2((uv.x - 0.5) * wide, (uv.y - 0.5) * tall), cs = float2(cos(pip.w), sin(pip.w));
                o = float2(o.x * cs.x - o.y * cs.y, o.x * cs.y + o.y * cs.x);
                return anchor + right * (pip.x + o.x) + up * (pip.y + o.y) + toward * pip.z;
            }

            // every mark of a rider row, one glyph per state, the same in both passes (seen, and through what hides it):
            //   plate (5 < a < 6.5)  a dark tag with rules above and below in the side's colour, behind the row
            //   thrown (a > 6.5)     a dot with its lower half dark - pitched off alive when his machine died
            //   flat (3.5 < a < 5)   a triangle, apex down - lying flat under a barrel swinging over him
            //   aboard (a = 3)       a solid dot
            //   climbing (2 < a < 3) a dot with a dark bar through it, its brightness the pulse
            //   empty (a = 2)        a faint hollow ring only (a dark disc could not be told from a man climbing: critic r10)
            half4 PipGlyph(float2 p, float4 side, half strength)
            {
                half3 dark = half3(0.02, 0.018, 0.016);
                float2 q = abs(p); float r = length(p);
                if (side.a > 7.5)   // a rider's SHOT: a star streaked along the aim, no rim (round dots read as eyes: critic r19)
                {
                    half streak = (1.0 - smoothstep(0.0, 0.14, q.y)) * (1.0 - smoothstep(0.25, 1.0, q.x));
                    half cross = (1.0 - smoothstep(0.0, 0.12, q.x)) * (1.0 - smoothstep(0.08, 0.5, q.y));
                    half hot = 1.0 - smoothstep(0.08, 0.3, r);
                    half fa = saturate(max(max(streak, cross * 0.8), hot));
                    return half4(side.rgb * (1.0 + hot * 0.4), fa * strength);
                }
                if (side.a > 5.0 && side.a < 6.5)
                {
                    half ends = 1.0 - smoothstep(0.93, 1.0, q.x);
                    half fillA = (1.0 - smoothstep(0.80, 0.86, q.y)) * ends;
                    half rule = smoothstep(0.80, 0.86, q.y) * (1.0 - smoothstep(0.92, 0.99, q.y)) * ends;
                    return half4(lerp(dark, side.rgb, rule), max(fillA * 0.8, rule * 0.75) * strength);   // 0.7 all but vanished on snow (critic r11)
                }
                half rim = 1.0 - smoothstep(0.72, 0.90, r);
                half disc = 1.0 - smoothstep(0.50, 0.60, r);
                if (side.a > 6.5)
                {
                    half3 c = lerp(dark, lerp(side.rgb * 1.1, dark, step(p.y, -0.05)), disc);
                    // outlined, so it keeps a full circle and is not taken for a climbing man's bar at the standard view (critic r11)
                    c = lerp(c, side.rgb * 1.1, smoothstep(0.42, 0.48, r) * (1.0 - smoothstep(0.56, 0.62, r)));
                    return half4(c, max(disc * 0.97, rim * 0.85) * strength);
                }
                if (side.a > 3.5)
                {
                    float bd = max(q.x - (p.y + 0.6) * 0.62, max(-0.6 - p.y, p.y - 0.45));
                    half tri = 1.0 - smoothstep(0.0, 0.06, bd);
                    half trim = 1.0 - smoothstep(0.12, 0.26, bd);
                    return half4(lerp(dark, side.rgb, tri), max(tri * 0.97, trim * 0.85) * strength);
                }
                half fill = side.a - 2.0;
                if (fill < 0.001)
                {
                    half band = smoothstep(0.36, 0.44, r) * (1.0 - smoothstep(0.54, 0.62, r));
                    return half4(side.rgb, band * 0.5 * strength);
                }
                half3 col = lerp(dark, side.rgb * (0.9 + 0.35 * fill), disc);   // brighter washed the cyan out to white
                if (fill < 0.999) col = lerp(col, dark, (1.0 - smoothstep(0.22, 0.28, q.y)) * disc);   // a thick bar: at ~20% of the dot it read as a hairline (r11, r12)
                return half4(col, max(disc * 0.97, rim * 0.85) * strength);
            }
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float4 side : TEXCOORD1; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.side = UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Color);
                o.positionCS = o.side.a > 1.5 ? TransformWorldToHClip(PipCorner(v.uv, UNITY_ACCESS_INSTANCED_PROP(DiscProps, _Pip))) : TransformObjectToHClip(v.positionOS.xyz);
                o.uv = v.uv;
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                float2 p = i.uv * 2.0 - 1.0;
                // a shot behind the hull is not seen: drawn through it, a man firing behind the Maw's head put his flash on the
                // head (critic t5). The seat row keeps showing through, it is a count to read
                if (i.side.a > 7.5) discard;
                if (i.side.a > 1.5) return PipGlyph(p, i.side, 0.8);   // under a rise or behind a post: nearly as strong - it is a count to read, rules included
                float2 q = abs(p) - 0.62;
                float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);
                half ring = (smoothstep(0.18, 0.23, d) * (1.0 - smoothstep(0.32, 0.39, d))) * i.side.a;
                return half4(i.side.rgb * 1.4, ring * 0.3);
            }
            ENDHLSL
        }
    }
}
