// Phase: CelFx flamethrower reference-first rebuild (2026-11-18) - the fire sheet. The jet, the catch, the fire at the
// target and the burn-out loops are drawn from per-band distance fields cut from the reference clip, one cell a film
// frame (FlameSheetData.hlsl: the layout and the registration; made by celfx/agents/flamethrower/refatlas_20261118).
// One card on the plane through the nozzle and the aim point, turned about the jet's axis to face the eye. Plane
// coordinates (metres along the axis, metres across) map to the sheet by a fixed homography, so at the beach Range's
// close shot every texel lands on the screen pixel it was cut from; from elsewhere it is an axial flame card.
// Bands are flat colours, no ink, no fog; their edges are as soft as the clip's (measured 20-80 % widths: yellow to
// orange 2.7 px, white to yellow 3.8 px, the silhouette 5 px with a glow outside): that softness is where the clip's
// golden step comes from. Transparent so the scene's depth ink never outlines it.
// Two passes: over the effect's own smoke (stencil bit 4) the fire is always in front, as in the clip, where the sheet
// already has holes for the smoke that stands before the fire; elsewhere its own depth test lets it wrap the target
// (the card passes through the board) while props well in front of it still hide it.
//   pos        the nozzle                       axis       the aim point (world)
//   born/life  the shot; cell = round(age x extra.x) - 1, held (a film never shows a blend)
//   extra      x cells per second (24); y, z the lens the shot is filmed through (URP vignette intensity x 3 and
//              smoothness x 5; 0 = none): the band colours are written past it so they show the same all along the jet
// _SheetA, _SheetA2  R silhouette, G orange or hotter, B yellow or hotter, A white (cells in order, A first)
// _SheetB  R dark red (its own smaller cells)
Shader "TW/Cel/FlameSheet"
{
    Properties
    {
        [NoScaleOffset] _SheetA ("Fire fields: silhouette, orange+, yellow+, white", 2D) = "black" {}
        [NoScaleOffset] _SheetA2 ("Fire fields, later cells", 2D) = "black" {}
        [NoScaleOffset] _SheetB ("Fire field: dark red", 2D) = "black" {}
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+5" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"
        #include "Assets/_Project/Shaders/Cel/Effects/flamethrower/FlameSheetData.hlsl"
        TEXTURE2D(_SheetA);
        SAMPLER(sampler_SheetA);
        TEXTURE2D(_SheetA2);
        TEXTURE2D(_SheetB);
        SAMPLER(sampler_SheetB);

        // What to WRITE (before the lens) so the beach night grade SHOWS the clip's bands. n18_01 wrote the clip's own
        // values and showed orange G +48, white B -19, red G +30; n18_02 corrected one step at the mid-jet's lens level
        // (0.87); n18_03 writes past the lens, so these are that level's values:
        //   shows about      white 252,249,235   yellow 254,253,121   orange 246,128,41   red 252,86,25   dark red 130,38,12
        // n18_07: white warmer and brighter (n18_03 showed 238,234,233: paper grey beside the clip's 250,250,240).
        static const half3 FS_WHITE = half3(190.0, 208.0, 224.0) / 255.0;
        static const half3 FS_YELLOW = half3(212.0, 211.0, 99.0) / 255.0;
        static const half3 FS_ORANGE = half3(202.0, 76.0, 29.0) / 255.0;
        static const half3 FS_RED = half3(222.0, 50.0, 18.0) / 255.0;
        static const half3 FS_DARKRED = half3(81.0, 18.0, 5.0) / 255.0;
        static const half3 FS_GLOW = half3(150.0, 44.0, 12.0) / 255.0;      // the burnt-orange fringe outside the silhouette
        // full widths (plate px) of the edges: silhouette, orange to red, yellow to orange, white to yellow
        #define FS_EDGE float4(3.0, 3.0, 4.5, 6.3)

        /// What the lens keeps at a screen position: URP's vignette with the stage's tint (Atmosphere.cs).
        half3 LensKeep(float2 suv, float2 lens)
        {
            float2 d = abs(suv - 0.5) * lens.x;
            float vf = pow(saturate(1.0 - dot(d, d)), max(lens.y, 1e-3));
            return lerp(half3(0.10, 0.08, 0.06), half3(1.0, 1.0, 1.0), vf);
        }

        struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
        struct Varyings
        {
            float4 positionCS : SV_POSITION;
            float2 sa : TEXCOORD0;                       // plane coordinates: metres along the axis, metres across
            float4 screen : TEXCOORD1;
            float3 world : TEXCOORD2;
            nointerpolation float cell : TEXCOORD3;      // the cell to show, -1 none
            nointerpolation float2 lens : TEXCOORD4;     // vignette intensity x 3, smoothness x 5 (0: no lens)
        };

        Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
        {
            CelRecord r = CelFetch(instanceID);
            Varyings o;
            float alive = CelAlive(r);
            float3 ax = r.axis - r.pos;
            ax = dot(ax, ax) > 1e-8 ? normalize(ax) : float3(0.0, 0.0, 1.0);
            float3 ac = cross(ax, _WorldSpaceCameraPos - r.pos);
            ac = dot(ac, ac) > 1e-10 ? normalize(ac) : float3(0.0, 1.0, 0.0);
            if (ac.y < 0.0) ac = -ac;
            float2 sa = float2(lerp(FS_CARD.x, FS_CARD.y, v.uv.x), lerp(FS_CARD.z, FS_CARD.w, v.uv.y));
            float3 p = r.pos + ax * sa.x + ac * sa.y;
            float frame = floor(max(CelAge(r), 0.0) * r.extra.x + 0.5) - (FS_FIRST_FRAME - FS_FIRE_FRAME);
            bool shown = alive > 0.5 && frame >= 0.0 && frame < FS_CELLS;
            o.positionCS = TransformWorldToHClip(p) * (shown ? 1.0 : 0.0);
            o.sa = sa;
            o.screen = ComputeScreenPos(o.positionCS);
            o.world = p;
            o.cell = shown ? frame : -1.0;
            o.lens = r.extra.yz;
            return o;
        }

        /// The card's premultiplied colour and cover at this fragment (before any depth rule).
        half4 Fire(Varyings i)
        {
            clip(i.cell);
            float3 h = float3(i.sa, 1.0);
            float2 px = float2(dot(FS_H0, h), dot(FS_H1, h)) / dot(FS_H2, h);     // plate px of the reference shot
            float2 c = (px - FS_BOX.xy) * FS_BOX.zw;                              // cell px, y down
            clip(min(min(c.x - 0.5, c.y - 0.5), min(FS_CELL.x - 0.5 - c.x, FS_CELL.y - 0.5 - c.y)));
            bool second = i.cell >= FS_PER_ATLAS;
            float k = second ? i.cell - FS_PER_ATLAS : i.cell;
            float2 cr = float2(fmod(k, FS_COLS), floor(k / FS_COLS));
            float2 uvA = (cr * FS_CELL + c) / FS_ATLAS;
            float2 crB = float2(fmod(i.cell, FS_COLS_B), floor(i.cell / FS_COLS_B));
            float2 uvB = (crB * FS_CELL_B + clamp(c / FS_CELL * FS_CELL_B, 0.5, FS_CELL_B - 0.5)) / (FS_ATLAS * 0.5);
            half4 f;
            if (second) f = SAMPLE_TEXTURE2D(_SheetA2, sampler_SheetA, float2(uvA.x, 1.0 - uvA.y));
            else f = SAMPLE_TEXTURE2D(_SheetA, sampler_SheetA, float2(uvA.x, 1.0 - uvA.y));
            half fd = SAMPLE_TEXTURE2D(_SheetB, sampler_SheetB, float2(uvB.x, 1.0 - uvB.y)).r;
            // distance to each band's edge in plate px; the edge is as wide as the clip's, never under a pixel of this view
            float perPx = max(0.7071 * length(float2(length(ddx(px)), length(ddy(px)))), 1e-4);
            float4 d = (f - 0.5) * (2.0 * FS_SDF_RANGE);
            float4 cov = saturate(d / max(FS_EDGE, perPx) + 0.5);
            float covD = saturate((fd - 0.5) * (2.0 * FS_SDF_RANGE) / max(FS_EDGE.y, perPx) + 0.5);
            half3 col = FS_RED;
            col = lerp(col, FS_DARKRED, covD);
            col = lerp(col, FS_ORANGE, cov.g);
            col = lerp(col, FS_YELLOW, cov.b);
            col = lerp(col, FS_WHITE, cov.a);
            // the fringe: a burnt-orange glow that falls off over the field's range outside the silhouette
            float g = saturate(1.0 + d.r / FS_SDF_RANGE);
            float ga = 0.5 * g * g * (1.0 - cov.r);
            col = (col * cov.r + FS_GLOW * ga) / max(cov.r + ga, 1e-4);
            float a = cov.r + ga;
            if (i.lens.x > 0.0)
            {
                // written past the lens; the 8-bit target clamps at 1, so keep the hue when the boost would pass it
                col /= max(LensKeep(i.screen.xy / i.screen.w, i.lens), 0.2);
                // the white band instead clamps channel by channel: plain white is the brightest the target holds
                half3 kept = col / max(1.0, max(col.r, max(col.g, col.b)));
                col = lerp(kept, min(col, 1.0), cov.a * cov.r);
            }
            return half4(col, a);
        }

        half4 fragOverSmoke(Varyings i) : SV_Target
        {
            half4 f = Fire(i);
            clip(f.a - 0.004);
            return half4(f.rgb * f.a, f.a);
        }

        half4 frag(Varyings i) : SV_Target
        {
            half4 f = Fire(i);
            // hidden only by what stands well in front of the card; round the target the card passes through the
            // board and its arms, and the sheet already has the clip's own holes there
            float2 suv = i.screen.xy / i.screen.w;
            #if UNITY_REVERSED_Z
                float d = SampleSceneDepth(suv);
            #else
                float d = lerp(UNITY_NEAR_CLIP_VALUE, 1.0, SampleSceneDepth(suv));
            #endif
            float3 sp = ComputeWorldSpacePosition(suv, d, UNITY_MATRIX_I_VP);
            float behind = distance(i.world, _WorldSpaceCameraPos) - distance(sp, _WorldSpaceCameraPos);
            float tol = lerp(0.45, 3.0, smoothstep(7.0, 9.5, i.sa.x));
            float a = f.a * (1.0 - smoothstep(tol, tol + 0.45, behind));
            clip(a - 0.004);
            return half4(f.rgb * a, a);
        }
        ENDHLSL

        Pass
        {
            // first: over the effect's own smoke (bit 4 set by the lobes), no depth rule
            Name "CelFlameSheetOverSmoke"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend One OneMinusSrcAlpha
            Cull Off ZWrite Off ZTest Always
            Stencil { Ref 4 ReadMask 4 WriteMask 0 Comp Equal Pass Keep Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment fragOverSmoke
            ENDHLSL
        }
        Pass
        {
            // then everywhere else; writes bit 4 so the ground wash stays off the fire
            Name "CelFlameSheet"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            Cull Off ZWrite Off ZTest Always
            Stencil { Ref 4 ReadMask 4 WriteMask 4 Comp NotEqual Pass Replace Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            ENDHLSL
        }
    }
}
