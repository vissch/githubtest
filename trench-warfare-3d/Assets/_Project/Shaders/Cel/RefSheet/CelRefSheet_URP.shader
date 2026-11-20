// Phase: CelFx reference sheets (2026-11-19) - an effect cut out of its approved reference clip frame by frame
// (celfx/tools/refsheet) and replayed. Generalised from fire_bolts' CelBoltSheet_URP (2026-11-18, owner-approved): the
// layout that was #defined per effect is a material property here, so one shader serves every effect; each effect's
// RefSheetKind subclass owns one material and sets it from <Prefix>SheetLayout.json.
// Each element (a bolt, a hit site, the whole frame) is one record: a card in the world, parallel to the reference
// camera's image plane at the element's depth, covering the element's rectangle of the reference frame. At the beach
// Range's close shot every texel lands on the screen pixel it was cut from; from elsewhere it is a card at its place.
//   born/life  frame = round(age x extra.x) + extra.y, held (a film never shows a blend)
//   extra      x frames a second (24), y the clip frame at birth, z the element's index in the table
// _SheetA, _SheetB  tiles stored (tile + 2)^2: premultiplied colour to write (already through the night grade, over
//                   the no-effect film) and opacity
// _SheetC.._SheetF  more tiles when A and B are full (a sheet holds 255 x 255 tiles: the index is 8 bits)
// _SheetInd   grid texels a frame, _RS_IndCols frames a row: R, G the tile's column, row; B the sheet 1-4, 0 = none
// _SheetElems Frames wide, 4 rows an element: plate rectangle (x, y, w, h; w 0 = not shown), world origin, U, V
Shader "TW/Cel/RefSheet"
{
    Properties
    {
        [NoScaleOffset] _SheetA ("Tiles A", 2D) = "black" {}
        [NoScaleOffset] _SheetB ("Tiles B", 2D) = "black" {}
        [NoScaleOffset] _SheetC ("Tiles C", 2D) = "black" {}
        [NoScaleOffset] _SheetD ("Tiles D", 2D) = "black" {}
        [NoScaleOffset] _SheetE ("Tiles E", 2D) = "black" {}
        [NoScaleOffset] _SheetF ("Tiles F", 2D) = "black" {}
        [NoScaleOffset] _SheetInd ("Tile index", 2D) = "black" {}
        [NoScaleOffset] _SheetElems ("Element table", 2D) = "black" {}
        _RS_Frames ("Frames", Float) = 0
        _RS_Elems ("Elements", Float) = 0
        _RS_Tile ("Tile px", Float) = 16
        _RS_Stored ("Stored tile px", Float) = 18
        _RS_Grid ("Tiles a frame (x, y)", Vector) = (84, 48, 0, 0)
        _RS_IndCols ("Index frames a row", Float) = 14
        _RS_IndSize ("Index size (x, y)", Vector) = (1176, 672, 0, 0)
        _RS_Sides ("Sheet sides (A, B, C, D)", Vector) = (4096, 4096, 1, 1)
        _RS_Sides2 ("Sheet sides (E, F)", Vector) = (1, 1, 0, 0)
        _RS_Border ("Tile border px", Float) = 1
        _RS_Gain ("Colour gain (hdr films)", Float) = 1
        _RS_Filter ("0 bilinear, 1 Catmull-Rom", Float) = 0
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+5" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"
        TEXTURE2D(_SheetA);
        SAMPLER(sampler_SheetA);
        TEXTURE2D(_SheetB);
        TEXTURE2D(_SheetC);
        TEXTURE2D(_SheetD);
        TEXTURE2D(_SheetE);
        TEXTURE2D(_SheetF);
        TEXTURE2D(_SheetInd);
        TEXTURE2D(_SheetElems);
        float _RS_Frames, _RS_Elems, _RS_Tile, _RS_Stored, _RS_IndCols, _RS_Border, _RS_Gain, _RS_Filter;
        float4 _RS_Grid, _RS_IndSize, _RS_Sides, _RS_Sides2;

        float SheetSide(float z) { return z < 1.5 ? _RS_Sides.x : (z < 2.5 ? _RS_Sides.y : (z < 3.5 ? _RS_Sides.z : (z < 4.5 ? _RS_Sides.w : (z < 5.5 ? _RS_Sides2.x : _RS_Sides2.y)))); }

        // one texel of sheet z at (x, y), y down from the sheet's top (PNG rows are flipped on import)
        half4 SheetTexel(float z, int2 t)
        {
            int2 q = int2(t.x, (int)SheetSide(z) - 1 - t.y);
            if (z < 1.5) return LOAD_TEXTURE2D(_SheetA, q);
            if (z < 2.5) return LOAD_TEXTURE2D(_SheetB, q);
            if (z < 3.5) return LOAD_TEXTURE2D(_SheetC, q);
            if (z < 4.5) return LOAD_TEXTURE2D(_SheetD, q);
            if (z < 5.5) return LOAD_TEXTURE2D(_SheetE, q);
            return LOAD_TEXTURE2D(_SheetF, q);
        }

        // Catmull-Rom at texel position c (y down): the close camera renders the plate at 1.86x, so bilinear blurs the
        // sheet (checks.py crisp: proof 0.94, film 0.67 on explosion_sheet r01) and point sampling stair-steps it (the
        // owner's "outline is too pixelated", acid_ball a81). Needs a 2 px tile border; the result is clamped
        // because the kernel rings.
        half4 SheetCatmull(float z, float2 c)
        {
            float2 p = c - 0.5, i0 = floor(p), f = p - i0;
            float2 w0 = f * (-0.5 + f * (1.0 - 0.5 * f));
            float2 w1 = 1.0 + f * f * (-2.5 + 1.5 * f);
            float2 w2 = f * (0.5 + f * (2.0 - 1.5 * f));
            float2 w3 = f * f * (-0.5 + 0.5 * f);
            float wx[4] = { w0.x, w1.x, w2.x, w3.x };
            float wy[4] = { w0.y, w1.y, w2.y, w3.y };
            half4 s = 0;
            [unroll] for (int j = 0; j < 4; j++)
                [unroll] for (int k = 0; k < 4; k++)
                    s += SheetTexel(z, int2(i0) + int2(k - 1, j - 1)) * (wx[k] * wy[j]);
            s = max(s, 0.0);
            s.a = saturate(s.a);
            return s;
        }

        struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
        struct Varyings
        {
            float4 positionCS : SV_POSITION;
            float2 px : TEXCOORD0;                       // plate px of the reference frame (y down)
            nointerpolation float frame : TEXCOORD1;     // the clip frame, -1 none
        };

        Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
        {
            CelRecord r = CelFetch(instanceID);
            Varyings o;
            float frame = floor(max(CelAge(r), 0.0) * r.extra.x + 0.5) + r.extra.y;
            int row = (int)(r.extra.z + 0.5) * 4;
            bool shown = CelAlive(r) > 0.5 && frame >= 0.0 && frame < _RS_Frames && r.extra.z < _RS_Elems - 0.5;
            int f = (int)clamp(frame, 0.0, _RS_Frames - 1.0);
            float4 rect = LOAD_TEXTURE2D_LOD(_SheetElems, int2(f, row), 0);
            float3 org = LOAD_TEXTURE2D_LOD(_SheetElems, int2(f, row + 1), 0).xyz;
            float3 du = LOAD_TEXTURE2D_LOD(_SheetElems, int2(f, row + 2), 0).xyz;
            float3 dv = LOAD_TEXTURE2D_LOD(_SheetElems, int2(f, row + 3), 0).xyz;
            shown = shown && rect.z > 0.5;
            // uv.y 0 is the card's top (plate y runs down)
            float2 q = float2(v.uv.x, 1.0 - v.uv.y);
            float3 p = org + du * q.x + dv * q.y;
            o.positionCS = TransformWorldToHClip(p) * (shown ? 1.0 : 0.0);
            o.px = rect.xy + rect.zw * q;
            o.frame = shown ? frame : -1.0;
            return o;
        }

        half4 frag(Varyings i) : SV_Target
        {
            clip(i.frame);
            float2 px = clamp(i.px, 0.0, _RS_Grid.xy * _RS_Tile - 0.001);
            float2 tile = floor(px / _RS_Tile);
            float fc = fmod(i.frame, _RS_IndCols), fr = floor(i.frame / _RS_IndCols);
            float2 t = float2(fc, fr) * _RS_Grid.xy + tile;
            // textures imported from a PNG have their last row at y = 0
            half4 ind = LOAD_TEXTURE2D(_SheetInd, int2((int)t.x, (int)(_RS_IndSize.y - 1.0 - t.y)));
            float3 e = floor(ind.rgb * 255.0 + 0.5);
            clip(e.z - 0.5);
            float2 c = e.xy * _RS_Stored + _RS_Border + (px - tile * _RS_Tile);     // texel position in the sheet, y down
            // a sheet holds at most 255 x 255 tiles (8-bit index): a larger cut spills into C and D
            half4 s;
            if (_RS_Filter > 0.5) s = SheetCatmull(e.z, c);
            else
            {
                float2 uv = c / SheetSide(e.z);
                uv.y = 1.0 - uv.y;
                if (e.z < 1.5) s = SAMPLE_TEXTURE2D_LOD(_SheetA, sampler_SheetA, uv, 0);
                else if (e.z < 2.5) s = SAMPLE_TEXTURE2D_LOD(_SheetB, sampler_SheetA, uv, 0);
                else if (e.z < 3.5) s = SAMPLE_TEXTURE2D_LOD(_SheetC, sampler_SheetA, uv, 0);
                else if (e.z < 4.5) s = SAMPLE_TEXTURE2D_LOD(_SheetD, sampler_SheetA, uv, 0);
                else if (e.z < 5.5) s = SAMPLE_TEXTURE2D_LOD(_SheetE, sampler_SheetA, uv, 0);
                else s = SAMPLE_TEXTURE2D_LOD(_SheetF, sampler_SheetA, uv, 0);
            }
            clip(max(max(s.r, s.g), max(s.b, s.a)) - 0.002);
            s.rgb *= _RS_Gain;      // stored colour = written / gain (hdr=1 films write past 1)
            return s;      // premultiplied
        }
        ENDHLSL

        Pass
        {
            // every pixel of the clip is drawn once, whichever element's card reaches it first (bit 32)
            Name "CelRefSheet"
            Tags { "LightMode"="UniversalForward" }
            Blend One OneMinusSrcAlpha
            Cull Off ZWrite Off ZTest Always
            Stencil { Ref 32 ReadMask 32 WriteMask 32 Comp NotEqual Pass Replace Fail Keep ZFail Keep }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            ENDHLSL
        }
    }
}
