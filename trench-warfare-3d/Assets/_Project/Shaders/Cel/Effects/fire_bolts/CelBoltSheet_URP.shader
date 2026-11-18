// Phase: CelFx fire_bolts reference sheets (2026-11-18) - the reference clip's fire and smoke, cut out frame by frame
// (celfx/agents/fire_bolts/sheets_20261118) and replayed. Each element (a bolt, a hit site, the whole frame) is one
// record: a card in the world, parallel to the reference camera's image plane at the element's depth, covering the
// element's rectangle of the reference frame. At the beach Range's close shot every texel lands on the screen pixel
// it was cut from; from elsewhere it is a card at the element's place.
//   born/life  frame = round(age x extra.x) + extra.y, held (a film never shows a blend)
//   extra      x frames a second (24), y the clip frame at birth, z the element's index in the table
// _SheetA, _SheetB  16 px tiles stored 18 x 18: premultiplied colour to write (already through the night grade, over
//                   the no-effect film) and opacity
// _SheetInd   84 x 48 texels a frame, 14 frames a row: R, G the tile's column, row; B 1 = sheet A, 2 = sheet B, 0 = none
// _SheetElems Frames wide, 4 rows an element: plate rectangle (x, y, w, h; w 0 = not shown), world origin, U, V
Shader "TW/Cel/BoltSheet"
{
    Properties
    {
        [NoScaleOffset] _SheetA ("Tiles A", 2D) = "black" {}
        [NoScaleOffset] _SheetB ("Tiles B", 2D) = "black" {}
        [NoScaleOffset] _SheetInd ("Tile index", 2D) = "black" {}
        [NoScaleOffset] _SheetElems ("Element table", 2D) = "black" {}
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+5" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"
        #include "Assets/_Project/Shaders/Cel/Effects/fire_bolts/BoltSheetData.hlsl"
        TEXTURE2D(_SheetA);
        SAMPLER(sampler_SheetA);
        TEXTURE2D(_SheetB);
        TEXTURE2D(_SheetInd);
        TEXTURE2D(_SheetElems);

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
            bool shown = CelAlive(r) > 0.5 && frame >= 0.0 && frame < BS_FRAMES && r.extra.z < BS_ELEMS - 0.5;
            int f = (int)clamp(frame, 0.0, BS_FRAMES - 1.0);
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
            float2 px = clamp(i.px, 0.0, BS_GRID * BS_TILE - 0.001);
            float2 tile = floor(px / BS_TILE);
            float fc = fmod(i.frame, BS_IND_COLS), fr = floor(i.frame / BS_IND_COLS);
            float2 t = float2(fc, fr) * BS_GRID + tile;
            // textures imported from a PNG have their last row at y = 0
            half4 ind = LOAD_TEXTURE2D(_SheetInd, int2((int)t.x, (int)(BS_IND_SIZE.y - 1.0 - t.y)));
            float3 e = floor(ind.rgb * 255.0 + 0.5);
            clip(e.z - 0.5);
            float2 c = e.xy * BS_STORED + 1.0 + (px - tile * BS_TILE);     // texel position in the sheet, y down
            half4 s;
            if (e.z < 1.5) { float2 uv = c / BS_SIDE_A; s = SAMPLE_TEXTURE2D_LOD(_SheetA, sampler_SheetA, float2(uv.x, 1.0 - uv.y), 0); }
            else { float2 uv = c / BS_SIDE_B; s = SAMPLE_TEXTURE2D_LOD(_SheetB, sampler_SheetA, float2(uv.x, 1.0 - uv.y), 0); }
            clip(max(max(s.r, s.g), max(s.b, s.a)) - 0.002);
            return s;      // premultiplied
        }
        ENDHLSL

        Pass
        {
            // every pixel of the clip is drawn once, whichever element's card reaches it first (bit 32)
            Name "CelBoltSheet"
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
