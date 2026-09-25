// Phase: C4 (implemented) — hand-drawn flipbook sprites for the fight: shell bursts, earth and water columns, the dust a
// round kicks up, the puff and star where a man is hit, the muzzle flare. One quad each, drawn instanced by FlipbookFx.
// Nothing about the sprite is in the mesh: the instance matrix is not a transform but a packed record (see FlipbookFx.Pack)
//   m03 m13 m23  world position          m00 width (m)      m11 height (m)     m22 frame (fractional: blends to the next)
//   m01 alpha    m10 brightness           m02 roll (rad)     m12 1 = upright (turns to the view about Y only, stays vertical)
//   m20 1 = anchored at its bottom edge   m21 the card's opacity, negative to mirror it   (m01 is the fade, 1 at birth)
// and the vertex shader builds the card facing the camera. The books are greyscale drawings with alpha: their own light and
// dark reads as the toon's lit and shade bands (_Lit), so a tinted cloud sits under the same moon as the men. Additive books
// (flash, star, muzzle) ignore the scene light and only dim with the fog.
Shader "TW/Flipbook (URP)"
{
    Properties
    {
        _MainTex ("Book", 2D) = "white" {}
        _Grid ("Columns, rows, frames, snap (1 = cut frame to frame, do not blend)", Vector) = (4, 4, 16, 0)
        _Tint ("Tint", Color) = (1, 1, 1, 1)
        _Shade ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _Levels ("Ink levels: the drawing's value that is full shade (x) and full light (y)", Vector) = (0, 1, 0, 0)
        _Lit ("Lit by the scene (0 additive/unlit, 1 toon lit)", Range(0, 1)) = 1
        _MaskOnly ("Use alpha only (a drawing in black)", Float) = 0
        _Fire ("Drawn fire: premultiplied, unlit, cel-banded", Float) = 0
        _Smoke ("Fire: the soot drawn into it", Color) = (0.20, 0.13, 0.11, 1)
        _Fringe ("Fire: the cool outer edge of the flame", Color) = (1.05, 0.22, 0.05, 1)
        _Core ("Fire: the heart, hotter than white", Color) = (1.95, 1.72, 1.40, 1)
        _InkColor ("Fire: the contour drawn round it", Color) = (0.26, 0.05, 0.03, 1)
        _Contour ("Fire contour: where the silhouette starts (x), how wide the line is (y), how dark (z)", Vector) = (0.10, 0.26, 0.85, 0)
        _Bands ("Fire: where soot becomes fringe (x), fringe body (y), body core (z), edge softness (w)", Vector) = (0.12, 0.40, 0.86, 0.75)
        _Rise ("Heat falloff up the card", Float) = 0
        _Hot ("Fire: what the rolloff compresses toward (1 = never brighter than white)", Float) = 2.4
        _Erode ("Tears apart as it fades (0 fades evenly)", Range(0, 1)) = 0
        _ShadeMood ("How much the mood tints the shade (smoke keeps more of its own grey)", Range(0, 1)) = 1
        [Enum(UnityEngine.Rendering.BlendMode)] _SrcBlend ("Src", Float) = 5
        [Enum(UnityEngine.Rendering.BlendMode)] _DstBlend ("Dst", Float) = 10
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+10" "IgnoreProjector"="True" }
        Pass
        {
            Name "Flipbook"
            Tags { "LightMode"="UniversalForward" }
            Blend [_SrcBlend] [_DstBlend]
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            TEXTURE2D(_MainTex); SAMPLER(sampler_MainTex);
            CBUFFER_START(UnityPerMaterial)
                float4 _Grid, _Levels, _Bands, _Contour;
                half4 _Tint, _Shade, _Smoke, _Fringe, _Core, _InkColor;
                float _Lit, _MaskOnly, _Erode, _SrcBlend, _DstBlend, _ShadeMood, _Fire, _Rise, _Hot;
            CBUFFER_END
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float4 uv : TEXCOORD0;        // xy this frame's cell, zw the next frame's
                float4 tone : TEXCOORD1;      // x fade (1 at birth), y brightness, z blend to the next frame, w fog
                float opacity : TEXCOORD3;
                float2 local : TEXCOORD4;     // the card's own uv, for the erode's edge bias and the underside
                float3 positionWS : TEXCOORD2;
                float4 screen : TEXCOORD5;    // where it lands on the screen, for the scene depth behind it
                float2 extra : TEXCOORD6;     // x the card's width (m), y its depth from the eye (m)
            };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                float4x4 m = UNITY_MATRIX_M;
                float3 at = float3(m._m03, m._m13, m._m23);
                float width = m._m00, height = m._m11, frame = m._m22;
                float alpha = m._m01, bright = m._m10, roll = m._m02, upright = m._m12, anchored = m._m20, mirror = m._m21 < 0 ? -1.0 : 1.0, opacity = abs(m._m21);
                // the card: centred, or standing on its bottom edge; mirrored; rolled in its own plane
                float2 c = float2((v.uv.x - 0.5) * mirror, v.uv.y - 0.5 * (1.0 - anchored));
                float sr, cr; sincos(roll, sr, cr);
                c = float2(c.x * cr - c.y * sr, c.x * sr + c.y * cr) * float2(width, height);
                float3 viewRight = UNITY_MATRIX_V[0].xyz, viewUp = UNITY_MATRIX_V[1].xyz;
                float3 toEye = normalize(_WorldSpaceCameraPos - at);
                float3 flatRight = normalize(cross(float3(0, 1, 0), toEye));
                float3 right = lerp(viewRight, flatRight, upright), up = lerp(viewUp, float3(0, 1, 0), upright);
                o.positionWS = at + right * c.x + up * c.y;
                o.positionCS = TransformWorldToHClip(o.positionWS);
                // the book: frames left to right, top row first; a fractional frame blends into the next
                float frames = max(1.0, _Grid.z), cols = max(1.0, _Grid.x), rows = max(1.0, _Grid.y);
                float f0 = clamp(floor(frame), 0.0, frames - 1.0), f1 = min(f0 + 1.0, frames - 1.0);
                float2 cell = float2(1.0 / cols, 1.0 / rows);
                float2 uv0 = (float2(fmod(f0, cols), rows - 1.0 - floor(f0 / cols)) + v.uv) * cell;
                float2 uv1 = (float2(fmod(f1, cols), rows - 1.0 - floor(f1 / cols)) + v.uv) * cell;
                o.uv = float4(uv0, uv1);
                o.tone = float4(alpha, bright, frame - floor(frame), ComputeFogFactor(o.positionCS.z));
                o.opacity = opacity;
                o.local = v.uv;
                o.screen = ComputeScreenPos(o.positionCS);
                o.extra = float2(width, -TransformWorldToView(o.positionWS).z);
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                half4 a = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv.xy), b = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv.zw);
                // Blending one frame into the next is right for a painted cloud, which has no edges to lose, and wrong
                // for a drawing: cross-dissolving two cels puts a ghost of each over the other and every crisp line in
                // the book goes soft. A book drawn frame by frame (_Grid.w) CUTS, the way it was animated.
                half4 tex = lerp(a, b, i.tone.z * (1.0 - step(0.5, _Grid.w)));
                // a cloud tears apart as it goes: the edges go first and the dense heart last, and what is left thins
                half gone = 1.0 - i.tone.x;
                half edge = saturate(length(i.local - 0.5) * 2.0);
                half threshold = gone * (0.45 + 0.55 * edge);
                half torn = saturate((tex.a - threshold) / 0.45) * pow(1.0 - gone, 1.5);
                half alpha = lerp(tex.a * i.tone.x, torn, _Erode) * i.opacity;
                // soft where it meets the ground or a wall (the scene's depth behind it, over 0.8 m), and gone as it comes
                // through the lens: up close a card wider than the picture was a wall of smoke for seconds
                float scene = LinearEyeDepth(SampleSceneDepth(i.screen.xy / i.screen.w), _ZBufferParams);
                alpha *= saturate((scene - i.extra.y) / 0.8) * saturate((i.extra.y - 0.25 * i.extra.x) / (0.5 * i.extra.x + 0.01));
                if (alpha < 0.004) discard;
                // the packs keep black under their transparent pixels, so the small mips of a thin wisp go dark: read the
                // drawing's value per unit of coverage
                half ink = _MaskOnly > 0.5 ? 1.0 : saturate(dot(tex.rgb, half3(0.299, 0.587, 0.114)) / max(tex.a, 0.06));
                // Drawn fire. Fire is additive everywhere else in this project and additive is why the drawing kept
                // disappearing: an added card can only ever make the frame brighter, so every DARK part of a cel - the
                // ink contour, the gaps between tongues, the smoke rolled into the drawing - contributes nothing and
                // the book collapses to its bright interior. Two of them crossing then run past 1 and clip to a flat
                // white lozenge. So fire is drawn premultiplied OVER the frame instead (One, OneMinusSrcAlpha): the
                // dark of the drawing is dark, the cel keeps its edge, and overlapping cards composite instead of
                // summing. What brightness it does carry is rolled off preserving hue, so the heart of a fire goes to
                // deep orange and then to white the way fire does, and never to the flat paper-white of a clipped sum.
                if (_Fire > 0.5)
                {
                    // premultiplied over uses alpha as the destination factor, so an opacity past 1 gives a NEGATIVE
                    // one and the card eats a dark halo out of the frame behind its edges instead of covering it
                    alpha = saturate(alpha);
                    half silhouette = alpha;   // kept before the soot is thinned below: the contour follows the SHAPE
                    half band = saturate((ink - _Levels.x) / max(0.01, _Levels.y - _Levels.x));
                    // The dark in a fire drawing is the smoke the artist drew INTO it, and smoke is thin. Left as
                    // opaque as the flame it came out as brown paint over a bright background (the same book that
                    // read as fire against a night trench read as mud against snow). Thinning by the value keeps the
                    // flame solid - and so keeps its cel edge - while the smoke inside it lets the scene through.
                    // 0.72, not 0.55. Thinning the dark of a fire drawing is right - it is smoke the artist drew in,
                    // and smoke is thin - but at 0.55 the darkest band went so sheer that the hillside read straight
                    // through the middle of the jet's head. A head you can see terrain through is not a body of
                    // burning fuel, it is a stain on glass, and it inverts the energy of the whole stream.
                    alpha *= lerp(0.72, 1.0, band);
                    // Cel bands, not a ramp. A single hue scaled by the drawing's value is a GRADIENT, and a gradient
                    // is the one thing hand-drawn fire never is: the artist puts down four flat shapes - the soot
                    // drawn into it, a cool fringe, the body, and a heart hotter than white - with a hard line between
                    // them, and that line is the whole look. The boundary is widened to about a pixel with fwidth so
                    // it stays a clean edge at any distance instead of crawling, and no wider, or the bands smear back
                    // into the ramp this replaces.
                    // The thresholds are not taste: the sheet's ink is strongly bimodal - a dark mass and a bright one
                    // with almost nothing between - and these four slices are measured to divide it into four bands
                    // that each cover a real share of the drawing. And the heart is whiter rather than merely
                    // brighter, because the rolloff below compresses everything above 1 toward the same place: a core
                    // that was only brighter than the body came out the same orange as the body.
                    half aa = max(fwidth(band), 1e-4) * _Bands.w;
                    half3 fire = lerp(_Smoke.rgb, _Fringe.rgb, smoothstep(_Bands.x - aa, _Bands.x + aa, band));
                    fire = lerp(fire, _Tint.rgb, smoothstep(_Bands.y - aa, _Bands.y + aa, band));
                    fire = lerp(fire, _Core.rgb, smoothstep(_Bands.z - aa, _Bands.z + aa, band));
                    // HEAT FALLS OFF WITH HEIGHT - as a hue, not as a value. A flame column is hottest and whitest
                    // low and inboard and cools to amber and then to soot as it thins and rises, and applying the cel
                    // bands flat across a card ignores that: the same cream appeared at the apex of a four-metre
                    // column as at its foot, and on the cook-off the gradient was actually inverted.
                    //
                    // The first attempt scaled BAND - the drawing's value before it is classified - and that was the
                    // wrong property by a mile. Pushing the band down drags the core, the body and the rim across
                    // their thresholds together, so they converge on one colour and the top half of every flame lost
                    // its interior drawing completely: three overlapping licks became one paper cutout with a bumpy
                    // top. A gradient nobody asked for, bought with the shapes that were doing the work.
                    //
                    // Here instead: the bands are classified first and untouched, and the RESULT is rotated toward
                    // umber. It is a per-channel multiply applied equally to all four bands, so every value ratio
                    // between them survives exactly - the core stays the brightest thing on the card at any height,
                    // it is simply a cooler brightest. And it lands before the contour, so the ink line is never
                    // touched and stays the darkest thing in the drawing right up to the tips.
                    fire *= lerp(half3(1.0, 1.0, 1.0), half3(0.94, 0.70, 0.42),
                                 _Rise * smoothstep(0.30, 1.0, i.local.y));
                    // and the contour. The sheets are keyed with a soft edge, so there is a narrow ring of part-alpha
                    // all round the drawing; laying ink into that ring gives the flame the drawn outline that a
                    // painted effect never has, and it is what stops a cel flame reading as a glow with a shape.
                    half rimIn  = smoothstep(_Contour.x, _Contour.x + _Contour.y * 0.5, silhouette);
                    half rimOut = smoothstep(_Contour.x + _Contour.y * 0.5, _Contour.x + _Contour.y * 1.8, silhouette);
                    fire = lerp(fire, _InkColor.rgb, rimIn * (1.0 - rimOut) * _Contour.z);
                    fire *= i.tone.y;
                    half peak = max(fire.r, max(fire.g, fire.b));
                    // The rolloff has a CEILING, and it used to be 1.0. (1 - exp(-peak)) asymptotes at exactly one,
                    // so however hot a cel was authored - _Core is (1.95, 1.72, 1.40), well into HDR - no fire pixel
                    // in the game could ever come out brighter than a plainly lit white surface. Measured across four
                    // unrelated books from four different sheets, every one of them capped at 133-137 luminance while
                    // the scene's own shell flashes and braziers cleared 240; the cream of every sheet was arriving as
                    // beige. A flamethrower at night is the brightest thing on the field and it was rendering at half
                    // its authored highlight, which is where a great many of the it-reads-as-brown complaints came
                    // from - and it could never cross the bloom threshold, so fire never glowed.
                    //
                    // 2.4. It was briefly cut to 2.0 on a report of red-channel clipping at 2.4; re-measured, the
                    // clipping was not there - zero clipped pixels in three of four fires and one pixel in the fourth
                    // - and the cut simply deleted every pixel over 200 from the cook-off. There was headroom and it
                    // was spent going down. The number to watch is the count of pixels at R >= 250: while that stays
                    // under about a third of a percent of the fire's area there is room to go up, and the slope is
                    // roughly 45 luminance per unit.
                    // Same curve, same reason for existing: two cels crossing must not sum to a flat white slab, and
                    // the compression is applied to the peak channel so the hue survives it. It simply compresses
                    // TOWARD _Hot instead of toward one. At _Hot = 1 this is the old behaviour exactly.
                    fire *= peak > 1.0 ? (_Hot * (1.0 - exp(-peak / _Hot))) / peak : 1.0;
                    half fireMist = 1.0 - saturate(FieldFogAmount(i.positionWS) + _TWMistColor.a * saturate((_TWMist.x - i.positionWS.y) * _TWMist.y) * saturate((distance(_WorldSpaceCameraPos, i.positionWS) - _TWMist.z) * _TWMist.w));
                    half fireFog = ComputeFogIntensity(i.tone.w);
                    return half4(fire * alpha * fireFog * fireMist, alpha);
                }
                // the drawing's light and dark are the toon's two bands (each book's own range stretched to them, so a
                // drawing done in mid greys still catches the light); an unlit book keeps its own values
                half band = saturate((ink - _Levels.x) / max(0.01, _Levels.y - _Levels.x));
                half3 lit = lerp(_Shade.rgb * lerp(half3(1, 1, 1), TWShadeTint(), _ShadeMood), _MainLightColor.rgb, band);   // smoke keeps more of its own grey under a blue moon
                half3 color = _Tint.rgb * lerp(ink.xxx, lit, _Lit) * i.tone.y;
                if (_Erode > 0.5) color *= lerp(0.78, 1.0, smoothstep(0.0, 0.6, i.local.y));   // a cloud's underside is in its own shadow
                // the shell's own flash lights the earth it threw up and the smoke rolling off it: the drawing's light
                // parts catch it most, its dark parts least, so the column is modelled by its own burst and not flooded
                color += lerp(ink, 1.0, 0.3) * TWBurstLight(i.positionWS) * _Lit;
                // and the same for a fire standing under its own smoke. Without this the plume over a bonfire is lit
                // by the moon and nothing else, which at night is dark grey against a dark sky: the smoke was being
                // drawn the whole time and simply could not be seen.
                color += lerp(ink, 1.0, 0.22) * TWHearthLight(i.positionWS) * _Lit;
                half fog = ComputeFogIntensity(i.tone.w);
                if (_Lit < 0.5)
                {
                    // additive: dims into the fog, adds nothing where the mist would have hidden it
                    half mist = 1.0 - saturate(FieldFogAmount(i.positionWS) + _TWMistColor.a * saturate((_TWMist.x - i.positionWS.y) * _TWMist.y) * saturate((distance(_WorldSpaceCameraPos, i.positionWS) - _TWMist.z) * _TWMist.w));
                    return half4(color * alpha * fog * mist, 1.0);
                }
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                return half4(MixFog(color, i.tone.w), alpha);
            }
            ENDHLSL
        }
    }
}
