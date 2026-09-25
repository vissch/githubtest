"""Phase 3, GPU work with the same image. Groups (apply any subset, in any order; each anchor must match once):
  vat    VAT_URP.shader: clip() only under _TW_LIMBCUT (the fallen)       [shader only]
  vatcs  VATRenderer.cs: the fallen material enables _TW_LIMBCUT           [C#]
  ink    InkLines_URP.shader blends; TW-Renderer.asset stops copying the colour buffer
  inkcs  InkLinesSetup.cs installs it that way                              [C#]
  rain   Rain_URP / RainCurtain_URP collapse what draws at alpha 0          [shader only]
  water  TWWater.hlsl loops to _TWRingCount                                 [shader only]
  watercs WaterRings.cs packs the live rings and sets the count             [C#]
Usage: gpu_edits.py <repo>/trench-warfare-3d group[,group...] [--check]"""
import sys, os
root = sys.argv[1]; groups = set(sys.argv[2].split(',')); check = '--check' in sys.argv
S = 'Assets/_Project/Shaders/'

def edit(rel, reps):
    path = os.path.join(root, rel)
    raw = open(path, 'rb').read(); bom = raw.startswith(b'\xef\xbb\xbf')
    text = (raw[3:] if bom else raw).decode('utf-8'); crlf = '\r\n' in text; text = text.replace('\r\n', '\n')
    for a, b in reps:
        n = text.count(a)
        if n != 1: raise SystemExit('%s: anchor x%d: %r' % (rel, n, a[:90]))
        text = text.replace(a, b)
    if check: print('ok', rel); return
    if crlf: text = text.replace('\n', '\r\n')
    open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + text.encode('utf-8')); print('written', rel)

if 'vat' in groups:
    edit(S + 'VAT_URP.shader', [
("""// Vertex colour: rgb = albedo, a = 1 where the team colour multiplies it.
""",
"""// Vertex colour: rgb = albedo, a = 1 where the team colour multiplies it.
// Only the fallen's material carries _TW_LIMBCUT (the lost-limb and wound clips); the living draw with no clip() at all.
"""),
("""        struct Animated { float3 positionOS; float3 positionWS; float3 normalWS; float tint; float scale; float gone; float cut; float2 grime; };
""",
"""        struct Animated { float3 positionOS; float3 positionWS; float3 normalWS; float tint; float scale; float gone; float cut; float2 grime; };

        // Only the fallen lose limbs (a living man's record packs none: VatPad, VATRenderer), so only their material
        // enables _TW_LIMBCUT. A shader that can discard is depth-tested after it has run, not before, so the clip that
        // never fired on a living man still turned early-Z off for all 3,000 of them, in every pass, and gave their
        // shadow pass pixel work to do. Without the keyword no pass of theirs can discard.
        #if defined(_TW_LIMBCUT)
            #define VAT_CLIP_LOST(gone) clip(0.5 - (gone))
        #else
            #define VAT_CLIP_LOST(gone)
        #endif
"""),
("""            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile_fog
""",
"""            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile_fog
            #pragma multi_compile_local_fragment _ _TW_LIMBCUT
"""),
("""                clip(0.5 - i.gone);   // a limb a shell took off (DebrisRenderer throws it)
""",
"""                VAT_CLIP_LOST(i.gone);   // a limb a shell took off (DebrisRenderer throws it)
"""),
("""                // B4: ellipsoid wound clip exposes embedded gore geometry
                if (_WoundRadii.x > 0.0)
                {
                    float3 d = (i.positionOS - _WoundCenter.xyz) / _WoundRadii.xyz;
                    clip(dot(d, d) - 1.0);
                }
""",
"""                // B4: ellipsoid wound clip exposes embedded gore geometry (the fallen only: nothing sets it on the living,
                // and even an untaken clip here would cost them early-Z)
            #if defined(_TW_LIMBCUT)
                if (_WoundRadii.x > 0.0)
                {
                    float3 d = (i.positionOS - _WoundCenter.xyz) / _WoundRadii.xyz;
                    clip(dot(d, d) - 1.0);
                }
            #endif
"""),
("""            #pragma fragment fragOutline
            #pragma multi_compile_fog
""",
"""            #pragma fragment fragOutline
            #pragma multi_compile_fog
            #pragma multi_compile_local_fragment _ _TW_LIMBCUT
"""),
("""half4 fragOutline(OutlineVaryings i) : SV_Target { clip(0.5 - i.gone); return""",
 """half4 fragOutline(OutlineVaryings i) : SV_Target { VAT_CLIP_LOST(i.gone); return"""),
("""            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
""",
"""            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
            #pragma multi_compile_local_fragment _ _TW_LIMBCUT
"""),
("""half4 fragNull(ShadowVaryings i) : SV_Target { clip(0.5 - i.gone); return 0; }""",
 """half4 fragNull(ShadowVaryings i) : SV_Target { VAT_CLIP_LOST(i.gone); return 0; }"""),
("""            #pragma fragment fragDepth
""",
"""            #pragma fragment fragDepth
            #pragma multi_compile_local_fragment _ _TW_LIMBCUT
"""),
("""half4 fragDepth(DepthVaryings i) : SV_Target { clip(0.5 - i.gone); return 0; }""",
 """half4 fragDepth(DepthVaryings i) : SV_Target { VAT_CLIP_LOST(i.gone); return 0; }"""),
    ])

if 'vatcs' in groups:
    edit('Assets/_Project/Presentation/Units/VATRenderer.cs', [
("""            fallen.SetFloat(CullId, (float)CullMode.Off);
""",
"""            fallen.SetFloat(CullId, (float)CullMode.Off);
            fallen.EnableKeyword("_TW_LIMBCUT");   // and only they can have lost a limb: the living draw with no clip() (VAT_URP)
"""),
    ])

if 'ink' in groups:
    edit(S + 'InkLines_URP.shader', [
("""// renderer feature before the transparents, so smoke, tracers and markers stay clean.
""",
"""// renderer feature before the transparents, so smoke, tracers and markers stay clean. The ink is blended over the frame
// where there is some and nothing is written where there is none, so the feature does not copy the colour buffer for it
// (fetchColorBuffer off): that copy was a full-screen HDR read and write every frame, for a lerp the blend does.
"""),
("""        ZWrite Off ZTest Always Cull Off Blend Off
""",
"""        ZWrite Off ZTest Always Cull Off Blend SrcAlpha OneMinusSrcAlpha, Zero One
"""),
("""                float2 uv = input.texcoord;
                half4 color = SAMPLE_TEXTURE2D_X(_BlitTexture, sampler_LinearClamp, uv);
""",
"""                float2 uv = input.texcoord;
"""),
("""                color.rgb = lerp(color.rgb, _InkColor.rgb, ink);
                return color;
""",
"""                if (ink <= 0.0) discard;              // most of the frame: the lerp left it as it was
                return half4(_InkColor.rgb, ink);     // blended: lerp(frame, ink colour, ink)
"""),
    ])
    edit('Assets/_Project/Settings/TW-Renderer.asset', [
("""  fetchColorBuffer: 1
""", """  fetchColorBuffer: 0
"""),
    ])

if 'inkcs' in groups:
    edit('Assets/_Project/Editor/InkLinesSetup.cs', [
("""// transparents. Idempotent; menu TW/Look/Install Ink Lines. Remove the feature on TW-Renderer to switch it off.
""",
"""// transparents. Idempotent; menu TW/Look/Install Ink Lines. Remove the feature on TW-Renderer to switch it off.
// The pass blends over the frame, so it needs no copy of the colour buffer (fetchColorBuffer off).
"""),
("""            feature.fetchColorBuffer = true;
""",
"""            feature.fetchColorBuffer = false;   // the shader blends; a copy would be a full-screen HDR read and write a frame
"""),
    ])

if 'rain' in groups:
    edit(S + 'Rain_URP.shader', [
("""                p = _Centre.xyz + (cell - 0.5) * _Size.xyz;
""",
"""                p = _Centre.xyz + (cell - 0.5) * _Size.xyz;
                float3 face = saturate((0.5 - abs(cell - 0.5)) * 10.0);
                float falls = saturate((_Level - v.random.y * 0.9) * 7.0);   // this streak only falls when it rains hard enough
                if (face.x * face.y * face.z * falls <= 0.0)
                {
                    // not falling now (most streaks in a drizzle, all of them in a dry spell): it was drawn at alpha 0. Its
                    // four corners on one point make a quad of no area, which the rasteriser drops.
                    o.positionCS = TransformWorldToHClip(p); o.alpha = 0; return o;
                }
"""),
("""                float3 face = saturate((0.5 - abs(cell - 0.5)) * 10.0);
                // snow hands over""",
"""                // snow hands over"""),
("""                    * saturate((_Level - v.random.y * 0.9) * 7.0) * (0.6 + 0.6 * _Level);   // this streak only falls when it rains hard enough
""",
"""                    * falls * (0.6 + 0.6 * _Level);
"""),
    ])
    edit(S + 'RainCurtain_URP.shader', [
("""// the camera's cell; a card whose cell is empty collapses to nothing in the vertex shader, so only real curtains are
// rasterised.""",
"""// the camera's cell; a card whose cell is empty, or whose curtain is faded out (nearer than 60 m, beyond 235 m), collapses
// to nothing in the vertex shader, so only curtains that show are rasterised."""),
("""                o.fade = half2(saturate((away - 60.0) / 55.0) * (1.0 - saturate((away - 175.0) / 60.0)) * present * (0.35 + 0.65 * _Weather.x), ComputeFogFactor(o.positionCS.z));
""",
"""                o.fade = half2(saturate((away - 60.0) / 55.0) * (1.0 - saturate((away - 175.0) / 60.0)) * present * (0.35 + 0.65 * _Weather.x), ComputeFogFactor(o.positionCS.z));
                // a curtain faded to nothing (the near cards, over a large part of the frame, were drawn at alpha 0): a point
                if (o.fade.x <= 0.0) o.positionCS = TransformWorldToHClip(float3(centre.x, _Weather.w, centre.y));
"""),
    ])

if 'water' in groups:
    edit(S + 'TWWater.hlsl', [
("""// WaterRings.cs owns _TWRings / _TWNow; with none in the scene every ring is long dead and the loop adds nothing.
""",
"""// WaterRings.cs owns _TWRings / _TWRingCount / _TWNow; with none in the scene the count is 0 and the loop does not run.
"""),
("""float4 _TWRings[TW_RING_COUNT];   // x, z, start time, size in metres
float _TWNow;
""",
"""float4 _TWRings[TW_RING_COUNT];   // x, z, start time, size in metres: the live rings first, in the order of their slots
float _TWRingCount;               // how many are live (WaterRings.PackLive): a pixel of water ran all sixteen, mostly for none
float _TWNow;
"""),
("""    for (int k = 0; k < TW_RING_COUNT; k++)
    {
        float4 r = _TWRings[k];""",
"""    int count = min((int)_TWRingCount, TW_RING_COUNT);
    for (int k = 0; k < count; k++)
    {
        float4 r = _TWRings[k];"""),
    ])

if 'watercs' in groups:
    edit('Assets/_Project/Presentation/Terrain/WaterRings.cs', [
("""        readonly Vector4[] rings = new Vector4[Count];
""",
"""        readonly Vector4[] rings = new Vector4[Count], live = new Vector4[Count];
"""),
("""        static readonly int RingsId = Shader.PropertyToID("_TWRings"), NowId = Shader.PropertyToID("_TWNow");
""",
"""        static readonly int RingsId = Shader.PropertyToID("_TWRings"), NowId = Shader.PropertyToID("_TWNow"), CountId = Shader.PropertyToID("_TWRingCount");
"""),
("""            Awake(); Shader.SetGlobalVectorArray(RingsId, rings);
""",
"""            Awake(); Shader.SetGlobalVectorArray(RingsId, rings); Shader.SetGlobalFloat(CountId, 0f);
"""),
("""            Shader.SetGlobalVectorArray(RingsId, rings);
            Shader.SetGlobalFloat(NowId, Time.time);
        }
""",
"""            // the live rings only, so a pixel of water loops over those instead of all sixteen (TWWater.hlsl)
            Shader.SetGlobalFloat(CountId, PackLive(rings, Time.time, live));
            Shader.SetGlobalVectorArray(RingsId, live);
            Shader.SetGlobalFloat(NowId, Time.time);
        }

        /// <summary>
        /// The rings TWWater.hlsl would draw at `now`, copied to the front of `into` in slot order; returns how many. A
        /// ring that is not live adds exactly nothing to the shader's sum, so leaving it out and keeping the others in
        /// their order changes no pixel. The test is the shader's own, with a margin: a ring on the edge of its life is
        /// kept, because keeping a dead one costs a loop and dropping a live one would change the picture.
        /// </summary>
        public static int PackLive(Vector4[] rings, float now, Vector4[] into)
        {
            int n = 0;
            for (int k = 0; k < rings.Length; k++)
            {
                var r = rings[k];
                float age = (now - r.z) / (0.9f + r.w * 0.4f);
                if (r.w < 0.009f || age < -0.001f || age > 1.001f) continue;
                into[n++] = r;
            }
            for (int k = n; k < into.Length; k++) into[k] = new Vector4(0f, 0f, -1000f, 0f);
            return n;
        }
"""),
    ])
