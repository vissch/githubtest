// Phase: B2 (implemented) — what is pressed into the mud: a boot print, a length of tank rut, or the pad a walking
// machine puts its weight on, one quad each, laid on the ground by CombatFx and drawn instanced. A boot print is a
// close-camera thing and fades within a few tens of metres; what a machine leaves is metres across and keeps its own
// far longer fade (_FadeFrom/_FadeOver), so the ruts and the footfalls are still on the ground at the standard view. The shape is computed (uv: x across, y along
// the direction of travel), the mark darkens the ground and, when the field is wet, holds a film of water that mirrors
// the sky at a low angle. One material a shape draws every mark of it at every age: the instance carries its own fade
// in its object Y scale, which a flat quad has no other use for, so the mark thins out continuously instead of
// stepping through three materials. What it costs is scaled to what can be seen of it - see _DetailFrom below.
Shader "TW/GroundMark (URP)"
{
    Properties
    {
        _Shape ("Shape (0 boot print, 1 track rut, 2 walker foot)", Float) = 0
        _Alpha ("Strength", Range(0,1)) = 0.6
        _Color ("Pressed mud", Color) = (0.035, 0.030, 0.026, 1)
        _FadeFrom ("Fades out from (m)", Float) = 30
        _FadeOver ("Fades out over (m)", Float) = 12
        _DetailFrom ("Fine detail holds to (m)", Float) = 14
        _DetailOver ("Fine detail goes over (m)", Float) = 20
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent-40" "IgnoreProjector"="True" }
        Pass
        {
            Name "GroundMark"
            Tags { "LightMode"="UniversalForward" }
            Blend SrcAlpha OneMinusSrcAlpha
            ZWrite Off
            Offset -1, -1
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_instancing
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"
            CBUFFER_START(UnityPerMaterial)
                half4 _Color;
                float _Shape, _Alpha, _FadeFrom, _FadeOver, _DetailFrom, _DetailOver;
            CBUFFER_END
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; UNITY_VERTEX_INPUT_INSTANCE_ID };
            // fade.x: how much of the mark survives at this range, the instance's own age already multiplied in.
            // fade.y: how much fine detail is worth computing. Both are per-vertex - a mark is a quad a metre or two
            // across, so interpolating them across it costs nothing and takes a distance() and two divides off every
            // pixel of every mark on the field.
            struct Varyings { float4 positionCS : SV_POSITION; float2 uv : TEXCOORD0; float3 positionWS : TEXCOORD1; float fog : TEXCOORD2; float2 fade : TEXCOORD3; };
            Varyings vert(Attributes v)
            {
                UNITY_SETUP_INSTANCE_ID(v);
                Varyings o;
                o.positionWS = TransformObjectToWorld(v.positionOS.xyz);
                o.positionCS = TransformWorldToHClip(o.positionWS);
                o.uv = v.uv;
                o.fog = ComputeFogFactor(o.positionCS.z);
                // The instance carries its own age in its object Y scale (the quad is flat, so nothing else uses it):
                // one material a shape draws marks at every stage of fading, instead of three materials a shape
                // stepping the alpha in visible jumps. The Y axis length is the scale whatever the ground tilt.
                float age = length(float3(UNITY_MATRIX_M._m01, UNITY_MATRIX_M._m11, UNITY_MATRIX_M._m21));
                float d = distance(_WorldSpaceCameraPos, o.positionWS);
                o.fade.x = age * (1.0 - saturate((d - _FadeFrom) / max(1.0, _FadeOver)));
                o.fade.y = 1.0 - saturate((d - _DetailFrom) / max(1.0, _DetailOver));
                return o;
            }
            half4 frag(Varyings i) : SV_Target
            {
                // What the mark is worth spending on. The uniform band above is the artistic call; the screen-space
                // derivative is the honest one - the moment one period of a fine pattern is down to a pixel it can only
                // shimmer, so it is faded out on its own. That is what lets the close mark carry detail at all: it is
                // paid for only where it can be seen, and it scales itself to the resolution rather than to a guess.
                half detail = min(i.fade.y, saturate(1.0 - 26.0 * max(fwidth(i.uv.x), fwidth(i.uv.y))));
                half soft = 1.0 - detail;   // and what is left over: a mark a few pixels across must read as a smudge
                float2 p = i.uv * 2.0 - 1.0;
                half shape, pooled;   // pooled: the part pressed deepest, where the water stands
                if (_Shape < 0.5)
                {
                    // a boot: the sole ahead, the heel behind, a narrow waist between
                    half sole = 1.0 - smoothstep(0.75 - soft * 0.20, 1.0, length(float2(p.x / 0.92, (p.y - 0.28) / 0.70)));
                    half heel = 1.0 - smoothstep(0.75 - soft * 0.20, 1.0, length(float2(p.x / 0.74, (p.y + 0.66) / 0.33)));
                    shape = max(sole, heel);
                    if (detail > 0.004)
                    {
                        // near enough to read the boot itself: the cleats barred across the sole, and the ring the
                        // heel's edge cuts round the softer ground it holds
                        half bar = frac(i.uv.y * 9.0);
                        half cleat = smoothstep(0.38, 0.52, bar) * (1.0 - smoothstep(0.86, 1.0, bar));
                        half ring = smoothstep(0.20, 0.60, heel) * (1.0 - smoothstep(0.76, 1.0, heel));
                        shape *= lerp(1.0, saturate(0.70 + 0.30 * cleat + 0.38 * ring), detail);
                    }
                    pooled = smoothstep(0.55, 0.95, shape);
                }
                else if (_Shape < 1.5)
                {
                    // a rut: a soft-edged band with the track's cross bars pressed into it
                    half band = (1.0 - smoothstep(0.62 - soft * 0.22, 1.0, abs(p.x))) * (1.0 - smoothstep(0.86, 1.0, abs(p.y)));
                    half groove = smoothstep(0.42, 0.5, frac(i.uv.y * 4.0)) * (1.0 - smoothstep(0.92, 1.0, frac(i.uv.y * 4.0)));   // the track's cross bars
                    shape = band * (0.72 + 0.28 * groove);
                    half bar = frac(sin(floor(i.uv.y * 4.0 + i.positionWS.z * 1.7) * 12.9898) * 43758.5453);   // no two bars hold the same water
                    pooled = band * groove * (0.25 + 0.55 * bar);
                    if (detail > 0.004)
                    {
                        // close up a rut is not one band but the plates that made it: the grousers' ends score lines
                        // along it, and the mud they pushed aside stands in a torn shoulder just outside the track
                        // The scoring is stepped along the rut rather than run straight down it: plates that line up
                        // bar for bar make a lattice, and a lattice reads as a grating laid on the ground instead of a
                        // track pressed into it. It also only modulates the rut - scoring it as hard as the cross bars
                        // lightened the whole band and cost the track its depth, which is the one thing it must have.
                        half plate = frac(i.uv.x * 5.0 + floor(i.uv.y * 4.0) * 0.37 + i.positionWS.x * 0.11);
                        half score = smoothstep(0.30, 0.44, plate) * (1.0 - smoothstep(0.78, 0.94, plate));
                        half shoulder = smoothstep(0.52, 0.70, abs(p.x)) * (1.0 - smoothstep(0.86, 1.02, abs(p.x)))
                                      * (0.55 + 0.45 * frac(sin(floor(i.uv.y * 11.0) * 78.233) * 43758.5453));
                        shape = saturate(shape * lerp(1.0, saturate(0.88 + 0.16 * score), detail) + detail * 0.26 * shoulder);
                    }
                }
                else
                {
                    // a walking machine's foot: a broad pad driven into the ground with the toes of the claw ahead of
                    // it. Nothing about it is a tread - the weight goes through one point, so the pad is deep and its
                    // edge is sharp where the ground was pushed up, and the water stands over most of it.
                    float2 q = float2(p.x / 0.86, (p.y + 0.16) / 0.78);
                    float rr = length(q);
                    // A perfect ellipse is what makes a print read as a rubber stamp rather than as a hole something
                    // stood in, and at this size the eye has no trouble telling the two apart. Close up the edge is
                    // walked round by a few waves, anchored to where the mark is so no two feet break up alike.
                    if (detail > 0.004)
                    {
                        float a = atan2(q.y, q.x);
                        rr *= 1.0 + detail * (0.034 * sin(a * 5.0 + i.positionWS.x * 1.3) + 0.021 * sin(a * 11.0 + i.positionWS.z * 2.1));
                    }
                    half pad = 1.0 - smoothstep(0.62 - soft * 0.18, 0.92, rr);
                    half toes = 0.0, gouge = 0.0;
                    [unroll] for (int t = -1; t <= 1; t++)
                    {
                        float2 c = float2(t * 0.52, 0.74);
                        toes = max(toes, 1.0 - smoothstep(0.55, 1.0, length((p - c) / float2(0.30, 0.34))));
                        // and, once the camera is in close, the furrow the claw cut as the leg came down and the
                        // machine's weight dragged it through. It runs from under the pad out past the toe, which is
                        // the length of quad there is: centred on the toe it would be half outside the mark and clipped.
                        if (detail > 0.004)
                            gouge = max(gouge, 1.0 - smoothstep(0.28, 1.0, length((p - float2(t * 0.52, 0.62)) / float2(0.085, 0.52))));
                    }
                    shape = saturate(max(pad, toes * 0.85));
                    // the rim: the ground stood up round the pad, so the mark is darkest just inside its edge
                    half rim = smoothstep(0.30, 0.68, shape) * (1.0 - smoothstep(0.78, 0.98, shape));
                    shape = saturate(shape * (0.82 + 0.34 * rim));
                    if (detail > 0.004)
                    {
                        // the pad's floor is not smooth either: ground under that much weight cracks apart under it
                        // broken into plates rather than diced into a grid, and only where the pad is actually bearing
                        half2 cell = half2(i.uv.x * 6.0, i.uv.y * 6.0 + i.uv.x * 2.3);
                        half crack = frac(sin(floor(cell.x) * 12.9898 + floor(cell.y) * 78.233) * 43758.5453);
                        shape = saturate(shape * lerp(1.0, 0.90 + 0.14 * crack, detail * pad) + detail * 0.42 * gouge);
                    }
                    pooled = smoothstep(0.45, 0.92, pad);
                }
                // pressed mud is matt and dark (it kills the glitter of the wet ground round it); what stands in the deepest
                // part is water, and mirrors the sky at the low angle a close camera looks from
                // _Color is PRESSED MUD, near black, and on a white field that reads as a hole punched through the
                // snow rather than as a footprint. Snow that has been trodden is darker and bluer than the field
                // beside it and nowhere near black, so the mark crosses to a cool grey with the snow amount and is
                // left exactly as it was on every other field.
                // A TRODDEN MARK IS THE SNOW ITSELF, DARKER - not a colour laid over it. The first attempt
                // crossed _Color toward a fixed grey and kept the x3 lift that exists to raise near-black
                // mud, which on a pale field produced wide translucent BLUE BANDS sitting on the snow rather
                // than prints pressed into it. Taking the biome's own snow colour and simply darkening it
                // keeps the hue of the ground the mark is in, which is what makes it read as a depression.
                half snowy = saturate(_TWSnow.x);
                half3 mud = _Color.rgb * TWShadeTint() * 3.0;
                half3 trodden = _TWSnowColor.rgb * TWShadeTint() * 0.62;
                half3 color = lerp(mud, trodden, snowy);
                // Standing water is a close thing twice over: it wants a view vector per pixel, and at range a rut is
                // thinner than a pixel, so a sky mirror in it is a white speckle crawling over the field. It keeps a
                // little of itself far out and comes fully in as the camera does.
                if (_TWWet.x > 0.01)
                {
                    float3 view = normalize(_WorldSpaceCameraPos - i.positionWS);
                    half fresnel = pow(1.0 - saturate(view.y), 3.0);
                    color = lerp(color, TWSky() * 1.15, pooled * (0.18 + 0.82 * fresnel) * _TWWet.x * (0.30 + 0.70 * detail));
                }
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                half alpha = shape * _Alpha * i.fade.x;
                return half4(MixFog(color, i.fog), alpha);
            }
            ENDHLSL
        }
    }
}
