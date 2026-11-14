// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - BiWash: the soft warm light a machine-gun burst throws
// on the field, after the clip bullet_impacts_cel. Unlike Glow it is NOT banded: light has no edge. It is a LIGHT, not an
// added colour: pass 1 (Blend DstColor One) makes the result scene x (1 + light), so the wooden dummy flares and the
// fog/sky gain no haze (r04: the additive wash drew a white-cream haze); pass 2 (additive) adds a small fill of the same
// light so near-black ground still warms (the clip's lit foreground). One quad, ZTest Always, one draw a pass for every
// wash of the frame.
//   mode 0 (scene wash): the quad covers a light sphere; each pixel reads the scene depth, rebuilds the surface it shows
//     and is lit by the distance of that surface to the centre (squashed vertically), so the ground, crates, reeds and the
//     dummy all catch it and there is no disc and no ring. The camera may be inside the sphere.
//   mode 1 (spot): an eye-facing soft light on whatever the scene shows round its centre, hidden where the scene is in
//     front of it.
//   size.xyz   radius (m) at birth, at death, ease        size.w  decay power of the intensity (0: 1.5)
//   motion     gravity, drag, spin (unused), buoyancy
//   erode      unused
//   tone       x palette (its c3 is the light colour), y intensity (HDR), z opacity, w additive fill (0..1 of the
//              intensity; the recipe's Ink field; 0: none)
//   extra      x mode, y falloff power (0: 1.6), z vertical squash for mode 0 (0: 1; 2 = half as tall; -2 = half as
//              wide, a column), w attack 0..1 of
//              the life (0: 0.12)
Shader "TW/Cel/BiWash"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+12" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #pragma multi_compile_fog
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DeclareDepthTexture.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
        struct Varyings
        {
            float4 positionCS : SV_POSITION;
            float2 uv : TEXCOORD0;                          // -1..1 (spot)
            nointerpolation float4 tone : TEXCOORD1;        // x palette, y intensity x envelope, z opacity, w radius
            nointerpolation float4 shape : TEXCOORD2;       // x mode, y falloff, z squash, w centre eye depth
            nointerpolation float4 centre : TEXCOORD3;      // xyz centre, w additive fill
        };

        Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float radius = max(CelCurve(r.size.xyz, k), 1e-3);
            float3 c = CelPosition(r, t);
            float2 corner = (v.uv * 2.0 - 1.0) * alive;
            float mode = r.extra.x;
            float3 cam = _WorldSpaceCameraPos;
            float3 toEye = cam - c;
            float dc = length(toEye);
            float3 w;
            if (mode < 0.5)
            {
                float squash = r.extra.z > 0.0 ? r.extra.z : 1.0;
                float reach = radius * max(1.0, 1.0 / squash) * 1.15;
                if (dc < reach + 0.6)
                {
                    // the eye is in (or at the rim of) the light: a quad over the whole view, a metre ahead
                    float3 fwd = -UNITY_MATRIX_V[2].xyz;
                    w = cam + fwd * 1.0 + (UNITY_MATRIX_V[0].xyz * corner.x + UNITY_MATRIX_V[1].xyz * corner.y) * 3.0;
                }
                else w = CelBillboard(c + toEye / dc * reach, corner * reach * 1.25);
            }
            else w = CelBillboard(c, corner * radius);
            float att = r.extra.w > 0.0 ? r.extra.w : 0.12;
            float decay = r.size.w > 0.0 ? r.size.w : 1.5;
            float env = saturate(k / att) * pow(saturate((1.0 - k) / (1.0 - att)), decay);
            Varyings o;
            o.positionCS = TransformWorldToHClip(w);
            o.uv = corner;
            o.tone = float4(r.tone.x, r.tone.y * env * alive, r.tone.z, radius);
            o.shape = float4(mode, r.extra.y > 0.0 ? r.extra.y : 1.6, abs(r.extra.z) > 1e-4 ? r.extra.z : 1.0, -TransformWorldToView(c).z);
            o.centre = float4(c, r.tone.w);
            return o;
        }

        // the light this pixel's surface receives (linear, HDR), fogged toward nothing
        half3 Light(Varyings i, float gain)
        {
            float2 suv = GetNormalizedScreenSpaceUV(i.positionCS);
            float raw = SampleSceneDepth(suv);
            float field;
            float3 ws;
            if (i.shape.x < 0.5)
            {
                ws = ComputeWorldSpacePosition(suv, raw, UNITY_MATRIX_I_VP);
                float3 d = ws - i.centre.xyz;
                // b04: a negative squash narrows the light sideways instead: a column round the dummy that lights
                // the board and not the dust beside it
                if (i.shape.z > 0.0) d.y *= i.shape.z; else d.xz *= -i.shape.z;
                field = saturate(1.0 - length(d) / i.tone.w);
            }
            else
            {
                float sceneEye = LinearEyeDepth(raw, _ZBufferParams);
                float hidden = saturate((i.shape.w - sceneEye) / max(i.tone.w, 0.05));   // the scene is in front of the centre
                field = saturate(1.0 - length(i.uv)) * (1.0 - hidden);
                ws = i.centre.xyz;
            }
            field = field * field * (3.0 - 2.0 * field);      // smooth: no edge
            float f = pow(field, i.shape.y);
            half3 col = CelPal(CelRowA(i.tone.x), 3) * (f * i.tone.y * i.tone.z * gain);
            float fog = ComputeFogFactor(TransformWorldToHClip(ws).z);
            return CelAdditiveFog(col, ws, fog);
        }
        ENDHLSL

        Pass
        {
            Name "CelBiWash"
            Tags { "LightMode"="UniversalForward" }
            Blend DstColor One
            ZWrite Off ZTest Always Cull Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment fragLight
            half4 fragLight(Varyings i) : SV_Target
            {
                if (i.tone.y <= 1e-4) discard;
                return half4(Light(i, 1.0), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "CelBiWashFill"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Blend One One
            ZWrite Off ZTest Always Cull Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment fragFill
            half4 fragFill(Varyings i) : SV_Target
            {
                if (i.tone.y <= 1e-4 || i.centre.w <= 1e-4) discard;
                return half4(Light(i, i.centre.w), 1.0);
            }
            ENDHLSL
        }
    }
}
