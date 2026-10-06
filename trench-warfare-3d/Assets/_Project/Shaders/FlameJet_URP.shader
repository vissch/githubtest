// Phase: C1 (unit look, look-06) - the flamethrower's stream as ONE LONG CARD.
//
// Three rounds of the jet were a CHAIN: short overlapping flipbook cards laid along the aim. Every one of them read,
// on the master's own pictures, as "a thin pale stick from the muzzle and a separate blob of orange fire a few metres
// out, with no taper joining them". A chain cannot help that - each link closes its own silhouette, and what the eye
// is given is a row of objects rather than one gesture. So the stream is now geometry: a ribbon strip built from the
// nozzle to the target (FlameJetCard), and this shader paints fire onto it.
//
// Nothing here is sampled from the fire pack. The flame is computed the way TW/Flame (URP) computes a torch - two
// reads of a small repeating value noise, scrolling at different speeds - with one difference that is the whole point:
// the noise scrolls ALONG the ribbon's length (u), not up a card, so the stream reads as fuel travelling. One of the
// two reads eats the edge, so the contour is ragged and the card boundary is never visible as a straight side.
//
// Additive and over-bright, like every other fire in the game, so the bloom takes the core.
// Vertex data: position = the rim point in WORLD space (the CPU has the camera and lays the rims; see
// FlameJetCard.Across), uv0 = (v across, -1..1; u along, 0 at the mouth to 1 at the head),
// uv1 = (half width in metres, scroll phase, alpha, glow).
Shader "TW/Flame Jet (URP)"
{
    Properties
    {
        _Noise ("Noise (R)", 2D) = "gray" {}
        _Strength ("Strength", Float) = 2.8
    }
    SubShader
    {
        Tags { "RenderType"="Transparent" "RenderPipeline"="UniversalPipeline" "Queue"="Transparent+35" "IgnoreProjector"="True" }
        Pass
        {
            Name "FlameJet"
            Tags { "LightMode"="UniversalForward" }
            Blend One One
            ZWrite Off Cull Off
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            TEXTURE2D(_Noise); SAMPLER(sampler_Noise);
            CBUFFER_START(UnityPerMaterial)
                float4 _Noise_ST;
                float _Strength;
            CBUFFER_END

            struct Attributes { float4 positionOS : POSITION; float2 vu : TEXCOORD0; float4 shape : TEXCOORD1; };
            struct Varyings { float4 positionCS : SV_POSITION; float2 vu : TEXCOORD0; float4 shape : TEXCOORD1; };

            Varyings vert(Attributes v)
            {
                Varyings o;
                o.positionCS = TransformWorldToHClip(TransformObjectToWorld(v.positionOS.xyz));
                o.vu = v.vu;
                o.shape = v.shape;
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                float v = i.vu.x, u = saturate(i.vu.y), t = _Time.y, phase = i.shape.y;
                float av = abs(v);
                // Along the LENGTH, at two speeds and two scales. The slow read is the body of the gout, the fast one
                // the tearing at its skin. Both travel toward the head, which is what makes a still frame of this read
                // as something thrown rather than something painted. The frequencies are HIGH on purpose: the first
                // cut of this sampled the noise at about one tile over the whole run, which varies so slowly that the
                // ribbon came back as a flat white slab with a straight top and bottom - a laser, not fire.
                half slow = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(u * 2.10 - t * 1.10 + phase, v * 0.50 + phase * 0.7)).r;
                half fast = SAMPLE_TEXTURE2D(_Noise, sampler_Noise, float2(u * 5.60 - t * 2.40 + phase * 2.3, v * 1.15 + 0.37)).r;
                half tear = slow * 0.60 + fast * 0.40;
                // The DRAWN edge sits well inside the mesh's rim and the noise eats into it, so the silhouette is
                // ragged all the way along and the quad's straight side is never the contour. Soft-shouldered, not a
                // step: a hard threshold on an additive card that is already over-bright clips to white everywhere
                // inside it and the taper stops being visible at all.
                float rim = 0.40 + 0.58 * tear;
                half body = saturate((rim - av) / 0.34);
                body *= saturate(u * 25.0);                     // nothing drawn at the nozzle lip itself
                body *= 1.0 - smoothstep(0.70, 1.00, u);        // and the head FRAYS out rather than being cut off
                // A BRIGHT CORE and a darker ragged edge: heat is highest at v~0 and rises downrange, because fuel
                // burns hotter the longer it has been in the air.
                half core = saturate(1.0 - av / 0.50); core *= core;
                // By DAY against bright mud this measured a pale wash at a strength of 1.6: the whole stream sat
                // under the value of the lit ground behind it. Fire is not dimmer at noon, it is only harder to win
                // against, so the floor under the core comes up and the strength with it. The RAMP along the run
                // stays gentle - the mouth has to be a hard bright rod, not a faint root.
                half heat = body * (0.26 + 1.05 * core) * (0.78 + 0.45 * u) * (0.62 + 0.70 * slow);
                half3 colour = lerp(half3(1.0, 0.16, 0.03), half3(1.0, 0.48, 0.10), smoothstep(0.10, 0.35, heat));
                colour = lerp(colour, half3(1.0, 0.85, 0.42), smoothstep(0.35, 0.65, heat));
                colour = lerp(colour, half3(1.0, 0.98, 0.90), smoothstep(0.70, 0.95, heat));
                return half4(colour * heat * _Strength * i.shape.z * i.shape.w, 1.0);
            }
            ENDHLSL
        }
    }
}
