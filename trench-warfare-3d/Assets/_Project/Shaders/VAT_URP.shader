// Phase: B3 (implemented), C1 (clip atlas) — Vertex Animation Texture shader for URP, drawn with Graphics.RenderMeshIndirect.
// Atlas layout (VATBaker / ProceduralSoldier / VatCodec): U = vertex index / VertexCount, V = frame / TotalFrames;
// positions RGBA64 quantised over _PosMin.._PosMin+_PosSize, normals RGBA32 (0..1 -> -1..1). Per-instance data is a
// 48-byte record in a StructuredBuffer filled by VATRenderer: the clip playing (row, 0..1 through it), the clip fading
// out (prevRow, prevT, blend = its weight) and the yaw. A row whose frame count is negative plays once and holds its
// last frame; a positive one loops. _Lerp = 1 blends two frames (near), 0 samples the nearest frame (zoomed out).
// Vertex colour: rgb = albedo, a = 1 where the team colour multiplies it.
// Look: the same two-step cartoon light, haze and ink outline as TW/Toon, so the men sit in the painted field.
Shader "TW/VAT Infantry (URP)"
{
    Properties
    {
        _PosTex ("VAT Position (RGBAHalf)", 2D) = "black" {}
        _NrmTex ("VAT Normal (RGBAHalf)", 2D) = "black" {}
        _VertexCount ("Vertex Count", Float) = 264
        _TotalFrames ("Total Frames", Float) = 288
        _Lerp ("Frame Lerp (1 near, 0 far)", Range(0,1)) = 1
        _TeamColorA ("Team 0 cloth", Color) = (0.64, 0.56, 0.34, 1)
        _TeamColorB ("Team 1 cloth", Color) = (0.25, 0.29, 0.32, 1)
        _OutlineColor ("Outline", Color) = (0.13, 0.10, 0.08, 1)
        _OutlineWidth ("Outline width (m)", Float) = 0.028
        _WoundCenter ("Wound Ellipsoid Center", Vector) = (0,0,0,0)
        _WoundRadii ("Wound Ellipsoid Radii", Vector) = (0,0,0,0)
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #define UNITY_INDIRECT_DRAW_ARGS IndirectDrawIndexedArgs
        #include "UnityIndirect.cginc"

        struct VatInstance { float3 pos; float yaw; float animRow; float animT; float tint; float scale; float prevRow; float prevT; float blend; float pad; };
        StructuredBuffer<VatInstance> _Instances;
        StructuredBuffer<float2> _RowTable;   // x = row start frame, y = row frame count (negative: play once and hold)

        TEXTURE2D(_PosTex); SAMPLER(sampler_PosTex);
        TEXTURE2D(_NrmTex); SAMPLER(sampler_NrmTex);
        CBUFFER_START(UnityPerMaterial)
            float _VertexCount, _TotalFrames, _Lerp;
            float4 _PosMin, _PosSize;
            float4 _TeamColorA, _TeamColorB;
            float4 _WoundCenter, _WoundRadii;
            float4 _OutlineColor; float _OutlineWidth;
        CBUFFER_END
        #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"   // ground mist and the quiet fog, as on the field

        struct Animated { float3 positionOS; float3 positionWS; float3 normalWS; float tint; float scale; };

        // one clip: the frame pair at t (0..1 through the row) and the blend between them
        void SampleClip(float u, float rowIndex, float t, out float3 p, out float3 n)
        {
            float2 row = _RowTable[(uint)rowIndex];
            float count = max(1.0, abs(row.y));
            bool loops = row.y > 0.0;
            float local = (loops ? frac(t) : saturate(t)) * count;
            float f0 = min(floor(local), count - 1.0);
            float f1 = loops ? fmod(f0 + 1.0, count) : min(f0 + 1.0, count - 1.0);
            float v0 = (row.x + f0 + 0.5) / _TotalFrames, v1 = (row.x + f1 + 0.5) / _TotalFrames;
            float w = saturate(local - f0) * _Lerp;
            p = lerp(SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, v0), 0).xyz,
                     SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, v1), 0).xyz, w);
            n = lerp(SAMPLE_TEXTURE2D_LOD(_NrmTex, sampler_NrmTex, float2(u, v0), 0).xyz,
                     SAMPLE_TEXTURE2D_LOD(_NrmTex, sampler_NrmTex, float2(u, v1), 0).xyz, w);
        }

        Animated Animate(uint vertexID, uint svInstanceID)
        {
            InitIndirectDrawArgs(0);
            VatInstance inst = _Instances[GetIndirectInstanceID(svInstanceID)];
            float u = (vertexID + 0.5) / _VertexCount;
            float3 p, n;
            SampleClip(u, inst.animRow, inst.animT, p, n);
            if (inst.blend > 0.001)   // the clip on its way out, while the cross-fade lasts
            {
                float3 pp, pn;
                SampleClip(u, inst.prevRow, inst.prevT, pp, pn);
                p = lerp(p, pp, inst.blend); n = lerp(n, pn, inst.blend);
            }
            p = _PosMin.xyz + p * _PosSize.xyz;
            n = n * 2.0 - 1.0;
            float s = sin(inst.yaw), c = cos(inst.yaw);   // yaw turns +Z towards +X, as Quaternion.Euler(0, yaw, 0)
            Animated o;
            o.positionOS = p;
            p *= inst.scale;
            o.positionWS = inst.pos + float3(p.x * c + p.z * s, p.y, -p.x * s + p.z * c);
            o.normalWS = normalize(float3(n.x * c + n.z * s, n.y, -n.x * s + n.z * c));
            o.tint = inst.tint;
            o.scale = inst.scale;
            return o;
        }
        ENDHLSL

        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode"="UniversalForward" }
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _FORWARD_PLUS
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/_Project/Shaders/TWWater.hlsl"
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"

            struct Attributes { uint vertexID : SV_VertexID; half4 color : COLOR; };
            struct Varyings { float4 positionCS : SV_POSITION; half4 color : COLOR; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2; float tint : TEXCOORD3; float3 positionOS : TEXCOORD4; float fog : TEXCOORD5; };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                Animated a = Animate(v.vertexID, instanceID);
                Varyings o;
                o.positionOS = a.positionOS;
                o.positionWS = a.positionWS;
                o.positionCS = TransformWorldToHClip(a.positionWS);
                o.normalWS = a.normalWS;
                o.color = v.color;
                o.tint = a.tint;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                // B4: ellipsoid wound clip exposes embedded gore geometry
                if (_WoundRadii.x > 0.0)
                {
                    float3 d = (i.positionOS - _WoundCenter.xyz) / _WoundRadii.xyz;
                    clip(dot(d, d) - 1.0);
                }
                half3 team = lerp(_TeamColorA.rgb, _TeamColorB.rgb, i.tint);
                half3 albedo = lerp(i.color.rgb, i.color.rgb * team, i.color.a);
                // webbing, pack and puttees were baked near white, so from the gameplay pitch a man read as a stack of pale
                // boxes with his kit brighter than his helmet: the brightest cloth is brought back towards the uniform
                half kit = dot(albedo, half3(0.3, 0.59, 0.11));
                albedo *= lerp(1.0, 0.52 / max(kit, 0.001), saturate((kit - 0.52) * 4.0));
                // the trench is on every man: boots and shins are caked with mud (a ragged line, higher when the ground is
                // soaked), and a man who crawls is muddy all over
                half ragged = frac(sin(dot(floor(i.positionOS.xz * 38.0 + i.positionOS.y * 11.0), float2(12.9898, 78.233))) * 43758.5453);
                half caked = (1.0 - smoothstep(0.06, 0.34 + 0.16 * _TWWet.x, i.positionOS.y + (ragged - 0.5) * 0.14)) * 0.78;
                albedo = lerp(albedo, half3(0.215, 0.170, 0.115) * (1.0 - 0.3 * _TWWet.x), caked);
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half lit = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;
                half3 color = albedo * 1.18 * lerp(half3(0.70, 0.72, 0.76) * lerp(half3(1, 1, 1), TWShadeTint(), 0.45), mainLight.color, band);   // men are lit a step above the field so they read in a shaded trench
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation);
                // at night the men must still read: the moon catches their edge (the rim only shows when a mood tints the shade)
                half rim = pow(1.0 - saturate(dot(normalize(i.normalWS), normalize(_WorldSpaceCameraPos - i.positionWS))), 2.2);
                color += (albedo * 0.6 + 0.10) * mainLight.color * rim * saturate(1.0 - dot(TWShadeTint(), half3(0.34, 0.33, 0.33))) * 1.4;
                half3 lampGlint;
                color += albedo * 1.18 * TWLocalLights(i.positionWS, normalize(i.normalWS), i.positionCS, normalize(_WorldSpaceCameraPos - i.positionWS), 0.25 * _TWWet.x, lampGlint);
                color += lampGlint;   // wet helmets and shoulders catch the lamps   // a muzzle flash lights the man behind it
                if (_TWClose > 0.0 && _TWWet.x > 0.0)
                {
                    // up close the rain is on him too: helmet and shoulders carry a film that mirrors the sky, and drops burst on them
                    float3 nrm = normalize(i.normalWS);
                    half top = saturate(nrm.y * 1.8 - 0.5) * smoothstep(1.15, 1.45, i.positionOS.y);
                    half nearMan = _TWClose * (1.0 - saturate((distance(_WorldSpaceCameraPos, i.positionWS) - 14.0) / 16.0));
                    half edge = pow(1.0 - saturate(dot(nrm, normalize(_WorldSpaceCameraPos - i.positionWS))), 2.0);
                    color = lerp(color, TWSky(), top * _TWWet.x * (0.10 + 0.30 * edge) * nearMan);
                    if (_TWWet.z > 0.0) color += TWRainSplash(i.positionWS.xz * 3.1 + i.positionOS.y) * top * nearMan * (TWSky() * 0.9 + mainLight.color * 0.2);
                }
                color = ApplyMist(color, i.positionWS);
                color = ApplyFieldFog(color, i.positionWS);
                return half4(MixFog(color, i.fog), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "Outline"
            Tags { "LightMode"="SRPDefaultUnlit" }
            Cull Front
            HLSLPROGRAM
            #pragma vertex vertOutline
            #pragma fragment fragOutline
            #pragma multi_compile_fog
            struct OutlineVaryings { float4 positionCS : SV_POSITION; float fog : TEXCOORD0; };
            OutlineVaryings vertOutline(uint vertexID : SV_VertexID, uint instanceID : SV_InstanceID)
            {
                Animated a = Animate(vertexID, instanceID);
                OutlineVaryings o;
                float w = TransformWorldToHClip(a.positionWS).w;
                // the line is extruded in world space, so it follows how big the man is drawn as well as how far off he is
                // (the back rank of a standard frame is 149 m out and used to lose its outline entirely)
                float width = _OutlineWidth * a.scale * clamp(w / 25.0, 1.0, 6.0);
                o.positionCS = TransformWorldToHClip(a.positionWS + a.normalWS * width);
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }
            half4 fragOutline(OutlineVaryings i) : SV_Target { return half4(MixFog(_OutlineColor.rgb, i.fog), 1.0); }
            ENDHLSL
        }

        Pass
        {
            Name "ShadowCaster"
            Tags { "LightMode"="ShadowCaster" }
            ZWrite On ZTest LEqual ColorMask 0
            HLSLPROGRAM
            #pragma vertex vertShadow
            #pragma fragment fragNull
            #pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"   // LerpWhiteTo, which Shadows.hlsl uses
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"

            float3 _LightDirection;
            float3 _LightPosition;

            float4 vertShadow(uint vertexID : SV_VertexID, uint instanceID : SV_InstanceID) : SV_POSITION
            {
                Animated a = Animate(vertexID, instanceID);
            #if _CASTING_PUNCTUAL_LIGHT_SHADOW
                float3 lightDir = normalize(_LightPosition - a.positionWS);
            #else
                float3 lightDir = _LightDirection;
            #endif
                float4 cs = TransformWorldToHClip(ApplyShadowBias(a.positionWS, a.normalWS, lightDir));
            #if UNITY_REVERSED_Z
                cs.z = min(cs.z, UNITY_NEAR_CLIP_VALUE);
            #else
                cs.z = max(cs.z, UNITY_NEAR_CLIP_VALUE);
            #endif
                return cs;
            }
            half4 fragNull() : SV_Target { return 0; }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R
            HLSLPROGRAM
            #pragma vertex vertDepth
            #pragma fragment fragDepth
            float4 vertDepth(uint vertexID : SV_VertexID, uint instanceID : SV_InstanceID) : SV_POSITION
            {
                return TransformWorldToHClip(Animate(vertexID, instanceID).positionWS);
            }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
