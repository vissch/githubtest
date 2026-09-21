// Phase: B3 (implemented) — Vertex Animation Texture shader for URP, drawn with Graphics.RenderMeshIndirect.
// Atlas layout (VATBaker / ProceduralSoldier): U = vertex index / VertexCount, V = frame / TotalFrames, RGBAHalf,
// object-space position and signed normal. Per-instance data is a 32-byte record in a StructuredBuffer filled by
// VATRenderer. _Lerp = 1 blends two frames (near), 0 samples the nearest frame (zoomed out). Loops wrap inside a row.
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
        _TeamColorA ("Team 0 cloth", Color) = (0.47, 0.40, 0.24, 1)
        _TeamColorB ("Team 1 cloth", Color) = (0.34, 0.38, 0.40, 1)
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

        struct VatInstance { float3 pos; float yaw; float animRow; float animT; float tint; float scale; };
        StructuredBuffer<VatInstance> _Instances;
        StructuredBuffer<float2> _RowTable;   // x = row start frame, y = row frame count (AnimRow order)

        TEXTURE2D(_PosTex); SAMPLER(sampler_PosTex);
        TEXTURE2D(_NrmTex); SAMPLER(sampler_NrmTex);
        CBUFFER_START(UnityPerMaterial)
            float _VertexCount, _TotalFrames, _Lerp;
            float4 _TeamColorA, _TeamColorB;
            float4 _WoundCenter, _WoundRadii;
            float4 _OutlineColor; float _OutlineWidth;
        CBUFFER_END
        float4 _TWMist, _TWMistColor;   // ground mist, as in TW/Toon (set by Atmosphere)

        struct Animated { float3 positionOS; float3 positionWS; float3 normalWS; float tint; };

        Animated Animate(uint vertexID, uint svInstanceID)
        {
            InitIndirectDrawArgs(0);
            VatInstance inst = _Instances[GetIndirectInstanceID(svInstanceID)];
            float2 row = _RowTable[(uint)inst.animRow];
            float local = frac(inst.animT) * row.y;
            float f0 = floor(local), f1 = fmod(f0 + 1.0, row.y);
            float u = (vertexID + 0.5) / _VertexCount;
            float v0 = (row.x + f0 + 0.5) / _TotalFrames, v1 = (row.x + f1 + 0.5) / _TotalFrames;
            float w = frac(local) * _Lerp;
            float3 p = lerp(SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, v0), 0).xyz,
                            SAMPLE_TEXTURE2D_LOD(_PosTex, sampler_PosTex, float2(u, v1), 0).xyz, w);
            float3 n = lerp(SAMPLE_TEXTURE2D_LOD(_NrmTex, sampler_NrmTex, float2(u, v0), 0).xyz,
                            SAMPLE_TEXTURE2D_LOD(_NrmTex, sampler_NrmTex, float2(u, v1), 0).xyz, w);
            float s = sin(inst.yaw), c = cos(inst.yaw);   // yaw turns +Z towards +X, as Quaternion.Euler(0, yaw, 0)
            Animated o;
            o.positionOS = p;
            p *= inst.scale;
            o.positionWS = inst.pos + float3(p.x * c + p.z * s, p.y, -p.x * s + p.z * c);
            o.normalWS = normalize(float3(n.x * c + n.z * s, n.y, -n.x * s + n.z * c));
            o.tint = inst.tint;
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
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile_fog
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

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
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                half lit = dot(normalize(i.normalWS), mainLight.direction) * 0.5 + 0.5;
                half band = smoothstep(0.32, 0.36, lit) * 0.5 + smoothstep(0.69, 0.74, lit) * 0.5;
                half3 color = albedo * lerp(half3(0.57, 0.60, 0.64), mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation);
                float mistFar = saturate((distance(_WorldSpaceCameraPos, i.positionWS) - _TWMist.z) * _TWMist.w);
                color = lerp(color, _TWMistColor.rgb, saturate((_TWMist.x - i.positionWS.y) * _TWMist.y) * mistFar * _TWMistColor.a);
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
                float width = _OutlineWidth * clamp(w / 40.0, 1.0, 2.2);   // hold the line's weight as the man gets smaller
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
