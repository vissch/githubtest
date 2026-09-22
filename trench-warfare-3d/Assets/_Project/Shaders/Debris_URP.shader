// Phase: B5 (implemented) — the pieces a blast throws, drawn with Graphics.RenderMeshIndirect from DebrisRenderer's
// records. Each record is written once; this shader works out where the piece is from the record and the clock
// (_DebrisNow, set by DebrisRenderer every frame from Time.time so a held capture holds the pieces too):
//   mode 0, an arc: p0 + v0 t - g t^2/2 until landT, then one bounce (0.45 of the sideways speed, 0.30 of the fall,
//           back up) and rest at landY; the tumble (axis, spin rad/s) runs through the flight, half speed through
//           the bounce, and the piece settles flat (yaw only) as it comes to rest;
//   mode 1, a hinge: the mesh's foot at p0, the pose rot0, turning about axis by spin radians over landT seconds,
//           accelerating as it goes (a tree top falling over);
// then, past life, sinking _Lift + 1.2 x scale into the mud over 3 s and gone (scale 0, no pixels).
// Look: the same two-step cartoon light, shade tint, haze and ink outline as TW/Toon, so a clod matches the ground it
// came out of; vertex colour is the piece's own shading, the record's tint its material. tint.a is how much it burns:
// embers in its seams that cool over the first half of its life (armour off a cooked-off tank).
Shader "TW/Debris (URP)"
{
    Properties
    {
        _ShadeColor ("Shade tint", Color) = (0.57, 0.60, 0.64, 1)
        _OutlineColor ("Outline", Color) = (0.13, 0.10, 0.08, 1)
        _OutlineWidth ("Outline width (pixels up close)", Float) = 2.0
        _EmberColor ("Ember colour", Color) = (2.6, 0.9, 0.22, 1)
        _Lift ("Rest height of this mesh (set per pool)", Float) = 0
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" "RenderPipeline"="UniversalPipeline" "Queue"="Geometry" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #define UNITY_INDIRECT_DRAW_ARGS IndirectDrawIndexedArgs
        #include "UnityIndirect.cginc"

        struct DebrisRecord { float3 p0; float born; float3 v0; float landT; float3 axis; float spin; float4 rot0; float4 tint; float scale; float life; float mode; float landY; };
        StructuredBuffer<DebrisRecord> _Records;
        float _DebrisNow;
        CBUFFER_START(UnityPerMaterial)
            half4 _ShadeColor, _OutlineColor, _EmberColor;
            float _OutlineWidth, _Lift;
        CBUFFER_END
        #include "Assets/_Project/Shaders/TWAtmosphere.hlsl"

        #define DEBRIS_G 9.8
        #define BOUNCE_KEEP 0.45
        #define BOUNCE_UP 0.30
        #define SINK_SECONDS 3.0
        #define SINK_DEPTH 1.2

        float4 QMul(float4 a, float4 b) { return float4(a.w * b.xyz + b.w * a.xyz + cross(a.xyz, b.xyz), a.w * b.w - dot(a.xyz, b.xyz)); }
        float3 QRotate(float4 q, float3 v) { return v + 2.0 * cross(q.xyz, cross(q.xyz, v) + q.w * v); }
        float4 QAxisAngle(float3 axis, float angle) { float h = angle * 0.5; return float4(axis * sin(h), cos(h)); }
        float4 QNlerp(float4 a, float4 b, float t) { if (dot(a, b) < 0.0) b = -b; return normalize(lerp(a, b, t)); }

        struct Placed { float3 positionWS; float3 normalWS; float3 smoothWS; float4 tint; float age; float life; float3 positionOS; };

        Placed Place(float3 positionOS, float3 normalOS, float3 smoothOS, uint svInstanceID)
        {
            InitIndirectDrawArgs(0);
            DebrisRecord r = _Records[GetIndirectInstanceID(svInstanceID)];
            float t = max(0.0, _DebrisNow - r.born);
            float3 p; float4 q;
            if (r.mode < 0.5)
            {
                float t1 = min(t, r.landT);
                p = r.p0 + r.v0 * t1 - float3(0, 0.5 * DEBRIS_G * t1 * t1, 0);
                float vy1 = r.v0.y - DEBRIS_G * r.landT;                       // the fall it lands with
                float3 v2 = float3(r.v0.x * BOUNCE_KEEP, -vy1 * BOUNCE_UP, r.v0.z * BOUNCE_KEEP);
                float T2 = max(0.0, 2.0 * v2.y / DEBRIS_G);
                float t2 = clamp(t - r.landT, 0.0, T2);
                p += v2 * t2 - float3(0, 0.5 * DEBRIS_G * t2 * t2, 0);
                if (t > r.landT) p.y = max(p.y, r.landY);
                float angle = r.spin * (t1 + 0.5 * t2);
                q = QMul(QAxisAngle(r.axis, angle), r.rot0);
                // as it comes to rest it settles onto the ground: the tumble gives way to a yaw-only pose
                float3 f = QRotate(q, float3(0, 0, 1));
                float yaw = atan2(f.x, f.z);
                float4 flat = float4(0, sin(yaw * 0.5), 0, cos(yaw * 0.5));
                float settle = saturate((t - r.landT) / max(T2, 0.25));
                q = QNlerp(q, flat, settle * settle);
            }
            else
            {
                float k = saturate(t / r.landT);
                float a = r.spin * (k * k);                                    // it goes over slowly at first and lands hard
                a += sin(saturate((t - r.landT) * 3.0) * 3.1416) * 0.06 * r.spin * saturate(1.0 - (t - r.landT));   // a small bounce back off the ground
                q = QMul(QAxisAngle(r.axis, a), r.rot0);
                p = r.p0;
            }
            float over = saturate((t - r.life) / SINK_SECONDS);
            float scale = r.scale * (over < 1.0 ? 1.0 : 0.0);
            p.y -= over * (_Lift + SINK_DEPTH) * r.scale;
            Placed o;
            o.positionOS = positionOS;
            o.positionWS = p + QRotate(q, positionOS * scale);
            o.normalWS = QRotate(q, normalOS);
            o.smoothWS = QRotate(q, dot(smoothOS, smoothOS) > 0.01 ? smoothOS : normalOS);
            o.tint = r.tint;
            o.age = t; o.life = r.life;
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
            #pragma multi_compile_fog
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile _ _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _FORWARD_PLUS
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/_Project/Shaders/TWLocalLights.hlsl"

            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; half4 color : COLOR; };
            struct Varyings { float4 positionCS : SV_POSITION; half4 color : COLOR; float3 normalWS : TEXCOORD1; float3 positionWS : TEXCOORD2; float4 tint : TEXCOORD3; float2 age : TEXCOORD4; float3 positionOS : TEXCOORD5; float fog : TEXCOORD6; };

            Varyings vert(Attributes v, uint instanceID : SV_InstanceID)
            {
                Placed a = Place(v.positionOS.xyz, v.normalOS, v.smoothOS, instanceID);
                Varyings o;
                o.positionWS = a.positionWS;
                o.positionCS = TransformWorldToHClip(a.positionWS);
                o.normalWS = a.normalWS;
                o.color = v.color;
                o.tint = a.tint;
                o.age = float2(a.age, a.life);
                o.positionOS = a.positionOS;
                o.fog = ComputeFogFactor(o.positionCS.z);
                return o;
            }

            half4 frag(Varyings i) : SV_Target
            {
                half3 albedo = i.color.rgb * i.tint.rgb;
                Light mainLight = GetMainLight(TransformWorldToShadowCoord(i.positionWS));
                float3 n = normalize(i.normalWS);
                if (_TWWet.x > 0.0) albedo *= 1.0 - 0.22 * _TWWet.x * saturate(n.y * 0.55 + 0.5);   // wet on top, like everything in the rain
                half wrap = dot(n, mainLight.direction) * 0.5 + 0.5;
                half band = smoothstep(0.32, 0.36, wrap) * 0.5 + smoothstep(0.69, 0.74, wrap) * 0.5;
                half3 color = albedo * lerp(_ShadeColor.rgb * TWShadeTint(), mainLight.color, band);
                color *= lerp(0.58, 1.0, mainLight.shadowAttenuation);
                half3 lampGlint;
                color += max(albedo, 0.16) * TWLocalLights(i.positionWS, n, i.positionCS, normalize(_WorldSpaceCameraPos - i.positionWS), 0.2 * _TWWet.x, lampGlint);
                color += lampGlint;
                color += albedo * TWBurstLight(i.positionWS);   // the burst that threw it lights it on the way up
                if (i.tint.a > 0.0)
                {
                    // hot metal: embers in the dark seams (a cellular pattern on the piece itself), flickering, cooling
                    // over the first half of its life and then gone
                    float3 cell = frac(i.positionOS * 7.0) - 0.5;
                    half seam = 1.0 - smoothstep(0.08, 0.20, min(min(abs(cell.x), abs(cell.y)), abs(cell.z)));
                    half heat = i.tint.a * pow(1.0 - saturate(i.age.x / max(0.5, i.age.y * 0.5)), 1.6);
                    half flicker = 0.75 + 0.25 * sin(_DebrisNow * 9.0 + dot(i.positionOS, float3(31.0, 17.0, 23.0)));
                    color += _EmberColor.rgb * seam * heat * flicker;
                    color = lerp(color, color * half3(0.55, 0.5, 0.48), i.tint.a * 0.6);   // and the paint is scorched
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
            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; };
            struct OutlineVaryings { float4 positionCS : SV_POSITION; float fog : TEXCOORD0; float3 positionWS : TEXCOORD1; };
            OutlineVaryings vertOutline(Attributes v, uint instanceID : SV_InstanceID)
            {
                Placed a = Place(v.positionOS.xyz, v.normalOS, v.smoothOS, instanceID);
                OutlineVaryings o;
                float4 cs = TransformWorldToHClip(a.positionWS);
                float2 dir = mul((float3x3)UNITY_MATRIX_VP, a.smoothWS).xy;
                dir = dir / max(length(dir), 1e-4);
                float px = clamp(_OutlineWidth * 70.0 / max(cs.w, 1.0), 0.0, _OutlineWidth);
                cs.xy += dir * px * 2.0 / _ScreenParams.xy * cs.w;
                o.positionCS = cs;
                o.positionWS = a.positionWS;
                o.fog = ComputeFogFactor(cs.z);
                return o;
            }
            half4 fragOutline(OutlineVaryings i) : SV_Target { return half4(MixFog(ApplyFieldFog(_OutlineColor.rgb, i.positionWS), i.fog), 1.0); }
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
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/CommonMaterial.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
            float3 _LightDirection;
            float3 _LightPosition;
            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; };
            float4 vertShadow(Attributes v, uint instanceID : SV_InstanceID) : SV_POSITION
            {
                Placed a = Place(v.positionOS.xyz, v.normalOS, v.smoothOS, instanceID);
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
            struct Attributes { float4 positionOS : POSITION; float3 normalOS : NORMAL; float3 smoothOS : TEXCOORD3; };
            float4 vertDepth(Attributes v, uint instanceID : SV_InstanceID) : SV_POSITION
            {
                return TransformWorldToHClip(Place(v.positionOS.xyz, v.normalOS, v.smoothOS, instanceID).positionWS);
            }
            half4 fragDepth() : SV_Target { return 0; }
            ENDHLSL
        }
    }
}
