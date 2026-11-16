// Phase: CelFx job (2026-11-10, lane/show/cel-explosion, round d11) - ExpChunk: dense dark angular debris chunks.
// One camera-facing quad per record, cut out (opaque, alpha-tested, ZWrite on) to an irregular 4-7 sided polygon
// picked from the seed, spinning in the screen plane, a flat dark fill with one lit facet band and an ink rim N screen
// px wide. Ballistic motion from the record (CelPosition: gravity, drag, buoyancy). Hundreds in one indirect draw:
// the kit's Streak is additive (a dark streak adds nothing) and a Lobe costs an icosphere and a pool slot the
// fireball needs, so this is the cheap way to put black chunks over fire and dust.
//   size.xyz   radius (m) at birth, at death, ease          size.w  aspect (0 or 1: as cut; 2: twice as long as wide)
//   motion     gravity, drag, spin (rad/s in the screen plane), buoyancy
//   erode      unused
//   tone       x palette (c0 fill, c2 lit facet, ink colour), y lit facet share (0..1, 0: 0.35), z unused, w ink px
//   extra      x sides min (0: 4), y sides extra range (0: 3), z corner jitter 0..1 (0: 0.35), w unused
Shader "TW/Cel/ExpChunk"
{
    Properties { }
    SubShader
    {
        Tags { "RenderType"="TransparentCutout" "RenderPipeline"="UniversalPipeline" "Queue"="AlphaTest+12" "IgnoreProjector"="True" }

        HLSLINCLUDE
        #pragma target 4.5
        #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
        #include "Assets/_Project/Shaders/Cel/CelFx.hlsl"

        struct ChunkV2F
        {
            float4 positionCS : SV_POSITION;
            float2 uv : TEXCOORD0;                         // -1..1 across the quad, already spun and stretched
            nointerpolation float4 shape : TEXCOORD1;      // x sides, y corner jitter, z seed, w lit share
            nointerpolation float4 tone : TEXCOORD2;       // x palette, y ink px, z fog, w unused
            float3 positionWS : TEXCOORD3;
            float ground : TEXCOORD4; // positive above the authored birth-ground plane
        };

        ChunkV2F ChunkVert(float4 positionOS, float2 uv, uint instanceID)
        {
            CelRecord r = CelFetch(instanceID);
            float t = CelAge(r), k = CelLife01(r), alive = CelAlive(r);
            float3 p = CelPosition(r, t);
            float radius = CelCurve(r.size.xyz, k) * alive;
            float aspect = r.size.w > 0.0 ? r.size.w : 1.0;
            float3 right = UNITY_MATRIX_V[0].xyz, up = UNITY_MATRIX_V[1].xyz;
            float2 q = (uv * 2.0 - 1.0);
            float3 w = p + (right * q.x + up * q.y) * radius * max(aspect, 1.0) * 1.05;
            float ang = r.seed * 6.2831853 + r.motion.z * t;
            float s, c; sincos(ang, s, c);
            float2 qa = q * max(aspect, 1.0);
            float2 rq = float2(c * qa.x - s * qa.y, s * qa.x + c * qa.y);
            rq.x /= max(aspect, 1.0);                      // stretched: the polygon is cut longer along the spun x
            ChunkV2F o;
            // Extra.w opts a site chunk into depth pull; Tone.z is its spawn height above ground.
            o.ground = r.extra.w > 0.0 ? w.y - (r.pos.y - r.tone.z) : 1.0;
            float3 drawn = r.extra.w > 0.0 ? _WorldSpaceCameraPos + (w - _WorldSpaceCameraPos) * r.extra.w : w;
            o.positionWS = drawn;
            o.positionCS = TransformWorldToHClip(drawn);
            o.uv = float2(rq.x, rq.y);
            float smin = r.extra.x > 0.0 ? r.extra.x : 4.0, srange = r.extra.y > 0.0 ? r.extra.y : 3.0;
            o.shape = float4(floor(smin + CelHash11(r.seed * 97.3) * (srange + 0.999)), r.extra.z > 0.0 ? r.extra.z : 0.35,
                             r.seed, r.tone.y > 0.0 ? r.tone.y : 0.35);
            if (r.extra.x < 0.0) o.shape.x = -1.0; // rounded normal-Site clump, isolated from all flying polygon chunks
            o.tone = float4(r.tone.x, CelLodInk(r.tone.w), ComputeFogFactor(o.positionCS.z), 0.0);
            return o;
        }

        /// Signed distance-like field of the seed's polygon: > 0 inside (1 at the middle), 0 on the edge.
        float ChunkField(float2 p, float sides, float jitter, float seed)
        {
            if (sides < 0.0)
            {
                // Unequal exposed shoulders and a shallow edge nick prevent polished oval stones.
                float2 q = abs(p / float2(0.82, 0.70));
                float a = (1.0 - pow(pow(q.x, 2.5) + pow(q.y, 2.5), 0.4)) * 0.70;
                float b = (1.0 - length((p - float2(0.55, 0.24)) / float2(0.43, 0.38))) * 0.38;
                float c = (1.0 - length((p - float2(-0.58, 0.37)) / float2(0.34, 0.34))) * 0.34;
                float ab = max(0.08 - abs(a - b), 0.0) / 0.08;
                float f = max(a, b) + ab * ab * 0.020;
                float bc = max(0.08 - abs(f - c), 0.0) / 0.08;
                f = max(f, c) + bc * bc * 0.020;
                float angle = atan2(p.y, p.x);
                f += 0.050 * sin(angle * 3.0 + seed * 11.0) + 0.025 * sin(angle * 5.0 - seed * 7.0);
                float2 begin = float2(0.32, -0.30), end = float2(1.30, -0.75);
                float2 span = end - begin;
                float u = saturate(dot(p - begin, span) / dot(span, span));
                float nick = lerp(0.045, 0.12, u) - length(p - (begin + span * u));
                return min(f, -nick);
            }
            float d = -1e3;
            [loop] for (int i = 0; i < 7; i++)
            {
                if (i >= (int)sides) break;
                float h = CelHash11(seed * 53.1 + i * 7.7);
                float a = (i + (h - 0.5) * jitter * 1.6) * 6.2831853 / sides;
                float rr = 0.62 + 0.38 * CelHash11(seed * 19.9 + i * 3.1) * (0.4 + jitter);
                d = max(d, dot(p, float2(cos(a), sin(a))) - rr);
            }
            return -d;
        }
        ENDHLSL

        Pass
        {
            Name "CelExpChunk"
            Tags { "LightMode"="UniversalForward" }
            Cull Off ZWrite On ZTest LEqual
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_fog
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            ChunkV2F vert(Attributes v, uint instanceID : SV_InstanceID) { return ChunkVert(v.positionOS, v.uv, instanceID); }

            half4 frag(ChunkV2F i) : SV_Target
            {
                clip(i.ground);
                float f = ChunkField(i.uv, i.shape.x, i.shape.y, i.shape.z);
                clip(f);
                uint row = CelRowA(i.tone.x);
                // one lit facet: the side toward a light from upper left in the chunk's own (spun) frame
                float lit = dot(normalize(i.uv + 1e-4), float2(-0.6, 0.8));
                half3 col = CelPal(row, 0);
                if (i.shape.x < 0.0)
                {
                    float face = dot(i.uv, normalize(float2(-0.6, 0.8)));
                    col = lerp(col, CelPal(row, 2), (half)(smoothstep(0.24, 0.52, face) * 0.48));
                }
                else col = lerp(col, CelPal(row, 2), CelAAStep(1.0 - 2.0 * i.shape.w, lit) * CelAAStep(0.25, f));
                col = lerp(col, CelPalInk(row).rgb, CelInk(f, 0.0, i.tone.y));
                col = ApplyFieldFog(col, i.positionWS);
                return half4(MixFog(col, i.tone.z), 1.0);
            }
            ENDHLSL
        }

        Pass
        {
            Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }
            ZWrite On ColorMask R Cull Off
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            struct Attributes { float4 positionOS : POSITION; float2 uv : TEXCOORD0; };
            ChunkV2F vert(Attributes v, uint instanceID : SV_InstanceID) { return ChunkVert(v.positionOS, v.uv, instanceID); }
            half4 frag(ChunkV2F i) : SV_Target
            {
                clip(i.ground);
                clip(ChunkField(i.uv, i.shape.x, i.shape.y, i.shape.z));
                return 0;
            }
            ENDHLSL
        }
    }
}
