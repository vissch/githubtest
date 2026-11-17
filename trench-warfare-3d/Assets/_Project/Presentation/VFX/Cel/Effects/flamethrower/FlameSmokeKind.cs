// Phase: CelFx flamethrower smoke staging (2026-11-13). Reuses Lobe's mesh, shader and keeper material.
// Extra.w < 0 opts into emitter-time palette staging; -Extra.w is the time scale (normally 1).
// Incoming Tone.z / layer Opacity = 1 + phase delay in seconds (unused by ordinary cutout shading).
// Preparation replaces it with the absolute emitter start plus delay. Direct Spawn requires this preparation.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameSmokeKind : CelKind
    {
        public override string Name => "FlameSmoke";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameSmokeOpaque");
        public override Mesh BuildMesh() => CelMeshes.Icosphere(3, "Cel FlameSmoke");
        public override int Capacity => 768;
        public override int Order => 0;

        // The actual owning renderer invokes this AFTER storing the prepared original.
        public override void AfterPreparedSpawn(CelFxRenderer owner, in CelRecord prepared)
        {
            if (prepared.Extra.w >= 0f) return;
            int softKind = owner.KindId("FlameSmokeSoft");
            if (softKind >= 0) owner.Spawn(softKind, prepared);
        }

        // Bridge-only reserved encoding: raw -16, prepared -(1024 + oct11x11).
        // Integer payloads stay exactly representable in float; Axis remains the original spin axis.
        public const float BridgeMarker = -16f;
        public const float BridgeDriftLength = 1.0228514f;
        static float SignNotZero(float v) => v < 0f ? -1f : 1f;

        public static float PackBridgeDirection(Vector3 direction)
        {
            Vector3 n = direction.normalized;
            n /= Mathf.Abs(n.x) + Mathf.Abs(n.y) + Mathf.Abs(n.z);
            Vector2 q = new Vector2(n.x, n.y);
            if (n.z < 0f) q = new Vector2((1f - Mathf.Abs(n.y)) * SignNotZero(n.x),
                (1f - Mathf.Abs(n.x)) * SignNotZero(n.y));
            int x = Mathf.Clamp(Mathf.FloorToInt((q.x * .5f + .5f) * 2047f + .5f), 0, 2047);
            int y = Mathf.Clamp(Mathf.FloorToInt((q.y * .5f + .5f) * 2047f + .5f), 0, 2047);
            return -(1024 + x + (y << 11));
        }

        // Decode is also exposed for record diagnostics; the local shader consumes the same packed contract.
        public static Vector3 DecodeBridgeDirection(float packed)
        {
            int bits = Mathf.RoundToInt(-packed - 1024f);
            float x = (bits & 2047) * (2f / 2047f) - 1f;
            float y = ((bits >> 11) & 2047) * (2f / 2047f) - 1f;
            Vector3 n = new Vector3(x, y, 1f - Mathf.Abs(x) - Mathf.Abs(y));
            if (n.z < 0f)
            {
                n.x = (1f - Mathf.Abs(y)) * SignNotZero(x);
                n.y = (1f - Mathf.Abs(x)) * SignNotZero(y);
            }
            return n.normalized;
        }

        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            record.Tone.z = now + Mathf.Max(0f, record.Tone.z - 1f);
            if (record.Size.w == BridgeMarker && record.Extra.w < 0f)
            {
                CelFx.Frame(target - origin, out _, out Vector3 right, out Vector3 up);
                record.Size.w = PackBridgeDirection(right + .215f * up);
            }
        }
    }
}
