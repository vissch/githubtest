// Phase: character sim (2026-10-19) — part of CombatFx (see CombatFx.cs for the pools and the shared batch): the
// Sapper's own look. He wore the Engineer's (InfantryArchetype.Repair) look and his tooltip called him a stand-in;
// neither is baked as its own figure, so what tells the two apart at T2 is the pack and two charges drawn over him
// off the near tier's sockets (CombatFx.Kit.cs does the same for the Flamethrower).
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>At most this many men are given their kit in a frame, so the stress preset cannot pay for a crowd.</summary>
        public const int SapperKitMax = 64;
        /// <summary>The most pieces one man's kit has (the pack and two charges), so a caller can size its array.</summary>
        public const int SapperKitPieceMax = 3;
        readonly Matrix4x4[] sapperPieces = new Matrix4x4[SapperKitPieceMax];

        /// <summary>
        /// The pieces of one man's kit, in the frame his barrel socket gives: 0 the pack on his back, 1 and 2 the two
        /// charges slung either side of it. Pure - no Unity state - so an EditMode test can measure it. Returns how many
        /// of <paramref name="into"/> were filled: 0 for every archetype but the Sapper.
        /// </summary>
        public static int SapperKitPieces(byte archetype, Vector3 chest, Vector3 muzzle, Vector3 barrel, float scale, Matrix4x4[] into)
        {
            if (archetype != InfantryArchetype.Sapper || into == null || into.Length < SapperKitPieceMax) return 0;
            Vector3 fwd = barrel.sqrMagnitude > 0.0001f ? barrel.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, fwd);
            right = right.sqrMagnitude > 0.0001f ? right.normalized : Vector3.right;
            Vector3 up = Vector3.Cross(fwd, right).normalized;
            var turn = Quaternion.LookRotation(fwd, up);
            // the pack: a satchel wider than it is deep, so it reads as a pack and not as the Flamer's tanks
            Vector3 pack = chest + (up * 0.05f - fwd * 0.20f) * scale;
            into[0] = Matrix4x4.TRS(pack, turn, new Vector3(0.58f, 0.52f, 0.32f) * scale);
            // the two charges, one each side of the pack (InfantrySpec.MineCharges is 2), slung a little lower
            Vector3 charges = chest + (-up * 0.06f - fwd * 0.20f) * scale;
            into[1] = Matrix4x4.TRS(charges + right * (0.19f * scale), turn, new Vector3(0.27f, 0.27f, 0.21f) * scale);
            into[2] = Matrix4x4.TRS(charges - right * (0.19f * scale), turn, new Vector3(0.27f, 0.27f, 0.21f) * scale);
            return SapperKitPieceMax;
        }

        /// <summary>Draw the kit of every live Sapper on the field: off the near tier's sockets where they exist, and
        /// off his drawn position and yaw beyond that (the far tier's box carries no sockets), so he is told apart
        /// from the Engineer at every zoom the camera reaches, not only close up.</summary>
        void DrawSapperKit(Bounds bounds)
        {
            if (units == null) return;
            var w = Host.Local.World;
            batch.Clear();
            int men = 0;
            ViewNow(out _, out _, out float scale);
            for (int slot = 0; slot < w.HighWater && men < SapperKitMax; slot++)
            {
                uint flags = w.Flags[slot];
                if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                byte archetype = w.Archetype[slot];
                if (archetype != InfantryArchetype.Sapper) continue;
                Vector3 muzzle, barrel, chest;
                if (!units.Sockets(slot, out muzzle, out barrel, out chest))
                {
                    // far tier, or the controller is off: estimate from his drawn position and yaw instead of skipping him
                    Vector3 at = Host.Presenter.Drawn(slot);
                    float ground = RenderGround.Sample(Host.Local.Map, at.x, at.z);
                    float yaw = w.Yaw[slot];
                    barrel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
                    chest = new Vector3(at.x, ground, at.z) + Vector3.up * (1.3f * scale);
                    muzzle = chest + barrel * (0.6f * scale);
                }
                int n = SapperKitPieces(archetype, chest, muzzle, barrel, scale, sapperPieces);
                for (int k = 0; k < n; k++) batch.Add(sapperPieces[k]);
                men++;
            }
            if (batch.Count > 0) Flush(cube, new RenderParams(dirtMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
        }
    }
}
