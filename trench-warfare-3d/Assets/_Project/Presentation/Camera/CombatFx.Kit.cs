// Phase: character sim (2026-10-19) — part of CombatFx (see CombatFx.cs for the pools and the shared batch): the kit a
// man wears that the baked figure does not carry. Only Soldier and Sniper are baked (VATRenderer.FigureOfArchetype), so
// a unit that must read as itself at T2 gets its pieces drawn over the baked soldier, off the sockets the near tier
// already samples. Today that is the Flamethrower: two fuel tanks on his back and the wand in his hands.
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>At most this many men are given their kit in a frame, so the stress preset cannot pay for a crowd.</summary>
        public const int KitMax = 64;
        /// <summary>The most pieces one man's kit has (two tanks and a wand), so a caller can size its array.</summary>
        public const int KitPieceMax = 3;
        readonly Matrix4x4[] kitPieces = new Matrix4x4[KitPieceMax];

        /// <summary>
        /// The pieces of one man's kit, in the frame his barrel socket gives: 0 and 1 the fuel tanks on his back, 2 the
        /// wand. Pure - no Unity state, no camera - so an EditMode test can measure it. Returns how many of
        /// <paramref name="into"/> were filled: 0 for every archetype that wears no kit.
        /// </summary>
        public static int KitPieces(byte archetype, Vector3 chest, Vector3 muzzle, Vector3 barrel, float scale, Matrix4x4[] into)
        {
            if (archetype != InfantryArchetype.Flamethrower || into == null || into.Length < KitPieceMax) return 0;
            Vector3 fwd = barrel.sqrMagnitude > 0.0001f ? barrel.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, fwd);
            right = right.sqrMagnitude > 0.0001f ? right.normalized : Vector3.right;
            Vector3 up = Vector3.Cross(fwd, right).normalized;
            var turn = Quaternion.LookRotation(fwd, up);
            // the two tanks, high on his back, one each side of the spine
            var tank = new Vector3(0.16f, 0.46f, 0.16f) * scale;
            Vector3 back = chest + (up * 0.02f - fwd * 0.18f) * scale;
            into[0] = Matrix4x4.TRS(back + right * (0.11f * scale), turn, tank);
            into[1] = Matrix4x4.TRS(back - right * (0.11f * scale), turn, tank);
            // the wand: a 1.10 m tube that starts 0.45 m behind the muzzle - in his hands, over the baked rifle - and
            // runs forward from there. (The plan's "centred on muzzle - 0.45*fwd" put the far end 0.65 m ahead of the
            // chest, which its own test rejects at 1.0 m; the back end is what 0.45 m measures.)
            float wand = 1.10f * scale;
            into[2] = Matrix4x4.TRS(muzzle + fwd * ((0.55f - 0.45f) * scale), turn, new Vector3(0.09f * scale, 0.09f * scale, wand));
            return KitPieceMax;
        }

        /// <summary>Draw the kit of every live man on the field who wears one: near tier only, where the sockets exist.</summary>
        void DrawKit(Bounds bounds)
        {
            if (units == null) return;
            var w = Host.Local.World;
            batch.Clear();
            int men = 0;
            for (int slot = 0; slot < w.HighWater && men < KitMax; slot++)
            {
                uint flags = w.Flags[slot];
                if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                byte archetype = w.Archetype[slot];
                if (archetype != InfantryArchetype.Flamethrower) continue;
                if (!units.Sockets(slot, out var muzzle, out var barrel, out var chest)) continue;   // far tier, or the controller is off
                ViewNow(out _, out _, out float scale);
                int n = KitPieces(archetype, chest, muzzle, barrel, scale, kitPieces);
                for (int k = 0; k < n; k++) batch.Add(kitPieces[k]);
                men++;
            }
            if (batch.Count > 0) Flush(cube, new RenderParams(dirtMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
        }
    }
}
