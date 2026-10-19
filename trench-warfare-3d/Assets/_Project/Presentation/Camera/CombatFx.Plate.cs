// Phase: character sim (2026-10-19) — part of CombatFx (see CombatFx.cs for the pools and the shared batch): the Sentry's
// own look. Only Soldier and Sniper are baked (VATRenderer.FigureOfArchetype), so he is the baked soldier like the MG
// man he borrows his gun from. What tells the two apart on a T2 sheet is drawn over him: the 8 mm plate he fires over,
// on its post, off the sockets the near tier already samples.
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>At most this many men are given their plate in a frame, so the stress preset cannot pay for a crowd.</summary>
        public const int PlateMax = 64;
        /// <summary>The most pieces one man's plate has (the plate and its post), so a caller can size its array.</summary>
        public const int PlatePieceMax = 2;
        readonly Matrix4x4[] platePieces = new Matrix4x4[PlatePieceMax];

        /// <summary>
        /// Leaves the plate and its post out of <see cref="PlatePieces"/> altogether. Off in play; set by a stills run
        /// that wants the same pose twice, with and without the prop, to measure how many pixels the prop is worth.
        /// </summary>
        public static bool HidePlate;
        readonly System.Collections.Generic.List<Matrix4x4> posts = new System.Collections.Generic.List<Matrix4x4>(PlateMax);

        /// <summary>
        /// The pieces of one Sentry's plate, in the frame his barrel socket gives: 0 the plate, 1 the post under it. Pure
        /// - no Unity state, no camera - so an EditMode test can measure it. Returns how many of <paramref name="into"/>
        /// were filled: 0 for every archetype that carries no plate.
        /// </summary>
        public static int PlatePieces(byte archetype, Vector3 chest, Vector3 muzzle, Vector3 barrel, float scale, Matrix4x4[] into)
        {
            if (HidePlate) return 0;
            if (archetype != InfantryArchetype.Sentry || into == null || into.Length < PlatePieceMax) return 0;
            Vector3 fwd = barrel.sqrMagnitude > 0.0001f ? barrel.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, fwd);
            right = right.sqrMagnitude > 0.0001f ? right.normalized : Vector3.right;
            Vector3 up = Vector3.Cross(fwd, right).normalized;
            var turn = Quaternion.LookRotation(fwd, up);
            // the plate: 0.90 m across (two shoulders, so the 60 degree arc reads at T2) by 0.62 m tall (chest to thigh
            // on a man 25 % shorter, UnitScale) by 0.03 m - the 8 mm plate drawn thick enough to see. Its top sits just
            // under his muzzle, so he fires over it: that is 0.26 m below the chest socket, not the plan's 0.10 m, which
            // put the top 0.16 m ABOVE the muzzle it was meant to clear (half of 0.62 m is 0.31 m, not 0.15 m).
            var plate = new Vector3(0.90f, 0.62f, 0.03f) * scale;
            Vector3 at = chest + (fwd * 0.32f - up * 0.26f) * scale;
            into[0] = Matrix4x4.TRS(at, turn, plate);
            // the post: half a plate-height below it, holding it off the ground
            into[1] = Matrix4x4.TRS(at - up * (0.31f * scale), turn, new Vector3(0.07f, 0.55f, 0.07f) * scale);
            return PlatePieceMax;
        }

        /// <summary>Draw the plate of every live Sentry on the field: near tier only, where the sockets exist.</summary>
        void DrawPlate(Bounds bounds)
        {
            if (units == null) return;
            var w = Host.Local.World;
            batch.Clear();
            posts.Clear();
            int men = 0;
            for (int slot = 0; slot < w.HighWater && men < PlateMax; slot++)
            {
                uint flags = w.Flags[slot];
                if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                byte archetype = w.Archetype[slot];
                if (archetype != InfantryArchetype.Sentry) continue;
                if (!units.Sockets(slot, out var muzzle, out var barrel, out var chest)) continue;   // far tier, or the controller is off
                ViewNow(out _, out _, out float scale);
                int n = PlatePieces(archetype, chest, muzzle, barrel, scale, platePieces);
                if (n > 0) batch.Add(platePieces[0]);
                if (n > 1) posts.Add(platePieces[1]);
                men++;
            }
            if (batch.Count > 0) Flush(cube, new RenderParams(kitMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
            if (posts.Count > 0)
            {
                batch.Clear();
                for (int k = 0; k < posts.Count; k++) batch.Add(posts[k]);
                Flush(cube, new RenderParams(woodMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
            }
        }
    }
}
