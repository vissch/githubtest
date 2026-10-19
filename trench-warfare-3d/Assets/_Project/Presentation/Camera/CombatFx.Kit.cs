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
        /// <summary>How many men were given their kit in the last frame: what a capture session reads to tell "no man
        /// matched" from "drawn but invisible".</summary>
        public int KitDrawnLast;

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
            // Clear of his back, not inside it: at 0.18 m behind the chest socket the tanks sat within the baked
            // soldier's own silhouette and only 563 px of a 113x116 box changed when he put them on (a still,
            // 2026-10-19). A real set rides high and proud of the shoulder blades.
            // The tanks hang off the way he FACES, not the way he points his weapon. Taken off the barrel they
            // followed a rifle held low and went up into his own head: the whole set projected onto his shoulders
            // and not one pixel of a 56x49 box changed when he put it on (a still, 2026-10-19). Flat forward and
            // world up put them on his back wherever he is aiming, and 0.55 m behind the chest socket clears the
            // baked soldier's cape - at 0.30 m one tank showed a corner and the rest was inside him.
            Vector3 flat = new Vector3(fwd.x, 0f, fwd.z);
            flat = flat.sqrMagnitude > 0.0001f ? flat.normalized : Vector3.forward;
            Vector3 side = Vector3.Cross(Vector3.up, flat).normalized;
            var stand = Quaternion.LookRotation(flat, Vector3.up);
            var tank = new Vector3(0.18f, 0.52f, 0.18f) * scale;
            Vector3 back = chest + (Vector3.up * 0.10f - flat * 0.55f) * scale;
            into[0] = Matrix4x4.TRS(back + side * (0.11f * scale), stand, tank);
            into[1] = Matrix4x4.TRS(back - side * (0.11f * scale), stand, tank);
            // the wand: a 1.10 m tube that starts 0.45 m behind the muzzle - in his hands, over the baked rifle - and
            // runs forward from there. (The plan's "centred on muzzle - 0.45*fwd" put the far end 0.65 m ahead of the
            // chest, which its own test rejects at 1.0 m; the back end is what 0.45 m measures.)
            float wand = 1.10f * scale;
            into[2] = Matrix4x4.TRS(muzzle + fwd * ((0.55f - 0.45f) * scale), turn, new Vector3(0.09f * scale, 0.09f * scale, wand));
            return KitPieceMax;
        }

        /// <summary>The most highlight bands one man's kit has (one along each tank's lid).</summary>
        public const int KitHighlightMax = 2;
        readonly Matrix4x4[] kitHighlights = new Matrix4x4[KitHighlightMax];

        /// <summary>
        /// The one lit edge of each fuel tank: a thin band laid along the lid, in the same standing frame the tanks
        /// use. Dark steel on its own reads as a hole at night, so the pair of bands is what says "metal" (harsh
        /// critic r1, 2026-10-19). Pure, like <see cref="KitPieces"/>, so an EditMode test can measure it. Returns how
        /// many of <paramref name="into"/> were filled: 0 for every archetype that wears no kit.
        /// </summary>
        public static int KitHighlights(byte archetype, Vector3 chest, Vector3 barrel, float scale, Matrix4x4[] into)
        {
            if (archetype != InfantryArchetype.Flamethrower || into == null || into.Length < KitHighlightMax) return 0;
            Vector3 fwd = barrel.sqrMagnitude > 0.0001f ? barrel.normalized : Vector3.forward;
            Vector3 flat = new Vector3(fwd.x, 0f, fwd.z);
            flat = flat.sqrMagnitude > 0.0001f ? flat.normalized : Vector3.forward;
            Vector3 side = Vector3.Cross(Vector3.up, flat).normalized;
            var stand = Quaternion.LookRotation(flat, Vector3.up);
            // Thin on purpose: the probe takes the MEDIAN luma of the pixels the kit changes, so a fat bright band
            // would drag the whole kit's median back up to the paper-white it is being taken away from.
            var band = new Vector3(0.20f, 0.05f, 0.20f) * scale;
            Vector3 back = chest + (Vector3.up * 0.10f - flat * 0.55f) * scale;
            Vector3 lid = back + Vector3.up * (0.26f * scale);
            into[0] = Matrix4x4.TRS(lid + side * (0.11f * scale), stand, band);
            into[1] = Matrix4x4.TRS(lid - side * (0.11f * scale), stand, band);
            return KitHighlightMax;
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
                // The man's own size, not ViewNow's: ViewNow still carries the old grow-with-zoom formula, while the
                // men are drawn one size at every zoom (owner, 2026-10-13 - VATRenderer.ScaleAt). Sized by ViewNow
                // the kit came out a 1.5 m slab over a 1.8 m man at the close view (seen in a still, 2026-10-19).
                ViewNow(out _, out float zoom, out _);
                float scale = units.ScaleAt(zoom);
                // The sockets only exist at the near tier, so taking them as a condition hid the kit at the very
                // views the game is played at. Past the near tier his drawn pose is estimated, as every other piece
                // hung on a man already does (CombatFx.Bodies.cs), so he reads as a Flamer at T2 and T1 too.
                if (!units.Sockets(slot, out var muzzle, out var barrel, out var chest))
                {
                    EstimateMuzzle(slot, -1, scale, out muzzle, out barrel);
                    chest = EstimateChest(slot, scale);
                }
                int n = KitPieces(archetype, chest, muzzle, barrel, scale, kitPieces);
                for (int k = 0; k < n; k++) batch.Add(kitPieces[k]);
                men++;
            }
            KitDrawnLast = men;
            if (batch.Count == 0) return;
            Flush(cube, new RenderParams(kitMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
            // One extra draw at the near tier (kits are rare; the ceiling is 300): the lit lids of the same men.
            batch.Clear();
            for (int slot = 0, lit = 0; slot < w.HighWater && lit < KitMax; slot++)
            {
                uint flags = w.Flags[slot];
                if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                byte archetype = w.Archetype[slot];
                if (archetype != InfantryArchetype.Flamethrower) continue;
                ViewNow(out _, out float zoom, out _);
                float scale = units.ScaleAt(zoom);
                if (!units.Sockets(slot, out _, out var barrel, out var chest))
                {
                    EstimateMuzzle(slot, -1, scale, out _, out barrel);
                    chest = EstimateChest(slot, scale);
                }
                int n = KitHighlights(archetype, chest, barrel, scale, kitHighlights);
                for (int k = 0; k < n; k++) batch.Add(kitHighlights[k]);
                lit++;
            }
            if (batch.Count > 0) Flush(cube, new RenderParams(kitHiMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
        }
    }
}
