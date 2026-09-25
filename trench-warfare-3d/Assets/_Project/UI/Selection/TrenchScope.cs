// Phase: B6 (implemented) — is the selection "these troop categories of trench t"? (owner, 2026-09-24: pick categories
// from a trench's chips, and its over the top sends only them.) It is when every selected man is alive, ours, on foot
// and garrisoned in the same trench; the mask is his archetypes as TrenchSelectAdvance takes them (1 << archetype).
// The sim moves whole categories, not men: three riflemen picked out of eight send all eight, which the tooltip says.
// A mask that covers every category present is a plain advance (mask 0). Pure rules, tested without a world.
using System.Collections.Generic;
using TW.Sim;

namespace TW.UI
{
    public static class TrenchScope
    {
        /// <summary>Archetypes that fit TrenchSelectAdvance's int mask.</summary>
        public const int MaxMaskArchetype = 30;

        /// <summary>
        /// Fold one selected unit into the scope. trench starts at -1 and mask at 0; false means the selection is not
        /// scoped to one trench (and the caller stops looking).
        /// </summary>
        public static bool Step(ref int trench, ref int mask, bool alive, bool ours, bool vehicle, int unitTrench, int archetype)
        {
            if (!alive || !ours || vehicle || unitTrench < 0 || archetype > MaxMaskArchetype) return false;
            if (trench >= 0 && unitTrench != trench) return false;
            trench = unitTrench; mask |= 1 << archetype;
            return true;
        }

        /// <summary>The selection's trench and category mask, or false when it is empty or not one trench's men.</summary>
        public static bool Of(SimWorld w, IReadOnlyList<UnitHandle> selection, out int trench, out int mask)
        {
            trench = -1; mask = 0;
            if (w == null || selection.Count == 0) return false;
            for (int i = 0; i < selection.Count; i++)
            {
                var h = selection[i];
                int s = h.Slot;
                bool alive = w.IsAlive(s) && w.Generation[s] == h.Gen;
                if (!Step(ref trench, ref mask, alive, alive && (w.Team[s] & 1) == 0, alive && (w.Flags[s] & (uint)UnitFlags.Vehicle) != 0,
                          alive ? w.TrenchId[s] : -1, alive ? w.Archetype[s] : 255))
                { trench = -1; mask = 0; return false; }
            }
            return true;
        }

        /// <summary>The categories present in trench t, as a mask.</summary>
        public static int Present(GarrisonStats g, int t)
        {
            int m = 0;
            if (g == null || t < 0 || t >= g.Trenches) return 0;
            for (int a = 0; a <= MaxMaskArchetype; a++) if (g.TypeCount(t, a) > 0) m |= 1 << a;
            return m;
        }

        /// <summary>The mask to send: 0 (everyone) when the selection covers every category present.</summary>
        public static int Effective(int mask, int present) => (present & ~mask) == 0 ? 0 : mask;
    }
}
