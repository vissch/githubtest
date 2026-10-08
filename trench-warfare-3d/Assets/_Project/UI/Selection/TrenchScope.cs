// Phase: B6 (implemented) — is the selection "these troop categories of trench t"? (owner, 2026-09-24: pick categories
// from a trench's chips, and its over the top sends only them.) It is when every selected man is alive, ours, on foot
// and garrisoned in the same trench; the mask is his archetypes (1 << archetype), which is what the chips show.
// The sim moves whole ORDER GROUPS, not men and not archetypes: TrenchSelectAdvance takes OrderGroup bits (Line: rifle and
// assault; Gun: the machine gunners; Marksman; Support; Raider). So three riflemen picked out of eight send all eight,
// and picking the riflemen sends the assault men too: Widen is every category the order will really move, and Groups
// is what is sent. Until 2026-09-29 the archetype mask itself was sent, which the sim read as groups: picking the
// riflemen sent the machine gunners too, and picking the gunners sent the officers and medics instead.
// A mask that covers every category present is a plain advance (mask 0). [I2] Of also drops a scope whose trench we
// do not own: men can be TrenchId-garrisoned in a trench the enemy now holds (overrun, not yet re-sorted), and
// ScopedTrench feeds that trench straight into G/Backspace, F/L and the "ALL OF TRENCH n" panel line.
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
            var fields = w.GetSystem<TW.Sim.Nav.FlowFieldManager>();
            if (fields == null || trench >= fields.Trenches.Length || fields.Trenches[trench].OwnerTeam != 0)
            { trench = -1; mask = 0; return false; }
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

        /// <summary>The order group a man of this archetype answers to, from the match's unit table.</summary>
        public static int GroupOf(SimWorld w, int archetype)
            => w != null && w.Units.Infantry.IsCreated && archetype >= 0 && archetype < w.Units.Infantry.Length ? w.Units.Infantry[archetype].Group : OrderGroup.Line;

        /// <summary>The OrderGroup mask TrenchSelectAdvance is sent for these categories: the groups they belong to.</summary>
        public static int Groups(int categories, System.Func<int, int> groupOf)
        {
            int g = 0;
            for (int a = 0; a <= MaxMaskArchetype; a++) if ((categories & (1 << a)) != 0) g |= groupOf(a);
            return g;
        }

        /// <summary>Every category present that an order for these categories moves: all those in the same groups.</summary>
        public static int Widen(int categories, int present, System.Func<int, int> groupOf)
        {
            int groups = Groups(categories, groupOf), m = 0;
            for (int a = 0; a <= MaxMaskArchetype; a++) if ((present & (1 << a)) != 0 && (groupOf(a) & groups) != 0) m |= 1 << a;
            return m;
        }
    }
}
