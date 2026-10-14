// Phase: B6 — the lay-mine / lay-tripwire orders of the selected sappers, as plain data.
// The owner (decisions.md 2026-10-06) asked for a button on the battle bar for what the sim already does on a
// UnitAbility command (SapperSystem): this is the half with no UI Toolkit in it, so an EditMode test can drive it
// over a real MatchSim and watch the sim consume the very command the bar sends (SapperOrderTests). The command is
// the one ProvingGround.OrderSappers builds, byte for byte.
// No ground test here: the sim refuses what may not lie there (SapperSystem.Accept asks MineSystem), cheaply.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Units;

namespace TW.UI
{
    public static class SapperOrders
    {
        /// <summary>A tripwire laid from the bar runs across the front, as the Proving Ground lays it.</summary>
        public const int WireHeading = 90;

        /// <summary>Is this one of our living sappers, as the handle took him, and does his spec carry charges?</summary>
        static bool IsSapper(SimWorld w, UnitHandle h)
        {
            int i = h.Slot;
            if (i < 0 || i >= w.HighWater || !w.IsAlive(i)) return false;
            if (w.Generation[i] != h.Gen || w.Team[i] != 0) return false;
            if ((w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) return false;
            return w.Units.Infantry[w.Archetype[i]].MineCharges > 0;
        }

        /// <summary>Does the selection hold a sapper of ours? This is what makes the row of buttons appear.</summary>
        public static bool Any(SimWorld w, SapperSystem sapper, IReadOnlyList<UnitHandle> sel)
        {
            if (w == null || sapper == null || sel == null) return false;
            for (int k = 0; k < sel.Count; k++) if (IsSapper(w, sel[k])) return true;
            return false;
        }

        /// <summary>Charges the selected sappers have left between them; 0 = nothing left to lay.</summary>
        public static int Charges(SimWorld w, SapperSystem sapper, IReadOnlyList<UnitHandle> sel)
        {
            if (w == null || sapper == null || sel == null) return 0;
            int n = 0;
            for (int k = 0; k < sel.Count; k++) if (IsSapper(w, sel[k])) n += sapper.ChargesOf(w, sel[k].Slot);
            return n;
        }

        /// <summary>
        /// One command per selected sapper who has a charge left and no errand of this system in hand, exactly as
        /// SapperSystem.Accept consumes it: A the slot, B the ability in the low byte with AbilityArgs above it, Pos
        /// the point clicked. Returns how many were added.
        /// </summary>
        public static int Commands(SimWorld w, SapperSystem sapper, IReadOnlyList<UnitHandle> sel, UnitAbilityId id, Vector3 point, List<SimCommand> into)
        {
            if (w == null || sapper == null || sel == null || into == null) return 0;
            if (id != UnitAbilityId.LayMine && id != UnitAbilityId.LayTripwire) return 0;
            int args = id == UnitAbilityId.LayTripwire ? AbilityArgs.Pack(WireHeading, 0, SapperSystem.DefaultTripwireMetres) : 0;
            var pos = new Unity.Mathematics.float3(point.x, 0f, point.z);
            int n = 0;
            for (int k = 0; k < sel.Count; k++)
            {
                int i = sel[k].Slot;
                if (!IsSapper(w, sel[k])) continue;
                if (sapper.ChargesOf(w, i) <= 0 || sapper.Phase[i] != SapperSystem.Idle) continue;
                into.Add(new SimCommand
                {
                    Tick = w.Tick, Player = 0, Type = CommandType.UnitAbility, A = i, B = (int)id | (args << 8), Pos = pos,
                });
                n++;
            }
            return n;
        }
    }
}
