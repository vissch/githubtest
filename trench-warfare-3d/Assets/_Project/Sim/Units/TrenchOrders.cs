// Phase: A1 (initial implementation) — depends on: FlowFieldManager (A1). A3 completes it: ownership flips through
// sector control, the 3 s suppression-gain grace on fallback, and roster masks driven by the UI.
// Consumes TrenchAdvance / TrenchSelectAdvance / TrenchLock / TrenchFallback / TrenchHoldFire from
// SimWorld.TickCommands in command order and turns them into goals. Advance: every unit garrisoned in the trench (or
// only the masked classes) gets the next goal in the chain, remembers the trench it left, and is Exposed until it
// garrisons again. Fallback: units in the open that left this trench get the trench back as their goal. Lock and
// hold-fire only flip per-trench flags that MovementSystem (arrivals) and A2 (fire) read. A command that names a
// trench the player does not own is rejected.
using System;
using Unity.Collections;
using TW.Sim.Nav;

namespace TW.Sim.Units
{
    public sealed class TrenchOrdersSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Command + 10;

        FlowFieldManager fields;

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new InvalidOperationException("TrenchOrdersSystem needs FlowFieldManager registered before it");
        }

        public void Step(SimWorld w)
        {
            var trenches = fields.Trenches;
            for (int c = 0; c < w.TickCommands.Length; c++)
            {
                var cmd = w.TickCommands[c];
                switch (cmd.Type)
                {
                    case CommandType.TrenchAdvance: case CommandType.TrenchSelectAdvance: case CommandType.TrenchLock:
                    case CommandType.TrenchFallback: case CommandType.TrenchHoldFire:
                        break;
                    default:
                        continue;
                }
                if (cmd.A < 0 || cmd.A >= trenches.Length || trenches[cmd.A].OwnerTeam != cmd.Player) { w.Reject(cmd); continue; }
                short id = (short)cmd.A;
                switch (cmd.Type)
                {
                    // An order that moves nobody REPORTS that, instead of being quietly accepted. The trench existing and being
                    // owned is not enough: it can be empty, because DefaultGoal garrisons the rear trench, so an
                    // advance aimed at the front line routinely finds no one there. Silently succeeding at nothing
                    // hid a whole missing battle for two review cycles (2026-09-23). It is its own event rather than
                    // CommandRejected: the command was legal, it just found an empty trench.
                    case CommandType.TrenchAdvance: if (Advance(w, id, cmd.Player, 0) == 0) w.Events.Add(w.Tick, SimEventType.OrderFoundNoOne, (int)cmd.Type, cmd.Player); break;
                    case CommandType.TrenchSelectAdvance: if (Advance(w, id, cmd.Player, cmd.B) == 0) w.Events.Add(w.Tick, SimEventType.OrderFoundNoOne, (int)cmd.Type, cmd.Player); break;
                    case CommandType.TrenchFallback: Fallback(w, id, cmd.Player); break;
                    case CommandType.TrenchLock: { var s = trenches[id]; s.Locked = (byte)(cmd.B != 0 ? 1 : 0); trenches[id] = s; break; }
                    case CommandType.TrenchHoldFire: { var s = trenches[id]; s.HoldFire = (byte)(cmd.B != 0 ? 1 : 0); trenches[id] = s; break; }
                }
            }
            // garrison counts (one tick stale for arrivals, which is fine for UI and AI)
            for (int t = 0; t < trenches.Length; t++) { var s = trenches[t]; s.GarrisonCount = 0; trenches[t] = s; }
            for (int i = 0; i < w.HighWater; i++)
            {
                short t = w.TrenchId[i];
                if (t < 0 || (w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                var s = trenches[t];
                if (w.Team[i] != s.OwnerTeam) continue;   // assaulters inside an enemy trench are not its garrison
                s.GarrisonCount++; trenches[t] = s;
            }
        }

        /// <summary>How many men actually went over the top. Zero means the order found nobody to give.</summary>
        int Advance(SimWorld w, short trenchId, byte team, int archetypeMask)
        {
            int goal = fields.NextGoalFrom(trenchId, team);
            if (goal < 0) return 0;
            int moved = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                if (w.Team[i] != team || w.TrenchId[i] != trenchId) continue;
                if (archetypeMask != 0 && (archetypeMask & (1 << w.Archetype[i])) == 0) continue;
                if (w.Suppression[i] >= StanceRules.PinnedSuppression) continue;   // pinned men refuse to go over the top
                w.GoalId[i] = goal;
                w.TrenchId[i] = -1;
                w.SourceTrench[i] = trenchId;
                w.Flags[i] = f | (uint)UnitFlags.Exposed;
                w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, i, trenchId, w.Position[i]);
                moved++;
            }
            return moved;
        }

        void Fallback(SimWorld w, short trenchId, byte team)
        {
            int goal = fields.GetGoal(GoalKey.Trench(trenchId));
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                if (w.Team[i] != team || w.TrenchId[i] >= 0 || w.SourceTrench[i] != trenchId) continue;
                w.GoalId[i] = goal;
                w.Flags[i] = f | (uint)UnitFlags.Exposed;
            }
        }

        public ulong Hash(ulong h) => h;   // trench state is owned and hashed by FlowFieldManager
        public void Dispose() { }
    }
}
