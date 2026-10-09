// Phase: A3 (implemented core) — depends on: ObjectiveDef, MapData.CellTrenchId, FlowFieldManager (trench ownership)
// An objective flips when >= RequiredUnits infantry of the other team stand in its cells for CaptureTicks with no
// defender inside, in OrderIndex order along that side's line (Outpost -> Main -> Reserve -> HQ): a team must own
// every lower-ordered objective on a side before it can take the next; a side winning back its OWN line goes the
// other way, nearest first (Reserve before Main), which is Unlocked's ownSide branch. Progress drains at the same rate when the
// attackers leave, and freezes while the cells are contested. When the objective lies on a trench, the trench
// changes owner with it (TrenchCaptured), which moves both teams' front trench and hands the garrison's orders to
// the new owner. Capturing an HQ ends the match. Traverse-by-traverse capture and the mustard-gas block are later.
using Unity.Collections;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    public struct ObjectiveState { public byte Owner; public int CaptureProgressTicks; public byte CapturingTeam; }

    public sealed class SectorControlSystem : ISimSystem
    {
        public int Order => SimSystemOrder.SectorControl;
        public NativeArray<ObjectiveState> States;

        readonly MapData map;
        FlowFieldManager fields;
        NativeArray<short> cellObjective;   // nav cell -> objective index, -1 none (derived from the map)

        /// <summary>The objective index a nav cell lies in, -1 for none (the salvage report asks whose ground a wreck lies on).</summary>
        public short ObjectiveAt(int navCell) => cellObjective.IsCreated && navCell >= 0 && navCell < cellObjective.Length ? cellObjective[navCell] : (short)-1;
        NativeArray<int> counts;            // objective * 2 + team, rebuilt every tick

        public SectorControlSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("SectorControlSystem needs FlowFieldManager registered before it");
            int n = map.Objectives.Length;
            States = new NativeArray<ObjectiveState>(n, Allocator.Persistent);
            counts = new NativeArray<int>(n * 2, Allocator.Persistent);
            cellObjective = new NativeArray<short>(map.NavWidth * map.NavLength, Allocator.Persistent);
            for (int c = 0; c < cellObjective.Length; c++) cellObjective[c] = -1;
            for (int o = 0; o < n; o++)
            {
                var def = map.Objectives[o];
                States[o] = new ObjectiveState { Owner = def.OwnerTeam, CapturingTeam = 255 };
                for (int k = 0; k < def.CellCount; k++) cellObjective[map.ObjectiveCells[def.CellStart + k]] = (short)o;
            }
        }

        /// <summary>
        /// True when <paramref name="team"/> may take objective <paramref name="o"/> now. Attacking the other side's
        /// line it must own every objective on that side with a LOWER OrderIndex (Outpost -> Main -> Reserve -> HQ).
        /// Winning back its own ground (the owner's call of 2026-10-07) it works the other way about, nearest first:
        /// it must own every objective on its own side with a HIGHER OrderIndex, so Reserve (2) comes back before
        /// Main (1). Pure: it reads only the defs and the states handed to it.
        /// </summary>
        public static bool Unlocked(NativeList<ObjectiveDef> objectives, NativeArray<ObjectiveState> states, int o, byte team)
        {
            var def = objectives[o];
            bool ownSide = team == def.SideTeam;
            for (int k = 0; k < objectives.Length; k++)
            {
                var other = objectives[k];
                if (k == o || other.SideTeam != def.SideTeam) continue;
                bool blocks = ownSide ? other.OrderIndex > def.OrderIndex : other.OrderIndex < def.OrderIndex;
                if (!blocks) continue;
                if (states[k].Owner != team) return false;
            }
            return true;
        }

        bool Unlocked(int o, byte team) => Unlocked(map.Objectives, States, o, team);

        public void Step(SimWorld w)
        {
            if (w.WinnerTeam >= 0) return;
            for (int c = 0; c < counts.Length; c++) counts[c] = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & ((uint)UnitFlags.Vehicle | (uint)UnitFlags.Emplacement)) != 0) continue;
                var cell = map.NavCellOf(w.Position[i]);
                short o = cellObjective[map.NavIndex(cell.x, cell.y)];
                if (o >= 0) counts[o * 2 + (w.Team[i] & 1)]++;
            }

            for (int o = 0; o < States.Length; o++)
            {
                var def = map.Objectives[o];
                var s = States[o];
                if (s.Owner > 1) continue;                       // neutral objectives are a mission feature (A6)
                byte attacker = (byte)(1 - s.Owner);
                int attackers = counts[o * 2 + attacker], defenders = counts[o * 2 + s.Owner];
                if (attackers >= def.RequiredUnits && defenders == 0 && Unlocked(o, attacker))
                {
                    s.CapturingTeam = attacker;
                    s.CaptureProgressTicks++;
                    if (s.CaptureProgressTicks >= def.CaptureTicks)
                    {
                        s.Owner = attacker; s.CaptureProgressTicks = 0; s.CapturingTeam = 255;
                        States[o] = s;
                        Captured(w, o, attacker);
                        continue;
                    }
                }
                else if (attackers == 0 && s.CaptureProgressTicks > 0)
                {
                    s.CaptureProgressTicks--;
                    if (s.CaptureProgressTicks == 0) s.CapturingTeam = 255;
                }
                States[o] = s;
            }
        }

        void Captured(SimWorld w, int o, byte team)
        {
            var def = map.Objectives[o];
            w.Events.Add(w.Tick, SimEventType.ObjectiveCaptured, def.Id, team);
            if (def.CellCount > 0)
            {
                short trench = map.CellTrenchId[map.ObjectiveCells[def.CellStart]];
                if (trench >= 0 && trench < fields.Trenches.Length)
                {
                    var ts = fields.Trenches[trench];
                    ts.OwnerTeam = team; ts.Locked = 0; ts.HoldFire = 0;
                    fields.Trenches[trench] = ts;
                    w.Events.Add(w.Tick, SimEventType.TrenchCaptured, trench, team);
                }
            }
            if (def.Kind == ObjectiveKind.HQ && !w.Config.Endless)   // a Proving Ground match never ends (SimConfig.Endless)
            {
                w.WinnerTeam = team;
                w.Events.Add(w.Tick, SimEventType.MatchEnded, team);
            }
        }

        public ulong Hash(ulong h) => States.IsCreated ? SimHash.Array(States, h) : h;

        public void Dispose()
        {
            if (States.IsCreated) States.Dispose();
            if (counts.IsCreated) counts.Dispose();
            if (cellObjective.IsCreated) cellObjective.Dispose();
        }
    }
}
