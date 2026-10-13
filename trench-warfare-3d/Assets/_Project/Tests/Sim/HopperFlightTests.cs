// Phase: A5b (implemented 2026-10-13) — the Hopper flies. Owner's brief: "Flies over everything, never lands;
// rifles and machine guns can shoot it down, tank guns and mines cannot."
// This file is the path half: a flyer crosses a trench, a wire belt, a house's footprint and a wreck in a straight
// line at its full speed, sets off no mine, runs nobody down, and plays the same twice.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class HopperFlightTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Func<bool> done = null, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                each?.Invoke(m.World);
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
                if (done != null && done()) break;
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        /// <summary>A Hopper of <paramref name="team"/> with its default goal, the enemy HQ: the point FlightSystem flies
        /// it at. Up the middle of the map, so the line it takes runs straight along +Z.</summary>
        static int SpawnHopper(MatchSim m, byte team, float3 at)
        {
            var r = m.World.Units.Roster[VehicleArchetype.Hopper];
            int slot = m.World.Spawn(team, VehicleArchetype.Hopper, at, r.Hp, r.Speed, true);
            m.World.GoalId[slot] = m.Fields.DefaultGoal(team, true);
            return slot;
        }

        static void PaintBand(MapData map, float z0, float z1, NavLayer add)
        {
            for (int z = (int)(z0 / MapData.NavCellSize); z <= (int)(z1 / MapData.NavCellSize); z++)
            for (int x = 0; x < map.NavWidth; x++)
                map.SetLayer(x, z, (NavLayer)map.NavLayers[map.NavIndex(x, z)] | add);
            map.RebuildCost();
        }

        /// <summary>Everything a machine on the ground has to go round or through, laid across the middle of the field:
        /// a trench band wider than any tank bridges, a wire belt, a house's footprint (NavLayer.Blocked, what a
        /// building paints) and a wreck. Returns the wreck's prop index.</summary>
        static int LayTheGround(MatchSim m)
        {
            PaintBand(m.Map, 150f, 158f, NavLayer.Trench);
            PaintBand(m.Map, 180f, 184f, NavLayer.Wire);
            PaintBand(m.Map, 210f, 216f, NavLayer.Blocked);
            return m.Map.AddProp(new PropDef { Pos = new float3(m.Map.SizeMeters.x * 0.5f, 0f, 240f), Kind = PropKind.Wreck, Hp = PropRules.StartHp(PropKind.Wreck) });
        }

        [Test]
        public void ItFliesOverEverythingInAStraightLineAtItsFullSpeed()
        {
            using var m = NewMatch();
            int wreck = LayTheGround(m);
            float x = m.Map.SizeMeters.x * 0.5f;
            int it = SpawnHopper(m, 0, new float3(x, 0f, 120f));
            Step(m);
            Assert.AreNotEqual(0u, m.World.Flags[it] & (uint)UnitFlags.Flying, "it is flying the tick it is put down");
            Assert.AreEqual(VehicleKinematicsSystem.DriveStraight, m.Vehicles.Drive[it], "and is driven in a line, not on its field");
            float3 target = m.Vehicles.DriveTarget[it];
            Assert.Greater(target.z, 300f, "the enemy HQ is up the field");
            // from where it was put, facing its line: what is tested is the flight, not the first turn
            m.World.Position[it] = new float3(x, 0f, 120f);
            m.World.Velocity[it] = float3.zero;
            m.World.Yaw[it] = SimMath.YawOf(math.normalize(new float3(target.x - x, 0f, target.z - 120f)));
            float slowest = float.MaxValue, offLine = 0f;
            int settle = 60;   // ticks to gather way at Accel 2.0 before its speed is held to account
            int tick = 0;
            var log = Run(m, 1600, () => m.World.Position[it].z > 260f, w =>
            {
                if (tick++ < settle) return;
                slowest = math.min(slowest, math.length(w.Velocity[it]));
                offLine = math.max(offLine, math.abs(w.Position[it].x - x));
            });
            float speed = m.World.Units.Roster[VehicleArchetype.Hopper].Speed;
            Assert.Greater(m.World.Position[it].z, 260f, "it crossed the trench, the wire, the house and the wreck");
            Assert.GreaterOrEqual(slowest, speed * 0.95f, "nothing on the ground slowed it");
            Assert.Less(offLine, 1f, "and it held its line");
            Assert.AreEqual(0, Count(log, SimEventType.WireBreached), "no wire broken");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleCrushed, it), "nothing crushed, felled or ground under it");
            Assert.AreEqual(0, Count(log, SimEventType.PropChanged), "the wreck and every tree stand");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleDitched, it), "no ditching");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleBogged, it), "no bogging");
            Assert.AreEqual(PropKind.Wreck, m.Map.Props[wreck].Kind, "the wreck is untouched");
            Assert.AreEqual(PropRules.StartHp(PropKind.Wreck), m.Map.Props[wreck].Hp, 1e-3f, "and unworn");
            Assert.AreEqual(-1, m.Vehicles.CrossTrench[it], "it never took a trench on");
        }

        [Test]
        public void NoMineGoesOffUnderIt()
        {
            using var m = NewMatch();
            float x = m.Map.SizeMeters.x * 0.5f;
            int it = SpawnHopper(m, 0, new float3(x, 0f, 150f));
            Step(m);
            m.World.Position[it] = new float3(x, 0f, 150f);
            m.World.Yaw[it] = 0f;
            var mines = new List<int>();
            for (float z = 170f; z <= 230f; z += 10f)
            {
                int id = m.Mines.Place(m.World, new float3(x, 0f, z), float3.zero, 0f, 1, MineKind.Mine);
                Assert.GreaterOrEqual(id, 0, "setup: a mine on its line");
                mines.Add(id);
            }
            int wire = m.Mines.Place(m.World, new float3(x - 10f, 0f, 240f), new float3(1f, 0f, 0f), 20f, 1, MineKind.Tripwire);
            Assert.GreaterOrEqual(wire, 0, "setup: a tripwire across its line");
            Run(m, MineSystem.ArmTicks + 2);
            var log = Run(m, 1200, () => m.World.Position[it].z > 250f);
            Assert.Greater(m.World.Position[it].z, 250f, "it flew the length of the minefield");
            Assert.AreEqual(0, Count(log, SimEventType.MineTriggered, int.MinValue, it), "no mine and no tripwire reached it");
            foreach (int id in mines) Assert.AreEqual((int)MineState.Armed, m.Mines.Mines[id].State, "every mine still lies armed");
            Assert.AreEqual((int)MineState.Armed, m.Mines.Mines[wire].State, "and the tripwire is unbroken");
            Assert.IsTrue(m.World.IsAlive(it));
        }

        [Test]
        public void ItRunsNobodyDown()
        {
            using var m = NewMatch();
            float x = m.Map.SizeMeters.x * 0.5f;
            var men = new List<int>();
            for (float z = 180f; z <= 220f; z += 10f)
            {
                int man = m.World.Spawn(1, InfantryArchetype.Rifle, new float3(x, 0f, z), 100f, 3f, false);
                m.World.GoalId[man] = -1;        // he stands where he is put, in the gunship's way
                m.World.Flags[man] |= (uint)UnitFlags.HoldFire;
                men.Add(man);
            }
            int it = SpawnHopper(m, 0, new float3(x, 0f, 150f));
            Step(m);
            m.World.Position[it] = new float3(x, 0f, 150f);
            m.World.Yaw[it] = 0f;
            int crushed = 0;
            var log = Run(m, 1600, () => m.World.Position[it].z > 240f, w =>
            {
                foreach (int man in men) if (w.Hp[man] <= 100f - VehicleKinematicsSystem.CrushDamage) crushed++;
            });
            Assert.Greater(m.World.Position[it].z, 240f, "it flew over the lot of them");
            Assert.AreEqual(0, crushed, "nobody was run down");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleCrushed, it), "its hull touched no man");
        }

        [Test]
        public void TwoWorldsStayEqual()
        {
            ulong Play()
            {
                using var m = NewMatch();
                LayTheGround(m);
                float x = m.Map.SizeMeters.x * 0.5f;
                SpawnHopper(m, 0, new float3(x, 0f, 120f));
                m.Mines.Place(m.World, new float3(x, 0f, 200f), float3.zero, 0f, 1, MineKind.Mine);
                Run(m, 900);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
