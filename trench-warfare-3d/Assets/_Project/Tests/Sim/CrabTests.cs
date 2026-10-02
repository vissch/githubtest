// Phase: A5b (implemented) — the owner's four crab walkers: that they walk over what a tank has to fight through,
// that they are stopped by losing legs rather than by a broken track, that their claws kill what comes close, that
// the Kettle's mortar drops shells on men it cannot see, that the Censer lays its gas as it goes, and that every bit
// of it is the same on both machines. (Owner, 2026-09-22: two crab sheets, then two more, "do the same for these".)
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Tests
{
    public class CrabTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a)) n++;
            return n;
        }

        static RosterEntry EntryFor(byte archetype)
            => archetype == VehicleArchetype.Pincer ? RosterEntry.Pincer
             : archetype == VehicleArchetype.Kettle ? RosterEntry.Kettle
             : archetype == VehicleArchetype.Censer ? RosterEntry.Censer
             : archetype == VehicleArchetype.Pavise ? RosterEntry.Pavise
             : archetype == VehicleArchetype.Banner ? RosterEntry.Banner : RosterEntry.Redoubt;

        /// <summary>A walker, sent straight up the map like the tanks in TankTests (its default goal is the enemy HQ,
        /// which an eight-way field would take it to at forty-five degrees).</summary>
        static int SpawnCrab(MatchSim m, byte team, byte archetype, float3 at, float speed = -1f)
        {
            var entry = EntryFor(archetype);
            int slot = m.World.Spawn(team, archetype, at, entry.Hp, speed < 0f ? entry.Speed : speed, true);
            // straight up its own column, or straight down it: the default goal is the enemy HQ off to one side, and
            // a machine standing still still TURNS towards its goal, which swings a narrow gun arc off the target
            var c = m.Map.NavCellOf(at);
            int goalZ = team == 0 ? m.Map.NavLength - 6 : 5;
            m.World.GoalId[slot] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, goalZ), NavMode.Tracked));
            return slot;
        }

        static void PaintBand(MapData map, float z0, float z1, NavLayer add)
        {
            for (int z = (int)(z0 / MapData.NavCellSize); z <= (int)(z1 / MapData.NavCellSize); z++)
            for (int x = 0; x < map.NavWidth; x++)
                map.SetLayer(x, z, (NavLayer)map.NavLayers[map.NavIndex(x, z)] | add);
            map.RebuildCost();
        }

        // ------------------------------------------------------------------ what a walker is
        [Test]
        public void EachWalkerHasItsLegs_AndNoneOfThemDitchesOrBogsLikeATank()
        {
            foreach (byte a in new[] { VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt })
            {
                var p = VehicleProfile.ForArchetype(a);
                Assert.IsTrue(p.Walker, $"{a} is a walker");
                Assert.GreaterOrEqual(p.Legs, 4, $"{a} stands on at least four legs");
                Assert.AreEqual(0f, p.DitchChance, $"{a} cannot nose into a trench: it steps over");
                Assert.Greater(p.TrenchCrossWidth, VehicleProfile.Tusk.TrenchCrossWidth, $"{a} strides wider than the light tank bridges");
                Assert.Greater(p.SlopeLimit, VehicleProfile.Maw.SlopeLimit, $"{a} climbs what the heavy tank cannot");
                Assert.Less(p.BogChance, VehicleProfile.Tusk.BogChance, $"{a} finds footing where a track sinks");
                Assert.IsTrue(TankSpec.For(a).Unmanned, $"{a} has nobody inside to bail out");
                Assert.Greater(TankSpec.For(a).ClawReach, 0f, $"{a} has claws");
                Assert.AreEqual(0f, p.DitchChance);
            }
        }

        [Test]
        public void AWalkerCrossesTheTrenches_WhereTheLightTankMightDitch()
        {
            using var m = NewMatch();
            // the playtest map's own lines: 3 m wide, which the Tusk bridges at 2.4 m only by trying its luck
            float chance = VehicleProfile.Tusk.DitchChance * math.saturate((3f - VehicleProfile.Tusk.TrenchCrossWidth)
                / (FlowFieldManager.TrackedCrossWidth - VehicleProfile.Tusk.TrenchCrossWidth));
            Assert.Greater(chance, 0.3f, "a light tank really does risk ditching in these");

            int crab = SpawnCrab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 20f));
            bool ditched = false;
            for (int t = 0; t < 1000; t++) { Step(m); if (m.Vehicles.DitchTicks[crab] > 0) ditched = true; }
            Assert.Greater(m.World.Position[crab].z, 150f, "it walked over both trench lines and kept going");
            Assert.IsFalse(ditched, "and never once nosed into one: a walker steps over a trench");
        }

        [Test]
        public void AWalkerStepsOverWireWithoutCuttingIt()
        {
            using var m = NewMatch();
            PaintBand(m.Map, 36f, 40f, NavLayer.Wire);
            int before = 0;
            for (int i = 0; i < m.Map.NavLayers.Length; i++) if (((NavLayer)m.Map.NavLayers[i] & NavLayer.Wire) != 0) before++;
            int crab = SpawnCrab(m, 0, VehicleArchetype.Censer, new float3(30f, 0f, 28f));
            var log = Run(m, 600);
            int after = 0;
            for (int i = 0; i < m.Map.NavLayers.Length; i++) if (((NavLayer)m.Map.NavLayers[i] & NavLayer.Wire) != 0) after++;
            Assert.Greater(m.World.Position[crab].z, 44f, "it went through the belt");
            Assert.AreEqual(before, after, "and left every strand of it standing: a walker lifts its legs over wire");
            Assert.AreEqual(0, Count(log, SimEventType.WireBreached));
        }

        // ------------------------------------------------------------------ legs as modules
        [Test]
        public void ItLosesLegsOneAtATime_LimpsAndOnlyStopsWhenASideIsGone()
        {
            using var m = NewMatch();
            int crab = SpawnCrab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f), 0f);
            Step(m);
            int legs = VehicleProfile.Pincer.Legs;
            // rounds into its left side, one after another. Its structure and its drive cores are kept whole between
            // them, because what is being tested is the legs, not how long a Pincer lives under fire.
            float3 fromLeft = new float3(1f, 0f, 0f);
            int gone = 0;
            for (int k = 0; k < 80 && (m.World.Flags[crab] & (uint)UnitFlags.Immobilised) == 0; k++)
            {
                m.World.Hp[crab] = m.World.MaxHp[crab];
                m.Modules.Crew[crab] = m.Modules.CrewMax[crab];
                m.Gunnery.PendingHits.Add(new VehicleHit
                {
                    Target = crab, Shooter = -1, Kind = VehicleHitKind.ArmourPiercing,
                    PenMm = 90f, Damage = 30f, Pos = m.World.Position[crab], Dir = fromLeft,
                });
                Step(m);
                int now = math.countbits((uint)m.Modules.LegsLost[crab]);
                if (now == gone) continue;
                gone = now;
                Assert.LessOrEqual(gone, legs, "it cannot lose more legs than it has");
                if (gone < legs / 2)
                    Assert.AreEqual(0u, m.World.Flags[crab] & (uint)UnitFlags.Immobilised, $"with {gone} of {legs} legs gone it limps on");
                Assert.Less(m.Vehicles.SpeedFactor[crab], 1f, "and every leg it loses slows it");
            }
            Assert.Greater(gone, 0, "rounds into its side take legs off");
            Assert.AreEqual(legs / 2, gone, "the side gives out when its last leg goes, not before");
            Assert.AreNotEqual(0u, m.World.Flags[crab] & (uint)UnitFlags.Immobilised, "and then it stands still");
        }

        // ------------------------------------------------------------------ claws
        [Test]
        public void TheClawsKillWhatComesWithinReach()
        {
            using var m = NewMatch();
            int crab = SpawnCrab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f), 0f);   // its goal keeps it facing +Z
            int man = m.World.Spawn(1, 0, new float3(30f, 0f, 33f), 100f, 0f, false);
            var log = Run(m, 120);
            Assert.IsFalse(m.World.IsAlive(man), "a man who walks into the claws does not walk out");
            Assert.Greater(Count(log, SimEventType.VehicleClawed, crab), 0, "and the claw closing is reported");
        }

        [Test]
        public void TheClawsOnlyReachForward()
        {
            using var m = NewMatch();
            int crab = SpawnCrab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f), 0f);
            int behind = m.World.Spawn(1, 0, new float3(30f, 0f, 26.5f), 100f, 0f, false);
            Run(m, 120);
            Assert.IsTrue(m.World.IsAlive(behind), "a man behind it is not taken in a claw (the guns are another matter)");
        }

        // ------------------------------------------------------------------ the mortar and the gas
        [Test]
        public void TheMortarDropsShellsOnMenItCannotSee_AndNotOnItsOwnFeet()
        {
            var spec = TankSpec.Kettle;
            Assert.IsTrue(spec.Gun0.Indirect, "the Kettle lobs");
            Assert.Greater(spec.Gun0.RangeMin, 40f, "and cannot drop one close in");
            using var m = NewMatch();
            int crab = SpawnCrab(m, 1, VehicleArchetype.Kettle, new float3(30f, 0f, 120f), 0f);
            var close = new List<int>();
            for (int k = 0; k < 4; k++) close.Add(m.World.Spawn(0, 0, new float3(28f + k, 0f, 110f), 100f, 0f, false));   // inside the minimum range
            var log = Run(m, 200);
            Assert.AreEqual(0, Count(log, SimEventType.VehicleFired, crab), "nothing within the minimum range is worth a shell");
            foreach (int i in close) Assert.IsTrue(m.World.IsAlive(i), "and they are not shelled");

            using var m2 = NewMatch();
            int crab2 = SpawnCrab(m2, 1, VehicleArchetype.Kettle, new float3(30f, 0f, 200f), 0f);
            for (int k = 0; k < 6; k++) m2.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 100f, 0f, false);        // well beyond it
            var log2 = Run(m2, 300);
            Assert.Greater(Count(log2, SimEventType.VehicleFired, crab2), 0, "at proper mortar range it fires");
        }

        [Test]
        public void TheCenserLaysGasAsItWalks()
        {
            using var m = NewMatch();
            int crab = SpawnCrab(m, 0, VehicleArchetype.Censer, new float3(30f, 0f, 30f));
            Assert.IsNotNull(m.Gas);
            Run(m, 200);
            Assert.Greater(m.Gas.ConcentrationAt(m.World.Position[crab]) + m.Gas.Sources.Length, 0f, "there is chlorine behind it");
        }

        [Test]
        public void HolingTheDrumStopsTheGas()
        {
            using var m = NewMatch();
            int crab = SpawnCrab(m, 0, VehicleArchetype.Censer, new float3(30f, 0f, 30f));
            Run(m, 80);
            m.Modules.Module[crab * (int)VehicleModule.Count + (int)VehicleModule.Ammo] = 0f;
            Run(m, 200);   // longer than any source it had already laid can last
            Assert.AreEqual(0, m.Gas.Sources.Length, "with the drum holed nothing more comes out of it");
        }

        [Test]
        public void TheStandardSteadiesTheMenAroundIt()
        {
            using var m = NewMatch();
            int banner = SpawnCrab(m, 0, VehicleArchetype.Banner, new float3(30f, 0f, 60f), 0f);
            var near = new List<int>(); var far = new List<int>();
            for (int k = 0; k < 6; k++) near.Add(m.World.Spawn(0, 0, new float3(26f + k, 0f, 64f), 100f, 0f, false));
            for (int k = 0; k < 6; k++) far.Add(m.World.Spawn(0, 0, new float3(26f + k, 0f, 140f), 100f, 0f, false));
            Step(m);
            foreach (int i in near) m.World.Suppression[i] = 60f;
            foreach (int i in far) m.World.Suppression[i] = 60f;
            Run(m, 40);
            float a = 0f, b = 0f;
            foreach (int i in near) a += m.World.Suppression[i];
            foreach (int i in far) b += m.World.Suppression[i];
            Assert.Less(a, b, "men under the standard come out of suppression faster than men away from it");
        }

        [Test]
        public void TheBlockhouseCarriesNoGunAndTakesWhatTheOthersCannot()
        {
            var r = TankSpec.Redoubt;
            Assert.AreEqual(0, r.GunCount, "the Redoubt has no gun: a slit, a hatch and two claws");
            Assert.Greater(r.ClawDamage, TankSpec.Pincer.ClawDamage, "and the heaviest claws on the field");
            Assert.Greater(r.Hull.FrontMm, TankSpec.Maw.Hull.FrontMm * 2f, "its plate is thicker than either tank's");
            Assert.Greater(VehicleProfile.Redoubt.Legs, 4);
        }

        // ------------------------------------------------------------------ lockstep
        [Test]
        public void TheWalkersAreTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                SpawnCrab(m, 0, VehicleArchetype.Pincer, new float3(28f, 0f, 30f));
                SpawnCrab(m, 0, VehicleArchetype.Pavise, new float3(36f, 0f, 30f));
                SpawnCrab(m, 1, VehicleArchetype.Kettle, new float3(30f, 0f, 150f));
                SpawnCrab(m, 1, VehicleArchetype.Censer, new float3(38f, 0f, 150f));
                SpawnCrab(m, 0, VehicleArchetype.Banner, new float3(44f, 0f, 30f));
                SpawnCrab(m, 1, VehicleArchetype.Redoubt, new float3(46f, 0f, 150f));
                for (int k = 0; k < 10; k++) m.World.Spawn(0, 0, new float3(26f + k, 0f, 60f), 100f, 1.2f, false);
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play(), "same seed, same walk, same shells");
        }

        // ------------------------------------------------------------------ the roster
        [Test]
        public void EachSideHasATankAndThreeWalkers()
        {
            using var m = NewMatch();
            var w = m.World;
            Assert.AreEqual(10, RosterEntry.SlotCount);
            // the machines sit in the last four slots of each faction's ten; Iron brings the Breaker where Brass
            // brings the Redoubt, and the Pavise has gone to Iron's pool
            Assert.AreEqual(VehicleArchetype.Maw, w.Roster[6].Archetype);
            Assert.AreEqual(VehicleArchetype.Pincer, w.Roster[7].Archetype);
            Assert.AreEqual(VehicleArchetype.Banner, w.Roster[8].Archetype);
            Assert.AreEqual(VehicleArchetype.Breaker, w.Roster[9].Archetype);
            int b = RosterEntry.SlotCount;
            Assert.AreEqual(VehicleArchetype.Tusk, w.Roster[b + 6].Archetype);
            Assert.AreEqual(VehicleArchetype.Kettle, w.Roster[b + 7].Archetype);
            Assert.AreEqual(VehicleArchetype.Censer, w.Roster[b + 8].Archetype);
            Assert.AreEqual(VehicleArchetype.Redoubt, w.Roster[b + 9].Archetype);
            for (int s = 0; s < RosterEntry.SlotCount; s++) Assert.AreEqual(1, w.SlotUnlocked[s], $"slot {s} can be deployed");
        }
    }
}
