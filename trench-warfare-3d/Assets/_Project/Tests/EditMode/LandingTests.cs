// Phase: A3 (implemented) — the coast and the boats that land on it. (Owner, 2026-09-22: "we're also adding an
// ocean with boats arriving. bringing units", and the sea is beyond the ENEMY line, so it is team 1 that lands.)
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class LandingTests
    {
        static MatchSim Coast(uint seed = 1917, int silver = 100000)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = silver; cfg.Seed = 0xC0FFEEu;
            var field = BattlefieldParams.ShelledForest(seed);
            field.Bombardment = 0f;   // a quiet sector: these tests count the men who land, and a stray shell is not the subject
            return MatchSim.CreateBattlefield(cfg, field);
        }

        static void Run(MatchSim m, int ticks)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < ticks; t++) m.Step(none);
        }

        static void Deploy(MatchSim m, byte player, int slot, int times = 1)
        {
            var list = new SimCommand[times];
            for (int i = 0; i < times; i++) list[i] = SimCommand.Deploy(m.World.Tick, player, slot);
            using var cmds = new NativeArray<SimCommand>(list, Allocator.Temp);
            m.Step(cmds);
        }

        [Test]
        public void TheMapRunsOnIntoSandAndWater_WithoutMovingTheBattle()
        {
            var p = BattlefieldParams.ShelledForest(7);
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.IsTrue(map.HasSea, "the generated field is a coast");
            Assert.AreEqual(p.Length + BattlefieldGenerator.SeaMargin, map.SizeMeters.y, .001f, "the coast is added to the map, not taken out of the layout");
            Assert.AreEqual(p.Length, map.SeaStartZ, .001f);
            Assert.AreEqual((byte)1, map.SeaTeam, "the sea lies behind the enemy");

            // the trenches and both HQs are where they always were: nothing of the battle moved onto the beach
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var cell = map.NavCellCenter(map.TrenchCells[map.Trenches[t].CellStart]);
                Assert.Less(cell.z, map.SeaStartZ, $"trench {t} is inland of the beach");
            }

            // the sand falls from the rear's own height, through the waterline, into the shallows
            float top = map.Height.Sample(map.SizeMeters.x * .5f, map.SeaStartZ + .5f);
            float atShore = map.Height.Sample(map.SizeMeters.x * .5f, map.ShoreZ);
            float out20 = map.Height.Sample(map.SizeMeters.x * .5f, map.ShoreZ + 12f);
            Assert.Greater(top, map.SeaLevel + .8f, "the top of the beach is dry land");
            Assert.AreEqual(map.SeaLevel, atShore, .45f, "the water meets the sand at ShoreZ");
            Assert.Less(out20, map.SeaLevel - .3f, "and the bed goes on down under the water");

            // no step where the beach joins the field: a man walking off the map's last trench must not fall
            float inland = map.Height.Sample(map.SizeMeters.x * .5f, map.SeaStartZ - 1f);
            Assert.AreEqual(inland, top, .55f, "the beach starts at the height the rear ground has");
        }

        [Test]
        public void ADeployForTheSeaTeam_PutsACraftInTheWaterInsteadOfAManOnTheField()
        {
            using var m = Coast();
            int before = m.World.AliveCount, silver = m.World.Silver[1];
            Deploy(m, 1, 0);
            Assert.AreEqual(before, m.World.AliveCount, "nobody appears out of the air behind the enemy line");
            Assert.AreEqual(silver - m.World.Roster[RosterEntry.SlotCount + 0].Cost, m.World.Silver[1], "and he is paid for at once");
            int running = 0;
            for (int i = 0; i < m.Landing.Count; i++) if (m.Landing.StateOf(i) != LandingState.Idle) running++;
            Assert.AreEqual(1, running, "one craft is inbound");
        }

        [Test]
        public void TheCraftGroundsAndPutsItsMenOnTheSand()
        {
            using var m = Coast();
            Deploy(m, 1, 0, 6);
            // where each man FIRST stood: after that he walks off inland, so his position later says nothing about
            // where he was put down
            var landedAt = new System.Collections.Generic.Dictionary<int, float3>();
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int tick = 0; tick < 500; tick++)                       // 96 m at 7 m/s, plus the ramp
            {
                m.Step(none);
                for (int i = 0; i < m.World.HighWater; i++)
                    if (m.World.IsAlive(i) && !landedAt.ContainsKey(i)) landedAt[i] = m.World.Position[i];
            }
            Assert.AreEqual(6, m.World.AliveCount, "every man aboard came ashore");
            Assert.AreEqual(6, landedAt.Count);
            foreach (var p in landedAt.Values)
            {
                Assert.LessOrEqual(p.z, m.Map.SizeMeters.y, "he is on the map");
                Assert.Less(m.Map.Offshore(p.z), 1f, "he was put down at the waterline, not out at sea");
                Assert.Greater(p.z, m.Map.SeaStartZ, "and on the beach, not inland of it");
            }
            bool retracting = false;
            for (int i = 0; i < m.Landing.Count; i++) if (m.Landing.StateOf(i) == LandingState.Retracting || m.Landing.StateOf(i) == LandingState.Idle) retracting = true;
            Assert.IsTrue(retracting, "an empty craft pulls off the beach again");
        }

        [Test]
        public void TheLandedMenWalkInlandToTheirLine()
        {
            using var m = Coast();
            Deploy(m, 1, 0, 4);
            Run(m, 400);
            float first = 0f; int found = 0;
            for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i)) { first += m.World.Position[i].z; found++; }
            Assert.Greater(found, 0);
            first /= found;
            Run(m, 600);
            float later = 0f; int still = 0;
            for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i)) { later += m.World.Position[i].z; still++; }
            later /= math.max(1, still);
            Assert.Less(later, first - 2f, "they leave the beach and head for the line rather than standing at the water");
        }

        [Test]
        public void TheOtherSideStillWalksUpFromItsOwnRear()
        {
            using var m = Coast();
            Deploy(m, 0, 0);
            Assert.AreEqual(1, m.World.AliveCount, "team 0 has no sea behind it, so its reinforcements arrive as before");
            for (int i = 0; i < m.World.HighWater; i++)
                if (m.World.IsAlive(i)) Assert.Less(m.World.Position[i].z, 40f, "at its own end of the field");
        }

        [Test]
        public void MoreMenThanTheBoatsCanCarry_StillArrive()
        {
            using var m = Coast();
            int total = SeaLandingSystem.MaxCraft * SeaLandingSystem.Berths + 12;
            Deploy(m, 1, 0, total);
            int cost = m.World.Roster[RosterEntry.SlotCount].Cost;
            Assert.AreEqual(100000 - total * cost, m.World.Silver[1], "each man is charged once, whether he walks or sails");
            Run(m, 600);
            Assert.AreEqual(total, m.World.AliveCount, "nobody is lost when every berth is full: the overflow comes up from the rear");
        }

        [Test]
        public void TheLandingsAreTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = Coast();
                Deploy(m, 1, 0, 5);
                Run(m, 320);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play(), "same seed, same tide, same boats");
        }

        [Test]
        public void ACraftCarriesEitherMenOrOneTank()
        {
            using var m = Coast();
            Deploy(m, 1, 6);                                // the Tusk: FactionRoster.Slot puts Brass's tank in slot 6
            Run(m, 500);
            int tanks = 0;
            for (int i = 0; i < m.World.HighWater; i++)
                if (m.World.IsAlive(i) && VehicleArchetype.IsTank(m.World.Archetype[i])) tanks++;
            Assert.AreEqual(1, tanks, "a tank comes ashore off its own boat");
        }
    }
}
