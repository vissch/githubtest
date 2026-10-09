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
                if (m.World.IsAlive(i) && ChassisKind.IsTank(m.World.ChassisOf(m.World.Archetype[i]))) tanks++;
            Assert.AreEqual(1, tanks, "a tank comes ashore off its own boat");
        }

        // [U1] AddSystem calls Initialize at once and MatchSim registers Blast after Landing, so the old code's
        // `blast = world.GetSystem<BlastSystem>()` in Initialize stayed null for good and the fleet never fired.
        [Test]
        public void TheFleetLaysAShellInland()
        {
            using var m = Coast();
            m.Landing.FleetFires = true;                        // silent unless switched on: the test below
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            int fired = 0;
            for (int t = 0; t < SeaLandingSystem.ShipEvery + 5; t++)
            {
                m.Step(none);
                var ev = m.World.Events.Events;                 // the buffer is cleared every tick
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.ShipFired) fired++;
            }
            Assert.Greater(fired, 0, "the gunboats fire once the match is past the first salvo tick");
        }

        // The owner, 2026-10-06: the guns stay silent in a match unless something switches them on (no constant
        // bombardment on the field, 2026-09-28). Three salvo ticks of a match as it is made: no shell, no burst.
        [Test]
        public void TheFleetIsSilentInAMatch_UnlessSwitchedOn()
        {
            using var m = Coast();
            Assert.IsFalse(m.Landing.FleetFires, "off as a match is made");
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            int fired = 0, bursts = 0;
            for (int t = 0; t < 3 * SeaLandingSystem.ShipEvery + 5; t++)
            {
                m.Step(none);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type == SimEventType.ShipFired) fired++;
                    if (ev[e].Type == SimEventType.Explosion && ev[e].A == SeaLandingSystem.ShipSource) bursts++;
                }
            }
            Assert.AreEqual(0, fired, "no ship fires");
            Assert.AreEqual(0, bursts, "and no naval shell lands");
        }

        // [U4] The field was full while the craft unloaded: it retracted and the men still in the hold were lost,
        // though SimWorld.Deploy had already charged for them.
        [Test]
        public void AFullFieldPaysBackTheMenItCouldNotTake()
        {
            var cfg = SimConfig.Default;
            cfg.Seed = 0xC0FFEEu; cfg.StartingSilver = 100000; cfg.SilverPerSecond = 0f; cfg.MaxSlots = 4;
            var field = BattlefieldParams.ShelledForest(1917);
            field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            Deploy(m, 1, 0, 6);                                  // one craft, Berths = 8, so all six go aboard
            int cost = m.World.Roster[RosterEntry.SlotCount].Cost;

            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            // no further than tick 450: the fleet's first salvo is at SeaLandingSystem.ShipEvery (460) and a shell
            // could free a slot and muddy the sum
            for (int t = 0; t < 450; t++) m.Step(none);

            Assert.AreEqual(4, m.World.AliveCount, "the field holds four and no more");

            Assert.AreEqual(100000 - 4 * cost, m.World.Silver[1], "four landed, the two the full field refused are paid back");
        }

        // [U4c] The owner's call of 2026-10-07: "A landing turned back by a full field gives back the unit's wait
        // with the silver." The refund paid the silver back but left the slot's cooldown running, so the button
        // the man never used stayed dark for the rest of his wait.
        [Test]
        public void AFullFieldPaysBackTheWaitWithTheSilver()
        {
            var cfg = SimConfig.Default;
            cfg.Seed = 0xC0FFEEu; cfg.StartingSilver = 100000; cfg.SilverPerSecond = 0f; cfg.MaxSlots = 4;
            var field = BattlefieldParams.ShelledForest(1917);
            field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);

            const int RedoubtSlot = 9;                           // Brass's walking fort: CooldownTicks 600, longer than the craft's run in
            int ri = 1 * RosterEntry.SlotCount + RedoubtSlot;
            Deploy(m, 0, 0, 4);                                  // player 0 is inland, so his four riflemen fill the field of four at once
            Deploy(m, 1, RedoubtSlot);                           // the sea team's fort embarks and has the hold to itself
            Assert.AreEqual(599, m.World.SlotCooldown[ri], "[U4c] his wait starts at the deploy (600, one tick already run)");

            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            int before = m.World.Silver[1], waitBefore = -1, waitOnRefund = -1;
            for (int t = 0; t < 450; t++)                        // short of the fleet's first salvo at ShipEvery (460)
            {
                int wait = m.World.SlotCooldown[ri];
                m.Step(none);
                if (m.World.Silver[1] > before) { waitBefore = wait; waitOnRefund = m.World.SlotCooldown[ri]; break; }
            }

            Assert.AreNotEqual(-1, waitOnRefund, "[U4c] the full field turned the man back and paid his silver");
            Assert.Greater(waitBefore, 1, "[U4c] the wait had not run out by itself");
            Assert.AreEqual(0, waitOnRefund, "[U4c] the wait goes back with the silver: the button is ready at once");
        }
    }
}
