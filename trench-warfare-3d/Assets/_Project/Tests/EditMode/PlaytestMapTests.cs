// Phase: M1.5 playtest (implemented) — the two-line map plays like the 2D game: reinforcements stop at the first
// trench they own, >> moves them up the chain, a locked trench passes them on, the lines fall in order, and
// infantry can take a tank apart from close range while small arms at a distance cannot touch it.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class PlaytestMapTests
    {
        const int Rifleman = 0, Tank = 6;   // FactionRoster.Slot: slots 6-9 are the machines on both sides

        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static void RunUntil(MatchSim m, int maxTicks, System.Func<bool> done)
        {
            for (int t = 0; t < maxTicks && !done(); t++) Step(m);
        }

        static float TrenchZ(MatchSim m, short trench)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[trench].CellStart]).z;

        [Test]
        public void Reinforcements_StopAtTheRearTrench_AndAdvanceUpTheChain()
        {
            using var m = NewMatch();
            Assert.AreEqual(4, m.Map.Trenches.Length);
            Assert.AreEqual(0, m.Fields.RearTrench(0));
            Assert.AreEqual(1, m.Fields.FrontTrench(0));
            Assert.AreEqual(3, m.Fields.RearTrench(1));
            Assert.AreEqual(2, m.Fields.FrontTrench(1));

            for (int k = 0; k < 6; k++) Step(m, SimCommand.Deploy(m.World.Tick, 0, Rifleman));
            RunUntil(m, 1200, () => m.Fields.Trenches[0].GarrisonCount == 6);
            Assert.AreEqual(6, m.Fields.Trenches[0].GarrisonCount, "fresh men stop in the reserve trench");
            Assert.AreEqual(0, m.Fields.Trenches[1].GarrisonCount);

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 0 });
            RunUntil(m, 1200, () => m.Fields.Trenches[1].GarrisonCount == 6);
            Assert.AreEqual(6, m.Fields.Trenches[1].GarrisonCount, ">> moves them up to the front trench");
        }

        [Test]
        public void LockedRearTrench_PassesReinforcementsThroughToTheFront()
        {
            using var m = NewMatch();
            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchLock, A = 0, B = 1 });
            for (int k = 0; k < 5; k++) Step(m, SimCommand.Deploy(m.World.Tick, 0, Rifleman));
            RunUntil(m, 2400, () => m.Fields.Trenches[1].GarrisonCount == 5);
            Assert.AreEqual(5, m.Fields.Trenches[1].GarrisonCount);
            Assert.AreEqual(0, m.Fields.Trenches[0].GarrisonCount);
        }

        [Test]
        public void EnemyLines_FallInOrder_FrontThenReserveThenHq()
        {
            using var m = NewMatch();
            var w = m.World;
            // five men dropped just short of the undefended enemy front trench, ordered into it
            int goal = m.Fields.GetGoal(GoalKey.Trench(2));
            for (int k = 0; k < 5; k++)
            {
                int s = w.Spawn(0, Rifleman, new float3(130f + k * 3f, 0f, TrenchZ(m, 2) - 25f), 100f, 3f, false);
                w.GoalId[s] = goal; w.Flags[s] |= (uint)UnitFlags.Exposed;
            }
            RunUntil(m, 900, () => m.Fields.Trenches[2].OwnerTeam == 0);
            Assert.AreEqual(0, m.Fields.Trenches[2].OwnerTeam, "front trench captured");
            Assert.AreEqual(2, m.Fields.FrontTrench(0));
            Assert.AreEqual(3, m.Fields.FrontTrench(1), "the enemy falls back on his reserve line");

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 2 });
            RunUntil(m, 1500, () => m.Fields.Trenches[3].OwnerTeam == 0);
            Assert.AreEqual(0, m.Fields.Trenches[3].OwnerTeam, "reserve trench captured");
            Assert.AreEqual(-1, w.WinnerTeam);

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 3 });
            RunUntil(m, 1500, () => w.WinnerTeam >= 0);
            Assert.AreEqual(0, w.WinnerTeam, "then the HQ");
        }

        [Test]
        public void Infantry_CloseAssaultsATank_ButCannotHurtItFromRange()
        {
            using var m = NewMatch();
            var w = m.World;
            int tank = w.Spawn(1, Tank, new float3(150f, 0f, 240f), 3000f, 0f, true);
            for (int k = 0; k < 6; k++) w.Spawn(0, Rifleman, new float3(140f + k * 4f, 0f, 190f), 100f, 0f, false);   // 50 m away, standing still
            for (int t = 0; t < 200; t++) Step(m);
            Assert.AreEqual(3000f, w.Hp[tank], "rifles at 50 m do nothing to armour");

            for (int k = 0; k < 8; k++) w.Spawn(0, Rifleman, new float3(146f + k, 0f, 235f), 100f, 0f, false);        // at the hull
            RunUntil(m, 1200, () => !w.IsAlive(tank));
            Assert.IsFalse(w.IsAlive(tank), "men at the hull destroy it with grenades");
        }
    }
}
