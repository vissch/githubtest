// Phase: A3 (implemented 2026-09-25) — the Victoria Cross moment: a forced pity makes exactly one hero who rallies
// the men round him over the top, a hero cannot be pinned and hits harder and is himself again when his window
// closes, nobody becomes a hero without desperation, a veteran's rank rides in the deploy command, the hero state
// is in the hash, and a battle with a hero in it is the same on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class HeroTests
    {
        static MatchSim NewMatch(float pity = 0f, uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed; cfg.HeroPity0 = pity;
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

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a)) n++;
            return n;
        }

        /// <summary>Riflemen of team 0 deployed the normal way and walked into their rear trench.</summary>
        static short Garrison(MatchSim m, int count)
        {
            short t = m.Fields.RearTrench(0);
            int want = m.Fields.Trenches[t].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, 0, 0));
            for (int i = 0; i < 2000 && m.Fields.Trenches[t].GarrisonCount < want; i++) Step(m);
            Assert.GreaterOrEqual(m.Fields.Trenches[t].GarrisonCount, want - 2, "setup: garrison did not form (a man or two may still be on the ladder)");
            return t;
        }

        static void PinTeam(SimWorld w, byte team)
        {
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team && (w.Flags[i] & (uint)UnitFlags.Hero) == 0) w.Suppression[i] = 95f;
        }

        [Test]
        public void AForcedPityMakesExactlyOneHeroWhoRalliesTheMenRoundHim()
        {
            using var m = NewMatch(pity: 1f);
            m.Hero.BaseChance = 0f;   // the dice never roll: only the pity can make him
            short t = Garrison(m, 12);
            var log = Run(m, 40, () => m.Hero.HeroCount > 0, w => PinTeam(w, 0));
            Assert.AreEqual(1, Count(log, SimEventType.HeroMoment), "one hero");
            var moment = log.Find(e => e.Type == SimEventType.HeroMoment);
            int hero = moment.A;
            Assert.Less(moment.Scalar, 0f, "the pity forced it");
            Assert.AreNotEqual(0u, m.World.Flags[hero] & (uint)UnitFlags.Hero);
            Assert.AreEqual(-1, m.World.TrenchId[hero], "he left the trench");
            Assert.GreaterOrEqual(m.World.GoalId[hero], 0, "for the next one");
            Assert.AreEqual(GoalKind.Trench, m.Fields.Goals[m.World.GoalId[hero]].Kind);
            int rallied = 0;
            for (int i = 0; i < m.World.HighWater; i++)
                if (i != hero && m.World.IsAlive(i) && m.World.Team[i] == 0 && m.World.TrenchId[i] < 0 && m.World.GoalId[i] == m.World.GoalId[hero]) rallied++;
            Assert.GreaterOrEqual(rallied, 3, "the men round him went over the top with him");
            Assert.AreEqual(0f, m.Hero.Pity[0]);
            Assert.Greater(m.Hero.Cooldown[0], 0);
            Assert.AreEqual(1, m.Hero.ForcedUsed[0]);
            log = Run(m, 100, null, w => PinTeam(w, 0));
            Assert.AreEqual(0, Count(log, SimEventType.HeroMoment), "one live hero a side, and the pity is spent");
        }

        [Test]
        public void AHeroCannotBePinnedHitsHarderAndIsHimselfAgainWhenTheWindowCloses()
        {
            using var m = NewMatch(pity: 1f);
            m.Hero.BaseChance = 0f;
            Garrison(m, 6);
            var log = Run(m, 40, () => m.Hero.HeroCount > 0, w => PinTeam(w, 0));
            int hero = log.Find(e => e.Type == SimEventType.HeroMoment).A;
            float baseSpeed = m.Hero.BaseSpeed[hero];
            Assert.AreEqual(baseSpeed * HeroRules.SpeedBonus, m.World.Speed[hero]);
            Assert.AreEqual(HeroRules.DamageBonus, m.Hero.HeroScale[hero]);
            Run(m, 10, null, w => w.Suppression[hero] = 100f);
            Assert.LessOrEqual(m.World.Suppression[hero], HeroRules.SuppressionCap);
            Assert.AreNotEqual((byte)Stance.Pinned, m.World.StanceOf[hero]);
            Step(m);
            Assert.AreEqual(HeroRules.DamageBonus, m.Aura.DamageMul[hero], "his scale reaches the one multiplier DirectFire reads");
            log = Run(m, HeroRules.WindowTicks + 5, () => Count(log, SimEventType.HeroSurvived) > 0 || m.Hero.HeroTicks[hero] == 0);
            Assert.AreEqual(1, Count(log, SimEventType.HeroSurvived, hero));
            Assert.AreEqual(baseSpeed, m.World.Speed[hero]);
            Assert.AreEqual(1f, m.Hero.HeroScale[hero]);
            Assert.AreEqual(0u, m.World.Flags[hero] & (uint)UnitFlags.Hero);
        }

        [Test]
        public void NoDesperationNoHero()
        {
            using var m = NewMatch();
            Garrison(m, 12);
            var log = Run(m, 600);
            Assert.AreEqual(0, Count(log, SimEventType.HeroMoment));
            Assert.AreEqual(0f, m.Hero.Pity[0]);
        }

        [Test]
        public void AVeteranTakesHisRankFromTheDeployCommand()
        {
            using var m = NewMatch();
            Step(m, SimCommand.Deploy(m.World.Tick, 0, 0, 2));
            var ev = m.World.Events.Events;
            int slot = -1; int rankEv = 0;
            for (int k = 0; k < ev.Length; k++) if (ev[k].Type == SimEventType.VeteranDeployed) { slot = ev[k].A; rankEv = ev[k].B; }
            Assert.GreaterOrEqual(slot, 0, "VeteranDeployed");
            Assert.AreEqual(2, rankEv);
            Assert.AreEqual(2, m.Hero.VeteranRank[slot]);
            Assert.That(m.World.MaxHp[slot], Is.EqualTo(120f).Within(0.01f));
            Assert.That(m.Hero.HeroScale[slot], Is.EqualTo(1.2f).Within(0.001f));
            Assert.AreNotEqual(0u, m.World.Flags[slot] & (uint)UnitFlags.Veteran);
            Step(m, SimCommand.Deploy(m.World.Tick, 0, 0));
            ev = m.World.Events.Events;
            for (int k = 0; k < ev.Length; k++) Assert.AreNotEqual(SimEventType.VeteranDeployed, ev[k].Type, "an ordinary deploy is nobody");
        }

        [Test]
        public void TheHeroStateIsInTheHash()
        {
            using var a = NewMatch(); using var b = NewMatch();
            Step(a); Step(b);
            Assert.AreEqual(a.World.Hash(), b.World.Hash());
            a.Hero.Pity[0] = 0.5f;
            Assert.AreNotEqual(a.World.Hash(), b.World.Hash());
        }

        [Test]
        public void ABattleWithAHeroInItIsTheSameOnEveryMachine()
        {
            (ulong, uint) Play()
            {
                using var m = NewMatch(pity: 0.9f);
                short t = Garrison(m, 8);
                float z = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[t].CellStart]).z;
                int goal = m.Fields.GetGoal(GoalKey.Trench(t));
                for (int k = 0; k < 14; k++)
                {
                    int s = m.World.Spawn(1, 0, new float3(110f + k * 3f, 0f, z + 70f), 100f, 3f, false);
                    m.World.GoalId[s] = goal; m.World.Flags[s] |= (uint)UnitFlags.Exposed;
                }
                uint first = 0;
                for (int i = 0; i < 700; i++)
                {
                    Step(m);
                    if (first == 0) { var ev = m.World.Events.Events; for (int k = 0; k < ev.Length; k++) if (ev[k].Type == SimEventType.HeroMoment) first = m.World.Tick; }
                }
                return (m.World.Hash(), first);
            }
            var x = Play(); var y = Play();
            Assert.AreEqual(x.Item1, y.Item1);
            Assert.AreEqual(x.Item2, y.Item2);
        }
    }
}
