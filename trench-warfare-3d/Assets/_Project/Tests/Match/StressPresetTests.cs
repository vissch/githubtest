// Phase: S04 (AOSA, 2026-09-25) - the stress preset spreads the player's army over its trenches instead of standing it
// all in the rear one. On the bench's battlefield (ShelledForest 1917) the rear trench has 79 posts; at 1,500 a side the
// old preset stood 1,409 men in it, 1,330 of them with no post. The spread preset locks a trench once it is full, so
// the rear one fills its posts, the front one fills next, and the rest go on. These run the preset as SimHost does
// (LockstepSession + ScriptedEnemy) at 200 a side, which already overflows both of the player's trenches, and hold:
// the spread preset leaves no crowd in the rear trench and mans the front one; StressSpread = false is still the old
// preset (the crowd, an empty front trench), so old and new can be measured in one build; and the locks, which are
// orders read from the player's own world, play the same match in single player and in the canary under latency.
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class StressPresetTests
    {
        const int PerSide = 200, Ticks = 900;

        struct Outcome
        {
            public int RearGarrison, RearPostless, FrontGarrison, Alive, HeroCount, HeroTeamMask;
            public bool RearLocked, FrontLocked, Desync;
            public ulong Hash;
        }

        static Outcome Play(bool spread, bool canary)
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = PerSide * 25;   // SimHost's stress silver
            using var session = new LockstepSession(() => SimHost.StressPreset(MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u))),
                                                    canary, canary ? 2 : 0, canary ? 1 : 0, canary ? 0.05f : 0f, cfg.Seed);
            var ai = new ScriptedEnemy { StressUnits = PerSide, StressSpread = spread };
            int guard = Ticks * 40;
            while (session.Local.World.Tick < Ticks && guard-- > 0) session.StepOnce(ai);
            Assert.That(guard > 0, $"lockstep stalled (spread {spread}, canary {canary})");

            var m = session.Local;
            var w = m.World;
            short rear = m.Fields.RearTrench(0), front = m.Fields.FrontTrench(0);
            Assert.That(rear >= 0 && front >= 0 && rear != front, "the player should own a rear and a front trench at this tick");
            var o = new Outcome { RearLocked = m.Fields.Trenches[rear].Locked != 0, FrontLocked = m.Fields.Trenches[front].Locked != 0,
                                  Desync = session.Desync, Hash = w.Hash(), Alive = w.AliveCount,
                                  HeroCount = w.GetSystem<TW.Sim.Combat.HeroSystem>().HeroCount,
                                  HeroTeamMask = w.GetSystem<TW.Sim.Combat.HeroSystem>().TeamMask };
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || w.Team[i] != 0) continue;
                if (w.TrenchId[i] == rear) { o.RearGarrison++; if (w.PostKind[i] == 0) o.RearPostless++; }
                else if (w.TrenchId[i] == front) o.FrontGarrison++;
            }
            return o;
        }

        [Test]
        public void Spread_FillsTheRearTrenchsPosts_ThenTheFrontTrench()
        {
            var o = Play(spread: true, canary: false);
            TestContext.WriteLine($"spread: rear {o.RearGarrison} ({o.RearPostless} without a post, locked {o.RearLocked}), front {o.FrontGarrison} (locked {o.FrontLocked}), alive {o.Alive}");
            Assert.That(o.RearLocked, "the rear trench filled and should have been locked, so later men pass on");
            Assert.That(o.RearPostless <= 16, $"{o.RearPostless} men stand in the rear trench without a post: the crowd is back");
            Assert.That(o.FrontGarrison >= 40, $"only {o.FrontGarrison} men hold the front trench: the overflow never reached it");
        }

        [Test]
        public void SpreadOff_IsThePresetBeforeS04()
        {
            var o = Play(spread: false, canary: false);
            TestContext.WriteLine($"old: rear {o.RearGarrison} ({o.RearPostless} without a post, locked {o.RearLocked}), front {o.FrontGarrison}, alive {o.Alive}");
            Assert.That(!o.RearLocked && !o.FrontLocked, "the old preset locks nothing");
            Assert.That(o.RearPostless >= 60, $"the old preset stands the whole army in the rear trench; only {o.RearPostless} there have no post");
            Assert.That(o.FrontGarrison == 0, $"the old preset never mans the front trench, and {o.FrontGarrison} men hold it");
        }

        /// <summary>The owner, 2026-10-06: the stress preset fields no hero on either side, so the bench times a steady
        /// scene. A hero need not appear at 200 a side in 900 ticks, so the mask is the proof, with the id counter beside it.</summary>
        [Test]
        public void StressPreset_FieldsNoHeroOnEitherSide()
        {
            var o = Play(spread: true, canary: false);
            TestContext.WriteLine($"heroes: mask {o.HeroTeamMask}, {o.HeroCount} ever raised");
            Assert.That(o.HeroTeamMask, Is.EqualTo(0), "the stress preset let a team field a hero");
            Assert.That(o.HeroCount, Is.EqualTo(0), $"{o.HeroCount} heroes rose in the stress preset");
        }

        /// <summary>The gate, not the preset: a match built with StressUnits == 0 is left alone, so its heroes are the
        /// sim's own (HeroSystem.TeamMask = 1). Remove the gate in SimHost.ApplyStressPreset and this goes red.</summary>
        [Test]
        public void NoStressUnits_KeepsItsHeroes()
        {
            Knobs.Clear();
            var cfg = SimConfig.Default;
            using var plain = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u));
            using var stressed = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u));
            Assert.That(SimHost.ApplyStressPreset(0, plain).World.GetSystem<TW.Sim.Combat.HeroSystem>().TeamMask, Is.EqualTo(1),
                        "a match that is not a stress run should keep the sim's own hero mask");
            Assert.That(SimHost.ApplyStressPreset(PerSide, stressed).World.GetSystem<TW.Sim.Combat.HeroSystem>().TeamMask, Is.EqualTo(0),
                        "a stress run should field no hero");
        }

        [Test]
        public void StressPreset_HeroesKnob_IsThePresetBefore20261006()
        {
            try
            {
                Knobs.Set(SimHost.StressHeroesKnob, "1");
                var cfg = SimConfig.Default;
                cfg.StartingSilver = PerSide * 25;
                var m = SimHost.StressPreset(MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u)));
                Assert.That(m.World.GetSystem<TW.Sim.Combat.HeroSystem>().TeamMask, Is.EqualTo(1),
                            "stress.heroes=1 should leave the sim's own default mask alone");
            }
            finally { Knobs.Clear(); }
        }

        [Test, Category("Long")]
        public void Spread_PlaysTheSameMatch_InSinglePlayerAndTheCanaryUnderLatency()
        {
            var one = Play(spread: true, canary: false);
            var lossy = Play(spread: true, canary: true);
            Assert.That(!lossy.Desync, "the canary desynced against itself");
            Assert.AreEqual(one.Hash, lossy.Hash, "the spread preset's orders depend on the network: single player and the canary disagree");
        }
    }
}
