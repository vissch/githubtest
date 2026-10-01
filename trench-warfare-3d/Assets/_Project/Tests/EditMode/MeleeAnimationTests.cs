// Phase: melee (2026-10-01) — hand to hand as the figures play it: a blow the sim struck (MeleeBlow) is a stab, the butt
// or an overhead smash by its style; a blow turned aside is the defender's block; and between blows a man fighting
// stands on guard, facing his man. What would go wrong silently: Stance.Melee (9) is a stance the controller had no
// clip for, so a brawl was two men idling, or kneeling to aim at each other from arm's length.
using System;
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Tests
{
    public class MeleeAnimationTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;
            public readonly int Man, Foe;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
                Man = M.World.Spawn(0, 0, new float3(150f, 0f, 300f), 100f, 3f, false);
                Foe = M.World.Spawn(1, 0, new float3(151.2f, 0f, 300f), 100f, 3f, false);
                M.World.StanceOf[Man] = M.World.StanceOf[Foe] = (byte)Stance.Melee;
                Tick();
            }

            public void Tick()
            {
                M.World.StanceOf[Man] = M.World.StanceOf[Foe] = (byte)Stance.Melee;   // the sim holds them in the fight
                A.Tick(M.World);
                A.Advance(M.World.Config.TickSeconds);
                M.World.Events.Clear();
                M.World.Tick++;
            }

            /// <summary>The man strikes his foe this tick: style 0 stab, 1 butt, 2 smash, 3 fists; damage 0 = blocked.</summary>
            public void Blow(int style, float damage)
                => M.World.Events.Add(M.World.Tick, SimEventType.MeleeBlow, Man, Foe, M.World.Position[Man], new float3(1f, style, 0f), damage);

            public Clip ClipOf(int slot) => A.State[slot].Clip;

            public void Dispose() { A.Dispose(); M.Dispose(); row.Dispose(); phase.Dispose(); yaw.Dispose(); }
        }

        [TestCase(0, Clip.MeleeStab)]
        [TestCase(1, Clip.MeleePunch)]
        [TestCase(2, Clip.MeleeSmash)]
        [TestCase(3, Clip.MeleePunch)]
        public void ABlow_PlaysTheClipOfItsStyle(int style, Clip want)
        {
            using var r = new Rig();
            r.Blow(style, 60f);
            r.Tick();
            Assert.AreEqual(want, r.ClipOf(r.Man));
        }

        [Test]
        public void ABlowTurnedAside_IsTheDefendersBlock()
        {
            using var r = new Rig();
            r.Blow(0, 0f);
            r.Tick();
            Assert.AreEqual(Clip.MeleeBlock, r.ClipOf(r.Foe), "the defender blocks");
            Assert.AreEqual(Clip.MeleeStab, r.ClipOf(r.Man), "and the attacker still swung");
        }

        [Test]
        public void TheStrikerStepsIn_AndTheManStruckGivesGround()
        {
            using var r = new Rig();
            float2 toFoe = math.normalize((r.M.World.Position[r.Foe] - r.M.World.Position[r.Man]).xz);
            r.Blow(0, 60f);
            float into = 0f, back = 0f;
            for (int t = 0; t < 14; t++)
            {
                r.Tick();
                into = math.max(into, math.dot(r.A.Lunge[r.Man], toFoe));
                back = math.max(back, math.dot(r.A.Lunge[r.Foe], toFoe));
            }
            Assert.Greater(into, 0.3f, "the striker is drawn stepping into his blow");
            Assert.Greater(back, 0.2f, "the man it landed on is drawn driven back, away from him");
            for (int t = 0; t < 40; t++) r.Tick();
            Assert.Less(math.length(r.A.Lunge[r.Man]) + math.length(r.A.Lunge[r.Foe]), 0.02f, "and both are back on their places after it");
        }

        [Test]
        public void ABlowTurnedAside_DrivesNobodyBack()
        {
            using var r = new Rig();
            float2 toFoe = math.normalize((r.M.World.Position[r.Foe] - r.M.World.Position[r.Man]).xz);
            r.Blow(0, 0f);
            float back = 0f;
            for (int t = 0; t < 14; t++) { r.Tick(); back = math.max(back, math.dot(r.A.Lunge[r.Foe], toFoe)); }
            Assert.Less(back, 0.02f, "he blocked it: he holds his ground");
        }

        [Test]
        public void BetweenBlows_HeStandsOnGuard_AndNeverKneels()
        {
            using var r = new Rig();
            for (int t = 0; t < 120; t++)
            {
                r.Tick();
                Assert.AreEqual((byte)Stance.Standing, r.A.State[r.Man].Stance, $"tick {t}: he left his feet in a brawl ({r.ClipOf(r.Man)})");
            }
            Assert.AreEqual(Clip.AimedIdle, r.ClipOf(r.Man), "on guard, rifle up");
        }
    }
}
