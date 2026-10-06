// Phase: deaths (2026-09-28, implemented) — the absurd deaths (DeathGags) on top of the ladder: at intensity 0 nothing
// changes; each cause gets its gag (a punt away from the gun, a machine gun's jig, a headshot's helmet, a shell's
// rocket and a heap's fountain, a track's pancake, a claw's fling, gas's wilt or plank, fire's skid, the beam's boots);
// a man in a trench goes up, not out; the same death chooses the same gag; nothing passes the caps; choosing allocates
// nothing. The rig is DeathVarietyTests': men placed, killed through SimWorld.Despawn, the controller ticked.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Perf;

namespace TW.Tests
{
    public class DeathGagTests
    {
        static readonly float3 Here = new float3(150f, 0f, 300f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            public SimWorld W => M.World;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig(float intensity)
            {
                DeathGags.Pin(intensity);
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float3 at, byte team = 0, byte archetype = 0) => W.Spawn(team, archetype, at, 100f, 3f, false);
            public int Tank(float3 at, byte team = 1) => W.Spawn(team, 4, at, 5000f, 2f, true);

            public void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            public DeathRecord Record(int slot, uint at) { Assert.IsTrue(A.TryDeath(slot, at, out var rec), "the death is on the ring"); return rec; }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
                DeathGags.Pin(-1f);
            }
        }

        [TearDown] public void Unpin() => DeathGags.Pin(-1f);

        static GagInput Input(DeathKind cause, Clip clip = Clip.DeathBack, uint seed = 12345u) => new GagInput
        {
            Cause = cause, Clip = clip, Travel = new float3(0f, 0f, 1f), BodyYaw = 0.3f, KillerYaw = 1.1f, Seed = seed, Tick = 5000,
        };

        [Test]
        public void AtIntensityZeroChoosingChangesNothing()
        {
            var rng = new System.Random(3);
            foreach (DeathKind cause in Enum.GetValues(typeof(DeathKind)))
                for (int n = 0; n < 200; n++)
                {
                    var g = Input(cause, (Clip)rng.Next((int)Clip.DeathFront, (int)Clip.DeathThrown + 1), (uint)rng.Next());
                    g.Flat = rng.Next(2) == 0; g.Crushed = rng.Next(4) == 0; g.Clawed = rng.Next(4) == 0; g.Heavy = rng.Next(2) == 0;
                    g.Throw = rng.Next(2) == 0 ? new float3(3f, 2f, 1f) : float3.zero; g.Density = rng.Next(6);
                    float3 fly = g.Throw; Clip clip = g.Clip; float yaw = g.BodyYaw;
                    var plan = DeathGags.Choose(g, 0f, ref fly, ref clip, ref yaw);
                    Assert.IsFalse(plan.Any, "no gag at 0");
                    Assert.AreEqual(g.Throw, fly); Assert.AreEqual(g.Clip, clip); Assert.AreEqual(g.BodyYaw, yaw);
                }
        }

        [Test]
        public void AtIntensityZeroTheControllersDeathIsTodaysAndTheDefaultIsTheNewLook()
        {
            DeathRecord Blasted(float intensity)
            {
                using (var r = new Rig(intensity))
                {
                    int man = r.Man(Here); r.Tick(); uint at = r.W.Tick;
                    r.W.Despawn(man, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 10f); r.Tick();
                    return r.Record(man, at);
                }
            }
            var today = Blasted(0f);
            Assert.IsFalse(today.Gag.Any);
            var on = Blasted(1f);
            var unset = Blasted(-1f);   // the knob, unset: the default intensity
            Assert.AreEqual(1f, DeathGags.DefaultIntensity, "the owner turned the new deaths on (2026-09-30)");
            Assert.AreEqual(new float3(on.ThrowX, on.ThrowUp, on.ThrowZ), new float3(unset.ThrowX, unset.ThrowUp, unset.ThrowZ));
            Assert.AreEqual(on.Clip, unset.Clip);
            Assert.AreNotEqual(new float3(today.ThrowX, today.ThrowUp, today.ThrowZ), new float3(unset.ThrowX, unset.ThrowUp, unset.ThrowZ), "a blast throws further with the new look on");
        }

        [Test]
        public void AHeavyHitPuntsHimAwayFromTheGunFacingIt()
        {
            using var r = new Rig(1f);
            int killer = r.Man(Here + new float3(0f, 0f, -40f), team: 1);
            int man = r.Man(Here);
            r.Tick(); uint at = r.W.Tick;
            r.W.Events.Add(r.W.Tick, SimEventType.Hit, killer, man, r.W.Position[man], new float3(0f, 0f, 1f), 90f);   // a heavy hit (> 0.4 of his hp)
            r.W.Despawn(man, killer, new float3(0f, 0f, 1f));
            r.Tick();
            var rec = r.Record(man, at);
            Assert.AreEqual(DeathGag.Punt, rec.Gag.Gag);
            Assert.AreEqual(Clip.DeathThrown, rec.Clip);
            Assert.Greater(rec.ThrowZ, 4.9f, "thrown along the round's line, away from the gun");
            Assert.AreEqual(0f, rec.ThrowX, 1e-3f);
            Assert.Greater(rec.ThrowUp, 1.4f);
            Assert.AreEqual(math.PI, math.abs(rec.Yaw), 1e-3f, "facing the gun that punted him");
            Assert.AreEqual(2, rec.Gag.Bounces);
        }

        [Test]
        public void AMachineGunJigsHimOnTheSpotFirst()
        {
            using var r = new Rig(1f);
            int gunner = r.Man(Here + new float3(30f, 0f, 0f), team: 1, archetype: TW.Sim.InfantryArchetype.Machinegunner);
            int man = r.Man(Here);
            r.Tick(); uint at = r.W.Tick;
            r.W.Despawn(man, gunner, new float3(-1f, 0f, 0f));
            r.Tick();
            var rec = r.Record(man, at);
            Assert.AreEqual(DeathGag.Jig, rec.Gag.Gag);
            Assert.GreaterOrEqual(rec.Gag.Delay, 0.35f); Assert.LessOrEqual(rec.Gag.Delay, 0.6f);
            Assert.Greater(rec.Gag.Jig, 0f);
            Assert.Less(rec.ThrowX, -1.9f, "then punted along the burst");
        }

        [Test]
        public void AHeadshotPopsTheHelmetAndMayTakeTheHead()
        {
            var g = Input(DeathKind.Shot, Clip.DeathHeadshot);
            float3 fly = 0; var clip = g.Clip; float yaw = 0f;
            var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
            Assert.AreEqual(DeathGag.HeadPop, plan.Gag);
            Assert.AreNotEqual(0, plan.Flags & GagFlags.HelmetPop);
            Assert.AreNotEqual(0, plan.Flags & GagFlags.HeadOff, "CombatFx rolls the head against GORE");
            Assert.Greater(plan.PulseQ, 0, "a jolt up the body");
            Assert.AreEqual(Clip.DeathHeadshot, clip, "he drops in place");
            Assert.AreEqual(0f, fly.y);
        }

        [Test]
        public void ATrackMakesAPancakeAndThrowsNobody()
        {
            using var r = new Rig(1f);
            int man = r.Man(Here);
            int tank = r.Tank(Here + new float3(0f, 0f, 3f));
            r.W.Yaw[tank] = 0.7f;
            r.Tick(); uint at = r.W.Tick;
            r.W.Events.Add(r.W.Tick, SimEventType.VehicleCrushed, tank, 2, r.W.Position[man]);
            r.W.Despawn(man, tank, new float3(0f, 0f, -1f));
            r.Tick();
            var rec = r.Record(man, at);
            Assert.AreEqual(DeathGag.Pancake, rec.Gag.Gag);
            Assert.AreEqual(0f, rec.ThrowUp);
            Assert.AreEqual(-32, rec.Gag.RestQ, "an eighth of his height");
            Assert.AreEqual(0.7f, rec.Yaw, 1e-4f, "pressed flat along the track");
            Assert.AreNotEqual(0, rec.Gag.Flags & GagFlags.Feet);
        }

        [Test]
        public void AClawLiftsHimThenFlingsHim()
        {
            using var r = new Rig(1f);
            int man = r.Man(Here);
            int walker = r.W.Spawn(1, VehicleArchetype.Pincer, Here + new float3(0f, 0f, 4f), 2300f, 2f, true);
            r.Tick(); uint at = r.W.Tick;
            r.W.Events.Add(r.W.Tick, SimEventType.VehicleClawed, walker, man, r.W.Position[man]);
            r.W.Despawn(man, walker, new float3(0f, 0f, -0.5f));
            r.Tick();
            var rec = r.Record(man, at);
            Assert.AreEqual(DeathGag.Flung, rec.Gag.Gag);
            Assert.AreEqual(2.2f, rec.Gag.HoldLift, 1e-4f);
            Assert.AreEqual(0.3f, rec.Gag.Delay, 1e-4f);
            Assert.GreaterOrEqual(rec.ThrowUp, 5f);
            Assert.GreaterOrEqual(math.length(new float2(rec.ThrowX, rec.ThrowZ)), 8f);
            Assert.GreaterOrEqual(rec.Gag.Rolls, 2);
        }

        [Test]
        public void GasNeverLaunchesAMan()
        {
            for (uint seed = 1; seed < 60; seed++)
            {
                var g = Input(DeathKind.Gas, Clip.DeathKneel, seed);
                float3 fly = 0; var clip = g.Clip; float yaw = 0f;
                var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
                Assert.IsTrue(plan.Gag == DeathGag.Wilt || plan.Gag == DeathGag.Plank, plan.Gag.ToString());
                Assert.AreEqual(0f, fly.y);
                if (plan.Gag == DeathGag.Plank) { Assert.AreEqual(8, plan.Topple); Assert.AreNotEqual(0, plan.Flags & GagFlags.Freeze); }
            }
        }

        [Test]
        public void ABurningManSkidsOnTheWayHeRan()
        {
            using var r = new Rig(1f);
            int man = r.Man(Here);
            r.Tick();
            r.A.SetAlight(man, 5f);
            r.Tick(); uint at = r.W.Tick;
            r.W.Despawn(man, (int)DeathCause.Burning, new float3(0f, 0f, 1f), 0f);
            r.Tick();
            var rec = r.Record(man, at);
            Assert.AreEqual(DeathGag.Skid, rec.Gag.Gag);
            Assert.GreaterOrEqual(rec.Gag.Skid, 2.5f); Assert.LessOrEqual(rec.Gag.Skid, DeathGags.SkidCap);
            Assert.AreEqual(0f, rec.ThrowUp);
        }

        [Test]
        public void TheBeamLeavesOnlyHisBoots()
        {
            var g = Input(DeathKind.Beam, Clip.DeathRunning);
            float3 fly = 0; var clip = g.Clip; float yaw = 0f;
            Assert.AreEqual(DeathGag.Boots, DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw).Gag);
            Assert.AreNotEqual(0, DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw).Flags & GagFlags.NoCorpse);
            Assert.AreEqual(DeathGag.Skid, DeathGags.Choose(g, 0.3f, ref fly, ref clip, ref yaw).Gag, "below BootsFrom he burns where he fell");
        }

        [Test]
        public void AShellRocketsHimFurtherHigherAndBouncing()
        {
            float3 Today(float intensity, out DeathRecord rec)
            {
                using var r = new Rig(intensity);
                int man = r.Man(Here); r.Tick(); uint at = r.W.Tick;
                r.W.Despawn(man, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 10f); r.Tick();
                rec = r.Record(man, at);
                return new float3(rec.ThrowX, rec.ThrowUp, rec.ThrowZ);
            }
            var before = Today(0f, out _); var after = Today(1f, out var gagged);
            Assert.AreEqual(DeathGag.Rocket, gagged.Gag.Gag);
            Assert.AreEqual(DeathGags.RocketFar, after.x / before.x, 0.01f, "nearly twice as far");
            Assert.AreEqual(math.min(DeathGags.RocketHigh * before.y, DeathGags.HighCap), after.y, 0.01f, "and higher");
            Assert.AreEqual(2, gagged.Gag.Bounces);
            Assert.GreaterOrEqual(gagged.Gag.Flips, 1);
        }

        [Test]
        public void AManInATrenchGoesUpNotOut()
        {
            var shot = Input(DeathKind.Shot); shot.InTrench = true; shot.Heavy = true;
            float3 fly = 0; var clip = shot.Clip; float yaw = 0f;
            Assert.IsFalse(DeathGags.Choose(shot, 2f, ref fly, ref clip, ref yaw).Any, "a shot never throws a man out of his trench");

            var blast = Input(DeathKind.Blast, Clip.DeathThrown); blast.Throw = new float3(1f, 3f, 0f); blast.InTrench = true;
            fly = blast.Throw;
            var plan = DeathGags.Choose(blast, 1f, ref fly, ref clip, ref yaw);
            Assert.Less(fly.x, 1f * 1.9f * 0.51f, "half the open field's reach again");
            Assert.LessOrEqual(plan.Bounces, 1); Assert.AreEqual(0f, plan.Skid);
        }

        [Test]
        public void AProneManIsHoppedNotPunted()
        {
            var g = Input(DeathKind.Shot, Clip.DeathProne); g.Flat = true; g.Heavy = true;
            float3 fly = 0; var clip = g.Clip; float yaw = 0f;
            var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
            Assert.AreEqual(DeathGag.Flop, plan.Gag);
            Assert.AreEqual(0f, fly.x); Assert.AreEqual(0f, fly.z); Assert.AreEqual(0.4f, fly.y, 1e-4f);
            Assert.AreEqual(Clip.DeathProne, clip);
        }

        [Test]
        public void TheSameDeathChoosesTheSameGag()
        {
            DeathRecord Once()
            {
                using var r = new Rig(1f);
                var others = new int[4];
                for (int k = 0; k < 4; k++) others[k] = r.Man(Here + new float3(0.8f * k, 0f, 0.6f));
                int man = r.Man(Here);
                r.Tick(); uint at = r.W.Tick;
                foreach (int o in others) r.W.Despawn(o, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 6f);
                r.W.Despawn(man, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 6f);
                r.Tick();
                return r.Record(man, at);
            }
            var a = Once(); var b = Once();
            Assert.AreEqual(DeathGag.Fountain, a.Gag.Gag, "a heap fountains");
            Assert.AreEqual(a.Gag.Gag, b.Gag.Gag); Assert.AreEqual(a.Gag.Seed, b.Gag.Seed); Assert.AreEqual(a.Gag.Delay, b.Gag.Delay);
            Assert.AreEqual(a.ThrowX, b.ThrowX); Assert.AreEqual(a.ThrowZ, b.ThrowZ); Assert.AreEqual(a.ThrowUp, b.ThrowUp);
        }

        [Test]
        public void NoGagPassesTheCaps()
        {
            var rng = new System.Random(11);
            var causes = (DeathKind[])Enum.GetValues(typeof(DeathKind));
            for (int n = 0; n < 10000; n++)
            {
                var g = Input(causes[rng.Next(causes.Length)], (Clip)rng.Next((int)Clip.DeathFront, (int)Clip.DeathThrown + 1), (uint)rng.Next());
                g.Flat = rng.Next(3) == 0; g.Crushed = rng.Next(5) == 0; g.Clawed = rng.Next(5) == 0; g.Heavy = rng.Next(2) == 0;
                g.MachineGun = rng.Next(3) == 0; g.InTrench = rng.Next(4) == 0; g.Density = rng.Next(8);
                g.Throw = rng.Next(2) == 0 ? new float3((float)rng.NextDouble() * 28f - 14f, (float)rng.NextDouble() * 9f, (float)rng.NextDouble() * 28f - 14f) : float3.zero;
                float a = (float)rng.NextDouble() * DeathGags.MaxIntensity;
                float3 fly = g.Throw; var clip = g.Clip; float yaw = 0f;
                var p = DeathGags.Choose(g, a, ref fly, ref clip, ref yaw);
                Assert.LessOrEqual(math.length(fly.xz), DeathGags.FarCap + 1e-3f);
                Assert.LessOrEqual(fly.y, DeathGags.HighCap + 1e-3f);
                Assert.LessOrEqual(p.Flips, DeathGags.FlipCap); Assert.LessOrEqual(p.Rolls, DeathGags.RollCap); Assert.LessOrEqual(p.Bounces, DeathGags.BounceCap);
                Assert.LessOrEqual(p.Skid, DeathGags.SkidCap + 1e-4f); Assert.LessOrEqual(p.Delay, DeathGags.DelayCap + 1e-4f);
            }
        }

        [Test]
        public void ChoosingAGagAllocatesNothing()
        {
            var g = Input(DeathKind.Blast, Clip.DeathThrown); g.Throw = new float3(3f, 4f, 1f); g.Density = 3;
            double perCall = AllocProbe.PerCall(() =>
            {
                float3 fly = g.Throw; var clip = g.Clip; float yaw = 0f;
                DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
            }, 10, 200);
            Assert.AreEqual(0.0, perCall, 1e-9, "no garbage a death");
        }
    }
}
