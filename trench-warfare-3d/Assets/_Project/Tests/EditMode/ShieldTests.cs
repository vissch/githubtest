// Phase: A3 (implemented 2026-09-25) — the shield bearer: his plate stops a rifle from the front, an MG holes it
// some of the time and a sniper always, a man standing behind him is not shot, from behind he is a man like any
// other, a line with shields in front loses fewer men, and it is the same on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class ShieldTests
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

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                each?.Invoke(m.World);
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Still(MatchSim m, byte team, byte archetype, float x, float z, float hp = -1f)
        {
            var at = new float3(x, 0f, z);
            Assert.AreEqual(0, (int)(m.Map.LayerAt(at) & NavLayer.Trench), $"setup: ({x},{z}) must be open ground");
            var e = RosterEntry.ForArchetype(archetype);
            return m.World.Spawn(team, archetype, at, hp < 0f ? e.Hp : hp, 0f, false);
        }

        static int Hits(List<SimEvent> log, int target, bool positive)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == SimEventType.Hit && e.B == target && (e.Scalar > 0f) == positive) n++;
            return n;
        }

        [Test]
        public void ARifleNeverHolesThePlateFromTheFrontAndTheManBehindHimIsNotShot()
        {
            using var m = NewMatch();
            const float z = 30f;
            int shield = Still(m, 0, InfantryArchetype.Shield, 60f, z);        // team 0 spawns facing +Z
            int behind = Still(m, 0, InfantryArchetype.Rifle, 60f, z - 2.5f);
            for (int k = 0; k < 4; k++) Still(m, 1, InfantryArchetype.Rifle, 57f + k * 2f, z + 30f);
            m.World.Suppression[behind] = 100f;   // he keeps his head down: this is about who gets shot, not who shoots
            var log = Run(m, 400, w => { w.Suppression[behind] = 100f; w.Yaw[shield] = 0f; });
            Assert.Greater(Hits(log, shield, false), 5, "the plate rang");
            Assert.AreEqual(0, Hits(log, shield, true), "a rifle does not hole 8 mm");
            Assert.AreEqual(RosterEntry.Shield.Hp, m.World.Hp[shield]);
            Assert.AreEqual(0, Hits(log, behind, true), "the man behind him was never hit");
            Assert.Greater(log.FindAll(e => e.Type == SimEventType.ShieldBlocked && e.A == shield).Count, 5);
        }

        [Test]
        public void FromBehindHeIsAManLikeAnyOther()
        {
            using var m = NewMatch();
            const float z = 60f;
            int shield = Still(m, 0, InfantryArchetype.Shield, 60f, z);
            for (int k = 0; k < 4; k++) Still(m, 1, InfantryArchetype.Rifle, 57f + k * 2f, z - 30f);   // behind his facing
            var log = Run(m, 400, w => { w.Suppression[shield] = 100f; w.Yaw[shield] = 0f; });          // pinned, so he does not turn to them
            Assert.Greater(Hits(log, shield, true), 3, "rounds in the back");
            Assert.AreEqual(0, Hits(log, shield, false));
        }

        [Test]
        public void AMachineGunHolesThePlateSomeOfTheTimeAndASniperAlways()
        {
            using var m = NewMatch();
            const float z = 30f;
            int shield = Still(m, 0, InfantryArchetype.Shield, 60f, z, 1e6f);
            Still(m, 1, InfantryArchetype.Machinegunner, 60f, z + 30f);
            var log = Run(m, 400, w => w.Yaw[shield] = 0f);
            int holed = Hits(log, shield, true), stopped = Hits(log, shield, false);
            Assert.Greater(holed, 0, "an MG round at 9 mm gets through sometimes");
            Assert.Greater(stopped, 0, "and is stopped sometimes");
            Assert.That(holed / (float)(holed + stopped), Is.EqualTo(0.78f).Within(0.2f));

            using var s = NewMatch(0xBEEF);
            int shield2 = Still(s, 0, InfantryArchetype.Shield, 60f, z, 1e6f);
            Still(s, 1, InfantryArchetype.Sniper, 60f, z + 40f);
            var log2 = Run(s, 400, w => w.Yaw[shield2] = 0f);
            Assert.Greater(Hits(log2, shield2, true), 0);
            Assert.AreEqual(0, Hits(log2, shield2, false), "nothing stops a sniper's round");
        }

        static int Deaths(List<SimEvent> log, SimWorld w, HashSet<int> men)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == SimEventType.Death && men.Contains(e.A)) n++;
            return n;
        }

        [Test]
        public void ALineWithShieldsInFrontLosesFewerMen()
        {
            int Fight(bool shields)
            {
                using var m = NewMatch();
                const float z = 30f;
                var men = new HashSet<int>();
                for (int k = 0; k < 6; k++) men.Add(Still(m, 0, InfantryArchetype.Rifle, 54f + k * 2.4f, z - 2.5f));
                if (shields) for (int k = 0; k < 3; k++) Still(m, 0, InfantryArchetype.Shield, 56f + k * 4.8f, z);
                for (int k = 0; k < 8; k++) Still(m, 1, InfantryArchetype.Rifle, 52f + k * 2.4f, z + 35f);
                var log = Run(m, 600);
                return Deaths(log, m.World, men);
            }
            int bare = Fight(false), covered = Fight(true);
            Assert.GreaterOrEqual(bare, 2, "setup: the bare line must lose men for the comparison to mean anything");
            Assert.LessOrEqual(covered, bare * 0.6f, $"with shields {covered} died, without {bare}");
        }

        [Test]
        public void TheShieldIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                for (int k = 0; k < 3; k++) Still(m, 0, InfantryArchetype.Shield, 56f + k * 4.8f, 30f);
                for (int k = 0; k < 6; k++) Still(m, 0, InfantryArchetype.Rifle, 54f + k * 2.4f, 27.5f);
                for (int k = 0; k < 8; k++) Still(m, 1, InfantryArchetype.Rifle, 52f + k * 2.4f, 65f);
                Still(m, 1, InfantryArchetype.Machinegunner, 60f, 66f);
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
