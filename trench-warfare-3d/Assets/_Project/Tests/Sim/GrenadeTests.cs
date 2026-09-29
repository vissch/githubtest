// Phase: A2 (implemented 2026-09-28) — the bomb and the running man. A man in the open bombs the trench man he is
// fighting from 5-17.6 m, the burst goes off on BlastSystem, and he spends one of his; he holds it while a friend
// stands by the mark; a gunner carries none and the next man in a slot carries his own. A running man is a harder
// mark the farther off he is, and no harder close up.
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
    public class GrenadeTests
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

        static float TrenchZ(MatchSim m, short t)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[t].CellStart + m.Map.Trenches[t].CellCount / 2]).z;

        /// <summary>A rifleman of team 1 walked into its front trench at x 120, and an unkillable rifleman of team 0
        /// standing still <paramref name="off"/> metres in front of him. Returns (defender, attacker).</summary>
        static int2 Face(MatchSim m, float off)
        {
            var w = m.World;
            short t = m.Fields.FrontTrench(1);
            float z = TrenchZ(m, t);
            var e = w.Roster[1 * RosterEntry.SlotCount + 0];
            int d = w.Spawn(1, e.Archetype, new float3(120f, 0f, z + 6f), e.Hp, e.Speed, false);
            w.GoalId[d] = m.Fields.GetGoal(GoalKey.Trench(t));
            for (int k = 0; k < 600 && w.TrenchId[d] < 0; k++) Step(m);
            Assert.GreaterOrEqual(w.TrenchId[d], 0, "setup: he got into his trench");
            for (int k = 0; k < 40; k++) Step(m);   // up onto the fire step
            float3 at = w.Position[d]; at.z -= off;
            int a = w.Spawn(0, InfantryArchetype.Rifle, at, 100000f, 0f, false);
            w.MaxHp[a] = 100000f;
            return new int2(d, a);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Func<SimEvent, bool> stop = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                bool done = false;
                for (int k = 0; k < ev.Length; k++) { log.Add(ev[k]); if (stop != null && stop(ev[k])) done = true; }
                if (done) break;
            }
            return log;
        }

        [Test]
        public void AManInTheOpen_BombsTheTrenchManHeIsFighting()
        {
            using var m = NewMatch();
            var s = Face(m, 14f);
            Assert.AreEqual(2, m.Fire.GrenadesLeft(m.World, s.y), "a rifleman goes in with two");
            var log = Run(m, 300, e => e.Type == SimEventType.Explosion && e.A == SourceId.Grenade);
            var thrown = log.FindIndex(e => e.Type == SimEventType.GrenadeThrown);
            Assert.GreaterOrEqual(thrown, 0, "he threw one");
            var throwEv = log[thrown];
            Assert.AreEqual(s.y, throwEv.A);
            Assert.AreEqual(s.x, throwEv.B, "at the man he was fighting");
            Assert.That(throwEv.Scalar, Is.InRange(CombatTables.GrenadeMin, CombatTables.GrenadeRange));
            int flight = CombatTables.GrenadeFlightTicks(throwEv.Scalar, m.World.Config.TickSeconds);
            Assert.That(flight * m.World.Config.TickSeconds, Is.InRange(0.5f, 1.3f), "a bomb thrown 5-17.6 m is in the air half a second to a second and a bit");
            var burst = log.Find(e => e.Type == SimEventType.Explosion && e.A == SourceId.Grenade);
            Assert.AreEqual(throwEv.Tick + (uint)flight, burst.Tick, "and it went off when it landed, not the tick he threw it");
            float3 landed = throwEv.Pos + throwEv.Dir;
            Assert.Less(math.distance(new float2(landed.x, landed.z), new float2(burst.Pos.x, burst.Pos.z)), 0.01f, "where the throw said it would land");
            Assert.AreEqual(1, m.Fire.GrenadesLeft(m.World, s.y), "one spent");
        }

        [Test]
        public void BeyondHisArm_HeShoots()
        {
            using var m = NewMatch();
            var s = Face(m, 40f);
            var log = Run(m, 300);
            Assert.IsFalse(log.Exists(e => e.Type == SimEventType.GrenadeThrown), "40 m is past GrenadeRange");
            Assert.IsTrue(log.Exists(e => e.Type == SimEventType.Shot && e.A == s.y), "he fires his rifle instead");
        }

        [Test]
        public void HeHoldsHisBomb_WhileAFriendStandsByTheMark()
        {
            using var m = NewMatch();
            var s = Face(m, 14f);
            var w = m.World;
            float3 by = w.Position[s.x]; by.z -= 2f;   // a friend at the parapet, two metres from the man in it
            int friend = w.Spawn(0, InfantryArchetype.Medic, by, 100000f, 0f, false);
            w.MaxHp[friend] = 100000f;
            var log = Run(m, 300);
            Assert.IsFalse(log.Exists(e => e.Type == SimEventType.GrenadeThrown), "not onto his own man");
            Assert.IsTrue(log.Exists(e => e.Type == SimEventType.Shot && e.A == s.y), "he fires instead");
        }

        [Test]
        public void TheGunnerCarriesNone_TheBomberFour_AndTheNextManInASlotHisOwn()
        {
            Assert.AreEqual(0, CombatTables.GrenadesFor(InfantryArchetype.Machinegunner));
            Assert.AreEqual(0, CombatTables.GrenadesFor(InfantryArchetype.Medic));
            Assert.AreEqual(4, CombatTables.GrenadesFor(InfantryArchetype.Assault));
            using var m = NewMatch();
            var s = Face(m, 14f);
            var w = m.World;
            Run(m, 600, e => e.Type == SimEventType.GrenadeThrown && m.Fire.GrenadesLeft(w, s.y) == 0);
            Assert.AreEqual(0, m.Fire.GrenadesLeft(w, s.y), "he threw both");
            var at = w.Position[s.y];
            w.Despawn(s.y, -1, default);
            int next = w.Spawn(0, InfantryArchetype.Rifle, at, 100f, 0f, false);
            Assert.AreEqual(s.y, next, "setup: the slot is reused");
            Assert.AreEqual(2, m.Fire.GrenadesLeft(w, next), "the next man brings his own");
        }

        [Test]
        public void ARunningMan_IsAHarderMarkTheFartherOffHeIs()
        {
            Assert.AreEqual(1f, CombatTables.RunningTarget(80f, 1.5f), "a walking man is no harder");
            Assert.AreEqual(1f, CombatTables.RunningTarget(CombatTables.RunningTargetNear, 4.5f), "close up it makes no odds");
            Assert.AreEqual(CombatTables.RunningTargetFloor, CombatTables.RunningTarget(100f, 4.5f), 1e-6f);
            float mid = CombatTables.RunningTarget(40f, 4.5f);
            Assert.Less(mid, 1f); Assert.Greater(mid, CombatTables.RunningTargetFloor);
        }
    }
}
