// Phase: P0 (implemented) — command validation rules from docs/02-contracts.md
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class CommandValidationTests
    {
        static int CountEvents(SimWorld w, SimEventType t)
        {
            int n = 0; for (int i = 0; i < w.Events.Events.Length; i++) if (w.Events.Events[i].Type == t) n++; return n;
        }

        [Test]
        public void Deploy_SpendsSilver_AndSpawns()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100;
            using var m = MatchSim.CreateGreybox(cfg);
            using var cmds = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 0) }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(1, m.World.AliveCount);
            Assert.AreEqual(75, m.World.Silver[0]);
            Assert.AreEqual(1, CountEvents(m.World, SimEventType.UnitSpawned));
        }

        [Test]
        public void Deploy_WithoutSilver_IsRejected()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 10;
            using var m = MatchSim.CreateGreybox(cfg);
            using var cmds = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 4) }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(0, m.World.AliveCount);
            Assert.AreEqual(1, CountEvents(m.World, SimEventType.CommandRejected));
        }

        [Test]
        public void SlotCooldown_BlocksSecondSpecial()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 1000;
            using var m = MatchSim.CreateGreybox(cfg);
            using var cmds = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 3), SimCommand.Deploy(0, 0, 3) }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(1, m.World.AliveCount);
        }

        [Test]
        public void Rally_IsClampedToMap()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            using var cmds = new NativeArray<SimCommand>(new[] { SimCommand.Rally(0, 1, new float3(-50f, 0f, 5000f)) }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(0f, m.World.Rally[1].x);
            Assert.AreEqual(m.Map.SizeMeters.y, m.World.Rally[1].z);
        }

        [Test]
        public void Commands_AreOrderedByPlayer_RegardlessOfArrivalOrder()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 1000;
            using var a = MatchSim.CreateGreybox(cfg);
            using var b = MatchSim.CreateGreybox(cfg);
            using var c1 = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 1, 0), SimCommand.Deploy(0, 0, 1) }, Allocator.Temp);
            using var c2 = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 1), SimCommand.Deploy(0, 1, 0) }, Allocator.Temp);
            a.Step(c1); b.Step(c2);
            Assert.AreEqual(a.World.LastHash, b.World.LastHash);
        }
    }
}
