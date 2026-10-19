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

        /// <summary>Pins the deploy cooldown boundary: `ApplyCoreCommands` (where Deploy sets SlotCooldown) runs
        /// before `StepEconomy` (where it decrements) in the same Step, so a slot set to N this tick is free again
        /// exactly N ticks later, not N-1. Reported as an off-by-one by a code look; it was not one. Guards against
        /// a future reorder of the two calls silently shifting every deploy by a tick.</summary>
        [Test]
        public void ADeploySlotWaitsExactlyItsCooldownTicks_RefusedAtOneTickShortTakenOnTheTick()
        {
            AssertWait(1, 3, 200);   // Brass slot 3: Sniper
            AssertWait(0, 3, 400);   // Iron slot 3: Officer
        }

        static void AssertWait(byte player, int slot, int cooldown)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 2000;
            using var m = MatchSim.CreateGreybox(cfg);
            int ri = player * RosterEntry.SlotCount + slot;
            Assert.AreEqual(cooldown, m.World.Roster[ri].CooldownTicks, "setup");
            using var one = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, player, slot) }, Allocator.Temp);
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(one);   // the deploy, at tick 0
            for (uint t = 1; t < (uint)cooldown - 1; t++) m.Step(none);   // ticks 1 .. cooldown-2
            m.Step(one);   // tick cooldown-1: one tick short
            Assert.AreEqual(1, CountEvents(m.World, SimEventType.CommandRejected), "refused one tick short");
            Assert.AreEqual(0, CountEvents(m.World, SimEventType.UnitDeployed), "and nobody came up");
            m.Step(one);   // tick cooldown: taken
            Assert.AreEqual(0, CountEvents(m.World, SimEventType.CommandRejected), "taken on the tick");
            Assert.AreEqual(1, CountEvents(m.World, SimEventType.UnitDeployed), "the second man is up");
        }

        /// <summary>A UnitAbility is the sapper's (SapperSystem, 2026-09-28): any other ability id, and a laying order for
        /// a slot that holds no sapper, is rejected rather than quietly dropped.</summary>
        [Test]
        public void UnitAbility_ThatNobodyCanCarryOut_IsRejected()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100;
            using var m = MatchSim.CreateGreybox(cfg);
            using var deploy = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 0) }, Allocator.Temp);
            m.Step(deploy);   // slot 0: a rifleman
            using var cmds = new NativeArray<SimCommand>(new[]
            {
                new SimCommand { Tick = 1, Player = 0, Type = CommandType.UnitAbility, A = 0, B = (int)TW.Sim.Units.UnitAbilityId.Bangalore, Pos = new float3(100f, 0f, 100f) },
                new SimCommand { Tick = 1, Player = 0, Type = CommandType.UnitAbility, A = 0, B = (int)TW.Sim.Units.UnitAbilityId.LayMine, Pos = new float3(100f, 0f, 100f) },
                new SimCommand { Tick = 1, Player = 0, Type = CommandType.UnitAbility, A = 999999, B = (int)TW.Sim.Units.UnitAbilityId.LayMine, Pos = new float3(100f, 0f, 100f) },
            }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(3, CountEvents(m.World, SimEventType.CommandRejected));
            Assert.AreEqual(0, CountEvents(m.World, SimEventType.SapperOrdered));
        }

        [Test]   // [S11]
        public void ACommandFromAPlayerPastMaxPlayers_IsRejectedWithAnEvent()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            using var cmds = new NativeArray<SimCommand>(new[] { new SimCommand { Player = 7, Type = CommandType.SetRally } }, Allocator.Temp);
            m.Step(cmds);
            Assert.AreEqual(1, CountEvents(m.World, SimEventType.CommandRejected),
                "[S11] a command from a player >= MaxPlayers must be rejected with a CommandRejected event, not silently dropped");
            bool found = false;
            for (int i = 0; i < m.World.Events.Events.Length; i++)
            {
                var e = m.World.Events.Events[i];
                if (e.Type == SimEventType.CommandRejected && e.B == 7) found = true;
            }
            Assert.IsTrue(found, "[S11] the rejection must carry the offending player");
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
