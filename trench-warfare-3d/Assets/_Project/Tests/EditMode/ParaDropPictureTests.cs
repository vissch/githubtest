// Phase: SHOW (2026-10-01) — the paratroop drop's picture: each canopy comes down on the spot the sim lands its man
// (CombatFx.DropSpot repeats the sim's die), and a canopy falls from the aircraft's height to the ground.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class ParaDropPictureTests
    {
        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        [Test]
        public void EachCanopyComesDownWhereTheSimLandsItsMan()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.FactionA = (byte)FactionId.Brass;
            using var m = MatchSim.CreateGreybox(cfg);
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            short rear = m.Fields.RearTrench(1);
            var def = m.Map.Trenches[rear];
            float z = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z - OffMapAbilitySystem.ParaDropKeepOut - 20f;
            SimEvent inbound = default; var landed = new List<SimEvent>();
            for (float x = 40f; x < m.Map.SizeMeters.x - 40f && inbound.Type != SimEventType.DropInbound; x += 4f)
            {
                Step(m, new SimCommand { Tick = m.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ParaDrop, Pos = new float3(x, 0f, z) });
                foreach (var e in m.World.Events.Events) if (e.Type == SimEventType.DropInbound) inbound = e;
            }
            Assert.AreEqual(SimEventType.DropInbound, inbound.Type, "setup: a drop was accepted");
            for (int t = 0; t < stats.WarmupTicks + 2; t++)
            {
                Step(m);
                foreach (var e in m.World.Events.Events) if (e.Type == SimEventType.DropLanded) landed.Add(e);
            }
            Assert.AreEqual(stats.Men, landed.Count, "setup: they all landed");
            Assert.AreEqual(inbound.Tick + (uint)stats.WarmupTicks, landed[0].Tick, "a flight after the call");
            for (int k = 0; k < landed.Count; k++)
            {
                float3 spot = CombatFx.DropSpot(m.World, m.Map, 0, k, inbound.Pos, inbound.Tick + (uint)stats.WarmupTicks, stats.Radius);
                Assert.Less(math.distance(spot.xz, landed[k].Pos.xz), 0.01f, "man " + k + " lands under his own canopy");
            }
        }

        [Test]
        public void ACanopyFallsFromTheAircraftToTheGround_AndNeverRises()
        {
            Assert.AreEqual(CombatFx.DropHeight, CombatFx.CanopyHeight(0f), 1e-4f);
            Assert.AreEqual(0f, CombatFx.CanopyHeight(1f), 1e-4f);
            Assert.LessOrEqual(CombatFx.DropHeight, CombatFx.PlaneLow, "he does not open his canopy above the aircraft he left");
            float last = CombatFx.DropHeight;
            for (int i = 1; i <= 20; i++)
            {
                float h = CombatFx.CanopyHeight(i / 20f);
                Assert.Less(h, last, "falling at " + i);
                last = h;
            }
            Assert.Greater(CombatFx.CanopyHeight(0f) - CombatFx.CanopyHeight(0.2f), CombatFx.CanopyHeight(0.8f) - CombatFx.CanopyHeight(1f), "fast before the silk fills, slow under it");
        }
    }
}
