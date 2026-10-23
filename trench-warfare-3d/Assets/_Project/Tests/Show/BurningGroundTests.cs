// Phase: fire pass (owner, 2026-10-23: "the ground the flamethrower sets alight does not burn on screen") — the
// drawn burning ground against the sim that burns it. BurningGround is the holder CombatFx.BurningGround.cs draws
// from; everything that can be got wrong here is in it, so it is driven straight: the sim's own CellBurning events
// go in, the sim's live cell list is Sync'd every tick, and the test asks whether a flame stood on every cell the
// sim was burning. On the old code neither the holder nor the behaviour existed and nothing drew CellBurning at all.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BurningGroundTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly BurningSystem Fire;
            public SimWorld W => M.World;
            public Rig()
            {
                M = MatchSim.CreatePlaytest(SimConfig.Default);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                Fire = M.World.GetSystem<BurningSystem>();
            }
            public float SimNow => W.Tick * W.Config.TickSeconds;
            public void Step()
            {
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                M.Step(none);
            }
            /// <summary>The cell centre, as BurningSystem puts it into the event.</summary>
            public Vector3 Centre(int cell) => new Vector3((cell % M.Map.NavWidth + 0.5f) * MapData.NavCellSize, 0f,
                                                           (cell / M.Map.NavWidth + 0.5f) * MapData.NavCellSize);
            /// <summary>The sim's live cells, as the drawing reads them out every frame.</summary>
            public List<BurningGround.SimCell> Live()
            {
                var live = new List<BurningGround.SimCell>();
                for (int k = 0; k < Fire.Cells.Length; k++)
                {
                    var c = Fire.Cells[k];
                    float left = c.Until > W.Tick ? (c.Until - W.Tick) * W.Config.TickSeconds : 0f;
                    live.Add(new BurningGround.SimCell { Cell = c.Cell, EndSim = SimNow + left });
                }
                return live;
            }
            /// <summary>Every CellBurning of this tick, fed into the holder exactly as CombatFx does.</summary>
            public void Pump(BurningGround g)
            {
                var events = W.Events.Events;
                for (int i = 0; i < events.Length; i++)
                {
                    var e = events[i];
                    if (e.Type != SimEventType.CellBurning) continue;
                    g.Ignite(e.A, (Vector3)e.Pos, SimNow, e.Scalar, 48);
                }
            }
            public void Dispose() => M.Dispose();
        }

        [Test]
        public void AFlameStandsOnEveryCellTheSimIsBurning()
        {
            using var rig = new Rig();
            var ground = new BurningGround();
            var at = new float3(60f, 0f, 60f);
            rig.Step();                                   // SimWorld.Step clears the event buffer first, so the ignition comes after it
            rig.Fire.IgniteCell(rig.W, at, BurningSystem.BurstCellSeconds, 0);
            int cell = rig.Fire.CellOf(at);

            int burning = 0, covered = 0;
            for (int t = 0; t < 400; t++)
            {
                rig.Pump(ground);
                ground.Sync(rig.SimNow, rig.Live());
                if (rig.Fire.Cells.Length == 0) break;
                burning++;
                if (ground.Covers(rig.Centre(cell), 0.5f)) covered++;
                rig.Step();
            }

            Assert.That(burning, Is.GreaterThan(4), "the sim did not keep the cell burning long enough to measure");
            Assert.That(covered, Is.EqualTo(burning),
                $"the drawing covered only {covered} of {burning} burning ticks; the owner's floor is 80 %");
        }

        [Test]
        public void ACellTheSimKeepsRelightingKeepsItsFlameTheWholeTime()
        {
            // The load-bearing case. IgniteCell emits CellBurning only for a cell that was NOT already burning, so a
            // cell a flamethrower sweeps for twelve seconds produces ONE event with five seconds on it. Driven from
            // the event alone the flame dies at five and the sim goes on burning men on clean mud.
            using var rig = new Rig();
            var ground = new BurningGround();
            var at = new float3(60f, 0f, 60f);
            int cell = rig.Fire.CellOf(at);
            int events = 0, uncovered = 0;
            float until = 12f;

            for (int t = 0; rig.SimNow < until; t++)
            {
                rig.Step();
                rig.Fire.IgniteCell(rig.W, at, BurningSystem.BurstCellSeconds, 0);   // the jet, still on this cell
                var frame = rig.W.Events.Events;
                for (int i = 0; i < frame.Length; i++) if (frame[i].Type == SimEventType.CellBurning) events++;
                rig.Pump(ground);
                ground.Sync(rig.SimNow, rig.Live());
                if (!ground.Covers(rig.Centre(cell), 0.5f)) uncovered++;
            }

            Assert.That(events, Is.EqualTo(1), "the sim announced the cell more than once: this test no longer tests the sync");
            Assert.That(uncovered, Is.EqualTo(0), "the flame went out while the sim was still burning the cell");
        }

        [Test]
        public void TheFlameGoesOutAndScorchesTheCellTheFrameTheSimDropsIt()
        {
            using var rig = new Rig();
            var ground = new BurningGround();
            var at = new float3(60f, 0f, 60f);
            rig.Step();
            rig.Fire.IgniteCell(rig.W, at, BurningSystem.BurstCellSeconds, 0);
            int cell = rig.Fire.CellOf(at);

            for (int t = 0; t < 400; t++)
            {
                rig.Pump(ground);
                bool simHad = rig.Fire.Cells.Length > 0;
                ground.Sync(rig.SimNow, rig.Live());
                if (simHad)
                {
                    Assert.That(ground.WentOut.Count, Is.EqualTo(0), "a cell was retired while the sim still burned it");
                    rig.Step();
                    continue;
                }
                Assert.That(ground.WentOut.Count, Is.EqualTo(1), "the fire was not retired the frame the sim put the cell out");
                Assert.That(ground.WentOut[0].Cell, Is.EqualTo(cell));
                Assert.That(ground.Retired, Is.EqualTo(1), "exactly one scorch is owed");
                Assert.That(ground.Count, Is.EqualTo(0));
                Assert.That(ground.Covers(rig.Centre(cell), 0.5f), Is.False, "something still draws on a cell the sim put out");
                return;
            }
            Assert.Fail("the sim never put the cell out");
        }

        [Test]
        public void AtTheCapTheFurthestFireGoesAndANearerOneIsKept()
        {
            var ground = new BurningGround { Distance = p => Mathf.Abs(p.x) };   // the look point is x = 0
            ground.Ignite(1, new Vector3(5f, 0f, 0f), 0f, 5f, 2);
            ground.Ignite(2, new Vector3(40f, 0f, 0f), 0f, 5f, 2);

            ground.Ignite(3, new Vector3(9f, 0f, 0f), 0f, 5f, 2);
            Assert.That(ground.Count, Is.EqualTo(2));
            Assert.That(ground.Covers(new Vector3(40f, 0f, 0f), 0.5f), Is.False, "the furthest fire should have been evicted");
            Assert.That(ground.Covers(new Vector3(5f, 0f, 0f), 0.5f), "the nearest fire was evicted");
            Assert.That(ground.Covers(new Vector3(9f, 0f, 0f), 0.5f), "the new nearer fire was not taken");

            ground.Ignite(4, new Vector3(300f, 0f, 0f), 0f, 5f, 2);
            Assert.That(ground.Covers(new Vector3(300f, 0f, 0f), 0.5f), Is.False,
                "a fire further off than everything already alight should not push a nearer one out");
            Assert.That(ground.Count, Is.EqualTo(2));
        }
    }
}
