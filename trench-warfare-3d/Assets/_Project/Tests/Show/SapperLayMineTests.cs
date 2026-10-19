// Phase: sapper-lay-mine-anim (2026-10-19) — the sim has the Sapper's LayMine (SimEvents.SapperLaying) but the
// sapper played no laying animation. He now kneels (StandToKneel) then places (KneelInspect) for the sim's
// SapperSystem.LaySeconds, built from clips already baked for the rifleman (no new Clip value, no rebake).
// No sim is stepped: the rig places one sapper and writes the SapperLaying event the way SapperSystem does
// (InfantryFiringTests' Rig).
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;
using TW.Presentation;

namespace TW.Tests
{
    public class SapperLayMineTests
    {
        static readonly float3 Where = new float3(150f, 0f, 300f);

        static void Step(AnimationController a, SimWorld w, SimConfig cfg)
        {
            a.Tick(w);
            a.Advance(cfg.TickSeconds);
            w.Events.Clear();
            w.Tick++;
        }

        [Test]
        public void SapperKneelsToLayMine()
        {
            var cfg = SimConfig.Default;
            using var m = MatchSim.CreateGreybox(cfg);
            var row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
            var phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
            var yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
            using var a = new AnimationController(cfg.MaxSlots, m.Map, m.Gas, row, phase, yaw);
            try
            {
                var stray = m.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                m.World.Tick = 1000;   // the controller's rate limits are against tick 0

                int sapper = m.World.Spawn(0, (byte)InfantryArchetype.Sapper, Where, 100f, 3f, false);
                m.World.Events.Clear();
                Step(a, m.World, cfg);

                m.World.Events.Add(m.World.Tick, SimEventType.SapperLaying, sapper, 0, Where, default, SapperSystem.LaySeconds);
                Step(a, m.World, cfg);

                // within 2 ticks: down on one knee, as an action (not a routine idle fidget)
                Clip seen = Clip.Idle;
                for (int t = 0; t < 2 && seen != Clip.StandToKneel; t++)
                {
                    seen = a.State[sapper].Clip;
                    if (seen == Clip.StandToKneel) break;
                    Step(a, m.World, cfg);
                }
                Assert.AreEqual(Clip.StandToKneel, seen, "the sapper does not kneel to start laying the mine");
                Assert.AreEqual(Rung.Action, a.State[sapper].Rung, "kneeling to lay a mine is not on the action rung");

                int layTicks = (int)math.round(SapperSystem.LaySeconds / cfg.TickSeconds);
                for (int t = 0; t < layTicks; t++)
                {
                    Step(a, m.World, cfg);
                    Assert.AreEqual(Rung.Action, a.State[sapper].Rung, "he leaves the action rung mid-lay at tick " + t);
                    Assert.AreNotEqual(Clip.Idle, a.State[sapper].Clip, "he stands idle mid-lay at tick " + t);
                }

                // by now (the full LaySeconds elapsed) he placed the mine: kneeling, on KneelInspect
                Assert.AreEqual(Clip.KneelInspect, a.State[sapper].Clip, "he is not placing the mine on the knee");
                Assert.AreEqual((byte)Stance.Crouch, a.State[sapper].Stance, "he is not kneeling while placing the mine");

                // 1.5 s past the deadline nothing forces the crouch any more: he rises on his own
                int riseTicks = (int)math.round(1.5f / cfg.TickSeconds);
                for (int t = 0; t < riseTicks; t++) Step(a, m.World, cfg);
                Assert.AreNotEqual(Clip.KneelInspect, a.State[sapper].Clip, "he is still placing a mine long after the deadline");
                Assert.AreEqual((byte)Stance.Standing, a.State[sapper].Stance, "he does not stand back up after laying the mine");
            }
            finally
            {
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }
    }
}
