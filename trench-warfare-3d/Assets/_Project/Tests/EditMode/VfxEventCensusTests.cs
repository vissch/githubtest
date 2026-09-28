// Phase: VFX pass (owner, 2026-09-28: "make a task list, monitor what is the most common vfx ... we have to optimize this") -
// a census of the events the picture draws: the bench's stress battle (LockstepSession + ScriptedEnemy on ShelledForest
// 1917, as SimHost and StressPresetTests build it, heroes on) stepped headless, every event counted by type and by who made
// it - the shooter's class for a shot, a hit, a death; the ability and burst shape for an explosion; the mine's kind. The
// ranking is what the VFX work is ordered by (tw3d-board evidence/vfx-run). Explicit: it is a measurement, not a gate check
// (run it by name: -runTests -testFilter TW.Tests.VfxEventCensusTests). Reads the sim only.
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class VfxEventCensusTests
    {
        const int PerSide = 400, Ticks = 2400;   // 2 minutes at 20 ticks a second: the opening, the first assaults, the barrages

        /// <summary>Who an event's slot belongs to, by class name; "" when the event names no unit.</summary>
        static string Who(SimWorld w, SimEvent e)
        {
            switch (e.Type)
            {
                case SimEventType.Shot: case SimEventType.Hit: case SimEventType.Death: case SimEventType.VehicleFired:
                case SimEventType.UnitAlight: case SimEventType.UnitHealed: case SimEventType.CriticalHit: case SimEventType.ShieldBlocked:
                case SimEventType.VehicleOnFire: case SimEventType.VehicleCookOff: case SimEventType.LeapStarted: case SimEventType.BreakerPhase:
                case SimEventType.VehicleHullMended: case SimEventType.NearMiss: case SimEventType.Suppressed:
                    return e.A >= 0 && e.A < w.Archetype.Length ? UnitLook.Name(w.Archetype[e.A]) : "?";
                case SimEventType.VehicleArmourHit:
                    return e.B >= 0 && e.B < w.Archetype.Length ? "by " + UnitLook.Name(w.Archetype[e.B]) : "by blast";
                case SimEventType.Explosion: case SimEventType.AbilityFired:
                    string ability = e.A > 0 && Enum.IsDefined(typeof(OffMapAbilityId), (short)e.A) ? ((OffMapAbilityId)e.A).ToString() : "weapon " + e.A;
                    return e.Type == SimEventType.Explosion ? ability + " shape " + Math.Round(e.Dir.y) : ability;
                case SimEventType.MineTriggered: case SimEventType.MinePlaced: case SimEventType.MineCleared:
                    return ((TW.Sim.Combat.MineKind)(int)e.Scalar).ToString();
                default: return "";
            }
        }

        [Test, Explicit("a measurement: run by name")]
        public void Census_TheBenchBattle()
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = PerSide * 25;   // SimHost's stress silver
            using var session = new LockstepSession(() => MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u)), false, 0, 0, 0f, cfg.Seed);
            var ai = new ScriptedEnemy { StressUnits = PerSide };
            var byType = new Dictionary<SimEventType, int>();
            var byWho = new Dictionary<string, int>();
            int overrun = 0, guard = Ticks * 40, ticks = 0;
            while (session.Local.World.Tick < Ticks && guard-- > 0)
            {
                if (!session.StepOnce(ai)) continue;
                ticks++;
                var w = session.Local.World;
                overrun += w.Events.Overrun;
                var list = w.Events.Events;
                for (int i = 0; i < list.Length; i++)
                {
                    var e = list[i];
                    byType[e.Type] = byType.TryGetValue(e.Type, out int n) ? n + 1 : 1;
                    string who = Who(w, e);
                    if (who.Length == 0) continue;
                    string key = e.Type + " | " + who;
                    byWho[key] = byWho.TryGetValue(key, out int m) ? m + 1 : 1;
                }
            }
            Assert.That(guard > 0, "lockstep stalled");

            var sb = new StringBuilder();
            float minutes = ticks / 20f / 60f;
            sb.AppendLine($"VFX event census: ShelledForest 1917, {PerSide} a side, {ticks} ticks ({minutes:0.0} min), overrun {overrun}");
            sb.AppendLine("-- by type (count, per minute) --");
            foreach (var kv in byType.OrderByDescending(k => k.Value))
                sb.AppendLine($"{kv.Key,-22} {kv.Value,9} {kv.Value / minutes,10:0}");
            sb.AppendLine("-- by type and source --");
            foreach (var kv in byWho.OrderByDescending(k => k.Value))
                sb.AppendLine($"{kv.Key,-48} {kv.Value,9} {kv.Value / minutes,10:0}");
            string text = sb.ToString();
            TestContext.WriteLine(text);
            string dir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "TrenchWarfare", "runs");
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "vfx-census.txt"), text);
            Assert.That(byType.Count > 0, "no events at all");
        }
    }
}
