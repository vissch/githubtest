// Phase: maintenance (2026-09-27) — one call that says what the sim and the picture think of a unit, what match is
// running, and which world array differs at a desync. For `Tools/tw eval` in Play (workflow.md section 9):
//     Tools/tw eval 'return TW.Editor.SimProbe.Unit(42);'
// Read-only: nothing here writes the sim or the scene.
using System.Text;
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Editor
{
    public static class SimProbe
    {
        static SimHost Host() => Object.FindFirstObjectByType<SimHost>();

        /// <summary>One unit, sim then picture: what it is, where the sim has it and its trench post, where it is drawn over which ground height, and what it is animating. Is it the sim or the drawing: compare the two.</summary>
        public static string Unit(int slot)
        {
            var h = Host();
            if (h == null || h.Local == null) return "no SimHost in Play";
            var w = h.Local.World;
            if (slot < 0 || slot >= w.HighWater) return $"slot {slot} out of range (HighWater {w.HighWater})";
            var sb = new StringBuilder();
            var p = w.Position[slot];
            sb.AppendLine($"tick {w.Tick} slot {slot} generation {w.Generation[slot]} alive {w.IsAlive(slot)} flags {(UnitFlags)w.Flags[slot]}");
            sb.AppendLine($"team {w.Team[slot]} archetype {w.Archetype[slot]} hp {w.Hp[slot]:0.#}/{w.MaxHp[slot]:0.#} target {w.TargetSlot[slot]}");
            sb.AppendLine($"sim at {p.x:0.00} {p.y:0.00} {p.z:0.00}  trench {w.TrenchId[slot]} post cell {w.PostCell[slot]} kind {w.PostKind[slot]} " +
                          $"stance {(Stance)w.StanceOf[slot]} layer {(TW.Sim.Terrain.NavLayer)w.Layer[slot]}");
            if (h.Presenter != null)
            {
                var d = h.Presenter.Drawn(slot);
                sb.AppendLine($"drawn at {d.x:0.00} {d.y:0.00} {d.z:0.00}  ground there {RenderGround.Sample(h.Local.Map, d.x, d.z):0.00}");
                for (int i = 0; i < h.Presenter.PoseCount; i++)
                    if (h.Presenter.PoseSlot[i] == slot)
                    {
                        var u = h.Presenter.Poses[i];
                        sb.AppendLine($"pose {i} at {u.Pos.x:0.00} {u.Pos.y:0.00} {u.Pos.z:0.00} yaw {(float)u.Yaw:0.00} row {u.AnimRow}");
                        break;
                    }
            }
            var a = h.Animation;
            if (a != null)
            {
                var s = a.State[slot];
                sb.AppendLine($"animation clip {s.Clip} rung {s.Rung} stance {(Stance)s.Stance} phase {a.Phase[slot]:0.00} lift {a.Lift[slot]:0.00} hop {a.Hop[slot]:0.00}" +
                              " (per-tick reasons: Animation.Follow(slot), then Animation.TraceText(80))");
            }
            else sb.AppendLine("animation: AnimationController is off");
            return sb.ToString();
        }

        /// <summary>The match being played: seed, battlefield seed and ground, bombardment, stress, tick and mission, the values a test needs to rebuild it.</summary>
        public static string Match()
        {
            var h = Host();
            if (h == null) return "no SimHost";
            var r = MatchLaunch.Running;
            return $"seed {h.Seed} generated {h.GeneratedBattlefield} battlefield seed {h.BattlefieldSeed} ground {h.Ground} " +
                   $"bombardment {h.BombardmentNow:0.#}/min (override {SimHost.BombardmentOverride}) stress {h.StressUnits} " +
                   $"(override {SimHost.StressOverride}) tick {(h.Local != null ? h.Local.World.Tick : 0)} mission '{(r != null ? r.MissionId : "")}'";
        }

        /// <summary>A hash per world array, so the canary's two worlds can be compared array by array at the desync tick: pause, then diff Arrays() with Arrays(true).</summary>
        public static string Arrays(bool peer = false)
        {
            var h = Host();
            var m = h == null ? null : peer ? h.Peer : h.Local;
            if (m == null) return peer ? "no peer world (canary off?)" : "no world";
            var w = m.World;
            int n = w.HighWater;
            return $"tick {w.Tick} position {SimHash.Array(w.Position, n, 0):X16} hp {SimHash.Array(w.Hp, n, 0):X16} " +
                   $"stance {SimHash.Array(w.StanceOf, n, 0):X16} layer {SimHash.Array(w.Layer, n, 0):X16} " +
                   $"trench {SimHash.Array(w.TrenchId, n, 0):X16} post {SimHash.Array(w.PostCell, n, 0):X16} " +
                   $"target {SimHash.Array(w.TargetSlot, n, 0):X16} flags {SimHash.Array(w.Flags, n, 0):X16}";
        }
    }
}
