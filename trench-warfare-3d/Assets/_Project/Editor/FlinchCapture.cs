// Phase: shelling-flinch-g1 (tooling) — depends on: SimHost, TankCapture, VfxLab, DeathLab, CaptureRig
// One staging: a rifleman walked into frame, shot locked off at zoom 16 by CaptureRig.Shot's `fit` (height, not
// zoom, fills the frame), shelled near enough to duck and far enough to live, three stills on exact sim ticks —
// before the burst, 0.3 s after (the flinch's hold) and 0.6 s after (past the hold, where the walk cut used to
// win and drop the clip). Driven from `unity command eval`, in Play: FlinchCapture.Set("<dir>"), then
// FlinchCapture.Status() until it reports "done".
using System.Collections;
using System.Globalization;
using System.IO;
using System.Text;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Editor
{
    public static class FlinchCapture
    {
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        const float Zoom = 16f, Yaw = 21f, Pitch = 25f, Fit = 2.6f;
        const int W = 1600, H = 900;

        /// <summary>Stages the man, walks him in, shells him near and writes before.png/duck.png/cut.png plus set.json
        /// into `dir`. Returns at once; poll Status() for "done" (or a failure message).</summary>
        public static string Set(string dir)
        {
            var h = Host;
            if (h == null || h.Local == null) return "no SimHost / not in Play";
            if (Object.FindFirstObjectByType<Driver>() != null) return "a set is already running: check Status()";
            var go = new GameObject("TW Flinch Capture") { hideFlags = HideFlags.DontSave };
            go.AddComponent<Driver>().Begin(dir);
            return "staging into " + dir;
        }

        /// <summary>"done", a failure, or what stage the run is on.</summary>
        public static string Status()
        {
            var d = Object.FindFirstObjectByType<Driver>();
            return d != null ? d.Stage : "idle";
        }

        sealed class Driver : MonoBehaviour
        {
            public string Stage = "starting";
            string dir;

            public void Begin(string dir)
            {
                this.dir = dir;
                StartCoroutine(Run());
            }

            IEnumerator Run()
            {
                var h = Host;
                var m = h.Local;

                Stage = "staging the man";
                const float sx = 70f, sz = 40f;
                string spawned = TankCapture.Spawn(0, InfantryArchetype.Rifle, sx, sz, 0f);
                if (!spawned.StartsWith("slot "))
                {
                    Stage = "failed to spawn: " + spawned;
                    Destroy(gameObject);
                    yield break;
                }
                int slot = int.Parse(spawned.Substring(5));
                h.Animation.Follow(slot);
                var cell = m.Map.NavCellOf(new float3(sx, 0f, sz + 25f));
                int navCell = m.Map.NavIndex(cell.x, cell.y);
                h.WriteWorlds(mm =>
                {
                    mm.World.GoalId[slot] = mm.Fields.GetGoal(GoalKey.Cell(navCell, NavMode.Infantry));
                    mm.World.Speed[slot] = 1.0f;   // inside the 0.3-1.5 m/s walk band AnimationController.cs:592 serves
                    // the live battle's crossfire and ambient bombardment kill a lone staged man in a few seconds,
                    // long before the staged shell: found live (2026-11-08) when every capture came back Death*
                    // instead of Duck. A huge hp pool survives it; the ambient shelling is silenced outright.
                    mm.World.Hp[slot] = 10000f; mm.World.MaxHp[slot] = 10000f;
                    var stray = mm.World.GetSystem<AmbientBombardmentSystem>();
                    if (stray != null) stray.ShellsPerMinute = 0f;
                });

                VfxLab.Rain(0f);
                VfxLab.Flare();

                Stage = "walking, letting the drawn speed settle";
                float until = Time.time + 2f;
                while (Time.time < until) yield return null;

                var drawn = h.Presenter.Drawn(slot);
                float fx = drawn.x, fz = drawn.z, aimY = drawn.y + 1.2f;   // chest height

                uint beforeTick = m.World.Tick + 4;
                uint burstTick = beforeTick + 1;
                // measured live (2026-11-08, tick trace via Animation.Follow/TraceText): the burst resolves one tick
                // after it is queued and Duck starts there, held only while tick - LastBlast < 6
                // (AnimationController.cs:595) — protected for 6 ticks (0.3 s) from the resolve tick, cut on the 7th.
                // duck.png sits mid-hold; cut.png sits on the last protected tick, past where the old code (no
                // guaranteed hold) dropped it on the very first walk-cut boundary.
                uint duckTick = burstTick + 3;
                uint cutTick = burstTick + 6;

                var shots = new Shot[]
                {
                    new Shot { Path = Path.Combine(dir, "before.png"), Tick = beforeTick },
                    new Shot { Path = Path.Combine(dir, "duck.png"), Tick = duckTick },
                    new Shot { Path = Path.Combine(dir, "cut.png"), Tick = cutTick },
                };

                Stage = "waiting for the before tick";
                while (m.World.Tick < beforeTick) yield return null;
                yield return Shoot(shots, 0, fx, fz, aimY, h, m, slot);

                while (m.World.Tick < burstTick) yield return null;
                float bx = fx + 5.5f, bz = fz;   // off him: outside the throw cutoff (0.85 r), inside WaveReach
                DeathLab.Shell(bx, bz, 5f, 15f);

                while (m.World.Tick < duckTick) yield return null;
                Stage = "shooting the duck";
                yield return Shoot(shots, 1, fx, fz, aimY, h, m, slot);

                while (m.World.Tick < cutTick) yield return null;
                Stage = "shooting the cut";
                yield return Shoot(shots, 2, fx, fz, aimY, h, m, slot);

                WriteSetJson(dir, shots);
                Stage = "done";
                Destroy(gameObject);
            }

            IEnumerator Shoot(Shot[] shots, int i, float fx, float fz, float aimY, SimHost h, MatchSim m, int slot)
            {
                // recorded on the tick that is actually captured, not read back once the whole set is done: read
                // at the end it only ever showed the LAST shot's clip for all three (found live, 2026-11-08).
                var shot = shots[i];
                shot.WorldSpeed = m.World.Speed[slot];
                shot.DrawnSpeed = h.Animation != null ? h.Animation.State[slot].Speed : 0f;
                shot.Clip = h.Animation != null ? h.Animation.State[slot].Clip.ToString() : "";
                shots[i] = shot;
                CaptureRig.Hold(120f);
                CaptureRig.Shot(shot.Path, fx, fz, Zoom, Yaw, Pitch, W, H, aimY, Fit);
                while (int.Parse(CaptureRig.Pending()) > 0) yield return null;
                CaptureRig.Release();
            }
        }

        struct Shot { public string Path; public uint Tick; public float WorldSpeed, DrawnSpeed; public string Clip; }

        static void WriteSetJson(string dir, Shot[] shots)
        {
            var sb = new StringBuilder("{\n");
            sb.Append("  \"sha\": \"").Append(ShortSha()).Append("\",\n");
            sb.Append("  \"shots\": [\n");
            for (int i = 0; i < shots.Length; i++)
            {
                var s = shots[i];
                sb.Append("    { \"path\": \"").Append(Path.GetFileName(s.Path)).Append("\", \"tick\": ").Append(s.Tick)
                  .Append(", \"world_speed\": ").Append(s.WorldSpeed.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"drawn_speed\": ").Append(s.DrawnSpeed.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"clip\": \"").Append(s.Clip).Append("\" }").Append(i < shots.Length - 1 ? ",\n" : "\n");
            }
            sb.Append("  ]\n}\n");
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "set.json"), sb.ToString());
        }

        /// <summary>Same as Gym.ShortSha: not referenced from here because Gym is its own file with its own concerns.</summary>
        static string ShortSha()
        {
            try
            {
                var p = new System.Diagnostics.Process { StartInfo = new System.Diagnostics.ProcessStartInfo("git", "rev-parse --short HEAD") { RedirectStandardOutput = true, UseShellExecute = false, CreateNoWindow = true, WorkingDirectory = Directory.GetCurrentDirectory() } };
                p.Start(); string s = p.StandardOutput.ReadToEnd().Trim(); p.WaitForExit(3000);
                return string.IsNullOrEmpty(s) ? "nosha" : s;
            }
            catch { return "nosha"; }
        }
    }
}
