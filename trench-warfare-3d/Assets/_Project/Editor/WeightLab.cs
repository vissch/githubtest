// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: TankRenderer.Probe, Knobs, RiderLab,
// TankCapture. Traces of how heavy a machine looks, as numbers, before anyone judges a picture. From `unity command eval`:
//   WeightLab.Knob("tank.weight", "1")   sets a knob in the game (an eval's own statics never reach compiled code, so the
//                                        lab's scene object sets it on its next frame; RiderLab.cs says why)
//   WeightLab.Trace(slot, csv, 6)        every drawn frame of that machine for 6 s of game time into a CSV: the sim's speed
//                                        along its nose, the drawn speed and felt acceleration, the ride springs and the
//                                        hull as drawn (pitch, roll, heave), the kick layer, the footfall, recoil, the
//                                        drawn gun yaw and its settle, feet landed
//   WeightLab.TraceStatus()              progress, then the stop's numbers: the dip, the rebound and when it settled
//   WeightLab.Halt(slot)                 its drive taken away: it brakes to a stop at its own rate (RiderLab.Stop stops it dead)
// A machine to trace comes from TankCapture.Spawn / RiderLab.Drive / RiderLab.Enemies. Presentation only apart from
// Halt, which writes the sim through SimHost.WriteWorlds as the other labs do; nothing here is hashed or replayed.
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Editor
{
    public static class WeightLab
    {
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>A knob set in the game on the lab's next frame (Knobs.Set from compiled code).</summary>
        public static string Knob(string name, string value) { Get().Knobs.Add((name, value)); return $"{name}={value} (next frame)"; }

        /// <summary>The machine's drive taken away (its Speed to 0): with momentum it sheds its way at its Brake and runs on
        /// to a stop, as at a halt. RiderLab.Stop is not this: its heading pin zeroes the velocity, a dead stop.</summary>
        public static string Halt(int slot)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart: try again";
            h.WriteWorlds(m => m.World.Speed[slot] = 0f);
            return "slot " + slot + " halted";
        }

        /// <summary>Traces the machine in `slot` for `seconds` of game time into `csv`.</summary>
        public static string Trace(int slot, string csv, float seconds = 6f)
        {
            var b = Get();
            if (b.Tracing) return "already tracing slot " + b.Slot;
            b.Slot = slot; b.Csv = csv; b.Seconds = seconds; b.Rows.Clear(); b.Began = -1f; b.Tracing = true; b.Result = null;
            return $"tracing slot {slot} for {seconds:0.#} s into {csv}";
        }

        /// <summary>The trace in progress, or, once done, where it went and the stop's numbers: dip, rebound, settled.</summary>
        public static string TraceStatus()
        {
            var b = Get();
            if (b.Tracing) return $"tracing slot {b.Slot}: {b.Rows.Count} frames";
            return b.Result ?? "no trace";
        }

        static Bench Get()
        {
            if (Bench.Instance != null) return Bench.Instance;
            var found = Object.FindFirstObjectByType<Bench>(FindObjectsInactive.Include);
            if (found == null) found = new GameObject("WeightLab") { hideFlags = HideFlags.DontSave }.AddComponent<Bench>();
            Bench.Instance = found;
            return found;
        }

        /// <summary>One drawn frame of the traced machine.</summary>
        public struct Row { public float T; public uint Tick; public float SimSpeed; public WeightProbe P; }

        /// <summary>What a stop did to the hull, from a trace: the moment the sim's speed fell under 0.1 m/s, the lowest
        /// nose after it (the dip, degrees against where it came to rest) and when, the highest after that (the rebound) and
        /// when, and the last moment it was more than 0.1 degrees from rest (settled). False when the trace has no stop.</summary>
        public static bool StopNumbers(IReadOnlyList<Row> rows, out float stopAt, out float dip, out float dipAt, out float rebound, out float reboundAt, out float settledAt)
        {
            stopAt = dip = dipAt = rebound = reboundAt = settledAt = 0f;
            int stop = -1;
            for (int i = 1; i < rows.Count; i++) if (Mathf.Abs(rows[i - 1].SimSpeed) >= 0.1f && Mathf.Abs(rows[i].SimSpeed) < 0.1f) { stop = i; break; }
            if (stop < 0 || rows.Count - stop < 4) return false;
            // rest: the mean of the last fifth of the trace
            int tail = Mathf.Max(1, (rows.Count - stop) / 5);
            float rest = 0f;
            for (int i = rows.Count - tail; i < rows.Count; i++) rest += rows[i].P.HullPitch;
            rest /= tail;
            stopAt = rows[stop].T;
            int low = stop;
            for (int i = stop; i < rows.Count; i++) if (rows[i].P.HullPitch < rows[low].P.HullPitch) low = i;
            int high = low;
            for (int i = low; i < rows.Count; i++) if (rows[i].P.HullPitch > rows[high].P.HullPitch) high = i;
            dip = rows[low].P.HullPitch - rest; dipAt = rows[low].T - stopAt;
            rebound = rows[high].P.HullPitch - rest; reboundAt = rows[high].T - stopAt;
            settledAt = 0f;
            for (int i = stop; i < rows.Count; i++) if (Mathf.Abs(rows[i].P.HullPitch - rest) > 0.1f) settledAt = rows[i].T - stopAt;
            return true;
        }

        /// <summary>Last in the frame, after every renderer: applies the knobs asked for, and samples the traced machine.</summary>
        [DefaultExecutionOrder(30002)]
        public sealed class Bench : MonoBehaviour
        {
            public static Bench Instance;
            public readonly List<(string name, string value)> Knobs = new List<(string, string)>();
            public readonly List<Row> Rows = new List<Row>(1024);
            public bool Tracing; public int Slot; public string Csv, Result; public float Seconds, Began = -1f;
            TankRenderer tanks;

            void Awake()
            {
                if (Instance != null && Instance != this) { Destroy(gameObject); return; }
                Instance = this;
            }

            void LateUpdate()
            {
                if (Knobs.Count > 0)
                {
                    foreach (var (name, value) in Knobs) TW.Presentation.Knobs.Set(name, value);
                    Knobs.Clear();
                }
                if (!Tracing) return;
                var h = Object.FindFirstObjectByType<SimHost>();
                if (tanks == null) tanks = Object.FindFirstObjectByType<TankRenderer>();
                if (h == null || h.Local == null || tanks == null) return;
                var w = h.Local.World;
                if (Began < 0f) Began = Time.time;
                float t = Time.time - Began;
                if (tanks.Probe(Slot, out var p) && Slot < w.HighWater)
                {
                    float3 vel = w.Velocity[Slot]; float yaw = w.Yaw[Slot];
                    Rows.Add(new Row { T = t, Tick = w.Tick, SimSpeed = vel.x * math.sin(yaw) + vel.z * math.cos(yaw), P = p });
                }
                if (t >= Seconds) Finish();
            }

            void Finish()
            {
                Tracing = false;
                var s = new StringBuilder(64 * (Rows.Count + 1));
                s.AppendLine("t,tick,sim_speed,speed,accel,yaw_rate,throttle,pitch,roll,heave,hull_pitch,hull_roll,hull_heave,kick_pitch,kick_roll,foot,recoil0,recoil1,gun_yaw0,settle0,settle1,feet_landed,legged,stalled,weighted");
                var c = CultureInfo.InvariantCulture;
                foreach (var r in Rows)
                {
                    var p = r.P;
                    s.Append(r.T.ToString("0.0000", c)).Append(',').Append(r.Tick).Append(',');
                    foreach (float f in new[] { r.SimSpeed, p.Speed, p.Accel, p.YawRate, p.Throttle, p.Pitch, p.Roll, p.Heave, p.HullPitch, p.HullRoll, p.HullHeave, p.KickPitch, p.KickRoll, p.Foot, p.Recoil0, p.Recoil1, p.GunYaw0, p.Settle0, p.Settle1 })
                        s.Append(f.ToString("0.#####", c)).Append(',');
                    s.Append(p.FeetLanded).Append(',').Append(p.Legged ? 1 : 0).Append(',').Append(p.Stalled ? 1 : 0).Append(',').Append(p.Weighted ? 1 : 0).AppendLine();
                }
                string dir = Path.GetDirectoryName(Csv);
                if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);
                File.WriteAllText(Csv, s.ToString());
                Result = StopNumbers(Rows, out float stopAt, out float dip, out float dipAt, out float rebound, out float reboundAt, out float settled)
                    ? $"done: {Rows.Count} frames into {Csv}; stop at {stopAt:0.00} s: dip {dip:0.00} deg at +{dipAt:0.00} s, rebound {rebound:0.00} deg at +{reboundAt:0.00} s, settled within 0.1 deg by +{settled:0.00} s"
                    : $"done: {Rows.Count} frames into {Csv} (no stop in the trace)";
            }
        }
    }
}
