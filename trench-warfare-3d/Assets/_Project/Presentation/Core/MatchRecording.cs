// Phase: tooling (record-match, 2026-10-14) — the debug option that records a played match, and the file it writes.
// The owner, 2026-10-06: "Record only when switched on: a debug option that pays the per-step check for that match
// alone". The option is the run-time knob `replay.record` (off by default; set by TW_KNOBS, `-twknob
// replay.record=1`, a bench's `knobs=`, or the editor's knob window). With it on SimHost builds a ReplayRecorder
// here and hands it to LockstepSession, which then leaves the world's HashInterval at 1 — the per-tick hash a
// replay needs, paid by that match and no other. With it off nothing is built and the session is what it was.
// The format is the existing one (ReplayRecorder v37): nothing here writes a new kind of file.
using System;
using System.IO;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    public static class MatchRecording
    {
        /// <summary>Run-time knob: 1 = record this match. Off by default.</summary>
        public const string Knob = "replay.record";

        public static bool Wanted => Knobs.Get(Knob, false);

        /// <summary>A recorder for a match that has just been built: the config and world init it started from, and
        /// the map it is played on. `mapParams` (BattlefieldParams.Serialize) lets a generated field be rebuilt.</summary>
        public static ReplayRecorder For(MatchSim m, in SimConfig cfg, byte[] mapParams = null)
        {
            var r = new ReplayRecorder(cfg, m.World.Init, m.Map.MapId, m.Map.Hash(0UL));
            if (mapParams != null) r.MapParams = mapParams;
            return r;
        }

        /// <summary>Write the recording and return its full path; null when it could not be written (a recording
        /// must never kill a match, so an IO failure is an error in the log and nothing else).</summary>
        public static string Write(ReplayRecorder r, string folder = null)
        {
            if (r == null) return null;
            try
            {
                string dir = Path.Combine(folder ?? Application.persistentDataPath, "Replays");
                Directory.CreateDirectory(dir);
                string path = Path.Combine(dir, $"match-{r.MapId}-{r.Config.Seed}-{DateTime.Now:yyyyMMdd-HHmmss}.twr");
                File.WriteAllBytes(path, r.Serialize());
                return path;
            }
            catch (Exception e)
            {
                Debug.LogError($"[record-match] could not write the replay: {e.Message}");
                return null;
            }
        }
    }
}
