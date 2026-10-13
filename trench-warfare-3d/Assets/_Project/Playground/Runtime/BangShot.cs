// Phase: Playground (2026-10-13, lane/show/idea-the-playground-camera-shoots-on-the-bang) — the schedule of a shot taken on the bang
// A playground still used to be one frame after a wait in real seconds, so every House5 picture was shot twenty seconds
// after the dust had settled (both critics saw none). This is the schedule that waits for the EVENT instead: it keeps a
// ring of pre-roll frames while nothing has happened, and once the bang comes it owes five frames around it, one every
// Step. Pure: no camera, no files, no Unity rendering, so the worked example is a plain EditMode test. PlaygroundHost
// does the grabbing; this says when, and writes the manifest the GIF recipe reads.
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace TW.Playground
{
    public sealed class BangShot
    {
        /// <summary>Where the frames go: "<dir>/demo_hit" or "<dir>/demo_hit.png"; frame k is "<stem>_f<k+1>.png".</summary>
        public string Path = "";
        /// <summary>The kind of bang waited for ("burst", "cook", "muzzle", "flame", "dust"), or "any".</summary>
        public string Kind = "any";
        public int W = 800, H = 450;
        /// <summary>Seconds between frames: five of them at 0.2 s is a second of action, played back as the two-second loop.</summary>
        public float Step = 0.2f;
        /// <summary>How many frames before the bang are kept (the ring's size).</summary>
        public int Pre = 1;
        /// <summary>Frames in the finished shot, the bang's own among them.</summary>
        public int Count = 5;
        /// <summary>Give up after this many seconds with no bang.</summary>
        public float Wait = 20f;

        /// <summary>When the shot was armed (the first PreRollDue sets it, if nothing else did).</summary>
        public float Armed = -1f;
        public bool Fired { get; private set; }
        public float BangTime { get; private set; } = -1f;
        /// <summary>Frames recorded so far, kept pre-roll frames included.</summary>
        public int Taken => frames.Count;
        public bool Done => frames.Count >= Count;

        readonly List<float> ring = new List<float>();     // pre-roll times still waiting for a bang
        readonly List<float> frames = new List<float>();   // the times of the frames of the shot, in order
        float lastPre = float.NegativeInfinity;
        int post;                                          // post-bang frames owed and taken

        /// <summary>True when a pre-roll frame is due: at once, then every Step, while no bang has come. Saying yes
        /// RECORDS it (the ring keeps the last Pre times), so ask once a frame and grab when it says so.</summary>
        public bool PreRollDue(float now)
        {
            if (Fired || Pre <= 0) return false;
            if (Armed < 0f) Armed = now;
            if (now < lastPre + Step) return false;
            lastPre = now;
            ring.Add(now);
            while (ring.Count > Pre) ring.RemoveAt(0);
            return true;
        }

        /// <summary>The bang happened: the kept pre-roll frames become the start of the shot and this frame is its
        /// middle. The host flushes its ring of PNGs in the same order.</summary>
        public void Fire(float now)
        {
            if (Fired) return;
            Fired = true; BangTime = now;
            if (Armed < 0f) Armed = now;
            foreach (float t in ring) if (frames.Count < Count - 1) frames.Add(t);
            ring.Clear();
            if (frames.Count < Count) frames.Add(now);
        }

        /// <summary>True when the next frame after the bang is owed (bang + k * Step). Saying yes records it.</summary>
        public bool Due(float now)
        {
            if (!Fired || Done) return false;
            if (now < BangTime + (post + 1) * Step) return false;
            post++;
            frames.Add(now);
            return true;
        }

        /// <summary>No bang by Wait: the shot is dead, and its manifest says so rather than hanging a script.</summary>
        public bool Expired(float now) => !Fired && Armed >= 0f && now - Armed >= Wait;

        /// <summary>The stem the frames are named from, without its ".png".</summary>
        public string Stem => Path.EndsWith(".png") ? Path.Substring(0, Path.Length - 4) : Path;

        /// <summary>Where frame k goes (k from 0).</summary>
        public string FramePath(int k) => Stem + "_f" + (k + 1) + ".png";
        static readonly char[] Seps = { '/', '\\' };

        /// <summary>The name of frame k, without its folder: what the manifest lists and loop.py joins.</summary>
        public string FrameName(int k)
        {
            string p = FramePath(k);
            int i = p.LastIndexOfAny(Seps);
            return i < 0 ? p : p.Substring(i + 1);
        }

        /// <summary>The shot in JSON: the kind, the bang's time, each frame's name and its offset from the bang (the kept
        /// pre-roll frame is negative), and whether a bang came at all.</summary>
        public string Manifest()
        {
            var inv = CultureInfo.InvariantCulture;
            var sb = new StringBuilder("{\n");
            sb.Append("  \"kind\": \"").Append(Kind).Append("\",\n");
            sb.Append("  \"bang\": ").Append(Fired ? "true" : "false").Append(",\n");
            sb.Append("  \"bangTime\": ").Append(BangTime.ToString("F3", inv)).Append(",\n");
            sb.Append("  \"step\": ").Append(Step.ToString("F3", inv)).Append(",\n");
            sb.Append("  \"w\": ").Append(W).Append(", \"h\": ").Append(H).Append(",\n");
            sb.Append("  \"frames\": [");
            for (int k = 0; k < frames.Count; k++) { if (k > 0) sb.Append(", "); sb.Append('"').Append(FrameName(k)).Append('"'); }
            sb.Append("],\n");
            sb.Append("  \"offsets\": [");
            for (int k = 0; k < frames.Count; k++) { if (k > 0) sb.Append(", "); sb.Append((frames[k] - BangTime).ToString("F3", inv)); }
            sb.Append("]\n}\n");
            return sb.ToString();
        }
    }
}
