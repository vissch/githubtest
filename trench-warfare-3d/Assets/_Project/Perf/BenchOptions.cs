// Phase: tooling (perf pass, 2026-09-23) — what one benchmark run measures, parsed from "key=value key=value".
// The same string drives the Windows player (`TrenchWarfare.exe -twbench "stress=1500 ticks=400 out=run.json"`) and
// the editor (CaptureRig.Bench("stress=1500 ...")), so a number from each is a measurement of the same battle.
using System;
using System.Globalization;

namespace TW.Perf
{
    public sealed class BenchOptions
    {
        /// <summary>Riflemen deployed by EACH side through SimHost's stress preset.</summary>
        public int Stress = 1500;
        /// <summary>The window opens at this sim tick (the armies deployed and in contact), reached fast-forwarded.</summary>
        public int SettleTicks = 1800;
        /// <summary>Sim ticks recorded, at 1x. 400 ticks = 20 s of battle.</summary>
        public int Ticks = 400;
        /// <summary>Frames at 1x before the window opens: late shader variants, Burst, pools growing to size.</summary>
        public int Warm = 120;
        /// <summary>SimHost.TimeScale while settling.</summary>
        public float FastForward = 8f;
        /// <summary>QualitySettings level to run at; -1 keeps whatever the player's settings chose.</summary>
        public int Quality = -1;
        public bool VSync;
        public int Width = 1920, Height = 1080;
        /// <summary>Atmosphere.PinnedClock during the run, so two runs see the same sky; below 0 lets it run.</summary>
        public float Weather = 120f;
        /// <summary>The camera, held for the whole window: the standard view by default (docs: owner, 2026-09-21).</summary>
        public float Zoom = 30f, Yaw = 21f, Pitch = 25f;
        /// <summary>Time every EventPump subscriber under its own marker (costs ~2 marker calls per event per subscriber).</summary>
        public bool Subscribers;
        /// <summary>1 runs the determinism canary (two worlds, every tick hashed and compared), 0 forces one world, -1 leaves
        /// the scene's setting: the before and after of the one-world change, on the same battle.</summary>
        public int Canary = -1;
        public string Label = "run";
        public string Out = "perf.json";
        /// <summary>A PNG of the held view, taken during the warm-up (so it is not a frame of the window): proof of what
        /// the numbers were measured on, e.g. that a player build draws what the editor does.</summary>
        public string Shot = "";
        /// <summary>Quit the player (or leave play mode in the editor) when the file is written.</summary>
        public bool Quit = true;
        public string Raw = "";

        public static BenchOptions Parse(string raw)
        {
            var o = new BenchOptions { Raw = raw ?? "" };
            if (string.IsNullOrWhiteSpace(raw)) return o;
            foreach (var token in raw.Split(new[] { ' ', ',', ';' }, StringSplitOptions.RemoveEmptyEntries))
            {
                int eq = token.IndexOf('=');
                if (eq <= 0) continue;
                string k = token.Substring(0, eq).Trim().ToLowerInvariant(), v = token.Substring(eq + 1).Trim();
                switch (k)
                {
                    case "stress": o.Stress = I(v, o.Stress); break;
                    case "settle_ticks": case "settle": o.SettleTicks = I(v, o.SettleTicks); break;
                    case "ticks": o.Ticks = I(v, o.Ticks); break;
                    case "warm": o.Warm = I(v, o.Warm); break;
                    case "ff": o.FastForward = F(v, o.FastForward); break;
                    case "quality": o.Quality = I(v, o.Quality); break;
                    case "vsync": o.VSync = v == "1" || v.ToLowerInvariant() == "true"; break;
                    case "w": case "width": o.Width = I(v, o.Width); break;
                    case "h": case "height": o.Height = I(v, o.Height); break;
                    case "weather": o.Weather = F(v, o.Weather); break;
                    case "zoom": o.Zoom = F(v, o.Zoom); break;
                    case "yaw": o.Yaw = F(v, o.Yaw); break;
                    case "pitch": o.Pitch = F(v, o.Pitch); break;
                    case "subs": o.Subscribers = v == "1" || v.ToLowerInvariant() == "true"; break;
                    case "canary": o.Canary = I(v, o.Canary); break;
                    case "label": o.Label = v; break;
                    case "out": o.Out = v; break;
                    case "shot": o.Shot = v; break;
                    case "quit": o.Quit = !(v == "0" || v.ToLowerInvariant() == "false"); break;
                }
            }
            return o;
        }

        static int I(string v, int d) => int.TryParse(v, NumberStyles.Integer, CultureInfo.InvariantCulture, out int x) ? x : d;
        static float F(string v, float d) => float.TryParse(v, NumberStyles.Float, CultureInfo.InvariantCulture, out float x) ? x : d;
    }
}
