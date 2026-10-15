// Phase: tooling (perf pass, 2026-09-23) - what one benchmark run measures, parsed from "key=value key=value".
// The same string drives the Windows player (`TrenchWarfare.exe -twbench "stress=1500 ticks=400 out=run.json"`) and
// the editor (CaptureRig.Bench("stress=1500 ...")), so a number from each is a measurement of the same battle.
using System;
using System.Globalization;
using System.Text;
using TW.Presentation;

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
        /// <summary>Where the held camera looks (map metres, x and z); NaN, the default, is the armies' centre. For a still of
        /// one place (a trench bay, a hamlet) beside the numbers: `fx=12 fz=40 zoom=10 shot=bay.png`.</summary>
        public float FocusX = float.NaN, FocusZ = float.NaN;
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
        /// <summary>Take the `shot=` still this many sim ticks into the window instead of in the paused warm-up, so it
        /// shows the battle (and the scenario) running. -1 (default) keeps the warm-up still. A run with shot_tick >= 0
        /// keeps the held clock (PerfBench, C33) through its window so the still repeats: its timings are then NOT real
        /// time, which makes it an image run, never a perf sample. Keep it below `ticks`.</summary>
        public int ShotTick = -1;
        /// <summary>0 hides every UI Toolkit panel (the HUD, the shell) through an image run's warm-up and window, so its still
        /// is the battlefield alone. The HUD animates on real time and follows the owner's pointer over the background
        /// window (C33: the only pixels that still differed between three held-clock runs). 1 (default) keeps it.</summary>
        public bool ShotHud = true;
        /// <summary>How many consecutive held frames an image run takes from `shot_tick` on: the first is `shot`, the rest
        /// `<shot>.f1.png`, `.f2.png`... A motion (a volley's ripple, a flash's hold) needs frames, not one still. Default 1.</summary>
        public int ShotFrames = 1;
        /// <summary>Quit the player (or leave play mode in the editor) when the file is written.</summary>
        public bool Quit = true;
        /// <summary>What happens in the measured window (docs/reference/aosa/README.md "Scenarios"), issued as sim
        /// commands once hash_start is taken: None is the stress battle alone.</summary>
        public BenchScenario Scenario = BenchScenario.None;
        /// <summary>What `scenario=` said, verbatim ("" when absent). An unknown value runs None and is kept here, so the
        /// report can say what was asked for.</summary>
        public string ScenarioRaw = "";
        /// <summary>The raw `knobs=` value, '|' between pairs (`knobs=vat.lodDistance=90|fx.maxMarks=400`), because the
        /// bench string itself splits on space, comma and semicolon. PerfBench hands it to TW.Presentation.Knobs.Parse
        /// before the scene loads. "" when absent.</summary>
        public string Knobs = "";
        /// <summary>C51: which battlefield the bench launches (MatchLaunch.Request.Ground, so its map AND its look:
        /// BiomeProfile.ForGround). ShelledForest, the night wood, is the zero value and the bench as it always was.
        /// WinterLine is the only day field. Any other ground is another MAP (MatchLaunch.Field), so hash_start is
        /// not the night bench's.</summary>
        public Ground Ground = Ground.ShelledForest;
        /// <summary>What `ground=` said, verbatim ("" when absent).</summary>
        public string GroundRaw = "";
        /// <summary>`ground=` named no Ground. PerfBench then refuses to run (exit 2) rather than bench the wood under
        /// another name: unlike scenario=, an unknown ground never falls back.</summary>
        public bool GroundUnknown;
        public string Raw = "";
        /// <summary>Keys the parser did not know ("tick=800" for ticks): PerfBench warns, so a typo cannot run 400 silently.</summary>
        public System.Collections.Generic.List<string> Unknown = new System.Collections.Generic.List<string>();
        /// <summary>F5: values that are not a number ("ticks=4e2"), which fell back to the default. PerfBench warns, so a
        /// bad number cannot run the default silently.</summary>
        public System.Collections.Generic.List<string> Bad = new System.Collections.Generic.List<string>();
        /// <summary>[F16] values a range check pulled up to a minimum (e.g. "settle=-1 -> 0"): without this, a negative
        /// settle or ticks cast to uint and ran for billions of ticks instead of stopping at the stage deadline.</summary>
        public System.Collections.Generic.List<string> Clamped = new System.Collections.Generic.List<string>();

        /// <summary>F5: the bench string cut into key=value tokens. Space, comma and semicolon separate, but only OUTSIDE
        /// double quotes, and the quotes themselves are dropped: `out="G:/My Drive/run.json"` stays one token.</summary>
        public static System.Collections.Generic.List<string> Tokens(string raw)
        {
            var list = new System.Collections.Generic.List<string>();
            var sb = new StringBuilder();
            bool q = false;
            foreach (char c in raw ?? "")
            {
                if (c == '\u0022') { q = !q; continue; }   // a double quote, written escaped so the brace check's string regex is not fooled
                if (!q && (c == ' ' || c == ',' || c == ';'))
                {
                    if (sb.Length > 0) { list.Add(sb.ToString()); sb.Length = 0; }
                    continue;
                }
                sb.Append(c);
            }
            if (sb.Length > 0) list.Add(sb.ToString());
            return list;
        }

        public static BenchOptions Parse(string raw)
        {
            var o = new BenchOptions { Raw = raw ?? "" };
            if (string.IsNullOrWhiteSpace(raw)) return o;
            foreach (var token in Tokens(raw))
            {
                int eq = token.IndexOf('=');
                if (eq <= 0) { o.Unknown.Add(token); continue; }
                string k = token.Substring(0, eq).Trim().ToLowerInvariant(), v = token.Substring(eq + 1).Trim();
                switch (k)
                {
                    case "stress": o.Stress = Clamp(I(v, o.Stress, k, o), 0, k, o); break;
                    case "settle_ticks": case "settle": o.SettleTicks = Clamp(I(v, o.SettleTicks, k, o), 0, k, o); break;
                    case "ticks": o.Ticks = Clamp(I(v, o.Ticks, k, o), 1, k, o); break;
                    case "warm": o.Warm = Clamp(I(v, o.Warm, k, o), 1, k, o); break;
                    case "ff": o.FastForward = ClampF(F(v, o.FastForward, k, o), 0.1f, k, o); break;
                    case "quality": o.Quality = I(v, o.Quality, k, o); break;
                    case "vsync": o.VSync = v == "1" || v.ToLowerInvariant() == "true"; break;
                    case "w": case "width": o.Width = Clamp(I(v, o.Width, k, o), 16, k, o); break;
                    case "h": case "height": o.Height = Clamp(I(v, o.Height, k, o), 16, k, o); break;
                    case "weather": o.Weather = F(v, o.Weather, k, o); break;
                    case "zoom": o.Zoom = F(v, o.Zoom, k, o); break;
                    case "yaw": o.Yaw = F(v, o.Yaw, k, o); break;
                    case "pitch": o.Pitch = F(v, o.Pitch, k, o); break;
                    case "fx": o.FocusX = F(v, o.FocusX, k, o); break;
                    case "fz": o.FocusZ = F(v, o.FocusZ, k, o); break;
                    case "subs": o.Subscribers = v == "1" || v.ToLowerInvariant() == "true"; break;
                    case "canary": o.Canary = I(v, o.Canary, k, o); break;
                    case "label": o.Label = v; break;
                    case "out": o.Out = v; break;
                    case "shot": o.Shot = v; break;
                    case "shot_tick": o.ShotTick = Clamp(I(v, o.ShotTick, k, o), -1, k, o); break;
                    case "shot_hud": o.ShotHud = !(v == "0" || v.ToLowerInvariant() == "false"); break;
                    case "shot_frames": o.ShotFrames = Math.Max(1, I(v, o.ShotFrames, k, o)); break;
                    case "quit": o.Quit = !(v == "0" || v.ToLowerInvariant() == "false"); break;
                    case "scenario": o.ScenarioRaw = v; o.Scenario = ParseScenario(v); break;
                    // several knobs= tokens add up rather than the last one winning
                    case "knobs": o.Knobs = string.IsNullOrEmpty(o.Knobs) ? v : o.Knobs + "|" + v; break;
                    case "ground":
                        o.GroundRaw = v;
                        o.GroundUnknown = !TryParseGround(v, out o.Ground);
                        if (o.GroundUnknown) o.Ground = Ground.ShelledForest;
                        break;
                    default: o.Unknown.Add(k); break;
                }
            }
            return o;
        }

        /// <summary>The scenario a `scenario=` value names; anything unknown is None.</summary>
        public static BenchScenario ParseScenario(string v)
        {
            switch ((v ?? "").Trim().ToLowerInvariant())
            {
                case "barrage": return BenchScenario.Barrage;
                case "armour": case "armor": return BenchScenario.Armour;
                case "vfx": return BenchScenario.Vfx;
                default: return BenchScenario.None;
            }
        }

        /// <summary>The name the report writes: "none", "barrage", "armour", "vfx".</summary>
        public static string ScenarioName(BenchScenario s) => s == BenchScenario.Barrage ? "barrage" : s == BenchScenario.Armour ? "armour" : s == BenchScenario.Vfx ? "vfx" : "none";

        /// <summary>A Ground by its enum name, any case (`WinterLine`, `winterline`), or by a short name: `forest`, `winter`.
        /// Numbers are refused, so `ground=1` cannot quietly mean a map. False for anything else.</summary>
        public static bool TryParseGround(string v, out Ground g)
        {
            g = Ground.ShelledForest;
            string s = (v ?? "").Trim();
            switch (s.ToLowerInvariant())
            {
                case "forest": g = Ground.ShelledForest; return true;
                case "winter": g = Ground.WinterLine; return true;
            }
            foreach (Ground x in Enum.GetValues(typeof(Ground)))
                if (string.Equals(s, x.ToString(), StringComparison.OrdinalIgnoreCase)) { g = x; return true; }
            return false;
        }

        /// <summary>The valid `ground=` values, for the error line: every Ground's name, then the short names.</summary>
        public static string GroundNames() => string.Join(", ", Enum.GetNames(typeof(Ground))) + " (or forest, winter)";

        static int I(string v, int d, string k, BenchOptions o)
        {
            if (int.TryParse(v, NumberStyles.Integer, CultureInfo.InvariantCulture, out int x)) return x;
            o.Bad.Add(k + "=" + v);
            return d;
        }

        static float F(string v, float d, string k, BenchOptions o)
        {
            if (float.TryParse(v, NumberStyles.Float, CultureInfo.InvariantCulture, out float x)) return x;
            o.Bad.Add(k + "=" + v);
            return d;
        }

        /// <summary>[F16] a floor only: a value below `min` is pulled up to it and remembered in o.Clamped, so a
        /// negative settle or ticks (cast to uint elsewhere) cannot run for billions of ticks unseen.</summary>
        static int Clamp(int x, int min, string k, BenchOptions o)
        {
            if (x >= min) return x;
            o.Clamped.Add(k + "=" + x + " -> " + min);
            return min;
        }

        static float ClampF(float x, float min, string k, BenchOptions o)
        {
            if (x >= min) return x;
            o.Clamped.Add(k + "=" + x.ToString(CultureInfo.InvariantCulture) + " -> " + min.ToString(CultureInfo.InvariantCulture));
            return min;
        }
    }

    /// <summary>What the measured window stages on top of the stress battle (PerfBench, BenchScenarios).</summary>
    public enum BenchScenario { None = 0, Barrage = 1, Armour = 2, Vfx = 3 }
}
