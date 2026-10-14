// Phase: tooling (2026-10-14, ingame-units-review) — which units a player actually meets, counted in a real match.
//
// The units half of the review had to pick SIX units out of the nineteen in the game "by cost and deploy counts from
// a recorded or scripted match, not taste" (owner, 2026-10-12). Cost is readable in RosterEntry; the counts were not
// recorded anywhere. This records them: GreyboxCorridor, the scripted enemy of HARD fielding its tank, and a tally of
// every FRESH unit either side puts on the field over a few minutes of game time.
//
// A fresh unit is a slot whose SimWorld.Generation changed (it bumps on every spawn), plus everything standing in the
// world the first time the tally looks (the starting garrisons). Both sides are counted, team by team, because the
// player meets the enemy's men as much as his own.
//
// Nobody plays, so the player side deploys nothing after its garrison: the reading is the garrisons plus everything
// the scripted enemy sends. ScriptedEnemy.Armed() also skips the Rifle in its round robin, so the enemy's rifles come
// only from its garrison. Read the csv with that in mind — it is a record of what the sim does when left alone, not a
// forecast of a human player's build.
//
// WHY THE COUNTING IS IN A MonoBehaviour and not in the test's own loop: a [UnityTest] that enters Play is a compiler
// state machine, and seven runs on 2026-10-14 died with a NullReferenceException blamed on the line that creates a
// Dictionary — locals of that state machine do not survive what entering Play does to the domain. MachineStudy has a
// Runner component for the same reason. The test starts a component, waits, and reads its file; the component owns
// every piece of state.
//
// It is [Explicit]: an ordinary EditMode gate must not enter Play. Run it by name, with a graphics device:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode \
//             -testFilter TheDeploysOfARealMatchAreTallied -testResults <out.xml> -logFile <out.log>
// TW_TALLY_SECONDS sets the game seconds to watch (default 240). The csv lands in Captures/unitsday/deploys.csv,
// which is outside git (trench-warfare-3d/.gitignore). Nothing here judges anything.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Sim;
using TW.UI;
using TW.Editor;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    /// <summary>Counts every fresh unit of a running match and writes the tally. Lives on the SimHost's own object, so
    /// it is there for as long as the match is.</summary>
    public sealed class DeployTally : MonoBehaviour
    {
        /// <summary>Game seconds to watch, counted in the sim's own ticks (20 a second, SimConfig.TickRate).</summary>
        public float Seconds = 240f;
        /// <summary>Where the csv goes. Set by whoever adds the component.</summary>
        public string Out;
        /// <summary>Written when the csv is on disk; the test waits for it.</summary>
        public bool Done;
        public string Note = "";

        readonly Dictionary<int, int> counts = new Dictionary<int, int>();   // (team << 8) | archetype -> fresh units
        readonly Dictionary<int, int> lastGen = new Dictionary<int, int>();  // slot -> generation last counted
        SimHost host;
        uint firstTick, lastTick;
        bool started;

        void Start()
        {
            host = GetComponent<SimHost>();
            // The enemy a player meets on HARD (CampaignGraph): quick deploys, early attacks, its tank whenever it can
            // pay. These are the host's own knobs, which SimHost.SyncEnemy copies to the scripted enemy every frame, so
            // setting them in Play is the same as choosing HARD in the shell. The silver stays the game's own 300 plus
            // 2 a second: a tally run on a sandbox pile would count what the enemy could never afford.
            if (host != null)
            {
                host.ScriptedPeer = true; host.PeerDeploysTanks = true; host.PeerDeployEveryTicks = 28;
                host.PeerAttacks = true; host.PeerAttackGarrison = 6; host.PeerUsesSupport = true; host.PeerDefends = true;
            }
        }

        void Update()
        {
            if (Done || host == null || host.Local == null) return;
            var w = host.Local.World;
            if (!started) { firstTick = w.Tick; lastTick = firstTick; started = true; }
            for (int s = 0; s < w.HighWater; s++)
            {
                if (!w.IsAlive(s)) continue;
                int gen = w.Generation[s];
                int seen;
                if (lastGen.TryGetValue(s, out seen) && seen == gen) continue;
                lastGen[s] = gen;
                int key = (w.Team[s] << 8) | w.Archetype[s];
                int n;
                counts.TryGetValue(key, out n);
                counts[key] = n + 1;
            }
            lastTick = w.Tick;
            if (lastTick - firstTick < (uint)Mathf.RoundToInt(Seconds * 20f)) return;
            Write();
        }

        /// <summary>Archetype id to the name the review and the brief use.</summary>
        public static string NameOf(byte a)
        {
            switch (a)
            {
                case InfantryArchetype.Rifle: return "Rifle";
                case InfantryArchetype.Assault: return "Assault";
                case InfantryArchetype.Machinegunner: return "Machinegunner";
                case InfantryArchetype.Sniper: return "Sniper";
                case InfantryArchetype.Officer: return "Officer";
                case InfantryArchetype.Shield: return "Shield";
                case InfantryArchetype.Medic: return "Medic";
                case InfantryArchetype.Repair: return "Repair";
                case InfantryArchetype.Para: return "Para";
                case InfantryArchetype.Jetpack: return "Jetpack";
                case VehicleArchetype.Maw: return "Maw";
                case VehicleArchetype.Tusk: return "Tusk";
                case VehicleArchetype.Pincer: return "Pincer";
                case VehicleArchetype.Kettle: return "Kettle";
                case VehicleArchetype.Censer: return "Censer";
                case VehicleArchetype.Pavise: return "Pavise";
                case VehicleArchetype.Banner: return "Banner";
                case VehicleArchetype.Redoubt: return "Redoubt";
                case VehicleArchetype.Breaker: return "Breaker";
                default: return "archetype" + a;
            }
        }

        void Write()
        {
            var sb = new StringBuilder();
            sb.AppendLine("unit,archetype,team,fresh_units,cost,is_vehicle");
            var keys = new List<int>(counts.Keys);
            keys.Sort((a, b) => counts[b] != counts[a] ? counts[b] - counts[a] : a - b);
            foreach (int key in keys)
            {
                byte arch = (byte)(key & 0xFF);
                var e = RosterEntry.ForArchetype(arch);
                sb.AppendLine(NameOf(arch) + "," + arch + "," + (key >> 8) + "," + counts[key] + "," + e.Cost
                    + "," + (e.IsVehicle ? 1 : 0));
            }
            // Both teams added up: the player meets a unit whichever side fields it.
            var both = new Dictionary<byte, int>();
            foreach (var kv in counts)
            {
                byte a = (byte)(kv.Key & 0xFF);
                int n;
                both.TryGetValue(a, out n);
                both[a] = n + kv.Value;
            }
            var archs = new List<byte>(both.Keys);
            archs.Sort((a, b) => both[b] != both[a] ? both[b] - both[a] : a - b);
            sb.AppendLine("# both teams together: unit,archetype,fresh_units,cost");
            foreach (byte a in archs)
                sb.AppendLine("# " + NameOf(a) + "," + a + "," + both[a] + "," + RosterEntry.ForArchetype(a).Cost);
            sb.AppendLine("# ticks " + firstTick + ".." + lastTick + " (" + Seconds
                + " game seconds at 20 ticks a second)");

            Directory.CreateDirectory(Path.GetDirectoryName(Out));
            File.WriteAllText(Out, sb.ToString(), new UTF8Encoding(false));
            Note = counts.Count + " unit kinds over ticks " + firstTick + ".." + lastTick;
            Debug.Log("DeployTally: " + Note + " -> " + Out);
            Done = true;
        }
    }

    /// <summary>Films one unit of the review through five beats and measures it as it goes. A MonoBehaviour for the same
    /// reason DeployTally is one: a [UnityTest] that enters Play loses its locals, so every piece of state lives here.</summary>
    public sealed class UnitFilm : MonoBehaviour
    {
        public string Unit = "Rifle";
        public byte Archetype;
        public bool Vehicle;
        public int Subject = -1;
        public int[] Row = new int[0];
        public string Dir;
        /// <summary>Where the row was put and in which field, written into the txt so a reader of the numbers knows
        /// whether they came off open ground by day or out of a trench at night.</summary>
        public string Where = "";
        public bool Done;
        public string Note = "";

        /// <summary>Seconds of game time the film runs: the five beats fill 13 s (plan step 4).</summary>
        public const float Seconds = 13f;
        public const int Fps = 24;

        SimHost host;
        float t0 = -1f;
        int beat;
        readonly Dictionary<int, int> clipTicks = new Dictionary<int, int>();
        int moving, outsideClamp;
        float leftoverSum, leftoverWorst;
        int shots, shotsWithoutPose;
        float worstYawStep;
        // per slot, not one value: when the subject dies the measuring moves to the next man alive, and a single
        // previous yaw turned that change of man into a 131 deg "snap" in the first twelve films
        readonly Dictionary<int, float> prevShownYaw = new Dictionary<int, float>();
        uint deathTick;
        float deathFrameBefore, deathFrameAfter, deathYawBefore, deathYawAfter;
        bool deathSeen, deathArmed;
        uint lastTick;
        float gunYawWorstStep = float.NaN, prevGunYaw = float.NaN, recoilPeak = float.NaN;
        string machineNote = "no renderer field named GunYaw or Recoil was readable";
        int hullLast = -1, hullMoving, hullShots;
        float hullX, hullZ, hullYaw, hullSpeedSum, hullSpeedPeak, hullYawStep, hullCooldown;


        void Update()
        {
            if (Done) return;
            if (host == null) host = GetComponent<SimHost>();
            if (host == null || host.Local == null || host.Animation == null) return;
            if (t0 < 0f) { t0 = Time.time; RiderLab.Film(Dir, Seconds, Fps, 1280, 720); }
            float t = Time.time - t0;

            Measure();
            Cue(t);

            if (t < Seconds + 1.5f) return;
            Write();
        }

        /// <summary>The beats, cued on game time (Time.captureDeltaTime pins a frame to 1/30 s, so Time.time IS game time).</summary>
        void Cue(float t)
        {
            // 0-3 s walking at play zoom: the enemies put 70 m ahead at spawn make the men advance on their own.
            // 3-5 s a turn: the group ahead is removed and a new one put off the right shoulder, so they come round.
            if (beat == 0 && t >= 3f)
            {
                beat = 1;
                if (Alive(Subject)) { RiderLab.ClearEnemies(Subject, 160f); RiderLab.Enemies(Subject, 5, 55f, 90f); }
            }
            // 5-9 s firing: a group back on the nose, close enough that the sim engages it
            else if (beat == 1 && t >= 5f)
            {
                beat = 2;
                if (Alive(Subject)) { RiderLab.ClearEnemies(Subject, 160f); RiderLab.Enemies(Subject, 6, 26f, 0f); }
            }
            // 9-11 s hit or dying: a real shell through the sim's own blast, on the far end of the row. The subject is
            // meant to live to the last beat; if it does not, the camera moves to whoever is left.
            else if (beat == 2 && t >= 9f)
            {
                beat = 3;
                deathArmed = true;
                int victim = Victim();
                if (Alive(victim))
                {
                    var p = host.Local.World.Position[victim];
                    DeathLab.Shell(p.x, p.z, 5f, 400f);
                }
            }
            // 11-13 s close, from the front quarter: walking and firing where a player leans in to look
            else if (beat == 3 && t >= 11f)
            {
                beat = 4;
                if (!Alive(Subject)) Subject = FirstAlive();
                var cam = Object.FindFirstObjectByType<TacticalCamera>();
                if (cam != null) cam.BaseYaw = -45f;
                if (Subject >= 0) RiderLab.Camera(Subject, Vehicle ? 11f : 7f, -45f, Vehicle ? 20f : 34f);
            }
        }

        int Victim() => Row.Length > 1 ? Row[Row.Length - 1] : Subject;

        bool Alive(int slot) => slot >= 0 && host.Local.World.IsAlive(slot);

        int FirstAlive()
        {
            for (int k = 0; k < Row.Length; k++) if (Alive(Row[k])) return Row[k];
            return -1;
        }

        void Measure()
        {
            var w = host.Local.World;
            uint tick = w.Tick;
            if (tick == lastTick) return;
            lastTick = tick;
            int slot = Alive(Subject) ? Subject : FirstAlive();
            if (slot < 0) return;
            var a = host.Animation;
            if (!a.State.IsCreated || slot >= a.State.Length) return;
            var s = a.State[slot];

            // (e) the clip histogram: a unit that never leaves one clip shows up here
            int key = (int)s.Clip;
            int n; clipTicks.TryGetValue(key, out n); clipTicks[key] = n + 1;

            // (a) foot slide: the rate clamp (AnimationController, clamp(speed/authored, 0.6, 1.6)) cannot stretch a
            // walk cycle past its ends, so whatever speed is left over is ground passing under a planted foot
            float authored = Clips.Table[key].Speed;
            if (s.Speed > 0.3f && authored > 0.01f)
            {
                moving++;
                float want = s.Speed / authored;
                float rate = Mathf.Clamp(want, 0.6f, 1.6f);
                if (want < 0.5999f || want > 1.6001f) outsideClamp++;
                float over = Mathf.Abs(s.Speed - authored * rate);
                leftoverSum += over;
                if (over > leftoverWorst) leftoverWorst = over;
            }

            // (b) a shot with no pose: the sim fired this tick but the ladder is not on a fire clip. The ladder only
            // holds a fire clip under 0.3 m/s, so a man firing on the move is EXPECTED here; the share says how often.
            if (s.LastShot == tick)
            {
                shots++;
                if (s.Rung != Rung.Fire) shotsWithoutPose++;
            }

            // (c) a turn that snaps: the largest ShownYaw step in one tick, of THIS man
            float was;
            if (prevShownYaw.TryGetValue(slot, out was))
            {
                float d = Mathf.Abs(Mathf.DeltaAngle(was * Mathf.Rad2Deg, s.ShownYaw * Mathf.Rad2Deg));
                if (d > worstYawStep) worstYawStep = d;
            }
            prevShownYaw[slot] = s.ShownYaw;

            // (d) a death that pops: the clip frame and the drawn yaw either side of the tick Dead turns true
            if (deathArmed && !deathSeen)
            {
                int victim = Victim();
                if (victim >= 0 && victim < a.State.Length)
                {
                    var v = a.State[victim];
                    float yaw = a.Yaw.IsCreated && victim < a.Yaw.Length ? a.Yaw[victim] * Mathf.Rad2Deg : float.NaN;
                    if (v.Dead)
                    {
                        deathSeen = true; deathTick = tick;
                        deathFrameAfter = v.Frame; deathYawAfter = yaw;
                    }
                    else { deathFrameBefore = v.Frame; deathYawBefore = yaw; }
                }
            }

            if (Vehicle) { MeasureHull(slot, tick); MeasureMachine(slot); }
        }

        /// <summary>A hull is not on the infantry ladder, so the ladder reads Idle for it and nothing else. These are the
        /// sim's own arrays: how fast it moves, the largest hull yaw step in a tick, every reset of its gun cooldown
        /// (one shot), and the tick it stopped being alive.</summary>
        void MeasureHull(int slot, uint tick)
        {
            var w = host.Local.World;
            var p = w.Position[slot];
            if (hullLast >= 0)
            {
                float d = Mathf.Sqrt((p.x - hullX) * (p.x - hullX) + (p.z - hullZ) * (p.z - hullZ));
                float dt = (tick - (uint)hullLast) / 20f;
                if (dt > 0f)
                {
                    float v = d / dt;
                    if (v > hullSpeedPeak) hullSpeedPeak = v;
                    if (v > 0.2f) { hullMoving++; hullSpeedSum += v; }
                }
                float dy = Mathf.Abs(Mathf.DeltaAngle(hullYaw * Mathf.Rad2Deg, w.Yaw[slot] * Mathf.Rad2Deg));
                if (dy > hullYawStep) hullYawStep = dy;
            }
            hullX = p.x; hullZ = p.z; hullYaw = w.Yaw[slot]; hullLast = (int)tick;

            float cd = w.FireCooldown[slot];
            if (cd > hullCooldown + 0.5f) hullShots++;   // the cooldown jumped back up: the gun has just fired
            hullCooldown = cd;
            // Measure() only ever hands this a living slot, so a hull's death is read in Write() instead.
        }

        /// <summary>A hull's gun yaw and recoil, by reflection: those fields are the renderer's own and move about, so a
        /// miss is reported in the txt rather than compiled against.</summary>
        void MeasureMachine(int slot)
        {
            var tr = Object.FindFirstObjectByType<MonoBehaviour>();
            foreach (var mb in Object.FindObjectsByType<MonoBehaviour>(FindObjectsSortMode.None))
                if (mb.GetType().Name == "TankRenderer") { tr = mb; break; }
            if (tr == null || tr.GetType().Name != "TankRenderer") return;
            float gy, rc;
            if (Read(tr, "GunYaw", slot, out gy))
            {
                if (!float.IsNaN(prevGunYaw))
                {
                    float d = Mathf.Abs(Mathf.DeltaAngle(prevGunYaw, gy));
                    if (float.IsNaN(gunYawWorstStep) || d > gunYawWorstStep) gunYawWorstStep = d;
                }
                prevGunYaw = gy;
                machineNote = "GunYaw read by reflection off TankRenderer";
            }
            if (Read(tr, "Recoil", slot, out rc) && (float.IsNaN(recoilPeak) || rc > recoilPeak)) recoilPeak = rc;
        }

        static bool Read(object o, string name, int slot, out float v)
        {
            v = float.NaN;
            var t = o.GetType();
            const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Public
                | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance;
            var f = t.GetField(name, Any);
            object raw = f != null ? f.GetValue(o) : null;
            if (raw == null)
            {
                var pr = t.GetProperty(name, Any);
                raw = pr != null ? pr.GetValue(o) : null;
            }
            if (raw == null) return false;
            if (raw is float) { v = (float)raw; return true; }
            if (raw is float[])
            {
                var arr = (float[])raw;
                if (slot >= arr.Length) return false;
                v = arr[slot]; return true;
            }
            if (raw is Unity.Collections.NativeArray<float>)
            {
                var na = (Unity.Collections.NativeArray<float>)raw;
                if (!na.IsCreated || slot >= na.Length) return false;
                v = na[slot]; return true;
            }
            return false;
        }

        void Write()
        {
            var sb = new StringBuilder();
            sb.AppendLine("unit " + Unit + " (archetype " + Archetype + ", " + (Vehicle ? "machine" : "man") + ")");
            sb.AppendLine("ticks measured up to " + lastTick + ", beats run " + beat + ", frames into " + Dir);
            sb.AppendLine(Where);
            sb.AppendLine();
            sb.AppendLine("(a) foot slide, from the rate clamp clamp(speed/authored, 0.6, 1.6)");
            sb.AppendLine("    moving ticks " + moving + ", outside the clamp " + outsideClamp
                + (moving > 0 ? " (" + (100f * outsideClamp / moving).ToString("0.0") + " %)" : "")
                + ", mean leftover " + (moving > 0 ? (leftoverSum / moving).ToString("0.000") : "-")
                + " m/s, worst " + leftoverWorst.ToString("0.000") + " m/s");
            sb.AppendLine("(b) a shot with no pose (LastShot == tick while Rung != Fire)");
            sb.AppendLine("    shots " + shots + ", without a fire pose " + shotsWithoutPose
                + (shots > 0 ? " (" + (100f * shotsWithoutPose / shots).ToString("0.0") + " %)" : ""));
            sb.AppendLine("(c) a turn that snaps: largest ShownYaw step in one tick " + worstYawStep.ToString("0.00") + " deg");
            sb.AppendLine("(d) a death that pops: " + (deathSeen
                ? "tick " + deathTick + ", clip frame " + deathFrameBefore.ToString("0.000") + " -> " + deathFrameAfter.ToString("0.000")
                  + ", drawn yaw " + deathYawBefore.ToString("0.0") + " -> " + deathYawAfter.ToString("0.0") + " deg"
                : "no death was seen in the row"));
            if (Vehicle)
            {
                // (a) to (d) above are the infantry ladder and read Idle for a hull: these are the sim's own numbers
                sb.AppendLine("(f) the hull, off the sim's arrays (the ladder above is infantry only)");
                sb.AppendLine("    moving ticks " + hullMoving + ", mean speed "
                    + (hullMoving > 0 ? (hullSpeedSum / hullMoving).ToString("0.00") : "-") + " m/s, peak "
                    + hullSpeedPeak.ToString("0.00") + " m/s");
                sb.AppendLine("    largest hull yaw step in a tick " + hullYawStep.ToString("0.00")
                    + " deg, gun fired " + hullShots + " times, alive at the end "
                    + (Row.Length > 0 && Alive(Row[0]) ? "yes" : "no"));
                sb.AppendLine("    " + machineNote + "; largest gun yaw step "
                    + (float.IsNaN(gunYawWorstStep) ? "-" : gunYawWorstStep.ToString("0.00") + " deg")
                    + ", recoil peak " + (float.IsNaN(recoilPeak) ? "-" : recoilPeak.ToString("0.000")));
            }
            sb.AppendLine("(e) clips held, in ticks");
            var keys = new List<int>(clipTicks.Keys);
            keys.Sort((x, y) => clipTicks[y] - clipTicks[x]);
            foreach (int k in keys) sb.AppendLine("    " + (Clip)k + " " + clipTicks[k]);

            Directory.CreateDirectory(Dir);
            File.WriteAllText(Path.Combine(Dir, Unit + ".txt"), sb.ToString(), new UTF8Encoding(false));
            Note = "beats " + beat + ", moving " + moving + ", shots " + shots + ", death " + deathSeen;
            Debug.Log("UnitFilm: " + Note);
            Done = true;
        }
    }

    public class UnitFilmStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Where the tally lands: &lt;project&gt;/Captures/unitsday, which git ignores.</summary>
        static string Dir()
        {
            string root = Directory.GetParent(Application.dataPath).FullName;
            string d = Path.Combine(root, "Captures", "unitsday");
            Directory.CreateDirectory(d);
            return d;
        }

        [UnityTest, Explicit("Instrument: tallies a scripted match's deploys; run by name, with a graphics device.")]
        public IEnumerator TheDeploysOfARealMatchAreTallied()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            // Read after entering Play: the domain reloads there and anything set before it comes back a default.
            string secs = System.Environment.GetEnvironmentVariable("TW_TALLY_SECONDS");
            float watch = string.IsNullOrEmpty(secs) ? 240f : float.Parse(secs);
            string path = Path.Combine(Dir(), "deploys.csv");
            if (File.Exists(path)) File.Delete(path);
            var tally = Host.gameObject.AddComponent<DeployTally>();
            tally.Seconds = watch;
            tally.Out = path;

            // 20 ticks a second of game time, and a batch frame runs well over a tick, so the cap is generous. The
            // component is asked for afresh every frame: the one added above is gone if the scene reloads.
            int cap = Mathf.RoundToInt(watch * 20f) * 40 + 1200;
            for (int f = 0; f < cap; f++)
            {
                var t = Object.FindFirstObjectByType<DeployTally>();
                if (t != null && t.Done) break;
                if (File.Exists(path)) break;
                yield return null;
            }

            Assert.That(File.Exists(path), Is.True, "the tally never wrote " + path);
            TestContext.Out.WriteLine(File.ReadAllText(path));
            yield return new ExitPlayMode();
        }

        /// <summary>Archetype id of a name DeployTally.NameOf would print, or 255.</summary>
        static byte ArchetypeOf(string name)
        {
            for (int a = 0; a < 32; a++)
                if (string.Equals(DeployTally.NameOf((byte)a), name, System.StringComparison.OrdinalIgnoreCase)) return (byte)a;
            return 255;
        }

        /// <summary>A lane of level open ground to film in: no trench, link, wire, bunker or prop within the walk, along
        /// the map's +x edge so nothing stands between it and the camera (the reason WalkerStills films there too).</summary>
        static bool OpenLane(SimHost h, out float x, out float z)
        {
            var map = h.Local.Map;
            var size = map.SizeMeters;
            // 34 m is what a man covers in the 13 s of the film at his own speed, and 12 m across holds six abreast
            // with room to turn. The whole map is searched, not a strip along one edge: GreyboxCorridor is a trench
            // corridor and the strip the walkers are filmed on has no open ground of this length in it.
            const float Ahead = 34f, Wide = 12f;
            var bad = TW.Sim.Terrain.NavLayer.Trench | TW.Sim.Terrain.NavLayer.Link
                    | TW.Sim.Terrain.NavLayer.Blocked | TW.Sim.Terrain.NavLayer.Wire
                    | TW.Sim.Terrain.NavLayer.Bunker;
            x = size.x * 0.5f; z = size.y * 0.25f;
            for (float zz = 15f; zz < size.y - Ahead - 10f; zz += 2f)
                for (float xx = Wide; xx <= size.x - Wide; xx += 2f)
                {
                    bool open = true;
                    for (float dz = -3f; dz <= Ahead && open; dz += 2f)
                        for (float dx = -Wide * 0.5f; dx <= Wide * 0.5f && open; dx += 2f)
                            if ((map.LayerAt(new Unity.Mathematics.float3(xx + dx, 0f, zz + dz)) & bad) != 0) open = false;
                    for (int i = 0; i < map.Props.Length && open; i++)
                    {
                        var pp = map.Props[i].Pos;
                        if (pp.x > xx - Wide && pp.x < xx + Wide && pp.z > zz - 4f && pp.z < zz + Ahead + 4f) open = false;
                    }
                    if (open) { x = xx; z = zz; return true; }
                }
            return false;
        }

        [UnityTest, Explicit("Films one unit of the review through five beats and measures it; run by name, with a graphics device.")]
        public IEnumerator AUnitIsFilmedByDay()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // Winter is the only daylight field, and GreyboxTerrainView.Start reads Atmosphere.Field: set both, or the
            // film is one of the dark ones the critique already rejected.
            // Winter is the only daylight field. TryParse writes default(Biome) - the night field - into its out
            // parameter when it fails, so an unset TW_STILLS_FIELD must NOT be allowed to touch the choice.
            var field = TW.Presentation.Terrain.Biome.Winter;
            TW.Presentation.Terrain.Biome asked;
            if (System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out asked)) field = asked;
            foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
            foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            yield return new EnterPlayMode();
            // everything after the reload is a fresh enumerator (WreckStills: a closure made before it comes back null)
            yield return FilmUnit();
        }

        static IEnumerator FilmUnit()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            string unit = System.Environment.GetEnvironmentVariable("TW_UNIT");
            if (string.IsNullOrEmpty(unit)) unit = "Rifle";
            string take = System.Environment.GetEnvironmentVariable("TW_TAKE");
            if (string.IsNullOrEmpty(take)) take = "take1";
            var sky0 = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            var field = sky0 != null ? sky0.Field : TW.Presentation.Terrain.Biome.Winter;
            byte arch = ArchetypeOf(unit);
            Assert.That(arch, Is.Not.EqualTo((byte)255), "TW_UNIT=" + unit + " is not a unit this review knows");
            bool vehicle = RosterEntry.ForArchetype(arch).IsVehicle;

            // quiet: nobody deploys, nobody shells the lane, no rain, the clock pinned to midday
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;
            RiderLab.Panel(false);

            float x, z;
            bool openGround = OpenLane(Host, out x, out z);
            TestContext.Out.WriteLine((openGround ? "open ground at " : "NO OPEN GROUND FOUND; filming anyway at ")
                + x.ToString("0") + ", " + z.ToString("0"));
            var row = new List<int>();
            // six men abreast, as a squad is met; one hull for a machine
            int n = vehicle ? 1 : 6;
            for (int k = 0; k < n; k++)
            {
                string m = TankCapture.Spawn(0, arch, x + (k - (n - 1) * 0.5f) * 1.8f, z, 0f);
                if (m.StartsWith("slot ")) row.Add(int.Parse(m.Substring(5)));
                yield return null;
            }
            Assert.That(row.Count, Is.GreaterThan(0), "nothing spawned for " + unit);
            int subject = row[0];

            // the zoom the game is played at (men 7 m, hulls 22 m: the plan's figures, off the Banner's film)
            RiderLab.Camera(subject, vehicle ? 22f : 7f, 150f, vehicle ? 24f : 42f);
            // something to walk towards, 70 m off, so the first beats are a real advance and not a man standing still
            RiderLab.Enemies(subject, 6, 40f, 0f);
            for (int f = 0; f < 60; f++) yield return null;

            string dir = Path.Combine(Dir(), unit, take);
            Directory.CreateDirectory(dir);
            foreach (var f in Directory.GetFiles(dir, "*.png")) File.Delete(f);
            var film = Host.gameObject.AddComponent<UnitFilm>();
            film.Unit = unit; film.Archetype = arch; film.Vehicle = vehicle;
            film.Subject = subject; film.Row = row.ToArray(); film.Dir = dir;
            film.Where = (openGround ? "open ground at " : "NO OPEN GROUND FOUND, filmed at ")
                + x.ToString("0") + ", " + z.ToString("0") + "; field " + field;

            // 13 s of game time at 1/30 s a frame, and a batch frame runs far slower than that: the cap is generous
            string txt = Path.Combine(dir, unit + ".txt");
            for (int f = 0; f < 40000; f++)
            {
                var c = Object.FindFirstObjectByType<UnitFilm>();
                if (c != null && c.Done) break;
                if (File.Exists(txt)) break;
                yield return null;
            }
            // the film runs on the same component; let the last frames land
            for (int f = 0; f < 900 && RiderLab.FilmStatus().StartsWith("filming"); f++) yield return null;

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            int frames = Directory.Exists(dir) ? Directory.GetFiles(dir, "f_*.png").Length : 0;
            TestContext.Out.WriteLine(unit + " " + take + ": " + frames + " frames in " + dir);
            if (File.Exists(txt)) TestContext.Out.WriteLine(File.ReadAllText(txt));
            yield return new ExitPlayMode();
            Assert.That(File.Exists(txt), Is.True, "the film never wrote " + txt);
            Assert.That(frames, Is.GreaterThanOrEqualTo(300), "too few frames landed to judge a cycle (13 s x 24 fps = 312)");
        }

    }
}
