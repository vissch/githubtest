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
    }
}
