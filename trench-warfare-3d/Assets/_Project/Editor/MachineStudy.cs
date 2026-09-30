// Phase: A5b (tooling, 2026-09-29) — depends on: SimHost, MatchLaunch, TankCapture, VehicleKinematicsSystem, FlowFieldManager
// How the machines behave in a real match, measured rather than watched: every kind of machine on our side and six on
// theirs, set down behind each side's spawn point, then left to the sim's own orders (the enemy fielding tanks, no
// shelling) for a set time at 4x. Each machine is sampled every frame and scored on what a player would see go wrong:
//  - jammed: an order, not laying a gun, not ditched, bogged, stalled or immobilised, not arrived, and not moving, for
//    more than 2 s at a stretch (the first study counted a ditching roll and an arrival as jams: both are the rules);
//  - spinning: turning in place (under 0.3 m/s, over 0.3 rad/s);
//  - flips: its turn changing direction at over 14 deg/s, per 100 m driven (a nose hunting either way);
//  - rubbing: another hull within 70 % of the two half widths.
// Each jam, and each spin over 1 s, is logged with its place, the ground under it, the nearest machine of either side
// (where it sits off the nose, how fast it goes) and the nearest enemy. FLAG lines name what is out of bounds.
// It found the machines shoving one another on a shared line (VehicleKinematics.Avoid), then Avoid swerving a Brute
// round a column driving on ahead (AvoidMaxBend). A jam also names what the kinematics holds: its trench crossing, halt,
// ditch and bog timers, its drive, the field's way against its nose and the men within 6 m.
//   <Unity.exe> -batchmode -projectPath <p> -executeMethod TW.Editor.MachineStudy.CommandLine -twstudy "<out dir>"
//       [-twstudyseed <match seed>] [-twstudysec <sim seconds, 150>]
// Writes machines.csv, jams.txt and summary.txt into the out dir. None of this is part of the game.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using Unity.Mathematics;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Editor
{
    public static class MachineStudy
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity", Request = "tw.machinestudy";
        /// <summary>Out of bounds for one machine over a study: seconds jammed, seconds spinning, flips per 100 m.</summary>
        public const float FlagJam = 10f, FlagSpin = 10f, FlagFlips = 60f;
        static readonly byte[] Ours = { 4, 5, 18, 6, 7, 8, 9, 10, 11, 19, 20, 21, 22, 23, 24 }, Theirs = { 4, 5, 18, 7, 19, 21 };

        public static void CommandLine()
        {
            var a = System.Environment.GetCommandLineArgs();
            string Arg(string k, string d) { int i = System.Array.IndexOf(a, k); return i >= 0 && i + 1 < a.Length ? a[i + 1] : d; }
            string outDir = Arg("-twstudy", Path.Combine(Path.GetTempPath(), "machinestudy"));
            uint seed = uint.Parse(Arg("-twstudyseed", "12648430"));
            EditorSceneManager.OpenScene(Scene);
            SessionState.SetString(Request, outDir + "|" + Arg("-twstudysec", "150"));
            MatchLaunch.Current = new MatchLaunch.Request { StartingSilver = 100000, PeerDeploysTanks = true, Bombardment = 0f, MatchSeed = seed };
            EditorApplication.EnterPlaymode();
        }

        [InitializeOnLoadMethod]
        static void Hook() { EditorApplication.playModeStateChanged -= OnPlay; EditorApplication.playModeStateChanged += OnPlay; }

        static void OnPlay(PlayModeStateChange c)
        {
            if (c != PlayModeStateChange.EnteredPlayMode) return;
            string r = SessionState.GetString(Request, null);
            if (r == null) return;
            SessionState.EraseString(Request);
            var h = Object.FindFirstObjectByType<SimHost>();
            if (h == null) { Debug.LogError("MachineStudy: no SimHost"); EditorApplication.Exit(1); return; }
            var run = h.gameObject.AddComponent<Runner>();
            run.Out = r.Split('|')[0]; run.Seconds = float.Parse(r.Split('|')[1]);
        }

        sealed class Machine
        {
            public byte Arch, Team; public int Samples;
            public float Alive, Metres, Jam, JamRun, Spin, SpinRun, Rub, Flips, LastRate, LastYaw;
            public float3 Start, Last; public bool Dead;
        }

        sealed class Runner : MonoBehaviour
        {
            public string Out; public float Seconds;
            readonly Dictionary<long, Machine> seen = new Dictionary<long, Machine>();
            readonly StringBuilder jams = new StringBuilder();
            SimHost host;

            IEnumerator Start()
            {
                Directory.CreateDirectory(Out);
                host = GetComponent<SimHost>();
                yield return new WaitForSeconds(2f);
                host.WriteWorlds(m => { if (m.Bombardment != null) m.Bombardment.ShellsPerMinute = 0f; });
                var init = host.Local.World.Init;
                // two ranks abreast, 12 m apart and 8 m deep (14 put the rear rank on the map's edge), within 45 m of the spawn point (the first study's one wide rank put
                // machines at the map's edge and in their own trench, and counted them as jammed)
                for (int n = 0; n < Ours.Length; n++) { Place(0, Ours[n], init.SpawnA, n, -1f); yield return null; }
                for (int n = 0; n < Theirs.Length; n++) { Place(1, Theirs[n], init.SpawnB, n, 1f); yield return null; }
                host.TimeScale = 4f;
                uint start = host.Local.World.Tick; float last = 0f;
                float tick = host.Local.World.Config.TickSeconds;
                while ((host.Local.World.Tick - start) * tick < Seconds)
                {
                    float now = (host.Local.World.Tick - start) * tick, dt = now - last; last = now;
                    if (dt > 0f && dt < 1f) Sample(host.Local, dt);
                    yield return null;
                }
                host.TimeScale = 1f;
                Write();
                EditorApplication.ExitPlaymode();
                EditorApplication.delayCall += () => EditorApplication.Exit(0);
            }

            static void Place(int team, byte archetype, float3 spawn, int n, float back)
            {
                int perRank = 8, rank = n / perRank, file = n % perRank;
                TankCapture.Spawn(team, archetype, spawn.x + (file - (perRank - 1) * 0.5f) * 12f, spawn.z + back * rank * 8f);
            }

            void Sample(TW.Sim.Match.MatchSim sim, float dt)
            {
                var w = sim.World; var k = sim.Vehicles; var f = sim.Fields; var map = sim.Map;
                const uint Live = (uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle;
                const uint Held = (uint)(UnitFlags.Bogged | UnitFlags.Stalled | UnitFlags.Immobilised);
                for (int i = 0; i < w.Flags.Length; i++)
                {
                    uint fl = w.Flags[i];
                    if ((fl & Live) != Live) continue;
                    long key = ((long)i << 16) | w.Generation[i];
                    float3 p = w.Position[i]; float yaw = w.Yaw[i];
                    if (!seen.TryGetValue(key, out var m)) { seen[key] = new Machine { Arch = w.Archetype[i], Team = w.Team[i], Start = p, Last = p, LastYaw = yaw }; continue; }
                    if ((fl & (uint)UnitFlags.KnockedOut) != 0) { m.Dead = true; continue; }
                    m.Samples++; m.Alive += dt;
                    float step = math.length((p - m.Last).xz), speed = step / dt;
                    m.Metres += step;
                    float rate = Mathf.DeltaAngle(m.LastYaw * Mathf.Rad2Deg, yaw * Mathf.Rad2Deg) * Mathf.Deg2Rad / dt;
                    int cell = map.NavIndex(map.NavCellOf(p).x, map.NavCellOf(p).y);
                    int goal = w.GoalId[i];
                    bool arrived = goal < 0 || f.Direction[goal * f.CellCount + cell] == FlowField.NoDirection;
                    bool excused = k.HaltTicks[i] > 0 || k.DitchTicks[i] > 0 || k.BogTicks[i] > 0 || (fl & Held) != 0 || arrived;
                    if (!excused && speed < 0.2f)
                    {
                        m.JamRun += dt;
                        if (m.JamRun > 2f) m.Jam += dt;
                        if (m.JamRun > 2f && m.JamRun - dt <= 2f) LogJam("jammed", w, map, i, p, (NavLayer)map.NavLayers[cell]);
                    }
                    else m.JamRun = 0f;
                    if (speed < 0.3f && Mathf.Abs(rate) > 0.3f)
                    {
                        m.Spin += dt; m.SpinRun += dt;
                        if (m.SpinRun > 1f && m.SpinRun - dt <= 1f) LogJam("spinning", w, map, i, p, (NavLayer)map.NavLayers[cell]);
                    }
                    else m.SpinRun = 0f;
                    if (Mathf.Abs(rate) > 0.25f)
                    {
                        if (Mathf.Abs(m.LastRate) > 0.25f && Mathf.Sign(rate) != Mathf.Sign(m.LastRate)) m.Flips++;
                        m.LastRate = rate;
                    }
                    for (int j = 0; j < w.Flags.Length; j++)
                    {
                        if (j == i || (w.Flags[j] & Live) != Live) continue;
                        float r = (k.Profiles[w.Archetype[i]].HalfWidth + k.Profiles[w.Archetype[j]].HalfWidth) * 0.7f;
                        if (math.lengthsq((w.Position[j] - p).xz) < r * r) { m.Rub += dt; break; }
                    }
                    m.Last = p; m.LastYaw = yaw;
                }
            }

            void LogJam(string what, SimWorld w, MapData map, int i, float3 p, NavLayer ground)
            {
                int foe = -1, mate = -1; float foeD = float.MaxValue, mateD = float.MaxValue;
                for (int j = 0; j < w.Flags.Length; j++)
                {
                    if (j == i || (w.Flags[j] & (uint)UnitFlags.Alive) == 0) continue;
                    float d = math.distance(w.Position[j].xz, p.xz);
                    if (w.Team[j] != w.Team[i] && d < foeD) { foeD = d; foe = j; }
                    if ((w.Flags[j] & (uint)UnitFlags.Vehicle) != 0 && d < mateD) { mateD = d; mate = j; }   // either side's: Avoid steers round both
                }
                float2 nose = new float2(math.sin(w.Yaw[i]), math.cos(w.Yaw[i]));
                string bearing = mate >= 0 ? $" {math.degrees(math.acos(math.clamp(math.dot(nose, math.normalizesafe(w.Position[mate].xz - p.xz)), -1f, 1f))):F0} deg off its nose, going {math.length(w.Velocity[mate]):F1} m/s" : "";
                jams.AppendLine($"t {w.Tick * w.Config.TickSeconds:F1} s  {what}  {Name(w.Archetype[i])} (team {w.Team[i]}) at ({p.x:F0}, {p.z:F0}) on {ground}; "
                    + $"nearest machine {(mate >= 0 ? $"{Name(w.Archetype[mate])} (team {w.Team[mate]}) {mateD:F1} m{bearing}" : "none")}, nearest enemy {(foe >= 0 ? $"{foeD:F0} m" : "none")}"
                    + (what == "jammed" ? "; " + State(w, map, i, p, nose) : ""));
            }

            /// <summary>What the kinematics holds for a jammed machine.</summary>
            string State(SimWorld w, MapData map, int i, float3 p, float2 nose)
            {
                var k = host.Local.Vehicles; var f = host.Local.Fields;
                int cell = map.NavIndex(map.NavCellOf(p).x, map.NavCellOf(p).y), goal = w.GoalId[i];
                byte d = goal < 0 ? FlowField.NoDirection : f.Direction[goal * f.CellCount + cell];
                string way = d == FlowField.NoDirection ? "none" : $"{math.degrees(math.acos(math.clamp(math.dot(nose, math.normalizesafe(FlowField.Offset(d))), -1f, 1f))):F0} deg off its nose";
                int men = 0;
                for (int j = 0; j < w.Flags.Length; j++)
                    if ((w.Flags[j] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) == (uint)UnitFlags.Alive && math.distance(w.Position[j].xz, p.xz) < 6f) men++;
                return $"goal {goal}, field's way {way}, cross {k.CrossTrench[i]}, halt {k.HaltTicks[i]}, ditch {k.DitchTicks[i]}, bog {k.BogTicks[i]}, drive {k.Drive[i]}, "
                    + $"speed {w.Speed[i]:F1}, factor {k.SpeedFactor[i]:F2}, men within 6 m {men}";
            }

            static string Name(byte archetype)
            {
                var n = typeof(VehicleArchetype).GetFields();
                foreach (var fi in n) if (fi.IsLiteral && fi.FieldType == typeof(byte) && (byte)fi.GetRawConstantValue() == archetype) return fi.Name;
                return archetype.ToString();
            }

            void Write()
            {
                var csv = new StringBuilder("machine,team,alive_s,metres,jam_s,spin_s,rub_s,flips,flips_per_100m,net_m,knocked_out\n");
                var sum = new StringBuilder($"MachineStudy, {Seconds:F0} s of match time\n");
                float metres = 0f, jam = 0f, spin = 0f, flips = 0f; int flagged = 0;
                foreach (var m in seen.Values)
                {
                    if (m.Samples < 20) continue;
                    float per100 = m.Metres > 1f ? 100f * m.Flips / m.Metres : 0f;
                    csv.AppendLine($"{Name(m.Arch)},{m.Team},{m.Alive:F1},{m.Metres:F1},{m.Jam:F1},{m.Spin:F1},{m.Rub:F1},{m.Flips},{per100:F1},{math.length((m.Last - m.Start).xz):F1},{(m.Dead ? 1 : 0)}");
                    if (m.Team != 0) continue;
                    metres += m.Metres; jam += m.Jam; spin += m.Spin; flips += m.Flips;
                    if (m.Jam > FlagJam) { sum.AppendLine($"FLAG {Name(m.Arch)} jammed {m.Jam:F0} s"); flagged++; }
                    if (m.Spin > FlagSpin) { sum.AppendLine($"FLAG {Name(m.Arch)} spun in place {m.Spin:F0} s"); flagged++; }
                    if (m.Metres > 20f && per100 > FlagFlips) { sum.AppendLine($"FLAG {Name(m.Arch)} hunted: {per100:F0} flips per 100 m"); flagged++; }
                }
                sum.AppendLine($"our side: {metres:F0} m driven, {jam:F0} s jammed, {spin:F0} s spinning, {(metres > 0f ? 100f * flips / metres : 0f):F1} flips per 100 m; {flagged} flags");
                File.WriteAllText(Path.Combine(Out, "machines.csv"), csv.ToString());
                File.WriteAllText(Path.Combine(Out, "jams.txt"), jams.ToString());
                File.WriteAllText(Path.Combine(Out, "summary.txt"), sum.ToString());
            }
        }
    }
}
