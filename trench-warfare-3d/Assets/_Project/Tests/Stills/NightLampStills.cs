// Phase: night lights (2026-10-07, tooling) — the look at the real-lamp rule (NightLights.RealLamps.cs: the 8 fixed
// lamps nearest the view keep their real light, the rest are painted only), from batch mode with nobody driving, the
// way WreckStills photographs the wrecks (an [Explicit] EditMode test that enters Play itself). One held frame of a
// night stress battle, with two burning machines in it, photographed from the same poses with:
//   today    lights.realLamps 43, look.poolsByView 0: the night before the rule
//   new      the defaults: 8 real lamps, the lamps by water, the painted pools chosen by the view
//   lamps8   lights.realLamps 8 with look.poolsByView 0: the lamp rule alone
//   nowater  the defaults with lights.waterLamps 0 (the poses by water only): what the water rule buys
// at the play view, two wide views, close by a bunker, close by a lamp among the men, by water (close, at the play
// view, and from far enough off that the lamp by the water is not one of the eight) and at a burning wreck; then a
// row of stills while the view pans across lamps, 0.1 s apart, to show the fade; then a rough timing of
// the camera's render with the rule off and on. info.txt says for every still which lamps kept a real light, how many
// lights were lit by kind and how many point lights reached into the picture. No game code is changed by the test.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter NightLampStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-nightlamps: never inside the checkout),
// TW_NIGHT_STRESS riflemen a side (250), TW_NIGHT_SETTLE ticks before the frame is held (1800), TW_NIGHT_TIMING
// renders a round for the timing (40; 0 skips it).
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class NightLampStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        static string Env(string k, string d) { string v = System.Environment.GetEnvironmentVariable(k); return string.IsNullOrEmpty(v) ? d : v; }

        /// <summary>Every Light in the loaded scene, the hidden ones too (NightLights' are HideFlags.DontSave, which
        /// FindObjectsByType leaves out).</summary>
        static List<Light> SceneLights()
        {
            var list = new List<Light>();
            foreach (var l in Resources.FindObjectsOfTypeAll<Light>())
                if (l != null && l.gameObject.scene.IsValid() && l.gameObject.activeInHierarchy) list.Add(l);
            return list;
        }

        static string Kind(string name)
        {
            int end = name.Length;
            while (end > 0 && (char.IsDigit(name[end - 1]) || name[end - 1] == ' ')) end--;
            return name.Substring(0, end);
        }

        static string CountLine(Dictionary<string, int> d)
        {
            var keys = new List<string>(d.Keys); keys.Sort();
            var sb = new StringBuilder();
            foreach (var k in keys) sb.Append($" [{k}] {d[k]}");
            return sb.ToString();
        }

        // what the capture's render saw: filled as each still is taken (after NightLights has chosen for that camera:
        // it hooked the same event first)
        static NightLights night; static List<Light> lamps; static string seen = ""; static float lastPools = -1f;
        static readonly List<string> seenAll = new List<string>();   // one entry a capture render, for a queued row of stills
        static bool once;                                            // the timing: look at the first render of a round only

        static void OnBegin(ScriptableRenderContext ctx, Camera cam)
        {
            if (cam.cameraType != CameraType.Game || cam.targetTexture == null || night == null) return;
            if (once && seen != "") return;
            var planes = GeometryUtility.CalculateFrustumPlanes(cam);
            var lit = new Dictionary<string, int>(); int inView = 0, fixedInView = 0;
            foreach (var l in SceneLights())
            {
                if (!l.enabled || l.intensity <= 0f) continue;
                string k = l.type == LightType.Point ? Kind(l.name) : l.type + " " + Kind(l.name);
                lit.TryGetValue(k, out int n); lit[k] = n + 1;
                if (l.type != LightType.Point) continue;
                bool reaches = true; Vector3 p = l.transform.position;
                for (int i = 0; i < 6 && reaches; i++) if (planes[i].GetDistanceToPoint(p) < -l.range) reaches = false;
                if (!reaches) continue;
                inView++; if (lamps.Contains(l)) fixedInView++;
            }
            var sb = new StringBuilder();
            int real = 0, byWater = 0, fading = 0; var names = new StringBuilder(); var fades = new StringBuilder();
            for (int i = 0; i < lamps.Count; i++)
            {
                float w = night.RealLamps.Weight(i);
                if (lamps[i].enabled) { real++; names.Append(' ').Append(lamps[i].name.Replace(" ", "")); if (night.LampByWater(i)) { byWater++; names.Append("(water)"); } }
                if (w > 0f && w < 1f) { fading++; fades.Append($" {lamps[i].name.Replace(" ", "")}={lamps[i].intensity / Mathf.Max(0.01f, LevelOf(i)):0.00}"); }
            }
            sb.Append($"fixed lamps with a real light {real} of {lamps.Count} ({byWater} of them by water); point lights reaching into the picture {inView} ({fixedInView} fixed lamps); lit:{CountLine(lit)}");
            if (fading > 0) sb.Append($"; fading:{fades}");
            sb.Append($"; real:{names}");
            seen = sb.ToString();
            if (!once) seenAll.Add(seen);
        }

        static float LevelOf(int i)
        {
            var level = night.GetType().GetField("lanternLevel", Any)?.GetValue(night) as List<float>;
            return level != null && i < level.Count ? level[i] : 1f;
        }

        static void OnEnd(ScriptableRenderContext ctx, Camera cam)
        {
            if (cam.cameraType != CameraType.Game || cam.targetTexture == null) return;
            lastPools = Shader.GetGlobalFloat("_TWPoolCount");
        }

        /// <summary>Renders the main camera `Count` times at the end of a frame (after every LateUpdate that draws), and
        /// says how long a render took: the timing's instrument.</summary>
        [DefaultExecutionOrder(31000)]
        sealed class RenderTimer : MonoBehaviour
        {
            public int Count; public double Ms = -1; public RenderTexture Target;
            void LateUpdate()
            {
                if (Count <= 0) return;
                var c = Camera.main; if (c == null) { Count = 0; Ms = -2; return; }
                var before = c.targetTexture; c.targetTexture = Target;
                var probe = new Texture2D(1, 1, TextureFormat.RGB24, false);
                var sw = System.Diagnostics.Stopwatch.StartNew();
                for (int k = 0; k < Count; k++) c.Render();
                var was = RenderTexture.active; RenderTexture.active = Target;
                probe.ReadPixels(new Rect(0, 0, 1, 1), 0, 0);   // waits for the graphics card: the renders are done when this returns
                RenderTexture.active = was;
                sw.Stop();
                c.targetTexture = before; Destroy(probe);
                Ms = sw.Elapsed.TotalMilliseconds / Count; Count = 0;
            }
        }

        [UnityTest, Explicit("The night's real lamps, before and after the rule, photographed; run by name, with a graphics device.")]
        public IEnumerator TheRealLampsArePhotographedBeforeAndAfter()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            var host = Host;
            Assert.That(host, Is.Not.Null, "no SimHost in the scene");
            host.StressUnits = int.Parse(Env("TW_NIGHT_STRESS", "250"), Inv);   // in memory only: the scene is never saved
            yield return new EnterPlayMode();
            yield return InPlay();
        }

        static Vector2 OpenNear(MapData map, Vector2 want, float clear = 8f, float propClear = 9f)
        {
            var size = map.SizeMeters;
            Vector2 best = want; float bestD = float.MaxValue;
            for (float zz = 20f; zz < size.y - 20f; zz += 3f)
                for (float xx = 20f; xx < size.x - 20f; xx += 3f)
                {
                    float d = (new Vector2(xx, zz) - want).sqrMagnitude;
                    if (d >= bestD) continue;
                    bool open = true;
                    for (float dz = -clear; dz <= clear && open; dz += 2f)
                        for (float dx = -clear; dx <= clear && open; dx += 2f)
                            if ((map.LayerAt(new float3(xx + dx, 0f, zz + dz)) & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) open = false;
                    for (int i = 0; i < map.Props.Length && open; i++) if (math.distancesq(map.Props[i].Pos.xz, new float2(xx, zz)) < propClear * propClear) open = false;
                    if (open) { best = new Vector2(xx, zz); bestD = d; }
                }
            return best;
        }

        static Vector2 Flat(Vector3 p) => new Vector2(p.x, p.z);

        /// <summary>The focus the game chooses lamps by (NightLights.FocusOf) for a still CaptureRig poses at `at` with this
        /// yaw and pitch at an ordinary zoom, where the rig aims the lens at y = 0 over `at` (CaptureRig's Pose): worked
        /// out from that lens, so the set this tool expects follows the game's focus and not a copy of it. A flat focus
        /// written out here did not see the game's own focus step at a trench's edge.</summary>
        static Vector3 GameFocus(Vector2 at, float yaw, float pitch)
        {
            var tc = Object.FindFirstObjectByType<TacticalCamera>();
            float baseYaw = !float.IsNaN(CaptureRig.Rig.YawPin) ? CaptureRig.Rig.YawPin : tc != null ? tc.BaseYaw : 0f;
            Vector3 forward = Quaternion.Euler(pitch, baseYaw + yaw, 0f) * Vector3.forward;
            return NightLights.FocusOf(new Vector3(at.x, 0f, at.y) - forward * 100f, forward);
        }

        /// <summary>The nearest point of the water sheet to `from` within `reach`, or NaN.</summary>
        static Vector2 WaterNear(MapData map, Vector2 from, float reach)
        {
            Vector2 best = new Vector2(float.NaN, float.NaN); float bestD = float.MaxValue;
            if (map.WaterLevel <= MapData.NoWater) return best;
            for (float dz = -reach; dz <= reach; dz += 1f)
                for (float dx = -reach; dx <= reach; dx += 1f)
                {
                    float x = from.x + dx, z = from.y + dz, d = dx * dx + dz * dz;
                    if (d >= bestD || x < 0f || z < 0f || x >= map.SizeMeters.x || z >= map.SizeMeters.y) continue;
                    if (RenderGround.Sample(map, x, z) < map.WaterLevel - .04f) { bestD = d; best = new Vector2(x, z); }
                }
            return best;
        }

        static IEnumerator InPlay()
        {
            for (int f = 0; f < 2000 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = Env("TW_STILLS_DIR", Path.Combine(Path.GetTempPath(), "tw-nightlamps"));
            Directory.CreateDirectory(dir);
            var info = new StringBuilder();
            void Say(string s) { info.AppendLine(s); File.WriteAllText(Path.Combine(dir, "info.txt"), info.ToString()); Debug.Log("[nightlamps] " + s); }
            CombatFx.ShowOverlays = false;
            int settle = int.Parse(Env("TW_NIGHT_SETTLE", "1800"), Inv);
            var map = Host.Local.Map;
            var size = map.SizeMeters;
            Say($"map {size.x:0} x {size.y:0} m, stress {Host.StressUnits}, settle {settle}, night {SceneMood.Night}, water level {(map.WaterLevel > MapData.NoWater ? map.WaterLevel.ToString("0.00", Inv) : "none")}");

            Host.TimeScale = 8f;
            float began = Time.realtimeSinceStartup;
            while (Host.Local.World.Tick < settle && Time.realtimeSinceStartup - began < 420f) yield return null;
            Host.TimeScale = 1f;
            Say($"settled at tick {Host.Local.World.Tick} after {Time.realtimeSinceStartup - began:0} s, alive {Host.Local.World.AliveCount}");
            for (int f = 0; f < 30; f++) yield return null;

            Vector2 crowd = CaptureRig.Crowd();
            Vector2 mid = new Vector2(size.x * 0.5f, size.y * 0.5f);
            Say($"crowd at {crowd.x:0.0}, {crowd.y:0.0}");

            // burning machines on open ground by the fight: two killed (wrecks), one alight and alive. Their lights
            // and pools are not the rule's: the stills show them the same before and after
            Vector2 wreckAt = OpenNear(map, Vector2.Lerp(crowd, mid, 0.35f));
            Vector2 wreck2 = OpenNear(map, wreckAt + new Vector2(22f, 9f));
            Vector2 alight = OpenNear(map, wreckAt + new Vector2(-20f, 14f));
            string s0 = TankCapture.Spawn(0, VehicleArchetype.Maw, wreckAt.x, wreckAt.y, 30f);
            string s1 = TankCapture.Spawn(1, VehicleArchetype.Tusk, wreck2.x, wreck2.y, 200f);
            string s2 = TankCapture.Spawn(0, VehicleArchetype.Tusk, alight.x, alight.y, 10f);
            Say($"machines: {s0} at {wreckAt.x:0.0}, {wreckAt.y:0.0} / {s1} at {wreck2.x:0.0}, {wreck2.y:0.0} / {s2} at {alight.x:0.0}, {alight.y:0.0}");
            int slot0 = s0.StartsWith("slot ") ? int.Parse(s0.Substring(5)) : -1, slot1 = s1.StartsWith("slot ") ? int.Parse(s1.Substring(5)) : -1, slot2 = s2.StartsWith("slot ") ? int.Parse(s2.Substring(5)) : -1;
            if (slot0 >= 0) RiderLab.Stop(slot0);
            if (slot1 >= 0) RiderLab.Stop(slot1);
            if (slot2 >= 0) RiderLab.Stop(slot2);
            float t = Time.time;
            while (Time.time - t < 2.5f) yield return null;
            Say(WreckLab.Shell(wreckAt.x, wreckAt.y, 50000f, 4f));
            Say(WreckLab.Shell(wreck2.x, wreck2.y, 50000f, 4f));
            if (slot2 >= 0) Say(TankCapture.Ignite(slot2, 0.62f));
            t = Time.time;
            while (Time.time - t < 8f) yield return null;
            Say($"wreck props: {WreckLab.Wreck(wreckAt.x, wreckAt.y)} / {WreckLab.Wreck(wreck2.x, wreck2.y)}; alight alive {(slot2 >= 0 && Host.Local.World.IsAlive(slot2))}");
            crowd = CaptureRig.Crowd();
            Say($"crowd now at {crowd.x:0.0}, {crowd.y:0.0}, tick {Host.Local.World.Tick}, alive {Host.Local.World.AliveCount}");

            Say(CaptureRig.Hold(120f));
            for (int f = 0; f < 8; f++) yield return null;

            night = Object.FindFirstObjectByType<NightLights>();
            Assert.That(night, Is.Not.Null, "no NightLights");
            lamps = new List<Light>();
            if (night.GetType().GetField("lanterns", Any)?.GetValue(night) is System.Collections.IList raw) foreach (var o in raw) if (o is Light l && l != null) lamps.Add(l);
            Assert.AreEqual(night.FixedLamps, lamps.Count, "the lamps list");
            var kinds = new Dictionary<string, int>(); int water = 0, outNow = 0;
            for (int i = 0; i < lamps.Count; i++)
            {
                string k = Kind(lamps[i].name); kinds.TryGetValue(k, out int n); kinds[k] = n + 1;
                if (night.LampByWater(i)) water++;
                if (night.LampIsOut(i)) outNow++;
            }
            Say($"fixed lamps {lamps.Count}:{CountLine(kinds)}; by water {water}; out {outNow}");
            var wl = new StringBuilder("lamps by water:");
            for (int i = 0; i < lamps.Count; i++) if (night.LampByWater(i)) wl.Append($" {lamps[i].name.Replace(" ", "")}@{night.LampPoints[i].x:0},{night.LampPoints[i].z:0}");
            Say(wl.ToString());

            // ---- where to look
            Vector2 NearestLamp(Vector2 to, System.Func<int, bool> which)
            {
                Vector2 best = new Vector2(float.NaN, float.NaN); float bd = float.MaxValue;
                for (int i = 0; i < lamps.Count; i++)
                {
                    if (night.LampIsOut(i) || !which(i)) continue;
                    float d = (Flat(night.LampPoints[i]) - to).sqrMagnitude;
                    if (d < bd) { bd = d; best = Flat(night.LampPoints[i]); }
                }
                return best;
            }
            Vector2 trenchLamp = NearestLamp(crowd, i => lamps[i].name.StartsWith("Trench lamp"));
            // a bunker with a lamp by it: of the map's bunker cells, the one a lamp hangs nearest
            Vector2 bunkerLamp = new Vector2(float.NaN, float.NaN), bunkerAt = bunkerLamp; float bunkerD = float.MaxValue; int bunkerCells = 0;
            for (int z = 0; z < map.NavLength; z++)
                for (int x = 0; x < map.NavWidth; x++)
                {
                    if (((NavLayer)map.NavLayers[map.NavIndex(x, z)] & NavLayer.Bunker) == 0) continue;
                    bunkerCells++;
                    Vector2 cell = new Vector2((x + .5f) * MapData.NavCellSize, (z + .5f) * MapData.NavCellSize);
                    Vector2 lamp = NearestLamp(cell, i => true);
                    float d = (lamp - cell).sqrMagnitude;
                    if (d < bunkerD) { bunkerD = d; bunkerLamp = lamp; bunkerAt = cell; }
                }
            bool bunker = bunkerCells > 0;
            Vector2 site = NearestLamp(crowd, i => lamps[i].name.StartsWith("Lantern"));
            Say(bunker ? $"bunker cells {bunkerCells}; the lamp nearest one hangs at {bunkerLamp.x:0.0}, {bunkerLamp.y:0.0}, {Mathf.Sqrt(bunkerD):0.0} m from the bunker cell at {bunkerAt.x:0.0}, {bunkerAt.y:0.0}"
                       : $"no bunker cell on this field: the close view is the site lantern (a dugout or shelter's) at {site.x:0.0}, {site.y:0.0}");
            // water: the lamp by water nearest the fight; with none, the lamp nearest any water
            Vector2 waterLamp = NearestLamp(crowd, i => night.LampByWater(i)), waterAt = new Vector2(float.NaN, float.NaN);
            if (float.IsNaN(waterLamp.x))
            {
                float bd = float.MaxValue;
                for (int i = 0; i < lamps.Count; i++)
                {
                    Vector2 w = WaterNear(map, Flat(night.LampPoints[i]), 30f);
                    if (float.IsNaN(w.x)) continue;
                    float d = (w - Flat(night.LampPoints[i])).sqrMagnitude;
                    if (d < bd) { bd = d; waterLamp = Flat(night.LampPoints[i]); waterAt = w; }
                }
                Say(float.IsNaN(waterLamp.x) ? "no water sheet within 30 m of any lamp: the water poses look at the field's middle"
                                             : $"no lamp stands by water; the lamp nearest water is at {waterLamp.x:0.0}, {waterLamp.y:0.0}, the water {Mathf.Sqrt(bd):0.0} m off at {waterAt.x:0.0}, {waterAt.y:0.0}");
                if (float.IsNaN(waterLamp.x)) { waterLamp = mid; waterAt = mid; }
            }
            else
            {
                waterAt = WaterNear(map, waterLamp, 12f);
                if (float.IsNaN(waterAt.x)) waterAt = waterLamp;
                Say($"water lamp at {waterLamp.x:0.0}, {waterLamp.y:0.0}, the water's nearest point at {waterAt.x:0.0}, {waterAt.y:0.0}");
            }
            Vector2 waterMid = Vector2.Lerp(waterLamp, waterAt, 0.5f);
            Vector2 away = (mid - waterMid).sqrMagnitude > 1f ? (mid - waterMid).normalized : new Vector2(0f, 1f);
            // and a view from so far off that eight other lamps are nearer its middle than the lamp by the water: the
            // water rule alone keeps that lamp real there (the rule itself says how far, on the lamps as they hang)
            var isOut = new List<bool>(); for (int i = 0; i < lamps.Count; i++) isOut.Add(night.LampIsOut(i));
            int waterIndex = -1;
            for (int i = 0; i < lamps.Count; i++) if ((Flat(night.LampPoints[i]) - waterLamp).sqrMagnitude < 0.01f) waterIndex = i;
            Vector2 farWater = waterMid + away * 60f; float farBy = 60f;
            for (float d = 24f; d <= 140f && waterIndex >= 0; d += 4f)
            {
                var sim = new RealLampSet(); Vector2 f2 = waterMid + away * d;
                sim.Step(night.LampPoints, isOut, null, new Vector3(f2.x, 0f, f2.y), RealLampSet.DefaultBudget, 0, 0f);
                if (!sim.Chosen(waterIndex)) { farWater = waterMid + away * (d + 6f); farBy = d + 6f; break; }
            }
            Say($"far_water looks {farBy:0} m short of the lamp by the water, at {farWater.x:0.0}, {farWater.y:0.0}");

            var poses = new List<(string name, Vector2 at, float zoom, float yaw, float pitch, bool water)>
            {
                ("play_trench", crowd, 30f, 21f, 25f, false),
                ("wide_trench", crowd, 60f, 21f, 25f, false),
                ("wide_field", Vector2.Lerp(crowd, mid, 0.5f), 90f, 21f, 25f, false),
                ("close_bunker", bunker ? (bunkerD < 12f * 12f ? Vector2.Lerp(bunkerLamp, bunkerAt, 0.5f) : bunkerAt) : site, 18f, 21f, 25f, false),
                ("close_lamp", trenchLamp + new Vector2(1f, 2f), 14f, 21f, 25f, false),
                ("close_water", waterMid, 18f, 21f, 25f, true),
                ("play_water", waterMid + away * 12f, 30f, 21f, 25f, true),
                ("far_water", farWater, 60f, 21f, 25f, true),
                ("play_wreck", wreck2 + new Vector2(3f, -1.2f), 22f, 21f, 25f, false),
            };

            void Variant(string v)
            {
                Knobs.Clear();
                if (v == "today") { Knobs.Set("lights.realLamps", "43"); Knobs.Set("look.poolsByView", "0"); }
                else if (v == "lamps8") Knobs.Set("look.poolsByView", "0");
                else if (v == "nowater") Knobs.Set("lights.waterLamps", "0");
            }

            RenderPipelineManager.beginCameraRendering += OnBegin;
            RenderPipelineManager.endCameraRendering += OnEnd;
            int written = 0, asked = 0;
            // every pose once and thrown away: a machine's light starts when a camera first sees it burn, and the clock
            // is held, so it then burns on; without this it was lit in the later variants and not in the first
            string warm = Path.Combine(dir, "warm.png");
            foreach (var p in poses)
            {
                if (File.Exists(warm)) File.Delete(warm);
                CaptureRig.Shot(warm, p.at.x, p.at.y, p.zoom, p.yaw, p.pitch, 320, 180);
                yield return Drain(warm);
            }
            if (File.Exists(warm)) File.Delete(warm);
            if (File.Exists(Path.ChangeExtension(warm, ".json"))) File.Delete(Path.ChangeExtension(warm, ".json"));
            foreach (string v in new[] { "today", "new", "lamps8", "nowater" })
            {
                Variant(v);
                for (int f = 0; f < 6; f++) yield return null;
                string set = Knobs.ToJson(); int cut = set.IndexOf(",\"read\"", System.StringComparison.Ordinal);
                Say("variant " + v + ": knobs set " + (cut > 7 ? set.Substring(7, cut - 7) : set));
                foreach (var p in poses)
                {
                    if (v == "nowater" && !p.water) continue;
                    string path = Path.Combine(dir, $"{p.name}.{v}.png");
                    if (File.Exists(path)) File.Delete(path);
                    seen = ""; lastPools = -1f; asked++;
                    CaptureRig.Shot(path, p.at.x, p.at.y, p.zoom, p.yaw, p.pitch, 1600, 900);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                    Say($"  {p.name}.{v}: focus {p.at.x:0.0},{p.at.y:0.0} zoom {p.zoom:0}; painted pools {lastPools:0}; {seen}");
                }
            }

            // ---- a pan across lamps at the play view, a still every 0.1 s: the fade. The path is the first of a few
            // that changes the set of real lamps early on (worked out with the rule itself, on the lamps as they hang)
            const int PanStills = 14; const float PanStep = 1.5f, PanDt = 0.1f;
            Vector2 panFrom = crowd, panDir = new Vector2(1f, 0f); int panChange = -1;
            var starts = new[] { crowd, trenchLamp, Vector2.Lerp(crowd, mid, 0.3f), site };
            var dirs = new[] { new Vector2(1f, 0f), new Vector2(-1f, 0f), new Vector2(0f, 1f), new Vector2(0f, -1f), new Vector2(0.7f, 0.7f), new Vector2(-0.7f, 0.7f) };
            foreach (var st in starts)
            {
                foreach (var d in dirs)
                {
                    var sim = new RealLampSet(); string first = null; int change = -1;
                    for (int k = 0; k < PanStills; k++)
                    {
                        Vector2 f2 = st + d * (PanStep * k);
                        if (f2.x < 6f || f2.y < 6f || f2.x > size.x - 6f || f2.y > size.y - 6f) { change = -1; break; }
                        sim.Step(night.LampPoints, isOut, null, GameFocus(f2, 21f, 25f), RealLampSet.DefaultBudget, 0, PanDt);   // the pan's own yaw and pitch (below)
                        var key = new StringBuilder(); for (int i = 0; i < lamps.Count; i++) if (sim.Chosen(i)) key.Append(i).Append(',');
                        if (first == null) first = key.ToString(); else if (change < 0 && key.ToString() != first) change = k;
                    }
                    if (change >= 2 && change <= 5) { panFrom = st; panDir = d; panChange = change; break; }
                }
                if (panChange >= 0) break;
            }
            Say($"pan: from {panFrom.x:0.0},{panFrom.y:0.0} along {panDir.x:0.0},{panDir.y:0.0}, {PanStep} m a still, {PanDt} s a still ({PanStep / PanDt:0} m/s), zoom 30; the rule alone says the set changes at still {panChange}");
            foreach (string v in new[] { "today", "new" })
            {
                Variant(v);
                for (int f = 0; f < 6; f++) yield return null;
                // the first still is a cut (the rig's own), the rest follow it with the fade running: two frames a
                // still in the rig's queue, so half the still's time a frame
                var paths = new List<string>();
                for (int k = 0; k < PanStills; k++)
                {
                    string path = Path.Combine(dir, $"pan_{k:00}.{v}.png");
                    if (File.Exists(path)) File.Delete(path);
                    paths.Add(path);
                }
                Vector2 f0 = panFrom;
                seen = ""; CaptureRig.Shot(paths[0], f0.x, f0.y, 30f, 21f, 25f, 1600, 900);
                yield return Drain(paths[0]);
                Say($"  pan_00.{v}: focus {f0.x:0.0},{f0.y:0.0}; {seen}");
                asked++; if (File.Exists(paths[0])) written++;
                CaptureRig.Rig.LampsFade = true;
                Time.captureDeltaTime = PanDt * 0.5f;
                seenAll.Clear();
                for (int k = 1; k < PanStills; k++)   // queued together: the rig takes them two frames apart
                {
                    Vector2 fk = panFrom + panDir * (PanStep * k);
                    CaptureRig.Shot(paths[k], fk.x, fk.y, 30f, 21f, 25f, 1600, 900);
                }
                yield return Drain(paths[PanStills - 1]);
                Time.captureDeltaTime = 0f;
                CaptureRig.Rig.LampsFade = false;
                for (int k = 1; k < PanStills; k++)
                {
                    Vector2 fk = panFrom + panDir * (PanStep * k);
                    asked++; if (File.Exists(paths[k])) written++;
                    Say($"  pan_{k:00}.{v}: focus {fk.x:0.0},{fk.y:0.0}; {(k - 1 < seenAll.Count ? seenAll[k - 1] : "not seen")}");
                }
                for (int f = 0; f < 4; f++) yield return null;
            }

            // ---- a rough timing: the main camera rendered N times at the end of a frame, the rule off and on in turn
            int timing = int.Parse(Env("TW_NIGHT_TIMING", "40"), Inv);
            if (timing > 0)
            {
                var tc = Object.FindFirstObjectByType<TacticalCamera>();
                var timer = new GameObject("render timer") { hideFlags = HideFlags.DontSave }.AddComponent<RenderTimer>();
                timer.Target = new RenderTexture(1600, 900, 24, RenderTextureFormat.ARGB32);
                foreach (var view in new[] { ("play", crowd, 30f), ("wide", crowd, 60f), ("widest", Vector2.Lerp(crowd, mid, 0.5f), 90f) })
                {
                    tc.FrameFrom(view.Item2, view.Item3, 0f);
                    for (int f = 0; f < 20; f++) yield return null;
                    var ms = new Dictionary<string, List<double>> { { "today", new List<double>() }, { "new", new List<double>() } };
                    string sawToday = "", sawNew = "";
                    for (int round = 0; round < 7; round++)
                        foreach (string v in new[] { "today", "new" })
                        {
                            Variant(v);
                            for (int f = 0; f < 4; f++) yield return null;
                            night.CutLamps();
                            once = true; seen = "";
                            timer.Ms = -1; timer.Count = timing;
                            for (int f = 0; f < 600 && timer.Ms == -1; f++) yield return null;
                            once = false;
                            if (round > 0) ms[v].Add(timer.Ms);   // the first round warms the shaders up
                            if (v == "today") sawToday = seen; else sawNew = seen;
                        }
                    string Line(List<double> l) { l.Sort(); return l.Count == 0 ? "none" : $"median {l[l.Count / 2]:0.00} ms, least {l[0]:0.00}, most {l[l.Count - 1]:0.00} ({l.Count} rounds of {timing} renders)"; }
                    string Short(string s) { int at = s.IndexOf("; real:", System.StringComparison.Ordinal); return at > 0 ? s.Substring(0, at) : s; }
                    Say($"timing {view.Item1} (zoom {view.Item3:0}, 1600 x 900, the camera's render alone, editor in batch mode): today {Line(ms["today"])}; new {Line(ms["new"])}");
                    Say("  today: " + Short(sawToday));
                    Say("  new:   " + Short(sawNew));
                }
                Object.Destroy(timer.Target); Object.Destroy(timer.gameObject);
            }

            RenderPipelineManager.beginCameraRendering -= OnBegin;
            RenderPipelineManager.endCameraRendering -= OnEnd;
            night = null; lamps = null;
            Knobs.Clear();
            Say($"stills written: {written} of {asked}");
            Say(CaptureRig.Release());
            Assert.AreEqual(asked, written, "a still was not written");
            yield return new ExitPlayMode();
        }
    }
}
