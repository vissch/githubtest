// Showcase film director (footage session, NOT for commit). Plays the real game in a Play session, composites the
// camera and the UI Toolkit panels into one RenderTexture per frame at a fixed frame rate (Time.captureFramerate), and
// writes numbered JPGs per segment: <out>/<segment>/00000.jpg, plus <segment>/caption.txt.
//   Footage.Open()              open the MainMenu scene and enter Play (call from edit mode)
//   Footage.Begin("parts=menu out=C:/x w=1920 h=1080 fps=30")   start the runner in Play
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Unity.Mathematics;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Perf;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Editor
{
    public static class Footage
    {
        public const string MenuScene = "Assets/_Project/Scenes/MainMenu.unity";

        public static string Open()
        {
            if (EditorApplication.isPlaying) return "already in Play";
            EditorSceneManager.OpenScene(MenuScene);
            EditorApplication.EnterPlaymode();
            return "entering Play on the menu";
        }

        public static string Begin(string opts)
        {
            if (!EditorApplication.isPlaying) return "not in Play";
            var old = UnityEngine.Object.FindFirstObjectByType<FootageRun>();
            if (old != null && old.State != "finished") return "a run is already going: " + old.State;
            if (old != null) UnityEngine.Object.Destroy(old.gameObject);
            var go = new GameObject("FootageRun");
            UnityEngine.Object.DontDestroyOnLoad(go);
            var run = go.AddComponent<FootageRun>();
            run.Go(opts ?? "");
            return "started " + opts;
        }

        public static string Status()
        {
            var r = UnityEngine.Object.FindFirstObjectByType<FootageRun>();
            return r == null ? "no run" : r.State;
        }

        public static string Stop()
        {
            var r = UnityEngine.Object.FindFirstObjectByType<FootageRun>();
            if (r != null) r.Abort();
            return "stopped";
        }

        /// <summary>One composed frame to a PNG (needs a run object, so start one with parts=probe first).</summary>
        public static string Still(string path)
        {
            var r = UnityEngine.Object.FindFirstObjectByType<FootageRun>();
            return r == null ? "no run" : r.StillTo(path);
        }

        /// <summary>What there is to film, from the live match (units, abilities, mission cards).</summary>
        public static string Discover()
        {
            var sb = new StringBuilder();
            var host = UnityEngine.Object.FindFirstObjectByType<SimHost>();
            if (host != null && host.Local != null)
            {
                sb.AppendLine("UNITS");
                foreach (var e in GymCatalogue.Units(host.Local.World))
                {
                    var r = host.Local.World.Units.Roster[e.Id];
                    sb.AppendLine($"  {e.Id,3} {e.Name,-14} hp {r.Hp,6:0} speed {r.Speed,5:0.0} vehicle {ChassisKind.IsArmoured(host.Local.World.ChassisOf((byte)e.Id))}");
                }
            }
            else sb.AppendLine("(no match running: units unknown)");
            sb.AppendLine("ABILITIES");
            foreach (var e in GymCatalogue.Abilities()) sb.AppendLine($"  {e.Id,3} {e.Name} {e.Expect}");
            sb.AppendLine("MISSION CARDS");
            foreach (var c in Resources.FindObjectsOfTypeAll<MissionCard>()) sb.AppendLine($"  {c.name}: {c.Title} ground {c.Ground}");
            return sb.ToString();
        }
    }

    [DefaultExecutionOrder(32000)]
    public sealed class FootageRun : MonoBehaviour
    {
        public string State = "idle";
        string outDir = "C:/Users/PC/Documents/footage";
        int W = 1920, H = 1080, Fps = 30;
        bool rec; int frame; string segDir; string segName = "";
        readonly Dictionary<PanelSettings, RenderTexture> panels = new Dictionary<PanelSettings, RenderTexture>();
        RenderTexture rtScene, rtOut; Texture2D readTex; Material blend;
        Coroutine routine; bool aborted;
        string logPath;

        void Log(string s)
        {
            State = s;
            try { File.AppendAllText(logPath ?? (outDir + "/footage.log"), DateTime.Now.ToString("HH:mm:ss ") + s + "\n"); } catch { }
        }

        public void Go(string opts)
        {
            string P(string k, string d) { var m = System.Text.RegularExpressions.Regex.Match(" " + opts + " ", @"\s" + k + @"=(\S+)"); return m.Success ? m.Groups[1].Value : d; }
            outDir = P("out", outDir).Replace('\\', '/'); W = int.Parse(P("w", "1920")); H = int.Parse(P("h", "1080")); Fps = int.Parse(P("fps", "30"));
            Directory.CreateDirectory(outDir); logPath = outDir + "/footage.log";
            rtScene = new RenderTexture(W, H, 24, RenderTextureFormat.ARGB32) { name = "Footage scene" };
            rtOut = new RenderTexture(W, H, 0, RenderTextureFormat.ARGB32) { name = "Footage out" };
            readTex = new Texture2D(W, H, TextureFormat.RGB24, false);
            blend = new Material(Shader.Find("Sprites/Default"));
            Time.captureFramerate = Fps;
            var parts = P("parts", "probe").Split(',');
            routine = StartCoroutine(RunParts(parts, opts));
        }

        public void Abort() { aborted = true; if (routine != null) StopCoroutine(routine); Finish(); }

        void Finish()
        {
            Time.captureFramerate = 0; rec = false;
            foreach (var kv in panels)
            {
                if (kv.Key == null) continue;
                kv.Key.targetTexture = null;
                if (orig.TryGetValue(kv.Key, out var o)) { kv.Key.scaleMode = o.Item1; kv.Key.scale = o.Item2; }
            }
            panels.Clear(); orig.Clear();
            Log("finished");
        }

        IEnumerator RunParts(string[] parts, string opts)
        {
            foreach (var part in parts)
            {
                Log("part " + part);
                IEnumerator it = null;
                switch (part)
                {
                    case "probe": it = PartProbe(); break;
                    case "layout": it = PartLayout(); break;
                    case "menu": it = PartMenu(); break;
                    case "normal": it = PartBattle(false, opts); break;
                    case "snow": it = PartBattle(true, opts); break;
                    case "units": it = PartUnits(opts); break;
                    case "abilities": it = PartAbilities(opts); break;
                    case "deaths": it = PartDeaths(opts); break;
                    case "landing": it = PartLanding(opts); break;
                    case "beach": it = PartBeach(opts); break;
                    case "beachzoom": it = PartBeachZoom(opts); break;
                    case "trenchsea": it = PartTrenchSea(opts); break;
                    case "assault": it = PartAssault(opts); break;
                    default: Log("unknown part " + part); continue;
                }
                yield return RunSafe(it, part);
                if (aborted) yield break;
            }
            Finish();
        }

        IEnumerator RunSafe(IEnumerator it, string part)
        {
            while (true)
            {
                object cur;
                try { if (!it.MoveNext()) yield break; cur = it.Current; }
                catch (Exception e) { Log("PART " + part + " FAILED: " + e.GetType().Name + " " + e.Message + "\n" + e.StackTrace); rec = false; yield break; }
                yield return cur;
            }
        }

        // ------------------------------------------------------------------------------------------ frames
        bool ui = true;
        void Seg(string name, string caption = "", bool withUi = true)
        {
            ui = withUi; segName = name; segDir = outDir + "/" + name; frame = 0;
            if (Directory.Exists(segDir)) Directory.Delete(segDir, true);
            Directory.CreateDirectory(segDir);
            if (!string.IsNullOrEmpty(caption)) File.WriteAllText(segDir + "/caption.txt", caption);
            Log("segment " + name);
        }

        // The figures and props queue their draws in their own LateUpdate, so a frame is only complete after all of them:
        // the grab happens in this component's LateUpdate (execution order 32000), not in the coroutine's Update.
        bool pending; string pendingPath;
        IEnumerator Tick()
        {
            if (rec && segDir != null) { pendingPath = segDir + "/" + frame.ToString("00000") + ".jpg"; frame++; pending = true; }
            yield return null;     // WaitForEndOfFrame never fires in a batch editor: pump per player-loop frame
        }

        void LateUpdate()
        {
            var hudc = FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            if (hudc != null && hudc.Dialogue != null) hudc.Dialogue.Clear();     // no tutorial pop-ups in the film
            if (!pending) return;
            pending = false;
            try { Grab(pendingPath); } catch (Exception e) { Log("GRAB FAILED " + e.Message); }
        }

        /// <summary>Hold for `seconds` of game time, one recorded frame per step; update(t 0..1) runs before each.</summary>
        IEnumerator Hold(float seconds, Action<float> update = null)
        {
            int n = Mathf.Max(1, Mathf.RoundToInt(seconds * Fps));
            rec = true;
            for (int i = 0; i < n; i++)
            {
                if (update != null) update(n == 1 ? 1f : i / (float)(n - 1));
                yield return Tick();
            }
            rec = false;
        }

        /// <summary>Unrecorded frames with the same camera pose each frame: the camera eases to its target over seconds.</summary>
        IEnumerator Settle(float seconds, Action pose)
        {
            rec = false;
            int n = Mathf.RoundToInt(seconds * Fps);
            pose();     // once: FrameFrom resets the camera's yaw/pitch state, so calling it every frame never settles
            for (int i = 0; i < n; i++) yield return Tick();
        }

        // Background frog infantry: spawned with no speed and a great deal of HP (they neither wander, clump nor die under the
        // enemy fire) and a clean idle clip pinned, so they stand the way a garrison stands.
        readonly List<(int slot, ushort gen)> still = new List<(int, ushort)>();
        static readonly int[] Garrison = { 0, 0, 1, 0, 2, 0, 1 };   // rifle, assault, MG: the line infantry
        static readonly Clip[] IdleClips = { Clip.Idle, Clip.AimedIdle, Clip.ReadyIdle, Clip.FidgetLookAround, Clip.Idle, Clip.AimedIdle };

        void ClearStill(SimHost host)
        {
            if (still.Count == 0 || host == null || host.Local == null) { still.Clear(); return; }
            var gone = still.ToArray(); still.Clear();
            host.Animation?.UnpinAll();
            host.WriteWorlds(m =>
            {
                var w = m.World;
                foreach (var (slot, gen) in gone)
                    if (slot < w.HighWater && w.Generation[slot] == gen && (w.Flags[slot] & (uint)UnitFlags.Alive) != 0) w.Despawn(slot);
            });
        }

        /// <summary>A real sim unit held where it stands (its goal is its own cell, as the gym holds a unit) with HP too great to
        /// lose: it idles, aims, fires and reloads on the sim's own animation, and the fight never runs out of men.</summary>
        int SpawnHeld(GymDirector dir, SimHost host, int team, int arch, float x, float z)
        {
            int first = -1;
            bool ok = host.WriteWorlds(m =>
            {
                var w = m.World;
                var e = w.Units.Roster[arch]; if (e.Hp <= 0f) e = w.Roster[team * RosterEntry.SlotCount];
                bool veh = ChassisKind.IsArmoured(w.ChassisOf((byte)arch));
                int sl = w.Spawn((byte)team, (byte)arch, new Unity.Mathematics.float3(x, 0f, z), 1000000f, e.Speed, veh);
                if (sl >= 0) w.Yaw[sl] = (team == 0 ? 0f : 180f) * Mathf.Deg2Rad;
                if (first < 0) first = sl;
            });
            if (!ok || first < 0) return -1;
            still.Add((first, host.Local.World.Generation[first]));
            dir.Hold(first);
            return first;
        }

        void SpawnStill(GymDirector dir, SimHost host, float x, float z, int n) => SpawnHeld(dir, host, 0, Garrison[n % Garrison.Length], x, z);

        /// <summary>Two ranks behind the stage and a small group on each flank, 3.4 m apart so none stand in another.</summary>
        /// <summary>The camera looks along -x, so the garrison stands BEHIND the hero (smaller x) in ranks across the view, never between.</summary>
        void Frogs(GymDirector dir, Vector2 at, int perRow = 6)
        {
            var host = Host; int n = 0;
            for (int r = 0; r < 3; r++)
                for (int k = 0; k < perRow; k++)
                    SpawnStill(dir, host, at.x - 9f - r * 4.5f, at.y + 3f + (k - (perRow - 1) * 0.5f) * 3.6f + (r % 2) * 1.8f, n++);
            for (int k = 0; k < 4; k++) { SpawnStill(dir, host, at.x - 2f - k * 3.4f, at.y - 11f, n++); SpawnStill(dir, host, at.x - 2f - k * 3.4f, at.y + 17f, n++); }
        }

        /// <summary>CaptureRig's pose: the shipped camera component is switched off and Camera.main is placed by hand.</summary>
        void PoseCam(Vector2 focus, float zoom, float yaw, float pitch, float thirds = 0f)
        {
            var tc = Cam; var cam = Camera.main; var host = Host;
            if (tc == null || cam == null) return;
            tc.enabled = false; poseOff = tc;
            zoom = Mathf.Clamp(zoom, tc.NearZoom, tc.ZoomMax);
            SceneHooks.CloseUp = tc.CloseUpAt(zoom);
            Shader.SetGlobalFloat("_TWClose", SceneHooks.CloseUp);
            float close = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(tc.NearZoom, tc.CloseZoom, zoom));
            float fov = Mathf.Lerp(tc.Fov, tc.CloseFov, close);
            cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
            var rot = Quaternion.Euler(Mathf.Clamp(pitch, tc.PitchMin, tc.PitchMax), tc.BaseYaw + yaw, 0f);
            float distance = zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
            float ground = host != null && host.Local != null ? host.Local.Map.Height.Sample(focus.x, focus.y) : 0f;
            if (thirds != 0f)
            {
                // look past the hero so he stands on a third: shift the aim sideways by a sixth of the view's width at his distance
                float psi = (tc.BaseYaw + yaw) * Mathf.Deg2Rad;
                float d = thirds * 0.59f * distance * Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
                focus += new Vector2(Mathf.Cos(psi), -Mathf.Sin(psi)) * d;
            }
            var aim = new Vector3(focus.x, ground * close + tc.CloseAimHeight * close, focus.y);
            cam.transform.SetPositionAndRotation(aim - rot * Vector3.forward * distance, rot);
        }
        TacticalCamera poseOff;

        /// <summary>Take every kit prop (wrecks, stakes, trees) out of the draw around the hero and along the camera's line to him.</summary>
        void ClearProps(Vector2 focus, float radius, float corridor)
        {
            var bp = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>(); var cam = Camera.main;
            if (bp == null || cam == null) return;
            var fi = typeof(TW.Presentation.Terrain.BattlefieldProps).GetField("batches", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            var dict = fi != null ? fi.GetValue(bp) as System.Collections.IDictionary : null;
            if (dict == null) { Log("ClearProps: no batches"); return; }
            var mods = new List<TW.Presentation.Terrain.BattlefieldKit.Module>();
            foreach (var k in dict.Keys) if (k is TW.Presentation.Terrain.BattlefieldKit.Module mm) mods.Add(mm);
            var from = new Vector2(cam.transform.position.x, cam.transform.position.z);
            var found = new List<(int page, int slot, Matrix4x4 m)>(); int hidden = 0;
            foreach (var mm in mods)
                for (int i = 0; i <= 10; i++)
                {
                    found.Clear(); bp.Within(mm, Vector2.Lerp(from, focus, i / 10f), i == 10 ? radius : corridor, found);
                    foreach (var f in found) { bp.Hide(mm, f.page, f.slot); hidden++; }
                    if (i == 10) { found.Clear(); bp.Within(mm, focus, radius, found); foreach (var f in found) { bp.Hide(mm, f.page, f.slot); hidden++; } }
                }
            Log("props hidden: " + hidden);
        }

        /// <summary>A firefight: two ranks a side, held and unkillable, a machine gun in every fifth place, centred on c.</summary>
        void Armies(GymDirector dir, SimHost host, Vector2 c, int perRank = 18, bool mortalEnemy = false)
        {
            float x0 = host.Local.Map.SizeMeters.x * 0.5f;
            for (int team = 0; team < 2; team++)
                for (int r = 0; r < 2; r++)
                    for (int k = 0; k < perRank; k++)
                    {
                        float ax = x0 + (k - (perRank - 1) * 0.5f) * 3.2f + r * 1.6f, az = c.y + (team == 0 ? -(13f + r * 4f) : 13f + r * 4f);
                        if (team == 1 && mortalEnemy) { int sl = dir.Spawn(1, k % 5 == 2 ? 2 : 0, ax, az, 180f); if (sl >= 0) dir.Hold(sl); }
                        else SpawnHeld(dir, host, team, k % 5 == 2 ? 2 : 0, ax, az);
                    }
        }

        readonly List<Vector2> usedStages = new List<Vector2>();
        /// <summary>The clearest ground on our half of the map: the point whose stage, middle and enemy line are farthest from every wreck prop.</summary>
        Vector2 CleanStage(SimHost host, float zMax = 64f)
        {
            var map = host.Local.Map; var wrecks = new List<Vector2>();
            for (int i = 0; i < map.Props.Length; i++)
            {
                string k = map.Props[i].Kind.ToString();
                if (k.Contains("Wreck") || k.Contains("Scrap")) wrecks.Add(new Vector2(map.Props[i].Pos.x, map.Props[i].Pos.z));
            }
            // z above about 85 is the yellow river bank and the sea shore: the dark battlefield (and the camera's view beyond
            // the hero) stays in the band below it
            var size = map.SizeMeters; Vector2 best = new Vector2(size.x * 0.5f, 40f); float bestScore = float.MinValue;
            for (float x = 24f; x <= size.x - 22f; x += 16f)
                for (float z = 28f; z <= zMax; z += 12f)
                {
                    float score = 1000f;
                    foreach (var w in wrecks)
                        foreach (float dz in new[] { 0f, 20f, 44f }) score = Mathf.Min(score, Vector2.Distance(w, new Vector2(x, z + dz)));
                    foreach (var u in usedStages) if (Vector2.Distance(u, new Vector2(x, z)) < 22f) score -= 500f;     // despawned machines leave wrecks drawn for a while
                    if (score > bestScore) { bestScore = score; best = new Vector2(x, z); }
                }
            usedStages.Add(best); if (usedStages.Count > 8) usedStages.RemoveAt(0);
            Log($"clean stage {best} score {bestScore:0} ({wrecks.Count} wrecks)");
            return best;
        }

        /// <summary>Call the ability, then run the clock (unrecorded) until just before the first shell, line or canopy: the zone
        /// the game draws until then is a telegraph, and the take should open on the effect.</summary>
        IEnumerator FireAndWait(GymDirector dir, SimHost host, OffMapAbilityId id, Vector2 at, int pattern, Action pose)
        {
            rec = false; bool fired = false;
            for (int i = 0; i < Mathf.RoundToInt(2f * Fps) && !fired; i++) { pose(); fired = dir.Ability(id, at, 0, pattern, -1); yield return Tick(); }
            float warm = TW.Sim.Match.OffMapAbilitySystem.TryGetStats((int)id, out var st) ? st.WarmupTicks / 20f : 4f;
            int frames = Mathf.RoundToInt(Mathf.Max(0f, warm - 0.9f) * Fps);
            for (int i = 0; i < frames; i++) { pose(); yield return Tick(); }
            Log($"{id}: fired {fired}, waited {frames / (float)Fps:0.0} s of {warm:0.0} s warm-up");
        }

        static string Pretty(string camel) => System.Text.RegularExpressions.Regex.Replace(camel, "(?<!^)([A-Z])", " $1").ToUpperInvariant();
        void PoseRelease() { if (poseOff != null) poseOff.enabled = true; poseOff = null; }

        IEnumerator WaitFor(Func<bool> cond, float seconds, string what)
        {
            rec = false;
            int n = Mathf.RoundToInt(seconds * Fps);
            for (int i = 0; i < n; i++) { if (cond()) yield break; yield return Tick(); }
            Log("TIMEOUT waiting for " + what);
        }

        void EnsurePanels()
        {
            foreach (var d in FindObjectsByType<UIDocument>(FindObjectsInactive.Exclude, FindObjectsSortMode.None))
            {
                var ps = d.panelSettings;
                if (ps == null || panels.ContainsKey(ps)) continue;
                var rt = new RenderTexture(1600, 900, 24, RenderTextureFormat.ARGB32) { name = "Footage UI " + ps.name };
                // a panel drawn into a texture lays itself out in the texture's raw pixels and then still applies its scale, so the
                // UI only fills `scale x texture`: draw it 1:1 into a 1600x900 texture (the reference layout) and stretch that to the frame
                orig[ps] = (ps.scaleMode, ps.scale);
                ps.scaleMode = PanelScaleMode.ConstantPixelSize; ps.scale = 1f;
                panels[ps] = rt; ps.targetTexture = rt; ps.clearColor = true; ps.colorClearValue = new Color(0f, 0f, 0f, 0f);
            }
        }
        readonly Dictionary<PanelSettings, (PanelScaleMode, float)> orig = new Dictionary<PanelSettings, (PanelScaleMode, float)>();

        void Grab(string path)
        {
            var cam = Camera.main; if (cam == null) return;
            EnsurePanels();
            var prev = cam.targetTexture; cam.targetTexture = rtScene; cam.Render(); cam.targetTexture = prev;
            Graphics.Blit(rtScene, rtOut);
            foreach (var kv in panels.OrderBy(k => k.Key != null ? k.Key.sortingOrder : 0))
            {
                if (kv.Key == null || !ui) continue;
                blend.color = new Color(1f, 1f, 1f, uiAlpha);     // the HUD fades in after the cut from the animatic
                Graphics.Blit(kv.Value, rtOut, blend);
            }
            var act = RenderTexture.active; RenderTexture.active = rtOut;
            readTex.ReadPixels(new Rect(0, 0, W, H), 0, 0); readTex.Apply(false);
            RenderTexture.active = act;
            File.WriteAllBytes(path, path.EndsWith(".png") ? readTex.EncodeToPNG() : readTex.EncodeToJPG(93));
        }

        public string StillTo(string path)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            Grab(path);
            foreach (var kv in panels)
            {
                var rt = kv.Value; var was = RenderTexture.active; RenderTexture.active = rt;
                var t = new Texture2D(rt.width, rt.height, TextureFormat.RGBA32, false);
                t.ReadPixels(new Rect(0, 0, rt.width, rt.height), 0, 0); t.Apply(false);
                RenderTexture.active = was;
                File.WriteAllBytes(path + ".ui_" + kv.Key.name + ".png", t.EncodeToPNG()); Destroy(t);
            }
            return "wrote " + path;
        }

        // ------------------------------------------------------------------------------------------ UI helpers
        static VisualElement Q(string name)
        {
            foreach (var d in FindObjectsByType<UIDocument>(FindObjectsInactive.Exclude, FindObjectsSortMode.None))
            {
                var root = d.rootVisualElement; if (root == null) continue;
                var e = root.Q(name); if (e != null && e.resolvedStyle.display != DisplayStyle.None) return e;
            }
            return null;
        }

        bool Click(string name) { var e = Q(name); return e != null && Click(e); }

        bool Click(VisualElement e)
        {
            if (e == null) return false;
            using (var ev = NavigationSubmitEvent.GetPooled()) { ev.target = e; e.SendEvent(ev); }
            Log("click " + (string.IsNullOrEmpty(e.name) ? e.GetType().Name : e.name));
            return true;
        }

        static ShellRouter Router => ShellRouter.Instance;
        static SimHost Host => FindFirstObjectByType<SimHost>();
        static TacticalCamera Cam => FindFirstObjectByType<TacticalCamera>();

        // ------------------------------------------------------------------------------------------ parts
        IEnumerator PartProbe()
        {
            Seg("probe");
            yield return Hold(0.2f);
            Log("probe frames: " + frame);
        }

        IEnumerator PartLayout()
        {
            Seg("layout");
            yield return Hold(0.4f);
            LogLayout();
            Click("btn-skirmish"); yield return Hold(2f);
            LogLayout();
            StillTo(outDir + "/layout_mission.png");
        }

        void LogLayout()
        {
            foreach (var d in FindObjectsByType<UIDocument>(FindObjectsInactive.Exclude, FindObjectsSortMode.None))
            {
                var ps = d.panelSettings; var root = d.rootVisualElement; if (ps == null || root == null || root.panel == null) continue;
                var vt = root.panel.visualTree;
                string Rc(string n) { var e = root.Q(n); return e == null ? n + "=none" : n + "=" + e.worldBound; }
                Log($"LAYOUT doc {d.name} mode {ps.scaleMode} scale {ps.scale} rt {(ps.targetTexture != null ? ps.targetTexture.width + "x" + ps.targetTexture.height : "none")} tree {vt.layout} root {root.worldBound} {Rc("menu-screen")} {Rc("info-panel")} {Rc("btn-quit")} {Rc("mission-screen")} {Rc("mission-list")} {Rc("btn-deploy")} {Rc("info-title")}");
            }
        }

        IEnumerator PartMenu()
        {
            yield return WaitFor(() => Q("btn-skirmish") != null, 20, "main menu");
            Seg("menu_1_title", "MAIN MENU");
            yield return Hold(7f);

            Seg("menu_2_missions", "SKIRMISH  -  MISSION SELECT");
            Click("btn-skirmish"); yield return Hold(2.5f);
            var list = Q("mission-list");
            var cards = list != null ? list.Query<Button>().ToList() : new List<Button>();
            Log("mission cards: " + cards.Count);
            for (int i = 1; i < Mathf.Min(cards.Count, 5); i++) { Click(cards[i]); yield return Hold(2.4f); }
            var tabs = Q("difficulty-tabs");
            var diffs = tabs != null ? tabs.Query<Button>().ToList() : new List<Button>();
            for (int i = 0; i < Mathf.Min(diffs.Count, 3); i++) { Click(diffs[i]); yield return Hold(1.2f); }
            if (cards.Count > 0) Click(cards[0]);
            yield return Hold(1.5f);
            Router?.Pop(); yield return Hold(1.2f);

            Seg("menu_3_armoury", "ARMOURY");
            Click("btn-armoury"); yield return Hold(2.2f);
            {
                var btns = new List<Button>();
                foreach (var d in FindObjectsByType<UIDocument>(FindObjectsInactive.Exclude, FindObjectsSortMode.None))
                    if (d.rootVisualElement != null) d.rootVisualElement.Query<Button>().ForEach(b =>
                    {
                        if ((b.name ?? "").StartsWith("btn-") || b.resolvedStyle.display == DisplayStyle.None) return;
                        for (var a = b.parent; a != null; a = a.parent) if (a.name == "menu-screen") return;     // the main menu under the armoury
                        btns.Add(b);
                    });
                Log("armoury buttons: " + btns.Count);
                foreach (int pick in new[] { 4, 9, 13, 2 }) if (pick < btns.Count) { Click(btns[pick]); yield return Hold(1.2f); }
            }
            Router?.PopTo<MainMenuScreen>(); yield return Hold(1f);

            Seg("menu_4_settings", "SETTINGS");
            Click("btn-settings"); yield return Hold(5f);
            Router?.Pop(); yield return Hold(1f);

            Seg("menu_5_campaign", "CAMPAIGN  -  STRATEGIC MAP");
            Click("btn-campaign"); yield return Hold(7f);
            Router?.Pop(); yield return Hold(1f);

            Seg("menu_6_homefront", "HOME FRONT");
            Click("btn-homefront"); yield return Hold(6f);
            Router?.PopTo<MainMenuScreen>(); yield return Hold(1.5f);
        }

        /// <summary>From the menu: Skirmish, pick the first card of that ground, Deploy; wait for the match and its HUD.</summary>
        IEnumerator StartMatch(bool snow, bool fresh = false)
        {
            rec = false; PoseRelease();
            var want0 = snow ? Ground.WinterLine : Ground.ShelledForest;
            if (Host != null && Host.Local != null && (Host.Ground != want0 || fresh))
            {
                Log("leaving the current match for the " + want0);
                Router?.QuitToMenu();
                yield return WaitFor(() => Host == null && Q("btn-skirmish") != null, 30, "menu");
            }
            if (Host == null || Host.Local == null)
            {
                Router?.PopTo<MainMenuScreen>();
                if (Q("btn-skirmish") == null) { Router?.QuitToMenu(); yield return WaitFor(() => Q("btn-skirmish") != null, 20, "menu"); }
                Click("btn-skirmish"); yield return WaitFor(() => Q("mission-list") != null, 5, "mission list");
                yield return Tick(); yield return Tick();
                var list = Q("mission-list").Query<Button>().ToList();
                int pick = 0;
                var want = snow ? Ground.WinterLine : Ground.ShelledForest;
                var ordered = MissionOrder();
                for (int i = 0; i < ordered.Count && i < list.Count; i++) if (ordered[i].Ground == want) { pick = i; break; }
                Log("picking card " + pick + (pick < ordered.Count ? " " + ordered[pick].Title : ""));
                Click(list[pick]); yield return Tick(); yield return Tick();
                Click("btn-deploy");
            }
            yield return WaitFor(() => Host != null && Host.Local != null && Host.Local.World.Tick > 40 && FindFirstObjectByType<HudController>() != null, 40, "match");
            Log("match up, tick " + (Host != null && Host.Local != null ? Host.Local.World.Tick : 0));
        }

        static List<MissionCard> MissionOrder()
        {
            var cat = Resources.FindObjectsOfTypeAll<MissionCatalog>();
            if (cat.Length > 0 && cat[0].Cards != null) return cat[0].Cards.Where(c => c != null).ToList();
            return Resources.FindObjectsOfTypeAll<MissionCard>().ToList();
        }

        IEnumerator PartBattle(bool snow, string opts)
        {
            string tag = snow ? "snow" : "normal";
            yield return StartMatch(snow);
            var host = Host; var hud = FindFirstObjectByType<HudController>(); var dir = GymDirector.Attach(host);
            host.WriteWorlds(m => { m.World.Silver[0] = 4000; });
            // let the enemy's deploys and the front fill up before anything is filmed
            float was = host.TimeScale; host.TimeScale = 4f;
            yield return Settle(10f, () => { });
            host.TimeScale = was;
            rec = false;
            Armies(dir, host, FightCentre(host), 14);
            yield return Settle(3f, () => { });
            var f = FightCentre(host);

            Seg(tag + "_1_open", snow ? "THE FROZEN LINE  -  SNOW" : "SHELLED FOREST  -  NIGHT", !snow);
            yield return Hold(7f, t => { f = Vector2.Lerp(f, FightCentre(host), 0.04f); PoseCam(f, Mathf.Lerp(48f, 38f, t), Mathf.Lerp(-12f, 8f, t), 42f); });

            Seg(tag + "_2_deploy", snow ? "REINFORCEMENTS ARRIVE" : "DEPLOYING FROM THE HUD", !snow);
            int slots = RosterEntry.SlotCount;
            for (int sl = 0; sl < slots; sl++)
            {
                hud.Deploy(sl);
                int slc = sl;
                yield return Hold(1.6f, t => { f = Vector2.Lerp(f, FightCentre(host), 0.04f); float u = (slc + t) / slots; PoseCam(f, Mathf.Lerp(40f, 30f, u), Mathf.Lerp(10f, -14f, u), 36f); });
            }

            Seg(tag + "_3_advance", "OVER THE TOP", !snow);
            hud.OrderFront(CommandType.TrenchAdvance);
            yield return Hold(7f, t => { f = Vector2.Lerp(f, FightCentre(host), 0.05f); PoseCam(f, Mathf.Lerp(36f, 26f, t), Mathf.Lerp(-20f, 10f, t), Mathf.Lerp(30f, 22f, t)); });

            rec = false;
            var fc = FightCentre(host);
            Armies(dir, host, fc, 18, true);
            float xmid = host.Local.Map.SizeMeters.x * 0.5f;
            yield return FireAndWait(dir, host, snow ? OffMapAbilityId.CreepingBarrage : OffMapAbilityId.HeBarrage, new Vector2(xmid, fc.y + 16f), 0, () => { });
            Seg(tag + "_4_close", "THE FIGHT, UP CLOSE", false);
            yield return FollowFight(8f, 20f, 30f, 19f);
            Seg(tag + "_5_wide", "THE LINE", false);
            yield return FollowFight(5f, 50f, 20f, 52f);
            ClearStill(host); dir.Clear(); PoseRelease();
        }

        // camera on the middle of the live fighting (centroid of living men), eased
        IEnumerator FollowFight(float seconds, float zoom, float yaw, float pitch, Action<float> extra = null)
        {
            var cam = Cam; var host = Host;
            var f = FightCentre(host);
            yield return Settle(1f, () => PoseCam(f, zoom, yaw, pitch));
            yield return Hold(seconds, t =>
            {
                f = Vector2.Lerp(f, FightCentre(host), 0.05f);
                PoseCam(f, Mathf.Lerp(zoom, zoom * 0.85f, t), yaw + Mathf.Lerp(-12f, 12f, t), pitch);
                if (extra != null) extra(t);
            });
            PoseRelease();
        }

        static Vector2 FightCentre(SimHost host)
        {
            var w = host.Local.World; var xs = new List<float>(); var zs = new List<float>(); var idx = new List<int>();
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                xs.Add(w.Position[i].x); zs.Add(w.Position[i].z); idx.Add(i);
            }
            if (idx.Count == 0) { var s = host.Local.Map.SizeMeters; return new Vector2(s.x * 0.5f, s.y * 0.5f); }
            var sx = new List<float>(xs); var sz = new List<float>(zs); sx.Sort(); sz.Sort();
            var med = new Vector2(sx[sx.Count / 2], sz[sz.Count / 2]);
            // the median can fall between two groups: take the nearest living unit's place, so a man is always at the centre
            float best = float.MaxValue; var at = med;
            for (int k = 0; k < idx.Count; k++) { float d = (xs[k] - med.x) * (xs[k] - med.x) + (zs[k] - med.y) * (zs[k] - med.y); if (d < best) { best = d; at = new Vector2(xs[k], zs[k]); } }
            return at;
        }

        /// <summary>Four camera recipes so no two neighbouring shots share a framing: low dolly push, orbit, high crane down, low side track.</summary>
        static void Recipe(int kind, float t, float z, out float zoom, out float yaw, out float pitch)
        {
            switch (kind & 3)
            {
                case 0: zoom = Mathf.Lerp(z * 1.0f, z * 0.7f, t); yaw = Mathf.Lerp(-35f, -12f, t); pitch = Mathf.Lerp(15f, 20f, t); break;     // low dolly push-in
                case 1: zoom = z * 0.85f; yaw = Mathf.Lerp(-55f, 25f, t); pitch = 30f; break;                                                    // orbit
                case 2: zoom = Mathf.Lerp(z * 1.6f, z * 0.9f, t); yaw = Mathf.Lerp(-15f, 12f, t); pitch = Mathf.Lerp(55f, 28f, t); break;       // crane down
                default: zoom = Mathf.Lerp(z * 0.95f, z * 0.75f, t); yaw = Mathf.Lerp(45f, 18f, t); pitch = 16f; break;                         // low side track
            }
        }

        /// <summary>The enemy's line: two ranks, held, unkillable, so the shot always has a firefight in it.</summary>
        void EnemyLine(GymDirector dir, SimHost host, Vector2 at, float dist, int perRank, bool killable)
        {
            for (int r = 0; r < 2; r++)
                for (int k = 0; k < perRank; k++)
                {
                    float x = at.x + (k - (perRank - 1) * 0.5f) * 3.6f + r * 1.8f, z = at.y + dist + r * 5f;
                    if (killable) { var sl = dir.Row(1, 0, 1, new Vector2(x, z), 1f); foreach (var q in sl) dir.Hold(q); }
                    else SpawnHeld(dir, host, 1, 0, x, z);
                }
        }

        IEnumerator PartUnits(string opts)
        {
            yield return StartMatch(false, true);
            var host = Host; var dir = GymDirector.Attach(host);
            dir.Quiet(); yield return Tick();
            var units = GymCatalogue.Units(host.Local.World);
            int idx = 0;
            // stand-ins that wear another unit's model, and units whose look repeats a neighbour's
            var skip = new HashSet<int> { 29, 30, 31, 32, 33, 34, 13, 14, 16, 24, 26, 27, 28 };
            foreach (var u in units)
            {
                if (skip.Contains(u.Id)) continue;
                bool vehicle = ChassisKind.IsArmoured(host.Local.World.ChassisOf((byte)u.Id));
                int count = vehicle ? 1 : 3;
                ClearStill(host); dir.Clear(); rec = false;
                var at = CleanStage(host);
                float z = vehicle ? 24f : 15f;
                var front = new Vector2(at.x, at.y + 3f);
                var mine = new List<int>();
                for (int k = 0; k < count; k++) { int sl = SpawnHeld(dir, host, 0, u.Id, front.x, front.y + (k - (count - 1) * 0.5f) * (vehicle ? 8f : 3.2f)); if (sl >= 0) mine.Add(sl); }
                EnemyLine(dir, host, at, 44f, 8, false);
                Frogs(dir, at, 8);
                if (mine.Count == 0) { Log("no spawn for " + u.Name); continue; }
                Vector2 Where() { var pp = host.Local.World.Position[mine[0]]; return new Vector2(pp.x, pp.z + 2f); }
                int kind = idx; float side = (idx % 2 == 0) ? 1f : -1f;
                yield return Settle(1.4f, () => { });
                Vector2 focus = Where();
                Recipe(kind, 0f, z, out float z0, out float y0, out float p0);
                yield return Settle(0.6f, () => PoseCam(focus, z0, y0, p0, side));
                ClearProps(focus, 18f, 7f);
                yield return Settle(0.1f, () => { });
                Seg($"unit_{idx++:00}_{u.Name}", u.Name.ToUpperInvariant(), false);
                yield return Hold(vehicle ? 4.2f : 3.2f, t =>
                {
                    if (host.Local.World.IsAlive(mine[0])) focus = Vector2.Lerp(focus, Where(), 0.15f);
                    Recipe(kind, t, z, out float zz, out float yy, out float pp);
                    PoseCam(focus, zz, yy, pp, side);
                });
            }
            ClearStill(host); dir.Clear(); dir.Unquiet(); PoseRelease();
        }

        /// <summary>The sea landing: reinforcements deployed on the coast mission ride landing craft in from the sea beyond the enemy line.</summary>
        IEnumerator PartLanding(string opts)
        {
            yield return StartMatch(false, true);
            var host = Host; var hud = FindFirstObjectByType<HudController>();
            host.WriteWorlds(m => { m.World.Silver[0] = 6000; });
            var map = host.Local.Map; float shore = map.ShoreZ, x0 = map.SizeMeters.x * 0.5f;
            Log($"shore z {shore:0.0}, map {map.SizeMeters}, sea {map.SeaSide}");
            float was = host.TimeScale; host.TimeScale = 3f;
            yield return Settle(6f, () => { });                 // the match settles
            host.TimeScale = was;
            // look straight out to sea (BaseYaw -90 + 90 = north): the craft run in from 96 m out toward the camera
            var focus = new Vector2(x0, shore + 4f);
            for (int i = 0; i < Mathf.RoundToInt(1.5f * Fps); i++) { PoseCam(focus, 50f, 85f, 28f); yield return Tick(); }
            ClearProps(new Vector2(x0, shore - 8f), 22f, 6f);
            for (int sl = 0; sl < RosterEntry.SlotCount; sl++) { hud.Deploy(sl); for (int i = 0; i < 6; i++) { PoseCam(focus, 50f, 85f, 28f); yield return Tick(); } }
            Seg("landing_0_craft", "REINFORCEMENTS  -  BY SEA", false);
            yield return Hold(18f, t => PoseCam(new Vector2(x0, shore + Mathf.Lerp(12f, -4f, t)), Mathf.Lerp(40f, 26f, t), Mathf.Lerp(78f, 105f, t), Mathf.Lerp(26f, 20f, t)));
            PoseRelease();
        }

        /// <summary>2026-11-16: the coast with the landing craft running in, as stills at a ladder of zooms from two angles
        /// (looking out to sea, and the game's own side-on view). opts: wait=seconds after the deploy (default 14), hud=1.</summary>
        IEnumerator PartBeachZoom(string opts)
        {
            string P(string k, string dflt) { var m = System.Text.RegularExpressions.Regex.Match(" " + opts + " ", @"\s" + k + @"=(\S+)"); return m.Success ? m.Groups[1].Value : dflt; }
            yield return StartMatch(false, true);
            var host = Host; var hud = FindFirstObjectByType<HudController>(); var tc = Cam;
            host.WriteWorlds(m => { m.World.Silver[0] = 6000; });
            var map = host.Local.Map; float shore = map.ShoreZ, sea0 = map.SeaStartZ, x0 = map.SizeMeters.x * 0.5f;
            Log($"beachzoom: shore {shore:0.0} sand {sea0:0.0} map {map.SizeMeters} zoom {tc.NearZoom:0.0}..{tc.ZoomMax:0.0} close {tc.CloseZoom:0.0} pitch {tc.PitchMin:0}..{tc.PitchMax:0} baseYaw {tc.BaseYaw:0}");
            VfxLab.Rain(0f);
            float was = host.TimeScale; host.TimeScale = 3f;
            yield return Settle(5f, () => { });
            host.TimeScale = was;
            var seaFocus = new Vector2(x0, shore + 4f);
            for (int i = 0; i < Mathf.RoundToInt(1f * Fps); i++) { PoseCam(seaFocus, 50f, 85f, 28f); yield return Tick(); }
            for (int sl = 0; sl < RosterEntry.SlotCount; sl++) { hud.Deploy(sl); for (int i = 0; i < 6; i++) { PoseCam(seaFocus, 50f, 85f, 28f); yield return Tick(); } }
            var lift = host.Local.World.SeaLift as SeaLandingSystem;
            string Crafts() { if (lift == null) return "no lift"; var sb2 = new StringBuilder(); for (int c = 0; c < lift.Count; c++) sb2.Append($" [{c} {lift.StateOf(c)} {lift.PositionOf(c).x:0},{lift.PositionOf(c).z:0}]"); return sb2.ToString(); }
            Log("after deploy:" + Crafts());
            // run the clock until a craft is grounding or close in (at most `wait` seconds)
            float wait = float.Parse(P("wait", "20"), System.Globalization.CultureInfo.InvariantCulture);
            float closeIn = float.Parse(P("closein", "18"), System.Globalization.CultureInfo.InvariantCulture);
            Vector2 craftAt = new Vector2(x0, shore + 10f);
            for (int i = 0; i < Mathf.RoundToInt(wait * Fps); i++)
            {
                PoseCam(seaFocus, 50f, 85f, 28f); yield return Tick();
                if (lift == null || i % 5 != 0) continue;
                bool hit = false; Vector2 sum = Vector2.zero; int n = 0;
                for (int c = 0; c < lift.Count; c++)
                {
                    var st = lift.StateOf(c); if (st == LandingState.Idle) continue;
                    var cp = lift.PositionOf(c); sum += new Vector2(cp.x, cp.z); n++;
                    if (st == LandingState.Grounding || st == LandingState.Unloading || (st == LandingState.RunIn && cp.z - shore < closeIn)) hit = true;
                }
                if (n > 0) craftAt = sum / n;
                if (hit) break;
            }
            Log("framing at " + craftAt + ":" + Crafts());
            ui = P("hud", "0") == "1";
            // freeze the clock so every zoom of one angle shows the same moment
            float ts = host.TimeScale; host.TimeScale = 0f;
            var ladder = new (string tag, float zoom, float pitch)[]
            {
                ("1_far", 140f, 55f), ("2_wide", 90f, 48f), ("3_mid", 55f, 40f), ("4_near", 32f, 30f), ("5_close", tc.NearZoom + 1f, 20f),
            };
            var angles = new (string name, Vector2 focus, float yaw)[]
            {
                ("sea", new Vector2(craftAt.x, Mathf.Lerp(shore, craftAt.y, 0.6f)), 85f),
                ("game", new Vector2(craftAt.x, Mathf.Lerp(shore, craftAt.y, 0.5f)), 0f),
            };
            foreach (var an in angles)
                foreach (var st in ladder)
                {
                    for (int i = 0; i < 8; i++) { PoseCam(an.focus, st.zoom, an.yaw, st.pitch); yield return Tick(); }
                    yield return Snap($"{outDir}/{an.name}_{st.tag}_z{st.zoom:0}.png");
                    yield return Tick();
                    Log($"{an.name} {st.tag} zoom {st.zoom:0.0} pitch {st.pitch:0} cam {Camera.main.transform.position}");
                }
            // the super-zoom: a free lens down on the waterline where the craft ground
            for (int i = 0; i < 8; i++) { PoseFree(new Vector2(craftAt.x, Mathf.Min(craftAt.y, shore + 8f)), 16f, 36f, 80f, 12f, 0.2f, 1.6f); yield return Tick(); }
            yield return Snap($"{outDir}/sea_6_superclose.png"); yield return Tick();
            host.TimeScale = ts;
            PoseRelease();
        }

        /// <summary>2026-11-16: close on the trenches nearest the coast, looking seaward so the sea and the landing craft sit in
        /// the background. Three spots along the seaward fire trench, a zoom ladder each, plus low free-lens shots.</summary>
        IEnumerator PartTrenchSea(string opts)
        {
            yield return StartMatch(false, true);
            var host = Host; var hud = FindFirstObjectByType<HudController>(); var tc = Cam;
            host.WriteWorlds(m => { m.World.Silver[0] = 6000; });
            var map = host.Local.Map; float shore = map.ShoreZ, x0 = map.SizeMeters.x * 0.5f;
            VfxLab.Rain(0f);
            float was = host.TimeScale; host.TimeScale = 3f;
            yield return Settle(5f, () => { });
            host.TimeScale = was;
            // the trenches: every one's mean position, then the fire trench nearest the sea
            const float cell = 2f;     // MapData.NavCellSize
            Vector2 CellPos(int idx) => new Vector2((idx % map.NavWidth + 0.5f) * cell, (idx / map.NavWidth + 0.5f) * cell);
            int best = -1; float bestZ = float.MinValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var td = map.Trenches[t]; if (td.CellCount == 0) continue;
                float zsum = 0f; for (int c = 0; c < td.CellCount; c++) zsum += CellPos(map.TrenchCells[td.CellStart + c]).y;
                float zm = zsum / td.CellCount;
                Log($"trench {td.Id} kind {td.Kind} team {td.OwnerTeam} cells {td.CellCount} mean z {zm:0.0}");
                if (td.Kind == 0 && zm < shore && zm > bestZ) { bestZ = zm; best = t; }
            }
            if (best < 0) { Log("no fire trench"); yield break; }
            var tr = map.Trenches[best];
            var spots = new List<Vector2>();
            foreach (float f in new[] { 0.25f, 0.5f, 0.75f }) spots.Add(CellPos(map.TrenchCells[tr.CellStart + Mathf.Clamp(Mathf.RoundToInt(f * (tr.CellCount - 1)), 0, tr.CellCount - 1)]));
            Log($"seaward trench {tr.Id} mean z {bestZ:0.0}, spots {string.Join(" ", spots)}");
            // reinforcements by sea, so craft are on the water behind the trench
            var seaFocus = new Vector2(x0, shore);
            for (int sl = 0; sl < RosterEntry.SlotCount; sl++) { hud.Deploy(sl); for (int i = 0; i < 6; i++) { PoseCam(seaFocus, 50f, 85f, 28f); yield return Tick(); } }
            for (int i = 0; i < Mathf.RoundToInt(9f * Fps); i++) { PoseCam(seaFocus, 50f, 85f, 28f); yield return Tick(); }
            ui = false;
            float ts = host.TimeScale; host.TimeScale = 0f;
            // aim past the trench toward the water and keep the pitch low: the trench fills the lower half, the sea the top
            var ladder = new (string tag, float zoom, float pitch, float ahead)[]
            {
                ("a_z40", 40f, 24f, 12f), ("b_z30", 30f, 19f, 10f), ("c_z22", 22f, 15f, 8f), ("d_z16", tc.NearZoom + 1f, 12f, 6f),
            };
            var yaws = new[] { 88f, 72f, 102f };     // out to sea, then a little to each side
            for (int s = 0; s < spots.Count; s++)
            {
                ClearProps(spots[s], 4f, 2f);
                var fw = Fwd(yaws[s]);
                foreach (var st in ladder)
                {
                    var aim = spots[s] + fw * st.ahead;
                    for (int i = 0; i < 8; i++) { PoseCam(aim, st.zoom, yaws[s], st.pitch); yield return Tick(); }
                    yield return Snap($"{outDir}/spot{s + 1}_{st.tag}.png"); yield return Tick();
                }
                // over the parapet: a free lens a few metres inland, at standing height, looking across the trench to the sea
                for (int i = 0; i < 8; i++) { PoseFree(spots[s] + fw * 4f, 12f, 42f, yaws[s], 9f, 0f, 2.2f); yield return Tick(); }
                yield return Snap($"{outDir}/spot{s + 1}_e_parapet.png"); yield return Tick();
                Log($"spot {s + 1} at {spots[s]} yaw {yaws[s]} fwd {fw}");
            }
            host.TimeScale = ts;
            PoseRelease();
        }

        IEnumerator PartDeaths(string opts)
        {
            yield return StartMatch(false, true);
            var host = Host; var dir = GymDirector.Attach(host);
            dir.Quiet(); yield return Tick();
            var shots = new (OffMapAbilityId id, string cap, int pattern)[]
            {
                (OffMapAbilityId.HeBarrage, "DIRECT HIT", 0), (OffMapAbilityId.CreepingBarrage, "CREEPING BARRAGE", 0),
                (OffMapAbilityId.HeBarrage, "BOX BARRAGE", AbilityPattern.Box), (OffMapAbilityId.ChlorineGas, "GAS ATTACK", 0),
                (OffMapAbilityId.StrafeRun, "STRAFING RUN", 0), (OffMapAbilityId.Beam, "THE BEAM", 0),
            };
            int idx = 0;
            foreach (var sh in shots)
            {
                ClearStill(host); dir.Clear(); rec = false;
                var at = CleanStage(host, 40f);
                var target = new Vector2(at.x, at.y + 30f);
                Frogs(dir, new Vector2(at.x, at.y + 6f), 8);
                for (int r = 0; r < 4; r++)
                    for (int k = 0; k < 9; k++) { int sl = dir.Spawn(1, k % 4 == 3 ? 2 : 0, target.x + (k - 4) * 2.2f, target.y + r * 2.2f, 180f); if (sl >= 0) dir.Hold(sl); }
                var focus = new Vector2(target.x, target.y + 3f); float side = (idx % 2 == 0) ? 1f : -1f;
                for (int i = 0; i < Mathf.RoundToInt(1.2f * Fps); i++) { PoseCam(focus, 26f, -25f, 28f, side * 0.5f); yield return Tick(); }
                ClearProps(focus, 22f, 8f);
                yield return FireAndWait(dir, host, sh.id, focus, sh.pattern, () => PoseCam(focus, 26f, -25f, 28f, side * 0.5f));
                Seg($"death_{idx++:00}_{sh.cap.Replace(' ', '-')}", sh.cap, false);
                float was = host.TimeScale; host.TimeScale = 0.55f;
                yield return Hold(8.5f, t => PoseCam(focus, Mathf.Lerp(26f, 19f, t), Mathf.Lerp(-28f, 22f, t), Mathf.Lerp(30f, 24f, t), side * 0.5f));
                host.TimeScale = was;
            }
            // the individual deaths, three men a shot, each by a different cause
            var groups = new[] { new[] { DeathKind.Shot, DeathKind.Burning, DeathKind.Blast }, new[] { DeathKind.Crushed, DeathKind.Beam, DeathKind.Gas } };
            foreach (var g in groups)
            {
                ClearStill(host); dir.Clear(); rec = false;
                var at = CleanStage(host, 40f);
                var mid = new Vector2(at.x, at.y + 26f);
                Frogs(dir, new Vector2(at.x, at.y + 6f), 8);
                for (int j = 0; j < g.Length; j++) dir.Death(g[j], 0, new Vector2(mid.x + (j - 1) * 11f, mid.y));
                for (int i = 0; i < Mathf.RoundToInt(1f * Fps); i++) { PoseCam(mid, 30f, -15f, 30f); yield return Tick(); }
                ClearProps(mid, 22f, 8f);
                Seg($"death_{idx++:00}_{string.Join("-", g)}", string.Join("  -  ", g).ToUpperInvariant(), false);
                float was = host.TimeScale; host.TimeScale = 0.6f;
                yield return Hold(7f, t => PoseCam(mid, Mathf.Lerp(30f, 24f, t), Mathf.Lerp(-18f, 14f, t), Mathf.Lerp(30f, 24f, t)));
                host.TimeScale = was;
            }
            ClearStill(host); dir.Clear(); dir.Unquiet(); PoseRelease();
        }

        IEnumerator PartAbilities(string opts)
        {
            yield return StartMatch(false, true);
            var host = Host; var dir = GymDirector.Attach(host);
            dir.Quiet(); yield return Tick();
            int idx = 0;
            foreach (var a in GymCatalogue.Abilities())
            {
                if (a.Expect == GymExpect.Rejected) continue;
                var id = (OffMapAbilityId)a.Id;
                ClearStill(host); dir.Clear(); rec = false;
                var at = CleanStage(host, 40f);
                var target = new Vector2(at.x, at.y + 38f);
                var focus = target;
                Frogs(dir, new Vector2(at.x, at.y + 4f), 8);
                EnemyLine(dir, host, at, 36f, 6, true);       // killable: the effect has something to do
                float side = (idx % 2 == 0) ? -1f : 1f; float was = host.TimeScale;
                for (int i = 0; i < Mathf.RoundToInt(1.2f * Fps); i++) { PoseCam(focus, 32f, -20f, 32f, side); yield return Tick(); }
                ClearProps(focus, 24f, 9f);
                yield return FireAndWait(dir, host, id, target, 0, () => PoseCam(focus, 32f, -20f, 32f, side));
                Seg($"ability_{idx++:00}_{id}", Pretty(id.ToString()), false);
                host.TimeScale = 0.7f;
                yield return Hold(8f, t => PoseCam(focus, Mathf.Lerp(34f, 26f, t), Mathf.Lerp(-22f, 18f, t), Mathf.Lerp(32f, 26f, t), side));
                host.TimeScale = was;
            }
            ClearStill(host); dir.Clear(); dir.Unquiet(); PoseRelease();
        }

        // ------------------------------------------------------------------------------------------ beach stills (2026-11-08)
        // A frog rifleman running over the coast's sand with fire behind him: five camera set-ups, a burst of PNG stills each.
        //   Footage.Begin("parts=beach out=C:/Users/PC/Documents/footage/beach1 w=2560 h=1440 shots=12345")
        readonly List<int> fxSlots = new List<int>();
        bool noFlare, closeOff, noFx;

        IEnumerator Snap(string path) { pendingPath = path; pending = true; yield return null; }

        static Vector2 Fwd(float yaw) { float p = (-90f + yaw) * Mathf.Deg2Rad; return new Vector2(Mathf.Sin(p), Mathf.Cos(p)); }
        static Vector2 Right(float yaw) { float p = (-90f + yaw) * Mathf.Deg2Rad; return new Vector2(Mathf.Cos(p), -Mathf.Sin(p)); }

        static Vector2 OnSand(SimHost host, Vector2 p)
        {
            var map = host.Local.Map;
            return new Vector2(Mathf.Clamp(p.x, 4f, map.SizeMeters.x - 4f), Mathf.Clamp(p.y, 4f, map.ShoreZ - 1.5f));
        }

        /// <summary>A free camera: `dist` metres from the man along the look, lens `fov`, aimed at `aimH` above the ground;
        /// `lead` slides the aim sideways (a fraction of the half-width) so he stands off-centre.</summary>
        void PoseFree(Vector2 focus, float dist, float fov, float yaw, float pitch, float lead, float aimH = 1.1f)
        {
            var tc = Cam; var cam = Camera.main; var host = Host;
            if (tc == null || cam == null) return;
            tc.enabled = false; poseOff = tc;
            SceneHooks.CloseUp = closeOff ? 0f : tc.CloseUpAt(tc.NearZoom);
            Shader.SetGlobalFloat("_TWClose", SceneHooks.CloseUp);
            cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
            var rot = Quaternion.Euler(pitch, tc.BaseYaw + yaw, 0f);
            float ground = host.Local.Map.Height.Sample(focus.x, focus.y);
            var aim = new Vector3(focus.x, ground + aimH, focus.y);
            float halfW = dist * Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad) * cam.aspect;
            aim += rot * Vector3.right * (lead * halfW * 0.66f);
            cam.transform.SetPositionAndRotation(aim - rot * Vector3.forward * dist, rot);
        }

        static Vector2 PosOf(SimHost host, int slot) { var p = host.Local.World.Position[slot]; return new Vector2(p.x, p.z); }

        int Runner(GymDirector dir, SimHost host, Vector2 at, Vector2 to, float speed)
        {
            var d = to - at; float yaw = Mathf.Atan2(d.x, d.y) * Mathf.Rad2Deg;
            int sl = dir.Spawn(0, (int)InfantryArchetype.Frog, at.x, at.y, yaw);     // the frog figure (VATRenderer: archetype 25)
            if (sl < 0) return -1;
            host.WriteWorlds(m =>
            {
                var w = m.World; w.Hp[sl] = 1e6f; w.MaxHp[sl] = 1e6f; w.Speed[sl] = speed;
                var c = m.Map.NavCellOf(new float3(to.x, 0f, to.y));
                w.GoalId[sl] = m.Fields.GetGoal(TW.Sim.Nav.GoalKey.Cell(m.Map.NavIndex(c.x, c.y), TW.Sim.Nav.NavMode.Infantry));
            });
            return sl;
        }

        void DespawnFx(SimHost host)
        {
            if (fxSlots.Count == 0) return;
            var gone = fxSlots.ToArray(); fxSlots.Clear();
            host.WriteWorlds(m => { foreach (var s in gone) if (s < m.World.HighWater && (m.World.Flags[s] & (uint)UnitFlags.Alive) != 0) m.World.Despawn(s); });
        }

        /// <summary>Despawn the enemy's men (and, with no effects, its machines too): new ones keep landing, so this runs every few frames.</summary>
        void ClearFoes(SimHost host)
        {
            host.WriteWorlds(m =>
            {
                var w = m.World;
                for (int i = 0; i < w.HighWater; i++)
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) != 0 && w.Team[i] == 1 && (noFx || (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0)) w.Despawn(i);
            });
        }

        IEnumerator Retry(Func<string> act, string what)
        {
            for (int i = 0; i < 8; i++)
            {
                string r = act();
                if (r == null || !r.Contains("tick apart")) { Log(what + ": " + r); yield break; }
                yield return Tick();
            }
            Log(what + ": gave up");
        }

        IEnumerator BeachShot(string name, Vector2 a, Vector2 b, float speed, float dist, float fov, float yaw, float pitch, float thirds,
                              int buddies, float fxDist, int[] machines, int stills = 14, int every = 4, float runup = 1.3f, bool shells = true)
        {
            var host = Host; var dir = GymDirector.Attach(host);
            ClearStill(host); dir.Clear(); DespawnFx(host); rec = false; PoseRelease(); ui = false;
            // no enemy riflemen: their fire pins the runner flat on the sand
            ClearFoes(host);
            for (int i = 0; i < 3; i++) yield return Tick();
            var dv = (b - a).normalized; var side = new Vector2(dv.y, -dv.x);
            var fw = Fwd(yaw); var rt = Right(yaw);
            float lead = thirds * Mathf.Sign(Vector2.Dot(dv, rt) == 0f ? 1f : Vector2.Dot(dv, rt));
            // where the runner is half-way through the burst: the fire is set beyond that point along the camera's look
            var anchor = a + dv * speed * (runup + stills * every * 0.5f / Fps);

            // the burning machines first, so they are well alight when the man arrives
            var blazes = new List<int>();
            if (noFx) { machines = new int[0]; shells = false; }
            for (int k = 0; k < machines.Length; k++)
            {
                var p = OnSand(host, anchor + fw * (fxDist + 6f * k) + rt * ((k - (machines.Length - 1) * 0.5f) * 18f + 7f));
                string s = TankCapture.Spawn(0, machines[k], p.x, p.y, yaw + 70f * k + 30f);     // our side: an enemy Maw burns the runner
                if (!s.StartsWith("slot ")) { Log(name + " blaze: " + s); continue; }
                int sl = int.Parse(s.Substring(5));
                RiderLab.Stop(sl);
                host.WriteWorlds(m => { m.World.Hp[sl] = 1e6f; m.World.MaxHp[sl] = 1e6f; });
                fxSlots.Add(sl); blazes.Add(sl);
            }
            for (int i = 0; i < 4; i++) yield return Tick();
            foreach (var sl in blazes) { int s2 = sl; yield return Retry(() => TankCapture.Ignite(s2, 0.95f), name + " ignite " + s2); }
            if (!noFlare) VfxLab.Flare();
            // let the fires take hold before the man starts
            for (int i = 0; i < Mathf.RoundToInt(1.5f * Fps); i++) { PoseFree(a, dist, fov, yaw, pitch, 0f); yield return Tick(); }

            int hero = Runner(dir, host, a, b, speed);
            if (hero < 0) { Log(name + ": no runner"); yield break; }
            for (int k = 0; k < buddies; k++)
            {
                var off = -dv * (2.6f + 2.3f * k) + side * ((k % 2 == 0 ? 1f : -1f) * (1.9f + 1.0f * k));
                Runner(dir, host, a + off, b + off, speed * (0.96f + 0.025f * k));
            }
            Vector2 f = a;
            int runFrames = Mathf.RoundToInt(runup * Fps);
            for (int i = 0; i < runFrames; i++)
            {
                if (i % 5 == 0) ClearFoes(host);
                f = Vector2.Lerp(f, PosOf(host, hero), 0.35f); PoseFree(f, dist, fov, yaw, pitch, lead);
                if (!noFx && i == runFrames - Mathf.RoundToInt(0.9f * Fps))
                    foreach (var o in new[] { new Vector2(-10f, 0f), new Vector2(8f, 2f), new Vector2(-2f, 9f), new Vector2(15f, 11f) })
                    {
                        var p = OnSand(host, anchor + fw * (fxDist + o.y) + rt * o.x); var pp = p;
                        yield return Retry(() => VfxLab.Fire(pp.x, pp.y, 4f), name + " fire");
                    }
                yield return Tick();
            }
            ClearProps(f, 7f, 3f);
            string d = outDir + "/" + name; Directory.CreateDirectory(d);
            var rnd = new System.Random(name.GetHashCode());
            for (int s = 0; s < stills; s++)
            {
                ClearFoes(host);
                if (shells && (s == 3 || s == 10))
                {
                    var p = OnSand(host, anchor + fw * (fxDist + 14f) + rt * ((float)rnd.NextDouble() * 24f - 12f)); var pp = p;
                    yield return Retry(() => VfxLab.Shell(pp.x, pp.y, 3f, 1f), name + " shell");
                }
                for (int i = 0; i < every - 1; i++) { f = Vector2.Lerp(f, PosOf(host, hero), 0.35f); PoseFree(f, dist, fov, yaw, pitch, lead); yield return Tick(); }
                f = Vector2.Lerp(f, PosOf(host, hero), 0.35f); PoseFree(f, dist, fov, yaw, pitch, lead);
                yield return Snap($"{d}/{s:00}.png");
            }
            var hp = host.Local.World.Velocity[hero];
            int foes = 0, foeVeh = 0; { var w = host.Local.World; for (int i = 0; i < w.HighWater; i++) if ((w.Flags[i] & (uint)UnitFlags.Alive) != 0 && w.Team[i] == 1) { foes++; if ((w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) foeVeh++; } }
            Log($"{name}: hero at {PosOf(host, hero)} speed {Mathf.Sqrt(hp.x * hp.x + hp.z * hp.z):0.00} hp {host.Local.World.Hp[hero]:0} foes {foes} ({foeVeh} machines) noFx {noFx}");
            PoseRelease();
        }

        IEnumerator PartBeach(string opts)
        {
            string P(string k, string dflt) { var m = System.Text.RegularExpressions.Regex.Match(" " + opts + " ", @"\s" + k + @"=(\S+)"); return m.Success ? m.Groups[1].Value : dflt; }
            yield return StartMatch(false, true);
            var host = Host; var dir = GymDirector.Attach(host);
            dir.Quiet(); yield return Tick();
            var map = host.Local.Map; float shore = map.ShoreZ, sea0 = map.SeaStartZ, X = map.SizeMeters.x;
            Log($"beach: shore {shore:0.0} sand from {sea0:0.0} map {map.SizeMeters} sea {map.SeaSide} nearZoom {Cam.NearZoom:0.0}");
            VfxLab.Rain(0f);
            float was = host.TimeScale; host.TimeScale = 3f;
            yield return Settle(4f, () => { });
            host.TimeScale = was;
            string want = P("shots", "12345");
            noFlare = P("flare", "1") == "0"; closeOff = P("close", "1") == "0"; noFx = P("fx", "1") == "0"; if (noFx) noFlare = true;
            string men = P("men", "");     // look.menClose: how much the night look lifts close figures (1 = the game's default, 0 = none)
            if (men != "") Knobs.Set("look.menClose", men);
            Log($"flare {!noFlare} close {!closeOff} fx {!noFx} menClose {(men == "" ? "default" : men)}");
            float zs = Mathf.Lerp(sea0, shore, 0.45f);
            // name, from, to, speed, distance, lens, yaw, pitch, lead, squad, fire distance, burning machines
            if (want.Contains("1")) yield return BeachShot("1_side_track", new Vector2(20f, zs), new Vector2(X - 8f, zs), 3.2f, 19f, 32f, -90f, 16f, 0.5f, 0, 16f, new[] { 7 });
            if (want.Contains("2")) yield return BeachShot("2_toward_camera", new Vector2(X * 0.5f - 4f, sea0 - 10f), new Vector2(X * 0.5f + 2f, shore - 1f), 3.4f, 17f, 34f, -100f, 16f, 0f, 0, 15f, new[] { 4 });
            if (want.Contains("3")) yield return BeachShot("3_over_shoulder", new Vector2(30f, shore - 3f), new Vector2(42f, sea0 - 40f), 3.2f, 16f, 36f, -125f, 19f, 0.45f, 0, 17f, new[] { 4 });
            if (want.Contains("4")) yield return BeachShot("4_squad", new Vector2(X - 12f, zs + 2f), new Vector2(8f, zs + 2f), 3.3f, 24f, 34f, -120f, 20f, 0.3f, 4, 18f, new[] { 4, 7 });
            if (want.Contains("5")) yield return BeachShot("5_close", new Vector2(18f, zs), new Vector2(X - 6f, zs), 3.9f, 13f, 36f, -70f, 14f, 0.4f, 0, 14f, new[] { 7 });
            ClearStill(host); dir.Clear(); DespawnFx(host); dir.Unquiet(); PoseRelease();
        }

        // ------------------------------------------------------------------------------------------ assault (2026-11-17)
        // The trailer animatic's "over the top" shot played in the game: the coast mission, a high camera behind our frogs
        // looking at the enemy's fire trench, easing forward for `secs` (25) while a cursor keeps pressing the infantry
        // cards and each press sends a squad of frogs over the top; extra tracers and shell bursts on top of the real fire.
        // `portraits=<dir>` puts <dir>/Rifleman|Gasmask|Piper|Officer|Tank.png on the cards (cursor.png is the pointer).
        //   Footage.Begin("parts=assault out=C:/Users/PC/Documents/footage/assault1 secs=25 portraits=C:/Users/PC/Documents/footage/portraits")
        // Knobs: zoom0/zoom1, pitch0/pitch1, yaw, start (metres behind our trench), end (share of the way to theirs),
        // every (seconds between presses), squad (frogs a press), shells (seconds between bursts), tracers (a frame), hp (x roster).
        VisualElement cursor;
        float uiAlpha = 1f;

        /// <summary>Every machine but `keep` to a corner out of the shot (ours to our rear left, theirs to their rear right),
        /// held there with no speed. A despawn would leave the wreck drawn where it stood.</summary>
        void ParkMachines(SimHost host, int keep)
        {
            var map = host.Local.Map; float X = map.SizeMeters.x;
            var moved = new List<int>(); int k0 = 0, k1 = 0;
            host.WriteWorlds(m =>
            {
                var w = m.World; k0 = 0; k1 = 0;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (i == keep || (w.Flags[i] & (uint)UnitFlags.Alive) == 0 || (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0) continue;
                    bool ours = w.Team[i] == 0; int k = ours ? k0++ : k1++;
                    float px = ours ? 6f + (k % 3) * 11f : X - 6f - (k % 3) * 11f, pz = ours ? 6f + (k / 3) * 11f : map.ShoreZ - 30f - (k / 3) * 11f;
                    if (Mathf.Abs(w.Position[i].x - px) < 1f && Mathf.Abs(w.Position[i].z - pz) < 1f) continue;
                    w.Position[i] = new float3(px, w.Position[i].y, pz); w.Speed[i] = 0f;
                    moved.Add(i);
                }
            });
            foreach (var i in moved) { RiderLab.Stop(i); GymDirector.Attach(host).Hold(i); }
            if (moved.Count > 0) Log("assault: parked machines " + string.Join(",", moved));
        }

        /// <summary>The animatic's field has no husks: drop the wreck views TankRenderer draws for the map's husks near the
        /// frame (the drawing only; the sim's props stay as they are).</summary>
        void HideHusks(Vector2 centre, float radius)
        {
            var tr = FindFirstObjectByType<TankRenderer>();
            var fi = tr != null ? typeof(TankRenderer).GetField("wrecks", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance) : null;
            var list = fi != null ? fi.GetValue(tr) as System.Collections.IList : null;
            if (list == null) { Log("assault: no wreck list"); return; }
            int gone = 0;
            for (int i = list.Count - 1; i >= 0; i--)
            {
                var v = list[i]; if (v == null) continue;
                var pf = v.GetType().GetField("Pos"); if (pf == null) continue;
                var pos = (Vector3)pf.GetValue(v);
                if (Vector2.Distance(new Vector2(pos.x, pos.z), centre) < radius) { list.RemoveAt(i); gone++; }
            }
            Log($"assault: husk views dropped {gone} of {list.Count + gone}");
        }

        Vector3[] hideAt;     // (x, z, radius) regions kept clear of kit props for the whole take
        TW.Presentation.Terrain.BattlefieldProps suppressBp; System.Func<TW.Presentation.Terrain.BattlefieldKit.Module, Matrix4x4, bool> suppressWas;
        Rect field; bool fieldOn; float fieldKeep = 0f;
        readonly Dictionary<string, float> labelScale = new Dictionary<string, float>();
        float hogH = 1.5f, stumpH = 3.0f;
        readonly Dictionary<TW.Presentation.Terrain.BattlefieldKit.Module, float> maxDist = new Dictionary<TW.Presentation.Terrain.BattlefieldKit.Module, float>();
        readonly List<(string kind, Vector3 pos, float yaw, float s)> scatter = new List<(string, Vector3, float, float)>();
        readonly HashSet<long> scatterKeys = new HashSet<long>(); int scatterGen = -1; readonly HashSet<string> missingParts = new HashSet<string>();
        TW.Presentation.Terrain.Atmosphere atmo; float atmoStart, atmoDepth;
        object lutProfile; Type lutType; Texture2D lutTex;

        string AddLut(Component at, string path, float mix)
        {
            var grade = at.transform.Find("Grade");
            if (grade == null) return "no Grade volume under the Atmosphere";
            var vol = grade.GetComponent("Volume");
            if (vol == null) return "no Volume on Grade";
            var vt = vol.GetType(); var profile = vt.GetField("sharedProfile") != null ? vt.GetField("sharedProfile").GetValue(vol) : vt.GetProperty("sharedProfile")?.GetValue(vol);
            lutType = AppDomain.CurrentDomain.GetAssemblies().Select(a => a.GetType("UnityEngine.Rendering.Universal.ColorLookup")).FirstOrDefault(t => t != null);
            if (profile == null || lutType == null) return "no profile or no ColorLookup type";
            lutTex = new Texture2D(1024, 32, TextureFormat.RGBA32, false, true) { name = "OfficerLut", filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.DontSave };
            if (!lutTex.LoadImage(File.ReadAllBytes(path))) return "could not read " + path;
            var pt = profile.GetType();
            var has = pt.GetMethods().First(m => m.Name == "Has" && !m.IsGenericMethod && m.GetParameters().Length == 1);
            if ((bool)has.Invoke(profile, new object[] { lutType })) pt.GetMethods().First(m => m.Name == "Remove" && !m.IsGenericMethod).Invoke(profile, new object[] { lutType });
            var add = pt.GetMethods().First(m => m.Name == "Add" && !m.IsGenericMethod && m.GetParameters().Length == 2);
            var comp = add.Invoke(profile, new object[] { lutType, true });
            void Over(string field, object v)
            {
                var prm = lutType.GetField(field).GetValue(comp);
                prm.GetType().GetMethods().First(m => m.Name == "Override" && m.GetParameters().Length == 1 && m.GetParameters()[0].ParameterType.IsInstanceOfType(v)).Invoke(prm, new[] { v });
            }
            Over("texture", lutTex); Over("contribution", Mathf.Clamp01(mix));
            lutProfile = profile;
            bool valid = (bool)lutType.GetMethod("ValidateLUT").Invoke(comp, null);
            return $"{path} {lutTex.width}x{lutTex.height} mix {mix} valid {valid}";
        }

        void RemoveLut()
        {
            if (lutProfile != null && lutType != null) lutProfile.GetType().GetMethods().First(m => m.Name == "Remove" && !m.IsGenericMethod).Invoke(lutProfile, new object[] { lutType });
            lutProfile = null; if (lutTex != null) UnityEngine.Object.Destroy(lutTex); lutTex = null;
        }
        /// <summary>One of the film's hedgehogs or stumps: the kit nudges a placed instance (ground, look), so by distance, not key.</summary>
        bool NearScatter(Vector3 p) { foreach (var sc in scatter) { float dx = p.x - sc.pos.x, dz = p.z - sc.pos.z; if (dx * dx + dz * dz < 1.2f) return true; } return false; }
        static long QKey(Vector3 p) => ((long)Mathf.RoundToInt(p.x * 2f) << 32) ^ (uint)Mathf.RoundToInt(p.z * 2f);
        static TW.Presentation.Terrain.BattlefieldKit.Module FindModule(TW.Presentation.Terrain.BattlefieldProps bp, string part)
        {
            var fi = typeof(TW.Presentation.Terrain.BattlefieldProps).GetField("batches", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            var dict = fi != null ? fi.GetValue(bp) as System.Collections.IDictionary : null; if (dict == null) return null;
            foreach (var k in dict.Keys) if (k is TW.Presentation.Terrain.BattlefieldKit.Module mm && Label(mm).Contains(part)) return mm;
            return null;
        }
        /// <summary>The animatic's hedgehogs and stumps, as the kit's own TimberHedgehog and SplitStumpTall, at the film's spots
        /// and turns; the size is the field's own median for the kind times the film's spread around its mean.</summary>
        void AddScatter(TW.Presentation.Terrain.BattlefieldProps bp)
        {
            if (scatter.Count == 0 || bp.Generation == scatterGen) return;
            scatterGen = bp.Generation; scatterKeys.Clear(); int n = 0, inView = 0;
            foreach (var sc in scatter)
            {
                string part = ScatterPart(sc.kind);
                var mod = FindModule(bp, part); if (mod == null) { if (missingParts.Add(part)) Log("assault: scatter no module " + part); continue; }
                if (!maxDist.ContainsKey(mod)) { maxDist[mod] = mod.MaxDistance; mod.MaxDistance = float.PositiveInfinity; }     // small kit is not drawn far off: the film's are
                // the film's sizes: a hedgehog about a frog's height, a split stump half again taller than a man
                float tall = ScatterTall(sc.kind), meshH = mod.Mesh != null ? Mathf.Max(0.05f, mod.Mesh.bounds.size.y) : 1f;
                float baseS = tall / meshH, mean = sc.kind == "stump" ? 1.8f : 1.3f;
                var pos = new Vector3(sc.pos.x, bp.Ground(sc.pos.x, sc.pos.z), sc.pos.z);
                float k = baseS * sc.s / mean;
                if (mod.Mesh != null) pos.y -= mod.Mesh.bounds.min.y * k;     // stand it on the ground by its lowest point (the hedgehog's pivot is its middle)
                bp.AddInstance(mod, Matrix4x4.TRS(pos, Quaternion.Euler(0f, sc.yaw, 0f), Vector3.one * k));
                scatterKeys.Add(QKey(pos)); n++;
                if (scatterGen <= 1 && n <= 3) Log($"assault: scatter {sc.kind} as {Label(mod)} meshH {meshH:0.00} scale {baseS * sc.s / mean:0.00} at {pos}");
                if (scatterGen <= 1) { var vp = Camera.main != null ? Camera.main.WorldToViewportPoint(pos) : Vector3.zero; if (vp.z > 0 && vp.x > 0 && vp.x < 1 && vp.y > 0 && vp.y < 1) inView++; }
            }
            if (scatterGen <= 1) Log($"assault: scatter placed {n}, {inView} in view");
        }
        float splitH = 1.8f, postH = 1.6f, wireH = 1.2f, snagH = 5f, logH = 0.7f, boardsH = 1.4f;
        static string ScatterPart(string kind) => kind switch
        {
            "hog" => "timberhedgehog", "stump" => "splitstumptall", "split" => "splitstump", "post" => "wirepost",
            "wire" => "wirefence", "snag" => "snag", "log" => "fallenlog", "boards" => "crossedboards",
            _ => kind,
        };
        float ScatterTall(string kind) => kind switch
        {
            "hog" => hogH, "stump" => stumpH, "split" => splitH, "post" => postH, "wire" => wireH,
            "snag" => snagH, "log" => logH, "boards" => boardsH, _ => 1.5f,
        };
        static string Label(TW.Presentation.Terrain.BattlefieldKit.Module m) => (m.Name ?? (m.Mesh != null ? m.Mesh.name : "?")).ToLowerInvariant();
        static readonly string[] KeepAll = { "sandbag", "duckboard", "plank", "ladder", "wire", "dugout", "revet", "trench", "parapet", "firestep", "bunker" };
        static readonly string[] KeepSome = { "hedgehog", "knife", "stump", "snag" };
        /// <summary>The field rule: true = not drawn. Hash of the spot decides which of the sparse kinds stay, so the same
        /// ones stay at every recomposition.</summary>
        bool FieldHides(TW.Presentation.Terrain.BattlefieldKit.Module m, Vector3 p)
        {
            if (!fieldOn || !field.Contains(new Vector2(p.x, p.z))) return false;
            if (NearScatter(p)) return false;
            string l = Label(m);
            foreach (var k in KeepAll) if (l.Contains(k)) return false;
            foreach (var k in KeepSome) if (l.Contains(k)) { uint h = (uint)(Mathf.RoundToInt(p.x * 4f) * 73856093 ^ Mathf.RoundToInt(p.z * 4f) * 19349663); return (h % 1000) / 1000f >= fieldKeep; }
            return true;
        }
        void FieldCensus(TW.Presentation.Terrain.BattlefieldProps bp)
        {
            var fi = typeof(TW.Presentation.Terrain.BattlefieldProps).GetField("batches", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            var dict = fi != null ? fi.GetValue(bp) as System.Collections.IDictionary : null; if (dict == null) return;
            var found = new List<(int page, int slot, Matrix4x4 m)>(); var tally = new SortedDictionary<string, int>(); int hid = 0;
            foreach (var k in dict.Keys)
                if (k is TW.Presentation.Terrain.BattlefieldKit.Module mm)
                {
                    found.Clear(); bp.Within(mm, field.center, field.width, found);
                    foreach (var f in found)
                    {
                        var pos = f.m.GetPosition(); if (!field.Contains(new Vector2(pos.x, pos.z))) continue;
                        string l = Label(mm); tally.TryGetValue(l, out int c); tally[l] = c + 1;
                        if (!labelScale.ContainsKey(l)) labelScale[l] = f.m.lossyScale.y;
                        if (FieldHides(mm, pos)) { bp.Hide(mm, f.page, f.slot); hid++; }
                    }
                }
            Log("assault: field census " + string.Join(", ", tally.Select(kv => kv.Key + " " + kv.Value)) + $"; hidden {hid}");
        }
        bool InHidden(Vector3 p)
        {
            if (hideAt == null) return false;
            foreach (var h in hideAt) { float dx = p.x - h.x, dz = p.z - h.y; if (dx * dx + dz * dz < h.z * h.z) return true; }
            return false;
        }
        void Rehide()
        {
            if (hideAt == null) return;
            var bp = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>(); if (bp == null) return;
            if (fieldOn) AddScatter(bp);
            var fi = typeof(TW.Presentation.Terrain.BattlefieldProps).GetField("batches", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            var dict = fi != null ? fi.GetValue(bp) as System.Collections.IDictionary : null; if (dict == null) return;
            var found = new List<(int page, int slot, Matrix4x4 m)>();
            foreach (var k in dict.Keys)
                if (k is TW.Presentation.Terrain.BattlefieldKit.Module mm)
                {
                    foreach (var h in hideAt) { found.Clear(); bp.Within(mm, new Vector2(h.x, h.y), h.z, found); foreach (var f in found) if (!NearScatter(f.m.GetPosition())) bp.Hide(mm, f.page, f.slot); }
                    if (fieldOn && Time.frameCount % 15 == 0) { found.Clear(); bp.Within(mm, field.center, field.width, found); foreach (var f in found) if (FieldHides(mm, f.m.GetPosition())) bp.Hide(mm, f.page, f.slot); }
                }
        }

        static Texture2D LoadPng(string path)
        {
            if (!File.Exists(path)) return null;
            var t = new Texture2D(2, 2, TextureFormat.RGBA32, false) { wrapMode = TextureWrapMode.Clamp };
            t.LoadImage(File.ReadAllBytes(path));
            return t;
        }

        IEnumerator PartAssault(string opts)
        {
            string P(string k, string dflt) { var m = System.Text.RegularExpressions.Regex.Match(" " + opts + " ", @"\s" + k + @"=(\S+)"); return m.Success ? m.Groups[1].Value : dflt; }
            float F(string k, float d) => float.Parse(P(k, d.ToString(System.Globalization.CultureInfo.InvariantCulture)), System.Globalization.CultureInfo.InvariantCulture);
            float secs = F("secs", 25f), every = F("every", 1.5f), shellEvery = F("shells", 0.35f), hpMul = F("hp", 1.5f), killShare = F("kills", 0.25f);
            float zoom0 = F("zoom0", 34f), zoom1 = F("zoom1", 30f), pitch0 = F("pitch0", 52f), pitch1 = F("pitch1", 46f), yawC = F("yaw", 90f);
            float overTop = F("over", 0f), ahead0 = F("start", 8f), short1 = F("stop", 30f), barScale = F("bar", 0.955f);
            int squad = Mathf.RoundToInt(F("squad", 6f)); float tracersPerFrame = F("tracers", 3f), tracerCarry = 0f;     // fractional: the remainder carries to the next frame
            string matchPath = P("match", "");     // animatic_match.py's file: the take opens on the animatic's frame
            float zIn = F("zin", 1f), zCx = F("zcx", 0f), zCy = F("zcy", 600f), zSecs = F("zsecs", 3f);     // the opening frame's zoom (see PoseMatch)
            float zPre = F("zpre", 0f);
            bool zLin = P("zlin", "0") == "1";
            List<Vector3> zPath = null;     // zpath=file: per take frame "frame k ax ay" (see PoseMatch)
            if (P("zpath", "") != "")
            {
                zPath = new List<Vector3>();
                foreach (var zl in File.ReadAllLines(P("zpath", "")))
                {
                    var za = zl.Split(' '); if (za.Length < 4) continue;
                    zPath.Add(new Vector3(float.Parse(za[1], System.Globalization.CultureInfo.InvariantCulture), float.Parse(za[2], System.Globalization.CultureInfo.InvariantCulture), float.Parse(za[3], System.Globalization.CultureInfo.InvariantCulture)));
                }
                Log("assault: zpath " + zPath.Count + " frames, zpre " + zPre);
            }
            float camV = F("camv", 2.2f), runShare = F("runspeed", 0.6f), hudIn = F("hudin", 1.0f), early = F("lead", 0.35f);
            bool match = matchPath != "" && File.Exists(matchPath);
            float igniteLevel = F("ignite", 0.3f), fireScale = F("fires", 0f), shellR = F("shellr", 1f); Vector2 tankAt = new Vector2(-999f, -999f); int tankSlot = -1;
            string portraits = P("portraits", "C:/Users/PC/Documents/footage/portraits").Replace('\\', '/');

            // 2026-11-18 the officer clip's ground: river=0 drops the channel (its random draws are taken either way, so the
            // trenches, wire and craters stay put), shelling= raises the crater count, water= lowers the water table so the
            // shell holes stay dry. Through SimHost.FieldEdit (footage hook); cleared once the match is built.
            string riverOpt = P("river", ""), shellOpt = P("shelling", ""), waterOpt = P("water", "");
            if (P("flood", "") != "") TW.Presentation.Terrain.GreyboxTerrainView.FloodingOverride = F("flood", 0f);     // share of shell holes under water
            if (riverOpt != "" || shellOpt != "" || waterOpt != "")
            {
                SimHost.FieldEdit = fp =>
                {
                    if (riverOpt == "0") fp.River = false;
                    if (shellOpt != "") fp.Shelling = float.Parse(shellOpt, System.Globalization.CultureInfo.InvariantCulture);
                    if (waterOpt != "") fp.WaterLevel = float.Parse(waterOpt, System.Globalization.CultureInfo.InvariantCulture);
                    Log($"assault: field river {fp.River} shelling {fp.Shelling} water {fp.WaterLevel}");
                    return fp;
                };
            }
            yield return StartMatch(false, true);
            SimHost.FieldEdit = null;
            var host = Host; var hud = FindFirstObjectByType<HudController>(); var dir = GymDirector.Attach(host);
            var fx = FindFirstObjectByType<CombatFx>();
            var map = host.Local.Map; float X = map.SizeMeters.x, x0 = X * 0.5f;
            VfxLab.Rain(0f);
            host.WriteWorlds(m => { m.World.Silver[0] = 9000; });
            if (match)
            {
                GymDirector.Attach(host).Quiet();
                ParkMachines(host, -1);
            }
            float was = host.TimeScale; host.TimeScale = 3f;
            yield return Settle(match ? 12f : 6f, () => { });             // the enemy's garrison fills its trenches
            host.TimeScale = was;

            // ---- the two fire trenches facing each other: ours nearest the enemy, theirs nearest us
            const float cell = 2f;     // MapData.NavCellSize
            Vector2 CellPos(int idx) => new Vector2((idx % map.NavWidth + 0.5f) * cell, (idx / map.NavWidth + 0.5f) * cell);
            int oursT = -1, theirsT = -1; float ours = float.MinValue, theirs = float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var td = map.Trenches[t]; if (td.Kind != 0 || td.CellCount == 0) continue;
                float zs = 0f; for (int c = 0; c < td.CellCount; c++) zs += CellPos(map.TrenchCells[td.CellStart + c]).y;
                float zm = zs / td.CellCount;
                Log($"assault: trench {td.Id} team {td.OwnerTeam} cells {td.CellCount} mean z {zm:0.0}");
                if (td.OwnerTeam == 0 && zm > ours) { ours = zm; oursT = t; }
                if (td.OwnerTeam == 1 && zm < theirs) { theirs = zm; theirsT = t; }
            }
            if (oursT < 0 || theirsT < 0) { Log("assault: no facing trenches"); yield break; }
            if (match)
            {
                // the map keeps machine husks (wreck props) where it put them: stage the frame on the stretch of our trench
                // nearest the middle with none in view (the lens sees about 35 m either side and 70 m ahead)
                var husks = new List<Vector2>();
                for (int i = 0; i < map.Props.Length; i++) if (TW.Sim.Terrain.PropRules.IsWreckage(map.Props[i].Kind)) husks.Add(new Vector2(map.Props[i].Pos.x, map.Props[i].Pos.z));
                float bestX = x0, bestScore = float.MinValue;
                for (float cx = 40f; cx <= X - 40f; cx += 2f)
                {
                    float gap = 999f;
                    foreach (var hk in husks) if (hk.y > ours - 4f && hk.y < ours + 75f) gap = Mathf.Min(gap, Mathf.Abs(hk.x - cx) - 40f * Mathf.InverseLerp(ours + 75f, ours, hk.y) - 8f);
                    float score = Mathf.Min(gap, 30f) - Mathf.Abs(cx - x0) * 0.05f;
                    if (score > bestScore) { bestScore = score; bestX = cx; }
                }
                Log($"assault: {husks.Count} husks on the map, frame centred on x {bestX:0} (was {x0:0}), score {bestScore:0.0}");
                x0 = bestX;
                if (P("cx", "") != "") { x0 = F("cx", x0); Log($"assault: frame centre pinned to x {x0:0}"); }     // keep a framing the cut is matched to
                HideHusks(new Vector2(x0, ours + 30f), 60f);
            }
            List<Vector2> Cells(int t, float halfWidth)
            {
                var td = map.Trenches[t]; var l = new List<Vector2>();
                for (int c = 0; c < td.CellCount; c++) { var p = CellPos(map.TrenchCells[td.CellStart + c]); if (Mathf.Abs(p.x - x0) <= halfWidth) l.Add(p); }
                l.Sort((a, b) => a.x.CompareTo(b.x));
                return l;
            }
            var ourCells = Cells(oursT, 34f); var theirCells = Cells(theirsT, 48f);
            Log($"assault: ours z {ours:0.0} ({ourCells.Count} cells in view), theirs z {theirs:0.0} ({theirCells.Count}), shore {map.ShoreZ:0.0}");

            // ---- the enemy's line: held, unkillable riflemen (an MG in every fifth place) in their trench, one every 3.4 m
            var foes = new List<int>(); float lastX = float.MinValue; int nf = 0;
            foreach (var p in theirCells)
            {
                if (p.x - lastX < 3.4f) continue; lastX = p.x;
                int sl = SpawnHeld(dir, host, 1, nf++ % 5 == 2 ? 2 : 0, p.x, p.y); if (sl >= 0) foes.Add(sl);
            }
            Log("assault: enemy line " + foes.Count);

            // ---- a squad over the top: frogs out of our trench round a random x, each running for the enemy trench ahead of him
            var rnd = new System.Random(1917);
            var frogs = new List<int>(); var goals = new Dictionary<int, int>(); var gens = new Dictionary<int, ushort>();
            void Squad()
            {
                if (ourCells.Count == 0) return;
                int mid = rnd.Next(ourCells.Count);
                var camN = Camera.main;
                if (camN != null)
                {
                    var seen = new List<int>();
                    for (int q = 0; q < ourCells.Count; q++)
                    {
                        var vp = camN.WorldToViewportPoint(new Vector3(ourCells[q].x, map.Height.Sample(ourCells[q].x, ourCells[q].y + overTop + 1f), ourCells[q].y + overTop + 1f));
                        if (vp.z > 0 && vp.x > 0.2f && vp.x < 0.8f && vp.y > 0.3f && vp.y < 0.9f) seen.Add(q);
                    }
                    if (seen.Count > 0) mid = seen[rnd.Next(seen.Count)];
                }
                for (int k = 0; k < squad; k++)
                {
                    // just over our parapet: a man left inside the trench is taken into its garrison and sent to the rear line
                    var at = ourCells[Mathf.Clamp(mid + (k - squad / 2) * 2, 0, ourCells.Count - 1)] + new Vector2((float)rnd.NextDouble() - 0.5f, overTop + (float)rnd.NextDouble() * 1.5f);
                    int sl = dir.Spawn(0, (int)InfantryArchetype.Frog, at.x, at.y, 0f);
                    if (sl < 0) continue;
                    // the game's own over-the-top order (TrenchOrders.Advance), man by man: the goal past our fire trench, out of
                    // the trench, exposed; the UnitLeftTrench event plays the vault over the parapet
                    short tid = (short)oursT;
                    host.WriteWorlds(m =>
                    {
                        var w = m.World; w.Hp[sl] *= hpMul; w.MaxHp[sl] *= hpMul;
                        int g = m.Fields.NextGoalFrom(tid, 0);
                        if (g < 0) return;
                        w.GoalId[sl] = g; w.TrenchId[sl] = -1; w.SourceTrench[sl] = tid;
                        w.Flags[sl] = w.Flags[sl] | (uint)UnitFlags.Exposed;
                        w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, sl, tid, w.Position[sl]);
                        goals[sl] = g;
                    });
                    frogs.Add(sl); gens[sl] = host.Local.World.Generation[sl];
                }
            }

            // ---- the cards: the five portraits on the bar, infantry first, the first machine gets the tank
            var cards = hud.Refs.Cards.Where(c => !c.IsSupport && c.Root != null).ToList();
            var inf = new List<CardRefs>(); CardRefs tankCard = null;
            foreach (var c in cards)
            {
                bool armoured = ChassisKind.IsArmoured(host.Local.World.ChassisOf(c.Archetype));
                Log($"assault: card slot {c.Slot} '{c.Label}' arch {c.Archetype} armoured {armoured} unlocked {host.Local.World.SlotUnlocked[c.Slot]}");
                if (armoured) { if (tankCard == null) tankCard = c; } else inf.Add(c);
            }
            void Put(CardRefs c, string n)
            {
                var tex = LoadPng(portraits + "/" + n + ".png");
                if (c == null || tex == null) { Log("assault: no portrait " + n); return; }
                c.Portrait.style.backgroundImage = new StyleBackground(Background.FromTexture2D(tex));
                c.Portrait.style.unityBackgroundImageTintColor = Color.white;
            }
            string[] infNames = { "Rifleman", "Gasmask", "Piper", "Officer" };
            for (int i = 0; i < inf.Count && i < infNames.Length; i++) Put(inf[i], infNames[i]);
            Put(tankCard, "Tank");
            foreach (var c in inf) if ((c.Label ?? "").ToUpperInvariant().Contains("ENGINEER")) Put(c, "Engineer");
            foreach (var c in inf) if ((c.Label ?? "").ToUpperInvariant().Contains("SHIELD")) Put(c, "Shield");
            var pressable = inf.Take(Mathf.Clamp(Mathf.RoundToInt(F("presscards", 1f)), 1, infNames.Length)).ToList();
            if (pressable.Count == 0) { Log("assault: no infantry card"); yield break; }

            // ---- the pointer, drawn by the HUD's own panel so it lands in the composed frame
            var panelRoot = hud.Refs.Root.panel.visualTree;
            cursor?.RemoveFromHierarchy();
            cursor = new VisualElement { name = "footage-cursor", pickingMode = PickingMode.Ignore };
            var ctex = LoadPng(portraits + "/cursor.png");
            if (ctex != null) cursor.style.backgroundImage = new StyleBackground(Background.FromTexture2D(ctex));
            cursor.style.position = Position.Absolute; cursor.style.width = 30; cursor.style.height = 30;
            cursor.style.transformOrigin = new TransformOrigin(0, 0, 0);
            panelRoot.Add(cursor);
            Vector2 CardPoint(CardRefs c) { var b = c.Root.worldBound; return new Vector2(b.center.x - 4f, b.center.y - 2f); }
            Vector2 cur = CardPoint(pressable[0]) + new Vector2(140f, -160f);
            void PlaceCursor(Vector2 p, float s) { cursor.style.left = p.x; cursor.style.top = p.y; cursor.style.scale = new Scale(new Vector3(s, s, 1f)); cursor.BringToFront(); }
            PlaceCursor(cur, 1f);
            if (hud.Refs.Bar != null) hud.Refs.Bar.style.scale = new Scale(new Vector3(barScale, barScale, 1f));     // the bar's outer cards fall just off a 16:9 frame

            // a press: the card sinks, silver goes, a short cooldown sweeps the card, and a squad goes over the top
            var glows = new List<(VisualElement e, int start, bool label, Vector2 at)>(); int fxFrame = 0;
            float glowSecs = F("glow", 0.7f);
            Color gold = new Color(1f, 0.78f, 0.3f);
            void Glow(CardRefs c)
            {
                var g = new VisualElement { name = "footage-glow", pickingMode = PickingMode.Ignore };
                g.style.position = Position.Absolute; g.style.left = -3; g.style.top = -3; g.style.right = -3; g.style.bottom = -3;
                g.style.borderTopWidth = g.style.borderBottomWidth = g.style.borderLeftWidth = g.style.borderRightWidth = 4;
                g.style.borderTopLeftRadius = g.style.borderTopRightRadius = g.style.borderBottomLeftRadius = g.style.borderBottomRightRadius = 8;
                c.Root.Add(g); glows.Add((g, fxFrame, false, Vector2.zero));
                var b = c.Root.worldBound;
                var l = new Label("+" + squad) { name = "footage-plus", pickingMode = PickingMode.Ignore };
                l.style.position = Position.Absolute; l.style.fontSize = 34; l.style.unityFontStyleAndWeight = FontStyle.Bold;
                l.style.color = gold; l.style.unityTextOutlineColor = new Color(0.15f, 0.08f, 0f); l.style.unityTextOutlineWidth = 1.5f;
                panelRoot.Add(l); glows.Add((l, fxFrame, true, new Vector2(b.center.x - 22f, b.yMin - 10f)));
            }
            // each frame: the glow fades from full to nothing, the label rises and fades
            void GlowTick()
            {
                fxFrame++;
                for (int k = glows.Count - 1; k >= 0; k--)
                {
                    var (e, st, isLabel, at) = glows[k];
                    float u = (fxFrame - st) / (glowSecs * Fps * (isLabel ? 1.6f : 1f));
                    if (u >= 1f) { e.RemoveFromHierarchy(); glows.RemoveAt(k); continue; }
                    float a = 1f - u * u;
                    if (isLabel) { e.style.left = at.x; e.style.top = at.y - 70f * Mathf.Sqrt(u); e.style.opacity = a; e.BringToFront(); }
                    else
                    {
                        var bc = new Color(gold.r, gold.g, gold.b, a);
                        e.style.borderTopColor = e.style.borderBottomColor = e.style.borderLeftColor = e.style.borderRightColor = bc;
                        e.style.backgroundColor = new Color(1f, 0.85f, 0.45f, 0.38f * a);
                    }
                }
            }
            void Press(CardRefs c)
            {
                int slot = c.Slot; int cost = c.BaseCost;
                host.WriteWorlds(m => { var w = m.World; w.Silver[0] = Mathf.Max(w.Silver[0] - cost, 0) < 600 ? 4000 : w.Silver[0] - cost; w.SlotCooldown[slot] = 0; });
                Glow(c);
                Squad();
                Log($"assault: press '{c.Label}' at t {host.Local.World.Tick}");
            }

            // ---- the camera's road: from behind our parapet toward theirs, eased; the look is +z (yaw 90 from the base)
            // near-linear (a soft start and stop) so the camera keeps the pace of the running wave, ~2.6 m/s on this map
            Vector2 Focus(float t) { float u = Mathf.Lerp(t, t * t * (3f - 2f * t), 0.35f); return new Vector2(x0, Mathf.Lerp(ours + ahead0, theirs - short1, u)); }
            // the animatic's lens, mapped onto our trench: animatic +x (toward the enemy) = game +z from the parapet, animatic z
            // (across, screen right) = game +x from the map's middle, animatic y = up from the ground in front of the parapet
            Vector3 camP = Vector3.zero, camL = Vector3.zero; float camFov = 32f, gy = 0f;
            float ParapetZ(float gx)
            {
                float best = float.MaxValue, z = ours;
                foreach (var q in ourCells) { float d = Mathf.Abs(q.x - gx); if (d < best) { best = d; z = q.y; } }
                return z;
            }
            const float TrenchA = -1.5f;
            Vector3 Map3(float ax, float ay, float az, bool local = true)
            {
                float gx = x0 + az; float zt = local ? ParapetZ(gx) : ParapetZ(x0);
                return new Vector3(gx, gy + ay, zt + (ax - TrenchA));
            }
            void PoseMatch(float seconds, bool settle = true)
            {
                var tc = Cam; var cam = Camera.main; if (tc == null || cam == null) return;
                tc.enabled = false; poseOff = tc;
                SceneHooks.CloseUp = tc.CloseUpAt(tc.NearZoom);     // true size, as the animatic draws its frogs
                Shader.SetGlobalFloat("_TWClose", SceneHooks.CloseUp);
                // forward along +z: eased in over 2 s from the animatic's near standstill, then camv m/s
                float cs = settle ? Mathf.Max(0f, seconds - zPre) : seconds;
                const float ramp = 2f; float d = cs < ramp ? camV * cs * cs / (2f * ramp) : camV * (cs - ramp * 0.5f);
                // zlin=1 (11-18 owner: the camera already moving forward at the transition): full speed from the first frame,
                // at the match pose at zpre seconds (behind it before, ahead after)
                if (settle && zLin) d = camV * (seconds - zPre);
                var fw = new Vector3(0f, 0f, d);
                var rot0 = Quaternion.LookRotation((camL + fw) - (camP + fw), Vector3.up);
                float fov = camFov; var rot = rot0;
                if (settle && zPath != null)
                {
                    // the view is the match frame magnified k about the screen centre, aimed through match pixel (ax, ay)
                    float zf = Mathf.Clamp(seconds * Fps, 0f, zPath.Count - 1.001f); int zi = (int)zf; float zw = zf - zi;
                    var zv = Vector3.Lerp(zPath[zi], zPath[Mathf.Min(zi + 1, zPath.Count - 1)], zw);
                    float ty = Mathf.Tan(camFov * 0.5f * Mathf.Deg2Rad), aspect = 1920f / 1080f;
                    var dirCam = new Vector3((2f * zv.y / 1920f - 1f) * ty * aspect, (1f - 2f * zv.z / 1080f) * ty, 1f);
                    rot = Quaternion.LookRotation(rot0 * dirCam, Vector3.up);
                    fov = 2f * Mathf.Atan(ty / zv.x) * Mathf.Rad2Deg;
                }
                else if (settle && zIn > 1.0001f)
                {
                    // the opening frame is the match frame scaled by k about pixel c: a lens tan(fov/2)/k aimed through the
                    // match frame's pixel c + (centre - c)/k. k eases from zIn to 1 (log scale, smootherstep) over zSecs.
                    float u = Mathf.Clamp01(seconds / Mathf.Max(0.01f, zSecs)), e = u * u * u * (u * (u * 6f - 15f) + 10f);
                    float k = Mathf.Pow(zIn, 1f - e);
                    float ty = Mathf.Tan(camFov * 0.5f * Mathf.Deg2Rad), aspect = 1920f / 1080f;
                    float px = zCx + (960f - zCx) / k, py = zCy + (540f - zCy) / k;
                    var dirCam = new Vector3((2f * px / 1920f - 1f) * ty * aspect, (1f - 2f * py / 1080f) * ty, 1f);
                    rot = Quaternion.LookRotation(rot0 * dirCam, Vector3.up);
                    fov = 2f * Mathf.Atan(ty / k) * Mathf.Rad2Deg;
                }
                cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
                cam.transform.SetPositionAndRotation(camP + fw, rot);
            }
            void Pose(float t)
            {
                float u = t * t * (3f - 2f * t);
                PoseCam(Focus(t), Mathf.Lerp(zoom0, zoom1, u), yawC + Mathf.Sin(t * Mathf.PI) * 3f, Mathf.Lerp(pitch0, pitch1, u));
            }

            // extra fire: rounds from the enemy parapet into the charge, and from the charge back at the parapet
            Vector3 At(int sl, float up) { var p = host.Local.World.Position[sl]; return new Vector3(p.x, map.Height.Sample(p.x, p.z) + up, p.z); }
            // alive and still ours: a dead man's slot is soon reused by a landing enemy
            bool Live(int sl) { var w = host.Local.World; return sl >= 0 && sl < w.HighWater && (w.Flags[sl] & (uint)UnitFlags.Alive) != 0 && (gens.TryGetValue(sl, out var gg) ? w.Generation[sl] == gg : true); }
            void Tracers()
            {
                if (fx == null) return;
                var liveFrogs = frogs.Where(Live).ToList();
                tracerCarry += tracersPerFrame; int tn = Mathf.FloorToInt(tracerCarry); tracerCarry -= tn;
                for (int k = 0; k < tn; k++)
                {
                    bool theirsFire = k % 3 != 2;
                    if (foes.Count == 0) break;
                    var foe = At(foes[rnd.Next(foes.Count)], 1.3f);
                    Vector3 frog = liveFrogs.Count > 0 ? At(liveFrogs[rnd.Next(liveFrogs.Count)], 1.0f)
                                                       : new Vector3(x0 + (float)rnd.NextDouble() * 50f - 25f, 0f, ours + 4f);
                    var miss = new Vector3((float)rnd.NextDouble() * 6f - 3f, (float)rnd.NextDouble() * 1.5f - 0.5f, (float)rnd.NextDouble() * 6f - 3f);
                    if (theirsFire) fx.AddTracer(foe, frog + miss, 1, 0.7f, (float)rnd.NextDouble() * 0.03f);
                    else fx.AddTracer(frog + Vector3.up * 0.2f, foe + miss * 0.5f, 0, 0.7f, (float)rnd.NextDouble() * 0.03f);
                }
            }
            // the charge keeps going: enemy fire pins a man flat (suppression), so it is cleared on the frogs every few frames
            void Unpin()
            {
                var live = frogs.Where(Live).ToArray();
                host.WriteWorlds(m => { var w = m.World; foreach (var sl in live) { w.Suppression[sl] = 0f; if (goals.TryGetValue(sl, out int g)) w.GoalId[sl] = g; } });
            }
            void Report(string when)
            {
                var w = host.Local.World; var live = frogs.Where(Live).ToList();
                if (live.Count == 0) { Log("assault: " + when + " no frogs"); return; }
                float zs = 0f, sp = 0f, su = 0f, zmax = float.MinValue;
                foreach (var sl in live) { zs += w.Position[sl].z; zmax = Mathf.Max(zmax, w.Position[sl].z); var v = w.Velocity[sl]; sp += Mathf.Sqrt(v.x * v.x + v.z * v.z); su += w.Suppression[sl]; }
                Log($"assault: {when} frogs {live.Count}/{frogs.Count} mean z {zs / live.Count:0.0} lead z {zmax:0.0} speed {sp / live.Count:0.00} supp {su / live.Count:0.0}");
            }
            float shellClock = 0f;
            void ClearMachines() => ParkMachines(host, tankSlot);
            void Shells(float dt, float t)
            {
                shellClock += dt; if (shellClock < shellEvery) return; shellClock -= shellEvery * (0.6f + 0.8f * (float)rnd.NextDouble());
                var f = Focus(t);
                float x = x0 + (float)rnd.NextDouble() * 64f - 32f;
                // most fall in no man's land and on their parapet, a few among our own men
                float z = rnd.NextDouble() < 0.25 ? f.y + (float)rnd.NextDouble() * 10f - 4f : Mathf.Lerp(f.y + 4f, theirs + 3f, (float)rnd.NextDouble());
                float r = rnd.NextDouble() < 0.2 ? 7f : 4f + (float)rnd.NextDouble() * 2f;
                r *= shellR;
                bool amongUs = rnd.NextDouble() < killShare;
                if (amongUs) { var lf = frogs.Where(Live).ToList(); if (lf.Count > 0) { var pp = At(lf[rnd.Next(lf.Count)], 0f); x = pp.x + (float)rnd.NextDouble() * 4f - 2f; z = pp.z + (float)rnd.NextDouble() * 4f - 2f; } }
                if (Vector2.Distance(new Vector2(x, z), tankAt) < 10f) return;
                VfxLab.Shell(x, z, r, amongUs ? 260f : 3f);
            }

            if (match)
            {
                // ---- the animatic's frame: camera, burning tank, every frog where the animatic has him
                dir.Quiet();
                ClearMachines();
                gy = map.Height.Sample(x0, ParapetZ(x0) + 4f);
                var rows = File.ReadAllLines(matchPath);
                float Fl(string v) => float.Parse(v, System.Globalization.CultureInfo.InvariantCulture);
                foreach (var row in rows)
                {
                    var a = row.Split(' '); if (a.Length < 3) continue;
                    if (a[0] == "cam") { camP = Map3(Fl(a[1]), Fl(a[2]), Fl(a[3]), false); camL = Map3(Fl(a[4]), Fl(a[5]), Fl(a[6]), false); camFov = Fl(a[7]); }
                }
                Log($"assault: match cam {camP} -> {camL} fov {camFov:0.00}, ground {gy:0.00}, parapet z {ParapetZ(x0):0.0}");
                for (int i = 0; i < 4; i++) { PoseMatch(0f, false); yield return Tick(); }
                fieldOn = P("field", "open") == "open"; fieldKeep = F("keep", 0f);
                scatter.Clear(); scatterGen = -1; hogH = F("hogh", 1.5f); stumpH = F("stumph", 3.0f);
                foreach (var row in rows)
                {
                    var a = row.Split(' ');
                    if (a[0] != "hog" && a[0] != "stump") continue;
                    var gp = Map3(Fl(a[1]), 0f, Fl(a[2]));
                    if (P("props", "") != "") continue;     // the officer frame's marks replace the animatic's scatter
                    scatter.Add((a[0], gp, -Fl(a[3]) * Mathf.Rad2Deg, Fl(a[4])));     // three.js turns the other way round y
                }
                splitH = F("splith", 1.8f); postH = F("posth", 1.6f); wireH = F("wireh", 1.2f); snagH = F("snagh", 5f); logH = F("logh", 0.7f); boardsH = F("boardsh", 1.4f);
                string propsPath = P("props", "");
                if (propsPath != "" && File.Exists(propsPath))
                {
                    var bpG = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>(); var camM = Camera.main;
                    float H(Vector3 q) => bpG != null ? bpG.Ground(q.x, q.z) : map.Height.Sample(q.x, q.z);
                    Vector3? Drop(float px, float py)
                    {
                        // marks are 1920x1080 game pixels, top-left origin, seen from the match camera at t = 0
                        var ray = camM.ViewportPointToRay(new Vector3(px / 1920f, 1f - py / 1080f, 0f));
                        float prev = 0f;
                        for (float t = 1f; t < 600f; t += 0.5f)
                        {
                            var q = ray.GetPoint(t);
                            if (q.y > H(q)) { prev = t; continue; }
                            float lo = prev, hi = t;
                            for (int it = 0; it < 20; it++) { float mid = (lo + hi) * 0.5f; var m = ray.GetPoint(mid); if (m.y <= H(m)) hi = mid; else lo = mid; }
                            var g = ray.GetPoint(hi); g.y = H(g); return g;
                        }
                        return null;
                    }
                    int marks = 0, missed = 0;
                    foreach (var row in File.ReadAllLines(propsPath))
                    {
                        var a = row.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries); if (a.Length < 3 || a[0].StartsWith("#")) continue;
                        if (a[0] == "wire" && a.Length >= 5)
                        {
                            var w0 = Drop(Fl(a[1]), Fl(a[2])); var w1 = Drop(Fl(a[3]), Fl(a[4]));
                            if (w0 == null || w1 == null) { missed++; continue; }
                            var d = w1.Value - w0.Value; d.y = 0f; float len = d.magnitude, step = a.Length > 5 ? Fl(a[5]) : 2.6f;
                            float yaw = Mathf.Atan2(d.x, d.z) * Mathf.Rad2Deg;
                            for (float u = 0f; u <= len; u += step) { var q = Vector3.Lerp(w0.Value, w1.Value, len > 0f ? u / len : 0f); q.y = H(q); scatter.Add(("wire", q, yaw, 1.3f)); marks++; }
                            scatter.Add(("post", w0.Value, yaw, 1.3f)); scatter.Add(("post", w1.Value, yaw, 1.3f));
                            continue;
                        }
                        var at = Drop(Fl(a[1]), Fl(a[2])); if (at == null) { missed++; continue; }
                        scatter.Add((a[0], at.Value, a.Length > 3 ? Fl(a[3]) : 0f, a.Length > 4 ? Fl(a[4]) : 1.3f)); marks++;
                    }
                    Log($"assault: props {propsPath}: {marks} placed, {missed} missed the ground");
                }
                // the animatic's lighter night: the haze lifted to grey-blue, no umber shadow grade, the puddles dark glass
                foreach (var kv in P("look", "look.lift:1,look.grade:0,look.puddleSky:0.25").Split(','))
                { var ab = kv.Split(':'); if (ab.Length == 2) { Knobs.Set(ab[0], ab[1]); Log("assault: knob " + ab[0] + " = " + ab[1]); } }
                atmo = FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
                if (atmo != null) { atmoStart = atmo.StartFactor; atmoDepth = atmo.Depth; atmo.StartFactor = F("fogstart", 2.4f); atmo.Depth = F("fogdepth", 650f); }
                // 2026-11-18 lut=<png strip 1024x32>: the officer clip's colour and contrast, fitted offline (tw3d-opening-cut/tools/fitlut.py),
                // added to the night grade's volume as a ColorLookup. By reflection: TW.Editor does not reference the Core runtime.
                if (P("lut", "") != "" && atmo != null) Log("assault: lut " + AddLut(atmo, P("lut", ""), F("lutmix", 1f)));
                field = Rect.MinMaxRect(x0 - 95f, ours - 8f, x0 + 95f, theirs + 70f);
                { var bp0 = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>(); if (bp0 != null && fieldOn) { FieldCensus(bp0); AddScatter(bp0); } }
                hideAt = new[] { new Vector3(x0, ParapetZ(x0) + 18f, 36f), new Vector3(x0 - 14f, ParapetZ(x0) + 26f, 30f), new Vector3(x0 + 14f, ParapetZ(x0) + 26f, 30f) };
                Rehide();
                // a crater recomposes the props inside BattlefieldProps.Update and draws them that frame: filter the regions at
                // composition too, chained ahead of whatever suppresses already
                { var bp = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
                  if (bp != null && suppressWas == null) { suppressBp = bp; suppressWas = bp.Suppress ?? new System.Func<TW.Presentation.Terrain.BattlefieldKit.Module, Matrix4x4, bool>((m, mx) => false); var prev = suppressWas;
                      bp.Suppress = (m, mx) => prev(m, mx) || InHidden(mx.GetPosition()) || FieldHides(m, mx.GetPosition()); } }
                // the tank first, so it is well alight at the cut
                foreach (var row in rows)
                {
                    var a = row.Split(' ');
                    if (a[0] != "tank") continue;
                    var tp = Map3(Fl(a[1]), 0f, Fl(a[2]));
                    string sp = "";
                    for (int k = 0; k < 8 && !sp.StartsWith("slot "); k++) { sp = TankCapture.Spawn(1, 21, tp.x, tp.z, 180f); if (!sp.StartsWith("slot ")) yield return Tick(); }     // 21 = the Brute, the animatic's tank
                    Log("assault: tank " + sp + " at " + tp);
                    if (sp.StartsWith("slot "))
                    {
                        int tsl = int.Parse(sp.Substring(5)); tankSlot = tsl; RiderLab.Stop(tsl);
                        var want = tp; host.WriteWorlds(m => { var w = m.World; w.Position[tsl] = new float3(want.x, w.Position[tsl].y, want.z); w.Yaw[tsl] = Mathf.PI; });
                        for (int k = 0; k < 2; k++) yield return Tick();
                        Log("assault: tank asked " + tp + " is at " + host.Local.World.Position[tsl]);
                        host.WriteWorlds(m => { m.World.Hp[tsl] = 1e6f; m.World.MaxHp[tsl] = 1e6f; });
                        for (int k = 0; k < 4; k++) yield return Tick();
                        yield return Retry(() => TankCapture.Ignite(tsl, igniteLevel), "assault ignite"); tankAt = new Vector2(tp.x, tp.z);
                    }
                }
                foreach (var row in rows)
                {
                    var a = row.Split(' ');
                    if (a[0] != "fire" || fireScale <= 0f) continue;
                    var fp = Map3(Fl(a[1]), 0f, Fl(a[2])); var fr = Fl(a[3]) * fireScale;
                    yield return Retry(() => VfxLab.Fire(fp.x, fp.z, fr), "assault fire");
                }
                bool flares = P("flare", "1") == "1";
                if (flares) { Log("assault: flare " + VfxLab.Flare()); }     // a star shell lights the field ~16 s: the animatic's field is lit
                for (int i = 0; i < Mathf.RoundToInt(1.5f * Fps); i++) { PoseMatch(0f); yield return Tick(); }
                // the frogs: spawned `lead` seconds early, that much further back, so they arrive on their marks running
                short tid = (short)oursT; int nr = 0, nd = 0;
                foreach (var row in rows)
                {
                    var a = row.Split(' ');
                    if (a[0] != "run" && a[0] != "climb" && a[0] != "crouch" && a[0] != "dead") continue;
                    float ax = Fl(a[1]), az = Fl(a[2]), spd = a.Length > 3 ? Fl(a[3]) : 6.6f;
                    if (a[0] == "dead") { var dp = Map3(ax, 0f, az); dir.Death(DeathKind.Shot, (int)InfantryArchetype.Frog, new Vector2(dp.x, dp.z)); nd++; continue; }
                    float gs = spd * runShare;
                    var gp = Map3(ax - gs * early, 0f, az);
                    int sl = dir.Spawn(0, (int)InfantryArchetype.Frog, gp.x, gp.z, 0f);
                    if (sl < 0) continue;
                    host.WriteWorlds(m =>
                    {
                        var w = m.World; w.Hp[sl] *= hpMul; w.MaxHp[sl] *= hpMul; w.Speed[sl] = gs;
                        int g = m.Fields.NextGoalFrom(tid, 0); if (g < 0) return;
                        w.GoalId[sl] = g; w.TrenchId[sl] = -1; w.SourceTrench[sl] = tid; w.Flags[sl] = w.Flags[sl] | (uint)UnitFlags.Exposed;
                        goals[sl] = g;
                    });
                    frogs.Add(sl); gens[sl] = host.Local.World.Generation[sl]; nr++;
                }
                Log($"assault: match frogs {nr} running, {nd} dead");
                { var w = host.Local.World; for (int i = 0; i < w.HighWater; i++) if ((w.Flags[i] & (uint)UnitFlags.Alive) != 0 && (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) Log($"assault: machine slot {i} team {w.Team[i]} arch {w.Archetype[i]} at {w.Position[i]}"); }
                { var tr = FindFirstObjectByType<TankRenderer>(); Log("assault: tank renderer " + (tr != null ? tr.name : "none")); }
                int leadFrames = Mathf.Max(2, Mathf.RoundToInt(early * Fps));
                for (int i = 0; i < leadFrames; i++)
                {
                    PoseMatch(0f); Tracers(); Unpin(); Rehide();
                    if (i == leadFrames - 2) { uiAlpha = 0f; pendingPath = outDir + "/_warm.jpg"; pending = true; }
                    yield return Tick();
                }
            }
            else
            {
            // ---- pre-roll: the first waves are already out when the take opens
            int pre = Mathf.RoundToInt(5f * Fps);
            for (int i = 0; i < pre; i++)
            {
                if (i % Mathf.RoundToInt(1.2f * Fps) == 0) Squad();
                Pose(0f); Tracers(); Shells(1f / Fps, 0f);
                if (i % 4 == 0) Unpin();
                if (i == pre - 2) { pendingPath = outDir + "/_warm.jpg"; pending = true; }     // the UI panels attach on the first grab: warm them before the take
                yield return Tick();
            }
            ClearProps(Focus(0.5f), 6f, 2f);
            }

            Seg("assault_" + Mathf.RoundToInt(secs) + "s", "OVER THE TOP", true);
            int n = Mathf.Max(1, Mathf.RoundToInt(secs * Fps)), per = Mathf.Max(8, Mathf.RoundToInt(every * Fps));
            int move = Mathf.RoundToInt(0.45f * Fps), down = Mathf.RoundToInt(0.12f * Fps);
            int pi = 0; Vector2 from = cur;
            rec = true;
            for (int i = 0; i < n; i++)
            {
                float t = n == 1 ? 1f : i / (float)(n - 1);
                // the press cycle: glide to the next card, press, hold the card down a moment, rest
                float sec = i / (float)Fps;
                if (match) uiAlpha = Mathf.Clamp01((sec - hudIn) / 0.5f);
                int ip = match ? i - Mathf.RoundToInt((hudIn + 0.3f) * Fps) : i;
                if (match && i % Fps == 0) ClearMachines();
                if (match && P("flare", "1") == "1" && i == Mathf.RoundToInt(12f * Fps)) Log("assault: flare " + VfxLab.Flare());
                if (match) Rehide();
                if (ip < 0) { PoseMatch(sec); Tracers(); Shells(1f / Fps, t); if (i % 4 == 0) Unpin(); yield return Tick(); continue; }
                int ph = ip % per; var target = pressable[pi % pressable.Count];
                if (ph == 0) from = cur;
                if (ph <= move) { float u = ph / (float)move; u = u * u * (3f - 2f * u); cur = Vector2.Lerp(from, CardPoint(target), u); PlaceCursor(cur, 1f); }
                else if (ph <= move + down)
                {
                    PlaceCursor(cur, 0.85f);
                    target.Root.style.scale = new Scale(new Vector3(1.05f, 1.05f, 1f));
                    if (ph == move + 1) Press(target);
                }
                else if (ph == move + down + 1) { PlaceCursor(cur, 1f); target.Root.style.scale = StyleKeyword.Null; pi++; }
                if (i % (Fps * 2) == 0) hud.OrderFront(CommandType.TrenchAdvance);
                if (i % Fps == 0) Report("t " + (i / (float)Fps).ToString("0")); 
                if (i % 4 == 0) Unpin();
                if (match) PoseMatch(sec); else Pose(t);
                Tracers(); Shells(1f / Fps, t); GlowTick();
                yield return Tick();
            }
            rec = false;
            foreach (var gl in glows) gl.e.RemoveFromHierarchy(); glows.Clear();
            int alive = frogs.Count(Live);
            Log($"assault: done, frogs {frogs.Count} spawned, {alive} alive, enemy line {foes.Count(Live)}");
            foreach (var c in cards) c.Root.style.scale = StyleKeyword.Null;
            cursor?.RemoveFromHierarchy(); cursor = null; uiAlpha = 1f; hideAt = null; fieldOn = false; scatter.Clear();
            foreach (var kv in maxDist) kv.Key.MaxDistance = kv.Value; maxDist.Clear();
            RemoveLut(); SimHost.FieldEdit = null; TW.Presentation.Terrain.GreyboxTerrainView.FloodingOverride = -1f;
            if (atmo != null) { atmo.StartFactor = atmoStart; atmo.Depth = atmoDepth; atmo = null; }
            if (suppressBp != null) { suppressBp.Suppress = suppressWas; suppressBp = null; suppressWas = null; }
            ClearStill(host); dir.Clear(); PoseRelease();
        }
    }
}
