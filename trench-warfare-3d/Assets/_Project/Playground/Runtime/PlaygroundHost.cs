// Phase: Playground (2026-09-26, lane/show/playground) — the playground scene, its panel and its command strings
// The playground: a test range where a new unit, vehicle or building is tried BEFORE it goes into the battle. It builds
// its own scene at Play (camera, the game's Atmosphere for the battle's night or winter look, a ground with a metre grid,
// the game's FlipbookFx and DebrisRenderer) and puts the library's assets on it:
//   Vehicle      one vehicle, auto LOD; hit it, knock it out, cook it off, repair it
//   Vehicle LODs the vehicle three times side by side, forced to LOD0 / LOD1 / LOD2, driven by the SAME hits: the proof
//                that it falls apart the same way at every distance (PoseSignature compares them to the centimetre)
//   Unit         one figure, auto LOD, any of the game's clips; kill it, set it alight
//   Unit LODs    the figure four times, LOD0..LOD3, same clip and time: the rig works on every LOD
//   Squad        figures in file from 8 m to 300 m, auto LOD: where each LOD takes over at the standard view
//   Mixed        the vehicle among a squad: the scale of the one against the other
// Everything the panel does is a command string, so a script (or `tw eval`) can drive it: host.Queue("seq").
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using TW.Presentation.Terrain;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.Rendering.Universal;

namespace TW.Playground
{
    [DefaultExecutionOrder(30000)]
    public sealed class PlaygroundHost : MonoBehaviour
    {
        public PlaygroundLibrary Library;
        public Biome Field = Biome.NightMud;
        public string StartMode = "vehicle";
        public float VehicleSize = 1.7f;     // VehicleSize.Tank in the sim: the machines are drawn this much over the sculpt
        public float UnitScale = 1.125f;     // VATRenderer.UnitScale: a 1.78 m bake drawn as a 2.0 m man
        public bool ShowPanel = true;

        public static PlaygroundHost Instance { get; private set; }
        public PlaygroundCamera Cam { get; private set; }
        public PlaygroundFx Fx { get; private set; }
        public ClipDeck Deck { get; private set; }
        public readonly List<VehicleRig> Vehicles = new List<VehicleRig>();
        public readonly List<UnitRig> Units = new List<UnitRig>();
        public string Mode { get; private set; } = "";
        public string LastShot = "", LastJson = "", Status = "";

        readonly Queue<string> queue = new Queue<string>();
        readonly List<(float at, string cmd)> script = new List<(float, string)>();
        float scriptStart = -1f;
        Transform stage;
        Atmosphere atmosphere;
        GameObject atmosphereGo;
        Light key;
        int vehicleIndex, unitIndex, clip = -1, forcedLod = -1;
        float cookDelay = 7f;
        Vector2 clipScroll;
        int tab;
        string shotPath; int shotW, shotH;
        float fpsAvg = 60f;

        // ------------------------------------------------------------------------------------------------ setup
        void Awake()
        {
            Instance = this;
            if (Library == null) { Debug.LogError("PlaygroundHost: no library. Run TW/Playground/Build."); enabled = false; return; }
            stage = new GameObject("Stage").transform; stage.SetParent(transform, false);
            // light first: Atmosphere takes the directional light it finds at its Start
            var lg = new GameObject("Key"); lg.transform.SetParent(transform, false);
            key = lg.AddComponent<Light>(); key.type = LightType.Directional; key.shadows = LightShadows.Soft;
            var cg = new GameObject("PlaygroundCamera") { tag = "MainCamera" };
            cg.transform.SetParent(transform, false);
            var c = cg.AddComponent<Camera>();
            c.clearFlags = CameraClearFlags.SolidColor; c.backgroundColor = new Color(0.08f, 0.09f, 0.11f);
            var urp = cg.AddComponent<UniversalAdditionalCameraData>(); urp.renderPostProcessing = true; urp.antialiasing = AntialiasingMode.SubpixelMorphologicalAntiAliasing;
            Cam = cg.AddComponent<PlaygroundCamera>();
            var fg = new GameObject("Fx"); fg.transform.SetParent(transform, false);
            Fx = fg.AddComponent<PlaygroundFx>();
            BuildGround();
            SetBiome(Field);
            Deck = ClipDeck.Make(Library, transform);
            clip = Library.ClipIndex("Rifle Idle");
        }

        void Start() { Queue(StartMode); }

        void OnDestroy() { Deck?.Destroy(); if (Instance == this) Instance = null; Time.timeScale = 1f; }

        void SetBiome(Biome b)
        {
            Field = b;
            if (atmosphereGo != null) Destroy(atmosphereGo);
            atmosphereGo = new GameObject("Atmosphere (" + b + ")");
            atmosphereGo.transform.SetParent(transform, false);
            atmosphere = atmosphereGo.AddComponent<Atmosphere>();
            atmosphere.Field = b;
            Fx.Night = true;
        }

        void BuildGround()
        {
            // 800 m square, a texture with a line every metre and a stronger one every ten, tiled every ten metres
            const int N = 256;
            var tex = new Texture2D(N, N, TextureFormat.RGB24, true) { name = "PlaygroundGrid", wrapMode = TextureWrapMode.Repeat, anisoLevel = 8 };
            var px = new Color32[N * N];
            var rnd = new System.Random(3);
            for (int y = 0; y < N; y++)
                for (int x = 0; x < N; x++)
                {
                    float n = (float)rnd.NextDouble() * 0.05f;
                    float v = 0.46f + n;
                    bool metre = (x % (N / 10)) == 0 || (y % (N / 10)) == 0;
                    bool ten = x == 0 || y == 0 || x == N - 1 || y == N - 1;
                    if (metre) v -= 0.07f;
                    if (ten) v -= 0.16f;
                    px[y * N + x] = new Color(v * 0.86f, v * 0.80f, v * 0.68f);
                }
            tex.SetPixels32(px); tex.Apply(true);
            const float half = 400f, tile = 10f;
            var mesh = new Mesh { name = "PlaygroundGround" };
            int seg = 40;
            var verts = new List<Vector3>(); var uvs = new List<Vector2>(); var cols = new List<Color>(); var tris = new List<int>();
            for (int j = 0; j <= seg; j++)
                for (int i = 0; i <= seg; i++)
                {
                    float x = -half + 2f * half * i / seg, z = -half + 2f * half * j / seg;
                    verts.Add(new Vector3(x, 0f, z)); uvs.Add(new Vector2(x / tile, z / tile)); cols.Add(Color.white);
                }
            for (int j = 0; j < seg; j++)
                for (int i = 0; i < seg; i++)
                {
                    int a = j * (seg + 1) + i, b = a + 1, c = a + seg + 1, d = c + 1;
                    tris.Add(a); tris.Add(c); tris.Add(b); tris.Add(b); tris.Add(c); tris.Add(d);
                }
            mesh.SetVertices(verts); mesh.SetUVs(0, uvs); mesh.SetColors(cols); mesh.SetTriangles(tris, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds();
            var g = new GameObject("Ground"); g.transform.SetParent(transform, false);
            g.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r = g.AddComponent<MeshRenderer>();
            var m = new Material(Shader.Find("TW/Toon (URP)") ?? Shader.Find("Universal Render Pipeline/Lit")) { name = "PlaygroundGround" };
            m.SetTexture("_BaseMap", tex); m.mainTexture = tex;
            r.sharedMaterial = m; r.receiveShadows = true; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        // ------------------------------------------------------------------------------------------------ stage
        void Clear()
        {
            foreach (var v in Vehicles) if (v != null) Destroy(v.gameObject);
            foreach (var u in Units) if (u != null) Destroy(u.gameObject);
            Vehicles.Clear(); Units.Clear(); script.Clear(); scriptStart = -1f;
            Fx.ClearLamps(); Fx.ClearDebris();
        }

        VehicleRig SpawnVehicle(Vector3 at, float yaw, int lod, int seed)
        {
            if (Library.Vehicles.Length == 0) return null;
            var v = VehicleRig.Build(Library.Vehicles[vehicleIndex % Library.Vehicles.Length], Fx, stage, at, yaw, VehicleSize, seed);
            v.ForcedLod = lod; v.CookDelay = cookDelay;
            Vehicles.Add(v);
            return v;
        }

        UnitRig SpawnUnit(Vector3 at, float yaw, int lod, float phase = 0f)
        {
            if (Library.Units.Length == 0) return null;
            var u = UnitRig.Build(Library.Units[unitIndex % Library.Units.Length], Deck, Fx, stage, at, yaw, UnitScale);
            u.ForcedLod = lod;
            if (clip >= 0) u.Play(clip, 0f, phase);
            Units.Add(u);
            return u;
        }

        void Scene(string mode)
        {
            Clear(); Mode = mode;
            switch (mode)
            {
                // vehicles face the camera's side (-Z) turned a little, so the front three-quarter reads; LOD0 on the left
                case "vehicle":
                    SpawnVehicle(Vector3.zero, 205f, forcedLod, 1);
                    Cam.Preset("default", new Vector3(0f, 2.5f, 0f)); Cam.Distance = 24f; Cam.Yaw = 0f; Cam.Pitch = 18f; break;
                case "vehicle.compare":
                    for (int k = 0; k < 3; k++) SpawnVehicle(new Vector3((k - 1) * 15f, 0f, 0f), 205f, k, 1);
                    Cam.Preset("default", new Vector3(0f, 2.5f, 0f)); Cam.Distance = 50f; Cam.Yaw = 0f; Cam.Pitch = 18f; break;
                case "unit":
                    SpawnUnit(Vector3.zero, 20f, forcedLod);
                    Cam.Preset("close", new Vector3(0f, 1.1f, 0f)); Cam.Distance = 5.5f; Cam.Yaw = 180f; break;
                case "unit.compare":
                    // LOD0 on the left as the camera sees it (the camera looks -Z, so screen left is +X)
                    for (int k = 0; k < 4; k++) SpawnUnit(new Vector3((1.5f - k) * 2.4f, 0f, 0f), 15f, k);
                    Cam.Preset("close", new Vector3(0f, 1.1f, 0f)); Cam.Distance = 10f; Cam.Yaw = 180f; Cam.Pitch = 8f; break;
                case "unit.squad":
                    {
                        // a file of men walking away from the standard view's camera, from 8 m out to 300 m
                        float[] d = { 8, 14, 22, 32, 45, 60, 80, 105, 135, 170, 210, 255, 300 };
                        for (int i = 0; i < d.Length; i++) SpawnUnit(new Vector3((i % 2 == 0 ? -1.2f : 1.2f), 0f, d[i]), 200f, forcedLod, i * 0.37f);
                        Cam.Focus = new Vector3(0f, 1f, 0f); Cam.Fov = 25f; Cam.Pitch = 14f; Cam.Yaw = 0f; Cam.Distance = 10f;
                        break;
                    }
                case "mixed":
                    SpawnVehicle(new Vector3(0f, 0f, 0f), 205f, forcedLod, 1);
                    for (int i = 0; i < 12; i++) SpawnUnit(new Vector3(-9f + (i % 6) * 1.6f, 0f, -6f - (i / 6) * 2.2f), 20f, forcedLod, i * 0.21f);
                    Cam.Preset("standard", new Vector3(-2f, 1.5f, -3f)); Cam.Distance = 45f; Cam.Yaw = 200f; break;
            }
        }

        // ------------------------------------------------------------------------------------------------ commands
        public void Queue(string cmd) { lock (queue) queue.Enqueue(cmd); }

        static float F(string[] a, int i, float d) => a.Length > i && float.TryParse(a[i], NumberStyles.Float, CultureInfo.InvariantCulture, out var v) ? v : d;

        /// <summary>The deterministic destruction run, on every vehicle on the stage at once (see Vehicle LODs).</summary>
        void Sequence()
        {
            foreach (var v in Vehicles) v.Repair();
            script.Clear(); scriptStart = Time.time;
            script.Add((0.3f, "ap Plate_LF 30"));
            script.Add((1.4f, "ap Track_L 45"));
            script.Add((2.6f, "he right 38"));
            script.Add((3.8f, "ap Hull 50"));
        }

        public string Do(string line)
        {
            var a = line.Trim().Split(new[] { ' ' }, System.StringSplitOptions.RemoveEmptyEntries);
            if (a.Length == 0) return "";
            string c = a[0].ToLowerInvariant();
            switch (c)
            {
                case "vehicle": case "vehicle.compare": case "unit": case "unit.compare": case "unit.squad": case "mixed": Scene(c); break;
                case "lod":
                    forcedLod = (int)F(a, 1, -1);
                    if (Mode != "vehicle.compare" && Mode != "unit.compare") { foreach (var v in Vehicles) v.ForcedLod = forcedLod; foreach (var u in Units) u.ForcedLod = forcedLod; }
                    break;
                case "seq": Sequence(); break;
                case "ap":
                    foreach (var v in Vehicles)
                    {
                        v.HitPart(v.Find(a.Length > 1 ? a[1] : "Hull") ?? v.Find("Hull"), F(a, 2, 30f));
                    }
                    break;
                case "he":
                    foreach (var v in Vehicles)
                    {
                        float side = a.Length > 1 && a[1] == "left" ? -1f : 1f;
                        v.HitLocal(new Vector3(side * 4.2f, 0f, 0f), Vector3.down, F(a, 2, 35f), VehicleRig.HitKind.HE);
                    }
                    break;
                case "ko": foreach (var v in Vehicles) v.KnockOut(); break;
                case "cook": foreach (var v in Vehicles) v.CookOff(); break;
                case "fire": foreach (var v in Vehicles) v.FireGun(); break;
                case "repair": foreach (var v in Vehicles) v.Repair(); script.Clear(); break;
                case "traverse": foreach (var v in Vehicles) v.Traverse = !v.Traverse; break;
                case "cookdelay": cookDelay = F(a, 1, 7f); foreach (var v in Vehicles) v.CookDelay = cookDelay; break;
                case "size": VehicleSize = F(a, 1, 1.7f); Scene(Mode); break;
                case "clip":
                    {
                        int k = Library.ClipIndex(line.Substring(line.IndexOf(' ') + 1).Trim());
                        if (k >= 0) { clip = k; foreach (var u in Units) if (!u.Dead) u.Play(k); }
                        else return "no clip " + line;
                        break;
                    }
                case "speed": foreach (var u in Units) u.Speed = F(a, 1, 1f); break;
                case "kill": foreach (var u in Units) u.Kill(DeathClip(u)); break;
                case "ignite": foreach (var u in Units) u.Ignite(Library.ClipIndex("Burning Run"), DeathClip(u)); break;
                case "revive": foreach (var u in Units) u.Revive(clip); break;
                case "cam":
                    if (a.Length >= 8) { Cam.Focus = new Vector3(F(a, 1, 0), F(a, 2, 0), F(a, 3, 0)); Cam.Yaw = F(a, 4, 0); Cam.Pitch = F(a, 5, 20); Cam.Distance = F(a, 6, 20); Cam.Fov = F(a, 7, 35); }
                    else Cam.Preset(a.Length > 1 ? a[1] : "default", Cam.Focus);
                    break;
                case "freeze": Time.timeScale = F(a, 1, 1f) > 0.5f ? 0f : 1f; break;
                case "timescale": Time.timeScale = F(a, 1, 1f); break;
                case "biome": if (System.Enum.TryParse<Biome>(a.Length > 1 ? a[1] : "NightMud", true, out var b)) SetBiome(b); break;
                case "panel": ShowPanel = F(a, 1, 1f) > 0.5f; break;
                case "labels": Labels = F(a, 1, 1f) > 0.5f; break;
                case "shot": shotPath = a.Length > 1 ? a[1] : Path.Combine(Application.dataPath, "../Captures/playground.png"); shotW = (int)F(a, 2, 1600); shotH = (int)F(a, 3, 900); break;
                default: return "unknown command " + c;
            }
            return "ok " + line;
        }

        int DeathClip(UnitRig u)
        {
            string[] deaths = { "Rifle Death", "Death From The Front", "Death From Right", "Death From The Back" };
            int i = Mathf.Abs(u.GetInstanceID()) % deaths.Length;
            int k = Library.ClipIndex(deaths[i]);
            return k >= 0 ? k : Library.ClipIndex("Death");
        }

        // ------------------------------------------------------------------------------------------------ frame
        void Update()
        {
            lock (queue) while (queue.Count > 0) Status = Do(queue.Dequeue());
            if (scriptStart >= 0f)
            {
                float t = Time.time - scriptStart;
                for (int i = 0; i < script.Count; i++)
                    if (script[i].at <= t) { Do(script[i].cmd); script.RemoveAt(i); i--; }
                if (script.Count == 0) scriptStart = -1f;
            }
            var k = Keyboard.current;
            if (k != null)
            {
                if (k.f1Key.wasPressedThisFrame) ShowPanel = !ShowPanel;
                if (k.spaceKey.wasPressedThisFrame) Do("freeze " + (Time.timeScale > 0f ? "1" : "0"));
                if (k.hKey.wasPressedThisFrame) Do("ap Hull 25");
                if (k.gKey.wasPressedThisFrame) Do("fire");
            }
            // click on the vehicle: an AP round where the pointer is
            var m = Mouse.current;
            if (m != null && m.leftButton.wasPressedThisFrame && !Cam.PointerOverUi && Camera.main != null)
            {
                var ray = Camera.main.ScreenPointToRay(m.position.ReadValue());
                foreach (var v in Vehicles)
                    if (Vector3.Cross(ray.direction, v.Centre - ray.origin).magnitude < v.Radius)
                    { v.Hit(ray.origin + ray.direction * (Vector3.Distance(ray.origin, v.Centre) - v.Radius * 0.3f), ray.direction, 22f, VehicleRig.HitKind.AP); break; }
            }
            if (Time.unscaledDeltaTime > 0f) fpsAvg = Mathf.Lerp(fpsAvg, 1f / Time.unscaledDeltaTime, 0.05f);
        }

        void LateUpdate()
        {
            if (shotPath != null)
            {
                string path = shotPath; shotPath = null;
                Capture(path, shotW, shotH);
            }
        }

        void Capture(string path, int w, int h)
        {
            var c = Camera.main; if (c == null) return;
            var rt = RenderTexture.GetTemporary(w, h, 24, RenderTextureFormat.ARGB32);
            var before = c.targetTexture;
            c.targetTexture = rt; c.Render(); c.targetTexture = before;
            var was = RenderTexture.active; RenderTexture.active = rt;
            var tex = new Texture2D(w, h, TextureFormat.RGB24, false);
            tex.ReadPixels(new Rect(0, 0, w, h), 0, 0); tex.Apply();
            RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            File.WriteAllBytes(path, tex.EncodeToPNG());
            LastJson = Report(tex);
            File.WriteAllText(Path.ChangeExtension(path, ".json"), LastJson);
            Destroy(tex);
            LastShot = path;
        }

        /// <summary>What the frame holds, as numbers: brightness, and the state of everything on the stage.</summary>
        public string Report(Texture2D tex = null)
        {
            var sb = new StringBuilder("{");
            if (tex != null)
            {
                var px = tex.GetPixels32(); var lum = new float[px.Length]; double sum = 0; int blown = 0;
                for (int i = 0; i < px.Length; i++) { float l = (0.2126f * px[i].r + 0.7152f * px[i].g + 0.0722f * px[i].b) / 255f; lum[i] = l; sum += l; if (l > 0.9f) blown++; }
                System.Array.Sort(lum);
                sb.AppendFormat(CultureInfo.InvariantCulture, "\"luma_mean\":{0:0.0},\"luma_p95\":{1:0.0},\"blown_frac\":{2:0.0000},", sum / px.Length * 255f, lum[(int)(lum.Length * 0.95f)] * 255f, (float)blown / px.Length);
            }
            sb.AppendFormat(CultureInfo.InvariantCulture, "\"mode\":\"{0}\",\"time\":{1:0.00},\"fps\":{2:0},\"cards\":{3},\"biome\":\"{4}\",", Mode, Time.time, fpsAvg, Fx.CardsAlive, Field);
            sb.Append("\"vehicles\":[");
            for (int i = 0; i < Vehicles.Count; i++)
            {
                var v = Vehicles[i]; if (i > 0) sb.Append(',');
                int loose = 0; foreach (var p in v.Parts) if (p.Loose) loose++;
                var sv = Camera.main != null ? Camera.main.WorldToViewportPoint(v.Centre + Vector3.up * v.Radius * 0.8f) : Vector3.zero;
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"lod\":{0},\"tris\":{1},\"stage\":\"{2}\",\"hp\":{3:0},\"fire\":{4:0.00},\"loose\":{5},\"sx\":{7:0.000},\"sy\":{8:0.000},\"pose\":\"{6}\"}}", v.Lod, v.TrisDrawn, v.State, v.Hp, v.FireLevel, loose, v.PoseSignature(), sv.x, sv.y);
            }
            sb.Append("],\"units\":[");
            for (int i = 0; i < Units.Count; i++)
            {
                var u = Units[i]; if (i > 0) sb.Append(',');
                float dist = Camera.main != null ? Vector3.Distance(Camera.main.transform.position, u.Centre) : 0f;
                var su = Camera.main != null ? Camera.main.WorldToViewportPoint(u.Centre + Vector3.up * u.Height * 0.7f * u.transform.lossyScale.y) : Vector3.zero;
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"lod\":{0},\"verts\":{1},\"bones\":{2},\"dist\":{3:0.0},\"clip\":\"{4}\",\"dead\":{5},\"sx\":{6:0.000},\"sy\":{7:0.000}}}", u.Lod, u.VertsDrawn, u.BonesPerLod[u.Lod], dist, u.Clip >= 0 ? Library.Clips[u.Clip].Name : "", u.Dead ? "true" : "false", su.x, su.y);
            }
            sb.Append("]}");
            return sb.ToString();
        }

        // ------------------------------------------------------------------------------------------------ panel
        void OnGUI()
        {
            LabelsGUI();
            if (!ShowPanel) { Cam.PointerOverUi = false; return; }
            var area = new Rect(10, 10, 300, Screen.height - 20);
            var mp = Mouse.current != null ? Mouse.current.position.ReadValue() : Vector2.zero;
            Cam.PointerOverUi = area.Contains(new Vector2(mp.x, Screen.height - mp.y));
            GUI.Box(area, GUIContent.none);
            GUILayout.BeginArea(new Rect(area.x + 8, area.y + 6, area.width - 16, area.height - 12));
            GUILayout.Label("<b>PLAYGROUND</b>  F1 panel · Space freeze · H hit · G fire · click = AP", Rich());
            tab = GUILayout.Toolbar(tab, new[] { "Vehicle", "Unit", "Look" });
            GUILayout.Space(4);
            if (tab == 0) VehiclePanel(); else if (tab == 1) UnitPanel(); else LookPanel();
            GUILayout.FlexibleSpace();
            GUILayout.Label(Info(), Rich());
            GUILayout.EndArea();
        }

        /// <summary>What each thing on the stage is drawn at, written over it: in the side-by-side views this is the
        /// legend (the captures render the camera directly, so they carry DrawLabels' text only when Labels is on).</summary>
        public bool Labels = true;
        void LabelsGUI()
        {
            if (!Labels || Camera.main == null) return;
            var cam = Camera.main;
            var style = new GUIStyle(GUI.skin.label) { alignment = TextAnchor.MiddleCenter, fontSize = 14, richText = true };
            foreach (var v in Vehicles) Label(cam, v.Centre + Vector3.up * v.Radius * 0.8f, $"<b>LOD{v.Lod}</b>  {v.TrisDrawn:N0} tris  {v.State}", style);
            if (Units.Count <= 6) foreach (var u in Units) Label(cam, u.Centre + Vector3.up * u.Height * 0.75f * u.transform.lossyScale.y, $"<b>LOD{u.Lod}</b> {u.VertsDrawn} v · {u.BonesPerLod[u.Lod]} bones", style);
        }

        static void Label(Camera cam, Vector3 at, string text, GUIStyle style)
        {
            var s = cam.WorldToScreenPoint(at);
            if (s.z <= 0f) return;
            var r = new Rect(s.x - 110f, Screen.height - s.y - 12f, 220f, 24f);
            var shadow = new Rect(r.x + 1, r.y + 1, r.width, r.height);
            var c = GUI.color; GUI.color = new Color(0, 0, 0, 0.8f); GUI.Label(shadow, text, style); GUI.color = c;
            GUI.Label(r, text, style);
        }

        static GUIStyle rich;
        static GUIStyle Rich() { if (rich == null) rich = new GUIStyle(GUI.skin.label) { richText = true, wordWrap = true, fontSize = 12 }; return rich; }

        void Row(params (string label, string cmd)[] b)
        {
            GUILayout.BeginHorizontal();
            foreach (var (l, c) in b) if (GUILayout.Button(l)) Queue(c);
            GUILayout.EndHorizontal();
        }

        void LodRow(int count)
        {
            GUILayout.BeginHorizontal();
            GUILayout.Label("LOD", GUILayout.Width(30));
            if (GUILayout.Toggle(forcedLod < 0, "Auto", "Button")) { if (forcedLod >= 0) Queue("lod -1"); }
            for (int k = 0; k < count; k++) if (GUILayout.Toggle(forcedLod == k, k.ToString(), "Button") && forcedLod != k) Queue("lod " + k);
            GUILayout.EndHorizontal();
        }

        void VehiclePanel()
        {
            if (Library.Vehicles.Length > 1)
            {
                var names = new string[Library.Vehicles.Length];
                for (int i = 0; i < names.Length; i++) names[i] = Library.Vehicles[i].Name;
                int was = vehicleIndex; vehicleIndex = GUILayout.Toolbar(vehicleIndex, names); if (was != vehicleIndex) Queue(Mode);
            }
            Row(("One", "vehicle"), ("LODs side by side", "vehicle.compare"), ("Among men", "mixed"));
            LodRow(3);
            GUILayout.Label("Damage");
            Row(("AP hull", "ap Hull 25"), ("AP turret", "ap Turret 30"), ("HE right", "he right 35"));
            Row(("Track L", "ap Track_L 45"), ("Track R", "ap Track_R 45"), ("Antenna", "ap Antenna 20"));
            Row(("Knock out", "ko"), ("Cook off", "cook"), ("Repair", "repair"));
            Row(("Sequence (same on every copy)", "seq"));
            Row(("Fire gun", "fire"), ("Traverse", "traverse"));
            GUILayout.BeginHorizontal();
            GUILayout.Label($"Cook-off delay {cookDelay:0} s", GUILayout.Width(130));
            float cd = GUILayout.HorizontalSlider(cookDelay, -1f, 20f); if (Mathf.Abs(cd - cookDelay) > 0.5f) Queue("cookdelay " + Mathf.Round(cd));
            GUILayout.EndHorizontal();
            GUILayout.BeginHorizontal();
            GUILayout.Label($"Size x{VehicleSize:0.00}", GUILayout.Width(130));
            float sz = GUILayout.HorizontalSlider(VehicleSize, 1f, 2.5f); if (Mathf.Abs(sz - VehicleSize) > 0.05f) Queue("size " + sz.ToString("0.0", CultureInfo.InvariantCulture));
            GUILayout.EndHorizontal();
        }

        void UnitPanel()
        {
            Row(("One", "unit"), ("LODs side by side", "unit.compare"), ("Squad 8-300 m", "unit.squad"));
            LodRow(4);
            Row(("Kill", "kill"), ("Set alight", "ignite"), ("Revive", "revive"));
            GUILayout.Label("Clip (the game's own, retargeted)");
            clipScroll = GUILayout.BeginScrollView(clipScroll, GUILayout.Height(260));
            for (int i = 0; i < Library.Clips.Length; i++)
                if (GUILayout.Toggle(clip == i, Library.Clips[i].Name, "Button") && clip != i) Queue("clip " + Library.Clips[i].Name);
            GUILayout.EndScrollView();
        }

        void LookPanel()
        {
            Row(("Night mud", "biome NightMud"), ("Winter", "biome Winter"), ("Lava", "biome Lava"));
            Row(("Close", "cam close"), ("Standard view", "cam standard"), ("Far", "cam far"), ("Top", "cam top"));
            Row(("Freeze", "freeze 1"), ("Run", "freeze 0"), ("x0.25", "timescale 0.25"), ("x1", "timescale 1"));
        }

        string Info()
        {
            var sb = new StringBuilder();
            sb.AppendFormat(CultureInfo.InvariantCulture, "<b>{0:0}</b> fps · {1} cards · t×{2:0.##}\n", fpsAvg, Fx.CardsAlive, Time.timeScale);
            foreach (var v in Vehicles)
                sb.AppendFormat("<b>{0}</b> LOD{1} {2} tris · {3} · hp {4:0} · fire {5:0.0}\n   {6}\n", v.name, v.Lod, v.TrisDrawn, v.State, v.Hp, v.FireLevel, v.LastEvent);
            int shown = 0;
            foreach (var u in Units)
            {
                if (shown++ >= 6) { sb.Append($"… {Units.Count} units\n"); break; }
                float d = Camera.main != null ? Vector3.Distance(Camera.main.transform.position, u.Centre) : 0f;
                sb.AppendFormat("unit LOD{0} {1} verts {2} bones @ {3:0} m {4}\n", u.Lod, u.VertsDrawn, u.BonesPerLod[u.Lod], d, u.Dead ? "(dead)" : "");
            }
            if (!string.IsNullOrEmpty(Status)) sb.Append("<i>").Append(Status).Append("</i>");
            return sb.ToString();
        }
    }
}
