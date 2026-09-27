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
        public readonly List<BuildingRig> Buildings = new List<BuildingRig>();
        public string BuildingSet = "Ruins";
        int buildingIndex, shellCount;
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

        Material groundMat; Texture2D gridTex, mudTex, mudDetail;
        /// <summary>grid: the range (a metre grid, to judge sizes); mud: the battle's night mud, dark and warm, no grid (to
        /// judge whether a figure reads against what it will stand on).</summary>
        void SetGround(string kind)
        {
            if (groundMat == null) return;
            if (kind == "mud")
            {
                if (mudTex == null)
                {
                    const int N = 256; mudTex = new Texture2D(N, N, TextureFormat.RGB24, true) { name = "PlaygroundMud", wrapMode = TextureWrapMode.Repeat };
                    var px = new Color32[N * N];
                    for (int y = 0; y < N; y++) for (int x = 0; x < N; x++)
                    {
                        float n = Mathf.PerlinNoise(x * 0.05f, y * 0.05f) * 0.6f + Mathf.PerlinNoise(x * 0.23f + 7f, y * 0.23f) * 0.4f;
                        float v = 0.85f + 0.3f * n;   // around the battle's MudMid (GreyboxTerrainView: 0.325, 0.285, 0.24)
                        px[y * N + x] = new Color(0.325f * v, 0.285f * v, 0.24f * v);
                    }
                    mudTex.SetPixels32(px); mudTex.Apply(true);
                }
                groundMat.SetTexture("_BaseMap", mudTex); groundMat.mainTexture = mudTex;
                // the battle's own close-up mud grain and relief (GreyboxTerrainView.BuildMudDetail), at its settings
                var build = typeof(TW.Presentation.Terrain.GreyboxTerrainView).GetMethod("BuildMudDetail", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);
                if (build != null && groundMat.HasProperty("_DetailMap"))
                {
                    if (mudDetail == null) mudDetail = build.Invoke(null, null) as Texture2D;
                    groundMat.SetTexture("_DetailMap", mudDetail); groundMat.SetFloat("_DetailScale", 1f / 12f);
                    groundMat.SetFloat("_DetailStrength", 0.20f); groundMat.SetFloat("_DetailBump", 1f);
                }
            }
            else { groundMat.SetTexture("_BaseMap", gridTex); groundMat.mainTexture = gridTex; if (groundMat.HasProperty("_DetailStrength")) groundMat.SetFloat("_DetailStrength", 0f); }
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
            groundMat = m; gridTex = tex;
        }

        // ------------------------------------------------------------------------------------------------ stage
        void Clear()
        {
            foreach (var v in Vehicles) if (v != null) Destroy(v.gameObject);
            foreach (var u in Units) if (u != null) Destroy(u.gameObject);
            foreach (var b in Buildings) if (b != null) Destroy(b.gameObject);
            Vehicles.Clear(); Units.Clear(); Buildings.Clear(); script.Clear(); scriptStart = -1f;
            Fx.ClearLamps(); Fx.ClearDebris(); Fx.ClearCards();
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
            u.ForcedLod = lod; u.Phase = phase;
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
                    for (int k = 0; k < 3; k++) SpawnVehicle(new Vector3((k - 1) * 30f, 0f, 0f), 205f, k, 1);
                    Cam.Preset("default", new Vector3(0f, 2.5f, 0f)); Cam.Distance = 66f; Cam.Yaw = 0f; Cam.Pitch = 18f; Cam.Fov = 42f; break;
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
                        for (int i = 0; i < d.Length; i++) SpawnUnit(new Vector3((i % 2 == 0 ? -1f : 1f) * (1.2f + 0.06f * d[i]), 0f, d[i]), 200f, forcedLod, i * 0.37f);
                        Cam.Focus = new Vector3(0f, 1f, 0f); Cam.Fov = 25f; Cam.Pitch = 14f; Cam.Yaw = 0f; Cam.Distance = 10f;
                        break;
                    }
                case "building":
                    {
                        var kit = BuildingRig.Kit(BuildingSet);
                        if (kit.Length == 0) break;
                        var b = BuildingRig.Build(BuildingSet, kit[buildingIndex % kit.Length].Name, Fx, stage, Vector3.zero, 200f, 1);
                        if (b != null) Buildings.Add(b);
                        shellCount = 0;
                        Cam.Preset("default", new Vector3(0f, b != null ? b.Radius * 0.45f : 3f, 0f)); Cam.Yaw = 0f; Cam.Pitch = 16f; Cam.Distance = b != null ? b.Radius * 4.5f : 40f;
                        break;
                    }
                case "mixed":
                    SpawnVehicle(new Vector3(0f, 0f, 0f), 205f, forcedLod, 1);
                    // twelve men 3 m apart, 6 m clear of the hull, so every one of them can be seen against it
                    for (int i = 0; i < 12; i++) SpawnUnit(new Vector3(-6f + (i % 6) * 3f, 0f, 9f + (i / 6) * 4f), 20f, forcedLod, i * 0.21f);   // between the hull and the camera
                    Cam.Preset("standard", new Vector3(-1f, 1.5f, 3f)); Cam.Distance = 55f; Cam.Yaw = 200f; break;
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
            // aimed at the tank's plate and track; anything else takes them on a fitting and on its running gear or a limb
            script.Add((0.3f, "ap Plate_LF 30 1"));
            script.Add((1.4f, "ap Track_L 45 2"));
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
                case "vehicle": case "vehicle.compare": case "unit": case "unit.compare": case "unit.squad": case "mixed": case "building": Scene(c); break;
                case "set": BuildingSet = a.Length > 1 ? a[1] : "Ruins"; buildingIndex = 0; Scene("building"); break;
                case "house":
                    {
                        var kit = BuildingRig.Kit(BuildingSet);
                        int k = a.Length > 1 ? System.Array.FindIndex(kit, x => x.Name == a[1]) : -1;
                        buildingIndex = k >= 0 ? k : buildingIndex + 1; Scene("building"); break;
                    }
                case "shell":
                    foreach (var bld in Buildings)
                    {
                        // a burst walking round the building, on its walls, at a height a shell would hit: deterministic
                        float ang = shellCount * 2.39996f, r = bld.Radius * 0.55f;
                        var at = a.Length >= 4 ? new Vector3(F(a, 1, 0), F(a, 2, 1), F(a, 3, 0)) : new Vector3(Mathf.Cos(ang) * r, 1f + (shellCount % 3) * 1.2f, Mathf.Sin(ang) * r);
                        bld.ShellLocal(at, F(a, 4, 70f));
                    }
                    shellCount++;
                    break;
                case "rebuild": foreach (var bld in Buildings) bld.Rebuild(); shellCount = 0; break;
                case "lod":
                    forcedLod = (int)F(a, 1, -1);
                    if (Mode != "vehicle.compare" && Mode != "unit.compare") { foreach (var v in Vehicles) v.ForcedLod = forcedLod; foreach (var u in Units) u.ForcedLod = forcedLod; }
                    break;
                case "seq": Sequence(); break;
                case "ap":
                    foreach (var v in Vehicles)
                    {
                        // "ap <part> <damage> [tier]": a machine without that part takes it on its first part of the tier
                        var part = v.Find(a.Length > 1 ? a[1] : "Hull") ?? (a.Length > 3 ? v.FirstOfTier((int)F(a, 3, 0f)) : null);
                        v.HitPart(part ?? v.Find("Hull"), F(a, 2, 30f));
                    }
                    break;
                case "he":
                    foreach (var v in Vehicles)
                    {
                        float side = a.Length > 1 && a[1] == "left" ? -1f : 1f;
                        // a flyer is burst beside it where it flies (an air burst): on the ground 14 m under it, the shell
                        // missed and the gunship was never knocked out (loop 2 r31)
                        var hull = v.Flyer != null ? v.Find("Hull") : null;
                        var at = hull != null && !hull.Loose ? hull.T.localPosition : Vector3.zero;
                        v.HitLocal(at + new Vector3(side * 2.6f, 0f, 0f), Vector3.down, F(a, 2, 35f), VehicleRig.HitKind.HE);
                    }
                    break;
                case "ko": foreach (var v in Vehicles) v.KnockOut(); break;
                case "cook": foreach (var v in Vehicles) v.CookOff(); break;
                case "fire": foreach (var v in Vehicles) v.FireGun(); break;
                case "repair": foreach (var v in Vehicles) v.Repair(); script.Clear(); break;
                case "traverse": foreach (var v in Vehicles) v.Traverse = !v.Traverse; break;
                case "cookdelay": cookDelay = F(a, 1, 7f); foreach (var v in Vehicles) v.CookDelay = cookDelay; break;
                case "size": VehicleSize = F(a, 1, 1.7f); Scene(Mode); break;
                case "fly": foreach (var v in Vehicles) if (v.Flyer != null) { v.Flyer.Speed = F(a, 1, 8f); if (a.Length > 2) v.Flyer.Altitude = F(a, 2, 14f); } break;   // "fly 8 [14]": m/s round a circle [altitude, m]
                case "walk": foreach (var v in Vehicles) if (v.Walker != null) { v.Walker.Speed = F(a, 1, 2f); v.Walker.InPlace = F(a, 2, 0f) > 0.5f; } break;   // "walk 2 [1]": a walker's pace, m/s [on the spot]
                case "clip":
                    {
                        int k = Library.ClipIndex(line.Substring(line.IndexOf(' ') + 1).Trim());
                        if (k >= 0) { clip = k; foreach (var u in Units) if (!u.Dead) u.Play(k, 0.2f, u.Phase); }
                        else return "no clip " + line;
                        break;
                    }
                case "speed": foreach (var u in Units) u.Speed = F(a, 1, 1f); break;
                case "face": foreach (var u in Units) u.transform.rotation = Quaternion.Euler(0f, F(a, 1, 0f), 0f); break;
                case "kill": foreach (var u in Units) u.Kill(DeathClip(u)); break;
                case "ignite": foreach (var u in Units) u.Ignite(Library.ClipIndex("Burning Run"), DeathClip(u)); break;
                case "revive": foreach (var u in Units) u.Revive(clip); break;
                case "cam":
                    follow = null;
                    // "cam follow yaw pitch distance fov": keep the first vehicle's hull in the middle (a flyer circling)
                    if (a.Length >= 2 && a[1] == "follow")
                    {
                        var h = Vehicles.Count > 0 ? Vehicles[0].Find("Hull") : null; follow = h != null ? h.T : null;
                        if (a.Length >= 6) { Cam.Yaw = F(a, 2, 0); Cam.Pitch = F(a, 3, 20); Cam.Distance = F(a, 4, 20); Cam.Fov = F(a, 5, 35); }
                        break;
                    }
                    if (a.Length >= 8) { Cam.Focus = new Vector3(F(a, 1, 0), F(a, 2, 0), F(a, 3, 0)); Cam.Yaw = F(a, 4, 0); Cam.Pitch = F(a, 5, 20); Cam.Distance = F(a, 6, 20); Cam.Fov = F(a, 7, 35); }
                    else Cam.Preset(a.Length > 1 ? a[1] : "default", Cam.Focus);
                    break;
                case "freeze": Time.timeScale = F(a, 1, 1f) > 0.5f ? 0f : 1f; break;
                case "timescale": Time.timeScale = F(a, 1, 1f); break;
                case "biome": if (System.Enum.TryParse<Biome>(a.Length > 1 ? a[1] : "NightMud", true, out var b)) SetBiome(b); break;
                case "panel": ShowPanel = F(a, 1, 1f) > 0.5f; break;
                case "lodfit": fitPath = a.Length > 1 ? a[1] : Path.Combine(Application.dataPath, "../Captures/lodfit.json"); break;
                case "lodpop": popPath = a.Length > 1 ? a[1] : Path.Combine(Application.dataPath, "../Captures/lodpop.json"); break;
                case "labels": Labels = F(a, 1, 1f) > 0.5f; break;
                case "ground": SetGround(a.Length > 1 ? a[1] : "grid"); break;
                case "sidehue": huePath = a.Length > 1 ? a[1] : Path.Combine(Application.dataPath, "../Captures/sidehue.json"); hueStep = 0; break;
                case "lodtint": foreach (var v in Vehicles) v.UseLodTints(F(a, 1, 1f) > 0.5f); foreach (var u in Units) u.UseLodTints(F(a, 1, 1f) > 0.5f); break;
                case "model":   // "model u 1": which of the library's figures (v: vehicles) the scenes build
                    if (a.Length > 2 && a[1] == "v") vehicleIndex = (int)F(a, 2, 0); else if (a.Length > 2) unitIndex = (int)F(a, 2, 0);
                    Queue(Mode); break;
                case "unitrings": Fx.UnitRings = F(a, 1, 1f) > 0.5f; break;
                case "cutsdebug": BuildingRig.DebugCuts = F(a, 1, 1f) > 0.5f; BuildingRig.ForgetCuts(); if (Mode == "building") Scene("building"); break;
                case "team":
                    {
                        // "team 0", "team 1", "team -1" for everyone; "team split": vehicles side 1, the first half of the
                        // figures side 0 and the second half side 1 (critic r8: m3 never showed a side-1 man)
                        bool split = a.Length > 1 && a[1] == "split"; int t = split ? 0 : (int)F(a, 1, -1);
                        foreach (var v in Vehicles) v.Team = split ? 1 : t;
                        for (int i = 0; i < Units.Count; i++) Units[i].Team = split ? (i < Units.Count / 2 ? 0 : 1) : t;
                        break;
                    }
                case "shot": shotPath = a.Length > 1 ? a[1] : Path.Combine(Application.dataPath, "../Captures/playground.png"); shotW = (int)F(a, 2, 1600); shotH = (int)F(a, 3, 900); break;
                default: return "unknown command " + c;
            }
            return "ok " + line;
        }

        int DeathClip(UnitRig u)
        {
            string[] deaths = { "Rifle Death", "Death From The Front", "Death From Right", "Death From The Back" };
            int i = Mathf.Max(0, Units.IndexOf(u)) % deaths.Length;   // by place in the list, not the instance id: the same every run
            int k = Library.ClipIndex(deaths[i]);
            return k >= 0 ? k : Library.ClipIndex("Death");
        }

        // ------------------------------------------------------------------------------------------------ frame
        Transform follow;

        void Update()
        {
            if (follow != null) Cam.Focus = follow.position;
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
            if (popPath != null)
            {
                string path = popPath; popPath = null;
                File.WriteAllText(path, LodPop(Path.ChangeExtension(path, null)));
            }
            if (huePath != null) SideSignalStep();
            if (fitPath != null) { string path = fitPath; fitPath = null; File.WriteAllText(path, LodFit()); }
        }

        // ------------------------------------------------------------------------------------------------ LOD pops
        string popPath, fitPath;

        /// <summary>Each LOD's tint fitted on what the camera sees: every LOD drawn alone from four sides (the lodpop
        /// sides, the object filling half a 320 px frame at the battle's 25 degree pitch), the mean colour over its
        /// silhouette compared with the LOD the standard view shows (see Seen), the ratio put on as a tint, twice (lighting and the grade are not linear in
        /// the albedo), within 0.75-1.25 (the frog's LOD3 wants 0.76: its far LODs render 23-30 % brighter). Kept by entry
        /// name (LodTint.Fitted) so every rig built after uses it. The mesh estimate overshot: it counts faces nobody sees
        /// (critic r8). What is left at a switch depends on the side it is seen from: shading, which no tint removes.</summary>
        string LodFit()
        {
            var cam = Camera.main; var sb = new StringBuilder("{\"fits\":[");
            var was = (cam.transform.position, cam.transform.rotation, cam.fieldOfView);
            Cam.enabled = false;
            bool first = true;
            const int W = 320, H = 320;
            // the reference is the LOD the battle's standard view shows (78 m, 25 degree lens), not LOD0: matched to LOD0 the
            // far frogs came out darker, and a squad's contrast against the mud fell from 20.1 to 14.3 (figure_gap)
            int Seen(float radius, float[] cuts)
            {
                float share = 2f * radius / (2f * 78f * Mathf.Tan(12.5f * Mathf.Deg2Rad)); int k = 0;
                while (k < cuts.Length && share < cuts[k]) k++;
                return k;
            }
            void Fit(string name, GameObject root, Vector3 centre, float radius, int lods, System.Action<int> force, System.Action<Color[]> apply, int refLod)
            {
                refLod = Mathf.Clamp(refLod, 0, lods - 1);
                var tints = new Color[lods]; for (int k = 0; k < lods; k++) tints[k] = Color.white;
                var means = new Vector3[lods]; var before = new Vector3[lods];
                for (int iter = 0; iter < 3; iter++)
                {
                    apply(tints);
                    for (int k = 0; k < lods; k++)
                    {
                        force(k); double r = 0, g = 0, b = 0; int n = 0;
                        foreach (float side in new[] { 200f, 290f, 20f, 110f })
                        {
                            float d = radius * 8f;
                            cam.fieldOfView = 2f * Mathf.Atan(2f * radius / d) * Mathf.Rad2Deg;
                            var dir = Quaternion.Euler(25f, side, 0f) * Vector3.forward;
                            cam.transform.SetPositionAndRotation(centre - dir * d, Quaternion.LookRotation(dir));
                            var px = Grab(cam, W, H); var m = Silhouette(cam, root, W, H);
                            for (int i = 0; i < px.Length; i++) if (m[i]) { r += px[i].r; g += px[i].g; b += px[i].b; n++; }
                        }
                        means[k] = n > 0 ? new Vector3((float)(r / n), (float)(g / n), (float)(b / n)) : Vector3.zero;
                    }
                    if (iter == 0) System.Array.Copy(means, before, lods);
                    if (iter == 2) break;   // the third pass only measures the result
                    for (int k = 0; k < lods; k++)
                    {
                        if (k == refLod) continue;
                        float f(float t, float want, float have) => Mathf.Clamp(have > 0.5f ? t * want / have : t, 0.75f, 1.25f);
                        tints[k] = new Color(f(tints[k].r, means[refLod].x, means[k].x), f(tints[k].g, means[refLod].y, means[k].y), f(tints[k].b, means[refLod].z, means[k].z), 1f);
                    }
                }
                LodTint.Fitted[name] = tints;
                if (!first) sb.Append(','); first = false;
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"what\":\"{0}\",\"reference\":{1},\"lods\":[", name, refLod);
                for (int k = 0; k < lods; k++)
                {
                    if (k > 0) sb.Append(',');
                    sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"lod\":{0},\"tint\":[{1:0.000},{2:0.000},{3:0.000}],\"before\":[{4:0.0},{5:0.0},{6:0.0}],\"after\":[{7:0.0},{8:0.0},{9:0.0}]}}",
                        k, tints[k].r, tints[k].g, tints[k].b, before[k].x, before[k].y, before[k].z, means[k].x, means[k].y, means[k].z);
                }
                sb.Append("]}");
            }
            var doneV = new HashSet<string>(); var doneU = new HashSet<string>();
            foreach (var v in Vehicles)
            {
                string name = v.name.Split('#')[0]; if (!doneV.Add(name)) continue;
                int prev = v.Lod;
                Fit(name, v.gameObject, v.Centre, v.Radius, v.LodCount, k => { foreach (var p in v.Parts) p.R.enabled = true; v.SetLod(k); }, t => v.ApplyLodTints(t), Seen(v.Radius, v.Picker.Cuts));
                v.SetLod(prev);
            }
            foreach (var u in Units)
            {
                if (!doneU.Add(u.name)) continue;
                int prev = Mathf.Max(0, u.Lod);
                Fit(u.name, u.gameObject, u.Centre, 0.5f * u.Height * u.transform.lossyScale.y, u.Lods.Length, k => u.SetLodSilently(k), t => u.ApplyLodTints(t), Seen(0.5f * u.Height * u.transform.lossyScale.y, u.Picker.Cuts));
                u.SetLodSilently(prev);
            }
            // every other rig of the same entry takes the fit too
            foreach (var v in Vehicles) if (LodTint.Fitted.TryGetValue(v.name.Split('#')[0], out var fv)) v.ApplyLodTints(fv);
            foreach (var u in Units) if (LodTint.Fitted.TryGetValue(u.name, out var fu)) u.ApplyLodTints(fu);
            cam.transform.SetPositionAndRotation(was.Item1, was.Item2); cam.fieldOfView = was.Item3;
            Cam.enabled = true;
            return sb.Append("]}").ToString();
        }

        /// <summary>At every LOD boundary of every vehicle and figure on the stage: the camera put exactly where the switch
        /// happens (the battle's 25 degree lens), the thing drawn at the LOD before and the LOD after, and what changes on
        /// screen: the silhouette's overlap (IoU of the two masks against an empty frame) and the mean colour shift inside
        /// them. 1.0 / 0 is a switch nobody can see. Measured magnified (the lens narrowed to fill half the frame from the
        /// switch distance), so "share" says how big it really is there. Writes a strip PNG per boundary beside the JSON.</summary>
        string LodPop(string stem)
        {
            var cam = Camera.main; var sb = new StringBuilder("{\"pops\":[");
            var was = (cam.transform.position, cam.transform.rotation, cam.fieldOfView);
            Cam.enabled = false;
            bool first = true;
            void Measure(string what, GameObject root, Vector3 centre, float radius, float[] cuts, int lods, System.Action<int> force)
            {
                for (int k = 0; k < lods - 1; k++)
                foreach (float side in new[] { 200f, 290f, 20f, 110f })
                {
                    float fov = 25f, share = cuts[k];
                    float d = 2f * radius / (share * 2f * Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad));
                    // the switch happens at distance d; at the battle lens the thing is a handful of pixels there, too few to
                    // compare shapes. Same place (same perspective), a lens narrowed until it fills half the frame.
                    cam.fieldOfView = 2f * Mathf.Atan(2f * radius / d) * Mathf.Rad2Deg;
                    var dir = Quaternion.Euler(25f, side, 0f) * Vector3.forward;
                    cam.transform.SetPositionAndRotation(centre - dir * d, Quaternion.LookRotation(dir));
                    const int W = 480, H = 480;
                    force(k); var a = Grab(cam, W, H); var sa = Silhouette(cam, root, W, H);
                    // the floor: the same LOD from one degree round (what a turn of the camera alone changes)
                    var rot0 = cam.transform.rotation; var pos0 = cam.transform.position;
                    cam.transform.RotateAround(centre, Vector3.up, 1f); var a2 = Grab(cam, W, H);
                    cam.transform.SetPositionAndRotation(pos0, rot0);
                    double fl = 0; int nfl = 0;
                    for (int i = 0; i < a.Length; i++) if (sa[i]) { fl += (Mathf.Abs(a[i].r - a2[i].r) + Mathf.Abs(a[i].g - a2[i].g) + Mathf.Abs(a[i].b - a2[i].b)) / 3.0; nfl++; }
                    double floorCol = nfl > 0 ? fl / nfl : 0.0;
                    double floorBlock = BlockShift(a, a2, sa, sa, W, H);
                    force(k + 1); var b = Grab(cam, W, H); var sbm = Silhouette(cam, root, W, H);
                    int inter = 0, uni = 0; double dc = 0, di = 0; int nd = 0;
                    // dcol (over the union) mixes a colour change with a shape change; dcol_inside is only where both LODs
                    // cover, and dmean is the change of the mean colour, the part a per-LOD tint can remove (LodTint)
                    double ar = 0, ag = 0, ab = 0, br = 0, bg = 0, bb = 0; int na = 0, nb = 0;
                    var strip = new Texture2D(W * 2, H, TextureFormat.RGB24, false);
                    for (int i = 0; i < a.Length; i++)
                    {
                        bool ma = sa[i], mb = sbm[i];
                        if (ma && mb) { inter++; di += (Mathf.Abs(a[i].r - b[i].r) + Mathf.Abs(a[i].g - b[i].g) + Mathf.Abs(a[i].b - b[i].b)) / 3.0; }
                        if (ma) { ar += a[i].r; ag += a[i].g; ab += a[i].b; na++; }
                        if (mb) { br += b[i].r; bg += b[i].g; bb += b[i].b; nb++; }
                        if (ma || mb) { uni++; dc += (Mathf.Abs(a[i].r - b[i].r) + Mathf.Abs(a[i].g - b[i].g) + Mathf.Abs(a[i].b - b[i].b)) / 3.0; nd++; }
                    }
                    strip.SetPixels32(0, 0, W, H, a); strip.SetPixels32(W, 0, W, H, b); strip.Apply();
                    File.WriteAllBytes($"{stem}_{what}_{k}to{k + 1}_{side:0}.png", strip.EncodeToPNG()); Destroy(strip);
                    if (!first) sb.Append(','); first = false;
                    double dcol = nd > 0 ? dc / nd : 0.0, dcolIn = inter > 0 ? di / inter : 0.0;
                    double dblock = BlockShift(a, b, sa, sbm, W, H);
                    double dmean = na > 0 && nb > 0 ? (System.Math.Abs(ar / na - br / nb) + System.Math.Abs(ag / na - bg / nb) + System.Math.Abs(ab / na - bb / nb)) / 3.0 : 0.0;
                    sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"what\":\"{0}\",\"from\":{1},\"to\":{2},\"side\":{8:0},\"dist\":{3:0.0},\"share\":{4:0.000},\"iou\":{5:0.000},\"dcol\":{6:0.0},\"dcol_floor\":{9:0.0},\"dcol_ratio\":{10:0.00},\"dcol_inside\":{11:0.0},\"dmean\":{12:0.0},\"dblock\":{13:0.0},\"dblock_floor\":{14:0.0},\"pixels\":{7}}}",
                        what, k, k + 1, d, share, uni > 0 ? (float)inter / uni : 1f, dcol, uni, side, floorCol, floorCol > 0.01 ? dcol / floorCol : 0.0, dcolIn, dmean, dblock, floorBlock);
                }
            }
            foreach (var v in Vehicles)
                Measure(v.name, v.gameObject, v.Centre, v.Radius, v.Picker.Cuts, v.LodCount, k => { foreach (var p in v.Parts) p.R.enabled = k >= -1; if (k >= 0) v.SetLod(k); });
            foreach (var u in Units)
                Measure(u.name, u.gameObject, u.Centre, 0.5f * u.Height * u.transform.lossyScale.y, u.Picker.Cuts, u.Lods.Length, k => { for (int j = 0; j < u.Lods.Length; j++) u.Lods[j].enabled = j == k; if (k >= 0) u.SetLodSilently(k); });
            cam.transform.SetPositionAndRotation(was.Item1, was.Item2); cam.fieldOfView = was.Item3;
            Cam.enabled = true;
            foreach (var v in Vehicles) foreach (var p in v.Parts) p.R.enabled = true;
            return sb.Append("]}").ToString();
        }

        /// <summary>The colour change a player sees as a colour change, not as a line moving: both frames averaged over
        /// 12-pixel blocks (a 480-pixel frame of a thing filling half of it: a block is ~1/20 of the thing), compared over
        /// the blocks both LODs cover entirely. Per-pixel differences are dominated by the ink lines and the shading moving
        /// with the shape (r15: re-painting the lower LODs from LOD0 took the per-pixel shift only from 20 to 18).</summary>
        static double BlockShift(Color32[] a, Color32[] b, bool[] ma, bool[] mb, int w, int h)
        {
            const int B = 12; double sum = 0; int n = 0;
            for (int by = 0; by + B <= h; by += B)
                for (int bx = 0; bx + B <= w; bx += B)
                {
                    double ar = 0, ag = 0, ab = 0, br = 0, bg = 0, bb = 0; bool full = true;
                    for (int y = by; y < by + B && full; y++)
                        for (int x = bx; x < bx + B; x++)
                        {
                            int i = y * w + x; if (!ma[i] || !mb[i]) { full = false; break; }
                            ar += a[i].r; ag += a[i].g; ab += a[i].b; br += b[i].r; bg += b[i].g; bb += b[i].b;
                        }
                    if (!full) continue;
                    sum += (System.Math.Abs(ar - br) + System.Math.Abs(ag - bg) + System.Math.Abs(ab - bb)) / (3.0 * B * B); n++;
                }
            return n > 0 ? sum / n : 0.0;
        }

        /// <summary>Where the thing covers the frame: it alone (its layer), on black, without fog or the grade (film grain and
        /// a night fog both hide a dark figure's edge from a comparison against an empty frame).</summary>
        static bool[] Silhouette(Camera c, GameObject root, int w, int h)
        {
            const int layer = 31;
            var ts = root.GetComponentsInChildren<Transform>(true); var was = new int[ts.Length];
            for (int i = 0; i < ts.Length; i++) { was[i] = ts[i].gameObject.layer; ts[i].gameObject.layer = layer; }
            var urp = c.GetComponent<UniversalAdditionalCameraData>();
            var mask = c.cullingMask; var flags = c.clearFlags; var bg = c.backgroundColor; bool post = urp != null && urp.renderPostProcessing; bool fog = RenderSettings.fog;
            c.cullingMask = 1 << layer; c.clearFlags = CameraClearFlags.SolidColor; c.backgroundColor = Color.black; if (urp != null) urp.renderPostProcessing = false; RenderSettings.fog = false;
            var px = Grab(c, w, h);
            c.cullingMask = mask; c.clearFlags = flags; c.backgroundColor = bg; if (urp != null) urp.renderPostProcessing = post; RenderSettings.fog = fog;
            for (int i = 0; i < ts.Length; i++) ts[i].gameObject.layer = was[i];
            var m = new bool[px.Length];
            for (int i = 0; i < px.Length; i++) m[i] = px[i].r + px[i].g + px[i].b > 6;
            return m;
        }

        static Color32[] Grab(Camera c, int w, int h)
        {
            var rt = RenderTexture.GetTemporary(w, h, 24, RenderTextureFormat.ARGB32);
            var before = c.targetTexture; c.targetTexture = rt; c.Render(); c.targetTexture = before;
            var was = RenderTexture.active; RenderTexture.active = rt;
            var tex = new Texture2D(w, h, TextureFormat.RGB24, false); tex.ReadPixels(new Rect(0, 0, w, h), 0, 0); tex.Apply();
            RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);
            var px = tex.GetPixels32(); Destroy(tex); return px;
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
            LastJson = Report(tex, Units.Count > 0 ? FigureGap(c, w, h, tex) : null);
            File.WriteAllText(Path.ChangeExtension(path, ".json"), LastJson);
            Destroy(tex);
            LastShot = path;
        }

        /// <summary>Mean brightness of the figures as drawn, of a ring of what is right round them, and the gap: whether a man
        /// reads against the ground by his colour or only by his ink line.</summary>
        /// <summary>The battle's own readability number (Editor/CaptureRig): each man's centre (a disc a tenth of his drawn
        /// height) against the median of a ring of ground round him (0.62 to 1.0 of his height), |man - ground| / (ground +
        /// 0.02), the median over the men at least 6 px tall. The playground's own figure_gap said a lift at range helped;
        /// in the battle scene the same lift made the men LESS distinct (0.160 -> 0.125): at night they read as dark
        /// shapes on lighter mud. Reported so the two benches speak the same language.</summary>
        string Contrast(Camera c, int w, int h, Texture2D frame)
        {
            var px = frame.GetPixels32(); var lum = new float[px.Length];
            for (int i = 0; i < px.Length; i++) lum[i] = (0.2126f * px[i].r + 0.7152f * px[i].g + 0.0722f * px[i].b) / 255f;
            var all = new List<float>(); var far = new List<float>();
            foreach (var u in Units)
            {
                var foot = c.WorldToScreenPoint(u.transform.position); var head = c.WorldToScreenPoint(u.transform.position + Vector3.up * u.Height * u.transform.lossyScale.y);
                if (foot.z <= 0f) continue;
                float sx = w / (float)c.pixelWidth, sy = h / (float)c.pixelHeight;
                float tall = Mathf.Abs(head.y - foot.y) * sy; if (tall < 6f) continue;
                var mid = c.WorldToScreenPoint(u.Centre); int cx = Mathf.RoundToInt(mid.x * sx), cy = Mathf.RoundToInt(mid.y * sy);
                int rMan = Mathf.Max(2, Mathf.RoundToInt(tall * 0.10f)), rIn = Mathf.RoundToInt(tall * 0.62f), rOut = Mathf.RoundToInt(tall * 1.00f);
                if (cx < rOut || cy < rOut || cx >= w - rOut || cy >= h - rOut) continue;
                double sm = 0; int nm = 0; var ring = new List<float>();
                for (int y = cy - rOut; y <= cy + rOut; y++)
                    for (int x = cx - rOut; x <= cx + rOut; x++)
                    {
                        int d2 = (x - cx) * (x - cx) + (y - cy) * (y - cy);
                        if (d2 <= rMan * rMan) { sm += lum[y * w + x]; nm++; }
                        else if (d2 >= rIn * rIn && d2 <= rOut * rOut) ring.Add(lum[y * w + x]);
                    }
                if (nm == 0 || ring.Count == 0) continue;
                ring.Sort(); float bg = ring[ring.Count / 2];
                float k = Mathf.Abs((float)(sm / nm) - bg) / (bg + 0.02f);
                all.Add(k); if ((u.transform.position - c.transform.position).magnitude > 60f) far.Add(k);
            }
            float Med(List<float> l) { if (l.Count == 0) return -1f; l.Sort(); return l[l.Count / 2]; }
            return string.Format(CultureInfo.InvariantCulture, "\"contrast_median\":{0:0.000},\"contrast_median_far\":{1:0.000},\"contrast_men\":{2},", Med(all), Med(far), all.Count);
        }

        string FigureGap(Camera c, int w, int h, Texture2D frame)
        {
            var near = Gap(c, w, h, frame, u => true);
            if (near == null) return null;
            near = Contrast(c, w, h, frame) + near;
            // the men past 60 m alone: at range a figure is a few pixels, and the near ones dominate the pixel count
            var far = GapOf(c, w, h, frame, u => (u.transform.position - c.transform.position).magnitude > 60f);
            return near + (far != null ? string.Format(CultureInfo.InvariantCulture, "\"figure_gap_far\":{0:0.0},\"figure_px_far\":{1},", far.Value.gap, far.Value.px) : "");
        }

        struct GapResult { public float fig, ground, gap; public int px; }

        string Gap(Camera c, int w, int h, Texture2D frame, System.Func<UnitRig, bool> which)
        {
            var r = GapOf(c, w, h, frame, which); if (r == null) return null;
            return string.Format(CultureInfo.InvariantCulture, "\"figure_luma\":{0:0.0},\"ground_luma\":{1:0.0},\"figure_gap\":{2:0.0},\"figure_px\":{3},", r.Value.fig, r.Value.ground, r.Value.gap, r.Value.px);
        }

        GapResult? GapOf(Camera c, int w, int h, Texture2D frame, System.Func<UnitRig, bool> which)
        {
            var mask = new bool[w * h]; bool any = false;
            foreach (var u in Units) { if (!which(u)) continue; any = true; var m = Silhouette(c, u.gameObject, w, h); for (int i = 0; i < m.Length; i++) mask[i] |= m[i]; }
            if (!any) return null;
            var px = frame.GetPixels32(); double fig = 0, ring = 0; int nf = 0, nr = 0; const int R = 4;
            for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
            {
                int i = y * w + x; float l = (0.2126f * px[i].r + 0.7152f * px[i].g + 0.0722f * px[i].b);
                if (mask[i]) { fig += l; nf++; continue; }
                bool near = false;
                for (int dy = -R; dy <= R && !near; dy += 2) for (int dx = -R; dx <= R && !near; dx += 2)
                {
                    int xx = x + dx, yy = y + dy; if (xx < 0 || yy < 0 || xx >= w || yy >= h) continue;
                    if (mask[yy * w + xx]) near = true;
                }
                if (near) { ring += l; nr++; }
            }
            if (nf == 0 || nr == 0) return null;
            return new GapResult { fig = (float)(fig / nf), ground = (float)(ring / nr), gap = (float)(fig / nf - ring / nr), px = nf };
        }

        // ------------------------------------------------------------------------------------------------ side colours
        string huePath; int hueStep; Color32[][] hueFrames; bool[][] hueMasks, hueRaw, hueMen; int[] hueV, hueU; float hueScale;

        /// <summary>Whether the two sides read apart by colour, measured as what each side's colouring ADDS to the frame.
        /// With time frozen the same frame is drawn three times: only side 0 coloured, only side 1, neither. A side's
        /// signal is its frame minus the plain one, summed on the opponent-colour plane over its own vehicles and figures
        /// (silhouettes grown by ~2% of the frame so its rings count); its angle is the side's hue, its length per pixel the
        /// strength, and the gap is the angle between the two sides. One side at a time, because a tank's ring reaches into
        /// a squad beside it and would count as the squad's colour. A frame between each change, because the rings are
        /// queued before this runs. The first version read the raw frame's hue and said 5 degrees for a red ring and a cyan
        /// one: the night grade turns everything blue, and against blue ground every model reads orange (critic r7: "the
        /// two sides can't be told apart at the standard view").</summary>
        void SideSignalStep()
        {
            var c = Camera.main; const int w = 1600, h = 900;
            void Only(int side)   // colour just this side (-1: neither)
            {
                for (int i = 0; i < Vehicles.Count && i < hueV.Length; i++) Vehicles[i].Team = hueV[i] == side ? side : -1;
                for (int i = 0; i < Units.Count && i < hueU.Length; i++) Units[i].Team = hueU[i] == side ? side : -1;
            }
            switch (hueStep)
            {
                case 0:
                    hueScale = Time.timeScale; Time.timeScale = 0f;
                    hueMasks = new bool[2][]; hueRaw = new bool[2][]; hueMen = new bool[2][]; hueFrames = new Color32[3][];
                    for (int side = 0; side < 2; side++)
                    {
                        var mask = new bool[w * h];
                        foreach (var v in Vehicles) if (v.Team == side) Or(mask, Silhouette(c, v.gameObject, w, h));
                        foreach (var u in Units) if (u.Team == side) Or(mask, Silhouette(c, u.gameObject, w, h));
                        hueRaw[side] = mask; hueMasks[side] = Grow(mask, w, h, Mathf.Max(4, w / 50));
                        var men = new bool[w * h];   // the side's figures alone (their rings, if on, inside the grown edge)
                        foreach (var u in Units) if (u.Team == side) Or(men, Silhouette(c, u.gameObject, w, h));
                        hueMen[side] = Grow(men, w, h, Mathf.Max(2, w / 200));
                    }
                    hueV = new int[Vehicles.Count]; hueU = new int[Units.Count];
                    for (int i = 0; i < Vehicles.Count; i++) hueV[i] = Vehicles[i].Team;
                    for (int i = 0; i < Units.Count; i++) hueU[i] = Units[i].Team;
                    Only(0); hueStep = 1; return;
                case 1: hueFrames[0] = Grab(c, w, h); Only(1); hueStep = 2; return;
                case 2: hueFrames[1] = Grab(c, w, h); Only(-1); hueStep = 3; return;
            }
            var off = Grab(c, w, h);
            for (int i = 0; i < Vehicles.Count && i < hueV.Length; i++) Vehicles[i].Team = hueV[i];
            for (int i = 0; i < Units.Count && i < hueU.Length; i++) Units[i].Team = hueU[i];
            Time.timeScale = hueScale;
            var hue = new float[2]; var strength = new float[2]; var mean = new Vector2[2]; var n = new int[2];
            for (int side = 0; side < 2; side++)
            {
                double a = 0, b = 0; var on = hueFrames[side];
                for (int i = 0; i < off.Length; i++)
                {
                    if (!hueMasks[side][i]) continue;
                    double r = on[i].r - off[i].r, g = on[i].g - off[i].g, bl = on[i].b - off[i].b;
                    a += r - 0.5 * (g + bl); b += 0.8660254 * (g - bl); n[side]++;   // opponent plane: red at 0, green 120, blue 240
                }
                hue[side] = Mathf.Repeat(Mathf.Atan2((float)b, (float)a) * Mathf.Rad2Deg, 360f);
                mean[side] = n[side] > 0 ? new Vector2((float)(a / n[side] / 255.0), (float)(b / n[side] / 255.0)) : Vector2.zero;
                strength[side] = mean[side].magnitude;
            }
            float gap = n[0] > 0 && n[1] > 0 ? Mathf.Abs(Mathf.DeltaAngle(hue[0], hue[1])) : -1f;
            // the same for the figures alone: the tank's ring and lamps are strong enough to carry a side by themselves
            var menHue = new float[2]; var menStrength = new float[2]; int menSides = 0;
            for (int side = 0; side < 2; side++)
            {
                double a = 0, b = 0; int k = 0; var on = hueFrames[side];
                for (int i = 0; i < off.Length; i++)
                {
                    if (!hueMen[side][i]) continue;
                    double r = on[i].r - off[i].r, g = on[i].g - off[i].g, bl = on[i].b - off[i].b;
                    a += r - 0.5 * (g + bl); b += 0.8660254 * (g - bl); k++;
                }
                if (k > 0) menSides++;
                menHue[side] = Mathf.Repeat(Mathf.Atan2((float)b, (float)a) * Mathf.Rad2Deg, 360f);
                menStrength[side] = k > 0 ? new Vector2((float)(a / k / 255.0), (float)(b / k / 255.0)).magnitude : 0f;
            }
            float menGap = menSides == 2 ? Mathf.Abs(Mathf.DeltaAngle(menHue[0], menHue[1])) : -1f;
            // cross-paint: the OTHER side's added colour over this side's own silhouettes (a ring drawn over a man beside
            // an enemy tank painted him the enemy's colour; critic r8). 0 when nothing of one side lands on the other.
            var cross = new float[2];
            for (int side = 0; side < 2; side++)
            {
                var on = hueFrames[1 - side]; double sum = 0; int m = 0;
                for (int i = 0; i < off.Length; i++)
                {
                    if (!hueRaw[side][i]) continue;
                    double r = on[i].r - off[i].r, g = on[i].g - off[i].g, bl = on[i].b - off[i].b;
                    double a = r - 0.5 * (g + bl), b = 0.8660254 * (g - bl); sum += System.Math.Sqrt(a * a + b * b); m++;
                }
                cross[side] = m > 0 ? (float)(sum / m / 255.0) : 0f;
            }
            File.WriteAllText(huePath, string.Format(CultureInfo.InvariantCulture,
                "{{\"side_hue\":[{0:0},{1:0}],\"side_strength\":[{2:0.0000},{3:0.0000}],\"side_pixels\":[{4},{5}],\"side_hue_gap\":{6:0},\"side_split\":{7:0.0000},\"side_cross\":[{8:0.0000},{9:0.0000}],\"men_hue\":[{10:0},{11:0}],\"men_strength\":[{12:0.0000},{13:0.0000}],\"men_gap\":{14:0}}}",
                hue[0], hue[1], strength[0], strength[1], n[0], n[1], gap, (mean[0] - mean[1]).magnitude, cross[0], cross[1],
                menHue[0], menHue[1], menStrength[0], menStrength[1], menGap));
            huePath = null; hueFrames = null; hueMasks = null; hueRaw = null; hueMen = null; hueStep = 0;
        }

        static void Or(bool[] into, bool[] m) { for (int i = 0; i < into.Length; i++) into[i] |= m[i]; }

        /// <summary>A mask grown by r pixels (a square, in two passes).</summary>
        static bool[] Grow(bool[] m, int w, int h, int r)
        {
            var row = new bool[m.Length]; var o = new bool[m.Length];
            for (int y = 0; y < h; y++)
            {
                int last = -100000;
                for (int x = 0; x < w; x++) { if (m[y * w + x]) last = x; if (x - last <= r) row[y * w + x] = true; }
                last = 100000;
                for (int x = w - 1; x >= 0; x--) { if (m[y * w + x]) last = x; if (last - x <= r) row[y * w + x] = true; }
            }
            for (int x = 0; x < w; x++)
            {
                int last = -100000;
                for (int y = 0; y < h; y++) { if (row[y * w + x]) last = y; if (y - last <= r) o[y * w + x] = true; }
                last = 100000;
                for (int y = h - 1; y >= 0; y--) { if (row[y * w + x]) last = y; if (last - y <= r) o[y * w + x] = true; }
            }
            return o;
        }

        /// <summary>What the frame holds, as numbers: brightness, and the state of everything on the stage.</summary>
        public string Report(Texture2D tex = null, string extra = null)
        {
            var sb = new StringBuilder("{");
            if (extra != null) sb.Append(extra);
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
                // the machine's middle (a label hangs 0.8 radius above it: reported, a walker in frame read sy 1.06, loop 2 r33)
                var sv = Camera.main != null ? Camera.main.WorldToViewportPoint(v.Centre) : Vector3.zero;
                int moving = 0; foreach (var p in v.Parts) if (p.Loose && !p.Fly.Resting) moving++;
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"lod\":{0},\"tris\":{1},\"stage\":\"{2}\",\"hp\":{3:0},\"fire\":{4:0.00},\"loose\":{5},\"moving\":{9},\"sx\":{7:0.000},\"sy\":{8:0.000},\"pose\":\"{6}\"}}", v.Lod, v.TrisDrawn, v.State, v.Hp, v.FireLevel, loose, v.PoseSignature(), sv.x, sv.y, moving);
            }
            sb.Append("],\"buildings\":[");
            for (int i = 0; i < Buildings.Count; i++)
            {
                var b = Buildings[i]; if (i > 0) sb.Append(',');
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"name\":\"{0}\",\"standing\":{1},\"chunks\":{2},\"floating\":{3},\"on_end\":{4}}}", b.name, b.Standing, b.Pieces.Count, b.Floating, b.OnEnd);
            }
            sb.Append("],\"units\":[");
            for (int i = 0; i < Units.Count; i++)
            {
                var u = Units[i]; if (i > 0) sb.Append(',');
                float dist = Camera.main != null ? Vector3.Distance(Camera.main.transform.position, u.Centre) : 0f;
                var su = Camera.main != null ? Camera.main.WorldToViewportPoint(u.Centre + Vector3.up * u.Height * 0.7f * u.transform.lossyScale.y) : Vector3.zero;
                u.Drift(out var hand, out var foot);
                sb.AppendFormat(CultureInfo.InvariantCulture, "{{\"lod\":{0},\"verts\":{1},\"bones\":{2},\"dist\":{3:0.0},\"clip\":\"{4}\",\"dead\":{5},\"sx\":{6:0.000},\"sy\":{7:0.000},\"hand\":[{8:0.000},{9:0.000},{10:0.000}],\"foot\":[{11:0.000},{12:0.000},{13:0.000}],\"rifleInside\":{14:0.00},\"rifleDir\":[{15:0.000},{16:0.000},{17:0.000}]}}", u.Lod, u.VertsDrawn, u.BonesPerLod[u.Lod], dist, u.Clip >= 0 ? Library.Clips[u.Clip].Name : "", u.Dead ? "true" : "false", su.x, su.y, hand.x, hand.y, hand.z, foot.x, foot.y, foot.z, u.RifleInside, u.RifleDir.x, u.RifleDir.y, u.RifleDir.z);
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
            tab = GUILayout.Toolbar(tab, new[] { "Vehicle", "Unit", "Building", "Look" });
            GUILayout.Space(4);
            if (tab == 0) VehiclePanel(); else if (tab == 1) UnitPanel(); else if (tab == 2) BuildingPanel(); else LookPanel();
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
            foreach (var (l, c) in b) if (GUILayout.Button(l)) foreach (var part in c.Split(';')) if (part.Trim().Length > 0) Queue(part.Trim());
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

        Vector2 houseScroll;
        void BuildingPanel()
        {
            Row(("Ruins", "set Ruins"), ("Houses", "set Houses"), ("Military", "set Military"));
            Row(("Shell it", "shell"), ("Shell x5", "shell; shell; shell; shell; shell"), ("Rebuild", "rebuild"));
            GUILayout.Label("Building in " + BuildingSet);
            houseScroll = GUILayout.BeginScrollView(houseScroll, GUILayout.Height(180));
            foreach (var h in BuildingRig.Kit(BuildingSet)) if (GUILayout.Button($"{h.Name}  ({h.Chunks.Length} chunks)")) Queue("house " + h.Name);
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
            foreach (var b in Buildings) sb.AppendFormat("<b>{0}</b> {1}/{2} chunks standing\n   {3}\n", b.name, b.Standing, b.Pieces.Count, b.LastEvent);
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
