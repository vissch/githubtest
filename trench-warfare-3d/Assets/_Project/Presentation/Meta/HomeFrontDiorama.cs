// Phase: B6 / docs/21 phase 6 (implemented) — the Home Front: a city block behind the menus, built at runtime inside the
// menu scene (no scene asset). Each building is a sliced house from Resources/Env (HouseKit), drawn as one mesh with
// the chunks above its stage's height masked off (HomeFrontStages), so stage 0 is the ground floor and stage 3 the
// whole building; expanding one raises the mask over RiseSeconds. Stages add chimneys that puff Book.Smoke and
// amber point lights; the faction sets the key light (Iron a warm dusk, Brass a cool night). Picking is a ray
// against a box collider per building while the pointer is not over the UI; the ring under a building marks the
// hovered and the chosen one. Talks to the screens through IHomeFrontView; MetaBoot makes one on demand.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.Rendering;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;

namespace TW.Presentation.Meta
{
    public sealed class HomeFrontDiorama : MonoBehaviour, IHomeFrontView
    {
        public const float BlockSize = 48f, RiseSeconds = 0.6f, PuffEvery = 0.35f, PickReach = 300f, LampHeight = 2.2f;
        public const int MaxLamps = 8;
        public static readonly Color IronKey = new Color(1f, 0.86f, 0.70f), BrassKey = new Color(0.58f, 0.64f, 0.82f);
        public const float IronKeyIntensity = 1.15f, BrassKeyIntensity = 0.75f;
        public static readonly Color IronPlate = new Color(0.36f, 0.33f, 0.29f), BrassPlate = new Color(0.27f, 0.25f, 0.23f), RoadColor = new Color(0.22f, 0.20f, 0.18f);
        public static readonly Color HoverRing = new Color(1f, 0.72f, 0.30f), PickedRing = new Color(0.92f, 0.88f, 0.78f), ChimneyColor = new Color(0.30f, 0.24f, 0.21f), LampColor = new Color(1f, 0.72f, 0.40f);
        static readonly Bounds Everything = new Bounds(Vector3.zero, Vector3.one * 400f);
        const string PickPrefix = "pick:";

        sealed class Placed
        {
            public BuildingView View; public HouseKit.House House; public Mesh Whole; public Matrix4x4 Matrix;
            public float Shown, Goal; public HouseKit.ChunkMask Mask; public GameObject Pick; public float Extent; public Vector3 Foot;
        }

        readonly Dictionary<string, HouseKit.House[]> sets = new Dictionary<string, HouseKit.House[]>();
        readonly Dictionary<string, Material> setMaterials = new Dictionary<string, Material>(), maskedMaterials = new Dictionary<string, Material>();
        readonly Dictionary<HouseKit.House, Mesh> wholes = new Dictionary<HouseKit.House, Mesh>();
        readonly List<Placed> placed = new List<Placed>();
        readonly List<Light> lamps = new List<Light>();
        readonly List<Vector3> chimneyTops = new List<Vector3>();
        readonly List<Matrix4x4> chimneyMatrices = new List<Matrix4x4>();
        readonly Matrix4x4[] one = new Matrix4x4[1]; readonly Vector4[] oneMask = new Vector4[1];
        MaterialPropertyBlock block;
        Mesh plate, road, ring, chimney; Material plateMat, roadMat, hoverMat, pickedMat, chimneyMat;
        FlipbookFx smoke; float nextPuff;
        Texture2D atlas; bool atlasLoaded;
        Light key; Color savedKeyColor; float savedKeyIntensity; bool keyHeld;
        bool shown;

        public MetaCamera Rig { get; } = new MetaCamera();
        public event Action<string> Picked;
        public string Highlighted { get; private set; }
        public string Hover { get; private set; }
        public byte Faction { get; private set; }
        public int Count => placed.Count;

        void Awake()
        {
            plate = MetaMeshes.Box(new Vector3(BlockSize + 6f, 0.6f, BlockSize + 6f), new Vector3(0f, -0.3f, 0f), "HomeFrontPlate");
            road = MetaMeshes.Ring(BlockSize * 0.34f, BlockSize * 0.42f, 48, "HomeFrontRoad");
            ring = MetaMeshes.Ring(0.92f, 1.06f, 32, "HomeFrontRing");
            chimney = MetaMeshes.Cylinder(0.32f, 0.24f, 1.7f, 10, "HomeFrontChimney");
            plateMat = MetaMeshes.Toon(IronPlate, 0.8f); roadMat = MetaMeshes.Toon(RoadColor, 0.5f);
            hoverMat = MetaMeshes.Toon(HoverRing, 0f); pickedMat = MetaMeshes.Toon(PickedRing, 0f); chimneyMat = MetaMeshes.Toon(ChimneyColor, 1f);
            block = new MaterialPropertyBlock();
        }

        // ---- IHomeFrontView ------------------------------------------------------------------------------------------
        public void Show(byte faction, IReadOnlyList<BuildingView> buildings)
        {
            Faction = faction; shown = true;
            gameObject.SetActive(true);
            ClearPlaced();
            plateMat.SetColor("_BaseColor", faction == 1 ? BrassPlate : IronPlate);
            for (int i = 0; i < buildings.Count; i++)
            {
                var view = buildings[i];
                var house = HouseFor(view.Set, view.Model);
                if (house == null) continue;
                var rot = Quaternion.Euler(0f, view.Yaw, 0f);
                var p = new Placed { View = view, House = house, Whole = WholeOf(house), Matrix = Matrix4x4.TRS(view.Place, rot, Vector3.one), Shown = view.ShownHeight, Goal = view.ShownHeight };
                p.Mask = HomeFrontStages.HiddenAt(house, p.Shown);
                p.Extent = Mathf.Max(house.Bounds.extents.x, house.Bounds.extents.z) + 0.6f;
                p.Foot = p.Matrix.MultiplyPoint3x4(new Vector3(house.Bounds.center.x, 0f, house.Bounds.center.z));
                p.Pick = new GameObject(PickPrefix + view.Id);
                p.Pick.transform.SetParent(transform, false);
                p.Pick.transform.SetPositionAndRotation(view.Place, rot);
                var col = p.Pick.AddComponent<BoxCollider>(); col.center = house.Bounds.center; col.size = house.Bounds.size;
                placed.Add(p);
            }
            RebuildLamps();
            TakeLight();
            smoke ??= new FlipbookFx();
            Rig.Take(Camera.main, MetaCamera.Mode.Orbit, new Vector3(0f, 1.5f, 0f));
        }

        public void SetStage(BuildingView view, bool animate)
        {
            var p = Find(view.Id);
            if (p == null) return;
            p.View = view; p.Goal = view.ShownHeight;
            if (!animate) { p.Shown = p.Goal; p.Mask = HomeFrontStages.HiddenAt(p.House, p.Shown); }
            RebuildLamps();
        }

        public void Highlight(string id) => Highlighted = id;

        public void Hide()
        {
            shown = false;
            Rig.Release();
            ReleaseLight();
            gameObject.SetActive(false);
        }

        /// <summary>How much of a building shows right now (0..1), for the tests and the screen's stage animation.</summary>
        public float ShownOf(string id) => Find(id)?.Shown ?? 0f;

        // ---- per frame ----------------------------------------------------------------------------------------------
        void Update()
        {
            if (!shown) return;
            float dt = Time.unscaledDeltaTime, now = Time.unscaledTime;
            for (int i = 0; i < placed.Count; i++)
            {
                var p = placed[i];
                if (p.Shown == p.Goal) continue;
                p.Shown = Mathf.MoveTowards(p.Shown, p.Goal, dt / RiseSeconds);
                p.Mask = HomeFrontStages.HiddenAt(p.House, p.Shown);
            }
            var mouse = Mouse.current;
            Vector2 at = mouse != null ? mouse.position.ReadValue() : default;
            bool free = mouse != null && !HudBridge.IsPointerOverUi(at);
            Rig.Tick(dt, free, now);
            Hover = null;
            if (free && Rig.TryRay(at, out var ray) && Physics.Raycast(ray, out var hit, PickReach))
            {
                string n = hit.collider != null ? hit.collider.name : "";
                if (n.StartsWith(PickPrefix, StringComparison.Ordinal)) Hover = n.Substring(PickPrefix.Length);
            }
            if (Hover != null && mouse.leftButton.wasPressedThisFrame) Picked?.Invoke(Hover);
            Draw(now);
        }

        void Draw(float now)
        {
            var rp = new RenderParams(plateMat) { worldBounds = Everything, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true };
            Graphics.RenderMesh(rp, plate, 0, Matrix4x4.identity);
            rp.material = roadMat; rp.shadowCastingMode = ShadowCastingMode.Off;
            Graphics.RenderMesh(rp, road, 0, Matrix4x4.Translate(new Vector3(0f, 0.02f, 0f)));

            chimneyMatrices.Clear(); chimneyTops.Clear();
            for (int i = 0; i < placed.Count; i++)
            {
                var p = placed[i];
                if (p.Whole != null && maskedMaterials.TryGetValue(p.View.Set, out var mat))
                {
                    one[0] = p.Matrix; oneMask[0] = p.Mask.Packed;
                    block.SetVectorArray("_ChunkMask", oneMask);
                    var hp = new RenderParams(mat) { worldBounds = Everything, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true, matProps = block };
                    Graphics.RenderMeshInstanced(hp, p.Whole, 0, one, 1);
                }
                int chimneys = p.View.Chimneys;
                if (chimneys > 0)
                {
                    var b = p.House.Bounds;
                    float roof = Mathf.Min(b.max.y, HomeFrontStages.Limit(p.House, p.Shown)) - 0.3f;
                    for (int c = 0; c < chimneys; c++)
                    {
                        var local = new Vector3(b.center.x + (c - (chimneys - 1) * 0.5f) * 1.4f, roof, b.center.z);
                        var world = p.Matrix.MultiplyPoint3x4(local);
                        chimneyMatrices.Add(Matrix4x4.TRS(world, Quaternion.identity, Vector3.one));
                        chimneyTops.Add(world + Vector3.up * 1.7f);
                    }
                }
                bool hover = Hover != null && Hover == p.View.Id, picked = Highlighted != null && Highlighted == p.View.Id;
                if (hover || picked)
                {
                    rp.material = picked ? pickedMat : hoverMat;
                    Graphics.RenderMesh(rp, ring, 0, Matrix4x4.TRS(p.Foot + Vector3.up * 0.04f, Quaternion.identity, new Vector3(p.Extent, 1f, p.Extent)));
                }
            }
            if (chimneyMatrices.Count > 0)
            {
                var cp = new RenderParams(chimneyMat) { worldBounds = Everything, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true };
                Graphics.RenderMeshInstanced(cp, chimney, 0, chimneyMatrices);
                if (smoke != null && now >= nextPuff)
                {
                    nextPuff = now + PuffEvery;
                    for (int c = 0; c < chimneyTops.Count; c++)
                        smoke.Add(FlipbookFx.Book.Smoke, chimneyTops[c], 0.7f, 2.4f, velocity: new Vector3(0.35f, 0.7f, 0.1f), grow: 0.6f, alpha: 0.35f);
                }
            }
            smoke?.Draw(now, Everything);
        }

        // ---- buildings, lamps, light ---------------------------------------------------------------------------------
        Placed Find(string id)
        {
            for (int i = 0; i < placed.Count; i++) if (placed[i].View.Id == id) return placed[i];
            return null;
        }

        HouseKit.House HouseFor(string set, string model)
        {
            if (string.IsNullOrEmpty(set) || string.IsNullOrEmpty(model)) return null;
            if (!sets.TryGetValue(set, out var houses))
            {
                var mat = SetMaterial(set);
                string folder = "Env/" + set + "/";
                houses = HouseKit.Load(set, chunk => new BattlefieldKit.Module { Mesh = Resources.Load<Mesh>(folder + chunk), Material = mat, Drawn = false, Name = null }, 0);
                sets[set] = houses;
            }
            foreach (var h in houses) if (h.Name == model) return h;
            Debug.LogWarning("HomeFrontDiorama: no model " + model + " in the " + set + " set");
            return null;
        }

        Material SetMaterial(string set)
        {
            if (setMaterials.TryGetValue(set, out var m)) return m;
            if (!atlasLoaded) { atlasLoaded = true; atlas = Resources.Load<Texture2D>("Env/EnvAtlas"); }
            m = MetaMeshes.Toon(Color.white, 1.3f);
            int cell = Array.IndexOf(BattlefieldKit.EnvSets, set);
            if (atlas != null && cell >= 0)
            {
                m.SetTexture("_BaseMap", atlas);
                m.SetTextureScale("_BaseMap", new Vector2(1f / BattlefieldKit.EnvCols, 1f / BattlefieldKit.EnvRows));
                m.SetTextureOffset("_BaseMap", BattlefieldKit.EnvOffset(cell));
            }
            setMaterials[set] = m;
            var masked = new Material(m) { hideFlags = HideFlags.HideAndDontSave, enableInstancing = true };
            masked.EnableKeyword("_CHUNKMASK");
            maskedMaterials[set] = masked;
            return m;
        }

        Mesh WholeOf(HouseKit.House house)
        {
            if (!wholes.TryGetValue(house, out var mesh)) { mesh = HouseKit.BuildWhole(house); wholes[house] = mesh; }
            return mesh;
        }

        void RebuildLamps()
        {
            foreach (var l in lamps) if (l != null) Destroy(l.gameObject);
            lamps.Clear();
            for (int i = 0; i < placed.Count && lamps.Count < MaxLamps; i++)
            {
                var p = placed[i]; var b = p.House.Bounds;
                for (int k = 0; k < p.View.Lamps && lamps.Count < MaxLamps; k++)
                {
                    var go = new GameObject("lamp");
                    go.transform.SetParent(transform, false);
                    var local = new Vector3(k % 2 == 0 ? b.min.x - 0.6f : b.max.x + 0.6f, LampHeight, k < 2 ? b.max.z + 0.6f : b.min.z - 0.6f);
                    go.transform.position = p.Matrix.MultiplyPoint3x4(local);
                    var l = go.AddComponent<Light>();
                    l.type = LightType.Point; l.color = LampColor; l.intensity = Faction == 1 ? 2.2f : 1.4f; l.range = 7f; l.shadows = LightShadows.None;
                    lamps.Add(l);
                }
            }
        }

        void TakeLight()
        {
            if (!keyHeld)
            {
                key = null;
                foreach (var l in FindObjectsByType<Light>(FindObjectsSortMode.None)) if (l.type == LightType.Directional) { key = l; break; }
                if (key == null) return;
                savedKeyColor = key.color; savedKeyIntensity = key.intensity; keyHeld = true;
            }
            key.color = Faction == 1 ? BrassKey : IronKey;
            key.intensity = Faction == 1 ? BrassKeyIntensity : IronKeyIntensity;
        }

        void ReleaseLight()
        {
            if (keyHeld && key != null) { key.color = savedKeyColor; key.intensity = savedKeyIntensity; }
            keyHeld = false;
        }

        void ClearPlaced()
        {
            foreach (var p in placed) if (p.Pick != null) Destroy(p.Pick);
            placed.Clear();
            foreach (var l in lamps) if (l != null) Destroy(l.gameObject);
            lamps.Clear();
            Hover = null; Highlighted = null;
        }

        void OnDestroy()
        {
            Rig.Release(); ReleaseLight();
            smoke?.Dispose(); smoke = null;
            foreach (var m in wholes.Values) if (m != null) Destroy(m);
            wholes.Clear();
            foreach (var m in setMaterials.Values) if (m != null) Destroy(m);
            foreach (var m in maskedMaterials.Values) if (m != null) Destroy(m);
            setMaterials.Clear(); maskedMaterials.Clear();
            foreach (var m in new[] { plate, road, ring, chimney }) if (m != null) Destroy(m);
            foreach (var m in new[] { plateMat, roadMat, hoverMat, pickedMat, chimneyMat }) if (m != null) Destroy(m);
        }
    }
}
