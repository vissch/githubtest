// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map behind the campaign menu: the continent (ContinentMesh)
// with a pin per country node tinted by its state (locked plate, available amber, current power-blue and pulsing,
// complete bone), a dashed front-line ribbon through the nodes in order, a ring under the hovered pin, and the fog
// of the unexplored map (MapFog): a sheet draped over the land that opens round every node the player can reach,
// thinner over the charted sea and along the known front, rebaked when a state changes; a locked pin is a stub
// under the haze. Built at runtime in the menu scene, no asset.
// Picking is a ray against a sphere collider per pin while the pointer is not over the UI; Focus eases the map
// camera onto a node. Talks to the screen through IStrategicMapView. Every draw goes through FrameBudget.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.Rendering;

namespace TW.Presentation.Meta
{
    public sealed class StrategicMapView : MonoBehaviour, IStrategicMapView
    {
        public const float PinHeight = 2.6f, PinRadius = 0.9f, PickRadius = 2.4f, RibbonWidth = 0.7f, RibbonDash = 2f, PulseSeconds = 1.6f, PickReach = 400f;
        /// <summary>A locked pin stands this share of a pin's height: a stub that stays under the fog sheet.</summary>
        public const float LockedPinShare = 0.35f;
        /// <summary>The fog sheet reaches this far past the land on every side, the same as the sea plate.</summary>
        public const float FogMargin = 14f;
        public static readonly Color SeaColor = new Color(0.16f, 0.22f, 0.27f), Lowland = new Color(0.45f, 0.47f, 0.30f), UplandColor = new Color(0.52f, 0.46f, 0.32f), HighlandColor = new Color(0.62f, 0.60f, 0.56f);
        public static readonly Color LockedColor = new Color(0.36f, 0.35f, 0.34f), AvailableColor = new Color(1f, 0.72f, 0.30f), CurrentColor = new Color(0.45f, 0.70f, 1f), CompleteColor = new Color(0.92f, 0.88f, 0.78f);
        public static readonly Color RibbonColor = new Color(0.55f, 0.16f, 0.12f), HoverRing = new Color(1f, 0.85f, 0.55f), KeyLight = new Color(0.82f, 0.84f, 0.88f);
        public static readonly Color FogColor = new Color(0.80f, 0.82f, 0.86f);
        public const float KeyIntensity = 0.95f;
        static readonly Bounds Everything = new Bounds(new Vector3(ContinentMesh.Width * 0.5f, 0f, ContinentMesh.Length * 0.5f), Vector3.one * 600f);
        const string PickPrefix = "pick:";

        /// <summary>The fog texel grid: the sheet's rectangle, two texels a metre.</summary>
        public static MapFog.Grid FogGrid => new MapFog.Grid(-FogMargin, -FogMargin, ContinentMesh.Width * ContinentMesh.Cell + 2f * FogMargin, ContinentMesh.Length * ContinentMesh.Cell + 2f * FogMargin);

        sealed class Pin { public NodeView View; public Vector3 Pos; public GameObject Pick; }

        float[] heights;
        Mesh land, sea, pin, disc, ring, ribbon, fog;
        Material seaMat, ribbonMat, ringMat, fogMat; readonly Material[] bandMats = new Material[3]; readonly Material[] stateMats = new Material[4];
        Texture2D fogTex; Color32[] fogPixels; bool fogDirty;
        readonly List<Vector2> holes = new List<Vector2>();
        readonly List<Vector2> frontLine = new List<Vector2>();   // the front's world XZ, for the fog's band along it
        readonly List<Pin> pins = new List<Pin>();
        Light key; Color savedKeyColor; float savedKeyIntensity; bool keyHeld;
        bool shown;

        public MetaCamera Rig { get; } = new MetaCamera();
        public event Action<string> Picked;
        public string Hover { get; private set; }
        public int Count => pins.Count;
        public float[] Heights => heights;
        /// <summary>The fog has a bake waiting for the next frame (a state changed since the last one).</summary>
        public bool FogDirty => fogDirty;

        void Awake()
        {
            heights = ContinentMesh.Heights(ContinentMesh.Seed);
            land = ContinentMesh.Land(heights);
            sea = ContinentMesh.Sea();
            fog = ContinentMesh.FogSheet(heights, MapFog.Sheet, FogMargin);
            pin = MetaMeshes.Cylinder(PinRadius, 0.25f, PinHeight, 10, "MapPin");
            disc = MetaMeshes.Disc(PinRadius * 1.6f, 24, "MapPinFoot");
            ring = MetaMeshes.Ring(PinRadius * 1.8f, PinRadius * 2.1f, 32, "MapRing");
            seaMat = MetaMeshes.Toon(SeaColor, 0f);
            bandMats[0] = MetaMeshes.Toon(Lowland, 0.6f); bandMats[1] = MetaMeshes.Toon(UplandColor, 0.6f); bandMats[2] = MetaMeshes.Toon(HighlandColor, 0.6f);
            stateMats[(int)NodeState.Locked] = MetaMeshes.Toon(LockedColor); stateMats[(int)NodeState.Available] = MetaMeshes.Toon(AvailableColor);
            stateMats[(int)NodeState.Current] = MetaMeshes.Toon(CurrentColor); stateMats[(int)NodeState.Complete] = MetaMeshes.Toon(CompleteColor);
            ribbonMat = MetaMeshes.Toon(RibbonColor, 0f); ringMat = MetaMeshes.Toon(HoverRing, 0f);
            fogMat = MetaMeshes.Transparent(FogColor);   // null without URP's Unlit shader: then the map has no fog
        }

        // ---- IStrategicMapView ---------------------------------------------------------------------------------------
        public void Show(IReadOnlyList<NodeView> nodes, IReadOnlyList<string> frontLine)
        {
            shown = true;
            gameObject.SetActive(true);
            ClearPins();
            var centre = Vector3.zero;
            for (int i = 0; i < nodes.Count; i++)
            {
                var p = new Pin { View = nodes[i], Pos = ContinentMesh.WorldOf(heights, nodes[i].MapPos) };
                p.Pick = new GameObject(PickPrefix + nodes[i].Id);
                p.Pick.transform.SetParent(transform, false);
                p.Pick.transform.position = p.Pos + Vector3.up * PinHeight * 0.5f;
                p.Pick.AddComponent<SphereCollider>().radius = PickRadius;
                pins.Add(p); centre += p.Pos;
            }
            if (pins.Count > 0) centre /= pins.Count; else centre = new Vector3(ContinentMesh.Width * 0.5f, 0f, ContinentMesh.Length * 0.5f);
            if (ribbon != null) { Destroy(ribbon); ribbon = null; }
            var points = new List<Vector3>();
            this.frontLine.Clear();
            if (frontLine != null)
                foreach (var id in frontLine) { var p = Find(id); if (p != null) { points.Add(p.Pos); this.frontLine.Add(new Vector2(p.Pos.x, p.Pos.z)); } }
            if (points.Count >= 2) ribbon = MetaMeshes.Ribbon(points, RibbonWidth, 0.18f, RibbonDash, "FrontLine");
            fogDirty = true;
            TakeLight();
            Rig.Take(Camera.main, MetaCamera.Mode.Map, centre);
        }

        public void SetState(string id, NodeState state)
        {
            var p = Find(id);
            if (p == null) return;
            if (p.View.State != state) fogDirty = true;
            var v = p.View; v.State = state; p.View = v;
        }

        public void Focus(string id, bool animate)
        {
            var p = Find(id);
            if (p != null) Rig.Focus(p.Pos, animate);
        }

        public void Hide()
        {
            shown = false;
            Rig.Release();
            ReleaseLight();
            gameObject.SetActive(false);
        }

        public NodeState StateOf(string id) => Find(id)?.View.State ?? NodeState.Locked;

        // ---- per frame ----------------------------------------------------------------------------------------------
        void Update()
        {
            if (!shown) return;
            float dt = Time.unscaledDeltaTime, now = Time.unscaledTime;
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
            var rp = new RenderParams(seaMat) { worldBounds = Everything, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true };
            FrameBudget.Draw(rp, sea, 0, Matrix4x4.identity);
            rp.shadowCastingMode = ShadowCastingMode.On;
            for (int b = 0; b < 3; b++) { rp.material = bandMats[b]; FrameBudget.Draw(rp, land, b, Matrix4x4.identity); }
            if (ribbon != null) { rp.material = ribbonMat; rp.shadowCastingMode = ShadowCastingMode.Off; FrameBudget.Draw(rp, ribbon, 0, Matrix4x4.identity); }
            for (int i = 0; i < pins.Count; i++)
            {
                var p = pins[i];
                bool locked = p.View.State == NodeState.Locked;
                float pulse = p.View.State == NodeState.Current ? 1f + 0.08f * Mathf.Sin(now * Mathf.PI * 2f / PulseSeconds) : 1f;
                rp.material = stateMats[(int)p.View.State]; rp.shadowCastingMode = ShadowCastingMode.On;
                FrameBudget.Draw(rp, pin, 0, Matrix4x4.TRS(p.Pos, Quaternion.identity, new Vector3(pulse, locked ? LockedPinShare : 1f, pulse)));
                rp.shadowCastingMode = ShadowCastingMode.Off;
                FrameBudget.Draw(rp, disc, 0, Matrix4x4.TRS(p.Pos + Vector3.up * 0.08f, Quaternion.identity, Vector3.one));
                if (Hover != null && Hover == p.View.Id) { rp.material = ringMat; FrameBudget.Draw(rp, ring, 0, Matrix4x4.TRS(p.Pos + Vector3.up * 0.12f, Quaternion.identity, Vector3.one)); }
            }
            if (fogMat != null)
            {
                if (fogDirty) BakeFog();
                rp.material = fogMat; rp.shadowCastingMode = ShadowCastingMode.Off; rp.receiveShadows = false;
                FrameBudget.Draw(rp, fog, 0, Matrix4x4.identity);
            }
        }

        /// <summary>The fog mask for the pins' states: clear round every node that is not locked. The texture is made
        /// once and rewritten; the mask itself is MapFog's arithmetic.</summary>
        void BakeFog()
        {
            fogDirty = false;
            var grid = FogGrid;
            if (fogTex == null)
            {
                fogTex = new Texture2D(grid.W, grid.H, TextureFormat.RGBA32, false) { name = "MapFog", hideFlags = HideFlags.HideAndDontSave, wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear };
                fogPixels = new Color32[grid.Count];
                fogMat.SetTexture("_BaseMap", fogTex);
            }
            holes.Clear();
            for (int i = 0; i < pins.Count; i++) if (pins[i].View.State != NodeState.Locked) holes.Add(new Vector2(pins[i].Pos.x, pins[i].Pos.z));
            var alpha = MapFog.Alpha(grid, holes, ContinentMesh.Seed, heights, frontLine);   // thinner over the sea, a band along the front
            for (int i = 0; i < alpha.Length; i++) fogPixels[i] = new Color32(255, 255, 255, (byte)Mathf.RoundToInt(Mathf.Clamp01(alpha[i]) * 255f));
            fogTex.SetPixels32(fogPixels);
            fogTex.Apply(false, false);
        }

        // ---- pins and light -------------------------------------------------------------------------------------------
        Pin Find(string id)
        {
            for (int i = 0; i < pins.Count; i++) if (pins[i].View.Id == id) return pins[i];
            return null;
        }

        void ClearPins()
        {
            foreach (var p in pins) if (p.Pick != null) Destroy(p.Pick);
            pins.Clear(); Hover = null;
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
            key.color = KeyLight; key.intensity = KeyIntensity;
        }

        void ReleaseLight()
        {
            if (keyHeld && key != null) { key.color = savedKeyColor; key.intensity = savedKeyIntensity; }
            keyHeld = false;
        }

        void OnDestroy()
        {
            Rig.Release(); ReleaseLight();
            foreach (var m in new[] { land, sea, pin, disc, ring, ribbon, fog }) if (m != null) Destroy(m);
            foreach (var m in new[] { seaMat, ribbonMat, ringMat, fogMat }) if (m != null) Destroy(m);
            if (fogTex != null) Destroy(fogTex);
            foreach (var m in bandMats) if (m != null) Destroy(m);
            foreach (var m in stateMats) if (m != null) Destroy(m);
        }
    }
}
