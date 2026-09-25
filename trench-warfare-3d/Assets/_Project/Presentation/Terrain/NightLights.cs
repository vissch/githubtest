// Phase: B2 (implemented) — the warm half of the night look (owner's target, 2026-09-21: a cold blue field lit by
// lanterns, muzzle flashes, shell bursts and flares). Everything here is an ordinary URP point light without shadows,
// which TW/Toon, TW/Water and the soldier shader add in hard steps (TWLocalLights.hlsl), plus additive glow cards.
// Lanterns: one at each composed site (dugouts, shelters, stores), at most MaxLanterns, a lamp on a post with a
// breathing flame; and lamps hung low on the trench wall every thirty metres or so, so the men in the line stand in
// warm light. Fires: a few shattered trees in no man's land still burn, and torches stand where the paths reach the
// dugouts; both carry a real flame (TW/Flame: computed, licking, white to red) over their glow and their light. Every
// lamp and flame light sways a little with the wind, so the pools of light on the mud are never still. Flashes: a pool of eight lights shared by rifle fire and shell bursts; a shot only takes one when it
// is near what the camera looks at, and no more than one every 30 ms, so a 3,000-man firefight costs eight lights.
// A shell's flash is the brightest thing on the field while it lasts and its reach rides on the shell's radius, and
// the hole it leaves keeps a low ember light for two seconds after (three of those, apart from the flash pool).
// Flare: a star shell goes up over the ground ahead of the camera every half minute or so and sinks on its parachute,
// lighting sixty metres of no man's land cold white. Distant fires glow through the fog bank beyond the far edges.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class NightLights : MonoBehaviour
    {
        public SimHost Host;
        public const int MaxLanterns = 12, MaxTrenchLamps = 14, MaxFires = 5, MaxTorches = 6, MaxPropLamps = 12, PoolSize = 8;
        public Color Lantern = new Color(1f, 0.60f, 0.26f), Muzzle = new Color(1f, 0.74f, 0.40f), Burst = new Color(1f, 0.52f, 0.20f), Flare = new Color(0.82f, 0.90f, 1f);
        public float LanternIntensity = 6f, LanternRange = 10f;
        [Tooltip("Seconds between star shells, least and most.")]
        public Vector2 FlareEvery = new Vector2(22f, 40f);

        struct Pooled { public Light Light; public float Born, Life, Peak, Card; }
        Pooled[] pool = new Pooled[PoolSize];   // sized again in Start (lights.poolSize)
        // the limits above, or the knobs lights.* (read at the top of Start)
        int maxLanterns = MaxLanterns, maxTrenchLamps = MaxTrenchLamps, maxFires = MaxFires, maxTorches = MaxTorches, maxPropLamps = MaxPropLamps, poolSize = PoolSize;
        // the hole a shell leaves keeps its heat: a low red light on the rim for a couple of seconds after the flash
        // is gone. Its own three lights, not the flash pool's, or the next shot would take the slot back at once.
        const int AfterglowCount = 3;
        readonly Pooled[] afterglow = new Pooled[AfterglowCount];
        int nextGlow;
        // the strongest burst alive, handed to the shaders so the drawn column and smoke are lit by their own shell
        Vector3 burstAt; float burstPeak, burstBorn, burstLife, burstRange;
        static readonly int BurstId = Shader.PropertyToID("_TWBurst"), BurstColorId = Shader.PropertyToID("_TWBurstColor");
        readonly List<Light> lanterns = new List<Light>();
        readonly List<float> lanternPhase = new List<float>(), lanternBase = new List<float>();
        readonly List<Vector3> lanternHome = new List<Vector3>();
        readonly List<Vector3> firePoints = new List<Vector3>();
        /// <summary>Where the lamps hang and the big fires burn, once built (SmallLife puts moths and ash there).</summary>
        public IReadOnlyList<Vector3> LampPoints => lanternHome;
        public IReadOnlyList<Vector3> FirePoints => firePoints;
        public bool Built => built;
        readonly List<Object> owned = new List<Object>();
        int nextPooled; float lastShot, nextFlare, flareBorn = -100f;

        /// <summary>Fire a star shell on the next frame instead of waiting for the timer (bench scenarios, captures of
        /// the moment). Presentation only: nothing in the sim sees a star shell.</summary>
        public void FireStarShell() { nextFlare = 0f; }
        bool subscribed, built;
        Light flareLight; Transform flare; Vector3 flareFrom;
        Material glow, flareGlow;
        const int EmberCount = 8;
        struct Ember { public Vector3 Pos; public float Born, Life, Size; }
        readonly Ember[] embers = new Ember[EmberCount]; int nextEmber;
        Mesh emberMesh; readonly Vector3[] emberPos = new Vector3[EmberCount * 4]; readonly Color[] emberCol = new Color[EmberCount * 4]; readonly List<Vector4> emberShape = new List<Vector4>(EmberCount * 4);
        float nextDrip, nextGuns;
        Mesh flashMesh; Vector3[] flashPos = new Vector3[PoolSize * 4]; Color[] flashCol = new Color[PoolSize * 4]; readonly List<Vector4> flashShape = new List<Vector4>(PoolSize * 4);
        const float FlareLife = 16f;

        void Start()
        {
            maxLanterns = Mathf.Max(0, Knobs.Get("lights.maxLanterns", MaxLanterns));
            maxTrenchLamps = Mathf.Max(0, Knobs.Get("lights.maxTrenchLamps", MaxTrenchLamps));
            maxFires = Mathf.Max(0, Knobs.Get("lights.maxFires", MaxFires));
            maxTorches = Mathf.Max(0, Knobs.Get("lights.maxTorches", MaxTorches));
            maxPropLamps = Mathf.Max(0, Knobs.Get("lights.maxPropLamps", MaxPropLamps));
            poolSize = Mathf.Max(1, Knobs.Get("lights.poolSize", PoolSize));
            if (poolSize != PoolSize) { pool = new Pooled[poolSize]; flashPos = new Vector3[poolSize * 4]; flashCol = new Color[poolSize * 4]; }
            SceneHooks.Flash = (at, color, peak, reach, life) => Flash(at, color, peak, reach, life);
            for (int i = 0; i < poolSize; i++)
            {
                pool[i].Light = MakeLight("Flash " + i, Muzzle, 0f, 8f);
                pool[i].Light.enabled = false;
            }
            for (int i = 0; i < AfterglowCount; i++)
            {
                afterglow[i].Light = MakeLight("Crater glow " + i, Burst, 0f, 12f);
                afterglow[i].Light.enabled = false;
            }
            glow = new Material(Shader.Find("TW/Glow (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            flareGlow = new Material(glow) { hideFlags = HideFlags.HideAndDontSave };
            owned.Add(glow); owned.Add(flareGlow);
            flareLight = MakeLight("Star shell", Flare, 0f, 95f);
            flare = flareLight.transform; flareLight.enabled = false;
            AddGlowMesh(flare.gameObject, flareGlow, new[] { Vector3.zero }, new[] { new Vector4(9f, .25f, .3f, .2f) }, new[] { new Color(Flare.r, Flare.g, Flare.b, 2.2f) });
            flareGlow.SetColor("_Tint", Color.black);
            nextFlare = Time.time + 6f;
            // one small mesh holds a card per pooled light; its vertices are rewritten each frame (32 of them)
            var host = new GameObject("Flash glows") { hideFlags = HideFlags.DontSave };
            host.transform.SetParent(transform, false);
            var centres = new Vector3[poolSize]; var shapes = new Vector4[poolSize]; var colors = new Color[poolSize];
            for (int i = 0; i < poolSize; i++) shapes[i] = new Vector4(1f, 0f, i * .19f, .15f);
            AddGlowMesh(host, glow, centres, shapes, colors);
            flashMesh = host.GetComponent<MeshFilter>().sharedMesh; flashMesh.MarkDynamic();
            for (int i = 0; i < poolSize * 4; i++) flashShape.Add(new Vector4(1f, 0f, (i / 4) * .19f, .15f));
            // embers: what a shell leaves glowing in its hole for a few seconds. Cards only, no lights.
            var emberHost = new GameObject("Ember glows") { hideFlags = HideFlags.DontSave };
            emberHost.transform.SetParent(transform, false);
            var ec = new Vector3[EmberCount]; var es = new Vector4[EmberCount]; var ecol = new Color[EmberCount];
            AddGlowMesh(emberHost, glow, ec, es, ecol);
            emberMesh = emberHost.GetComponent<MeshFilter>().sharedMesh; emberMesh.MarkDynamic();
            for (int i = 0; i < EmberCount * 4; i++) emberShape.Add(new Vector4(1f, .7f, (i / 4) * .37f, .3f));
            for (int i = 0; i < EmberCount; i++) embers[i].Born = -100f;
        }

        Light MakeLight(string name, Color color, float intensity, float range)
        {
            var go = new GameObject(name) { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            var l = go.AddComponent<Light>();
            l.type = LightType.Point; l.color = color; l.intensity = intensity; l.range = range; l.shadows = LightShadows.None;
            return l;
        }

        void AddGlowMesh(GameObject host, Material material, Vector3[] centres, Vector4[] shapes, Color[] colors)
        {
            var pos = new List<Vector3>(); var corner = new List<Vector2>(); var shape = new List<Vector4>(); var col = new List<Color>(); var tris = new List<int>();
            for (int g = 0; g < centres.Length; g++)
            {
                int v0 = pos.Count;
                for (int k = 0; k < 4; k++)
                {
                    pos.Add(centres[g]); corner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f)); shape.Add(shapes[g]); col.Add(colors[g]);
                }
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 1); tris.Add(v0); tris.Add(v0 + 3); tris.Add(v0 + 2);
            }
            var mesh = new Mesh { name = host.name + " glow", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(pos); mesh.SetUVs(0, corner); mesh.SetUVs(1, shape); mesh.SetColors(col); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);   // the cards are sized in the shader
            owned.Add(mesh);
            host.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r = host.AddComponent<MeshRenderer>();
            r.sharedMaterial = material; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r.receiveShadows = false;
        }

        /// <summary>Lamps at the composed sites, and fires far off beyond the two ends and the far side.</summary>
        void Build(IReadOnlyList<BattlefieldComposer.Site> sites)
        {
            var map = Host.Local.Map;
            var post = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            post.SetColor("_BaseColor", new Color(.20f, .17f, .14f));
            var glass = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            glass.SetColor("_BaseColor", new Color(1f, .78f, .45f)); glass.SetColor("_Emission", new Color(2.4f, 1.35f, .5f));
            owned.Add(post); owned.Add(glass);
            var centres = new List<Vector3>(); var shapes = new List<Vector4>(); var colors = new List<Color>();
            var flameFeet = new List<Vector3>(); var flameShapes = new List<Vector4>();
            float w = map.SizeMeters.x, len = map.SizeMeters.y;
            int step = Mathf.Max(1, Mathf.CeilToInt(sites.Count / (float)maxLanterns));
            for (int i = 0; i < sites.Count && lanterns.Count < maxLanterns; i += step)
            {
                var site = sites[i];
                Vector3 side = site.Rotation * new Vector3(1.5f, 0f, .9f);
                Vector3 foot = site.Position + side; foot.y = RenderGround.Sample(map, foot.x, foot.z);
                Vector3 lamp = foot + Vector3.up * 1.75f;
                Part(PrimitiveType.Cube, foot + Vector3.up * .9f, new Vector3(.09f, 1.8f, .09f), post, site.Rotation);
                Part(PrimitiveType.Cube, lamp + site.Rotation * new Vector3(.18f, .02f, 0f), new Vector3(.42f, .07f, .07f), post, site.Rotation);
                Part(PrimitiveType.Cube, lamp + site.Rotation * new Vector3(.34f, -.22f, 0f), new Vector3(.20f, .30f, .20f), glass, site.Rotation);
                var l = MakeLight("Lantern " + lanterns.Count, Lantern, LanternIntensity, LanternRange);
                l.transform.position = lamp + site.Rotation * new Vector3(.34f, -.20f, 0f);
                lanterns.Add(l); lanternPhase.Add(i * 1.618f);
                centres.Add(l.transform.position); shapes.Add(new Vector4(2.6f, .18f, i * .137f, .5f)); colors.Add(new Color(Lantern.r, Lantern.g, Lantern.b, .55f));
            }
            // a lamp on a post by each observation stand and field gun, on the side the standard view sees (critique round 1:
            // the big imported props stood unlit, grey boxes in the dark). Off the map too: the stands beyond the far edge.
            var props = GetComponent<BattlefieldProps>();
            int propLamps = 0;
            if (props != null)
                foreach (var prop in props.Editable)
                {
                    if (propLamps >= maxPropLamps) break;
                    if (prop.Module.Name != "Siege/ArmouredStand" && prop.Module.Name != "Weapons/FieldGun") continue;
                    var m = prop.Matrix; Vector3 centre = m.GetPosition();
                    Vector3 half = Vector3.Scale(prop.Module.Mesh.bounds.extents, m.lossyScale);
                    var toViewer = props.PreferredFront; toViewer.y = 0f; toViewer.Normalize();
                    var along = Vector3.Cross(Vector3.up, toViewer);
                    Vector3 foot = centre + toViewer * (Mathf.Max(half.x, half.z) + .7f) + along * ((Hash(propLamps, 71) - .5f) * half.z);
                    foot.y = props.Ground(foot.x, foot.z);
                    var facing = Quaternion.LookRotation(-toViewer);
                    Vector3 lamp = foot + Vector3.up * 1.75f;
                    Part(PrimitiveType.Cube, foot + Vector3.up * .9f, new Vector3(.09f, 1.8f, .09f), post, facing);
                    Part(PrimitiveType.Cube, lamp + facing * new Vector3(.18f, .02f, 0f), new Vector3(.42f, .07f, .07f), post, facing);
                    Part(PrimitiveType.Cube, lamp + facing * new Vector3(.34f, -.22f, 0f), new Vector3(.20f, .30f, .20f), glass, facing);
                    var l = MakeLight("Prop lamp " + propLamps, Lantern, LanternIntensity, LanternRange);
                    l.transform.position = lamp + facing * new Vector3(.34f, -.20f, 0f);
                    lanterns.Add(l); lanternPhase.Add(propLamps * 2.39f);
                    centres.Add(l.transform.position); shapes.Add(new Vector4(2.6f, .18f, propLamps * .151f, .5f)); colors.Add(new Color(Lantern.r, Lantern.g, Lantern.b, .55f));
                    propLamps++;
                }
            // lamps in the line: on the wall of a trench cell whose neighbour toward z- is open ground, spaced along x
            int hung = 0;
            for (int z = 1; z < map.NavLength - 1 && hung < maxTrenchLamps; z++)
            for (int x = 3; x < map.NavWidth - 3 && hung < maxTrenchLamps; x++)
            {
                bool trench = ((NavLayer)map.NavLayers[map.NavIndex(x, z)] & NavLayer.Trench) != 0, wall = ((NavLayer)map.NavLayers[map.NavIndex(x, z - 1)] & (NavLayer.Trench | NavLayer.Link)) == 0;
                if (!trench || !wall || (x + (z / 7) * 5) % 15 != 4 || Hash(x, z) < .2f) continue;
                float wx = (x + .5f) * MapData.NavCellSize, wz = (z + .12f) * MapData.NavCellSize;
                Vector3 at = new Vector3(wx, RenderGround.Sample(map, wx, (z + .5f) * MapData.NavCellSize) + 1.35f, wz);
                var terrain = GetComponent<GreyboxTerrainView>();
                if (terrain != null && terrain.Surface != null)
                    foreach (var edge in terrain.Surface.Edges)
                        if (edge.Cell == map.NavIndex(x, z) && edge.Outward.z < -.9f)
                        {
                            var mount = edge.DressCenter - edge.DressOutward * .24f;
                            at.x = mount.x; at.z = mount.z; break;
                        }
                Part(PrimitiveType.Cube, at, new Vector3(.18f, .26f, .18f), glass, Quaternion.identity);
                Part(PrimitiveType.Cube, at + Vector3.up * .2f, new Vector3(.05f, .16f, .05f), post, Quaternion.identity);
                var l = MakeLight("Trench lamp " + hung, Lantern, LanternIntensity * .8f, 8.5f);
                l.transform.position = at + new Vector3(0f, .05f, .25f);
                lanterns.Add(l); lanternPhase.Add(x * 2.3f + z);
                centres.Add(at); shapes.Add(new Vector4(2.0f, .18f, x * .091f, .5f)); colors.Add(new Color(Lantern.r, Lantern.g, Lantern.b, .5f));
                hung++;
            }
            // shattered trees still burning out in the open
            int fires = 0;
            for (int i = 0; i < map.Props.Length && fires < maxFires; i++)
            {
                var prop = map.Props[i];
                if (prop.Kind != PropKind.BrokenTree && prop.Kind != PropKind.Stump) continue;
                if (prop.Pos.z < len * .22f || prop.Pos.z > len * .78f || Hash(i, 61) > .16f) continue;
                Vector3 at = new Vector3(prop.Pos.x, RenderGround.Sample(map, prop.Pos.x, prop.Pos.z) + (prop.Kind == PropKind.Stump ? .5f : 1.3f), prop.Pos.z);
                var l = MakeLight("Fire " + fires, Burst, 7f, 11f);
                l.transform.position = at + Vector3.up * .4f;
                lanterns.Add(l); lanternPhase.Add(i * .77f);
                centres.Add(at); shapes.Add(new Vector4(3.4f, .6f, i * .173f, .4f)); colors.Add(new Color(1f, .45f, .12f, .9f));
                flameFeet.Add(at - Vector3.up * .15f); flameShapes.Add(new Vector4(1.25f, 1.9f, Hash(i, 97), 0f));
                flameFeet.Add(at + new Vector3(.35f, -.3f, .2f)); flameShapes.Add(new Vector4(.7f, 1.0f, Hash(i, 101), 0f));
                fires++;
            }
            // torches on a stake where a path reaches a dugout
            int torches = 0;
            for (int i = 0; i < sites.Count && torches < maxTorches; i++)
            {
                var site = sites[i];
                Vector3 at = site.ApproachEnd;
                if (at == Vector3.zero || (at - site.Position).sqrMagnitude < 9f || Hash(i, 83) < .25f) continue;
                at += site.Rotation * new Vector3(.9f, 0f, 0f);
                if (at.x < 2f || at.z < 2f || at.x > w - 2f || at.z > len - 2f) continue;
                at.y = RenderGround.Sample(map, at.x, at.z);
                Part(PrimitiveType.Cube, at + Vector3.up * .8f, new Vector3(.07f, 1.6f, .07f), post, Quaternion.Euler(0f, i * 37f, 4f));
                Part(PrimitiveType.Cube, at + Vector3.up * 1.62f, new Vector3(.14f, .16f, .14f), post, Quaternion.Euler(0f, i * 37f, 4f));
                Vector3 wick = at + Vector3.up * 1.68f;
                var l = MakeLight("Torch " + torches, new Color(1f, .58f, .22f), 6.5f, 10f);
                l.transform.position = wick + Vector3.up * .25f;
                lanterns.Add(l); lanternPhase.Add(i * 3.1f);
                centres.Add(wick + Vector3.up * .25f); shapes.Add(new Vector4(3.0f, .5f, i * .213f, .4f)); colors.Add(new Color(1f, .50f, .15f, .8f));
                flameFeet.Add(wick); flameShapes.Add(new Vector4(.42f, .80f, Hash(i, 89), 0f));
                torches++;
            }
            // fires on the horizon: seen through the fog bank as low orange glows lying along the ground, half behind it, that
            // flare and sink (a round glow up in the air read as a pink moon: critique round 1, 2026-09-22)
            var fireCentres = new List<Vector3>(); var fireShapes = new List<Vector4>(); var fireColors = new List<Color>();
            for (int k = 0; k < 10; k++)
            {
                float a = Hash(k, 3), b = Hash(k, 5);
                Vector3 p = k < 7 ? new Vector3(-16f - 40f * a, 0f, len * (.05f + .9f * Hash(k, 7))) : new Vector3(w * Hash(k, 9), 0f, len + 30f + 60f * a);
                p.y = GreyboxTerrainView.SkirtLevel + .2f * b;
                fireCentres.Add(p); fireShapes.Add(new Vector4(4.5f + 5f * b, .55f, k * .31f, .6f)); fireColors.Add(new Color(1f, .55f, .2f, .16f + .1f * a));
            }
            var fireHost = new GameObject("Horizon fires") { hideFlags = HideFlags.DontSave };
            fireHost.transform.SetParent(transform, false);
            var fireGlow = new Material(glow) { hideFlags = HideFlags.HideAndDontSave };
            fireGlow.SetFloat("_Squash", .35f); owned.Add(fireGlow);
            AddGlowMesh(fireHost, fireGlow, fireCentres.ToArray(), fireShapes.ToArray(), fireColors.ToArray());
            foreach (var l in lanterns) { lanternBase.Add(l.intensity); lanternHome.Add(l.transform.position); }
            // dugout stoves smoke and rained-on fires steam (CombatFx draws the puffs)
            SceneHooks.SmokeSources.Clear();
            for (int i = 0; i < sites.Count && SceneHooks.SmokeSources.Count < 8; i += step) SceneHooks.SmokeSources.Add(sites[i].Position + sites[i].Rotation * new Vector3(-.8f, 0f, -.6f) + Vector3.up * 2.3f);
            for (int i = 0; i < flameFeet.Count; i += 2) if (flameShapes[i].x > 1f) SceneHooks.SmokeSources.Add(flameFeet[i] + Vector3.up * 1.8f);
            firePoints.Clear();
            for (int i = 0; i < flameFeet.Count; i++) if (flameShapes[i].x > 1f) firePoints.Add(flameFeet[i]);
            if (flameFeet.Count > 0) AddFlameMesh(flameFeet, flameShapes);
            var host = new GameObject("Night glows") { hideFlags = HideFlags.DontSave };
            host.transform.SetParent(transform, false);
            AddGlowMesh(host, glow, centres.ToArray(), shapes.ToArray(), colors.ToArray());
        }

        void AddFlameMesh(List<Vector3> feet, List<Vector4> flameShapes)
        {
            var pos = new List<Vector3>(); var corner = new List<Vector2>(); var shape = new List<Vector4>(); var tris = new List<int>();
            for (int g = 0; g < feet.Count; g++)
            {
                int v0 = pos.Count;
                for (int k = 0; k < 4; k++) { pos.Add(feet[g]); corner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f)); shape.Add(flameShapes[g]); }
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 1); tris.Add(v0); tris.Add(v0 + 3); tris.Add(v0 + 2);
            }
            var mesh = new Mesh { name = "Flames", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(pos); mesh.SetUVs(0, corner); mesh.SetUVs(1, shape); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
            const int n = 64;
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float c = Tile(x / (float)n * 4f, z / (float)n * 4f, 4) * .55f + Tile(x / (float)n * 9f, z / (float)n * 9f, 9) * .45f;
                byte b = (byte)(Mathf.Clamp01(c) * 255f); px[z * n + x] = new Color32(b, b, b, 255);
            }
            var noise = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Flame noise", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            noise.SetPixels32(px); noise.Apply(true, true);
            var material = new Material(Shader.Find("TW/Flame (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            material.SetTexture("_Noise", noise);
            owned.Add(mesh); owned.Add(noise); owned.Add(material);
            var host = new GameObject("Flames") { hideFlags = HideFlags.DontSave };
            host.transform.SetParent(transform, false);
            host.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r = host.AddComponent<MeshRenderer>();
            r.sharedMaterial = material; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r.receiveShadows = false;
        }

        static float Tile(float x, float z, int period)
        {
            int x0 = Mathf.FloorToInt(x), z0 = Mathf.FloorToInt(z);
            float tx = x - x0, tz = z - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            int xa = x0 % period, xb = (xa + 1) % period, za = z0 % period, zb = (za + 1) % period;
            return Mathf.Lerp(Mathf.Lerp(Hash(xa + period * 131, za), Hash(xb + period * 131, za), tx), Mathf.Lerp(Hash(xa + period * 131, zb), Hash(xb + period * 131, zb), tx), tz);
        }

        void Part(PrimitiveType type, Vector3 at, Vector3 scale, Material material, Quaternion rotation)
        {
            var go = GameObject.CreatePrimitive(type);
            go.hideFlags = HideFlags.DontSave;
            var c = go.GetComponent<Collider>(); if (c != null) Destroy(c);
            go.transform.SetParent(transform, false);
            go.transform.SetPositionAndRotation(at, rotation); go.transform.localScale = scale;
            var r = go.GetComponent<MeshRenderer>(); r.sharedMaterial = material; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        static float Hash(int a, int b)
        {
            uint h = (uint)a * 0x9E3779B1u ^ (uint)b * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        void OnDestroy()
        {
            Shader.SetGlobalColor(BurstColorId, Color.clear);   // nothing is burning once we are gone
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            SceneHooks.SmokeSources.Clear();
            SceneHooks.Flash = null;
            foreach (var o in owned) if (o != null) Destroy(o);
        }

        /// <summary>
        /// Lends one of the pool's lights to a flash. It used to be a plain round robin, which meant a shell's light
        /// (half a second long) was taken back by an ordinary rifle shot within a quarter of a second — eight slots at
        /// one shot every 30 ms — and even by a gun flickering beyond the horizon at a fortieth of its brightness.
        /// Now the dimmest slot goes first, and nothing takes a slot from a light still burning brighter than itself:
        /// a burst holds its light for as long as it was given, and the shot that cannot have one simply goes unlit.
        /// </summary>
        void Flash(Vector3 at, Color color, float peak, float range, float life, float card = 2.6f)
        {
            int slot = -1; float dimmest = float.MaxValue;
            for (int i = 0; i < poolSize; i++)
            {
                float left = pool[i].Light.enabled ? Mathf.Max(0f, 1f - (Time.time - pool[i].Born) / pool[i].Life) : 0f;
                float live = pool[i].Peak * left * left;   // the same curve Update draws it with
                if (live >= dimmest) continue;
                dimmest = live; slot = i;
            }
            if (slot < 0 || dimmest > peak) return;   // every light out there is brighter than this one: it stays dark
            ref var p = ref pool[slot];
            p.Light.transform.position = at; p.Light.color = color; p.Light.range = range; p.Light.intensity = peak; p.Light.enabled = true;
            p.Born = Time.time; p.Life = life; p.Peak = peak; p.Card = card;
        }

        void OnSimEvent(SimEvent e)
        {
            var cam = Camera.main; if (cam == null) return;
            if (e.Type == SimEventType.Shot)
            {
                if (Time.time - lastShot < .03f) return;
                Vector3 at = (Vector3)e.Pos;
                Vector3 toCam = at - cam.transform.position;
                if (toCam.sqrMagnitude > 150f * 150f || Vector3.Dot(toCam, cam.transform.forward) < 0f) return;
                at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 1.2f;
                lastShot = Time.time;
                Flash(at, Muzzle, 13f, 10f, .09f);
            }
            else if (e.Type == SimEventType.Explosion)
            {
                Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 1.5f;
                // the burst is the brightest thing on the field for a quarter of a second (owner, 2026-09-22: twice as
                // strong), and a big shell lights more ground than a light one. Peak rides hard on the shell's radius;
                // REACH DOES NOT. A light that reaches past about 23 m covers the whole picture at the standard view and
                // the night grade goes with it — the field turned flat yellow-olive at 44 m (claude-10's fxb_2, 20:05).
                // So: a hotter core over the same ground, not a bigger one.
                float r = Mathf.Clamp(e.Scalar, 2f, 9f);
                float peak = 40f + 3f * r, reach = 13f + 1.1f * r;
                Flash(at, Burst, peak, reach, .48f, 3.0f);
                // the drawn burst is lit by this one while it is the brightest on the field; a smaller shell going off
                // beside a big one does not take the picture back off it
                float livePeak = burstPeak * Mathf.Max(0f, 1f - (Time.time - burstBorn) / Mathf.Max(.01f, burstLife));
                if (peak >= livePeak) { burstAt = at; burstPeak = peak; burstBorn = Time.time; burstLife = .48f; burstRange = reach * 1.35f; }
                if (SceneHooks.IsWater == null || !SceneHooks.IsWater(at.x, at.z))
                {
                    // and the hole it tore goes on glowing: the light drops to an ember red and dies over two seconds.
                    // An eighth of the flash and half its reach — an ember down in the hole, not a second flash. Any
                    // more and a barrage never lets the field go back to night between shells.
                    ref var g = ref afterglow[nextGlow]; nextGlow = (nextGlow + 1) % AfterglowCount;
                    g.Light.transform.position = at - Vector3.up * 1.1f;
                    g.Light.color = new Color(1f, .34f, .10f); g.Light.range = 6f + 0.55f * r; g.Light.enabled = true;
                    g.Peak = 4f + 1f * r; g.Born = Time.time; g.Life = 2.1f;
                    embers[nextEmber] = new Ember { Pos = at - Vector3.up * 1.25f, Born = Time.time, Life = 7f + 5f * Hash(Time.frameCount, 29), Size = Mathf.Clamp(e.Scalar * .55f, 1.6f, 4f) };
                    nextEmber = (nextEmber + 1) % EmberCount;
                }
            }
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.NightLightsUpdate.Auto();
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (!built)
            {
                var props = GetComponent<BattlefieldProps>();
                if (props != null && props.Sites.Count > 0) { Build(props.Sites); built = true; }
            }
            for (int i = 0; i < lanterns.Count; i++)
            {
                float t = Time.time * 7f + lanternPhase[i];
                // the wind rocks every lamp and tears at every flame: the pools of light wander a hand's width
                float rock = .025f + .012f * Atmosphere.WindNow.magnitude;
                lanterns[i].transform.position = lanternHome[i] + new Vector3(Mathf.Sin(t * .31f) * rock, 0f, Mathf.Cos(t * .23f + 1.7f) * rock);
                lanterns[i].intensity = lanternBase[i] * (.86f + .10f * Mathf.Sin(t) * Mathf.Sin(t * .43f) + .04f * Mathf.Sin(t * 3.1f));
            }
            for (int i = 0; i < poolSize; i++)
            {
                float age = pool[i].Light.enabled ? (Time.time - pool[i].Born) / pool[i].Life : 1f;
                if (age >= 1f) pool[i].Light.enabled = false;
                else pool[i].Light.intensity = pool[i].Peak * (1f - age) * (1f - age);
                // the card: as wide as a third of the light's reach, over-bright at birth so the bloom takes it
                float live = age >= 1f ? 0f : (1f - age) * (1f - age);
                var c = pool[i].Light.color; var card = new Color(c.r, c.g, c.b, live * pool[i].Card);
                var shape = new Vector4(pool[i].Light.range * (.30f + .25f * age) * Mathf.Lerp(1f, .55f, SceneHooks.CloseUp), 0f, i * .19f, .15f);   // among the men it was wider than the picture
                for (int k = 0; k < 4; k++) { flashPos[i * 4 + k] = pool[i].Light.transform.position; flashCol[i * 4 + k] = card; flashShape[i * 4 + k] = shape; }
            }
            flashMesh.vertices = flashPos; flashMesh.colors = flashCol; flashMesh.SetUVs(1, flashShape);
            flashMesh.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
            // hand the live burst to the shaders: its colour carries what is left of it, black once it is out
            float burstLeft = burstPeak > 0f ? Mathf.Max(0f, 1f - (Time.time - burstBorn) / Mathf.Max(.01f, burstLife)) : 0f;
            Shader.SetGlobalVector(BurstId, new Vector4(burstAt.x, burstAt.y, burstAt.z, burstRange));
            // 0.85, not the 2.2 it started at: the drawn column and smoke already sit near white in their lit band, and
            // anything above about one added the whole burst to a flat white blob with no shape in it, seen from close
            // by (close/burst_b.png, 20:2x). It has to model the cloud, not replace it.
            Shader.SetGlobalColor(BurstColorId, Burst * (0.85f * burstLeft * burstLeft));
            if (burstLeft <= 0f) burstPeak = 0f;
            for (int i = 0; i < AfterglowCount; i++)
            {
                if (!afterglow[i].Light.enabled) continue;
                float age = (Time.time - afterglow[i].Born) / afterglow[i].Life;
                // it falls away fast at first and then lingers low, the way hot earth cools
                if (age >= 1f) afterglow[i].Light.enabled = false;
                else afterglow[i].Light.intensity = afterglow[i].Peak * (1f - age) * (1f - age) * (1f - age);
            }
            for (int i = 0; i < EmberCount; i++)
            {
                float age = (Time.time - embers[i].Born) / Mathf.Max(.1f, embers[i].Life);
                float live = age >= 1f ? 0f : (1f - age) * (1f - age);
                // up close the hole's glow is an ember down in it, not a ball of light standing in the crater
                float near = SceneHooks.CloseUp;
                var card = new Color(1f, .33f, .07f, live * .9f * Mathf.Lerp(1f, .7f, near)); var shape = new Vector4(embers[i].Size * Mathf.Lerp(1f, .5f, near), .7f, i * .37f, .3f);
                for (int k = 0; k < 4; k++) { emberPos[i * 4 + k] = embers[i].Pos; emberCol[i * 4 + k] = card; emberShape[i * 4 + k] = shape; }
            }
            emberMesh.vertices = emberPos; emberMesh.colors = emberCol; emberMesh.SetUVs(1, emberShape);
            emberMesh.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
            // the guns beyond the horizon: a soft flash far off in the fog every few seconds, sometimes two or three together
            if (Time.time >= nextGuns)
            {
                var map = Host.Local.Map;
                bool salvo = Hash(Time.frameCount, 41) < .35f;
                nextGuns = Time.time + (salvo ? .18f : Mathf.Lerp(2.5f, 8f, Hash(Time.frameCount, 43)));
                bool far = Hash(Time.frameCount, 47) < .7f;
                Vector3 at = far ? new Vector3(-90f - 110f * Hash(Time.frameCount, 53), 1.5f, map.SizeMeters.y * Hash(Time.frameCount, 59))
                                 : new Vector3(map.SizeMeters.x * Hash(Time.frameCount, 61), 1.5f, map.SizeMeters.y + 100f + 110f * Hash(Time.frameCount, 67));
                Flash(at, new Color(1f, .72f, .45f), .6f, 60f, .22f, .16f);
            }
            UpdateFlare();
        }

        void UpdateFlare()
        {
            var map = Host.Local.Map;
            if (Time.time >= nextFlare)
            {
                nextFlare = Time.time + Mathf.Lerp(FlareEvery.x, FlareEvery.y, Hash(Time.frameCount, 17));
                var cam = Camera.main;
                // over the ground ahead of what the camera looks at, pushed toward the middle of the field
                Vector3 look = cam != null ? cam.transform.position + cam.transform.forward * (cam.transform.position.y / Mathf.Max(.15f, -cam.transform.forward.y)) : new Vector3(map.SizeMeters.x * .5f, 0f, map.SizeMeters.y * .5f);
                float z = Mathf.Lerp(look.z, map.SizeMeters.y * .5f, .45f) + (Hash(Time.frameCount, 19) - .5f) * 30f;
                float x = Mathf.Clamp(look.x + (Hash(Time.frameCount, 23) - .5f) * 40f, 8f, map.SizeMeters.x - 8f);
                flareFrom = new Vector3(x, RenderGround.Sample(map, x, Mathf.Clamp(z, 0f, map.SizeMeters.y - 1f)) + 42f, z);
                flareBorn = Time.time; flareLight.enabled = true;
            }
            float age = (Time.time - flareBorn) / FlareLife;
            if (age >= 1f) { if (flareLight.enabled) { flareLight.enabled = false; flareGlow.SetColor("_Tint", Color.black); } return; }
            float sway = Mathf.Sin(Time.time * .9f) * 2.5f;
            flare.position = flareFrom + new Vector3(sway + age * 9f, -age * 26f, Mathf.Cos(Time.time * .7f) * 2f);
            float burn = Mathf.SmoothStep(0f, 1f, age / .06f) * (1f - Mathf.SmoothStep(.8f, 1f, age));
            flareLight.intensity = 420f * burn * (.92f + .08f * Mathf.Sin(Time.time * 31f));
            flareGlow.SetColor("_Tint", Color.white * burn);
            if (burn > .5f && Time.time >= nextDrip) { nextDrip = Time.time + .14f; SceneHooks.Sparks?.Invoke(flare.position, 1); }   // the star shell sheds burning drops
        }
    }
}
