// Phase: B2 (implemented) — the warm half of the night look (owner's target, 2026-09-21: a cold blue field lit by
// lanterns, muzzle flashes, shell bursts and flares). Everything here is an ordinary URP point light without shadows,
// which TW/Toon, TW/Water and the soldier shader add in hard steps (TWLocalLights.hlsl), plus additive glow cards.
// Lanterns: one at each composed site (dugouts, shelters, stores), at most MaxLanterns, a lamp on a post with a
// breathing flame; and lamps hung low on the trench wall every thirty metres or so, so the men in the line stand in
// warm light. Fires: a few shattered trees in no man's land still burn. Flashes: a pool of eight lights shared by rifle fire and shell bursts; a shot only takes one when it
// is near what the camera looks at, and no more than one every 30 ms, so a 3,000-man firefight costs eight lights.
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
        public const int MaxLanterns = 12, MaxTrenchLamps = 14, MaxFires = 5, PoolSize = 8;
        public Color Lantern = new Color(1f, 0.60f, 0.26f), Muzzle = new Color(1f, 0.74f, 0.40f), Burst = new Color(1f, 0.52f, 0.20f), Flare = new Color(0.82f, 0.90f, 1f);
        public float LanternIntensity = 6f, LanternRange = 10f;
        [Tooltip("Seconds between star shells, least and most.")]
        public Vector2 FlareEvery = new Vector2(22f, 40f);

        struct Pooled { public Light Light; public float Born, Life, Peak; }
        readonly Pooled[] pool = new Pooled[PoolSize];
        readonly List<Light> lanterns = new List<Light>();
        readonly List<float> lanternPhase = new List<float>(), lanternBase = new List<float>();
        readonly List<Object> owned = new List<Object>();
        int nextPooled; float lastShot, nextFlare, flareBorn = -100f;
        bool subscribed, built;
        Light flareLight; Transform flare; Vector3 flareFrom;
        Material glow, flareGlow;
        Mesh flashMesh; readonly Vector3[] flashPos = new Vector3[PoolSize * 4]; readonly Color[] flashCol = new Color[PoolSize * 4]; readonly List<Vector4> flashShape = new List<Vector4>(PoolSize * 4);
        const float FlareLife = 16f;

        void Start()
        {
            for (int i = 0; i < PoolSize; i++)
            {
                pool[i].Light = MakeLight("Flash " + i, Muzzle, 0f, 8f);
                pool[i].Light.enabled = false;
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
            var centres = new Vector3[PoolSize]; var shapes = new Vector4[PoolSize]; var colors = new Color[PoolSize];
            for (int i = 0; i < PoolSize; i++) shapes[i] = new Vector4(1f, 0f, i * .19f, .15f);
            AddGlowMesh(host, glow, centres, shapes, colors);
            flashMesh = host.GetComponent<MeshFilter>().sharedMesh; flashMesh.MarkDynamic();
            for (int i = 0; i < PoolSize * 4; i++) flashShape.Add(new Vector4(1f, 0f, (i / 4) * .19f, .15f));
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
            int step = Mathf.Max(1, Mathf.CeilToInt(sites.Count / (float)MaxLanterns));
            for (int i = 0; i < sites.Count && lanterns.Count < MaxLanterns; i += step)
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
            // lamps in the line: on the wall of a trench cell whose neighbour toward z- is open ground, spaced along x
            int hung = 0;
            for (int z = 1; z < map.NavLength - 1 && hung < MaxTrenchLamps; z++)
            for (int x = 3; x < map.NavWidth - 3 && hung < MaxTrenchLamps; x++)
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
            float w = map.SizeMeters.x, len = map.SizeMeters.y;
            int fires = 0;
            for (int i = 0; i < map.Props.Length && fires < MaxFires; i++)
            {
                var prop = map.Props[i];
                if (prop.Kind != PropKind.BrokenTree && prop.Kind != PropKind.Stump) continue;
                if (prop.Pos.z < len * .22f || prop.Pos.z > len * .78f || Hash(i, 61) > .16f) continue;
                Vector3 at = new Vector3(prop.Pos.x, RenderGround.Sample(map, prop.Pos.x, prop.Pos.z) + (prop.Kind == PropKind.Stump ? .5f : 1.3f), prop.Pos.z);
                var l = MakeLight("Fire " + fires, Burst, 7f, 11f);
                l.transform.position = at + Vector3.up * .4f;
                lanterns.Add(l); lanternPhase.Add(i * .77f);
                centres.Add(at); shapes.Add(new Vector4(3.4f, .6f, i * .173f, .4f)); colors.Add(new Color(1f, .45f, .12f, .9f));
                centres.Add(at + Vector3.up * .5f); shapes.Add(new Vector4(1.3f, .8f, i * .311f, .3f)); colors.Add(new Color(1f, .75f, .35f, 1.6f));
                fires++;
            }
            // fires on the horizon: seen through the fog bank as soft orange glows that flare and sink
            for (int k = 0; k < 10; k++)
            {
                float a = Hash(k, 3), b = Hash(k, 5);
                Vector3 p = k < 7 ? new Vector3(-16f - 40f * a, 1.2f + 1.6f * b, len * (.05f + .9f * Hash(k, 7))) : new Vector3(w * Hash(k, 9), 1.5f + 2f * b, len + 30f + 60f * a);
                centres.Add(p); shapes.Add(new Vector4(4.5f + 5f * b, .55f, k * .31f, .6f)); colors.Add(new Color(1f, .42f, .13f, .30f + .2f * a));   // low and small: a glow on the ground far off, not a sun
            }
            foreach (var l in lanterns) lanternBase.Add(l.intensity);
            var host = new GameObject("Night glows") { hideFlags = HideFlags.DontSave };
            host.transform.SetParent(transform, false);
            AddGlowMesh(host, glow, centres.ToArray(), shapes.ToArray(), colors.ToArray());
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
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            foreach (var o in owned) if (o != null) Destroy(o);
        }

        void Flash(Vector3 at, Color color, float peak, float range, float life)
        {
            ref var p = ref pool[nextPooled]; nextPooled = (nextPooled + 1) % PoolSize;
            p.Light.transform.position = at; p.Light.color = color; p.Light.range = range; p.Light.intensity = peak; p.Light.enabled = true;
            p.Born = Time.time; p.Life = life; p.Peak = peak;
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
                Flash(at, Muzzle, 7f, 7.5f, .08f);
            }
            else if (e.Type == SimEventType.Explosion)
            {
                Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 1.5f;
                Flash(at, Burst, 40f, 22f, .45f);
            }
        }

        void Update()
        {
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
                lanterns[i].intensity = lanternBase[i] * (.86f + .10f * Mathf.Sin(t) * Mathf.Sin(t * .43f) + .04f * Mathf.Sin(t * 3.1f));
            }
            for (int i = 0; i < PoolSize; i++)
            {
                float age = pool[i].Light.enabled ? (Time.time - pool[i].Born) / pool[i].Life : 1f;
                if (age >= 1f) pool[i].Light.enabled = false;
                else pool[i].Light.intensity = pool[i].Peak * (1f - age) * (1f - age);
                // the card: as wide as a third of the light's reach, over-bright at birth so the bloom takes it
                float live = age >= 1f ? 0f : (1f - age) * (1f - age);
                var c = pool[i].Light.color; var card = new Color(c.r, c.g, c.b, live * 2.6f);
                var shape = new Vector4(pool[i].Light.range * (.30f + .25f * age), 0f, i * .19f, .15f);
                for (int k = 0; k < 4; k++) { flashPos[i * 4 + k] = pool[i].Light.transform.position; flashCol[i * 4 + k] = card; flashShape[i * 4 + k] = shape; }
            }
            flashMesh.vertices = flashPos; flashMesh.colors = flashCol; flashMesh.SetUVs(1, flashShape);
            flashMesh.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
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
        }
    }
}
