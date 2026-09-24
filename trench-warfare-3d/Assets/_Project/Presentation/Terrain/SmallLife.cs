// Phase: B2 (implemented) — the smallest life of the field, for a camera among the men (SceneHooks.CloseUp) and costing
// nothing at the standard view:
//  * motes (TW/Motes, one mesh, moved entirely in the vertex shader): moths round every lamp, ash rising off the fires,
//    water dripping from the roof beams of the dugouts and from the branches of the shattered trees;
//  * rats: a handful run the trench floors near the view, from one board to the next, and bolt when a man comes close
//    or a shell lands. They are presentation only: nothing in the sim knows of them.
// Placement is hashed, never System.Random, so a field looks the same each time it is built.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class SmallLife : MonoBehaviour
    {
        public SimHost Host;
        public const int MaxRats = 6, MaxMotes = 900;
        public float RatReach = 26f;

        struct Rat { public Vector3 Pos, Goal; public float Speed, Wait, Fear; public int Hops; public bool Out; }
        readonly Rat[] rats = new Rat[MaxRats];
        readonly List<Vector3> floors = new List<Vector3>();
        readonly Matrix4x4[] bodies = new Matrix4x4[MaxRats], tails = new Matrix4x4[MaxRats];
        Mesh motes, capsule, cube;
        Material moteMat, ratMat;
        MeshRenderer moteRenderer;
        bool built, subscribed;
        float nextScan; int tick;

        static float Hash(int i, int salt)
        {
            uint h = (uint)(i * 374761393 + salt * 668265263); h = (h ^ (h >> 13)) * 1274126177u; h ^= h >> 16;
            return (h & 0xFFFF) / 65535f;
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            if (motes != null) Destroy(motes); if (moteMat != null) Destroy(moteMat); if (ratMat != null) Destroy(ratMat);
        }

        void OnSimEvent(SimEvent e)
        {
            if (e.Type != SimEventType.Explosion) return;
            for (int i = 0; i < MaxRats; i++)
                if (rats[i].Out && (rats[i].Pos - (Vector3)e.Pos).sqrMagnitude < 30f * 30f) rats[i].Fear = 2.5f;
        }

        void Build()
        {
            built = true;
            var view = GetComponent<GreyboxTerrainView>(); var props = GetComponent<BattlefieldProps>(); var night = GetComponent<NightLights>();
            var map = Host.Local.Map;
            // where a rat can run: one point on the boards of every trench cell
            var seen = new HashSet<int>();
            if (view != null && view.Surface != null)
                foreach (var edge in view.Surface.Edges)
                {
                    if (!seen.Add(edge.Cell)) continue;
                    var inside = edge.Center - edge.Outward * .95f;
                    floors.Add(new Vector3(inside.x, map.Height.Sample(inside.x, inside.z) + .11f, inside.z));
                }

            var pos = new List<Vector3>(); var corner = new List<Vector2>(); var what = new List<Vector4>(); var tris = new List<int>();
            void Mote(Vector3 anchor, float kind, float phase, float size, float reach)
            {
                if (pos.Count / 4 >= MaxMotes) return;
                int v = pos.Count;
                for (int k = 0; k < 4; k++) { pos.Add(anchor); corner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f)); what.Add(new Vector4(kind, phase, size, reach)); }
                tris.Add(v); tris.Add(v + 1); tris.Add(v + 2); tris.Add(v); tris.Add(v + 2); tris.Add(v + 3);
            }
            if (night != null)
            {
                for (int i = 0; i < night.LampPoints.Count; i++)
                    for (int k = 0; k < 4; k++) Mote(night.LampPoints[i], 0f, Hash(i * 8 + k, 201), .022f + .012f * Hash(i * 8 + k, 202), .35f + .5f * Hash(i * 8 + k, 203));
                for (int i = 0; i < night.FirePoints.Count; i++)
                    for (int k = 0; k < 14; k++) Mote(night.FirePoints[i] + Vector3.up * .6f, 1f, Hash(i * 16 + k, 204), .018f + .014f * Hash(i * 16 + k, 205), 4.5f + 3f * Hash(i * 16 + k, 206));
            }
            if (props != null)
                for (int i = 0; i < props.Sites.Count; i++)
                    for (int k = 0; k < 7; k++)   // along the front beam of the roof
                        Mote(props.Sites[i].Position + props.Sites[i].Rotation * new Vector3((Hash(i * 8 + k, 207) - .5f) * 3.4f, 1.80f, -1.22f), 2f, Hash(i * 8 + k, 208), .012f, 1.75f);
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                if (p.Kind != PropKind.Tree && p.Kind != PropKind.BrokenTree) continue;
                float ground = map.Height.Sample(p.Pos.x, p.Pos.z);
                for (int k = 0; k < 2; k++)
                {
                    float high = p.Kind == PropKind.Tree ? 2.2f + 1.6f * Hash(i * 4 + k, 209) : 1.2f + .8f * Hash(i * 4 + k, 209), a = Hash(i * 4 + k, 210) * 6.2832f, far = .35f + .7f * Hash(i * 4 + k, 211);
                    Mote(new Vector3(p.Pos.x + Mathf.Cos(a) * far, ground + high, p.Pos.z + Mathf.Sin(a) * far), 2f, Hash(i * 4 + k, 212), .012f, high - .05f);
                }
            }
            var shader = Shader.Find("TW/Motes (URP)");
            if (pos.Count > 0 && shader != null)
            {
                motes = new Mesh { name = "Motes", hideFlags = HideFlags.HideAndDontSave, indexFormat = IndexFormat.UInt32 };
                motes.SetVertices(pos); motes.SetUVs(0, corner); motes.SetUVs(1, what); motes.SetTriangles(tris, 0);
                motes.bounds = new Bounds(new Vector3(map.SizeMeters.x * .5f, 0f, map.SizeMeters.y * .5f), new Vector3(map.SizeMeters.x + 80f, 80f, map.SizeMeters.y + 80f));
                moteMat = new Material(shader) { hideFlags = HideFlags.HideAndDontSave };
                var go = new GameObject("Motes") { hideFlags = HideFlags.DontSave };
                go.transform.SetParent(transform, false);
                go.AddComponent<MeshFilter>().sharedMesh = motes;
                moteRenderer = go.AddComponent<MeshRenderer>();
                moteRenderer.sharedMaterial = moteMat; moteRenderer.shadowCastingMode = ShadowCastingMode.Off; moteRenderer.receiveShadows = false;
                moteRenderer.enabled = false;
            }
            capsule = Resources.GetBuiltinResource<Mesh>("Capsule.fbx"); cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            var toon = Shader.Find("TW/Toon (URP)");
            if (toon != null)
            {
                ratMat = new Material(toon) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
                ratMat.SetColor("_BaseColor", new Color(0.19f, 0.165f, 0.15f)); ratMat.SetFloat("_OutlineWidth", 0.6f);
            }
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.SmallLifeUpdate.Auto();
            if (Host == null || Host.Local == null) return;
            if (!built)
            {
                // after the sites are composed and the lamps are lit (both take a few frames), or without them after a while
                var props = GetComponent<BattlefieldProps>(); var night = GetComponent<NightLights>();
                bool ready = props != null && props.Sites.Count > 0 && (night == null || night.Built);
                if (!ready && Time.timeSinceLevelLoad < 8f) return;
                Build();
            }
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            bool close = SceneHooks.CloseUp > 0f;
            if (moteRenderer != null && moteRenderer.enabled != close) moteRenderer.enabled = close;
            if (!close || ratMat == null || floors.Count == 0) { for (int i = 0; i < MaxRats; i++) rats[i].Out = false; return; }
            var cam = Camera.main; if (cam == null) return;
            Vector3 eye = cam.transform.position; float dt = Time.deltaTime, now = Time.time;

            // a man within three metres sends a rat running (checked a few times a second, not every frame)
            if (now >= nextScan)
            {
                nextScan = now + 0.25f;
                var w = Host.Local.World;
                for (int r = 0; r < MaxRats; r++)
                {
                    if (!rats[r].Out) continue;
                    for (int i = 0; i < w.HighWater; i++)
                    {
                        if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                        float dx = w.Position[i].x - rats[r].Pos.x, dz = w.Position[i].z - rats[r].Pos.z;
                        if (dx * dx + dz * dz < 9f) { rats[r].Fear = 1.5f; break; }
                    }
                }
            }

            int drawn = 0;
            for (int r = 0; r < MaxRats; r++)
            {
                var rat = rats[r];
                if (!rat.Out)
                {
                    rat.Wait -= dt;
                    if (rat.Wait <= 0f)
                    {
                        // come out of the wall somewhere near the view
                        tick++;
                        var at = floors[(int)(Hash(tick, 221 + r) * (floors.Count - 1))];
                        float dx = at.x - eye.x, dz = at.z - eye.z;
                        if (dx * dx + dz * dz < RatReach * RatReach) { rat.Pos = rat.Goal = at; rat.Out = true; rat.Hops = 3 + (int)(Hash(tick, 222) * 7f); rat.Speed = 1.6f + Hash(tick, 223) * 1.2f; rat.Fear = 0f; }
                        else rat.Wait = 0.15f;
                    }
                    rats[r] = rat; continue;
                }
                rat.Fear = Mathf.Max(0f, rat.Fear - dt);
                Vector3 to = rat.Goal - rat.Pos; to.y = 0f;
                if (to.sqrMagnitude < 0.02f)
                {
                    if (rat.Wait > 0f && rat.Fear <= 0f) { rat.Wait -= dt; }   // a rat stops, looks, goes on
                    else if (--rat.Hops <= 0) { rat.Out = false; rat.Wait = 2f + Hash(++tick, 224) * 5f; rats[r] = rat; continue; }
                    else
                    {
                        // the next board: any floor point one or two cells on, picked by hash
                        tick++;
                        int start = (int)(Hash(tick, 225) * (floors.Count - 1)); bool found = false;
                        for (int n = 0; n < floors.Count && !found; n++)
                        {
                            var f = floors[(start + n) % floors.Count];
                            float sq = (f.x - rat.Pos.x) * (f.x - rat.Pos.x) + (f.z - rat.Pos.z) * (f.z - rat.Pos.z);
                            if (sq > 1.2f && sq < 9f) { rat.Goal = f + new Vector3((Hash(tick, 226) - .5f) * .7f, 0f, (Hash(tick, 227) - .5f) * .7f); found = true; }
                        }
                        if (!found) rat.Hops = 0;
                        rat.Wait = Hash(tick, 228) < .35f ? .4f + Hash(tick, 229) * 1.2f : 0f;
                    }
                }
                else
                {
                    float speed = rat.Speed * (rat.Fear > 0f ? 2.6f : 1f);
                    Vector3 step = to.normalized * Mathf.Min(speed * dt, to.magnitude);
                    rat.Pos += step; rat.Pos.y = Mathf.Lerp(rat.Pos.y, rat.Goal.y, dt * 6f);
                    var facing = Quaternion.LookRotation(to.normalized);
                    float scurry = Mathf.Sin(now * 38f + r) * .012f;
                    bodies[drawn] = Matrix4x4.TRS(rat.Pos + Vector3.up * (.045f + scurry), facing * Quaternion.Euler(90f, 0f, 0f), new Vector3(.085f, .11f, .075f));
                    tails[drawn] = Matrix4x4.TRS(rat.Pos + Vector3.up * .03f - facing * Vector3.forward * .22f, facing * Quaternion.Euler(0f, Mathf.Sin(now * 9f + r) * 14f, 0f), new Vector3(.012f, .012f, .2f));
                    drawn++;
                    rats[r] = rat; continue;
                }
                var still = Quaternion.LookRotation(to.sqrMagnitude > 1e-5f ? to.normalized : Vector3.forward);
                bodies[drawn] = Matrix4x4.TRS(rat.Pos + Vector3.up * .045f, still * Quaternion.Euler(90f, 0f, 0f), new Vector3(.085f, .11f, .075f));
                tails[drawn] = Matrix4x4.TRS(rat.Pos + Vector3.up * .03f - still * Vector3.forward * .22f, still, new Vector3(.012f, .012f, .2f));
                drawn++;
                rats[r] = rat;
            }
            if (drawn > 0)
            {
                var size = Host.Local.Map.SizeMeters;
                var rp = new RenderParams(ratMat) { worldBounds = new Bounds(new Vector3(size.x * .5f, 0f, size.y * .5f), new Vector3(size.x + 20f, 60f, size.y + 20f)), shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true };
                FrameBudget.Draw(rp, capsule, 0, bodies, drawn);
                FrameBudget.Draw(rp, cube, 0, tails, drawn);
            }
        }
    }
}
