// Phase: B1 (implemented; greybox stand-in for C4 VFX and B5 ragdolls)
// Makes the fight readable with capsules only: every Shot event becomes a short-lived tracer, every Death leaves a
// flattened body in the team colour, and every trench or objective capture raises a banner. Instanced draws, no
// GameObjects per effect. Listens to SimHost.Events, so it sees exactly what the local sim produced.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class CombatFx : MonoBehaviour
    {
        public SimHost Host;
        public float TracerSeconds = 0.12f;
        public int MaxBodies = 600;

        struct Tracer { public Vector3 From, To; public float Born; public bool Hit; }
        struct Body { public Vector3 Pos; public float Yaw; public byte Team; }

        readonly List<Tracer> tracers = new List<Tracer>(512);
        readonly List<Body> bodies = new List<Body>(600);
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        readonly Matrix4x4[] batchArray = new Matrix4x4[1023];
        Mesh cube, capsule;
        Material tracerMat, bodyMatA, bodyMatB;
        string banner; float bannerUntil;
        bool subscribed;

        void Start()
        {
            cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            capsule = Resources.GetBuiltinResource<Mesh>("Capsule.fbx");
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            var lit = Shader.Find("Universal Render Pipeline/Lit");
            if (unlit == null) unlit = Shader.Find("Unlit/Color");
            if (lit == null) lit = Shader.Find("Standard");
            tracerMat = new Material(unlit) { enableInstancing = true, color = new Color(1f, 0.9f, 0.45f) };
            bodyMatA = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.25f, 0.14f) };
            bodyMatB = new Material(lit) { enableInstancing = true, color = new Color(0.19f, 0.22f, 0.28f) };
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
        }

        void OnSimEvent(SimEvent e)
        {
            var w = Host.Local.World;
            var hf = Host.Local.Map.Height;
            switch (e.Type)
            {
                case SimEventType.Shot:
                {
                    if (tracers.Count >= 1500 || e.B < 0 || e.B >= w.Position.Length) break;
                    Vector3 from = (Vector3)e.Pos, to = (Vector3)w.Position[e.B];
                    from.y = hf.Sample(from.x, from.z) + 1.1f;
                    to.y = hf.Sample(to.x, to.z) + 0.9f;
                    tracers.Add(new Tracer { From = from, To = to, Born = Time.time });
                    break;
                }
                case SimEventType.Death:
                {
                    if (bodies.Count >= MaxBodies) bodies.RemoveAt(0);
                    Vector3 p = (Vector3)e.Pos;
                    p.y = hf.Sample(p.x, p.z) + 0.25f;
                    byte team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0;
                    bodies.Add(new Body { Pos = p, Yaw = Mathf.Atan2(e.Dir.x, e.Dir.z) * Mathf.Rad2Deg, Team = team });
                    break;
                }
                case SimEventType.TrenchCaptured:
                    Banner(e.B == 0 ? $"Trench {e.A} captured!" : $"Trench {e.A} lost!");
                    break;
                case SimEventType.MatchEnded:
                    Banner(e.A == 0 ? "VICTORY: enemy HQ taken" : "DEFEAT: your HQ has fallen", 3600f);
                    break;
            }
        }

        void Banner(string text, float seconds = 4f) { banner = text; bannerUntil = Time.time + seconds; }

        void Update()
        {
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));

            // tracers
            float now = Time.time;
            tracers.RemoveAll(t => now - t.Born > TracerSeconds);
            var rpT = new RenderParams(tracerMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            batch.Clear();
            for (int i = 0; i < tracers.Count; i++)
            {
                var t = tracers[i];
                Vector3 d = t.To - t.From;
                float len = d.magnitude;
                if (len < 0.1f) continue;
                // a streak that travels from muzzle to target over the tracer's life
                float k = Mathf.Clamp01((now - t.Born) / TracerSeconds);
                float streak = Mathf.Min(len, 14f);
                Vector3 mid = t.From + d.normalized * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
                batch.Add(Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(0.07f, 0.07f, streak)));
                if (batch.Count == 1023) Flush(cube, rpT);
            }
            if (batch.Count > 0) Flush(cube, rpT);

            // bodies
            for (int team = 0; team < 2; team++)
            {
                var rp = new RenderParams(team == 0 ? bodyMatA : bodyMatB) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                batch.Clear();
                for (int i = 0; i < bodies.Count; i++)
                {
                    var b = bodies[i];
                    if (b.Team != team) continue;
                    batch.Add(Matrix4x4.TRS(b.Pos, Quaternion.Euler(90f, b.Yaw, 0f), new Vector3(0.55f, 0.85f, 0.45f)));
                    if (batch.Count == 1023) Flush(capsule, rp);
                }
                if (batch.Count > 0) Flush(capsule, rp);
            }
        }

        void Flush(Mesh mesh, RenderParams rp)
        {
            batch.CopyTo(batchArray);
            Graphics.RenderMeshInstanced(rp, mesh, 0, batchArray, batch.Count);
            batch.Clear();
        }

        void OnGUI()
        {
            if (banner == null || Time.time > bannerUntil) return;
            var style = new GUIStyle(GUI.skin.label) { fontSize = 30, fontStyle = FontStyle.Bold, alignment = TextAnchor.MiddleCenter };
            var rect = new Rect(0, Screen.height * 0.12f, Screen.width, 50);
            style.normal.textColor = Color.black; GUI.Label(new Rect(rect.x + 2, rect.y + 2, rect.width, rect.height), banner, style);
            style.normal.textColor = new Color(1f, 0.92f, 0.6f); GUI.Label(rect, banner, style);
        }
    }
}
