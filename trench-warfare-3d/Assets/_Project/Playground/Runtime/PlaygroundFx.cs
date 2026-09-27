// Phase: Playground (2026-09-26, lane/show/playground) — FlipbookFx + DebrisRenderer + lamps for the playground
// The effects the playground spends, through the game's own renderers: FlipbookFx (the painted fire, smoke, bursts and
// flashes CombatFx and TankRenderer use) and DebrisRenderer (the pooled flying scrap). What is tested here is what the
// battle will draw. Plus a small pool of point lights, which the battle gets from its own lamp system.
using System.Collections.Generic;
using TW.Presentation.Tactical;
using UnityEngine;
using Book = TW.Presentation.Tactical.FlipbookFx.Book;
using Kind = TW.Presentation.Tactical.FlipbookFx.Kind;

namespace TW.Playground
{
    public sealed class PlaygroundFx : MonoBehaviour
    {
        public FlipbookFx Books { get; private set; }
        public DebrisRenderer Debris { get; private set; }
        /// <summary>Night doubles the glow of anything self-lit, as the battle does (SceneMood.Night).</summary>
        public bool Night = true;
        public float Glow => Night ? 1f : 0.55f;
        /// <summary>Cards spawned per second may not pass this, whatever asks: the battle has a budget too.</summary>
        public int CardsAlive => Books != null ? Books.Alive : 0;

        sealed class LampState { public Light L; public float Born, Life, Peak; public Transform Follow; public Vector3 Offset; public bool Flicker; public float Seed; }
        readonly List<LampState> lamps = new List<LampState>();
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 5000f);

        void Awake()
        {
            Books = new FlipbookFx();
            if (!Books.Ready) Debug.LogWarning("PlaygroundFx: FlipbookFx not ready (TW/Flipbook or a Resources/VFX book missing)");
            Debris = gameObject.AddComponent<DebrisRenderer>();
        }

        void OnDestroy() { Books?.Dispose(); if (discMat != null) Destroy(discMat); if (discMesh != null) Destroy(discMesh); }

        // ------------------------------------------------------------------ side rings (TankRenderer's disc, same shader)
        /// <summary>Rings under figures too (a playground proposal: the game's figures show their side on their cloth).</summary>
        public bool UnitRings;   // off: measured, they add a third to the side's colour but cost the figure a quarter of its contrast (r15)
        Material discMat; Mesh discMesh; MaterialPropertyBlock discProps;
        readonly List<Matrix4x4> discM = new List<Matrix4x4>(); readonly List<Vector4> discC = new List<Vector4>();

        /// <summary>Queue a contact blob and a ring in the side's colour: footprint centre, yaw, half width and length.</summary>
        public void Ring(Vector3 at, float yaw, float halfW, float halfL, float padW, float padL, int team, bool dead)
        {
            if (team < 0) return;
            float w = (halfW + padW) * 2f / 0.72f, l = (halfL + padL) * 2f / 0.72f;   // TankRenderer: the ring sits at 0.72 of the quad
            discM.Add(Matrix4x4.TRS(at + Vector3.up * 0.12f, Quaternion.Euler(0f, yaw, 0f), new Vector3(w, 1f, l)));
            var c = team == 1 ? TankRenderer.TeamB : TankRenderer.TeamA;
            discC.Add(new Vector4(c.r, c.g, c.b, dead ? 0f : 1f));
        }

        void DrawRings()
        {
            if (discM.Count == 0) return;
            if (discMat == null)
            {
                var shader = Shader.Find("TW/TankDisc (URP)"); if (shader == null) { discM.Clear(); discC.Clear(); return; }
                discMat = new Material(shader) { enableInstancing = true, name = "Playground disc" };
                discProps = new MaterialPropertyBlock();
                discMesh = new Mesh { name = "Playground disc" };
                discMesh.SetVertices(new[] { new Vector3(-0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, 0.5f), new Vector3(-0.5f, 0f, 0.5f) });
                discMesh.SetUVs(0, new[] { new Vector2(0f, 0f), new Vector2(1f, 0f), new Vector2(1f, 1f), new Vector2(0f, 1f) });
                discMesh.SetTriangles(new[] { 0, 2, 1, 0, 3, 2 }, 0);
                discMesh.bounds = new Bounds(Vector3.zero, new Vector3(1f, 0.1f, 1f));
            }
            // a single instanced draw ignores per-instance properties (project memory): pad to two with an empty one
            if (discM.Count == 1) { discM.Add(Matrix4x4.Scale(Vector3.zero)); discC.Add(Vector4.zero); }
            discProps.SetVectorArray("_Color", discC);
            var rp = new RenderParams(discMat) { worldBounds = Everywhere, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false, matProps = discProps };
            Graphics.RenderMeshInstanced(rp, discMesh, 0, discM);
            discM.Clear(); discC.Clear();
        }

        void LateUpdate()
        {
            Books?.Draw(Time.time, Everywhere);
            DrawRings();
            float now = Time.time;
            for (int i = lamps.Count - 1; i >= 0; i--)
            {
                var p = lamps[i];
                float k = (now - p.Born) / p.Life;
                if (k >= 1f || (p.Follow == null && p.Offset.x == float.MaxValue)) { Destroy(p.L.gameObject); lamps.RemoveAt(i); continue; }
                if (p.Follow != null) p.L.transform.position = p.Follow.TransformPoint(p.Offset);
                float fade = p.Flicker ? 1f - Mathf.SmoothStep(0.8f, 1f, k) : (1f - k) * (1f - k);
                float flick = p.Flicker ? 0.75f + 0.25f * Mathf.PerlinNoise(p.Seed, now * 7f) : 1f;
                p.L.intensity = p.Peak * fade * flick;
            }
        }

        /// <summary>A point light: a flash (fades fast) or a fire (holds, flickers, fades at the end).</summary>
        public Light Lamp(Vector3 at, Color color, float intensity, float range, float life, bool flicker = false, Transform follow = null)
        {
            var go = new GameObject(flicker ? "FireLight" : "FlashLight");
            go.transform.SetParent(transform, false);
            var l = go.AddComponent<Light>();
            l.type = LightType.Point; l.color = color; l.range = range; l.intensity = intensity; l.shadows = LightShadows.None;
            go.transform.position = at;
            lamps.Add(new LampState { L = l, Born = Time.time, Life = life, Peak = intensity, Follow = follow, Offset = follow != null ? follow.InverseTransformPoint(at) : Vector3.zero, Flicker = flicker, Seed = Random.value * 50f });
            return l;
        }

        /// <summary>Every painted card alive (fire, smoke, bursts): gone. A new scene must not inherit the last one's smoke.</summary>
        public void ClearCards()
        {
            Books?.Dispose();
            Books = new FlipbookFx();
        }

        /// <summary>Everything thrown so far off the ground: the debris pools are rebuilt empty.</summary>
        public void ClearDebris()
        {
            if (Debris != null) Destroy(Debris);
            Debris = gameObject.AddComponent<DebrisRenderer>();
        }

        public void ClearLamps()
        {
            foreach (var p in lamps) if (p.L != null) Destroy(p.L.gameObject);
            lamps.Clear();
        }

        // -------------------------------------------------------------------------------------------- the effects
        public void Spark(Vector3 at, Vector3 dir, float size)
        {
            if (Books == null) return;
            Books.Add(Book.Star, at, 1.5f * size, 0.09f, roll: Random.value * 6.28f, glow: 3.5f * Glow);
            Books.Add(Book.Flash, at, 2.2f * size, 0.1f, roll: Random.value * 6.28f, glow: 3f * Glow, pop: 0.5f);
            Books.Add(Book.Smoke, at, 1.4f * size, 2.2f, velocity: -dir * 0.8f + Vector3.up * 0.8f, grow: 1.4f, alpha: 0.7f);
            Lamp(at, new Color(1f, 0.75f, 0.45f), 6f * Glow, 6f * size, 0.18f);
        }

        public void Burst(Vector3 at, float r)
        {
            if (Books == null) return;
            Books.Add(Book.Flash, at + Vector3.up * (r * 0.3f), r * 3.2f, 0.18f, roll: Random.value * 6.28f, glow: 7f * Glow, pop: 0.5f);
            Books.Add(Book.Burst, at + Vector3.up * (r * 0.55f), r * 2.6f, 1.8f, Kind.Upright, pop: 0.3f);
            Books.Add(Book.Column, at, r * 1.6f, 1.4f, Kind.Upright | Kind.Anchored);
            for (int k = 0; k < 3; k++)
                Books.Add(Book.Smoke, at + new Vector3(Random.Range(-0.4f, 0.4f), 0.3f + k * 0.2f, Random.Range(-0.4f, 0.4f)) * r, r * Random.Range(1.1f, 1.6f), Random.Range(4f, 6.5f),
                          (k & 1) == 0 ? Kind.Mirror : Kind.None, velocity: Vector3.up * 0.8f, grow: 1.5f, alpha: 0.8f, pop: 0.3f, delay: 0.15f + k * 0.1f);
            Lamp(at + Vector3.up, new Color(1f, 0.7f, 0.4f), 14f * Glow, 10f * r, 0.35f);
        }

        /// <summary>One beat of a standing fire, sized so the DRAWING stands from foot to top (Flamethrower.Standing).</summary>
        public void Flame(Vector3 foot, float width, float tall, float life, float alpha = 1f)
        {
            if (Books == null) return;
            FlipbookFx.Geometry(Book.Stand, out float inkLow, out float inkHigh, out _);
            float h = tall / Mathf.Max(0.1f, inkHigh - inkLow);
            float hang = -inkLow * h;
            bool mirror = Random.value < 0.5f;
            Books.Add(Book.Stand, foot + Vector3.up * hang, width * 1.35f, life, Kind.Anchored | Kind.Upright | (mirror ? Kind.Mirror : 0),
                      velocity: Vector3.up * 0.4f, height: h, alpha: alpha, glow: 1.6f * Glow, startFrame: Random.value * 6f);
            if (Random.value < 0.5f)
                Books.Add(Book.Fire, foot + Vector3.up * (tall * 0.75f), width * 0.8f, life * 0.8f, mirror ? Kind.None : Kind.Mirror,
                          velocity: Vector3.up * 1.6f, grow: 0.7f, roll: (Random.value - 0.5f) * 0.28f, alpha: alpha * 0.9f, glow: 1.6f * Glow);
        }

        public void Smoke(Vector3 at, float width, float rise, float alpha, float life = 6f)
        {
            if (Books == null) return;
            Books.Add(Book.Smoke, at, width, life * Random.Range(0.8f, 1.2f), Random.value < 0.5f ? Kind.Mirror : Kind.None,
                      velocity: Vector3.up * rise + new Vector3(Random.Range(-0.3f, 0.3f), 0f, Random.Range(-0.3f, 0.3f)),
                      grow: 2.2f, roll: Random.Range(-0.7f, 0.7f), alpha: alpha, pop: 0.2f);
        }

        /// <summary>A hovercraft's ground effect: a low puff of spray and mud blown out from under a pod, short-lived.</summary>
        public void Spray(Vector3 at, Vector3 outward, float width, float alpha)
        {
            if (Books == null) return;
            Books.Add(Book.Smoke, at, width, Random.Range(0.9f, 1.4f), Random.value < 0.5f ? Kind.Mirror : Kind.None,
                      velocity: outward * Random.Range(1.5f, 3f) + Vector3.up * Random.Range(0.3f, 0.9f), grow: 2.4f,
                      roll: Random.Range(-0.7f, 0.7f), alpha: alpha, pop: 0.15f);
        }

        /// <summary>The ammunition going up: one Blast drawing, the flash, and the black smoke that boils up after it.</summary>
        public void CookOff(Vector3 at, float size)
        {
            if (Books == null) return;
            Books.Add(Book.Flash, at + Vector3.up * (2f * size), 16f * size, 0.2f, roll: Random.value * 6.28f, glow: 6f * Glow, pop: 0.4f);
            Books.Add(Book.Blast, at + Vector3.up * (1.2f * size), 7.2f * size, 2.33f, velocity: Vector3.up * 0.9f, grow: 0.35f, glow: 1.6f * Glow, pop: 0.15f);
            for (int k = 0; k < 5; k++)
                Books.Add(Book.Smoke, at + Vector3.up * ((2f + k * 1.2f) * size), (5f + k) * size, Random.Range(5f, 8f), (k & 1) == 0 ? Kind.Mirror : Kind.None,
                          velocity: Vector3.up * (3f - k * 0.3f), grow: 2f, alpha: 0.85f, pop: 0.3f, delay: 0.3f + k * 0.2f);
            Lamp(at + Vector3.up * 2f * size, new Color(1f, 0.62f, 0.3f), 30f * Glow, 28f * size, 0.9f);
        }

        /// <summary>A rifle's shot: one small flash and one small puff, no light (the tank's Muzzle on 200 riflemen would be
        /// 2,000 cards and 200 lights).</summary>
        public void RifleShot(Vector3 at, Vector3 dir)
        {
            if (Books == null) return;
            Books.Add(Book.Muzzle, at + dir * 0.25f, 0.6f, 0.05f, roll: FlipbookFx.ScreenRoll(Camera.main, dir), glow: 2.5f * Glow);
            Books.Add(Book.Smoke, at + dir * 0.3f, 0.35f, 1f, Random.value < 0.5f ? Kind.Mirror : Kind.None, velocity: dir * 0.6f + Vector3.up * 0.3f, grow: 1.4f, alpha: 0.45f);
        }

        public void Muzzle(Vector3 at, Vector3 dir, float size)
        {
            if (Books == null) return;
            Books.Add(Book.Flash, at + dir * 0.4f * size, 3.2f * size, 0.1f, roll: Random.value * 6.28f, glow: 4f * Glow, pop: 0.5f);
            Books.Add(Book.Muzzle, at + dir * 1.2f * size, 2.6f * size, 0.08f, roll: FlipbookFx.ScreenRoll(Camera.main, dir), glow: 3f * Glow);
            for (int k = 0; k < 3; k++)
                Books.Add(Book.Smoke, at + dir * (0.6f + k * 0.7f) * size, (1.2f + k * 0.4f) * size, 2.5f + k * 0.5f, (k & 1) == 0 ? Kind.Mirror : Kind.None,
                          velocity: dir * (3f - k) + Vector3.up * 0.5f, grow: 1.5f, alpha: 0.6f);
            Lamp(at, new Color(1f, 0.8f, 0.5f), 10f * Glow, 12f * size, 0.12f);
        }
    }
}
