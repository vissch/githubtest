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
            TintDust();
            Debris = gameObject.AddComponent<DebrisRenderer>();
        }

        /// <summary>A round's spurt in the mud's colour, as CombatFx tints it from the terrain (untinted it was the grey of
        /// the hop's dust, and a burst's strikes 40 m off read as one pale puff, Bullfrog critic g1).</summary>
        /// Dark mud read black on the night ground (critic g2): the dirt a round throws up is lighter than the ground it
        /// lies on, as it is drier and catches the light.
        void TintDust()
        {
            // pre-divided by the night's blue moon: the lit card multiplies its tint by the light, and a mud tint of
            // (0.78, 0.64, 0.50) came out (81, 95, 129), blue strongest (critic g4). Out of this it comes out mud-warm.
            Books?.Tint(Book.Spurt, new Color(1.25f, 0.72f, 0.36f)); Books?.Tint(Book.Column, new Color(1.15f, 0.68f, 0.34f));
            Books?.Tint(Book.Puff, new Color(0.46f, 0.40f, 0.34f));
            // the shell burst's smoke, the same way: untinted, the moon made its three cards a solid blue ball in front of
            // the Bullfrog (g13, still open at g15); this comes out a cool grey under the moon alone, about (64, 74, 93), warming by a fire (0.72, 0.48, 0.27 came out mustard beside a burning hull)
            Books?.Tint(Book.Smoke, new Color(0.62f, 0.50f, 0.36f));
            // and the shell burst's own card: its last frames are its smoke, and they were the blue ball (found by drawing the
            // burst without it, g15); lighter than the smoke's so the flame frames keep their fire
            Books?.Tint(Book.Burst, new Color(0.9f, 0.66f, 0.42f));
        }

        void OnDestroy() { Books?.Dispose(); if (discMat != null) Destroy(discMat); if (tetherMat != null) Destroy(tetherMat); if (discMesh != null) Destroy(discMesh); if (tracerMat != null) Destroy(tracerMat); if (tailMat != null) Destroy(tailMat); }

        // ------------------------------------------------------------------ rounds in flight (a gatling's)
        // Each round flies from the muzzle to where it lands at RoundSpeed; one in TracerEvery draws a streak on the way
        // (a belt's tracers), every one kicks up a spurt of dirt where it lands (and a spark, for a tracer). The streak is
        // CombatFx's: a thin white-hot box, instanced, stretched along the flight.
        /// <summary>A tracer: a bright head TracerHead long on a thinner, dimmer tail TracerLength long (metres; an even bar
        /// 3.5 m long read as a laser close up, critic g2).</summary>
        // 350 m/s, for the look: at 700 a tracer crossed 40 m in under a tenth of a second and most stills missed it (g8)
        public const float RoundSpeed = 350f, TracerLength = 5f, TracerHead = 1.5f, TracerWidth = 0.1f;
        // every second round (one in three was on screen a third of the time: neither wide still caught one, critic g6)
        public const int TracerEvery = 2;
        /// <summary>Seconds a tracer's tail stays after it lands, shrinking into the strike: a streak that vanished the
        /// frame it arrived never joined the gun to where its rounds fell (critic g6).</summary>
        public const float TracerLinger = 0.12f;
        struct Round { public Vector3 From, To; public float Born, Flight, Size; public bool Tracer, Landed; }
        readonly List<Round> rounds = new List<Round>(256);
        readonly List<Matrix4x4> tracerM = new List<Matrix4x4>(64), tailM = new List<Matrix4x4>(64);
        Material tracerMat, tailMat;
        /// <summary>Rounds in the air now (tests, the report).</summary>
        public int RoundsInFlight => rounds.Count;

        /// <summary>A round from the muzzle to where it lands (to: the ground, or 250 m out into the air: no spurt then).</summary>
        public void Fly(Vector3 from, Vector3 to, bool tracer, float size, bool lands = true)
        {
            if (rounds.Count >= 512) return;
            rounds.Add(new Round { From = from, To = to, Born = Time.time, Flight = Vector3.Distance(from, to) / RoundSpeed, Size = lands ? size : -size, Tracer = tracer });
        }

        /// <summary>A spent case, thrown out of the gun's side: a sliver of brass that tumbles and lies a few seconds.</summary>
        public void Case(Vector3 at, Vector3 outward, float size)
        {
            if (Debris == null) return;
            var rng = new DebrisRng(at, (uint)(Time.frameCount * 131 + rounds.Count));
            var v = outward.normalized * rng.Range(2.5f, 4f) + Vector3.up * rng.Range(2f, 3.5f);
            // hot, so it glows as it tumbles (burn): cold brass at night was a dark speck that read as dirt (critic g1), and
            // at 0.45 its glow did not show either (g2)
            // 2 s on the ground, not 4: at 24 rounds a second a hundred cooled cases lay in a dark ring round it (g8)
            Debris.Throw(DebrisRenderer.Piece.Shard, at, v, 0.2f * size, new Color(0.95f, 0.74f, 0.32f), ref rng, 2f, 1f);
            // and a glint as it leaves: a hot shard alone stayed a dark speck even at full burn (critic g3)
            Books?.Add(Book.Star, at + outward.normalized * 0.3f * size, 0.35f * size, 0.08f, velocity: v * 0.5f, roll: Random.value * 6.28f, glow: 2.5f * Glow);
            // and a link of the belt with it, dark steel, a little slower and lower
            var lv = outward.normalized * rng.Range(1.5f, 2.5f) + Vector3.up * rng.Range(1f, 2f);
            Debris.Throw(DebrisRenderer.Piece.Shard, at - Vector3.up * 0.1f * size, lv, 0.13f * size, new Color(0.2f, 0.19f, 0.18f), ref rng, 2f, 0f);
        }

        void FlyRounds()
        {
            float now = Time.time;
            var cam = Camera.main;
            for (int i = rounds.Count - 1; i >= 0; i--)
            {
                var r = rounds[i];
                float t = now - r.Born;
                if (t >= r.Flight && !r.Landed)
                {
                    // it lands: dirt kicked up (a spurt, a few clods), a spark off a tracer
                    if (r.Size > 0f && Books != null)
                    {
                        float s = r.Size;
                        // as the battle's own spurt (CombatFx: upright, anchored on the ground, mirrored at random)
                        // brightened (glow): a lit card is multiplied by the moon, and tinted mud still came out blue-grey,
                        // 25 luma over the night ground (critic g3); a shell's column is lit by its own burst, a round's is not
                        float dirt = 1f + 1.6f * Glow;
                        Books.Add(Book.Spurt, r.To, (1.3f + Random.value * 0.6f) * s, 0.5f, Kind.Upright | Kind.Anchored | (Random.value < 0.5f ? Kind.Mirror : Kind.None), grow: 0.3f, alpha: 0.95f, glow: dirt, pop: 0.3f);
                        // and a little column of earth that rises and falls back: the spurt alone had no height (critic g2)
                        Books.Add(Book.Column, r.To, (0.6f + Random.value * 0.3f) * s, 0.55f, Kind.Upright | Kind.Anchored | (Random.value < 0.5f ? Kind.Mirror : Kind.None), alpha: 1f, glow: dirt, pop: 0.2f);
                        if (r.Tracer) Books.Add(Book.Star, r.To + Vector3.up * 0.1f, 0.7f * s, 0.07f, roll: Random.value * 6.28f, glow: 3f * Glow);
                        // one round in three leaves a low haze of dust that hangs and spreads: a burst builds a cloud on the
                        // ground where it falls, which ties the strikes to the ground (they read as a pale blob in the air, g6/g7)
                        if (Random.value < 0.34f)
                            Books.Add(Book.Puff, r.To + new Vector3(Random.Range(-0.5f, 0.5f), 0f, Random.Range(-0.5f, 0.5f)) * s, (2.2f + Random.value) * s, 2.8f, Kind.Anchored | (Random.value < 0.5f ? Kind.Mirror : Kind.None), velocity: new Vector3(Random.Range(-0.3f, 0.3f), 0.15f, Random.Range(-0.3f, 0.3f)), grow: 1.8f, alpha: 0.4f, glow: dirt);
                        if (Debris != null) Debris.Burst(DebrisRenderer.Piece.Clod, r.To, 3, 3.5f, 0.07f * s, new Color(0.52f, 0.42f, 0.32f), 3f, 0f, 1.8f, default, (uint)(i + rounds.Count * 7));
                    }
                    r.Landed = true; rounds[i] = r;
                }
                if (r.Landed && (!r.Tracer || t - r.Flight >= TracerLinger)) { rounds.RemoveAt(i); continue; }
                if (!r.Tracer) continue;
                var dir = (r.To - r.From) / Mathf.Max(1e-4f, r.Flight * RoundSpeed);
                var head = r.Landed ? r.To : r.From + dir * (RoundSpeed * t);
                float shrink = r.Landed ? 1f - (t - r.Flight) / TracerLinger : 1f;
                float px = cam != null ? Vector3.Distance(cam.transform.position, head) * 2f * Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad) / Mathf.Max(1, cam.pixelHeight) : 0.05f;
                // far off a streak has to be long to be seen at all (it drew 120 px at 40 m, critic g2): at least 30 px
                float sz = Mathf.Abs(r.Size), grow = Mathf.Max(1f, 30f * px / (TracerLength * sz));
                float len = Mathf.Min(TracerLength * sz * grow, RoundSpeed * Mathf.Min(t, r.Flight)) * shrink, headLen = r.Landed ? 0f : Mathf.Min(TracerHead * sz * grow, len);
                // at least ~2 px thick wherever it is (a tracer is a line of light, not a solid); the tail half that
                float w = Mathf.Max(TracerWidth * sz, 2f * px);
                var look = Quaternion.LookRotation(dir);
                if (headLen > 0f) tracerM.Add(Matrix4x4.TRS(head - dir * (headLen * 0.5f), look, new Vector3(w, w, headLen)));
                // the tail half the head's width close up (as wide, a bright even bar read as a laser, g9), but never under
                // 2 px: at half width it was a 1 px brown stick at 40 m (critic g8)
                float wt = Mathf.Max(0.5f * TracerWidth * sz, 2f * px);
                tailM.Add(Matrix4x4.TRS(head - dir * (len * 0.5f), look, new Vector3(wt, wt, len)));
            }
            // (tails alone too: a tail lingering after its head landed was neither drawn nor cleared, and piled up to be
            // drawn later where it no longer was, critic g10)
            if (tracerM.Count == 0 && tailM.Count == 0) return;
            if (tracerMat == null)
            {
                var sh = Shader.Find("Universal Render Pipeline/Unlit"); if (sh == null) { tracerM.Clear(); tailM.Clear(); return; }
                tracerMat = new Material(sh) { enableInstancing = true, name = "Playground tracer", color = new Color(3.4f, 2.4f, 1.1f) };   // the head: orange-white hot
                tailMat = new Material(sh) { enableInstancing = true, name = "Playground tracer tail", color = new Color(2.6f, 1.4f, 0.45f) };   // the tail: a dimmer orange (1.3, 0.55, 0.15 read brown far off, g8)
            }
            var cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            foreach (var (list, mat) in new[] { (tracerM, tracerMat), (tailM, tailMat) })
            {
                if (list.Count == 0) continue;
                if (list.Count == 1) list.Add(Matrix4x4.Scale(Vector3.zero));
                var rp = new RenderParams(mat) { worldBounds = Everywhere, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false };
                Graphics.RenderMeshInstanced(rp, cube, 0, list);
                list.Clear();
            }
        }

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

        // ------------------------------------------------------------------ a flyer's tether
        // the gunship's ring lies on the ground 14 m under it and nothing tied the two together (critic, loop 2): a thin line
        // in the side's colour from the ring up to the aircraft, strongest at the ground and fading up to it
        readonly List<LineRenderer> tethers = new List<LineRenderer>(); int tethersUsed; Material tetherMat;

        /// <summary>Queue this frame's line from the ring (ground) up to the aircraft (top).</summary>
        public void Tether(Vector3 ground, Vector3 top, int team, float width)
        {
            if (team < 0) return;
            if (tetherMat == null) { var sh = Shader.Find("Sprites/Default"); if (sh == null) return; tetherMat = new Material(sh) { name = "Playground tether" }; }
            if (tethersUsed == tethers.Count)
            {
                var go = new GameObject("tether"); go.transform.SetParent(transform, false);
                var lr = go.AddComponent<LineRenderer>();
                lr.sharedMaterial = tetherMat; lr.positionCount = 2; lr.useWorldSpace = true;
                lr.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; lr.receiveShadows = false;
                tethers.Add(lr);
            }
            var l = tethers[tethersUsed++];
            l.enabled = true; l.SetPosition(0, ground + Vector3.up * 0.15f); l.SetPosition(1, top);
            // at least ~2.5 px wide at any range (1-3 px at the battle's 78 m, critic loop 3), and still there where it meets
            // the aircraft: it fades to 0.45, not to nothing
            var cam = Camera.main;
            float px = cam != null ? Vector3.Distance(cam.transform.position, (ground + top) * 0.5f) * 2f * Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad) / Mathf.Max(1, cam.pixelHeight) : 0f;
            float w = Mathf.Max(width, 2.5f * px);
            l.startWidth = w; l.endWidth = w * 0.8f;
            var c = team == 1 ? TankRenderer.TeamB : TankRenderer.TeamA;
            l.startColor = new Color(c.r, c.g, c.b, 0.85f); l.endColor = new Color(c.r, c.g, c.b, 0.45f);
        }

        void DrawRings()
        {
            for (int i = tethersUsed; i < tethers.Count; i++) tethers[i].enabled = false;
            tethersUsed = 0;
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
            FlyRounds();
            Books?.Draw(Time.time, Everywhere);
            DrawRings();
            float now = Time.time;
            for (int i = lamps.Count - 1; i >= 0; i--)
            {
                var p = lamps[i];
                // a light someone else destroyed (a machine's OnDestroy): drop its entry, or it throws here every frame
                if (p.L == null) { lamps.RemoveAt(i); continue; }
                float k = (now - p.Born) / p.Life;
                if (k >= 1f || (p.Follow == null && p.Offset.x == float.MaxValue)) { Destroy(p.L.gameObject); lamps.RemoveAt(i); continue; }
                if (p.Follow != null) p.L.transform.position = p.Follow.TransformPoint(p.Offset);
                // (Mathf.SmoothStep interpolates from its first argument to its second; it is not GLSL's smoothstep(edge0,
                // edge1, x): SmoothStep(0.8, 1, k) started at 0.8, so every fire burnt its light at a fifth, critic g6)
                float fade = p.Flicker ? 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.8f, 1f, k)) : (1f - k) * (1f - k);
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

        /// <summary>Set a held lamp's strength (its flicker and fade still apply). Setting the Light's intensity does nothing:
        /// LateUpdate writes Peak over it every frame, so a fire's light burnt at full from its first flicker and lit a
        /// damaged machine cream all over (critic loop 3, the _2_hits stills).</summary>
        public void SetPeak(Light l, float peak)
        {
            foreach (var p in lamps) if (p.L == l) { p.Peak = peak; return; }
        }

        /// <summary>Every painted card alive (fire, smoke, bursts): gone. A new scene must not inherit the last one's smoke.</summary>
        public void ClearCards()
        {
            Books?.Dispose();
            Books = new FlipbookFx();
            TintDust();
            rounds.Clear();
        }

        /// <summary>Everything thrown so far off the ground: the debris pools are rebuilt empty.</summary>
        public void ClearDebris()
        {
            if (Debris != null) Destroy(Debris);
            Debris = gameObject.AddComponent<DebrisRenderer>();
        }

        public int LampsAlive => lamps.Count;

        /// <summary>Put out one lamp and forget it. Destroying the Light yourself left its entry behind: a held fire
        /// (life 99999 s, following a machine that is still there) then threw in LateUpdate every frame after a repair.</summary>
        public void EndLamp(Light l)
        {
            for (int i = lamps.Count - 1; i >= 0; i--) if (lamps[i].L == l) lamps.RemoveAt(i);
            if (l == null) return;
            if (Application.isPlaying) Destroy(l.gameObject); else DestroyImmediate(l.gameObject);
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
            // (dust-brown, thinner and rising: the grey smoke card at 0.7 read as a solid blue ball under the moon, critic g13)
            Books.Add(Book.Puff, at, 1.4f * size, 1.6f, velocity: -dir * 0.8f + Vector3.up * 1.2f, grow: 1.6f, alpha: 0.4f, glow: 1f + 1.6f * Glow);
            Lamp(at, new Color(1f, 0.75f, 0.45f), 6f * Glow, 6f * size, 0.18f);
        }

        public void Burst(Vector3 at, float r)
        {
            if (Books == null) return;
            Books.Add(Book.Flash, at + Vector3.up * (r * 0.3f), r * 3.2f, 0.18f, roll: Random.value * 6.28f, glow: 7f * Glow, pop: 0.5f);
            // lit and rising as the battle draws it (CombatFx: glow 3.4 at night): unlit, the moon made it a solid blue ball
            // parked in front of the Bullfrog's belly (critic g13)
            Books.Add(Book.Burst, at + Vector3.up * (r * 0.55f), r * 2.6f, 1.8f, Kind.Upright | (Random.value < 0.5f ? Kind.Mirror : Kind.None),
                      velocity: Vector3.up * (r * 0.5f), grow: 0.5f, roll: Random.Range(-0.15f, 0.15f), glow: 3.4f * Glow, pop: 0.3f);
            Books.Add(Book.Column, at, r * 1.6f, 1.4f, Kind.Upright | Kind.Anchored);
            // as the battle draws them (CombatFx): thinner (0.65), after the flash (0.5 s on), and each drifting its own way so
            // the three do not stack into one ball
            for (int k = 0; k < 3; k++)
            {
                var off = new Vector3(Random.Range(-0.5f, 0.5f), 0.3f + k * 0.18f, Random.Range(-0.5f, 0.5f));
                Books.Add(Book.Smoke, at + off * r, r * Random.Range(1.1f, 1.6f), Random.Range(4f, 6.5f),
                          (k & 1) == 0 ? Kind.Mirror : Kind.None, velocity: new Vector3(off.x, 0f, off.z) * 1.6f + Vector3.up * 0.6f, grow: 2f,
                          roll: Random.Range(-0.6f, 0.6f), alpha: 0.65f, pop: 0.3f, delay: 0.5f + k * 0.15f);
            }
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

        /// <summary>Dust kicked off the ground (a hopper landing): a low brown haze thrown out and hanging, not a smoke card.</summary>
        public void Dust(Vector3 at, Vector3 outward, float width)
        {
            if (Books == null) return;
            Books.Add(Book.Puff, at, width, Random.Range(1.4f, 2f), Kind.Anchored | (Random.value < 0.5f ? Kind.Mirror : Kind.None),
                      velocity: outward * Random.Range(1f, 1.8f) + Vector3.up * 0.2f, grow: 1.8f, alpha: 0.55f, glow: 1f + 1.6f * Glow);
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

        /// <summary>One gatling round: a small flash and a flame card, a thin puff every third round, and a light that lasts
        /// less than the gap to the next round (16 a second would otherwise pile up lamps).</summary>
        public void GatlingShot(Vector3 at, Vector3 dir, float size)
        {
            if (Books == null) return;
            // each gun fires every 1/8 s: a flame that lasts 0.08 s is up two thirds of the time, a stream rather than
            // a flicker (0.05 s and a metre across hardly showed in a still). A star and a flash, each turned at random:
            // the Muzzle card seen side-on (its drawing is a cannon's plume along the shot) read as a hook (critic g1)
            Books.Add(Book.Flash, at + dir * 0.35f * size, 1.7f * size, 0.08f, roll: Random.value * 6.28f, glow: 3.5f * Glow, pop: 0.3f);
            Books.Add(Book.Star, at + dir * 0.55f * size, 1.4f * size, 0.07f, roll: Random.value * 6.28f, glow: 3.5f * Glow);
            Books.Add(Book.Smoke, at + dir * 0.5f * size, 0.35f * size, 0.6f, Random.value < 0.5f ? Kind.Mirror : Kind.None, velocity: dir * 1.5f + Vector3.up * 0.4f, grow: 1.6f, alpha: 0.4f);
            Lamp(at, new Color(1f, 0.8f, 0.5f), 5f * Glow, 7f * size, 0.05f);
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
