// Phase: CelFx kit (2026-11-09) - the one entry point: CelFx.Play(recipe, origin, dir, target, seed). It reads the
// zoom tier and the quality once, spawns every layer's records (born now or later), and queues the layers that are not
// GPU primitives (Chunks through DebrisRenderer, Light through NightLights) for the frame they are due. After that the
// effect costs the CPU nothing. Also: the clock the records are born on (pinned by a still), and the LOD rules.
//
// LOD (README.md): the tier comes from the camera's zoom (ViewEye.Zoom; GymCatalogue bands T3 7.5, T2 16, T1 30,
// O120): Close up to 16, Mid at 30, Far from 120, blended linearly between (LodBlend 0 -> 1 -> 2). A layer's count is
// interpolated between its Close, Mid and Far counts and scaled by FxQuality (Low 0.5, Medium 0.75, High 1, Epic 1.25).
// Ink thins going out (InkScale).
using System;
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.VFX.Cel
{
    public static class CelFx
    {
        public const float CloseZoom = 16f, MidZoom = 30f, FarZoom = 120f;

        // the live renderer (made on first use by Renderer; a destroyed one reads null and the next use makes another)
        static CelFxRenderer live;
        /// <summary>A pinned clock for stills (NaN: Time.time). Records are born on it and the shader reads it.</summary>
        public static float Clock = float.NaN;
        /// <summary>A pinned zoom for stills and tests (NaN: the camera's, ViewEye.Zoom).</summary>
        public static float ZoomOverride = float.NaN;
        /// <summary>DebrisRenderer.Burst, lent by whoever owns the debris (CombatFx, PlaygroundFx, the Range stage):
        /// piece, at, count, speed, scale, tint, life, burn, salt. Null: a Chunks layer throws nothing.</summary>
        public static Action<int, Vector3, int, float, float, Color, float, float, uint> Chunks;
        /// <summary>Effects played since the session began (a probe for the tests and the HUD).</summary>
        public static int Plays { get; private set; }

        public static float Now => float.IsNaN(Clock) ? Time.time : Clock;
        public static float Zoom => float.IsNaN(ZoomOverride) ? ViewEye.Zoom : ZoomOverride;

        static CelFx() { SceneStatics.Register(nameof(CelFx), ResetSession); }

        /// <summary>When Play ends: unpin the clock and the zoom, drop the lent debris and the queue.</summary>
        public static void ResetSession()
        {
            Clock = float.NaN; ZoomOverride = float.NaN; Chunks = null; Plays = 0;
            for (int i = 0; i < pendingCount; i++) pending[i] = default;
            pendingCount = 0;
        }

        /// <summary>The renderer, made on a new hidden object if the scene has none.</summary>
        public static CelFxRenderer Renderer
        {
            get
            {
                if (live != null) return live;
                live = UnityEngine.Object.FindFirstObjectByType<CelFxRenderer>();
                if (live == null)
                {
                    var go = new GameObject("CelFx") { hideFlags = HideFlags.DontSave };
                    live = go.AddComponent<CelFxRenderer>();
                }
                live.Init();
                return live;
            }
        }

        /// <summary>Make this the live renderer (a renderer made by hand: a test, a stage).</summary>
        public static void Adopt(CelFxRenderer r) => live = r;
        internal static void Release(CelFxRenderer r) { if (live == r) live = null; }

        // ------------------------------------------------------------------ LOD
        /// <summary>0 at Close (zoom 16 and in), 1 at Mid (30), 2 at Far (120 and out), linear between.</summary>
        public static float LodBlend(float zoom)
        {
            if (zoom <= CloseZoom) return 0f;
            if (zoom <= MidZoom) return (zoom - CloseZoom) / (MidZoom - CloseZoom);
            if (zoom <= FarZoom) return 1f + (zoom - MidZoom) / (FarZoom - MidZoom);
            return 2f;
        }

        /// <summary>The ink width going out: 1 at Close, 0.75 at Mid, 0.5 at Far.</summary>
        public static float InkScale(float lod) => 1f - 0.25f * Mathf.Clamp(lod, 0f, 2f);

        /// <summary>The share of every layer's count at a quality tier.</summary>
        public static float QualityShare(FxTier tier) => tier == FxTier.Low ? 0.5f : tier == FxTier.Medium ? 0.75f : tier == FxTier.Epic ? 1.25f : 1f;

        // ------------------------------------------------------------------ spawning
        /// <summary>One primitive by kind id (CelKinds.Streak / Lobe / Glow, or Renderer.KindId(name)). No allocation.</summary>
        public static void Spawn(int kind, in CelRecord r) => Renderer.Spawn(kind, r);

        /// <summary>
        /// Play an effect: origin is where it comes from (the muzzle, the burst), dir the way it faces (normalised here;
        /// zero = +z), target where it goes (the hit; origin + dir when it has none), seed its variety (the same seed
        /// plays the same effect). Every layer is spawned now; nothing is left to do per frame. No allocation.
        /// </summary>
        public static void Play(CelFxRecipe recipe, Vector3 origin, Vector3 dir, Vector3 target, uint seed)
        {
            if (recipe == null) return;
            var r = Renderer;
            float now = Now;
            float lod = LodBlend(Zoom);
            float q = QualityShare(FxQuality.Tier);
            Frame(dir, out Vector3 fwd, out Vector3 right, out Vector3 up);
            Plays++;
            for (int i = 0; i < recipe.Layers.Count; i++)
            {
                var l = recipe.Layers[i];
                if (l.kindId == int.MinValue) l.kindId = r.KindId(l.Kind);
                if (l.paletteId < 0) l.paletteId = Mathf.Max(0, CelPalette.Find(l.Palette));
                if (l.coolId < 0) l.coolId = string.IsNullOrEmpty(l.CoolTo) ? l.paletteId : Mathf.Max(0, CelPalette.Find(l.CoolTo));
                if (l.kindId == -1) continue;
                int n = l.CountAt(lod, q);
                if (n <= 0) continue;
                if (l.kindId == CelKinds.Chunks || l.kindId == CelKinds.Light) { Queue(l, n, origin, fwd, right, up, target, seed + (uint)i * 7919u, now + l.Start); continue; }
                var rng = new CelRng(seed * 31u + (uint)i * 7919u + 1u);
                for (int j = 0; j < n; j++)
                {
                    Make(l, j, n, origin, fwd, right, up, target, now, ref rng, out CelRecord rec);
                    r.Spawn(l.kindId, rec);
                }
            }
        }

        /// <summary>A frame from a direction: forward, a level right, and up.</summary>
        public static void Frame(Vector3 dir, out Vector3 fwd, out Vector3 right, out Vector3 up)
        {
            fwd = dir.sqrMagnitude > 1e-8f ? dir.normalized : Vector3.forward;
            right = Vector3.Cross(Vector3.up, fwd);
            if (right.sqrMagnitude < 1e-6f) right = Vector3.right; else right.Normalize();
            up = Vector3.Cross(fwd, right);
        }

        /// <summary>The j-th of n records of a layer (public for the tests and for kinds that spawn their own).</summary>
        public static void Make(CelLayer l, int j, int n, Vector3 origin, Vector3 fwd, Vector3 right, Vector3 up, Vector3 target, float now, ref CelRng rng, out CelRecord rec)
        {
            Vector3 anchor = l.Anchor == CelAnchor.Target ? target : origin;
            anchor += right * l.Offset.x + up * l.Offset.y + fwd * l.Offset.z;
            Vector3 p = anchor;
            switch (l.Shape)
            {
                case CelShape.Sphere: p += rng.InSphere() * l.ShapeSize.x; break;
                case CelShape.Shell: p += rng.OnSphere() * l.ShapeSize.x; break;
                case CelShape.Disc:
                {
                    Vector2 d = rng.InDisc();
                    float rad = l.ShapeSize.y > 0f ? Mathf.Lerp(l.ShapeSize.y, l.ShapeSize.x, d.magnitude) : d.magnitude * l.ShapeSize.x;
                    Vector2 dn = d.sqrMagnitude > 1e-8f ? d.normalized : Vector2.right;
                    Vector3 level = new Vector3(fwd.x, 0f, fwd.z);
                    level = level.sqrMagnitude > 1e-6f ? level.normalized : Vector3.forward;
                    p += right * (dn.x * rad) + level * (dn.y * rad);
                    break;
                }
                case CelShape.Box: p += right * rng.Range(-0.5f, 0.5f) * l.ShapeSize.x + up * rng.Range(-0.5f, 0.5f) * l.ShapeSize.y + fwd * rng.Range(-0.5f, 0.5f) * l.ShapeSize.z; break;
                case CelShape.Line: p = Vector3.Lerp(origin, target, n > 1 ? j / (float)(n - 1) : rng.Next()) + right * l.Offset.x + up * l.Offset.y; break;
            }
            float life = Mathf.Max(0.02f, rng.Jitter(l.Life, l.LifeJitter));
            float speed = rng.Jitter(l.Speed, l.SpeedJitter);
            Vector3 v = Vector3.zero;
            switch (l.Aim)
            {
                case CelAim.Dir: v = rng.InCone(fwd, right, up, l.Spread) * speed; break;
                case CelAim.Back: v = rng.InCone(-fwd, right, up, l.Spread) * speed; break;
                case CelAim.Up: v = rng.InCone(Vector3.up, right, fwd, l.Spread) * speed; break;
                case CelAim.Radial:
                {
                    Vector3 o = p - anchor;
                    Vector3 d = o.sqrMagnitude > 1e-6f ? o.normalized : rng.OnSphere();
                    d.y = d.y + l.UpBias * (Mathf.Abs(d.y) + 0.5f);
                    v = d.normalized * speed;
                    break;
                }
                case CelAim.Target:
                {
                    Vector3 to = target - p;
                    float arrive = Mathf.Max(0.02f, life * Mathf.Clamp(l.Arrive, 0.01f, 1f));
                    v = l.Speed > 0f ? to.normalized * speed : to / arrive;
                    if (l.Spread > 0f) v = rng.InCone(v.normalized, right, up, l.Spread) * v.magnitude;
                    break;
                }
            }
            float born = now + l.Start + (n > 1 ? l.Duration * j / (n - 1) : 0f) + (l.TimeJitter > 0f ? rng.Range(-l.TimeJitter, l.TimeJitter) : 0f);
            float s = rng.Jitter(1f, l.SizeJitter);
            rec = new CelRecord
            {
                Pos = p, Born = Mathf.Max(now, born), Vel = v, Life = life,
                Axis = rng.OnSphere(), Seed = rng.Next(),
                Size = new Vector4(l.Size.From * s, l.Size.To * s, l.Size.Pow > 0f ? l.Size.Pow : 1f, l.Stretch),
                Motion = new Vector4(l.Gravity, l.Drag, l.Spin * s, l.Buoyancy),
                Erode = l.Erode.Pack(l.Feather),
                Tone = new Vector4(CelRecord.PackPalette(l.paletteId, l.coolId), l.Core, l.Opacity, l.Ink),
                Extra = l.Extra,
            };
            // a beam-like kind wants its far end: Axis carries target - p for the Line shape and the Target aim with no speed
            if (l.Aim == CelAim.None && l.Shape == CelShape.Line) rec.Axis = (target - origin);
        }

        // ------------------------------------------------------------------ chunks and lights, on their frame
        struct Pending
        {
            public CelLayer Layer; public int Count; public Vector3 Origin, Fwd, Right, Up, Target; public uint Seed; public float Due;
        }
        const int PendingMax = 64;
        static readonly Pending[] pending = new Pending[PendingMax];
        static int pendingCount;

        static void Queue(CelLayer l, int n, Vector3 origin, Vector3 fwd, Vector3 right, Vector3 up, Vector3 target, uint seed, float due)
        {
            if (pendingCount >= PendingMax) { Fire(l, n, origin, fwd, right, up, target, seed); return; }   // full: now rather than never
            pending[pendingCount++] = new Pending { Layer = l, Count = n, Origin = origin, Fwd = fwd, Right = right, Up = up, Target = target, Seed = seed, Due = due };
            if (due <= Now) Pump();
        }

        /// <summary>Fire every queued chunk and light layer that is due (the renderer's LateUpdate calls it).</summary>
        public static void Pump()
        {
            float now = Now;
            for (int i = 0; i < pendingCount;)
            {
                if (pending[i].Due > now) { i++; continue; }
                var p = pending[i];
                pending[i] = pending[--pendingCount];
                pending[pendingCount] = default;
                Fire(p.Layer, p.Count, p.Origin, p.Fwd, p.Right, p.Up, p.Target, p.Seed);
            }
        }

        static void Fire(CelLayer l, int n, Vector3 origin, Vector3 fwd, Vector3 right, Vector3 up, Vector3 target, uint seed)
        {
            Vector3 at = (l.Anchor == CelAnchor.Target ? target : origin) + right * l.Offset.x + up * l.Offset.y + fwd * l.Offset.z;
            int row = Mathf.Max(0, l.paletteId);
            var rows = CelPalette.Rows;
            if (l.kindId == CelKinds.Light)
            {
                if (SceneHooks.Flash == null) return;
                Vector4 c = rows[row * CelPalette.Stride + 3];
                SceneHooks.Flash(at, new Color(c.x, c.y, c.z, 1f), l.Core, l.Extra.x > 0f ? l.Extra.x : 12f, l.Life);
                return;
            }
            if (Chunks == null) return;
            Vector4 t = rows[row * CelPalette.Stride + 1];
            Chunks((int)l.Extra.x, at, n, l.Speed, l.Size.From, new Color(t.x, t.y, t.z, 1f), l.Life, l.Extra.y, seed);
        }

        /// <summary>Chunk and light layers still waiting for their frame (tests).</summary>
        public static int PendingCount => pendingCount;
    }
}
