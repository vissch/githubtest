// Phase: unit motion (implemented 2026-09-30) — what the troops' own actions look like where the figure alone cannot
// show it, drawn from events the sim already raises (no sim state is written): the paratroopers' aircraft and their
// canopies (DropInbound, DropLanded), the jetpack's flame and smoke on his leap (LeapStarted; the arc itself is drawn
// by VATRenderer from AnimationController.LeapLine), a spark where a round rings off a shield plate (ShieldBlocked),
// and the small signs of a medic's and an engineer's work (UnitHealed, VehicleHullMended). Timed by the sim's clock
// (SimClock), so a paused or slowed match holds them. Uses FlipbookFx's and SceneHooks' public calls only.
// The paratroopers: the sim's dice are read ahead (UnitMotion.ParaLandings), so each canopy comes down on the point
// and the tick the sim puts its man there; the man hanging under it is a VATRenderer extra (set 1) until the sim's
// own man takes over on DropLanded, landing in Jumping Down's crouch (AnimationController).
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed class TroopMotionFx : MonoBehaviour
    {
        public SimHost Host;

        /// <summary>The VATRenderer extra set the hanging paratroopers are drawn through (0 is the riders').</summary>
        public const int ExtraSet = 1;
        public const int MaxDrops = 4, MenPerDrop = 8;
        public const float CanopyRadius = 3f, CanopyHeight = 1.7f, Lines = 4.2f;   // m: the canopy and its rigging
        public const float CollapseSeconds = 1.4f, LieSeconds = 8f, SinkSeconds = 2f;
        const float JetEvery = 0.03f, SmokeEvery = 0.07f;

        sealed class Drop
        {
            public int Player; public float3 Point, Heading; public float Called, Lands; public int Men;
            public readonly float3[] Landing = new float3[MenPerDrop];
            public readonly float[] Exit = new float[MenPerDrop];
            public readonly float[] Down = new float[MenPerDrop];   // sim seconds his canopy touched down (0: still up)
            public readonly bool[] Handed = new bool[MenPerDrop];   // the sim's man has taken over
            public readonly uint[] Seed = new uint[MenPerDrop];
        }

        sealed class Leap { public int Slot; public ushort Gen; public float Start, Air; public float3 From, To; public float Peak, NextJet, NextSmoke; }

        readonly List<Drop> drops = new List<Drop>(MaxDrops);
        readonly List<Leap> leaps = new List<Leap>(16);
        readonly Dictionary<int, float> nextCue = new Dictionary<int, float>(32);   // per medic or engineer slot: the next sim second a cue may show
        FlipbookFx books;
        VATRenderer vat;
        Mesh canopy, cube; Material canopyMat, lineMat, planeMat;
        readonly VatInstance[] hang = new VatInstance[MaxDrops * MenPerDrop];
        readonly byte[] hangFig = new byte[MaxDrops * MenPerDrop];
        readonly Matrix4x4[] canopies = new Matrix4x4[MaxDrops * MenPerDrop];
        readonly Matrix4x4[] lines = new Matrix4x4[MaxDrops * MenPerDrop * 4];
        bool subscribed;
        MatchSim lastMatch;
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 5000f);

        /// <summary>Drops in the air or with canopies still lying (for tools and tests).</summary>
        public int ActiveDrops => drops.Count;
        /// <summary>Paratroopers hanging under a canopy this frame.</summary>
        public int Hanging { get; private set; }
        /// <summary>Canopies drawn this frame (in the air, collapsing or lying).</summary>
        public int CanopiesDrawn { get; private set; }
        /// <summary>Of the men the last drop put down, how many landed within half a metre of where their canopy came down,
        /// and how many the sim put down at all (the tool's count: TW.Editor eval reads it).</summary>
        public int LastDropMatched { get; private set; }
        public int LastDropLanded { get; private set; }
        /// <summary>Jetpack men in the air, drawn with a flame this frame.</summary>
        public int Jets { get; private set; }

        void Start()
        {
            if (Host == null) Host = FindFirstObjectByType<SimHost>();
            books = new FlipbookFx();
            cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            canopy = BuildCanopy();
            var toon = Shader.Find("TW/Toon (URP)");
            if (toon != null)
            {
                canopyMat = Toon(toon, new Color(0.80f, 0.77f, 0.66f), 1.6f);   // undyed canvas
                lineMat = Toon(toon, new Color(0.25f, 0.23f, 0.2f), 0f);
                planeMat = Toon(toon, new Color(0.42f, 0.4f, 0.33f), 1.8f);
            }
        }

        static Material Toon(Shader shader, Color c, float outline)
        {
            var m = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            m.SetColor("_BaseColor", c); m.SetFloat("_OutlineWidth", outline);
            return m;
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            if (canopy != null) Destroy(canopy);
            if (canopyMat != null) Destroy(canopyMat);
            if (lineMat != null) Destroy(lineMat);
            if (planeMat != null) Destroy(planeMat);
        }

        VATRenderer Vat() { if (vat == null) vat = FindFirstObjectByType<VATRenderer>(); return vat; }
        float TickSeconds => Host != null && Host.Local != null ? Host.Local.World.Config.TickSeconds : 0.05f;
        float Ground(float x, float z) => Host != null && Host.Local != null ? RenderGround.Sample(Host.Local.Map, x, z) : 0f;

        // ------------------------------------------------------------------ events
        void OnSimEvent(SimEvent e)
        {
            if (Host == null || Host.Local == null) return;
            var w = Host.Local.World;
            float tick = TickSeconds;
            switch (e.Type)
            {
                case SimEventType.DropInbound:
                {
                    if (!OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats)) break;
                    if (drops.Count >= MaxDrops) drops.RemoveAt(0);
                    var d = new Drop { Player = e.A, Point = e.Pos, Heading = UnitMotion.DropHeading(e.A), Called = e.Tick * tick, Men = math.min(e.B, MenPerDrop) };
                    uint landTick = e.Tick + (uint)math.round(e.Scalar / tick);
                    d.Lands = landTick * tick;
                    d.Men = UnitMotion.ParaLandings(w, Host.Local.Map, e.A, e.Pos, landTick, d.Men, stats.Radius, d.Landing);
                    for (int k = 0; k < d.Men; k++)
                    {
                        d.Exit[k] = UnitMotion.ParaExit(d.Point, d.Heading, d.Landing[k]);
                        d.Seed[k] = (uint)(e.Tick * 31 + k * 7919);
                    }
                    drops.Add(d);
                    LastDropMatched = 0; LastDropLanded = 0;
                    break;
                }
                case SimEventType.DropLanded:
                {
                    // the sim's man is on the ground: his canopy's hanging figure gives way to him (the nearest canopy of a
                    // drop of his side that is due now), and the canopy starts to fall in
                    Drop best = null; int bestK = -1; float bestD = float.MaxValue;
                    foreach (var d in drops)
                    {
                        if (d.Player != e.B) continue;
                        for (int k = 0; k < d.Men; k++)
                        {
                            if (d.Handed[k]) continue;
                            float dd = math.distancesq(d.Landing[k].xz, e.Pos.xz);
                            if (dd < bestD) { bestD = dd; best = d; bestK = k; }
                        }
                    }
                    LastDropLanded++;
                    if (best == null) break;
                    if (bestD <= 0.25f) LastDropMatched++;
                    best.Handed[bestK] = true;
                    if (best.Down[bestK] <= 0f) best.Down[bestK] = e.Tick * tick;
                    best.Landing[bestK] = e.Pos;   // the sim has the last word on where
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, (Vector3)e.Pos + Vector3.up * (Ground(e.Pos.x, e.Pos.z) + 0.2f - e.Pos.y), 1.2f, 0.9f, velocity: Vector3.up * 0.3f, grow: 1f, alpha: 0.35f);
                    break;
                }
                case SimEventType.LeapStarted:
                {
                    if (e.A < 0 || e.A >= w.HighWater) break;
                    if (leaps.Count >= 16) leaps.RemoveAt(0);
                    float3 from = e.Dir, to = e.Pos;
                    leaps.Add(new Leap { Slot = e.A, Gen = w.Generation[e.A], Start = e.Tick * tick, Air = e.Scalar, From = from, To = to, Peak = UnitMotion.LeapPeak(math.distance(from.xz, to.xz)) });
                    if (books != null && books.Ready)
                    {
                        // the take-off: the jet kicks the ground up under him
                        var at = new Vector3(from.x, Ground(from.x, from.z) + 0.1f, from.z);
                        books.Add(FlipbookFx.Book.Puff, at, 2.4f, 1.2f, FlipbookFx.Kind.None, velocity: Vector3.up * 0.6f, grow: 1.4f, alpha: 0.55f);
                        books.Add(FlipbookFx.Book.Wings, at, 3f, 0.6f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.4f, alpha: 0.5f);
                        books.Add(FlipbookFx.Book.Flash, at + Vector3.up * 0.6f, 1.8f, 0.12f, glow: SceneMood.Night ? 3f : 1.6f, pop: 0.4f);
                    }
                    break;
                }
                case SimEventType.ShieldBlocked:
                {
                    // the round rings off the plate: a spark on its face, towards the shot
                    if (e.A < 0 || e.A >= w.HighWater || Host.Presenter == null) break;
                    float3 p = Host.Presenter.Drawn(e.A);
                    var dir = new Vector3(-e.Dir.x, 0f, -e.Dir.z);
                    if (dir.sqrMagnitude < 1e-4f) dir = new Vector3(Mathf.Sin(w.Yaw[e.A]), 0f, Mathf.Cos(w.Yaw[e.A]));
                    dir.Normalize();
                    float grow = Vat() != null ? vat.CurrentGrow : 1f;
                    var at = new Vector3(p.x, Ground(p.x, p.z), p.z) + Vector3.up * (1.05f * FigureMetrics.UnitScale * grow) + dir * (0.45f * grow);
                    SceneHooks.Sparks?.Invoke(at, 5);
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Star, at, 0.7f, 0.07f, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 2.6f : 1.6f);
                    break;
                }
                case SimEventType.UnitHealed:
                {
                    // a field dressing going on: a pale puff at the patient now and then, when the medic is close enough to be at him
                    if (e.A < 0 || e.B < 0 || e.A >= w.HighWater || e.B >= w.HighWater || !Due(e.A, 0.9f)) break;
                    float3 a = w.Position[e.A], b = w.Position[e.B];
                    if (math.distance(a.xz, b.xz) > AnimationController.TendReach) break;
                    float grow = Vat() != null ? vat.CurrentGrow : 1f;
                    var at = new Vector3(b.x, Ground(b.x, b.z) + 0.9f * FigureMetrics.UnitScale * grow, b.z);
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, at, 0.45f, 0.7f, velocity: Vector3.up * 0.25f, grow: 0.6f, alpha: 0.45f);
                    break;
                }
                case SimEventType.VehicleHullMended:
                {
                    // the engineer's tools on the plate: a few sparks on the hull where he stands
                    if (e.A < 0 || e.B < 0 || e.A >= w.HighWater || e.B >= w.HighWater || !Due(e.B, 0.5f)) break;
                    float3 hull = w.Position[e.A], man = w.Position[e.B];
                    var to = new Vector3(man.x - hull.x, 0f, man.z - hull.z);
                    float d = to.magnitude;
                    if (d < 0.01f) break;
                    var kin = Host.Local.Vehicles;
                    float half = kin != null && kin.Profiles.IsCreated ? math.min(kin.Profiles[w.Archetype[e.A]].HalfWidth + 0.3f, d) : math.min(1.5f, d);
                    var at = new Vector3(hull.x, 0f, hull.z) + to / d * half;
                    at.y = Ground(at.x, at.z) + 1.1f;
                    SceneHooks.Sparks?.Invoke(at, 3);
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Star, at, 0.5f, 0.06f, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 2.4f : 1.4f);
                    break;
                }
            }
        }

        bool Due(int slot, float every)
        {
            float now = SimClock.Seconds(Host);
            if (nextCue.TryGetValue(slot, out float at) && now < at) return false;
            nextCue[slot] = now + every;
            return true;
        }

        // ------------------------------------------------------------------ frame
        void LateUpdate()
        {
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (Host.Local != lastMatch) { lastMatch = Host.Local; drops.Clear(); leaps.Clear(); nextCue.Clear(); }
            float now = SimClock.Seconds(Host);
            Jets = 0;
            JetsFrame(now);
            DropsFrame(now);
            if (books != null && books.Ready) books.Draw(Time.time, Everywhere);
        }

        void JetsFrame(float now)
        {
            var w = Host.Local.World;
            var anim = Host.Animation;
            float grow = Vat() != null ? vat.CurrentGrow : 1f;
            for (int i = leaps.Count - 1; i >= 0; i--)
            {
                var l = leaps[i];
                bool gone = l.Slot >= w.HighWater || w.Generation[l.Slot] != l.Gen || !w.IsAlive(l.Slot);
                if (gone || now > l.Start + l.Air + 0.2f) { leaps.RemoveAt(i); continue; }
                if (Host.Presenter == null || books == null || !books.Ready) continue;
                float3 p = Host.Presenter.Drawn(l.Slot);
                float h = UnitMotion.ArcHeight(l.From.xz, l.To.xz, p.xz, l.Peak);
                float u = UnitMotion.LeapAlong(l.From.xz, l.To.xz, p.xz);
                if (u >= 0.999f) continue;
                Jets++;
                float3 fwd3 = math.normalizesafe(new float3(l.To.x - l.From.x, 0f, l.To.z - l.From.z), new float3(0f, 0f, 1f));
                var fwd = (Vector3)fwd3;
                float size = FigureMetrics.UnitScale * grow;
                // the pack sits between his shoulder blades; the jet points down and back, harder going up than coming down
                var pack = new Vector3(p.x, Ground(p.x, p.z) + h, p.z) + Vector3.up * (1.3f * size) - fwd * (0.3f * size);
                var jet = (Vector3.down * (u < 0.5f ? 1f : 0.7f) - fwd * 0.35f).normalized;
                if (now >= l.NextJet)   // on the sim's clock: slowed or paused, the jet puffs no faster than he flies
                {
                    l.NextJet = now + JetEvery;
                    // centred cards a hand's breadth under the pack, moving no more than his own length in their life (2026-09-30:
                    // an upright card thrown down at 7 m/s drew the flame a metre under his feet)
                    books.Add(FlipbookFx.Book.Fire, pack + jet * (0.2f * size), 0.6f * size, 0.12f, FlipbookFx.Kind.None, velocity: jet * 2.5f, grow: 0.4f, roll: UnityEngine.Random.Range(-0.3f, 0.3f), glow: SceneMood.Night ? 2.4f : 1.4f);
                    books.Add(FlipbookFx.Book.Flash, pack + jet * (0.1f * size), 0.8f * size, 0.06f, glow: SceneMood.Night ? 2.2f : 1.1f);
                }
                if (now >= l.NextSmoke)
                {
                    l.NextSmoke = now + SmokeEvery;
                    books.Add(FlipbookFx.Book.Smoke, pack + jet * (0.6f * size), 0.8f * size, 1.2f, UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: jet * 2.5f + Vector3.up * 0.4f, grow: 1.6f, roll: UnityEngine.Random.Range(-1f, 1f), alpha: 0.45f);
                }
            }
        }

        void DropsFrame(float now)
        {
            var v = Vat();
            var w = Host.Local.World;
            float tick = TickSeconds;
            float grow = v != null ? v.CurrentGrow : 1f;
            float size = FigureMetrics.UnitScale * grow;
            int men = 0, cans = 0, nLines = 0;
            var kit = SceneHooks.Biplane?.Invoke();
            for (int di = drops.Count - 1; di >= 0; di--)
            {
                var d = drops[di];
                float t = now - d.Called;
                // the aircraft: in low along its run, over the drop point DropOverAfter after the call, climbing away after
                float along = (t - UnitMotion.DropOverAfter) * UnitMotion.DropPlaneSpeed;
                float groundAtPoint = Ground(d.Point.x, d.Point.z);
                if (along < 260f)
                {
                    float climb = along > 0f ? along * 0.12f : 0f;
                    var at = (Vector3)(d.Point + d.Heading * along); at.y = groundAtPoint + UnitMotion.DropAltitude + 2f + climb;
                    var rot = Quaternion.LookRotation((Vector3)d.Heading) * Quaternion.Euler(along > 0f ? -6f : 0f, 0f, Mathf.Sin(now * 1.3f) * 3f);
                    if (kit.HasValue && kit.Value.mesh != null && kit.Value.material != null)
                        FrameBudget.Draw(new RenderParams(kit.Value.material) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.On }, kit.Value.mesh, 0, Matrix4x4.TRS(at, rot, kit.Value.scale));
                    else if (planeMat != null)
                        FrameBudget.Draw(new RenderParams(planeMat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.On }, cube, 0, Matrix4x4.TRS(at, rot, new Vector3(9f, 1.2f, 7f)));
                }
                bool alive = along < 260f;
                for (int k = 0; k < d.Men; k++)
                {
                    float3 land = d.Landing[k];
                    float3 exitAt = d.Point + d.Heading * ((d.Exit[k] - UnitMotion.DropOverAfter) * UnitMotion.DropPlaneSpeed);
                    float landT = d.Lands - d.Called;
                    UnitMotion.ParaFall(t, d.Exit[k], landT, out float height, out float frac, out float open);
                    if (t < d.Exit[k]) { alive = true; continue; }   // still aboard
                    float3 xz = math.lerp(exitAt, land, frac);
                    float gy = Ground(xz.x, xz.z);
                    float yaw = Mathf.Atan2(d.Heading.x, d.Heading.z) + (Hash(d.Seed[k]) - 0.5f) * 1.2f;
                    float sway = Mathf.Sin((now + d.Seed[k] % 97) * 1.7f) * 0.12f * open;   // he swings a little under it
                    bool down = d.Handed[k] || now >= d.Lands;
                    if (down && d.Down[k] <= 0f) d.Down[k] = d.Lands;
                    // the man, until the sim's own takes over
                    if (!d.Handed[k] && now < d.Lands + 0.5f && men < hang.Length)
                    {
                        float left = d.Lands - now;
                        Clip clip; float phase;
                        if (open <= 0f) { clip = Clip.JumpDown; phase = AnimationController.JumpAir / Mathf.Max(0.1f, Clips.Table[(int)Clip.JumpDown].Seconds); }
                        else if (left <= AnimationController.JumpDescent) { clip = Clip.JumpDown; phase = Mathf.Clamp01((AnimationController.JumpTouch - Mathf.Max(0f, left)) / Mathf.Max(0.1f, Clips.Table[(int)Clip.JumpDown].Seconds)); }
                        else { clip = Clip.ClimbHold; phase = 0.3f; }   // hands up on the risers (the ladder's reach, held), knees bent
                        hang[men] = new VatInstance
                        {
                            Pos = new float3(xz.x, gy + math.max(0f, height), xz.z), Yaw = yaw + sway, AnimRow = (int)clip, AnimT = phase, Tint = d.Player, Scale = size,
                            PrevRow = (int)clip, PrevT = phase, Blend = 0f, Pad = VatPad.Pack(0, 0f, (int)(d.Seed[k] & 255)),
                        };
                        hangFig[men] = 0;
                        men++;
                        alive = true;
                    }
                    // the canopy: filling, then carrying him, then falling in beside him and lying there a while
                    if (open <= 0f || canopyMat == null) continue;
                    float since = d.Down[k] > 0f ? now - d.Down[k] : -1f;
                    if (since > CollapseSeconds + LieSeconds + SinkSeconds) continue;
                    alive = true;
                    var dir = (Vector3)d.Heading;
                    Vector3 feet = new Vector3(xz.x, gy + Mathf.Max(0f, height), xz.z);
                    if (since >= 0f) feet = new Vector3(land.x, Ground(land.x, land.z), land.z);
                    float fill = Mathf.SmoothStep(0.25f, 1f, open);
                    float r = CanopyRadius * fill, hgt = CanopyHeight * (0.4f + 0.6f * fill);
                    Vector3 skirt = feet + Vector3.up * (1.8f * size + Lines);
                    Quaternion tilt = Quaternion.AngleAxis(sway * Mathf.Rad2Deg, Vector3.Cross(Vector3.up, dir));
                    float squash = 1f;
                    Vector3 centre = skirt;
                    if (since >= 0f)
                    {
                        // it falls over downwind (the way the aircraft went), flattening as it comes down onto the ground
                        float c = Mathf.Clamp01(since / CollapseSeconds), ce = c * c * (3f - 2f * c);
                        tilt = Quaternion.AngleAxis(82f * ce, Vector3.Cross(Vector3.up, dir));
                        squash = Mathf.Lerp(1f, 0.18f, ce);
                        var lieAt = feet + dir * (1.2f + Lines * 0.8f);
                        lieAt.y = Ground(lieAt.x, lieAt.z) + 0.15f;
                        centre = Vector3.Lerp(skirt, lieAt, ce);
                        r = CanopyRadius * Mathf.Lerp(1f, 0.85f, ce);
                        float sink = Mathf.Clamp01((since - CollapseSeconds - LieSeconds) / SinkSeconds);
                        centre.y -= sink * 0.5f; r *= 1f - 0.6f * sink; squash *= 1f - 0.8f * sink;
                    }
                    if (cans < canopies.Length) canopies[cans++] = Matrix4x4.TRS(centre, tilt * Quaternion.AngleAxis(yaw * Mathf.Rad2Deg, Vector3.up), new Vector3(r, hgt * squash, r));
                    // the rigging: four lines from the skirt to his shoulders, while he hangs on them
                    if (since < 0f && lineMat != null)
                    {
                        Vector3 shoulders = feet + Vector3.up * (1.55f * size);
                        for (int q = 0; q < 4 && nLines < lines.Length; q++)
                        {
                            float a = yaw + q * Mathf.PI * 0.5f + Mathf.PI * 0.25f;
                            Vector3 rim = centre + tilt * new Vector3(Mathf.Sin(a) * r * 0.92f, 0f, Mathf.Cos(a) * r * 0.92f);
                            Vector3 mid = (rim + shoulders) * 0.5f, span = rim - shoulders;
                            lines[nLines++] = Matrix4x4.TRS(mid, Quaternion.LookRotation(span.normalized), new Vector3(0.035f, 0.035f, span.magnitude));
                        }
                    }
                }
                if (!alive) drops.RemoveAt(di);
            }
            Hanging = men; CanopiesDrawn = cans;
            if (v != null) v.DrawExtras(hang, hangFig, men, ExtraSet);
            if (cans > 0) FrameBudget.Draw(new RenderParams(canopyMat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true }, canopy, 0, canopies, cans);
            if (nLines > 0) FrameBudget.Draw(new RenderParams(lineMat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.Off }, cube, 0, lines, nLines);
        }

        static float Hash(uint s) { s ^= s >> 13; s *= 0x5bd1e995u; s ^= s >> 15; return (s & 0xFFFF) / 65535f; }

        /// <summary>A canopy: a shallow dome of unit radius and height, open underneath, its skirt scalloped between the
        /// gores, faced both ways so it reads from below and above.</summary>
        static Mesh BuildCanopy()
        {
            const int Gores = 12, Rings = 5;
            var verts = new List<Vector3>(); var tris = new List<int>();
            verts.Add(new Vector3(0f, 1f, 0f));
            for (int r = 1; r <= Rings; r++)
            {
                float a = r / (float)Rings * Mathf.PI * 0.5f;
                for (int g = 0; g < Gores * 2; g++)
                {
                    float b = g * Mathf.PI / Gores;
                    float scallop = r == Rings && (g & 1) == 1 ? 0.12f : 0f;   // the skirt lifts between the lines
                    float rad = Mathf.Sin(a) * (1f - scallop * 0.3f);
                    verts.Add(new Vector3(Mathf.Sin(b) * rad, Mathf.Cos(a) + scallop, Mathf.Cos(b) * rad));
                }
            }
            int n = Gores * 2;
            for (int g = 0; g < n; g++) { tris.Add(0); tris.Add(1 + g); tris.Add(1 + (g + 1) % n); }
            for (int r = 1; r < Rings; r++)
            {
                int a0 = 1 + (r - 1) * n, b0 = 1 + r * n;
                for (int g = 0; g < n; g++)
                {
                    int g1 = (g + 1) % n;
                    tris.Add(a0 + g); tris.Add(b0 + g); tris.Add(b0 + g1);
                    tris.Add(a0 + g); tris.Add(b0 + g1); tris.Add(a0 + g1);
                }
            }
            // the inside: its own vertices, so its normals face in (shared ones would average to nothing)
            int vcount = verts.Count, count = tris.Count;
            for (int k = 0; k < vcount; k++) verts.Add(verts[k]);
            for (int k = 0; k < count; k += 3) { tris.Add(vcount + tris[k]); tris.Add(vcount + tris[k + 2]); tris.Add(vcount + tris[k + 1]); }
            var mesh = new Mesh { name = "Canopy", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(verts); mesh.SetTriangles(tris, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds();
            return mesh;
        }
    }
}
