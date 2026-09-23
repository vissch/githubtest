// Phase: A5b / C4 (implemented) — depends on: SimHost (Local: world, TankGunnerySystem, VehicleModulesSystem,
// VehicleKinematicsSystem; Presenter for the drawn position; Events), TankModel, TW/Tank, TW/Flame, FlipbookFx,
// CameraShake, RenderGround, SceneHooks.
// The owner's two tanks, drawn from their parts (one instanced draw per part mesh, no GameObjects), and everything a
// tank does on screen:
//  - the hull sits on its tracks: the ground is read along each track and the hull rests on the highest points
//    front and back, so it bridges a trench level and noses down when it goes in; springs give it weight (it rocks
//    when it stops, fires or is hit) and the engine shivers it. Ditched it lies nose down in the trench; bogged it
//    sinks and its tracks spin;
//  - the tracks run: each track's tread rolls at the hull's speed plus or minus the turn (pivoting on the spot they
//    run opposite ways), the wheels turn with them;
//  - the guns: a sponson or turret traverses as the sim lays it (interpolated between ticks), the gun elevates to what
//    it is aimed at, recoils and throws a muzzle flare, smoke and a dust ring when it fires; a broken gun droops;
//    the Maw's commander turns his cupola to look round, or at what the guns are after; the hatch opens when the
//    crew bail out;
//  - damage: soot creeps over the paint as the structure goes, a broken track is thrown off and lies beside the
//    tank until it is mended, a hole can shear off a horn, an armour hit strikes sparks (a ricochet two), a burning
//    tank has flames on its engine deck and a smoke column, a hurt engine smokes black and a dead one stops;
//  - the end: a cook-off throws the turret, cupola, sponsons, horns and hatch into the air on fire and pops its
//    rounds for a few seconds; a tank that burned out or lost its crew just stays, scorched, hatch open. Either way the
//    hull stays where it died as the wreck, its pieces where they fell, embers and smoke dying away over a minute or
//    two. The sim's wreck prop (PropChanged with the slot) is drawn from that hull (SceneHooks.DrawnWreck); a hull the
//    sim found no place for (it died in a trench with no free cell near) burns out and sinks away, since nothing of it
//    blocks or gives cover. Dropping the oldest wreck over MaxWrecks hands its prop back to the prop layer.
// Beyond LodDistance the far model is drawn (the same pivots; no tracks or wheels of its own).
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Units;

namespace TW.Presentation.Tactical
{
    [DefaultExecutionOrder(500)]
    public sealed class TankRenderer : MonoBehaviour
    {
        public SimHost Host;
        public float LodDistance = 170f;
        public int MaxWrecks = 48;
        const float UnlinkedSinkAfter = 75f;   // seconds a hull with no sim wreck prop burns before it sinks away
        public static readonly Vector4 TeamTintB = new Vector4(0.52f, 0.56f, 0.60f, 0.8f);   // field grey
        // the sides' colours (horns, ground ring): blue and red read against the night grade and against each other
        public static readonly Color TeamA = new Color(0.35f, 0.85f, 1.0f), TeamB = new Color(0.88f, 0.25f, 0.16f);

        struct Spring
        {
            public float Value, Velocity;
            /// <summary>
            /// Critically damped toward the target at angular frequency omega, solved exactly over the step:
            /// x(t) = (x0 + (v0 + omega x0) t) e^(-omega t). The explicit step it replaces goes unstable once omega dt
            /// nears 2, which a long frame reaches (a hitch, an editor stall: 0.33 s at omega 10), and a blast's kick then
            /// grew each frame until the hull was drawn kilometres up (seen in Play 2026-09-23).
            /// </summary>
            public void Step(float target, float dt, float omega)
            {
                float x = Value - target, e = Mathf.Exp(-omega * dt), j = Velocity + omega * x;
                Value = target + (x + j * dt) * e;
                Velocity = (Velocity - omega * j * dt) * e;
            }
        }

        sealed class View
        {
            public int Slot; public ushort Gen; public byte Team; public TankModel Model;
            public Vector3 Pos, LastPos; public float Yaw, LastYaw; public bool Seen;
            public float Speed, YawRate;
            public Spring Pitch, Roll, Heave;
            public float TreadL, TreadR, WheelL, WheelR;
            public readonly float[] GunYaw = new float[2], GunPitch = new float[2], Recoil = new float[2];
            public float Hatch, Cupola, CupolaWant, NextLook;
            /// <summary>A walker: how far through its leg cycle it is (0..1, advanced by distance travelled so the
            /// feet never skate), how much of a stride it is taking, the bob that puts on the body, what its claws
            /// are doing, and which legs are gone.</summary>
            public float Stride, Gait, Bob, Claw, ClawOpen;
            public byte LegsLost;
            public byte Archetype;
            public float Scorch, Burn, Flash, Furnace, Throttle;
            public bool Ditched, Bogged, Stalled, Dead, CookOff, Hurt;
            public int State; public float Fire;
            public bool[] Off;                     // LOD0 parts drawn apart (debris), by index
            public float NextExhaust, NextDust, NextSmoke, Born, DiedAt;
            public bool Linked; public Vector3 PropPos;   // the sim's wreck prop drawn by this hull
            public Matrix4x4[] World;              // LOD0 part matrices, this frame
            public readonly List<Debris> Pieces = new List<Debris>();
        }

        sealed class Debris
        {
            public View Owner; public int Part; public Matrix4x4 World;
            public Vector3 Vel, Spin; public bool Resting, Thrown;   // Thrown: a track slid off, put back when it is mended
            public float Burn;
            public readonly Dictionary<int, Matrix4x4> Local = new Dictionary<int, Matrix4x4>();   // the part's children as they were
        }

        sealed class Batch
        {
            public Mesh Mesh; public Material Material; public int Count;
            public readonly Matrix4x4[] M = new Matrix4x4[1023];
            public readonly float[] Tread = new float[1023];
            public readonly Vector4[] Damage = new Vector4[1023], Tint = new Vector4[1023], Team = new Vector4[1023];
            public readonly MaterialPropertyBlock Props = new MaterialPropertyBlock();
        }

        struct Pop { public View Owner; public float At; public Vector3 Offset; public float Size; }
        struct Flame { public Vector3 Foot; public float Width, Height, Phase; }

        TankModel maw, tusk;
        /// <summary>The owner's four crab walkers (Tools/crabsplit.py), by archetype. Each has its own atlas, where
        /// the two tanks share one, so a crab's material is per machine and per LOD.</summary>
        readonly TankModel[] crabs = new TankModel[6];
        readonly Material[,] crabMats = new Material[6, 2];
        static readonly string[] CrabNames = { "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt" };
        public const float StrideMetres = 1.15f;   // how far a walker travels per full leg cycle
        readonly Material[] mats = new Material[2];
        FlipbookFx books;
        readonly Dictionary<int, View> views = new Dictionary<int, View>();
        readonly List<View> wrecks = new List<View>();
        readonly List<Debris> debris = new List<Debris>();
        readonly List<Pop> pops = new List<Pop>();
        readonly Dictionary<Mesh, Batch> batches = new Dictionary<Mesh, Batch>();
        readonly List<Flame> flames = new List<Flame>();
        readonly List<int> gone = new List<int>();
        Material flameMat; Mesh flameMesh;
        readonly List<Vector3> fPos = new List<Vector3>(); readonly List<Vector2> fCorner = new List<Vector2>(); readonly List<Vector4> fShape = new List<Vector4>(); readonly List<int> fTris = new List<int>();
        float[] prevYaw, curYaw, prevGun, curGun;
        uint lastTick = uint.MaxValue;
        bool subscribed;
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 5000f);
        static readonly int TreadId = Shader.PropertyToID("_Tread"), DamageId = Shader.PropertyToID("_Damage"), TintId = Shader.PropertyToID("_Tint"), TeamId = Shader.PropertyToID("_Team"), ColorId = Shader.PropertyToID("_Color");
        Material discMat; Mesh discMesh;
        readonly Matrix4x4[] discM = new Matrix4x4[256]; readonly Vector4[] discC = new Vector4[256]; int discCount;
        MaterialPropertyBlock discProps;   // made in BuildDisc: Unity does not allow it from a MonoBehaviour's field initialiser

        static Vector4 TeamBand(View v, TankPartRole role)
        {
            if (role != TankPartRole.Horn) return Vector4.zero;
            var c = v.Team == 1 ? TeamB : TeamA;
            return new Vector4(c.r, c.g, c.b, v.Dead ? 0.5f : 1f);
        }

        public bool Ready => maw != null && mats[0] != null;

        void Start()
        {
            if (Host == null) Host = FindFirstObjectByType<SimHost>();
            maw = TankModel.Load("Maw", VehicleArchetype.Maw);
            tusk = TankModel.Load("Tusk", VehicleArchetype.Tusk);
            for (int c = 0; c < CrabNames.Length; c++) crabs[c] = TankModel.Load(CrabNames[c], (byte)(VehicleArchetype.Pincer + c), "Body");
            var shader = Shader.Find("TW/Tank (URP)");
            if (shader == null || maw == null) { Debug.LogWarning("TankRenderer: TW/Tank or the tank models are missing; the box tanks stay."); enabled = false; return; }
            for (int lod = 0; lod < 2; lod++)
            {
                mats[lod] = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, name = "Tank LOD" + lod };
                var atlas = Resources.Load<Texture2D>("Vehicles/TankAtlas_LOD" + lod);
                if (atlas != null) mats[lod].SetTexture("_BaseMap", atlas);
                mats[lod].SetFloat("_OutlineWidth", lod == 0 ? 2.2f : 1.4f);
            }
            for (int c = 0; c < CrabNames.Length; c++)
            {
                var atlas = Resources.Load<Texture2D>("Vehicles/" + CrabNames[c] + "Atlas");
                for (int lod = 0; lod < 2; lod++)
                {
                    var m = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, name = CrabNames[c] + " LOD" + lod };
                    if (atlas != null) m.SetTexture("_BaseMap", atlas);
                    m.SetFloat("_OutlineWidth", lod == 0 ? 2.2f : 1.4f);
                    crabMats[c, lod] = m;
                }
            }
            books = new FlipbookFx();
            BuildFlames();
            BuildDisc();
            SceneHooks.TanksDrawn = true;
            SceneHooks.VehicleTracks = slot => { var m = ModelFor(Host?.Local?.World, slot); return m != null ? new Vector2(m.HalfGauge, m.HalfLength) : new Vector2(0.78f, 1.9f); };
            SceneHooks.DrawnWreck = (x, z) =>
            {
                foreach (var v in wrecks) if (v.Linked && (v.PropPos.x - x) * (v.PropPos.x - x) + (v.PropPos.z - z) * (v.PropPos.z - z) < 1f) return true;
                return false;
            };
            SceneHooks.IsTankSlot = slot => views.TryGetValue(slot, out var tv) && !tv.Dead;
            SceneHooks.VehicleGunPort = slot =>
            {
                // the machine guns fire from the Maw's mouth, and beside the Tusk's 37 mm
                if (!views.TryGetValue(slot, out var v) || v.Dead) return Vector4.zero;
                var at = SocketWorld(v, v.Model.Sockets.ContainsKey("Socket_HullMG") && v.Model.Archetype == VehicleArchetype.Maw ? "Socket_HullMG" : "Socket_Muzzle", out bool ok);
                return ok ? new Vector4(at.x, at.y, at.z, 1f) : Vector4.zero;
            };
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            SceneHooks.TanksDrawn = false; SceneHooks.VehicleTracks = null; SceneHooks.DrawnWreck = null; SceneHooks.VehicleGunPort = null; SceneHooks.IsTankSlot = null;
            foreach (var m in mats) if (m != null) Destroy(m);
            if (flameMat != null) Destroy(flameMat);
            if (discMat != null) Destroy(discMat);
            if (discMesh != null) Destroy(discMesh);
            if (flameMesh != null) Destroy(flameMesh);
        }

        TankModel ModelFor(SimWorld w, int slot) => w == null || slot < 0 || slot >= w.HighWater ? null : ModelFor(w.Archetype[slot]);
        TankModel ModelFor(byte archetype)
        {
            if (VehicleArchetype.IsWalker(archetype))
            {
                var c = crabs[archetype - VehicleArchetype.Pincer];
                if (c != null) return c;
            }
            return archetype == VehicleArchetype.Tusk && tusk != null ? tusk : maw;
        }

        /// <summary>The material a machine is drawn in: the tanks share an atlas, each crab has its own.</summary>
        Material MaterialFor(byte archetype, int lod)
            => VehicleArchetype.IsWalker(archetype) && crabMats[archetype - VehicleArchetype.Pincer, lod] != null
                ? crabMats[archetype - VehicleArchetype.Pincer, lod] : mats[lod];

        static bool IsTank(SimWorld w, int i)
            => (w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) == ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle) && VehicleArchetype.IsArmoured(w.Archetype[i]);

        // ------------------------------------------------------------------ frame
        void LateUpdate()
        {
            if (Host == null || Host.Local == null || Host.Presenter == null || !Ready) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            var match = Host.Local; var w = match.World;
            float dt = Mathf.Max(1e-4f, Time.deltaTime), now = Time.time;
            Capture(match);
            foreach (var v in views.Values) v.Seen = false;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!IsTank(w, i)) continue;
                if (!views.TryGetValue(i, out var v) || v.Gen != w.Generation[i]) { v = NewView(w, i, now); views[i] = v; }
                v.Seen = true;
                Animate(match, v, dt, now);
            }
            gone.Clear();
            foreach (var kv in views) if (!kv.Value.Seen) gone.Add(kv.Key);
            foreach (int slot in gone) { Wreckify(views[slot], now); views.Remove(slot); }   // missed its VehicleDestroyed: still leave a wreck
            for (int k = wrecks.Count - 1; k >= 0; k--) if (!Smoulder(wrecks[k], dt, now)) Drop(k);
            while (wrecks.Count > MaxWrecks) Drop(0);
            FlyDebris(dt, match);
            RunPops(now);
            Draw();
            if (books != null && books.Ready) books.Draw(now, Everywhere);
            DrawFlames();
        }

        void Capture(TW.Sim.Match.MatchSim match)
        {
            var w = match.World;
            int n = w.Config.MaxSlots;
            if (curYaw == null) { prevYaw = new float[n]; curYaw = new float[n]; prevGun = new float[n * 2]; curGun = new float[n * 2]; }
            if (w.Tick == lastTick) return;
            lastTick = w.Tick;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!IsTank(w, i)) continue;
                bool fresh = !views.TryGetValue(i, out var v) || v.Gen != w.Generation[i];
                prevYaw[i] = fresh ? w.Yaw[i] : curYaw[i]; curYaw[i] = w.Yaw[i];
                for (int k = 0; k < 2; k++)
                {
                    float g = match.Gunnery != null ? match.Gunnery.GunYaw[i * TankGunnerySystem.Guns + k] : 0f;
                    prevGun[i * 2 + k] = fresh ? g : curGun[i * 2 + k]; curGun[i * 2 + k] = g;
                }
            }
        }

        View NewView(SimWorld w, int slot, float now)
        {
            var model = ModelFor(w.Archetype[slot]);
            var v = new View { Slot = slot, Gen = w.Generation[slot], Team = w.Team[slot], Model = model, Born = now, Archetype = w.Archetype[slot] };
            v.Pos = v.LastPos = (Vector3)(float3)w.Position[slot];
            v.Yaw = v.LastYaw = w.Yaw[slot];
            v.Off = new bool[model.Lods[0].Parts.Count];
            v.World = new Matrix4x4[model.Lods[0].Parts.Count];
            v.Heave.Value = Ground(v.Pos.x, v.Pos.z);
            v.Cupola = v.CupolaWant = 0f;
            for (int k = 0; k < 2; k++) v.GunYaw[k] = k < TankSpec.For(w.Archetype[slot]).GunCount ? TankSpec.For(w.Archetype[slot]).Gun(k).RestYaw : 0f;
            return v;
        }

        float Ground(float x, float z) => Host != null && Host.Local != null ? RenderGround.Sample(Host.Local.Map, x, z) : 0f;

        // ------------------------------------------------------------------ one tank, one frame
        void Animate(TW.Sim.Match.MatchSim match, View v, float dt, float now)
        {
            var w = match.World; var m = v.Model; int s = v.Slot;
            float3 drawn = Host.Presenter.Drawn(s);
            v.LastPos = v.Pos; v.LastYaw = v.Yaw;
            v.Pos = new Vector3(drawn.x, 0f, drawn.z);
            v.Yaw = LerpAngle(prevYaw[s], curYaw[s], Host.Alpha);
            Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
            float speed = Vector3.Dot(v.Pos - v.LastPos, fwd) / dt, yawRate = Mathf.DeltaAngle(v.LastYaw * Mathf.Rad2Deg, v.Yaw * Mathf.Rad2Deg) * Mathf.Deg2Rad / dt;
            if (Host.TimeScale <= 0f) { speed = 0f; yawRate = 0f; }
            v.Speed = Mathf.Lerp(v.Speed, speed, 1f - Mathf.Exp(-dt * 10f));
            v.YawRate = Mathf.Lerp(v.YawRate, yawRate, 1f - Mathf.Exp(-dt * 10f));

            // what the sim says about it
            var modules = match.Modules; var kin = match.Vehicles; var gun = match.Gunnery;
            int M = (int)VehicleModule.Count;
            v.State = modules != null ? modules.State[s] : 0;
            v.Fire = modules != null ? modules.Fire[s] : 0f;
            float engine = modules != null ? modules.Module[s * M + (int)VehicleModule.Engine] : 1f;
            bool trackL = modules == null || modules.Module[s * M + (int)VehicleModule.TrackLeft] > 0f;
            bool trackR = modules == null || modules.Module[s * M + (int)VehicleModule.TrackRight] > 0f;
            // A walker's legs. The cycle is advanced by the distance covered rather than by the clock, so its feet
            // never skate however fast it is going, and the body rides on the same phase.
            if (v.Model.LegCount > 0)
            {
                float pace = Mathf.Abs(v.Speed) + Mathf.Abs(v.YawRate) * 1.3f;
                bool dead = modules != null && modules.State[s] != 0;
                v.Gait = Mathf.Lerp(v.Gait, dead ? 0f : Mathf.Clamp01(pace / 1.4f), 1f - Mathf.Exp(-dt * 6f));
                v.Stride += pace * dt / StrideMetres;
                if (v.Stride >= 1f) v.Stride -= Mathf.Floor(v.Stride);
                v.Bob = Mathf.Sin(v.Stride * Mathf.PI * 4f) * 0.05f * v.Gait;
                byte lost = modules != null ? modules.LegsLost[s] : (byte)0;
                if (lost != v.LegsLost)
                {
                    for (int k = 0; k < 8; k++) if ((lost & (1 << k)) != 0 && (v.LegsLost & (1 << k)) == 0) ThrowLeg(v, k);
                    v.LegsLost = lost;
                }
                v.Claw = Mathf.Max(0f, v.Claw - dt * 1.7f);
                v.ClawOpen = Mathf.Lerp(v.ClawOpen, v.Claw > 0.45f ? 1f : 0f, 1f - Mathf.Exp(-dt * 14f));
            }
            v.Ditched = kin != null && kin.DitchTicks[s] > 0;
            v.Bogged = kin != null && kin.BogTicks[s] > 0;
            v.Stalled = engine <= 0f || v.State != 0;
            float hp = w.MaxHp[s] > 0f ? Mathf.Clamp01(w.Hp[s] / w.MaxHp[s]) : 1f;

            // the hull on its tracks
            float pitch, roll, heave;
            Settle(v, fwd, right, out pitch, out roll, out heave);
            if (v.Ditched) { pitch -= 17f * Mathf.Deg2Rad; heave -= 1.1f; }
            if (v.Bogged) heave -= 0.25f;
            float vib = v.Stalled ? 0f : (0.006f + 0.01f * v.Throttle);
            v.Pitch.Step(pitch, dt, 7f); v.Roll.Step(roll, dt, 7f); v.Heave.Step(heave + Mathf.Sin(now * 41f + s) * vib, dt, 10f);
            v.Throttle = Mathf.MoveTowards(v.Throttle, v.Stalled ? 0f : Mathf.Clamp01(Mathf.Abs(v.Speed) / 1.6f + Mathf.Abs(v.YawRate) * 0.8f + (v.Bogged || v.Ditched ? 0.9f : 0f)), dt * 1.5f);

            // tracks and wheels: each at the hull's speed plus or minus the turn; stuck, they spin
            float vl = v.Speed + v.YawRate * m.HalfGauge, vr = v.Speed - v.YawRate * m.HalfGauge;
            if ((v.Bogged || v.Ditched) && !v.Stalled) { vl = vr = 0.9f; }
            if (!trackL) vl = 0f; if (!trackR) vr = 0f;
            v.TreadL = Mathf.Repeat(v.TreadL + vl * dt / m.LinkLength, 1000f);
            v.TreadR = Mathf.Repeat(v.TreadR + vr * dt / m.LinkLength, 1000f);
            v.WheelL += vl * dt / m.WheelRadius; v.WheelR += vr * dt / m.WheelRadius;

            // guns: traverse as the sim lays them, elevate to the target, recoil
            var spec = TankSpec.For(w.Archetype[s]);
            for (int k = 0; k < spec.GunCount; k++)
            {
                v.GunYaw[k] = LerpAngle(prevGun[s * 2 + k], curGun[s * 2 + k], Host.Alpha);
                float want = 0f;
                int t = gun != null ? gun.GunTarget[s * TankGunnerySystem.Guns + k] : -1;
                if (t >= 0 && t < w.HighWater && w.IsAlive(t))
                {
                    Vector3 q = (Vector3)(float3)w.Position[t];
                    float dist = new Vector2(q.x - v.Pos.x, q.z - v.Pos.z).magnitude;
                    float dh = Ground(q.x, q.z) + ((w.Flags[t] & (uint)UnitFlags.Vehicle) != 0 ? 1.5f : 0.6f) - (v.Heave.Value + spec.Gun(k).Mount3.y);
                    want = Mathf.Atan2(dh, Mathf.Max(1f, dist)) + dist * 0.0007f;   // and a little for the drop
                }
                if (gun != null && gun.GunHealth[s * TankGunnerySystem.Guns + k] <= 0f) want = -7f * Mathf.Deg2Rad;
                v.GunPitch[k] = Mathf.MoveTowards(v.GunPitch[k], Mathf.Clamp(want, -8f * Mathf.Deg2Rad, 22f * Mathf.Deg2Rad), dt * 12f * Mathf.Deg2Rad);
                v.Recoil[k] = Mathf.Max(0f, v.Recoil[k] - dt * 2.6f);
            }

            // the commander: looks round, or at what the guns are after; out of the hatch when it is all over
            if (now >= v.NextLook)
            {
                v.NextLook = now + UnityEngine.Random.Range(2.5f, 6f);
                int t0 = gun != null ? gun.GunTarget[s * TankGunnerySystem.Guns] : -1;
                v.CupolaWant = t0 >= 0 && t0 < w.HighWater ? Mathf.DeltaAngle(0f, Mathf.Atan2(w.Position[t0].x - v.Pos.x, w.Position[t0].z - v.Pos.z) * Mathf.Rad2Deg - v.Yaw * Mathf.Rad2Deg) : UnityEngine.Random.Range(-70f, 70f);
            }
            v.Cupola = Mathf.MoveTowardsAngle(v.Cupola, v.CupolaWant, dt * 40f);
            v.Hatch = Mathf.MoveTowards(v.Hatch, v.State != 0 ? 1f : 0f, dt * 1.4f);

            // the paint: soot as the structure goes, embers while it burns, a flash when struck, the furnace working
            v.Scorch = Mathf.MoveTowards(v.Scorch, v.State != 0 ? 0.8f : (1f - hp) * 0.6f, dt * 0.2f);
            v.Burn = Mathf.MoveTowards(v.Burn, v.Fire, dt * 0.5f);
            v.Flash = Mathf.Max(0f, v.Flash - dt * 5f);
            v.Furnace = Mathf.MoveTowards(v.Furnace, v.Stalled ? 0.1f : 0.35f + 0.65f * v.Throttle, dt);

            Pose(v, v.Model.Lods[0], v.World);
            // a broken track is thrown off, and put back when it is mended
            Throw(v, "Track_L", !trackL, -1); Throw(v, "Track_R", !trackR, 1);
            Effects(match, v, engine, dt, now, fwd);
        }

        /// <summary>Where the hull rests: the ground under each track, front half and back half; it lies on the highest
        /// points, so a trench narrower than half its length is bridged level.</summary>
        void Settle(View v, Vector3 fwd, Vector3 right, out float pitch, out float roll, out float heave)
        {
            var m = v.Model;
            float hl = m.HalfLength * 0.9f, g = m.HalfGauge;
            float front = float.MinValue, rear = float.MinValue, left = float.MinValue, rightH = float.MinValue;
            for (int side = -1; side <= 1; side += 2)
            for (int k = 0; k < 9; k++)
            {
                float t = -1f + k * 0.25f;
                Vector3 at = v.Pos + fwd * (t * hl) + right * (side * g);
                float h = Ground(at.x, at.z);
                if (t > 0.25f) front = Mathf.Max(front, h);
                if (t < -0.25f) rear = Mathf.Max(rear, h);
                if (side < 0) left = Mathf.Max(left, h); else rightH = Mathf.Max(rightH, h);
            }
            // tracks longer than the gap bridge it: the hull rides the lips, not the hole between them
            pitch = Mathf.Clamp(Mathf.Atan2(front - rear, 1.5f * hl), -0.26f, 0.26f);
            roll = Mathf.Clamp(Mathf.Atan2(left - rightH, 2f * g), -0.2f, 0.2f);
            heave = Mathf.Max((front + rear) * 0.5f, Mathf.Max(front, rear) - 0.4f);
        }

        // ------------------------------------------------------------------ pose
        Quaternion HullRotation(View v)
            => Quaternion.AngleAxis(v.Yaw * Mathf.Rad2Deg, Vector3.up) * Quaternion.AngleAxis(-v.Pitch.Value * Mathf.Rad2Deg, Vector3.right) * Quaternion.AngleAxis(-v.Roll.Value * Mathf.Rad2Deg, Vector3.forward);

        /// <summary>A part's matrix in its parent's frame, with what it is doing now.</summary>
        Matrix4x4 PartLocal(View v, TankModel.Part p)
        {
            var rot = p.LocalRot;
            Vector3 pos = p.Local;
            switch (p.Role)
            {
                case TankPartRole.Turret:
                {
                    int k = p.Side > 0 ? 1 : 0;   // a crab has one turret a side; a tank has one on the centre line
                    rot *= Quaternion.AngleAxis(v.GunYaw[k] * Mathf.Rad2Deg, Vector3.up);
                    break;
                }
                case TankPartRole.Gun:
                {
                    int k = p.Gun >= 0 ? p.Gun : 0;
                    if (p.SelfAimed) rot *= Quaternion.AngleAxis(v.GunYaw[k] * Mathf.Rad2Deg, Vector3.up);   // a mortar on its bed, a gun on its pintle
                    rot *= Quaternion.AngleAxis(-v.GunPitch[k] * Mathf.Rad2Deg, Vector3.right);
                    pos += p.LocalRot * (Quaternion.AngleAxis(-v.GunPitch[k] * Mathf.Rad2Deg, Vector3.right) * Vector3.back) * (Kick(v.Recoil[k]) * 0.45f);
                    break;
                }
                // ---- the walkers ----
                case TankPartRole.Leg:
                case TankPartRole.Thigh:
                {
                    // the leg swings fore and aft about the body's up axis and lifts about the line across it, half
                    // the legs a half-cycle behind the other half: the alternating tripod a crab actually walks with
                    Vector3 across = Vector3.Cross(Vector3.up, p.Outward);
                    float phase = Phase(v, p);
                    rot = Quaternion.AngleAxis(Mathf.Sin(phase) * 14f * v.Gait, Vector3.up)
                        * Quaternion.AngleAxis(-Lift(v, phase), across) * rot;
                    break;
                }
                case TankPartRole.Shin:
                    rot = Quaternion.AngleAxis(Lift(v, Phase(v, p)) * 1.35f, Vector3.Cross(Vector3.up, p.Outward)) * rot;
                    break;
                case TankPartRole.Foot:
                    rot = Quaternion.AngleAxis(-Lift(v, Phase(v, p)) * 0.85f, Vector3.Cross(Vector3.up, p.Outward)) * rot;
                    break;
                case TankPartRole.Claw:
                {
                    float sway = Mathf.Sin(Phase(v, p) + (p.Side < 0 ? 0f : Mathf.PI)) * 5f * v.Gait;
                    rot = Quaternion.AngleAxis(sway - v.Claw * 24f * (p.Side < 0 ? 1f : -1f), Vector3.up) * rot;
                    break;
                }
                case TankPartRole.Jaw:
                    rot = Quaternion.AngleAxis(-36f * v.ClawOpen, Vector3.right) * rot;
                    break;
                case TankPartRole.Sponson:
                {
                    int k = Mathf.Max(0, p.Gun);
                    float rest = v.Model.ArtRestYaw[k];
                    Vector3 barrel = new Vector3(Mathf.Sin(rest), 0f, Mathf.Cos(rest)), across = new Vector3(barrel.z, 0f, -barrel.x);
                    rot *= Quaternion.AngleAxis((v.GunYaw[k] - rest) * Mathf.Rad2Deg, Vector3.up) * Quaternion.AngleAxis(-v.GunPitch[k] * Mathf.Rad2Deg, across);
                    pos += p.LocalRot * (Quaternion.AngleAxis((v.GunYaw[k] - rest) * Mathf.Rad2Deg, Vector3.up) * -barrel) * (Kick(v.Recoil[k]) * 0.3f);
                    break;
                }
                case TankPartRole.Hatch: rot *= Quaternion.AngleAxis(-105f * Mathf.SmoothStep(0f, 1f, v.Hatch), Vector3.right); break;
                case TankPartRole.Cupola: rot *= Quaternion.AngleAxis(v.Cupola, Vector3.up); break;
                case TankPartRole.Wheel: rot *= Quaternion.AngleAxis((p.Side < 0 ? v.WheelL : v.WheelR) * Mathf.Rad2Deg, Vector3.right); break;
            }
            return Matrix4x4.TRS(pos, rot, Vector3.one);
        }

        static float Kick(float r) => r <= 0f ? 0f : Mathf.Sin(Mathf.Clamp01((1f - r) * 6f) * Mathf.PI * 0.5f) * r;   // snaps back, runs out slow

        /// <summary>Where one leg is in the cycle: legs alternate along each side, and the two sides are opposite,
        /// which is the tripod gait. A part with no leg of its own (a claw) rides the body's own phase.</summary>
        static float Phase(View v, TankModel.Part p)
        {
            int perSide = Mathf.Max(1, v.Model.LegCount / 2);
            int leg = Mathf.Max(0, p.Leg);
            int group = ((leg % perSide) + (leg >= perSide ? 1 : 0)) & 1;
            return (v.Stride + group * 0.5f) * Mathf.PI * 2f;
        }

        /// <summary>How far a leg is off the ground at that phase: up through the forward half of the stride, down
        /// and planted through the back half.</summary>
        static float Lift(View v, float phase) => Mathf.Max(0f, Mathf.Cos(phase)) * 11f * v.Gait;

        /// <summary>A leg the damage system has taken off: it goes the way a track does, thrown clear.</summary>
        void ThrowLeg(View v, int leg)
        {
            var parts = v.Model.Lods[0].Parts;
            for (int i = 0; i < parts.Count; i++)
            {
                var p = parts[i];
                if (p.Leg != leg || (p.Role != TankPartRole.Leg && p.Role != TankPartRole.Thigh)) continue;
                Throw(v, p.Name, true, p.Outward.x < 0f ? -1 : 1);
                return;
            }
        }

        /// <summary>Every part of one LOD in the world; a part drawn apart (debris) and what hangs off it are left out.</summary>
        void Pose(View v, TankModel.Lod lod, Matrix4x4[] world)
        {
            var root = Matrix4x4.TRS(new Vector3(v.Pos.x, v.Heave.Value + v.Bob, v.Pos.z), HullRotation(v), Vector3.one);
            for (int i = 0; i < lod.Parts.Count; i++)
            {
                var p = lod.Parts[i];
                world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * PartLocal(v, p);
            }
        }

        bool IsOff(View v, TankModel.Lod lod, int i)
        {
            var parts0 = v.Model.Lods[0];
            for (int k = i; k >= 0; k = lod.Parts[k].Parent)
            {
                int j = lod == parts0 ? k : parts0.Find(lod.Parts[k].Name);
                if (j >= 0 && v.Off[j]) return true;
            }
            return false;
        }

        Vector3 SocketWorld(View v, string name, out bool ok)
        {
            ok = v.Model.Sockets.TryGetValue(name, out var s);
            return ok ? v.World[s.part].MultiplyPoint3x4(s.local) : v.Pos;
        }

        Vector3 MuzzleWorld(View v, int k, out Vector3 dir)
        {
            int part = v.Model.GunPart[k];
            dir = Vector3.forward;
            if (part < 0) return v.Pos + Vector3.up * 2.5f;
            var mtx = v.World[part];
            dir = mtx.MultiplyVector(v.Model.MuzzleLocal[k] - Vector3.zero).normalized;
            if (v.Model.Lods[0].Parts[part].Role == TankPartRole.Gun) dir = mtx.MultiplyVector(Vector3.forward).normalized;
            return mtx.MultiplyPoint3x4(v.Model.MuzzleLocal[k]);
        }

        // ------------------------------------------------------------------ effects
        void Effects(TW.Sim.Match.MatchSim match, View v, float engine, float dt, float now, Vector3 fwd)
        {
            if (books == null || !books.Ready) return;
            var cam = Camera.main;
            bool near = cam == null || (cam.transform.position - v.Pos).sqrMagnitude < 150f * 150f;
            // exhaust: faster as the engine works, black when it is hurt, nothing when it is dead
            if (!v.Stalled && now >= v.NextExhaust && near)
            {
                v.NextExhaust = now + Mathf.Lerp(0.4f, 0.13f, v.Throttle);
                for (int k = 0; k < 2; k++)
                {
                    var at = SocketWorld(v, "Socket_Exhaust" + k, out bool ok);
                    if (!ok) continue;
                    bool hurt = engine < 0.5f;
                    books.Add(FlipbookFx.Book.Smoke, at + Vector3.up * 0.15f, (hurt ? 1.3f : 0.8f) * (0.8f + v.Throttle * 0.5f), hurt ? 2.6f : 1.8f,
                        UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: Vector3.up * (1.1f + v.Throttle) - fwd * 0.6f + UnityEngine.Random.insideUnitSphere * 0.2f, grow: 1.4f, roll: UnityEngine.Random.Range(-1f, 1f), alpha: hurt ? 0.85f : 0.4f);
                }
            }
            // dust behind the tracks, mud when it is stuck or wading
            bool moving = Mathf.Abs(v.Speed) > 0.25f || v.Bogged || v.Ditched;
            if (moving && now >= v.NextDust && near)
            {
                v.NextDust = now + (v.Bogged || v.Ditched ? 0.08f : 0.16f);
                var layer = match.Map.LayerAt(new float3(v.Pos.x, 0f, v.Pos.z));
                bool mud = v.Bogged || v.Ditched || (layer & TW.Sim.Terrain.NavLayer.Mud) != 0;
                for (int side = 0; side < 2; side++)
                {
                    var at = SocketWorld(v, side == 0 ? "Socket_Dust_L" : "Socket_Dust_R", out bool ok);
                    if (!ok) continue;
                    if (SceneHooks.IsWater != null && SceneHooks.IsWater(at.x, at.z)) { SceneHooks.AddRing?.Invoke(at.x, at.z, 1.6f); continue; }
                    if (mud) books.Add(FlipbookFx.Book.Spurt, at, 1.1f, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (side == 0 ? FlipbookFx.Kind.Mirror : 0), velocity: -fwd * 1.5f + Vector3.up * 0.4f);
                    else books.Add(FlipbookFx.Book.Puff, at + Vector3.up * 0.15f, 1.0f, 0.8f, side == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None, velocity: -fwd * 0.5f + Vector3.up * 0.3f, grow: 1.0f, alpha: 0.28f);
                }
            }
            // fire on the engine deck and smoke off it
            if (v.Fire > 0f || v.State != 0) Burning(v, now, v.Fire, 1f);
        }

        void Burning(View v, float now, float fire, float smoke)
        {
            if (fire > 0.02f)
            {
                for (int k = 0; k < 2; k++)
                {
                    var at = SocketWorld(v, "Socket_Fire" + k, out bool ok);
                    if (ok) flames.Add(new Flame { Foot = at, Width = 0.7f + 1.3f * fire, Height = 1.2f + 2.6f * fire, Phase = v.Slot * 0.37f + k * 0.5f });
                }
                if (fire > 0.55f)
                {
                    var hatch = SocketWorld(v, "Socket_Crew", out bool ok);
                    if (ok) flames.Add(new Flame { Foot = hatch, Width = 0.9f * fire, Height = 2.4f * fire, Phase = v.Slot * 0.71f });
                }
                // the fire spreads over the deck: a few more tongues at fixed places on the hull (seeded by the slot, so
                // they do not jump), one of them big
                var hull = v.World[0];
                float hl = v.Model.HalfLength, hg = v.Model.HalfGauge;
                for (int k = 0; k < 3; k++)
                {
                    if (fire < 0.25f + 0.2f * k) break;
                    float a = Mathf.Sin((v.Slot + 1) * 12.9898f + k * 78.233f) * 43758.5453f; a -= Mathf.Floor(a);
                    float b = Mathf.Sin((v.Slot + 1) * 39.3468f + k * 11.135f) * 24634.6345f; b -= Mathf.Floor(b);
                    Vector3 local = new Vector3((a - 0.5f) * hg * 1.2f, 0f, (b - 0.6f) * hl * 1.4f);
                    Vector3 top = hull.MultiplyPoint3x4(local);
                    top.y = v.Heave.Value + v.Model.Height * (0.75f + 0.1f * a);
                    bool big = k == 0;
                    flames.Add(new Flame { Foot = top, Width = (big ? 1.6f : 0.8f) * fire, Height = (big ? 3.6f : 1.8f) * fire, Phase = v.Slot * 0.53f + k * 1.7f });
                }
            }
            if (books != null && books.Ready && now >= v.NextSmoke && smoke > 0.02f)
            {
                v.NextSmoke = now + Mathf.Lerp(0.6f, 0.18f, Mathf.Max(fire, smoke * 0.5f));
                var at = SocketWorld(v, "Socket_Fire0", out _);
                books.Add(FlipbookFx.Book.Smoke, at + Vector3.up * (0.6f + fire), (1.6f + 2.2f * fire) * Mathf.Max(0.4f, smoke), UnityEngine.Random.Range(4f, 7f),
                    UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: Vector3.up * (1.4f + fire * 1.5f) + new Vector3(UnityEngine.Random.Range(-0.3f, 0.3f), 0f, UnityEngine.Random.Range(-0.3f, 0.3f)),
                    grow: 2.2f, roll: UnityEngine.Random.Range(-0.7f, 0.7f), alpha: Mathf.Clamp01(0.45f + fire * 0.4f) * Mathf.Clamp01(smoke), pop: 0.2f);
            }
        }

        // ------------------------------------------------------------------ parts coming away
        void Throw(View v, string name, bool off, int side)
        {
            int i = v.Model.Lods[0].Find(name);
            if (i < 0 || v.Off[i] == off) return;
            if (off)
            {
                var d = Detach(v, i, v.World[i]);
                d.Thrown = true;
                Vector3 sideways = new Vector3(Mathf.Cos(v.Yaw), 0f, -Mathf.Sin(v.Yaw)) * side;
                d.Vel = sideways * 2.2f + Vector3.up * 1.2f;
                d.Spin = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)) * (side * 1.8f);
            }
            else
            {
                for (int k = v.Pieces.Count - 1; k >= 0; k--)
                    if (v.Pieces[k].Part == i && v.Pieces[k].Thrown) { debris.Remove(v.Pieces[k]); v.Pieces.RemoveAt(k); }
                v.Off[i] = false;
            }
        }

        Debris Detach(View v, int part, Matrix4x4 world)
        {
            var d = new Debris { Owner = v, Part = part, World = world };
            var parts = v.Model.Lods[0].Parts;
            // the children keep their pose relative to the piece
            for (int c = part + 1; c < parts.Count; c++)
            {
                int up = parts[c].Parent; bool under = false;
                for (int k = up; k >= 0; k = parts[k].Parent) if (k == part) { under = true; break; }
                if (under) d.Local[c] = PartLocal(v, parts[c]);
            }
            v.Off[part] = true;
            v.Pieces.Add(d); debris.Add(d);
            TrimDebris();
            return d;
        }

        /// <summary>Loose parts on the field at once; past it the oldest piece at rest is taken away (a track slid off is kept: it goes back when mended).</summary>
        public const int MaxLoose = 160;
        void TrimDebris()
        {
            while (debris.Count > MaxLoose)
            {
                int victim = -1;
                for (int k = 0; k < debris.Count; k++) if (debris[k].Resting && !debris[k].Thrown) { victim = k; break; }
                if (victim < 0) for (int k = 0; k < debris.Count; k++) if (!debris[k].Thrown) { victim = k; break; }
                if (victim < 0) return;
                var d = debris[victim];
                d.Owner?.Pieces.Remove(d);
                debris.RemoveAt(victim);
            }
        }

        static readonly Color Steel = new Color(0.38f, 0.39f, 0.36f);
        /// <summary>Plates and scrap off a hull: the armour that a round holed, that a leg tore away, that the ammunition threw (DebrisRenderer).</summary>
        static void Scrap(Vector3 at, int count, float speed, float size, float burn, float life, Vector3 lean, uint salt)
        {
            var d = DebrisRenderer.Instance;
            if (d == null || !d.Ready) return;
            d.Burst(DebrisRenderer.Piece.Plate, at, count, speed, size, Steel, life, burn, 1.4f, lean, salt);
        }

        void FlyDebris(float dt, TW.Sim.Match.MatchSim match)
        {
            foreach (var d in debris)
            {
                d.Burn = Mathf.Max(0f, d.Burn - dt * 0.03f);
                if (d.Resting) continue;
                var p = d.Owner.Model.Lods[0].Parts[d.Part];
                Vector3 pos = d.World.GetColumn(3);
                Quaternion rot = d.World.rotation;
                d.Vel += Vector3.down * 9.81f * dt;
                pos += d.Vel * dt;
                if (d.Spin.sqrMagnitude > 1e-6f) rot = Quaternion.AngleAxis(d.Spin.magnitude * Mathf.Rad2Deg * dt, d.Spin.normalized) * rot;
                Vector3 centre = pos + rot * p.Center;
                float ground = Ground(centre.x, centre.z), bottom = centre.y - Mathf.Min(p.Radius, 1.2f) * 0.45f;
                if (bottom < ground)
                {
                    pos.y += ground - bottom;
                    if (d.Vel.y < -2f && books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, new Vector3(centre.x, ground + 0.2f, centre.z), 1.6f, 1.3f, velocity: Vector3.up * 0.5f, grow: 1f, alpha: 0.7f);
                    d.Vel = new Vector3(d.Vel.x * 0.45f, -d.Vel.y * 0.25f, d.Vel.z * 0.45f);
                    d.Spin *= 0.5f;
                    if (d.Vel.magnitude < 0.6f) { d.Resting = true; d.Vel = Vector3.zero; d.Spin = Vector3.zero; }
                }
                d.World = Matrix4x4.TRS(pos, rot, Vector3.one);
            }
        }

        // ------------------------------------------------------------------ the end
        void Wreckify(View v, float now)
        {
            if (v.Dead) return;
            v.Dead = true; v.DiedAt = now; v.Scorch = Mathf.Max(v.Scorch, 0.85f); v.Hatch = 1f; v.Throttle = 0f;
            v.Burn = Mathf.Max(v.Burn, v.CookOff ? 1f : 0.5f);
            Pose(v, v.Model.Lods[0], v.World);
            if (v.CookOff)
            {
                // the rounds go: whatever sits on top goes up with them
                var parts = v.Model.Lods[0].Parts;
                for (int i = 0; i < parts.Count; i++)
                {
                    var r = parts[i].Role;
                    bool top = r == TankPartRole.Turret || r == TankPartRole.Cupola || r == TankPartRole.Horn || r == TankPartRole.Sponson || r == TankPartRole.Exhaust || (r == TankPartRole.Hatch && parts[i].Parent >= 0 && parts[parts[i].Parent].Role != TankPartRole.Turret);
                    if (!top || v.Off[i]) continue;
                    if (parts[i].Parent >= 0 && parts[parts[i].Parent].Role != TankPartRole.Hull) continue;   // goes with its parent
                    var d = Detach(v, i, v.World[i]);
                    Vector3 away = (Vector3)v.World[i].GetColumn(3) - v.Pos; away.y = 0f;
                    bool big = r == TankPartRole.Turret || r == TankPartRole.Cupola;
                    d.Vel = away.normalized * UnityEngine.Random.Range(1.5f, 5f) + Vector3.up * (big ? UnityEngine.Random.Range(9f, 13f) : UnityEngine.Random.Range(5f, 10f));
                    d.Spin = UnityEngine.Random.onUnitSphere * UnityEngine.Random.Range(2f, big ? 4f : 9f);
                    d.Burn = 1f;
                }
                for (int k = 0; k < 9; k++)
                    pops.Add(new Pop { Owner = v, At = now + UnityEngine.Random.Range(0.4f, 6f), Offset = new Vector3(UnityEngine.Random.Range(-1.2f, 1.2f), UnityEngine.Random.Range(1.2f, 2.6f), UnityEngine.Random.Range(-1.8f, 1.8f)), Size = UnityEngine.Random.Range(0.6f, 1.4f) });
                CameraShake.Add(v.Pos, 12f);
            }
            wrecks.Add(v);
        }

        void Drop(int k)
        {
            var v = wrecks[k];
            foreach (var p in v.Pieces) debris.Remove(p);
            wrecks.RemoveAt(k);
            // its prop is drawn as the stand-in again (the prop layer is in an assembly this one does not reference)
            if (v.Linked) foreach (var mb in FindObjectsByType<MonoBehaviour>(FindObjectsSortMode.None)) if (mb.GetType().Name == "BattlefieldProps") mb.SendMessage("Recompose", SendMessageOptions.DontRequireReceiver);
        }

        /// <summary>Returns false once an unlinked hull has sunk away.</summary>
        bool Smoulder(View v, float dt, float now)
        {
            float age = now - v.DiedAt;
            if (!v.Linked && age > UnlinkedSinkAfter)
            {
                // no wreck prop in the sim: nothing here blocks or gives cover, so the hull and its pieces settle out of sight
                float dy = dt * 0.2f;
                v.Heave.Value -= dy;
                foreach (var d in v.Pieces) if (d.Resting) d.World.m13 -= dy;
                if (age > UnlinkedSinkAfter + 18f) return false;
            }
            // the fire dies down, but embers glow dimly in the seams for three minutes: burnt metal, not a grey prop
            v.Burn = Mathf.MoveTowards(v.Burn, age < 180f ? 0.15f : 0f, dt / (v.CookOff ? 70f : 45f));
            v.Flash = Mathf.Max(0f, v.Flash - dt * 5f);
            Pose(v, v.Model.Lods[0], v.World);
            float fire = v.CookOff ? Mathf.Clamp01(1f - age / 40f) : Mathf.Clamp01(v.Fire - age / 30f);
            float smoke = Mathf.Clamp01(1f - age / 120f);
            Burning(v, now, fire, smoke);
            foreach (var d in v.Pieces)
                if (d.Burn > 0.3f)
                {
                    Vector3 at = d.World.MultiplyPoint3x4(v.Model.Lods[0].Parts[d.Part].Center);
                    flames.Add(new Flame { Foot = at, Width = 0.5f * d.Burn, Height = 1.0f * d.Burn, Phase = d.Part * 0.3f + v.Slot });
                }
            return true;
        }

        void RunPops(float now)
        {
            for (int k = pops.Count - 1; k >= 0; k--)
            {
                var p = pops[k];
                if (now < p.At) continue;
                pops.RemoveAt(k);
                if (books == null || !books.Ready) continue;
                Vector3 at = p.Owner.Pos + Quaternion.AngleAxis(p.Owner.Yaw * Mathf.Rad2Deg, Vector3.up) * p.Offset + Vector3.up * p.Owner.Heave.Value;
                books.Add(FlipbookFx.Book.Flash, at, 2.2f * p.Size, 0.12f, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 4f : 2.2f, pop: 0.5f);
                books.Add(FlipbookFx.Book.Star, at, 1.4f * p.Size, 0.1f, roll: UnityEngine.Random.value * 6.28f, glow: 2f);
                books.Add(FlipbookFx.Book.Smoke, at, 1.8f * p.Size, 3f, velocity: Vector3.up * 1.5f, grow: 1.6f, alpha: 0.7f);
                SceneHooks.Sparks?.Invoke(at, 6);
                p.Owner.Flash = 1f;
            }
        }

        // ------------------------------------------------------------------ events
        void OnSimEvent(SimEvent e)
        {
            if (Host == null || Host.Local == null) return;
            float now = Time.time;
            if (e.Type == SimEventType.Explosion) { Blasted(e); return; }
            if (e.Type == SimEventType.PropChanged)
            {
                // a wreck prop for a tank that just died (dir.x = slot + 1): tie it to the newest unlinked hull of that slot
                if (e.B != (int)TW.Sim.Terrain.PropKind.Wreck || e.Dir.x < 1f) return;
                int slot = (int)e.Dir.x - 1;
                for (int k = wrecks.Count - 1; k >= 0; k--)
                    if (!wrecks[k].Linked && wrecks[k].Slot == slot) { wrecks[k].Linked = true; wrecks[k].PropPos = (Vector3)e.Pos; break; }
                return;
            }
            views.TryGetValue(e.A, out var v);
            switch (e.Type)
            {
                case SimEventType.VehicleFired:
                {
                    if (v == null || v.Dead || e.B < 0 || e.B > 1) break;
                    v.Recoil[e.B] = 1f;
                    Vector3 muzzle = MuzzleWorld(v, e.B, out Vector3 dir);
                    // the hull answers the shot: rocks back along the barrel
                    Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
                    v.Pitch.Velocity += Vector3.Dot(dir, fwd) * 0.35f;
                    v.Roll.Velocity -= Vector3.Dot(dir, right) * 0.35f;
                    CameraShake.Add(muzzle, 4f);
                    SceneHooks.Flash?.Invoke(muzzle + dir * 1.2f, new Color(1f, 0.72f, 0.38f), 34f, 14f, 0.16f);   // the gun lights the ground in front of it
                    if (books == null || !books.Ready) break;
                    var cam = Camera.main;
                    float roll = cam != null ? FlipbookFx.ScreenRoll(cam, dir) : 0f;
                    books.Add(FlipbookFx.Book.Muzzle, muzzle + dir * 0.7f, 1.8f, 0.12f, roll: roll, glow: SceneMood.Night ? 3f : 1.8f);
                    books.Add(FlipbookFx.Book.Flash, muzzle + dir * 0.4f, 3.2f, 0.1f, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 4f : 2f, pop: 0.5f);
                    for (int k = 0; k < 3; k++)
                        books.Add(FlipbookFx.Book.Smoke, muzzle + dir * (0.6f + k * 0.7f), 1.2f + k * 0.4f, 2.5f + k * 0.5f, (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: dir * (2.5f - k * 0.6f) + Vector3.up * 0.5f, grow: 1.6f, alpha: 0.55f, delay: k * 0.03f);
                    float g = Ground(muzzle.x, muzzle.z);
                    if (muzzle.y - g < 3.5f)   // the blast lifts the dust under the muzzle
                        books.Add(FlipbookFx.Book.Wings, new Vector3(muzzle.x, g, muzzle.z) + dir * 1.5f, 3.5f, 0.8f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.4f, alpha: 0.5f);
                    // an armour-piercing round that missed kicks up the ground where it went
                    if (e.Scalar < 0.5f && !VehicleNear(e.Pos))
                    {
                        Vector3 at = (Vector3)e.Pos; at.y = Ground(at.x, at.z);
                        float delay = Vector3.Distance(muzzle, at) / 700f;
                        books.Add(FlipbookFx.Book.Spurt, at, 1.6f, 0.6f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, delay: delay);
                        books.Add(FlipbookFx.Book.Puff, at + Vector3.up * 0.3f, 1.8f, 1.2f, grow: 1f, alpha: 0.6f, delay: delay);
                    }
                    break;
                }
                case SimEventType.VehicleArmourHit:
                {
                    if (v == null || v.Dead) break;
                    v.Flash = 1f;
                    Vector3 dir = new Vector3(e.Dir.x, e.Dir.y, e.Dir.z);
                    if (dir.sqrMagnitude < 1e-4f) dir = Vector3.down;
                    dir.Normalize();
                    Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
                    Vector3 centre = v.Pos + Vector3.up * (v.Heave.Value + 1.6f);
                    Vector3 at = centre - new Vector3(dir.x, 0f, dir.z).normalized * (v.Model.HalfLength * 0.55f) + Vector3.up * UnityEngine.Random.Range(-0.4f, 0.6f);
                    if (dir.y < -0.9f) at = centre + Vector3.up * 1.2f;
                    bool holed = e.Scalar > 0f;
                    v.Pitch.Velocity += Vector3.Dot(dir, fwd) * (holed ? 0.5f : 0.25f);
                    v.Roll.Velocity -= Vector3.Dot(dir, right) * (holed ? 0.5f : 0.25f);
                    // the strike lights the hull and the ground by it, and is felt: a round through the plate more than one off it
                    SceneHooks.Flash?.Invoke(at, holed ? new Color(1f, 0.78f, 0.48f) : new Color(1f, 0.9f, 0.72f), holed ? 22f : 12f, holed ? 9f : 6f, 0.12f);
                    CameraShake.Add(at, holed ? 3f : 1.5f);
                    if (books != null && books.Ready)
                    {
                        books.Add(FlipbookFx.Book.Star, at, holed ? 2.2f : 1.6f, 0.09f, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 3.5f : 2f);
                        if (holed)
                        {
                            books.Add(FlipbookFx.Book.Flash, at, 2.4f, 0.1f, roll: UnityEngine.Random.value * 6.28f, glow: 2f, pop: 0.4f);
                            books.Add(FlipbookFx.Book.Smoke, at, 1.4f, 2.2f, velocity: -dir * 0.8f + Vector3.up * 0.8f, grow: 1.4f, alpha: 0.7f);
                        }
                        else books.Add(FlipbookFx.Book.Star, at + Vector3.Reflect(dir, (at - centre).normalized) * 0.6f, 1.1f, 0.12f, velocity: Vector3.Reflect(dir, (at - centre).normalized) * 25f, roll: UnityEngine.Random.value * 6.28f, glow: 2f);
                    }
                    SceneHooks.Sparks?.Invoke(at, holed ? 8 : 5);
                    if (holed && UnityEngine.Random.value < 0.25f) ShearHorn(v, at);
                    if (holed) Scrap(at, 3, 7f, 0.22f, 0.7f, 40f, -dir * 0.6f, e.Tick + (uint)e.A);   // the plate the round went through, in pieces
                    break;
                }
                case SimEventType.VehicleOnFire:
                    if (v != null && e.B == 1 && books != null && books.Ready)
                    {
                        var at = SocketWorld(v, "Socket_Fire0", out _);
                        books.Add(FlipbookFx.Book.Flash, at, 2.6f, 0.15f, glow: 2.4f, pop: 0.4f);
                        books.Add(FlipbookFx.Book.Smoke, at + Vector3.up, 2.4f, 4f, velocity: Vector3.up * 2f, grow: 1.8f, alpha: 0.8f);
                    }
                    break;
                case SimEventType.VehicleStalled:
                    if (v != null && books != null && books.Ready)
                        for (int k = 0; k < 2; k++)
                        {
                            var at = SocketWorld(v, "Socket_Exhaust" + k, out bool ok);
                            if (ok) books.Add(FlipbookFx.Book.Smoke, at + Vector3.up * 0.3f, e.B == 1 ? 2.2f : 1.4f, 3f, velocity: Vector3.up * 1.6f, grow: 1.6f, alpha: e.B == 1 ? 0.95f : 0.6f);
                        }
                    break;
                case SimEventType.VehicleBailedOut:
                    if (v != null && books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, SocketWorld(v, "Socket_Crew", out _), 1.4f, 1f, velocity: Vector3.up, grow: 1f, alpha: 0.6f);
                    break;
                case SimEventType.VehicleCookOff:
                    if (v != null)
                    {
                        v.CookOff = true;
                        var at = v.Pos + Vector3.up * (v.Heave.Value + 2f);
                        for (int k = 0; k < 6; k++) flames.Add(new Flame { Foot = at + UnityEngine.Random.insideUnitSphere * 1.5f, Width = 3.5f, Height = 6f, Phase = k });
                        Scrap(at, 14, 13f, 0.35f, 1f, 60f, default, e.Tick + (uint)e.A);   // the hull's plates go up with the rounds, burning as they come down
                    }
                    break;
                case SimEventType.VehicleDestroyed:
                    if (v != null) { Wreckify(v, now); views.Remove(e.A); }
                    break;
                case SimEventType.VehicleBogged:
                    if (v != null && e.B == 1 && books != null && books.Ready)
                        for (int side = 0; side < 2; side++)
                            books.Add(FlipbookFx.Book.Spurt, SocketWorld(v, side == 0 ? "Socket_Dust_L" : "Socket_Dust_R", out _), 1.8f, 0.7f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored);
                    break;
                case SimEventType.VehicleDitched:
                    if (v != null && books != null && books.Ready)
                    {
                        Vector3 nose = v.Pos + new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)) * v.Model.HalfLength; nose.y = Ground(nose.x, nose.z);
                        books.Add(FlipbookFx.Book.Column, nose, e.B >= 0 ? 3.2f : 2f, 1.3f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.2f, alpha: 0.9f);
                        books.Add(FlipbookFx.Book.Puff, nose + Vector3.up * 0.5f, 3f, 2f, grow: 1.4f, alpha: 0.7f);
                        if (e.B >= 0) CameraShake.Add(nose, 5f);
                    }
                    break;
                case SimEventType.VehicleCrushed:
                    if (books != null && books.Ready)
                    {
                        Vector3 at = (Vector3)e.Pos; at.y = Ground(at.x, at.z) + 0.3f;
                        books.Add(FlipbookFx.Book.Puff, at, e.B == 1 ? 3f : 1.6f, 1.4f, velocity: Vector3.up * 0.6f, grow: 1.2f, alpha: 0.6f);
                    }
                    break;
                case SimEventType.VehicleClawed:
                    // the claw closes: it swings in, and whatever it caught throws sparks and grit
                    if (v != null) v.Claw = 1f;
                    if (SceneHooks.Sparks != null) SceneHooks.Sparks((Vector3)e.Pos + Vector3.up * 0.5f, 7);
                    if (books != null && books.Ready)
                    {
                        Vector3 at = (Vector3)e.Pos; at.y = Ground(at.x, at.z) + 0.5f;
                        books.Add(FlipbookFx.Book.Puff, at, 1.3f, 1.0f, velocity: Vector3.up * 0.5f, grow: 1.15f, alpha: 0.5f);
                    }
                    break;
                case SimEventType.VehicleLegLost:
                    // a leg comes off: the throw itself is done from the module state (Animate), this is the noise of it
                    if (SceneHooks.Sparks != null) SceneHooks.Sparks((Vector3)e.Pos + Vector3.up * 0.8f, 12);
                    if (books != null && books.Ready)
                    {
                        Vector3 at = (Vector3)e.Pos; at.y = Ground(at.x, at.z) + 0.7f;
                        books.Add(FlipbookFx.Book.Puff, at, 2.2f, 1.2f, velocity: Vector3.up * 0.8f, grow: 1.3f, alpha: 0.55f);
                    }
                    { Vector3 at = (Vector3)e.Pos; at.y = Ground(at.x, at.z) + 1.0f; Scrap(at, 4, 6f, 0.28f, 0.4f, 40f, default, e.Tick + (uint)e.B); }   // the joint's plates and pins
                    break;
                case SimEventType.VehicleRepaired:
                    if (v != null && books != null && books.Ready) books.Add(FlipbookFx.Book.Star, v.Pos + Vector3.up * (v.Heave.Value + 1.2f), 0.8f, 0.2f, glow: 1.5f);
                    break;
            }
        }

        /// <summary>
        /// A burst near a hull rocks it on its springs and lifts it (the sim only damages it; the push is ours): tipped away
        /// from the burst, the near side up, harder the nearer and the bigger the shell; one right under it heaves it
        /// straight up. The earth the burst threw comes down on the deck a moment later. Walkers ride the same springs.
        /// </summary>
        void Blasted(SimEvent e)
        {
            Vector3 at = (Vector3)e.Pos;
            float r = Mathf.Max(1f, e.Scalar), size = Mathf.Clamp(r / 6f, 0.5f, 1.6f);
            foreach (var v in views.Values)
            {
                if (v == null || v.Dead || v.Model == null) continue;
                Vector3 off = v.Pos - at; off.y = 0f;
                float d = off.magnitude, reach = r + 4f + v.Model.HalfLength;
                if (d >= reach) continue;
                float near = 1f - d / reach, push = near * near * size;
                Vector3 away = d > 0.8f ? off / d : Vector3.zero;
                Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
                v.Pitch.Velocity -= Vector3.Dot(away, fwd) * 1.3f * push;   // in front of the nose: it rocks back
                v.Roll.Velocity += Vector3.Dot(away, right) * 1.3f * push;  // on the right: the right side comes up
                v.Heave.Velocity += 4f * push;
                if (books != null && books.Ready && near > 0.35f)
                {
                    Vector3 deck = v.Pos + Vector3.up * (v.Heave.Value + v.Model.Height);   // on the deck
                    books.Add(FlipbookFx.Book.Puff, deck, 2.2f + 0.4f * v.Model.HalfLength, 1.4f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.3f - away * 0.6f, grow: 0.8f, alpha: 0.6f, pop: 0.4f, delay: 0.35f + 0.3f * (1f - near));
                }
            }
        }

        bool VehicleNear(float3 p)
        {
            foreach (var v in views.Values) if ((v.Pos.x - p.x) * (v.Pos.x - p.x) + (v.Pos.z - p.z) * (v.Pos.z - p.z) < 16f) return true;
            return false;
        }

        void ShearHorn(View v, Vector3 at)
        {
            var parts = v.Model.Lods[0].Parts;
            int best = -1; float bestD = 3.5f * 3.5f;
            for (int i = 0; i < parts.Count; i++)
            {
                if (parts[i].Role != TankPartRole.Horn || v.Off[i]) continue;
                float d = ((Vector3)v.World[i].GetColumn(3) - at).sqrMagnitude;
                if (d < bestD) { bestD = d; best = i; }
            }
            if (best < 0) return;
            var piece = Detach(v, best, v.World[best]);
            Vector3 away = (Vector3)v.World[best].GetColumn(3) - (v.Pos + Vector3.up * (v.Heave.Value + 1.5f));
            piece.Vel = away.normalized * 4f + Vector3.up * 4f;
            piece.Spin = UnityEngine.Random.onUnitSphere * 10f;
        }

        // ------------------------------------------------------------------ drawing
        Batch BatchFor(Mesh mesh, Material mat)
        {
            if (!batches.TryGetValue(mesh, out var b)) { b = new Batch { Mesh = mesh, Material = mat }; batches[mesh] = b; }
            return b;
        }

        void Queue(Mesh mesh, Material mat, Matrix4x4 m, float tread, Vector4 damage, Vector4 tint, Vector4 team = default)
        {
            var b = BatchFor(mesh, mat);
            if (b.Count >= b.M.Length) return;
            b.M[b.Count] = m; b.Tread[b.Count] = tread; b.Damage[b.Count] = damage; b.Tint[b.Count] = tint; b.Team[b.Count] = team;
            b.Count++;
        }

        void BuildDisc()
        {
            var shader = Shader.Find("TW/TankDisc (URP)");
            if (shader == null) return;
            discMat = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, name = "Tank disc" };
            discProps = new MaterialPropertyBlock();
            discMesh = new Mesh { name = "Tank disc", hideFlags = HideFlags.HideAndDontSave };
            discMesh.SetVertices(new[] { new Vector3(-0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, 0.5f), new Vector3(-0.5f, 0f, 0.5f) });
            discMesh.SetUVs(0, new[] { new Vector2(0f, 0f), new Vector2(1f, 0f), new Vector2(1f, 1f), new Vector2(0f, 1f) });
            discMesh.SetTriangles(new[] { 0, 2, 1, 0, 3, 2 }, 0);
            discMesh.bounds = new Bounds(Vector3.zero, new Vector3(1f, 0.1f, 1f));
        }

        /// <summary>The contact blob and side ring under a tank: the footprint plus a margin, on the ground, turned with the hull.</summary>
        void QueueDisc(View v)
        {
            if (discMat == null || discCount >= discM.Length) return;
            var m = v.Model;
            // a walker has no tracks to measure, so its ring comes from its own footprint (VehicleProfile) instead of
            // the model's default gauge, which is a tank's and swallows a crab
            float halfW = m.HalfGauge, halfL = m.HalfLength;
            if (VehicleArchetype.IsWalker(v.Archetype))
            {
                var prof = TW.Sim.Nav.VehicleProfile.ForArchetype(v.Archetype);
                halfW = prof.HalfWidth * 0.72f; halfL = prof.HalfLength * 0.72f;
            }
            float w = (halfW + 0.9f) * 2f / 0.72f, l = (halfL + 0.8f) * 2f / 0.72f;   // the ring sits at 0.72 of the quad
            // on the ground the tracks settle on (over a trench that is the lip, not the bottom of the hole between)
            var at = new Vector3(v.Pos.x, Mathf.Max(Ground(v.Pos.x, v.Pos.z), v.Heave.Value - 0.3f) + 0.12f, v.Pos.z);
            discM[discCount] = Matrix4x4.TRS(at, Quaternion.AngleAxis(v.Yaw * Mathf.Rad2Deg, Vector3.up), new Vector3(w, 1f, l));
            var c = v.Team == 1 ? TeamB : TeamA;
            discC[discCount] = new Vector4(c.r, c.g, c.b, v.Dead ? 0f : 1f);
            discCount++;
        }

        readonly Matrix4x4[] lodWorld = new Matrix4x4[64];

        void Draw()
        {
            var cam = Camera.main;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            foreach (var v in views.Values) DrawTank(v, eye);
            foreach (var v in wrecks) DrawTank(v, eye);
            foreach (var d in debris)
            {
                try { DrawDebris(d); }   // one bad piece must not stop every tank being drawn
                catch (System.Exception ex) { if (!debrisLogged) { debrisLogged = true; Debug.LogError($"TankRenderer debris part {d.Part} owner {(d.Owner == null ? "null" : d.Owner.Slot.ToString())} model {(d.Owner?.Model == null ? "null" : "ok")} local {d.Local?.Count}: {ex}"); } }
            }
            DrawRest();
        }

        bool debrisLogged;

        void DrawDebris(Debris d)
        {
            {
                var v = d.Owner; var parts = v.Model.Lods[0].Parts;
                var dmg = new Vector4(Mathf.Max(v.Scorch, d.Burn > 0f ? 0.9f : v.Scorch), Mathf.Max(d.Burn, v.Burn * 0.5f), v.Flash, 0f);
                var tint = v.Team == 1 ? TeamTintB : new Vector4(1f, 1f, 1f, 0f);
                Queue(parts[d.Part].Mesh, MaterialFor(v.Archetype, 0), d.World, parts[d.Part].Role == TankPartRole.Track ? (parts[d.Part].Side < 0 ? v.TreadL : v.TreadR) : 0f, dmg, tint, TeamBand(v, parts[d.Part].Role));
                // what hangs off the piece rides with it
                for (int c = d.Part + 1; c < parts.Count; c++)
                {
                    if (!d.Local.TryGetValue(c, out var local)) continue;
                    lodWorld[c] = (parts[c].Parent == d.Part ? d.World : lodWorld[parts[c].Parent]) * local;
                    Queue(parts[c].Mesh, MaterialFor(v.Archetype, 0), lodWorld[c], 0f, dmg, tint, TeamBand(v, parts[c].Role));
                }
            }
        }

        void DrawRest()
        {
            discCount = 0;
            foreach (var v in views.Values) if (!v.Ditched) QueueDisc(v);
            foreach (var v in wrecks) QueueDisc(v);
            if (discCount > 0 && discProps != null)
            {
                discProps.SetVectorArray(ColorId, discC);
                var dp = new RenderParams(discMat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = false, matProps = discProps };
                Graphics.RenderMeshInstanced(dp, discMesh, 0, discM, discCount);
            }
            foreach (var b in batches.Values)
            {
                if (b.Count == 0) continue;
                b.Props.SetFloatArray(TreadId, b.Tread);
                b.Props.SetVectorArray(DamageId, b.Damage);
                b.Props.SetVectorArray(TintId, b.Tint);
                b.Props.SetVectorArray(TeamId, b.Team);
                var rp = new RenderParams(b.Material) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true, matProps = b.Props };
                Graphics.RenderMeshInstanced(rp, b.Mesh, 0, b.M, b.Count);
                b.Count = 0;
            }
        }

        void DrawTank(View v, Vector3 eye)
        {
            int lod = (v.Pos - eye).sqrMagnitude > LodDistance * LodDistance && v.Model.Lods[1] != null ? 1 : 0;
            var l = v.Model.Lods[lod];
            Matrix4x4[] world = v.World;
            if (lod == 1) { if (l.Parts.Count > lodWorld.Length) return; Pose(v, l, lodWorld); world = lodWorld; }
            var damage = new Vector4(v.Scorch, v.Burn, v.Flash, v.Model.Archetype == VehicleArchetype.Maw ? v.Furnace : 0f);
            var tint = v.Team == 1 ? TeamTintB : new Vector4(1f, 1f, 1f, 0f);
            for (int i = 0; i < l.Parts.Count; i++)
            {
                if (IsOff(v, l, i)) continue;
                var p = l.Parts[i];
                float tread = p.Role == TankPartRole.Track ? (p.Side < 0 ? v.TreadL : v.TreadR) : 0f;
                Queue(p.Mesh, MaterialFor(v.Archetype, lod), world[i], tread, damage, tint, TeamBand(v, p.Role));
            }
        }

        // ------------------------------------------------------------------ flames
        void BuildFlames()
        {
            var shader = Shader.Find("TW/Flame (URP)");
            if (shader == null) return;
            const int n = 64;
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float c = Mathf.PerlinNoise(x * 4f / n, z * 4f / n) * 0.55f + Mathf.PerlinNoise(x * 9f / n + 3.7f, z * 9f / n + 1.3f) * 0.45f;
                byte b = (byte)(Mathf.Clamp01(c) * 255f); px[z * n + x] = new Color32(b, b, b, 255);
            }
            // Perlin is not tileable; mirror the edges in so the scrolling read does not show a seam
            for (int z = 0; z < n; z++) for (int x = n / 2; x < n; x++) px[z * n + x] = px[z * n + (n - 1 - x)];
            for (int z = n / 2; z < n; z++) for (int x = 0; x < n; x++) px[z * n + x] = px[(n - 1 - z) * n + x];
            var noise = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Tank flame noise", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            noise.SetPixels32(px); noise.Apply(true, true);
            flameMat = new Material(shader) { hideFlags = HideFlags.HideAndDontSave };
            flameMat.SetTexture("_Noise", noise);
            flameMat.SetFloat("_Strength", 2.8f);
            flameMesh = new Mesh { name = "Tank flames", hideFlags = HideFlags.HideAndDontSave };
            flameMesh.MarkDynamic();
        }

        void DrawFlames()
        {
            if (flameMat == null || flameMesh == null) { flames.Clear(); return; }
            fPos.Clear(); fCorner.Clear(); fShape.Clear(); fTris.Clear();
            for (int g = 0; g < flames.Count && g < 256; g++)
            {
                var f = flames[g];
                int v0 = fPos.Count;
                for (int k = 0; k < 4; k++) { fPos.Add(f.Foot); fCorner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f)); fShape.Add(new Vector4(f.Width, f.Height, f.Phase, 0f)); }
                fTris.Add(v0); fTris.Add(v0 + 2); fTris.Add(v0 + 1); fTris.Add(v0); fTris.Add(v0 + 3); fTris.Add(v0 + 2);
            }
            flames.Clear();
            flameMesh.Clear();
            if (fPos.Count == 0) return;
            flameMesh.SetVertices(fPos); flameMesh.SetUVs(0, fCorner); flameMesh.SetUVs(1, fShape); flameMesh.SetTriangles(fTris, 0);
            flameMesh.bounds = Everywhere;
            Graphics.RenderMesh(new RenderParams(flameMat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = false }, flameMesh, 0, Matrix4x4.identity);
        }

        static float LerpAngle(float a, float b, float t) => a + Mathf.DeltaAngle(a * Mathf.Rad2Deg, b * Mathf.Rad2Deg) * Mathf.Deg2Rad * t;
    }
}
