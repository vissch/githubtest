// Phase: riders prototype (presentation only) — depends on: RiderSeats, VATRenderer.Extras, TankModel, WalkerGait
// Infantry riding on the walkers, and the walkers' sizes, as a LOOK first: nothing here touches the simulation.
//
// A rider goes through a small state machine, every position worked out afresh each frame from the posed body part,
// so he follows the machine's walk, tilt and heave at every step:
//   Approach  runs from where he stood to the foot of his seat's way up (RiderSeats.Seat.Foot), chasing it if it moves
//   Climb     up the hull at the foot, face to the plating, to the deck's edge
//   Over      hauls himself over the edge, then crouch-walks to his seat
//   Seated    kneels; turns to and fires at the machine's own target when it has one; flinches when the hull is rocked
//   ToEdge    stands and crouch-walks back to the edge          (Dismount)
//   Descend   climbs down the hull to a drop's height
//   Drop      lets go and falls the last of it, landing clear of the body
//   Thrown    the machine was destroyed under him: pitched off on an arc (the unlucky ones die in the air, as bodies)
//   Down      on the ground: lands (or gets up), then is handed over - RiderLanded - and drawn a moment longer
// Riders have no sim state of their own. A rider taken from a real man (BoardFrom) carries that man's slot, which
// VATRenderer.Hide keeps from being drawn twice; whoever listens to RiderLanded puts the man back (RiderLab does).
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        public enum RiderState : byte { Approach, Climb, Over, Seated, ToEdge, Descend, Drop, Thrown, Down }

        public sealed class Rider
        {
            public byte Team, Archetype; public int Seed, Seat = -1, SimSlot = -1;
            public RiderState State; public float StateAt;
            public Vector3 Pos, From, Land; public float Yaw, Flight, Top, Up;
            public float Phase, ShotAt = -1f, NextShot, FlinchAt = -10f, LastHeave;
            public int Aim = -1;   // the sim unit he last fired at
            public int Machine = -1;   // the slot of the machine he rides
            public bool Lying;   // thrown down: lies a moment before he gets up
            public float DuckUntil = -10f;   // a barrel is coming round over him
            public bool Flat; public float FlatAt = -10f, RiseAt = -10f;   // lying flat under it, since; getting up, since
            public float SweptUntil = -10f;   // the barrel over him is MOVING (the pip only turns amber for that)
            public bool Dismounting, Handed, Hurt, Flashed;
            // the clip, and the one it is cross-fading out of
            public Clip Clip = Clip.Run, Prev = Clip.Run; public float ClipAt, PrevT, SwitchAt = -10f, Rate = 1f;
        }

        /// <summary>Per-crab size on top of VehicleSize.Walker, by archetype - Pincer; 1 = as shipped. Presentation only.</summary>
        public static readonly float[] WalkerSizeFactor = { 1f, 1f, 1f, 1f, 1f, 1f };
        /// <summary>Riders fire from the shell at whatever the machine's own guns are after.</summary>
        public bool RidersFire = true;
        public Clip RiderIdle = Clip.KneelIdle, RiderShot = Clip.FireKneel;
        /// <summary>What a rider does while one of his machine's barrels swings over him (seats under the guns:
        /// RiderSeats.ClearOfGuns off): he goes flat. Measured on the Pincer, its barrels pass 0.26 to 1.39 m above the
        /// deck where men sit, and a kneeling man is 1.35 m before he grows with the zoom; Duck (a standing duck) put
        /// his head higher, not lower. Down, lying, and up again.</summary>
        public Clip RiderDuckDown = Clip.KneelToProne, RiderDuck = Clip.ProneIdle, RiderDuckUp = Clip.ProneToKneel, RiderShotProne = Clip.FireProne;
        [Tooltip("Seconds a rider stays flat after the barrel has passed him.")]
        public float RiderStayFlat = 0.8f;
        readonly float[] gunYaw = new float[8];
        readonly bool[] gunSweeping = new bool[8];
        readonly Dictionary<int, float[]> lastGunYaw = new Dictionary<int, float[]>();
        [Tooltip("Radians a second: a turret turning faster than this is sweeping (its riders' pips go amber).")]
        public float SweepSpeed = 0.35f;   // 20 degrees a second
        const float AmberAhead = 15f * Mathf.Deg2Rad, AmberHold = 20f * Mathf.Deg2Rad;   // on at 15, off past 20: the row flickered (critic r5)
        /// <summary>Riders ducking under a barrel this frame (the lab reads it).</summary>
        public int RidersDucking { get; private set; }
        public float RiderShotEvery = 2.6f;
        [Tooltip("Metres a second: running to the machine, up its side, and across its deck.")]
        public float RiderRun = 3.6f, RiderClimb = 3.0f, RiderDeck = 1.5f;
        /// <summary>Metres a climber keeps off the hull: on the plating itself his legs went through the Pincer's.</summary>
        public float ClimbStandOff = 0.3f;
        /// <summary>How much a rider may grow with the zoom. Men on the ground grow up to VATRenderer.MaxGrow (4x) so they
        /// stay readable from far out; on a deck whose seats are a man's width apart that piles eight giants through each
        /// other and the hull, so riders stop at this (their machine is big enough to find them by).</summary>
        public float RiderMaxGrow = 2.2f;
        /// <summary>Past this growth only every other rider is drawn (the seats are handed out spread, so the half left is
        /// still spread over the deck): at the standard view a man drawn at his true size vanished against the hull, and
        /// eight drawn large overlapped. Four readable men say "carrying infantry" better than eight blobs.</summary>
        public float RiderThinAbove = 1.4f;
        [Tooltip("Share of the riders killed when their machine is destroyed under them; the rest are thrown clear.")]
        public float RiderDeathShare = 0.5f;
        [Tooltip("Seconds a man thrown off a dying machine lies where he fell before he gets up.")]
        public float ThrownLie = 1.2f;
        /// <summary>A rider is on his feet on the ground again: where, facing, and who he was. Listeners put the man back in
        /// the sim (RiderLab); with nobody listening he stands a moment and is gone.</summary>
        public event System.Action<Rider, Vector3, float> RiderLanded;

        const float Gravity = 14f, BlendSeconds = 0.2f, DropHeight = 1.4f, StandAfter = 0.9f;

        readonly Dictionary<int, List<Rider>> riders = new Dictionary<int, List<Rider>>();
        readonly List<Rider> loose = new List<Rider>();   // off the machine: thrown, dropped, landing
        readonly Dictionary<TankModel, RiderSeats> seatsOf = new Dictionary<TankModel, RiderSeats>();
        VATRenderer vat;
        readonly VatInstance[] riderInst = new VatInstance[VATRenderer.MaxExtras];
        readonly byte[] riderFig = new byte[VATRenderer.MaxExtras];
        readonly List<int> riderGone = new List<int>();

        public int RiderCount(int slot) => riders.TryGetValue(slot, out var l) ? l.Count : 0;
        public int RiderTotal { get { int n = loose.Count; foreach (var l in riders.Values) n += l.Count; return n; } }
        /// <summary>The slots of the machines that have men on (or on their way onto) them.</summary>
        public IEnumerable<int> RiddenSlots => riders.Keys;
        public IEnumerable<Rider> RidersOf(int slot) => riders.TryGetValue(slot, out var l) ? l : (IEnumerable<Rider>)System.Array.Empty<Rider>();

        RiderSeats SeatsFor(TankModel model)
        {
            if (model == null) return null;
            if (!seatsOf.TryGetValue(model, out var s)) { s = RiderSeats.For(model); seatsOf[model] = s; }
            return s;
        }

        /// <summary>Solve every machine's seats again (after RiderSeats.ClearOfGuns changes).</summary>
        public void ForgetSeats() => seatsOf.Clear();

        /// <summary>How many men the machine in this slot has seats for (0 for a tank or an empty slot).</summary>
        public int SeatCount(int slot)
        {
            var w = Host?.Local?.World;
            if (w == null || slot < 0 || slot >= w.HighWater || !VehicleArchetype.IsArmoured(w.Archetype[slot])) return 0;   // walkers and tanks
            var s = SeatsFor(ModelFor(w.Archetype[slot]));
            return s != null ? s.Seats.Count : 0;
        }

        /// <summary>Put `count` men of the walker's own team on its back at once, no climb (for stills). Returns how many ride.</summary>
        public int Board(int slot, int count, int archetype = 0)
        {
            if (!CanBoard(slot, out var list, out int seats)) return 0;
            float now = Time.time;
            while (count-- > 0 && list.Count < seats)
            {
                var r = NewRider(slot, list, archetype, -1, now);
                r.State = RiderState.Seated; Play(r, RiderIdle, now); r.SwitchAt = -10f;
                list.Add(r);
            }
            return list.Count;
        }

        /// <summary>Men run in from these places (their yaw as well) and climb aboard. `simSlots` (or null) are the sim men
        /// they are; those are hidden while they ride. Returns how many are riding or on their way.</summary>
        public int BoardFrom(int slot, Vector3[] from, float[] yaw, int[] simSlots, int archetype = 0)
        {
            if (!CanBoard(slot, out var list, out int seats)) return 0;
            float now = Time.time;
            for (int i = 0; i < from.Length && list.Count < seats; i++)
            {
                var r = NewRider(slot, list, archetype, simSlots != null && i < simSlots.Length ? simSlots[i] : -1, now);
                r.Pos = from[i]; r.Yaw = yaw != null && i < yaw.Length ? yaw[i] : 0f;
                r.State = RiderState.Approach; r.StateAt = now + i * 0.32f;   // one after another, not in step (at 0.18 s five men arrived as one blob)
                r.Clip = r.Prev = Clip.Idle; r.ClipAt = now;
                list.Add(r);
                if (r.SimSlot >= 0 && Vat() != null) vat.Hide(r.SimSlot, true);
            }
            return list.Count;
        }

        /// <summary>The last `count` riders climb down and drop off. Returns how many stay aboard.</summary>
        public int Dismount(int slot, int count = int.MaxValue)
        {
            if (!riders.TryGetValue(slot, out var list)) return 0;
            float now = Time.time; int left = list.Count;
            for (int k = list.Count - 1; k >= 0 && count > 0; k--)
            {
                var r = list[k];
                if (r.Dismounting) continue;
                r.Dismounting = true; count--; left--;
                // stagger them, or they all stand up at once like a drill
                if (r.State == RiderState.Seated) { r.State = RiderState.ToEdge; r.StateAt = now + (list.Count - 1 - k) * 0.35f; }
            }
            return left;
        }

        /// <summary>Remove riders at once, no transition (and give their men back).</summary>
        public int Unboard(int slot, int count = int.MaxValue)
        {
            if (!riders.TryGetValue(slot, out var list)) return 0;
            while (count-- > 0 && list.Count > 0) { var r = list[list.Count - 1]; list.RemoveAt(list.Count - 1); Release(r); }
            if (list.Count == 0) riders.Remove(slot);
            return list.Count;
        }

        public void UnboardAll() { foreach (var l in riders.Values) foreach (var r in l) Release(r); riders.Clear(); foreach (var r in loose) Release(r); loose.Clear(); }

        void Release(Rider r) { if (r.SimSlot >= 0 && Vat() != null) vat.Hide(r.SimSlot, false); }

        bool CanBoard(int slot, out List<Rider> list, out int seats)
        {
            list = null; seats = 0;
            var w = Host?.Local?.World;
            if (w == null || slot < 0 || slot >= w.HighWater || !w.IsAlive(slot) || !VehicleArchetype.IsArmoured(w.Archetype[slot])) return false;
            seats = SeatCount(slot);
            if (!riders.TryGetValue(slot, out list)) { list = new List<Rider>(); riders[slot] = list; }
            return seats > 0;
        }

        Rider NewRider(int slot, List<Rider> list, int archetype, int simSlot, float now)
        {
            var w = Host.Local.World;
            int seat = 0;
            for (; seat < MaxSeatIndex; seat++) { bool taken = false; foreach (var o in list) if (o.Seat == seat) { taken = true; break; } if (!taken) break; }
            int seed = slot * 131 + seat * 17 + 7 + (simSlot >= 0 ? simSlot * 3 : 0);
            return new Rider { Machine = slot, Team = w.Team[slot], Archetype = (byte)archetype, Seed = seed, Seat = seat, SimSlot = simSlot, Phase = Hash(seed) * 3f, NextShot = NextVolley(slot, now + 1f) + Hash(seed + 3) * VolleySpread, StateAt = now, ClipAt = now };
        }
        const int MaxSeatIndex = RiderSeats.MaxSeats;

        /// <summary>The size a crab is drawn at now, as a multiple of the sculpt (VehicleSize.Walker * its factor).</summary>
        public static float SizeOf(int archetype)
        {
            int c = archetype - VehicleArchetype.Pincer;
            return c >= 0 && c < WalkerSizeFactor.Length ? VehicleSize.Walker * WalkerSizeFactor[c] : 0f;
        }

        /// <summary>
        /// Rebuild one crab at VehicleSize.Walker * factor and re-seat every machine of that kind on the field. The
        /// gait is replanted (its rig numbers come off the model), so the machine takes a step or two to settle. Riders
        /// without a seat on the new size climb down.
        /// </summary>
        public bool Resize(int archetype, float factor)
        {
            int c = archetype - VehicleArchetype.Pincer;
            if (c < 0 || c >= CrabNames.Length || factor <= 0.05f) return false;
            var model = TankModel.Load(CrabNames[c], (byte)archetype, "Body", VehicleSize.Walker * factor);
            if (model == null) return false;
            WalkerSizeFactor[c] = factor;
            var old = crabs[c];
            crabs[c] = model;
            if (old != null) seatsOf.Remove(old);
            int seats = SeatsFor(model).Seats.Count;
            foreach (var v in views.Values)
            {
                if (v.Archetype != archetype || v.Dead) continue;
                v.Model = model;
                v.Off = new bool[model.Lods[0].Parts.Count];
                v.World = new Matrix4x4[model.Lods[0].Parts.Count];
                v.Legs = null; v.LegLocal = null; v.LegSolved = null;
                v.Pieces.Clear();
                if (riders.TryGetValue(v.Slot, out var list))
                    foreach (var r in list) if (r.Seat >= seats && !r.Dismounting) { r.Dismounting = true; r.State = RiderState.ToEdge; r.StateAt = Time.time; }
            }
            return true;
        }

        VATRenderer Vat() { if (vat == null) vat = FindFirstObjectByType<VATRenderer>(); return vat; }
        CombatFx fx;
        CombatFx Fx() { if (fx == null) fx = FindFirstObjectByType<CombatFx>(); return fx; }

        // ------------------------------------------------------------------ every frame
        /// <summary>Every rider where he is this frame, handed to the VAT renderer. After Animate has posed the hulls.</summary>
        void RidersFrame(float now)
        {
            if (riders.Count == 0 && loose.Count == 0) return;
            if (Vat() == null) return;
            var match = Host.Local; var w = match.World;
            float grow = Mathf.Min(vat.CurrentGrow * 1.15f, RiderMaxGrow);   // a touch larger than the men on the ground: he is up against a bright hull
            float scale = vat.UnitScale * grow, dt = Mathf.Max(1e-4f, Time.deltaTime);
            bool thin = grow > RiderThinAbove;
            if (Host.TimeScale <= 0f) dt = 0f;
            int n = 0, ducking = 0;
            riderGone.Clear();
            foreach (var kv in riders)
            {
                bool alive = views.TryGetValue(kv.Key, out var v) && !v.Dead && v.World != null;
                var list = kv.Value;
                if (!alive) { ThrowOff(kv.Key, list, now); riderGone.Add(kv.Key); continue; }   // the machine went under them
                var seats = SeatsFor(v.Model);
                if (seats == null || seats.Seats.Count == 0) continue;
                var body = v.World[seats.BodyPart];
                // where each of its turrets points now, in the body's frame
                int guns = Mathf.Min(seats.Guns.Count, gunYaw.Length);
                if (guns > 0)
                {
                    var inv = body.inverse;
                    for (int g = 0; g < guns; g++) { int t = seats.Guns[g].Turret; gunYaw[g] = t < v.World.Length && !v.Off[t] ? RiderSeats.YawOf(inv * v.World[t]) : seats.Guns[g].Yaw0; }
                    // which are turning: a barrel at rest over a man keeps him lying flat (that is only honest), but only one
                    // swinging round is news - with the pips amber for every man under a resting barrel, they were amber always
                    if (!lastGunYaw.TryGetValue(kv.Key, out var was)) { was = new float[gunYaw.Length]; System.Array.Copy(gunYaw, was, gunYaw.Length); lastGunYaw[kv.Key] = was; }
                    for (int g = 0; g < guns; g++)
                    {
                        gunSweeping[g] = dt > 0f && Mathf.Abs(Mathf.DeltaAngle(was[g] * Mathf.Rad2Deg, gunYaw[g] * Mathf.Rad2Deg)) * Mathf.Deg2Rad / dt > SweepSpeed;
                        was[g] = gunYaw[g];
                    }
                }
                // what the machine is shooting at: the riders shoot at it too
                int target = -1;
                if (match.Gunnery != null)
                    for (int k = 0; k < TankGunnerySystem.Guns && target < 0; k++) { int t = match.Gunnery.GunTarget[kv.Key * TankGunnerySystem.Guns + k]; if (t >= 0 && t < w.HighWater && w.IsAlive(t)) target = t; }
                if (target < 0) target = NearestEnemy(w, kv.Key, v.Pos, now);
                float heaveKick = Mathf.Abs(v.Heave.Velocity);
                for (int k = list.Count - 1; k >= 0; k--)
                {
                    var r = list[k];
                    if (r.SimSlot >= 0 && !w.IsAlive(r.SimSlot))   // shot where the sim keeps him
                    {
                        // on the machine he topples off its side: a body laid where he knelt hung in the air once it walked on
                        Vector3 fly = Vector3.zero;
                        if (r.State != RiderState.Approach)
                        {
                            Vector3 away = r.Pos - v.Pos; away.y = 0f;
                            away = away.sqrMagnitude > 1e-4f ? away.normalized : new Vector3(Mathf.Sin(v.Yaw + Mathf.PI * 0.5f), 0f, Mathf.Cos(v.Yaw + Mathf.PI * 0.5f));
                            fly = away * (2f + Hash(r.Seed + 13) * 1.5f) + Vector3.up * 0.5f;
                        }
                        Kill(r, now, fly); list.RemoveAt(k);
                        NoteDeath(kv.Key, r.Seat, now);
                        continue;
                    }
                    if (r.Seat >= seats.Seats.Count && r.State == RiderState.Seated) { r.Dismounting = true; r.State = RiderState.ToEdge; r.StateAt = now; }
                    var seat = seats.Seats[Mathf.Min(r.Seat, seats.Seats.Count - 1)];
                    if (r.State == RiderState.Seated)
                        for (int g = 0; g < guns; g++)
                            // the 40-degree warning only ahead of a SWINGING barrel; a still one flattens just the men it is over
                            // (with both barrels trained forward on the enemy, the wide cone laid 14 of 15 flat and silenced
                            // the deck: measured after critic r17)
                            if (seats.UnderBarrel(seat.Local, g, gunYaw[g], gunSweeping[g] ? RiderSeats.DuckAhead : 0f))
                            {
                                r.DuckUntil = now + RiderStayFlat; ducking++;
                                // amber only for the men the barrel is actually crossing, on a machine that is fighting (every man
                                // under a turning turret went amber, on a hull turn with nobody to shoot at too: critic r4)
                                if (gunSweeping[g] && target >= 0 && seats.UnderBarrel(seat.Local, g, gunYaw[g], now < r.SweptUntil ? AmberHold : AmberAhead)) r.SweptUntil = now + RiderStayFlat;
                                break;
                            }
                    if (Step(r, v, body, seat, target, heaveKick, now, dt)) { list.RemoveAt(k); loose.Add(r); continue; }
                    // not on a tank: its men kneel on one level, where eight do not overlap, and half of them drawn left the
                    // men on show and the pips disagreeing (critic t3)
                    if (thin && !VehicleArchetype.IsTank(v.Model.Archetype) && r.State == RiderState.Seated && (r.Seat & 1) == 1) continue;
                    if (n < riderInst.Length) Emit(r, now, scale, ref n);
                }
                if (list.Count == 0) { riderGone.Add(kv.Key); pipLinger[kv.Key] = now; }
            }
            foreach (int slot in riderGone) riders.Remove(slot);
            RidersDucking = ducking;
            for (int k = loose.Count - 1; k >= 0; k--)
            {
                var r = loose[k];
                if (StepLoose(r, now)) { Release(r); loose.RemoveAt(k); continue; }
                if (n < riderInst.Length) Emit(r, now, scale, ref n);
            }
            vat.DrawExtras(riderInst, riderFig, n);
        }

        /// <summary>One rider on (or getting onto, or off) a live machine. True when he has left it (he is then loose).</summary>
        bool Step(Rider r, View v, Matrix4x4 body, RiderSeats.Seat seat, int target, float heaveKick, float now, float dt)
        {
            Vector3 seatW = body.MultiplyPoint3x4(seat.Local), edgeW = body.MultiplyPoint3x4(seat.Edge);
            Vector3 footW = body.MultiplyPoint3x4(new Vector3(seat.Foot.x, seat.Edge.y, seat.Foot.z));
            footW.y = Ground(footW.x, footW.z);
            Vector3 outW = body.MultiplyVector(seat.Out); outW.y = 0f; outW = outW.sqrMagnitude > 1e-6f ? outW.normalized : Vector3.forward;
            float inward = Mathf.Atan2(-outW.x, -outW.z);
            if (now < r.StateAt) { Stand(r, now); return false; }   // waiting his turn
            switch (r.State)
            {
                case RiderState.Approach:
                {
                    Vector3 to = footW - r.Pos; to.y = 0f;
                    float d = to.magnitude, stepLen = RiderRun * dt;
                    Play(r, Clip.Run, now, RiderRun / 3.2f);
                    if (d > 1e-3f) r.Yaw = TurnTo(r.Yaw, Mathf.Atan2(to.x, to.z), dt * 8f);
                    if (d <= Mathf.Max(0.15f, stepLen)) { r.Pos = footW; Enter(r, RiderState.Climb, now); }
                    else { r.Pos += to / d * stepLen; r.Pos.y = Ground(r.Pos.x, r.Pos.z); }
                    break;
                }
                case RiderState.Climb:
                {
                    float h = Mathf.Max(0.1f, edgeW.y - footW.y), u = Mathf.Clamp01((now - r.StateAt) * RiderClimb / h);
                    Play(r, Clip.ClimbLadder, now, 1.4f);
                    r.Yaw = TurnTo(r.Yaw, inward, dt * 10f);
                    // straight up the plating at the foot, in over the edge at the top
                    float inw = Mathf.SmoothStep(0f, 1f, (u - 0.8f) / 0.2f);
                    Vector3 off = outW * ClimbStandOff;
                    r.Pos = Vector3.Lerp(footW + off, new Vector3(edgeW.x, footW.y, edgeW.z), inw);
                    r.Pos.y = Mathf.Lerp(footW.y, edgeW.y, u);
                    if (u >= 1f) Enter(r, RiderState.Over, now);
                    break;
                }
                case RiderState.Over:
                {
                    float over = vat.SecondsFor(Clip.ClimbOut);
                    if (now - r.StateAt < over) { Play(r, Clip.ClimbOut, now); r.Pos = edgeW; r.Yaw = inward; break; }
                    if (Walk(r, seatW, now, dt, Clip.StoopWalk)) { Enter(r, RiderState.Seated, now); Play(r, RiderIdle, now); }
                    break;
                }
                case RiderState.Seated:
                {
                    r.Pos = seatW;
                    // each man settles at his own angle: all square to the same heading, ten men read as a crate stack (critic r8)
                    float rest = v.Yaw + seat.Yaw + (Hash(r.Seed + 21) - 0.5f) * 50f * Mathf.Deg2Rad;
                    bool live = target >= 0 && RidersFire && !v.Stalled;
                    float want = rest;
                    if (live)
                    {
                        var tp = (Vector3)(Unity.Mathematics.float3)Host.Local.World.Position[target];
                        float to = Mathf.Atan2(tp.x - seatW.x, tp.z - seatW.z);
                        // a man turns on the deck to face the enemy, but not round through the machine's own guns behind him
                        float off = Mathf.DeltaAngle(rest * Mathf.Rad2Deg, to * Mathf.Rad2Deg);
                        want = rest + Mathf.Clamp(off, -120f, 120f) * Mathf.Deg2Rad;
                        // ...nor through the machine's own bulk: behind the Maw's head he holds his fire (critic t4)
                        live = Mathf.Abs(off) <= 120f && !seat.BlindAt(to - v.Yaw);
                        if (!live) want = rest;
                    }
                    r.Yaw = TurnTo(r.Yaw, want, dt * 4f);
                    // a barrel swinging over him: he gets his head down until it has passed, and holds his fire
                    if (now < r.DuckUntil && !r.Dismounting)
                    {
                        if (!r.Flat) { r.Flat = true; r.FlatAt = now; r.RiseAt = -10f; r.ShotAt = -1f; }
                        bool down = now - r.FlatAt >= vat.SecondsFor(RiderDuckDown);
                        // under a barrel that is NOT swinging he fires from where he lies, with the volley
                        if (down && live && now >= r.SweptUntil)
                        {
                            if (r.ShotAt < 0f && now >= r.NextShot) { r.ShotAt = now; r.Flashed = false; r.Aim = target; }
                            if (r.ShotAt >= 0f && now - r.ShotAt >= vat.SecondsFor(RiderShotProne)) { r.ShotAt = -1f; r.NextShot = NextVolley(v.Slot, now) + Ripple(r); }
                            Play(r, r.ShotAt >= 0f ? RiderShotProne : RiderDuck, now);
                            break;
                        }
                        r.ShotAt = -1f;
                        Play(r, down ? RiderDuck : RiderDuckDown, now);
                        break;
                    }
                    if (r.Flat)
                    {
                        // the barrel has gone by: up onto his knee again before he does anything else
                        if (r.RiseAt < 0f) r.RiseAt = now;
                        if (now - r.RiseAt < vat.SecondsFor(RiderDuckUp)) { r.ShotAt = -1f; Play(r, RiderDuckUp, now); break; }
                        r.Flat = false; r.RiseAt = -10f;
                    }
                    // the hull is rocked by a burst: he ducks, and the shot waits
                    // ...and waits for the next volley, not firing alone as soon as he is up (every cannon shot flinched the deck,
                    // and the volley broke into single shots: 1-4 marks where 15 men fired, critic r18). The machine's own
                    // recoil is a smaller kick than a burst: 2.0, not 1.2
                    if (heaveKick > 2f && now - r.FlinchAt > 1.5f) { r.FlinchAt = now; r.ShotAt = -1f; r.NextShot = NextVolley(v.Slot, now + 0.6f) + Ripple(r); }
                    if (now - r.FlinchAt < vat.SecondsFor(Clip.KneelFlinch)) { Play(r, Clip.KneelFlinch, now); break; }
                    if (r.ShotAt < 0f && live && now >= r.NextShot) { r.ShotAt = now; r.Flashed = false; r.Aim = target; }
                    float shot = vat.SecondsFor(RiderShot);
                    if (r.ShotAt >= 0f && now - r.ShotAt >= shot) { r.ShotAt = -1f; r.NextShot = NextVolley(v.Slot, now) + Ripple(r); }
                    Play(r, r.ShotAt >= 0f ? RiderShot : RiderIdle, now);
                    if (r.Dismounting) Enter(r, RiderState.ToEdge, now);
                    break;
                }
                case RiderState.ToEdge:
                    if (Walk(r, edgeW, now, dt, Clip.StoopWalk)) { r.Yaw = inward; Enter(r, RiderState.Descend, now); }
                    break;
                case RiderState.Descend:
                {
                    float h = Mathf.Max(0.1f, edgeW.y - footW.y - DropHeight), u = Mathf.Clamp01((now - r.StateAt) * RiderClimb / h);
                    Play(r, Clip.ClimbLadder, now, -1.4f);   // the climb, backwards
                    r.Yaw = TurnTo(r.Yaw, inward, dt * 10f);
                    float outw = Mathf.SmoothStep(0f, 1f, u / 0.2f);
                    r.Pos = Vector3.Lerp(edgeW, new Vector3(footW.x, edgeW.y, footW.z) + outW * ClimbStandOff, outw);
                    r.Pos.y = Mathf.Lerp(edgeW.y, footW.y + DropHeight, u);
                    if (u >= 1f)
                    {
                        // let go: a short fall, landing a stride out from the hull
                        Vector3 land = footW + outW * 0.5f; land.y = Ground(land.x, land.z);
                        Launch(r, land, 0.15f, now);
                        r.State = RiderState.Drop;
                        Play(r, Clip.JumpDown, now, vat.SecondsFor(Clip.JumpDown) * 0.45f / Mathf.Max(0.2f, r.Flight));
                        return true;
                    }
                    break;
                }
            }
            return false;
        }

        /// <summary>A rider no longer on the machine: in the air, landing, getting up. True when he is done with.</summary>
        bool StepLoose(Rider r, float now)
        {
            if (r.State == RiderState.Drop || r.State == RiderState.Thrown)
            {
                float t = now - r.StateAt;
                if (t < 0f) return false;   // a staggered throw not yet begun: still where he knelt
                if (t < r.Flight) { r.Pos = Arc(r, t); return false; }
                r.Pos = r.Land;
                r.State = RiderState.Down; r.StateAt = now;
                Play(r, r.Hurt ? Clip.GetUp : Clip.ClimbLand, now);
                // a man thrown down hard kicks up the earth where he lands, so each one thrown clear is seen to land (critic t4)
                if (r.Hurt && r.Flight > 0.5f && books != null && books.Ready)
                    books.Add(FlipbookFx.Book.Puff, r.Land + Vector3.up * 0.4f, 2.6f, 1.6f, velocity: Vector3.up * 0.6f, grow: 1.3f, roll: Hash(r.Seed + 21) * 6.28f, alpha: 0.7f);
                // and lies where he fell a moment: getting straight up read as a man stepping off, not thrown (critic t5)
                if (r.Hurt && r.Flight > 0.5f) { r.Lying = true; Play(r, Clip.ProneIdle, now); }
                return false;
            }
            if (r.State == RiderState.Down)
            {
                if (r.Lying)
                {
                    if (now - r.StateAt < ThrownLie) return false;
                    r.Lying = false; r.StateAt = now; Play(r, Clip.GetUp, now);
                }
                float t = now - r.StateAt, up = vat.SecondsFor(r.Hurt ? Clip.GetUp : Clip.ClimbLand);
                if (t >= up && !r.Handed) { r.Handed = true; RiderLanded?.Invoke(r, r.Pos, r.Yaw); Play(r, Clip.Idle, now); }
                // someone took him back into the sim: his own man is drawn now; else he stands a moment and is gone
                return r.Handed && (t >= up + (RiderLanded != null ? 0.3f : StandAfter));
            }
            return true;
        }

        /// <summary>The machine is gone: the unlucky die thrown, the rest are pitched clear, and get up.</summary>
        void ThrowOff(int slot, List<Rider> list, float now)
        {
            pipLinger[slot] = now;   // the row stays over the wreck a while, with the dead in red
            var w = Host?.Local?.World;
            var hull = w != null && slot < w.HighWater ? SeatsFor(ModelFor(w.Archetype[slot])) : null;
            // pitched away from the HULL's middle, not the riders': on the Maw they kneel on the rear deck, and measured from
            // their own middle half of them were thrown forward into the tracks (critic t4)
            Vector3 mid = Centre(list);
            var lens = Camera.main;
            if (w != null && slot < w.HighWater)
            {
                var hp = (Vector3)(Unity.Mathematics.float3)w.Position[slot];
                if (new Vector2(hp.x - mid.x, hp.z - mid.z).sqrMagnitude < 400f) { mid.x = hp.x; mid.z = hp.z; }
            }
            foreach (var r in list)
            {
                // on the way up or down, a man is closer to the ground: he just drops
                Vector3 away = r.Pos - mid; away.y = 0f;
                float inside = hull != null ? Mathf.Max(0f, hull.Span - away.magnitude) : 0f;   // what is left of the machine to clear
                if (away.sqrMagnitude < 1e-4f) away = new Vector3(Hash(r.Seed) - 0.5f, 0f, Hash(r.Seed + 1) - 0.5f);
                away.Normalize();
                // toward the side the player looks from: pitched off the far side they land behind the wreck, out of sight (critic t5)
                if (lens != null) { var toLens = lens.transform.position - mid; toLens.y = 0f; if (toLens.sqrMagnitude > 1f) { away = (away + toLens.normalized * 0.9f); away.y = 0f; away.Normalize(); } }
                if (r.State == RiderState.Approach) { r.State = RiderState.Down; r.StateAt = now; r.Hurt = true; Play(r, Clip.Duck, now); loose.Add(r); NoteDeath(slot, r.Seat, now, thrown: true); continue; }
                bool dies = r.SimSlot < 0 && Hash(r.Seed + 11) < RiderDeathShare;   // a man the sim still holds is kept alive: the sim decides his death
                // pitched clear of the hull and then some: 3-6.5 m from where he knelt landed the Maw's men beside its tracks (critic t2)
                float reach = inside + 2f + Hash(r.Seed + 5) * 3.5f;
                if (dies) { Kill(r, now, away * reach + Vector3.up * (1.5f + Hash(r.Seed + 7) * 1.5f)); NoteDeath(slot, r.Seat, now); continue; }
                Vector3 land = r.Pos + away * reach; land.y = Ground(land.x, land.z);
                // a low, quick arc: 1.2-2.2 m over a 6 m deck hung them 2 s in the air over the burning husk, where they read as
                // men still standing on the wreck (critic t4)
                Launch(r, land, 0.3f + Hash(r.Seed + 7) * 1.1f, now);
                r.StateAt += Hash(r.Seed + 31) * 0.3f;   // not all on one frame: thrown together they flew as one lump (critic t6)
                NoteDeath(slot, r.Seat, now, thrown: true);   // pitched off alive: his pip pulses on the wreck's row, apart from the dead
                r.State = RiderState.Thrown; r.Hurt = true;
                // the thrown clip tumbles him (a standing clip carried him through the air upright, like a statue)
                Play(r, Clip.DeathThrown, now, vat.SecondsFor(Clip.DeathThrown) * 0.55f / Mathf.Max(0.3f, r.Flight));
                loose.Add(r);
            }
        }

        void Kill(Rider r, float now, Vector3 fly)
        {
            Release(r);
            vat.AddFallen(r.Pos, r.Yaw, r.Team, r.Seed & 3, fly.y > 0.05f ? Clip.DeathThrown : Clip.DeathKneel, r.Archetype, r.Clip, T(r, now), 0.15f, fly, 0, 0.5f);
        }

        static Vector3 Centre(List<Rider> list)
        {
            Vector3 c = Vector3.zero;
            foreach (var r in list) c += r.Pos;
            return list.Count > 0 ? c / list.Count : c;
        }

        /// <summary>A ballistic arc from where he is to `land`, rising `rise` above the higher end first.</summary>
        void Launch(Rider r, Vector3 land, float rise, float now)
        {
            r.From = r.Pos; r.Land = land; r.StateAt = now;
            float top = Mathf.Max(r.Pos.y, land.y) + rise;
            r.Up = Mathf.Sqrt(2f * (top - r.Pos.y) / Gravity);
            r.Flight = r.Up + Mathf.Sqrt(2f * (top - land.y) / Gravity);
            r.Top = top;
            Vector3 d = land - r.Pos; d.y = 0f;
            if (d.sqrMagnitude > 1e-4f && r.State != RiderState.Descend) r.Yaw = Mathf.Atan2(d.x, d.z);
        }

        static Vector3 Arc(Rider r, float t)
        {
            Vector3 at = Vector3.Lerp(r.From, r.Land, t / r.Flight);
            float fromTop = t - r.Up;
            at.y = r.Top - 0.5f * Gravity * fromTop * fromTop;
            return at;
        }

        /// <summary>Crouch-walk over the deck to `to`. True on arrival.</summary>
        bool Walk(Rider r, Vector3 to, float now, float dt, Clip clip)
        {
            Vector3 d = to - r.Pos;
            float flat = new Vector2(d.x, d.z).magnitude, stepLen = RiderDeck * dt;
            if (flat <= Mathf.Max(0.05f, stepLen)) { r.Pos = to; return true; }
            Play(r, clip, now, RiderDeck / 1f);
            r.Yaw = TurnTo(r.Yaw, Mathf.Atan2(d.x, d.z), dt * 8f);
            r.Pos += d * (stepLen / flat);   // the deck's own rise comes with the straight line between the two points
            return false;
        }

        void Stand(Rider r, float now) { if (r.State == RiderState.Approach) Play(r, Clip.Idle, now); }

        void Enter(Rider r, RiderState s, float now) { r.State = s; r.StateAt = now; }

        void Play(Rider r, Clip c, float now, float rate = 1f)
        {
            r.Rate = rate;
            if (r.Clip == c) return;
            r.PrevT = T(r, now); r.Prev = r.Clip; r.SwitchAt = now;
            r.Clip = c; r.ClipAt = now;
        }

        float T(Rider r, float now)
        {
            float x = (now - r.ClipAt) * r.Rate / vat.SecondsFor(r.Clip) + (r.Clip == RiderIdle ? r.Phase : 0f);
            return vat.Loops(r.Clip) ? Mathf.Repeat(x, 1f) : Mathf.Clamp01(x);
        }

        void Emit(Rider r, float now, float scale, ref int n)
        {
            var inst = new VatInstance
            {
                Pos = r.Pos, Yaw = r.Yaw, AnimRow = (int)r.Clip, AnimT = T(r, now), Tint = r.Team, Scale = scale,
                PrevRow = (int)r.Prev, PrevT = r.PrevT, Blend = Mathf.Clamp01(1f - (now - r.SwitchAt) / BlendSeconds), Pad = VatPad.Pack(0, r.Flat ? 0.2f : 0f, r.Seed),   // no grime (it sank them into the hull); a man flat under a barrel a touch darker - at 0.5 the flat men vanished into the hull at the standard view (critic r15)
            };
            byte fig = (byte)VATRenderer.FigureOfArchetype(r.Archetype);
            // the shot: a flash at the muzzle as the round goes (a fifth of the way into the fire clip)
            if (r.State == RiderState.Seated && r.ShotAt >= 0f && !r.Flashed && now - r.ShotAt >= vat.SecondsFor(RiderShot) * 0.2f)
            {
                r.Flashed = true;
                if (vat.MuzzleOf(inst, fig, out var muzzle, out var dir) && books != null && books.Ready)
                {
                    // bigger the further out the camera is (2.5x at the standard view): at their close-up size a volley did not
                    // register beside the machine's own gun (critic r13)
                    var lens = Camera.main;
                    float away = lens != null ? Mathf.Clamp01((Vector3.Distance(lens.transform.position, muzzle) - 30f) / 45f) : 1f;
                    float big = Mathf.Lerp(1f, 2.5f, away);
                    books.Add(FlipbookFx.Book.Muzzle, muzzle + dir * 0.35f * big, 1.3f * big * scale / vat.UnitScale, 0.1f, roll: Hash(r.Seed + (int)(now * 13f)) * 6.28f, glow: SceneMood.Night ? 3.5f : 2f);
                    books.Add(FlipbookFx.Book.Flash, muzzle + dir * 0.2f, 1.1f * big * scale / vat.UnitScale, 0.08f, roll: Hash(r.Seed + 5) * 6.28f, glow: SceneMood.Night ? 3f : 1.8f, pop: 0.5f);
                    shotMarks.Add((muzzle, dir, now)); RiderShots++;
                    // a small puff of powder smoke at every rifle: a volley leaves a row of them (critic r19)
                    {
                        // small, soft and rising straight up off the deck: bigger and carried along the shot, it drifted away as grey balls (critic r14)
                        books.Add(FlipbookFx.Book.Puff, muzzle + dir * 0.6f + Vector3.up * 0.15f, 0.8f * big, 1.0f, velocity: Vector3.up * 0.3f + dir * 0.25f, grow: 0.9f, roll: Hash(r.Seed + (int)(now * 3f)) * 6.28f, alpha: 0.35f);
                    }
                }
                SceneHooks.Flash?.Invoke(muzzle, new Color(1f, 0.75f, 0.45f), 6f, 5f, 0.07f);
                // and the round: at the night's range a flash alone did not say where he was shooting. It is not a sim hit,
                // so it goes to about the man, not into him
                var w = Host.Local.World;
                if (r.Aim >= 0 && r.Aim < w.HighWater && w.IsAlive(r.Aim) && Fx() != null)
                {
                    var tp = (Vector3)(Unity.Mathematics.float3)w.Position[r.Aim];
                    int shot = (int)(r.ShotAt * 10f);
                    tp += new Vector3(Hash(r.Seed + shot) - 0.5f, 0f, Hash(r.Seed + shot + 1) - 0.5f) * 3f;
                    tp.y = Ground(tp.x, tp.z) + (0.4f + Hash(r.Seed + shot + 2) * 1.2f) * scale / vat.UnitScale;
                    // wider at the standard view, back to the sim's width close in (doubled there they read as laser beams: critic r7)
                    var eye = Camera.main;
                    float far = eye != null ? Mathf.Clamp01((Vector3.Distance(eye.transform.position, muzzle) - 30f) / 45f) : 1f;
                    fx.AddTracer(muzzle, tp, r.Team, Mathf.Lerp(1f, RiderTracerWidth, far));
                }
            }
            riderInst[n] = inst; riderFig[n] = fig; n++;
        }

        readonly Dictionary<int, (int target, float until)> riderAim = new Dictionary<int, (int, float)>();
        [Tooltip("Metres: riders fire on their own at the nearest enemy this close when the machine's guns have nothing.")]
        public float RiderRange = 90f;

        /// <summary>The nearest living enemy within RiderRange of the machine, looked up at most twice a second.</summary>
        int NearestEnemy(SimWorld w, int slot, Vector3 at, float now)
        {
            if (riderAim.TryGetValue(slot, out var aim) && now < aim.until && (aim.target < 0 || w.IsAlive(aim.target))) return aim.target;
            int best = -1; float bestSq = RiderRange * RiderRange; byte team = w.Team[slot];
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || w.Team[i] == team) continue;
                float dx = w.Position[i].x - at.x, dz = w.Position[i].z - at.z, d = dx * dx + dz * dz;
                if (d < bestSq) { bestSq = d; best = i; }
            }
            riderAim[slot] = (best, now + 0.5f);
            return best;
        }

        // ------------------------------------------------------------------ pips on the ring
        [Tooltip("A dot by the ground ring for every seat of a machine carrying men: bright aboard, pulsing on the way on or off, faint empty.")]
        public bool RiderPips = true;
        [Tooltip("Metres across one pip per metre from the camera: the same size on screen at every zoom (a HUD mark, not a thing on the field).")]
        public float PipScreen = 0.0137f;   // 11 px dots at the standard view: at 0.0225 the row was 2.5x the ring (critic r4)   // 1.75 m at the standard view (camera 77.6 m off); sized in metres, fifteen ran off the screen close in (critic r4)
        const int MaxPips = 256;
        readonly Matrix4x4[] pipM = new Matrix4x4[MaxPips]; readonly Vector4[] pipC = new Vector4[MaxPips], pipP = new Vector4[MaxPips]; int pipCount;
        static readonly int PipId = Shader.PropertyToID("_Pip");
        MaterialPropertyBlock pipProps; Material pipMat;
        // per SEAT: its state this frame, and where it lies across the screen (the row is in that order)
        readonly byte[] seatState = new byte[RiderSeats.MaxSeats + 8];
        readonly float[] seatKey = new float[RiderSeats.MaxSeats + 8];
        readonly int[] seatOrder = new int[RiderSeats.MaxSeats + 8];
        // per machine: when its last rider left (the empty seats stay shown a while), and which seats lost their man when
        readonly Dictionary<int, float> pipLinger = new Dictionary<int, float>();
        // a seat whose man was killed (thrown = false) or pitched off alive when the machine died (thrown = true): kept until
        // someone takes the seat or the row goes - it is the machine's tally (after 4 s the dead turned back into empty
        // seats on a live machine, and read as men who had got off: critic r8)
        readonly Dictionary<int, List<(int seat, float at, bool thrown)>> pipDeaths = new Dictionary<int, List<(int seat, float at, bool thrown)>>();
        [Tooltip("Seconds the seats stay shown after the last rider has left or died (or the machine under them), so 'all dead' does not read as 'no seats'.")]
        public float PipLinger = 4f;
        // a killed rider's seat: solid red, then a red ring, then empty (0.6 s of red was missed at a glance: critic r6)
        const float PipDeathFlash = 1.2f, PipDeathRing = 4f;   // then dark red for as long as the row lasts: turned back into an empty ring, the dead read as men who got off (critic r7)
        const byte PipEmpty = 0, PipMoving = 1, PipAboard = 2, PipFlat = 3, PipDead = 4, PipDeadRing = 5, PipThrown = 6;

        readonly Dictionary<int, Vector3> pipAnchor = new Dictionary<int, Vector3>();

        /// <summary>A wreck's ring, faintly, while its riders' row still shows over it: without it the row floated below
        /// nothing (critic r13). 0 otherwise, as before (a wreck shows only its contact blob).</summary>
        float WreckRing(View v)
        {
            if (!RiderPips || !pipLinger.TryGetValue(v.Slot, out float died) || Time.time - died > PipLinger) return 0f;
            return views.TryGetValue(v.Slot, out var live) && !live.Dead ? 0f : 0.45f;   // 0.35 was only just visible over rubble
        }
        readonly Dictionary<int, (bool on, float since)> pipWrap = new Dictionary<int, (bool on, float since)>();

        void NoteDeath(int slot, int seat, float now, bool thrown = false)
        {
            if (!pipDeaths.TryGetValue(slot, out var dl)) { dl = new List<(int seat, float at, bool thrown)>(); pipDeaths[slot] = dl; }
            dl.Add((seat, now, thrown));
        }

        static bool SeatTaken(List<Rider> list, int seat) { if (list != null) foreach (var r in list) if (r.Seat == seat) return true; return false; }

        /// <summary>
        /// At the standard view a man on a deck is a few pixels against a bright hull, and a walker carrying eight reads
        /// much like an empty one (review round 2). So every seat gets a pip in ONE straight row across the screen under
        /// the machine's ground ring, a pip clear of it, grouped in fives: a solid dot in the side's colour for a man aboard,
        /// pulsing while he climbs on or off, an amber triangle while he lies flat under a barrel swinging over him, red
        /// where a man was just killed, a dark ringed disc for an empty seat. The pips are in the order the SEATS lie across
        /// the screen, so the amber ones bunch on the side the barrel is crossing and a death shows where it happened (in
        /// rider order they interleaved: critic r6). The shader builds each pip facing the camera that draws it (_Pip).
        /// Called by QueueDisc with the ring it just placed, for a wreck too: the row outlives the machine for PipLinger.
        /// </summary>
        void QueuePips(View v, Vector3 at, float w, float l)
        {
            if (!RiderPips) return;
            float now = Time.time;
            // a wreck keeps its row only while its own death lingers, and never once a live unit has its slot (the sim reuses
            // slots: a wreck drew the next machine's riders against its own seats, with no ring - critic r13 footage)
            if (v.Dead && (views.ContainsKey(v.Slot) && !views[v.Slot].Dead || !pipLinger.TryGetValue(v.Slot, out float died) || now - died > PipLinger)) return;
            riders.TryGetValue(v.Slot, out var list);
            int aboard = list != null ? list.Count : 0;
            pipDeaths.TryGetValue(v.Slot, out var deaths);
            if (deaths != null)
            {
                deaths.RemoveAll(d => SeatTaken(list, d.seat));
                if (deaths.Count == 0) { pipDeaths.Remove(v.Slot); deaths = null; }
            }
            // after a plain dismount the empty row goes sooner: fifteen empty rings under a machine carrying nobody (critic r7)
            bool lingering = aboard == 0 && pipLinger.TryGetValue(v.Slot, out float since) && now - since < (deaths != null ? PipLinger : PipLinger * 0.5f);
            if (aboard == 0 && !lingering) { if (deaths != null) pipDeaths.Remove(v.Slot); return; }
            var seats = SeatsFor(v.Model);
            int total = Mathf.Min(seats != null ? seats.Seats.Count : 0, seatState.Length);
            if (total == 0) return;
            for (int k = 0; k < total; k++) seatState[k] = PipEmpty;
            if (deaths != null)
                foreach (var d in deaths) if (d.seat >= 0 && d.seat < total) seatState[d.seat] = d.thrown ? PipThrown : now - d.at < PipDeathFlash ? PipDead : PipDeadRing;
            if (list != null)
                foreach (var r in list)
                {
                    if (r.Seat < 0 || r.Seat >= total) continue;
                    seatState[r.Seat] = r.State == RiderState.Seated && !r.Dismounting ? (r.Flat && now < r.SweptUntil ? PipFlat : PipAboard) : PipMoving;
                }

            var rot = Quaternion.AngleAxis(v.Yaw * Mathf.Rad2Deg, Vector3.up);
            var cam = Camera.main;
            Vector3 fwd = cam != null ? cam.transform.forward : Vector3.forward, right = cam != null ? cam.transform.right : Vector3.right;
            float size = Mathf.Max(0.3f, PipScreen * (cam != null ? Vector3.Distance(cam.transform.position, at) : 77.6f)), hw = w * 0.5f, hl = l * 0.5f;
            // the ring's band is drawn 0.18..0.32 beyond a box 0.62 of the quad's half size: the row hangs under its point
            // nearest the camera, centred on the ring across the screen
            float bx = 0.62f * hw, bz = 0.62f * hl, rx = 0.32f * hw + size * 0.45f, rz = 0.32f * hl + size * 0.45f;
            Vector3 toward = -new Vector3(fwd.x, 0f, fwd.z); toward = toward.sqrMagnitude > 1e-4f ? toward.normalized : Vector3.back;
            var local = Quaternion.Inverse(rot) * toward;
            Vector2 near = Vector2.zero; float best = float.MinValue, lo = float.MaxValue, hi = float.MinValue;
            var rightLocal = Quaternion.Inverse(rot) * right;
            float[] cx = { bx, bx, -bx, -bx }, cz = { -bz, bz, bz, -bz };
            for (int c = 0; c < 4; c++)
                for (int k = 0; k < 12; k++)
                {
                    float a = (c - 1 + k / 11f) * Mathf.PI * 0.5f;
                    var p = new Vector2(cx[c] + rx * Mathf.Cos(a), cz[c] + rz * Mathf.Sin(a));
                    float d = p.x * local.x + p.y * local.z;
                    if (d > best) { best = d; near = p; }
                    // the ring's VISIBLE band (0.32 of the half size past the box), not the pip rim: measured with the pips'
                    // margin the 80% clamp never bit (critic r12)
                    float across = (cx[c] + 0.32f * hw * Mathf.Cos(a)) * rightLocal.x + (cz[c] + 0.32f * hl * Mathf.Sin(a)) * rightLocal.z;
                    lo = Mathf.Min(lo, across); hi = Mathf.Max(hi, across);
                }
            Vector3 anchor = at + rot * new Vector3(near.x, 0f, near.y) + toward * (size * 0.6f);
            // eased: the nearest corner changes as the walker turns, and the row jumped 40% of the ring sideways (critic r12)
            if (pipAnchor.TryGetValue(v.Slot, out var was) && (was - anchor).sqrMagnitude < 400f)
                anchor = Vector3.Lerp(was, anchor, 1f - Mathf.Exp(-Time.deltaTime / 0.2f));
            pipAnchor[v.Slot] = anchor;
            // centred on the ring's corner nearest the lens (its lowest point on screen), not on its middle: with walkers side
            // by side, a row centred on its ring's middle read as the next machine's (critic r11)

            // grouped BY STATE, left to right: men flat under a barrel, men aboard, men climbing, the dead, the empty seats. In
            // the order the seats lie across the screen the amber ones still interleaved, since a sweep threatens by angle and
            // the seats sit in two rows (critic r7); a count reads at a glance, and the ring shows where the machine is
            for (int k = 0; k < total; k++)
            {
                byte st = seatState[k];
                int rank = st == PipFlat ? 0 : st == PipAboard ? 1 : st == PipMoving || st == PipThrown ? 2 : st == PipDead || st == PipDeadRing ? 3 : 4;
                seatKey[k] = rank * 100 + k;
                seatOrder[k] = k;
            }
            System.Array.Sort(seatKey, seatOrder, 0, total);
            // 11 px dots on a 14 px pitch at the standard view (a 19 px pitch made the row wider than the machine: critic r6).
            // Within 80% of its own ring across the screen - walkers side by side had rows running under each other (r9-r11):
            // smaller first, down to 55%, then a second line (up to ten a line).
            float step = size * 0.78f, gap = size * 0.45f;
            int perLine = total;
            float RowLen(int n) => (n - 1) * step + ((n - 1) / 5) * gap;
            if (total > 10) perLine = 10;
            float room = Mathf.Max(size * 2f, (hi - lo) * 0.8f), len = RowLen(perLine);
            if (len > room)
            {
                float k = Mathf.Max(0.55f, room / len);
                step *= k; gap *= k; size *= k;
                len = RowLen(perLine);
            }
            // two lines of up to ten when one will not fit - with hysteresis, or a row at the edge flipped between one line
            // and two from frame to frame (critic r12): wrap past the room, unwrap only under 87% of it, hold 0.5 s either way
            // the layout is the machine's, never the moment's: more than ten seats are always two lines of up to ten (fitting
            // to the ring's width as the walker turned flipped a row between one line and two mid-action: critic r15)
            if (total > 10) { perLine = 10; len = RowLen(perLine); }
            int lines = (total + perLine - 1) / perLine;
            float lineH = size * 1.0f;

            var c0 = v.Team == 1 ? TeamB : TeamA;
            var red = new Color(0.9f, 0.22f, 0.2f);
            // a dark plate behind the row, drawn first: with walkers side by side their rows ran into each other's rings and
            // could not be told apart (critic r9)
            int rowFirst = pipCount;
            if (pipCount < MaxPips)
            {
                pipM[pipCount] = Matrix4x4.TRS(anchor, Quaternion.identity, new Vector3(len + size * 1.1f, 1f, size * 1.25f + (lines - 1) * lineH));
                pipP[pipCount] = new Vector4(0f, -size * 0.6f - (lines - 1) * lineH * 0.5f, size * 0.9f, 0f);
                pipC[pipCount] = new Vector4(c0.r, c0.g, c0.b, 6f);
                pipCount++;
            }
            float pulse = 0.35f + 0.3f * Mathf.Sin(now * 6f);
            for (int j = 0; j < total && pipCount < MaxPips; j++)
            {
                pipM[pipCount] = Matrix4x4.TRS(anchor, Quaternion.identity, new Vector3(size, 1f, size));
                int line = j / perLine, at5 = j % perLine;
                // a short last line is centred under the first (flush left, its half-empty plate looked lopsided: critic r16)
                int onLine = Mathf.Min(perLine, total - line * perLine);
                float shift = (len - ((onLine - 1) * step + ((onLine - 1) / 5) * gap)) * 0.5f;
                pipP[pipCount] = new Vector4(-len * 0.5f + shift + at5 * step + (at5 / 5) * gap, -size * 0.6f - line * lineH, size, 0f);   // 0.95 hung two pitches under the ring (critic r7)
                byte f = seatState[seatOrder[j]];
                float fill = f == PipAboard || f == PipDead || f == PipDeadRing ? 1f : f == PipMoving ? pulse : 0f;
                var col = f == PipDead ? red : f == PipDeadRing ? new Color(0.66f, 0.2f, 0.2f) : f == PipFlat ? new Color(1f, 0.68f, 0.18f) : c0;
                pipC[pipCount] = new Vector4(col.r, col.g, col.b, f == PipFlat ? 4f : f == PipThrown ? 7f : 2f + fill);
                pipCount++;
            }
            float plateH = size * 1.25f + (lines - 1) * lineH;
            pipRows.Add(new PipRow { First = rowFirst, Count = pipCount - rowFirst, Anchor = anchor, HalfW = (len + size * 1.1f) * 0.5f, HalfH = plateH * 0.5f, MidY = -size * 0.6f - (lines - 1) * lineH * 0.5f });
        }

        void DrawPips()
        {
            if (pipCount == 0 || discMat == null) { pipCount = 0; pipRows.Clear(); return; }
            if (pipProps == null) pipProps = new MaterialPropertyBlock();
            // after the transparent effects: under the ring's queue, battle smoke covered the row exactly when it mattered (critic r2)
            if (pipMat == null) pipMat = new Material(discMat) { name = "Rider pips", hideFlags = HideFlags.HideAndDontSave, renderQueue = 3400 };
            SeparateRows();
            AddShotMarks();
            pipProps.SetVectorArray(ColorId, pipC);
            pipProps.SetVectorArray(PipId, pipP);
            var rp = new RenderParams(pipMat) { worldBounds = Everywhere, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false, matProps = pipProps };
            FrameBudget.Draw(rp, discMesh, 0, pipM, pipCount);
            pipCount = 0;
        }

        // ------------------------------------------------------------------ shot marks
        readonly List<(Vector3 at, Vector3 dir, float born)> shotMarks = new List<(Vector3 at, Vector3 dir, float born)>();
        /// <summary>Rider shots so far (the lab measures the volley rate from it).</summary>
        public int RiderShots { get; private set; }
        [Tooltip("Seconds a rider's shot shows as a bright dot at his muzzle (a screen-sized mark, like the pips).")]
        public float ShotMarkSeconds = 0.35f;

        /// <summary>
        /// Each rider's shot as a small warm dot at his muzzle, a constant size on screen, drawn with the pips (so through
        /// the hull and after smoke): at the standard view the flipbook flash of a rifle was lost beside the machine's own
        /// cannon, and nothing said where the riders' fire came from (critic r16).
        /// </summary>
        void AddShotMarks()
        {
            float now = Time.time;
            shotMarks.RemoveAll(m => now - m.born > ShotMarkSeconds);
            var cam = Camera.main;
            if (cam == null) return;
            foreach (var (muzzleAt, aim, born) in shotMarks)
            {
                if (pipCount >= MaxPips) break;
                var at = muzzleAt + aim * 0.5f;   // out in front of the muzzle, where the flash is
                // the aim's angle on screen, so the star streaks along the shot
                Vector3 s0 = cam.WorldToScreenPoint(muzzleAt), s1 = cam.WorldToScreenPoint(muzzleAt + aim);
                float angle = Mathf.Atan2(s1.y - s0.y, s1.x - s0.x);
                // ~7 px at the standard view, a near-white core on the pip's dark rim: 3 px warm yellow was lost in the cannon's
                // gold light, the lamps and the amber pips (critic r17)
                float size = PipScreen * 2.2f * Vector3.Distance(cam.transform.position, at);   // ~14 px: at 7 they read as sparks (critic r18), at 9 they vanished beside a tank's own gun (critic t3)
                float k = 1f - (now - born) / ShotMarkSeconds;
                pipM[pipCount] = Matrix4x4.TRS(at, Quaternion.identity, new Vector3(size * 1.8f * (0.6f + 0.4f * k), 1f, size));
                pipP[pipCount] = new Vector4(0f, 0f, 0.1f, angle);   // not pulled toward the lens: it must hide behind what hides the rifle
                pipC[pipCount] = new Vector4(1f, 0.93f * k + 0.7f * (1f - k), 0.75f * k + 0.35f * (1f - k), 8f);   // warm white, cooling
                pipCount++;
            }
        }

        struct PipRow { public int First, Count; public Vector3 Anchor; public float HalfW, HalfH, MidY; }
        readonly List<PipRow> pipRows = new List<PipRow>();
        readonly List<Rect> placedRows = new List<Rect>();
        readonly List<(float y, int i)> rowsDown = new List<(float y, int i)>();

        /// <summary>
        /// Walkers in a tight column put their rows on top of each other and into each other's rings, so a row could not be
        /// told from its neighbour's (critics r9-r14). On screen, from the top down, each row's tag is pushed below any tag
        /// already placed that it overlaps, by the overlap and 3 px. The rows are still each under their own ring.
        /// </summary>
        void SeparateRows()
        {
            var cam = Camera.main;
            if (cam == null || pipRows.Count < 2) { pipRows.Clear(); return; }
            float tanHalf = Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad), px = cam.pixelHeight;
            rowsDown.Clear();
            for (int i = 0; i < pipRows.Count; i++)
            {
                var sp = cam.WorldToScreenPoint(pipRows[i].Anchor);
                if (sp.z > 0.1f) rowsDown.Add((sp.y + pipRows[i].MidY * px / (2f * sp.z * tanHalf), i));
            }
            rowsDown.Sort((a, b) => b.y.CompareTo(a.y));
            placedRows.Clear();
            foreach (var (_, i) in rowsDown)
            {
                var row = pipRows[i];
                var sp = cam.WorldToScreenPoint(row.Anchor);
                float k = px / (2f * sp.z * tanHalf);   // pixels per metre at the row's distance
                var rect = new Rect(sp.x - row.HalfW * k, sp.y + row.MidY * k - row.HalfH * k, row.HalfW * 2f * k, row.HalfH * 2f * k);
                float pushed = 0f;
                for (int pass = 0; pass < 8; pass++)
                {
                    bool moved = false;
                    foreach (var other in placedRows)
                        if (rect.Overlaps(other)) { float d = rect.yMax - other.yMin + 3f; rect.y -= d; pushed += d; moved = true; }
                    if (!moved) break;
                }
                placedRows.Add(rect);
                if (pushed > 0f)
                    for (int j = row.First; j < row.First + row.Count && j < pipCount; j++) pipP[j].y -= pushed / k;
            }
            pipRows.Clear();
        }

        // ------------------------------------------------------------------ volleys
        /// <summary>Riders on one machine fire TOGETHER, on the machine's clock every RiderShotEvery seconds, each within
        /// VolleySpread of it: scattered one at a time, a shot every third of a second with a 0.12 s tracer, their fire did
        /// not register at the standard view at all (critic r5). A volley of eight does - and it is how men fired from a hull.</summary>
        [Tooltip("Seconds over which a volley's shots are spread.")]
        public float VolleySpread = 0.12f;   // 0.3 read as scattered shots, not one volley (critic r7)
        [Tooltip("Riders' tracers, this many times the sim's: a rifle round's line vanished at the standard view (critic r6).")]
        public float RiderTracerWidth = 2.0f;   // 2 read as a glowing rod in some frames (critic r15); 1.6 lost beside a tank's gun (critic t3)
        readonly Dictionary<int, float> volleyBase = new Dictionary<int, float>(), volleyPuff = new Dictionary<int, float>();

        /// <summary>A volley ripples down the deck, a man every 35 ms in seat order (all at once, the flashes read as a set of
        /// eyes: critic r19).</summary>
        float Ripple(Rider r) => (r.Seat < 0 ? 0 : r.Seat % 12) * 0.035f;

        float NextVolley(int slot, float now)
        {
            if (!volleyBase.TryGetValue(slot, out float b)) { b = now; volleyBase[slot] = b; }
            float every = Mathf.Max(0.5f, RiderShotEvery);
            return b + Mathf.Ceil((now - b) / every + 1e-4f) * every;
        }

        static float TurnTo(float from, float to, float rate) => from + Mathf.Clamp(Mathf.DeltaAngle(from * Mathf.Rad2Deg, to * Mathf.Rad2Deg) * Mathf.Deg2Rad, -rate, rate);

        static float Hash(int seed) { float h = Mathf.Sin(seed * 12.9898f + 78.233f) * 43758.5453f; return h - Mathf.Floor(h); }
    }
}
