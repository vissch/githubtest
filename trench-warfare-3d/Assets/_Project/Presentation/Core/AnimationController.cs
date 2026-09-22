// Phase: C1 (implemented; docs/15-character-controller.md sections 3 to 9; lives in Core so SimHost can drive it) — the decision layer of the character
// controller. Every sim tick it reads each man's stance, speed, target, flags, the ground under him and the events
// that named him, and runs the priority ladder: death, trench edge, reaction, action, stance change, turn,
// locomotion, fire, idle. The top rung with something to say owns the body; a one-shot keeps it until it ends unless
// a higher rung takes over. Between ticks Advance moves the frame, closes the cross-fade and eases the shown yaw. Each clip is
// a row of the baked atlas (Editor/InfantryClipTable maps it to its Mixamo file; VATRenderer plays it) and also names
// one of the 18 procedural rows (Fallback) for the far tier and for a project without the bake. The trace
// (Follow / TraceText) records one man's rung changes with their reasons, which is how the ladder is judged.
using System.Collections.Generic;
using System.Text;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    /// <summary>The clips the controller can ask for: the families of docs/15 section 2, named for the future clip table.</summary>
    public enum Clip : byte
    {
        None,
        // idle (KneelAimedIdle is the squat with the rifle up; FidgetRubEyes is a kneeling fidget, the others stand)
        Idle, AimedIdle, ReadyIdle, KneelIdle, KneelAimedIdle, ProneIdle, StoopIdle, FidgetLookAround, FidgetRubEyes, FidgetCheckShoe, FidgetCollar, FidgetInspect,
        // locomotion
        Walk, WalkAimed, WalkWary, WalkBack, WalkLeft, WalkRight, Run, RunBack, Sprint, StoopWalk, StoopLow, CrouchRun, Crawl, CrawlBack, Wade, SideStep, MGCarry, WireCross,
        FireWalk, FireRun, FireSprint, FireStoop,
        // fire (FireSnap is the 0.27 s aimed shot: the bolt-action rifle and the sniper, followed by ReloadBolt)
        FireStand, FireSnap, FireKneel, FireProne, FireMG, AimUp, AimDown, KneelAimUp, KneelAimDown,
        // actions
        ReloadStand, ReloadStoop, ReloadProne, ReloadBolt, Throw, MeleeStab, MeleePunch, MeleeSmash, MeleeBlock, Sling, Unsling,
        // reactions
        HitStand, HitHeavy, HitWalk, HitRun, HitProne, KneelFlinch, ProneFlinch, Duck, Shield, DiveRoll, ProneRoll, Trip, GetUp, MaskOn, Burning, Stumble,
        // trench and stance (the climb out is three parts: the push up, the hold while the sim lifts him, the landing)
        JumpDown, ClimbOut, ClimbHold, ClimbLand, ClimbLadder, StandToKneel, KneelToStand, KneelToProne, ProneToKneel, StandToStoop, StoopToStand, StoopToKneel, KneelToStoop, TakeCover, Emerge,
        Turn90L, Turn90R, Turn180, KneelTurn90L, KneelTurn90R, StoopTurn90L, StoopTurn90R, StoopTurn180,
        // deaths
        DeathFront, DeathBack, DeathRight, DeathLeft, DeathHeadshot, DeathWalking, DeathRunning, DeathKneel, DeathSquat, DeathProne, DeathBlast,
        Count
    }

    public enum Rung : byte { None = 0, Death = 1, Trench = 2, Reaction = 3, Action = 4, StanceChange = 5, Turn = 6, Locomotion = 7, Fire = 8, Idle = 9 }

    /// <summary>What a clip is: how long, whether it loops, the speed it was authored at (loops), and today's stand-in row.</summary>
    public struct ClipInfo { public float Seconds, Speed, Turn; public bool Loop; public AnimRow Fallback; public byte Stance; }

    public static class Clips
    {
        public static readonly ClipInfo[] Table = Build();

        /// <summary>The bake's own lengths replace the table's estimates (VATRenderer, once the atlas is loaded).</summary>
        public static void Apply(float[] rowSeconds)
        {
            for (int c = 0; c < Table.Length && c < rowSeconds.Length; c++) if (rowSeconds[c] > 0.01f) Table[c].Seconds = rowSeconds[c];
        }
        static ClipInfo L(float s, AnimRow row, float speed = 0f, Stance stance = Stance.Standing) => new ClipInfo { Seconds = s, Loop = true, Fallback = row, Speed = speed, Stance = (byte)stance };
        static ClipInfo O(float s, AnimRow row, Stance stance = Stance.Standing) => new ClipInfo { Seconds = s, Loop = false, Fallback = row, Stance = (byte)stance };
        static ClipInfo[] Build()
        {
            var t = new ClipInfo[(int)Clip.Count];
            t[(int)Clip.Idle] = L(2.4f, AnimRow.Idle); t[(int)Clip.AimedIdle] = L(2f, AnimRow.FireStanding); t[(int)Clip.ReadyIdle] = L(2.6f, AnimRow.Idle);
            t[(int)Clip.KneelIdle] = L(1.7f, AnimRow.CrouchWalk, 0f, Stance.Crouch); t[(int)Clip.KneelAimedIdle] = O(0.08f, AnimRow.FireFireStep, Stance.Crouch);
            t[(int)Clip.ProneIdle] = L(4.9f, AnimRow.PinnedLoop, 0f, Stance.Prone); t[(int)Clip.StoopIdle] = L(6.7f, AnimRow.CrouchWalk, 0f, Stance.Crouch);
            t[(int)Clip.FidgetLookAround] = O(5.3f, AnimRow.Idle); t[(int)Clip.FidgetRubEyes] = O(1.8f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.FidgetCheckShoe] = O(4.7f, AnimRow.Idle); t[(int)Clip.FidgetCollar] = O(5.9f, AnimRow.Idle);
            t[(int)Clip.FidgetInspect] = O(3.2f, AnimRow.Idle);
            t[(int)Clip.Walk] = L(1.07f, AnimRow.Walk, 1.4f); t[(int)Clip.WalkAimed] = L(1.1f, AnimRow.Walk, 1.3f); t[(int)Clip.WalkWary] = L(1.33f, AnimRow.Walk, 1.1f);
            t[(int)Clip.WalkBack] = L(1.1f, AnimRow.Walk, 1.1f); t[(int)Clip.WalkLeft] = L(1.1f, AnimRow.Walk, 1.2f); t[(int)Clip.WalkRight] = L(1.1f, AnimRow.Walk, 1.2f);
            t[(int)Clip.Run] = L(0.7f, AnimRow.Sprint, 3.2f); t[(int)Clip.RunBack] = L(0.8f, AnimRow.Sprint, 2.6f); t[(int)Clip.Sprint] = L(0.6f, AnimRow.Sprint, 5f);
            t[(int)Clip.StoopWalk] = L(1.2f, AnimRow.CrouchWalk, 1f, Stance.Crouch); t[(int)Clip.StoopLow] = L(1.3f, AnimRow.CrouchWalk, 0.8f, Stance.Crouch); t[(int)Clip.CrouchRun] = L(0.8f, AnimRow.CrouchWalk, 2.4f, Stance.Crouch);
            t[(int)Clip.Crawl] = L(1.9f, AnimRow.ProneCrawl, 0.5f, Stance.Prone); t[(int)Clip.CrawlBack] = L(1.9f, AnimRow.ProneCrawl, 0.4f, Stance.Prone);
            t[(int)Clip.Wade] = L(2.5f, AnimRow.Walk, 0.7f); t[(int)Clip.SideStep] = L(1.2f, AnimRow.Walk, 0.8f);
            t[(int)Clip.MGCarry] = L(1.3f, AnimRow.Walk, 1.2f); t[(int)Clip.WireCross] = L(2.4f, AnimRow.CrouchWalk, 0.5f, Stance.Crouch);
            t[(int)Clip.FireWalk] = L(1.1f, AnimRow.FireStanding, 1.3f); t[(int)Clip.FireRun] = L(0.75f, AnimRow.Sprint, 3f); t[(int)Clip.FireSprint] = L(0.65f, AnimRow.Sprint, 4.5f); t[(int)Clip.FireStoop] = L(1.2f, AnimRow.CrouchWalk, 1f, Stance.Crouch);
            t[(int)Clip.FireStand] = O(1.17f, AnimRow.FireStanding); t[(int)Clip.FireSnap] = O(0.27f, AnimRow.FireStanding); t[(int)Clip.FireKneel] = O(1.0f, AnimRow.FireFireStep, Stance.Crouch);
            t[(int)Clip.FireProne] = O(0.9f, AnimRow.FireProne, Stance.Prone); t[(int)Clip.FireMG] = L(0.45f, AnimRow.FireProne, 0f, Stance.Prone);
            t[(int)Clip.AimUp] = O(0.6f, AnimRow.FireStanding); t[(int)Clip.AimDown] = O(0.6f, AnimRow.Idle); t[(int)Clip.KneelAimUp] = O(0.6f, AnimRow.FireFireStep, Stance.Crouch); t[(int)Clip.KneelAimDown] = O(0.6f, AnimRow.CrouchWalk, Stance.Crouch);
            t[(int)Clip.ReloadStand] = O(2.3f, AnimRow.Idle); t[(int)Clip.ReloadStoop] = O(2.4f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.ReloadProne] = O(2.6f, AnimRow.PinnedLoop, Stance.Prone); t[(int)Clip.ReloadBolt] = O(0.8f, AnimRow.FireStanding);
            t[(int)Clip.Throw] = O(1.9f, AnimRow.Throw); t[(int)Clip.MeleeStab] = O(1.1f, AnimRow.FireStanding); t[(int)Clip.MeleePunch] = O(1f, AnimRow.FireStanding); t[(int)Clip.MeleeSmash] = O(1.2f, AnimRow.FireStanding); t[(int)Clip.MeleeBlock] = O(0.9f, AnimRow.Flinch1);
            t[(int)Clip.Sling] = O(1.6f, AnimRow.Idle); t[(int)Clip.Unsling] = O(1.4f, AnimRow.Idle);
            t[(int)Clip.HitStand] = O(0.7f, AnimRow.Flinch1); t[(int)Clip.HitHeavy] = O(1.2f, AnimRow.Flinch2); t[(int)Clip.HitWalk] = O(0.8f, AnimRow.Flinch1); t[(int)Clip.HitRun] = O(0.6f, AnimRow.Flinch0);
            t[(int)Clip.HitProne] = O(0.8f, AnimRow.PinnedLoop, Stance.Prone); t[(int)Clip.KneelFlinch] = O(0.6f, AnimRow.Flinch1, Stance.Crouch); t[(int)Clip.ProneFlinch] = O(0.6f, AnimRow.PinnedLoop, Stance.Prone);
            t[(int)Clip.Duck] = O(0.8f, AnimRow.Flinch2); t[(int)Clip.Shield] = O(1.4f, AnimRow.Flinch2); t[(int)Clip.DiveRoll] = O(1.5f, AnimRow.Flinch2); t[(int)Clip.ProneRoll] = O(1.2f, AnimRow.PinnedLoop, Stance.Prone);
            t[(int)Clip.Trip] = O(1.3f, AnimRow.Flinch2); t[(int)Clip.GetUp] = O(2.3f, AnimRow.CrouchWalk); t[(int)Clip.MaskOn] = O(2.6f, AnimRow.Flinch2); t[(int)Clip.Burning] = L(1.33f, AnimRow.Sprint, 2.5f); t[(int)Clip.Stumble] = O(2.3f, AnimRow.Sprint);
            t[(int)Clip.JumpDown] = O(1.8f, AnimRow.Vault); t[(int)Clip.ClimbOut] = O(0.53f, AnimRow.Vault); t[(int)Clip.ClimbHold] = L(1f, AnimRow.Vault); t[(int)Clip.ClimbLand] = O(0.67f, AnimRow.Vault); t[(int)Clip.ClimbLadder] = L(1.2f, AnimRow.Vault, 0.8f);
            t[(int)Clip.StandToKneel] = O(0.93f, AnimRow.CrouchWalk); t[(int)Clip.KneelToStand] = O(1.23f, AnimRow.CrouchWalk); t[(int)Clip.KneelToProne] = O(1.97f, AnimRow.ProneCrawl, Stance.Prone); t[(int)Clip.ProneToKneel] = O(1.97f, AnimRow.CrouchWalk, Stance.Prone);
            t[(int)Clip.StandToStoop] = O(1.4f, AnimRow.CrouchWalk); t[(int)Clip.StoopToStand] = O(1.5f, AnimRow.CrouchWalk); t[(int)Clip.StoopToKneel] = O(1.5f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.KneelToStoop] = O(1.5f, AnimRow.CrouchWalk, Stance.Crouch);
            t[(int)Clip.TakeCover] = O(2.3f, AnimRow.CrouchWalk); t[(int)Clip.Emerge] = O(2.3f, AnimRow.Walk);
            t[(int)Clip.Turn90L] = O(1.33f, AnimRow.Idle); t[(int)Clip.Turn90R] = O(1.5f, AnimRow.Idle); t[(int)Clip.Turn180] = O(1.9f, AnimRow.Idle); t[(int)Clip.KneelTurn90L] = O(1.3f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.KneelTurn90R] = O(1.3f, AnimRow.CrouchWalk, Stance.Crouch);
            t[(int)Clip.StoopTurn90L] = O(1.27f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.StoopTurn90R] = O(1.5f, AnimRow.CrouchWalk, Stance.Crouch); t[(int)Clip.StoopTurn180] = O(1.47f, AnimRow.CrouchWalk, Stance.Crouch);
            // the yaw each turn clip carries (measured by the bake; negative = left), so the drawn yaw can pick up where the clip leaves it
            t[(int)Clip.Turn90L].Turn = -1.34f; t[(int)Clip.Turn90R].Turn = 1.48f; t[(int)Clip.Turn180].Turn = math.PI; t[(int)Clip.KneelTurn90L].Turn = -1.57f; t[(int)Clip.KneelTurn90R].Turn = 1.57f;
            t[(int)Clip.StoopTurn90L].Turn = -1.57f; t[(int)Clip.StoopTurn90R].Turn = 1.47f; t[(int)Clip.StoopTurn180].Turn = math.PI;
            t[(int)Clip.DeathFront] = O(1.8f, AnimRow.Death0); t[(int)Clip.DeathBack] = O(1.8f, AnimRow.Death1); t[(int)Clip.DeathRight] = O(1.8f, AnimRow.Death2); t[(int)Clip.DeathLeft] = O(1.8f, AnimRow.Death3);
            t[(int)Clip.DeathHeadshot] = O(1.6f, AnimRow.Death1); t[(int)Clip.DeathWalking] = O(2f, AnimRow.Death0); t[(int)Clip.DeathRunning] = O(2.2f, AnimRow.Death0); t[(int)Clip.DeathKneel] = O(1.7f, AnimRow.Death2); t[(int)Clip.DeathSquat] = O(1.5f, AnimRow.Death2);
            t[(int)Clip.DeathProne] = O(1.5f, AnimRow.Death3); t[(int)Clip.DeathBlast] = O(1.9f, AnimRow.Death1);
            return t;
        }
    }

    /// <summary>Per slot, what the body is doing (docs/15 section 3).</summary>
    public struct AnimState
    {
        public Clip Clip, PrevClip;
        public float Frame, PrevFrame, Blend, Fade, Rate;   // seconds into the clip; the fade-out clip; blend left (s) of Fade; playback rate
        public Rung Rung;
        public byte Stance, WantStance;                 // animated stance, the stance the situation implies
        public float AimYaw, BodyYaw, ShownYaw, TurnTo;   // where he aims; where his feet point; what is drawn (eases to BodyYaw); the yaw a turn clip ends at
        public uint IdleSince, LastShot, LastHit, LastNearMiss, LastBlast, LastDuck, ClipStart;
        public ushort Shots, Generation;
        public uint Seed;
        public bool Aimed, Gassed, Down, Dead;          // rifle up; has met the gas; on the ground after a trip; death played
        public byte PrevLayer;
        public float Speed;                             // smoothed over the last few ticks: the separation push jolts a man for a tick
        public Clip Pending; public byte PendingTicks;  // a gait change waits until the new gait has held for a few ticks
        public uint LastClimb, LastTarget, TargetSince; // the last tick he had a target, and when he got it: a garrison waits a moment before rising or dropping back
        public byte Routine, Routines; public uint RoutineUntil;  // the trench routine in progress (see Decide), how many he has done, when its hold ends
    }

    public sealed class AnimationController : System.IDisposable
    {
        public NativeArray<AnimState> State;
        public NativeArray<ushort> Row;      // the Clip per slot (the renderer maps it to its atlas row)
        public NativeArray<float> Phase;     // 0..1 through the clip
        public NativeArray<float> Yaw;       // the yaw the body is drawn at
        public NativeArray<ushort> PrevRow;  // the clip fading out, where it stopped, and its weight (VATRenderer reads these)
        public NativeArray<float> PrevPhase, Blend;
        readonly int maxSlots;
        int count;
        uint tick; float tickSeconds = 0.05f;
        readonly MapData map;
        readonly TW.Sim.Combat.GasSmokeSystem gas;
        // this tick's latched events, per slot
        NativeArray<byte> hitKind;      // 0 none, 1 hit, 2 heavy hit, 3 near miss
        NativeArray<float3> hitDir;
        NativeArray<float> blastRadius; // > 0: a burst this near (distance stored in blastDist)
        NativeArray<float> blastDist;
        NativeArray<byte> shotThisTick, leftTrench, gasHere;
        NativeArray<int> shotAt;        // whom the shot went to (the sim clears TargetSlot on a kill the same tick)
        float3[] prevPos;

        // ---- trace: one man followed through his fight
        public int FollowSlot = -1;
        public struct TraceEntry { public uint Tick; public Rung Rung; public Clip Clip; public string Why; public float3 Pos; public byte Stance; public float Speed; public int Target; public float Suppression; }
        public readonly List<TraceEntry> Trace = new List<TraceEntry>(512);
        readonly Dictionary<Clip, float> timeIn = new Dictionary<Clip, float>();
        public string LastWhy = "";

        /// <param name="row">The presenter's row, phase and yaw arrays: the controller writes them, the presenter owns them.</param>
        public AnimationController(int maxSlots, MapData map, TW.Sim.Combat.GasSmokeSystem gas, NativeArray<ushort> row, NativeArray<float> phase, NativeArray<float> yaw)
        {
            this.maxSlots = maxSlots; this.map = map; this.gas = gas;
            State = new NativeArray<AnimState>(maxSlots, Allocator.Persistent);
            Row = row; Phase = phase; Yaw = yaw;
            PrevRow = new NativeArray<ushort>(maxSlots, Allocator.Persistent);
            PrevPhase = new NativeArray<float>(maxSlots, Allocator.Persistent);
            Blend = new NativeArray<float>(maxSlots, Allocator.Persistent);
            hitKind = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            hitDir = new NativeArray<float3>(maxSlots, Allocator.Persistent);
            blastRadius = new NativeArray<float>(maxSlots, Allocator.Persistent);
            blastDist = new NativeArray<float>(maxSlots, Allocator.Persistent);
            shotThisTick = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            leftTrench = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            gasHere = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            shotAt = new NativeArray<int>(maxSlots, Allocator.Persistent);
            prevPos = new float3[maxSlots];
        }

        public void Dispose()
        {
            State.Dispose(); PrevRow.Dispose(); PrevPhase.Dispose(); Blend.Dispose(); hitKind.Dispose(); blastRadius.Dispose(); blastDist.Dispose(); hitDir.Dispose();
            shotThisTick.Dispose(); leftTrench.Dispose(); gasHere.Dispose(); shotAt.Dispose();
        }

        // ------------------------------------------------------------------------------------------------------ tick
        /// <summary>Once per stepped sim tick, after the world has moved and before its events are cleared.</summary>
        public void Tick(SimWorld w)
        {
            tick = w.Tick; tickSeconds = w.Config.TickSeconds; count = w.HighWater;
            Latch(w);
            for (int i = 0; i < count; i++)
            {
                uint f = w.Flags[i];
                var s = State[i];
                if (s.Generation != w.Generation[i]) { s = Fresh(i, w); }
                if ((f & (uint)UnitFlags.Vehicle) != 0) { State[i] = s; Row[i] = (ushort)Clip.Idle; Yaw[i] = w.Yaw[i]; continue; }
                if ((f & (uint)UnitFlags.Alive) == 0)
                {
                    if (!s.Dead) { s = Die(i, s, w); }
                    State[i] = s; prevPos[i] = w.Position[i]; continue;
                }
                Decide(i, ref s, w);
                State[i] = s;
                prevPos[i] = w.Position[i];
            }
        }

        AnimState Fresh(int i, SimWorld w)
        {
            var s = new AnimState { Clip = Clip.Idle, Rung = Rung.Idle, Rate = 1f, Generation = w.Generation[i], Seed = (uint)i * 2654435761u ^ (uint)w.Generation[i] * 40503u, IdleSince = w.Tick, ClipStart = w.Tick };
            s.Stance = s.WantStance = w.StanceOf[i]; s.BodyYaw = s.AimYaw = s.ShownYaw = w.Yaw[i];
            prevPos[i] = w.Position[i];
            return s;
        }

        void Latch(SimWorld w)
        {
            for (int i = 0; i < count; i++) { hitKind[i] = 0; blastRadius[i] = 0f; shotThisTick[i] = 0; leftTrench[i] = 0; gasHere[i] = 0; }
            var ev = w.Events.Events;
            for (int k = 0; k < ev.Length; k++)
            {
                var e = ev[k];
                switch (e.Type)
                {
                    case SimEventType.Hit:
                        if (e.B >= 0 && e.B < count) { hitKind[e.B] = (byte)(e.Scalar > 0.4f * math.max(1f, w.MaxHp[e.B]) ? 2 : 1); hitDir[e.B] = e.Dir; }
                        break;
                    case SimEventType.NearMiss:
                        if (e.A >= 0 && e.A < count && hitKind[e.A] == 0) hitKind[e.A] = 3;
                        break;
                    case SimEventType.Shot:
                        if (e.A >= 0 && e.A < count && e.Scalar < 0.5f) { shotThisTick[e.A] = 1; shotAt[e.A] = e.B; }
                        break;
                    case SimEventType.UnitLeftTrench:
                        if (e.A >= 0 && e.A < count) leftTrench[e.A] = 1;
                        break;
                    case SimEventType.Explosion:
                    {
                        float reach = e.Scalar + 3f, reach2 = reach * reach;
                        for (int i = 0; i < count; i++)
                        {
                            if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                            float3 d = w.Position[i] - e.Pos; d.y = 0f; float dd = math.lengthsq(d);
                            if (dd < reach2 && (blastRadius[i] <= 0f || dd < blastDist[i] * blastDist[i])) { blastRadius[i] = e.Scalar; blastDist[i] = math.sqrt(dd); hitDir[i] = -math.normalizesafe(d, new float3(0, 0, 1)); }
                        }
                        break;
                    }
                }
            }
            if (gas != null && gas.Active)
                for (int i = 0; i < count; i++) if ((w.Flags[i] & (uint)UnitFlags.Alive) != 0 && gas.ConcentrationAt(w.Position[i]) > 6f) gasHere[i] = 1;
        }

        // ------------------------------------------------------------------------------------------------ the ladder
        bool Playing(in AnimState s) => !Clips.Table[(int)s.Clip].Loop && s.Frame < Clips.Table[(int)s.Clip].Seconds - 0.02f;
        float Left(in AnimState s) => Clips.Table[(int)s.Clip].Seconds - s.Frame;

        void Start(int i, ref AnimState s, Clip clip, Rung rung, string why, float rate = 1f, float fade = 0.15f)
        {
            if (s.Clip == clip && Clips.Table[(int)clip].Loop) { s.Rung = rung; s.Rate = rate; return; }   // same loop: keep the phase
            s.PrevClip = s.Clip; s.PrevFrame = s.Frame; s.Blend = s.Fade = fade;
            s.Clip = clip; s.Frame = 0f; s.Rate = rate; s.Rung = rung; s.ClipStart = tick;
            if (i == FollowSlot) Note(i, rung, clip, why);
        }

        void Note(int i, Rung rung, Clip clip, string why)
        {
            LastWhy = why;
            Trace.Add(new TraceEntry { Tick = tick, Rung = rung, Clip = clip, Why = why, Pos = lastPos, Stance = lastStance, Speed = lastSpeed, Target = lastTarget, Suppression = lastSupp });
        }
        float3 lastPos; byte lastStance; float lastSpeed; int lastTarget; float lastSupp;

        AnimState Die(int i, AnimState s, SimWorld w)
        {
            // stance first, then gait, then the direction the impulse came from against the body
            Clip clip;
            var st = (Stance)s.Stance;
            bool blast = blastRadius[i] > 0f;
            float speed = s.Speed;   // the smoothed speed: the sim kills before it moves him this tick
            if (st == Stance.Prone || st == Stance.Pinned) clip = Clip.DeathProne;
            else if (st == Stance.Crouch || st == Stance.FireStep) clip = (s.Seed & 4) != 0 ? Clip.DeathSquat : Clip.DeathKneel;
            else if (blast) clip = Clip.DeathBlast;
            else if (speed > 2.2f) clip = Clip.DeathRunning;
            else if (speed > 0.3f) clip = Clip.DeathWalking;
            else
            {
                float rel = Relative(hitDir[i], s.BodyYaw);
                clip = (s.Seed % 5) == 0 ? Clip.DeathHeadshot : math.abs(rel) < 0.79f ? Clip.DeathBack : math.abs(rel) > 2.36f ? Clip.DeathFront : rel > 0f ? Clip.DeathRight : Clip.DeathLeft;
            }
            Start(i, ref s, clip, Rung.Death, "killed: " + (blast ? "blast" : "shot") + ", " + st + (speed > 0.3f ? ", moving" : ""));
            s.Dead = true;
            return s;
        }

        /// <summary>The angle (rad, -pi..pi) between a world direction and the body's facing; 0 = the direction the body faces.</summary>
        static float Relative(float3 dir, float bodyYaw)
        {
            float a = math.atan2(dir.x, dir.z) - bodyYaw;
            while (a > math.PI) a -= 2f * math.PI; while (a < -math.PI) a += 2f * math.PI;
            return a;
        }

        void Decide(int i, ref AnimState s, SimWorld w)
        {
            float3 p = w.Position[i];
            float3 step = p - prevPos[i]; step.y = 0f;
            float raw = math.length(step) / tickSeconds;
            s.Speed = raw < 0.15f || s.Speed < 0.15f ? raw : math.lerp(s.Speed, raw, 0.3f);   // stops and starts at once, otherwise smoothed
            float speed = s.Speed;
            var simStance = (Stance)w.StanceOf[i];
            uint f = w.Flags[i];
            int target = w.TargetSlot[i];
            float supp = w.Suppression[i];
            byte arche = w.Archetype[i];
            bool inTrench = (f & (uint)UnitFlags.InTrench) != 0;
            int ncx = math.clamp((int)(p.x / MapData.NavCellSize), 0, map.NavWidth - 1), ncz = math.clamp((int)(p.z / MapData.NavCellSize), 0, map.NavLength - 1);
            byte layer = map.NavLayers[map.NavIndex(ncx, ncz)];
            bool mud = (layer & (byte)NavLayer.Mud) != 0, wire = (layer & (byte)NavLayer.Wire) != 0;
            float depth = map.WaterLevel > MapData.NoWater ? math.max(0f, map.WaterLevel - map.Height.Sample(p.x, p.z)) : 0f;
            if (i == FollowSlot) { lastPos = p; lastStance = (byte)simStance; lastSpeed = speed; lastTarget = target; lastSupp = supp; }

            // yaw: the body follows the heading when moving, aim follows the target; standing still with a target
            // within 60 degrees the feet simply come round (Advance eases the shown yaw); beyond that a turn clip plays
            if (speed > 0.15f) s.BodyYaw = math.atan2(step.x, step.z);
            int aimAt = target >= 0 ? target : shotThisTick[i] != 0 ? shotAt[i] : -1;
            if (aimAt >= 0 && aimAt < count) { float3 d = w.Position[aimAt] - p; s.AimYaw = math.atan2(d.x, d.z); }
            else if (speed > 0.15f) s.AimYaw = s.BodyYaw;

            // the stance the situation implies (Wanted): the sim's, damped against its target flicker and held by a routine
            byte want = (byte)simStance;
            if (simStance == Stance.Vault || simStance == Stance.FireStep || simStance == Stance.Sprint) want = (byte)Stance.Standing;
            if (arche == 3 && inTrench && want == (byte)Stance.Standing && simStance == Stance.FireStep) want = (byte)Stance.Crouch;   // the sniper fires from the knee
            s.WantStance = want;

            // a one-shot that a higher rung did not take keeps the body
            var cur = s.Clip; var info = Clips.Table[(int)cur];

            // ---- rung 2: the trench edge. Out: the push up, then the airborne hold while the sim lifts him, then the
            // landing once he is on the surface. In: the drop, which starts in the air and lands.
            bool climbing = cur == Clip.ClimbOut || cur == Clip.ClimbHold || cur == Clip.ClimbLadder;
            if (leftTrench[i] != 0 || simStance == Stance.Vault)
            {
                if (!climbing && tick - s.LastClimb > 40) { s.LastClimb = tick; Start(i, ref s, (layer & (byte)NavLayer.Link) != 0 ? Clip.ClimbLadder : Clip.ClimbOut, Rung.Trench, "climbing out of the trench"); }
                else if (cur == Clip.ClimbOut && !Playing(s)) Start(i, ref s, Clip.ClimbHold, Rung.Trench, "over the parapet");
                s.Stance = (byte)Stance.Standing; s.PrevLayer = (byte)NavLayer.Surface; s.Routine = 0; return;
            }
            if (climbing && !inTrench && (cur != Clip.ClimbOut || !Playing(s))) { Start(i, ref s, Clip.ClimbLand, Rung.Trench, "lands on the surface"); s.Stance = (byte)Stance.Standing; s.PrevLayer = (byte)NavLayer.Surface; return; }
            if (climbing && !inTrench) return;
            if (inTrench && s.PrevLayer == (byte)NavLayer.Surface && s.Clip != Clip.JumpDown && !climbing)
            {
                s.PrevLayer = (byte)NavLayer.Trench;
                Start(i, ref s, Clip.JumpDown, Rung.Trench, "dropping into the trench"); s.Stance = (byte)Stance.Standing; s.Routine = 0; return;
            }
            s.PrevLayer = inTrench ? (byte)NavLayer.Trench : (byte)NavLayer.Surface;
            if (s.Rung == Rung.Trench && Playing(s) && !(cur == Clip.JumpDown && speed > 0.5f && s.Frame > 0.9f)) return;   // a drop that lands running is cut short
            if (s.Rung == Rung.Trench && climbing && tick - s.LastClimb < 20 && speed < 3f) return;   // the ladder and the push-up hold while the sim lifts him

            // ---- rung 3: reactions (a hit interrupts anything below a death; a duck does not interrupt a hit)
            bool prone = simStance == Stance.Prone || simStance == Stance.Pinned;
            bool low = s.Stance == (byte)Stance.Crouch;
            bool hitClip = cur == Clip.HitHeavy || cur == Clip.HitStand || cur == Clip.HitWalk || cur == Clip.HitRun || cur == Clip.HitProne || cur == Clip.KneelFlinch;   // the animated stance: a man on one knee flinches and aims from the knee
            if (hitKind[i] == 1 || hitKind[i] == 2)
            {
                s.LastHit = tick; s.Routine = 0;
                Clip hit = prone ? Clip.HitProne : low ? Clip.KneelFlinch : hitKind[i] == 2 && speed < 0.3f ? Clip.HitHeavy : speed > 2.2f ? Clip.HitRun : speed > 0.3f ? Clip.HitWalk : Clip.HitStand;
                Start(i, ref s, hit, Rung.Reaction, "hit" + (hitKind[i] == 2 ? " hard" : "") + (speed > 0.3f ? " on the move" : "") + ", from " + Side(hitDir[i], s.BodyYaw));
                return;
            }
            if (s.Rung == Rung.Reaction && Playing(s) && hitClip && !(speed > 0.3f && cur != Clip.HitRun && Left(s) > 0.4f)) return;   // a hit the sim runs through is dropped
            if (blastRadius[i] > 0f && tick - s.LastBlast > 30)
            {
                s.LastBlast = tick; s.Routine = 0;
                float r = blastRadius[i], d = blastDist[i];
                if (prone) Start(i, ref s, Clip.ProneRoll, Rung.Reaction, "shell at " + d.ToString("0.0") + " m: rolls away");
                else if (d < r * 0.6f && !inTrench && arche <= 1) Start(i, ref s, Clip.DiveRoll, Rung.Reaction, "shell at " + d.ToString("0.0") + " m inside the burst: dives");
                else if (d < r * 0.6f || (inTrench && d < r)) { Start(i, ref s, Clip.Shield, Rung.Reaction, "shell at " + d.ToString("0.0") + " m" + (inTrench ? " on the trench" : " inside the burst") + ": shields the face"); s.Stance = (byte)Stance.Crouch; }   // the shield ends on a knee
                else Start(i, ref s, low ? Clip.KneelFlinch : Clip.Duck, Rung.Reaction, "shell at " + d.ToString("0.0") + " m: " + (low ? "flinches" : "ducks"));
                return;
            }
            if (s.Rung == Rung.Reaction && Playing(s) && !(speed > 0.3f && cur != Clip.Stumble && cur != Clip.HitRun && Left(s) > 0.4f && !s.Down)) return;
            if (hitKind[i] == 3 && speed < 0.3f && !prone && s.Routine != 6 && tick - s.LastDuck > (supp > 30f ? 20 : 60))
            {
                s.LastDuck = tick; s.LastNearMiss = tick;
                Start(i, ref s, low ? Clip.KneelFlinch : Clip.Duck, Rung.Reaction, "near miss, standing still: ducks");
                return;
            }
            if (prone && hitKind[i] == 3 && tick - s.LastDuck > 40) { s.LastDuck = tick; Start(i, ref s, Clip.ProneFlinch, Rung.Reaction, "near miss, prone: flinches"); return; }
            if (gasHere[i] != 0 && !s.Gassed && (f & (uint)UnitFlags.Masked) == 0) { s.Gassed = true; s.Routine = 0; Start(i, ref s, Clip.MaskOn, Rung.Reaction, "gas on his cell: mask on"); return; }
            if (gasHere[i] == 0 && s.Gassed && tick - s.ClipStart > 200) s.Gassed = false;
            if (s.Down)
            {
                if (cur == Clip.Trip && !Playing(s)) { Start(i, ref s, Clip.GetUp, Rung.Reaction, "gets up"); return; }
                if (cur == Clip.GetUp && Playing(s)) return;
                s.Down = false; s.Stance = (byte)Stance.Standing;
            }
            if (!prone && !inTrench && (wire || mud) && s.Rung == Rung.Locomotion && tick - s.LastBlast > 100 && Hash(s.Seed, tick) < 0.002f)
            {
                // 4 % a second in wire or mud: nearly stopped he goes down (Trip, then GetUp); at a run he stumbles and keeps going
                s.LastBlast = tick;
                if (speed < 0.7f) { s.Down = true; Start(i, ref s, Clip.Trip, Rung.Reaction, wire ? "caught in the wire: falls" : "stuck in the mud: falls"); return; }
                if (speed > 1.5f) { Start(i, ref s, Clip.Stumble, Rung.Reaction, wire ? "catches a foot in the wire: stumbles" : "stumbles in the mud"); return; }
            }

            // ---- rung 4: actions (the shot first: it cuts an aim-up, a flinch or a duck, never a hit)
            int magazine = arche == 1 ? 20 : arche == 2 ? 50 : 5;
            bool bolt = arche == 0 || arche == 3;   // the rifle and the sniper work a bolt after every shot
            if (shotThisTick[i] != 0) { s.Shots++; s.LastShot = tick; s.Aimed = true; }
            if (shotThisTick[i] != 0 && speed < 0.3f && !(s.Rung == Rung.Reaction && hitClip && Playing(s)))
            {
                s.Routine = 0;
                Clip fire = prone ? (arche == 2 ? Clip.FireMG : Clip.FireProne) : low ? Clip.FireKneel : arche == 2 ? Clip.FireStoop : Clip.FireStand;
                Start(i, ref s, fire, Rung.Fire, "fires at " + shotAt[i], 1f, 0.05f);
                return;
            }
            if (s.Rung == Rung.Action && Playing(s)) { if (speed > 0.3f && Left(s) > 0.4f && cur != Clip.Throw) { /* dropped: the run takes over below */ } else return; }
            byte wantNow = Wanted(ref s, w, i, simStance, inTrench, target, speed);
            if (bolt && cur == Clip.FireStand && !Playing(s) && s.Rung == Rung.Fire && speed < 0.3f && !prone && !low && s.Shots < magazine)
            { Start(i, ref s, Clip.ReloadBolt, Rung.Action, "works the bolt"); return; }   // kneeling there is no bolt clip: the shot's settle stands in
            if (target >= 0 && !s.Aimed && speed < 0.3f && !prone && s.Stance == wantNow) { s.Aimed = true; Start(i, ref s, low ? Clip.KneelAimUp : Clip.AimUp, Rung.Action, "target seen: rifle up"); return; }
            if (target < 0 && s.Aimed && tick - s.LastShot > 80 && tick - s.LastTarget > 80 && speed < 0.3f && !prone && s.Stance == wantNow) { s.Aimed = false; Start(i, ref s, low ? Clip.KneelAimDown : Clip.AimDown, Rung.Action, "target lost 4 s: rifle down"); return; }
            if (s.Shots >= magazine && shotThisTick[i] == 0 && speed < 0.3f && arche != 2 && w.FireCooldown[i] > 40 && !(s.Rung == Rung.Fire && Playing(s)) && !(bolt && Playing(s) && cur == Clip.ReloadBolt))
            {
                s.Shots = 0;
                Start(i, ref s, prone ? Clip.ReloadProne : low ? Clip.ReloadStoop : Clip.ReloadStand, Rung.Action, "magazine empty: reloads"); return;
            }

            // ---- rung 5: stance change (the animated stance lags the sim's by a transition); a moving man skips it, the
            // gait cross-fade does the job. A trench routine holds him where it put him until it ends.
            if (s.Rung == Rung.StanceChange && Playing(s) && !(shotThisTick[i] != 0 && s.Frame > 0.3f)) return;   // a shot cuts the transition short
            if (s.Rung == Rung.StanceChange && Playing(s)) { s.Stance = s.WantStance; }
            want = wantNow;
            if (s.Stance != want && speed < 0.15f)
            {
                var from = (Stance)s.Stance; var to = (Stance)want;
                bool fromProne = from == Stance.Prone || from == Stance.Pinned, toProne = to == Stance.Prone || to == Stance.Pinned;
                bool fromLow = from == Stance.Crouch, toLow = to == Stance.Crouch;
                s.Routine = 0;
                if (fromProne && !toProne) { Start(i, ref s, Clip.ProneToKneel, Rung.StanceChange, "stance " + from + " -> " + to + ": up to a knee"); s.Stance = (byte)Stance.Crouch; return; }
                if (!fromProne && toProne) { Start(i, ref s, fromLow ? Clip.KneelToProne : Clip.StandToKneel, Rung.StanceChange, "stance " + from + " -> " + to + ": " + (fromLow ? "down flat" : "down to a knee")); s.Stance = fromLow ? want : (byte)Stance.Crouch; return; }
                if (fromLow && !toLow) { Start(i, ref s, Clip.KneelToStand, Rung.StanceChange, "stance " + from + " -> " + to + (inTrench ? ": up to the parapet" : "")); s.Stance = want; return; }
                if (!fromLow && toLow) { Start(i, ref s, Clip.StandToKneel, Rung.StanceChange, "stance " + from + " -> " + to + (inTrench ? ": down behind the parapet" : "")); s.Stance = want; return; }
                s.Stance = want;
            }
            else if (s.Stance != want) s.Stance = want;

            // ---- rung 6: turn in place (the clip carries the rotation; the drawn yaw snaps to the new facing as it ends)
            if (s.Rung == Rung.Turn && Playing(s)) return;
            if (s.Rung == Rung.Turn) { s.ShownYaw += info.Turn; s.BodyYaw = s.TurnTo; }   // the clip turned him this far; Advance eases the rest
            if (speed < 0.15f && !prone && (target >= 0 || inTrench))
            {
                float faceTo = target >= 0 ? s.AimYaw : w.Yaw[i];   // no target: the way the sim faces him (the parapet)
                float turn = faceTo - s.BodyYaw; while (turn > math.PI) turn -= 2f * math.PI; while (turn < -math.PI) turn += 2f * math.PI;
                if (math.abs(turn) > 1.05f && target >= 0)
                {
                    Clip t = math.abs(turn) > 2.4f ? (low ? Clip.KneelTurn90R : Clip.Turn180) : low ? (turn > 0f ? Clip.KneelTurn90R : Clip.KneelTurn90L) : turn > 0f ? Clip.Turn90R : Clip.Turn90L;
                    if (t == Clip.KneelTurn90R && turn < 0f) t = Clip.KneelTurn90L;
                    Start(i, ref s, t, Rung.Turn, "turns " + (int)math.degrees(turn) + " deg to the target");
                    s.TurnTo = s.AimYaw; s.Routine = 0; return;
                }
                s.BodyYaw = faceTo;   // within 60 degrees (or with no target) the feet come round without a clip
            }

            // ---- rung 7: locomotion
            if (speed > 0.15f)
            {
                s.IdleSince = tick;
                float rel = target >= 0 ? Relative(step, s.AimYaw) : 0f;
                bool firing = target >= 0 && w.FireCooldown[i] < 30;
                Clip gait; float authored;
                string env = "";
                s.Routine = 0;
                if (prone) gait = math.abs(rel) > 2.27f ? Clip.CrawlBack : Clip.Crawl;
                else if (depth > 0.5f) { gait = Clip.Wade; env = " wading"; }
                else if (wire && speed < 2.2f) { gait = Clip.WireCross; env = " through the wire"; }
                else if (inTrench || simStance == Stance.Crouch) gait = speed > 1.8f ? Clip.CrouchRun : supp > 40f ? Clip.StoopLow : Clip.StoopWalk;
                else if (arche == 2 && speed < 2.2f && target < 0) { gait = Clip.MGCarry; env = " carrying the gun"; }
                else if ((simStance == Stance.Sprint && speed > 2.2f) || speed > 3.6f) gait = firing && arche == 1 ? Clip.FireSprint : Clip.Sprint;
                else if (speed > 2.2f) gait = math.abs(rel) > 2.27f && arche <= 1 ? Clip.RunBack : firing && math.abs(rel) < 0.87f ? Clip.FireRun : Clip.Run;
                else
                {
                    if (target >= 0 && math.abs(rel) > 2.27f) gait = Clip.WalkBack;
                    else if (target >= 0 && math.abs(rel) > 0.87f) gait = rel > 0f ? Clip.WalkRight : Clip.WalkLeft;
                    else if (firing && target >= 0) gait = Clip.FireWalk;
                    else if (depth > 0.15f || mud || supp > 20f || SceneMood.Night) { gait = Clip.WalkWary; env = depth > 0.15f ? " in water" : mud ? " in mud" : supp > 20f ? " under fire" : " at night"; }
                    else gait = target >= 0 ? Clip.WalkAimed : Clip.Walk;
                }
                // a gait change waits three ticks, so a jolt does not flip the cycle; a start from rest does not wait
                if (s.Rung == Rung.Locomotion && gait != s.Clip)
                {
                    if (gait == s.Pending) s.PendingTicks++; else { s.Pending = gait; s.PendingTicks = 1; }
                    if (s.PendingTicks < 3) gait = s.Clip;
                }
                authored = Clips.Table[(int)gait].Speed;
                float rate = authored > 0f ? math.clamp(speed / authored, 0.6f, 1.6f) : 1f;
                if (wire && !prone && gait != Clip.WireCross) rate = math.min(rate, 0.6f);
                Start(i, ref s, gait, Rung.Locomotion, "moves at " + speed.ToString("0.0") + " m/s: " + gait + env, rate);
                s.Stance = simStance == Stance.Crouch ? (byte)Stance.Crouch : prone ? (byte)simStance : (byte)Stance.Standing;   // the gait sets the stance, no transition needed
                return;
            }

            // ---- rung 8: fire, standing still
            if (s.Rung == Rung.Fire && (Playing(s) || ((cur == Clip.FireMG || cur == Clip.FireStoop) && w.FireCooldown[i] < 6))) return;

            // ---- rung 9: idle
            if (s.Rung == Rung.Idle && Playing(s)) return;   // a fidget or a routine's transition
            if (s.Rung != Rung.Idle) { s.IdleSince = tick; s.Routine = 0; }
            uint idleFor = tick - s.IdleSince;
            bool aimed = s.Aimed || target >= 0;
            if (inTrench && !prone && target < 0 && TrenchRoutine(i, ref s, idleFor, supp)) return;
            Clip idle = prone ? Clip.ProneIdle : low ? (aimed ? Clip.KneelAimedIdle : Clip.KneelIdle) : aimed ? Clip.AimedIdle : Clip.ReadyIdle;
            if (idle == Clip.ReadyIdle && idleFor > 240 + (s.Seed % 360) && target < 0 && supp < 10f)
            {
                s.IdleSince = tick;
                uint pick = ((s.Seed >> 3) + s.Routines++) % 4;
                Clip fidget = pick == 0 ? Clip.FidgetInspect : pick == 1 ? Clip.FidgetCollar : pick == 2 ? Clip.FidgetLookAround : SceneMood.Night ? Clip.FidgetInspect : Clip.FidgetCheckShoe;
                Start(i, ref s, fidget, Rung.Idle, "idle " + (idleFor * tickSeconds).ToString("0") + " s out of contact: fidgets"); return;
            }
            if (s.Clip == idle && !Clips.Table[(int)idle].Loop) { s.Rung = Rung.Idle; return; }   // a held pose (the kneeling aim) stays held
            Start(i, ref s, idle, Rung.Idle, "idle: " + idle);
        }

        // Routines: 1 peek rising, 2 peek looking over, 3 peek dropping, 4 squat shift, 5 kneeling fidget, 6 under fire (shielding).
        /// <summary>
        /// A man waiting in a trench between contacts. His base is one knee down behind the parapet (KneelIdle). Every
        /// 6 to 18 s (by seed) he does one thing a man bracing for a fight does: rises to look over the parapet for a
        /// couple of seconds and drops back; shifts to a squat for a while and back to the knee; rubs his eyes (night)
        /// or settles his kit. Under fire (Suppression over 30) he only crouches low and flinches. Returns true when
        /// it started a clip this tick.
        /// </summary>
        bool TrenchRoutine(int i, ref AnimState s, uint idleFor, float supp)
        {
            bool held = s.Routine != 0 && tick < s.RoutineUntil;
            switch (s.Routine)
            {
                case 1: if (!Playing(s)) { s.Routine = 2; s.RoutineUntil = tick + 30 + (s.Seed >> 5) % 40; Start(i, ref s, Clip.AimedIdle, Rung.Idle, "looks over the parapet"); } return true;
                case 2: if (held || tick - s.LastTarget < 50) { Start(i, ref s, Clip.AimedIdle, Rung.Idle, "looks over the parapet"); return true; }
                        s.Routine = 3; s.Stance = (byte)Stance.Crouch; Start(i, ref s, Clip.StandToKneel, Rung.Idle, "drops back behind the parapet"); return true;
                case 3: if (Playing(s)) return true; s.Routine = 0; s.IdleSince = tick; s.RoutineUntil = tick + 100 + (uint)(Hash(s.Seed, tick + 5) * 400f); break;
                case 4: if (held) { Start(i, ref s, Clip.StoopIdle, Rung.Idle, "shifts to a squat", 1f, 0.5f); return true; } s.Routine = 0; s.IdleSince = tick; s.RoutineUntil = tick + 100 + (uint)(Hash(s.Seed, tick + 5) * 400f); break;
                case 5: if (Playing(s)) return true; s.Routine = 0; s.IdleSince = tick; s.RoutineUntil = tick + 100 + (uint)(Hash(s.Seed, tick + 5) * 400f); break;
                case 6: if (supp > 20f) { if (s.Clip != Clip.Shield) Start(i, ref s, Clip.Shield, Rung.Idle, "keeps his head down under fire", 1f, 0.3f); return true; } s.Routine = 0; s.IdleSince = tick; s.RoutineUntil = tick + 60; break;
            }
            if (supp > 30f) { s.Routine = 6; s.Stance = (byte)Stance.Crouch; Start(i, ref s, Clip.Shield, Rung.Idle, "under fire: arm over the face, head down", 1f, 0.3f); return true; }   // held on its last frame, kneeling
            if (s.Stance != (byte)Stance.Crouch) return false;   // standing in a trench: the stance rung takes him down first
            // every 5 to 25 s, rolled fresh each time (a fixed period made the line a row of cuckoo clocks); a third of the beats do nothing
            if (s.RoutineUntil == 0 || s.RoutineUntil > tick + 600) s.RoutineUntil = tick + 100 + (uint)(Hash(s.Seed, tick) * 400f);
            if (idleFor < 100 || tick < s.RoutineUntil) return false;
            s.IdleSince = tick; s.RoutineUntil = tick + 100 + (uint)(Hash(s.Seed, tick + 7) * 400f);
            float pick = Hash(s.Seed, tick + 3); s.Routines++;
            if (pick < 0.35f) return false;   // nothing this beat: he just waits
            if (pick < 0.62f) { s.Routine = 1; s.Stance = (byte)Stance.Standing; Start(i, ref s, Clip.KneelToStand, Rung.Idle, "idle " + (idleFor * tickSeconds).ToString("0") + " s: rises to look over the parapet"); return true; }
            if (pick < 0.84f) { s.Routine = 4; s.RoutineUntil = tick + 80 + (uint)(Hash(s.Seed, tick + 11) * 160f); Start(i, ref s, Clip.StoopIdle, Rung.Idle, "idle " + (idleFor * tickSeconds).ToString("0") + " s: shifts to a squat", 1f, 0.5f); return true; }
            s.Routine = 5; Start(i, ref s, Clip.FidgetRubEyes, Rung.Idle, "idle " + (idleFor * tickSeconds).ToString("0") + " s: " + (SceneMood.Night ? "rubs his eyes" : "settles his kit")); return true;
        }

        /// <summary>
        /// The stance he should be moving towards this tick: the sim's (WantStance), held by a trench routine, and damped
        /// against the sim's target flicker: a garrison man whose target has just dropped out of sight holds the parapet a
        /// couple of seconds before kneeling, and one who has just seen a target takes half a second before rising to it.
        /// </summary>
        byte Wanted(ref AnimState s, SimWorld w, int i, Stance simStance, bool inTrench, int target, float speed)
        {
            byte want = s.WantStance;
            if (target >= 0) { if (tick - s.LastTarget > 1) s.TargetSince = tick; s.LastTarget = tick; }
            if (s.Routine != 0 && speed < 0.15f && target < 0) return s.Stance;
            if (inTrench && s.Stance == (byte)Stance.Standing && want == (byte)Stance.Crouch && target < 0 && tick - s.LastTarget < 50) return s.Stance;
            if (inTrench && s.Stance == (byte)Stance.Crouch && want == (byte)Stance.Standing && target >= 0 && tick - s.TargetSince < 10) return s.Stance;
            return want;
        }

        static string Side(float3 dir, float bodyYaw)
        {
            float rel = Relative(dir, bodyYaw);
            return math.abs(rel) < 0.79f ? "behind" : math.abs(rel) > 2.36f ? "the front" : rel > 0f ? "the right" : "the left";
        }

        static float Hash(uint seed, uint tick) { uint h = seed ^ (tick * 2246822519u); h ^= h >> 13; h *= 3266489917u; h ^= h >> 16; return (h & 0xFFFFFF) / 16777216f; }

        // ---------------------------------------------------------------------------------------------- render frame
        /// <summary>Move every frame on, close the cross-fades, end the one-shots; then the stand-in row and phase per slot.</summary>
        public void Advance(float dt)
        {
            for (int i = 0; i < count; i++)
            {
                var s = State[i];
                var info = Clips.Table[(int)s.Clip];
                if (s.Clip == Clip.None) { Row[i] = (ushort)Clip.Idle; Phase[i] = 0f; Blend[i] = 0f; continue; }
                s.Frame += dt * s.Rate;
                if (info.Loop) { if (s.Frame >= info.Seconds) s.Frame -= info.Seconds * math.floor(s.Frame / info.Seconds); }
                else if (s.Frame > info.Seconds) s.Frame = info.Seconds;
                if (s.Blend > 0f) s.Blend -= dt;
                // the feet come round to the body yaw at a turn's pace; a turn clip holds the old facing until it ends
                if (s.Rung != Rung.Turn || Clips.Table[(int)s.Clip].Loop)
                {
                    float d = s.BodyYaw - s.ShownYaw; while (d > math.PI) d -= 2f * math.PI; while (d < -math.PI) d += 2f * math.PI;
                    float step = math.min(math.abs(d), dt * (s.Rung == Rung.Locomotion ? 7f : 3f));
                    s.ShownYaw += math.sign(d) * step;
                    while (s.ShownYaw > math.PI) s.ShownYaw -= 2f * math.PI; while (s.ShownYaw < -math.PI) s.ShownYaw += 2f * math.PI;
                }
                if (i == FollowSlot) { timeIn.TryGetValue(s.Clip, out float t); timeIn[s.Clip] = t + dt; }
                State[i] = s;
                Row[i] = (ushort)s.Clip;
                float ph = info.Seconds > 0f ? s.Frame / info.Seconds : 0f;
                Phase[i] = info.Loop ? math.frac(ph) : math.min(ph, 1f);
                Yaw[i] = s.ShownYaw;
                var prev = Clips.Table[(int)s.PrevClip];
                PrevRow[i] = (ushort)s.PrevClip;
                float pp = prev.Seconds > 0f ? s.PrevFrame / prev.Seconds : 0f;
                PrevPhase[i] = prev.Loop ? math.frac(pp) : math.min(pp, 1f);
                Blend[i] = s.Blend > 0f && s.Fade > 0f ? math.saturate(s.Blend / s.Fade) : 0f;
            }
        }

        // ----------------------------------------------------------------------------------------------------- trace
        public void Follow(int slot) { FollowSlot = slot; Trace.Clear(); timeIn.Clear(); }

        public string TraceText(int max = 400)
        {
            var sb = new StringBuilder();
            sb.AppendLine("tick   t(s)   rung          clip              stance    speed  tgt   supp  reason");
            int from = math.max(0, Trace.Count - max);
            for (int k = from; k < Trace.Count; k++)
            {
                var e = Trace[k];
                sb.AppendLine(string.Format("{0,-6} {1,6:0.00} {2,-13} {3,-17} {4,-9} {5,5:0.0} {6,4} {7,6:0} {8}", e.Tick, e.Tick * tickSeconds, e.Rung, e.Clip, (Stance)e.Stance, e.Speed, e.Target, e.Suppression, e.Why));
            }
            sb.AppendLine();
            sb.AppendLine("time in each clip (s):");
            var keys = new List<Clip>(timeIn.Keys); keys.Sort((a, b) => timeIn[b].CompareTo(timeIn[a]));
            foreach (var k in keys) sb.AppendLine(string.Format("  {0,-17} {1,7:0.0}", k, timeIn[k]));
            return sb.ToString();
        }
    }
}
