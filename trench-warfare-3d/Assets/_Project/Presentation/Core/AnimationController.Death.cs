// Phase: B5 / docs/21 phase 4 (implemented) — part of AnimationController: how a man dies, as the picture shows it.
// The sim's Death event says what killed him (DeathCause when no slot did) and how hard a blast threw him (dir,
// scalar); Latch keeps it per slot for the tick and Die reads it. Every death is written to a ring the effects read
// back (TryDeath): events are dispatched once per render frame after every tick of the frame has run, and by then
// the slot may hold another man, so State[slot] is not the dead man's. A second ring of recent deaths (where, when,
// which standing clip) gives the density round a death: a burst that drops five men in a bay throws each of them
// harder than the one before, and two men who die standing next to each other do not die the same way.
// The ladder: alight (or a beam) -> he drops mid-stride and burns; gas -> down on his knees; under a track or a claw
// -> flat and hard; a blast close enough to throw him -> DeathThrown along the sim's knock; then his stance, his gait,
// and the side the shot came from. DeathBurning, DeathGas, DeathCrushed, DeathStagger, DeathWalking2 and
// DeathBackHeadshot are the clips the next bake adds (docs/21 D1); until then the nearest clip in the atlas stands in.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    /// <summary>One man's death as the controller decided it, kept for the effects (CombatFx reads it by slot and tick).</summary>
    public struct DeathRecord
    {
        public int Slot; public ushort Generation; public uint Tick;
        public Clip Clip, PrevClip; public float PrevFrame, Fade;
        public float Yaw, ThrowX, ThrowZ, ThrowUp, Grime;
        /// <summary>Char: 0..3 (VatPad). Density: how many died within PileRadius in the last PileWindow ticks. Cause: a DeathKind.</summary>
        public byte Char, Density, Cause;
        public bool Valid;
    }

    /// <summary>DeathRecord.Cause.</summary>
    public enum DeathKind : byte { Shot = 0, Blast = 1, Gas = 2, Crushed = 3, Burning = 4, Beam = 5 }

    public sealed partial class AnimationController
    {
        /// <summary>Deaths within this many metres and ticks of each other count as one heap.</summary>
        public const float PileRadius = 4f; public const int PileWindow = 12;
        /// <summary>Each man already dead in the heap throws the next this much further and higher, up to DensityCap men.</summary>
        public const float DensityPerMan = 0.35f; public const int DensityCap = 5;
        /// <summary>No corpse flies further or higher than this, whatever the heap.</summary>
        public const float FarCap = 14f, HighCap = 9f;
        /// <summary>m/s of the sim's knock at which a blast death is thrown as far as it goes (BlastSystem throws the dead at up to 18).</summary>
        public const float FullThrowKnock = 12f;
        /// <summary>A standing death is not repeated within this many metres and ticks of the same one.</summary>
        public const float RepeatRadius = 6f; public const int RepeatWindow = 20;
        public const int RecentDeaths = 64, DeathRingSize = 256;

        // this tick's Death events, per slot
        NativeArray<byte> died;          // 1: a Death event for the slot this tick
        NativeArray<int> deathB;         // the event's b: the killer's slot, or a DeathCause below zero
        NativeArray<uint> deathTick;     // the event's tick (the sim's, one behind the controller's on the host)
        NativeArray<float3> deathDir;    // dir: a blast's knock (y = 1), a shot's line
        NativeArray<float> deathSpeed;   // scalar: the knock, m/s
        /// <summary>0..3 how burned a man is drawn (VatPad bits 22-23): 1 a man who has been alight and lives, 3 a man who died alight.</summary>
        public NativeArray<byte> Char;
        NativeArray<float4> recent;      // x, z, tick, the standing clip as a float (-1 none): the last RecentDeaths deaths
        int recentCursor;
        NativeArray<DeathRecord> ring;
        int ringCursor;

        void AllocateDeaths(int maxSlots)
        {
            died = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            deathB = new NativeArray<int>(maxSlots, Allocator.Persistent);
            deathTick = new NativeArray<uint>(maxSlots, Allocator.Persistent);
            deathDir = new NativeArray<float3>(maxSlots, Allocator.Persistent);
            deathSpeed = new NativeArray<float>(maxSlots, Allocator.Persistent);
            Char = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            recent = new NativeArray<float4>(RecentDeaths, Allocator.Persistent);
            for (int k = 0; k < RecentDeaths; k++) recent[k] = new float4(0f, 0f, -1e9f, -1f);
            ring = new NativeArray<DeathRecord>(DeathRingSize, Allocator.Persistent);
        }

        void DisposeDeaths()
        {
            died.Dispose(); deathB.Dispose(); deathTick.Dispose(); deathDir.Dispose(); deathSpeed.Dispose(); Char.Dispose(); recent.Dispose(); ring.Dispose();
        }

        void LatchDeath(in SimEvent e)
        {
            if (e.A < 0 || e.A >= count) return;
            died[e.A] = 1; deathB[e.A] = e.B; deathTick[e.A] = e.Tick; deathDir[e.A] = e.Dir; deathSpeed[e.A] = e.Scalar;
        }

        /// <summary>The record of the death of the man who was in the slot on that tick (the Death event's), while the
        /// ring still holds it. False for a death the controller never saw (no controller, an overrun).</summary>
        public bool TryDeath(int slot, uint atTick, out DeathRecord record)
        {
            if (ring.IsCreated)
                for (int k = 0; k < DeathRingSize; k++)
                {
                    var r = ring[((ringCursor - 1 - k) % DeathRingSize + DeathRingSize) % DeathRingSize];
                    if (!r.Valid) break;   // filled in order: past the first empty entry there is nothing older
                    if (r.Slot == slot && r.Tick == atTick) { record = r; return true; }
                }
            record = default; return false;
        }

        /// <summary>The number of deaths within PileRadius of the point in the last PileWindow ticks.</summary>
        public int Density(float3 p, uint at)
        {
            int n = 0;
            float r2 = PileRadius * PileRadius;
            for (int k = 0; k < RecentDeaths; k++)
            {
                var r = recent[k];
                if (at - r.z > PileWindow || r.z > at) continue;
                float dx = r.x - p.x, dz = r.y - p.z;
                if (dx * dx + dz * dz <= r2) n++;
            }
            return n;
        }

        /// <summary>The standing death played nearest in time within RepeatRadius and RepeatWindow, or -1.</summary>
        int LastStandingNear(float3 p, uint at)
        {
            int best = -1; float bestTick = -1e9f;
            float r2 = RepeatRadius * RepeatRadius;
            for (int k = 0; k < RecentDeaths; k++)
            {
                var r = recent[k];
                if (r.w < 0f || at - r.z > RepeatWindow || r.z > at || r.z < bestTick) continue;
                float dx = r.x - p.x, dz = r.y - p.z;
                if (dx * dx + dz * dz <= r2) { best = (int)r.w; bestTick = r.z; }
            }
            return best;
        }

        void Remember(float3 p, uint at, int standingClip)
        {
            recent[recentCursor] = new float4(p.x, p.z, at, standingClip);
            recentCursor = (recentCursor + 1) % RecentDeaths;
        }

        static bool IsStandingDeath(Clip c) => c == Clip.DeathFront || c == Clip.DeathBack || c == Clip.DeathRight || c == Clip.DeathLeft || c == Clip.DeathHeadshot;

        AnimState Die(int i, AnimState s, SimWorld w)
        {
            bool latched = died[i] != 0;
            int b = latched ? deathB[i] : -1;
            float knock = latched ? deathSpeed[i] : 0f;
            float3 simDir = latched ? deathDir[i] : float3.zero;
            uint diedAt = latched ? deathTick[i] : tick;
            float3 p = w.Position[i];
            // how many went down round him in the last moments: a bay under a shell goes down as a heap, and each man
            // in it is thrown harder than the one before
            int density = Density(p, tick);
            float densityScale = 1f + DensityPerMan * math.min(density, DensityCap);

            var st = (Stance)s.Stance;
            bool flat = st == Stance.Prone || st == Stance.Pinned, low = st == Stance.Crouch || st == Stance.FireStep;
            bool blast = blastRadius[i] > 0f || b == (int)DeathCause.Blast;
            bool burning = b == (int)DeathCause.Burning || s.AlightUntil > tick;
            bool beam = b == (int)DeathCause.Beam;
            bool gassed = b == (int)DeathCause.Gas;
            bool crushed = b >= 0 && b < w.HighWater && (w.Flags[b] & (uint)UnitFlags.Vehicle) != 0;   // a track or a claw: the killer is the vehicle
            float speed = s.Speed;   // the smoothed speed: the sim kills before it moves him this tick
            s.ThrowX = s.ThrowZ = s.ThrowUp = 0f;
            byte chr = Char[i]; var cause = DeathKind.Shot;
            Clip clip;
            if (burning || beam)
            {
                // he was running in flames: he drops mid-stride and burns where he lies (DeathBurning, the run slowed
                // into a fold, comes with the next bake; the run-to-dying clip is the fall until then)
                clip = Clip.DeathRunning; chr = 3; cause = beam ? DeathKind.Beam : DeathKind.Burning;
                s.Stance = (byte)Stance.Standing;
            }
            else if (gassed) { clip = flat ? Clip.DeathProne : Clip.DeathKneel; cause = DeathKind.Gas; }        // choking: to his knees, then over (DeathGas with the bake)
            else if (crushed) { clip = flat ? Clip.DeathProne : Clip.DeathBlast; cause = DeathKind.Crushed; }   // flat and hard (DeathCrushed with the bake)
            else
            {
                // a heavy shell inside four fifths of its radius throws him through the air, whatever his stance: he goes up
                // facing it and comes down on his back, further and higher the closer it was (the corpse's arc: VATRenderer).
                // The sim's knock says how hard when it gives one; the burst it latched says the rest
                float closeness = blast && blastRadius[i] >= 4f ? math.saturate(1.15f - blastDist[i] / (0.8f * blastRadius[i])) : 0f;
                if (knock > 0f) closeness = math.max(closeness, math.saturate(knock / FullThrowKnock));
                if (closeness > 0f)
                {
                    // which way: the sim's knock (it leans with the shell's flight) when it gave one, else away from the burst
                    float3 away = new float3(simDir.x, 0f, simDir.z);
                    float3 toward = math.lengthsq(away) > 1e-4f ? -math.normalize(away) : hitDir[i];
                    toward.y = 0f; toward = math.normalizesafe(toward, new float3(0f, 0f, 1f));
                    float jitter = 0.8f + 0.4f * Hash(s.Seed, tick + 31);
                    float far = math.lerp(1.8f, 7.5f, closeness) * jitter * densityScale, high = math.lerp(1.4f, 6.0f, closeness) * jitter * densityScale;
                    bool dug = s.PrevLayer == (byte)NavLayer.Trench;   // Despawn cleared his flags: the layer he was drawn on says
                    if (flat) { far *= 0.5f; high *= 0.5f; }
                    if (dug) { far *= 0.25f; high *= 0.8f; }   // in a trench he goes up, not out
                    far = math.min(far, FarCap); high = math.min(high, HighCap);
                    s.ThrowX = -toward.x * far; s.ThrowZ = -toward.z * far; s.ThrowUp = high;
                    s.ShownYaw = s.BodyYaw = math.atan2(toward.x, toward.z);   // facing the burst: it throws him backwards
                    clip = Clip.DeathThrown; cause = DeathKind.Blast;
                }
                else if (flat) { clip = Clip.DeathProne; cause = blast ? DeathKind.Blast : DeathKind.Shot; }
                else if (low) { clip = (s.Seed & 4) != 0 ? Clip.DeathSquat : Clip.DeathKneel; cause = blast ? DeathKind.Blast : DeathKind.Shot; }
                else if (blast) { clip = Clip.DeathBlast; cause = DeathKind.Blast; }
                else if (speed > 2.2f) clip = Clip.DeathRunning;
                else if (speed > 0.3f) clip = Clip.DeathWalking;
                else clip = StandingDeath(i, in s, p, hitKind[i] != 0 ? hitDir[i] : simDir);
            }
            Start(i, ref s, clip, Rung.Death, (i == FollowSlot ? "killed: " + cause + (s.ThrowUp > 0f ? ", thrown " + math.sqrt(s.ThrowX * s.ThrowX + s.ThrowZ * s.ThrowZ).ToString("0.0") + " m, " + s.ThrowUp.ToString("0.0") + " m up" : "") + ", " + st + (speed > 0.3f ? ", moving" : "") + (density > 0 ? ", " + density + " down beside him" : "") : null));
            s.Dead = true;
            Char[i] = chr;
            Remember(p, tick, IsStandingDeath(clip) ? (int)clip : -1);
            ring[ringCursor] = new DeathRecord
            {
                Slot = i, Generation = s.Generation, Tick = diedAt, Clip = clip, PrevClip = s.PrevClip, PrevFrame = s.PrevFrame, Fade = s.Fade,
                Yaw = s.ShownYaw, ThrowX = s.ThrowX, ThrowZ = s.ThrowZ, ThrowUp = s.ThrowUp, Grime = Grime[i],
                Char = chr, Density = (byte)math.min(density, 255), Cause = (byte)cause, Valid = true,
            };
            ringCursor = (ringCursor + 1) % DeathRingSize;
            return s;
        }

        /// <summary>A man shot standing still: by the side it came from, one in five a headshot; and never the same
        /// death as the man beside him a moment ago.</summary>
        Clip StandingDeath(int i, in AnimState s, float3 p, float3 from)
        {
            float rel = Relative(from, s.BodyYaw);
            Clip side = math.abs(rel) < 0.79f ? Clip.DeathBack : math.abs(rel) > 2.36f ? Clip.DeathFront : rel > 0f ? Clip.DeathRight : Clip.DeathLeft;
            Clip first = (s.Seed % 5) == 0 ? Clip.DeathHeadshot : side;
            int last = LastStandingNear(p, tick);
            if (last < 0 || last != (int)first) return first;
            Clip second = first == Clip.DeathHeadshot ? side : Hash(s.Seed, tick + 97) < 0.5f ? Clip.DeathHeadshot : rel > 0f ? Clip.DeathRight : Clip.DeathLeft;
            if (second == first) second = first == Clip.DeathRight ? Clip.DeathLeft : Clip.DeathRight;
            return second;
        }
    }
}
