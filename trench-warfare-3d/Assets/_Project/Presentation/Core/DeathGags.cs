// Phase: deaths (2026-09-28, implemented) — the absurd deaths (owner, 2026-09-28: "lots of the fun arrives as units
// die, we need to make this more absurd"): slapstick on top of the death the controller's ladder already chose. A shot
// man is punted off his feet, a machine gun jigs him first, a headshot pops his helmet sky-high, a shell rockets him
// through several flips and bounces, a heap of them fountains, a track makes a pancake of him, a claw lifts and flings
// him, gas topples him stiff as a plank, fire sends him skidding, the beam leaves only his boots, and a shot frog now and
// then blows up and whizzes off like a balloon (2026-10-10).
//
// How absurd is one knob, fx.deathAbsurd (Knobs): 0 is exactly today's deaths (Choose returns no gag and nothing
// downstream changes), 1 the look the owner signs off, 2 ludicrous. Numbers scale from today's towards the table's with
// it, and every dice is a hash of the man's seed and the sim's tick, so a replay and a capture die the same way
// (decisions.md 2026-09-26: presentation only, seeded). The caps are applied last, whatever the intensity.
using Unity.Mathematics;
using TW.Sim;

namespace TW.Presentation
{
    public enum DeathGag : byte { None, Punt, Flop, HeadPop, Jig, Rocket, Fountain, Pancake, Flung, Wilt, Plank, Skid, Boots, Balloon }

    /// <summary>GagPlan.Flags.</summary>
    public static class GagFlags
    {
        /// <summary>His helmet flies straight up (a headshot).</summary>
        public const byte HelmetPop = 1;
        /// <summary>No corpse is laid down: what is left of him is thrown by CombatFx (the beam's boots).</summary>
        public const byte NoCorpse = 2;
        /// <summary>His head may come off with the helmet, as GORE allows (CombatFx rolls Dice).</summary>
        public const byte HeadOff = 4;
        /// <summary>He turns and squashes about his feet, not his middle (a plank, a pancake).</summary>
        public const byte Feet = 8;
        /// <summary>He leaves a splat where he lies (a pancake): CombatFx paints it, GORE willing.</summary>
        public const byte Splat = 16;
        /// <summary>His clip is held on its first frame (a man going over stiff as a plank).</summary>
        public const byte Freeze = 32;
        /// <summary>His clip plays from the death, not from the launch (a balloon: the swell IS the delay, so waiting for
        /// the launch would blow his sac up in the air, after he let go).</summary>
        public const byte PlayAtDeath = 64;
    }

    /// <summary>One death's gag: what the controller decided, carried in the DeathRecord to CombatFx and the renderer.</summary>
    public struct GagPlan
    {
        public DeathGag Gag;
        public byte Flags, Flips, Rolls, Bounces, Topple;
        /// <summary>A balloon's hops (FallenFlight.MakeHops, 0 for every other gag) and how far each one turns off
        /// the last, in radians, signed: positive he goes left then right, negative right then left.</summary>
        public byte Hops; public float HopTurn;
        /// <summary>A squash pulse at the start and the squash he lies at, as VatTint codes (height 1 + q x 0.0275).</summary>
        public sbyte PulseQ, RestQ;
        /// <summary>Seconds before the launch, metres a claw lifts him meanwhile, the jig's yaw (radians), metres of skid
        /// and its bearing, seconds of the plank's topple and the pulse, the clip's rate (0 = the renderer's own), a
        /// multiple of the flight's yaw spin, the gore dice (0..1) and the intensity it was chosen at.</summary>
        public float Delay, HoldLift, Jig, Skid, SkidYaw, ToppleDur, PulseDur, ClipRate, SpinScale, Dice, Intensity;
        public uint Seed;
        public bool Any => Gag != DeathGag.None;
    }

    /// <summary>What the controller knows of a death when it chooses a gag (AnimationController.Gags builds it).</summary>
    public struct GagInput
    {
        public DeathKind Cause; public Clip Clip;
        /// <summary>What the dead man was (InfantryArchetype), read from his latched state, never from his slot:
        /// the sim may have given the slot a newcomer on the tick he died.</summary>
        public int Archetype;
        public bool Flat, InTrench, Crushed, Clawed, Heavy, MachineGun;
        public int Density;
        /// <summary>The way the harm went, flat and unit (the round's line, the knock); zero when unknown.</summary>
        public float3 Travel;
        /// <summary>The ladder's throw: x, z how far (world), y how high. Zero when it threw nobody.</summary>
        public float3 Throw;
        public float BodyYaw, KillerYaw;
        public uint Seed, Tick;
    }

    public static class DeathGags
    {
        public const string Knob = "fx.deathAbsurd";
        /// <summary>The new look: the owner turned it on (2026-09-30, "turn it on and let me test it").</summary>
        public const float DefaultIntensity = 1f, MaxIntensity = 2f;
        /// <summary>Whatever the intensity, no body flies further or higher than this, turns more, bounces more, slides
        /// further or waits longer. High stays under the fallen draw's bounds (VATRenderer).</summary>
        public const float FarCap = 26f, HighCap = 11f, SkidCap = 4f, DelayCap = 0.6f;
        /// <summary>A shell's throw at intensity 1 against today's: how much further, how much higher (a heap's fountain
        /// its own), and how wide a heap fans out off the burst's line (degrees each way). Critic round 1 (2026-09-29): at
        /// 2.4 and 3.0 high, capped at 18 m, the men left the top of the frame and the heap never read as a heap.</summary>
        public const float RocketFar = 1.9f, RocketHigh = 1.7f, FountainFar = 1.3f, FountainHigh = 1.9f, FountainFan = 80f;   // round 2: 2.2 threw them to the ice's edge; round 11: 1.6 still put bodies on the ice
        public const int FlipCap = 5, RollCap = 4, BounceCap = 2;
        /// <summary>Gravity the renderer throws a corpse with (VATRenderer.ThrowGravity), for counting the turns a flight holds.</summary>
        public const float BodyGravity = 14f;
        /// <summary>Share of standing shot deaths punted off their feet at intensity 1.</summary>
        public const float PuntShare = 0.45f;
        /// <summary>The balloon, a frog's own death (docs/design/idea-a-shot-frog-deflates-and-whizzes-off-lik.md section 3):
        /// 1 in 8 of the frogs shot standing in the open at intensity 1, well under the plank's 0.2, and 0.4 s of swelling
        /// before he lets go (under DelayCap).</summary>
        public const float BalloonShare = 0.12f, BalloonSwell = 0.4f;
        /// <summary>Three hops, 6 / 4 / 2.5 m far and 3 / 2.2 / 1.2 m high (the design's; the second and third as shares of
        /// the first in FallenFlight.HopFar2 and friends), each turned 50 to 70 degrees off the last.</summary>
        public const int BalloonHops = 3;
        public const float BalloonFar = 6f, BalloonHigh = 3f, BalloonTurnMin = 50f, BalloonTurnMax = 70f;
        /// <summary>His size as he swells and on hops one to three; the skin lies at the smallest, flattened to RestHeight
        /// (the pancake's, the lowest VatTint holds).</summary>
        public const float BalloonSize0 = 1f, BalloonSize1 = 0.8f, BalloonSize2 = 0.6f, BalloonSize3 = 0.5f, BalloonRestHeight = 0.12f;
        /// <summary>No-corpse gags (the beam's boots) only from this intensity: below it he burns where he fell.</summary>
        public const float BootsFrom = 0.5f;

        static float pinned = -1f, cached; static int cachedAt = -1;
        static DeathGag forced;
        static DeathGags() => SceneStatics.Register(nameof(DeathGags), () => { pinned = -1f; cachedAt = -1; forced = DeathGag.None; });

        /// <summary>The knob, read again only when a knob changes (Knobs.Get takes a lock and logs; not once a death).</summary>
        public static float Intensity
        {
            get
            {
                if (pinned >= 0f) return pinned;
                int g = Knobs.Generation;
                if (g != cachedAt) { cached = math.clamp(Knobs.Get(Knob, DefaultIntensity), 0f, MaxIntensity); cachedAt = g; }
                return cached;
            }
        }

        /// <summary>Pins the intensity (a test, a capture tool); a negative value hands it back to the knob.</summary>
        public static void Pin(float intensity) => pinned = intensity < 0f ? -1f : math.clamp(intensity, 0f, MaxIntensity);

        /// <summary>Pins the gag every death that can takes, so a rare one can be filmed (DeathLab.Force): a gag still only
        /// happens where its own conditions hold (a balloon wants a frog shot standing in the open). None hands the share
        /// dice back. A tool and a test only; nothing in the game calls it.</summary>
        public static void Force(DeathGag gag) => forced = gag;
        public static DeathGag Forced => forced;

        /// <summary>Whether a gag decided by a share happens: the pin if there is one, else the dice at this intensity.</summary>
        static bool Picked(DeathGag gag, uint seed, uint tick, uint salt, float share, float a)
            => forced != DeathGag.None ? forced == gag : Dice(seed, tick, salt) < math.min(1f, share * a);

        static float Dice(uint seed, uint tick, uint salt)
        {
            uint h = seed ^ ((tick + salt) * 2246822519u); h ^= h >> 13; h *= 3266489917u; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }
        static float Range(uint seed, uint tick, uint salt, float a, float b) => a + (b - a) * Dice(seed, tick, salt);
        static float Towards(float today, float target, float a) => today + (target - today) * a;
        static float Yaw(float3 d) => math.atan2(d.x, d.z);
        static float3 Flat(float yaw) => new float3(math.sin(yaw), 0f, math.cos(yaw));
        /// <summary>Whole turns a flight of this height (above its start) holds at the body's gravity, at turnsPerSecond.</summary>
        static int TurnsFor(float high, float turnsPerSecond) => (int)math.round(turnsPerSecond * 2f * math.sqrt(2f * math.max(0f, high) / BodyGravity));

        /// <summary>
        /// The gag for this death at intensity a (0 = none), and what it does to the ladder's choice: the throw (x, up, z),
        /// the clip and the yaw he is drawn at. With no gag the three come back as they went in.
        /// </summary>
        public static GagPlan Choose(in GagInput g, float a, ref float3 fly, ref Clip clip, ref float yaw)
        {
            var plan = new GagPlan { Seed = g.Seed, Intensity = a, SpinScale = 1f, Dice = Dice(g.Seed, g.Tick, 901u) };
            if (a <= 0f) return default;
            uint s = g.Seed, t = g.Tick;
            float3 travel = math.lengthsq(g.Travel.xz) > 1e-4f ? math.normalize(new float3(g.Travel.x, 0f, g.Travel.z)) : -Flat(g.BodyYaw);

            if (g.Cause == DeathKind.Beam)
            {
                if (a >= BootsFrom) { plan.Gag = DeathGag.Boots; plan.Flags = GagFlags.NoCorpse | GagFlags.HelmetPop; return plan; }
                return SkidFrom(ref plan, g, a);
            }
            if (g.Cause == DeathKind.Burning) return SkidFrom(ref plan, g, a);
            if (g.Cause == DeathKind.Gas)
            {
                if (!g.Flat && Picked(DeathGag.Plank, s, t, 911u, 0.2f, a))
                {
                    // stiff as a plank: the standing pose held, and he goes over forwards about his feet
                    plan.Gag = DeathGag.Plank; plan.Flags = GagFlags.Feet | GagFlags.Freeze;
                    plan.Topple = 8; plan.ToppleDur = 0.55f;
                    clip = Clip.DeathFront;
                    return plan;
                }
                plan.Gag = DeathGag.Wilt;
                plan.PulseQ = (sbyte)VatTintCode(Towards(1f, 1.18f, a)); plan.PulseDur = 0.35f;   // a last gasp
                plan.RestQ = (sbyte)VatTintCode(Towards(1f, 0.86f, a));                          // and he deflates
                return plan;
            }
            if (g.Crushed)
            {
                plan.Gag = DeathGag.Pancake; plan.Flags = GagFlags.Feet | GagFlags.Splat;
                plan.RestQ = (sbyte)VatTintCode(Towards(1f, 0.12f, a));
                plan.ClipRate = 4f;   // down at once: the track does not wait for him to fall
                yaw = g.KillerYaw;    // pressed flat along the way the track went
                return plan;
            }
            if (g.Clawed)
            {
                plan.Gag = DeathGag.Flung;
                plan.Delay = Towards(0f, 0.3f, a); plan.HoldLift = Towards(0f, 2.2f, a); plan.Jig = 0.25f * math.min(1f, a);
                float bearing = g.KillerYaw + math.radians(Range(s, t, 921u, -50f, 50f));
                float3 way = Flat(bearing);
                float far = Towards(0f, Range(s, t, 922u, 8f, 16f), a), high = Towards(0f, Range(s, t, 923u, 5f, 9f), a);
                fly = new float3(way.x * far, high, way.z * far);
                plan.Rolls = (byte)(2 + (Dice(s, t, 924u) < 0.5f ? 1 : 0)); plan.Bounces = 2;
                clip = Clip.DeathThrown; yaw = Yaw(-way);
                return Capped(ref plan, ref fly);
            }
            bool thrown = g.Throw.y > 0.05f;
            if (thrown && (g.Cause == DeathKind.Blast))
            {
                float3 way = math.lengthsq(g.Throw.xz) > 1e-4f ? math.normalize(new float3(g.Throw.x, 0f, g.Throw.z)) : float3.zero;
                float far = math.length(g.Throw.xz), high = g.Throw.y;
                bool heap = g.Density >= 2;
                far *= Towards(1f, heap ? FountainFar : RocketFar, a); high *= Towards(1f, heap ? FountainHigh : RocketHigh, a);
                if (heap && math.lengthsq(way) > 0f)
                {
                    // the bay goes up as a fountain: each man fanned off the burst's line, a beat apart
                    way = Flat(Yaw(way) + math.radians(Range(s, t, 931u, -FountainFan, FountainFan)));
                    plan.Delay = Range(s, t, 932u, 0.1f, 0.5f) * math.min(1f, a);   // frog rounds 1-2: 0.05-0.3 s apart, the bay left as one clump
                }
                if (g.InTrench) { far *= 0.5f; }   // the ladder already made it up, not out: keep it so
                fly = new float3(way.x * far, high, way.z * far);
                plan.Gag = heap ? DeathGag.Fountain : DeathGag.Rocket;
                int turns = TurnsFor(math.min(high, HighCap), Towards(0.6f, 1.6f, a));
                if (heap && (s & 1u) != 0u) { plan.Rolls = (byte)math.clamp(turns, 1, RollCap); plan.Flips = (byte)math.min(1, turns); }   // a cartwheel
                else plan.Flips = (byte)math.clamp(turns, 1, FlipCap);
                plan.Bounces = (byte)(g.InTrench ? math.min(1, (int)math.round(2f * a)) : (int)math.round(2f * a));
                plan.Skid = g.InTrench ? 0f : math.min(0.12f * far, 2f) * math.min(1f, a);
                plan.SpinScale = Towards(1f, 1.6f, a);
                return Capped(ref plan, ref fly);
            }
            if (g.Cause != DeathKind.Shot) return default;

            // shot
            if (g.Clip == Clip.DeathHeadshot)
            {
                plan.Gag = DeathGag.HeadPop; plan.Flags = GagFlags.HelmetPop | GagFlags.HeadOff;
                plan.PulseQ = (sbyte)VatTintCode(Towards(1f, 1.25f, a)); plan.PulseDur = 0.12f;
                return plan;
            }
            if (g.InTrench) return default;   // a shot never throws a man out of his trench
            if (!g.Flat && g.Archetype == InfantryArchetype.Frog && Picked(DeathGag.Balloon, s, t, 981u, BalloonShare, a))
            {
                // the balloon: his throat sac blows up, he lets go, and he whizzes off backwards in three shrinking zigzag
                // hops before he drops as a flat skin. Only a frog, and only standing in the open (the design, section 1).
                plan.Gag = DeathGag.Balloon;
                plan.Flags = GagFlags.PlayAtDeath;   // the swell plays through the delay, not after it
                plan.Delay = BalloonSwell * math.min(1f, a);
                plan.Hops = (byte)BalloonHops;
                plan.HopTurn = math.radians(Range(s, t, 982u, BalloonTurnMin, BalloonTurnMax)) * (Dice(s, t, 983u) < 0.5f ? -1f : 1f);
                plan.RestQ = (sbyte)VatTintCode(BalloonRestHeight);   // the skin, as flat as the pancake
                float farB = Towards(0f, BalloonFar, a), highB = Towards(0f, BalloonHigh, a);
                fly = new float3(travel.x * farB, highB, travel.z * farB);   // hop one off the round's line, as the punt goes
                clip = Clip.DeathBalloon; yaw = Yaw(-travel);
                // the row (a standing half second the sac blows up over) is played to its end inside the swell
                plan.ClipRate = Clips.Table[(int)Clip.DeathBalloon].Seconds / math.max(1e-3f, plan.Delay);
                return Capped(ref plan, ref fly);
            }
            if (g.Flat)
            {
                plan.Gag = DeathGag.Flop;
                fly = new float3(0f, Towards(0f, 0.4f, a), 0f);
                plan.Rolls = 1;
                return Capped(ref plan, ref fly);
            }
            float farShot, highShot; int flips; int bounces; float skid;
            if (g.MachineGun)
            {
                plan.Gag = DeathGag.Jig;
                plan.Delay = Towards(0f, Range(s, t, 941u, 0.35f, 0.6f), a); plan.Jig = 0.3f * math.min(1f, a);
                farShot = Range(s, t, 942u, 2f, 5f); highShot = Range(s, t, 943u, 0.8f, 1.6f); flips = 1; bounces = 1; skid = Range(s, t, 944u, 0.5f, 1.5f);
            }
            else if (g.Heavy)
            {
                plan.Gag = DeathGag.Punt;
                farShot = Range(s, t, 951u, 5f, 9f); highShot = Range(s, t, 952u, 1.5f, 3f); flips = Dice(s, t, 953u) < 0.5f ? 1 : 2; bounces = 2; skid = Range(s, t, 954u, 1f, 3f);
            }
            else if (Picked(DeathGag.Punt, s, t, 961u, PuntShare, a))
            {
                plan.Gag = DeathGag.Punt;
                farShot = Range(s, t, 962u, 1.5f, 4f); highShot = Range(s, t, 963u, 0.5f, 1.4f); flips = 0; bounces = 1; skid = Range(s, t, 964u, 0.5f, 1.5f);
            }
            else return default;
            farShot = Towards(0f, farShot, a); highShot = Towards(0f, highShot, a);
            // a backflip if the flight is long enough to turn in (0.6 s at the body's gravity: about 0.63 m up)
            if (flips == 0 && TurnsFor(highShot, 1f) >= 1) flips = 1;
            fly = new float3(travel.x * farShot, highShot, travel.z * farShot);
            plan.Flips = (byte)flips; plan.Bounces = (byte)bounces; plan.Skid = skid * math.min(1f, a);
            clip = Clip.DeathThrown; yaw = Yaw(-travel);   // facing the gun that punted him, thrown backwards
            return Capped(ref plan, ref fly);
        }

        static GagPlan SkidFrom(ref GagPlan plan, in GagInput g, float a)
        {
            // down mid-stride and sliding on along the way he was running
            plan.Gag = DeathGag.Skid;
            plan.Skid = Towards(0f, Range(g.Seed, g.Tick, 971u, 2.5f, 4f), a); plan.SkidYaw = g.BodyYaw;
            return Capped(ref plan);
        }

        static GagPlan Capped(ref GagPlan plan)
        {
            plan.Skid = math.min(plan.Skid, SkidCap); plan.Delay = math.min(plan.Delay, DelayCap);
            plan.Flips = (byte)math.min(plan.Flips, FlipCap); plan.Rolls = (byte)math.min(plan.Rolls, RollCap); plan.Bounces = (byte)math.min(plan.Bounces, BounceCap);
            return plan;
        }

        static GagPlan Capped(ref GagPlan plan, ref float3 fly)
        {
            float far = math.length(fly.xz);
            if (far > FarCap) { fly.x *= FarCap / far; fly.z *= FarCap / far; }
            fly.y = math.min(fly.y, HighCap);
            return Capped(ref plan);
        }

        /// <summary>A height factor as a VatTint squash code (-32..31); kept here so Core needs nothing from Units.</summary>
        public static int VatTintCode(float height) => math.clamp((int)math.round((height - 1f) / 0.0275f), -32, 31);
    }
}
