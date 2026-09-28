// Phase: P0 (implemented) — Pose stream contract, see docs/02-contracts.md
// Produced per render frame by SimPresenter (interpolation of two ticks). Consumed by VAT renderer, ragdolls, UI.
using Unity.Mathematics;

namespace TW.Sim
{
    public enum Stance : byte { Standing = 0, Crouch = 1, FireStep = 2, Prone = 3, Sprint = 4, Pinned = 5, Vault = 6, Dead = 7,
        /// <summary>In the air on a jetpack leap (LeapSystem): a straight line to an enemy trench, nothing can be done to him.</summary>
        Leap = 8,
        /// <summary>Hand to hand (MeleeSystem, 2026-09-28): on his feet at arm's length from his foe, facing him, striking.</summary>
        Melee = 9 }

    [System.Flags]
    public enum UnitFlags : uint
    {
        None = 0,
        Alive = 1 << 0,
        Exposed = 1 << 1,       // on the surface layer with an advance order
        InTrench = 1 << 2,
        HoldFire = 1 << 3,
        Vehicle = 1 << 4,
        Emplacement = 1 << 5,
        Immobilised = 1 << 6,
        Stalled = 1 << 7,
        Masked = 1 << 8,        // gas mask issued (mission unlock)
        Burning = 1 << 9,
        Bogged = 1 << 10,
        KnockedOut = 1 << 11,   // a vehicle whose crew is dead or gone: it burns until it is despawned into a wreck prop (only then does it block a cell and give cover; tanks already cannot push past it)
        // ---- 2026-09-25. Bits 8 and up never reach UnitPose.Flags (the low byte); presentation reads SimWorld.Flags by name ----
        Airborne = 1 << 12,     // mid-leap (LeapSystem): not a target, not suppressed, not pushed
        Charging = 1 << 13,     // a Breaker between wind-up and withdrawal (BreakerSystem): sees below the rim, crits, a thicker deck
        Hero = 1 << 14,         // his moment (HeroSystem): the named man leading the section over the top
        Veteran = 1 << 15,      // a named man from the profile (HeroSystem): a rank, a little more of everything
        // ---- 2026-09-28 (lane/sim/melee) ----
        Melee = 1 << 16,        // charging a foe within 8 m or fighting him hand to hand (MeleeSystem): he does not shoot
        Disarmed = 1 << 17,     // his weapon lies on the ground (a fists man in a fight, MeleeSystem): he does not shoot
        Pouncing = 1 << 18,     // a crab crouched to leap or in the leap (PounceSystem)
    }

    public struct UnitPose
    {
        public float3 Pos;
        public half Yaw;
        public ushort AnimRow;
        public half AnimT;
        public byte Archetype;
        public byte Flags;      // low byte of UnitFlags
        public byte Lod;
        public byte Team;
    }

    /// <summary>Animation rows shared by every VAT atlas (see B3). Order is part of the contract.</summary>
    public enum AnimRow : ushort
    {
        Idle = 0, Walk, Sprint, CrouchWalk, ProneCrawl, FireStanding, FireFireStep, FireProne, Throw, Vault,
        Flinch0, Flinch1, Flinch2, Death0, Death1, Death2, Death3, PinnedLoop, Count
    }
}
