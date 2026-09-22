// Phase: P0 (implemented) — Pose stream contract, see docs/02-contracts.md
// Produced per render frame by SimPresenter (interpolation of two ticks). Consumed by VAT renderer, ragdolls, UI.
using Unity.Mathematics;

namespace TW.Sim
{
    public enum Stance : byte { Standing = 0, Crouch = 1, FireStep = 2, Prone = 3, Sprint = 4, Pinned = 5, Vault = 6, Dead = 7 }

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
