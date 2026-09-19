// Phase: P0 (implemented) — Map data contract, see docs/02-contracts.md
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    /// <summary>A fire trench or communication trench: an ordered chain of nav cells with fire-steps and links.</summary>
    public struct TrenchDef
    {
        public short Id;
        public byte OwnerTeam;         // initial owner (0/1), 255 = neutral
        public byte Kind;              // 0 fire trench, 1 communication trench, 2 sap
        public int CellStart, CellCount;       // range into MapData.TrenchCells (nav grid indices, ordered along the trench)
        public int FireStepStart, FireStepCount; // range into MapData.FireStepCells
        public int LinkStart, LinkCount;         // range into MapData.LinkCells (cells where units can vault/climb)
        public short NextTrenchForTeam0;         // trench a ">>" from here sends team 0 to (-1 = enemy HQ)
        public short NextTrenchForTeam1;
        public float FacingYaw;                  // parapet facing (radians) for the directional cover arc
    }

    public enum ObjectiveKind : byte { OutpostLine = 0, MainLine = 1, ReserveLine = 2, HQ = 3, Custom = 4 }

    public struct ObjectiveDef
    {
        public short Id;
        public ObjectiveKind Kind;
        public byte OwnerTeam;         // 255 = neutral
        public byte SideTeam;          // which team's line this is (whose HQ, etc.)
        public int CellStart, CellCount; // range into MapData.ObjectiveCells
        public int RequiredUnits;      // friendly infantry needed inside to capture
        public int CaptureTicks;       // ticks of uncontested presence
        public short OrderIndex;       // capture order along the sector (0 = first)
    }

    /// <summary>Directional cover. Applies only if the attacker's bearing lies within the protected arc.</summary>
    public struct CoverVolume
    {
        public float3 Center;
        public float Radius;
        public float ArcCenterYaw;     // radians
        public float ArcHalfWidth;     // radians; Pi = omnidirectional
        public float Bonus;            // 0..1 hit-chance reduction
        public float Height;           // metres above local ground the cover reaches
        public int OwnerSlot;          // -1 static; else moves with a unit (tank hull, sentry, shield)
    }

    public struct SpawnPoint { public byte Team; public float3 Pos; public byte Kind; } // Kind: 0 road, 1 communication trench

    public struct TriggerVolume { public short Id; public float3 Min, Max; }

    public struct EmplacementDef { public byte Kind; public byte Team; public float3 Pos; public float Yaw; } // Kind: 0 MG nest, 1 bunker, 2 field gun, 3 OP/HQ dugout, 4 Livens battery, 5 mine cell
}
