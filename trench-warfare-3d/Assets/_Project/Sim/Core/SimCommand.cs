// Phase: P0 (implemented) — Command contract, see docs/02-contracts.md
// Every input to the simulation is a SimCommand. Net, UI and AI produce them; SimWorld consumes them.
using Unity.Mathematics;

namespace TW.Sim
{
    public enum CommandType : byte
    {
        None = 0,
        /// <summary>a = roster slot (0..4). Spends silver, spawns at the player's spawn point.</summary>
        DeployUnit = 1,
        /// <summary>a = trench id. All garrisoned units vault and sprint to the next forward trench (">>").</summary>
        TrenchAdvance = 2,
        /// <summary>a = trench id, b = archetype bitmask. Roster "↑": only masked classes advance.</summary>
        TrenchSelectAdvance = 3,
        /// <summary>a = trench id, b = 1 lock / 0 unlock. Arrivals pass through without garrisoning.</summary>
        TrenchLock = 4,
        /// <summary>a = trench id. Units in the open that came from this trench return to it ("↩").</summary>
        TrenchFallback = 5,
        /// <summary>a = unit slot, b = Stance. 3D addition: explicit prone/crouch/sprint override.</summary>
        UnitStance = 6,
        /// <summary>a = ability id, pos = the target (a point, an area's centre, or where a line starts),
        /// b = AbilityArgs.Pack(heading, pattern, length) for a line or a pattern; 0 is the plain ability at its own length.</summary>
        SupportFire = 7,
        /// <summary>a = unit slot, b = ability id, pos = target. Officer smoke call, Bangalore, fascine, etc.</summary>
        UnitAbility = 8,
        /// <summary>pos = new reinforcement rally point for this player.</summary>
        SetRally = 9,
        /// <summary>Player concedes.</summary>
        Surrender = 10,
        /// <summary>a = trench id, b = 1 hold fire / 0 free fire. 3D addition: stay below the rim.</summary>
        TrenchHoldFire = 11,
    }

    public struct SimCommand
    {
        public uint Tick;
        public byte Player;
        public CommandType Type;
        public int A;
        public int B;
        public float3 Pos;

        public static SimCommand Deploy(uint tick, byte player, int rosterSlot)
            => new SimCommand { Tick = tick, Player = player, Type = CommandType.DeployUnit, A = rosterSlot };

        public static SimCommand Rally(uint tick, byte player, float3 pos)
            => new SimCommand { Tick = tick, Player = player, Type = CommandType.SetRally, Pos = pos };

        /// <summary>Deterministic ordering key: commands for one tick are sorted by (Player, sequence) before execution.</summary>
        public long OrderKey(int sequence) => ((long)Player << 32) | (uint)sequence;
    }
}
