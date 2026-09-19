// Phase: A6 (schema implemented as data; runner is a stub) — see docs/08-missions.md "Trigger table format"
// MissionRunner evaluates triggers in id order every tick, inside the sim, so mission flow is deterministic
// and replayable. Presentation reacts to MissionTriggerFired / WaveStarted events.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Match
{
    public enum TriggerCondition : byte
    {
        TickAtLeast, WaveStarted, WaveCleared, ObjectiveCaptured, ObjectiveLost, UnitsInVolume,
        SilverAtLeast, AbilityUsed, VehicleDeployed, Timer, UnitNearCell, FirstUnitReachesLayer,
    }

    public enum TriggerAction : byte
    {
        SpawnWave, UnlockSlot, LockSlot, UnlockAbility, LockAbility, SetEnemyBudget, EnemyAbility, SetWind, SetFog,
        Dialogue, SetWinRule, SetLoseRule, RevealCells, StartTimer, EndMission, GrantMasks, SetIncome,
    }

    public enum WinRule : byte { CaptureHQ, SurviveWaves, HoldObjective, Timer }
    public enum LoseRule : byte { LoseHQ, LoseObjective, Timer, AllUnitsDead }

    public struct MissionTrigger
    {
        public short Id;
        public TriggerCondition Condition;
        public int CondA, CondB;      // objective id / wave index / volume id / count / silver
        public byte CondTeam;
        public int DelayTicks;
        public bool Once;
        public int ActionStart, ActionCount;   // range into MissionScript.Actions
    }

    public struct MissionAction
    {
        public TriggerAction Kind;
        public int A, B;              // wave table id / slot / ability id / rule / dialogue id / timer id
        public float3 Pos;
        public float2 Vec;            // wind
    }

    public struct WaveEntry { public short UnitId; public int Count; public byte EntryPoint; public byte Order; } // Order: 0 garrison, 1 advance

    public struct WaveTable { public short Id; public int EntryStart, EntryCount; public int IntervalTicks; }

    public sealed class MissionScript : System.IDisposable
    {
        public NativeList<MissionTrigger> Triggers;
        public NativeList<MissionAction> Actions;
        public NativeList<WaveTable> Waves;
        public NativeList<WaveEntry> WaveEntries;
        public WinRule Win; public int WinParam;
        public LoseRule Lose; public int LoseParam;
        public int StartingSilver; public float IncomePerSecond;
        public byte PlayerFaction, EnemyFaction, Difficulty;

        public MissionScript(Allocator a)
        {
            Triggers = new NativeList<MissionTrigger>(32, a);
            Actions = new NativeList<MissionAction>(64, a);
            Waves = new NativeList<WaveTable>(16, a);
            WaveEntries = new NativeList<WaveEntry>(64, a);
        }

        public void Dispose() { Triggers.Dispose(); Actions.Dispose(); Waves.Dispose(); WaveEntries.Dispose(); }
    }

    public sealed class MissionRunner : ISimSystem
    {
        public int Order => SimSystemOrder.Mission;
        readonly MissionScript script;
        public NativeArray<int> TriggerFiredTick;   // -1 until fired
        public NativeArray<int> TimerEndTick;
        public int CurrentWave;

        public MissionRunner(MissionScript script) { this.script = script; }
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A6: MissionRunner.Step");
        public ulong Hash(ulong h)
        {
            h = SimHash.Value(CurrentWave, h);
            if (TriggerFiredTick.IsCreated) h = SimHash.Array(TriggerFiredTick, h);
            if (TimerEndTick.IsCreated) h = SimHash.Array(TimerEndTick, h);
            return h;
        }
        public void Dispose() { if (TriggerFiredTick.IsCreated) TriggerFiredTick.Dispose(); if (TimerEndTick.IsCreated) TimerEndTick.Dispose(); }
    }
}
