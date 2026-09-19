// Phase: C2 (implemented for the schemas above; sim consumers land in A3/A5/A6)
// Turns ScriptableObjects into the unmanaged tables the sim reads. Bake once at match start, never per tick.
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Data
{
    public static class DataBaker
    {
        public static NativeArray<UnitStats> BakeUnits(IReadOnlyList<UnitDefinition> defs, Allocator a)
        {
            var arr = new NativeArray<UnitStats>(defs.Count, a);
            for (int i = 0; i < defs.Count; i++) arr[i] = defs[i].ToStats();
            return arr;
        }

        public static NativeArray<WeaponStats> BakeWeapons(IReadOnlyList<WeaponDefinition> defs, Allocator a)
        {
            var arr = new NativeArray<WeaponStats>(defs.Count, a);
            for (int i = 0; i < defs.Count; i++) arr[i] = defs[i].ToStats();
            return arr;
        }

        public static NativeArray<AbilityStats> BakeAbilities(IReadOnlyList<AbilityDefinition> defs, Allocator a)
        {
            var arr = new NativeArray<AbilityStats>(defs.Count, a);
            for (int i = 0; i < defs.Count; i++) arr[i] = defs[i].ToStats();
            return arr;
        }

        public static MissionScript BakeMission(MissionDefinition m, Allocator a)
        {
            var s = new MissionScript(a)
            {
                Win = m.Win, WinParam = m.WinParam, Lose = m.Lose, LoseParam = m.LoseParam,
                StartingSilver = m.StartingSilver, IncomePerSecond = m.IncomePerSecond, Difficulty = m.Difficulty,
                PlayerFaction = (byte)(m.PlayerFaction != null ? m.PlayerFaction.Faction : 0),
                EnemyFaction = (byte)(m.EnemyFaction != null ? m.EnemyFaction.Faction : 0),
            };
            if (m.Waves != null)
                foreach (var w in m.Waves)
                {
                    var table = new WaveTable { Id = w.Id, EntryStart = s.WaveEntries.Length, IntervalTicks = w.IntervalTicks };
                    if (w.Entries != null)
                        foreach (var e in w.Entries)
                            s.WaveEntries.Add(new WaveEntry { UnitId = e.Unit != null ? e.Unit.Id : (short)-1, Count = e.Count, EntryPoint = e.EntryPoint, Order = e.Order });
                    table.EntryCount = s.WaveEntries.Length - table.EntryStart;
                    s.Waves.Add(table);
                }
            if (m.Triggers != null)
                foreach (var t in m.Triggers)
                {
                    var trig = new MissionTrigger { Id = t.Id, Condition = t.Condition, CondA = t.CondA, CondB = t.CondB, CondTeam = t.CondTeam, DelayTicks = t.DelayTicks, Once = t.Once, ActionStart = s.Actions.Length };
                    if (t.Actions != null)
                        foreach (var act in t.Actions)
                            s.Actions.Add(new MissionAction { Kind = act.Kind, A = act.A, B = act.B, Pos = act.Pos, Vec = act.Vec });
                    trig.ActionCount = s.Actions.Length - trig.ActionStart;
                    s.Triggers.Add(trig);
                }
            return s;
        }

        /// <summary>Apply meta-progression upgrades (+5 per tier to HP / DMG / ACC) before baking. Phase A7.</summary>
        public static UnitStats ApplyUpgrades(UnitStats stats, int hpTier, int dmgTier, int accTier)
        {
            stats.Hp += 5f * hpTier;
            // damage and accuracy live on the weapon; A7 resolves them through the weapon table
            return stats;
        }
    }
}
