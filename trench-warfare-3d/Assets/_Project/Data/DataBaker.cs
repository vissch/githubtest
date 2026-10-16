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

        // Used to cast the unset case straight to the literal 0 (FactionId.Iron), the greybox roster, instead of
        // FactionMap's own default (British) — and never said that the field was empty.
        static byte MissionFaction(MissionDefinition m, FactionDefinition f, string field)
        {
            if (f != null) return (byte)FactionMap.ToSim(f.Faction);
            Debug.LogWarning($"MissionDefinition '{(string.IsNullOrEmpty(m.Title) ? m.name : m.Title)}' has no {field}: " +
                             $"baking {FactionMap.Unmapped} (FactionMap's default for an unmapped nation) instead of the greybox Iron roster.");
            return (byte)FactionMap.Unmapped;
        }

        public static MissionScript BakeMission(MissionDefinition m, Allocator a)
        {
            var s = new MissionScript(a)
            {
                Win = m.Win, WinParam = m.WinParam, Lose = m.Lose, LoseParam = m.LoseParam,
                StartingSilver = m.StartingSilver, IncomePerSecond = m.IncomePerSecond, Difficulty = m.Difficulty,
                PlayerFaction = MissionFaction(m, m.PlayerFaction, nameof(m.PlayerFaction)),
                EnemyFaction = MissionFaction(m, m.EnemyFaction, nameof(m.EnemyFaction)),
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

        /// <summary>Apply meta-progression upgrades before baking: +5 per tier to HP and to weapon Damage, +5% per
        /// tier to weapon Accuracy. Phase A7. Damage and accuracy live on the weapon, not the unit, so this also
        /// takes and returns the primary weapon's stats.</summary>
        public static UnitStats ApplyUpgrades(UnitStats stats, WeaponStats weapon, int hpTier, int dmgTier, int accTier, out WeaponStats upgradedWeapon)
        {
            stats.Hp += 5f * hpTier;
            weapon.Damage += 5f * dmgTier;
            weapon.Accuracy += 0.05f * accTier;
            upgradedWeapon = weapon;
            return stats;
        }
    }
}
