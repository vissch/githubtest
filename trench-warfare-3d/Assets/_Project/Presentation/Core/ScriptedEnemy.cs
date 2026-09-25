// Phase: P0 (implemented; moved out of SimHost in the perf pass, 2026-09-23) — the scripted enemy: deploys on a
// clock, sends its front trench over the top once it is manned, shells or gasses your front trench when it can afford
// to, and runs the stress preset for both sides. It READS a world (the enemy's view of the match) and ISSUES to a
// sink (the enemy seat), so it neither knows nor cares whether a second world exists behind the seat.
// Once per tick. As SimHost.IssuePeerCommands it ran on every pass of the host's tick loop, keyed to the peer
// world's tick, so when the loopback peer stalled for a pass it ran again at the SAME tick: a second deploy, a second
// trench order, and on the support tick a barrage AND a gas attack, because the alternation flag flipped twice. That
// only happened under simulated latency, so single player (one world, no latency) and the canary now agree exactly.
// The stress preset orders BOTH sides, and each side's orders are timed by that side's own world: the player's by the
// player's tick, the enemy's by the enemy view's. Keyed to the enemy's tick (as it was), the player's deploys landed
// wherever the player's world happened to be when the peer's tick came round, which under latency is network timing.
// A6 replaces this with WaveAiSystem.
using UnityEngine;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    public sealed class ScriptedEnemy
    {
        public bool Enabled = true;
        public int DeployEveryTicks = 40;
        public bool Attacks = true;
        public bool DeploysTanks;
        public int AttackGarrison = 8;
        public bool UsesSupport = true;
        public int SupportReserve = 180;
        /// <summary>Stress preset: riflemen a side, deployed 4 a tick by BOTH players; 0 = off.</summary>
        public int StressUnits;
        public int StressAdvanceDelayTicks = 300;

        struct Stress { public int Deployed; public uint AdvanceTick; public bool Advanced; public uint LastTick; }
        int supportCount;
        Stress playerStress = new Stress { LastTick = uint.MaxValue }, enemyStress = new Stress { LastTick = uint.MaxValue };
        uint lastTick = uint.MaxValue;

        /// <summary>Decide this tick's orders. `view` is the world the enemy reads (the player's own in single player, the
        /// peer's in the canary: the same state at the same tick); `local` is the player's match, for the stress preset's
        /// own-side orders; `enemy` and `player` are the seats the orders go to.</summary>
        public void Think(MatchSim view, MatchSim local, ICommandSink enemy, ICommandSink player)
        {
            if (!Enabled) return;
            uint t = view.World.Tick;
            if (t != lastTick) { lastTick = t; EnemyOrders(view, enemy, t); }   // once per tick, whatever the host loop does
            // after the enemy's own orders, as before the split: the sim sorts a tick's commands by player and keeps each
            // player's in issue order, so this keeps both players' order within a tick exactly what it was
            if (StressUnits > 0)
            {
                StressSide(ref playerStress, local, player, 0);
                StressSide(ref enemyStress, view, enemy, 1);
            }
        }

        /// <summary>
        /// The n-th armed foot soldier in the enemy's own roster, round-robin. Slot indices are no use here: the
        /// factions field different units in the same slots, so slots 0-2 are three riflemen for one side and, for the
        /// other, whatever its table happens to put there. A medic has no weapon at all and an engineer fires about
        /// once every twenty seconds, so a script deploying by index spent a third of its silver on men who cannot
        /// shoot; paratroopers are dropped by the air card and are not deployable from a trench. -1 if it fields none.
        /// </summary>
        static int ArmedSlot(SimWorld pw, int n)
        {
            int count = 0;
            for (int s = 0; s < RosterEntry.SlotCount; s++) if (Armed(pw, s)) count++;
            if (count == 0) return -1;
            int want = ((n % count) + count) % count;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                if (Armed(pw, s) && want-- == 0) return s;
            return -1;
        }

        /// <summary>A foot soldier of the enemy's roster who can actually shoot back.</summary>
        static bool Armed(SimWorld pw, int slot)
        {
            var e = pw.Roster[RosterEntry.SlotCount + slot];
            if (e.IsVehicle) return false;
            return e.Archetype != InfantryArchetype.Medic && e.Archetype != InfantryArchetype.Repair
                && e.Archetype != InfantryArchetype.Para;
        }

        /// <summary>The enemy's first machine, whatever its faction calls it; -1 if it fields none.</summary>
        static int MachineSlot(SimWorld pw)
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                if (pw.Roster[RosterEntry.SlotCount + s].IsVehicle) return s;
            return -1;
        }

        void EnemyOrders(MatchSim view, ICommandSink enemy, uint t)
        {
            var pw = view.World;
            if (t % (uint)Mathf.Max(1, DeployEveryTicks) == 0)
            {
                // keep a reserve for support fire once the first squad is out; silver is the only brake on the script
                int slot = ArmedSlot(pw, (int)(t / (uint)Mathf.Max(1, DeployEveryTicks)));
                if (slot >= 0)
                {
                    int cost = pw.Roster[RosterEntry.SlotCount + slot].Cost;
                    int reserve = UsesSupport && pw.AliveCount > 0 && t > 600 ? SupportReserve : 0;
                    if (pw.Silver[1] >= cost + reserve) enemy.Issue(SimCommand.Deploy(t, 1, slot));
                }
            }
            if (DeploysTanks && t % 100 == 70)
            {
                int slot = MachineSlot(pw);
                int ri = RosterEntry.SlotCount + slot;
                if (slot >= 0 && pw.SlotCooldown[ri] == 0 && pw.Silver[1] >= pw.Roster[ri].Cost)
                    enemy.Issue(SimCommand.Deploy(t, 1, slot));
            }
            if (t % 100 == 20)
            {
                // every trench behind the front is locked so reinforcements walk through to the front line
                short front = view.Fields.FrontTrench(1);
                for (int k = 0; k < view.Fields.Trenches.Length; k++)
                {
                    var ts = view.Fields.Trenches[k];
                    if (ts.OwnerTeam != 1) continue;
                    byte want = (byte)(k != front ? 1 : 0);
                    if (ts.Locked != want) enemy.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchLock, A = k, B = want });
                    if (k != front && ts.GarrisonCount > 0) enemy.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchAdvance, A = k });
                }
            }
            if (Attacks && t % 100 == 50)
            {
                short front = view.Fields.FrontTrench(1);
                if (front >= 0 && view.Fields.Trenches[front].GarrisonCount >= AttackGarrison)
                    enemy.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchAdvance, A = front });
            }
            if (UsesSupport && t % 200 == 150 && view.Abilities != null)
            {
                short mine = view.Fields.FrontTrench(0);
                var ability = (supportCount & 1) == 0 ? OffMapAbilityId.HeBarrage : OffMapAbilityId.ChlorineGas;
                if (mine >= 0 && view.Fields.Trenches[mine].GarrisonCount >= 6 && view.Abilities.CooldownOf(1, ability) == 0
                    && OffMapAbilitySystem.TryGetStats((int)ability, out var stats) && pw.Silver[1] >= stats.Cost + 20)
                {
                    Vector3 sum = Vector3.zero; int n = 0;
                    for (int i = 0; i < pw.HighWater; i++)
                        if (pw.IsAlive(i) && pw.TrenchId[i] == mine) { sum += (Vector3)pw.Position[i]; n++; }
                    if (n > 0)
                    {
                        sum /= n;
                        // gas is released upwind (the map wind blows toward -Z) so the cloud rolls over the trench
                        float dz = ability == OffMapAbilityId.ChlorineGas ? 12f : 0f;
                        enemy.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.SupportFire, A = (int)ability, Pos = new Unity.Mathematics.float3(sum.x, 0f, sum.z + dz) });
                        supportCount++;
                    }
                }
            }
        }

        /// <summary>The stress preset for one side, once per tick of that side's own world: 4 riflemen a tick until
        /// StressUnits are out, then its front trench over the top StressAdvanceDelayTicks after the last of them.</summary>
        void StressSide(ref Stress s, MatchSim world, ICommandSink seat, byte side)
        {
            uint t = world.World.Tick;
            if (t == s.LastTick) return;
            s.LastTick = t;
            if (s.Deployed < StressUnits)
            {
                for (int k = 0; k < 4 && s.Deployed < StressUnits; k++, s.Deployed++) seat.Issue(SimCommand.Deploy(t, side, 0));
                s.AdvanceTick = t + (uint)StressAdvanceDelayTicks;
            }
            else if (!s.Advanced && t >= s.AdvanceTick)
            {
                s.Advanced = true;
                short front = world.Fields.FrontTrench(side);
                if (front >= 0) seat.Issue(new SimCommand { Type = CommandType.TrenchAdvance, A = front });
            }
        }
    }
}
