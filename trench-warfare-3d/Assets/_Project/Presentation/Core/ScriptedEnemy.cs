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
// S04 (AOSA, 2026-09-25): the stress preset spreads the player's army. Every deploy walks to the rear trench and stops
// there, and the preset's one `>>` went to the front trench, which was empty, so at 1,500 a side 1,409 men stood in a
// rear trench with 79 posts, ~38 of them within 2 m of each man: the benchmark measured a crush, not a battle. Now each
// trench of the player's that is full (a man in it has gone a garrison step without a post) is LOCKED, so later men
// pass on: the rear trench fills its posts, then the front one, then the rest go over the top at the enemy (measured
// on the Mono port at 1,500 a side, tick 1,800: 85 in the rear trench, 74 in the front one, 790 in the open, 11.7 men
// within 2 m of each instead of 37.6). Only sim commands, through the player's seat, as a player would give them. The
// enemy side needs none of it: EnemyOrders already locks its rear trench and sends its front over every 100 ticks.
// StressSpread = false (the default; knob stress.spread=1 turns spreading on) is the preset as it was, so old and new
// measure in one build.
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
        /// <summary>Stress preset (S04): lock each of the player's trenches once it is full, so the army fills the posts of
        /// every trench and the overflow goes over the top, instead of all of it standing in the rear trench. false
        /// (default) = the preset as it was: the benchmark baselines stay comparable, and spread wipes the enemy out
        /// mid-window at 1,500 a side (knob stress.spread=1 turns it on).</summary>
        public bool StressSpread = false;

        struct Stress
        {
            public int Deployed; public uint AdvanceTick; public bool Advanced; public uint LastTick;
            /// <summary>Spread: trenches (bit per id) this preset has locked; each is locked once.</summary>
            public ulong LockSent;
            /// <summary>Spread: per slot, 1 + the trench he stood in without a post at the last check, else 0.</summary>
            public short[] Postless;
        }
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

        void EnemyOrders(MatchSim view, ICommandSink enemy, uint t)
        {
            var pw = view.World;
            if (t % (uint)Mathf.Max(1, DeployEveryTicks) == 0)
            {
                // keep a reserve for support fire once the first squad is out; silver is the only brake on the script
                int slot = (int)(t / (uint)Mathf.Max(1, DeployEveryTicks)) % 3;
                int cost = pw.Roster[RosterEntry.SlotCount + slot].Cost;
                int reserve = UsesSupport && pw.AliveCount > 0 && t > 600 ? SupportReserve : 0;
                if (pw.Silver[1] >= cost + reserve) enemy.Issue(SimCommand.Deploy(t, 1, slot));
            }
            if (DeploysTanks && t % 100 == 70)
            {
                int ri = RosterEntry.SlotCount + 4;
                if (pw.SlotCooldown[ri] == 0 && pw.Silver[1] >= pw.Roster[ri].Cost) enemy.Issue(SimCommand.Deploy(t, 1, 4));
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
            // the enemy's trenches are EnemyOrders' (it unlocks its front every 100 ticks), so only the player's spread
            if (StressSpread && side == 0) Spread(ref s, world, seat, side);
        }

        /// <summary>S04: lock every trench of `side` that is full, once. A trench is full when one of its own men has stood
        /// in it without a post across a whole check: TrenchGarrisonSystem hands the free posts out every tick, before
        /// movement, so a man who garrisoned on the last step has no post YET, and only a full trench leaves him without
        /// one on the next. A locked trench passes arrivals on to the next trench in the chain (MovementSystem), and past
        /// the front one that is the enemy's line. Reads the side's own world at its own tick, like the rest of the preset.</summary>
        void Spread(ref Stress s, MatchSim world, ICommandSink seat, byte side)
        {
            var w = world.World;
            var trenches = world.Fields.Trenches;
            int n = Mathf.Min(trenches.Length, 64);
            if (s.Postless == null || s.Postless.Length < w.HighWater) s.Postless = new short[w.TrenchId.Length];
            ulong full = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                short k = w.TrenchId[i];
                bool postless = k >= 0 && k < n && w.PostKind[i] == 0 && w.Team[i] == side && (w.Flags[i] & (uint)UnitFlags.Alive) != 0;
                short was = s.Postless[i];
                s.Postless[i] = postless ? (short)(k + 1) : (short)0;
                if (postless && was == k + 1) full |= 1ul << k;
            }
            if (full == 0) return;
            for (int k = 0; k < n; k++)
            {
                ulong bit = 1ul << k;
                if ((full & bit) == 0 || (s.LockSent & bit) != 0) continue;
                var ts = trenches[k];
                if (ts.OwnerTeam != side || ts.Locked != 0) continue;
                s.LockSent |= bit;
                seat.Issue(new SimCommand { Type = CommandType.TrenchLock, A = k, B = 1 });
            }
        }
    }
}
