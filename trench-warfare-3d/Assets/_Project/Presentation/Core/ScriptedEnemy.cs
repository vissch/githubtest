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
// It plans its attacks (2026-09-29, the owner: "make the best game"). An assault takes a trench at about three to one
// bare or two to one behind a barrage (AssaultLadderTests), so it masses until it has those odds against the player's
// front garrison, lays a barrage on that trench and goes over the top as the shells come down (PlannedAttack); it keeps
// no silver back for support until it fields as many men as the player, and it shells or gasses only out of silver it
// has to spare. Before, it went over the top with eight men whatever stood in front of them, and kept 180 silver
// back from the first minute: once the player held six men it spent every coin on a barrage or gas every ten seconds
// and never deployed another man (MatchLoopTests).
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
        /// <summary>The seat it plays (2026-09-29): 1, the enemy, as it always has; 0 lets a test or an attract mode put
        /// the same script on the player's side. Everything below reads Side and Other, never a literal seat.</summary>
        public byte Side = 1;
        byte Other => (byte)(1 - Side);
        public int DeployEveryTicks = 40;
        public bool Attacks = true;
        public bool DeploysTanks;
        public int AttackGarrison = 8;
        public bool UsesSupport = true;
        public int SupportReserve = 180;
        /// <summary>The odds it wants behind a barrage: its front garrison against the player's. Bare, one more.</summary>
        public float Odds = 2f;
        /// <summary>Patience (2026-09-29): every PatienceTicks without going over the top, the odds it wants behind a
        /// barrage fall a step toward PatienceOdds (two, one and a half, then even). With the trench mouth hidden from far
        /// guns (v23) nobody dies between attacks, both armies grow alike and it never reached two to one: ten-minute
        /// matches of the script against itself took no trench at all. Behind smoke and a barrage, even odds are an even
        /// trade that takes the trench one time in three, and one and a half to one takes it every time
        /// (AssaultLadderTests). 0 turns it off. The bare attack keeps its three to one.</summary>
        public int PatienceTicks = 2400;
        public float PatienceOdds = 1f;
        uint lastAttack;
        /// <summary>The odds it wants behind a barrage at tick <paramref name="t"/>.</summary>
        public float WantedOdds(uint t)
        {
            if (PatienceTicks <= 0 || Odds <= PatienceOdds) return Odds;
            uint waited = t > lastAttack ? t - lastAttack : 0u;
            int steps = (int)(waited / (uint)PatienceTicks);
            return Mathf.Max(PatienceOdds, Odds - steps * 0.5f * (Odds - PatienceOdds));
        }
        /// <summary>Ticks from calling the barrage to going over the top: its warm-up and the first shells.</summary>
        public int BarrageLeadTicks = 110;
        /// <summary>The tick its planned attack goes over the top, 0 with none planned.</summary>
        public uint PlannedAttack;
        /// <summary>Defensive fire: an HE line on the player's men in the open coming at its trench (the SOS barrage).</summary>
        /// Off by default: it is the Hard difficulty's (PeerDefends). One barrage on a bare attack at three to one turned
        /// seven trenches taken of eight into none (20 of 30 men lost to 26), so on every difficulty it would undo the
        /// assault ladder while the enemy had 150 silver.
        public bool Defends;
        /// <summary>How many of the player's men in the open, how near its front trench, before it calls one.</summary>
        public int SosMen = 5;
        public float SosReach = 70f;
        /// <summary>Told each decision it takes, for a test or a log: what it did and on what count.</summary>
        public System.Action<string> Said;
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

        /// <summary>
        /// The n-th armed foot soldier in the enemy's own roster, round-robin. Slot indices are no use here: the
        /// factions field different units in the same slots, so slots 0-2 are three riflemen for one side and, for the
        /// other, whatever its table happens to put there. A medic has no weapon at all and an engineer fires about
        /// once every twenty seconds, so a script deploying by index spent a third of its silver on men who cannot
        /// shoot; paratroopers are dropped by the air card and are not deployable from a trench. -1 if it fields none.
        /// </summary>
        int ArmedSlot(SimWorld pw, int n)
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
        bool Armed(SimWorld pw, int slot)
        {
            var e = pw.Roster[Side * RosterEntry.SlotCount + slot];
            if (e.IsVehicle) return false;
            return e.Archetype != InfantryArchetype.Medic && e.Archetype != InfantryArchetype.Repair
                && e.Archetype != InfantryArchetype.Para;
        }

        /// <summary>The enemy's first machine, whatever its faction calls it; -1 if it fields none.</summary>
        int MachineSlot(SimWorld pw)
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                if (pw.Roster[Side * RosterEntry.SlotCount + s].IsVehicle) return s;
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
                    int cost = pw.Roster[Side * RosterEntry.SlotCount + slot].Cost;
                    // men first: silver is kept back for the barrage only once it fields the men for the attack it wants,
                    // Odds times the player's front garrison, counting those still walking up. Saving at parity, as it
                    // did, held it at the player's count for good and it never had the odds to attack (Play, 2026-09-29)
                    short theirs = view.Fields.FrontTrench(Other);
                    int held = theirs >= 0 ? view.Fields.Trenches[theirs].GarrisonCount : 0;
                    int army = Mathf.Max(AttackGarrison, Mathf.CeilToInt(WantedOdds(t) * held));
                    int reserve = UsesSupport && t > 600 && MenOf(pw, Side) >= army ? SupportReserve : 0;
                    // threatened (the player's front garrison is at least eight and no smaller than its own), it keeps an
                    // SOS barrage's price in hand: spending every coin on men, it never had one when an attack came over
                    short mineFront = view.Fields.FrontTrench(Side);
                    int ours = mineFront >= 0 ? view.Fields.Trenches[mineFront].GarrisonCount : 0;
                    if (UsesSupport && Defends && t > 600 && held >= Mathf.Max(AttackGarrison, ours)
                        && OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var sos))
                        reserve = Mathf.Max(reserve, sos.Cost);
                    if (pw.Silver[Side] >= cost + reserve) enemy.Issue(SimCommand.Deploy(t, Side, slot));
                }
            }
            if (DeploysTanks && t % 100 == 70)
            {
                int slot = MachineSlot(pw);
                int ri = Side * RosterEntry.SlotCount + slot;
                if (slot >= 0 && pw.SlotCooldown[ri] == 0 && pw.Silver[Side] >= pw.Roster[ri].Cost)
                    enemy.Issue(SimCommand.Deploy(t, Side, slot));
            }
            if (t % 100 == 20)
            {
                // every trench behind the front is locked so reinforcements walk through to the front line
                short front = view.Fields.FrontTrench(Side);
                for (int k = 0; k < view.Fields.Trenches.Length; k++)
                {
                    var ts = view.Fields.Trenches[k];
                    if (ts.OwnerTeam != Side) continue;
                    byte want = (byte)(k != front ? 1 : 0);
                    if (ts.Locked != want) enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.TrenchLock, A = k, B = want });
                    if (k != front && ts.GarrisonCount > 0) enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.TrenchAdvance, A = k });
                }
            }
            if (UsesSupport && Defends && PlannedAttack == 0 && t % 20 == 10 && view.Abilities != null) Sos(view, enemy, t);
            if (Attacks && PlannedAttack != 0 && t >= PlannedAttack)
            {
                // the barrage is coming down: over the top now, whatever the count, or the shells were wasted
                PlannedAttack = 0;
                short front = view.Fields.FrontTrench(Side), theirs = view.Fields.FrontTrench(Other);
                if (front >= 0 && view.Fields.Trenches[front].GarrisonCount > 0)
                {
                    lastAttack = t;
                    enemy.Issue(OverTheTop(t, front));
                    Said?.Invoke($"{t / 20} s over the top behind the barrage, {view.Fields.Trenches[front].GarrisonCount} against {(theirs >= 0 ? view.Fields.Trenches[theirs].GarrisonCount : 0)}");
                }
            }
            if (Attacks && PlannedAttack == 0 && t % 100 == 50)
            {
                short front = view.Fields.FrontTrench(Side), theirs = view.Fields.FrontTrench(Other);
                int mine = front >= 0 ? view.Fields.Trenches[front].GarrisonCount : 0;
                int held = theirs >= 0 ? view.Fields.Trenches[theirs].GarrisonCount : 0;
                if (front >= 0 && mine >= AttackGarrison)
                {
                    float want = WantedOdds(t);
                    if (mine >= (Odds + 1f) * held)
                    {
                        lastAttack = t;
                        enemy.Issue(OverTheTop(t, front));
                        Said?.Invoke($"{t / 20} s over the top bare, {mine} against {held}");
                    }
                    else if (mine >= want * held && UsesSupport && Barrage(view, enemy, t, theirs))
                    {
                        PlannedAttack = t + (uint)Mathf.Max(1, BarrageLeadTicks);
                        Said?.Invoke($"{t / 20} s barrage called{(want < Odds ? $" (patience, odds {want:0.0})" : "")}, {mine} against {held}");
                    }
                }
            }
            if (UsesSupport && PlannedAttack == 0 && t % 200 == 150 && view.Abilities != null)
            {
                short mine = view.Fields.FrontTrench(Other);
                var ability = (supportCount & 1) == 0 ? OffMapAbilityId.HeBarrage : OffMapAbilityId.ChlorineGas;
                // harassing fire only out of silver to spare: the reserve stays for the barrage before an attack
                if (mine >= 0 && view.Fields.Trenches[mine].GarrisonCount >= 6 && view.Abilities.CooldownOf(Side, ability) == 0
                    && OffMapAbilitySystem.TryGetStats((int)ability, out var stats) && pw.Silver[Side] >= stats.Cost + SupportReserve)
                {
                    Vector3 sum = Vector3.zero; int n = 0;
                    for (int i = 0; i < pw.HighWater; i++)
                        if (pw.IsAlive(i) && pw.TrenchId[i] == mine) { sum += (Vector3)pw.Position[i]; n++; }
                    if (n > 0)
                    {
                        sum /= n;
                        // gas is released upwind (the map wind blows toward -Z) so the cloud rolls over the trench
                        float dz = ability == OffMapAbilityId.ChlorineGas ? 12f : 0f;
                        enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.SupportFire, A = (int)ability, Pos = new Unity.Mathematics.float3(sum.x, 0f, sum.z + dz) });
                        supportCount++;
                    }
                }
            }
        }

        /// <summary>Over the top from <paramref name="front"/>: everyone but the machine gunners, who stay on the parapet
        /// and fire over the attack (OrderGroup.Gun, v22). Four guns covering twenty men cost the attack about half the
        /// men that sending them with it did (AssaultLadderTests, behind smoke and a barrage).</summary>
        SimCommand OverTheTop(uint t, short front)
            => new SimCommand { Tick = t, Player = Side, Type = CommandType.TrenchSelectAdvance, A = front, B = OrderGroup.All & ~OrderGroup.Gun };

        /// <summary>The middle of a trench along z, NaN when it has no cells.</summary>
        static float TrenchZ(MatchSim view, short trench)
        {
            if (trench < 0) return float.NaN;
            var def = view.Map.Trenches[trench];
            return def.CellCount == 0 ? float.NaN : view.Map.NavCellCenter(view.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
        }

        /// <summary>The SOS barrage (2026-09-29): SosMen or more of the player's men on foot in the open between 8 m and
        /// SosReach in front of its front trench bring an HE line down on them, 60 m across their middle and 10 m nearer
        /// the trench than they are (the shells take four seconds, and they are coming on), never nearer than 12 m to it.
        /// It spends the attack's reserve on it: holding the trench comes first. Not while its own attack is planned or
        /// its own men are out near the mark. Before, it only ever shelled the player's trench, on a timer.</summary>
        bool Sos(MatchSim view, ICommandSink enemy, uint t)
        {
            var pw = view.World;
            short own = view.Fields.FrontTrench(Side), theirs = view.Fields.FrontTrench(Other);
            float ownZ = TrenchZ(view, own), theirZ = TrenchZ(view, theirs);
            if (float.IsNaN(ownZ) || float.IsNaN(theirZ) || Mathf.Abs(theirZ - ownZ) < 1f) return false;
            if (view.Abilities.CooldownOf(Side, OffMapAbilityId.HeBarrage) != 0) return false;
            if (!OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var stats) || pw.Silver[Side] < stats.Cost) return false;
            float toward = Mathf.Sign(theirZ - ownZ);   // out of its trench, into no man's land
            float sx = 0f, sz = 0f; int n = 0;
            for (int i = 0; i < pw.HighWater; i++)
            {
                uint f = pw.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0 || pw.Team[i] != Other || pw.TrenchId[i] >= 0) continue;
                float ahead = (pw.Position[i].z - ownZ) * toward;
                if (ahead < 8f || ahead > SosReach) continue;
                sx += pw.Position[i].x; sz += pw.Position[i].z; n++;
            }
            if (n < SosMen) return false;
            sx /= n; sz /= n;
            float z = ownZ + toward * Mathf.Max(12f, (sz - ownZ) * toward - 10f);
            for (int i = 0; i < pw.HighWater; i++)
                if (pw.IsAlive(i) && pw.Team[i] == Side && pw.TrenchId[i] < 0 && Mathf.Abs(pw.Position[i].z - z) < 20f) return false;   // its own men are out there
            float width = view.Map.SizeMeters.x;
            float start = Mathf.Clamp(sx - 30f, 0f, Mathf.Max(0f, width - 60f));
            enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage,
                                         Pos = new Unity.Mathematics.float3(start, 0f, z), B = AbilityArgs.Pack(90, AbilityPattern.Line, 60) });
            supportCount++;
            Said?.Invoke($"{t / 20} s SOS barrage on {n} men coming over");
            return true;
        }

        /// <summary>Men of <paramref name="team"/> alive on foot.</summary>
        static int MenOf(SimWorld w, int team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++)
                if ((w.Flags[i] & (uint)UnitFlags.Alive) != 0 && (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0 && w.Team[i] == team) n++;
            return n;
        }

        /// <summary>The preparation for an attack on <paramref name="trench"/>: an HE line 60 m along it through the
        /// middle of its garrison, and, if the silver runs to it, a smoke screen just in front of it on the attacker's
        /// side, so its machine guns are blind as the men cross (a garrison with two gunners in ten beat every bare
        /// attack up to three to one, and two to one behind both took it four times in four: AssaultLadderTests).
        /// True when the barrage was called.</summary>
        bool Barrage(MatchSim view, ICommandSink enemy, uint t, short trench)
        {
            var pw = view.World;
            if (trench < 0 || view.Abilities == null || view.Abilities.CooldownOf(Side, OffMapAbilityId.HeBarrage) != 0) return false;
            if (!OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var stats) || pw.Silver[Side] < stats.Cost) return false;
            Vector3 sum = Vector3.zero; int n = 0;
            for (int i = 0; i < pw.HighWater; i++)
                if (pw.IsAlive(i) && pw.TrenchId[i] == trench) { sum += (Vector3)pw.Position[i]; n++; }
            if (n == 0)
            {
                // nobody in it: the trench's own middle, so the men who walk up into it walk into the shells
                var def = view.Map.Trenches[trench];
                if (def.CellCount == 0) return false;
                sum = (Vector3)view.Map.NavCellCenter(view.Map.TrenchCells[def.CellStart + def.CellCount / 2]); n = 1;
            }
            sum /= n;
            float width = view.Map.SizeMeters.x;
            float start = Mathf.Clamp(sum.x - 30f, 0f, Mathf.Max(0f, width - 60f));
            enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage,
                                         Pos = new Unity.Mathematics.float3(start, 0f, sum.z), B = AbilityArgs.Pack(90, AbilityPattern.Line, 60) });
            supportCount++;
            if (OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.SmokeScreen, out var smoke) && view.Abilities.CooldownOf(Side, OffMapAbilityId.SmokeScreen) == 0
                && pw.Silver[Side] >= stats.Cost + smoke.Cost)
            {
                short own = view.Fields.FrontTrench(Side);
                float toward = own >= 0 && view.Map.Trenches[own].CellCount > 0
                    ? Mathf.Sign(view.Map.NavCellCenter(view.Map.TrenchCells[view.Map.Trenches[own].CellStart]).z - sum.z) : 1f;
                // one screen (its cooldown refuses a second the same tick), 40 m across the middle of the garrison
                float from = Mathf.Clamp(sum.x - smoke.Length * 0.5f, 0f, Mathf.Max(0f, width - smoke.Length));
                enemy.Issue(new SimCommand { Tick = t, Player = Side, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.SmokeScreen,
                                             Pos = new Unity.Mathematics.float3(from, 0f, sum.z + 12f * toward), B = AbilityArgs.Pack(90, 0, (int)smoke.Length) });
            }
            return true;
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
