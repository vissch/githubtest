// Phase: A3 (implemented 2026-09-25) — the Victoria Cross moment. From the design chat: "Any soldier can snap into a
// hero moment ... The chance rises the worse things are: pinned, outnumbered, officer dead. A hidden pity
// guarantees one eventually. Losing becomes a story rather than a wall." Owner decisions: sim-side, and the hero
// RALLIES his section (a lone man can capture nothing: an objective needs three men and no defenders).
// Once a second each infantryman of a team in TeamMask is scored for desperation S: pinned 3 / suppressed 1,
// outnumbered in the 20 m cells round him 2 or 3, half his hit points gone 1, out in the open under orders 1,
// and one more for having no officer over him when any of that is true (it deepens desperation, never starts it). Only a man at S >= NaturalFrom rolls, at BaseChance x S. Pity gathers S x PityPerPoint per
// check and, once past 1, FORCES the most desperate man seen in the last CandidateTicks — once a match
// (ForcedUsed), so the beat lands once, late, when the side is losing, and never becomes a metronome. One live
// hero a team, TeamCooldown between two.
// The moment: he is whole again, unpinnable (Suppression capped), faster, hits harder (HeroScale, folded into
// AuraSystem.DamageMul), flagged Hero, and every friendly infantryman within RallyRadius goes over the top with him
// on the goal a ">>" from his trench would give (TrenchOrdersSystem.Advance). His feats are DirectFire's kills.
// Veterans: a UnitDeployed with a rank (SimCommand.Deploy B, the profile's named men) gets more of everything
// (VeteranPerRank) and the Veteran flag. Everything here is hashed; the profile only ever supplies HeroPity.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public static class HeroRules
    {
        public const uint CheckEvery = 20;
        public const float CountCell = 20f;
        public const int WindowTicks = 600;
        public const float DamageBonus = 1.5f, SpeedBonus = 1.3f, SuppressionCap = 40f;
        public const float BaseChance = 0.0001f, PityPerPoint = 0.0002f, PityThreshold = 1f;
        public const int NaturalFrom = 4;
        public const int TeamCooldownTicks = 1200;
        public const uint CandidateTicks = 20;
        public const float RallyRadius = 20f;
        public const float VeteranPerRank = 0.10f;
        public const int PinnedPts = 3, SuppressedPts = 1, OutnumberedPts = 2, OutnumberedHardPts = 3, NoOfficerPts = 1, HurtPts = 1, ExposedPts = 1;
    }

    public sealed class HeroSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Hero;

        readonly MapData map;
        FlowFieldManager fields;
        DirectFireSystem fire;
        AuraSystem aura;
        IAuraProvider cover;

        /// <summary>Bit per team that may have heroes. The player's side only, until the enemy has a story of its own.</summary>
        public int TeamMask = 1;
        public float BaseChance = HeroRules.BaseChance;

        // ---- per slot, hashed ----
        public NativeArray<int> HeroTicks, HeroId;
        public NativeArray<ushort> HeroGen, HeroKills, seenGen;
        public NativeArray<float> BaseSpeed, HeroScale;
        public NativeArray<byte> VeteranRank;
        // ---- per team, hashed ----
        public NativeArray<float> Pity;
        public NativeArray<int> Cooldown, CandSlot, CandS;
        public NativeArray<uint> CandTick;
        public NativeArray<ushort> CandGen;
        public NativeArray<byte> ForcedUsed;
        public int HeroCount;
        // ---- transient ----
        NativeArray<int> counts;
        int gridW, gridL;

        public HeroSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("HeroSystem needs FlowFieldManager");
            fire = world.GetSystem<DirectFireSystem>();
            aura = world.GetSystem<AuraSystem>();
            cover = aura;
            int n = world.Config.MaxSlots, p = SimConfig.MaxPlayers;
            HeroTicks = new NativeArray<int>(n, Allocator.Persistent);
            HeroId = new NativeArray<int>(n, Allocator.Persistent);
            HeroGen = new NativeArray<ushort>(n, Allocator.Persistent);
            HeroKills = new NativeArray<ushort>(n, Allocator.Persistent);
            seenGen = new NativeArray<ushort>(n, Allocator.Persistent);
            BaseSpeed = new NativeArray<float>(n, Allocator.Persistent);
            HeroScale = new NativeArray<float>(n, Allocator.Persistent);
            VeteranRank = new NativeArray<byte>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) HeroScale[i] = 1f;
            Pity = new NativeArray<float>(p, Allocator.Persistent);
            Cooldown = new NativeArray<int>(p, Allocator.Persistent);
            CandSlot = new NativeArray<int>(p, Allocator.Persistent);
            CandS = new NativeArray<int>(p, Allocator.Persistent);
            CandTick = new NativeArray<uint>(p, Allocator.Persistent);
            CandGen = new NativeArray<ushort>(p, Allocator.Persistent);
            ForcedUsed = new NativeArray<byte>(p, Allocator.Persistent);
            for (int t = 0; t < p; t++) CandSlot[t] = -1;
            Pity[0] = math.saturate(world.Config.HeroPity0); Pity[1] = math.saturate(world.Config.HeroPity1);
            gridW = math.max(1, (int)math.ceil(map.SizeMeters.x / HeroRules.CountCell));
            gridL = math.max(1, (int)math.ceil(map.SizeMeters.y / HeroRules.CountCell));
            counts = new NativeArray<int>(gridW * gridL * 2, Allocator.Persistent);
            aura?.SetHeroScale(HeroScale);
        }

        public bool IsHero(int slot) => slot >= 0 && slot < HeroTicks.Length && HeroTicks[slot] > 0;

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            // fresh slots
            for (int i = 0; i < n; i++)
                if (seenGen[i] != w.Generation[i]) { seenGen[i] = w.Generation[i]; HeroTicks[i] = 0; HeroId[i] = 0; HeroKills[i] = 0; HeroScale[i] = 1f; VeteranRank[i] = 0; }

            // veterans: the rank rode in the deploy command (SimWorld.Deploy / the sea lift emit UnitDeployed)
            var ev = w.Events.Events;
            int evCount = ev.Length;
            for (int e = 0; e < evCount; e++)
            {
                var d = ev[e];
                if (d.Type != SimEventType.UnitDeployed || d.Dir.x <= 0f) continue;
                int s = d.A;
                if (!w.IsAlive(s) || (w.Flags[s] & (uint)UnitFlags.Vehicle) != 0) continue;
                byte rank = (byte)math.clamp((int)d.Dir.x, 1, 3);
                float more = 1f + HeroRules.VeteranPerRank * rank;
                VeteranRank[s] = rank;
                w.MaxHp[s] *= more; w.Hp[s] = w.MaxHp[s];
                HeroScale[s] = more;
                w.Flags[s] |= (uint)UnitFlags.Veteran;
                w.Events.Add(w.Tick, SimEventType.VeteranDeployed, s, rank, w.Position[s], new float3(d.B, w.Team[s], 0f));
            }

            // who stands where, in 20 m cells
            for (int c = 0; c < counts.Length; c++) counts[c] = 0;
            for (int i = 0; i < n; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                counts[Cell(w.Position[i]) * 2 + (w.Team[i] & 1)]++;
            }

            // live heroes
            var live = new NativeArray<int>(SimConfig.MaxPlayers, Allocator.Temp);
            for (int i = 0; i < n; i++)
            {
                if (HeroTicks[i] <= 0) continue;
                if (!w.IsAlive(i) || w.Generation[i] != HeroGen[i])
                {
                    w.Events.Add(w.Tick, SimEventType.HeroFallen, i, HeroId[i], w.Position[i], new float3(0f, w.Team[i], 0f), HeroKills[i]);
                    HeroTicks[i] = 0; HeroScale[i] = 1f;
                    continue;
                }
                if (fire != null)
                {
                    var killed = fire.Killed;
                    for (int k = 0; k < killed.Length; k++)
                        if (killed[k].y == i) { HeroKills[i]++; w.Events.Add(w.Tick, SimEventType.HeroFeat, i, HeroId[i], w.Position[i], new float3(0f, w.Team[i], 0f), HeroKills[i]); }
                }
                if (w.Suppression[i] > HeroRules.SuppressionCap) w.Suppression[i] = HeroRules.SuppressionCap;
                w.Flags[i] |= (uint)UnitFlags.Hero;
                if (--HeroTicks[i] == 0)
                {
                    w.Speed[i] = BaseSpeed[i];
                    HeroScale[i] = 1f + HeroRules.VeteranPerRank * VeteranRank[i];
                    w.Flags[i] &= ~(uint)UnitFlags.Hero;
                    w.Events.Add(w.Tick, SimEventType.HeroSurvived, i, HeroId[i], w.Position[i], new float3(0f, w.Team[i], 0f), HeroKills[i]);
                }
                else live[w.Team[i] & 1]++;
            }

            for (int t = 0; t < SimConfig.MaxPlayers; t++) if (Cooldown[t] > 0) Cooldown[t]--;
            if (w.WinnerTeam < 0)
            {
                // the once-a-second look at each man
                for (int i = 0; i < n; i++)
                {
                    if ((uint)i % HeroRules.CheckEvery != w.Tick % HeroRules.CheckEvery) continue;
                    uint f = w.Flags[i];
                    if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0 || HeroTicks[i] > 0) continue;
                    int team = w.Team[i] & 1;
                    if ((TeamMask & (1 << team)) == 0) continue;
                    int s = Desperation(w, i, f);
                    if (s <= 0) continue;
                    Pity[team] = math.min(HeroRules.PityThreshold, Pity[team] + s * HeroRules.PityPerPoint);
                    if (s > CandS[team] || w.Tick - CandTick[team] >= HeroRules.CandidateTicks || CandSlot[team] < 0)
                    { CandSlot[team] = i; CandS[team] = s; CandTick[team] = w.Tick; CandGen[team] = w.Generation[i]; }
                    if (Cooldown[team] > 0 || live[team] > 0 || s < HeroRules.NaturalFrom) continue;
                    var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Hero, (uint)i);
                    if (rng.NextFloat() < BaseChance * s) { Trigger(w, i, s, false); live[team]++; }
                }
                // the pity: the most desperate man seen lately, once a match
                for (int t = 0; t < SimConfig.MaxPlayers; t++)
                {
                    if ((TeamMask & (1 << t)) == 0 || ForcedUsed[t] != 0 || Pity[t] < HeroRules.PityThreshold) continue;
                    if (Cooldown[t] > 0 || live[t] > 0) continue;
                    int c = CandSlot[t];
                    if (c < 0 || !w.IsAlive(c) || w.Generation[c] != CandGen[t] || w.Tick - CandTick[t] >= HeroRules.CandidateTicks || HeroTicks[c] > 0) continue;
                    ForcedUsed[t] = 1;
                    Trigger(w, c, CandS[t], true); live[t]++;
                }
            }
            live.Dispose();
        }

        int Cell(float3 p)
        {
            int cx = math.clamp((int)(p.x / HeroRules.CountCell), 0, gridW - 1);
            int cz = math.clamp((int)(p.z / HeroRules.CountCell), 0, gridL - 1);
            return cz * gridW + cx;
        }

        int Desperation(SimWorld w, int i, uint f)
        {
            int s = 0;
            float supp = w.Suppression[i];
            if (supp >= SuppressionRules.PinnedThreshold) s += HeroRules.PinnedPts;
            else if (supp >= SuppressionRules.ProneThreshold) s += HeroRules.SuppressedPts;
            int team = w.Team[i] & 1, friends = 0, enemies = 0;
            int cell = Cell(w.Position[i]);
            int cx = cell % gridW, cz = cell / gridW;
            for (int dz = -1; dz <= 1; dz++)
            for (int dx = -1; dx <= 1; dx++)
            {
                int x = cx + dx, z = cz + dz;
                if (x < 0 || z < 0 || x >= gridW || z >= gridL) continue;
                friends += counts[(z * gridW + x) * 2 + team];
                enemies += counts[(z * gridW + x) * 2 + (1 - team)];
            }
            if (enemies >= 2 * friends + 2) s += HeroRules.OutnumberedHardPts;
            else if (enemies >= friends + 3) s += HeroRules.OutnumberedPts;
            if (s > 0 && cover != null && !cover.Covered(i)) s += HeroRules.NoOfficerPts;   // a missing officer deepens desperation, never starts it
            if (w.Hp[i] < 0.5f * w.MaxHp[i]) s += HeroRules.HurtPts;
            if ((f & (uint)UnitFlags.Exposed) != 0) s += HeroRules.ExposedPts;
            return s;
        }

        void Trigger(SimWorld w, int i, int s, bool forced)
        {
            byte team = w.Team[i];
            HeroId[i] = ++HeroCount;
            HeroGen[i] = w.Generation[i];
            HeroTicks[i] = HeroRules.WindowTicks;
            HeroKills[i] = 0;
            BaseSpeed[i] = w.Speed[i];
            w.Speed[i] *= HeroRules.SpeedBonus;
            HeroScale[i] = HeroRules.DamageBonus * (1f + HeroRules.VeteranPerRank * VeteranRank[i]);
            w.Suppression[i] = 0f;
            w.Hp[i] = w.MaxHp[i];
            w.Flags[i] |= (uint)UnitFlags.Hero;
            // the rally: the men round him go over the top with him, on the goal a ">>" from his trench would give
            short front = w.TrenchId[i] >= 0 ? w.TrenchId[i] : w.SourceTrench[i] >= 0 ? w.SourceTrench[i] : fields.FrontTrench(team);
            int goal = front >= 0 ? fields.NextGoalFrom(front, team) : -1;
            float3 at = w.Position[i];
            float r2 = HeroRules.RallyRadius * HeroRules.RallyRadius;
            for (int j = 0; j < w.HighWater; j++)
            {
                uint fj = w.Flags[j];
                if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.Vehicle) != 0 || w.Team[j] != team) continue;
                float3 d = w.Position[j] - at; d.y = 0f;
                if (math.lengthsq(d) > r2) continue;
                if (goal >= 0) w.GoalId[j] = goal;
                if (w.TrenchId[j] >= 0)
                {
                    w.SourceTrench[j] = w.TrenchId[j]; w.TrenchId[j] = -1; w.PostCell[j] = -1; w.PostKind[j] = 0;
                    w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, j, w.SourceTrench[j], w.Position[j]);
                }
                if (w.Suppression[j] >= SuppressionRules.PinnedThreshold) w.Suppression[j] = SuppressionRules.PinnedThreshold - 1f;   // they get up and go
                w.Flags[j] |= (uint)UnitFlags.Exposed;
            }
            Pity[team & 1] = 0f;
            Cooldown[team & 1] = HeroRules.TeamCooldownTicks;
            w.Events.Add(w.Tick, SimEventType.HeroMoment, i, HeroId[i], at, new float3(w.Generation[i], team, 0f), forced ? -s : s);
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(HeroTicks, h); h = SimHash.Array(HeroId, h); h = SimHash.Array(HeroGen, h); h = SimHash.Array(HeroKills, h);
            h = SimHash.Array(seenGen, h); h = SimHash.Array(BaseSpeed, h); h = SimHash.Array(HeroScale, h); h = SimHash.Array(VeteranRank, h);
            h = SimHash.Array(Pity, h); h = SimHash.Array(Cooldown, h); h = SimHash.Array(CandSlot, h); h = SimHash.Array(CandS, h);
            h = SimHash.Array(CandTick, h); h = SimHash.Array(CandGen, h); h = SimHash.Array(ForcedUsed, h);
            return SimHash.Value(new int2(HeroCount, TeamMask), h);
        }

        public void Dispose()
        {
            if (HeroTicks.IsCreated) HeroTicks.Dispose();
            if (HeroId.IsCreated) HeroId.Dispose();
            if (HeroGen.IsCreated) HeroGen.Dispose();
            if (HeroKills.IsCreated) HeroKills.Dispose();
            if (seenGen.IsCreated) seenGen.Dispose();
            if (BaseSpeed.IsCreated) BaseSpeed.Dispose();
            if (HeroScale.IsCreated) HeroScale.Dispose();
            if (VeteranRank.IsCreated) VeteranRank.Dispose();
            if (Pity.IsCreated) Pity.Dispose();
            if (Cooldown.IsCreated) Cooldown.Dispose();
            if (CandSlot.IsCreated) CandSlot.Dispose();
            if (CandS.IsCreated) CandS.Dispose();
            if (CandTick.IsCreated) CandTick.Dispose();
            if (CandGen.IsCreated) CandGen.Dispose();
            if (ForcedUsed.IsCreated) ForcedUsed.Dispose();
            if (counts.IsCreated) counts.Dispose();
        }
    }
}
