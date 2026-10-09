// Phase: wrecks (2026-09-28, implemented) — part of DirectFireSystem — depends on: PropHarm, PropRules, MapData.CellCover
// Sustained fire wears a wreck (owner, 2026-09-28): a machine gun's round that a man's cover stopped (FireJob: the same
// roll, a miss that would have hit with no cover) is recorded with its target's cell and the prop's share of the cover.
// After the job, in the order they were fired, each goes to the wreck that gives that cell its cover (the wreckage
// within one cell of it with the most cover; ties to the lower index) through PropHarm: PropWorn b = 1 while the stage
// stands, the next stage when its hit points run out. A cell whose cover is a tree's wears nothing (trees do not wear
// under gunfire, plan default). Rifles do not wear wrecks (CombatTables.WearsWrecks: a machine gun's rate of fire).
// Guns with nobody to shoot at (owner, 2026-09-28: "automatic only", no order, no HUD): each tick the wrecks men are
// behind are listed with their sides (Shelter: within ShelterRadius of the wreck's body). A man or a machine in the fire
// job with no target (a living target always wins), not pinned and not holding a trench, fires at the nearest wreck in
// his range and sight that shelters his enemies: a Shot whose b names the prop (PropTarget), a hit (a hull-sized mark)
// from a machine gun wears it by WreckFireShare of the round, and every round keeps the heads behind it down (the near
// miss's suppression on each enemy within its reach). Nothing is drawn for a man with a target, so a field with no one
// behind a wreck fights as before. Tank guns are TankGunnerySystem's and do not do this.
// The props' hit points are in MapData's hash, so this system still hashes nothing of its own.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    /// <summary>A round a prop's cover stopped, or one fired at a wreck: the target's nav cell, what it does to the wreck,
    /// which way it went. Target 0: the wreck covering Cell; a prop's PropTarget.Encode (<= -2): that wreck.</summary>
    public struct WreckRound
    {
        public int Cell, Target;
        public float Damage;
        public float3 Way;
    }

    /// <summary>A wreck men are behind this tick: where it is, which, how far its shelter reaches, whose men (a bit a side).</summary>
    public struct Shelter
    {
        public float3 Pos;
        public int Prop, Teams;
        public float Reach;
    }

    public sealed partial class DirectFireSystem
    {
        NativeList<WreckRound> stopped;
        NativeList<Shelter> shelters;
        /// <summary>Men this far past a wreck's body (WreckBlastReach x size, halved) are behind it.</summary>
        public const float ShelterRadius = 3f;
        /// <summary>What a machine gun's round that hits a wreck it was fired at does to it, of the round's damage.</summary>
        public const float WreckFireShare = 0.5f;
        FlowFieldManager fields;
        ulong wearChecksum = SimHash.Offset;   // PropHarm folds stage changes here; the map's hash already has them
        /// <summary>Rounds that wore a wreck since the match started. Derived from hashed state, not hashed itself.</summary>
        public int WreckRounds;

        /// <summary>This tick's stopped rounds, each on the wreck giving its cell cover, in the order they were fired.</summary>
        void WearWrecks(SimWorld w)
        {
            if (stopped.Length == 0) return;
            bool nav = false;
            for (int k = 0; k < stopped.Length; k++)
            {
                var round = stopped[k];
                int p = PropTarget.IsProp(round.Target) ? PropTarget.Decode(round.Target) : WreckCovering(round.Cell);
                if (p < 0) continue;
                var harm = PropHarm.Harm(w, map, p, round.Damage, round.Way, 1, ref wearChecksum, out bool changed);
                if (harm == PropHarm.Outcome.None) continue;
                WreckRounds++;
                nav |= changed;
            }
            if (nav)
            {
                if (fields == null) fields = w.GetSystem<FlowFieldManager>();
                fields?.MarkCostDirty(0);
            }
        }

        /// <summary>The wrecks with men behind them this tick, in prop order, from the spatial hash (a tick old, as the near
        /// miss's). Main thread, before the fire job.</summary>
        void BuildShelters(SimWorld w)
        {
            shelters.Clear();
            var hash = movement.Spatial;
            for (int p = 0; p < map.Props.Length; p++)
            {
                var prop = map.Props[p];
                if (!PropRules.IsWreckage(prop.Kind) || prop.Hp <= 0f) continue;
                float reach = ShelterRadius + PropRules.WreckBlastReach * 0.5f * (prop.Scale > 0f ? prop.Scale : 1f);
                int x0 = math.max(0, (int)((prop.Pos.x - reach) / hash.CellSize)), x1 = math.min(hash.Width - 1, (int)((prop.Pos.x + reach) / hash.CellSize));
                int z0 = math.max(0, (int)((prop.Pos.z - reach) / hash.CellSize)), z1 = math.min(hash.Length - 1, (int)((prop.Pos.z + reach) / hash.CellSize));
                int teams = 0;
                for (int z = z0; z <= z1; z++)
                for (int x = x0; x <= x1; x++)
                {
                    if (!hash.Map.TryGetFirstValue(hash.KeyXZ(x, z), out int j, out var it)) continue;
                    do
                    {
                        uint f = w.Flags[j];
                        if ((f & (uint)UnitFlags.Alive) == 0 || (f & ((uint)UnitFlags.Vehicle | (uint)UnitFlags.Airborne)) != 0) continue;
                        if (math.distancesq(w.Position[j].xz, prop.Pos.xz) <= reach * reach) teams |= 1 << (w.Team[j] & 7);
                    } while (hash.Map.TryGetNextValue(out j, ref it));
                }
                if (teams != 0) shelters.Add(new Shelter { Pos = prop.Pos, Prop = p, Teams = teams, Reach = reach });
            }
        }

        partial struct FireJob
        {
            /// <summary>Slot i has nobody to shoot at: the nearest wreck in its range and sight with its enemies behind
            /// it, fired at (the fire job's own cooldown and roll). A pinned man and a garrison holding its trench do not,
            /// nor a knocked-out hull: it has no target because it is dead, not because nobody is seen ([N5.2]).</summary>
            void AtWreck(int i)
            {
                uint f = Flags[i];
                if ((f & ((uint)UnitFlags.Airborne | (uint)UnitFlags.KnockedOut)) != 0 || Suppression[i] >= SuppressionRules.PinnedThreshold || TrenchId[i] >= 0) return;
                var weapon = Weapons[Archetype[i]];
                if (weapon.Damage <= 0f || weapon.RangeMax <= 0f) return;
                float3 p = Position[i];
                float range = weapon.RangeMax;
                if ((f & (uint)UnitFlags.Exposed) != 0 && (f & (uint)UnitFlags.Vehicle) == 0) range = math.min(range, CombatTables.AdvanceFireRange);
                int mine = 1 << (Team[i] & 7), best = -1;
                float bestSq = range * range;
                for (int k = 0; k < Shelters.Length; k++)
                {
                    if ((Shelters[k].Teams & ~mine) == 0) continue;   // only his own side behind it
                    float d = math.distancesq(Shelters[k].Pos.xz, p.xz);
                    if (d < bestSq) { bestSq = d; best = k; }
                }
                if (best < 0) return;
                var s = Shelters[best];
                float3 eye = new float3(p.x, Height.Sample(p.x, p.z) + HeightfieldRaycast.EyeHeight((Stance)StanceOf[i]), p.z);
                float3 top = new float3(s.Pos.x, Height.Sample(s.Pos.x, s.Pos.z) + 1.5f, s.Pos.z);
                if (!HeightfieldRaycast.HasLineOfSight(Height, eye, top)) return;

                FireCooldown[i] = CombatTables.CooldownTicks(weapon, TickSeconds);
                float3 d3 = s.Pos - p; d3.y = 0f;
                float dist = SimMath.Length(d3);
                float3 dir = dist > 1e-3f ? d3 / dist : new float3(0f, 0f, 1f);
                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = PropTarget.Encode(s.Prop), Pos = p, Dir = dir, Scalar = 0f });
                float chance = weapon.Accuracy * CombatTables.RangeFalloff(dist, weapon.RangeMax) * CombatTables.HullTargetBonus * (1f - 0.5f * math.saturate(Suppression[i] * 0.01f));
                if (SimMath.Length(Velocity[i]) > CombatTables.MovingSpeed && (f & (uint)UnitFlags.Vehicle) == 0) chance *= CombatTables.MovingAccuracy;
                var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                if (rng.NextFloat() < math.clamp(chance, 0.02f, 0.95f) && CombatTables.WearsWrecks(weapon))
                    CoverStops.Add(new WreckRound { Cell = CellOf(s.Pos), Target = PropTarget.Encode(s.Prop), Damage = weapon.Damage * DamageMul[i] * WreckFireShare, Way = dir });
                // hit or not, every round keeps their heads down
                float near = weapon.SuppressionPerShot * 0.6f * (InSmoke(s.Pos) ? CombatTables.SmokeSuppression : 1f);
                int cx0 = math.clamp((int)((s.Pos.x - s.Reach) / Spatial.CellSize), 0, Spatial.Width - 1), cx1 = math.clamp((int)((s.Pos.x + s.Reach) / Spatial.CellSize), 0, Spatial.Width - 1);
                int cz0 = math.clamp((int)((s.Pos.z - s.Reach) / Spatial.CellSize), 0, Spatial.Length - 1), cz1 = math.clamp((int)((s.Pos.z + s.Reach) / Spatial.CellSize), 0, Spatial.Length - 1);
                for (int z = cz0; z <= cz1; z++)
                for (int x = cx0; x <= cx1; x++)
                {
                    if (!Spatial.Map.TryGetFirstValue(Spatial.KeyXZ(x, z), out int j, out var it)) continue;
                    do
                    {
                        if ((Flags[j] & (uint)UnitFlags.Alive) == 0 || Hp[j] <= 0f || Team[j] == Team[i]) continue;
                        if (math.distancesq(Position[j].xz, s.Pos.xz) > s.Reach * s.Reach) continue;
                        AddSuppression(j, near * SuppressionMul[j]);
                    } while (Spatial.Map.TryGetNextValue(out j, ref it));
                }
            }
        }

        /// <summary>The wreckage prop that gives nav cell `cell` its cover: within one cell of it (MapData.StampCover's
        /// reach), the most cover, the lower index on a tie; -1 when the cover there is a tree's or nothing's.</summary>
        int WreckCovering(int cell)
        {
            int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
            int best = -1; byte bestCover = 0, anyCover = 0;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var prop = map.Props[i];
                int px = prop.Cell % map.NavWidth, pz = prop.Cell / map.NavWidth;
                if (math.abs(px - cx) > 1 || math.abs(pz - cz) > 1) continue;
                byte c = PropRules.CoverPercent(prop.Kind);
                if (c > anyCover) anyCover = c;
                if (!PropRules.IsWreckage(prop.Kind) || prop.Hp <= 0f || c <= bestCover) continue;
                best = i; bestCover = c;
            }
            return best >= 0 && bestCover >= anyCover ? best : -1;   // a tree's more cover there took it
        }
    }
}
