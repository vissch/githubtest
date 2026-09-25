// Phase: A2 (implemented) — depends on: MapData (heightfield, CellTrenchId), FlowFieldManager (trench hold-fire),
// HeightfieldRaycast, CombatTables. Smoke attenuation (A5) and sniper priorities (SpecialAbilities) come later.
// Staggered scan: one third of the slots per tick (slot % 3 == tick % 3); the other two ticks only re-validate the
// current target. A scan looks through a coarse 16 m grid for the three nearest engageable enemies and takes the
// first with a terrain line of sight. Ties break on the slot index, so the result does not depend on bucket order.
// Rules of engagement:
//  - a garrison below the rim (in a trench, not on the fire-step) can only be engaged from within 8 m or from
//    inside the same trench; a garrison whose trench is on hold-fire, or that is suppressed past 40, does not look;
//  - units under a >> order are running: they only engage within 60 m;
//  - small arms cannot hurt vehicles; infantry within 8 m close-assault them with grenades (a charge on the armour);
//  - a knocked-out vehicle (UnitFlags.KnockedOut) is no target and fires nothing. A tank's main guns choose their own
//    targets (TankGunnerySystem); what is found here is for its machine guns;
//  - a shield bearer (InfantrySpec.ShieldPlateMm) standing between a shooter and the man he picked, within
//    ShieldGuardRadius of that man and inside a 15-degree cone on the bearing, takes the shot instead (2026-09-25).
//    DirectFire then rolls the round against his plate.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class TargetAcquisitionSystem : ISimSystem
    {
        public const float GridCell = 16f;
        public int Order => SimSystemOrder.TargetAcquisition;

        readonly MapData map;
        FlowFieldManager fields;
        NativeParallelMultiHashMap<int, int> grid;
        int gridW, gridL;

        public TargetAcquisitionSystem(MapData map) { this.map = map; }

        CombatCatalogueSystem catalogue;

        public void Initialize(SimWorld world)
        {
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("TargetAcquisitionSystem needs CombatCatalogueSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("TargetAcquisitionSystem needs FlowFieldManager registered before it");
            gridW = (int)math.ceil(map.SizeMeters.x / GridCell);
            gridL = (int)math.ceil(map.SizeMeters.y / GridCell);
            grid = new NativeParallelMultiHashMap<int, int>(world.Config.MaxSlots, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            new BuildGridJob { Grid = grid, Position = w.Position, Flags = w.Flags, Count = n, GridW = gridW, GridL = gridL }.Run();
            new AcquireJob
            {
                Grid = grid, GridW = gridW, GridL = gridL, Tick = w.Tick,
                Position = w.Position, Velocity = w.Velocity, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype,
                StanceOf = w.StanceOf, Suppression = w.Suppression, TrenchId = w.TrenchId, TargetSlot = w.TargetSlot, Specs = w.Units.Infantry, Weapons = catalogue.Weapon,
                Trenches = fields.Trenches, CellTrenchId = map.CellTrenchId, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Height = map.Height,
            }.Schedule(n, 32).Complete();
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BuildGridJob : IJob
        {
            public NativeParallelMultiHashMap<int, int> Grid;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            public int Count, GridW, GridL;

            public void Execute()
            {
                Grid.Clear();
                for (int i = 0; i < Count; i++)
                {
                    if ((Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    int cx = math.clamp((int)(Position[i].x / GridCell), 0, GridW - 1);
                    int cz = math.clamp((int)(Position[i].z / GridCell), 0, GridL - 1);
                    Grid.Add(cz * GridW + cx, i);
                }
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct AcquireJob : IJobParallelFor
        {
            [ReadOnly] public NativeParallelMultiHashMap<int, int> Grid;
            public int GridW, GridL, NavWidth, NavLength;
            public uint Tick;
            [ReadOnly] public NativeArray<float3> Position, Velocity;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype, StanceOf;
            [ReadOnly] public NativeArray<InfantrySpec> Specs;   // the match table, by archetype (SimWorld.Units)
            [ReadOnly] public NativeArray<WeaponStats> Weapons;
            [ReadOnly] public NativeArray<float> Suppression;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<TrenchState> Trenches;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public Heightfield Height;
            [NativeDisableParallelForRestriction] public NativeArray<int> TargetSlot;   // each index writes only its own entry

            short TrenchAt(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return CellTrenchId[cz * NavWidth + cx];
            }

            /// <summary>The shield bearer standing between the shooter and his pick, if there is one: the nearest man of
            /// the pick's side with a plate, within the guard radius of the pick, nearer the shooter, inside a 15-degree
            /// cone on the bearing (ties to the lower slot). Otherwise the pick itself.</summary>
            int Shielded(int i, int pick, float3 p, short myTrench, float rangeSq)
            {
                float3 tp = Position[pick];
                float3 toT = tp - p; toT.y = 0f;
                float lenT = SimMath.Length(toT);
                if (lenT <= 1e-3f) return pick;
                float3 dirT = toT / lenT;
                int best = -1; float bestLen = float.MaxValue;
                int tcx = math.clamp((int)(tp.x / GridCell), 0, GridW - 1), tcz = math.clamp((int)(tp.z / GridCell), 0, GridL - 1);
                for (int dz = -1; dz <= 1; dz++)
                for (int dx = -1; dx <= 1; dx++)
                {
                    int cx = tcx + dx, cz = tcz + dz;
                    if (cx < 0 || cz < 0 || cx >= GridW || cz >= GridL) continue;
                    if (!Grid.TryGetFirstValue(cz * GridW + cx, out int j, out var it)) continue;
                    do
                    {
                        if (j == pick || Team[j] != Team[pick]) continue;
                        uint fj = Flags[j];
                        if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.Vehicle) != 0) continue;
                        var spec = Specs[Archetype[j]];
                        if (spec.ShieldPlateMm <= 0f) continue;
                        float3 g = Position[j] - tp; g.y = 0f;
                        if (math.lengthsq(g) > spec.ShieldGuardRadius * spec.ShieldGuardRadius) continue;
                        float3 toJ = Position[j] - p; toJ.y = 0f;
                        float lenJ = SimMath.Length(toJ);
                        if (lenJ >= lenT || lenJ <= 1e-3f) continue;
                        if (math.dot(toJ / lenJ, dirT) < 0.966f) continue;
                        if (lenJ < bestLen || (lenJ == bestLen && j < best)) { best = j; bestLen = lenJ; }
                    } while (Grid.TryGetNextValue(out j, ref it));
                }
                if (best >= 0 && Engageable(i, best, p, myTrench, rangeSq, out _)) return best;
                return pick;
            }

            bool Engageable(int i, int j, float3 p, short myTrench, float rangeSq, out float distSq)
            {
                distSq = 0f;
                uint fj = Flags[j];
                if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.KnockedOut) != 0 || Team[j] == Team[i]) return false;
                if ((fj & (uint)UnitFlags.Airborne) != 0) return false;   // a jetpack man in the air, or just down (LeapSystem)
                float3 d = Position[j] - p; d.y = 0f;
                distSq = math.lengthsq(d);
                if (distSq > rangeSq) return false;
                if ((fj & (uint)UnitFlags.Vehicle) != 0)   // small arms do nothing to armour (A5 adds penetration); infantry close-assault it instead
                    return (Flags[i] & (uint)UnitFlags.Vehicle) == 0 && distSq <= CombatTables.CloseAssaultRange * CombatTables.CloseAssaultRange;
                if ((fj & (uint)UnitFlags.InTrench) != 0 && StanceOf[j] != (byte)Stance.FireStep)
                {
                    short theirs = TrenchAt(Position[j]);
                    bool sameTrench = myTrench >= 0 && theirs == myTrench;
                    float reveal = (Flags[i] & (uint)UnitFlags.Charging) != 0 ? CombatTables.ChargeRevealRange : CombatTables.BelowRimRevealRange;   // a charging Breaker looks down into it
                    if (!sameTrench && distSq > reveal * reveal) return false;
                }
                return true;
            }

            float3 Muzzle(int k, float3 p, float3 toward)
            {
                if ((Flags[k] & (uint)UnitFlags.InTrench) != 0)
                {
                    // over the parapet: walk out of the carved footprint toward the other party, so the back row of a
                    // trench sees as well as the fire-step does
                    float3 dir = toward - p; dir.y = 0f;
                    float len = SimMath.Length(dir);
                    float3 q = p;
                    if (len > 1e-3f)
                    {
                        dir /= len;
                        float limit = math.min(8f, len * 0.5f);
                        for (float s = 1f; s <= limit; s += 1f)
                        {
                            q = p + dir * s;
                            if (TrenchAt(q) < 0) { q = p + dir * math.min(limit, s + 0.5f); break; }
                        }
                    }
                    return new float3(q.x, Height.Sample(q.x, q.z) + 0.3f, q.z);
                }
                return new float3(p.x, Height.Sample(p.x, p.z) + HeightfieldRaycast.EyeHeight((Stance)StanceOf[k]), p.z);
            }

            bool Sees(int i, int j, short myTrench)
            {
                float3 a = Position[i], b = Position[j];
                float3 d = b - a; d.y = 0f;
                if (math.lengthsq(d) < 36f) return true;
                if (myTrench >= 0 && TrenchAt(b) == myTrench) return true;
                return HeightfieldRaycast.HasLineOfSight(Height, Muzzle(i, a, b), Muzzle(j, b, a));
            }

            public void Execute(int i)
            {
                uint f = Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.KnockedOut) != 0) { TargetSlot[i] = -1; return; }
                short garrison = TrenchId[i];
                bool silent = Suppression[i] >= SuppressionRules.PinnedThreshold
                              || (garrison >= 0 && (Trenches[garrison].HoldFire != 0 || Suppression[i] >= CombatTables.GarrisonFireSuppressionLimit));
                if (silent) { TargetSlot[i] = -1; return; }

                float3 p = Position[i];
                var weapon = Weapons[Archetype[i]];
                float range = weapon.RangeMax;
                if ((f & (uint)UnitFlags.Exposed) != 0 && (f & (uint)UnitFlags.Vehicle) == 0) range = math.min(range, CombatTables.AdvanceFireRange);
                float rangeSq = range * range;
                short myTrench = (f & (uint)UnitFlags.InTrench) != 0 ? TrenchAt(p) : (short)-1;

                if ((uint)i % 3u != Tick % 3u)
                {
                    int cur = TargetSlot[i];
                    if (cur >= 0 && !Engageable(i, cur, p, myTrench, rangeSq, out _)) TargetSlot[i] = -1;
                    return;
                }

                // three nearest engageable enemies, ordered by (distance, slot)
                int b0 = -1, b1 = -1, b2 = -1;
                float d0 = float.MaxValue, d1 = float.MaxValue, d2 = float.MaxValue;
                int minX = math.clamp((int)((p.x - range) / GridCell), 0, GridW - 1), maxX = math.clamp((int)((p.x + range) / GridCell), 0, GridW - 1);
                int minZ = math.clamp((int)((p.z - range) / GridCell), 0, GridL - 1), maxZ = math.clamp((int)((p.z + range) / GridCell), 0, GridL - 1);
                for (int cz = minZ; cz <= maxZ; cz++)
                for (int cx = minX; cx <= maxX; cx++)
                {
                    if (!Grid.TryGetFirstValue(cz * GridW + cx, out int j, out var it)) continue;
                    do
                    {
                        if (j == i || !Engageable(i, j, p, myTrench, rangeSq, out float ds)) continue;
                        if (ds < d0 || (ds == d0 && j < b0)) { d2 = d1; b2 = b1; d1 = d0; b1 = b0; d0 = ds; b0 = j; }
                        else if (ds < d1 || (ds == d1 && j < b1)) { d2 = d1; b2 = b1; d1 = ds; b1 = j; }
                        else if (ds < d2 || (ds == d2 && j < b2)) { d2 = ds; b2 = j; }
                    } while (Grid.TryGetNextValue(out j, ref it));
                }
                int pick = -1;
                if (b0 >= 0 && Sees(i, b0, myTrench)) pick = b0;
                else if (b1 >= 0 && Sees(i, b1, myTrench)) pick = b1;
                else if (b2 >= 0 && Sees(i, b2, myTrench)) pick = b2;
                if (pick >= 0 && (Flags[pick] & (uint)UnitFlags.Vehicle) == 0) pick = Shielded(i, pick, p, myTrench, rangeSq);
                TargetSlot[i] = pick;
            }
        }

        public ulong Hash(ulong h) => h;   // TargetSlot lives in SimWorld; the grid is rebuilt every tick
        public void Dispose() { if (grid.IsCreated) grid.Dispose(); }
    }
}
