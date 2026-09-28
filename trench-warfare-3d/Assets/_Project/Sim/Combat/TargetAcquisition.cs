// Phase: A2 (implemented) — depends on: MapData (heightfield, CellTrenchId), FlowFieldManager (trench hold-fire),
// HeightfieldRaycast, CombatTables, GasSmokeSystem (the smoke field, read a tick old). Sniper priorities
// (SpecialAbilities) come later.
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
//  - past CombatTables.SmokeBlindMetres of thick smoke on the line (SmokeLos) nobody is seen, whatever the ground says;
//    the scan drops such a target within three ticks.
//  - a shield bearer (InfantrySpec.ShieldPlateMm) standing between a shooter and the man he picked, within
//    ShieldGuardRadius of that man and inside a 15-degree cone on the bearing, takes the shot instead (2026-09-25).
//    DirectFire then rolls the round against his plate.
//  - the same scan finds, for a man on foot in the open, the nearest enemy in the open within EngageSystem.HuntRadius,
//    seen or not: the man he goes after (EngageSystem.Hunt, 2026-09-28). One walk through the grid serves both.
//  - dead ground (2026-09-29): a man on foot in the open more than CombatTables.DeadGroundMetres behind his own side's
//    front trench (measured along the field, at his own column: FrontZ) cannot be seen by a shooter on the far side of
//    that trench. He is on the approaches, which the parapet, the traverses and the communication trenches hide. Before
//    it a machine gun in a front trench reached 170 m, past the whole of no man's land, and shot the other side's
//    reinforcements dead between their spawn and their lines (MatchLoopTests): no army ever grew.
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
        GasSmokeSystem gas;          // registered after this system: resolved on the first step
        bool lookedForGas;
        NativeArray<float> noSmoke;  // a one-cell stand-in for the job while there is no smoke
        NativeParallelMultiHashMap<int, int> grid;
        int gridW, gridL;
        EngageSystem engage;         // registered after this system: resolved on the first step
        bool lookedForEngage;
        NativeArray<int> noHunt;     // one-cell stand-ins for the job in a match without an EngageSystem
        NativeArray<ushort> noHuntGen;
        // ---- dead ground (2026-09-29) ----
        NativeArray<float> trenchColumnZ;   // trench * NavWidth + nav column: the mean z of its cells there (its own mean where it has none)
        NativeArray<float> frontZ;          // team * NavWidth + column: that team's front trench there, NaN with no front trench
        readonly short[] frontOf = { -2, -2 };
        float2 homeSign;                     // per team: +1 when its home end of the field is at the high z, -1 at the low

        public TargetAcquisitionSystem(MapData map) { this.map = map; }

        /// <summary>This tick's grid of the living (GridCell squares, slots in slot order), for other systems' near
        /// searches after this one steps (MeleeSystem). Read only.</summary>
        public NativeParallelMultiHashMap<int, int> Grid => grid;
        public int GridW => gridW;
        public int GridL => gridL;

        CombatCatalogueSystem catalogue;

        public void Initialize(SimWorld world)
        {
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("TargetAcquisitionSystem needs CombatCatalogueSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("TargetAcquisitionSystem needs FlowFieldManager registered before it");
            gridW = (int)math.ceil(map.SizeMeters.x / GridCell);
            gridL = (int)math.ceil(map.SizeMeters.y / GridCell);
            grid = new NativeParallelMultiHashMap<int, int>(world.Config.MaxSlots, Allocator.Persistent);
            noSmoke = new NativeArray<float>(1, Allocator.Persistent);
            noHunt = new NativeArray<int>(1, Allocator.Persistent);
            noHuntGen = new NativeArray<ushort>(1, Allocator.Persistent);
            int wdt = map.NavWidth, nt = map.Trenches.Length;
            trenchColumnZ = new NativeArray<float>(math.max(1, nt * wdt), Allocator.Persistent);
            frontZ = new NativeArray<float>(2 * wdt, Allocator.Persistent);
            for (int k = 0; k < frontZ.Length; k++) frontZ[k] = float.NaN;
            var sum = new float[wdt]; var cnt = new int[wdt];
            for (int t = 0; t < nt; t++)
            {
                System.Array.Clear(sum, 0, wdt); System.Array.Clear(cnt, 0, wdt);
                var def = map.Trenches[t];
                float all = 0f;
                for (int c = 0; c < def.CellCount; c++)
                {
                    int cell = map.TrenchCells[def.CellStart + c];
                    float z = map.NavCellCenter(cell).z;
                    sum[cell % wdt] += z; cnt[cell % wdt]++; all += z;
                }
                float mean = def.CellCount > 0 ? all / def.CellCount : float.NaN;
                for (int x = 0; x < wdt; x++) trenchColumnZ[t * wdt + x] = cnt[x] > 0 ? sum[x] / cnt[x] : mean;
            }
            float home0 = world.Init.SpawnA.z <= world.Init.SpawnB.z ? -1f : 1f;
            homeSign = new float2(home0, -home0);
        }

        /// <summary>Where each team's front trench runs, column by column, for the dead-ground rule. Rewritten only
        /// when a front moves (a capture).</summary>
        void UpdateFronts()
        {
            int wdt = map.NavWidth;
            for (int team = 0; team < 2; team++)
            {
                short f = fields.FrontTrench((byte)team);
                if (f == frontOf[team]) continue;
                frontOf[team] = f;
                for (int x = 0; x < wdt; x++) frontZ[team * wdt + x] = f >= 0 && f < map.Trenches.Length ? trenchColumnZ[f * wdt + x] : float.NaN;
            }
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            if (!lookedForGas) { gas = w.GetSystem<GasSmokeSystem>(); lookedForGas = true; }
            if (!lookedForEngage) { engage = w.GetSystem<EngageSystem>(); lookedForEngage = true; }
            bool smokeOn = gas != null && gas.SmokeActive;
            UpdateFronts();
            new BuildGridJob { Grid = grid, Position = w.Position, Flags = w.Flags, Count = n, GridW = gridW, GridL = gridL }.Run();
            new AcquireJob
            {
                Grid = grid, GridW = gridW, GridL = gridL, Tick = w.Tick,
                Smoke = smokeOn ? gas.Smoke : noSmoke, SmokeW = smokeOn ? gas.Width : 1, SmokeL = smokeOn ? gas.Length : 1, SmokeOn = smokeOn,
                Position = w.Position, Velocity = w.Velocity, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype,
                StanceOf = w.StanceOf, Suppression = w.Suppression, TrenchId = w.TrenchId, TargetSlot = w.TargetSlot, Specs = w.Units.Infantry, Weapons = catalogue.Weapon, Tanks = catalogue.Tank, Yaw = w.Yaw,
                Trenches = fields.Trenches, CellTrenchId = map.CellTrenchId, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Height = map.Height,
                Hunts = engage != null, Hunt = engage != null ? engage.Hunt : noHunt, HuntGen = engage != null ? engage.HuntGen : noHuntGen,
                Generation = w.Generation,
                FrontZ = frontZ, HomeSign = homeSign,
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
            [ReadOnly] public NativeArray<float> FrontZ;   // team * NavWidth + column (dead ground)
            public float2 HomeSign;

            /// <summary>True when <paramref name="j"/>, on foot in the open, stands more than DeadGroundMetres behind
            /// his own front trench and a shooter at <paramref name="p"/> is on the far side of it: the approaches hide him.</summary>
            bool InDeadGround(int j, float3 p)
            {
                int team = Team[j] & 1;
                float3 q = Position[j];
                int col = math.clamp((int)(q.x / MapData.NavCellSize), 0, NavWidth - 1);
                float fz = FrontZ[team * NavWidth + col];
                if (math.isnan(fz)) return false;
                float s = team == 0 ? HomeSign.x : HomeSign.y;
                return (q.z - fz) * s > CombatTables.DeadGroundMetres && (p.z - fz) * s < 0f;
            }

            [ReadOnly] public NativeParallelMultiHashMap<int, int> Grid;
            public int GridW, GridL, NavWidth, NavLength;
            public uint Tick;
            [ReadOnly] public NativeArray<float3> Position, Velocity;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype, StanceOf;
            [ReadOnly] public NativeArray<InfantrySpec> Specs;   // the match table, by archetype (SimWorld.Units)
            [ReadOnly] public NativeArray<WeaponStats> Weapons;
            [ReadOnly] public NativeArray<TankSpec> Tanks;
            [ReadOnly] public NativeArray<float> Yaw;
            [ReadOnly] public NativeArray<float> Suppression;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<TrenchState> Trenches;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public Heightfield Height;
            [ReadOnly] public NativeArray<float> Smoke;
            public int SmokeW, SmokeL;
            public bool SmokeOn;
            [NativeDisableParallelForRestriction] public NativeArray<int> TargetSlot;   // each index writes only its own entry
            // the man each man on foot in the open goes after (EngageSystem): each index writes only its own entry
            public bool Hunts;
            [NativeDisableParallelForRestriction] public NativeArray<int> Hunt;
            [NativeDisableParallelForRestriction] public NativeArray<ushort> HuntGen;
            [ReadOnly] public NativeArray<ushort> Generation;

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
                if ((fj & (uint)UnitFlags.Vehicle) != 0)   // small arms do nothing to armour; infantry close-assault it instead
                {
                    // a shooter whose small arms hunt armour (InfantrySpec.HuntsArmour: the Skimmer, and since 2026-09-28 a
                    // man with an anti-tank rifle) takes a machine whose plate facing it they beat: a light machine's side
                    // or rear, never a heavy one's front
                    if (Specs[Archetype[i]].HuntsArmour)
                    {
                        var facing = Armor.FacingOf(Position[j] - p, Yaw[j], out _, out _);
                        if (Weapons[Archetype[i]].PenetrationMm > Armor.PlateFor(Tanks[Archetype[j]].Hull, facing)) return true;
                    }
                    // otherwise a machine's guns never look at one, and a man close-assaults it
                    if ((Flags[i] & (uint)UnitFlags.Vehicle) != 0) return false;
                    return distSq <= CombatTables.CloseAssaultRange * CombatTables.CloseAssaultRange;
                }
                if ((fj & (uint)UnitFlags.InTrench) == 0 && TrenchId[j] < 0 && InDeadGround(j, p)) return false;
                if ((fj & (uint)UnitFlags.InTrench) != 0 && StanceOf[j] != (byte)Stance.FireStep)
                {
                    short theirs = TrenchAt(Position[j]);
                    bool sameTrench = myTrench >= 0 && theirs == myTrench;
                    float reveal = (Flags[i] & (uint)UnitFlags.Charging) != 0 ? CombatTables.ChargeRevealRange : CombatTables.BelowRimRevealRange;   // a charging Breaker looks down into it
                    reveal = math.max(reveal, Specs[Archetype[i]].LooksDownMetres);   // so does a raider driven up to the trench (the Skimmer)
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
                // a screen between them: past SmokeBlindMetres of thick smoke nobody is seen, whatever the ground says
                if (SmokeOn && SmokeLos.MetresThrough(Smoke, SmokeW, SmokeL, a, b) >= CombatTables.SmokeBlindMetres) return false;
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
                // and, for a man who goes after the enemy, the nearest of them in the open, seen or not
                bool hunter = Hunts && EngageSystem.Hunter(f, garrison) && EngageSystem.Fights(Specs[Archetype[i]], weapon);
                int hunt = -1; float huntSq = EngageSystem.HuntRadius * EngageSystem.HuntRadius;
                float look = hunter ? math.max(range, EngageSystem.HuntRadius) : range;
                int minX = math.clamp((int)((p.x - look) / GridCell), 0, GridW - 1), maxX = math.clamp((int)((p.x + look) / GridCell), 0, GridW - 1);
                int minZ = math.clamp((int)((p.z - look) / GridCell), 0, GridL - 1), maxZ = math.clamp((int)((p.z + look) / GridCell), 0, GridL - 1);
                for (int cz = minZ; cz <= maxZ; cz++)
                for (int cx = minX; cx <= maxX; cx++)
                {
                    if (!Grid.TryGetFirstValue(cz * GridW + cx, out int j, out var it)) continue;
                    do
                    {
                        if (j == i) continue;
                        if (hunter && Team[j] != Team[i] && EngageSystem.InTheOpen(Flags[j], TrenchId[j]))
                        {
                            float3 e = Position[j] - p; e.y = 0f;
                            float es = math.lengthsq(e);
                            if (es < huntSq || (es == huntSq && hunt >= 0 && j < hunt)) { huntSq = es; hunt = j; }
                        }
                        if (!Engageable(i, j, p, myTrench, rangeSq, out float ds)) continue;
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
                if (hunter) { Hunt[i] = hunt; HuntGen[i] = hunt >= 0 ? Generation[hunt] : (ushort)0; }
            }
        }

        public ulong Hash(ulong h) => h;   // TargetSlot lives in SimWorld; the grid is rebuilt every tick
        public void Dispose()
        {
            if (grid.IsCreated) grid.Dispose();
            if (noSmoke.IsCreated) noSmoke.Dispose();
            if (noHunt.IsCreated) noHunt.Dispose();
            if (noHuntGen.IsCreated) noHuntGen.Dispose();
            if (trenchColumnZ.IsCreated) trenchColumnZ.Dispose();
            if (frontZ.IsCreated) frontZ.Dispose();
        }
    }
}
