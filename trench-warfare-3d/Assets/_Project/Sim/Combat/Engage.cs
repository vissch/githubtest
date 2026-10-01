// Phase: A3 (implemented 2026-09-28) — the owner, playing the game: "units should be attacking each other, go out
// of their way to attack each other."
// Until now a man's feet knew nothing of the enemy: TargetAcquisition gave him someone to shoot at and MoveJob walked
// him along his goal's field regardless, so two sections crossing no man's land shot at each other on the run and
// walked past. EngageSystem decides, for every man on foot in the open, what he does about the enemy:
//  - he is AFTER someone: the man he is shooting at if that man is in the open, otherwise the nearest enemy in the
//    open within HuntRadius (Hunt: found by TargetAcquisition's scan, every third tick, on the same walk through
//    its grid that finds his target; no line of sight needed, he knows where they are);
//  - farther off than his weapon likes (HoldDistance: half its range, less when he is under a >> order and only
//    engages within AdvanceFireRange; a braced gun sets up at three quarters), he CLOSES on him, in a straight line,
//    if the ground between them is open: no wire, no trench, nothing blocked (every cell the line crosses; once
//    closing, the next CloseKeep metres, 2026-10-01). Otherwise his goal's field knows the way;
//  - near enough and seeing him, he HOLDS: stands where he is, kneels, faces him and shoots (MoveJob), which takes
//    the moving penalty off his rounds. He holds until the man is dead or has gone HoldSlack past that distance,
//    and only while the ground between them is open: nobody stands to duel across wire or a trench.
// Men in a trench are left to the garrison rules (they shoot from the step), a man in an enemy trench is stormed
// along the goal's field rather than hunted, and nobody hunts a machine except a man whose weapon beats its plate.
// Pinned men, medics, engineers, unarmed men and a sapper on his errand (a cell goal) do not hunt.
// In Combat rather than Nav because it reads the weapon table; the orders live in MovementSystem (Nav) because
// MoveJob reads them. State: Hunt, HuntGen, the generation last seen and MovementSystem.Engage; all hashed.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class EngageSystem : ISimSystem
    {
        public const float HuntRadius = 70f;     // metres within which a man goes after an enemy in the open
        public const float HuntKeep = 1.25f;     // he stays after him until he is this many times as far
        public const float HoldShare = 0.5f;     // he stops to shoot at this share of his weapon's range (RangeFalloff is whole inside it)
        public const float BracedShare = 0.75f;  // a braced gun is set up sooner
        public const float ReachShare = 0.75f;   // and never beyond this share of the range he engages at just now
        public const float HoldSlack = 1.25f;    // a man who is holding keeps holding until the enemy is this much further
        public const float MinHold = 4f;         // this close he stops whatever his weapon is
        public const float ClearAhead = 48f;     // metres of the way to him that must be open ground
        public const float CloseKeep = 8f;       // a man already closing keeps on while this much of the way is open

        public int Order => SimSystemOrder.Engage;

        readonly MapData map;
        MovementSystem movement;
        FlowFieldManager fields;
        CombatCatalogueSystem catalogue;
        /// <summary>Who each man is after (a slot, -1 nobody), and that slot's generation when he chose him. Written by
        /// TargetAcquisition's scan, kept true and used here.</summary>
        public NativeArray<int> Hunt;
        public NativeArray<ushort> HuntGen;
        NativeArray<ushort> gen;

        public EngageSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            movement = world.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("EngageSystem needs MovementSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("EngageSystem needs FlowFieldManager registered before it");
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("EngageSystem needs CombatCatalogueSystem registered before it");
            int n = world.Config.MaxSlots;
            Hunt = new NativeArray<int>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) Hunt[i] = -1;
            HuntGen = new NativeArray<ushort>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            new EngageJob
            {
                Position = w.Position, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype, Generation = w.Generation,
                Suppression = w.Suppression, TrenchId = w.TrenchId, TargetSlot = w.TargetSlot, GoalId = w.GoalId,
                Specs = w.Units.Infantry, Weapons = catalogue.Weapon, Goals = fields.Goals, Layers = map.NavLayers,
                NavWidth = map.NavWidth, NavLength = map.NavLength,
                Hunt = Hunt, HuntGen = HuntGen, Gen = gen, Mode = movement.Engage, Dir = movement.EngageDir,
            }.Schedule(n, 64).Complete();
        }

        /// <summary>True for a man placed to go after the enemy: alive, on foot, on the ground, in the open.</summary>
        public static bool Hunter(uint flags, short garrison)
            => (flags & (uint)UnitFlags.Alive) != 0 && (flags & (uint)(UnitFlags.Vehicle | UnitFlags.Airborne | UnitFlags.InTrench)) == 0 && garrison < 0;

        /// <summary>True for a man who can be gone after: alive, on foot, on the ground, in the open.</summary>
        public static bool InTheOpen(uint flags, short garrison)
            => (flags & (uint)UnitFlags.Alive) != 0 && (flags & (uint)(UnitFlags.Vehicle | UnitFlags.KnockedOut | UnitFlags.Airborne | UnitFlags.InTrench)) == 0 && garrison < 0;

        /// <summary>True for a man who goes after the enemy: he has a weapon that hurts, and his trade is fighting.</summary>
        public static bool Fights(in InfantrySpec spec, in WeaponStats weapon)
            => weapon.Damage > 0f && weapon.RangeMax > 0f && spec.HealRadius <= 0f && spec.RepairRadius <= 0f;

        /// <summary>The distance at which a man with this weapon stops closing and shoots.</summary>
        public static float HoldDistance(in InfantrySpec spec, in WeaponStats weapon, bool exposed)
        {
            float reach = exposed ? math.min(weapon.RangeMax, CombatTables.AdvanceFireRange) : weapon.RangeMax;
            return math.max(MinHold, math.min(weapon.RangeMax * (spec.Braced ? BracedShare : HoldShare), reach * ReachShare));
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct EngageJob : IJobParallelFor
        {
            public int NavWidth, NavLength;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype;
            [ReadOnly] public NativeArray<ushort> Generation;
            [ReadOnly] public NativeArray<float> Suppression;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<int> TargetSlot, GoalId;
            [ReadOnly] public NativeArray<InfantrySpec> Specs;
            [ReadOnly] public NativeArray<WeaponStats> Weapons;
            [ReadOnly] public NativeArray<GoalKey> Goals;
            [ReadOnly] public NativeArray<byte> Layers;
            // each index reads and writes only its own entry of these
            public NativeArray<int> Hunt;
            public NativeArray<ushort> HuntGen, Gen;
            public NativeArray<byte> Mode;
            public NativeArray<float2> Dir;

            /// <summary>An enemy a man goes after: alive, on his feet in the open. A machine only for a man whose
            /// weapon is at its plate already (<paramref name="armour"/>: TargetAcquisition chose it for him).</summary>
            bool Huntable(int i, int j, bool armour)
            {
                uint fj = Flags[j];
                if (Team[j] == Team[i]) return false;
                if ((fj & (uint)UnitFlags.Vehicle) != 0)
                    return armour && (fj & (uint)UnitFlags.Alive) != 0 && (fj & (uint)(UnitFlags.KnockedOut | UnitFlags.Airborne)) == 0;
                return InTheOpen(fj, TrenchId[j]);
            }

            /// <summary>Open ground all the way along <paramref name="dir"/> for <paramref name="length"/> metres.</summary>
            /// <summary>Is the way `length` metres along `dir` open ground? Every nav cell the line crosses after his own
            /// (a grid walk, 2026-10-01). It sampled the line a metre apart from where he stood, so a step along it moved
            /// every sample: by a wire or trench cell's corner the answer changed with a tenth of a metre, and a man
            /// closed and stopped closing on alternate ticks, zig-zagging where he stood. Walked cell by cell, a step
            /// along the line crosses the same cells.</summary>
            bool Clear(float3 p, float3 dir, float length)
            {
                const byte closed = (byte)(NavLayer.Blocked | NavLayer.Wire | NavLayer.Trench | NavLayer.Link | NavLayer.Bunker);
                length = math.min(length, ClearAhead);
                if (length <= 0f) return true;
                float cs = MapData.NavCellSize;
                int x = math.clamp((int)(p.x / cs), 0, NavWidth - 1), z = math.clamp((int)(p.z / cs), 0, NavLength - 1);
                int sx = dir.x > 0f ? 1 : -1, sz = dir.z > 0f ? 1 : -1;
                float ax = math.abs(dir.x), az = math.abs(dir.z);
                float nextX = ax > 1e-6f ? ((sx > 0 ? x + 1 : x) * cs - p.x) / dir.x : float.MaxValue;
                float nextZ = az > 1e-6f ? ((sz > 0 ? z + 1 : z) * cs - p.z) / dir.z : float.MaxValue;
                float stepX = ax > 1e-6f ? cs / ax : float.MaxValue, stepZ = az > 1e-6f ? cs / az : float.MaxValue;
                for (int guard = 0; guard < 128; guard++)
                {
                    float t;
                    if (nextX < nextZ) { t = nextX; nextX += stepX; x += sx; } else { t = nextZ; nextZ += stepZ; z += sz; }
                    if (t > length || x < 0 || z < 0 || x >= NavWidth || z >= NavLength) return true;
                    byte layer = Layers[z * NavWidth + x];
                    if ((layer & closed) != 0 || (layer & (byte)NavLayer.Surface) == 0) return false;
                }
                return true;
            }

            public void Execute(int i)
            {
                if (Gen[i] != Generation[i]) { Gen[i] = Generation[i]; Hunt[i] = -1; Mode[i] = MovementSystem.EngageNone; }
                byte was = Mode[i];
                Mode[i] = MovementSystem.EngageNone;
                Dir[i] = float2.zero;
                uint f = Flags[i];
                if (!Hunter(f, TrenchId[i])) { Hunt[i] = -1; return; }
                var spec = Specs[Archetype[i]];
                var weapon = Weapons[Archetype[i]];
                if (!Fights(spec, weapon)) { Hunt[i] = -1; return; }
                float supp = Suppression[i];
                if (supp >= SuppressionRules.PinnedThreshold) return;
                int goal = GoalId[i];
                if (goal >= 0 && Goals[goal].Kind == GoalKind.Cell) { Hunt[i] = -1; return; }   // on an errand

                float3 p = Position[i];
                if (Hunt[i] >= 0)
                {
                    // the man the scan found him: still there, still in the open, not too far gone
                    int h = Hunt[i];
                    float3 e = Position[h] - p; e.y = 0f;
                    if (HuntGen[i] != Generation[h] || !Huntable(i, h, false) || math.lengthsq(e) > HuntRadius * HuntRadius * HuntKeep * HuntKeep) Hunt[i] = -1;
                }

                int quarry = Hunt[i];
                bool seen = false;
                int t = TargetSlot[i];
                if (t >= 0 && Huntable(i, t, spec.HuntsArmour)) { quarry = t; seen = true; }
                if (quarry < 0) return;

                float3 d = Position[quarry] - p; d.y = 0f;
                float dist = SimMath.Length(d);
                if (dist <= 1e-3f) { Mode[i] = MovementSystem.EngageHold; return; }
                float3 dir = d / dist;
                float hold = HoldDistance(spec, weapon, (f & (uint)UnitFlags.Exposed) != 0);
                if (was == MovementSystem.EngageHold) hold *= HoldSlack;
                bool prone = supp >= SuppressionRules.ProneThreshold;
                if (dist <= MinHold) { Mode[i] = MovementSystem.EngageHold; Dir[i] = new float2(dir.x, dir.z); return; }
                if (seen && (dist <= hold || prone) && Clear(p, dir, dist))
                {
                    Mode[i] = MovementSystem.EngageHold; Dir[i] = new float2(dir.x, dir.z);
                    return;
                }
                if (prone) return;   // he crawls on along his goal's field
                // wire, a trench or a wall between them: his goal's field knows the way round, and he shoots on the move
                // as he always did. He does not stand in the open to duel across an obstacle: the men beyond an enemy
                // trench are not worth being shot from its fire step for.
                // already closing, he keeps on while the next CloseKeep metres are open (2026-10-01): judged on the whole way
                // every tick, a step toward him clipped a wire or trench cell on the line, the field took him a step back,
                // the line was clear again, and he closed and did not on alternate ticks, zig-zagging where he stood
                float need = dist - hold * 0.8f;
                if (was == MovementSystem.EngageClose) need = math.min(need, CloseKeep);
                if (!Clear(p, dir, need)) return;
                Mode[i] = MovementSystem.EngageClose; Dir[i] = new float2(dir.x, dir.z);
            }
        }

        public ulong Hash(ulong h)
        {
            if (!Hunt.IsCreated) return h;
            h = SimHash.Array(Hunt, h);
            h = SimHash.Array(HuntGen, h);
            h = SimHash.Array(gen, h);
            return SimHash.Array(movement.Engage, h);
        }

        public void Dispose()
        {
            if (Hunt.IsCreated) Hunt.Dispose();
            if (HuntGen.IsCreated) HuntGen.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
