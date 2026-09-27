// Phase: A3 (implemented 2026-09-25) — the jetpack trooper. Owner's brief: "A jet pack troop that can jump from a
// distance."
// A man with InfantrySpec.JumpRange, off cooldown and not pinned, who has an enemy-held fire trench within reach,
// leaves the ground: LeapSystem picks the landing cell (the trench cell of his target if the target is in an
// enemy trench, else the nearest cell of the nearest enemy-held fire trench), hands MovementSystem a straight line
// (LeapTicks / LeapTarget) and the goal of that trench, and flags him Airborne. MoveJob flies him over wire and
// walls and lands him ON the cell, where the ordinary arrival rule garrisons him without a ladder. Airborne he is
// no target and takes no suppression (TargetAcquisition, DirectFire); landing he is granted LandingGraceTicks more
// of that so a single man is not simply dead on arrival, and his landing bursts a grenade under him
// (LandingBlast* through BlastSystem). State here: LeapCooldown, GraceTicks, InAir (was he leaping last tick),
// gen; all hashed, all reset when the slot is re-used.
// In Combat rather than Nav because the landing burst needs BlastSystem; the flight arrays live in MovementSystem
// (Nav) because MoveJob reads them.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class LeapSystem : ISimSystem
    {
        public const uint CheckEvery = 10;      // a jetpack man looks for a trench to jump into every half second
        public static readonly int LandingSource = SourceId.Unit(InfantryArchetype.Jetpack);   // Explosion.a for the landing burst
        public int Order => SimSystemOrder.Leap;

        readonly MapData map;
        FlowFieldManager fields;
        MovementSystem movement;
        BlastSystem blast;
        public NativeArray<int> LeapCooldown, GraceTicks;
        public NativeArray<byte> InAir;
        NativeArray<ushort> gen;
        public int Leaps;

        public LeapSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("LeapSystem needs FlowFieldManager");
            movement = world.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("LeapSystem needs MovementSystem registered before it");
            blast = world.GetSystem<BlastSystem>();
            int n = world.Config.MaxSlots;
            LeapCooldown = new NativeArray<int>(n, Allocator.Persistent);
            GraceTicks = new NativeArray<int>(n, Allocator.Persistent);
            InAir = new NativeArray<byte>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            for (int i = 0; i < n; i++)
            {
                if (gen[i] != w.Generation[i]) { gen[i] = w.Generation[i]; LeapCooldown[i] = 0; GraceTicks[i] = 0; InAir[i] = 0; movement.LeapTicks[i] = 0; }
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                var spec = w.Units.Infantry[w.Archetype[i]];
                if (spec.JumpRange <= 0f) continue;

                // landed last tick: the burst under him, and a moment's grace
                if (InAir[i] != 0 && movement.LeapTicks[i] == 0)
                {
                    InAir[i] = 0;
                    GraceTicks[i] = spec.LandingGraceTicks;
                    if (blast != null && spec.LandingBlastDamage > 0f)
                        blast.Queue(new Impact { Pos = w.Position[i], Damage = spec.LandingBlastDamage, Radius = spec.LandingBlastRadius, Suppression = 40f, Source = LandingSource, Player = w.Team[i] });
                }
                if (GraceTicks[i] > 0 && --GraceTicks[i] == 0) w.Flags[i] &= ~(uint)UnitFlags.Airborne;
                if (LeapCooldown[i] > 0) LeapCooldown[i]--;
                if (InAir[i] != 0 || GraceTicks[i] > 0 || LeapCooldown[i] > 0) continue;
                if (w.Suppression[i] >= SuppressionRules.PinnedThreshold) continue;
                if (w.Tick % CheckEvery != (uint)i % CheckEvery) continue;

                int cell = LandingCell(w, i, spec.JumpRange);
                if (cell < 0) continue;
                Jump(w, i, cell, spec);
            }
        }

        /// <summary>A trench body cell to land on within reach: his target's, if the target stands in an enemy trench,
        /// else the nearest cell of the nearest enemy-held fire trench. -1 when nothing is in reach.</summary>
        int LandingCell(SimWorld w, int i, float range)
        {
            float3 p = w.Position[i];
            byte team = w.Team[i];
            float r2 = range * range;
            int t = w.TargetSlot[i];
            if (t >= 0 && w.IsAlive(t) && (w.Flags[t] & (uint)UnitFlags.InTrench) != 0)
            {
                var c = map.NavCellOf(w.Position[t]);
                int cell = map.NavIndex(c.x, c.y);
                short trench = map.CellTrenchId[cell];
                if (trench >= 0 && trench != w.TrenchId[i] && fields.Trenches[trench].OwnerTeam != team && (map.NavLayers[cell] & (byte)NavLayer.Link) == 0
                    && math.distancesq(map.NavCellCenter(cell).xz, p.xz) <= r2) return cell;
            }
            int best = -1; float bestD = r2;
            for (int tr = 0; tr < map.Trenches.Length; tr++)
            {
                if (fields.Trenches[tr].OwnerTeam == team || tr == w.TrenchId[i]) continue;   // never the trench he already holds
                var def = map.Trenches[tr];
                if (def.Kind != 0) continue;
                for (int k = 0; k < def.CellCount; k++)
                {
                    int cell = map.TrenchCells[def.CellStart + k];
                    if ((map.NavLayers[cell] & (byte)NavLayer.Link) != 0) continue;
                    float d = math.distancesq(map.NavCellCenter(cell).xz, p.xz);
                    if (d < 16f) continue;   // a hop of under four metres is a step
                    if (d < bestD) { bestD = d; best = cell; }
                }
            }
            return best;
        }

        void Jump(SimWorld w, int i, int cell, in InfantrySpec spec)
        {
            short trench = map.CellTrenchId[cell];
            float3 from = w.Position[i];
            float3 land = map.NavCellCenter(cell) + TrenchPost.Offset(cell, map.NavWidth);
            land.y = 0f;
            float dist = math.distance(from.xz, land.xz);
            int ticks = math.max(1, (int)math.ceil(dist / (spec.JumpSpeed * w.Config.TickSeconds)));
            movement.LeapTarget[i] = land;
            movement.LeapTicks[i] = ticks;
            InAir[i] = 1;
            LeapCooldown[i] = spec.JumpCooldownTicks;
            Leaps++;
            if (w.TrenchId[i] >= 0) { w.SourceTrench[i] = w.TrenchId[i]; w.TrenchId[i] = -1; w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, i, w.SourceTrench[i], from); }
            w.PostCell[i] = -1; w.PostKind[i] = 0; w.Cooldown[i] = 0; w.Knock[i] = float3.zero;
            w.GoalId[i] = fields.GetGoal(GoalKey.Trench(trench));
            w.Flags[i] = (w.Flags[i] | (uint)UnitFlags.Airborne | (uint)UnitFlags.Exposed) & ~(uint)UnitFlags.InTrench;
            w.StanceOf[i] = (byte)Stance.Leap;
            w.TargetSlot[i] = -1;
            w.Events.Add(w.Tick, SimEventType.LeapStarted, i, trench, land, from, ticks * w.Config.TickSeconds);
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(LeapCooldown, h);
            h = SimHash.Array(GraceTicks, h);
            h = SimHash.Array(InAir, h);
            h = SimHash.Array(gen, h);
            return SimHash.Value(Leaps, h);
        }

        public void Dispose()
        {
            if (LeapCooldown.IsCreated) LeapCooldown.Dispose();
            if (GraceTicks.IsCreated) GraceTicks.Dispose();
            if (InAir.IsCreated) InAir.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
