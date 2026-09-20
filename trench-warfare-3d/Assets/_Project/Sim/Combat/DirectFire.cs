// Phase: A2 (implemented) — depends on: TargetAcquisition, CombatTables, StanceRules, SimRandom.SystemId.DirectFire,
// MovementSystem.Spatial (near-miss neighbours; one tick stale, which is fine for a 1.5 m radius). Armour is A5.
// One sequential Burst job resolves every shot in slot order, so damage is applied in a fixed order and a target
// that dies mid-tick is not shot again. hit = accuracy × range falloff × shooter stance × moving × own suppression
// × (1 − target cover). A hit adds the weapon's suppression to the target; a miss adds 60 % of it to every enemy
// of the shooter within 1.5 m of the target (NearMiss). Deaths are applied on the main thread after the job.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class DirectFireSystem : ISimSystem
    {
        public int Order => SimSystemOrder.DirectFire;

        readonly MapData map;
        MovementSystem movement;
        NativeList<SimEvent> events;
        NativeList<int2> killed;     // (slot, killer) in the order they died

        /// <summary>Totals since the match started, per team: shots fired and kills scored. Derived from hashed state, not hashed itself.</summary>
        public readonly int[] Shots = new int[SimConfig.MaxPlayers];
        public readonly int[] Kills = new int[SimConfig.MaxPlayers];

        public DirectFireSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            events = new NativeList<SimEvent>(1024, Allocator.Persistent);
            killed = new NativeList<int2>(256, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            if (movement == null) movement = w.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("DirectFireSystem needs MovementSystem");
            events.Clear();
            killed.Clear();
            new FireJob
            {
                Count = n, Tick = w.Tick, Seed = w.Config.Seed, TickSeconds = w.Config.TickSeconds,
                Position = w.Position, Velocity = w.Velocity, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype, StanceOf = w.StanceOf,
                TargetSlot = w.TargetSlot, FireCooldown = w.FireCooldown, Hp = w.Hp, Suppression = w.Suppression,
                Spatial = movement.Spatial, CellTrenchId = map.CellTrenchId, Layers = map.NavLayers, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Events = events, Killed = killed,
            }.Run();

            for (int e = 0; e < events.Length; e++)
            {
                var ev = events[e];
                if (ev.Type == SimEventType.Shot) Shots[w.Team[ev.A] & 1]++;
                w.Events.Add(ev);
            }
            for (int k = 0; k < killed.Length; k++)
            {
                int slot = killed[k].x, killer = killed[k].y;
                Kills[w.Team[killer] & 1]++;
                float3 impulse = w.Position[slot] - w.Position[killer]; impulse.y = 0f;
                w.Despawn(slot, killer, SimMath.Length(impulse) > 1e-3f ? impulse / SimMath.Length(impulse) : default);
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct FireJob : IJob
        {
            public int Count, NavWidth, NavLength;
            public uint Tick, Seed;
            public float TickSeconds;
            [ReadOnly] public NativeArray<float3> Position, Velocity;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype, StanceOf;
            [ReadOnly] public SpatialHash Spatial;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public NativeArray<byte> Layers;
            public NativeArray<int> TargetSlot, FireCooldown;
            public NativeArray<float> Hp, Suppression;
            public NativeList<SimEvent> Events;
            public NativeList<int2> Killed;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            short TrenchAt(float3 p) => CellTrenchId[CellOf(p)];

            void AddSuppression(int slot, float amount)
            {
                float before = Suppression[slot], after = math.min(100f, before + amount);
                Suppression[slot] = after;
                if (before < SuppressionRules.PinnedThreshold && after >= SuppressionRules.PinnedThreshold)
                    Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Pinned, A = slot, Pos = Position[slot] });
                else if (before < SuppressionRules.ProneThreshold && after >= SuppressionRules.ProneThreshold)
                    Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Suppressed, A = slot, Pos = Position[slot], Scalar = after });
            }

            public void Execute()
            {
                for (int i = 0; i < Count; i++)
                {
                    if ((Flags[i] & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f) continue;
                    if (FireCooldown[i] > 0) { FireCooldown[i]--; continue; }
                    int t = TargetSlot[i];
                    if (t < 0) continue;
                    if ((Flags[t] & (uint)UnitFlags.Alive) == 0 || Hp[t] <= 0f) { TargetSlot[i] = -1; continue; }

                    float3 p = Position[i], q = Position[t];
                    if ((Flags[t] & (uint)UnitFlags.Vehicle) != 0)
                    {
                        // close assault: a bundle of grenades on the tracks or through a vision slit
                        FireCooldown[i] = CombatTables.CloseAssaultCooldownTicks;
                        var dice = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                        float3 toward = q - p; toward.y = 0f;
                        float reach = SimMath.Length(toward);
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = t, Pos = p, Dir = reach > 1e-3f ? toward / reach : new float3(0f, 0f, 1f), Scalar = 1f });
                        if (dice.NextFloat() < CombatTables.CloseAssaultChance)
                        {
                            Hp[t] = Hp[t] - CombatTables.CloseAssaultDamage;
                            Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Hit, A = i, B = t, Pos = q, Scalar = CombatTables.CloseAssaultDamage });
                            if (Hp[t] <= 0f) { Killed.Add(new int2(t, i)); TargetSlot[i] = -1; }
                        }
                        continue;
                    }
                    var weapon = CombatTables.WeaponFor(Archetype[i]);
                    FireCooldown[i] = CombatTables.CooldownTicks(weapon, TickSeconds);
                    float3 d = q - p; d.y = 0f;
                    float dist = SimMath.Length(d);
                    float3 dir = dist > 1e-3f ? d / dist : new float3(0f, 0f, 1f);

                    var myStance = (Stance)StanceOf[i];
                    var theirStance = (Stance)StanceOf[t];
                    float chance = weapon.Accuracy * CombatTables.RangeFalloff(dist, weapon.RangeMax)
                                 * StanceRules.AccuracyMultiplier(myStance, Archetype[i] == 2 || Archetype[i] == 4)
                                 * (1f - 0.5f * math.saturate(Suppression[i] * 0.01f));
                    if (SimMath.Length(Velocity[i]) > CombatTables.MovingSpeed && (Flags[i] & (uint)UnitFlags.Vehicle) == 0) chance *= CombatTables.MovingAccuracy;

                    float cover;
                    if ((Flags[t] & (uint)UnitFlags.InTrench) != 0)
                    {
                        bool sameTrench = (Flags[i] & (uint)UnitFlags.InTrench) != 0 && TrenchAt(p) == TrenchAt(q);
                        cover = sameTrench ? 0f : theirStance == Stance.FireStep ? CombatTables.TrenchCover : 0.4f;   // below the rim but reached from the parapet
                    }
                    else
                    {
                        cover = StanceRules.CoverBonusInOpen(theirStance);
                        if ((Layers[CellOf(q)] & (byte)NavLayer.Crater) != 0) cover = math.min(0.8f, cover + CombatTables.CraterCover);   // a shell hole is the only cover in no man's land
                    }
                    chance = math.clamp(chance * (1f - cover), 0.02f, 0.95f);

                    var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                    bool hit = rng.NextFloat() < chance;
                    Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = t, Pos = p, Dir = dir, Scalar = 0f });
                    if (hit)
                    {
                        float dmg = weapon.Damage * rng.NextFloat(0.8f, 1.2f);
                        Hp[t] = Hp[t] - dmg;
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Hit, A = i, B = t, Pos = q, Dir = dir, Scalar = dmg });
                        if (Hp[t] <= 0f) { Killed.Add(new int2(t, i)); TargetSlot[i] = -1; }
                        else AddSuppression(t, weapon.SuppressionPerShot);
                    }
                    else
                    {
                        // near miss: everyone on the target's side within 1.5 m of where the round went
                        float near = weapon.SuppressionPerShot * 0.6f;
                        int cx = math.clamp((int)(q.x / Spatial.CellSize), 0, Spatial.Width - 1);
                        int cz = math.clamp((int)(q.z / Spatial.CellSize), 0, Spatial.Length - 1);
                        bool targetSeen = false;
                        for (int dz = -2; dz <= 2; dz++)
                        for (int dx = -2; dx <= 2; dx++)
                        {
                            int x = cx + dx, z = cz + dz;
                            if (x < 0 || z < 0 || x >= Spatial.Width || z >= Spatial.Length) continue;
                            if (!Spatial.Map.TryGetFirstValue(Spatial.KeyXZ(x, z), out int j, out var it)) continue;
                            do
                            {
                                if ((Flags[j] & (uint)UnitFlags.Alive) == 0 || Hp[j] <= 0f || Team[j] != Team[t]) continue;
                                float3 e = Position[j] - q; e.y = 0f;
                                if (math.lengthsq(e) > SuppressionRules.NearMissRadius * SuppressionRules.NearMissRadius) continue;
                                if (j == t) targetSeen = true;
                                AddSuppression(j, near);
                            } while (Spatial.Map.TryGetNextValue(out j, ref it));
                        }
                        if (!targetSeen) AddSuppression(t, near);   // the hash is a tick old: the target may have moved cells
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.NearMiss, A = t, Pos = q, Scalar = near });
                    }
                }
            }
        }

        public ulong Hash(ulong h) => h;   // Hp, Suppression, TargetSlot and FireCooldown live in SimWorld
        public void Dispose()
        {
            if (events.IsCreated) events.Dispose();
            if (killed.IsCreated) killed.Dispose();
        }
    }
}
