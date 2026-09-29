// Phase: A2 (implemented) — depends on: TargetAcquisition, CombatTables, StanceRules, SimRandom.SystemId.DirectFire,
// MovementSystem.Spatial (near-miss neighbours; one tick stale, which is fine for a 1.5 m radius). Armour is A5.
// One sequential Burst job resolves every shot in slot order, so damage is applied in a fixed order and a target
// that dies mid-tick is not shot again. hit = accuracy × range falloff × shooter stance × moving × own suppression
// × running target (a man in the open running past RunningTargetSpeed, CombatTables.RunningTarget, 2026-09-28)
// × (1 − target cover). A hit adds the weapon's suppression to the target; a miss adds 60 % of it to every enemy
// of the shooter within 1.5 m of the target (NearMiss). Deaths are applied on the main thread after the job.
// A vehicle is only ever close-assaulted (TargetAcquisition): the bundle of grenades is a VehicleHit on the armour,
// queued on TankGunnerySystem.PendingHits for VehicleModulesSystem (without them, it takes the damage straight off).
// A knocked-out hulk is not worth a grenade.
// Smoke (docs/21 phase 5): every metre of thick smoke on the line of fire (SmokeLos) takes SmokeAccuracyPerMetre
// off the chance, down to SmokeAccuracyFloor, and a target standing inside thick smoke gains only SmokeSuppression
// of the suppression a hit or a near miss would give him: he cannot tell how close it was.
// A shield bearer (InfantrySpec.ShieldPlateMm, 2026-09-25) shot from inside his plate's arc gets a penetration roll
// first: PenetrationMm x 0.8..1.2 under the plate and the round is STOPPED (Hit with a negative scalar, ShieldBlocked,
// a quarter of the suppression). The roll is drawn only for him, so every other man's stream is what it was.
// The bomb (2026-09-28): a man on foot in the open, not pinned, with a bomb left, whose target is a man in a trench
// 5-22 m off throws one instead of firing (Throws): it lands within GrenadeScatter of him and goes off as an Impact on
// BlastSystem (720, after this system), so the bay, the traverse and the parapet still count. He holds it while a
// friend stands within GrenadeFriend of the mark. Bombs are per slot and refill when a slot's Generation moves on (a
// new man); both arrays are in the hash.
// Its flight (2026-09-29): the bomb is in the air CombatTables.GrenadeFlightTicks (0.5 s at 5 m, 1.2 s at 22 m) and
// goes off the tick it lands, where it was aimed, whether or not the thrower still lives. It went off the tick it was
// thrown, so nothing could be drawn between the throw and the burst. The bombs in the air are hashed.
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
        NativeList<VehicleHit> ownHits;   // close assaults when no TankGunnerySystem is registered
        TankGunnerySystem gunnery;
        GasSmokeSystem gas;               // registered after this system: resolved on the first step
        bool lookedForGas;
        NativeArray<float> noSmoke;       // a one-cell stand-in for the job while there is no smoke
        AuraSystem aura;                  // the officer's multipliers; without one, every man's are 1
        NativeArray<float> ones;
        // ---- the bomb (2026-09-28) ----
        BlastSystem blast; bool lookedForBlast;   // registered after this system: resolved on the first step
        NativeArray<byte> grenades;              // bombs each slot's man has left
        NativeArray<ushort> grenadeGen;          // the Generation they were counted for: another means a new man
        NativeList<Impact> noImpacts;            // the job's list in a match without a BlastSystem (it throws nothing then)
        NativeList<Impact> thrown;               // this tick's throws, from the job
        NativeList<Impact> flying;               // bombs in the air, in throw order
        NativeList<uint> lands;                  // the tick each of them lands

        /// <summary>Totals since the match started, per team: shots fired and kills scored. Derived from hashed state, not hashed itself.</summary>
        public readonly int[] Shots = new int[SimConfig.MaxPlayers];
        public readonly int[] Kills = new int[SimConfig.MaxPlayers];

        public DirectFireSystem(MapData map) { this.map = map; }

        CombatCatalogueSystem catalogue;
        // ---- fire (2026-09-28, the Flamethrower): what a weapon that SetsBurning lit this tick ----
        BurningSystem burning; bool lookedForBurning;   // registered after this system: resolved on the first step
        NativeList<int2> ignited;      // (target, shooter) of every hit by such a weapon, in shot order
        NativeList<float4> scorched;   // where every round of one landed, hit or miss (xyz), and the shooter's side (w)

        public void Initialize(SimWorld world)
        {
            ignited = new NativeList<int2>(64, Allocator.Persistent);
            scorched = new NativeList<float4>(64, Allocator.Persistent);
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("DirectFireSystem needs CombatCatalogueSystem registered before it");
            events = new NativeList<SimEvent>(1024, Allocator.Persistent);
            killed = new NativeList<int2>(256, Allocator.Persistent);
            ownHits = new NativeList<VehicleHit>(16, Allocator.Persistent);
            noSmoke = new NativeArray<float>(1, Allocator.Persistent);
            ones = new NativeArray<float>(world.Config.MaxSlots, Allocator.Persistent);
            for (int i = 0; i < ones.Length; i++) ones[i] = 1f;
            grenades = new NativeArray<byte>(world.Config.MaxSlots, Allocator.Persistent);
            grenadeGen = new NativeArray<ushort>(world.Config.MaxSlots, Allocator.Persistent);
            noImpacts = new NativeList<Impact>(1, Allocator.Persistent);
            thrown = new NativeList<Impact>(16, Allocator.Persistent);
            flying = new NativeList<Impact>(32, Allocator.Persistent);
            lands = new NativeList<uint>(32, Allocator.Persistent);
        }

        /// <summary>Bombs in the air now.</summary>
        public int GrenadesFlying => flying.IsCreated ? flying.Length : 0;

        /// <summary>Bombs the man in <paramref name="slot"/> has left (a man not yet counted has his archetype's full load).</summary>
        public int GrenadesLeft(SimWorld w, int slot)
            => grenadeGen[slot] == w.Generation[slot] ? grenades[slot] : CombatTables.GrenadesFor(w.Archetype[slot]);

        /// <summary>This tick's kills, (slot, killer) in the order they died; valid until the next Step (HeroSystem reads it).</summary>
        public NativeList<int2> Killed => killed;

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            if (movement == null) movement = w.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("DirectFireSystem needs MovementSystem");
            if (gunnery == null) gunnery = w.GetSystem<TankGunnerySystem>();
            if (!lookedForGas) { gas = w.GetSystem<GasSmokeSystem>(); lookedForGas = true; }
            bool smokeOn = gas != null && gas.SmokeActive;
            if (aura == null) aura = w.GetSystem<AuraSystem>();
            if (!lookedForBlast) { blast = w.GetSystem<BlastSystem>(); lookedForBlast = true; }
            events.Clear();
            killed.Clear();
            ownHits.Clear();
            ignited.Clear();
            scorched.Clear();
            thrown.Clear();
            // the bombs that land this tick go off now (BlastSystem steps after this system), in the order they were thrown
            if (flying.Length > 0 && blast != null)
            {
                int keep = 0;
                for (int k = 0; k < flying.Length; k++)
                {
                    if (lands[k] <= w.Tick) { blast.Queue(flying[k]); continue; }
                    flying[keep] = flying[k]; lands[keep] = lands[k]; keep++;
                }
                flying.ResizeUninitialized(keep); lands.ResizeUninitialized(keep);
            }
            new FireJob
            {
                Ignited = ignited, Scorched = scorched,
                Count = n, Tick = w.Tick, Seed = w.Config.Seed, TickSeconds = w.Config.TickSeconds,
                Position = w.Position, Velocity = w.Velocity, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype, StanceOf = w.StanceOf, Yaw = w.Yaw,
                Specs = w.Units.Infantry, Roster = w.Units.Roster, Weapons = catalogue.Weapon, Tanks = catalogue.Tank,
                TargetSlot = w.TargetSlot, FireCooldown = w.FireCooldown, Hp = w.Hp, Suppression = w.Suppression,
                Spatial = movement.Spatial, CellTrenchId = map.CellTrenchId, Layers = map.NavLayers, CellCover = map.CellCover, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Events = events, Killed = killed, VehicleHits = gunnery != null ? gunnery.PendingHits : ownHits,
                Smoke = smokeOn ? gas.Smoke : noSmoke, SmokeW = smokeOn ? gas.Width : 1, SmokeL = smokeOn ? gas.Length : 1, SmokeOn = smokeOn,
                DamageMul = aura != null ? aura.DamageMul : ones, SuppressionMul = aura != null ? aura.SuppressionMul : ones,
                Generation = w.Generation, Grenades = grenades, GrenadeGen = grenadeGen,
                Impacts = blast != null ? thrown : noImpacts, CanThrow = blast != null,
            }.Run();
            for (int k = 0; k < thrown.Length; k++)
            {
                float3 d = thrown[k].Dir;   // the job keeps the throw's length in Dir.y until here
                flying.Add(new Impact
                {
                    Pos = thrown[k].Pos, Dir = new float3(d.x, 0f, d.z), Damage = thrown[k].Damage, Radius = thrown[k].Radius,
                    Suppression = thrown[k].Suppression, CraterRadius = thrown[k].CraterRadius, CraterDepth = thrown[k].CraterDepth,
                    Source = thrown[k].Source, Player = thrown[k].Player, Shape = thrown[k].Shape,
                });
                lands.Add(w.Tick + (uint)CombatTables.GrenadeFlightTicks(d.y, w.Config.TickSeconds));
            }
            for (int k = 0; k < ownHits.Length; k++)
            {
                var hit = ownHits[k];   // no armour model registered: the charge's damage comes straight off
                w.Hp[hit.Target] = w.Hp[hit.Target] - hit.Damage;
                w.Events.Add(w.Tick, SimEventType.Hit, hit.Shooter, hit.Target, hit.Pos, hit.Dir, hit.Damage);
                if (w.Hp[hit.Target] <= 0f && w.IsAlive(hit.Target)) killed.Add(new int2(hit.Target, hit.Shooter));
            }

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

            // what the fire lit: the man it hit (if the hit left him alive) and the ground its rounds landed on. After
            // the deaths, so a man the flame killed is not set alight as the next tenant of his slot. BurningSystem steps
            // at 725, after this system, so he burns from this tick on.
            if (ignited.Length > 0 || scorched.Length > 0)
            {
                if (!lookedForBurning) { burning = w.GetSystem<BurningSystem>(); lookedForBurning = true; }
                if (burning != null)
                {
                    for (int k = 0; k < ignited.Length; k++) burning.Ignite(w, ignited[k].x, BurningSystem.BurstManSeconds);
                    for (int k = 0; k < scorched.Length; k++) burning.IgniteCell(w, scorched[k].xyz, BurningSystem.BurstCellSeconds, (int)scorched[k].w);
                }
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
            [ReadOnly] public NativeArray<InfantrySpec> Specs;   // the match table, by archetype (SimWorld.Units)
            [ReadOnly] public NativeArray<RosterEntry> Roster;   // the same table: cost, hit points, and the chassis
            [ReadOnly] public NativeArray<WeaponStats> Weapons;
            [ReadOnly] public NativeArray<TankSpec> Tanks;
            [ReadOnly] public NativeArray<float> Yaw;
            [ReadOnly] public SpatialHash Spatial;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public NativeArray<byte> CellCover;
            [ReadOnly] public NativeArray<byte> Layers;
            public NativeArray<int> TargetSlot, FireCooldown;
            public NativeArray<float> Hp, Suppression;
            public NativeList<SimEvent> Events;
            public NativeList<int2> Killed;
            public NativeList<VehicleHit> VehicleHits;
            public NativeList<int2> Ignited;      // WeaponStats.SetsBurning: (target, shooter) of each hit
            public NativeList<float4> Scorched;   // and where each round landed (xyz), the shooter's side in w
            [ReadOnly] public NativeArray<float> Smoke;
            public int SmokeW, SmokeL;
            public bool SmokeOn;

            bool InSmoke(float3 p)
            {
                if (!SmokeOn) return false;
                int cx = math.clamp((int)(p.x / MapData.FieldCellSize), 0, SmokeW - 1);
                int cz = math.clamp((int)(p.z / MapData.FieldCellSize), 0, SmokeL - 1);
                return Smoke[cz * SmokeW + cx] > SmokeLos.Thick;
            }
            [ReadOnly] public NativeArray<float> DamageMul, SuppressionMul;   // the officer's aura (AuraSystem), 1 without
            [ReadOnly] public NativeArray<ushort> Generation;
            public NativeArray<byte> Grenades;
            public NativeArray<ushort> GrenadeGen;
            public NativeList<Impact> Impacts;   // this tick's throws; Step puts them in the air
            public bool CanThrow;

            /// <summary>A man in the open throws a bomb at the trench man <paramref name="t"/> instead of firing, when
            /// he has one, is not pinned, the man is 5-22 m off and no friend stands by the mark.</summary>
            bool Throws(int i, int t, float3 p, float3 q)
            {
                if (!CanThrow) return false;
                if ((Flags[i] & (uint)(UnitFlags.Vehicle | UnitFlags.InTrench | UnitFlags.Airborne)) != 0) return false;
                if ((Flags[t] & (uint)UnitFlags.InTrench) == 0 || StanceOf[i] == (byte)Stance.Pinned) return false;
                if (GrenadeGen[i] != Generation[i]) { GrenadeGen[i] = Generation[i]; Grenades[i] = CombatTables.GrenadesFor(Archetype[i]); }
                if (Grenades[i] == 0) return false;
                float3 d = q - p; d.y = 0f;
                float dist = SimMath.Length(d);
                if (dist < CombatTables.GrenadeMin || dist > CombatTables.GrenadeRange) return false;
                if (FriendNear(i, q)) return false;
                var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                float spread = CombatTables.GrenadeScatter + CombatTables.GrenadeScatterPerMetre * dist;
                float3 at = q + new float3(rng.NextFloat(-spread, spread), 0f, rng.NextFloat(-spread, spread));
                float3 way = d / dist;
                Grenades[i] = (byte)(Grenades[i] - 1);
                FireCooldown[i] = (int)math.round(CombatTables.GrenadeCooldownSeconds / TickSeconds);
                Impacts.Add(new Impact
                {
                    Pos = at, Dir = new float3(way.x, dist, way.z), Damage = CombatTables.GrenadeDamage, Radius = CombatTables.GrenadeRadius,
                    Suppression = CombatTables.GrenadeSuppression, CraterRadius = 0.8f, CraterDepth = 0.15f,
                    Source = SourceId.Grenade, Player = Team[i], Shape = (int)BlastShape.Shell,
                });
                float3 flight = at - p; flight.y = 0f;
                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.GrenadeThrown, A = i, B = t, Pos = p, Dir = flight, Scalar = dist });
                return true;
            }

            /// <summary>True when a man of <paramref name="i"/>'s side stands within GrenadeFriend of <paramref name="q"/>.</summary>
            bool FriendNear(int i, float3 q)
            {
                int r = (int)math.ceil(CombatTables.GrenadeFriend / Spatial.CellSize);
                int cx = math.clamp((int)(q.x / Spatial.CellSize), 0, Spatial.Width - 1);
                int cz = math.clamp((int)(q.z / Spatial.CellSize), 0, Spatial.Length - 1);
                for (int dz = -r; dz <= r; dz++)
                for (int dx = -r; dx <= r; dx++)
                {
                    int x = cx + dx, z = cz + dz;
                    if (x < 0 || z < 0 || x >= Spatial.Width || z >= Spatial.Length) continue;
                    if (!Spatial.Map.TryGetFirstValue(Spatial.KeyXZ(x, z), out int j, out var it)) continue;
                    do
                    {
                        if (j == i || Team[j] != Team[i] || (Flags[j] & (uint)UnitFlags.Alive) == 0) continue;
                        float3 e = Position[j] - q; e.y = 0f;
                        if (math.lengthsq(e) <= CombatTables.GrenadeFriend * CombatTables.GrenadeFriend) return true;
                    } while (Spatial.Map.TryGetNextValue(out j, ref it));
                }
                return false;
            }

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            short TrenchAt(float3 p) => CellTrenchId[CellOf(p)];

            /// <summary>A round that hit a shield bearer inside his plate's arc: does the plate stop it? Draws the
            /// penetration roll and, when it stops, the damage the plate took, logs it and suppresses him a little.</summary>
            bool Stopped(int i, int t, in WeaponStats weapon, float3 p, float3 q, float3 dir, ref Random rng)
            {
                var spec = Specs[Archetype[t]];
                if (spec.ShieldPlateMm <= 0f) return false;
                if (weapon.SetsBurning) return false;   // fire goes round a plate: it stops rounds, not a jet of flame
                float3 back = p - q; back.y = 0f;
                float bearing = SimMath.Atan2(back.x, back.z);                       // sim yaw: 0 is +Z, positive to the right
                if (math.abs(SimMath.WrapAngle(bearing - Yaw[t])) > spec.ShieldArcHalf) return false;
                float pen = weapon.PenetrationMm * rng.NextFloat(0.8f, 1.2f);
                if (pen >= spec.ShieldPlateMm) return false;
                float took = weapon.Damage * DamageMul[i] * rng.NextFloat(0.8f, 1.2f);
                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Hit, A = i, B = t, Pos = q, Dir = dir, Scalar = -took });
                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.ShieldBlocked, A = t, B = i, Pos = q, Dir = dir, Scalar = took });
                AddSuppression(t, weapon.SuppressionPerShot * 0.25f * SuppressionMul[t]);
                return true;
            }

            void AddSuppression(int slot, float amount)
            {
                if ((Flags[slot] & (uint)UnitFlags.Airborne) != 0) return;   // nothing reaches a man in the air (LeapSystem)
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
                    // his weapon on the ground, or charging or fighting hand to hand (MeleeSystem): no shot, except the
                    // grenade bundle for a machine beside him (a charge made escorted tanks immune to it, critic r2)
                    if ((Flags[i] & (uint)UnitFlags.Disarmed) != 0) continue;
                    int t = TargetSlot[i];
                    if ((Flags[i] & (uint)UnitFlags.Melee) != 0 && (t < 0 || (Flags[t] & (uint)UnitFlags.Vehicle) == 0)) continue;
                    if (t < 0) continue;
                    if ((Flags[t] & (uint)UnitFlags.Alive) == 0 || Hp[t] <= 0f) { TargetSlot[i] = -1; continue; }

                    float3 p = Position[i], q = Position[t];
                    if ((Flags[t] & (uint)UnitFlags.Vehicle) != 0)
                    {
                        if ((Flags[t] & (uint)UnitFlags.KnockedOut) != 0) { TargetSlot[i] = -1; continue; }
                        // a man with an anti-tank rifle (2026-09-28) fires it at a plate it beats, from its own range; at a
                        // plate it does not beat he is a man beside a machine, with a bundle of grenades (below)
                        bool pierces = (Flags[i] & (uint)UnitFlags.Vehicle) == 0 && Specs[Archetype[i]].HuntsArmour
                            && Weapons[Archetype[i]].PenetrationMm > Armor.PlateFor(Tanks[Archetype[t]].Hull, Armor.FacingOf(q - p, Yaw[t], out _, out _));
                        if ((Flags[i] & (uint)UnitFlags.Vehicle) != 0 || pierces)
                        {
                            // armour-hunting small arms (InfantrySpec.HuntsArmour, TargetAcquisition): a burst of
                            // armour-piercing at the hull, resolved against the plate it strikes by VehicleModulesSystem
                            var mg = Weapons[Archetype[i]];
                            FireCooldown[i] = CombatTables.CooldownTicks(mg, TickSeconds);
                            float3 at = q - p; at.y = 0f;
                            float range = SimMath.Length(at);
                            float3 way = range > 1e-3f ? at / range : new float3(0f, 0f, 1f);
                            Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = t, Pos = p, Dir = way, Scalar = 0f });
                            float odds = mg.Accuracy * CombatTables.RangeFalloff(range, mg.RangeMax) * CombatTables.HullTargetBonus;
                            if (SimMath.Length(Velocity[i]) > CombatTables.MovingSpeed) odds *= CombatTables.MovingAccuracy;
                            if (SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i).NextFloat() < odds)
                                VehicleHits.Add(new VehicleHit { Target = t, Shooter = i, Kind = VehicleHitKind.ArmourPiercing, PenMm = mg.PenetrationMm, Damage = mg.Damage, Pos = q, Dir = way });
                            continue;
                        }
                        // close assault: a bundle of grenades on the engine deck, the tracks or through a vision slit
                        FireCooldown[i] = CombatTables.CloseAssaultCooldownTicks;
                        var dice = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                        float3 toward = q - p; toward.y = 0f;
                        float reach = SimMath.Length(toward);
                        float3 along = reach > 1e-3f ? toward / reach : new float3(0f, 0f, 1f);
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = t, Pos = p, Dir = along, Scalar = 1f });
                        if (dice.NextFloat() < CombatTables.CloseAssaultChance)
                            VehicleHits.Add(new VehicleHit { Target = t, Shooter = i, Kind = VehicleHitKind.CloseAssault, PenMm = CombatTables.CloseAssaultPenMm, Damage = CombatTables.CloseAssaultDamage, Pos = q, Dir = along });
                        continue;
                    }
                    if (Throws(i, t, p, q)) continue;
                    var weapon = Weapons[Archetype[i]];
                    FireCooldown[i] = CombatTables.CooldownTicks(weapon, TickSeconds);
                    float3 d = q - p; d.y = 0f;
                    float dist = SimMath.Length(d);
                    float3 dir = dist > 1e-3f ? d / dist : new float3(0f, 0f, 1f);

                    var myStance = (Stance)StanceOf[i];
                    var theirStance = (Stance)StanceOf[t];
                    float chance = weapon.Accuracy * CombatTables.RangeFalloff(dist, weapon.RangeMax)
                                 * StanceRules.AccuracyMultiplier(myStance, Specs[Archetype[i]].Braced || ChassisKind.IsTank(Roster[Archetype[i]].Chassis))
                                 * (1f - 0.5f * math.saturate(Suppression[i] * 0.01f));
                    if (SimMath.Length(Velocity[i]) > CombatTables.MovingSpeed && (Flags[i] & (uint)UnitFlags.Vehicle) == 0) chance *= CombatTables.MovingAccuracy;

                    float cover;
                    if ((Flags[t] & (uint)UnitFlags.InTrench) != 0)
                    {
                        bool sameTrench = (Flags[i] & (uint)UnitFlags.InTrench) != 0 && TrenchAt(p) == TrenchAt(q);
                        bool charging = (Flags[i] & (uint)UnitFlags.Charging) != 0;   // a Breaker in the trench: nothing between its guns and them
                        cover = sameTrench || charging ? 0f : theirStance == Stance.FireStep ? CombatTables.TrenchCover : 0.4f;   // below the rim but reached from the parapet
                    }
                    else
                    {
                        cover = StanceRules.CoverBonusInOpen(theirStance);
                        chance *= CombatTables.RunningTarget(dist, SimMath.Length(Velocity[t]));   // he has to be led
                        if ((Layers[CellOf(q)] & (byte)NavLayer.Crater) != 0) cover = math.min(0.8f, cover + CombatTables.CraterCover);
                        cover = math.min(0.8f, cover + CellCover[CellOf(q)] * 0.01f);   // a tree, a stump, a wreck next to him
                    }
                    if (SmokeOn)
                    {
                        float through = SmokeLos.MetresThrough(Smoke, SmokeW, SmokeL, p, q);
                        if (through > 0f) chance *= math.max(CombatTables.SmokeAccuracyFloor, 1f - CombatTables.SmokeAccuracyPerMetre * through);
                    }
                    chance = math.clamp(chance * (1f - cover), 0.02f, 0.95f);
                    float keepDown = InSmoke(q) ? CombatTables.SmokeSuppression : 1f;

                    var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.DirectFire, (uint)i);
                    bool hit = rng.NextFloat() < chance;
                    Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Shot, A = i, B = t, Pos = p, Dir = dir, Scalar = 0f });
                    if (hit && Stopped(i, t, weapon, p, q, dir, ref rng)) { }
                    else if (hit)
                    {
                        float dmg = weapon.Damage * DamageMul[i] * rng.NextFloat(0.8f, 1.2f);
                        if ((Flags[i] & (uint)UnitFlags.Charging) != 0)
                        {
                            // a charging Breaker's round finds the mark (the roll is drawn only for it)
                            var ts = Tanks[Archetype[i]];
                            if (ts.CritChance > 0f && rng.NextFloat() < ts.CritChance)
                            {
                                dmg *= ts.CritMul;
                                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.CriticalHit, A = i, B = t, Pos = q, Dir = dir, Scalar = dmg });
                            }
                        }
                        Hp[t] = Hp[t] - dmg;
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.Hit, A = i, B = t, Pos = q, Dir = dir, Scalar = dmg });
                        if (weapon.SetsBurning) { Ignited.Add(new int2(t, i)); Scorched.Add(new float4(q, Team[i])); }
                        if (Hp[t] <= 0f) { Killed.Add(new int2(t, i)); TargetSlot[i] = -1; }
                        else AddSuppression(t, weapon.SuppressionPerShot * keepDown * SuppressionMul[t]);
                    }
                    else
                    {
                        if (weapon.SetsBurning) Scorched.Add(new float4(q, Team[i]));   // the jet missed him and lit the ground
                        // near miss: everyone on the target's side within 1.5 m of where the round went
                        float near = weapon.SuppressionPerShot * 0.6f * keepDown;
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
                                AddSuppression(j, near * SuppressionMul[j]);
                            } while (Spatial.Map.TryGetNextValue(out j, ref it));
                        }
                        if (!targetSeen) AddSuppression(t, near * SuppressionMul[t]);   // the hash is a tick old: the target may have moved cells
                        // a round that misses a man on the fire step strikes the parapet at his face: the whole of it
                        if (theirStance == Stance.FireStep && (Flags[t] & (uint)UnitFlags.InTrench) != 0)
                            AddSuppression(t, weapon.SuppressionPerShot * (1f - 0.6f) * keepDown * SuppressionMul[t]);
                        Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.NearMiss, A = t, Pos = q, Scalar = near });
                    }
                }
            }
        }

        // Hp, Suppression, TargetSlot and FireCooldown live in SimWorld; the bombs are this system's own
        public ulong Hash(ulong h)
        {
            if (!grenades.IsCreated) return h;
            h = SimHash.Array(grenades, h);
            h = SimHash.Array(grenadeGen, h);
            h = SimHash.Value(flying.Length, h);
            h = SimHash.Array(flying.AsArray(), h);
            return SimHash.Array(lands.AsArray(), h);
        }
        public void Dispose()
        {
            if (events.IsCreated) events.Dispose();
            if (killed.IsCreated) killed.Dispose();
            if (ownHits.IsCreated) ownHits.Dispose();
            if (ignited.IsCreated) ignited.Dispose();
            if (scorched.IsCreated) scorched.Dispose();
            if (noSmoke.IsCreated) noSmoke.Dispose();
            if (ones.IsCreated) ones.Dispose();
            if (grenades.IsCreated) grenades.Dispose();
            if (grenadeGen.IsCreated) grenadeGen.Dispose();
            if (noImpacts.IsCreated) noImpacts.Dispose();
            if (thrown.IsCreated) thrown.Dispose();
            if (flying.IsCreated) flying.Dispose();
            if (lands.IsCreated) lands.Dispose();
        }
    }
}
