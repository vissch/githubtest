// Phase: A3 (implemented 2026-09-28, lane/sim/melee) — the owner: "Anytime a unit becomes into a melee state it should
// have different behavior and animations even vfx ... Foot soldiers might throw their rifle away and hit with their
// hands or keep their rifle and hit with it." Decisions (decisions.md 2026-09-28): men close in from 8 m and fight at
// 2.5 m; rifle or fists goes by unit type.
// Until now two men a few metres apart held and shot each other (EngageSystem's MinHold 4 m). MeleeSystem decides, for
// every man on foot, whether he is in the fight at arm's length:
//  - his FOE is the nearest enemy man within ChargeRange (found on TargetAcquisition's grid, no line of sight needed at
//    this distance), kept until he is dead, gone past BreakRange of a fight or ChargeRange*Keep of a charge;
//  - a man whose trade is fighting (EngageSystem.Fights), not pinned or down, not on an errand, CHARGES him
//    (MovementSystem.EngageClose with UnitFlags.Melee: a sprint in a straight line), over open ground or down into
//    the trench the foe is in, or along the trench both are in (a garrison man leaves his post for it): the ground on
//    the way must be his foe's trench or open, never wire, a wall, a bunker or another trench (critic r1: charges
//    dropped into trenches and died there, and nobody inside a stormed trench closed in);
//  - within ContactRange both are IN MELEE (MovementSystem.EngageMelee: Stance.Melee, facing him, stepping in to
//    StandOff): a blow every BlowTicks (jittered, so two men do not strike in step), which misses, is blocked or lands.
//    A man in a trench or a garrison fights where he stands; so does a pinned man, and a medic: anyone struck fights back.
//  - a man who fights with his fists (KeepsRifle false: assault troops, officers, sappers, medics and the other
//    pistol, machine-pistol and flame men) throws his weapon down as the fight starts (WeaponDropped, UnitFlags.Disarmed)
//    and picks it up PickUpTicks after it ends (WeaponPickedUp). A man with the Melee or Disarmed flag does not shoot
//    (DirectFire), charging included: he runs in instead of holding to shoot.
// Blows: MeleeBlow (a = attacker, b = defender, pos = the attacker, dir.xz = towards the defender, dir.y = the style,
// scalar = damage, 0 blocked, -1 missed) and, when it lands, the ordinary Hit (so the hit reactions and the impact
// flash need nothing new); a killing blow despawns the defender with the attacker as his killer.
// A blow's damage takes the officer's aura, the Hero's and a veteran's share (AuraSystem.DamageMul, as a round's does);
// a kill counts for the side (DirectFireSystem.Kills) and is listed in Killed for HeroSystem's feats. A man struck by
// someone other than his foe turns on him when his foe is busy with another (StruckBy): nobody stands being stabbed in
// the back (critic r2). A man with no weapon (a medic) has nothing to throw down.
// Found in parallel (each man reads the others and writes only his own entries), resolved on the main thread in slot
// order (a blow kills; the next man must see it). State: Foe, FoeGen, Contact, Swing, Dropped, Calm, StruckBy, gen; all
// hashed.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class MeleeSystem : ISimSystem
    {
        public const float ChargeRange = 8f;     // metres: an enemy man this close is charged (the owner, 2026-09-28)
        public const float ContactRange = 2.5f;  // and this close they fight
        public const float BreakRange = 3.5f;    // a fight holds until they are this far apart (pushed, knocked back)
        public const float Keep = 1.25f;         // a charge holds until the foe is this many times ChargeRange off
        public const float StandOff = 1.2f;      // in a fight he steps in to this distance and no nearer
        public const int BlowTicks = 24;         // a blow every 1.2 s at 20 Hz ...
        public const int BlowJitter = 8;         // ... give or take up to this many ticks
        public const int PickUpTicks = 30;       // a thrown-down weapon is picked up this long after the fight ends
        public const float HitChance = 0.7f;     // a blow that is not blocked lands this often
        public const float BlockRifle = 0.35f;   // a man with a rifle in his hands turns a landing blow aside this often
        public const float BlockFists = 0.2f;    // a man with his fists, less often
        public const float StabDamage = 60f, ButtDamage = 45f, SmashDamage = 55f, FistDamage = 25f;
        public const float Knock = 1.5f;         // m/s: the killing blow's shove
        public const byte StyleStab = 0, StyleButt = 1, StyleSmash = 2, StyleFists = 3;

        public int Order => SimSystemOrder.Melee;

        readonly MapData map;
        MovementSystem movement;
        TargetAcquisitionSystem acquisition;
        CombatCatalogueSystem catalogue;
        FlowFieldManager fields;
        /// <summary>Who each man is fighting or charging (a slot, -1 nobody) and that slot's generation.</summary>
        public NativeArray<int> Foe;
        public NativeArray<ushort> FoeGen;
        /// <summary>1 while he is within arm's length of his foe (the fight itself), 0 charging or free.</summary>
        public NativeArray<byte> Contact;
        /// <summary>Ticks to his next blow.</summary>
        public NativeArray<short> Swing;
        /// <summary>1 while his weapon lies on the ground (a fists man in a fight), and ticks since the fight ended.</summary>
        public NativeArray<byte> Dropped;
        public NativeArray<short> Calm;
        /// <summary>Who struck him on the last tick (a slot, -1 nobody): FindJob turns him on that man.</summary>
        public NativeArray<int> StruckBy;
        /// <summary>This step's kills (x the dead, y the killer), for HeroSystem's feats (read the tick after).</summary>
        public NativeList<int2> Killed;
        NativeArray<ushort> gen;
        NativeArray<int> found;
        AuraSystem aura;
        DirectFireSystem fire;
        NativeArray<float3> towards;   // xz: the unit direction to the foe, y: the distance

        public MeleeSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            movement = world.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("MeleeSystem needs MovementSystem registered before it");
            acquisition = world.GetSystem<TargetAcquisitionSystem>() ?? throw new System.InvalidOperationException("MeleeSystem needs TargetAcquisitionSystem registered before it");
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("MeleeSystem needs CombatCatalogueSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("MeleeSystem needs FlowFieldManager registered before it");
            int n = world.Config.MaxSlots;
            Foe = new NativeArray<int>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) Foe[i] = -1;
            FoeGen = new NativeArray<ushort>(n, Allocator.Persistent);
            Contact = new NativeArray<byte>(n, Allocator.Persistent);
            Swing = new NativeArray<short>(n, Allocator.Persistent);
            Dropped = new NativeArray<byte>(n, Allocator.Persistent);
            Calm = new NativeArray<short>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
            StruckBy = new NativeArray<int>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) StruckBy[i] = -1;
            Killed = new NativeList<int2>(16, Allocator.Persistent);
            found = new NativeArray<int>(n, Allocator.Persistent);
            towards = new NativeArray<float3>(n, Allocator.Persistent);
        }

        /// <summary>True for a man who keeps his rifle in a fight and stabs, butts or smashes with it; false for one who
        /// throws his weapon down and fights with his fists (the owner, 2026-09-28: "by unit type"). Riflemen, snipers
        /// and the other men with a rifle or a gun to swing; assault troops, officers, sappers, medics and the pistol,
        /// machine-pistol and flame men use their fists.</summary>
        public static bool KeepsRifle(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Assault: case InfantryArchetype.Officer: case InfantryArchetype.Sapper: case InfantryArchetype.Medic:
                case InfantryArchetype.Shield: case InfantryArchetype.Jetpack: case InfantryArchetype.Flamethrower:
                    return false;
                default:
                    return true;   // Rifle, Machinegunner, Sniper, Repair, Para, Frog, Sentry, AtRifle, DeathBattalion
            }
        }

        /// <summary>A man who can be in a fight on foot: alive, not a machine, not in the air.</summary>
        public static bool OnFoot(uint flags)
            => (flags & (uint)UnitFlags.Alive) != 0 && (flags & (uint)(UnitFlags.Vehicle | UnitFlags.Airborne)) == 0;

        /// <summary>The damage of a blow of this style.</summary>
        public static float DamageOf(byte style)
            => style == StyleStab ? StabDamage : style == StyleButt ? ButtDamage : style == StyleSmash ? SmashDamage : FistDamage;

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            Killed.Clear();
            if (n == 0) return;
            aura ??= w.GetSystem<AuraSystem>();
            fire ??= w.GetSystem<DirectFireSystem>();
            // a slot taken by a new man starts out of any fight, armed
            const uint mine = (uint)(UnitFlags.Melee | UnitFlags.Disarmed);
            for (int i = 0; i < n; i++)
            {
                if (gen[i] == w.Generation[i]) continue;
                gen[i] = w.Generation[i]; Foe[i] = -1; FoeGen[i] = 0; Contact[i] = 0; Swing[i] = 0; Dropped[i] = 0; Calm[i] = 0; StruckBy[i] = -1;
                if ((w.Flags[i] & mine) != 0) w.Flags[i] &= ~mine;
            }
            new FindJob
            {
                Position = w.Position, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype, Generation = w.Generation,
                Suppression = w.Suppression, TrenchId = w.TrenchId, GoalId = w.GoalId,
                Specs = w.Units.Infantry, Weapons = catalogue.Weapon, Goals = fields.Goals, Layers = map.NavLayers,
                NavWidth = map.NavWidth, NavLength = map.NavLength,
                Grid = acquisition.Grid, GridW = acquisition.GridW, GridL = acquisition.GridL, CellTrench = map.CellTrenchId,
                Foe = Foe, FoeGen = FoeGen, Contact = Contact, StruckBy = StruckBy, Found = found, Towards = towards,
            }.Schedule(n, 64).Complete();
            Resolve(w, n);
        }

        /// <summary>The main-thread half, in slot order: take up the foes found, set each man's feet, drop and pick up
        /// weapons, strike.</summary>
        void Resolve(SimWorld w, int n)
        {
            // FindJob has read last tick's strikes; this tick's blows write their own (a one-tick memory)
            for (int i = 0; i < n; i++) StruckBy[i] = -1;
            const uint melee = (uint)UnitFlags.Melee, disarmed = (uint)UnitFlags.Disarmed;
            for (int i = 0; i < n; i++)
            {
                uint f = w.Flags[i];
                if (!OnFoot(f))
                {
                    if (Foe[i] >= 0 || Contact[i] != 0) { Foe[i] = -1; Contact[i] = 0; }
                    if (Dropped[i] != 0 && (f & (uint)UnitFlags.Alive) != 0) { Dropped[i] = 0; Calm[i] = 0; w.Events.Add(w.Tick, SimEventType.WeaponPickedUp, i, 0, w.Position[i]); }
                    if ((f & (melee | disarmed)) != 0) w.Flags[i] = f & ~(melee | disarmed);
                    continue;
                }
                int was = Foe[i];
                int foe = found[i];
                float3 to = towards[i];
                if (foe >= 0 && !w.IsAlive(foe)) foe = -1;   // killed by a blow earlier in this loop
                bool touching = foe >= 0 && to.y <= (Contact[i] != 0 && was == foe ? BreakRange : ContactRange);
                Foe[i] = foe; FoeGen[i] = foe >= 0 ? w.Generation[foe] : (ushort)0;
                byte before = Contact[i];
                Contact[i] = (byte)(touching ? 1 : 0);
                // the first blow comes a moment after they close; one pushed apart and back keeps his rhythm (a new
                // wait each time made blows quicker than BlowTicks, critic r1)
                if (touching && before == 0 && Swing[i] <= 0) Swing[i] = (short)(BlowTicks / 3 + (int)(SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Melee, (uint)i).NextUInt() % (uint)BlowJitter));
                else if (!touching && Swing[i] > 0) Swing[i]--;

                // his feet: charging (FindJob chose it) or fighting; the flags DirectFire reads
                float2 dir = new float2(to.x, to.z);
                if (touching)
                {
                    movement.Engage[i] = MovementSystem.EngageMelee;
                    movement.EngageDir[i] = MovementSystem.MeleeDir(dir, (to.y - StandOff) / (ContactRange - StandOff));
                }
                else if (foe >= 0 && Charges(w, i)) { movement.Engage[i] = MovementSystem.EngageClose; movement.EngageDir[i] = dir; }
                else if (foe >= 0) foe = -1;   // found, but he is not one to run at him: he fights only when it comes to blows
                if (foe < 0) { Foe[i] = -1; FoeGen[i] = 0; }
                f = foe >= 0 ? f | melee : f & ~melee;

                // a fists man throws his weapon down as it comes to blows, and picks it up once it is over (a man with
                // none, a medic, has nothing to throw)
                if (touching && Dropped[i] == 0 && !KeepsRifle(w.Archetype[i]) && catalogue.Weapon[w.Archetype[i]].Damage > 0f)
                {
                    Dropped[i] = 1; Calm[i] = 0; f |= disarmed;
                    float3 aside = new float3(-dir.y, 0f, dir.x) * ((i & 1) == 0 ? 1f : -1f);
                    w.Events.Add(w.Tick, SimEventType.WeaponDropped, i, 0, w.Position[i], aside);
                }
                else if (Dropped[i] != 0 && foe < 0)
                {
                    if (++Calm[i] >= PickUpTicks)
                    {
                        Dropped[i] = 0; Calm[i] = 0; f &= ~disarmed;
                        w.Events.Add(w.Tick, SimEventType.WeaponPickedUp, i, 0, w.Position[i]);
                    }
                }
                else Calm[i] = 0;
                w.Flags[i] = f;

                if (touching && --Swing[i] <= 0) Blow(w, i, foe, dir);
            }
        }

        /// <summary>True for a man who runs at an enemy within ChargeRange: his trade is fighting, he is not down or
        /// pinned, not on an errand, and his weapon is not a flame's short cone (FindJob has checked the ground between
        /// them, the trenches included).</summary>
        bool Charges(SimWorld w, int i)
        {
            if (w.Suppression[i] >= SuppressionRules.ProneThreshold) return false;   // down or pinned: he crawls on his way (EngageSystem)
            var spec = w.Units.Infantry[w.Archetype[i]];
            var weapon = catalogue.Weapon[w.Archetype[i]];
            if (!EngageSystem.Fights(spec, weapon)) return false;
            // a flamethrower (9.6 m) would put his weapon down before he could use it: he flames until it comes to blows
            if (weapon.Mode == FireMode.Cone) return false;
            int goal = w.GoalId[i];
            if (goal < 0) return true;
            var g = fields.Goals[goal];
            if (g.Kind == GoalKind.Cell || g.Kind == GoalKind.Rally) return false;   // on an errand, or called back to his rally point
            // going back to a trench his side holds (a fallback, a return, reinforcements coming up): he goes, and fights
            // only a man who comes to blows with him (critic r3: nothing got a man out of a fight)
            if (g.Kind == GoalKind.Trench && g.Ref >= 0 && g.Ref < fields.Trenches.Length && fields.Trenches[g.Ref].OwnerTeam == w.Team[i]
                && w.TrenchId[i] != g.Ref) return false;
            return true;
        }

        /// <summary>One blow of man i at his foe j: missed, blocked or landed; the next blow's time.</summary>
        void Blow(SimWorld w, int i, int j, float2 dir)
        {
            var dice = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Melee, (uint)i + 0x10000u);
            Swing[i] = (short)(BlowTicks - BlowJitter / 2 + (int)(dice.NextUInt() % (uint)(BlowJitter + 1)));
            if (!w.IsAlive(j)) return;
            byte style;
            if (Dropped[i] != 0 || !KeepsRifle(w.Archetype[i])) style = StyleFists;
            else { float r = dice.NextFloat(); style = r < 0.45f ? StyleStab : r < 0.75f ? StyleButt : StyleSmash; }
            float3 at = new float3(dir.x, style, dir.y);
            float damage;
            if (dice.NextFloat() >= HitChance) damage = -1f;   // missed
            else
            {
                // he turns it aside if he is in the fight too, facing it, with his rifle or his fists up
                bool guard = Contact[j] != 0 && Foe[j] == i;
                // a rifle or a shield's plate turns a blow better than bare hands
                bool plate = w.Archetype[j] == InfantryArchetype.Shield;
                float block = !plate && (Dropped[j] != 0 || !KeepsRifle(w.Archetype[j])) ? BlockFists : BlockRifle;
                damage = guard && dice.NextFloat() < block ? 0f : DamageOf(style) * (aura != null ? aura.DamageMul[i] : 1f);
            }
            StruckBy[j] = i;
            w.Events.Add(w.Tick, SimEventType.MeleeBlow, i, j, w.Position[i], at, damage);
            if (damage <= 0f) return;
            w.Hp[j] = w.Hp[j] - damage;
            w.Events.Add(w.Tick, SimEventType.Hit, i, j, w.Position[j], new float3(dir.x, 0f, dir.y), damage);
            if (w.Hp[j] <= 0f)
            {
                Killed.Add(new int2(j, i));
                if (fire != null) fire.Kills[w.Team[i] & 1]++;
                w.Despawn(j, i, new float3(dir.x, 0f, dir.y), Knock);
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct FindJob : IJobParallelFor
        {
            public int NavWidth, NavLength, GridW, GridL;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype;
            [ReadOnly] public NativeArray<ushort> Generation;
            [ReadOnly] public NativeArray<float> Suppression;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<int> GoalId;
            [ReadOnly] public NativeArray<InfantrySpec> Specs;
            [ReadOnly] public NativeArray<WeaponStats> Weapons;
            [ReadOnly] public NativeArray<GoalKey> Goals;
            [ReadOnly] public NativeArray<byte> Layers;
            [ReadOnly] public NativeArray<short> CellTrench;
            [ReadOnly] public NativeParallelMultiHashMap<int, int> Grid;
            [ReadOnly] public NativeArray<int> Foe;
            [ReadOnly] public NativeArray<ushort> FoeGen;
            [ReadOnly] public NativeArray<byte> Contact;
            [ReadOnly] public NativeArray<int> StruckBy;
            // each index writes only its own entry of these
            public NativeArray<int> Found;
            public NativeArray<float3> Towards;

            bool Enemy(int i, int j) => j != i && Team[j] != Team[i] && OnFoot(Flags[j]);

            int Cell(float3 c)
            {
                int cx = math.clamp((int)(c.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(c.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            /// <summary>Ground a man can run across to his foe: no wire, wall or bunker, and no trench but the foe's own
            /// (he jumps down into it, or runs along it when both are in it). Another trench on the way would swallow
            /// the charge (critic r1).</summary>
            bool Open(float3 p, float3 q)
            {
                const byte closed = (byte)(NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker);
                short foeTrench = CellTrench[Cell(q)], mine = CellTrench[Cell(p)];
                if (mine >= 0 && mine != foeTrench) return false;   // out of his own trench at another's man: over the top is Movement's business
                float3 d = q - p; d.y = 0f;
                float len = SimMath.Length(d);
                for (float s = 0.5f; s < len; s += 0.5f)
                {
                    int c = Cell(p + d * (s / len));
                    if ((Layers[c] & closed) != 0) return false;
                    short t = CellTrench[c];
                    if (t >= 0 && t != foeTrench) return false;
                    if (mine >= 0 && t != mine) return false;       // along his trench: never up out of it
                }
                return true;
            }

            public void Execute(int i)
            {
                Found[i] = -1; Towards[i] = new float3(0f, float.MaxValue, 0f);
                if (!OnFoot(Flags[i])) return;
                float3 p = Position[i];
                // the man he is at already, while he is still there and near enough
                int foe = Foe[i], kept = -1; float keptD = float.MaxValue;
                if (foe >= 0 && FoeGen[i] == Generation[foe] && Enemy(i, foe))
                {
                    float3 e = Position[foe] - p; e.y = 0f;
                    float d = SimMath.Length(e);
                    float keep = Contact[i] != 0 ? BreakRange : ChargeRange * Keep;
                    if (d <= keep) { kept = foe; keptD = d; }
                }
                // the nearest other: by distance, then slot, whatever order the grid gives them (critic r1: a margin on
                // every candidate made the pick follow the hash map's order)
                int best = -1; float bestD = float.MaxValue;
                if (kept < 0 || Contact[i] == 0)
                {
                    // the nearest enemy man within ChargeRange, on TargetAcquisition's grid (slot order breaks ties)
                    float cell = TargetAcquisitionSystem.GridCell;
                    int minX = math.clamp((int)((p.x - ChargeRange) / cell), 0, GridW - 1), maxX = math.clamp((int)((p.x + ChargeRange) / cell), 0, GridW - 1);
                    int minZ = math.clamp((int)((p.z - ChargeRange) / cell), 0, GridL - 1), maxZ = math.clamp((int)((p.z + ChargeRange) / cell), 0, GridL - 1);
                    for (int cz = minZ; cz <= maxZ; cz++)
                        for (int cx = minX; cx <= maxX; cx++)
                        {
                            if (!Grid.TryGetFirstValue(cz * GridW + cx, out int j, out var it)) continue;
                            do
                            {
                                if (!Enemy(i, j)) continue;
                                float3 e = Position[j] - p; e.y = 0f;
                                float d = SimMath.Length(e);
                                if (d > ChargeRange || j == kept) continue;
                                if (d < bestD || (d == bestD && j < best)) { best = j; bestD = d; }
                            } while (Grid.TryGetNextValue(out j, ref it));
                        }
                }
                // he keeps the man he is at unless another is clearly nearer
                if (kept >= 0 && !(best >= 0 && bestD < keptD - 0.5f)) { best = kept; bestD = keptD; }
                // struck by another while his own foe fights someone else: he turns on the man who struck him
                int s = StruckBy[i];
                if (s >= 0 && s != best && Enemy(i, s) && (best < 0 || Foe[best] != i))
                {
                    // only a man within arm's length and with nothing between them: turning on one further off dropped
                    // him out of his own fight (critic r3)
                    float3 e = Position[s] - p; e.y = 0f;
                    float d = SimMath.Length(e);
                    if (d <= ContactRange && (Layers[Cell((p + Position[s]) * 0.5f)] & (byte)(NavLayer.Blocked | NavLayer.Bunker)) == 0) { best = s; bestD = d; }
                }
                if (best < 0) return;
                float3 to = Position[best] - p; to.y = 0f;
                float dist = SimMath.Length(to);
                float2 dir = dist > 1e-4f ? new float2(to.x, to.z) / dist : new float2(0f, 1f);
                // he may only run at him over ground he can cross; a fight that is on holds to BreakRange without it (the
                // check flickering beside an obstacle made fights start and stop, critic r1). Within arm's length only a
                // wall or a bunker between them stops it.
                bool fighting = best == kept && Contact[i] != 0;
                if (!fighting && dist > ContactRange && !Open(p, Position[best])) return;
                if (!fighting && dist <= ContactRange && (Layers[Cell((p + Position[best]) * 0.5f)] & (byte)(NavLayer.Blocked | NavLayer.Bunker)) != 0) return;
                Found[i] = best;
                Towards[i] = new float3(dir.x, dist, dir.y);
            }
        }

        public ulong Hash(ulong h)
        {
            if (!Foe.IsCreated) return h;
            h = SimHash.Array(Foe, h);
            h = SimHash.Array(FoeGen, h);
            h = SimHash.Array(Contact, h);
            h = SimHash.Array(Swing, h);
            h = SimHash.Array(Dropped, h);
            h = SimHash.Array(Calm, h);
            h = SimHash.Array(StruckBy, h);
            return SimHash.Array(gen, h);
        }

        public void Dispose()
        {
            if (Foe.IsCreated) Foe.Dispose();
            if (FoeGen.IsCreated) FoeGen.Dispose();
            if (Contact.IsCreated) Contact.Dispose();
            if (Swing.IsCreated) Swing.Dispose();
            if (Dropped.IsCreated) Dropped.Dispose();
            if (Calm.IsCreated) Calm.Dispose();
            if (gen.IsCreated) gen.Dispose();
            if (StruckBy.IsCreated) StruckBy.Dispose();
            if (Killed.IsCreated) Killed.Dispose();
            if (found.IsCreated) found.Dispose();
            if (towards.IsCreated) towards.Dispose();
        }
    }
}
