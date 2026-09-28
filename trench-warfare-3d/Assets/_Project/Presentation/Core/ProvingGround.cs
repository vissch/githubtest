// Phase: tooling (2026-09-28) — the Proving Ground's director: the test level where every unit can be fielded, for
// either side, in a real battle (owner, 2026-09-28: "a level or mode that includes all the ideas and unfinished units
// also the finished", "a ui that selects enemy waves", "an expanded units selection for the allied units").
//
// Plain C# over a MatchSim, so it runs in an EditMode test with no scene:
//  - Catalogue: every unit the tables define, with what state it is in (BUILT, PROTOTYPE, STAND-IN) and the ideas that
//    are words only (IDEA: listed, never spawned);
//  - Spawn: put N of a unit at a side's rally point, in ranks (the Unit Sandbox's arithmetic, which now calls this);
//  - Waves: a named list of squads for the enemy. A wave is either PLACED (spawned at the enemy's rally, free, any
//    unit) or DEPLOYED through the enemy's own roster slots by command, a man a tick per slot as the cooldown and the
//    silver allow, which is the path a real opponent's units take (boats on the coast, the rear trench, the walk up);
//  - the queue: one wave repeated on a timer of sim ticks, so the match speed scales it;
//  - enemy support fire on the player's front trench, now;
//  - the sappers of a side ordered to lay a mine or a tripwire ahead of where each stands (the order a HUD button will
//    give for one selected sapper and a point; here for all of them at once, so the unit can be tested before that).
// Placing writes the world directly (SimHost.WriteWorlds: every world the match has, aligned to one tick), as the Unit
// Sandbox and TankCapture do; a replay of such a match does not verify, which a test level does not need. Deploying
// and support fire are commands and replay like any others.
using System;
using System.Collections.Generic;
using Unity.Mathematics;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Presentation
{
    /// <summary>How far along a unit is. The panel prints it on every tile.</summary>
    public enum UnitStage : byte
    {
        /// <summary>Shipped: a model, a portrait, numbers that were balanced.</summary>
        Built = 0,
        /// <summary>The asset playground's art under placeholder numbers (Brute, Croaker, Hopper, Mercy, Frog).</summary>
        Prototype = 1,
        /// <summary>An idea from docs/06 on an existing spec, wearing another unit's model.</summary>
        StandIn = 2,
        /// <summary>Words only: nothing in the sim carries it yet.</summary>
        Idea = 3,
    }

    public sealed class ProvingGround
    {
        public const string MissionId = "proving-ground";
        public static readonly string[] StatusNames = { "BUILT", "PROTOTYPE", "STAND-IN", "IDEA" };

        // ---- the catalogue -------------------------------------------------------------------------------------------
        public struct Unit
        {
            public byte Archetype;
            public string Name, Tip;
            public UnitStage Status;
            public bool Machine;
            public int Cost; public float Hp, Speed;
        }

        /// <summary>A unit that is words only: what it is called and what the sim lacks to carry it.</summary>
        public struct Idea
        {
            public string Name, Needs;
            public bool Machine;
            public Idea(string name, bool machine, string needs) { Name = name; Machine = machine; Needs = needs; }
        }

        /// <summary>docs/06 section 5.4b, "Still ideas", and what each waits for.</summary>
        public static readonly Idea[] Ideas =
        {
            new Idea("Mortar team", false, "indirect fire from a man on foot: only the Kettle lobs today"),
            new Idea("Arditi", false, "a grenade-and-dagger assault rule past the Assault's"),
            new Idea("Field gun crew", false, "a crewed gun: emplacements are not in the sim"),
            new Idea("Cavalry", false, "a mount: a charge, a rider and a horse that can each be hit"),
            new Idea("Mark IV Female", true, "five machine guns in sponsons: a hull carries two guns today"),
            new Idea("Saint-Chamond", true, "a long overhang that ditches nose first"),
            new Idea("Schneider CA1", true, "one offset gun and a wire-cutting prow"),
            new Idea("Garford-Putilov", true, "a rear-facing gun: mounts face forward or turn"),
            new Idea("Lancia 1ZM", true, "twin turrets one over the other"),
            new Idea("Pierce-Arrow", true, "a lorry with an anti-aircraft gun: nothing flies"),
            new Idea("Motorcycle MG", true, "a machine of one man and a sidecar gunner"),
            new Idea("Ehrhardt", true, "an armoured car that drives as fast backwards"),
            new Idea("Observation balloon", true, "a spotter aloft: no air layer in the sim"),
            new Idea("Airship", true, "a bomber aloft: no air layer in the sim"),
        };

        public static UnitStage StatusOf(byte archetype)
        {
            if (archetype >= VehicleArchetype.Brute && archetype <= InfantryArchetype.Frog) return UnitStage.Prototype;   // 21..25
            return Array.IndexOf(UnitDefinitions.ProvingGround, archetype) >= 0 ? UnitStage.StandIn : UnitStage.Built;
        }

        /// <summary>The roster line of an archetype with no match to ask: the compiled switch, else its definition.</summary>
        public static RosterEntry Line(byte archetype)
        {
            var e = RosterEntry.ForArchetype(archetype);
            if (e.Hp > 0f) return e;
            foreach (var d in UnitDefinitions.All) if (d.Archetype == archetype) return d.Roster;
            return default;
        }

        /// <summary>Every unit that can be fielded: men first, then machines, each group in id order. With a world it
        /// reads the match's own table (what will actually spawn); without one, the compiled tables.</summary>
        public static List<Unit> Catalogue(SimWorld world = null)
        {
            var list = new List<Unit>();
            for (int a = 0; a < Archetypes.Count; a++)
            {
                var e = world != null ? world.Units.Roster[a] : Line((byte)a);
                if (e.Hp <= 0f) continue;
                list.Add(new Unit
                {
                    Archetype = (byte)a, Name = UnitLook.Name((byte)a), Tip = UnitLook.Tip((byte)a), Status = StatusOf((byte)a),
                    Machine = e.IsVehicle, Cost = e.Cost, Hp = e.Hp, Speed = e.Speed,
                });
            }
            list.Sort((x, y) => x.Machine != y.Machine ? (x.Machine ? 1 : -1) : x.Archetype.CompareTo(y.Archetype));
            return list;
        }

        /// <summary>A side's ten as the faction hands them out, for the launch screen's starting point.</summary>
        public static byte[] DefaultTen(FactionId faction)
        {
            var ten = new byte[RosterEntry.SlotCount];
            for (int s = 0; s < ten.Length; s++) ten[s] = FactionRoster.Slot(faction, s).Archetype;
            return ten;
        }

        /// <summary>The request the launch screen starts: an endless match on the chosen ground with the two tens, every
        /// support ability on offer, and the scripted enemy as told (off: only the tester's waves come).</summary>
        public static MatchLaunch.Request Request(Ground ground, uint seed, float bombardment, int silver, byte[] ours, byte[] theirs, int ai)
        {
            var r = new MatchLaunch.Request
            {
                MissionId = MissionId, Title = "PROVING GROUND", Difficulty = AiNames[math.clamp(ai, 0, AiNames.Length - 1)],
                ProvingGround = true, Endless = true,
                GeneratedBattlefield = true, Ground = ground, BattlefieldSeed = seed, Bombardment = bombardment,
                StartingSilver = silver, SilverPerSecond = 2f,
                LoadoutA = ours, LoadoutB = theirs,
                AbilityMaskA = 0, AbilityMaskB = 0,
            };
            ApplyAi(ai, r);
            return r;
        }

        // ---- the scripted enemy's presets (MissionCard's difficulties, and OFF) ---------------------------------------
        public static readonly string[] AiNames = { "OFF", "EASY", "NORMAL", "HARD" };

        public struct Ai { public bool On; public int DeployEveryTicks, AttackGarrison, SupportReserve; public bool Attacks, Tanks, Support; }
        public static readonly Ai[] AiPresets =
        {
            new Ai { On = false, DeployEveryTicks = 40, Attacks = false, AttackGarrison = 8, Tanks = false, Support = false, SupportReserve = 180 },
            new Ai { On = true, DeployEveryTicks = 60, Attacks = true, AttackGarrison = 12, Tanks = false, Support = false, SupportReserve = 400 },
            new Ai { On = true, DeployEveryTicks = 40, Attacks = true, AttackGarrison = 8, Tanks = false, Support = true, SupportReserve = 180 },
            new Ai { On = true, DeployEveryTicks = 28, Attacks = true, AttackGarrison = 6, Tanks = true, Support = true, SupportReserve = 120 },
        };

        static void ApplyAi(int ai, MatchLaunch.Request r)
        {
            var p = AiPresets[math.clamp(ai, 0, AiPresets.Length - 1)];
            r.ScriptedPeer = p.On; r.PeerDeployEveryTicks = p.DeployEveryTicks; r.PeerAttacks = p.Attacks; r.PeerAttackGarrison = p.AttackGarrison;
            r.PeerDeploysTanks = p.Tanks; r.PeerUsesSupport = p.Support; r.PeerSupportReserve = p.SupportReserve;
        }

        /// <summary>The same, on a running match.</summary>
        public static void ApplyAi(int ai, SimHost h)
        {
            if (h == null) return;
            var p = AiPresets[math.clamp(ai, 0, AiPresets.Length - 1)];
            h.ScriptedPeer = p.On; h.PeerDeployEveryTicks = p.DeployEveryTicks; h.PeerAttacks = p.Attacks; h.PeerAttackGarrison = p.AttackGarrison;
            h.PeerDeploysTanks = p.Tanks; h.PeerUsesSupport = p.Support; h.PeerSupportReserve = p.SupportReserve;
        }

        // ---- placing ---------------------------------------------------------------------------------------------------
        public const float ManStep = 1.5f, MachineStep = 9f;
        public const int MenPerRank = 10, MachinesPerRank = 4;

        /// <summary>
        /// N of a unit at the side's rally point, in ranks centred on it, the later ranks behind the first (away from the
        /// enemy). Each has the hit points and speed the match's table gives the archetype. Returns how many were placed:
        /// fewer than asked when the world ran out of slots, none for an id nothing defines.
        /// </summary>
        public static int Place(MatchSim m, int team, byte archetype, int count)
        {
            if (m == null || count <= 0 || team < 0 || team >= SimConfig.MaxPlayers || archetype >= Archetypes.Count) return 0;
            var w = m.World;
            var e = w.Units.Roster[archetype];
            if (e.Hp <= 0f) return 0;
            bool machine = ChassisKind.IsArmoured(w.ChassisOf(archetype));
            float step = machine ? MachineStep : ManStep;
            int perRank = machine ? MachinesPerRank : MenPerRank;
            float3 at = w.Rally[team];
            float back = team == 0 ? -1f : 1f;   // player 0 attacks toward +z (SimWorld.Spawn faces him that way)
            int placed = 0;
            for (int i = 0; i < count; i++)
            {
                int rank = i / perRank, file = i % perRank;
                int inRank = math.min(perRank, count - rank * perRank);
                var p = new float3(at.x + (file - (inRank - 1) * 0.5f) * step, 0f, at.z + back * rank * step);
                if (w.Spawn((byte)team, archetype, p, e.Hp, e.Speed, machine) < 0) break;
                placed++;
            }
            return placed;
        }

        // ---- waves -----------------------------------------------------------------------------------------------------
        public struct Squad
        {
            public byte Archetype; public int Count;
            public Squad(byte archetype, int count) { Archetype = archetype; Count = count; }
        }

        public sealed class Wave
        {
            public string Name = "", Blurb = "";
            public readonly List<Squad> Squads = new List<Squad>();
            public Wave() { }
            public Wave(string name, string blurb, params Squad[] squads) { Name = name; Blurb = blurb; Squads.AddRange(squads); }
            public int Units { get { int n = 0; foreach (var s in Squads) n += s.Count; return n; } }
            public bool Empty => Units == 0;

            /// <summary>One more squad, merged into the squad of the same unit if the wave has one. Capped per unit.</summary>
            public void Add(byte archetype, int count)
            {
                if (count <= 0) return;
                for (int i = 0; i < Squads.Count; i++)
                    if (Squads[i].Archetype == archetype) { Squads[i] = new Squad(archetype, math.min(MaxPerSquad, Squads[i].Count + count)); return; }
                Squads.Add(new Squad(archetype, math.min(MaxPerSquad, count)));
            }

            public Wave Copy() { var c = new Wave { Name = Name, Blurb = Blurb }; c.Squads.AddRange(Squads); return c; }

            /// <summary>"12 RIFLE, 2 MAW": what the wave holds, for a row of the panel.</summary>
            public string Describe()
            {
                if (Squads.Count == 0) return "EMPTY";
                var sb = new System.Text.StringBuilder();
                foreach (var s in Squads)
                {
                    if (sb.Length > 0) sb.Append(", ");
                    sb.Append(s.Count).Append(' ').Append(UnitLook.Name(s.Archetype).ToUpperInvariant());
                }
                return sb.ToString();
            }
        }

        public const int MaxPerSquad = 60;

        static Squad S(byte a, int n) => new Squad(a, n);

        /// <summary>The waves the panel offers ready made. "EVERYTHING" is built from the catalogue, so a new unit joins it.</summary>
        public static List<Wave> Presets()
        {
            var list = new List<Wave>
            {
                new Wave("RIFLE LINE", "Twelve riflemen: the plainest attack there is.", S(InfantryArchetype.Rifle, 12)),
                new Wave("ASSAULT RUSH", "Fast men with short guns behind a screen of rifles.", S(InfantryArchetype.Assault, 10), S(InfantryArchetype.Rifle, 6)),
                new Wave("GUNS AND GLASS", "Machine guns and snipers: a wave that stops and shoots.", S(InfantryArchetype.Machinegunner, 4), S(InfantryArchetype.Sniper, 3), S(InfantryArchetype.Rifle, 6)),
                new Wave("COMBINED ARMS", "An officer, a medic, a shield, rifles and a Tusk.", S(InfantryArchetype.Officer, 1), S(InfantryArchetype.Medic, 1), S(InfantryArchetype.Shield, 2), S(InfantryArchetype.Rifle, 10), S(VehicleArchetype.Tusk, 1)),
                new Wave("ARMOUR PUSH", "Two Maws and a Tusk with infantry behind.", S(VehicleArchetype.Maw, 2), S(VehicleArchetype.Tusk, 1), S(InfantryArchetype.Rifle, 8)),
                new Wave("WALKERS", "A Pincer, a Kettle, a Pavise and a Redoubt.", S(VehicleArchetype.Pincer, 1), S(VehicleArchetype.Kettle, 1), S(VehicleArchetype.Pavise, 1), S(VehicleArchetype.Redoubt, 1)),
                new Wave("PROTOTYPES", "The playground's five: Brute, Croaker, Hopper, Mercy and six Frogs.", S(VehicleArchetype.Brute, 1), S(VehicleArchetype.Croaker, 1), S(VehicleArchetype.Hopper, 1), S(VehicleArchetype.Mercy, 1), S(InfantryArchetype.Frog, 6)),
                new Wave("HISTORICAL ARMOUR", "Mark IV, Mark V, A7V, Renault FT, Whippet and Austin.", S(VehicleArchetype.MarkIV, 1), S(VehicleArchetype.MarkV, 1), S(VehicleArchetype.A7V, 1), S(VehicleArchetype.RenaultFT, 1), S(VehicleArchetype.Whippet, 1), S(VehicleArchetype.Austin, 1)),
                new Wave("SPECIALISTS", "Sentries, AT rifles, the Death Battalion, sappers and flamethrowers.", S(InfantryArchetype.Sentry, 2), S(InfantryArchetype.AtRifle, 2), S(InfantryArchetype.DeathBattalion, 6), S(InfantryArchetype.Sapper, 1), S(InfantryArchetype.Flamethrower, 2)),
            };
            var all = new Wave { Name = "EVERYTHING", Blurb = "One of every unit the tables define." };
            foreach (var u in Catalogue()) all.Squads.Add(new Squad(u.Archetype, 1));
            list.Add(all);
            return list;
        }

        // ---- a running match -------------------------------------------------------------------------------------------
        readonly Func<Action<MatchSim>, bool> write;
        readonly Func<MatchSim> view;
        readonly Func<ICommandSink> enemy;
        readonly Func<ICommandSink> player;

        /// <summary>Waves go through the enemy's roster slots by command instead of being placed.</summary>
        public bool ThroughSlots;
        /// <summary>The wave the timer sends, or null.</summary>
        public Wave Scheduled { get; private set; }
        public int EveryTicks { get; private set; }
        public uint NextTick { get; private set; }
        /// <summary>Men and machines the last few calls could not field, and why (the panel's status line).</summary>
        public string Last { get; private set; } = "";
        public int WavesSent { get; private set; }

        struct Owed { public int Slot; public int Count; }
        readonly List<Owed> owed = new List<Owed>();
        /// <summary>A deploy that was issued and is not in the view yet: a seat's command runs InputDelayTicks after
        /// it is issued, and until then the view shows its slot ready and its price unspent.</summary>
        struct Flight { public int Slot; public int Cost; public uint Seen; }
        readonly List<Flight> flying = new List<Flight>();
        /// <summary>Ticks past the seat's delay before a deploy is looked for in the view (the tick it runs in, and one
        /// the enemy's view may be behind the world the seat counts from).</summary>
        public const int SeenMargin = 2;
        uint lastTick = uint.MaxValue;

        /// <param name="write">Write every world of the match (SimHost.WriteWorlds); false when it could not.</param>
        /// <param name="view">The world the enemy reads (SimHost.EnemyView).</param>
        /// <param name="enemy">The enemy's seat (SimHost.EnemySeat).</param>
        /// <param name="player">The player's seat (SimHost.LocalDriver); null when nothing orders the player's men.</param>
        public ProvingGround(Func<Action<MatchSim>, bool> write, Func<MatchSim> view, Func<ICommandSink> enemy, Func<ICommandSink> player = null)
        {
            this.write = write; this.view = view; this.enemy = enemy; this.player = player;
        }

        public static ProvingGround For(SimHost h) => new ProvingGround(h.WriteWorlds, () => h.EnemyView, () => h.EnemySeat, () => h.LocalDriver);

        public const float LayAheadMetres = 18f;
        public const int TripwireMetres = 8;

        /// <summary>
        /// Every sapper of a side who has a charge and no errand is ordered to lay ahead of himself: a mine at the point
        /// LayAheadMetres toward the enemy, or a tripwire across the front from there. The order is the sim's own
        /// (CommandType.UnitAbility). A sapper the sim would refuse is not ordered and is counted in what the panel
        /// says (his point is in a trench, a bunker or off the map, or he is pinned: the sim's own rules, asked of its
        /// own mine system): seen in Play on 2026-09-28, five men walking up to their trench were all "sent" and all
        /// refused, and nothing on the panel said so. Returns how many were ordered.
        /// </summary>
        public int OrderSappers(int team, UnitAbilityId ability, float ahead = LayAheadMetres)
        {
            var v = view(); var seat = team == 0 ? player?.Invoke() : enemy();
            if (v == null || seat == null || v.Sapper == null) { Last = "NOBODY TO ORDER"; return 0; }
            if (ability != UnitAbilityId.LayMine && ability != UnitAbilityId.LayTripwire) { Last = "SAPPERS LAY MINES AND TRIPWIRES"; return 0; }
            var w = v.World; int n = 0, noGround = 0, pinned = 0;
            float forward = team == 0 ? 1f : -1f;
            bool wire = ability == UnitAbilityId.LayTripwire;
            int args = wire ? AbilityArgs.Pack(90, 0, TripwireMetres) : 0;   // 90: across the front
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || w.Team[i] != team || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                if (w.Units.Infantry[w.Archetype[i]].MineCharges <= 0 || v.Sapper.ChargesOf(w, i) <= 0 || v.Sapper.Phase[i] != 0) continue;
                var p = w.Position[i];
                var at = new float3(p.x, 0f, p.z + forward * ahead);
                if (w.Suppression[i] >= StanceRules.PinnedSuppression) { pinned++; continue; }
                if (v.Mines != null)
                {
                    var on = w.ClampToMap(at); on.y = 0f;
                    if (wire ? !v.Mines.LiesAlong(on, AbilityArgs.Heading(90), TripwireMetres) : !v.Mines.Lies(on)) { noGround++; continue; }
                }
                seat.Issue(new SimCommand
                {
                    Tick = w.Tick, Player = (byte)team, Type = CommandType.UnitAbility, A = i, B = (int)ability | (args << 8),
                    Pos = at,
                });
                n++;
            }
            string what = wire ? "A TRIPWIRE" : "A MINE", whose = team == 0 ? "OURS" : "THEIRS";
            string kept = (noGround > 0 ? $"; {noGround} NOT: NO OPEN GROUND {ahead:0} M AHEAD OF {(noGround == 1 ? "HIM" : "THEM")}" : "")
                + (pinned > 0 ? $"; {pinned} PINNED" : "");
            Last = n > 0 ? $"{n} SAPPER{(n == 1 ? "" : "S")} OF {whose} SENT TO LAY {what} {ahead:0} M AHEAD{kept}"
                : noGround + pinned > 0 ? $"NO SAPPER OF {whose} SENT{kept}"
                : $"NO SAPPER OF {whose} HAS A CHARGE AND NO ERRAND";
            return n;
        }

        /// <summary>Deploys still owed to the enemy's slots (a wave sent THROUGH THEIR SLOTS that is not all out yet).</summary>
        public int Pending { get { int n = 0; foreach (var o in owed) n += o.Count; return n; } }

        /// <summary>N of a unit for a side, at its rally point. Returns how many were placed.</summary>
        public int Spawn(int team, byte archetype, int count)
        {
            int placed = 0;
            bool ok = write(m => placed = Place(m, team, archetype, count));
            string name = UnitLook.Name(archetype).ToUpperInvariant();
            Last = !ok ? "THE WORLDS ARE A TICK APART: TRY AGAIN"
                : placed == count ? $"{placed} {name} FOR {(team == 0 ? "US" : "THEM")}"
                : $"{placed} OF {count} {name}: THE FIELD IS FULL";
            return placed;
        }

        public void GiveSilver(int team, int amount)
        {
            write(m => { if (team >= 0 && team < m.World.Silver.Length) m.World.Silver[team] += amount; });
            Last = $"{amount} SILVER FOR {(team == 0 ? "US" : "THEM")}";
        }

        /// <summary>Send a wave at the player now. Returns how many units are on their way (placed, or owed to a slot).</summary>
        public int Send(Wave wave)
        {
            if (wave == null || wave.Empty) { Last = "THE WAVE IS EMPTY"; return 0; }
            int sent = ThroughSlots ? Deploy(wave) : PlaceWave(wave);
            if (sent > 0) WavesSent++;
            return sent;
        }

        int PlaceWave(Wave wave)
        {
            int placed = 0, asked = wave.Units;
            bool ok = write(m => { foreach (var s in wave.Squads) placed += Place(m, 1, s.Archetype, s.Count); });
            Last = !ok ? "THE WORLDS ARE A TICK APART: TRY AGAIN"
                : placed == asked ? $"{wave.Name}: {placed} PLACED" : $"{wave.Name}: {placed} OF {asked} PLACED, THE FIELD IS FULL";
            return placed;
        }

        /// <summary>The enemy's roster slot that fields an archetype; -1 when its ten does not.</summary>
        public static int SlotOf(SimWorld w, int player, byte archetype)
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                int ri = player * RosterEntry.SlotCount + s;
                if (w.SlotUnlocked[ri] != 0 && w.Roster[ri].Archetype == archetype && w.Roster[ri].Hp > 0f) return s;
            }
            return -1;
        }

        int Deploy(Wave wave)
        {
            var v = view(); if (v == null) { Last = "NO MATCH"; return 0; }
            var w = v.World;
            int queued = 0, cost = 0; var missing = new List<string>();
            foreach (var s in wave.Squads)
            {
                int slot = SlotOf(w, 1, s.Archetype);
                if (slot < 0) { missing.Add(UnitLook.Name(s.Archetype).ToUpperInvariant()); continue; }
                owed.Add(new Owed { Slot = slot, Count = s.Count });
                queued += s.Count; cost += s.Count * w.Roster[RosterEntry.SlotCount + slot].Cost;
            }
            if (cost > 0) write(m => m.World.Silver[1] += cost);   // the wave is the tester's, not the enemy's purse's
            Last = missing.Count == 0 ? $"{wave.Name}: {queued} GO UP THROUGH THEIR SLOTS"
                : $"{wave.Name}: {queued} THROUGH THEIR SLOTS; NOT IN THEIR TEN: {string.Join(", ", missing)}";
            return queued;
        }

        /// <summary>Repeat a wave every so many seconds of sim time; the first comes after one interval. Null or 0 stops it.</summary>
        public void Schedule(Wave wave, float everySeconds)
        {
            var v = view();
            if (wave == null || wave.Empty || everySeconds <= 0f || v == null) { Scheduled = null; EveryTicks = 0; return; }
            Scheduled = wave.Copy();
            EveryTicks = math.max(1, (int)math.round(everySeconds / v.World.Config.TickSeconds));
            NextTick = v.World.Tick + (uint)EveryTicks;
        }

        public void Unschedule() { Scheduled = null; EveryTicks = 0; }

        /// <summary>Seconds of sim time until the timer's next wave; negative when nothing is scheduled.</summary>
        public float SecondsToNext
        {
            get
            {
                var v = view();
                if (Scheduled == null || v == null) return -1f;
                return NextTick > v.World.Tick ? (NextTick - v.World.Tick) * v.World.Config.TickSeconds : 0f;
            }
        }

        /// <summary>Every frame (any number of times a tick: it acts once per tick of the enemy's view).</summary>
        public void Tick()
        {
            var v = view(); if (v == null) return;
            var w = v.World; uint t = w.Tick;
            if (t == lastTick) return;
            lastTick = t;
            if (Scheduled != null && EveryTicks > 0 && t >= NextTick)
            {
                NextTick = t + (uint)EveryTicks;
                Send(Scheduled);
            }
            if (owed.Count == 0) return;
            var seat = enemy(); if (seat == null) return;
            // a man per slot at a time, and the next when the last is in the view: seen in Play on 2026-09-28, a deploy
            // issued every tick was issued again before the first had run (three ticks later), while the view still
            // showed the slot ready, and the sim refused it: of two Brutes one came, and the panel said none was owed
            int spent = 0; ulong used = 0;
            uint seen = t + (uint)(math.max(0, w.Config.InputDelayTicks) + SeenMargin);
            flying.RemoveAll(f => t >= f.Seen || f.Seen > seen);   // in the view by now, or of a match before this one
            foreach (var f in flying) { used |= 1ul << f.Slot; spent += f.Cost; }
            for (int i = 0; i < owed.Count; i++)
            {
                var o = owed[i];
                ulong bit = 1ul << o.Slot;
                if ((used & bit) != 0) continue;
                int ri = RosterEntry.SlotCount + o.Slot;
                int cost = w.Roster[ri].Cost;
                if (w.SlotCooldown[ri] != 0 || w.Silver[1] - spent < cost) continue;
                used |= bit; spent += cost;
                flying.Add(new Flight { Slot = o.Slot, Cost = cost, Seen = seen });
                seat.Issue(SimCommand.Deploy(t, 1, o.Slot));
                o.Count--; owed[i] = o;
            }
            owed.RemoveAll(o => o.Count <= 0);
        }

        /// <summary>The enemy's guns or gas on the player's front trench, now: on its men if it has any, else on the
        /// player's rally point. The enemy is given the silver; its cooldown still holds (the sim refuses, and says so
        /// in its own event). False when there is no seat to issue from or no such ability.</summary>
        public bool EnemySupport(OffMapAbilityId ability)
        {
            var v = view(); var seat = enemy();
            if (v == null || seat == null || v.Abilities == null || !OffMapAbilitySystem.TryGetStats((int)ability, out var stats)) { Last = "NO SUCH SUPPORT"; return false; }
            var w = v.World;
            if (v.Abilities.CooldownOf(1, ability) != 0) { Last = $"THEIR {ability.ToString().ToUpperInvariant()} IS STILL RELOADING"; return false; }
            float3 at = w.Rally[0]; float3 sum = float3.zero; int n = 0;
            short mine = v.Fields.FrontTrench(0);
            if (mine >= 0)
                for (int i = 0; i < w.HighWater; i++)
                    if (w.IsAlive(i) && w.TrenchId[i] == mine) { sum += w.Position[i]; n++; }
            if (n > 0) at = sum / n;
            if (ability == OffMapAbilityId.ChlorineGas || ability == OffMapAbilityId.MustardGas) at.z += 12f;   // upwind, as the scripted enemy does
            int cost = stats.Cost;
            write(m => m.World.Silver[1] += cost);
            seat.Issue(new SimCommand { Tick = w.Tick, Player = 1, Type = CommandType.SupportFire, A = (int)ability, Pos = new float3(at.x, 0f, at.z) });
            Last = $"THEIR {ability.ToString().ToUpperInvariant()} ON {(n > 0 ? "YOUR FRONT TRENCH" : "YOUR RALLY POINT")}";
            return true;
        }

        /// <summary>Every living unit of a side removed (cause: none), to clear the field between tests.</summary>
        public int Clear(int team)
        {
            int n = 0;
            write(m =>
            {
                var w = m.World; int c = 0;
                for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) { w.Despawn(i); c++; }
                n = c;
            });
            Last = $"{n} OF {(team == 0 ? "OURS" : "THEIRS")} REMOVED";
            return n;
        }
    }
}
