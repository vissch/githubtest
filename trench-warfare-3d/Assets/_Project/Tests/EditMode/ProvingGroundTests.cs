// Phase: tooling (2026-09-28) — the Proving Ground's director (Presentation/Core/ProvingGround.cs): the catalogue
// lists every defined unit once with words of its own, placing puts N of a unit at a side's rally in ranks, a wave is
// placed or goes up through the enemy's slots by command, the timer repeats one, the enemy's support is a command, the
// launch request is endless with the chosen tens, and placing is the same on two worlds.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Net;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class ProvingGroundTests
    {
        sealed class Seat : ICommandSink
        {
            readonly byte player;
            public Seat(byte player = 1) { this.player = player; }
            public int Player => player;
            public readonly List<SimCommand> Issued = new List<SimCommand>();
            public readonly List<SimCommand> All = new List<SimCommand>();
            public void Issue(SimCommand c) { c.Player = player; Issued.Add(c); All.Add(c); }
        }

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly Seat Enemy = new Seat(), Player = new Seat(0);
            public readonly ProvingGround Ground;
            public readonly List<SimEvent> Log = new List<SimEvent>();
            public SimWorld W => M.World;

            public Rig(byte[] theirs = null, uint seed = 0xC0FFEE)
            {
                var cfg = SimConfig.Default; cfg.Seed = seed; cfg.Endless = true;
                if (theirs != null) { var l = new FixedList32Bytes<byte>(); foreach (var a in theirs) l.Add(a); cfg.LoadoutB = l; }
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.HashInterval = 1;
                Ground = new ProvingGround(a => { a(M); return true; }, () => M, () => Enemy, () => Player);
            }

            /// <summary>One tick: the director thinks, then the world steps with what the enemy's seat was told.</summary>
            public void Step()
            {
                Ground.Tick();
                var both = new List<SimCommand>(Player.Issued); both.AddRange(Enemy.Issued);   // by player, each in issue order
                using var cmds = new NativeArray<SimCommand>(both.ToArray(), Allocator.Temp);
                Enemy.Issued.Clear(); Player.Issued.Clear();
                M.Step(cmds);
                var ev = W.Events.Events;
                for (int k = 0; k < ev.Length; k++) Log.Add(ev[k]);
            }

            public int Alive(int team, int archetype = -1)
            {
                int n = 0;
                for (int i = 0; i < W.HighWater; i++)
                    if (W.IsAlive(i) && W.Team[i] == team && (archetype < 0 || W.Archetype[i] == archetype)) n++;
                return n;
            }

            public int Count(SimEventType type) { int n = 0; foreach (var e in Log) if (e.Type == type) n++; return n; }
            public void Dispose() => M.Dispose();
        }

        [Test]
        public void TheCatalogueListsEveryDefinedUnitOnceWithWordsOfItsOwn()
        {
            using var r = new Rig();
            var units = ProvingGround.Catalogue(r.W);
            int defined = 0;
            for (int a = 0; a < Archetypes.Count; a++) if (r.W.Units.Roster[a].Hp > 0f) defined++;
            Assert.AreEqual(defined, units.Count, "one row for every id the match's table defines");
            Assert.GreaterOrEqual(units.Count, 37, "ids 0..36");

            var names = new Dictionary<string, byte>();
            string generic = UnitLook.Tip(200);
            bool machines = false;
            foreach (var u in units)
            {
                Assert.AreNotEqual("Vehicle", u.Name, $"archetype {u.Archetype} has no name of its own");
                Assert.AreNotEqual(generic, u.Tip, $"archetype {u.Archetype} has no tooltip of its own");
                Assert.LessOrEqual(u.Tip.Length, 100, $"{u.Name}'s tooltip is clipped");
                Assert.IsFalse(names.ContainsKey(u.Name), $"two units are called {u.Name}");
                names[u.Name] = u.Archetype;
                Assert.AreEqual(r.W.Units.Roster[u.Archetype].IsVehicle, u.Machine);
                if (u.Machine) machines = true; else Assert.IsFalse(machines, "men first, then machines");
                var want = u.Archetype >= 21 && u.Archetype <= 25 ? UnitStage.Prototype : u.Archetype >= 26 && u.Archetype <= 36 ? UnitStage.StandIn : UnitStage.Built;
                Assert.AreEqual(want, u.Status, $"{u.Name} ({u.Archetype})");
            }
            // with no match to ask it says the same
            var offline = ProvingGround.Catalogue();
            Assert.AreEqual(units.Count, offline.Count);
            for (int i = 0; i < units.Count; i++)
            {
                Assert.AreEqual(units[i].Archetype, offline[i].Archetype);
                Assert.AreEqual(units[i].Cost, offline[i].Cost, units[i].Name);
                Assert.AreEqual(units[i].Hp, offline[i].Hp, units[i].Name);
            }
        }

        [Test]
        public void EveryUnitWearsAPortraitTheSkinHas()
        {
            foreach (var u in ProvingGround.Catalogue())
                Assert.GreaterOrEqual(Array.IndexOf(TW.UI.SkinSpec.PortraitNames, UnitLook.PortraitName(u.Archetype)), 0,
                    $"{u.Name} ({u.Archetype}) names the portrait \"{UnitLook.PortraitName(u.Archetype)}\", which the skin does not bake");
        }

        [Test]
        public void PlacingPutsThemAtTheRallyInRanksWithTheTablesNumbers()
        {
            using var r = new Rig();
            Assert.AreEqual(0, r.Alive(0), "setup: an empty field");
            Assert.AreEqual(25, r.Ground.Spawn(0, InfantryArchetype.Flamethrower, 25));
            Assert.AreEqual(25, r.Alive(0, InfantryArchetype.Flamethrower));
            var rally = r.W.Rally[0]; var e = r.W.Units.Roster[InfantryArchetype.Flamethrower];
            float minX = float.MaxValue, maxX = float.MinValue, minZ = float.MaxValue, maxZ = float.MinValue;
            for (int i = 0; i < r.W.HighWater; i++)
            {
                if (!r.W.IsAlive(i)) continue;
                var p = r.W.Position[i];
                minX = math.min(minX, p.x); maxX = math.max(maxX, p.x); minZ = math.min(minZ, p.z); maxZ = math.max(maxZ, p.z);
                Assert.AreEqual(e.Hp, r.W.Hp[i]); Assert.AreEqual(e.Speed, r.W.Speed[i]);
                Assert.AreEqual(0u, r.W.Flags[i] & (uint)UnitFlags.Vehicle, "a man is not a vehicle");
            }
            Assert.AreEqual((ProvingGround.MenPerRank - 1) * ProvingGround.ManStep, maxX - minX, 1e-3f, "ten to a rank");
            Assert.AreEqual(rally.x, (minX + maxX) * 0.5f, 1e-3f, "centred on the rally point");
            Assert.AreEqual(rally.z, maxZ, 1e-3f, "the first rank on it");
            Assert.AreEqual(rally.z - 2 * ProvingGround.ManStep, minZ, 1e-3f, "two more ranks behind, away from the enemy");

            Assert.AreEqual(3, r.Ground.Spawn(1, VehicleArchetype.Brute, 3));
            for (int i = 0; i < r.W.HighWater; i++)
                if (r.W.IsAlive(i) && r.W.Team[i] == 1)
                {
                    Assert.AreNotEqual(0u, r.W.Flags[i] & (uint)UnitFlags.Vehicle, "a Brute is a vehicle");
                    Assert.AreEqual(r.W.Units.Roster[VehicleArchetype.Brute].Hp, r.W.Hp[i]);
                }
            StringAssert.Contains("3 BRUTE FOR THEM", r.Ground.Last);
        }

        [Test]
        public void AnIdNothingDefinesIsNotPlaced()
        {
            using var r = new Rig();
            Assert.AreEqual(0, r.Ground.Spawn(0, 50, 5));
            Assert.AreEqual(0, r.Ground.Spawn(0, InfantryArchetype.Rifle, 0));
            Assert.AreEqual(0, ProvingGround.Place(r.M, 7, InfantryArchetype.Rifle, 1), "no such side");
            Assert.AreEqual(0, r.W.AliveCount);
        }

        [Test]
        public void EveryPresetFieldsOnlyUnitsTheTableDefines()
        {
            using var r = new Rig();
            var presets = ProvingGround.Presets();
            Assert.GreaterOrEqual(presets.Count, 8);
            var seen = new HashSet<string>();
            foreach (var wave in presets)
            {
                Assert.IsTrue(seen.Add(wave.Name), $"two waves are called {wave.Name}");
                Assert.IsNotEmpty(wave.Blurb, wave.Name);
                Assert.Greater(wave.Units, 0, wave.Name);
                foreach (var s in wave.Squads) Assert.Greater(r.W.Units.Roster[s.Archetype].Hp, 0f, $"{wave.Name} names archetype {s.Archetype}, which nothing defines");
            }
            var all = presets[presets.Count - 1];
            Assert.AreEqual("EVERYTHING", all.Name);
            Assert.AreEqual(ProvingGround.Catalogue(r.W).Count, all.Units, "one of each");
        }

        [Test]
        public void AWaveIsPlacedForTheEnemyAtHisRally()
        {
            using var r = new Rig();
            var wave = ProvingGround.Presets().Find(w => w.Name == "PROTOTYPES");
            Assert.AreEqual(wave.Units, r.Ground.Send(wave));
            Assert.AreEqual(wave.Units, r.Alive(1));
            Assert.AreEqual(0, r.Alive(0));
            foreach (var s in wave.Squads) Assert.AreEqual(s.Count, r.Alive(1, s.Archetype), UnitLook.Name(s.Archetype));
            Assert.AreEqual(1, r.Ground.WavesSent);
            Assert.AreEqual(0, r.Enemy.All.Count, "placed, not ordered");
            for (int t = 0; t < 100; t++) r.Step();
            Assert.AreEqual(-1, r.W.WinnerTeam);
            // they march on the player: the furthest forward has left the rally behind him
            float front = float.MaxValue;
            for (int i = 0; i < r.W.HighWater; i++) if (r.W.IsAlive(i) && r.W.Team[i] == 1) front = math.min(front, r.W.Position[i].z);
            Assert.Less(front, r.W.Rally[1].z - 2f, "the wave moves toward the player's line");
        }

        [Test]
        public void AWaveThroughTheirSlotsIsDeployedByCommandAManATickPerSlot()
        {
            var ten = new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Flamethrower, VehicleArchetype.Croaker, 0, 0, 0, 0, 0, 0, 0 };
            using var r = new Rig(ten);
            r.Ground.ThroughSlots = true;
            int silver = r.W.Silver[1];
            var wave = new ProvingGround.Wave { Name = "TEST" };
            wave.Add(InfantryArchetype.Rifle, 4); wave.Add(InfantryArchetype.Flamethrower, 2); wave.Add(VehicleArchetype.Croaker, 1);
            wave.Add(VehicleArchetype.Brute, 2);   // not in their ten
            Assert.AreEqual(7, r.Ground.Send(wave), "the Brutes cannot go through a slot");
            StringAssert.Contains("BRUTE", r.Ground.Last);
            Assert.AreEqual(7, r.Ground.Pending);
            Assert.AreEqual(0, r.Alive(1), "nothing is placed");
            int cost = 4 * r.W.Roster[RosterEntry.SlotCount + 0].Cost + 2 * r.W.Roster[RosterEntry.SlotCount + 1].Cost + r.W.Roster[RosterEntry.SlotCount + 2].Cost;
            Assert.AreEqual(silver + cost, r.W.Silver[1], "the wave's price is put in their purse");

            r.Step();
            Assert.AreEqual(3, r.Enemy.All.Count, "one deploy per slot in a tick");
            for (int t = 0; t < 400 && r.Ground.Pending > 0; t++) r.Step();
            Assert.AreEqual(0, r.Ground.Pending);
            for (int t = 0; t < 5; t++) r.Step();
            Assert.AreEqual(7, r.Enemy.All.Count);
            foreach (var c in r.Enemy.All) { Assert.AreEqual(CommandType.DeployUnit, c.Type); Assert.AreEqual(1, c.Player); }
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected), "none was refused");
            Assert.AreEqual(4, r.Alive(1, InfantryArchetype.Rifle));
            Assert.AreEqual(2, r.Alive(1, InfantryArchetype.Flamethrower));
            Assert.AreEqual(1, r.Alive(1, VehicleArchetype.Croaker));
            Assert.AreEqual(0, r.Alive(1, VehicleArchetype.Brute));
        }

        [Test]
        public void TheTimerSendsItsWaveEveryInterval()
        {
            using var r = new Rig();
            var wave = new ProvingGround.Wave { Name = "DRIP" }; wave.Add(InfantryArchetype.Rifle, 3);
            r.Ground.Schedule(wave, 5f);
            wave.Add(InfantryArchetype.Rifle, 50);   // the schedule keeps its own copy
            int every = (int)math.round(5f / r.W.Config.TickSeconds);
            Assert.AreEqual(every, r.Ground.EveryTicks);
            Assert.AreEqual(5f, r.Ground.SecondsToNext, 0.06f);
            for (int t = 0; t < every; t++) r.Step();
            Assert.AreEqual(0, r.Ground.WavesSent, "the first comes after one interval");
            r.Step();
            Assert.AreEqual(1, r.Ground.WavesSent);
            for (int t = 0; t < every; t++) r.Step();
            Assert.AreEqual(2, r.Ground.WavesSent);
            Assert.AreEqual(6, r.Alive(1, InfantryArchetype.Rifle), "two waves of three");
            r.Ground.Unschedule();
            for (int t = 0; t < 2 * every; t++) r.Step();
            Assert.AreEqual(2, r.Ground.WavesSent, "stopped");
            Assert.Less(r.Ground.SecondsToNext, 0f);
        }

        [Test]
        public void TheEnemysSupportIsACommandOnThePlayersSide()
        {
            using var r = new Rig();
            int silver = r.W.Silver[1];
            Assert.IsTrue(r.Ground.EnemySupport(OffMapAbilityId.HeBarrage));
            Assert.AreEqual(1, r.Enemy.All.Count);
            var c = r.Enemy.All[0];
            Assert.AreEqual(CommandType.SupportFire, c.Type); Assert.AreEqual((int)OffMapAbilityId.HeBarrage, c.A);
            Assert.AreEqual(r.W.Rally[0].x, c.Pos.x, 1e-3f, "nobody in the front trench: the rally point");
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var stats);
            Assert.AreEqual(silver + stats.Cost, r.W.Silver[1]);
            r.Step();
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected), "the sim took it");
            Assert.Greater(r.M.Abilities.CooldownOf(1, OffMapAbilityId.HeBarrage), 0);
            Assert.IsFalse(r.Ground.EnemySupport(OffMapAbilityId.HeBarrage), "reloading: no second command");
            Assert.AreEqual(1, r.Enemy.All.Count);
        }

        [Test]
        public void TheSappersOfASideAreOrderedToLayAheadOfThemselves()
        {
            using var r = new Rig();
            Assert.AreEqual(0, r.Ground.OrderSappers(0, TW.Sim.Units.UnitAbilityId.LayMine), "no sapper on the field");
            StringAssert.Contains("NO SAPPER", r.Ground.Last);
            r.Ground.Spawn(0, InfantryArchetype.Sapper, 2);
            r.Ground.Spawn(0, InfantryArchetype.Rifle, 3);
            r.Ground.Spawn(1, InfantryArchetype.Sapper, 1);
            Assert.AreEqual(0, r.Ground.OrderSappers(0, TW.Sim.Units.UnitAbilityId.MobileCover), "not a sapper's ability");
            Assert.AreEqual(2, r.Ground.OrderSappers(0, TW.Sim.Units.UnitAbilityId.LayMine), "ours, and only the sappers");
            Assert.AreEqual(2, r.Player.All.Count); Assert.AreEqual(0, r.Enemy.All.Count);
            foreach (var c in r.Player.All)
            {
                Assert.AreEqual(CommandType.UnitAbility, c.Type); Assert.AreEqual(0, c.Player);
                Assert.AreEqual((int)TW.Sim.Units.UnitAbilityId.LayMine, c.B & 0xFF);
                Assert.AreEqual(InfantryArchetype.Sapper, r.W.Archetype[c.A]);
                Assert.AreEqual(r.W.Position[c.A].z + ProvingGround.LayAheadMetres, c.Pos.z, 1e-3f, "toward the enemy");
            }
            r.Step();
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected), "the sim took both orders");
            Assert.AreEqual(2, r.Count(SimEventType.SapperOrdered));
            Assert.AreEqual(0, r.Ground.OrderSappers(0, TW.Sim.Units.UnitAbilityId.LayMine), "both are on an errand");
            for (int t = 0; t < 600 && r.Count(SimEventType.MinePlaced) < 2; t++) r.Step();
            Assert.AreEqual(2, r.Count(SimEventType.MinePlaced), "each walked out and laid his mine");

            Assert.AreEqual(1, r.Ground.OrderSappers(1, TW.Sim.Units.UnitAbilityId.LayTripwire), "theirs, through their seat");
            var wire = r.Enemy.All[r.Enemy.All.Count - 1];
            Assert.AreEqual(1, wire.Player);
            Assert.AreEqual((int)TW.Sim.Units.UnitAbilityId.LayTripwire, wire.B & 0xFF);
            AbilityArgs.Unpack(wire.B >> 8, out int heading, out _, out int length);
            Assert.AreEqual(90, heading); Assert.AreEqual(ProvingGround.TripwireMetres, length);
            Assert.AreEqual(r.W.Position[wire.A].z - ProvingGround.LayAheadMetres, wire.Pos.z, 1e-3f, "toward us");
        }

        [Test]
        public void OnlyAWeaponThatSetsBurningFiresAFlameShot()
        {
            using var r = new Rig();
            int flame = r.W.Spawn(0, InfantryArchetype.Flamethrower, r.W.Rally[0], 100f, 3f, false);
            int rifle = r.W.Spawn(0, InfantryArchetype.Rifle, r.W.Rally[0] + new float3(3f, 0f, 0f), 100f, 3f, false);
            var cat = r.M.Catalogue;
            Assert.IsTrue(TW.Presentation.Tactical.CombatFx.FlameShot(r.W, cat, new SimEvent { Type = SimEventType.Shot, A = flame, B = rifle }));
            Assert.IsFalse(TW.Presentation.Tactical.CombatFx.FlameShot(r.W, cat, new SimEvent { Type = SimEventType.Shot, A = rifle, B = flame }), "a rifle throws a round");
            Assert.IsFalse(TW.Presentation.Tactical.CombatFx.FlameShot(r.W, cat, new SimEvent { Type = SimEventType.Hit, A = flame, B = rifle }), "only the Shot");
            Assert.IsFalse(TW.Presentation.Tactical.CombatFx.FlameShot(r.W, cat, new SimEvent { Type = SimEventType.Shot, A = -1, B = rifle }));
            Assert.IsFalse(TW.Presentation.Tactical.CombatFx.FlameShot(r.W, null, new SimEvent { Type = SimEventType.Shot, A = flame, B = rifle }));
            Assert.Greater(TW.Presentation.Tactical.CombatFx.FlameShotSeconds, 1f / cat.Weapon[InfantryArchetype.Flamethrower].RoundsPerSecond,
                "one shot holds the stream open until the next, so a man who keeps firing keeps one stream");
        }

        [Test]
        public void ClearingRemovesOneSideOnly()
        {
            using var r = new Rig();
            r.Ground.Spawn(0, InfantryArchetype.Rifle, 6); r.Ground.Spawn(1, InfantryArchetype.Rifle, 4);
            Assert.AreEqual(4, r.Ground.Clear(1));
            Assert.AreEqual(6, r.Alive(0)); Assert.AreEqual(0, r.Alive(1));
        }

        [Test]
        public void TheRequestIsEndlessWithTheChosenTens()
        {
            var ours = new byte[] { 21, 22, 23, 24, 25, 26, 27, 28, 35, 36 };
            var theirs = ProvingGround.DefaultTen(FactionId.Brass);
            var q = ProvingGround.Request(Ground.WinterLine, 42, 0f, 5000, ours, theirs, 0);
            Assert.IsTrue(q.ProvingGround); Assert.IsTrue(q.Endless);
            Assert.AreEqual(ProvingGround.MissionId, q.MissionId);
            Assert.AreEqual(Ground.WinterLine, q.Ground); Assert.AreEqual(42u, q.BattlefieldSeed); Assert.AreEqual(5000, q.StartingSilver);
            Assert.AreEqual(ours, q.LoadoutA); Assert.AreEqual(theirs, q.LoadoutB);
            Assert.AreEqual(0u, q.AbilityMaskA, "every support ability is offered");
            Assert.IsFalse(q.ScriptedPeer, "AI off: only the tester's waves come");
            Assert.AreEqual(RosterEntry.SlotCount, theirs.Length);
            var hard = ProvingGround.Request(Ground.ShelledForest, 1, 8f, 300, ours, theirs, 3);
            Assert.IsTrue(hard.ScriptedPeer); Assert.IsTrue(hard.PeerDeploysTanks); Assert.AreEqual(28, hard.PeerDeployEveryTicks);
            Assert.AreEqual("HARD", hard.Difficulty);

            // the sim takes the ten as chosen
            var l = SimHost.Loadout(ours);
            Assert.AreEqual(10, l.Length);
            var cfg = SimConfig.Default; cfg.LoadoutA = l;
            using var m = MatchSim.CreatePlaytest(cfg);
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                Assert.AreEqual(ours[s], m.World.Roster[s].Archetype, $"slot {s}");
                Assert.AreEqual(1, m.World.SlotUnlocked[s], $"slot {s} is open");
            }
        }

        [Test]
        public void TheIdeasAreNamedAndNoneIsAUnitAlready()
        {
            var names = new HashSet<string>();
            foreach (var u in ProvingGround.Catalogue()) names.Add(u.Name.ToUpperInvariant());
            Assert.GreaterOrEqual(ProvingGround.Ideas.Length, 12);
            foreach (var i in ProvingGround.Ideas)
            {
                Assert.IsNotEmpty(i.Name); Assert.IsNotEmpty(i.Needs);
                Assert.IsTrue(names.Add(i.Name.ToUpperInvariant()), $"{i.Name} is listed as an idea and is a unit, or is listed twice");
            }
        }

        [Test]
        public void PlacingIsTheSameOnTwoWorlds()
        {
            ulong[] Play()
            {
                using var r = new Rig();
                var hashes = new ulong[300];
                var presets = ProvingGround.Presets();
                for (int t = 0; t < hashes.Length; t++)
                {
                    if (t == 5) r.Ground.Spawn(0, InfantryArchetype.DeathBattalion, 10);
                    if (t == 6) r.Ground.Spawn(0, VehicleArchetype.Mercy, 1);
                    if (t == 20) r.Ground.Send(presets.Find(w => w.Name == "SPECIALISTS"));
                    if (t == 40) r.Ground.Send(presets.Find(w => w.Name == "HISTORICAL ARMOUR"));
                    r.Step(); hashes[t] = r.W.LastHash;
                }
                Assert.Greater(r.Alive(1), 0);
                return hashes;
            }
            var one = Play(); var two = Play();
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"the worlds part at tick {t}");
        }
    }
}
