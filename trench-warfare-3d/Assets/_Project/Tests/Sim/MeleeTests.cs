// Phase: A3 (2026-09-28, lane/sim/melee) — hand to hand (MeleeSystem) and the crab's pounce (PounceSystem), the owner's
// decisions of 2026-09-28: men close in from 8 m and fight at 2.5 m, rifle or fists by unit type, crabs pounce.
// Each behaviour test runs long enough for the real cadence (blows every 1.2 s, the acquisition scan, the pounce's look
// every 5 ticks), not one tick: the character lane's lesson (tw3d-board lessons.md, troop-anims).
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class MeleeTests
    {
        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<int> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
                each?.Invoke(t);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a)) n++;
            return n;
        }

        /// <summary>Two men in the open on the greybox field, <paramref name="apart"/> metres across it; the first walks
        /// up the field towards the enemy trench, the second stands.</summary>
        static MatchSim Pair(byte archetype, byte foeArchetype, float apart, out int man, out int foe, float hp = 100f, float foeHp = 100f)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            man = w.Spawn(0, archetype, new float3(150f, 0f, 300f), hp, 3f, false);
            w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(1));
            w.Flags[man] |= (uint)UnitFlags.Exposed;
            foe = w.Spawn(1, foeArchetype, new float3(150f + apart, 0f, 300f), foeHp, 0.001f, false);
            return m;
        }

        // ---- the rules ---------------------------------------------------------------------------------------------

        [Test]
        public void RifleOrFists_GoesByUnitType()
        {
            // decisions.md 2026-09-28: riflemen and snipers keep the rifle; assault troops, officers, sappers and medics fight with their fists
            foreach (var a in new[] { InfantryArchetype.Rifle, InfantryArchetype.Sniper, InfantryArchetype.Machinegunner, InfantryArchetype.Para,
                                      InfantryArchetype.Frog, InfantryArchetype.DeathBattalion, InfantryArchetype.AtRifle, InfantryArchetype.Repair })
                Assert.IsTrue(MeleeSystem.KeepsRifle(a), $"archetype {a} keeps his rifle");
            foreach (var a in new[] { InfantryArchetype.Assault, InfantryArchetype.Officer, InfantryArchetype.Sapper, InfantryArchetype.Medic })
                Assert.IsFalse(MeleeSystem.KeepsRifle(a), $"archetype {a} fights with his fists");
            Assert.Greater(MeleeSystem.StabDamage, MeleeSystem.FistDamage, "a bayonet hurts more than a fist");
            Assert.Less(MeleeSystem.ContactRange, MeleeSystem.ChargeRange);
            Assert.Less(MeleeSystem.StandOff, MeleeSystem.ContactRange);
        }

        [Test]
        public void EveryAttackReach_IsCutByAFifth_AndTheRestKeepTheirs()
        {
            // decisions.md 2026-09-28: every attack reach at 0.8 of its design; claws, heal radii and leaps keep theirs
            Assert.AreEqual(0.8f, CombatTables.RangeScale);
            Assert.AreEqual(104f, CombatTables.WeaponFor(InfantryArchetype.Rifle).RangeMax, 1e-3f, "the rifle: 130 m designed");
            Assert.AreEqual(136f, CombatTables.WeaponFor(InfantryArchetype.Machinegunner).RangeMax, 1e-3f, "the machine gun: 170 m");
            Assert.AreEqual(184f, CombatTables.WeaponFor(InfantryArchetype.Sniper).RangeMax, 1e-3f, "the sniper: 230 m");
            Assert.AreEqual(48f, CombatTables.AdvanceFireRange, 1e-3f, "under >> men engage from 48 m, not 60");
            Assert.AreEqual(56f, EngageSystem.HuntRadius, 1e-3f, "and go after men within 56 m, not 70");
            Assert.AreEqual(288f, TankSpec.Pavise.Gun0.RangeMax, 1e-3f, "the Pavise's long gun: 360 m");
            Assert.AreEqual(46f * 0.8f, TankSpec.Kettle.Gun0.RangeMin, 1e-3f, "the Kettle's minimum range shrinks with it");
            Assert.AreEqual(3.4f, TankSpec.Pincer.ClawReach, 1e-6f, "claws keep their reach");
            Assert.AreEqual(28f, InfantrySpec.For(InfantryArchetype.Jetpack).JumpRange, 1e-6f, "and the jetpack its leap");
        }

        // ---- hand to hand ------------------------------------------------------------------------------------------

        [Test]
        public void AManWithinEightMetres_ChargesInsteadOfShooting_AndItComesToBlows()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Rifle, 7f, out int man, out int foe, hp: 1000f);
            var w = m.World;
            bool charged = false, fought = false, shotWhileCharging = false, wasMelee = false;
            var log = Run(m, 400, t =>
            {
                if (!w.IsAlive(man)) return;
                // (DirectFire steps before MeleeSystem: on the tick he first comes within 8 m he may still fire; from the
                // next tick the Melee flag holds his fire)
                var ev = w.Events.Events;
                for (int k = 0; k < ev.Length; k++)
                    if (ev[k].Type == SimEventType.Shot && ev[k].A == man && wasMelee) shotWhileCharging = true;
                if (m.Movement.Engage[man] == MovementSystem.EngageClose && (w.Flags[man] & (uint)UnitFlags.Melee) != 0) charged = true;
                if (w.StanceOf[man] == (byte)Stance.Melee) fought = true;
                wasMelee = (w.Flags[man] & (uint)UnitFlags.Melee) != 0;
            });
            Assert.IsTrue(charged, "he ran at him (EngageClose with the Melee flag)");
            Assert.IsTrue(fought, "and fought him at arm's length (Stance.Melee)");
            Assert.IsFalse(shotWhileCharging, "he did not stop to shoot on the way in");
            Assert.Greater(Count(log, SimEventType.MeleeBlow, man), 0, "blows were struck");
            Assert.IsFalse(w.IsAlive(foe), "a man with ten times the health won the fight");
            bool killedByHim = false;
            foreach (var e in log) if (e.Type == SimEventType.Death && e.A == foe && e.B == man) killedByHim = true;
            Assert.IsTrue(killedByHim, "his death names his killer");
            foreach (var e in log)
                if (e.Type == SimEventType.MeleeBlow && e.A == man)
                    Assert.AreNotEqual((float)MeleeSystem.StyleFists, e.Dir.y, "a rifleman strikes with his rifle");
        }

        [Test]
        public void FartherThanEightMetres_HeHoldsAndShoots_AsBefore()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 12f, out int man, out int foe, foeHp: 100000f);
            var w = m.World;
            var log = Run(m, 200, t => Assert.AreEqual(0u, w.Flags[man] & (uint)UnitFlags.Melee, $"tick {t}: no melee at 12 m"));
            Assert.AreEqual(0, Count(log, SimEventType.MeleeBlow), "nobody came to blows");
            Assert.Greater(Count(log, SimEventType.Shot, man), 0, "he shot at him");
        }

        [Test]
        public void AFistsMan_ThrowsHisWeaponDown_FightsWithHisFists_AndPicksItUpAfter()
        {
            using var m = Pair(InfantryArchetype.Assault, InfantryArchetype.Rifle, 5f, out int man, out int foe, hp: 2000f);
            var w = m.World;
            bool disarmedInFight = false;
            var log = Run(m, 600, t =>
            {
                if (m.Melee.Contact[man] != 0 && (w.Flags[man] & (uint)UnitFlags.Disarmed) != 0) disarmedInFight = true;
            });
            Assert.AreEqual(1, Count(log, SimEventType.WeaponDropped, man), "he threw his weapon down once");
            Assert.IsTrue(disarmedInFight, "and fought disarmed");
            int blows = 0;
            foreach (var e in log)
                if (e.Type == SimEventType.MeleeBlow && e.A == man) { blows++; Assert.AreEqual((float)MeleeSystem.StyleFists, e.Dir.y, "with his fists"); }
            Assert.Greater(blows, 0);
            Assert.IsFalse(w.IsAlive(foe), "and won");
            Assert.AreEqual(1, Count(log, SimEventType.WeaponPickedUp, man), "he picked his weapon up again");
            Assert.AreEqual(0u, w.Flags[man] & (uint)(UnitFlags.Disarmed | UnitFlags.Melee), "armed and out of the fight");
            int dropped = -1, picked = -1, died = -1;
            for (int k = 0; k < log.Count; k++)
            {
                if (log[k].Type == SimEventType.WeaponDropped && log[k].A == man) dropped = k;
                if (log[k].Type == SimEventType.WeaponPickedUp && log[k].A == man) picked = k;
                if (log[k].Type == SimEventType.Death && log[k].A == foe) died = k;
            }
            Assert.Less(dropped, died); Assert.Less(died, picked, "dropped, the fight, then picked up");
            Assert.GreaterOrEqual(log[picked].Tick - log[died].Tick, (uint)MeleeSystem.PickUpTicks, "not before the fight had been over a while");
        }

        [Test]
        public void AMedic_DoesNotCharge_ButFightsBackWhenStruck()
        {
            using var m = Pair(InfantryArchetype.Medic, InfantryArchetype.Rifle, 6f, out int medic, out int rifle, hp: 400f);
            var w = m.World;
            // the rifleman stands still (speed ~0): the medic walks past him and nobody runs at anybody
            bool medicCharged = false;
            var log = Run(m, 160, t => { if (m.Movement.Engage[medic] == MovementSystem.EngageClose) medicCharged = true; });
            Assert.IsFalse(medicCharged, "a medic runs at nobody");
            // now the rifleman comes for him
            using var m2 = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 6f, out int r2, out int med2, hp: 5000f, foeHp: 400f);
            var log2 = Run(m2, 600);
            Assert.Greater(Count(log2, SimEventType.MeleeBlow, med2), 0, "struck, the medic hits back");
        }

        [Test]
        public void AFlamethrower_FlamesInsteadOfCharging()
        {
            // his 9.6 m cone is his weapon: charging at 8 m he would never use it (the agent's choice, applying the owner's
            // rule to a weapon that reaches little further than the charge; MeleeSystem.Charges)
            using var m = Pair(InfantryArchetype.Flamethrower, InfantryArchetype.Medic, 7f, out int man, out int foe, foeHp: 100000f);
            var w = m.World;
            var log = Run(m, 120, t => Assert.AreEqual(0u, w.Flags[man] & (uint)UnitFlags.Melee, $"tick {t}: he charged"));
            Assert.Greater(Count(log, SimEventType.Shot, man), 0, "he used his flame");
        }

        [Test]
        public void TheSameFight_RunsTheSameTwice()
        {
            ulong Run1()
            {
                using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Assault, 6f, out int a, out int b);
                for (int t = 0; t < 400; t++) Step(m);
                return m.World.Hash();
            }
            Assert.AreEqual(Run1(), Run1());
        }

        // ---- the pounce ----------------------------------------------------------------------------------------------

        static MatchSim Playtest()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = 0xC0FFEE;
            return MatchSim.CreatePlaytest(cfg);
        }

        static int Crab(MatchSim m, byte team, byte archetype, float3 at, bool moving = false)
        {
            var entry = archetype == VehicleArchetype.Pincer ? RosterEntry.Pincer : RosterEntry.Kettle;
            int slot = m.World.Spawn(team, archetype, at, entry.Hp, moving ? entry.Speed : 0f, true);
            var c = m.Map.NavCellOf(at);
            int goalZ = team == 0 ? m.Map.NavLength - 6 : 5;   // straight up (team 0) its own column: it faces +Z
            m.World.GoalId[slot] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, goalZ), NavMode.Tracked));
            return slot;
        }

        [Test]
        public void WhatPounces_IsAWalkerWithClaws()
        {
            foreach (var a in new[] { VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt })
                Assert.IsTrue(PounceSystem.Pounces(VehicleProfile.ForArchetype(a), TankSpec.For(a)), $"crab {a}");
            foreach (var a in new[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker })
                Assert.IsFalse(PounceSystem.Pounces(VehicleProfile.ForArchetype(a), TankSpec.For(a)), $"machine {a} does not pounce");
        }

        [Test]
        public void ACrab_CrouchesLeapsAndLandsOnAManInFront()
        {
            using var m = Playtest();
            var w = m.World;
            // it walks (its own speed, up the map towards the man): the crouch must hold it against its drive
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f), moving: true);
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            float3 manAt = new float3(30f, 0f, 30f + front + 9f);
            int man = w.Spawn(1, InfantryArchetype.Rifle, manAt, 100f, 0f, false);
            float3 start = default;
            bool crouchedStill = true;
            int crouchTick = -1;
            var log = Run(m, 120, t =>
            {
                if (m.Pounce.Phase[crab] == PounceSystem.Crouched)
                {
                    if (crouchTick < 0) { crouchTick = t; start = w.Position[crab]; }
                    if (math.distance(w.Position[crab].xz, start.xz) > 0.05f) crouchedStill = false;
                }
            });
            Assert.AreEqual(1, Count(log, SimEventType.PounceCrouched, crab), "it crouched to leap once");
            Assert.IsTrue(crouchedStill, "and held still while it crouched");
            Assert.AreEqual(1, Count(log, SimEventType.PounceLanded, crab), "it landed");
            SimEvent crouch = default, land = default;
            foreach (var e in log) { if (e.Type == SimEventType.PounceCrouched) crouch = e; if (e.Type == SimEventType.PounceLanded) land = e; }
            Assert.AreEqual(man, crouch.B, "at the man");
            Assert.AreEqual((uint)(PounceSystem.CrouchTicks + PounceSystem.AirTicks), land.Tick - crouch.Tick, "0.6 s crouched, 0.5 s in the air");
            Assert.IsFalse(w.IsAlive(man), "the man under it is dead");
            bool byCrab = false;
            foreach (var e in log) if (e.Type == SimEventType.Death && e.A == man && e.B == crab) byCrab = true;
            Assert.IsTrue(byCrab, "killed by the crab");
            // of its weight as it comes down, not taken by its claws while it is still in the air (TankGunnery clawed
            // him four ticks before the landing, which then hit nobody; review, 2026-10-06)
            uint died = 0;
            bool landedOn = false;
            foreach (var e in log)
            {
                if (e.Type == SimEventType.Death && e.A == man) died = e.Tick;
                if (e.Type == SimEventType.Hit && e.A == crab && e.B == man && e.Tick == land.Tick) landedOn = true;
            }
            Assert.AreEqual(land.Tick, died, "he dies when it comes down, not before");
            Assert.IsTrue(landedOn, "under the landing's weight");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleClawed, crab), "and its claws stayed shut on the way over");
            Assert.Greater(m.Pounce.Cooldown[crab], 0, "and it waits before the next");
            Assert.Less(math.distance(land.Pos.xz, manAt.xz), 0.5f, "it came down where he stood");
            Assert.AreEqual(0u, w.Flags[crab] & (uint)UnitFlags.Pouncing, "and is no longer pouncing");
        }

        [Test]
        public void ACrab_DoesNotPounceBehindItself_OrPastTenMetres()
        {
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            w.Spawn(1, InfantryArchetype.Rifle, new float3(30f, 0f, 30f - front - 6f), 100f, 0f, false);           // behind it
            w.Spawn(1, InfantryArchetype.Rifle, new float3(30f, 0f, 30f + front + PounceSystem.Range + 4f), 100f, 0f, false);   // too far
            var log = Run(m, 100);
            Assert.AreEqual(0, Count(log, SimEventType.PounceCrouched, crab));
        }

        [Test]
        public void ACrab_DoesNotLandOnItsOwnMen()
        {
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            float3 at = new float3(30f, 0f, 30f + front + 7f);
            w.Spawn(1, InfantryArchetype.Rifle, at, 100f, 0f, false);
            int friend = w.Spawn(0, InfantryArchetype.Rifle, at + new float3(1.5f, 0f, 0f), 100f, 0f, false);   // its own man beside him
            var log = Run(m, 60);
            Assert.AreEqual(0, Count(log, SimEventType.PounceCrouched, crab), "it does not come down on its own man");
        }

        [Test]
        public void MenInOneTrench_CloseOnEachOtherAlongIt_AndFight()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World; var map = m.Map;
            // a straight stretch of one trench, 6 m along x, every cell of it that trench (not a ladder)
            int ax = -1, az = -1, run = 3; short id = -1;
            for (int z = 0; z < map.NavLength && ax < 0; z++)
                for (int x = 0; x + run < map.NavWidth && ax < 0; x++)
                {
                    short t = map.CellTrenchId[z * map.NavWidth + x];
                    if (t < 0) continue;
                    bool ok = true;
                    for (int k = 0; k <= run && ok; k++)
                    {
                        int c = z * map.NavWidth + x + k;
                        ok = map.CellTrenchId[c] == t && (map.NavLayers[c] & (byte)TW.Sim.Terrain.NavLayer.Link) == 0;
                    }
                    if (ok) { ax = x; az = z; id = t; }
                }
            Assert.GreaterOrEqual(ax, 0, "setup: a straight trench stretch");
            float cs = TW.Sim.Terrain.MapData.NavCellSize;
            float3 pa = new float3((ax + 0.5f) * cs, 0f, (az + 0.5f) * cs), pb = pa + new float3(run * cs, 0f, 0f);
            int a = w.Spawn(0, InfantryArchetype.Rifle, pa, 2000f, 3f, false);
            int b = w.Spawn(1, InfantryArchetype.Rifle, pb, 100f, 3f, false);
            bool charged = false;
            var log = Run(m, 400, t => { if (w.IsAlive(a) && m.Movement.Engage[a] == MovementSystem.EngageClose && (w.Flags[a] & (uint)UnitFlags.Melee) != 0) charged = true; });
            Assert.IsTrue(charged, $"he ran at the man {run * cs} m along trench {id}");
            Assert.Greater(Count(log, SimEventType.MeleeBlow), 0, "and it came to blows in the trench");
        }

        [Test]
        public void TheWinner_LeavesTheFight_AndTheSlotsNextTenantStartsArmed()
        {
            using var m = Pair(InfantryArchetype.Assault, InfantryArchetype.Rifle, 5f, out int man, out int foe, hp: 20f, foeHp: 2000f);
            var w = m.World;
            Run(m, 600);
            Assert.IsFalse(w.IsAlive(man), "setup: the weak man lost");
            Assert.AreEqual(0u, w.Flags[foe] & (uint)UnitFlags.Melee, "the winner is out of the fight");
            Assert.AreEqual(-1, m.Melee.Foe[foe]); Assert.AreEqual(0, m.Melee.Contact[foe]);
            // (a dead man's flags are zeroed by Despawn whatever MeleeSystem does: what matters is the next man in his slot)
            int next = w.Spawn(0, InfantryArchetype.Assault, new float3(40f, 0f, 60f), 100f, 3f, false);
            Assert.AreEqual(man, next, "setup: the slot is used again");
            Run(m, 2);
            Assert.AreEqual(0, m.Melee.Dropped[next], "he comes with his weapon in his hands");
            Assert.AreEqual(0u, w.Flags[next] & (uint)(UnitFlags.Melee | UnitFlags.Disarmed));
        }

        [Test]
        public void AChargingMan_StillThrowsHisBundleAtAMachineBesideHim()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            int man = w.Spawn(0, InfantryArchetype.Rifle, new float3(150f, 0f, 300f), 5000f, 0.001f, false);
            w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(1));   // on his way to the enemy (going home he would not charge)
            w.Spawn(1, InfantryArchetype.Medic, new float3(156f, 0f, 300f), 1e6f, 0.001f, false);   // a man to charge
            int tank = w.Spawn(1, VehicleArchetype.Tusk, new float3(150f, 0f, 305f), RosterEntry.Tusk.Hp, 0f, true);   // and a machine within 8 m
            bool bundle = false; int charging = 0;
            // a bundle thrown while the Melee flag was up since the tick before (DirectFire steps before MeleeSystem)
            bool flagged = false;
            Run(m, 300, t =>
            {
                var ev = w.Events.Events;
                for (int k = 0; k < ev.Length; k++) if (ev[k].Type == SimEventType.Shot && ev[k].A == man && ev[k].B == tank && ev[k].Scalar == 1f && flagged) bundle = true;
                flagged = (w.Flags[man] & (uint)UnitFlags.Melee) != 0;
                if (flagged) charging++;
            });
            Assert.Greater(charging, 60, "setup: he was in the fight with the man beside the tank");
            Assert.IsTrue(bundle, "a grenade bundle at the tank, charge or no charge");
        }

        [Test]
        public void ADisarmedFistsMan_StillBundlesAMachineBesideHim()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            int man = w.Spawn(0, InfantryArchetype.Assault, new float3(150f, 0f, 300f), 5000f, 0.001f, false);
            w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(1));
            w.Spawn(1, InfantryArchetype.Medic, new float3(152f, 0f, 300f), 1e6f, 0.001f, false);   // at arm's length, nearer than the tank
            int tank = w.Spawn(1, VehicleArchetype.Tusk, new float3(150f, 0f, 305f), RosterEntry.Tusk.Hp, 0f, true);
            bool bundle = false; int disarmed = 0;
            Run(m, 300, t =>
            {
                bool now = (w.Flags[man] & (uint)UnitFlags.Disarmed) != 0;
                if (now) disarmed++;
                var ev = w.Events.Events;
                for (int k = 0; k < ev.Length; k++)
                    if (now && ev[k].Type == SimEventType.Shot && ev[k].A == man && ev[k].B == tank) bundle = true;
            });
            Assert.Greater(disarmed, 60, "setup: he threw his weapon down");
            Assert.IsTrue(bundle, "his weapon on the ground, he still throws a bundle at the tank");
        }

        [Test]
        public void MenOrderedBack_DoNotCharge()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 6f, out int man, out int foe, foeHp: 1e6f);
            var w = m.World;
            short own = m.Fields.RearTrench(0);
            w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(own));   // back to a trench his side holds
            Run(m, 80, t => Assert.AreNotEqual(MovementSystem.EngageClose, (w.Flags[man] & (uint)UnitFlags.Melee) != 0 ? m.Movement.Engage[man] : (byte)255, $"tick {t}: he charged on his way back"));
        }

        [Test]
        public void MeleeKills_CountForTheSide()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 2f, out int man, out int foe, hp: 1e6f, foeHp: 60f);
            int before = m.Fire.Kills[0];
            Run(m, 400);
            Assert.IsFalse(m.World.IsAlive(foe), "setup: he won");
            Assert.AreEqual(before + 1, m.Fire.Kills[0], "the bayonet kill counts");
            Assert.AreEqual(1, m.Fire.KillsWithoutShot[0], "and is not a round that hit (a hit rate over 100 %)");
        }

        [Test]
        public void AHeroWhoFallsRightAfterABayonetKill_KeepsItInHisTally()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 2f, out int man, out int foe, hp: 1e6f, foeHp: 60f);
            var w = m.World;
            Run(m, 1);   // HeroSystem has seen the slot (a fresh slot's hero state is cleared)
            m.Hero.HeroTicks[man] = 400; m.Hero.HeroId[man] = 99; m.Hero.HeroGen[man] = w.Generation[man];
            bool killed = false;
            for (int t = 0; t < 400 && !killed; t++)
            {
                Step(m);
                for (int k = 0; k < m.Melee.Killed.Length; k++) if (m.Melee.Killed[k].y == man) killed = true;
            }
            Assert.IsTrue(killed, "setup: the bayonet killed");
            w.Despawn(man, -1);   // and he falls before the next tick's HeroSystem
            var log = Run(m, 1);
            float kills = -1f; bool feat = false;
            foreach (var e in log)
            {
                if (e.Type == SimEventType.HeroFallen && e.A == man) kills = e.Scalar;
                if (e.Type == SimEventType.HeroFeat && e.A == man) feat = true;
            }
            Assert.AreEqual(1f, kills, "his last kill is in his tally");
            Assert.IsFalse(feat, "no feat announced for a dead man");
        }

        [Test]
        public void AHeroWhoseSlotIsTakenRightAfterABayonetKill_StillFallsWithIt()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 2f, out int man, out int foe, hp: 1e6f, foeHp: 60f);
            var w = m.World;
            Run(m, 1);
            m.Hero.HeroTicks[man] = 400; m.Hero.HeroId[man] = 99; m.Hero.HeroGen[man] = w.Generation[man];
            bool killed = false;
            for (int t = 0; t < 400 && !killed; t++)
            {
                Step(m);
                for (int k = 0; k < m.Melee.Killed.Length; k++) if (m.Melee.Killed[k].y == man) killed = true;
            }
            Assert.IsTrue(killed, "setup: the bayonet killed");
            float3 stood = w.Position[man];
            w.Despawn(man, -1);
            int next = w.Spawn(0, InfantryArchetype.Rifle, new float3(40f, 0f, 60f), 100f, 3f, false);   // a deploy takes his slot first
            Assert.AreEqual(man, next, "setup: the slot is used again before the next step");
            var log = Run(m, 1);
            var fallen = log.FindAll(e => e.Type == SimEventType.HeroFallen && e.A == man);
            Assert.AreEqual(1, fallen.Count, "he falls, once");
            Assert.AreEqual(1f, fallen[0].Scalar, "with his last kill");
            Assert.Less(math.distance(fallen[0].Pos, stood), 0.5f, "where he stood (at the last hero step), not where the next man came in");
            Assert.AreEqual(0, m.Hero.HeroTicks[next], "and the next man is no hero");
        }

        [Test]
        public void AManBrawlingInHisTrench_IsNotSeenFromAcrossTheField()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            short t = m.Fields.FrontTrench(1);
            float z = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[t].CellStart + m.Map.Trenches[t].CellCount / 2]).z;
            int d = w.Spawn(1, InfantryArchetype.Rifle, new float3(120f, 0f, z + 6f), 1e6f, 3f, false);
            w.GoalId[d] = m.Fields.GetGoal(GoalKey.Trench(t));
            for (int k = 0; k < 600 && w.TrenchId[d] < 0; k++) Step(m);
            Assert.GreaterOrEqual(w.TrenchId[d], 0, "setup: he got into his trench");
            Run(m, 40);
            float3 at = w.Position[d];
            w.Spawn(0, InfantryArchetype.Rifle, at + new float3(1.5f, 0f, 0f), 1e6f, 0f, false);   // a raider in the trench beside him
            int far = w.Spawn(0, InfantryArchetype.Rifle, at - new float3(0f, 0f, 50f), 1e6f, 0f, false);   // a rifleman 50 m out
            int brawling = 0;
            Run(m, 120, k =>
            {
                if (w.StanceOf[d] != (byte)Stance.Melee) return;
                brawling++;
                Assert.AreNotEqual(d, w.TargetSlot[far], $"tick {k}: the man 50 m out had the brawler below the rim as his target");
            });
            Assert.Greater(brawling, 40, "setup: he fought in his trench");
        }

        [Test]
        public void TheDeathBattalion_HitsHarderWithTheButt()
        {
            using var m = Pair(InfantryArchetype.DeathBattalion, InfantryArchetype.Medic, 2f, out int man, out int foe, hp: 1e6f, foeHp: 1e6f);
            float want = MeleeSystem.WeaponMul(m.Catalogue.Weapon[InfantryArchetype.DeathBattalion].Damage, m.Catalogue.Weapon[InfantryArchetype.Rifle].Damage);
            Assert.Greater(want, 1.05f, "setup: his rifle hits harder than the rifleman's");
            var log = Run(m, 400);
            int landed = 0;
            foreach (var e in log)
            {
                if (e.Type != SimEventType.MeleeBlow || e.A != man || e.Scalar <= 0f) continue;
                landed++;
                Assert.AreEqual(MeleeSystem.DamageOf((byte)e.Dir.y) * want, e.Scalar, 1e-3f, "the blow carries his rifle's weight");
            }
            Assert.Greater(landed, 3, "setup: blows landed");
        }

        [Test]
        public void AManStabbedFromBehind_TurnsOnTheManWhoStabbedHim()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            // A (team 0) and B (team 1) fight; C (team 1) comes up behind A. A must turn on C when C strikes him, B being
            // at A already (B's foe is A, so the rule's "his foe is busy with another" is false): give B a second foe D
            int a = w.Spawn(0, InfantryArchetype.Rifle, new float3(150f, 0f, 300f), 1e6f, 0.001f, false);
            int b = w.Spawn(1, InfantryArchetype.Rifle, new float3(151.8f, 0f, 300f), 1e6f, 0.001f, false);
            int d = w.Spawn(0, InfantryArchetype.Rifle, new float3(153.4f, 0f, 300f), 1e6f, 0.001f, false);   // B's other foe, on B's far side
            int c = w.Spawn(1, InfantryArchetype.Rifle, new float3(148.0f, 0f, 300f), 1e6f, 0.001f, false);   // behind A, farther than B: B is A's first foe
            bool turned = false;
            Step(m);
            Assert.AreEqual(b, m.Melee.Foe[a], "setup: A is at B first");
            Assert.AreEqual(d, m.Melee.Foe[b], "setup: B is at D");
            Run(m, 300, t => { if (m.Melee.Foe[a] == c && m.Melee.Contact[a] != 0) turned = true; });
            Assert.IsTrue(turned, "A turned on the man at his back");
        }

        [Test]
        public void AManDownUnderFire_CrawlsOnInsteadOfCharging()
        {
            using var m = Pair(InfantryArchetype.Rifle, InfantryArchetype.Medic, 6f, out int man, out int foe, foeHp: 1e6f);
            var w = m.World;
            w.Suppression[man] = SuppressionRules.ProneThreshold + 5f;
            Run(m, 60, t =>
            {
                w.Suppression[man] = SuppressionRules.ProneThreshold + 5f;
                if (m.Movement.Engage[man] == MovementSystem.EngageClose && (w.Flags[man] & (uint)UnitFlags.Melee) != 0) Assert.Fail($"tick {t}: a man pressed flat charged");
            });
        }

        [Test]
        public void ACrabWoundUpToPounce_HoldsItsGuns()
        {
            // its sponson gun shelled the man it had crouched to leap on, and it landed on nobody (seen in Play, 2026-10-01)
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            Run(m, 80);   // its guns are loaded and laid ahead
            int man = w.Spawn(1, InfantryArchetype.Rifle, new float3(30f, 0f, 30f + front + 7f), 100f, 0f, false);
            var log = Run(m, 60);
            uint crouched = 0, landed = 0;
            foreach (var e in log)
            {
                if (e.Type == SimEventType.PounceCrouched && e.A == crab && crouched == 0) crouched = e.Tick;
                if (e.Type == SimEventType.PounceLanded && e.A == crab && landed == 0) landed = e.Tick;
            }
            Assert.Greater(crouched, 0u, "setup: it crouched to leap");
            Assert.Greater(landed, crouched, "and it landed");
            foreach (var e in log)
                if (e.Type == SimEventType.VehicleFired && e.A == crab && e.Tick >= crouched && e.Tick <= landed)
                    Assert.Fail($"it fired a gun at tick {e.Tick}, wound up to leap ({crouched}..{landed})");
            bool underIt = false;
            foreach (var e in log) if (e.Type == SimEventType.Death && e.A == man && e.B == crab) underIt = true;
            Assert.IsTrue(underIt, "the man was there to be landed on");
        }

        [Test]
        public void AStalledCrab_DoesNotPounce()
        {
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            w.Spawn(1, InfantryArchetype.Rifle, new float3(30f, 0f, 30f + front + 7f), 100f, 0f, false);
            // its engine shot out: VehicleModulesSystem rewrites Stalled from it every tick
            Run(m, 1);
            m.Modules.Module[crab * (int)VehicleModule.Count + (int)VehicleModule.Engine] = 0.05f;
            var log = Run(m, 60);
            Assert.AreNotEqual(0u, w.Flags[crab] & (uint)UnitFlags.Stalled, "setup: it is stalled");
            Assert.AreEqual(0, Count(log, SimEventType.PounceCrouched, crab), "a stalled machine does not leap");
        }

        [Test]
        public void APouncingCrab_SetsOffNoMineUnderItsLeap()
        {
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            float3 manAt = new float3(30f, 0f, 30f + front + 9f);
            // an enemy mine half way along its leap, where no foot and no track ever touches the ground; armed first
            // (MineSystem.ArmTicks), then the man it leaps at
            m.Mines.Place(w, (new float3(30f, 0f, 30f) + manAt) * 0.5f, new float3(1f, 0f, 0f), 0f, 1, TW.Sim.Combat.MineKind.Mine);
            Run(m, TW.Sim.Combat.MineSystem.ArmTicks + 5);
            w.Spawn(1, InfantryArchetype.Rifle, manAt, 100f, 0f, false);
            var log = Run(m, 40);
            Assert.AreEqual(1, Count(log, SimEventType.PounceLanded, crab), "setup: it leapt");
            Assert.AreEqual(0, Count(log, SimEventType.MineTriggered), "nothing went off under it in the air");
        }

        [Test]
        public void ACrab_DoesNotLandOnAnotherMachine()
        {
            using var m = Playtest();
            var w = m.World;
            int crab = Crab(m, 0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f));
            float front = VehicleProfile.ForArchetype(VehicleArchetype.Pincer).HalfLength;
            float3 at = new float3(30f, 0f, 30f + front + 7f);
            w.Spawn(1, InfantryArchetype.Rifle, at, 100f, 0f, false);
            w.Spawn(0, VehicleArchetype.Tusk, at + new float3(2f, 0f, 0f), RosterEntry.Tusk.Hp, 0f, true);   // its own tank beside the man
            var log = Run(m, 60);
            Assert.AreEqual(0, Count(log, SimEventType.PounceCrouched, crab), "no room to come down");
        }
    }
}
