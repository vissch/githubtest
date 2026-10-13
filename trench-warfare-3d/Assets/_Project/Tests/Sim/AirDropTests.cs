// Phase: A3 (implemented 2026-09-25) — paratroopers: eight men come down where the drop was called, a flight later,
// on open ground, and go for the nearest enemy trench; the silver is spent and the cooldown runs; a drop on water,
// in a trench, too near the enemy's rear line or by a faction without the ability is refused; and it is the same
// on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class AirDropTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE, byte factionA = (byte)FactionId.Brass, byte factionB = (byte)FactionId.Iron)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed; cfg.FactionA = factionA; cfg.FactionB = factionB;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        static SimCommand Drop(MatchSim m, byte player, float3 at)
            => new SimCommand { Tick = m.World.Tick, Player = player, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ParaDrop, Pos = at };

        /// <summary>Open ground on the greybox between the two lines, a safe way short of the enemy rear.</summary>
        static float3 OpenPoint(MatchSim m, byte player)
        {
            short rear = m.Fields.RearTrench((byte)(1 - player));
            var def = m.Map.Trenches[rear];
            float rearZ = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
            float z = player == 0 ? rearZ - OffMapAbilitySystem.ParaDropKeepOut - 20f : rearZ + OffMapAbilitySystem.ParaDropKeepOut + 20f;
            for (float x = 40f; x < m.Map.SizeMeters.x - 40f; x += 4f)
            {
                var at = new float3(x, 0f, z);
                bool open = true;
                for (int dz = -8; dz <= 8 && open; dz += 2) for (int dx = -8; dx <= 8; dx += 2)
                    if ((m.Map.LayerAt(at + new float3(dx, 0f, dz)) & (NavLayer.Trench | NavLayer.Blocked | NavLayer.Link)) != 0) { open = false; break; }
                if (open) return at;
            }
            Assert.Fail("setup: no open ground for a drop"); return default;
        }

        /// <summary>Mid-cell Z of a trench.</summary>
        static float MidZ(MatchSim m, short trench)
        {
            var def = m.Map.Trenches[trench];
            return m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
        }

        /// <summary>Open ground for a stick inside a band of Z, walking inland from <paramref name="zFar"/>: the
        /// first point with 4 m clear of trench, link and blocked ground round it. A band, not one line of Z,
        /// because the trenches bend and the band this test needs is only a few metres deep.</summary>
        static float3 OpenInBand(MatchSim m, float zFar, float zNear)
        {
            for (float z = zFar; z > zNear; z -= 1f)
            for (float x = 20f; x < m.Map.SizeMeters.x - 20f; x += 2f)
            {
                var at = new float3(x, 0f, z);
                bool open = true;
                for (int dz = -4; dz <= 4 && open; dz += 2) for (int dx = -4; dx <= 4; dx += 2)
                    if ((m.Map.LayerAt(at + new float3(dx, 0f, dz)) & (NavLayer.Trench | NavLayer.Blocked | NavLayer.Link)) != 0) { open = false; break; }
                if (open) return at;
            }
            Assert.Fail("setup: no open ground for a drop between z " + zNear.ToString("F0") + " and " + zFar.ToString("F0"));
            return default;
        }

        [Test]
        public void EightMenComeDownAFlightLaterOnOpenGroundAndGoForTheEnemyTrench()
        {
            using var m = NewMatch();
            var at = OpenPoint(m, 0);
            int silver = m.World.Silver[0];
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            Step(m, Drop(m, 0, at));
            Assert.AreEqual(silver - stats.Cost, m.World.Silver[0], "paid at once");
            Assert.AreEqual(1, m.World.Events.Events.Length > 0 ? CountType(m, SimEventType.DropInbound) : 0);
            var log = Run(m, stats.WarmupTicks - 2);
            Assert.AreEqual(0, Count(log, SimEventType.DropLanded), "nobody before the flight is over");
            log = Run(m, 3);
            Assert.AreEqual(stats.Men, Count(log, SimEventType.DropLanded, 0));
            Assert.AreEqual(stats.Men, Count(log, SimEventType.UnitSpawned, InfantryArchetype.Para));
            int goal = -1;
            foreach (var e in log)
            {
                if (e.Type != SimEventType.DropLanded) continue;
                int s = e.A;
                Assert.Less(math.distance(m.World.Position[s].xz, at.xz), stats.Radius + 2.5f, "inside the disc, or the open cell next to it");
                Assert.AreEqual(0, (int)(m.Map.LayerAt(m.World.Position[s]) & (NavLayer.Trench | NavLayer.Blocked)), "on open ground");
                Assert.AreNotEqual(0u, m.World.Flags[s] & (uint)UnitFlags.Exposed);
                Assert.GreaterOrEqual(m.World.GoalId[s], 0, "he has somewhere to go");
                goal = m.World.GoalId[s];
            }
            var key = m.Fields.Goals[goal];
            Assert.AreEqual(GoalKind.Trench, key.Kind);
            Assert.AreNotEqual(0, m.Fields.Trenches[key.Ref].OwnerTeam, "an enemy trench");
            Assert.Greater(m.Abilities.CooldownOf(0, OffMapAbilityId.ParaDrop), 0);
        }

        static int CountType(MatchSim m, SimEventType t)
        {
            int n = 0; var ev = m.World.Events.Events;
            for (int k = 0; k < ev.Length; k++) if (ev[k].Type == t) n++;
            return n;
        }

        [Test]
        public void ADropIsRefusedWhereItCannotLandOrByAFactionWithoutIt()
        {
            using var m = NewMatch();
            int silver = m.World.Silver[0];
            // in a trench
            short rear = m.Fields.RearTrench(1);
            var def = m.Map.Trenches[rear];
            float3 inTrench = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]);
            Step(m, Drop(m, 0, inTrench));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected), "not into a trench");
            // too near the enemy rear
            Step(m, Drop(m, 0, inTrench + new float3(0f, 0f, -OffMapAbilitySystem.ParaDropKeepOut * 0.5f)));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected), "not on the enemy's doorstep");
            // off the map
            Step(m, Drop(m, 0, new float3(-5f, 0f, 50f)));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected));
            Assert.AreEqual(silver, m.World.Silver[0], "nothing was paid for");
            // Iron has no aircraft
            using var iron = NewMatch(0xC0FFEE, (byte)FactionId.Iron, (byte)FactionId.Brass);
            var at = OpenPoint(iron, 0);
            Step(iron, Drop(iron, 0, at));
            Assert.AreEqual(1, CountType(iron, SimEventType.CommandRejected), "Iron cannot call a drop");
            Step(iron, Drop(iron, 1, OpenPoint(iron, 1)));
            Assert.AreEqual(0, CountType(iron, SimEventType.CommandRejected), "Brass can");
        }

        /// <summary>
        /// [C3] On the defended coast the keep-out is measured from the rearmost line team 1 HOLDS, not from his
        /// rearmost trench. The shore trench sits at the top of the beach, well seaward of his reserve line and is
        /// no objective, so measuring from it let team 0 drop a stick down behind the reserve line - the very thing
        /// the keep-out exists to stop.
        /// </summary>
        [Test]
        public void ADropIsRefusedBehindTheDefendedCoastsRearLine()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = 0xC0FFEEu;
            cfg.FactionA = (byte)FactionId.Brass; cfg.FactionB = (byte)FactionId.Iron;
            var field = BattlefieldParams.Landing(1917);
            field.Bombardment = 0f;                     // a quiet sector: a stray shell is not the subject
            using var m = MatchSim.CreateBattlefield(cfg, field);
            short shore = m.Fields.RearTrench(1), line = m.Fields.RearLineTrench(1);
            Assert.AreNotEqual(shore, line, "setup: on the defended coast his rearmost trench is the shore line");
            float shoreZ = MidZ(m, shore), lineZ = MidZ(m, line);
            Assert.Greater(shoreZ, lineZ + 1f, "setup: the shore trench lies seaward of the reserve line");

            // Just inside the old bound (measured off the shore trench) and well past the new one: refused now.
            var behind = OpenInBand(m, shoreZ - OffMapAbilitySystem.ParaDropKeepOut - 0.5f,
                                       lineZ - OffMapAbilitySystem.ParaDropKeepOut + 1f);
            Assert.Greater(behind.z, lineZ - OffMapAbilitySystem.ParaDropKeepOut, "setup: and behind the reserve line's keep-out");
            Step(m, Drop(m, 0, behind));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected),
                "a stick came down " + (shoreZ - behind.z).ToString("F0") + " m short of the shore trench but inside the "
                + "keep-out of team 1's reserve line, which is the line the rule protects");

            // Short of the reserve line's keep-out it is still allowed, so the rule did not simply close the map.
            var ahead = OpenInBand(m, lineZ - OffMapAbilitySystem.ParaDropKeepOut - 20f,
                                      lineZ - OffMapAbilitySystem.ParaDropKeepOut - 40f);
            Step(m, Drop(m, 0, ahead));
            Assert.AreEqual(0, CountType(m, SimEventType.CommandRejected), "short of that keep-out a drop still goes in");
        }

        [Test]
        public void TheBarrageAndTheGasStillWorkForBothFactions()
        {
            using var m = NewMatch();
            var at = OpenPoint(m, 0);
            Step(m, new SimCommand { Tick = m.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = at },
                    new SimCommand { Tick = m.World.Tick, Player = 1, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ChlorineGas, Pos = at });
            Assert.AreEqual(0, CountType(m, SimEventType.CommandRejected));
            Assert.AreEqual(2, CountType(m, SimEventType.AbilityFired));
        }

        [Test]
        public void TheDropIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                Step(m, Drop(m, 0, OpenPoint(m, 0)));
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
