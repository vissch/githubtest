// Phase: review fix M3 (2026-10-14) — one support order a tick. At tick 150 three of ScriptedEnemy.EnemyOrders' gates
// are true at once (150%20==10 the SOS barrage, 150%100==50 the attack barrage, 150%200==150 harassing fire), and each
// read the silver and the cooldowns from BEFORE the others landed (a command takes three ticks to apply), so the script
// could call a barrage AND a gas attack on the same tick: the fault the file's own header says was removed.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Mathematics;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class EnemySupportTickTests
    {
        sealed class Rec : TW.Net.ICommandSink
        {
            public readonly List<SimCommand> Given = new List<SimCommand>();
            public int Player => 1;
            public void Issue(SimCommand c) => Given.Add(c);
        }

        static float TrenchZ(MatchSim m, short t)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[t].CellStart + m.Map.Trenches[t].CellCount / 2]).z;

        /// <summary>[M3] The SOS barrage and harassing fire both fire on tick 150: two SupportFire commands on one tick,
        /// an HE line and (the alternation having flipped) chlorine gas.</summary>
        [Test]
        public void OnlyOneSupportOrderGoesOutOnATickWhereEveryGateIsOpen()
        {
            var cfg = SimConfig.Default;
            using var m = MatchSim.CreatePlaytest(cfg);
            var w = m.World;
            w.Tick = 150;
            w.Silver[1] = 100000;
            short enemyFront = m.Fields.FrontTrench(1), playerFront = m.Fields.FrontTrench(0);
            Assert.GreaterOrEqual(enemyFront, 0, "[M3] the playtest map gives the enemy a front trench");
            Assert.GreaterOrEqual(playerFront, 0, "[M3] the playtest map gives the player a front trench");
            float enemyZ = TrenchZ(m, enemyFront), playerZ = TrenchZ(m, playerFront);
            float toward = math.sign(playerZ - enemyZ);
            var rifle = w.Roster[0 * RosterEntry.SlotCount + 0];
            float width = m.Map.SizeMeters.x;

            // five of the player's men in the open 30 m in front of the enemy's trench: the SOS barrage's count
            for (int k = 0; k < 5; k++)
            {
                int s = w.Spawn(0, rifle.Archetype, new float3((k + 0.5f) * width / 5f, 0f, enemyZ + toward * 30f), rifle.Hp, rifle.Speed, false);
                w.TrenchId[s] = -1;
            }
            // and six in his own front trench: harassing fire's count, and the men it aims at
            for (int k = 0; k < 6; k++)
            {
                int s = w.Spawn(0, rifle.Archetype, new float3((k + 0.5f) * width / 6f, 0f, playerZ), rifle.Hp, rifle.Speed, false);
                w.TrenchId[s] = playerFront;
            }
            var ts = m.Fields.Trenches[playerFront]; ts.GarrisonCount = 6; m.Fields.Trenches[playerFront] = ts;

            var rec = new Rec();
            var ai = new ScriptedEnemy { Side = 1, UsesSupport = true, Defends = true, Attacks = false, SupportReserve = 0, SosMen = 5 };
            ai.Think(m, m, rec, rec);

            int support = 0;
            foreach (var c in rec.Given) if (c.Type == CommandType.SupportFire) support++;
            Assert.AreEqual(1, support, "[M3] a barrage AND a gas attack in one tick");
        }
    }
}
