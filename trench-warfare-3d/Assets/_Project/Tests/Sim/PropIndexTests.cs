// Phase: prop index (2026-10-19) — DeformationSystem.Shake used to walk every prop in the map for every blast impact,
// so a barrage of two dozen shells on a forest map cost props x impacts distance tests in one tick. It now asks
// MapData.PropsByCell for the props near the burst instead. These two tests guard both halves of that: the work drops,
// and exactly the same props are harmed in exactly the same order (the order feeds the sim's hash chain).
using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class PropIndexTests
    {
        const int Props = 400, Impacts = 24;

        /// <summary>What the old full scan cost: every prop measured against every impact.</summary>
        const int ScansBefore = Props * Impacts;   // 9,600

        /// <summary>Every PropWorn / PropChanged the barrage below raised, in order, measured on 2317a9cb5 (the full
        /// scan) before the index existed. "W:index:kind" worn, "C:index:kind" changed one stage.</summary>
        const string HarmBefore = "C:49:1 C:53:1 C:57:1 C:61:1 C:62:1 C:66:1 C:70:1 C:265:1 C:289:1 C:269:1 C:293:1 C:273:1 C:282:1 C:286:1 C:310:1";

        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.Seed = 0xF0E51;
            return MatchSim.CreatePlaytest(cfg);
        }

        /// <summary>A forest: Props trees on a deterministic grid over the playtest map, skipping the cells AddProp
        /// refuses (trenches, links, blocked).</summary>
        static void Forest(MapData map)
        {
            int placed = 0;
            for (int gz = 0; gz < 40 && placed < Props; gz++)
                for (int gx = 0; gx < 24 && placed < Props; gx++)
                {
                    var pos = new float3(8f + gx * 12f, 0f, 8f + gz * 11.5f);
                    if (map.AddProp(new PropDef { Pos = pos, Kind = PropKind.Tree, Scale = 1f }) >= 0) placed++;
                }
            Assert.AreEqual(Props, placed, "the forest did not fill up");
        }

        /// <summary>A barrage: Impacts shells spread over the same ground, all resolved in one tick.</summary>
        static void Barrage(MatchSim m)
        {
            for (int i = 0; i < Impacts; i++)
            {
                var at = new float3(20f + (i % 6) * 50f, 0f, 30f + (i / 6) * 110f);
                m.Blast.Queue(new Impact { Pos = at, Damage = 300f, Radius = 20f, Player = -1 });
            }
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);   // BlastSystem resolves them and Shake runs on the same tick
        }

        static string HarmLog(MatchSim m)
        {
            var sb = new StringBuilder();
            var ev = m.World.Events.Events;
            for (int i = 0; i < ev.Length; i++)
            {
                if (ev[i].Type == SimEventType.PropWorn) sb.Append("W:").Append(ev[i].A).Append(':').Append(ev[i].B).Append(' ');
                else if (ev[i].Type == SimEventType.PropChanged) sb.Append("C:").Append(ev[i].A).Append(':').Append(ev[i].B).Append(' ');
            }
            return sb.ToString().TrimEnd();
        }

        [Test]
        public void ABarrageInAForestNoLongerMeasuresEveryPropAgainstEveryShell()
        {
            using var m = NewMatch();
            Forest(m.Map);
            Barrage(m);
            Assert.Less(m.Deformation.PropsScanned, ScansBefore / 10,
                "the cell index should leave under a tenth of the " + ScansBefore + " distance tests the full scan did");
            Assert.Greater(m.Deformation.PropsScanned, 0, "it still has to look at the props near the bursts");
        }

        [Test]
        public void TheSamePropsAreHarmedInTheSameOrderAsTheFullScan()
        {
            using var m = NewMatch();
            Forest(m.Map);
            Barrage(m);
            Assert.AreEqual(HarmBefore, HarmLog(m),
                "the candidates must be visited in ascending prop index: PropHarm folds (prop, stage) into the sim's hash in visit order");
        }
    }
}
