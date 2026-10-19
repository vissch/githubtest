using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class ScratchPropIndexDiag
    {
        [Test]
        public void QueryFindsEveryPropTheFullScanWouldHit()
        {
            var p = BattlefieldParams.Landing(1234u);
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            var sb = new StringBuilder();
            sb.Append("props=").Append(map.Props.Length).Append(" size=").Append(map.SizeMeters.x).Append('x').Append(map.SizeMeters.y)
              .Append(" grid=").Append(map.PropsByCell.Width).Append('x').Append(map.PropsByCell.Length)
              .Append(" reach=").Append(map.PropsByCell.MaxWreckReach).Append('\n');
            using var cand = new NativeList<int>(64, Allocator.Persistent);
            var rnd = new Unity.Mathematics.Random(12345u);
            int bad = 0;
            for (int t = 0; t < 400 && bad < 10; t++)
            {
                var at = new float3(rnd.NextFloat(0f, map.SizeMeters.x), 0f, rnd.NextFloat(0f, map.SizeMeters.y));
                float r = rnd.NextFloat(5f, 25f);
                map.PropsByCell.Query(at, r, cand);
                for (int i = 0; i < map.Props.Length; i++)
                {
                    var prop = map.Props[i];
                    float3 d = prop.Pos - at; d.y = 0f;
                    float dist = SimMath.Length(d);
                    if (PropRules.IsWreckage(prop.Kind)) dist = math.max(0f, dist - PropRules.WreckBlastReach * (prop.Scale > 0f ? prop.Scale : 1f));
                    if (dist >= r) continue;
                    bool found = false;
                    for (int k = 0; k < cand.Length; k++) if (cand[k] == i) { found = true; break; }
                    if (!found && bad < 10)
                    {
                        bad++;
                        sb.Append("MISS prop=").Append(i).Append(" kind=").Append((int)prop.Kind).Append(" scale=").Append(prop.Scale)
                          .Append(" pos=").Append(prop.Pos.x).Append(',').Append(prop.Pos.z)
                          .Append(" impact=").Append(at.x).Append(',').Append(at.z).Append(" r=").Append(r)
                          .Append(" dist=").Append(dist).Append('\n');
                    }
                }
            }
            System.IO.File.WriteAllText("C:/Users/PC/AppData/Local/Temp/tw-diag.txt", sb.ToString());
            Assert.AreEqual(0, bad, "the query missed props the full scan would hit");
        }
    }
}
