// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — MachineLamps, where each machine's running lamps
// and the Maw's furnace glow sit. Read off each of the ten models the battle draws, in the hull's own frame (the frame
// TankRenderer poses), so a root part carrying a rotation of its own from its FBX cannot put a lamp under the hull or
// a front lamp at the back. Needs the models: Unity only (otr cannot load an FBX).
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class MachineLampTests
    {
        static readonly (string Name, byte Archetype, string Root, float Scale)[] Models =
        {
            ("Maw", VehicleArchetype.Maw, "Hull", VehicleSize.Tank), ("Tusk", VehicleArchetype.Tusk, "Hull", VehicleSize.Tank),
            ("Pincer", VehicleArchetype.Pincer, "Body", VehicleSize.Walker), ("Kettle", VehicleArchetype.Kettle, "Body", VehicleSize.Walker),
            ("Censer", VehicleArchetype.Censer, "Body", VehicleSize.Walker), ("Pavise", VehicleArchetype.Pavise, "Body", VehicleSize.Walker),
            ("Banner", VehicleArchetype.Banner, "Body", VehicleSize.Walker), ("Redoubt", VehicleArchetype.Redoubt, "Body", VehicleSize.Walker),
            ("Skimmer", VehicleArchetype.Skimmer, "Hull", 1f), ("Salvo", VehicleArchetype.Salvo, "Hull", 1f),
        };

        [Test]
        public void EveryMachineHasFourLampsOnTheUpperCornersOfItsHull_FrontAtTheFront()
        {
            foreach (var (name, archetype, root, scale) in Models)
            {
                var m = TankModel.Load(name, archetype, root, scale);
                Assert.IsNotNull(m, name);
                var lamps = MachineLamps.Place(m);
                Assert.IsNotNull(lamps, $"{name}: its hull mesh can be read");
                Assert.AreEqual(4, lamps.Length, name);
                var hull = m.Lods[0].Parts[0];
                var toHull = MachineLamps.HullFrame(hull);
                var b = HullBounds(hull, toHull);
                var at = new Vector3[4];
                for (int k = 0; k < 4; k++) at[k] = toHull.MultiplyPoint3x4(lamps[k]);
                Assert.Greater(Mathf.Min(at[MachineLamps.FrontLeft].z, at[MachineLamps.FrontRight].z), Mathf.Max(at[MachineLamps.RearLeft].z, at[MachineLamps.RearRight].z), $"{name}: the front pair ahead of the rear");
                Assert.Less(at[MachineLamps.FrontLeft].x, at[MachineLamps.FrontRight].x, $"{name}: front left is left (-x)");
                Assert.Less(at[MachineLamps.RearLeft].x, at[MachineLamps.RearRight].x, $"{name}: rear left is left");
                for (int pair = 0; pair < 4; pair += 2)
                {
                    Vector3 l = at[pair], r = at[pair + 1];
                    Assert.AreEqual(b.center.x, (l.x + r.x) * 0.5f, 0.02f, $"{name}: pair {pair / 2} mirrored across the hull (an odd pair reads as two stray dots)");
                    Assert.AreEqual(l.y, r.y, 0.02f, $"{name}: pair {pair / 2} at one height"); Assert.AreEqual(l.z, r.z, 0.02f, $"{name}: pair {pair / 2} abreast");
                }
                for (int k = 0; k < 4; k++)
                {
                    Assert.GreaterOrEqual(at[k].y, b.center.y, $"{name} lamp {k}: on the upper half of the hull, not under it");
                    Assert.GreaterOrEqual(Mathf.Abs(at[k].x - b.center.x), 0.15f * b.extents.x, $"{name} lamp {k}: off the centre line, a pair and not one lamp ({at[k]} against {b})");
                    var grown = b; grown.Expand(2f * (MachineLamps.Nudge + 0.01f));
                    Assert.IsTrue(grown.Contains(at[k]), $"{name} lamp {k}: on the hull ({at[k]} against {b})");
                    for (int j = k + 1; j < 4; j++) Assert.Greater(Vector3.Distance(at[k], at[j]), 0.2f, $"{name}: lamps {k} and {j} apart");
                }
            }
        }

        [Test]
        public void TheMawsFurnaceGlowsAtTheFrontOfItsHull_OnItsCentreLine()
        {
            var maw = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", VehicleSize.Tank);
            Assert.IsNotNull(maw);
            Assert.IsTrue(MachineLamps.Furnace(maw, out Vector3 mouth), "the Maw's hull has its furnace painted (UV2.y)");
            var hull = maw.Lods[0].Parts[0];
            var toHull = MachineLamps.HullFrame(hull);
            var b = HullBounds(hull, toHull);
            var at = toHull.MultiplyPoint3x4(mouth);
            Assert.Greater(at.z, b.center.z, "in the front half (tanksplit.py: front of the hull, centre line, mid height)");
            Assert.Less(Mathf.Abs(at.x - b.center.x), 0.3f * b.extents.x, "near the centre line");
            Assert.IsFalse(MachineLamps.Furnace(null, out _));
        }

        static Bounds HullBounds(TankModel.Part hull, Matrix4x4 toHull)
        {
            var v = hull.Mesh.vertices;
            var b = new Bounds(toHull.MultiplyPoint3x4(v[0]), Vector3.zero);
            for (int i = 1; i < v.Length; i++) b.Encapsulate(toHull.MultiplyPoint3x4(v[i]));
            return b;
        }
    }
}
