// Phase: C4 (2026-09-28) — a machine's third, very-far level (Resources/Vehicles/<Name>/<Name>_LOD2.fbx: simple shapes
// for the 120-600 zoom bands). What would go wrong silently: a machine with no LOD2 file drawing nothing past
// FarLodDistance; a LOD2 that loses a part LOD1 has (it pops, and IsOff finds nothing to hide); a LOD2 that is not
// cheaper than the level above it; a LOD2 without its own baked atlas (its UVs are on <Name>Atlas_LOD2, not the machine's
// atlas); the level picked by distance skipping a level or ignoring a missing one.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TankLodTests
    {
        static readonly (string name, byte archetype, string root)[] Machines =
        {
            ("Maw", VehicleArchetype.Maw, "Hull"), ("Tusk", VehicleArchetype.Tusk, "Hull"),
            ("Pincer", VehicleArchetype.Pincer, "Body"), ("Kettle", VehicleArchetype.Kettle, "Body"),
            ("Censer", VehicleArchetype.Censer, "Body"), ("Pavise", VehicleArchetype.Pavise, "Body"),
            ("Banner", VehicleArchetype.Banner, "Body"), ("Redoubt", VehicleArchetype.Redoubt, "Body"),
            ("Cutter", 0, "Body"), ("Skimmer", VehicleArchetype.Skimmer, "Hull"),
            ("Salvo", VehicleArchetype.Salvo, "Hull"),
        };

        static string Tree(Transform t) => t.name + (t.childCount == 0 ? "" : "(" + string.Join(",", System.Linq.Enumerable.Select(System.Linq.Enumerable.Cast<Transform>(t), Tree)) + ")");

        static int Triangles(TankModel.Lod l)
        {
            int t = 0;
            foreach (var p in l.Parts) if (p.Mesh != null) t += (int)(p.Mesh.GetIndexCount(0) / 3);
            return t;
        }

        [Test]
        public void PickLod_WalksOutOneLevelAtATime_AndOnlyToALevelTheModelHas()
        {
            var m = new TankModel();
            m.Lods[0] = new TankModel.Lod(); m.Lods[1] = new TankModel.Lod(); m.Lods[2] = new TankModel.Lod();
            const float near = 170f, far = 320f;
            Assert.AreEqual(0, TankRenderer.PickLod(100f * 100f, near, far, m));
            Assert.AreEqual(1, TankRenderer.PickLod(200f * 200f, near, far, m));
            Assert.AreEqual(2, TankRenderer.PickLod(400f * 400f, near, far, m));
            m.Lods[2] = null;   // a model that never loaded a third level
            Assert.AreEqual(1, TankRenderer.PickLod(400f * 400f, near, far, m));
            m.Lods[1] = null;
            Assert.AreEqual(0, TankRenderer.PickLod(400f * 400f, near, far, m));
        }

        /// <summary>Every machine has all three levels after loading: its own LOD2 file, or LOD1 again (the same
        /// object, so nothing is built twice). An own LOD2 has every part LOD1 has, each with a mesh, and fewer
        /// triangles than LOD1.</summary>
        [Test]
        public void EveryMachineHasAThirdLevel_ItsOwnCheaperThanLod1_OrLod1Again()
        {
            foreach (var (name, archetype, root) in Machines)
            {
                var m = TankModel.Load(name, archetype, root, 1f);
                Assert.NotNull(m, $"{name} did not load");
                Assert.NotNull(m.Lods[2], $"{name}: no third level at all");
                bool file = Resources.Load<GameObject>($"Vehicles/{name}/{name}_LOD2") != null;
                if (!file)
                {
                    Assert.AreSame(m.Lods[1], m.Lods[2], $"{name}: with no LOD2 file the third level is LOD1");
                    Assert.IsFalse(m.OwnLod(2), $"{name}: a borrowed level is not its own");
                    continue;
                }
                Assert.IsTrue(m.OwnLod(2), $"{name}: its LOD2 file did not load as a level of its own; its nodes: {Tree(Resources.Load<GameObject>($"Vehicles/{name}/{name}_LOD2").transform)}");
                // its UVs are on its own baked atlas: without it the machine would be painted from the wrong texture
                Assert.NotNull(TankRenderer.FarAtlas(m), $"{name}: a LOD2 with no Resources/Vehicles/{name}Atlas_LOD2");
                var l1 = m.Lods[1]; var l2 = m.Lods[2];
                foreach (var p in l1.Parts)
                {
                    int i = l2.Find(p.Name);
                    Assert.GreaterOrEqual(i, 0, $"{name} LOD2 has no {p.Name}");
                    Assert.IsTrue(l2.Parts[i].Mesh != null && l2.Parts[i].Mesh.vertexCount > 0, $"{name} LOD2 {p.Name} has no mesh");
                    Assert.AreEqual(p.Local, l2.Parts[i].Local, $"{name} LOD2 {p.Name} sits somewhere else than at LOD1");
                }
                Assert.Less(Triangles(l2), Triangles(l1), $"{name}: LOD2 is not cheaper than LOD1");
            }
        }
    }
}
