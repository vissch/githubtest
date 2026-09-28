// Phase: A5b (2026-09-29) — the walkers and the Cutter as imported stand where crabsplit.py built them. Before the import
// put them there (TankImport, CrabManifest) every node two levels under the body was moved by Blender's FBX export: the
// Pincer's legs and guns 0.34 m, the Kettle's 0.27 m and its feet 0.39 m, the Redoubt's feet 0.86 m, the Cutter's gun
// 6.2 m, and the muzzle, fire and exhaust points with them. Nothing failed: the legs just hung off the body.
using NUnit.Framework;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class WalkerPivotTests
    {
        static readonly string[] Machines = { "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt", "Cutter" };

        [Test]
        public void EveryNodeStandsWhereTheSplitPutItAtBothLods()
        {
            foreach (var name in Machines)
            {
                var places = CrabManifest.Places(name);
                Assert.IsNotNull(places, $"crabs.json has no {name}");
                for (int lod = 0; lod < 2; lod++)
                {
                    var model = Resources.Load<GameObject>($"Vehicles/{name}/{name}_LOD{lod}");
                    Assert.IsNotNull(model, $"{name} LOD{lod} did not load");
                    int checkedNodes = 0;
                    foreach (var t in model.GetComponentsInChildren<Transform>(true))
                    {
                        if (t == model.transform || t.name.StartsWith("Socket_Muzzle") || !places.TryGetValue(t.name, out var at)) continue;
                        var got = model.transform.InverseTransformPoint(t.position);
                        Assert.Less(Vector3.Distance(got, at), 0.01f, $"{name} LOD{lod} {t.name} at {got}, built at {at}");
                        checkedNodes++;
                    }
                    Assert.Greater(checkedNodes, 4, $"{name} LOD{lod}: too few nodes matched the manifest to mean anything");
                }
            }
        }

        [Test]
        public void TheManifestReadsBackAsWritten()
        {
            var kettle = CrabManifest.Places("Kettle");
            Assert.AreEqual(new Vector3(-1.2547f, 1.2891f, 0.8195f), kettle["Thigh_LF"], "a part's pivot");
            Assert.AreEqual(new Vector3(0.0102f, 1.9465f, -1.132f), kettle["Socket_Exhaust"], "a socket's place");
            Assert.IsNull(CrabManifest.Places("Maw"), "a machine crabsplit.py did not make");
            Assert.AreEqual("Kettle", CrabManifest.MachineOf("Assets/_Project/Resources/Vehicles/Kettle/Kettle_LOD1.fbx"));
        }
    }
}
