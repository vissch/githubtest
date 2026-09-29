// Phase: A5c (2026-09-28) — the Playground's four machines drawn as themselves in the battle (Resources/Vehicles/<Name>,
// written by Tools/battleform.py from the Playground's own files; before this each drew as a Maw). What would go wrong
// silently: a part left under the file's root is never drawn, a far LOD short of a part pops, a model a different size from
// the footprint the sim drives it on overlaps its neighbours, and the two-legged Croaker would stand on no legs at all.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class PlaygroundMachineModelTests
    {
        static readonly (string name, byte archetype)[] Machines =
        {
            ("Brute", VehicleArchetype.Brute), ("Croaker", VehicleArchetype.Croaker), ("Mercy", VehicleArchetype.Mercy), ("Hopper", VehicleArchetype.Hopper),
        };

        static TankModel Load(string name, byte archetype)
        {
            float scale = TankRenderer.ScaleOf(archetype);   // as it is drawn: the Brute at the tanks' size
            Assert.Greater(scale, 0f, $"{name} has no row of its own in TankRenderer.Machines");
            var m = TankModel.Load(name, archetype, "Hull", scale);
            Assert.NotNull(m, $"{name} did not load from Resources/Vehicles/{name}");
            return m;
        }

        static UnitDef Def(byte archetype)
        {
            foreach (var d in UnitDefinitions.All) if (d.Archetype == archetype) return d;
            Assert.Fail($"archetype {archetype} is not in UnitDefinitions.All");
            return default;
        }

        [Test]
        public void BothLodsLoadWithTheSamePartsAllUnderTheHull()
        {
            foreach (var (name, archetype) in Machines)
            {
                var m = Load(name, archetype);
                Assert.AreNotSame(m.Lods[0], m.Lods[1], $"{name} has no far LOD of its own");
                Assert.AreEqual(-1, m.Lods[0].Parts[m.Lods[0].Find("Hull")].Parent, $"{name}: the Hull is the root part");
                foreach (var p in m.Lods[0].Parts)
                {
                    Assert.IsTrue(p.Mesh != null && p.Mesh.vertexCount > 0, $"{name} {p.Name} has no mesh");
                    if (p.Name != "Hull") Assert.GreaterOrEqual(p.Parent, 0, $"{name} {p.Name} hangs off nothing");
                    Assert.GreaterOrEqual(m.Lods[1].Find(p.Name), 0, $"{name}: the far LOD has no {p.Name}, so it pops away");
                }
                Assert.AreEqual(m.Lods[0].Parts.Count, m.Lods[1].Parts.Count, $"{name}: both LODs have the same parts");
                Assert.IsTrue(m.Sockets.ContainsKey("Socket_Muzzle") || archetype == VehicleArchetype.Mercy, $"{name} has no muzzle to fire from");
            }
        }

        /// <summary>Drawn the size of the footprint the sim drives it on (UnitDefinitions: VehicleProfile.HalfLength,
        /// HalfWidth), to within a third: the whole machine but its barrels and aerial (arms and wings count).</summary>
        [Test]
        public void EachIsDrawnTheSizeOfItsFootprint()
        {
            foreach (var (name, archetype) in Machines)
            {
                var m = Load(name, archetype);
                var drive = Def(archetype).Drive;
                var b = new Bounds(Vector3.zero, Vector3.zero);
                var parts = m.Lods[0].Parts;
                var pos = new Vector3[parts.Count];
                var rot = new Quaternion[parts.Count];
                for (int i = 0; i < parts.Count; i++)
                {
                    int up = parts[i].Parent;   // each part's frame in the machine's, as Pose builds it
                    rot[i] = up >= 0 ? rot[up] * parts[i].LocalRot : parts[i].LocalRot;
                    pos[i] = up >= 0 ? pos[up] + rot[up] * parts[i].Local : parts[i].Local;
                    // a barrel and an aerial reach past a footprint as they would past any hull
                    if (parts[i].Role == TankPartRole.Gun || parts[i].Name.StartsWith("RearGun") || parts[i].Name == "Antenna") continue;
                    var pb = parts[i].Mesh.bounds;
                    for (int c = 0; c < 8; c++)
                    {
                        var corner = new Vector3((c & 1) != 0 ? pb.max.x : pb.min.x, (c & 2) != 0 ? pb.max.y : pb.min.y, (c & 4) != 0 ? pb.max.z : pb.min.z);
                        b.Encapsulate(pos[i] + rot[i] * corner);
                    }
                }
                float halfLength = Mathf.Max(-b.min.z, b.max.z), halfWidth = Mathf.Max(-b.min.x, b.max.x);
                Assert.That(halfLength, Is.InRange(drive.HalfLength * 0.66f, drive.HalfLength * 1.5f), $"{name}: drawn {halfLength:F2} m half long on a {drive.HalfLength:F2} m footprint");
                Assert.That(halfWidth, Is.InRange(drive.HalfWidth * 0.66f, drive.HalfWidth * 1.5f), $"{name}: drawn {halfWidth:F2} m half wide on a {drive.HalfWidth:F2} m footprint");
            }
        }

        [Test]
        public void TheCroakerStandsOnTwoLegsAndTheOthersOnNone()
        {
            Assert.AreEqual(2, Load("Croaker", VehicleArchetype.Croaker).LegCount, "the Croaker walks on two legs");
            foreach (var (name, archetype) in Machines)
                if (archetype != VehicleArchetype.Croaker) Assert.AreEqual(0, Load(name, archetype).LegCount, $"{name} has no legs");
            var rigs = Load("Croaker", VehicleArchetype.Croaker).Lods[0].Legs;
            foreach (var r in rigs)
            {
                Assert.IsNotNull(r, "a leg with no rig");
                Assert.Less(r.Rest.y, -0.05f, "its toe hangs below its hip");
            }
        }
    }
}
