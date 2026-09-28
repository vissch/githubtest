// Phase: C4 (2026-09-28) — the Skimmer's and the Salvo's models as the battle draws them (Resources/Vehicles/<Name>,
// Tools/mechsplit.py TW_BATTLE=1, drawn by TankRenderer's Machines table at scale 1). What would go wrong silently: a part
// parented to the file's root instead of the Hull is never drawn; a model exported the wrong way round fires its gun out
// of its tail (pipelines.md: check facing by the barrel, not the bounds); a far LOD missing a part the near one has pops;
// and a model a different size from the footprint the sim drives it on overlaps its neighbours or floats apart from them.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class DefinedMachineModelTests
    {
        static readonly (string name, byte archetype, string[] parts)[] Machines =
        {
            ("Skimmer", VehicleArchetype.Skimmer, new[] { "Hull", "Turret", "Gun", "Engine", "FanRing", "Fan", "Pod_FL", "Pod_FR", "Pod_RL", "Pod_RR" }),
            ("Salvo", VehicleArchetype.Salvo, new[] { "Hull", "Turret", "Gun", "Wheel_L", "Wheel_R" }),
        };

        static TankModel Load(string name, byte archetype)
        {
            var m = TankModel.Load(name, archetype, "Hull", 1f);
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
        public void BothLodsLoadWithEveryPartUnderTheHull()
        {
            foreach (var (name, archetype, want) in Machines)
            {
                var m = Load(name, archetype);
                Assert.AreNotSame(m.Lods[0], m.Lods[1], $"{name} has no far LOD of its own");
                for (int lod = 0; lod < 2; lod++)
                {
                    var l = m.Lods[lod];
                    Assert.AreEqual(want.Length, l.Parts.Count, $"{name} LOD{lod}: parts {string.Join(",", l.Parts.ConvertAll(p => p.Name))}");
                    foreach (var n in want) Assert.GreaterOrEqual(l.Find(n), 0, $"{name} LOD{lod} has no {n}");
                    Assert.AreEqual(-1, l.Parts[l.Find("Hull")].Parent, $"{name} LOD{lod}: the Hull is the root part");
                    for (int i = 0; i < l.Parts.Count; i++)
                    {
                        Assert.IsTrue(l.Parts[i].Mesh != null && l.Parts[i].Mesh.vertexCount > 0, $"{name} LOD{lod} {l.Parts[i].Name} has no mesh");
                        if (l.Parts[i].Name != "Hull") Assert.GreaterOrEqual(l.Parts[i].Parent, 0, $"{name} LOD{lod} {l.Parts[i].Name} hangs off nothing");
                    }
                    Assert.AreEqual("Turret", l.Parts[l.Parts[l.Find("Gun")].Parent].Name, $"{name} LOD{lod}: the gun rides the turret");
                    Assert.AreEqual(TankPartRole.Turret, l.Parts[l.Find("Turret")].Role);
                    Assert.AreEqual(TankPartRole.Gun, l.Parts[l.Find("Gun")].Role);
                    Assert.IsFalse(l.Parts[l.Find("Gun")].SelfAimed, $"{name}: the turret aims the gun");
                }
                Assert.IsTrue(m.Sockets.ContainsKey("Socket_Muzzle"), $"{name} has no muzzle");
                Assert.AreEqual(m.Lods[0].Find("Gun"), m.Sockets["Socket_Muzzle"].part, $"{name}: the muzzle is on the gun");
            }
        }

        /// <summary>The axis trap (pipelines.md): the barrel must run forward (+Z) from its breech, and the muzzle sit ahead.</summary>
        [Test]
        public void TheBarrelPointsForward()
        {
            foreach (var (name, archetype, _) in Machines)
            {
                var m = Load(name, archetype);
                for (int lod = 0; lod < 2; lod++)
                {
                    var gun = m.Lods[lod].Parts[m.Lods[lod].Find("Gun")];
                    var b = gun.Mesh.bounds;
                    Assert.Greater(b.max.z, 1.5f, $"{name} LOD{lod}: the barrel reaches {b.max.z:F2} m ahead of its breech");
                    // (the Salvo's Gun is the whole rocket box, which reaches behind its trunnion as well: its tubes are checked below)
                    if (archetype != VehicleArchetype.Salvo)
                        Assert.Greater(b.max.z, -4f * b.min.z, $"{name} LOD{lod}: the barrel runs backwards (z {b.min.z:F2}..{b.max.z:F2})");
                }
                // a node two deep (the gun under the turret) keeps its offset the right way round through TankImport: the
                // Salvo's rack (the box and its tubes) is pitched 1.15 m up its yoke (mechsplit's manifest, 2026-09-28)
                if (archetype == VehicleArchetype.Salvo)
                {
                    var trunnion = m.Lods[0].Parts[m.Lods[0].Find("Gun")].Local;
                    Assert.AreEqual(1.15f, trunnion.y, 0.1f, $"{name}: the rack's trunnion is {trunnion.y:F2} m above the turntable");
                    Assert.AreEqual(0f, trunnion.z, 0.1f, $"{name}: and {trunnion.z:F2} m ahead of it");
                    for (int k = 0; k < 16; k++)
                    {
                        Assert.IsTrue(m.Sockets.TryGetValue("Socket_Tube" + k.ToString("00"), out var tube), $"{name} has no tube {k}");
                        Assert.Greater(tube.local.z, 2f, $"{name}: tube {k}'s mouth is {tube.local.z:F2} m ahead of the trunnion");
                    }
                }
                var muzzle = m.Sockets["Socket_Muzzle"].local;
                Assert.Greater(muzzle.z, 1.5f, $"{name}: the muzzle is {muzzle.z:F2} m ahead of the breech");
                Assert.AreEqual(0f, m.ArtRestYaw[0], 0.15f, $"{name}: the gun was modelled pointing {m.ArtRestYaw[0] * Mathf.Rad2Deg:F0} degrees off the nose");
            }
        }

        [Test]
        public void TheSkimmerFanSpinsAboutTheHullsLengthAndTheSalvoRollsOnItsWheels()
        {
            var sk = Load("Skimmer", VehicleArchetype.Skimmer);
            var fan = sk.Lods[0].Parts[sk.Lods[0].Find("Fan")];
            Assert.AreEqual(TankPartRole.Fan, fan.Role);
            var fb = fan.Mesh.bounds;
            Assert.Less(fb.size.z, 0.5f * Mathf.Min(fb.size.x, fb.size.y), "the fan is a disc facing along the hull, so it turns about z");
            Assert.Less(Mathf.Abs(fb.center.x) + Mathf.Abs(fb.center.y), 0.25f * fb.size.x, "and its pivot is its hub");
            Assert.Less(sk.Lods[0].Parts[sk.Lods[0].Find("FanRing")].Local.z, 0f, "the fan is astern");

            var sa = Load("Salvo", VehicleArchetype.Salvo);
            for (int s = 0; s < 2; s++)
            {
                var wheel = sa.Lods[0].Parts[sa.Lods[0].Find(s == 0 ? "Wheel_L" : "Wheel_R")];
                Assert.AreEqual(TankPartRole.Wheel, wheel.Role);
                Assert.AreEqual(s == 0 ? -1 : 1, wheel.Side, $"{wheel.Name} is on the {(s == 0 ? "left (-x)" : "right (+x)")}");
                Assert.AreEqual(s == 0 ? -1f : 1f, Mathf.Sign(wheel.Local.x), $"{wheel.Name} stands on the wrong side");
                Assert.Greater(wheel.Local.z, 0f, $"{wheel.Name}: the wheels are at the front");
                Assert.AreEqual(wheel.Mesh.bounds.extents.y, wheel.Local.y, 0.2f, $"{wheel.Name}: its axle is a radius off the ground");
            }
            Assert.AreEqual(sa.Lods[0].Parts[sa.Lods[0].Find("Wheel_L")].Mesh.bounds.extents.y, sa.WheelRadius, 1e-3f, "the wheels turn at their own radius");
        }

        /// <summary>The sim drives each on a footprint (VehicleProfile.HalfLength, HalfWidth); the drawn model is that size.</summary>
        [Test]
        public void EachIsDrawnTheSizeOfTheFootprintTheSimDrivesIt()
        {
            foreach (var (name, archetype, _) in Machines)
            {
                var m = Load(name, archetype);
                var drive = Def(archetype).Drive;
                float halfLength = 0f, halfWidth = 0f;
                var l = m.Lods[0];
                for (int i = 0; i < l.Parts.Count; i++)
                {
                    // every part's box, into the hull's frame (parts are not rotated: mechsplit exports them unturned)
                    Vector3 at = Vector3.zero;
                    for (int k = i; k >= 0; k = l.Parts[k].Parent) at += l.Parts[k].Local;
                    var b = l.Parts[i].Mesh.bounds;
                    halfLength = Mathf.Max(halfLength, Mathf.Abs(at.z + b.min.z), Mathf.Abs(at.z + b.max.z));
                    halfWidth = Mathf.Max(halfWidth, Mathf.Abs(at.x + b.min.x), Mathf.Abs(at.x + b.max.x));
                }
                Assert.AreEqual(drive.HalfLength, halfLength, drive.HalfLength * 0.12f, $"{name}: drawn half-length {halfLength:F2} m");
                Assert.AreEqual(drive.HalfWidth, halfWidth, drive.HalfWidth * 0.12f, $"{name}: drawn half-width {halfWidth:F2} m");
            }
        }

        [Test]
        public void EachHasItsOwnAtlas()
        {
            foreach (var (name, _, _) in Machines)
                Assert.NotNull(Resources.Load<Texture2D>("Vehicles/" + name + "Atlas"), $"Resources/Vehicles/{name}Atlas is missing");
        }
    }
}
