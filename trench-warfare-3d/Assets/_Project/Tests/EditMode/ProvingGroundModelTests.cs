// Phase: C4 (2026-09-28) — every machine is drawn with a model, and the playground's four with their own: the Brute,
// the Croaker, the Hopper and the Mercy as the battle draws them (Resources/Vehicles/<Name>, written by the splitters'
// TW_BATTLE=1 through Tools/battleform.py, drawn by TankRenderer's Machines table at scale 1). What would go wrong
// silently: a part parented to the file's root is never drawn, a model exported the wrong way round drives backwards,
// a far LOD missing a part pops, a walker with no leg rig slides, and a machine with no row is drawn as the Maw with
// nothing said. DefinedMachineModelTests holds the Skimmer and the Salvo the same way.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class ProvingGroundModelTests
    {
        static readonly (string name, byte archetype, string[] parts)[] Four =
        {
            ("Brute", VehicleArchetype.Brute, new[] { "Hull", "Antenna", "Turret", "Lamp_L", "Lamp_R", "Gun", "RearGun", "Stack", "Track_L", "Track_R", "Plate_LF", "Plate_RF", "Plate_LB", "Plate_RB" }),
            ("Croaker", VehicleArchetype.Croaker, new[] { "Hull", "Turret", "Gun", "Claw_L", "Claw_R", "Jaw_L", "Jaw_R", "Thigh_L", "Thigh_R", "Shin_L", "Shin_R", "Foot_L", "Foot_R" }),
            ("Hopper", VehicleArchetype.Hopper, new[] { "Hull", "Turret", "Wing_L", "Wing_R", "Engine_L", "Engine_R", "Tail", "Skid_L", "Skid_R" }),
            ("Mercy", VehicleArchetype.Mercy, new[] { "Hull", "Hood", "Cab", "Box_L", "Box_R", "Wheel_FL", "Wheel_FR", "Wheel_RL", "Wheel_RR", "Lamp_L", "Lamp_R", "Stack", "Door_BL", "Door_BR", "Fitting", "Spare" }),
        };

        static TankModel Load(string name, byte archetype)
        {
            var m = TankModel.Load(name, archetype, "Hull", TankRenderer.ScaleOf(archetype));   // as the battle builds it
            Assert.NotNull(m, $"{name} did not load from Resources/Vehicles/{name}");
            return m;
        }

        /// <summary>A part's pivot in the hull's frame (parts are exported unturned).</summary>
        static Vector3 At(TankModel.Lod l, int part)
        {
            Vector3 at = Vector3.zero;
            for (int k = part; k >= 0; k = l.Parts[k].Parent) at += l.Parts[k].Local;
            return at;
        }

        /// <summary>The model's half length and half width about its origin, every part counted.</summary>
        static void Footprint(TankModel m, out float halfLength, out float halfWidth, out float top)
        {
            halfLength = halfWidth = top = 0f;
            var l = m.Lods[0];
            for (int i = 0; i < l.Parts.Count; i++)
            {
                Vector3 at = At(l, i); var b = l.Parts[i].Mesh.bounds;
                halfLength = Mathf.Max(halfLength, Mathf.Abs(at.z + b.min.z), Mathf.Abs(at.z + b.max.z));
                halfWidth = Mathf.Max(halfWidth, Mathf.Abs(at.x + b.min.x), Mathf.Abs(at.x + b.max.x));
                top = Mathf.Max(top, at.y + b.max.y);
            }
        }

        [Test]
        public void BothLodsLoadWithEveryPartUnderTheHull()
        {
            foreach (var (name, archetype, want) in Four)
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
                        // the same skeleton at both levels, or a part jumps when the level changes
                        int near = m.Lods[0].Find(l.Parts[i].Name);
                        Assert.Less((At(l, i) - At(m.Lods[0], near)).magnitude, 0.02f, $"{name} LOD{lod} {l.Parts[i].Name} is not where LOD0 has it");
                    }
                }
                int tris0 = 0, tris1 = 0;
                foreach (var p in m.Lods[0].Parts) tris0 += p.Mesh.triangles.Length / 3;
                foreach (var p in m.Lods[1].Parts) tris1 += p.Mesh.triangles.Length / 3;
                Assert.Less(tris1, 1600, $"{name}: the far LOD has {tris1} triangles");
                Assert.Greater(tris0, 3 * tris1, $"{name}: the near LOD ({tris0}) is the detailed one");
                Assert.NotNull(Resources.Load<Texture2D>("Vehicles/" + name + "Atlas"), $"Resources/Vehicles/{name}Atlas is missing");
            }
        }

        /// <summary>The axis trap (pipelines.md): judged by what points forward on each, never by the bounds alone.</summary>
        [Test]
        public void EachFacesTheWayItDrives()
        {
            var brute = Load("Brute", VehicleArchetype.Brute);
            for (int lod = 0; lod < 2; lod++)
            {
                var l = brute.Lods[lod]; var gun = l.Parts[l.Find("Gun")];
                Assert.Greater(gun.Mesh.bounds.max.z, 1.5f, $"Brute LOD{lod}: the barrel reaches forward of its breech");
                Assert.Greater(gun.Mesh.bounds.max.z, -4f * gun.Mesh.bounds.min.z, $"Brute LOD{lod}: the barrel runs backwards");
                Assert.Greater(At(l, l.Find("Gun")).z, At(l, l.Find("RearGun")).z + 1f, "the gun is ahead of the rear gun");
                Assert.Less(At(l, l.Find("Track_L")).x, 0f, "the left track is on the left"); Assert.Greater(At(l, l.Find("Track_R")).x, 0f);
            }
            Assert.IsTrue(brute.Sockets.ContainsKey("Socket_Muzzle"));
            Assert.AreEqual(brute.Lods[0].Find("Gun"), brute.Sockets["Socket_Muzzle"].part, "the muzzle is on the gun");
            Assert.Greater(brute.Sockets["Socket_Muzzle"].local.z, 1.5f);
            Assert.AreEqual(1.35f, TankRenderer.ScaleOf(VehicleArchetype.Brute), "drawn at the Maw's size, not its sculpt's");
            Assert.AreEqual(TankRenderer.ScaleOf(VehicleArchetype.Brute), TankRenderer.ScaleOf(VehicleArchetype.A7V), "the A7V wears it at the same size");
            Assert.AreEqual(1, Def(VehicleArchetype.Brute).Machine.GunCount, "the sim fires the one gun the model has");
            Assert.AreEqual(0f, brute.ArtRestYaw[0], 0.15f, "the gun was modelled pointing along the nose");
            Assert.IsTrue(brute.Lods[0].Parts[brute.Lods[0].Find("Gun")].SelfAimed, "a gun in the hull takes the traverse itself");

            var croaker = Load("Croaker", VehicleArchetype.Croaker);
            {
                var l = croaker.Lods[0]; var gun = l.Parts[l.Find("Gun")];
                Assert.Greater(gun.Mesh.bounds.max.z, 1.0f, "Croaker: the barrels reach forward");
                Assert.Greater(gun.Mesh.bounds.max.z, -4f * gun.Mesh.bounds.min.z);
                Assert.AreEqual("Turret", l.Parts[gun.Parent].Name, "the gun rides the turret");
                Assert.Less(At(l, l.Find("Claw_L")).x, 0f); Assert.Greater(At(l, l.Find("Claw_R")).x, 0f);
                Assert.Greater(croaker.Sockets["Socket_Muzzle"].local.z, 0.8f);
            }

            var hopper = Load("Hopper", VehicleArchetype.Hopper);
            {
                var l = hopper.Lods[0];
                Assert.Less(At(l, l.Find("Engine_L")).x, 0f); Assert.Greater(At(l, l.Find("Engine_R")).x, 0f);
                Assert.Less(At(l, l.Find("Tail")).z, At(l, l.Find("Engine_L")).z, "the tail is astern of the engines");
                Assert.IsTrue(hopper.Sockets.ContainsKey("Socket_Muzzle_L") && hopper.Sockets.ContainsKey("Socket_Muzzle_R"), "a gun in each engine's nose");
                var ml = hopper.Sockets["Socket_Muzzle_L"]; var mr = hopper.Sockets["Socket_Muzzle_R"];
                Assert.AreEqual(l.Find("Engine_L"), ml.part); Assert.AreEqual(l.Find("Engine_R"), mr.part);
                Assert.Greater(ml.local.z, 0.3f, "the muzzle is ahead of its engine's middle"); Assert.Greater(mr.local.z, 0.3f);
            }

            var mercy = Load("Mercy", VehicleArchetype.Mercy);
            {
                var l = mercy.Lods[0];
                foreach (var (wheel, side, front) in new[] { ("Wheel_FL", -1, true), ("Wheel_FR", 1, true), ("Wheel_RL", -1, false), ("Wheel_RR", 1, false) })
                {
                    var p = l.Parts[l.Find(wheel)]; var at = At(l, l.Find(wheel));
                    Assert.AreEqual(TankPartRole.Wheel, p.Role, wheel);
                    Assert.AreEqual(side, p.Side, $"{wheel}: which side it turns with");
                    Assert.AreEqual((float)side, Mathf.Sign(at.x), $"{wheel} stands on the wrong side");
                    Assert.AreEqual(front, at.z > 0f, $"{wheel}: front or back");
                    Assert.AreEqual(p.Mesh.bounds.extents.y, at.y, 0.2f, $"{wheel}: its axle is a radius off the ground");
                }
                Assert.Greater(At(l, l.Find("Hood")).z, At(l, l.Find("Box_L")).z, "the bonnet is ahead of the box");
                Assert.AreEqual(l.Parts[l.Find("Wheel_FL")].Mesh.bounds.extents.y, mercy.WheelRadius, 0.15f, "the wheels turn at their own radius");
            }
        }

        [Test]
        public void TheCroakerStandsOnTwoLegsTheGaitCanWalk()
        {
            var m = Load("Croaker", VehicleArchetype.Croaker);
            Assert.AreEqual(2, m.LegCount); Assert.AreEqual(1, m.LegsPerSide);
            var drive = Def(VehicleArchetype.Croaker).Drive;
            Assert.AreEqual(m.LegCount, drive.Legs, "the sim takes off the legs the model has");
            for (int lod = 0; lod < 2; lod++)
            {
                var legs = m.Lods[lod].Legs;
                Assert.NotNull(legs, $"LOD{lod} has no leg rigs"); Assert.AreEqual(2, legs.Length);
                for (int k = 0; k < 2; k++)
                {
                    Assert.NotNull(legs[k], $"LOD{lod} leg {k}");
                    Assert.AreEqual(3, legs[k].Chain.Length, "thigh, shin, foot");
                    Assert.Greater(legs[k].Drop, 1.0f, $"LOD{lod} leg {k}: the toe is below the hip");
                    Assert.Greater(legs[k].Reach, 1.5f); Assert.Less(legs[k].Reach, 4f);
                    // the toe stands on the ground the model was sculpted on (y = 0), under the ankle and not metres from it
                    Vector3 toe = legs[k].Hip + legs[k].Rest;
                    Assert.AreEqual(0f, toe.y, 0.1f, $"LOD{lod} leg {k}: the toe is {toe.y:F2} m off the ground");
                    Assert.Less(Mathf.Abs(toe.z), 1.5f, $"LOD{lod} leg {k}: the toe is {toe.z:F2} m from under the hip");   // the foot is 2.2 m long
                    Assert.AreEqual(k == 0 ? -1f : 1f, Mathf.Sign(legs[k].Hip.x), "leg 0 is the left one, as the sim counts them");
                }
            }
        }

        static UnitDef Def(byte archetype)
        {
            foreach (var d in UnitDefinitions.All) if (d.Archetype == archetype) return d;
            Assert.Fail($"archetype {archetype} is not in UnitDefinitions.All");
            return default;
        }

        /// <summary>The sim drives each on a footprint (VehicleProfile.HalfLength, HalfWidth); the drawn model is that size.</summary>
        [Test]
        public void EachIsDrawnTheSizeOfTheFootprintTheSimDrivesIt()
        {
            var said = new List<string>();
            foreach (var (name, archetype, _) in Four)
            {
                Footprint(Load(name, archetype), out float halfLength, out float halfWidth, out float top);
                var drive = Def(archetype).Drive;
                said.Add($"{name}: drawn {halfLength:F2} x {halfWidth:F2} m (top {top:F2}), driven {drive.HalfLength:F2} x {drive.HalfWidth:F2}");
            }
            Debug.Log("ProvingGroundModelTests footprints: " + string.Join("; ", said));
            foreach (var (name, archetype, _) in Four)
            {
                Footprint(Load(name, archetype), out float halfLength, out float halfWidth, out _);
                var drive = Def(archetype).Drive;
                Assert.AreEqual(drive.HalfLength, halfLength, drive.HalfLength * Tolerance, $"{name}: drawn half-length {halfLength:F2} m");
                Assert.AreEqual(drive.HalfWidth, halfWidth, drive.HalfWidth * Tolerance, $"{name}: drawn half-width {halfWidth:F2} m");
            }
        }
        const float Tolerance = 0.12f;

        /// <summary>No machine the tables define is drawn as the Maw for want of a line: each has a row of its own, or a
        /// line that says whose model it wears, and that model loads.</summary>
        [Test]
        public void EveryMachineIsDrawnWithAModelSomebodyChose()
        {
            var own = new HashSet<string> { "Maw", "Tusk", "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt", "Skimmer", "Salvo", "Brute", "Croaker", "Hopper", "Mercy" };
            var wears = new Dictionary<byte, string>
            {
                [VehicleArchetype.MarkIV] = "Maw", [VehicleArchetype.MarkV] = "Maw", [VehicleArchetype.A7V] = "Brute",
                [VehicleArchetype.RenaultFT] = "Tusk", [VehicleArchetype.Whippet] = "Tusk", [VehicleArchetype.Austin] = "Tusk",
                [VehicleArchetype.Breaker] = "Maw",   // shipped without a model of its own: the fallback, written down
            };
            foreach (var u in ProvingGround.Catalogue())
            {
                if (!u.Machine) continue;
                string model = TankRenderer.ModelName(u.Archetype);
                if (wears.TryGetValue(u.Archetype, out var want)) Assert.AreEqual(want, model, $"{u.Name} wears the wrong model");
                else Assert.AreEqual(u.Name, model, $"{u.Name} ({u.Archetype}) has no model of its own and no line saying whose it wears");
                Assert.IsTrue(own.Contains(model), model);
                Assert.NotNull(Resources.Load<GameObject>($"Vehicles/{model}/{model}_LOD0"), $"Resources/Vehicles/{model}/{model}_LOD0 is missing");
                // the picture on its card is the model's (the Breaker has a picture of its own and no model)
                if (u.Archetype != VehicleArchetype.Breaker)
                    Assert.AreEqual(model, UnitLook.PortraitName(u.Archetype), $"{u.Name} is drawn as the {model} and pictured as the {UnitLook.PortraitName(u.Archetype)}");
            }
        }

        [Test]
        public void TheHopperFliesTheSkimmerSkimsTheRestStand()
        {
            Assert.AreEqual(TankRenderer.FlyerLift, TankRenderer.LiftOf(VehicleArchetype.Hopper));
            Assert.GreaterOrEqual(TankRenderer.FlyerLift, TankRenderer.FlyingFrom);
            float skim = TankRenderer.LiftOf(VehicleArchetype.Skimmer);
            Assert.Greater(skim, 0f); Assert.Less(skim, TankRenderer.FlyingFrom, "a cushion, not flight");
            foreach (byte a in new[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Pincer, VehicleArchetype.Salvo, VehicleArchetype.Brute,
                                       VehicleArchetype.Croaker, VehicleArchetype.Mercy, VehicleArchetype.A7V, VehicleArchetype.Austin })
                Assert.AreEqual(0f, TankRenderer.LiftOf(a), $"archetype {a} stands on the ground");
        }
    }
}
