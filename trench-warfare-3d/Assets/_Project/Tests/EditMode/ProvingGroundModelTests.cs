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
            // 2026-09-30: the toad mech (Tools/mechsplit.py TW_KIND=gatling TW_BATTLE=1), drawn hopping
            ("Bullfrog", VehicleArchetype.Bullfrog, new[] { "Hull", "Turret", "Gun_L", "Gun_R", "Barrels_L", "Barrels_R" }),
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
        public void TheBullfrogsBarrelsStandOutInFrontOfTheirGuns()
        {
            // parts three levels down (Hull > Turret > Gun > Barrels) import displaced unless TankImport.ParentAware names
            // the model: the barrels sat 1 m behind and 0.4 m over their guns, inside the housings, and every picture
            // still looked like a gun (2026-10-01)
            var m = Load("Bullfrog", VehicleArchetype.Bullfrog);
            float scale = TankRenderer.ScaleOf(VehicleArchetype.Bullfrog);
            foreach (var l in m.Lods)
                for (int i = 0; i < l.Parts.Count; i++)
                {
                    if (!l.Parts[i].Name.StartsWith("Barrels")) continue;
                    int gun = l.Parts[i].Parent;
                    Assert.IsTrue(gun >= 0 && l.Parts[gun].Name.StartsWith("Gun"), $"{l.Parts[i].Name} hangs on {(gun >= 0 ? l.Parts[gun].Name : "nothing")}");
                    Vector3 barrels = At(l, i) + l.Parts[i].Center, housing = At(l, gun) + l.Parts[gun].Center;
                    Assert.Greater(barrels.z - housing.z, 0.4f * scale, $"{l.Parts[i].Name}: the barrels stand out in front of the housing");
                    Assert.Less(Mathf.Abs(barrels.x - At(l, gun).x), 0.3f * scale, $"{l.Parts[i].Name}: on its gun's line");
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
            var own = new HashSet<string> { "Maw", "Tusk", "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt", "Skimmer", "Salvo", "Brute", "Croaker", "Hopper", "Mercy", "Bullfrog" };
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

        /// <summary>The Frog is drawn as the playground's frog (TW/VAT/Bake Frog), every other man as a man.</summary>
        [Test]
        public void TheFrogIsDrawnAsAFrog()
        {
            Assert.AreEqual(3, TW.Presentation.Units.VATRenderer.FigureNames.Length);
            Assert.AreEqual("Frog", TW.Presentation.Units.VATRenderer.FigureNames[2]);
            Assert.AreEqual(2, TW.Presentation.Units.VATRenderer.FigureOfArchetype(InfantryArchetype.Frog));
            Assert.AreEqual(1, TW.Presentation.Units.VATRenderer.FigureOfArchetype(InfantryArchetype.Sniper));
            foreach (var u in ProvingGround.Catalogue())
                if (!u.Machine && u.Archetype != InfantryArchetype.Frog && u.Archetype != InfantryArchetype.Sniper)
                    Assert.AreEqual(0, TW.Presentation.Units.VATRenderer.FigureOfArchetype(u.Archetype), $"{u.Name} is drawn as the soldier");

            var frog = Resources.Load<TW.Presentation.Units.VatAssetData>("Units/FigureFrog");
            var man = Resources.Load<TW.Presentation.Units.VatAssetData>("Units/FigureSoldier");
            Assert.NotNull(frog, "Resources/Units/FigureFrog is missing: run TW/VAT/Bake Frog");
            Assert.IsTrue(frog.Valid);
            Assert.AreEqual(man.Rows, frog.Rows, "a row for every clip the controller plays, as the soldier has");
            Assert.AreEqual(man.TotalFrames, frog.TotalFrames, "the same clips at the same rates: the frog's poses are the soldier's, carried over");
            Assert.Less(frog.VertexCount, 1100, "under eleven hundred vertices, like the men");
            Assert.AreNotSame(man.Mesh, frog.Mesh);
            // a man 1.78 m tall standing on the ground, as every figure is normalised
            var b = frog.Mesh.bounds;
            var vs = frog.Mesh.vertices; float lo = float.MaxValue, hi = float.MinValue;
            foreach (var v in vs) { lo = Mathf.Min(lo, v.y); hi = Mathf.Max(hi, v.y); }
            Assert.AreEqual(0f, lo, 0.08f, "his feet are on the ground in the idle frame");
            Assert.AreEqual(1.78f, hi - lo, 0.25f, "and he stands as tall as a man");
        }

        /// <summary>
        /// How much of a model's edge length is a FOLD: two triangles that share the edge and face away from each other
        /// (their normals' dot under -0.3), vertices welded by place. A sound model folds at the rims of its thin plates
        /// and nowhere else; a mesh decimated until it tore is folds all over.
        /// </summary>
        public static float Folded(TankModel.Lod l)
        {
            double length = 0, folded = 0;
            foreach (var part in l.Parts)
            {
                if (part.Mesh == null) continue;
                var vs = part.Mesh.vertices; var ix = part.Mesh.triangles;
                var place = new Dictionary<(int, int, int), int>(); var weld = new int[vs.Length];
                for (int i = 0; i < vs.Length; i++)
                {
                    var k = (Mathf.RoundToInt(vs[i].x * 2000f), Mathf.RoundToInt(vs[i].y * 2000f), Mathf.RoundToInt(vs[i].z * 2000f));
                    if (!place.TryGetValue(k, out int j)) { j = place.Count; place[k] = j; }
                    weld[i] = j;
                }
                var edges = new Dictionary<(int, int), (Vector3 normal, int count, float length, bool fold)>();
                for (int t = 0; t + 2 < ix.Length; t += 3)
                {
                    var cross = Vector3.Cross(vs[ix[t + 1]] - vs[ix[t]], vs[ix[t + 2]] - vs[ix[t]]);
                    if (cross.magnitude < 2e-10f) continue;
                    var n = cross.normalized;
                    for (int e = 0; e < 3; e++)
                    {
                        int a = weld[ix[t + e]], b = weld[ix[t + (e + 1) % 3]];
                        if (a == b) continue;
                        var k = a < b ? (a, b) : (b, a);
                        float len = (vs[ix[t + e]] - vs[ix[t + (e + 1) % 3]]).magnitude;
                        edges[k] = edges.TryGetValue(k, out var was)
                            ? (was.normal, was.count + 1, len, was.fold || Vector3.Dot(was.normal, n) < -0.3f)
                            : (n, 1, len, false);
                    }
                }
                foreach (var e in edges.Values) { length += e.length; if (e.count > 1 && e.fold) folded += e.length; }
            }
            return length > 0 ? (float)(folded / length) : 0f;
        }

        /// <summary>
        /// Seen in Play on 2026-09-28, with the far models drawn close: the Brute's and the Mercy's, decimated from
        /// their near models to a sixth of the triangles, were torn into shards (folds on 20.7 % and 15.5 % of their
        /// edge length). Every far model that looks whole folds on 9 % or less (the Croaker's, 8.9 %, is the most);
        /// the two are now Tripo's own low sculpts (5.0 % and 0.8 %), on atlases of their own.
        /// </summary>
        [Test]
        public void NoFarModelIsTornIntoShards()
        {
            var all = new List<(string name, byte archetype, string root)>
            {
                ("Maw", VehicleArchetype.Maw, "Hull"), ("Tusk", VehicleArchetype.Tusk, "Hull"),
                ("Pincer", VehicleArchetype.Pincer, "Body"), ("Kettle", VehicleArchetype.Kettle, "Body"), ("Censer", VehicleArchetype.Censer, "Body"),
                ("Pavise", VehicleArchetype.Pavise, "Body"), ("Banner", VehicleArchetype.Banner, "Body"), ("Redoubt", VehicleArchetype.Redoubt, "Body"),
                ("Skimmer", VehicleArchetype.Skimmer, "Hull"), ("Salvo", VehicleArchetype.Salvo, "Hull"),
            };
            foreach (var (name, archetype, _) in Four) all.Add((name, archetype, "Hull"));
            foreach (var (name, archetype, root) in all)
            {
                var m = TankModel.Load(name, archetype, root, 1f);
                Assert.NotNull(m, $"{name} did not load");
                Assert.NotNull(m.Lods[1], $"{name} has no far model");
                Assert.Less(Folded(m.Lods[1]), 0.12f, $"{name}'s far model is torn: it folds back on itself along this share of its edges");
                Assert.Less(Folded(m.Lods[0]), 0.12f, $"{name}'s near model is torn");
            }
        }

        /// <summary>The measure can fail: a sheet crumpled into a fan of facing triangles is all folds, a flat one none.</summary>
        [Test]
        public void TheFoldMeasureTellsACrumpledSheetFromAFlatOne()
        {
            TankModel.Lod Sheet(bool crumpled)
            {
                // a strip of quads along x; crumpled, every other row of vertices is folded back over the last
                var vs = new List<Vector3>(); var ix = new List<int>();
                for (int i = 0; i <= 8; i++)
                {
                    float x = crumpled ? (i % 2) * 0.05f : i * 0.5f, y = crumpled ? i * 0.01f : 0f;
                    vs.Add(new Vector3(x, y, 0f)); vs.Add(new Vector3(x, y, 1f));
                }
                for (int i = 0; i < 8; i++) { int a = i * 2; ix.AddRange(new[] { a, a + 1, a + 2, a + 1, a + 3, a + 2 }); }
                var mesh = new Mesh { hideFlags = HideFlags.HideAndDontSave };
                mesh.SetVertices(vs); mesh.SetTriangles(ix, 0);
                var l = new TankModel.Lod(); l.Parts.Add(new TankModel.Part { Name = "Hull", Mesh = mesh });
                return l;
            }
            var flat = Sheet(false); var torn = Sheet(true);
            Assert.AreEqual(0f, Folded(flat), 1e-6f);
            Assert.Greater(Folded(torn), 0.12f, "the folds of a crumpled sheet are counted");
            Object.DestroyImmediate(flat.Parts[0].Mesh); Object.DestroyImmediate(torn.Parts[0].Mesh);
        }

        /// <summary>A far model that is a sculpt of its own has UVs of its own, and is black or scrambled on the near
        /// model's atlas: the battle loads Resources/Vehicles/&lt;Name&gt;Atlas_LOD1 for it.</summary>
        [Test]
        public void AFarModelThatIsItsOwnSculptHasItsOwnAtlas()
        {
            foreach (var name in new[] { "Brute", "Mercy" })
            {
                Assert.NotNull(Resources.Load<Texture2D>("Vehicles/" + name + "Atlas"), $"{name} has no atlas");
                Assert.NotNull(Resources.Load<Texture2D>("Vehicles/" + name + TankRenderer.FarAtlasSuffix), $"{name}'s far model has no atlas of its own");
            }
            // the two derived from their near models wear the near model's atlas
            foreach (var name in new[] { "Croaker", "Hopper" })
                Assert.IsNull(Resources.Load<Texture2D>("Vehicles/" + name + TankRenderer.FarAtlasSuffix), $"{name}'s far model is derived: it wears the near atlas");
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
