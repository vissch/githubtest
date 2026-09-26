// Phase: Playground (2026-09-26, lane/show/playground) — what the playground's art promises, checked on the imported assets
// What the playground promises about its art, checked on the imported assets themselves:
//  a vehicle has the same named parts at every LOD, each in the same place, facing forward, cheaper with each LOD,
//  and three copies forced to three LODs and given the same hits end up in the same pose to the centimetre;
//  a figure's LODs share one skeleton, the simpler LODs are skinned to fewer bones with fewer influences, they are the
//  same size, and the game's clips carried over by Retarget leave it standing on its feet.
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using TW.Playground;
using TW.Playground.Editor;
using UnityEditor;
using UnityEngine;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundAssetTests
    {
        static PlaygroundLibrary lib;
        static PlaygroundLibrary Lib()
        {
            if (lib == null) { PlaygroundSetup.Build(); lib = AssetDatabase.LoadAssetAtPath<PlaygroundLibrary>(PlaygroundSetup.LibraryPath); }
            return lib;
        }

        static Transform FindDeep(Transform t, string name)
        {
            if (t.name == name) return t;
            for (int i = 0; i < t.childCount; i++) { var r = FindDeep(t.GetChild(i), name); if (r != null) return r; }
            return null;
        }

        [Test]
        public void The_Library_Lists_The_Art_And_The_Games_Clips()
        {
            var l = Lib();
            Assert.That(l.Vehicles.Length, Is.GreaterThanOrEqualTo(1), "vehicles");
            Assert.That(l.Units.Length, Is.GreaterThanOrEqualTo(1), "units");
            Assert.That(l.Clips.Length, Is.GreaterThanOrEqualTo(20), "clips");
            Assert.That(l.ClipSource, Is.Not.Null, "clip source rig");
            Assert.That(l.ClipIndex(l.ReferenceClip), Is.GreaterThanOrEqualTo(0), "reference clip " + l.ReferenceClip);
        }

        [Test]
        public void Every_Vehicle_LOD_Has_Every_Part_Of_The_Manifest()
        {
            foreach (var v in Lib().Vehicles)
            {
                var m = VehicleManifest.Parse(v.Manifest);
                Assert.That(v.Lods.Length, Is.EqualTo(3), v.Name + " LOD count");
                for (int k = 0; k < v.Lods.Length; k++)
                    foreach (var p in m.partList)
                    {
                        var t = FindDeep(v.Lods[k].transform, p.name);
                        Assert.That(t, Is.Not.Null, $"{v.Name} LOD{k} has no {p.name}");
                        var f = t.GetComponent<MeshFilter>();
                        Assert.That(f != null && f.sharedMesh != null && f.sharedMesh.vertexCount > 0, $"{v.Name} LOD{k} {p.name} has no mesh");
                    }
            }
        }

        [Test]
        public void Every_Vehicle_Part_Sits_In_The_Same_Place_At_Every_LOD()
        {
            foreach (var v in Lib().Vehicles)
            {
                var m = VehicleManifest.Parse(v.Manifest);
                float length = m.scale;   // tank3split models are one unit long, so its scale is the length in metres
                var bad = new List<string>();
                foreach (var p in m.partList)
                {
                    var pivot = VehicleManifest.V(p.pivot);
                    Vector3 c0 = Vector3.zero;
                    for (int k = 0; k < v.Lods.Length; k++)
                    {
                        var mesh = FindDeep(v.Lods[k].transform, p.name).GetComponent<MeshFilter>().sharedMesh;
                        var c = pivot + mesh.bounds.center;
                        if (k == 0) c0 = c;
                        // the mesh the FBX carries agrees with where the split script measured the part (axis/turn check)
                        var lp = m.lodList[k].parts.First(q => q.name == p.name);
                        var want = (VehicleManifest.V(lp.min) + VehicleManifest.V(lp.max)) * 0.5f;
                        var ext = VehicleManifest.V(lp.max) - VehicleManifest.V(lp.min);
                        // the manifest's min/max each went through the axis turn, so low and high can swap: compare centres
                        var wantC = want;
                        if (Vector3.Distance(c, wantC) > 0.08f * Mathf.Max(1f, ext.magnitude) + 0.05f) bad.Add($"LOD{k} {p.name} mesh centre {c} vs manifest {wantC}");
                        if (Vector3.Distance(c, c0) > 0.12f * length) bad.Add($"LOD{k} {p.name} {Vector3.Distance(c, c0):0.00} m from LOD0");
                    }
                }
                Assert.That(bad, Is.Empty, v.Name + ": " + string.Join("; ", bad));
            }
        }

        [Test]
        public void Every_Vehicle_Faces_Forward()
        {
            foreach (var v in Lib().Vehicles)
            {
                var m = VehicleManifest.Parse(v.Manifest);
                var gun = m.partList.FirstOrDefault(p => p.name == "Gun");
                if (gun == null) continue;
                Assert.That(VehicleManifest.V(gun.pivot).z, Is.GreaterThan(0f), "the main gun's trunnion is in the front half (+Z)");
                for (int k = 0; k < v.Lods.Length; k++)
                    Assert.That(FindDeep(v.Lods[k].transform, "Gun").GetComponent<MeshFilter>().sharedMesh.bounds.center.z, Is.GreaterThan(0f), $"LOD{k}: the barrel runs forward of its trunnion");
            }
        }

        [Test]
        public void Every_Vehicle_Gets_Cheaper_With_Each_LOD()
        {
            foreach (var v in Lib().Vehicles)
            {
                int last = int.MaxValue;
                for (int k = 0; k < v.Lods.Length; k++)
                {
                    int tris = v.Lods[k].GetComponentsInChildren<MeshFilter>().Sum(f => (int)f.sharedMesh.GetIndexCount(0) / 3);
                    Assert.That(tris, Is.LessThan(last), $"{v.Name} LOD{k} {tris} tris");
                    last = tris;
                }
                Assert.That(last, Is.LessThanOrEqualTo(1500), "the far LOD stays under 1,500 triangles");
            }
        }

        [Test]
        public void Three_Copies_At_Three_LODs_Fall_Apart_Identically()
        {
            var v = Lib().Vehicles[0];
            var parent = new GameObject("test stage").transform;
            try
            {
                var rigs = new VehicleRig[3];
                for (int k = 0; k < 3; k++) { rigs[k] = VehicleRig.Build(v, null, parent, new Vector3(k * 30f, 0f, 0f), 160f, 1.7f, 7); rigs[k].ForcedLod = k; rigs[k].SetLod(k); rigs[k].CookDelay = -1f; }
                foreach (var r in rigs)
                {
                    r.HitLocal(r.LocalCentre(r.Find("Track_L")), Vector3.right, 60f, VehicleRig.HitKind.AP);
                    r.HitLocal(new Vector3(4.2f, 0f, 0f), Vector3.down, 40f, VehicleRig.HitKind.HE);
                    r.CookOff();
                }
                for (int f = 0; f < 360; f++) foreach (var r in rigs) r.Advance(1f / 60f);
                int loose = rigs[0].Parts.Count(p => p.Loose);
                Assert.That(loose, Is.GreaterThanOrEqualTo(8), "a cook-off throws most parts off");
                Assert.That(rigs[1].PoseSignature(), Is.EqualTo(rigs[0].PoseSignature()), "LOD1 copy ended in another pose than LOD0");
                Assert.That(rigs[2].PoseSignature(), Is.EqualTo(rigs[0].PoseSignature()), "LOD2 copy ended in another pose than LOD0");
                foreach (var p in rigs[0].Parts.Where(p => p.Loose))
                    Assert.That(p.T.position.y, Is.GreaterThan(-0.5f), p.Name + " fell through the ground");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void Every_Unit_LOD_Is_Skinned_To_One_Skeleton_And_Simpler_LODs_To_Fewer_Bones()
        {
            foreach (var u in Lib().Units)
            {
                var smrs = u.Model.GetComponentsInChildren<SkinnedMeshRenderer>(true).OrderBy(s => s.name).ToArray();
                Assert.That(smrs.Length, Is.EqualTo(4), u.Name + " LODs");
                var root = smrs[0].rootBone;
                int lastBones = int.MaxValue, lastVerts = int.MaxValue;
                int[] maxInfluence = { 4, 4, 2, 2 };
                for (int k = 0; k < smrs.Length; k++)
                {
                    var s = smrs[k]; var mesh = s.sharedMesh;
                    Assert.That(s.bones.All(b => b != null && b.IsChildOf(root.parent != null ? root.parent : root)), $"LOD{k} is skinned to bones outside the one armature");
                    var perVertex = mesh.GetBonesPerVertex();
                    var weights = mesh.GetAllBoneWeights();
                    var used = new HashSet<string>(); int most = 0, at = 0;
                    for (int i = 0; i < perVertex.Length; i++)
                    {
                        int n = 0;
                        for (int j = 0; j < perVertex[i]; j++) { var w = weights[at + j]; if (w.weight > 0.001f) { n++; used.Add(s.bones[w.boneIndex].name); } }
                        at += perVertex[i]; most = Mathf.Max(most, n);
                    }
                    Assert.That(most, Is.LessThanOrEqualTo(maxInfluence[k]), $"LOD{k} influences per vertex");
                    Assert.That(used.Count, Is.LessThanOrEqualTo(lastBones), $"LOD{k} uses {used.Count} bones, more than the LOD before");
                    Assert.That(mesh.vertexCount, Is.LessThan(lastVerts), $"LOD{k} vertices");
                    lastBones = used.Count; lastVerts = mesh.vertexCount;
                    if (k == 3) { Assert.That(used.Count, Is.LessThanOrEqualTo(11), "LOD3 rig"); Assert.That(mesh.GetIndexCount(0) / 3, Is.LessThanOrEqualTo(300), "LOD3 triangles"); }
                }
                // height as drawn: the mesh is stored Z-up under a turned node, so measure through the node's matrix
                float Height(SkinnedMeshRenderer s)
                {
                    var m = s.transform.localToWorldMatrix; float lo = float.MaxValue, hi = float.MinValue;
                    foreach (var v in s.sharedMesh.vertices) { float y = m.MultiplyPoint3x4(v).y; lo = Mathf.Min(lo, y); hi = Mathf.Max(hi, y); }
                    return hi - lo;
                }
                float h0 = Height(smrs[0]);
                Assert.That(h0, Is.EqualTo(1.78f).Within(0.05f), "LOD0 stands as tall as the game's baked man (1.78 m)");
                for (int k = 1; k < smrs.Length; k++)
                    Assert.That(Height(smrs[k]), Is.EqualTo(h0).Within(0.03f * h0), $"LOD{k} is another size than LOD0");
            }
        }

        [Test]
        public void The_Games_Clips_Leave_The_Figure_Standing_On_Its_Feet()
        {
            var l = Lib();
            var parent = new GameObject("test stage").transform;
            ClipDeck deck = null;
            try
            {
                deck = ClipDeck.Make(l, parent);
                Assert.That(deck.Source.Found, Is.EqualTo(Retarget.Names.Length), "the clip rig has every bone");
                var u = UnitRig.Build(l.Units[0], deck, null, parent, Vector3.zero, 0f, 1f);
                Assert.That(u.Skel.Found, Is.EqualTo(Retarget.Names.Length), "the figure has every bone");
                // the importer's own LODGroup (built from the *_LODn names) must be gone, or it, not the picker, hides LODs
                Assert.That(u.GetComponentsInChildren<LODGroup>(true), Is.Empty, "an LODGroup is left on the figure");
                Assert.That(u.Height, Is.EqualTo(1.78f).Within(0.06f), "the figure's height as drawn");
                for (int k = 1; k < u.BonesPerLod.Length; k++) Assert.That(u.BonesPerLod[k], Is.LessThanOrEqualTo(u.BonesPerLod[k - 1]), $"LOD{k} moves more bones than LOD{k - 1}");
                float hip = u.Skel.HipsRest.y;
                foreach (var name in new[] { "Rifle Idle", "Walk With Rifle", "Crouch Idle" })
                {
                    int c = l.ClipIndex(name); if (c < 0) continue;
                    foreach (float t in new[] { 0f, 0.3f, 0.6f })
                    {
                        deck.Pose(u, c, t, 1f, null);
                        float lf = u.Skel.Bones[16].position.y, rf = u.Skel.Bones[20].position.y;
                        float feet = Mathf.Min(lf, rf);
                        Assert.That(feet, Is.InRange(-0.12f * hip, 0.45f * hip), $"{name} @{t}: lowest ankle at {feet:0.00} m, hips rest {hip:0.00}");
                        Assert.That(u.Skel.Bones[0].position.y, Is.InRange(0.3f * hip, 1.25f * hip), $"{name} @{t}: hips at {u.Skel.Bones[0].position.y:0.00}");
                        Assert.That(u.Skel.Bones[5].position.y, Is.GreaterThan(u.Skel.Bones[0].position.y), $"{name} @{t}: head below the hips");
                    }
                }
                // and a walk actually moves the legs
                int walk = l.ClipIndex("Walk With Rifle");
                if (walk >= 0)
                {
                    deck.Pose(u, walk, 0.1f, 1f, null); var a = u.Skel.Bones[14].localRotation;
                    deck.Pose(u, walk, 0.6f, 1f, null); var b = u.Skel.Bones[14].localRotation;
                    Assert.That(Quaternion.Angle(a, b), Is.GreaterThan(8f), "the thigh swings through a walk");
                }
            }
            finally { deck?.Destroy(); Object.DestroyImmediate(parent.gameObject); }
        }
    }
}
