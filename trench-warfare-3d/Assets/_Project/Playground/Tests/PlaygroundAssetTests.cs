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

        /// <summary>What a vehicle runs on: its tracks, or its wheels (the ambulance). Thrown, each shows every side.</summary>
        static IEnumerable<string> RunningGear(PlaygroundLibrary.VehicleEntry v) =>
            VehicleManifest.Parse(v.Manifest).partList.Select(p => p.name).Where(n => n.StartsWith("Track_") || n.StartsWith("Wheel_"));

        [Test]
        public void Every_Vehicle_Track_Is_Closed_So_A_Thrown_Track_Shows_No_Hole()
        {
            // Tripo modelled only the outside of the skirts: tipped over, the open back showed the ink pass as a black slab.
            // Every open loop longer than an eighth of the track (a hole you could see into) must be capped.
            foreach (var v in Lib().Vehicles)
                for (int k = 0; k < v.Lods.Length; k++)
                    foreach (var side in RunningGear(v))
                    {
                        var mesh = FindDeep(v.Lods[k].transform, side).GetComponent<MeshFilter>().sharedMesh;
                        var verts = mesh.vertices; var tris = mesh.triangles;
                        // weld by position (UV seams split vertices), then find the edges used by one triangle only
                        var id = new Dictionary<Vector3Int, int>(); var map = new int[verts.Length];
                        for (int i = 0; i < verts.Length; i++)
                        {
                            var key = new Vector3Int(Mathf.RoundToInt(verts[i].x * 1000), Mathf.RoundToInt(verts[i].y * 1000), Mathf.RoundToInt(verts[i].z * 1000));
                            if (!id.TryGetValue(key, out map[i])) { map[i] = id.Count; id[key] = map[i]; }
                        }
                        var pos = new Vector3[id.Count]; for (int i = 0; i < verts.Length; i++) pos[map[i]] = verts[i];
                        var count = new Dictionary<(int, int), int>();
                        for (int i = 0; i < tris.Length; i += 3)
                            for (int e = 0; e < 3; e++)
                            {
                                int a = map[tris[i + e]], b = map[tris[i + (e + 1) % 3]]; var key = a < b ? (a, b) : (b, a);
                                count.TryGetValue(key, out int n); count[key] = n + 1;
                            }
                        var open = count.Where(kv => kv.Value == 1).Select(kv => kv.Key).ToList();
                        // join open edges into loops and measure each
                        var adj = new Dictionary<int, List<int>>();
                        foreach (var (a, b) in open) { if (!adj.ContainsKey(a)) adj[a] = new List<int>(); if (!adj.ContainsKey(b)) adj[b] = new List<int>(); adj[a].Add(b); adj[b].Add(a); }
                        var seen = new HashSet<int>(); float length = mesh.bounds.size.z, worst = 0f;
                        foreach (var start in adj.Keys)
                        {
                            if (seen.Contains(start)) continue;
                            float per = 0f; bool cycle = true; var stack = new Stack<int>(); stack.Push(start); seen.Add(start);
                            while (stack.Count > 0) { int x = stack.Pop(); if (adj[x].Count < 2) cycle = false; foreach (int y in adj[x]) { per += (pos[x] - pos[y]).magnitude * 0.5f; if (seen.Add(y)) stack.Push(y); } }
                            // a hole has open edges all the way round; a lone open edge is a T-junction seam (one long
                            // triangle edge meeting several small ones along the same line): hairline, not a hole
                            if (cycle) worst = Mathf.Max(worst, per);
                        }
                        Assert.That(worst, Is.LessThan(0.125f * length), $"{v.Name} LOD{k} {side}: an open loop {worst:0.0} m round (track {length:0.0} m long)");
                    }
        }

        [Test]
        public void No_Track_Face_Is_Painted_With_The_Soot_Texel()
        {
            // the caps that close a track's open back are textured from the track itself; any that kept the soot texel
            // rendered pitch black when the track was thrown (critic r6). Share of track surface on near-black texels:
            foreach (var v in Lib().Vehicles)
                for (int k = 0; k < v.Lods.Length; k++)
                {
                    var tex = new Texture2D(2, 2); tex.LoadImage(System.IO.File.ReadAllBytes(AssetDatabase.GetAssetPath(v.Atlas[k])));
                    foreach (var side in RunningGear(v))
                    {
                        var m = FindDeep(v.Lods[k].transform, side).GetComponent<MeshFilter>().sharedMesh;
                        var vs = m.vertices; var uv = m.uv; var tr = m.triangles; double dark = 0, all = 0;
                        for (int i = 0; i < tr.Length; i += 3)
                        {
                            float area = Vector3.Cross(vs[tr[i + 1]] - vs[tr[i]], vs[tr[i + 2]] - vs[tr[i]]).magnitude;
                            var c = tex.GetPixelBilinear((uv[tr[i]].x + uv[tr[i + 1]].x + uv[tr[i + 2]].x) / 3f, (uv[tr[i]].y + uv[tr[i + 1]].y + uv[tr[i + 2]].y) / 3f);
                            all += area; if (c.grayscale < 0.035f) dark += area;
                        }
                        Assert.That(dark / all, Is.LessThan(0.03), $"{v.Name} LOD{k} {side}: {dark / all:P1} of its surface on near-black texels");
                    }
                    Object.DestroyImmediate(tex);
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
                // the muzzle end in the front half (+Z): a machine turned 180 degrees fails this. (Not the trunnion: the
                // hovercraft's gun is mounted mid-deck, its trunnion 0.65 m behind the middle.)
                var barrel = FindDeep(v.Lods[0].transform, "Gun").GetComponent<MeshFilter>().sharedMesh.bounds;
                Assert.That(VehicleManifest.V(gun.pivot).z + barrel.max.z, Is.GreaterThan(0f), v.Name + ": the gun's muzzle is in the front half (+Z)");
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
                // and every piece ends lying down: its centre no higher than half its middle dimension (turned about its
                // pivot, a gun or an antenna balanced on its tip 4 m tall for good; critic round 2)
                for (int f = 0; f < 900; f++) rigs[0].Advance(1f / 60f);
                foreach (var p in rigs[0].Parts.Where(p => p.Loose))
                {
                    var e = p.Box.size * rigs[0].Size; var dims = new[] { e.x, e.y, e.z }.OrderBy(x => x).ToArray();
                    float centre = (p.Fly.Pos + p.Fly.Rot * p.Box.center).y * rigs[0].Size;
                    Assert.That(new Vector2(p.Fly.Pos.x, p.Fly.Pos.z).magnitude * rigs[0].Size, Is.LessThan(30f), $"{p.Name} ran away (a topple rolled a stack 140 m once)");
                    Assert.That(centre, Is.LessThanOrEqualTo(0.5f * dims[1] + 0.2f), $"{p.Name} ended standing on end: centre {centre:0.00} m up, dims {dims[0]:0.0}/{dims[1]:0.0}/{dims[2]:0.0}");
                }
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Shelled_Building_Leaves_Nothing_Floating_And_No_Piece_On_End()
        {
            // eight shells walking round the ruin, as round.sh fires them; then twenty seconds to settle. A slab 0.44 m thick
            // was kept standing as a corner and read as a post on end (critic r8); loose pieces must lie on a broad face
            var parent = new GameObject("test stage").transform;
            try
            {
                var b = BuildingRig.Build("Ruins", null, null, parent, Vector3.zero, 0f, 11);
                Assert.That(b, Is.Not.Null, "the Ruins set loads");
                foreach (var p in b.Pieces.Where(p => p.Anchored && p.Grounded))
                    Assert.That(BuildingRig.Stout(p), Is.True, $"{p.T.name} is kept as a corner but cannot stand alone");
                for (int k = 0; k < 8; k++)
                {
                    float ang = k * 2.39996f, r = b.Radius * 0.55f;
                    b.ShellLocal(new Vector3(Mathf.Cos(ang) * r, 1f + (k % 3) * 1.2f, Mathf.Sin(ang) * r), 70f);
                    for (int f = 0; f < 45; f++) b.Advance(1f / 60f);
                }
                for (int f = 0; f < 1200; f++) b.Advance(1f / 60f);
                Assert.That(b.Standing, Is.LessThan(b.Pieces.Count), "eight shells bring something down");
                Assert.That(b.Standing, Is.GreaterThan(0), "a ruin is left standing");
                Assert.That(b.Floating, Is.EqualTo(0), "standing chunks with nothing under them");
                Assert.That(b.OnEnd, Is.EqualTo(0), "loose pieces resting on a small face");
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
                    // 13: the shoulders stay in the simpler rigs (folded, they moved the arm's outline at each switch; docs/22)
                    if (k == 3) { Assert.That(used.Count, Is.LessThanOrEqualTo(13), "LOD3 rig"); Assert.That(mesh.GetIndexCount(0) / 3, Is.LessThanOrEqualTo(300), "LOD3 triangles"); }
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
                // the rifle measure reads the pose: with the body push off, a crouch walk puts the barrel in the torso; with
                // it on, never (critic r5: a number that never changes proves nothing)
                int crouch = l.ClipIndex("Rifle Crouch Walk");
                if (crouch >= 0)
                {
                    float worst = 0f, kept = 0f;
                    u.KeepRifleClear = false;
                    foreach (float t in new[] { 0.1f, 0.4f, 0.7f, 1.0f }) { u.PoseAt(crouch, t); worst = Mathf.Max(worst, u.RifleInside); }
                    u.KeepRifleClear = true;
                    foreach (float t in new[] { 0.1f, 0.4f, 0.7f, 1.0f }) { u.PoseAt(crouch, t); kept = Mathf.Max(kept, u.RifleInside); }
                    Assert.That(worst, Is.GreaterThan(0.1f), "with the push off, the crouch walk should put the rifle in the body (positive control)");
                    Assert.That(kept, Is.LessThanOrEqualTo(0.25f), "with the push on, the rifle stays out of the body");
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
    
        static float Lowest(Transform t, Mesh m)
        {
            float low = float.MaxValue; var w = t.localToWorldMatrix;
            foreach (var v in m.vertices) low = Mathf.Min(low, w.MultiplyPoint3x4(v).y);
            return low;
        }

        static float Highest(Transform t, Mesh m)
        {
            float high = float.MinValue; var w = t.localToWorldMatrix;
            foreach (var v in m.vertices) high = Mathf.Max(high, w.MultiplyPoint3x4(v).y);
            return high;
        }

        [Test]
        public void A_Walker_Walks_With_Its_Feet_On_The_Ground()
        {
            // WalkerDrive: the game's WalkerGait places the feet, the playground bends the knees forward. Walking for five
            // seconds, no foot may sink into the ground and one must always be standing on it, and the body stays up.
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).walker);
            if (e == null) Assert.Ignore("no walker in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                // built away from the origin: a walker once walked from the world's origin wherever it was built (loop 2 r33)
                var spawn = new Vector3(200f, 0f, 0f);
                var r = VehicleRig.Build(e, null, parent, spawn, 0f, 1.4f, 3); r.ForcedLod = 0; r.SetLod(0);
                r.Walker.Speed = 2.5f;
                float worstSink = 0f, worstLift = 0f, lowHull = float.MaxValue;
                for (int f = 0; f < 300; f++)
                {
                    r.Advance(1f / 60f);
                    if (f < 30) continue;
                    float l = Lowest(r.Find("Foot_L").T, r.Find("Foot_L").Lods[0]), rr = Lowest(r.Find("Foot_R").T, r.Find("Foot_R").Lods[0]);
                    worstSink = Mathf.Min(worstSink, Mathf.Min(l, rr));
                    worstLift = Mathf.Max(worstLift, Mathf.Min(l, rr));
                    lowHull = Mathf.Min(lowHull, r.Find("Hull").T.position.y);
                }
                Assert.That(worstSink, Is.GreaterThan(-0.25f), "a foot sank into the ground");
                Assert.That(worstLift, Is.LessThan(0.35f), "both feet were off the ground at once");
                Assert.That(lowHull, Is.GreaterThan(1f), "the body sat down while walking");
                Assert.That(r.Walker.WalkPos.magnitude, Is.GreaterThan(8f), "it did not walk anywhere");
                var h = r.Find("Hull").T.position;
                Assert.That(new Vector2(h.x - spawn.x, h.z - spawn.z).magnitude, Is.LessThan(2f * r.Walker.Radius + 5f), "it walked somewhere other than where it was built");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Gatling_Spins_Up_Fires_Both_Guns_And_Winds_Down_Its_Barrels_Turning_On_Their_Axis()
        {
            // the Bullfrog (mechsplit.py TW_KIND=gatling): a burst spins the barrels up, rounds come only once they are near
            // speed, by turns from both guns, and the barrels wind down after. They turn on their own axis: spun half a
            // turn, their mesh's middle is where it was (a pivot off the axis would swing the barrels round the gun).
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).hopper);
            if (e == null) Assert.Ignore("no gatling in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.4f, 3); r.ForcedLod = 0; r.SetLod(0);
                foreach (var s in new[] { "L", "R" })
                {
                    var b = r.Find("Barrels_" + s); Assert.NotNull(b, "no Barrels_" + s);
                    Assert.That(new Vector2(b.Box.center.x, b.Box.center.y).magnitude * r.Size, Is.LessThan(0.05f), $"Barrels_{s} do not turn on their own axis");
                    Assert.NotNull(r.Find("Gun_" + s), "no Gun_" + s);
                }
                var bar = r.Find("Barrels_L"); var barGun = r.Find("Gun_L");
                // in the gun's frame: firing, the whole body leans and shudders (HopDrive), which moves the gun too
                Vector3 mid0 = barGun.T.InverseTransformPoint(bar.T.TransformPoint(bar.Box.center));
                r.FireGun();
                r.Advance(0.2f);
                Assert.That(r.RoundsFired[0] + r.RoundsFired[1], Is.EqualTo(0), "it fired before the barrels were at speed");
                for (int f = 0; f < 60; f++) r.Advance(1f / 60f);
                Assert.That(r.BarrelSpeed, Is.EqualTo(VehicleRig.BarrelMax).Within(1f), "the barrels are not at full speed a second into the burst");
                Assert.That(Vector3.Distance(barGun.T.InverseTransformPoint(bar.T.TransformPoint(bar.Box.center)), mid0) * r.Size, Is.LessThan(0.05f), "spinning, the barrels moved off their axis");
                Assert.That(r.RoundsFired[0], Is.GreaterThan(4), "the left gun did not fire"); Assert.That(r.RoundsFired[1], Is.GreaterThan(4), "the right gun did not fire");
                Assert.That(Mathf.Abs(r.RoundsFired[0] - r.RoundsFired[1]), Is.LessThanOrEqualTo(1), "the guns do not fire by turns");
                for (int f = 0; f < 300; f++) r.Advance(1f / 60f);
                int fired = r.RoundsFired[0] + r.RoundsFired[1];
                Assert.That(fired, Is.InRange((int)((VehicleRig.Burst - 0.5f) * VehicleRig.RoundsPerSecond * 0.7f), (int)(VehicleRig.Burst * VehicleRig.RoundsPerSecond) + 2), "a burst fires about its length in rounds");
                Assert.That(r.BarrelSpeed, Is.EqualTo(0f), "the barrels did not wind down after the burst");
                // a gun shot off falls silent; the other keeps firing
                r.Detach(r.Find("Gun_L"), Vector3.up, Vector3.zero);
                int left = r.RoundsFired[0], right = r.RoundsFired[1];
                r.FireGun(); for (int f = 0; f < 120; f++) r.Advance(1f / 60f);
                Assert.That(r.RoundsFired[0], Is.EqualTo(left), "a gun that came off still fired");
                Assert.That(r.RoundsFired[1], Is.GreaterThan(right + 8), "the gun still on it stopped firing");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Gatling_Aims_At_Its_Target_And_Its_Rounds_Land_Round_It()
        {
            // "target": the saddle turns to the point, each gun elevates to it (from its own trunnion), and the rounds of a
            // burst land within the spread of it. Aimed well off to the side and down at the ground 60 m out.
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).hopper);
            if (e == null) Assert.Ignore("no gatling in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var spawn = new Vector3(30f, 0f, -20f);
                var r = VehicleRig.Build(e, null, parent, spawn, 20f, 1.4f, 3); r.ForcedLod = 0; r.SetLod(0);
                var target = spawn + Quaternion.Euler(0f, 20f, 0f) * new Vector3(-40f, 0f, 45f);
                r.AimAt = target;
                for (int f = 0; f < 180; f++) r.Advance(1f / 60f);
                foreach (var s in new[] { "L", "R" })
                {
                    var gun = r.Find("Gun_" + s); var muzzle = r.Socket("Socket_Muzzle_" + s);
                    float off = Vector3.Angle(gun.T.forward, target - muzzle);
                    Assert.That(off, Is.LessThan(2f), $"Gun_{s} points {off:0.0} degrees off its target");
                }
                r.FireGun();
                float worst = 0f; int landed = 0;
                var line = (target - spawn); line.y = 0f; line.Normalize(); var side = new Vector3(line.z, 0f, -line.x);
                var acrossAt = new System.Collections.Generic.List<float>();
                for (int f = 0; f < 150; f++)
                {
                    int before = r.RoundsFired[0] + r.RoundsFired[1];
                    r.Advance(1f / 60f);
                    if (r.RoundsFired[0] + r.RoundsFired[1] == before) continue;
                    Assert.IsTrue(r.LastRoundLands, "a round aimed at the ground did not land");
                    worst = Mathf.Max(worst, Vector3.Distance(r.LastRoundTo, target)); landed++;
                    acrossAt.Add(Vector3.Dot(r.LastRoundTo - target, side));
                }
                Assert.That(landed, Is.GreaterThan(10), "the burst fired nothing");
                float range = Vector3.Distance(r.Socket("Socket_Muzzle_L"), target);
                // scattered Spread of the range across the line of fire and twice that along it, walked Sweep either side
                Assert.That(worst, Is.LessThan((VehicleRig.Spread * 2.3f + VehicleRig.Sweep) * range), "a round landed wide of the target");
                // and the burst walks across the target: its first rounds fall on one side, its last on the other (a burst
                // in one spot read as one blob 40 m off, critic g6)
                int q = acrossAt.Count / 4;
                float first = 0f, last = 0f; for (int k = 0; k < q; k++) { first += acrossAt[k] / q; last += acrossAt[acrossAt.Count - 1 - k] / q; }
                Assert.That(Mathf.Abs(last - first), Is.GreaterThan(VehicleRig.Sweep * range), "the burst did not walk across its target");
                r.AimAt = null;
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Gatling_Cook_Off_Throws_Each_Gun_Clear_Of_The_Body()
        {
            // twin guns on a small saddle: they come off on their own and land outside the body's footprint, not riding
            // the saddle down into the body (critic g1: both guns ended inside the toad, 2.2 m under where they stood)
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).hopper);
            if (e == null) Assert.Ignore("no gatling in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.4f, 3); r.ForcedLod = 0; r.SetLod(0); r.CookDelay = -1f;
                var hull = r.Find("Hull");
                var stood = new Dictionary<string, Vector3>();
                foreach (var s in new[] { "L", "R" }) { var g = r.Find("Gun_" + s); stood[s] = g.T.TransformPoint(g.Box.center); }
                r.KnockOut(); r.CookOff();
                for (int f = 0; f < 600; f++) r.Advance(1f / 60f);
                foreach (var s in new[] { "L", "R" })
                {
                    var g = r.Find("Gun_" + s);
                    Assert.IsTrue(g.Loose, $"Gun_{s} stayed on");
                    var c = r.transform.InverseTransformPoint(g.T.TransformPoint(g.Box.center));
                    var hb = hull.Box; var hc = hull.RestLocal + hb.center;
                    bool outside = Mathf.Abs(c.x - hc.x) > hb.extents.x || Mathf.Abs(c.z - hc.z) > hb.extents.z;
                    Assert.IsTrue(outside, $"Gun_{s} came down inside the body's footprint at {c}");
                    Assert.That(Lowest(g.T, g.Lods[0]), Is.GreaterThan(-0.3f), $"Gun_{s} lies under the ground");
                    // and near: thrown 7-10 m, a wreck's guns lay nearer the next machine than their own (critic g2, g3)
                    var at = g.T.TransformPoint(g.Box.center);
                    float far = new Vector2(at.x - stood[s].x, at.z - stood[s].z).magnitude;
                    Assert.That(far, Is.LessThan(5.5f), $"Gun_{s} was thrown {far:0.0} m from where it stood");
                }
                // the saddle, a sparse frame in a big box, stays on the wreck: thrown it toppled corner over corner 12.9 m off
                Assert.IsFalse(r.Find("Turret").Loose, "the saddle was thrown (it toppled 12.9 m away, critic g2)");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Hopper_Hops_Clear_Of_The_Ground_And_Lands_Back_On_Its_Feet()
        {
            // HopDrive: each hop lifts the whole body clear of the ground (a toad's feet leave it) and puts it back down at
            // its modelled height; it gets somewhere; knocked out it slumps but does not sink through the ground.
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).hopper);
            if (e == null) Assert.Ignore("no hopper in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var spawn = new Vector3(-150f, 0f, 40f);
                var r = VehicleRig.Build(e, null, parent, spawn, 0f, 1.4f, 3); r.ForcedLod = 0; r.SetLod(0); r.CookDelay = -1f;
                var hull = r.Find("Hull");
                // its legs are skinned (Bullfrog_legs.json matched its hull; a mismatch leaves them still with a warning)
                Assert.IsTrue(r.Hopper.HasLegs, "the hopper's legs file did not match its hull: run Tools/legrig.py (docs/22)");
                float rest = Lowest(hull.T, hull.Lods[0]);
                Assert.That(rest, Is.InRange(-0.1f, 0.1f), "standing, the toad's feet are not on the ground");
                float top = Highest(hull.T, hull.Lods[0]);
                var gun = r.Find("Gun_L");
                r.Hopper.Speed = 2f;
                float high = float.MinValue, low = float.MaxValue, flat = float.MaxValue, tall = float.MinValue, shear = 0f, sadLo = 0f, sadHi = 0f;
                // the legs: the body's own footprint, back to front, in its frame (Lods[0] is the rig's bent copy)
                float Reach(bool front) { float z = front ? float.MinValue : float.MaxValue; foreach (var v in hull.Lods[0].vertices) if (v.y < hull.Box.min.y + 0.1f * hull.Box.size.y) z = front ? Mathf.Max(z, v.z) : Mathf.Min(z, v.z); return z; }
                float back0 = Reach(false), front0 = Reach(true), backMost = back0, frontMost = front0, pushed = 0f, sunk = 0f;
                for (int f = 0; f < 360; f++)
                {
                    r.Advance(1f / 60f);
                    float y = Lowest(hull.T, hull.Lods[0]);
                    high = Mathf.Max(high, y); low = Mathf.Min(low, y);
                    // the body's height over its own feet: squashed flat landing, stretched springing
                    float bh = Highest(hull.T, hull.Lods[0]) - y;
                    flat = Mathf.Min(flat, bh); tall = Mathf.Max(tall, bh);
                    var g = gun.T.lossyScale; shear = Mathf.Max(shear, Mathf.Max(Mathf.Abs(g.x - g.y), Mathf.Abs(g.z - g.y)) / g.y);
                    sadLo = Mathf.Min(sadLo, r.Hopper.SaddleOffset); sadHi = Mathf.Max(sadHi, r.Hopper.SaddleOffset);
                    if (f % 4 == 0) { backMost = Mathf.Min(backMost, Reach(false)); frontMost = Mathf.Max(frontMost, Reach(true)); }
                    if (!r.Hopper.InAir && r.Hopper.Extend > 0.3f) pushed = Mathf.Max(pushed, r.Hopper.Lift);
                    if (!r.Hopper.InAir) sunk = Mathf.Min(sunk, r.Hopper.Lift);
                }
                // on the ground the crouch and the landing let the body down over its planted feet, the legs folding under
                // it (planted, the body only ever rose, critic g13); the feet stay on the ground (below)
                Assert.That(sunk, Is.LessThan(-0.12f), "crouching and landing, the body did not sink over its feet");
                // in the air the hind feet trail back and the fore feet reach forward (tucked, a statue lifted: g8)
                Assert.That((back0 - backMost) * r.Size, Is.GreaterThan(0.25f), "the hind legs did not trail off the take-off");
                Assert.That((frontMost - front0) * r.Size, Is.GreaterThan(0.2f), "the forelegs did not reach for the landing");
                // and it pushes off: its hind legs unfold while its feet are still down and stand it up on them (it rose
                // with its legs folded and unfolded them only in the air, critic g10)
                Assert.That(pushed, Is.GreaterThan(0.3f), "the hind legs did not push it off the ground");
                // the saddle rides a spring: sags as the body springs, bounces on the landing (critic g6)
                Assert.That(sadLo, Is.LessThan(-0.03f), "the saddle did not sag as the body sprang up");
                Assert.That(sadHi, Is.GreaterThan(0.02f), "the saddle did not bounce back");
                // rigid legs: planted, the crouch and landing squat showed nothing, and three hop stills looked alike (g6)
                Assert.That(flat, Is.LessThan(0.9f * (top - rest)), "landing, the body did not squash");
                Assert.That(tall, Is.GreaterThan(1.06f * (top - rest)), "springing, the body did not stretch");
                Assert.That(shear, Is.LessThan(0.01f), "the hull's squash sheared the guns riding on it");
                Assert.That(r.Hopper.Landings, Is.GreaterThanOrEqualTo(4), "it did not hop");
                Assert.That(high, Is.GreaterThan(0.4f), "a hop did not lift its feet off the ground");
                Assert.That(low, Is.GreaterThan(rest - 0.05f), "crouching or landing, its feet went into the ground (the legs fold to let the body down)");
                Assert.That(r.Hopper.HopPos.magnitude, Is.GreaterThan(6f), "it hopped nowhere");
                var h = hull.T.position;
                Assert.That(new Vector2(h.x - spawn.x, h.z - spawn.z).magnitude, Is.LessThan(2f * r.Hopper.Radius + 5f), "it hopped somewhere other than where it was built");
                // standing and firing, the body leans back and shudders with the guns (nothing moved, critic g6)
                r.Hopper.Speed = 0f;
                // sitting, its throat swells (and the vertices under the chin move out with it)
                float swell = 0f, moveOut = 0f; var rest0 = hull.Lods[0].vertices;
                for (int f = 0; f < 240; f++)
                {
                    r.Advance(1f / 60f);
                    // (only above the legs: the weight shift moves the thighs)
                    if (r.Hopper.Throat > swell) { swell = r.Hopper.Throat; var now = hull.Lods[0].vertices; moveOut = 0f; for (int i = 0; i < now.Length; i++) if (rest0[i].y > 1.2f) moveOut = Mathf.Max(moveOut, (now[i] - rest0[i]).magnitude * r.Size); }
                }
                Assert.That(swell, Is.GreaterThan(0.9f), "sitting, its throat never swelled");
                Assert.That(moveOut, Is.GreaterThan(0.1f), "the throat swelled but no vertex under the chin moved");
                // a hit jolts it away from the blow
                var before = hull.T.localRotation;
                r.HitPart(r.Find("Hull"), 20f); float jolt = 0f, dropped = 0f;
                for (int f = 0; f < 20; f++) { r.Advance(1f / 60f); jolt = Mathf.Max(jolt, Quaternion.Angle(before, hull.T.localRotation)); dropped = Mathf.Min(dropped, r.Hopper.Lift); }
                Assert.That(jolt, Is.GreaterThan(2f), "a hit did not jolt it");
                // and drops it on its legs (a hit moved the body only up, critic g13)
                Assert.That(dropped, Is.LessThan(-0.03f), "a hit did not drop it on its legs");
                for (int f = 0; f < 60; f++) r.Advance(1f / 60f);
                var calm = hull.T.localRotation;
                var seat = r.Find("Turret");
                r.FireGun(); float moved = 0f, turned = 0f, bob = 0f, driven = 0f; Quaternion was = calm;
                for (int f = 0; f < 90; f++)
                {
                    r.Advance(1f / 60f);
                    var off = seat.T.localPosition - seat.RestLocal; off.y = 0f; driven = Mathf.Max(driven, off.magnitude * r.Size);
                    if (f > 30) bob = Mathf.Max(bob, Mathf.Abs(r.Hopper.SaddleOffset));
                    moved = Mathf.Max(moved, Quaternion.Angle(calm, hull.T.localRotation));
                    turned += Quaternion.Angle(was, hull.T.localRotation); was = hull.T.localRotation;
                }
                Assert.That(moved, Is.GreaterThan(1.5f), "firing, the body did not lean back against the guns");
                Assert.That(turned, Is.GreaterThan(20f), "firing, the body did not shudder");
                // but the saddle stays seated: the shudder rang its spring 0.1 m and the guns jumped between stills (g8)
                Assert.That(bob, Is.LessThan(0.02f), "firing standing, the saddle bobbed on its spring");
                // but it is driven back along its guns (firing had no weight, critic g13)
                Assert.That(driven, Is.GreaterThan(0.025f), "firing, the gun block was not driven back");
                // one hit on a barrel set while it is still healthy scars it but does not knock it off (the first hit took half
                // its firepower away at 85 % health, critic g13)
                r.HitPart(r.Find("Barrels_L"), 15f); r.Advance(1f / 60f);
                Assert.IsFalse(r.Find("Barrels_L").Loose, "a healthy gatling lost a barrel set to its first hit");
                Assert.That(Lowest(hull.T, hull.Lods[0]), Is.GreaterThan(rest - 0.05f), "firing, its feet went into the ground");
                r.KnockOut();
                float kicked = 0f, downLo = float.MaxValue, downHi = float.MinValue;
                for (int f = 0; f < 180; f++)
                {
                    r.Advance(1f / 60f); kicked = Mathf.Max(kicked, r.Hopper.Extend);
                    if (f >= 60) { downLo = Mathf.Min(downLo, r.Hopper.Lift); downHi = Mathf.Max(downHi, r.Hopper.Lift); }
                }
                Assert.That(kicked, Is.GreaterThan(0.6f), "knocked out, its hind legs never kicked");
                // down, it lies on its belly below its standing height, its legs laid out flat (propped on its sprawled legs
                // it stood 0.44 m higher, a table), and the kicks move only the legs (they dropped the body 0.21 m, g13)
                Assert.That(downHi, Is.LessThan(-0.03f), "knocked out, its legs held it up off its belly");
                Assert.That(downHi - downLo, Is.LessThan(0.01f), "knocked out, the kicks moved the whole body");
                float dead = Lowest(hull.T, hull.Lods[0]);
                // over onto a side with that corner dug 5 cm in: not sunk (0.22 m down put its feet through the floor)
                Assert.That(dead, Is.InRange(-0.08f, 0.01f), "knocked out, it did not settle onto the ground, or sank into it");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void A_Flyer_Hovers_And_Falls_When_Knocked_Out()
        {
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).flyer);
            if (e == null) Assert.Ignore("no flyer in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.4f, 3); r.CookDelay = -1f;
                for (int f = 0; f < 60; f++) r.Advance(1f / 60f);
                Assert.That(r.Find("Hull").T.position.y, Is.GreaterThan(10f), "it is not flying");
                r.KnockOut();
                for (int f = 0; f < 360; f++) r.Advance(1f / 60f);
                var hull = r.Find("Hull");
                Assert.That(Lowest(hull.T, hull.Lods[0]), Is.LessThan(0.6f).And.GreaterThan(-1.5f), "knocked out, it came down onto the ground");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
    
        [Test]
        public void A_Vehicle_That_Loses_A_Wheel_Sits_Down_On_That_Corner_With_The_Rest_On_The_Ground()
        {
            // VehicleRig.Sag once rolled the wrong way: every wreck leaned onto the wheel it still had, into the mud (r38)
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).partList.Any(p => p.name == "Wheel_FL"));
            if (e == null) Assert.Ignore("no wheeled vehicle in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.7f, 3); r.CookDelay = -1f;
                var lost = r.Find("Wheel_FL"); r.Detach(lost, Vector3.zero, Vector3.zero);
                for (int f = 0; f < 90; f++) r.Advance(1f / 60f);
                var hull = r.Find("Hull"); var wl = r.Find("Wheel_RL"); var wr = r.Find("Wheel_FR");
                // the lost wheel's side is lower: the corner above it, against the same corner on the other side
                var cornerLost = hull.T.TransformPoint(lost.RestLocal + Vector3.up * lost.Box.extents.y * 2f);
                var cornerKept = hull.T.TransformPoint(new Vector3(-lost.RestLocal.x, lost.RestLocal.y, lost.RestLocal.z) + Vector3.up * lost.Box.extents.y * 2f);
                Assert.That(cornerLost.y, Is.LessThan(cornerKept.y - 0.05f), "it leaned away from the wheel it lost");
                foreach (var w in new[] { wl, wr })
                    Assert.That(Lowest(w.T, w.Lods[0]), Is.GreaterThan(-0.12f), w.Name + " sank into the ground");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
    
        [Test]
        public void Every_Machine_Keeps_Its_Thrown_Parts_Near()
        {
            // a cook-off's pieces land within 30 m on every machine in the bay (the gunship's engine landed 29 m off, r35),
            // and lie down flat on a face: the hovercraft's turret rested balanced on an edge and the tank's antenna 2 cm
            // over the standing-on-end limit (seed 5) until a piece that stops tilted is laid flat before it rests
            var bad = new List<string>();
            var parent = new GameObject("test stage").transform;
            try
            {
                foreach (var e in Lib().Vehicles)
                {
                    var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.7f, 5);
                    r.CookOff();
                    for (int f = 0; f < 900; f++) r.Advance(1f / 60f);
                    foreach (var p in r.Parts.Where(p => p.Loose))
                    {
                        // judged on the box it tumbles as (its principal axes where those are tighter: the antenna's
                        // axis-aligned box is three times its own, and it lay 34 degrees off its length, loop 3)
                        var body = p.Fly.Rot * p.BodyAxes;
                        var d = p.Body.size * r.Size; var dims = new[] { d.x, d.y, d.z }.OrderBy(x => x).ToArray();
                        float centre = (p.Fly.Pos + body * p.Body.center).y * r.Size;
                        float far = new Vector2(p.Fly.Pos.x, p.Fly.Pos.z).magnitude * r.Size;
                        if (far > 30f) bad.Add($"{e.Name} {p.Name} {far:0} m away");
                        if (centre > 0.5f * dims[1] + 0.2f) bad.Add($"{e.Name} {p.Name} on end: centre {centre:0.00} m up, dims {dims[0]:0.0}/{dims[1]:0.0}/{dims[2]:0.0}");
                        float up = Mathf.Max(Mathf.Abs((body * Vector3.right).y), Mathf.Abs((body * Vector3.up).y), Mathf.Abs((body * Vector3.forward).y));
                        if (!p.Fly.Resting) bad.Add($"{e.Name} {p.Name} still moving after 15 s");
                        else if (up < Mathf.Cos(3f * Mathf.Deg2Rad)) bad.Add($"{e.Name} {p.Name} resting tilted {Mathf.Acos(up) * Mathf.Rad2Deg:0} degrees off a face");
                    }
                    Object.DestroyImmediate(r.gameObject);
                }
                Assert.That(bad, Is.Empty, string.Join("; ", bad));
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
    }
}
