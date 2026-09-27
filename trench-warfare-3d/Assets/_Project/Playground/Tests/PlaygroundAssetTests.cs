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
