// Phase: A5c (2026-10-07, the standard for cutting models, PLAN_model_cutting.md on the Drive) — a walker cut to the
// standard is the model the standard says: its root part the Hull with its pivot on the origin, every leg a Thigh, a
// Shin and a Foot named by side and numbered from the rear, every joint on the piece it hangs from AS CUT (the file,
// nothing mended on loading), a toe socket on every sole, the legs the rules count, no deeper than three parts under
// the root, inside the part budget, the far model the near one's names and pivots. And it walks like it: the hull
// clear of the ground, a foot that is down neither sliding nor turning with its shin.
// The Banner is the first (the owner, 2026-10-07: "Only the Banner first: cut it, film it, then decide the rest");
// a walker cut next is one more row in Cut and in TankModel.HeldFeet.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class ModelCutTests
    {
        static readonly (string name, byte archetype)[] Cut = { ("Banner", VehicleArchetype.Banner) };

        /// <summary>A joint within this of its parent's surface is on it: 5 cm at the size the walkers are drawn.</summary>
        const float JointTol = 0.05f;
        const int NearBudget = 30, FarBudget = 18;

        static readonly string[] StandardNames = { "Hull", "Thigh", "Shin", "Foot", "Claw", "Jaw", "Turret", "Gun", "Shield", "Drum", "Reactor", "Banner" };

        static TankModel Load(string name, byte archetype)
        {
            var m = TankModel.Load(name, archetype, "Hull", VehicleSize.Walker);
            Assert.NotNull(m, $"{name} did not load with a Hull for its root");
            return m;
        }

        static bool Limb(TankPartRole r) => r == TankPartRole.Thigh || r == TankPartRole.Shin || r == TankPartRole.Foot;

        /// <summary>How far a point stands off a mesh: the distance to the nearest triangle, less than nothing when it
        /// lies behind that triangle (inside a closed piece, as the middle of a ball joint does).</summary>
        static float Off(Mesh mesh, Vector3 at)
        {
            var v = mesh.vertices; var t = mesh.triangles;
            float best = float.MaxValue; bool inside = false;
            for (int k = 0; k + 2 < t.Length; k += 3)
            {
                Vector3 a = v[t[k]], b = v[t[k + 1]], c = v[t[k + 2]];
                Vector3 n = Vector3.Cross(b - a, c - a);
                if (n.sqrMagnitude < 1e-12f) continue;
                Vector3 p = Nearest(at, a, b, c);
                float d = (p - at).magnitude;
                if (d < best - 1e-5f) { best = d; inside = Vector3.Dot(at - p, n) < 0f; }
            }
            return inside ? -best : best;
        }

        /// <summary>Ericson, Real-Time Collision Detection 5.1.5: the point of triangle abc nearest p.</summary>
        static Vector3 Nearest(Vector3 p, Vector3 a, Vector3 b, Vector3 c)
        {
            Vector3 ab = b - a, ac = c - a, ap = p - a;
            float d1 = Vector3.Dot(ab, ap), d2 = Vector3.Dot(ac, ap);
            if (d1 <= 0f && d2 <= 0f) return a;
            Vector3 bp = p - b; float d3 = Vector3.Dot(ab, bp), d4 = Vector3.Dot(ac, bp);
            if (d3 >= 0f && d4 <= d3) return b;
            float vc = d1 * d4 - d3 * d2;
            if (vc <= 0f && d1 >= 0f && d3 <= 0f) return a + ab * (d1 / (d1 - d3));
            Vector3 cp = p - c; float d5 = Vector3.Dot(ab, cp), d6 = Vector3.Dot(ac, cp);
            if (d6 >= 0f && d5 <= d6) return c;
            float vb = d5 * d2 - d1 * d6;
            if (vb <= 0f && d2 >= 0f && d6 <= 0f) return a + ac * (d2 / (d2 - d6));
            float va = d3 * d6 - d5 * d4;
            if (va <= 0f && (d4 - d3) >= 0f && (d5 - d6) >= 0f) return b + (c - b) * ((d4 - d3) / ((d4 - d3) + (d5 - d6)));
            float denom = 1f / (va + vb + vc);
            return a + ab * (vb * denom) + ac * (vc * denom);
        }

        [Test]
        public void NamesAndCountsFollowTheStandard()
        {
            foreach (var (name, archetype) in Cut)
            {
                var m = Load(name, archetype);
                var near = m.Lods[0].Parts; var far = m.Lods[1].Parts;
                Assert.AreEqual("Hull", near[0].Name, $"{name}: the root part");
                Assert.Less(near[0].Local.magnitude, 1e-4f, $"{name}: the root part's pivot is on the origin");
                Assert.LessOrEqual(near.Count, NearBudget, $"{name}: parts near");
                Assert.LessOrEqual(far.Count, FarBudget, $"{name}: parts far");
                foreach (var p in near)
                {
                    string kind = p.Name.Split('_')[0];
                    CollectionAssert.Contains(StandardNames, kind, $"{name}: {p.Name} is not a part the standard names");
                    int depth = 0;
                    for (int k = p.Parent; k >= 0; k = near[k].Parent) depth++;
                    Assert.LessOrEqual(depth, 3, $"{name}: {p.Name} hangs {depth} parts under the root");
                    if (Limb(p.Role))
                    {
                        // Thigh_L2: the side, then the leg's number from the rear
                        string end = p.Name.Substring(p.Name.IndexOf('_') + 1);
                        Assert.IsTrue(end.Length == 2 && (end[0] == 'L' || end[0] == 'R') && char.IsDigit(end[1]), $"{name}: {p.Name} is not named by side and number");
                    }
                }
                // the far model is the near one's names and pivots, never other parts
                foreach (var p in far)
                {
                    int k = m.Lods[0].Find(p.Name);
                    Assert.GreaterOrEqual(k, 0, $"{name}: the far model has a part {p.Name} the near one lacks");
                    Assert.Less((near[k].Local - p.Local).magnitude, 1e-3f, $"{name}: {p.Name} has another pivot far than near");
                }
                // a socket is a leaf: TankModel reads it as a point on a part, and every one it found names a part
                foreach (var s in m.Sockets) Assert.IsTrue(s.Value.part >= 0 && s.Value.part < near.Count, $"{name}: {s.Key}");
            }
        }

        [Test]
        public void EveryWalkerHasTheLegsTheRulesCount()
        {
            foreach (var (name, archetype) in Cut)
            {
                var m = Load(name, archetype);
                var parts = m.Lods[0].Parts;
                int legs = VehicleProfile.ForArchetype(archetype).Legs;
                Assert.AreEqual(legs, m.LegCount, $"{name}: thighs in the model against the legs the rules count");
                Assert.AreEqual(legs / 2, m.LegsPerSide, $"{name}: as many a side");
                for (int leg = 0; leg < legs; leg++)
                {
                    var rig = m.Lods[0].Legs[leg];
                    Assert.NotNull(rig, $"{name} leg {leg}");
                    Assert.AreEqual(3, rig.Chain.Length, $"{name} leg {leg}: thigh, shin, foot");
                    string end = parts[rig.Chain[0]].Name.Substring(6);
                    Assert.AreEqual("Thigh_" + end, parts[rig.Chain[0]].Name);
                    Assert.AreEqual("Shin_" + end, parts[rig.Chain[1]].Name);
                    Assert.AreEqual("Foot_" + end, parts[rig.Chain[2]].Name);
                    Assert.IsTrue(rig.Held, $"{name} leg {leg}: a leg cut to the standard holds its foot");
                    // numbered from the rear: the lower number stands further back
                    if (leg % m.LegsPerSide > 0)
                        Assert.Greater(rig.Hip.z + rig.Rest.z, m.Lods[0].Legs[leg - 1].Hip.z + m.Lods[0].Legs[leg - 1].Rest.z, $"{name}: leg {end} stands ahead of the one numbered before it");
                    Assert.AreEqual(leg < m.LegsPerSide ? -1f : 1f, Mathf.Sign(rig.Hip.x), $"{name} leg {end}: the left side first, as the sim takes legs off");
                }
                // left mirrors right, joint for joint
                for (int leg = 0; leg < m.LegsPerSide; leg++)
                {
                    var l = m.Lods[0].Legs[leg]; var r = m.Lods[0].Legs[leg + m.LegsPerSide];
                    for (int k = 0; k < 3; k++)
                    {
                        Vector3 a = parts[l.Chain[k]].Local, b = parts[r.Chain[k]].Local;
                        Assert.Less((new Vector3(-a.x, a.y, a.z) - b).magnitude, 2e-3f, $"{name}: {parts[l.Chain[k]].Name} does not mirror {parts[r.Chain[k]].Name}");
                    }
                }
            }
        }

        [Test]
        public void EveryJointLiesOnItsParentAsCut()
        {
            foreach (var (name, archetype) in Cut)
            {
                CollectionAssert.DoesNotContain(TankModel.JoinedLegs, name, $"{name}: nothing mends its joints on loading; this is the file");
                var m = Load(name, archetype);
                var parts = m.Lods[0].Parts;
                int joints = 0;
                foreach (var p in parts)
                {
                    if (!Limb(p.Role) || p.Parent < 0) continue;
                    joints++;
                    float off = Off(parts[p.Parent].Mesh, p.Local);
                    Assert.Less(off, JointTol, $"{name} {p.Name}: its joint stands {off:0.000} m off {parts[p.Parent].Name}");
                    // and not lost deep inside it either: a joint is where two pieces meet (a ball's middle at most)
                    Assert.Greater(off, -0.45f, $"{name} {p.Name}: its joint lies {-off:0.000} m inside {parts[p.Parent].Name}");
                }
                Assert.AreEqual(m.LegCount * 3, joints, $"{name}: a hip, a knee and an ankle a leg");
            }
        }

        [Test]
        public void EveryToeSocketSitsOnTheSole()
        {
            foreach (var (name, archetype) in Cut)
            {
                var m = Load(name, archetype);
                var parts = m.Lods[0].Parts;
                int found = 0;
                foreach (var rig in m.Lods[0].Legs)
                {
                    int foot = rig.Chain[rig.Chain.Length - 1];
                    string socket = "Socket_Toe_" + parts[foot].Name.Substring(5);
                    Assert.IsTrue(m.Sockets.TryGetValue(socket, out var s), $"{name}: {parts[foot].Name} has no {socket}");
                    Assert.AreEqual(foot, s.part, $"{name}: {socket} hangs under its own foot");
                    float low = float.MaxValue;
                    foreach (var v in parts[foot].Mesh.vertices) low = Mathf.Min(low, v.y);
                    Assert.Less(Mathf.Abs(s.local.y - low), 0.02f, $"{name}: {socket} stands {s.local.y - low:0.000} m off the sole");
                    // and the sole is on the ground the machine was modelled standing on
                    Assert.Less(Mathf.Abs(rig.Hip.y + rig.Rest.y), 0.03f, $"{name}: {parts[foot].Name}'s toe is modelled {rig.Hip.y + rig.Rest.y:0.000} m off the ground");
                    found++;
                }
                Assert.AreEqual(m.LegCount, found);
            }
        }

        /// <summary>Walk it on the level and hand every tick's posed parts to the caller.</summary>
        static void Walk(TankModel m, float speed, float yawRate, float seconds, System.Action<WalkerGait, Matrix4x4, Matrix4x4[], float> each)
        {
            var lod = m.Lods[0];
            var local = new Matrix4x4[lod.Parts.Count]; var solved = new bool[lod.Parts.Count]; var world = new Matrix4x4[lod.Parts.Count];
            var gait = new WalkerGait();
            const float dt = 1f / 60f;
            Vector3 pos = new Vector3(20f, 0f, 20f);
            float yaw = 0f;
            gait.Plant(m, pos, yaw, (x, z) => 0f);
            for (float t = 0f; t < seconds; t += dt)
            {
                yaw += yawRate * dt;
                Vector3 vel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)) * speed;
                pos += vel * dt;
                gait.Step(m, pos, yaw, vel, yawRate, 0, false, dt, (x, z) => 0f);
                var root = Matrix4x4.TRS(new Vector3(pos.x, gait.Height, pos.z), gait.Carriage(yaw), Vector3.one);
                gait.Solve(lod, root, local, solved);
                for (int i = 0; i < lod.Parts.Count; i++)
                {
                    var p = lod.Parts[i];
                    var l = solved[i] ? local[i] : Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                    world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * l;
                }
                each(gait, root, world, t);
            }
        }

        [Test]
        public void EveryWalkerCarriesItsBellyClearOfTheGround()
        {
            foreach (var (name, archetype) in Cut)
            {
                var m = Load(name, archetype);
                var lod = m.Lods[0];
                // everything that is not a leg: the hull and what stands on it
                var body = new List<(int part, Vector3[] verts)>();
                for (int i = 0; i < lod.Parts.Count; i++)
                    if (!Limb(lod.Parts[i].Role)) body.Add((i, lod.Parts[i].Mesh.vertices));
                float lowest = float.MaxValue;
                float speed = RosterSpeed(archetype);
                Walk(m, speed, 0f, 6f, (gait, root, world, t) =>
                {
                    foreach (var (part, verts) in body)
                        for (int k = 0; k < verts.Length; k += 3) lowest = Mathf.Min(lowest, world[part].MultiplyPoint3x4(verts[k]).y);
                });
                TestContext.Out.WriteLine($"{name} at {speed} m/s: the lowest of its body {lowest:0.00} m over the ground");
                Assert.Greater(lowest, 0.5f, $"{name}: walking the level at {speed} m/s its body came down to {lowest:0.00} m over the ground");
            }
        }

        /// <summary>The Banner's top, cut off its guns (the owner, 2026-10-08: the top "should also be able to
        /// rotate", the guns "need to be cut so they can animate"). A Turret with its pivot on the turning axis, a
        /// Gun_Upper and a Gun_Lower hanging off it with their pivots at their mounts, and the cut lost nothing: the
        /// near model still holds the vertices, triangles and size the Banner had in the game before it (crabs.json
        /// at a0b98135), and no vertex position sits in two parts.
        /// No Shield: there is no plate in the sculpt (leg 02 looked - the only plate-sized piece at the back is a
        /// strap round a canister), and the standard forbids inventing one. The shield waits on the owner.</summary>
        [Test]
        public void TheTopAndTheGunsAreCutAsTheirOwnParts()
        {
            var m = Load("Banner", VehicleArchetype.Banner);
            var lod = m.Lods[0]; var parts = lod.Parts;

            int turret = lod.Find("Turret"), upper = lod.Find("Gun_Upper"), lower = lod.Find("Gun_Lower");
            Assert.GreaterOrEqual(turret, 0, "Banner: no Turret - its top is not cut off the hull");
            Assert.GreaterOrEqual(upper, 0, "Banner: no Gun_Upper - its guns are not cut off the top");
            Assert.GreaterOrEqual(lower, 0, "Banner: no Gun_Lower - its guns are not cut off the top");
            Assert.AreEqual(TankPartRole.Turret, parts[turret].Role, "Banner: Turret's role");
            Assert.AreEqual(TankPartRole.Gun, parts[upper].Role, "Banner: Gun_Upper's role");
            Assert.AreEqual(TankPartRole.Gun, parts[lower].Role, "Banner: Gun_Lower's role");
            Assert.AreEqual("Hull", parts[parts[turret].Parent].Name, "Banner: the Turret hangs off the Hull");
            Assert.AreEqual(turret, parts[upper].Parent, "Banner: Gun_Upper hangs off the Turret");
            Assert.AreEqual(turret, parts[lower].Parent, "Banner: Gun_Lower hangs off the Turret");

            // the top turns about one axis, so its pivot is ON that axis: on the machine's centre line
            Assert.Less(Mathf.Abs(parts[turret].Local.x), 0.002f, $"Banner: the Turret's pivot stands {parts[turret].Local.x * 1000f:0.0} mm off the turning axis in x");
            Assert.Less(Mathf.Abs(parts[turret].Local.z), 0.002f, $"Banner: the Turret's pivot stands {parts[turret].Local.z * 1000f:0.0} mm off the turning axis in z");
            float ringOff = Off(parts[parts[turret].Parent].Mesh, parts[turret].Local);
            // on the Hull means on it or in it: the axis runs down through the jar, so this pivot sits 0.47 m inside
            // the body it turns on. The guns, below, carry the 0.45 m bound the standard gives a joint between pieces.
            Assert.Less(ringOff, JointTol, $"Banner: the Turret's pivot stands {ringOff:0.000} m off the Hull");

            // a gun is animated about its mount, so its pivot is where it meets the top - not somewhere in its barrel
            foreach (int g in new[] { upper, lower })
            {
                float onTop = Off(parts[turret].Mesh, parts[g].Local);
                Assert.Less(onTop, JointTol, $"Banner: {parts[g].Name}'s pivot stands {onTop:0.000} m off the Turret");
                float inOwn = Off(parts[g].Mesh, Vector3.zero);
                Assert.Greater(inOwn, -0.45f, $"Banner: {parts[g].Name}'s pivot lies {-inOwn:0.000} m inside its own barrel");
                // both barrels are the one gun the sim gives the Banner, and they elevate with the top, not alone
                Assert.AreEqual(0, parts[g].Gun, $"Banner: {parts[g].Name} is gun {parts[g].Gun}");
                Assert.IsFalse(parts[g].SelfAimed, $"Banner: {parts[g].Name} aims itself, though it hangs off the Turret");
            }
            Assert.IsTrue(m.Sockets.TryGetValue("Socket_Muzzle", out var muzzle), "Banner: no Socket_Muzzle");
            Assert.AreEqual(TankPartRole.Gun, parts[muzzle.part].Role, $"Banner: Socket_Muzzle hangs off {parts[muzzle.part].Name}, not a gun");

            // nothing lost and nothing doubled: the Banner the game had before this cut (crabs.json at a0b98135)
            int verts = 0, tris = 0;
            var bounds = new Bounds(Vector3.zero, Vector3.zero); bool any = false;
            var seen = new Dictionary<long, string>();
            var world = new Vector3[parts.Count];
            for (int i = 0; i < parts.Count; i++) world[i] = parts[i].Parent >= 0 ? world[parts[i].Parent] + parts[i].Local : parts[i].Local;
            for (int i = 0; i < parts.Count; i++)
            {
                var mesh = parts[i].Mesh;
                Assert.NotNull(mesh, $"Banner: {parts[i].Name} has no mesh");
                verts += mesh.vertexCount; tris += mesh.triangles.Length / 3;
                var own = new HashSet<long>();
                foreach (var v in mesh.vertices)
                {
                    Vector3 w = world[i] + v;
                    if (any) bounds.Encapsulate(w); else { bounds = new Bounds(w, Vector3.zero); any = true; }
                    // 0.1 mm in machine space, the grid the sculpt was built on
                    long key = ((long)Mathf.RoundToInt(w.x * 10000f) * 1000003L + Mathf.RoundToInt(w.y * 10000f)) * 1000003L + Mathf.RoundToInt(w.z * 10000f);
                    if (!own.Add(key)) continue;   // a seam doubles a vertex inside one part; that is the importer's
                    Assert.IsFalse(seen.TryGetValue(key, out string had), $"Banner: the vertex at {w} lies in both {had} and {parts[i].Name}");
                    seen[key] = parts[i].Name;
                }
            }
            TestContext.Out.WriteLine($"Banner near: {parts.Count} parts, {seen.Count} vertex positions ({verts} vertices as Unity holds them), {tris} triangles, {bounds.size / VehicleSize.Walker} m in the sculpt");
            // 1021 is what Blender counts, and what crabs.json records. Unity's importer splits a vertex wherever the
            // normals or the UVs either side of it differ, so the same model loads here as 2170 vertices: the number
            // that can be compared with the sculpt is how many POSITIONS there are, and the cut must not move it.
            Assert.AreEqual(1021, seen.Count, "Banner near: vertex positions against the Banner the game had at a0b98135");
            Assert.AreEqual(1745, tris, "Banner near: triangles against the Banner the game had at a0b98135");
            Vector3 size = bounds.size / VehicleSize.Walker;
            var was = new Vector3(2.0271f, 2.8173f, 3.3983f);     // crabs.json size_m at a0b98135
            Assert.Less((size - was).magnitude, 1e-3f, $"Banner near: it measures {size} where it measured {was}");
        }

        // ------------------------------------------------------------------ the parts move
        // The owner, 2026-10-08: the top "should also be able to rotate", the guns "need to be cut so they can
        // animate". These two pose the real parts with the very code that draws them (TankRenderer.PartLocal), so a
        // test cannot pass while the battle looks wrong. The view and that method are TankRenderer's own, read by
        // reflection as WalkerStills and WreckStills already read its views: the plan asked for them to be made
        // public, which C# will not allow without dragging Spring, DriveStyle and Debris out with them.
        // No shield test: there is no plate in the sculpt (leg 02 looked), and the standard forbids inventing one.
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>A TankRenderer and a view of its own kind holding this model. Awake never runs in EditMode, so its
        /// knobs keep their defaults (weightOn and calibreRecoil both true, the battle's own setting).</summary>
        static (TankRenderer r, object v) Rig(TankModel m, byte archetype)
        {
            var go = new GameObject("TankRendererUnderTest");
            var r = go.AddComponent<TankRenderer>();
            var view = r.GetType().GetNestedType("View", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            Assert.NotNull(view, "TankRenderer has no View");
            object v = System.Activator.CreateInstance(view, true);
            view.GetField("Model", Any).SetValue(v, m);
            view.GetField("Archetype", Any).SetValue(v, archetype);
            return (r, v);
        }

        static float[] Arr(object v, string field) => (float[])v.GetType().GetField(field, Any).GetValue(v);

        /// <summary>The part's matrix in its parent's frame, as TankRenderer draws it this instant.</summary>
        static Matrix4x4 Posed(TankRenderer r, object v, TankModel m, int part)
        {
            var mi = r.GetType().GetMethod("PartLocal", Any);
            Assert.NotNull(mi, "TankRenderer has no PartLocal");
            return (Matrix4x4)mi.Invoke(r, new object[] { v, m.Lods[0].Parts[part], part });
        }

        /// <summary>How far a posed part has been turned about the machine's up, in degrees.</summary>
        static float Yaw(Matrix4x4 posed, TankModel.Part p)
            => Vector3.SignedAngle(p.LocalRot * Vector3.forward, posed.MultiplyVector(Vector3.forward), Vector3.up);

        /// <summary>The top turns toward what the Banner shoots at and comes back to the front when it has none
        /// (the owner, 2026-10-08). The sim already says where to point: TankGunnery lays Gun0 at the target, inside
        /// the gun's arc and no faster than its traverse rate, and the show only draws that. So this test lays the gun
        /// the way the sim does and reads the Turret's rotation back out of the code that draws it.
        /// 90 degrees off the nose is NOT reached: TankSpec.Banner's arc is 38 degrees either side and widening it
        /// would be a sim change. The top goes to its stop and the hull brings the rest of the way.</summary>
        [Test]
        public void TheTopTurnsToTheTargetAndComesBack()
        {
            var m = Load("Banner", VehicleArchetype.Banner);
            var lod = m.Lods[0];
            int turret = lod.Find("Turret"), upper = lod.Find("Gun_Upper"), lower = lod.Find("Gun_Lower");
            var (r, v) = Rig(m, VehicleArchetype.Banner);
            var gun = TankSpec.For(VehicleArchetype.Banner).Gun(0);
            float arc = gun.ArcHalf * Mathf.Rad2Deg, rate = gun.TraverseRate * Mathf.Rad2Deg;
            Assert.AreEqual(38f, arc, 0.01f, "TankSpec.Banner's arc; the top cannot be let past it from the show");
            var yaws = Arr(v, "GunYaw");

            // a target 90 degrees off the nose: the sim clamps it into the arc and traverses at the gun's rate
            float want = TankSpec.ClampToArc(gun, 90f * Mathf.Deg2Rad) * Mathf.Rad2Deg;
            Assert.AreEqual(arc, want, 0.01f, "the sim lays a 90 degree target at the arc's edge");
            const float dt = 1f / 60f;
            float half = 0f;
            for (float t = 0f; t < 3f; t += dt)
            {
                yaws[0] = Mathf.MoveTowardsAngle(yaws[0] * Mathf.Rad2Deg, want, rate * dt) * Mathf.Deg2Rad;
                float drawn = Yaw(Posed(r, v, m, turret), lod.Parts[turret]);
                if (half == 0f && Mathf.Abs(drawn) > arc * 0.5f) half = t;
                Assert.LessOrEqual(Mathf.Abs(drawn), arc + 0.5f, $"the top turned {drawn:0.0} degrees, past its {arc:0} degree stop, at {t:0.00} s");
            }
            float at = Yaw(Posed(r, v, m, turret), lod.Parts[turret]);
            Assert.AreEqual(arc, at, 0.5f, $"with a target 90 degrees off the nose the top came to {at:0.0} degrees, not its {arc:0} degree stop");
            // it reads at play zoom: it is most of the way there inside a second, not a creep
            Assert.Greater(half, 0.01f); Assert.Less(half, 1f, $"the top took {half:0.00} s to turn half its arc");
            TestContext.Out.WriteLine($"Banner's top: {arc:0} degree stop at {rate:0} deg/s, half way in {half:0.00} s, and the hull brings the other {90f - arc:0} degrees");

            // the guns hang off the top and do not yaw of their own: they are carried round by it
            foreach (int g in new[] { upper, lower })
                Assert.Less(Mathf.Abs(Yaw(Posed(r, v, m, g), lod.Parts[g])), 0.01f, $"{lod.Parts[g].Name} yawed by itself as well as with the top");

            // no target: the gun goes back to its rest yaw, and the top with it
            for (float t = 0f; t < 3f; t += dt)
                yaws[0] = Mathf.MoveTowardsAngle(yaws[0] * Mathf.Rad2Deg, gun.RestYaw * Mathf.Rad2Deg, rate * dt) * Mathf.Deg2Rad;
            float back = Yaw(Posed(r, v, m, turret), lod.Parts[turret]);
            Assert.AreEqual(0f, back, 0.1f, $"with nothing to shoot at the top stayed at {back:0.0} degrees");
            Object.DestroyImmediate(r.gameObject);
        }

        /// <summary>The guns animate (the owner, 2026-10-08): the shot the sim fires drives a recoil back along each
        /// barrel, and the sim's aim raises both. TankRenderer sets Recoil to 1 on the sim's shot event and runs it
        /// down; this reads the pose that comes out at the two ends of that.</summary>
        [Test]
        public void AShotRecoilsBothBarrelsAlongThemselves()
        {
            var m = Load("Banner", VehicleArchetype.Banner);
            var lod = m.Lods[0];
            int upper = lod.Find("Gun_Upper"), lower = lod.Find("Gun_Lower");
            var (r, v) = Rig(m, VehicleArchetype.Banner);
            var recoil = Arr(v, "Recoil"); var pitch = Arr(v, "GunPitch");

            // at rest a barrel is drawn exactly where it was cut
            foreach (int g in new[] { upper, lower })
            {
                Vector3 at = Posed(r, v, m, g).GetColumn(3);
                Assert.Less((at - lod.Parts[g].Local).magnitude, 1e-5f, $"{lod.Parts[g].Name} is not on its pivot with no shot and no pitch");
            }
            // the shot, run down the way TankRenderer runs it (Recoil 1 at the shot, decaying at 1 / ReturnT): back
            // along the barrel's own length, both of them, and far enough to read. The peak is NOT at Recoil 1 -
            // HullRide.Kick snaps the barrel back over its first 35 ms - so the pose is read all the way through.
            const float dt = 1f / 60f, ret = 0.65f;        // View.ReturnT's default, every gun's today
            var peak = new float[2]; var home = new float[2];
            recoil[0] = 1f;
            for (float t = 0f; t < 1.5f; t += dt)
            {
                for (int i = 0; i < 2; i++)
                {
                    int g = i == 0 ? upper : lower;
                    var p = lod.Parts[g];
                    Vector3 own = Quaternion.Inverse(p.LocalRot) * ((Vector3)Posed(r, v, m, g).GetColumn(3) - p.Local);
                    Assert.LessOrEqual(own.z, 1e-5f, $"{p.Name} ran FORWARD out of its mount at {t:0.00} s: {own}");
                    Assert.Less(new Vector2(own.x, own.y).magnitude, 0.01f, $"{p.Name} moved sideways on a shot at {t:0.00} s: {own}");
                    peak[i] = Mathf.Max(peak[i], -own.z);
                    if (home[i] == 0f && t > 0.1f && -own.z < 0.005f) home[i] = t;
                }
                recoil[0] = Mathf.Max(0f, recoil[0] - dt / ret);
            }
            for (int i = 0; i < 2; i++)
            {
                string name = lod.Parts[i == 0 ? upper : lower].Name;
                Assert.Greater(peak[i], 0.03f, $"{name} came back {peak[i] * 100f:0.0} cm at most on a shot: too little to see");
                Assert.Greater(home[i], 0f, $"{name} never came back into battery");
                Assert.Less(home[i], 1f, $"{name} took {home[i]:0.00} s to come back into battery");
            }
            TestContext.Out.WriteLine($"Banner's barrels recoil {peak[0] * 100f:0.0} cm and are back in battery in {home[0]:0.00} s");
            // and with no shot at all it is home: exactly on its pivot
            recoil[0] = 0f;
            foreach (int g in new[] { upper, lower })
                Assert.Less(((Vector3)Posed(r, v, m, g).GetColumn(3) - lod.Parts[g].Local).magnitude, 1e-5f, $"{lod.Parts[g].Name} did not come back off its recoil");

            // the sim's elevation raises both barrels about their mounts, nose up
            pitch[0] = 20f * Mathf.Deg2Rad;
            foreach (int g in new[] { upper, lower })
            {
                var p = lod.Parts[g];
                Vector3 was = (p.LocalRot * Vector3.forward).normalized;
                Vector3 nose = Posed(r, v, m, g).MultiplyVector(Vector3.forward).normalized;
                float up = Vector3.SignedAngle(was, nose, Vector3.Cross(Vector3.up, was).normalized);
                Assert.AreEqual(20f, Mathf.Abs(up), 1f, $"{p.Name} turned {up:0.0} degrees where the sim laid 20 of elevation");
                Assert.Greater(nose.y, was.y, $"{p.Name} dipped where the sim laid 20 degrees of elevation");
            }
            TestContext.Out.WriteLine("Banner's guns: both barrels recoil along themselves on the sim's shot and elevate with its aim");
            Object.DestroyImmediate(r.gameObject);
        }

        static float RosterSpeed(byte archetype) => archetype == VehicleArchetype.Banner ? RosterEntry.Banner.Speed : 2f;

        [Test]
        public void APlantedFootKeepsItsSoleOnTheGround()
        {
            foreach (var (name, archetype) in Cut)
            {
                var m = Load(name, archetype);
                var lod = m.Lods[0];
                foreach (var (speed, yawRate) in new[] { (RosterSpeed(archetype), 0f), (1.2f, 0.4f) })
                {
                    int n = lod.Legs.Length;
                    var downAt = new Vector3[n]; var was = new bool[n];
                    float slide = 0f, tip = 0f; int planted = 0, upright = 0; string worst = "";
                    Walk(m, speed, yawRate, 8f, (gait, root, world, t) =>
                    {
                        for (int i = 0; i < n; i++)
                        {
                            var rig = lod.Legs[i];
                            int foot = rig.Chain[2];
                            bool down = gait.Feet[i].Swing < 0f;
                            Vector3 toe = world[foot].MultiplyPoint3x4(rig.Toe);
                            if (down && !was[i]) downAt[i] = toe;
                            was[i] = down;
                            if (!down || t < 1f) continue;
                            planted++;
                            // the toe the leg DRAWS, against where it was drawn when the foot came down
                            float d = Vector3.Distance(toe, downAt[i]);
                            if (d > slide) { slide = d; worst = $"leg {i} at {t:0.00} s"; }
                            // and the foot itself: how far its own up has tipped from the way it was modelled
                            float lean = Vector3.Angle(world[foot].MultiplyVector(Quaternion.Inverse(rig.RestRot[2]) * Vector3.up), Vector3.up);
                            tip = Mathf.Max(tip, lean);
                            if (lean < 2f) upright++;
                        }
                    });
                    TestContext.Out.WriteLine($"{name} at {speed} m/s, turning {yawRate}: {planted} planted ticks, a toe moved {slide * 100f:0.00} cm at most, upright {upright * 100f / Mathf.Max(1, planted):0} % of them, tipped {tip:0.0} degrees at most");
                    Assert.Greater(planted, 200, $"{name}: feet were down");
                    Assert.Less(slide, 0.03f, $"{name} at {speed} m/s, turning {yawRate}: a planted toe moved {slide * 100f:0.0} cm on the ground ({worst})");
                    // it may roll off its toe as the leg runs out, and no more: most of a stance it stands as modelled
                    Assert.Greater(upright, planted * 0.7f, $"{name} at {speed} m/s, turning {yawRate}: its feet stood upright for {upright} of {planted} planted ticks (the most a foot tipped: {tip:0} degrees)");
                    Assert.Less(tip, 50f, $"{name} at {speed} m/s, turning {yawRate}: a planted foot tipped {tip:0} degrees");
                }
            }
        }
    }
}
