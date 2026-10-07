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
