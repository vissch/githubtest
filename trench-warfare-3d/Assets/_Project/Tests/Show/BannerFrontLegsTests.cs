// Phase: A5c / banner-legs-and-fire (2026-10-13) — the owner, on the Banner's day film: "the legs are still clipping
// through eachother, the front legs i mean". Judged by eye for three cycles and never measured, so this measures it,
// on the real geometry: the smallest distance between the two front legs' MESHES, posed by WalkerGait tick by tick.
//
// Two cheaper measures were tried first and are recorded here so nobody spends the afternoon on them again:
//   - Capsules down the bone polylines, each part's radius its smallest mesh-bounds extent: -0.654 m on EVERY tick
//     of all three cases. The thighs' hips are 0.90 m apart and that "radius" is 0.772 m each, so it reads a model
//     constant and nothing about the walk.
//   - Signed oriented-box separation (15-axis separating-axis test) of the posed mesh bounds: -2.289 m at worst,
//     which looks damning until the worst tick is printed. There the two legs are exactly symmetric, hips at x
//     19.56 and 20.45 and knees at 18.53 and 21.47 — nearly three metres apart, nowhere near each other. The
//     Banner's leg parts are near-cubic blocks (Thigh_L2 extents 0.80, 0.85, 0.77 with its bounds centre 0.61 m off
//     its own pivot), so their boxes overlap round the hips while the legs themselves stand wide. A box is not a leg.
//
// What is measured here, every other tick of three cases (straight walk, turn on the spot, walk and stop): the
// smallest vertex-to-triangle distance between the two front legs' parts, both ways, with a bounding-sphere reject
// so only the parts that could be close are tested. It cannot say HOW DEEP an overlap goes — a vertex inside the
// other leg still measures its distance to that leg's surface as a small positive number — so it is not read as a
// depth. It answers the owner's question, which is whether there is daylight between the two front legs at all.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BannerFrontLegsTests
    {
        /// <summary>The daylight the two front legs must keep between their meshes, through a whole walk cycle, a
        /// turn on the spot and a stop. The owner's own figure from the day film's brief.</summary>
        public const float Margin = 0.10f;

        /// <summary>The daylight a front leg must keep between its OWN parts that are not parent and child — its
        /// foot against its own thigh. Measured on commit fda931814: the left front leg keeps 0.453 m while the
        /// right closes to 0.015 m, the same geometry solved two ways. 0.06 m is red on that right leg today, far
        /// below the good side's own number, and is not a figure any code computes.</summary>
        public const float SelfMargin = 0.06f;

        /// <summary>Every third vertex of each part is tested against all of the other part's triangles. The
        /// Banner's leg parts carry 66 to 124 vertices, so this leaves 22 to 42 probes a part — enough to find a leg
        /// inside another leg, and cheap enough to run 1500 ticks of three cases in one test.</summary>
        const int VertStep = 3;

        /// <summary>A part pair further apart than this by their bounding spheres is not tested at all: nothing that
        /// far away can hold the minimum, and the reject is what makes the mesh measure affordable.</summary>
        const float Interest = 1.0f;

        sealed class Shape
        {
            public string Name;
            public Vector3[] Vert;         // every VertStep-th vertex, in the part's own frame: the probes
            public Vector3[] Full;         // every vertex: the triangles are read off these
            public int[] Tri;
            public Vector3 Center;         // bounds centre and radius, for the reject
            public float Radius;
        }

        static TankModel Load()
        {
            var m = TankModel.Load("Banner", VehicleArchetype.Banner, "Hull", VehicleSize.Walker);
            Assert.NotNull(m, "the Banner did not load");
            return m;
        }

        /// <summary>The two rigs standing furthest forward.</summary>
        static int[] FrontPair(TankModel.Lod lod)
        {
            int a = -1, b = -1;
            for (int i = 0; i < lod.Legs.Length; i++)
            {
                if (lod.Legs[i] == null) continue;
                if (a < 0 || lod.Legs[i].Hip.z > lod.Legs[a].Hip.z) { b = a; a = i; }
                else if (b < 0 || lod.Legs[i].Hip.z > lod.Legs[b].Hip.z) b = i;
            }
            return new[] { a, b };
        }

        /// <summary>The body a leg is mounted inside: those pairs are reported but not claimed on.</summary>
        static bool Body(string name) => name == "Hull" || name == "Turret";

        static Shape Read(TankModel.Part p)
        {
            var mesh = p.Mesh;
            var all = mesh.vertices;
            var keep = new Vector3[(all.Length + VertStep - 1) / VertStep];
            for (int k = 0; k < keep.Length; k++) keep[k] = all[k * VertStep];
            return new Shape
            {
                Name = p.Name, Vert = keep, Full = all, Tri = mesh.triangles,
                Center = mesh.bounds.center, Radius = mesh.bounds.extents.magnitude
            };
        }

        static float PointTri(Vector3 p, Vector3 a, Vector3 b, Vector3 c)
        {
            Vector3 ab = b - a, ac = c - a, ap = p - a;
            float d1 = Vector3.Dot(ab, ap), d2 = Vector3.Dot(ac, ap);
            if (d1 <= 0f && d2 <= 0f) return ap.magnitude;
            Vector3 bp = p - b;
            float d3 = Vector3.Dot(ab, bp), d4 = Vector3.Dot(ac, bp);
            if (d3 >= 0f && d4 <= d3) return bp.magnitude;
            float vc = d1 * d4 - d3 * d2;
            if (vc <= 0f && d1 >= 0f && d3 <= 0f) return (ap - ab * (d1 / Mathf.Max(1e-12f, d1 - d3))).magnitude;
            Vector3 cp = p - c;
            float d5 = Vector3.Dot(ab, cp), d6 = Vector3.Dot(ac, cp);
            if (d6 >= 0f && d5 <= d6) return cp.magnitude;
            float vb = d5 * d2 - d1 * d6;
            if (vb <= 0f && d2 >= 0f && d6 <= 0f) return (ap - ac * (d2 / Mathf.Max(1e-12f, d2 - d6))).magnitude;
            float va = d3 * d6 - d5 * d4;
            if (va <= 0f && (d4 - d3) >= 0f && (d5 - d6) >= 0f)
                return (bp - (c - b) * ((d4 - d3) / Mathf.Max(1e-12f, (d4 - d3) + (d5 - d6)))).magnitude;
            Vector3 n = Vector3.Cross(ab, ac);
            float len = n.magnitude;
            return len < 1e-12f ? ap.magnitude : Mathf.Abs(Vector3.Dot(n / len, ap));
        }

        /// <summary>The smallest distance from one part's sampled vertices to the other part's triangles, both ways,
        /// in the world. `best` is the running minimum, used to cut the search short.</summary>
        static float Gap(Shape a, Matrix4x4 ma, Shape b, Matrix4x4 mb, float best)
        {
            float got = best;
            got = OneWay(a, ma, b, mb, got);
            got = OneWay(b, mb, a, ma, got);
            return got;
        }

        static float OneWay(Shape a, Matrix4x4 ma, Shape b, Matrix4x4 mb, float best)
        {
            // the probes are brought into the other part's own frame, so its triangles are read once as they are
            Matrix4x4 into = mb.inverse * ma;
            float scale = ((Vector3)mb.GetColumn(0)).magnitude;
            float bestLocal = best / Mathf.Max(1e-4f, scale);     // the search runs in b's own frame
            for (int i = 0; i < a.Vert.Length; i++)
            {
                Vector3 p = into.MultiplyPoint3x4(a.Vert[i]);
                if ((p - b.Center).magnitude - b.Radius > bestLocal) continue;
                for (int t = 0; t + 2 < b.Tri.Length; t += 3)
                {
                    float d = PointTri(p, b.Full[b.Tri[t]], b.Full[b.Tri[t + 1]], b.Full[b.Tri[t + 2]]);
                    if (d < bestLocal) bestLocal = d;
                }
            }
            return Mathf.Min(best, bestLocal * scale);
        }

        /// <summary>One run of the gait, the front pair measured every other tick.</summary>
        static (float gap, string worst, string trail) Run(string what, float speed, float yawRate,
                                                           float seconds, bool stop,
                                                           System.Func<float, float, float> ground = null,
                                                           int lodIndex = 0)
        {
            var m = Load();
            var lod = m.Lods[lodIndex] ?? m.Lods[0];
            int[] pair = FrontPair(lod);
            Assert.GreaterOrEqual(pair[1], 0, "the Banner needs two front legs to measure");

            var legA = lod.Legs[pair[0]];
            var legB = lod.Legs[pair[1]];
            var shapeA = new Shape[legA.Chain.Length];
            var shapeB = new Shape[legB.Chain.Length];
            for (int k = 0; k < shapeA.Length; k++) shapeA[k] = Read(lod.Parts[legA.Chain[k]]);
            for (int k = 0; k < shapeB.Length; k++) shapeB[k] = Read(lod.Parts[legB.Chain[k]]);

            var gait = new WalkerGait();
            var local = new Matrix4x4[lod.Parts.Count];
            var solved = new bool[lod.Parts.Count];
            var world = new Matrix4x4[lod.Parts.Count];
            const float dt = 1f / 60f;
            ground = ground ?? ((x, z) => 0f);
            Vector3 pos = new Vector3(20f, 0f, 20f);
            float yaw = 0f;
            gait.Plant(m, pos, yaw, ground);

            float gap = float.MaxValue;
            string worst = "";
            var trail = new System.Text.StringBuilder();
            int ticks = (int)(seconds * 60f);
            for (int k = 0; k < ticks; k++)
            {
                float v = stop && k > ticks / 2 ? 0f : speed;
                float w = stop && k > ticks / 2 ? 0f : yawRate;
                yaw += w * dt;
                Vector3 vel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)) * v;
                pos += vel * dt;
                gait.Step(m, pos, yaw, vel, w, 0, false, dt, ground);
                if ((k & 1) != 0) continue;
                var root = Matrix4x4.TRS(new Vector3(pos.x, gait.Height, pos.z), gait.Carriage(yaw), Vector3.one);
                gait.Solve(lod, root, local, solved);
                for (int i = 0; i < lod.Parts.Count; i++)
                {
                    var p = lod.Parts[i];
                    var l = solved[i] ? local[i] : Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                    world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * l;
                }

                float tick = float.MaxValue; string which = "";
                for (int i = 0; i < shapeA.Length; i++)
                    for (int j = 0; j < shapeB.Length; j++)
                    {
                        Matrix4x4 ma = world[legA.Chain[i]], mb = world[legB.Chain[j]];
                        float sa = ((Vector3)ma.GetColumn(0)).magnitude, sb = ((Vector3)mb.GetColumn(0)).magnitude;
                        float rough = Vector3.Distance(ma.MultiplyPoint3x4(shapeA[i].Center),
                                                       mb.MultiplyPoint3x4(shapeB[j].Center))
                                    - shapeA[i].Radius * sa - shapeB[j].Radius * sb;
                        if (rough > Mathf.Min(tick, Interest)) continue;
                        float d = Gap(shapeA[i], ma, shapeB[j], mb, Mathf.Min(tick, Interest));
                        if (d < tick) { tick = d; which = $"{shapeA[i].Name}/{shapeB[j].Name}"; }
                    }
                if (tick == float.MaxValue) { tick = Interest; which = "nothing within reject"; }
                if (tick < gap)
                {
                    gap = tick;
                    worst = $"{which} at t {k / 60f:F2}s of the {what}";
                }
                if (k % 30 == 0) trail.Append($"\n  {what} t {k / 60f:F1}s legs {tick:F3} ({which})");
            }
            trail.Append($"\n  {what} WORST legs {gap:F3} ({worst})");
            return (gap, worst, trail.ToString());
        }

        /// <summary>One run of the gait, each front leg measured against ITSELF every other tick: every pair of
        /// that leg's own parts that is not a parent and its own child (on the Banner, Foot against Thigh).</summary>
        static (float gap, string worst, string trail) RunSelf(string what, float speed, float yawRate,
                                                               float seconds, bool stop,
                                                               System.Func<float, float, float> ground = null,
                                                               int lodIndex = 0)
        {
            var m = Load();
            var lod = m.Lods[lodIndex] ?? m.Lods[0];
            int[] pair = FrontPair(lod);
            Assert.GreaterOrEqual(pair[1], 0, "the Banner needs two front legs to measure");

            var shapes = new System.Collections.Generic.Dictionary<int, Shape>();
            var probe = new System.Collections.Generic.List<(int a, int b)>();
            foreach (int leg in pair)
            {
                var r = lod.Legs[leg];
                for (int i = 0; i < r.Chain.Length; i++)
                    for (int j = i + 1; j < r.Chain.Length; j++)
                    {
                        int ia = r.Chain[i], ib = r.Chain[j];
                        if (lod.Parts[ia].Parent == ib || lod.Parts[ib].Parent == ia) continue;
                        if (Body(lod.Parts[ia].Name) || Body(lod.Parts[ib].Name)) continue;
                        if (!shapes.ContainsKey(ia)) shapes[ia] = Read(lod.Parts[ia]);
                        if (!shapes.ContainsKey(ib)) shapes[ib] = Read(lod.Parts[ib]);
                        probe.Add((ia, ib));
                    }
            }
            Assert.Greater(probe.Count, 0, "a front leg with no two parts that are not parent and child");

            var gait = new WalkerGait();
            var local = new Matrix4x4[lod.Parts.Count];
            var solved = new bool[lod.Parts.Count];
            var world = new Matrix4x4[lod.Parts.Count];
            const float dt = 1f / 60f;
            ground = ground ?? ((x, z) => 0f);
            Vector3 pos = new Vector3(20f, 0f, 20f);
            float yaw = 0f;
            gait.Plant(m, pos, yaw, ground);

            float gap = float.MaxValue;
            string worst = "";
            var trail = new System.Text.StringBuilder();
            int ticks = (int)(seconds * 60f);
            for (int k = 0; k < ticks; k++)
            {
                float v = stop && k > ticks / 2 ? 0f : speed;
                float w = stop && k > ticks / 2 ? 0f : yawRate;
                yaw += w * dt;
                Vector3 vel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)) * v;
                pos += vel * dt;
                gait.Step(m, pos, yaw, vel, w, 0, false, dt, ground);
                if ((k & 1) != 0) continue;
                var root = Matrix4x4.TRS(new Vector3(pos.x, gait.Height, pos.z), gait.Carriage(yaw), Vector3.one);
                gait.Solve(lod, root, local, solved);
                for (int i = 0; i < lod.Parts.Count; i++)
                {
                    var p = lod.Parts[i];
                    var l = solved[i] ? local[i] : Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                    world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * l;
                }

                float tick = float.MaxValue; string which = "";
                foreach (var (ia, ib) in probe)
                {
                    Shape sa = shapes[ia], sb = shapes[ib];
                    Matrix4x4 ma = world[ia], mb = world[ib];
                    float ka = ((Vector3)ma.GetColumn(0)).magnitude, kb = ((Vector3)mb.GetColumn(0)).magnitude;
                    float rough = Vector3.Distance(ma.MultiplyPoint3x4(sa.Center), mb.MultiplyPoint3x4(sb.Center))
                                - sa.Radius * ka - sb.Radius * kb;
                    if (rough > Mathf.Min(tick, Interest)) continue;
                    float d = Gap(sa, ma, sb, mb, Mathf.Min(tick, Interest));
                    if (d < tick) { tick = d; which = $"{sa.Name}/{sb.Name}"; }
                }
                if (tick == float.MaxValue) { tick = Interest; which = "nothing within reject"; }
                if (tick < gap)
                {
                    gap = tick;
                    worst = $"{which} at t {k / 60f:F2}s of the {what}";
                }
                if (k % 30 == 0) trail.Append($"\n  {what} t {k / 60f:F1}s self {tick:F3} ({which})");
            }
            trail.Append($"\n  {what} WORST self {gap:F3} ({worst})");
            return (gap, worst, trail.ToString());
        }

        /// <summary>The front pair is the pair the owner named, but if something on this machine passes through
        /// something else it should be found whatever it is. Every pair of parts that is not a parent and its own
        /// child is measured, over a turning walk on rolling ground, and the closest are reported.
        ///
        /// The Hull and the Turret are left out of the claim. Measured, every thigh and most shins read 0.000 m
        /// against the Turret and a shin reads 0.000 m against the Hull — not a fault but the way the machine is
        /// built: the legs are mounted up inside the turret's skirt, so the top of each leg is INSIDE the body by
        /// design and is never seen. Asserting on those pairs would be asserting the sculpt is wrong.</summary>
        [Test, Category("Long")]
        public void NoPartOfTheBannerPassesThroughAnother()
        {
            var m = Load();
            var lod = m.Lods[0];
            int n = lod.Parts.Count;
            var shapes = new Shape[n];
            for (int i = 0; i < n; i++) shapes[i] = Read(lod.Parts[i]);

            var gait = new WalkerGait();
            var local = new Matrix4x4[n];
            var solved = new bool[n];
            var world = new Matrix4x4[n];
            const float dt = 1f / 60f;
            System.Func<float, float, float> ground = (x, z) => Mathf.Sin(x * 0.7f) * 0.35f + Mathf.Sin(z * 0.9f) * 0.35f;
            Vector3 pos = new Vector3(20f, 0f, 20f);
            float yaw = 0f;
            gait.Plant(m, pos, yaw, ground);

            var closest = new float[n, n];
            for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) closest[i, j] = float.MaxValue;

            for (int k = 0; k < 480; k++)
            {
                yaw += 0.8f * dt;
                Vector3 vel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)) * 1.6f;
                pos += vel * dt;
                gait.Step(m, pos, yaw, vel, 0.8f, 0, false, dt, ground);
                if (k % 4 != 0) continue;
                var root = Matrix4x4.TRS(new Vector3(pos.x, gait.Height, pos.z), gait.Carriage(yaw), Vector3.one);
                gait.Solve(lod, root, local, solved);
                for (int i = 0; i < n; i++)
                {
                    var p = lod.Parts[i];
                    var l = solved[i] ? local[i] : Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                    world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * l;
                }
                for (int i = 0; i < n; i++)
                    for (int j = i + 1; j < n; j++)
                    {
                        if (lod.Parts[i].Parent == j || lod.Parts[j].Parent == i) continue;
                        if (Body(lod.Parts[i].Name) || Body(lod.Parts[j].Name)) continue;
                        float sa = ((Vector3)world[i].GetColumn(0)).magnitude, sb = ((Vector3)world[j].GetColumn(0)).magnitude;
                        float rough = Vector3.Distance(world[i].MultiplyPoint3x4(shapes[i].Center),
                                                       world[j].MultiplyPoint3x4(shapes[j].Center))
                                    - shapes[i].Radius * sa - shapes[j].Radius * sb;
                        if (rough > Mathf.Min(Interest, closest[i, j])) continue;
                        float d = Gap(shapes[i], world[i], shapes[j], world[j], Mathf.Min(Interest, closest[i, j]));
                        if (d < closest[i, j]) closest[i, j] = d;
                    }
            }

            var rank = new System.Collections.Generic.List<(float d, string what, bool sameLeg)>();
            for (int i = 0; i < n; i++)
                for (int j = i + 1; j < n; j++)
                    if (closest[i, j] < Interest)
                    {
                        bool same = lod.Parts[i].Leg >= 0 && lod.Parts[i].Leg == lod.Parts[j].Leg;
                        rank.Add((closest[i, j], $"{lod.Parts[i].Name}/{lod.Parts[j].Name}", same));
                    }
            rank.Sort((x, y) => x.d.CompareTo(y.d));
            var report = new System.Text.StringBuilder("BANNER CLOSEST PARTS, a turning walk on rolling ground:");
            foreach (var r in rank)
                report.Append($"\n  {r.what} {r.d:F3} m{(r.sameLeg ? "   (one leg against itself: reported, not claimed)" : "")}");
            Debug.Log(report.ToString());

            // The claim is the owner's: no part of one leg meets a part of another. A leg against ITSELF is only
            // reported, because a leg that folds brings its own foot up near its own thigh and that is what folding
            // is — but the number is worth a look on film: measured 2026-10-13, the RIGHT front leg closes to
            // 0.015 m between Foot_R2 and Thigh_R2 while its left twin stays 0.453 m, which is an asymmetry in the
            // solved pose, not in the model. Whoever films the front quarter should watch that leg fold.
            float worstPair = float.MaxValue; string worstWhat = "";
            foreach (var r in rank)
                if (!r.sameLeg && r.d < worstPair) { worstPair = r.d; worstWhat = r.what; }
            Assert.IsTrue(worstPair == float.MaxValue || worstPair > Margin,
                $"two of the Banner's legs meet as it walks: {worstWhat} at {worstPair:F3} m\n{report}");
        }

        [Test, Category("Long")]
        public void TheBannersTwoFrontLegsNeverPassThroughEachOther()
        {
            var m = Load();
            var lod = m.Lods[0];
            int[] pair = FrontPair(lod);
            var shape = new System.Text.StringBuilder();
            foreach (int leg in pair)
            {
                var r = lod.Legs[leg];
                shape.Append($"\n  leg {leg}: hip {r.Hip} rest {r.Rest} reach {r.Reach:F3} parts=[");
                for (int k = 0; k < r.Chain.Length; k++)
                    shape.Append(k > 0 ? ", " : "").Append(lod.Parts[r.Chain[k]].Name);
                shape.Append($"] held={r.Held}");
            }

            // Flat ground is not the film. The Banner was filmed on GreyboxCorridor, where every foot lands at its
            // own height, so a slope and a rolling ground are walked too, and the turn is taken both on the spot and
            // while moving. LOD1 is a separately cut rig, so it is walked as well.
            System.Func<float, float, float> slope = (x, z) => z * 0.18f;
            System.Func<float, float, float> rolling = (x, z) => Mathf.Sin(x * 0.7f) * 0.35f + Mathf.Sin(z * 0.9f) * 0.35f;
            var cases = new[]
            {
                Run("straight", 2.2f, 0f, 6f, false),
                Run("turn", 0f, 0.5f, 6f, false),
                Run("stop", 2.2f, 0f, 5f, true),
                Run("slope", 2.0f, 0f, 6f, false, slope),
                Run("rolling", 2.2f, 0f, 8f, false, rolling),
                Run("rolling-turn", 1.6f, 0.8f, 8f, false, rolling),
                Run("lod1-rolling-turn", 1.6f, 0.8f, 8f, false, rolling, 1),
            };

            float legs = float.MaxValue; string worst = "";
            var all = new System.Text.StringBuilder();
            foreach (var c in cases)
            {
                all.Append(c.trail);
                if (c.gap < legs) { legs = c.gap; worst = c.worst; }
            }
            string trail = shape.ToString() + all + $"\n  ALL legs {legs:F3}, worst {worst} (margin {Margin:F2})";

            // the numbers go to the log whether it passes or fails: this test exists to be quoted
            Debug.Log("BANNER FRONT LEGS" + trail);

            Assert.Greater(legs, Margin,
                $"the Banner's two front legs come within {legs:F3} m of each other, where {Margin:F2} m of daylight "
              + $"was asked for. Worst: {worst}.{trail}");
        }
        /// <summary>The owner's "still clipping" with the leg-on-leg measure green: a front leg folding its own foot
        /// into its own thigh looks exactly the same on film. Same machinery, same seven cases, but the pairs are
        /// one leg's own parts.</summary>
        [Test, Category("Long")]
        public void TheBannersFrontLegsDoNotFoldIntoThemselves()
        {
            System.Func<float, float, float> slope = (x, z) => z * 0.18f;
            System.Func<float, float, float> rolling = (x, z) => Mathf.Sin(x * 0.7f) * 0.35f + Mathf.Sin(z * 0.9f) * 0.35f;
            var cases = new[]
            {
                RunSelf("straight", 2.2f, 0f, 6f, false),
                RunSelf("turn", 0f, 0.5f, 6f, false),
                RunSelf("stop", 2.2f, 0f, 5f, true),
                RunSelf("slope", 2.0f, 0f, 6f, false, slope),
                RunSelf("rolling", 2.2f, 0f, 8f, false, rolling),
                RunSelf("rolling-turn", 1.6f, 0.8f, 8f, false, rolling),
                RunSelf("lod1-rolling-turn", 1.6f, 0.8f, 8f, false, rolling, 1),
            };

            float self = float.MaxValue; string worst = "";
            var all = new System.Text.StringBuilder();
            foreach (var c in cases)
            {
                all.Append(c.trail);
                if (c.gap < self) { self = c.gap; worst = c.worst; }
            }
            string trail = all + $"\n  ALL self {self:F3}, worst {worst} (self margin {SelfMargin:F2})";
            Debug.Log("BANNER FRONT LEG FOLD" + trail);

            Assert.Greater(self, SelfMargin,
                $"a Banner front leg folds to {self:F3} m between its own parts, where {SelfMargin:F2} m was asked "
              + $"for. Worst: {worst}.{trail}");
        }
    }
}
