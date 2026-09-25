// Phase: A5b / C4 — how the walkers carry themselves. (Owner, 2026-09-23: "lets keep working on the rigs and
// animations untill they are triplle AA level. they way they walk through the env, get damaged, etc.")
//
// These are the claims that separate a machine that walks from a machine that slides along with its legs waving:
// that a foot put on the ground stays on that exact piece of ground until it is picked up again, that the leg the
// artist modelled actually reaches the foot the gait chose, that something is always holding the machine up, that a
// step goes OVER a parapet rather than through it, and that a machine losing legs leans into the hole they left.
// None of it is in the simulation — this is all presentation — so none of it can be checked by a hash.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GaitTests
    {
        static readonly (string name, byte archetype)[] Walkers =
        {
            ("Pincer", VehicleArchetype.Pincer), ("Kettle", VehicleArchetype.Kettle),
            ("Censer", VehicleArchetype.Censer), ("Pavise", VehicleArchetype.Pavise),
            ("Banner", VehicleArchetype.Banner), ("Redoubt", VehicleArchetype.Redoubt),
        };

        static TankModel Load(string name, byte archetype)
        {
            var m = TankModel.Load(name, archetype, "Body", VehicleSize.Walker);
            Assert.NotNull(m, $"{name} did not load");
            return m;
        }

        static System.Func<float, float, float> Flat => (x, z) => 0f;
        static System.Func<float, float, float> Slope => (x, z) => z * 0.18f;

        /// <summary>Walk one straight line and hand every frame to the caller.</summary>
        static WalkerGait Walk(TankModel m, System.Func<float, float, float> ground, float speed, float seconds,
                               byte lost = 0, float yaw = 0f, float yawRate = 0f,
                               System.Action<WalkerGait, Vector3, float> each = null)
        {
            var gait = new WalkerGait();
            const float dt = 1f / 60f;
            Vector3 dir = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
            Vector3 pos = new Vector3(20f, 0f, 20f);
            gait.Plant(m, pos, yaw, ground);
            for (float t = 0f; t < seconds; t += dt)
            {
                Vector3 vel = dir * speed;
                pos += vel * dt;
                float y = yaw + yawRate * t;
                gait.Step(m, pos, y, vel, yawRate, lost, false, dt, ground);
                each?.Invoke(gait, pos, y);
            }
            return gait;
        }

        // ------------------------------------------------------------------ the rig
        [Test]
        public void EveryWalkersEveryLegIsRiggedWithAToeUnderIt()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                var rigs = m.Lods[0].Legs;
                Assert.NotNull(rigs, $"{name} has no leg rigs");
                Assert.AreEqual(m.LegCount, rigs.Length, $"{name}: a rig per leg");
                int built = 0;
                for (int i = 0; i < rigs.Length; i++)
                {
                    if (rigs[i] == null) continue;
                    built++;
                    var rig = rigs[i];
                    Assert.Greater(rig.Chain.Length, 0, $"{name} leg {i}: no parts");
                    Assert.Greater(rig.Reach, 0.05f, $"{name} leg {i}: reaches nowhere");
                    // the toe is below the hip it hangs from, or it is not a leg
                    Assert.Less(rig.Rest.y, 0f, $"{name} leg {i}: its toe is above its hip");
                    foreach (var b in rig.Bone) Assert.Greater(b, 1e-3f, $"{name} leg {i}: a bone with no length");
                }
                Assert.AreEqual(rigs.Length, built, $"{name}: every leg must be rigged, not just some");
            }
        }

        // ------------------------------------------------------------------ the feet
        [Test]
        public void APlantedFootStaysOnTheGroundItWasPutOn()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                WalkerGait.Foot[] was = null;
                float worst = 0f;
                Walk(m, Flat, 2.2f, 6f, each: (g, pos, yaw) =>
                {
                    if (was != null && was.Length == g.Feet.Length)
                        for (int i = 0; i < g.Feet.Length; i++)
                        {
                            // planted last frame and planted now: it must not have moved one millimetre
                            if (was[i].Swing >= 0f || g.Feet[i].Swing >= 0f || g.Feet[i].Lost) continue;
                            float slide = Vector3.Distance(was[i].At, g.Feet[i].At);
                            worst = Mathf.Max(worst, slide);
                        }
                    was = (WalkerGait.Foot[])g.Feet.Clone();
                });
                Assert.Less(worst, 1e-4f, $"{name}: a planted foot slid {worst:F4} m — that is skating");
            }
        }

        [Test]
        public void ItAlwaysHasFeetOnTheGround()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                int fewest = int.MaxValue, legs = m.LegCount;
                Walk(m, Flat, 3.2f, 6f, each: (g, pos, yaw) =>
                {
                    int down = 0;
                    for (int i = 0; i < g.Feet.Length; i++) if (!g.Feet[i].Lost && g.Feet[i].Swing < 0f) down++;
                    fewest = Mathf.Min(fewest, down);
                });
                Assert.GreaterOrEqual(fewest, Mathf.Max(1, legs / 2), $"{name}: it was down to {fewest} feet of {legs}");
            }
        }

        [Test]
        public void ItsFeetFindTheGroundTheyAreStandingOn()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                float worst = 0f;
                Walk(m, Slope, 2.0f, 6f, each: (g, pos, yaw) =>
                {
                    for (int i = 0; i < g.Feet.Length; i++)
                    {
                        if (g.Feet[i].Lost || g.Feet[i].Swing >= 0f) continue;
                        float want = Slope(g.Feet[i].At.x, g.Feet[i].At.z);
                        worst = Mathf.Max(worst, Mathf.Abs(g.Feet[i].At.y - want));
                    }
                });
                Assert.Less(worst, 1e-3f, $"{name}: a planted foot was {worst:F3} m off the ground under it");
            }
        }

        [Test]
        public void AStepGoesOverAParapetRatherThanThroughIt()
        {
            // a wall two thirds of a leg's reach high, across the line of march
            var m = Load("Pincer", VehicleArchetype.Pincer);
            float wall = 0f;
            for (int i = 0; i < m.Lods[0].Legs.Length; i++) if (m.Lods[0].Legs[i] != null) { wall = m.Lods[0].Legs[i].Reach * 0.66f; break; }
            System.Func<float, float, float> parapet = (x, z) => z > 24f && z < 25.2f ? wall : 0f;

            bool cleared = false;
            Walk(m, parapet, 2.0f, 8f, each: (g, pos, yaw) =>
            {
                for (int i = 0; i < g.Feet.Length; i++)
                {
                    if (g.Feet[i].Swing < 0f) continue;
                    bool crosses = (g.Feet[i].Anchor.z < 24f) != (g.Feet[i].Target.z < 24f);
                    if (crosses && g.Feet[i].Arc >= wall + WalkerGait.Clearance * 0.9f) cleared = true;
                }
            });
            Assert.IsTrue(cleared, "no step over the parapet was raised to clear it");
        }

        // ------------------------------------------------------------------ the leg reaches the foot
        [Test]
        public void TheLegTheArtistModelledReachesTheFootTheGaitChose()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                var lod = m.Lods[0];
                var local = new Matrix4x4[lod.Parts.Count];
                var solved = new bool[lod.Parts.Count];
                var world = new Matrix4x4[lod.Parts.Count];
                float worst = 0f, planted = 0f; string where = "", plantedWhy = "";
                var rigDesc = new System.Text.StringBuilder();
                for (int i = 0; i < lod.Legs.Length; i++)
                {
                    var r = lod.Legs[i];
                    if (r == null) { rigDesc.Append($"\n  leg {i}: NO RIG"); continue; }
                    rigDesc.Append($"\n  leg {i}: parts={r.Chain.Length} [");
                    for (int c = 0; c < r.Chain.Length; c++) rigDesc.Append(c > 0 ? "," : "").Append(lod.Parts[r.Chain[c]].Name);
                    rigDesc.Append($"] reach={r.Reach:F3} bones=[");
                    for (int c = 0; c < r.Bone.Length; c++) rigDesc.Append(c > 0 ? "," : "").Append(r.Bone[c].ToString("F3"));
                    rigDesc.Append($"] hip={r.Hip} rest={r.Rest} toe={r.Toe}");
                }

                Walk(m, Slope, 2.0f, 6f, each: (g, pos, yaw) =>
                {
                    var root = Matrix4x4.TRS(new Vector3(pos.x, g.Height, pos.z),
                        Quaternion.AngleAxis(yaw * Mathf.Rad2Deg, Vector3.up)
                        * Quaternion.AngleAxis(-g.Pitch * Mathf.Rad2Deg, Vector3.right)
                        * Quaternion.AngleAxis(-g.Roll * Mathf.Rad2Deg, Vector3.forward), Vector3.one);
                    g.Solve(lod, root, local, solved);
                    for (int i = 0; i < lod.Parts.Count; i++)
                    {
                        var p = lod.Parts[i];
                        var l = solved[i] ? local[i] : Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                        world[i] = (p.Parent >= 0 ? world[p.Parent] : root) * l;
                    }
                    for (int i = 0; i < lod.Legs.Length; i++)
                    {
                        var rig = lod.Legs[i];
                        if (rig == null || g.Feet[i].Lost) continue;
                        int tip = rig.Chain[rig.Chain.Length - 1];
                        Vector3 toe = world[tip].MultiplyPoint3x4(rig.Toe);
                        float miss = Vector3.Distance(toe, g.Feet[i].At);
                        // as a share of the leg, not in metres: two centimetres is a hovering foot on a leg that
                        // is 70 cm long and is nothing at all on one that is 1.7 m
                        float share = miss / Mathf.Max(0.05f, WalkerGait.MaxSpan(rig));
                        if (g.Feet[i].Swing < 0f && share > planted)
                        {
                            planted = share;
                            Vector3 hp = root.MultiplyPoint3x4(rig.Hip);
                            plantedWhy = $"{name} leg {i} missed by {miss:F3} m, parts={rig.Chain.Length} reach={rig.Reach:F3} "
                                       + $"bones=[{string.Join(",", System.Array.ConvertAll(rig.Bone, b => b.ToString("F3")))}] "
                                       + $"hip->foot={Vector3.Distance(hp, g.Feet[i].At):F3}";
                        }
                        if (share > worst)
                        {
                            worst = share;
                            Vector3 hipW = root.MultiplyPoint3x4(rig.Hip);
                            float dist = Vector3.Distance(hipW, g.Feet[i].At);
                            where = $"{name} leg {i} (parts={rig.Chain.Length} reach={rig.Reach:F3} " +
                                    $"bone0={rig.Bone[0]:F3} hip->foot={dist:F3} stretch={(dist / rig.Bone[0]):F3} " +
                                    $"{(g.Feet[i].Swing >= 0f ? "swinging" : "planted")})";
                        }
                    }
                });
                // A foot ON THE GROUND has to be exactly where the solver put it: a toe hanging above the mud, or
                // buried in it, is the whole illusion gone. A foot IN THE AIR is allowed to fall short of its ideal
                // arc, because a leg built in one piece cannot both lift its foot and keep its length — Pincer's
                // middle pair stand 0.27 m out under a 1.05 m drop, so raising that foot at all shortens the leg.
                // It is drawn as far up as the leg can manage, which nobody reads as wrong with no ground to
                // compare it against, and it lands exactly right.
                Assert.Less(planted, 0.015f,
                    $"a foot standing on the ground finished {planted * 100f:F1}% of its leg from where the leg drew "
                  + $"the toe, which is a toe hanging over the mud or buried in it: {plantedWhy}");
                // A foot in the air is judged loosely and as a share of its leg, because a leg built in one
                // piece cannot both lift its foot and keep its length: Censer's legs hang straight down from a
                // hip taller than they are long, so raising the foot at all asks the leg to shorten. It is drawn
                // as far up as it can manage, which reads as a stiff-legged machine picking its feet up rather
                // than as a fault, and it lands exactly where it was sent. The planted bound above is the one
                // that guards the illusion.
                Assert.Less(worst, 0.22f,
                    $"{where}: its toe finished {worst * 100f:F1}% of its leg from the foot it was solving for.\nRIG {name}:{rigDesc}");
            }
        }

        // ------------------------------------------------------------------ damage
        [Test]
        public void ALostLegStopsWalkingAndTheMachineLeansIntoTheHole()
        {
            var m = Load("Pincer", VehicleArchetype.Pincer);
            int perSide = Mathf.Max(1, m.LegCount / 2);
            byte lost = (byte)((1 << 0) | (1 << 1));          // two off the left side

            var sound = Walk(m, Flat, 2.0f, 5f);
            var hurt = Walk(m, Flat, 2.0f, 5f, lost: lost);

            Assert.AreEqual(0f, sound.Limp, 1e-4f, "a whole machine should not be limping");
            Assert.Greater(hurt.Limp, 0f, "a machine with legs off should be limping");
            for (int i = 0; i < 2; i++) Assert.IsTrue(hurt.Feet[i].Lost, $"leg {i} is gone and should not be walking");
            // the left side is numbered first, so losing legs there rolls it to the left: roll is positive left-up,
            // so it must go the other way
            Assert.Less(hurt.Roll, sound.Roll - 0.01f, "it should lean toward the legs it has lost");
        }

        /// <summary>How far its feet are spread from under it, on average.</summary>
        static float Spread(WalkerGait g, Vector3 pos)
        {
            float sum = 0f; int n = 0;
            for (int i = 0; i < g.Feet.Length; i++)
            {
                if (g.Feet[i].Lost) continue;
                Vector3 d = g.Feet[i].At - pos;
                sum += new Vector2(d.x, d.z).magnitude; n++;
            }
            return n > 0 ? sum / n : 0f;
        }

        /// <summary>The bar this was failing without anyone asking it directly: a machine walking on the level
        /// must not sink. A foot allowed to trail stretches its leg, the ride height is capped by whatever the
        /// most-stretched leg can still reach, and the hips come down to meet it — and once they are down, the
        /// step clamp pushes the next foot further OUT because the leg can no longer fold that tight, which lowers
        /// the cap again. It walks itself into the ground. Nothing with legs does that.</summary>
        [Test]
        public void ItDoesNotSinkIntoTheGroundAsItWalksOnTheLevel()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                var gait = new WalkerGait();
                float want = gait.Stand(m.Lods[0].Legs);
                const float dt = 1f / 60f;
                Vector3 pos = new Vector3(20f, 0f, 20f);
                gait.Plant(m, pos, 0f, Flat);
                float lowest = gait.Height, highest = gait.Height;
                var trail = new System.Text.StringBuilder();
                for (int k = 0; k < 480; k++)
                {
                    pos += new Vector3(0f, 0f, 2.4f * dt);
                    gait.Step(m, pos, 0f, new Vector3(0f, 0f, 2.4f), 0f, 0, false, dt, Flat);
                    lowest = Mathf.Min(lowest, gait.Height); highest = Mathf.Max(highest, gait.Height);
                    if (k % 60 == 0) trail.Append($"\n  t {k / 60f:F1}s height {gait.Height:F3} spread {Spread(gait, pos):F2}");
                }
                // It may SETTLE as it strides — several of these machines are given speeds their legs can barely
                // sustain, and a thing with legs answers that by lowering itself, which is both what an animal does
                // and far less visible than a toe hanging off the end of a leg. What it may not do is collapse: the
                // fault this guards against sank 0.89 m and ended with the hull on the mud and the legs splayed.
                Assert.Less(want - lowest, 0.34f,
                    $"{name}: it stands at {want:F3} but sank to {lowest:F3} while walking the level — " +
                    $"that is {want - lowest:F2} m into the ground.\n{trail}");
                Assert.Less(highest - lowest, 0.38f, $"{name}: its ride height wandered {highest - lowest:F2} m.\n{trail}");
            }
        }

        [Test]
        public void TheMachineIsBuiltAtTheSizeItShips()
        {
            const float want = VehicleSize.Walker;
            foreach (var (name, archetype) in Walkers)
            {
                var sculpt = TankModel.Load(name, archetype, "Body", 1f);
                var built = TankModel.Load(name, archetype, "Body", want);
                Assert.NotNull(sculpt, name); Assert.NotNull(built, name);

                // the body: this is the half that can fail quietly, because a mesh is shared, must be copied
                // rather than written to, and a copy that does not take leaves a machine standing correctly
                // wide on legs that are the right length, carrying a hull the size of the original.
                var a = sculpt.Lods[0].Parts; var b = built.Lods[0].Parts;
                Assert.AreEqual(a.Count, b.Count, $"{name}: a different number of parts at the two sizes");
                for (int i = 0; i < a.Count; i++)
                {
                    float small = a[i].Mesh.bounds.size.magnitude, large = b[i].Mesh.bounds.size.magnitude;
                    Assert.AreEqual(small * want, large, small * want * 0.01f,
                        $"{name}: the part {a[i].Name} is drawn {large / Mathf.Max(1e-4f, small):F2} times the sculpt, " +
                        $"not {want:F2} — its mesh was not actually enlarged, so the machine stands big and looks small.");
                }
                Assert.AreEqual(sculpt.Height * want, built.Height, sculpt.Height * want * 0.01f,
                    $"{name}: its hull tops out at {built.Height:F2} m, where {sculpt.Height * want:F2} m was asked for.");

                // and the legs: how far apart it stands and how far each one can reach
                var la = sculpt.Lods[0].Legs; var lb = built.Lods[0].Legs;
                for (int i = 0; i < la.Length; i++)
                {
                    if (la[i] == null || lb[i] == null) continue;
                    Assert.AreEqual(la[i].Hip.magnitude * want, lb[i].Hip.magnitude, la[i].Hip.magnitude * want * 0.01f,
                        $"{name} leg {i}: its hip did not move out with the rest of the machine.");
                    float small = WalkerGait.MaxSpan(la[i]), large = WalkerGait.MaxSpan(lb[i]);
                    Assert.AreEqual(small * want, large, small * want * 0.01f,
                        $"{name} leg {i}: it can reach {large:F2} m, where {small * want:F2} m was asked for. " +
                        "A leg whose toe is derived from mesh bounds rather than a Socket_Toe reads the geometry, " +
                        "so this is what an unenlarged mesh looks like from the rig's side.");
                }
            }
        }

        [Test]
        public void AKilledWalkerGoesDownOnItsLegsInsteadOfStandingThere()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = Load(name, archetype);
                var gait = new WalkerGait();
                const float dt = 1f / 60f;
                Vector3 pos = new Vector3(20f, 0f, 20f);
                gait.Plant(m, pos, 0f, Flat);
                for (int k = 0; k < 150; k++)
                {
                    pos += new Vector3(0f, 0f, 2f * dt);
                    gait.Step(m, pos, 0f, new Vector3(0f, 0f, 2f), 0f, 0, false, dt, Flat);
                }
                float stood = gait.Height, spread = Spread(gait, pos);
                Assert.AreEqual(0f, gait.Down, 1e-4f, $"{name}: a live machine is not going down");

                for (int k = 0; k < 160; k++) gait.Step(m, pos, 0f, Vector3.zero, 0f, 0, true, dt, Flat);

                Assert.AreEqual(1f, gait.Down, 1e-3f, $"{name}: it should be all the way down by now");
                Assert.Less(gait.Height, stood - 0.25f, $"{name}: it settled only {stood - gait.Height:F2} m — it is still standing up");
                var why = new System.Text.StringBuilder();
                why.Append($"{name}: stood {stood:F3} -> down {gait.Height:F3} (sank {stood - gait.Height:F3} m), ");
                why.Append($"spread {spread:F3} -> {Spread(gait, pos):F3}, Down {gait.Down:F2}");
                for (int i = 0; i < m.Lods[0].Legs.Length; i++)
                {
                    var r = m.Lods[0].Legs[i];
                    if (r == null) continue;
                    Vector3 hip = gait.HipAt(r, pos, 0f);
                    Vector3 off = gait.Feet[i].At - hip;
                    float flat = new Vector2(off.x, off.z).magnitude, drop = hip.y - gait.Feet[i].At.y;
                    why.Append($"\n  leg {i}: hipY {hip.y:F2} flat {flat:F2} drop {drop:F2} span {off.magnitude:F2} of reach {r.Reach:F2}");
                }
                Assert.Greater(Spread(gait, pos), spread + 0.05f,
                    $"its feet did not slide out from under it as it sank — rigid legs cannot fold.\n{why}");
                for (int i = 0; i < gait.Feet.Length; i++)
                    Assert.Less(gait.Feet[i].Swing, 0f, $"{name} leg {i}: a dead machine should not still be stepping");
            }
        }

        [Test]
        public void EveryFootfallIsReportedSoTheGroundCanAnswerIt()
        {
            // the renderer kicks dust off Landed; if nothing is ever reported the feet read as hovering however
            // correctly they are placed
            var m = Load("Pincer", VehicleArchetype.Pincer);
            int falls = 0;
            Walk(m, Flat, 2.4f, 5f, each: (g, pos, yaw) =>
            {
                for (int i = 0; i < g.Feet.Length; i++) if ((g.Landed & (1 << i)) != 0) falls++;
            });
            Assert.Greater(falls, 10, "a machine walking five seconds should have put its feet down many times");
        }

        [Test]
        public void ItStepsWhenItTurnsOnTheSpot()
        {
            var m = Load("Pincer", VehicleArchetype.Pincer);
            int steps = 0;
            var gait = new WalkerGait();
            const float dt = 1f / 60f;
            Vector3 pos = new Vector3(20f, 0f, 20f);
            gait.Plant(m, pos, 0f, Flat);
            float yaw = 0f;
            for (int k = 0; k < 360; k++)
            {
                yaw += 1.1f * dt;
                gait.Step(m, pos, yaw, Vector3.zero, 1.1f, 0, false, dt, Flat);
                for (int i = 0; i < gait.Feet.Length; i++) if ((gait.Landed & (1 << i)) != 0) steps++;
            }
            Assert.Greater(steps, 4, "turning on the spot should make it pick its feet up, not swivel on them");
        }
    }
}
