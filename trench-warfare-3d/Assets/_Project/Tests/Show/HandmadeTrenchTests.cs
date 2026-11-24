// Phase: B2 (2026-11-24) — the hand-built trench lining (BattlefieldKit.Handmade.cs): no panel heavier than the even
// panel it replaces, in vertices or triangles; every part carries its pivot and seed for the shader's jitter; every sack
// rests on the ground or on another sack, never in the air and never through the ground; the tallest course stays inside
// its scale row at the composer's tallest scale; and knob props.handmade = 0 builds the even panels again, with no jitter.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class HandmadeTrenchTests
    {
        BattlefieldKit handmade, even;

        [OneTimeSetUp]
        public void Build()
        {
            Knobs.Set(BattlefieldKit.HandmadeKnob, "0");
            try { even = new BattlefieldKit(); }
            finally { Knobs.Clear(); }
            handmade = new BattlefieldKit();
        }

        [OneTimeTearDown] public void Tear() { handmade?.Dispose(); even?.Dispose(); handmade = even = null; }

        static BattlefieldKit.Module[][] Lining(BattlefieldKit k) =>
            new[] { k.TrenchWalls, k.TrenchBags, k.TrenchFloors, k.TrenchWallsDamaged, k.TrenchBagsDamaged, k.TrenchFloorsDamaged };

        [Test]
        public void The_Knob_Builds_The_Even_Panels_Again()
        {
            Assert.IsTrue(handmade.Handmade, "hand-made is the default");
            Assert.IsFalse(even.Handmade, "props.handmade = 0 is read");
            StringAssert.StartsWith("Weathered revetment", even.TrenchWalls[0].Mesh.name);
            StringAssert.StartsWith("Hand-built revetment", handmade.TrenchWalls[0].Mesh.name);
            foreach (var set in Lining(even))
                foreach (var m in set) Assert.IsFalse(m.Material.IsKeywordEnabled("_KITJITTER"), m.Mesh.name + ": the even panels take no jitter");
            foreach (var set in Lining(handmade))
                foreach (var m in set)
                {
                    Assert.IsTrue(m.Material.IsKeywordEnabled("_KITJITTER"), m.Mesh.name + " is jittered");
                    Assert.Greater(m.Material.GetVector("_KitJitter").x, 0f, m.Mesh.name + " turns its parts");
                }
        }

        [Test]
        public void No_Hand_Built_Panel_Is_Heavier_Than_The_Even_One()
        {
            var made = Lining(handmade); var old = Lining(even);
            long verts = 0, oldVerts = 0;
            for (int s = 0; s < made.Length; s++)
                for (int k = 0; k < 3; k++)
                {
                    var a = made[s][k].Mesh; var b = old[s][k].Mesh;
                    Assert.LessOrEqual(a.vertexCount, b.vertexCount, a.name + " against " + b.name + ": vertices");
                    Assert.LessOrEqual(a.triangles.Length, b.triangles.Length, a.name + " against " + b.name + ": triangles");
                    verts += a.vertexCount; oldVerts += b.vertexCount;
                }
            TestContext.WriteLine("lining vertices, all eighteen: " + verts + " hand-made, " + oldVerts + " even");
        }

        [Test]
        public void Every_Lining_Part_Carries_Its_Pivot_And_Seed()
        {
            var parts = new List<Vector4>();
            foreach (var set in Lining(handmade))
                foreach (var m in set)
                {
                    var mesh = m.Mesh;
                    mesh.GetUVs(2, parts);
                    Assert.AreEqual(mesh.vertexCount, parts.Count, mesh.name + ": a part for every vertex");
                    var room = mesh.bounds; room.Expand(.6f);
                    var seeds = new HashSet<float>();
                    foreach (var p in parts)
                    {
                        Assert.That(Mathf.Abs(p.w), Is.InRange(.01f, .99f), mesh.name + ": a seed the shader can hash");
                        Assert.IsTrue(room.Contains(new Vector3(p.x, p.y, p.z)), mesh.name + ": a pivot " + (Vector3)p + " on the panel");
                        seeds.Add(p.w);
                    }
                    Assert.Greater(seeds.Count, 1, mesh.name + " is several parts, each turned on its own");
                }
        }

        [Test]
        public void A_Sack_Rests_On_The_Ground_Or_On_Another_Sack()
        {
            var verts = new List<Vector3>(); var parts = new List<Vector4>();
            foreach (var set in new[] { handmade.TrenchBags, handmade.TrenchBagsDamaged })
                foreach (var m in set)
                {
                    m.Mesh.GetVertices(verts); m.Mesh.GetUVs(2, parts);
                    var lowest = new Dictionary<float, Vector3>();
                    for (int i = 0; i < verts.Count; i++)
                        if (!lowest.TryGetValue(parts[i].w, out var low) || verts[i].y < low.y) lowest[parts[i].w] = verts[i];
                    foreach (var kv in lowest)
                    {
                        var foot = kv.Value;
                        Assert.GreaterOrEqual(foot.y, -.06f, m.Mesh.name + ": a sack sinks into the mud, not through the ground");
                        if (foot.y <= .03f) continue;   // on the ground
                        bool held = false;
                        for (int i = 0; i < verts.Count && !held; i++)
                        {
                            if (parts[i].w == kv.Key) continue;
                            var v = verts[i];
                            held = Mathf.Abs(v.x - foot.x) < .25f && Mathf.Abs(v.z - foot.z) < .25f && v.y > foot.y - .08f && v.y < foot.y + .10f;
                        }
                        Assert.IsTrue(held, m.Mesh.name + ": the sack whose foot is at " + foot + " rests on nothing");
                    }
                }
        }

        [Test]
        public void The_Tallest_Course_Stays_Inside_Its_Row()
        {
            // the composer scales a course up to 1.06 high (BattlefieldComposer.Trench, hand-made); the row reports and
            // never clamps, so a course that grew past it would be a FAIL in the composed field's audit, not a clamp
            Assert.IsTrue(AssetScaleTable.TryGet("kit/TrenchBags", out var rule));
            foreach (var m in handmade.TrenchBags)
            {
                float su = AssetScaleTable.SoldierUnits(rule, m.Mesh.bounds.size, new Vector3(1f, 1.06f, 1f));
                Assert.IsTrue(rule.Holds(su), m.Mesh.name + " is " + su.ToString("0.00") + " SU at its tallest, row " + rule.MinSU + "-" + rule.MaxSU);
            }
        }
    }
}
