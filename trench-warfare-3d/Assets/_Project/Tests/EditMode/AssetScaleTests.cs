// Phase: B7 (docs/21 phase 1) — the soldier is the unit, and the man-made things stand in proportion to him.
// The cheap tests here read the looks and the meshes; the composed-field audit is Explicit (it builds the whole kit
// and composes a battlefield) and is what Editor/AssetScaleAudit writes to docs/reference/asset-scale.md.
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class AssetScaleTests
    {
        const uint Seed = 1917;

        [Test]
        public void One_Soldier_Unit_Is_The_Figures_Drawn_Height()
        {
            Assert.AreEqual(1.78f * FigureMetrics.UnitScale, AssetScaleTable.SoldierM, 1e-4f, "1 SU = the 1.78 m bake at UnitScale");
            Assert.AreEqual(2.0f, AssetScaleTable.SoldierM, 0.01f, "which is the two-metre man of the owner's 2026-09-23 decision");
            Assert.AreEqual(AssetScaleTable.SoldierM * 1.25f, FigureMetrics.DrawnHeightM(30f), 1e-3f, "at the standard view he is grown a quarter: a men-only exception");
        }

        [Test]
        public void The_Clamp_Leaves_A_Prop_In_Bounds_Alone_And_Brings_One_Outside_Back()
        {
            var door = new ScaleRule(ScaleClass.Strict, ScaleAxis.Height, 0.85f, 1.15f);
            var mesh = new Vector3(0.9f, 1.8f, 0.1f);   // a 1.8 m door at scale 1 = 0.9 SU
            Assert.AreEqual(Vector3.one, AssetScaleTable.Clamp(door, mesh, Vector3.one), "in bounds: untouched");
            var big = AssetScaleTable.Clamp(door, mesh, Vector3.one * 3f);   // 2.7 SU
            Assert.AreEqual(1.15f, AssetScaleTable.SoldierUnits(door, mesh, big), 1e-4f, "pulled down to the top of its bounds");
            Assert.AreEqual(big.x, big.y, 1e-5f); Assert.AreEqual(big.y, big.z, 1e-5f);
            var wide = AssetScaleTable.Clamp(door, mesh, new Vector3(3f, 3f, 6f));
            Assert.AreEqual(2f, wide.z / wide.x, 1e-4f, "one uniform factor: a look's proportions survive the clamp");
            var small = AssetScaleTable.Clamp(door, mesh, Vector3.one * 0.5f);
            Assert.AreEqual(0.85f, AssetScaleTable.SoldierUnits(door, mesh, small), 1e-4f, "and a shrunken one is raised to the bottom");
            var boulder = new ScaleRule(ScaleClass.Organic, ScaleAxis.Height, 0.1f, 3f);
            Assert.AreEqual(Vector3.one * 9f, AssetScaleTable.Clamp(boulder, mesh, Vector3.one * 9f), "an organic thing is never clamped");
            var house = new ScaleRule(ScaleClass.Structure, ScaleAxis.Height, 2f, 4f, enforce: false);
            Assert.AreEqual(Vector3.one * 9f, AssetScaleTable.Clamp(house, mesh, Vector3.one * 9f), "a building that says so is reported, not clamped");
            Assert.AreEqual("FAIL", AssetScaleTable.Verdict(house, 9f));
            Assert.AreEqual("warn", AssetScaleTable.Verdict(boulder, 9f));
        }

        /// <summary>The audit judges what is drawn, which the clamp always pulls into bounds, so it must also judge how
        /// often the clamp acted: past one instance in ten the composer is asking for the wrong size (critique 2026-09-27).</summary>
        [Test]
        public void A_Row_The_Clamp_Keeps_Pulling_In_Is_Not_Ok()
        {
            var stakes = new ScaleRule(ScaleClass.Strict, ScaleAxis.Height, 0.50f, 0.85f);
            Assert.AreEqual("OK", AssetScaleTable.Verdict(stakes, 0.76f, 100, 10), "one in ten clamped is a look straying");
            Assert.AreEqual("CLAMPED", AssetScaleTable.Verdict(stakes, 0.76f, 100, 11), "more is the composer's size");
            Assert.AreEqual("FAIL", AssetScaleTable.Verdict(stakes, 0.95f, 100, 0), "out of bounds is still FAIL");
            Assert.AreEqual("OK", AssetScaleTable.Verdict(stakes, 0.76f, 0, 0), "nothing placed: the look is judged alone");
            var log = new ScaleRule(ScaleClass.Organic, ScaleAxis.Height, 0.1f, 3.0f);
            Assert.AreEqual("OK", AssetScaleTable.Verdict(log, 1f, 100, 50), "an Organic row is never clamped, so never CLAMPED");
            Assert.AreEqual("FAIL", AssetScaleTable.Verdict(stakes, 0.76f, 100, 0, outside: 11), "a tail out of band the median hides still fails");
            Assert.AreEqual("CLAMPED", AssetScaleTable.Verdict(stakes, 0.76f, 100, 1, 0, 0.6f, 0.85f * AssetScaleTable.MaxAsk + 0.01f), "one asking for half as much again past the top is the composer's bug");
            Assert.AreEqual("OK", AssetScaleTable.Verdict(stakes, 0.76f, 100, 1, 0, 0.6f, 0.9f), "one a little over is a look straying");
        }

        /// <summary>A hand edit the clamp shrinks keeps the share of it the owner left showing: the depth it was sunk by
        /// shrinks with it (ArmouredStand@-734,1396: 7.65 m sunk 5.15 m; clamped to 4.40 m with the depth kept whole, its
        /// top ended 0.75 m underground; critique 2026-09-27).</summary>
        [Test]
        public void A_Clamped_Hand_Edit_Stays_Above_The_Ground()
        {
            var mesh = new Mesh { vertices = new[] { new Vector3(-.5f, 0f, -.5f), new Vector3(.5f, 1f, .5f), new Vector3(.5f, 0f, -.5f) }, triangles = new[] { 0, 1, 2 } };
            mesh.RecalculateBounds();
            var stand = new BattlefieldKit.Module { Mesh = mesh, Rule = new ScaleRule(ScaleClass.Structure, ScaleAxis.Height, 1.5f, 2.2f) };
            var edit = new PropLayout.Edit { Module = "Siege/ArmouredStand", Position = new Vector3(10f, -5.15f, 20f), Scale = Vector3.one * 7.65f };
            var m = BattlefieldProps.PlaceEdit(edit, stand, 1f, 3f, out bool clamped);
            Assert.IsTrue(clamped, "a 7.65 m stand is past 2.2 SU");
            float height = m.lossyScale.y, foot = m.GetPosition().y - 3f;
            Assert.AreEqual(2.2f * AssetScaleTable.SoldierM, height, 1e-3f, "clamped to the band's top");
            Assert.Greater(foot + height, 0f, "its top stays above the ground");
            Assert.AreEqual(2.5f / 7.65f, (foot + height) / height, 1e-3f, "the same share of it shows as the owner left");
            var small = new PropLayout.Edit { Module = "Siege/ArmouredStand", Position = new Vector3(0f, -.5f, 0f), Scale = Vector3.one * 4f };
            var ok = BattlefieldProps.PlaceEdit(small, stand, 1f, 0f, out bool untouched);
            Assert.IsFalse(untouched); Assert.AreEqual(-.5f, ok.GetPosition().y, 1e-5f, "an edit in bounds keeps its height exactly");
            Object.DestroyImmediate(mesh);
        }

        [Test]
        public void Every_Kit_Piece_And_Imported_Prop_Has_A_Rule()
        {
            var missing = new List<string>();
            var fieldKeys = new List<string>(BattlefieldKit.FieldKeys());
            foreach (var key in fieldKeys)
                if (!AssetScaleTable.TryGet(key, out _)) missing.Add(key);
            // the imported props are keyed by asset name, never by their field: FieldKeys leaves them out ([Imported])
            foreach (var imported in new[] { "kit/fieldGun", "kit/sandbag", "kit/grass", "kit/biplane" })
                Assert.IsFalse(fieldKeys.Contains(imported), imported + " is an imported prop, keyed as Set/Prop");
            // the imported props are named in BattlefieldKit.BuildImported: Imported("Set", "Prop", ...)
            string src = File.ReadAllText(Path.Combine("Assets", "_Project", "Presentation", "Terrain", "BattlefieldKit.cs"));   // relative to the project: Unity's working folder, and Tools/otr.py's
            foreach (Match m in Regex.Matches(src, "Imported\\(\"(\\w+)\", \"(\\w+)\""))
            {
                string key = m.Groups[1].Value + "/" + m.Groups[2].Value;
                if (!AssetScaleTable.TryGet(key, out _)) missing.Add(key);
            }
            Assert.IsEmpty(missing, "every module the kit draws has a row in AssetScaleTable (Strict, Structure, Organic or Machine): " + string.Join(", ", missing));
        }

        [Test]
        public void The_Looks_Of_Man_Made_Props_Stand_In_Proportion_To_The_Man()
        {
            var layout = Resources.Load<PropLayout>(PropLayout.ResourcePath((int)Seed));
            if (layout == null) Assert.Ignore("no Resources/Layouts/Battlefield" + Seed);
            var failed = new List<string>();
            foreach (var look in layout.Looks)
            {
                if (look == null || !AssetScaleTable.TryGet(look.Module, out var rule) || !rule.Bounded) continue;
                var mesh = Resources.Load<Mesh>("Env/" + look.Module);
                if (mesh == null) continue;
                float su = AssetScaleTable.SoldierUnits(rule, mesh.bounds.size, look.Baseline);
                TestContext.WriteLine(look.Module + ": " + su.ToString("0.00") + " SU (" + rule.MinSU + "-" + rule.MaxSU + ")");
                if (!rule.Holds(su)) failed.Add(look.Module + " draws " + su.ToString("0.00") + " SU, bounds " + rule.MinSU + "-" + rule.MaxSU);
            }
            Assert.IsEmpty(failed, "a look was learned when men were 2.67 m and never rescaled; run python Tools/looks.py --apply: " + string.Join("; ", failed));
        }

        [Test]
        public void Hand_Edits_Of_Bounded_Kinds_Are_Brought_Into_Bounds_By_The_Clamp()
        {
            var layout = Resources.Load<PropLayout>(PropLayout.ResourcePath((int)Seed));
            if (layout == null) Assert.Ignore("no Resources/Layouts/Battlefield" + Seed);
            int touched = 0;
            foreach (var (key, module, raw, after) in AssetScaleReport.Edits(layout))
            {
                AssetScaleTable.TryGet(module, out var rule);
                Assert.IsTrue(rule.Holds(after), key + " after the clamp: " + after.ToString("0.00") + " SU");
                if (Mathf.Abs(raw - after) > 1e-3f) { touched++; TestContext.WriteLine(key + ": " + raw.ToString("0.00") + " -> " + after.ToString("0.00") + " SU"); }
            }
            TestContext.WriteLine(touched + " hand edit(s) the clamp touches");
        }

        /// <summary>The whole audit on all three grounds: builds the kit, composes each field and judges every module, so
        /// a composer size (a wire belt, a backdrop cluster, the coast's surf obstacles) cannot drift out of proportion
        /// unseen. In the gate since 2026-09-27 (about 6 s a ground; it was Explicit, so no size fix had a test);
        /// Editor/AssetScaleAudit writes the same for docs/reference/asset-scale.md.</summary>
        [Test]
        public void The_Composed_Field_Stands_In_Proportion_To_The_Man()
        {
            var kit = new BattlefieldKit();
            try
            {
                var layout = Resources.Load<PropLayout>(PropLayout.ResourcePath((int)Seed));
                var failed = new List<string>();
                foreach (var (name, ground) in AssetScaleReport.Grounds)
                {
                    var rows = AssetScaleReport.Measure(kit, layout, Seed, ground);
                    Assert.Greater(rows.Count, 40, name + ": the kit has many modules");
                    foreach (var r in rows)
                    {
                        TestContext.WriteLine(name + " " + r.Key + ": " + r.Verdict + " " + r.JudgedSU.ToString("0.00") + " SU x" + r.Instances);
                        // CLAMPED: in bounds only because the emit clamp pulled in what the composer asked for too often, or too far
                        if (r.Verdict == "FAIL" || r.Verdict == "CLAMPED")
                            failed.Add(name + " " + r.Key + " " + r.Verdict + " " + r.JudgedSU.ToString("0.00") + " SU (" + r.Clamped + " clamped, " + r.Outside + " outside, of " + r.Instances
                                + "; asked " + r.AskedMinSU.ToString("0.00") + "-" + r.AskedMaxSU.ToString("0.00") + ")");
                    }
                }
                Assert.IsEmpty(failed, "man-made things out of proportion to the man: " + string.Join("; ", failed));
            }
            finally { kit.Dispose(); }
        }

        /// <summary>The cheap half of the audit, in the gate: every procedural piece with a bounded row that enforces
        /// must stand in its bounds at the scale the kit builds it, or the clamp rescales it silently in every scene.
        /// Builds the kit once (as TrenchSectionTests does); the composed field stays Explicit below.</summary>
        [Test]
        public void The_Kit_Pieces_Stand_In_Bounds_As_Built()
        {
            var kit = new BattlefieldKit();
            try
            {
                kit.ResolveKeysAndRules();
                var failed = new List<string>(); int judged = 0;
                foreach (var kv in kit.KeyOf)
                {
                    var m = kv.Key; string key = kv.Value;
                    if (!key.StartsWith("kit/") || m.Mesh == null || !m.Rule.Bounded || !m.Rule.Enforce) continue;
                    float su = AssetScaleTable.SoldierUnits(m.Rule, m.Mesh.bounds.size, Vector3.one);
                    judged++;
                    TestContext.WriteLine(key + ": " + su.ToString("0.00") + " SU (" + m.Rule.MinSU + "-" + m.Rule.MaxSU + ")");
                    if (!m.Rule.Holds(su)) failed.Add(key + " is " + su.ToString("0.00") + " SU as built, bounds " + m.Rule.MinSU + "-" + m.Rule.MaxSU);
                }
                Assert.Greater(judged, 10, "the kit has bounded pieces");
                Assert.IsEmpty(failed, "a bounded kit piece built outside its own bounds is rescaled by the clamp in every scene; fix the mesh or the row: " + string.Join("; ", failed));
            }
            finally { kit.Dispose(); }
        }

        /// <summary>The scatter's camp jitter and the debris step's size jitter never leave the Strict rows: a clamp that
        /// fired on what the composer itself laid would quantise a third of the tins to one size (critique round 5).</summary>
        [Test]
        public void The_Composers_Jitter_Stays_Inside_The_Rows()
        {
            var kit = new BattlefieldKit();
            try
            {
                kit.ResolveKeysAndRules();
                var failed = new List<string>();
                void Check(BattlefieldKit.Module m, float scale, string where)
                {
                    if (m == null || m.Mesh == null || !m.Rule.Bounded || !m.Rule.Enforce) return;
                    float su = AssetScaleTable.SoldierUnits(m.Rule, m.Mesh.bounds.size, Vector3.one * scale);
                    if (!m.Rule.Holds(su)) failed.Add(kit.KeyOf[m] + " at x" + scale.ToString("0.00") + " (" + where + ") is " + su.ToString("0.00") + " SU, bounds " + m.Rule.MinSU + "-" + m.Rule.MaxSU);
                }
                foreach (var m in new[] { kit.ammoTin, kit.messKit, kit.spade, kit.helmet, kit.boots })
                    foreach (var s in new[] { ScatterLayers.CampKitScaleMin, ScatterLayers.CampKitScaleMin + ScatterLayers.CampKitScaleRange }) Check(m, s, "the camp");
                foreach (var s in new[] { BattlefieldComposer.DebrisSizeMin, BattlefieldComposer.DebrisSizeMin + BattlefieldComposer.DebrisSizeRange }) Check(kit.shellCases, s, "the debris step");
                foreach (var s in new[] { ScatterLayers.CasesScaleMin, ScatterLayers.CasesScaleMin + ScatterLayers.DebrisScaleRange }) Check(kit.shellCases, s, "wall debris");
                foreach (var s in new[] { ScatterLayers.BoardsScaleMin, ScatterLayers.BoardsScaleMin + ScatterLayers.DebrisScaleRange }) Check(kit.looseBoards, s, "wall debris");
                foreach (var s in new[] { ScatterLayers.CrateScaleMin, ScatterLayers.CrateScaleMin + ScatterLayers.CrateScaleRange }) Check(kit.supplies, s, "the camp's crates");
                Assert.IsEmpty(failed, "the composer lays a piece the clamp then rescales: widen the row or narrow the jitter: " + string.Join("; ", failed));
            }
            finally { kit.Dispose(); }
        }
    }
}
