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

        [Test]
        public void Every_Kit_Piece_And_Imported_Prop_Has_A_Rule()
        {
            var missing = new List<string>();
            foreach (var key in BattlefieldKit.FieldKeys())
                if (key != "kit/Houses" && !AssetScaleTable.TryGet(key, out _)) missing.Add(key);
            // the imported props are named in BattlefieldKit.BuildImported: Imported("Set", "Prop", ...)
            string src = File.ReadAllText(Path.Combine(Application.dataPath, "_Project", "Presentation", "Terrain", "BattlefieldKit.cs"));
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

        /// <summary>The whole audit: builds the kit, composes the field and judges every module. Explicit because it
        /// takes seconds and every material in the kit; Editor/AssetScaleAudit runs the same and writes the report.</summary>
        [Test, Explicit("builds the whole kit and composes a field; run it by name, or the audit menu")]
        public void The_Composed_Field_Stands_In_Proportion_To_The_Man()
        {
            var kit = new BattlefieldKit();
            try
            {
                var layout = Resources.Load<PropLayout>(PropLayout.ResourcePath((int)Seed));
                var rows = AssetScaleReport.Measure(kit, layout, Seed);
                Assert.Greater(rows.Count, 40, "the kit has many modules");
                var failed = new List<string>();
                foreach (var r in rows)
                {
                    TestContext.WriteLine(r.Key + ": " + r.Verdict + " " + r.JudgedSU.ToString("0.00") + " SU x" + r.Instances);
                    if (r.Verdict == "FAIL") failed.Add(r.Key + " " + r.JudgedSU.ToString("0.00") + " SU");
                }
                Assert.IsEmpty(failed, "man-made things out of proportion to the man: " + string.Join("; ", failed));
            }
            finally { kit.Dispose(); }
        }
    }
}
