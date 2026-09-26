// Phase: B7 (docs/21 phase 1) — measures the composed field against the soldier, module by module.
// No scene: the real battlefield is generated (BattlefieldGenerator), surfaced (BattlefieldSurface) and composed
// (BattlefieldComposer) into a list, every instance is styled exactly as BattlefieldProps styles it (the look, then
// the clamp), and each module reports how many soldiers tall or long it is drawn. The EditMode test reads the rows;
// Editor/AssetScaleAudit writes them to docs/reference/asset-scale.md.
using System.Collections.Generic;
using System.Text;
using Unity.Collections;
using UnityEngine;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public static class AssetScaleReport
    {
        public sealed class Row
        {
            public string Key;
            public ScaleRule Rule;
            public Vector3 MeshSize;
            /// <summary>The look's baseline (Restyle's Module.Size before the clamp), or one.</summary>
            public Vector3 Baseline;
            public int Instances;
            /// <summary>Soldier units along the rule's axis over the instances the composer placed, before the clamp.</summary>
            public float MinSU, MedianSU, MaxSU;
            /// <summary>Soldier units of mesh × baseline: what the look alone gives, placed or not.</summary>
            public float LookSU;
            public int Clamped;
            public string Verdict;
            /// <summary>The judged size: the median placed instance, or the look when nothing was placed.</summary>
            public float JudgedSU => Instances > 0 ? MedianSU : LookSU;
            /// <summary>The same size against the man as he is drawn at the standard view (zoom 30): what the eye compares.</summary>
            public float StandardRatio => JudgedSU / FigureMetrics.Grow(30f);
        }

        /// <summary>Every module of the kit, measured on the composed field of a seed. The kit must be built.</summary>
        public static List<Row> Measure(BattlefieldKit kit, PropLayout layout, uint seed = 1917)
        {
            kit.ResolveKeysAndRules();
            var sizes = new Dictionary<BattlefieldKit.Module, List<float>>();
            var clamped = new Dictionary<BattlefieldKit.Module, int>();
            var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(seed), Allocator.Persistent);
            try
            {
                var surface = new BattlefieldSurface(map);
                foreach (var module in kit.Modules)
                    if (module.Name != null)
                    {
                        var baseline = layout?.LookOf(module.Name)?.Baseline ?? Vector3.one;
                        module.Size = module.Mesh != null ? AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, baseline) : baseline;
                    }
                var composer = new BattlefieldComposer(kit, (int)seed);
                float Ground(float x, float z) => surface.VisualHeight(x, z);
                composer.Build(map, surface, (module, m) =>
                {
                    if (module.Mesh == null || !module.Rule.Has) return;
                    var matrix = m;
                    if (module.Name != null)
                    {
                        var look = layout?.LookOf(module.Name);
                        if (look != null) matrix = PropLayout.Style(look, m, PropLayout.GeneratedKey(module.Name, m.GetPosition()), Ground);
                    }
                    var scale = matrix.lossyScale;
                    float su = AssetScaleTable.SoldierUnits(module.Rule, module.Mesh.bounds.size, scale);
                    if (!sizes.TryGetValue(module, out var list)) sizes[module] = list = new List<float>();
                    list.Add(su);
                    if (AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, scale) != scale) { clamped.TryGetValue(module, out int c); clamped[module] = c + 1; }
                });
            }
            finally { map.Dispose(); }

            var rows = new List<Row>();
            foreach (var module in kit.Modules)
            {
                if (!kit.KeyOf.TryGetValue(module, out var key)) continue;   // a chunk, or the whole of a sliced prop
                var meshSize = kit.HouseOfWhole.TryGetValue(module, out var house) ? house.Bounds.size : module.Mesh != null ? module.Mesh.bounds.size : Vector3.zero;
                var baseline = module.Name != null ? (layout?.LookOf(module.Name)?.Baseline ?? Vector3.one) : Vector3.one;
                var row = new Row { Key = key, Rule = module.Rule, MeshSize = meshSize, Baseline = baseline };
                if (module.Rule.Has) row.LookSU = AssetScaleTable.SoldierUnits(module.Rule, meshSize, baseline);
                if (sizes.TryGetValue(module, out var list) && list.Count > 0)
                {
                    list.Sort();
                    row.Instances = list.Count; row.MinSU = list[0]; row.MaxSU = list[list.Count - 1]; row.MedianSU = list[list.Count / 2];
                }
                clamped.TryGetValue(module, out row.Clamped);
                row.Verdict = AssetScaleTable.Verdict(module.Rule, row.JudgedSU);
                rows.Add(row);
            }
            // the machines: their footprints, as the sim keeps them (VehicleSize already baked in)
            foreach (var (name, archetype) in new[] { ("Maw", 4), ("Tusk", 5), ("Pincer", 6), ("Kettle", 7), ("Censer", 8), ("Pavise", 9), ("Banner", 10), ("Redoubt", 11) })
            {
                var profile = VehicleProfile.ForArchetype((byte)archetype);
                var size = new Vector3(profile.HalfWidth * 2f, 0f, profile.HalfLength * 2f);
                AssetScaleTable.TryGet("vehicle/" + name, out var rule);
                var row = new Row { Key = "vehicle/" + name, Rule = rule, MeshSize = size, Baseline = Vector3.one, LookSU = AssetScaleTable.SoldierUnits(rule, size, Vector3.one) };
                row.Verdict = AssetScaleTable.Verdict(rule, row.LookSU);
                rows.Add(row);
            }
            rows.Sort((a, b) => string.CompareOrdinal(a.Key, b.Key));
            return rows;
        }

        /// <summary>The hand edits of bounded kinds, and what the clamp does to each: (key, module, raw SU, clamped SU).</summary>
        public static List<(string key, string module, float rawSU, float clampedSU)> Edits(PropLayout layout)
        {
            var list = new List<(string, string, float, float)>();
            if (layout == null) return list;
            foreach (var edit in layout.Edits)
            {
                if (edit == null || edit.Removed || edit.Module == null) continue;
                if (!AssetScaleTable.TryGet(edit.Module, out var rule) || !rule.Bounded) continue;
                var mesh = Resources.Load<Mesh>("Env/" + edit.Module);
                if (mesh == null) continue;
                var scale = edit.Scale * layout.SizeOf(edit.Module);
                float raw = AssetScaleTable.SoldierUnits(rule, mesh.bounds.size, scale);
                float after = AssetScaleTable.SoldierUnits(rule, mesh.bounds.size, AssetScaleTable.Clamp(rule, mesh.bounds.size, scale));
                list.Add((edit.Key, edit.Module, raw, after));
            }
            return list;
        }

        public static string Markdown(List<Row> rows, List<(string key, string module, float rawSU, float clampedSU)> edits, uint seed)
        {
            var sb = new StringBuilder();
            sb.Append("# Asset scale audit\n\n");
            sb.Append("Generated by `TW/Audit/Asset Scale` (`Editor/AssetScaleAudit.cs`) from the composed field of seed ").Append(seed)
              .Append("; 1 SU = the drawn man, ").Append(AssetScaleTable.SoldierM.ToString("0.00")).Append(" m (`FigureMetrics.HeightM`). ")
              .Append("Verdicts: Strict and Structure rows outside their bounds FAIL, Organic rows warn, Machine rows report. ")
              .Append("The standard-view column is the same size against the man as he is grown at zoom 30 (x").Append(FigureMetrics.Grow(30f).ToString("0.00")).Append(").\n\n");
            sb.Append("<!-- gen:asset-scale -->\n");
            sb.Append("| Module | Class | Axis | Bounds SU | Mesh (m) | Look | Placed | Drawn SU min / median / max | Look SU | Standard view | Clamped | Verdict |\n|---|---|---|---|---|---|---|---|---|---|---|---|\n");
            foreach (var r in rows)
            {
                sb.Append("| `").Append(r.Key).Append("` | ").Append(r.Rule.Has ? r.Rule.Class.ToString() : "-").Append(" | ").Append(r.Rule.Has ? r.Rule.Axis.ToString() : "-")
                  .Append(" | ").Append(r.Rule.Bounded ? r.Rule.MinSU.ToString("0.00") + "-" + r.Rule.MaxSU.ToString("0.00") : "-")
                  .Append(" | ").Append(V(r.MeshSize)).Append(" | ").Append(r.Baseline == Vector3.one ? "1" : V(r.Baseline))
                  .Append(" | ").Append(r.Instances)
                  .Append(" | ").Append(r.Instances > 0 ? r.MinSU.ToString("0.00") + " / " + r.MedianSU.ToString("0.00") + " / " + r.MaxSU.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Rule.Has ? r.LookSU.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Rule.Has ? r.StandardRatio.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Clamped)
                  .Append(" | ").Append(r.Verdict).Append(" |\n");
            }
            sb.Append("<!-- /gen:asset-scale -->\n\n## Hand edits of bounded kinds\n\n");
            if (edits.Count == 0) sb.Append("None.\n");
            else
            {
                sb.Append("| Edit | Module | Raw SU | After the clamp | Touched |\n|---|---|---|---|---|\n");
                foreach (var e in edits)
                    sb.Append("| `").Append(e.key).Append("` | `").Append(e.module).Append("` | ").Append(e.rawSU.ToString("0.00")).Append(" | ").Append(e.clampedSU.ToString("0.00"))
                      .Append(" | ").Append(Mathf.Abs(e.rawSU - e.clampedSU) > 1e-3f ? "yes" : "no").Append(" |\n");
            }
            return sb.ToString();
        }

        static string V(Vector3 v) => v.x.ToString("0.00") + " x " + v.y.ToString("0.00") + " x " + v.z.ToString("0.00");
    }
}
