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
            /// <summary>Soldier units along the rule's axis over the instances as drawn: styled by the look, then through the
            /// clamp BattlefieldProps.Enforce applies at emit (so an enforcing row is judged on what the eye sees).</summary>
            public float MinSU, MedianSU, MaxSU;
            /// <summary>The same before the clamp: what the composer and the look asked for (equal to the drawn range when
            /// the clamp touched nothing).</summary>
            public float AskedMinSU, AskedMaxSU;
            /// <summary>Placed instances drawn outside the band (only a row the clamp does not enforce can have any).</summary>
            public int Outside;
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
        /// <summary>Every ground the game ships, for the audit and its gate test. The first three are BattlefieldParams
        /// presets; the Narrows has no preset (it is built inline in MatchLaunch.Field, which is the one place a Ground
        /// becomes a battlefield), so it is read from there rather than restated here.</summary>
        public static readonly (string name, System.Func<uint, BattlefieldParams> make)[] Grounds =
            {
                ("ShelledForest", BattlefieldParams.ShelledForest), ("Landing", BattlefieldParams.Landing),
                ("WinterLine", BattlefieldParams.WinterLine),
                ("Narrows", s => MatchLaunch.Field(Ground.Narrows, s)),
            };

        public static List<Row> Measure(BattlefieldKit kit, PropLayout layout, uint seed = 1917, System.Func<uint, BattlefieldParams> ground = null)
        {
            kit.ResolveKeysAndRules();
            var sizes = new Dictionary<BattlefieldKit.Module, List<float>>();
            var clamped = new Dictionary<BattlefieldKit.Module, int>();
            var asked = new Dictionary<BattlefieldKit.Module, Vector2>();
            var map = BattlefieldGenerator.Create((ground ?? BattlefieldParams.ShelledForest)(seed), Allocator.Persistent);
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
                // a hand edit counts in the drawn sizes (and the tail outside the band) but not in the clamp's share or what
                // was asked: the owner sized it on purpose, and the hand-edit table below lists every one the clamp touches
                void Count(BattlefieldKit.Module module, Vector3 scale, Vector3 drawn, bool edit = false)
                {
                    if (!sizes.TryGetValue(module, out var list)) sizes[module] = list = new List<float>();
                    list.Add(AssetScaleTable.SoldierUnits(module.Rule, module.Mesh.bounds.size, drawn));
                    if (edit) return;
                    float before = AssetScaleTable.SoldierUnits(module.Rule, module.Mesh.bounds.size, scale);
                    asked[module] = asked.TryGetValue(module, out var range) ? new Vector2(Mathf.Min(range.x, before), Mathf.Max(range.y, before)) : new Vector2(before, before);
                    if (drawn != scale) { clamped.TryGetValue(module, out int c); clamped[module] = c + 1; }
                }
                // BattlefieldProps.Emit, step for step: the '#n' key of a second prop on one spot, a hand edit in place of
                // the generated prop (PlaceEdit, clamped), a removed one dropped, then the owner's added props
                var spots = new Dictionary<string, int>();
                var byName = new Dictionary<string, BattlefieldKit.Module>();
                foreach (var module in kit.Modules) if (module.Name != null) byName[module.Name] = module;
                composer.Build(map, surface, (module, m) =>
                {
                    if (module.Mesh == null || !module.Rule.Has) return;
                    if (module.Name == null) { var s = m.lossyScale; Count(module, s, AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, s)); return; }
                    string key = PropLayout.GeneratedKey(module.Name, m.GetPosition());
                    spots.TryGetValue(key, out int n); spots[key] = n + 1;
                    if (n > 0) key += "#" + n;
                    var edit = layout?.Find(key);
                    if (edit != null && edit.Removed) return;
                    if (edit != null) { EditScale(edit, module); return; }
                    var look = layout?.LookOf(module.Name);
                    var styled = look != null ? PropLayout.Style(look, m, key, Ground) : m;
                    var scale = styled.lossyScale;
                    Count(module, scale, AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, scale));   // what Enforce does at emit
                });
                void EditScale(PropLayout.Edit edit, BattlefieldKit.Module module)
                {
                    var placed = BattlefieldProps.PlaceEdit(edit, module, layout.SizeOf(edit.Module), 0f, out _);
                    Count(module, edit.Scale * layout.SizeOf(edit.Module), placed.lossyScale, edit: true);
                }
                if (layout != null)
                    foreach (var edit in layout.Edits)
                        if (edit != null && edit.Added && !edit.Removed && edit.Module != null && byName.TryGetValue(edit.Module, out var module) && module.Mesh != null && module.Rule.Has)
                            EditScale(edit, module);
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
                    if (asked.TryGetValue(module, out var range)) { row.AskedMinSU = range.x; row.AskedMaxSU = range.y; }
                    if (module.Rule.Bounded) foreach (var su in list) if (!module.Rule.Holds(su)) row.Outside++;
                }
                clamped.TryGetValue(module, out row.Clamped);
                row.Verdict = AssetScaleTable.Verdict(module.Rule, row.JudgedSU, row.Instances, row.Clamped, row.Outside, row.AskedMinSU, row.AskedMaxSU);
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
              .Append("Drawn sizes are after the look and the clamp `BattlefieldProps.Enforce` applies at emit; the Clamped column counts the instances it pulled in and the range they asked for. ")
              .Append("Verdicts, on the median drawn instance: Strict and Structure rows outside their bounds FAIL, Organic rows warn, Machine rows report; a row the clamp pulled in on more than ")
              .Append((AssetScaleTable.MaxClampedShare * 100f).ToString("0")).Append("% of its instances is CLAMPED (the composer asks for the wrong size), which fails too. ")
              .Append("The standard-view column is the same size against the man as he is grown at zoom 30 (x").Append(FigureMetrics.Grow(30f).ToString("0.00")).Append(").\n\n");
            sb.Append("<!-- gen:asset-scale -->\n");
            sb.Append("| Module | Class | Axis | Bounds SU | Mesh (m) | Look | Placed | Drawn SU min / median / max | Look SU | Standard view | Clamped | Verdict |\n|---|---|---|---|---|---|---|---|---|---|---|---|\n");
            foreach (var r in rows)
            {
                sb.Append("| `").Append(r.Key).Append("` | ").Append(r.Rule.Has ? r.Rule.Class.ToString() : "-").Append(" | ").Append(r.Rule.Has ? r.Rule.Axis.ToString() : "-")
                  .Append(" | ").Append(r.Rule.Bounded || r.Rule.Class == ScaleClass.Organic ? r.Rule.MinSU.ToString("0.00") + "-" + r.Rule.MaxSU.ToString("0.00") : "-")
                  .Append(" | ").Append(V(r.MeshSize)).Append(" | ").Append(r.Baseline == Vector3.one ? "1" : V(r.Baseline))
                  .Append(" | ").Append(r.Instances)
                  .Append(" | ").Append(r.Instances > 0 ? r.MinSU.ToString("0.00") + " / " + r.MedianSU.ToString("0.00") + " / " + r.MaxSU.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Rule.Has ? r.LookSU.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Rule.Has ? r.StandardRatio.ToString("0.00") : "-")
                  .Append(" | ").Append(r.Clamped > 0 ? r.Clamped + " (asked " + r.AskedMinSU.ToString("0.00") + " - " + r.AskedMaxSU.ToString("0.00") + ")" : "0")
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
