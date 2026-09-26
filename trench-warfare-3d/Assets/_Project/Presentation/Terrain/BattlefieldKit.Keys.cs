// Phase: B7 (docs/21 phase 1) — every module has a name the scale table can key on, and carries its rule.
// An imported prop is its "Set/Prop"; a procedural piece is "kit/<field>" (the arrays "kit/TrenchWalls" and so on);
// a building's whole mesh is "house/<Name>"; house chunks and the wholes of sliced props (a Well, a FieldGun) have no
// key of their own, because they are placed by their prop's or house's matrix and must not be clamped apart from it.
using System.Collections.Generic;
using System.Reflection;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldKit
    {
        /// <summary>Marks the Module fields BuildImported fills from Resources/Env: keyed by their asset name
        /// ("Weapons/FieldGun"), never "kit/&lt;field&gt;", so FieldKeys leaves them out.</summary>
        [System.AttributeUsage(System.AttributeTargets.Field)]
        public sealed class ImportedAttribute : System.Attribute { }

        /// <summary>Module → the key AssetScaleTable knows it by. Filled by ResolveKeysAndRules.</summary>
        public readonly Dictionary<Module, string> KeyOf = new Dictionary<Module, string>();

        /// <summary>The kit's procedural field names, "kit/<field>", without building a kit (a test reads them).</summary>
        public static IEnumerable<string> FieldKeys()
        {
            foreach (var f in typeof(BattlefieldKit).GetFields(BindingFlags.Public | BindingFlags.Instance))
                if ((f.FieldType == typeof(Module) || f.FieldType == typeof(Module[])) && f.GetCustomAttribute<ImportedAttribute>() == null) yield return "kit/" + f.Name;
        }

        /// <summary>Names every module and gives it its scale rule. Called once by BattlefieldProps after the kit is
        /// built, and by the audit.</summary>
        public void ResolveKeysAndRules()
        {
            KeyOf.Clear();
            foreach (var f in typeof(BattlefieldKit).GetFields(BindingFlags.Public | BindingFlags.Instance))
            {
                if (f.FieldType == typeof(Module))
                {
                    var m = (Module)f.GetValue(this);
                    if (m != null) KeyOf[m] = m.Name ?? "kit/" + f.Name;
                }
                else if (f.FieldType == typeof(Module[]))
                {
                    var arr = (Module[])f.GetValue(this);
                    if (arr == null) continue;
                    foreach (var m in arr) if (m != null) KeyOf[m] = "kit/" + f.Name;
                }
            }
            var slicedHouses = new HashSet<HouseKit.House>();
            foreach (var m in Modules) if (m.Sliced != null) slicedHouses.Add(m.Sliced);
            foreach (var kv in HouseOfWhole)
                if (!slicedHouses.Contains(kv.Value)) KeyOf[kv.Key] = "house/" + kv.Value.Name;
            foreach (var m in Modules)
            {
                if (!KeyOf.TryGetValue(m, out var key)) { if (m.Name != null) KeyOf[m] = key = m.Name; }
                m.Rule = key != null && AssetScaleTable.TryGet(key, out var rule) ? rule : default;
            }
        }
    }
}
