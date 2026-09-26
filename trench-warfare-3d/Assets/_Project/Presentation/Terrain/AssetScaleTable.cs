// Phase: B7 (docs/21 phase 1, 2026-09-26) — the soldier is the unit: every man-made thing is drawn in proportion to him.
//
// One soldier unit (SU) is the man's true drawn height, FigureMetrics.HeightM (the 1.78 m bake at UnitScale, 2.0 m).
// The readability grow that makes him 2.5 m at the standard view is a men-only exception, like the giant machines
// (owner, 2026-09-26: man-made props true to the soldier; machines stay giant). Every module the kit draws has a
// row here, keyed the way BattlefieldKit.Keys names it ("Stones/Sandbag" for an imported prop, "kit/helmet" for a
// procedural piece, "house/Watchtower" for a building, "vehicle/Maw" for a machine), and the row says what class
// of thing it is and how big it may be drawn along one axis:
//   Strict     a thing men made to a size (a door, a crate, a helmet, a gun): bounded, and BattlefieldProps clamps
//              any instance outside the bounds at emit time (Enforce), whatever the look or the hand edit said.
//   Structure  a building or a shelter: bounded and reported; clamped only when the row says so, because a house's
//              chunks are placed by their own matrices and would come apart from a clamped whole.
//   Organic    rocks, stumps, logs, debris, plants: free to vary (the audit only warns), and tinted per instance.
//   Machine    vehicles and wrecks: reported against the man and never touched (VehicleSize is the owner's).
// The audit (AssetScaleReport, AssetScaleTests, Editor/AssetScaleAudit) measures the composed field against these
// rows; the looks that were learned when men were 2.67 m are corrected by Tools/looks.py.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public enum ScaleClass : byte { Strict = 0, Structure = 1, Organic = 2, Machine = 3 }

    /// <summary>Which side of the mesh the rule bounds: its height, its longest ground side, or its shortest.</summary>
    public enum ScaleAxis : byte { Height, Length, Width }

    public readonly struct ScaleRule
    {
        public readonly ScaleClass Class;
        public readonly ScaleAxis Axis;
        public readonly float MinSU, MaxSU;
        public readonly string Note;
        /// <summary>False for default(ScaleRule): a module nobody has classed yet.</summary>
        public readonly bool Has;
        /// <summary>Clamped at emit time. Strict rows always; Structure rows only when they say so.</summary>
        public readonly bool Enforce;

        public ScaleRule(ScaleClass cls, ScaleAxis axis, float minSU, float maxSU, string note = "", bool enforce = true)
        {
            Class = cls; Axis = axis; MinSU = minSU; MaxSU = maxSU; Note = note ?? ""; Has = true;
            Enforce = enforce && cls <= ScaleClass.Structure;
        }

        /// <summary>A bounded class: Strict or Structure.</summary>
        public bool Bounded => Has && Class <= ScaleClass.Structure;
        public bool Holds(float su) => su >= MinSU && su <= MaxSU;
    }

    public static class AssetScaleTable
    {
        /// <summary>One soldier unit, in metres: the man as drawn at close zoom.</summary>
        public const float SoldierM = FigureMetrics.HeightM;

        static readonly Dictionary<string, ScaleRule> rules = Build();
        public static IReadOnlyDictionary<string, ScaleRule> Rules => rules;

        /// <summary>The rule for a key, or a prefix default: any "house/..." is a building, any "vehicle/..." a machine.</summary>
        public static bool TryGet(string key, out ScaleRule rule)
        {
            if (key == null) { rule = default; return false; }
            if (rules.TryGetValue(key, out rule)) return true;
            if (key.StartsWith("house/")) { rule = new ScaleRule(ScaleClass.Structure, ScaleAxis.Height, 2.0f, 5.5f, "a building (prefix default)", enforce: false); return true; }
            if (key.StartsWith("vehicle/")) { rule = new ScaleRule(ScaleClass.Machine, ScaleAxis.Length, 0f, float.PositiveInfinity, "a machine (prefix default)"); return true; }
            rule = default; return false;
        }

        public static float Along(ScaleAxis axis, Vector3 size)
            => axis == ScaleAxis.Height ? size.y : axis == ScaleAxis.Length ? Mathf.Max(size.x, size.z) : Mathf.Min(size.x, size.z);

        static Vector3 Abs(Vector3 v) => new Vector3(Mathf.Abs(v.x), Mathf.Abs(v.y), Mathf.Abs(v.z));

        /// <summary>How many soldiers tall (or long) a mesh is when drawn at a scale.</summary>
        public static float SoldierUnits(in ScaleRule rule, Vector3 meshSize, Vector3 scale)
            => Along(rule.Axis, Vector3.Scale(meshSize, Abs(scale))) / SoldierM;

        /// <summary>The scale pulled into the rule's bounds by one uniform factor, so a look's proportions survive. A
        /// rule that does not enforce, or a mesh with no size along the axis, gives the scale back untouched.</summary>
        public static Vector3 Clamp(in ScaleRule rule, Vector3 meshSize, Vector3 scale)
        {
            if (!rule.Enforce) return scale;
            float su = SoldierUnits(rule, meshSize, scale);
            if (su <= 1e-5f) return scale;
            float f = su < rule.MinSU ? rule.MinSU / su : su > rule.MaxSU ? rule.MaxSU / su : 1f;
            return f == 1f ? scale : scale * f;
        }

        public static string Verdict(in ScaleRule rule, float su)
        {
            if (!rule.Has) return "no rule";
            if (rule.Class == ScaleClass.Machine) return "report";
            if (rule.Holds(su)) return "OK";
            return rule.Class == ScaleClass.Organic ? "warn" : "FAIL";
        }

        static Dictionary<string, ScaleRule> Build()
        {
            var t = new Dictionary<string, ScaleRule>();
            void S(string key, ScaleAxis axis, float min, float max, string note = "") => t[key] = new ScaleRule(ScaleClass.Strict, axis, min, max, note);
            void B(string key, ScaleAxis axis, float min, float max, string note = "", bool enforce = true) => t[key] = new ScaleRule(ScaleClass.Structure, axis, min, max, note, enforce);
            void O(string key, string note = "") => t[key] = new ScaleRule(ScaleClass.Organic, ScaleAxis.Height, 0.1f, 3.0f, note);
            void M(string key, string note = "") => t[key] = new ScaleRule(ScaleClass.Machine, ScaleAxis.Length, 0f, float.PositiveInfinity, note);

            // ---- the trench lining and its kit --------------------------------------------------------------------
            S("Stones/Sandbag", ScaleAxis.Length, 0.30f, 0.55f, "a filled sack: 0.6-1.1 m long");
            S("kit/sandbags", ScaleAxis.Height, 0.25f, 0.45f, "the procedural parapet sack");
            S("kit/TrenchBags", ScaleAxis.Height, 0.25f, 0.45f, "a parapet course");
            B("kit/TrenchWalls", ScaleAxis.Height, 0.50f, 1.40f, "a revetment: waist to head high");
            B("kit/TrenchWallsDamaged", ScaleAxis.Height, 0.25f, 1.40f, "a broken revetment, drawn at its whole twin's matrix", enforce: false);
            S("kit/TrenchBagsDamaged", ScaleAxis.Height, 0.08f, 0.45f, "a burst course: the lower sacks");
            S("kit/TrenchFloorsDamaged", ScaleAxis.Length, 0.90f, 1.10f, "broken duckboards on their rails");
            B("kit/planks", ScaleAxis.Height, 0.50f, 1.40f, "never placed");
            S("kit/TrenchFloors", ScaleAxis.Length, 0.90f, 1.10f, "duckboards for a 2 m cell");
            S("kit/duckboards", ScaleAxis.Length, 0.90f, 1.10f);
            S("kit/ladder", ScaleAxis.Height, 1.00f, 1.40f, "a trench ladder reaches the parapet");
            S("kit/knifeRest", ScaleAxis.Height, 0.50f, 0.85f);
            O("kit/wire", "a wire belt follows the map");
            // ---- doors, boards, sheets, crates and small kit --------------------------------------------------------
            S("Wood/PlankDoor", ScaleAxis.Height, 0.85f, 1.15f, "a door a man walks through");
            S("Wood/HatchLid", ScaleAxis.Length, 0.35f, 0.50f);
            S("Wood/BracedPlank", ScaleAxis.Length, 0.55f, 0.90f);
            S("Wood/CrossedBoards", ScaleAxis.Length, 0.55f, 0.90f);
            S("Wood/CorrugatedSheet", ScaleAxis.Length, 0.50f, 1.10f);
            S("kit/supplies", ScaleAxis.Length, 0.30f, 0.50f, "an ammunition crate two men lift");
            S("kit/looseBoards", ScaleAxis.Length, 0.40f, 1.10f);
            S("kit/shellCases", ScaleAxis.Length, 0.20f, 0.35f);
            S("kit/helmet", ScaleAxis.Length, 0.13f, 0.18f, "a helmet fits a head");
            S("kit/boots", ScaleAxis.Length, 0.12f, 0.17f);
            S("kit/ammoTin", ScaleAxis.Length, 0.14f, 0.24f);
            S("kit/messKit", ScaleAxis.Height, 0.10f, 0.22f);
            S("kit/bucket", ScaleAxis.Height, 0.10f, 0.22f);
            S("kit/lantern", ScaleAxis.Height, 0.60f, 0.90f, "a lamp on a post: chest to head high");
            S("kit/hangingTins", ScaleAxis.Height, 0.10f, 0.22f);
            S("kit/wireTins", ScaleAxis.Height, 0.10f, 0.22f);
            S("kit/spade", ScaleAxis.Height, 0.25f, 0.40f, "an entrenching tool");
            S("kit/leanRifle", ScaleAxis.Height, 0.50f, 0.70f, "a rifle is as long as a man's leg and a half");
            S("kit/graveMarker", ScaleAxis.Height, 0.50f, 0.70f);
            S("kit/signBoard", ScaleAxis.Height, 0.60f, 0.85f);
            O("kit/phoneWire"); O("kit/rag");
            // ---- weapons and emplacements -----------------------------------------------------------------------------
            S("Weapons/FieldGun", ScaleAxis.Length, 2.00f, 3.00f, "a field gun with its trail: 4-6 m; its wheel reaches a man's chest");
            S("Weapons/ShellStack", ScaleAxis.Height, 0.40f, 0.60f);
            S("Weapons/WreckedLimber", ScaleAxis.Height, 0.55f, 0.80f);
            S("Weapons/DudShell", ScaleAxis.Height, 0.25f, 0.50f);
            S("Stones/Gabion", ScaleAxis.Height, 0.30f, 0.55f, "a gabion is knee to waist high");
            S("Fence/StoneBarricade", ScaleAxis.Height, 0.40f, 0.60f);
            S("Fence/Stakes", ScaleAxis.Height, 0.50f, 0.85f);
            S("Fence/TimberHedgehog", ScaleAxis.Height, 0.50f, 0.85f);
            S("Fence/WireFence", ScaleAxis.Height, 0.50f, 0.85f);
            S("Fence/WirePost", ScaleAxis.Height, 0.50f, 0.85f);
            S("Siege/MGNest", ScaleAxis.Height, 0.60f, 1.00f, "a sandbagged nest a gunner kneels behind");
            B("Siege/SodShelterRuin", ScaleAxis.Height, 1.20f, 1.70f, "a shelter a man ducks into");
            B("Siege/ArmouredStand", ScaleAxis.Height, 1.50f, 2.20f, "an observation stand");
            B("Siege/Pillbox", ScaleAxis.Height, 1.00f, 1.80f);
            B("Siege/Well", ScaleAxis.Height, 1.00f, 1.80f);
            B("kit/dugout", ScaleAxis.Height, 1.00f, 1.80f); B("kit/bunker", ScaleAxis.Height, 1.00f, 1.80f); B("kit/roof", ScaleAxis.Height, 1.00f, 1.80f);
            B("Stones/WallStub", ScaleAxis.Height, 0.80f, 1.40f);
            O("kit/ruin", "the horizon's ruins are scaled by Horizon() to 5-9 m (docs/21 phase 1)");
            M("kit/bridge", "sized to the river");
            // ---- buildings: reported, never clamped (their chunks are placed by their own matrices) -------------------
            foreach (var h in new[] { "House0", "House1", "House2", "House3", "House4", "House5" }) B("house/" + h, ScaleAxis.Height, 2.00f, 4.00f, "a village house: one to two storeys", enforce: false);
            foreach (var h in new[] { "CommandPost", "Blockhouse", "GuardPost" }) B("house/" + h, ScaleAxis.Height, 2.00f, 3.50f, "a rear building", enforce: false);
            B("house/Watchtower", ScaleAxis.Height, 3.00f, 4.00f, "a watchtower over the wire", enforce: false);
            // ---- organic: free ----------------------------------------------------------------------------------------
            foreach (var p in new[] { "Plants/FallenLog", "Plants/SplitStumpTall", "Plants/SplitStump", "Plants/MossStump", "Plants/Poppies", "Plants/Cattails", "Plants/GrassClump",
                                      "Stones/Boulder", "Stones/RebarSlab" }) O(p);
            foreach (var k in new[] { "trunk", "snag", "fallen", "stump", "fork", "branches", "bush", "tuft", "stones", "reeds", "icicles", "drift", "iceShard", "frostTuft", "snowClod", "grassMicro", "poppiesMicro" }) O("kit/" + k);
            // ---- machines: reported only -----------------------------------------------------------------------------
            M("Weapons/TankTurret", "a turret off a wreck"); M("Weapons/Biplane"); M("kit/wreck", "a wrecked hull: 7.4 m at scale 1");
            foreach (var v in new[] { "Maw", "Tusk", "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt" }) M("vehicle/" + v, "VehicleSize is the owner's");
            return t;
        }
    }
}
