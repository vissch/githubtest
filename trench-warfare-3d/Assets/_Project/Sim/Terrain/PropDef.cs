// Phase: A4 (implemented) — depends on: MapData (Props, CellCover), NavLayer.Blocked
// Things standing on the battlefield that the sim cares about: trees, what is left of them, wrecks, bridges. A prop
// blocks its own nav cell when it is solid and gives cover to men in the cells around it. Blasts wear props down
// (DeformationSystem): tree -> broken tree -> stump. A destroyed vehicle leaves a wreck, which breaks in stages
// (owner, 2026-09-28): wreck -> broken wreck -> scrap -> cleared. A cleared prop is gone from the field (no block, no
// cover, never drawn) but keeps its index, because events and WreckRecord.PropIndex name props by index.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    /// <summary>Appended only: the kind is hashed as its number.</summary>
    public enum PropKind : byte { Tree = 0, BrokenTree, Stump, Log, Wreck, Bridge, BrokenWreck, Scrap, Cleared }

    public struct PropDef
    {
        public float3 Pos;
        public float Yaw;
        public float Hp;
        public float Scale;     // drawn size, 0 = unset (1). Clumps have one big member, some medium, many small
        public int Cell;        // nav cell index, set by MapData.AddProp
        public PropKind Kind;
    }

    public static class PropRules
    {
        /// <summary>Hit-chance reduction (percent) for a man in the prop's cell or one of the eight around it.</summary>
        public static byte CoverPercent(PropKind kind)
        {
            switch (kind)
            {
                case PropKind.Tree: return 30;
                case PropKind.BrokenTree: return 30;
                case PropKind.Stump: return 20;
                case PropKind.Log: return 35;
                case PropKind.Wreck: return 50;
                case PropKind.BrokenWreck: return 35;
                case PropKind.Scrap: return 15;
                default: return 0;
            }
        }

        /// <summary>A scrap pile no longer blocks: men and machines go over it.</summary>
        public static bool Blocks(PropKind kind) => kind == PropKind.Tree || kind == PropKind.BrokenTree || kind == PropKind.Wreck || kind == PropKind.BrokenWreck;

        /// <summary>A prop's hit points when it becomes this kind; 0 = blasts do not change it (a stump, a log, a bridge, a
        /// cleared wreck). A wreck's stages are for a wreck of size 1: StartHp(kind, scale) sizes them to their machine.
        /// About three to five heavy shells a stage (a barrage shell is 150, a Maw's HE 220).</summary>
        public static float StartHp(PropKind kind)
        {
            switch (kind)
            {
                case PropKind.Tree: return 220f;
                case PropKind.BrokenTree: return 160f;
                case PropKind.Wreck: return 600f;
                case PropKind.BrokenWreck: return 450f;
                case PropKind.Scrap: return 250f;
                default: return 0f;
            }
        }

        /// <summary>StartHp, a wreck's sized by its PropDef.Scale (WreckSize: a Maw's 1.5, a Skimmer's 0.6).</summary>
        public static float StartHp(PropKind kind, float scale) => StartHp(kind) * (IsWreckage(kind) && scale > 0f ? scale : 1f);

        /// <summary>A burst this many metres (times the wreck's size) from a wreck's middle counts as on it: the hull is
        /// several metres long, and a shell on its deck should not be measured from the middle of it.</summary>
        public const float WreckBlastReach = 2f;

        /// <summary>What a prop becomes when its hit points run out; the same kind when it is already at the end.</summary>
        public static PropKind Next(PropKind kind)
        {
            switch (kind)
            {
                case PropKind.Tree: return PropKind.BrokenTree;
                case PropKind.BrokenTree: return PropKind.Stump;
                case PropKind.Wreck: return PropKind.BrokenWreck;
                case PropKind.BrokenWreck: return PropKind.Scrap;
                case PropKind.Scrap: return PropKind.Cleared;
                default: return kind;
            }
        }

        /// <summary>A wreck at any stage that is still on the field (not yet cleared).</summary>
        public static bool IsWreckage(PropKind kind) => kind == PropKind.Wreck || kind == PropKind.BrokenWreck || kind == PropKind.Scrap;

        /// <summary>A wreck's size class, kept in PropDef.Scale: its machine's hull hit points over WreckSizeHp, clamped. A Maw
        /// (3600) leaves a wreck half again as tough as a Tusk's (2000 x 1.5 / 1.67); the map generator's wrecks are 1.</summary>
        public const float WreckSizeHp = 2400f, WreckSizeMin = 0.6f, WreckSizeMax = 1.5f;
        public static float WreckSize(float hullHp) => math.clamp(hullHp / WreckSizeHp, WreckSizeMin, WreckSizeMax);
    }
}
