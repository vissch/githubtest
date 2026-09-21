// Phase: A4 (implemented) — depends on: MapData (Props, CellCover), NavLayer.Blocked
// Things standing on the battlefield that the sim cares about: trees, what is left of them, wrecks, bridges. A prop
// blocks its own nav cell when it is solid and gives cover to men in the cells around it. Blasts wear props down
// (DeformationSystem): tree -> broken tree -> stump. A destroyed vehicle leaves a wreck.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public enum PropKind : byte { Tree = 0, BrokenTree, Stump, Log, Wreck, Bridge }

    public struct PropDef
    {
        public float3 Pos;
        public float Yaw;
        public float Hp;
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
                default: return 0;
            }
        }

        public static bool Blocks(PropKind kind) => kind == PropKind.Tree || kind == PropKind.BrokenTree || kind == PropKind.Wreck;

        public static float StartHp(PropKind kind) => kind == PropKind.Tree ? 220f : kind == PropKind.BrokenTree ? 160f : 0f;   // 0 = blasts do not change it

        /// <summary>What a prop becomes when its hit points run out; the same kind when it is already at the end.</summary>
        public static PropKind Next(PropKind kind) => kind == PropKind.Tree ? PropKind.BrokenTree : kind == PropKind.BrokenTree ? PropKind.Stump : kind;
    }
}
