// Phase: wrecks (2026-09-28, the seam) — a prop named where a slot is expected: Shot.b is the target's slot, or a wreck
// a gun fired at (idle guns fire at a wreck that shelters enemies, owner 2026-09-28: "automatic only"). A prop is
// -2 - its index, so every prop is <= -2 and -1 stays "no target". docs/02-contracts.md.
namespace TW.Sim
{
    public static class PropTarget
    {
        public static int Encode(int prop) => -2 - prop;
        public static bool IsProp(int target) => target <= -2;
        public static int Decode(int target) => -2 - target;
    }
}
