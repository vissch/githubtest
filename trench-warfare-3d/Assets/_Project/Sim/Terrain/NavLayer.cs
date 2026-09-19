// Phase: P0 (implemented) — Map data contract, see docs/02-contracts.md
namespace TW.Sim.Terrain
{
    [System.Flags]
    public enum NavLayer : byte
    {
        None    = 0,
        Surface = 1 << 0,   // open ground
        Trench  = 1 << 1,   // below the parapet; protected from horizontal fire
        Link    = 1 << 2,   // ladder / ramp / parapet vault connecting Surface and Trench
        Blocked = 1 << 3,   // impassable (bunker wall, deep water, map edge)
        Wire    = 1 << 4,   // barbed wire belt (cost 40 until breached)
        Mud     = 1 << 5,   // speed 0.5x, vehicle bog checks
        Crater  = 1 << 6,   // shell hole: low 360° cover, gas sink
        Bunker  = 1 << 7,   // interior of a concrete emplacement
    }

    public static class NavCosts
    {
        public const byte Surface = 1;
        public const byte Trench = 1;
        public const byte Link = 6;
        public const byte Mud = 4;
        public const byte Crater = 2;
        public const byte Wire = 40;
        public const byte Blocked = 255;

        public static byte For(NavLayer layer)
        {
            if ((layer & NavLayer.Blocked) != 0) return Blocked;
            if ((layer & NavLayer.Wire) != 0) return Wire;
            if ((layer & NavLayer.Link) != 0) return Link;
            if ((layer & NavLayer.Mud) != 0) return Mud;
            if ((layer & NavLayer.Crater) != 0) return Crater;
            return Surface;
        }
    }
}
