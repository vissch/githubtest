// Phase: P0 (implemented)
// Systems are registered on SimWorld in a fixed order (see docs/04-architecture.md, "Sim system order").
// A system may hold its own NativeArrays; any array that is part of authoritative state must be hashed.
namespace TW.Sim
{
    public interface ISimSystem
    {
        /// <summary>Stable order key. Lower runs first. Values are listed in SimSystemOrder.</summary>
        int Order { get; }
        void Initialize(SimWorld world);
        void Step(SimWorld world);
        /// <summary>Fold system-owned authoritative state into the tick hash.</summary>
        ulong Hash(ulong h);
        void Dispose();
    }

    public static class SimSystemOrder
    {
        /// <summary>Steps nothing: it is the system that folds MapData (the heightfield, the nav layers, the holes
        /// the shells have dug) into the tick hash, which SimWorld cannot do itself because TW.Sim.Core does not
        /// reference TW.Sim.Terrain. First, so a diverging map is named before anything reads it.</summary>
        public const int TerrainHash = 50;
        public const int Command = 100;
        public const int Mission = 200;
        public const int Economy = 300;
        public const int FlowField = 400;
        public const int SpatialHash = 500;
        public const int TargetAcquisition = 600;
        /// <summary>Officer auras: the damage and suppression multipliers DirectFire reads, rewritten every tick.</summary>
        public const int Aura = 650;
        public const int DirectFire = 700;
        public const int IndirectFire = 710;
        public const int AmbientBombardment = 715;
        public const int Blast = 720;
        /// <summary>Buildings: what a burst broke, which storeys come down next, and the masonry that falls on the
        /// men underneath. After Blast (it reads Resolved) and Burning, before VehicleModules.</summary>
        public const int Building = 728;
        /// <summary>Medics and repair engineers: after VehicleModules has written the modules, before Suppression.</summary>
        public const int Support = 735;
        public const int Suppression = 800;
        /// <summary>Heroes: after Suppression decayed, before Stance/Garrison read it. Not 810: Stance holds that and
        /// AddSystem's List.Sort is unstable, so two systems at one order step in an order nobody chose.</summary>
        public const int Hero = 805;
        public const int Stance = 810;
        public const int TrenchGarrison = 820;
        public const int GasSmoke = 900;
        public const int Deformation = 1000;
        public const int Separation = 1100;
        /// <summary>Jetpack leaps: decided before Movement moves him along the line it sets.</summary>
        public const int Leap = 1105;
        public const int Movement = 1110;
        /// <summary>The Breaker's phases: after Movement listed the vehicles, before Kinematics drives them.</summary>
        public const int Breaker = 1115;
        public const int VehicleKinematics = 1120;
        public const int SectorControl = 1200;
        public const int Death = 1300;
    }
}
