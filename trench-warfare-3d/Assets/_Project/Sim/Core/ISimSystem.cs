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
        public const int Command = 100;
        public const int Mission = 200;
        public const int Economy = 300;
        public const int FlowField = 400;
        public const int SpatialHash = 500;
        public const int TargetAcquisition = 600;
        public const int DirectFire = 700;
        public const int IndirectFire = 710;
        public const int Blast = 720;
        public const int Suppression = 800;
        public const int Stance = 810;
        public const int TrenchGarrison = 820;
        public const int GasSmoke = 900;
        public const int Deformation = 1000;
        public const int Separation = 1100;
        public const int Movement = 1110;
        public const int VehicleKinematics = 1120;
        public const int SectorControl = 1200;
        public const int Death = 1300;
    }
}
