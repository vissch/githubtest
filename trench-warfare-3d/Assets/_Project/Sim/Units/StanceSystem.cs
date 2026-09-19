// Phase: A3 (stub) — depends on: Suppression (A2), TrenchGarrison, UnitStance command
// Resolves stance each tick: Pinned > forced Prone > FireStep (firing from trench) > Crouch (in trench) >
// Sprint (advance/fallback order) > player override > Standing. Speed multipliers per stance live in StanceRules.
namespace TW.Sim.Units
{
    public static class StanceRules
    {
        public static float SpeedMultiplier(Stance s)
        {
            switch (s)
            {
                case Stance.Crouch: return 0.8f;
                case Stance.Prone: return 0.4f;
                case Stance.Sprint: return 1.5f;
                case Stance.FireStep: case Stance.Pinned: return 0f;
                default: return 1f;
            }
        }
        public static float CoverBonusInOpen(Stance s) => s == Stance.Prone || s == Stance.Pinned ? 0.4f : s == Stance.Crouch ? 0.1f : s == Stance.Sprint ? -0.1f : 0f;
        public static float AccuracyMultiplier(Stance s, bool machinegun) => s == Stance.FireStep ? 1.25f : s == Stance.Prone ? (machinegun ? 1.2f : 0.9f) : 1f;
    }

    public sealed class StanceSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Stance;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: StanceSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
