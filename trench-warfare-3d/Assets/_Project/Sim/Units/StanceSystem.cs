// Phase: A3 (stub) — depends on: Suppression (A2), TrenchGarrison, UnitStance command
// Resolves stance each tick: Pinned > forced Prone > FireStep (firing from trench) > Crouch (in trench) >
// Sprint (advance/fallback order) > player override > Standing. Speed multipliers per stance live in
// TW.Sim.StanceRules (Core), which the A1 movement job already applies for Crouch/Sprint/Standing/Vault.
namespace TW.Sim.Units
{
    public sealed class StanceSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Stance;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: StanceSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
