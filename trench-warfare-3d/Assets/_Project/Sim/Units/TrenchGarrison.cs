// Phase: A3 (stub) — depends on: TrenchDef, MovementSystem (A1), Suppression
// Units entering a Trench cell of a trench that is not Locked stop and garrison (TrenchId set, InTrench flag,
// Crouch below the rim). When firing they snap to the fire-step (FireStep stance, head exposed only). Under
// suppression > 40 they stay below the rim and hold fire. Emits UnitEnteredTrench / UnitLeftTrench / TrenchCaptured.
namespace TW.Sim.Units
{
    public sealed class TrenchGarrisonSystem : ISimSystem
    {
        public int Order => SimSystemOrder.TrenchGarrison;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: TrenchGarrisonSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
