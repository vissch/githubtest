// Phase: A3 (implemented 2026-09-25) — who is under an officer this tick.
// AuraSystem (TW.Sim.Combat) implements it; HeroSystem asks it whether a man has an officer near him, which is
// one of the things that makes a man desperate. An interface, like ISeaLift, so Core need not name Combat.
namespace TW.Sim
{
    public interface IAuraProvider
    {
        /// <summary>True when a living, unpinned officer of his own side covers the slot this tick.</summary>
        bool Covered(int slot);
    }
}
