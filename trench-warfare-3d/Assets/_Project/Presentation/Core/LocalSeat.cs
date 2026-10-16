// Phase: B1 (implemented 2026-10-16) — one place answers "which team is mine".
// The one presentation-side answer to "which team is mine". It exists because three sites each kept their own 0: the
// camera scanned 'w.Team[i] == 0' into your side, QuietFog had a LocalTeam field, CombatFx.Mines a LocalPlayer
// constant. Seated as player 1 that put you on the enemy side of the field with the standard view's turn reversed.
// The seat comes off the transport (ILockstepTransport.LocalPlayer), read once by SimHost in Awake, never guessed.
namespace TW.Presentation
{
    public static class LocalSeat
    {
        /// <summary>The player this client is seated as. 0 until SimHost reads the transport.</summary>
        public static int Player { get; private set; }
        /// <summary>The sim team of that seat (player n plays team n).</summary>
        public static byte Team => (byte)Player;

        /// <summary>Take the seat from the transport that drives the local world. A null transport means seat 0.</summary>
        public static void SetFrom(TW.Net.ILockstepTransport t) => Player = t == null ? 0 : t.LocalPlayer;

        /// <summary>Set the seat by hand: tests and tooling that run without a transport.</summary>
        public static void Set(int player) => Player = player;

        /// <summary>Back to seat 0, so one test cannot leak its seat into the next.</summary>
        public static void Reset() => Set(0);

        // the seat is a mutable static: it goes back to 0 when Play ends, as the next session reads it from its own transport
        static LocalSeat() => SceneStatics.Register(nameof(LocalSeat), Reset);
    }
}
