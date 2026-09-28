// Phase: VFX pass (owner, 2026-09-28: a reference still of the snow field with "explosions with more fidelity": a shell
// burst that is FIRE inside its smoke, not a flash in a grey cloud) — part of CombatFx. Bench snow0 (WinterLine, by day)
// drew no fire in a shell burst at all: the Flash card is additive, so on white snow it adds nothing, and what was left
// was a cold grey cloud. So every dry shell burst now also draws
//  - a FIREBALL: the pack's ground bloom (Book.Bloom) standing on the hole, its throw frames only (ShellFireLife: the
//    frames after them are the bloom fallen back and spread, a stain and not a burst), and
//  - two FIRE POCKETS: the dense bolus (Book.Head) born a beat later up inside the rising cloud and riding up with it, so
//    the smoke has fire showing through it the way the reference's does.
// Fire cards are premultiplied banded ink (Flipbook_URP), so unlike the flash they read on snow. fx.shellFire scales
// them (0: none, the burst as it was). No random draws: the pockets' places are a hash of the spot.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const string ShellFireKnob = "fx.shellFire";
        public const float ShellFireLife = 1.0f;     // s: the bloom's throw frames (12 of its 21 at 12 fps), fading over the last third
        public const float PocketLife = 0.85f;       // s
        public const int ShellPockets = 2;
        float shellFire = 1f;                         // knob fx.shellFire (Awake)

        public static float ReadShellFire() => Mathf.Clamp(Knobs.Get(ShellFireKnob, 1f), 0f, 2f);

        /// <summary>The fireball's width for a burst of radius r (2 to 9 m): about two thirds of the cloud (Burst is 2.6 r),
        /// and smaller up close, where the cloud is too (the flash comes down the same way).</summary>
        public static float ShellFireWidth(float r, float closeUp) => r * 1.7f * Mathf.Lerp(1f, 0.7f, Mathf.Clamp01(closeUp));

        /// <summary>Where fire pocket k sits over the hole, from a hash of the spot: up in the lower half of the cloud
        /// (0.7 to 1.3 r) and off to one side, the two on opposite sides, never further out than half the cloud's width.</summary>
        public static Vector3 PocketOffset(Vector3 p, int k, float r)
        {
            uint h = (uint)Mathf.FloorToInt(p.x * 13f) * 73856093u ^ (uint)Mathf.FloorToInt(p.z * 13f) * 19349663u ^ (uint)(k + 1) * 83492791u;
            h ^= h >> 13; h *= 0x5bd1e995u; h ^= h >> 15;
            float a = (h & 0x3FF) / 1024f * 6.2832f, side = 0.25f + ((h >> 10) & 0xFF) / 255f * 0.25f, up = 0.7f + ((h >> 18) & 0xFF) / 255f * 0.3f + k * 0.3f;
            if ((k & 1) == 1) a += 3.1416f;
            return new Vector3(Mathf.Cos(a) * side * r, up * r, Mathf.Sin(a) * side * r);
        }

        /// <summary>The shell's fire: the bloom on the hole and the pockets in the cloud. `rise` is the cloud's own velocity,
        /// so the pockets go up with it and stay inside it.</summary>
        void ShellFire(Vector3 p, float r, float closeUp, bool mirror, Vector3 rise)
        {
            if (shellFire <= 0f) return;
            float glow = (SceneMood.Night ? 3.4f : 2.0f) * SceneTints.Now.Glow;   // the flamethrower's: the same fire
            float w = ShellFireWidth(r, closeUp) * Mathf.Min(1f, shellFire);
            FlipbookFx.Geometry(FlipbookFx.Book.Bloom, out float inkLow, out _, out _);
            float h = books.CardHeight(FlipbookFx.Book.Bloom, w);
            // its ink's foot on the ground, a little sunk so no daylight shows under a fire standing in its own hole
            books.Add(FlipbookFx.Book.Bloom, p - Vector3.up * (inkLow * h + 0.1f * r), w, ShellFireLife, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0),
                velocity: rise * 0.35f, grow: 0.3f, alpha: Mathf.Min(1f, shellFire), glow: glow, pop: 0.25f);
            for (int k = 0; k < ShellPockets; k++)
                books.Add(FlipbookFx.Book.Head, p + PocketOffset(p, k, r), w * 0.45f, PocketLife, (mirror ^ k == 1) ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: rise, grow: 0.4f, alpha: Mathf.Min(1f, shellFire) * 0.9f, glow: glow, pop: 0.3f, delay: 0.08f + 0.12f * k, startFrame: 2f + k * 3f);
        }
    }
}
