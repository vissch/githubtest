// Phase: water-entry-splash (2026-10-19, harsh critic: "a man or body dying in water, a flung body landing in
// water, or a dropped item makes no splash; only shells ring the water"). The one place a falling thing's splash
// lives: a ring sized by mass (the cube root, so it grows with girth, not weight) plus a Splash card, and nothing
// at all on dry ground. Modelled on GoreScale.cs: named constants and pure arithmetic, the two hooks it calls
// (SceneHooks.IsWater, SceneHooks.AddRing) both in RenderGround.cs.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class WaterEntry
    {
        struct PendingRing { public float X, Z, Size, Left; }
        const int Pending = 32;
        static readonly PendingRing[] pending = new PendingRing[Pending];
        static int pendingCount;

        static WaterEntry() => SceneStatics.Register(nameof(WaterEntry), Clear);

        public const float ManKg = 75f, RifleKg = 4.3f, HelmetKg = 1.2f;
        /// <summary>The ring a man's own mass opens, and the clamp either side of it.</summary>
        public const float ManRing = 1.8f, MinRing = 0.35f, MaxRing = 2.6f;
        /// <summary>The Splash card a man's own mass throws (a rifle round's is 0.9 x scale, CombatFx.cs:657).</summary>
        public const float ManCard = 1.1f;

        /// <summary>How big a thing of this mass draws against a man: a cube root, so a heavier man is not a wider
        /// splash, he is a girthier one.</summary>
        public static float Scale(float massKg) => Mathf.Clamp(Mathf.Pow(massKg / ManKg, 1f / 3f), 0.22f, 1.25f);

        /// <summary>The ring a thing of this mass opens, clamped under the smallest shell ring (WaterRings.OnSimEvent
        /// clamps a shell to 3..8 m) so a shell still rings the widest.</summary>
        public static float Ring(float massKg) => Mathf.Clamp(ManRing * Scale(massKg), MinRing, MaxRing);

        /// <summary>The Splash card a thing of this mass throws.</summary>
        public static float Card(float massKg) => ManCard * Scale(massKg);

        public struct Splash
        {
            public float Ring, Card;
            public bool Any;
        }

        /// <summary>A man, a flung body, or a dropped item enters the water at `at`: one ring sized by mass and a
        /// Splash card, nothing on dry ground. `books` null (no scene, a test) still rings.</summary>
        public static Splash Throw(FlipbookFx books, Vector3 at, float massKg, float delay = 0f, float scale = 1f, bool mirror = false)
        {
            if (SceneHooks.IsWater == null || !SceneHooks.IsWater(at.x, at.z)) return default;
            var sp = new Splash { Ring = Ring(massKg), Card = Card(massKg), Any = true };
            if (delay <= 0f)
            {
                SceneHooks.AddRing?.Invoke(at.x, at.z, sp.Ring);
            }
            else
            {
                int slot = pendingCount < Pending ? pendingCount++ : 0;
                if (slot == 0 && pendingCount >= Pending)
                    SceneHooks.AddRing?.Invoke(pending[0].X, pending[0].Z, pending[0].Size);
                pending[slot] = new PendingRing { X = at.x, Z = at.z, Size = sp.Ring, Left = delay };
            }
            if (books != null && books.Ready)
                books.Add(FlipbookFx.Book.Splash, at, sp.Card * scale, 0.5f,
                    FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0), alpha: 0.9f, delay: delay);
            return sp;
        }

        /// <summary>Called once a frame (CombatFx.Update): counts down every pending ring and opens each that
        /// reached its landing, so a flung body's ring opens with its Splash card, not a whole flight before it.</summary>
        public static void Pump(float dt)
        {
            if (pendingCount == 0) return;
            for (int i = pendingCount - 1; i >= 0; i--)
            {
                pending[i].Left -= dt;
                if (pending[i].Left > 0f) continue;
                SceneHooks.AddRing?.Invoke(pending[i].X, pending[i].Z, pending[i].Size);
                pending[i] = pending[--pendingCount];
            }
        }

        /// <summary>No scene is live: drop every pending ring (SceneStatics.ResetSession).</summary>
        public static void Clear() => pendingCount = 0;
    }
}
