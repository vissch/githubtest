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
        static readonly CardPlan[] cards = new CardPlan[3];

        static WaterEntry() => SceneStatics.Register(nameof(WaterEntry), Clear);

        public const float ManKg = 75f, RifleKg = 4.3f, HelmetKg = 1.2f;
        /// <summary>The ring a man's own mass opens, and the clamp either side of it.</summary>
        public const float ManRing = 2.8f, MinRing = 0.35f, MaxRing = 2.95f;
        /// <summary>The Splash card a man's own mass throws (a rifle round's is 0.9 x scale, CombatFx.cs:657). Raised
        /// from 1.1 after harsh critic r1: at T1 a 1.1 m card measured 326 px against a bar of 1000.</summary>
        public const float ManCard = 2.4f;
        /// <summary>The crown's two wings: how far out they sit (times the ring) and how big against the crown.</summary>
        public const float WingOut = 0.45f, WingSize = 0.55f;
        /// <summary>How long the crown and its wings take to collapse. 0.7 s, not 0.5: at 0.5 s the capture measured
        /// 597 changed pixels a third of a second after the entry, where the owner's bar for "it still reads" is 600,
        /// and a column of water that is gone in half a second looks snatched away rather than fallen back.</summary>
        public const float CardLife = 0.7f;

        /// <summary>How big a thing of this mass draws against a man: a cube root, so a heavier man is not a wider
        /// splash, he is a girthier one.</summary>
        public static float Scale(float massKg) => Mathf.Clamp(Mathf.Pow(massKg / ManKg, 1f / 3f), 0.22f, 1.25f);

        /// <summary>The ring a thing of this mass opens, clamped under the smallest shell ring (WaterRings.OnSimEvent
        /// clamps a shell to 3..8 m) so a shell still rings the widest.</summary>
        public static float Ring(float massKg) => Mathf.Clamp(ManRing * Scale(massKg), MinRing, MaxRing);

        /// <summary>The Splash card a thing of this mass throws.</summary>
        public static float Card(float massKg) => ManCard * Scale(massKg);

        /// <summary>One card of the crown: how far out along x it sits, how big it draws, and whether its sheet is
        /// flipped (so the two wings are not the same picture twice).</summary>
        public struct CardPlan { public float Dx, Size; public bool Mirror; }

        /// <summary>The crown a thing of this mass throws, as pure arithmetic a test can read: a crown over the
        /// entry point and a wing either side of it. Returns how many cards were written (always 3).</summary>
        public static int Cards(float massKg, float scale, CardPlan[] into)
        {
            float crown = Card(massKg) * scale, reach = WingOut * Ring(massKg), wing = WingSize * crown;
            into[0] = new CardPlan { Dx = 0f, Size = crown, Mirror = false };
            into[1] = new CardPlan { Dx = -reach, Size = wing, Mirror = false };
            into[2] = new CardPlan { Dx = reach, Size = wing, Mirror = true };
            return 3;
        }

        public struct Splash
        {
            public float Ring, Card;
            /// <summary>The height the Splash card's foot sits at: the water's surface, not the bed under it.</summary>
            public float Y;
            public bool Any;
        }

        /// <summary>The height of the water's surface at this point. No hook wired up (a test, or a map with no
        /// water sheet) leaves the point's own y, so nothing can regress.</summary>
        public static float SurfaceY(Vector3 at) =>
            SceneHooks.WaterY == null ? at.y : Mathf.Max(at.y, SceneHooks.WaterY(at.x, at.z));

        /// <summary>A man, a flung body, or a dropped item enters the water at `at`: one ring sized by mass and a
        /// Splash card, nothing on dry ground. `books` null (no scene, a test) still rings.</summary>
        public static Splash Throw(FlipbookFx books, Vector3 at, float massKg, float delay = 0f, float scale = 1f, bool mirror = false)
        {
            if (SceneHooks.IsWater == null || !SceneHooks.IsWater(at.x, at.z)) return default;
            var sp = new Splash { Ring = Ring(massKg), Card = Card(massKg), Y = SurfaceY(at), Any = true };
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
            {
                int n = Cards(massKg, scale, cards);
                for (int i = 0; i < n; i++)
                {
                    var c = cards[i];
                    bool flip = c.Mirror ^ mirror;
                    books.Add(i == 0 ? FlipbookFx.Book.Splash : FlipbookFx.Book.Wings,
                        new Vector3(at.x + c.Dx, sp.Y, at.z), c.Size, CardLife,
                        FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (flip ? FlipbookFx.Kind.Mirror : 0),
                        alpha: 0.9f, delay: delay);
                }
            }
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
