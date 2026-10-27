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
        static readonly CardPlan[] cards = new CardPlan[4];

        static WaterEntry() => SceneStatics.Register(nameof(WaterEntry), Clear);

        public const float ManKg = 75f, RifleKg = 4.3f, HelmetKg = 1.2f;
        /// <summary>The ring a man's own mass opens, and the clamp either side of it.</summary>
        public const float ManRing = 2.8f, MinRing = 0.35f, MaxRing = 2.95f;
        /// <summary>The Splash card a man's own mass throws (a rifle round's is 0.9 x scale, CombatFx.cs:657). Cut
        /// from 2.4 for r7-g1: the owner read r7's stills as "a brighter copy of the old wisp", and the mask was
        /// measured at 3.4 m wide by 5.9 m tall (aspect 0.58) with a near-constant 35 px width at every row - a
        /// column, the same shape as the dim one before it. A crown is WIDER than it is tall, so the card that
        /// stands up is lowered and its spray thrown out (WingOut).</summary>
        public const float ManCard = 1.6f;
        /// <summary>The crown's two wings: how far out they sit (times the ring) and how big against the crown. Out
        /// from 0.45/0.55 for r7-g1: at 0.45 the wings sat 1.26 m out and the whole mask only reached 1.7 m, so they
        /// added almost nothing to the width. 0.80 x ManRing 2.8 puts them 2.24 m out, a 5.6 m span.</summary>
        public const float WingOut = 0.80f, WingSize = 0.70f;
        /// <summary>How long the crown and its wings take to collapse. Cut from 0.7 for r7-g1: at +0.3 s only 41 %
        /// of r7's mask lay within 0.5 m of the water line and its top was still 5.77 m up - nothing had fallen
        /// back. What stands at the foam beat is the Foam card, not the crown.
        /// 0.5 was captured too, to see whether a crown still fading at the foam beat could carry r7's "it still
        /// reads a third of a second later" guard: it cannot. At 0.5 the remnant was still 3.69 m up against the
        /// 2.5 m ceiling the fallen-foam beat sets, and it only reached 0.068 of the 0.09 contrast anyway. The two
        /// cannot both be had from a standing card, so the crown goes out on time and the foam carries the
        /// moment (probe.py measures that contrast in the foam's own band - see its docstring).</summary>
        public const float CardLife = 0.35f;
        /// <summary>The fallen foam: a flat card lying ON the water, this much wider than the ring, and this long.
        /// It is the owner's third beat ("body then crown then fallen foam"), and being flat it reads as width at
        /// every zoom, which is what the crown could not do.</summary>
        public const float FoamAcross = 1.8f, FoamLife = 1.1f;
        /// <summary>Taken off the delay of the ring and the cards: a 2 m man's feet break the water about this long
        /// before his middle reaches it. It is what makes the body beat (land - 0.05 s) show water already breaking
        /// where he goes in - r7's body moment measured exactly ZERO changed pixels, because ring and cards were
        /// both still queued for the landing instant.</summary>
        public const float Lead = 0.06f;

        /// <summary>How much of the way to white the crown's tint is pushed, and how far past 1 the whole tint goes.
        /// Harsh critic r1: the crown was blue-white on blue water, the same hue as the foam flecks, and measured a
        /// 0.03 value step over water whose own p90 in the crown box is 0.313. The biome's splash hue is kept (a
        /// desert ford is not the same water as a flooded trench) and its VALUE is lifted, which is what a player
        /// reads at night.</summary>
        public const float CrownWhite = 0.6f, CrownGain = 1.6f;

        /// <summary>The same for the fallen foam, held under the crown's: the crown must still stand above its own
        /// foam at night (r7's win, crown_contrast_post), so the foam takes a smaller step than the crown does.
        /// Measured, r7-g1: on the flat card this tint does not reach the screen at all (see the Foam row in
        /// FlipbookFx.Sheets) - it is kept so the hue is right wherever the flat path starts reading it.</summary>
        public const float FoamWhite = 0.45f, FoamGain = 1.3f;

        /// <summary>The crown's tint for a biome whose splash is `splash`: its own hue, its value lifted.</summary>
        public static Color CrownTint(Color splash)
        {
            var c = Color.Lerp(splash, Color.white, CrownWhite) * CrownGain;
            return new Color(c.r, c.g, c.b, 1f);
        }

        /// <summary>The fallen foam's tint for a biome whose splash is `splash`: the same lift as the crown's, less
        /// of it, so the crown still reads as the brighter of the two.</summary>
        public static Color FoamTint(Color splash)
        {
            var c = Color.Lerp(splash, Color.white, FoamWhite) * FoamGain;
            return new Color(c.r, c.g, c.b, 1f);
        }

        /// <summary>How big a thing of this mass draws against a man: a cube root, so a heavier man is not a wider
        /// splash, he is a girthier one.</summary>
        public static float Scale(float massKg) => Mathf.Clamp(Mathf.Pow(massKg / ManKg, 1f / 3f), 0.22f, 1.25f);

        /// <summary>The ring a thing of this mass opens, clamped under the smallest shell ring (WaterRings.OnSimEvent
        /// clamps a shell to 3..8 m) so a shell still rings the widest.</summary>
        public static float Ring(float massKg) => Mathf.Clamp(ManRing * Scale(massKg), MinRing, MaxRing);

        /// <summary>The Splash card a thing of this mass throws.</summary>
        public static float Card(float massKg) => ManCard * Scale(massKg);

        /// <summary>The fallen foam's card: this wide, lying on the water.</summary>
        public static float Foam(float massKg) => FoamAcross * Ring(massKg);

        /// <summary>One card of the splash: how far out along x it sits, how big it draws, whether its sheet is
        /// flipped (so the two wings are not the same picture twice), and whether it lies flat on the water.</summary>
        public struct CardPlan { public float Dx, Size; public bool Mirror, Flat; }

        /// <summary>The splash a thing of this mass throws, as pure arithmetic a test can read: a crown over the
        /// entry point, a wing either side of it, and the fallen foam lying flat on the water under all three.
        /// Returns how many cards were written (always 4).</summary>
        public static int Cards(float massKg, float scale, CardPlan[] into)
        {
            float crown = Card(massKg) * scale, reach = WingOut * Ring(massKg), wing = WingSize * crown;
            into[0] = new CardPlan { Dx = 0f, Size = crown, Mirror = false };
            into[1] = new CardPlan { Dx = -reach, Size = wing, Mirror = false };
            into[2] = new CardPlan { Dx = reach, Size = wing, Mirror = true };
            into[3] = new CardPlan { Dx = 0f, Size = Foam(massKg), Mirror = false, Flat = true };
            return 4;
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
            // his feet break the water before his middle reaches it: everything fires Lead earlier than the landing
            delay = Mathf.Max(0f, delay - Lead);
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
                    // the wings draw on the Crown book too, not the Wings book: measured, the Wings row's tan tint
                    // and its 0.15/0.42 window make them invisible at night, which is the other half of why the
                    // entry read as one column.
                    books.Add(c.Flat ? FlipbookFx.Book.Foam : FlipbookFx.Book.Crown,
                        new Vector3(at.x + c.Dx, sp.Y, at.z), c.Size, c.Flat ? FoamLife : CardLife,
                        (c.Flat ? FlipbookFx.Kind.Flat : FlipbookFx.Kind.Upright) | FlipbookFx.Kind.Anchored
                            | (flip ? FlipbookFx.Kind.Mirror : 0),
                        alpha: 1f, delay: delay);
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
