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
        static readonly CardPlan[] cards = new CardPlan[8];

        static WaterEntry() => SceneStatics.Register(nameof(WaterEntry), Clear);

        public const float ManKg = 75f, RifleKg = 4.3f, HelmetKg = 1.2f;
        /// <summary>The ring a man's own mass opens, and the clamp either side of it.</summary>
        public const float ManRing = 2.8f, MinRing = 0.35f, MaxRing = 2.95f;
        /// <summary>The crown card's WIDTH (a rifle round's is 0.9 x scale, CombatFx.cs:657). Every water card is
        /// drawn from Book.Splash - the one water-white sheet, an upright column (FlipbookFx.cs:16) - because
        /// CombatFx.ApplyTints repaints Wings and GroundRing from BiomeProfile, so a card moved off Splash turns
        /// mud. Eight columns inside one footprint stack into ONE pale blob: at 3.0 m wide the whole crown read as a
        /// single 3 m disc at night (water-splash-night-read-g1's before run, 2990 px of crystal and no lobes). So
        /// the crown is now a column the height of a man rather than a bale: 1.1 m wide, CrownTall x that high, and
        /// the wings, droplets and skirt are pushed out far enough to own screen space of their own.</summary>
        public const float ManCard = 1.6f, CrownTall = 2.0f;
        /// <summary>The crown's two wings: how far out they sit (times the ring), how wide against the crown's width,
        /// and how high against their own width. WingOut clears the crown: WingOut x ManRing >= 0.5 x (crown + wing)
        /// needs only 0.31, and 0.75 puts them 2.1 m out, in the 1.5-3.0 m band the probe reads.</summary>
        public const float WingOut = 0.8f, WingSize = 1.75f, WingTall = 0.8f;
        /// <summary>The four droplets thrown up and out off the crown: how far out they sit (times the ring, on the
        /// diagonal, so the radius is sqrt(2) x that) and how big against the crown.</summary>
        public const float DropletOut = 0.65f, DropletSize = 1.0f;
        /// <summary>The foam skirt left standing on the water after the crown: how wide against the crown's width,
        /// how long it stands (long enough to still be there at the O120 shot) and how faint. It lies FLAT on the
        /// water, so it is a foam disc around his feet and not a ninth column in the blob.</summary>
        public const float SkirtSize = 3.75f, SkirtLife = 1.2f, SkirtAlpha = 0.5f;
        /// <summary>A crown or wing card's life, and the glow band every other night effect uses
        /// (CombatFx.Chunks.cs: SceneMood.Night ? 3.4f : 1.6f) — nothing lifts a faint blue streak out of the dark
        /// without it.</summary>
        public const float CardLife = 0.5f, NightGlow = 3.4f, DayGlow = 1.6f;

        /// <summary>The glow a water card draws at: the night band every other effect uses, or the day one.</summary>
        public static float Glow(bool night) => night ? NightGlow : DayGlow;

        /// <summary>How big a thing of this mass draws against a man: a cube root, so a heavier man is not a wider
        /// splash, he is a girthier one.</summary>
        public static float Scale(float massKg) => Mathf.Clamp(Mathf.Pow(massKg / ManKg, 1f / 3f), 0.22f, 1.25f);

        /// <summary>The ring a thing of this mass opens, clamped under the smallest shell ring (WaterRings.OnSimEvent
        /// clamps a shell to 3..8 m) so a shell still rings the widest.</summary>
        public static float Ring(float massKg) => Mathf.Clamp(ManRing * Scale(massKg), MinRing, MaxRing);

        /// <summary>The Splash card a thing of this mass throws.</summary>
        public static float Card(float massKg) => ManCard * Scale(massKg);

        /// <summary>One card of the crown: where it sits (x and z off the entry point), how wide it draws, how high
        /// (metres; 0 leaves the sheet's own aspect to decide), how long it lives, how bright, whether it is thrown
        /// up and out (a droplet, not anchored to the surface), whether it lies flat on the water (the foam skirt)
        /// and whether its sheet is flipped (so the two wings, and the two pairs of droplets, are not the same
        /// picture twice).</summary>
        public struct CardPlan { public float Dx, Dz, Size, Height, Life, Alpha; public bool Up, Flat, Mirror; }

        /// <summary>The crown a thing of this mass throws, as pure arithmetic a test can read: a crown over the
        /// entry point, a wing either side of it, four droplets thrown up and out of it, and a foam skirt left
        /// standing on the water. Returns how many cards were written (always 8).</summary>
        public static int Cards(float massKg, float scale, CardPlan[] into)
        {
            float crown = Card(massKg) * scale, ring = Ring(massKg);
            float wingReach = WingOut * ring, wing = WingSize * crown;
            float dropReach = DropletOut * ring, drop = DropletSize * crown;
            float skirt = SkirtSize * crown;
            into[0] = new CardPlan { Dx = 0f, Dz = 0f, Size = crown, Height = CrownTall * crown, Life = CardLife, Alpha = 1f, Up = false, Flat = false, Mirror = false };
            into[1] = new CardPlan { Dx = -wingReach, Dz = 0f, Size = wing, Height = WingTall * wing, Life = CardLife, Alpha = 1f, Up = false, Flat = false, Mirror = false };
            into[2] = new CardPlan { Dx = wingReach, Dz = 0f, Size = wing, Height = WingTall * wing, Life = CardLife, Alpha = 1f, Up = false, Flat = false, Mirror = true };
            into[3] = new CardPlan { Dx = -dropReach, Dz = -dropReach, Size = drop, Height = drop, Life = CardLife, Alpha = 1f, Up = true, Flat = false, Mirror = false };
            into[4] = new CardPlan { Dx = dropReach, Dz = -dropReach, Size = drop, Height = drop, Life = CardLife, Alpha = 1f, Up = true, Flat = false, Mirror = true };
            into[5] = new CardPlan { Dx = -dropReach, Dz = dropReach, Size = drop, Height = drop, Life = CardLife, Alpha = 1f, Up = true, Flat = false, Mirror = false };
            into[6] = new CardPlan { Dx = dropReach, Dz = dropReach, Size = drop, Height = drop, Life = CardLife, Alpha = 1f, Up = true, Flat = false, Mirror = true };
            into[7] = new CardPlan { Dx = 0f, Dz = 0f, Size = skirt, Height = skirt, Life = SkirtLife, Alpha = SkirtAlpha, Up = false, Flat = true, Mirror = false };
            return 8;
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
        public static Splash Throw(FlipbookFx books, Vector3 at, float massKg, float delay = 0f, float scale = 1f, bool mirror = false, bool night = false)
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
                float glow = Glow(night);
                for (int i = 0; i < n; i++)
                {
                    var c = cards[i];
                    bool flip = c.Mirror ^ mirror;
                    var kind = (c.Flat ? FlipbookFx.Kind.Flat : FlipbookFx.Kind.Upright)
                             | (flip ? FlipbookFx.Kind.Mirror : 0) | (c.Up ? 0 : FlipbookFx.Kind.Anchored);
                    Vector3 velocity = c.Up ? new Vector3(c.Dx, 1.4f, c.Dz).normalized * 1.8f : default;
                    books.Add(FlipbookFx.Book.Splash,
                        new Vector3(at.x + c.Dx, sp.Y, at.z + c.Dz), c.Size, c.Life, kind,
                        velocity: velocity, grow: c.Up ? 1.5f : 0f, alpha: c.Alpha, glow: glow, height: c.Height, delay: delay);
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
