// Phase: VFX pass (owner, 2026-09-28: "we need to go that extra mile and have everything customized, add the extra effort
// zoom and quality level") — part of CombatFx. ArmsFor (CombatFx.Weapons.cs) sizes what every shot already draws by the
// shooter's class, and at the standard view that reads as sizes only (cls1). Among the men there is room for each weapon's
// own drawing, and this is it, spent only as near the eye as the effects' tier allows (FxQuality: High and Epic, inside
// ExtraReach, zoomed in): the sniper's muzzle brake throws two side jets and a crack and kicks the dust at his feet; the
// machine gun's flare is a star, and at Epic its belt links spill; the submachine gun and the machine pistol flicker twice;
// the pistol and the carbine pop. Epic also gives every round a drawn puff of smoke over the ball. Also here: the close
// assault's bundle of grenades, THROWN (it was drawn as a bullet), and its burst where it lands. No draw is taken from the
// shared UnityEngine.Random stream (FxQuality.Hash), so turning any of it on or off moves no lightning; fx.classArms 0 draws
// none of it.
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>What a class carries, as drawn.</summary>
        public enum ArmsKind : byte { Rifle, Smg, Mg, Sniper, Pistol, Carbine, MachinePistol, HullMg }

        public static ArmsKind KindOf(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Assault: return ArmsKind.Smg;
                case InfantryArchetype.Machinegunner: return ArmsKind.Mg;
                case InfantryArchetype.Sniper: return ArmsKind.Sniper;
                case InfantryArchetype.Officer: return ArmsKind.Carbine;
                case InfantryArchetype.Shield: return ArmsKind.Pistol;
                case InfantryArchetype.Jetpack: return ArmsKind.MachinePistol;
                case VehicleArchetype.Maw: case VehicleArchetype.Tusk: case VehicleArchetype.Breaker: case VehicleArchetype.Skimmer: return ArmsKind.HullMg;
                default: return ArmsKind.Rifle;
            }
        }

        /// <summary>A man struck: the spike of light where the round went in, x by the weapon that fired it (a sniper's round
        /// flashes harder than a pistol's).</summary>
        public static float HitFlashOf(ArmsKind kind)
        {
            switch (kind)
            {
                case ArmsKind.Sniper: return 1.35f;
                case ArmsKind.HullMg: return 1.15f;
                case ArmsKind.Mg: return 1.05f;
                case ArmsKind.Smg: case ArmsKind.MachinePistol: return 0.85f;
                case ArmsKind.Pistol: return 0.75f;
                case ArmsKind.Carbine: return 0.9f;
                default: return 1f;
            }
        }

        /// <summary>The widest a close-up piece (a star, a brake's jet) is drawn, x the men's scale.</summary>
        public const float CloseMost = 1.6f;

        /// <summary>Whether a place is inside the effects' tier's close-up reach of the eye (read once a frame by ViewNow).</summary>
        bool CloseEnough(Vector3 p, float reach)
        {
            if (reach <= 0f || SceneHooks.CloseUp <= 0f || viewCam == null) return false;
            Vector3 eye = viewCam.transform.position; float dx = p.x - eye.x, dz = p.z - eye.z;
            return dx * dx + dz * dz < reach * reach;
        }

        /// <summary>A screen direction at `roll` (the flare's own convention: 0 = the camera's right).</summary>
        static Vector3 ScreenDir(Camera cam, float roll) => cam.transform.right * Mathf.Cos(roll) + cam.transform.up * Mathf.Sin(roll);

        /// <summary>The weapon's own drawing up close, on top of what every shot draws. `flare` is this round's flare width,
        /// `roll` the barrel on screen, `flared` whether this round drew its flare card.</summary>
        void ShotExtras(byte archetype, Camera cam, Vector3 from, Vector3 barrel, float roll, float flare, Vector3 carried, float delay, float scale,
                        uint round, bool flared, bool smoked, float smoke)
        {
            var q = FxQuality.Now;
            if (classArms <= 0f || cam == null || !CloseEnough(from, q.ExtraReach)) return;
            float glow = (SceneMood.Night ? 3.2f : 1.6f) * SceneTints.Now.Glow;
            float most = CloseMost * scale;   // no close piece wider than this: a sniper's star at 1.4 x his flare was 7 m, a shell hit (lin6)
            uint h = FxQuality.Hash(round * 2654435761u + archetype);
            float spin = (h & 0xFFFFu) / 65536f * 6.2832f;
            Vector3 along = ScreenDir(cam, roll);
            switch (KindOf(archetype))
            {
                case ArmsKind.Sniper:
                    // the muzzle brake: two short jets out either side of the barrel, a crack at the mouth, and the dust his
                    // shot kicks off the ground under the rifle
                    for (int s = -1; s <= 1; s += 2)
                    {
                        float side = roll + s * 1.5708f;
                        books.Add(FlipbookFx.Book.Muzzle, from + along * (flare * 0.12f) + ScreenDir(cam, side) * (flare * 0.3f), Mathf.Min(flare * 0.8f, most), 0.22f,
                            s > 0 ? FlipbookFx.Kind.None : FlipbookFx.Kind.Mirror, velocity: carried, roll: side, glow: glow, delay: delay);
                    }
                    books.Add(FlipbookFx.Book.Star, from + along * (flare * 0.15f), Mathf.Min(flare * 0.55f, most), 0.13f, roll: spin, glow: glow * 0.9f, delay: delay);
                    if (Host != null && Host.Local != null)
                    {
                        Vector3 foot = new Vector3(from.x, RenderGround.Sample(Host.Local.Map, from.x, from.z), from.z);
                        Vector3 kick = new Vector3(barrel.x, 0f, barrel.z) * 0.8f + Vector3.up * 0.25f;
                        books.Add(FlipbookFx.Book.DustPuff, foot + barrel * (0.4f * scale), 1.6f * scale, 1.4f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | ((h & 1u) != 0 ? FlipbookFx.Kind.Mirror : 0),
                            velocity: kick, grow: 0.4f, alpha: 0.45f, delay: delay);
                    }
                    break;
                case ArmsKind.Mg:
                case ArmsKind.HullMg:
                    // the burning muzzle of a gun at work is a star as much as a flare; at Epic the belt's links spill
                    if (flared) books.Add(FlipbookFx.Book.Star, from + along * (flare * 0.18f), Mathf.Min(flare * 1.0f, most), 0.13f, roll: spin, glow: glow, delay: delay);
                    if (q.Epic && KindOf(archetype) == ArmsKind.Mg && (round & 1u) == 0u && chunks.Count < q.Cap(600))
                    {
                        Vector3 left = Vector3.Cross(barrel, Vector3.up).normalized;
                        float a = FxQuality.Hash01(h + 1u), b = FxQuality.Hash01(h + 2u);
                        chunks.Add(new Chunk { Pos = from - barrel * (0.5f * scale) + left * (0.06f * scale), Vel = left * Mathf.Lerp(0.6f, 1.2f, a) + Vector3.up * Mathf.Lerp(0.8f, 1.4f, b),
                            Born = Time.time, Life = 3f, Size = 1f, Kind = 5 });
                    }
                    break;
                case ArmsKind.Smg:
                case ArmsKind.MachinePistol:
                    // a burst, not a shot: the SMG's flare flickers a second time a round later, the machine pistol's twice more
                    if (flared)
                    {
                        bool smg = KindOf(archetype) == ArmsKind.Smg;
                        for (int n = 1; n <= (smg ? 1 : 2); n++)
                            books.Add(FlipbookFx.Book.Muzzle, from + along * (flare * 0.4f), flare * (smg ? 0.8f : 0.9f), 0.12f,
                                ((h >> n) & 1u) != 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None, velocity: carried, roll: roll + (((h >> n) & 1u) != 0 ? Mathf.PI : 0f), glow: glow, delay: delay + n * (smg ? 0.08f : 0.05f));
                    }
                    break;
                case ArmsKind.Pistol:
                case ArmsKind.Carbine:
                    // a short barrel: more pop than flame
                    books.Add(FlipbookFx.Book.Star, from + along * (flare * 0.1f), Mathf.Min(flare * (KindOf(archetype) == ArmsKind.Pistol ? 0.9f : 1.0f), most), 0.13f, roll: spin, glow: glow * 1.2f, delay: delay);
                    break;
            }
            // Epic: the round's smoke as a drawn puff over the ball, which reads as a sphere up close
            if (q.Epic && smoked)
                books.Add(FlipbookFx.Book.Smoke, from + barrel * (0.35f * scale), 0.7f * scale * smoke, 1.2f, (h & 4u) != 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: barrel * 1.1f + Vector3.up * 0.3f + carried, grow: 1.3f, roll: spin, alpha: 0.35f, pop: 0.2f, delay: delay + 0.03f);
        }

        /// <summary>A sniper's round up close: the exit spray behind the man it went through.</summary>
        void HitExtras(ArmsKind kind, Vector3 p, Vector3 toward, float scale, uint salt)
        {
            if (kind != ArmsKind.Sniper || classArms <= 0f || !CloseEnough(p, FxQuality.Now.ExtraReach)) return;
            books.Add(FlipbookFx.Book.DustPuff, p + toward * (0.45f * scale), 1.2f * scale, 0.9f, (FxQuality.Hash(salt) & 1u) != 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                velocity: toward * 2.2f + Vector3.down * 0.3f, grow: 0.6f, alpha: 0.5f);
        }

        // ---- the close assault's bundle of grenades (DirectFire: Shot with scalar 1 at a vehicle) ----

        /// <summary>How long the bundle is in the air over a throw of `distance` m.</summary>
        public static float BundleFlight(float distance) => Mathf.Clamp(distance / 14f, 0.35f, 0.7f);

        /// <summary>The throw that carries a bundle from `hand` onto `to` in `seconds` under gravity (9.8, as the chunks fall).</summary>
        public static Vector3 BundleVelocity(Vector3 hand, Vector3 to, float seconds) => (to - hand) / seconds + Vector3.up * (0.5f * 9.8f * seconds);

        /// <summary>A bundle of grenades thrown over from his hand onto the hull, and where it lands, a sharp burst: a flash, a
        /// small boiling cloud, dust, and smoke that hangs. The sim's damage has already happened; the burst is drawn as it
        /// lands (under 0.7 s later).</summary>
        void ThrowBundle(Vector3 from, Vector3 to, float delay, float scale, uint salt)
        {
            Vector3 hand = from + Vector3.up * (0.35f * scale);
            float t = BundleFlight(Vector3.Distance(hand, to));
            if (chunks.Count < FxQuality.Now.Cap(MaxChunks))
                chunks.Add(new Chunk { Pos = hand, Vel = BundleVelocity(hand, to, t), Born = Time.time, Life = t, Size = 1f, Kind = 8 });
            if (books == null || !books.Ready) return;
            float land = delay + t, glow = SceneTints.Now.Glow;
            bool mirror = (FxQuality.Hash(salt) & 1u) != 0;
            Vector4 wind = Shader.GetGlobalVector(WindGlobalId); Vector3 drift = new Vector3(wind.x, 0f, wind.y) * 3.5f + Vector3.up * 0.55f;
            books.Add(FlipbookFx.Book.Flash, to + Vector3.up * 0.3f, 2.8f, 0.12f, roll: FxQuality.Hash01(salt + 1u) * 6.2832f, glow: (SceneMood.Night ? 4.5f : 2f) * glow, pop: 0.5f, delay: land);
            books.Add(FlipbookFx.Book.Burst, to + Vector3.up * 0.6f, 2.2f, 1.1f, FlipbookFx.Kind.Upright | (mirror ? FlipbookFx.Kind.Mirror : 0),
                velocity: Vector3.up * 1.2f + drift, grow: 0.5f, glow: (SceneMood.Night ? 2.6f : 1.3f) * glow * burstGlow, pop: 0.3f, delay: land);
            books.Add(FlipbookFx.Book.Puff, to + Vector3.up * 0.2f, 2.4f, 0.8f, mirror ? FlipbookFx.Kind.None : FlipbookFx.Kind.Mirror,
                velocity: Vector3.up * 1.4f, grow: 1f, alpha: 0.8f, pop: 0.4f, delay: land);
            books.Add(FlipbookFx.Book.Smoke, to + Vector3.up * 0.8f, 2.2f, 3.5f, mirror ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                velocity: drift * 1.4f + Vector3.up * 0.4f, grow: 1.6f, alpha: FlipbookFx.SmokeOpacity(0.55f, smokeAlpha, SceneHooks.CloseUp), pop: 0.3f, delay: land + 0.1f);
        }
    }
}
