// Phase: VFX pass (owner, 2026-09-28: "each class unit has a different vfx specific to that class ... think of mortars", then
// "have everything customized") — part of CombatFx. Every Explosion drew the one shell burst, whatever made it: the Tusk's
// 37 mm and a long gun's shell, the Kettle's mortar round and the Salvo's rocket, a buried mine and a tripwire's charge. The
// sim says who in Explosion.a (SourceId: a unit's shell is UnitBase + its archetype; a mine is MineSystem.SourceBase + its
// kind), and BurstBy turns that into the burst's own shape: its size, how much fire, whether it stands a column, and the
// pieces only it has. It is on top of the recipe (fx.recipes) and under fx.classArms (0: every burst as before). The new
// pieces take no draw from the shared UnityEngine.Random stream (FxQuality.Hash).
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>A burst by what made it. Size scales the burst's radius as drawn; Fire the fire in it (0 none); Column the
        /// earth column's width (0 none). Mortar: a lobbed round's wide low burst over it; Rocket: a hard white crack and more
        /// fire; Mine: the earth thrown straight up out of the ground, dust at its foot and black smoke; Tripwire: a charge at
        /// shin height, thrown out sideways, sparks and dust and no column.</summary>
        public struct BurstLook
        {
            public float Size, Fire, Column, Wings, Flash;
            public bool Mortar, Rocket, Mine, Tripwire;
            public static readonly BurstLook Same = new BurstLook { Size = 1f, Fire = 1f, Column = 1f, Wings = 1f, Flash = 1f };
        }

        public static BurstLook BurstBy(int source)
        {
            var look = BurstLook.Same;
            if (source == MineSystem.SourceBase + (int)MineKind.Mine) { look.Mine = true; look.Fire = 0f; look.Column = 0.8f; look.Wings = 0.8f; look.Flash = 0.25f; return look; }   // a buried charge: a dull thump, not a white-out
            if (source == MineSystem.SourceBase + (int)MineKind.Tripwire) { look.Tripwire = true; look.Fire = 0.5f; look.Column = 0f; look.Wings = 1.8f; look.Size = 0.85f; look.Flash = 0.2f; return look; }
            if (!SourceId.IsUnit(source)) return look;   // an ability's shells, a cook-off, the fleet: the shell burst as it is
            switch (SourceId.ArchetypeOf(source))
            {
                case VehicleArchetype.Kettle: look.Mortar = true; look.Size = 0.9f; look.Fire = 0.7f; look.Column = 0.85f; look.Wings = 1.25f; break;   // a mortar: low and wide
                case VehicleArchetype.Salvo: look.Rocket = true; look.Fire = 1.35f; look.Column = 0.8f; break;                                        // a rocket: fire and a crack
                case VehicleArchetype.Tusk: look.Size = 0.75f; look.Fire = 0.8f; break;                                                               // a 37 mm's small shell
                case VehicleArchetype.Pavise: case VehicleArchetype.Banner: look.Size = 1.15f; look.Column = 1.1f; break;                            // the long guns
            }
            return look;
        }

        /// <summary>The pieces only this burst has, drawn after the shell burst's own. p: the burst on the ground; r: its radius
        /// as drawn (after look.Size); salt: the tick, for variety.</summary>
        void BurstExtras(in BurstLook look, Vector3 p, float r, bool mirror, Vector3 drift, uint salt)
        {
            if (books == null || !books.Ready) return;
            var ground = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
            float glow = SceneTints.Now.Glow;
            if (look.Rocket)   // the rocket's own crack: a star and a second hard flash, with no lean
            {
                books.Add(FlipbookFx.Book.Star, p + Vector3.up * (r * 0.4f), r * 2.2f, 0.08f, roll: FxQuality.Hash01(salt) * 6.2832f, glow: (SceneMood.Night ? 5f : 2.2f) * glow);
                books.Add(FlipbookFx.Book.Flash, p + Vector3.up * (r * 0.5f), r * 1.8f, 0.1f, roll: FxQuality.Hash01(salt + 1u) * 6.2832f, glow: (SceneMood.Night ? 4f : 1.8f) * glow, pop: 0.5f, delay: 0.06f);
            }
            if (look.Mine || look.Tripwire)
            {
                // the dust at the foot, thrown out either side along the ground
                for (int k = 0; k < 2; k++)
                    books.Add(FlipbookFx.Book.DustPuff, p + Vector3.up * 0.2f, r * (look.Tripwire ? 1.5f : 1.2f), 2f, ground | (k == 1 ? FlipbookFx.Kind.Mirror : 0),
                        velocity: new Vector3(k == 0 ? 1.6f : -1.6f, 0.3f, 0f), alpha: 0.75f, delay: 0.05f);
            }
            if (look.Mine)   // a buried charge: the black smoke of the explosive (the wreck's black book), standing over the thrown earth
                for (int k = 0; k < 2; k++)
                    books.Add(FlipbookFx.Book.WreckSmoke, p + Vector3.up * (r * (0.3f + k * 0.4f)), r * (2.2f + k * 0.3f), 4.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | ((mirror ^ k == 1) ? FlipbookFx.Kind.Mirror : 0),
                        velocity: drift * 1.3f + Vector3.up * 0.4f, grow: 0.8f, alpha: 0.95f * (1f - 0.5f * SceneHooks.CloseUp), pop: 0.2f, delay: 0.05f + k * 0.2f);
            if (look.Tripwire)
            {
                Sparks(p + Vector3.up * 0.4f, 60, 14f, 0.18f, salt, low: true);   // the charge's casing, flung out low and sideways, day and night
                books.Add(FlipbookFx.Book.GroundRing, p + Vector3.up * 0.15f, r * 3f, 0.9f, FlipbookFx.Kind.Flat | (mirror ? FlipbookFx.Kind.Mirror : 0), grow: 1f, alpha: 0.85f * (1f - SceneHooks.CloseUp * 0.5f));
            }
        }

        /// <summary>Throw's sparks (kind 3) on FxQuality.Hash in place of the shared stream: out and up, each its own speed and life.</summary>
        void Sparks(Vector3 at, int count, float speed, float size, uint salt, bool low = false)
        {
            int most = FxQuality.Now.Cap(MaxChunks);
            for (int k = 0; k < count && chunks.Count < most; k++)
            {
                uint h = FxQuality.Hash(salt * 97u + (uint)k);
                float a = FxQuality.Hash01(h) * 6.2832f, up = low ? Mathf.Lerp(0.05f, 0.3f, FxQuality.Hash01(h + 1u)) : Mathf.Lerp(0.3f, 1.2f, FxQuality.Hash01(h + 1u)), v = speed * Mathf.Lerp(0.5f, 1.2f, FxQuality.Hash01(h + 2u));
                Vector3 dir = new Vector3(Mathf.Cos(a), up, Mathf.Sin(a)).normalized;
                chunks.Add(new Chunk { Pos = at, Vel = dir * v, Born = Time.time, Life = Mathf.Lerp(0.45f, 1.1f, FxQuality.Hash01(h + 3u)), Size = size * Mathf.Lerp(0.6f, 1.5f, FxQuality.Hash01(h + 4u)), Kind = 3 });
            }
        }
    }
}
