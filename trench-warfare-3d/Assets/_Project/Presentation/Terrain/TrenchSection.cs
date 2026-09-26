// Phase: B5 (docs/21 phase 3) — a section of the trench lining (one revetment panel, one parapet course, one length of
// duckboards: each is one instanced 2 m piece) goes intact -> damaged -> gone. Damaged at half its strength (the
// piece is swapped for its broken twin at the same matrix: BattlefieldKit.TrenchWallsDamaged and the others), gone
// at nothing (the twin collapses to pieces, as any prop does); heavy ordnance close by shatters it outright. Plain
// arithmetic, no scene: PropDestruction and PropWear call it, TrenchSectionTests pin it down.
namespace TW.Presentation.Terrain
{
    public enum SectionState : byte { Intact = 0, Damaged = 1, Gone = 2 }

    public static class TrenchSectionRules
    {
        /// <summary>At this share of its strength left (or less) an intact section is damaged.</summary>
        public const float DamagedBelow = 0.5f;
        /// <summary>Heavy ordnance: a burst of this radius or more (the barrage's 8 m, a gunboat's 7.5, a cook-off's 9, the
        /// ambient shells from 6) or of this harm or more (the AP round's 1.1) shatters a section inside HeavyInner of
        /// its reach, whatever the section's state.</summary>
        public const float HeavyRadius = 6f, HeavyPower = 1.05f, HeavyInner = 0.5f;
        /// <summary>Seconds the pieces of a broken section are drawn before they sink (a bay is clear of its fragments
        /// fifteen seconds after the shell, with DebrisMath.SinkSeconds).</summary>
        public const float LiningLife = 12f;
        /// <summary>At most this many pieces one strike throws off the lining; past it a section throws dust only.</summary>
        public const int MaxSectionPiecesPerStrike = 120;

        public static bool IsHeavy(float radius, float power, float distance, float reach)
            => (radius >= HeavyRadius || power >= HeavyPower) && distance <= reach * HeavyInner;

        /// <summary>One hit on a section: the strength it has left goes down by the harm and the state it is in comes
        /// back. Heavy ordnance takes everything at once; a gone section stays gone.</summary>
        public static SectionState Apply(ref float hp, float maxHp, float harm, bool heavy, SectionState was)
        {
            if (was == SectionState.Gone) { hp = 0f; return SectionState.Gone; }
            if (heavy) { hp = 0f; return SectionState.Gone; }
            hp -= harm;
            if (hp <= 0f) { hp = 0f; return SectionState.Gone; }
            return hp <= maxHp * DamagedBelow ? SectionState.Damaged : SectionState.Intact;
        }
    }
}
