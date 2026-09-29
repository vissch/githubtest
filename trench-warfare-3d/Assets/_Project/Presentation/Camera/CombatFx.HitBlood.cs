// Phase: hit blood (2026-09-29, owner: "i need blood as feedback when the infantry get hit") — part of CombatFx: a man
// struck and still standing bleeds. Out of the far side of him, along the round's way, a spray of red droplets (more for a
// heavier hit); a card of the VFX lane's BloodSpurt where the round met him, when the build has that book; and a few
// drops on the ground behind him that lie there a while, shorter than a death's splat. GORE scales all of it (0: none).
// Not behind fx.deathAbsurd: this is a hit, not a death gag. Seeded from the event, so a replay bleeds the same.
// Bounded: droplets only near the look and by the zoom's share (DebrisRenderer.ZoomShare), ground drops a few a frame.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>Droplets for a light hit and the most for a heavy one; the damage that counts as heavy.</summary>
        public const int HitDropsMin = 4, HitDropsMax = 9;
        public const float HitHeavy = 60f;
        /// <summary>Metres from the look beyond which a hit throws no droplets (its mark and its card still show).</summary>
        public const float HitBloodReach = 70f;
        /// <summary>Ground drops laid a frame at most, and how long one lies (a death's splat lies BloodLife).</summary>
        public const int HitMarksPerFrame = 4;
        public const float HitMarkLife = 30f;
        /// <summary>A droplet's red: brighter than a gore lump's (GoreRed), which the debris shading took to brown specks.</summary>
        static readonly Color HitRed = new Color(0.85f, 0.03f, 0.03f);
        int hitMarksFrame = -1, hitMarksThisFrame;

        /// <summary>A man hit and not killed (the Hit event's b, damage above 0): his blood, at `p` (where the round met him,
        /// his chest as drawn), `toward` the way the round was going (flat, unit), `scale` his drawn size.</summary>
        void HitBlood(in SimEvent e, Vector3 p, Vector3 toward, float scale)
        {
            float gore = DebrisRenderer.Gore;
            if (gore <= 0f) return;
            var rng = new DebrisRng(p, 0xB1EEu + e.Tick * 31u + (uint)Mathf.Max(0, e.B));
            float heavy = Mathf.Clamp01(e.Scalar / HitHeavy);
            bool near = CameraShake.DistanceToLook(p) < HitBloodReach;
            // the spray: out of the far side, fanned, up a little, droplets that land and lie a few seconds
            if (near && debris != null && debris.Ready)
            {
                int drops = Mathf.RoundToInt(Mathf.Lerp(HitDropsMin, HitDropsMax, heavy) * gore * DebrisRenderer.ZoomShare);
                Vector3 exit = p + toward * (0.15f * scale);
                for (int k = 0; k < drops; k++)
                {
                    Vector3 v = toward * rng.Range(2.5f, 4.5f) + Vector3.up * rng.Range(0.8f, 2.2f) + rng.OnSphere() * 1.1f;
                    debris.Throw(DebrisRenderer.Piece.Clod, exit, v, rng.Range(0.09f, 0.15f) * scale, HitRed, ref rng, 4f);   // smaller did not show at the shot scenes' zoom (first film)
                }
            }
            // the card of blood where the round struck, when the build has the book (CombatFx.Gags looks it up by name)
            if (bloodBook == -2) bloodBook = System.Enum.TryParse("BloodSpurt", out FlipbookFx.Book found) ? (int)found : -1;
            if (bloodBook >= 0 && books != null && books.Ready && near)
                books.Add((FlipbookFx.Book)bloodBook, p, (0.7f + 0.5f * heavy) * scale * Mathf.Sqrt(gore), 0.35f, FlipbookFx.Kind.None, velocity: toward * 1.2f, alpha: Mathf.Clamp01(gore));
            // drops on the ground behind him, where the spray comes down: a few a frame, so a machine gun on a line does not
            // push the deaths' splats out of their pool (and these lie shorter, so they are the ones pushed out first)
            if (hitMarksFrame != Time.frameCount) { hitMarksFrame = Time.frameCount; hitMarksThisFrame = 0; }
            if (hitMarksThisFrame >= HitMarksPerFrame) return;
            hitMarksThisFrame++;
            Vector3 at = p + toward * (rng.Range(0.5f, 1.1f) * scale) + new Vector3(rng.Range(-0.2f, 0.2f), 0f, rng.Range(-0.2f, 0.2f)) * scale;
            float size = (0.45f + 0.4f * heavy) * Mathf.Sqrt(gore) * scale;
            float life = SceneTints.Now.Frozen ? HitMarkLife * 2f : HitMarkLife;
            AddGagMark(at, rng.Range(0f, 360f), new Vector2(size, size * rng.Range(0.7f, 1.3f)), life, 3, 0.3f);
        }
    }
}
