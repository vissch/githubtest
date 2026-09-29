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
        /// <summary>Blobs in the splash where the round met him, and how long they last; the zoom past which a hit's blood
        /// is drawn larger, over how many zoom units it doubles, the most, and the most for a mark on the ground.</summary>
        public const int HitSplash = 3;
        public const float HitSplashLife = 0.5f, HitZoomFrom = 12f, HitZoomOver = 15f, HitZoomMost = 2.2f, HitMarkBoostCap = 1.6f;
        int hitMarksFrame = -1, hitMarksThisFrame;
        static readonly int TWGoreId = Shader.PropertyToID("_TWGore");

        /// <summary>A man hit and not killed (the Hit event's b, damage above 0): his blood, at `p` (where the round met him,
        /// his chest as drawn), `toward` the way the round was going (flat, unit), `scale` his drawn size.</summary>
        void HitBlood(in SimEvent e, Vector3 p, Vector3 toward, float scale)
        {
            float gore = DebrisRenderer.Gore;
            if (gore <= 0f) return;
            var rng = new DebrisRng(p, 0xB1EEu + e.Tick * 31u + (uint)Mathf.Max(0, e.B));
            float heavy = Mathf.Clamp01(e.Scalar / HitHeavy);
            bool near = CameraShake.DistanceToLook(p) < HitBloodReach;
            // pulled back, what flies is drawn larger (his drawn size alone grows too little: at the zoom the game is played
            // at, droplets and marks sized for a close look were not there at all, four-view film 2026-09-29)
            var cam = Camera.main;
            float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
            float boost = HitZoomBoost(zoom);
            if (near && debris != null && debris.Ready)
            {
                // the splash where the round met him: a few large blobs, gone in half a second, the read at a distance
                float blob = 0.22f * scale * boost * Mathf.Sqrt(gore);
                for (int k = 0; k < HitSplash; k++)
                    debris.Throw(DebrisRenderer.Piece.Clod, p, toward * rng.Range(0.6f, 1.4f) + Vector3.up * rng.Range(0.4f, 1.2f) + rng.OnSphere() * 0.6f, blob * rng.Range(0.8f, 1.2f), HitRed, ref rng, HitSplashLife);
                // the spray: out of the far side, fanned, up a little, droplets that land and lie a few seconds
                int drops = Mathf.RoundToInt(Mathf.Lerp(HitDropsMin, HitDropsMax, heavy) * gore * DebrisRenderer.ZoomShare);
                Vector3 exit = p + toward * (0.15f * scale);
                for (int k = 0; k < drops; k++)
                {
                    Vector3 v = toward * rng.Range(2.5f, 4.5f) + Vector3.up * rng.Range(0.8f, 2.2f) + rng.OnSphere() * 1.1f;
                    debris.Throw(DebrisRenderer.Piece.Clod, exit, v, rng.Range(0.09f, 0.15f) * scale * boost, HitRed, ref rng, 4f);   // smaller did not show at the shot scenes' zoom (first film)
                }
                // and a little back out of the way it went in, so the shooter's side sees blood too (from behind the row the
                // spray and its marks were hidden by the men themselves)
                for (int k = 0; k < 2; k++)
                    debris.Throw(DebrisRenderer.Piece.Clod, p - toward * (0.1f * scale), -toward * rng.Range(1.2f, 2.2f) + Vector3.up * rng.Range(0.6f, 1.4f) + rng.OnSphere() * 0.6f, rng.Range(0.08f, 0.12f) * scale * boost, HitRed, ref rng, 4f);
            }
            // the card of blood where the round struck, when the build has the book (CombatFx.Gags looks it up by name)
            if (bloodBook == -2) bloodBook = System.Enum.TryParse("BloodSpurt", out FlipbookFx.Book found) ? (int)found : -1;
            if (bloodBook >= 0 && books != null && books.Ready && near)
                books.Add((FlipbookFx.Book)bloodBook, p, (0.7f + 0.5f * heavy) * scale * boost * Mathf.Sqrt(gore), 0.35f, FlipbookFx.Kind.None, velocity: toward * 1.2f, alpha: Mathf.Clamp01(gore));
            // on the ground: behind him where the spray comes down, and at his feet (seen from either side); a few a frame, so
            // a machine gun on a line does not push the deaths' splats out of their pool (these lie shorter: pushed out first)
            if (hitMarksFrame != Time.frameCount) { hitMarksFrame = Time.frameCount; hitMarksThisFrame = 0; }
            if (hitMarksThisFrame >= HitMarksPerFrame) return;
            hitMarksThisFrame += 2;
            float grow = Mathf.Min(boost, HitMarkBoostCap);   // marks outlast the zoom they were laid at: grown less
            float size = (0.45f + 0.4f * heavy) * Mathf.Sqrt(gore) * scale * grow;
            float life = SceneTints.Now.Frozen ? HitMarkLife * 2f : HitMarkLife;
            Vector3 behind = p + toward * (rng.Range(0.5f, 1.1f) * scale) + new Vector3(rng.Range(-0.2f, 0.2f), 0f, rng.Range(-0.2f, 0.2f)) * scale;
            AddGagMark(behind, rng.Range(0f, 360f), new Vector2(size, size * rng.Range(0.7f, 1.3f)), life, 3, 0.3f);
            Vector3 feet = p + new Vector3(rng.Range(-0.25f, 0.25f), 0f, rng.Range(-0.25f, 0.25f)) * scale;
            AddGagMark(feet, rng.Range(0f, 360f), new Vector2(size, size) * 0.7f, life, 3, 0.15f);
        }

        /// <summary>How much larger a hit's blood is drawn at a zoom: 1 close in, growing past HitZoomFrom, HitZoomMost at most.</summary>
        public static float HitZoomBoost(float zoom) => Mathf.Clamp(1f + (zoom - HitZoomFrom) / HitZoomOver, 1f, HitZoomMost);
    }
}
