// Phase: deaths (2026-09-28, implemented) — part of CombatFx: what a death gag (DeathGags) leaves on the field besides
// the body. A headshot's helmet goes straight up (and, GORE willing, the head comes off with it: the VAT shader cuts it
// from the body); a pancake squirts out sideways from under the track and its helmet rolls clear; the beam leaves two
// smoking boots with the helmet dropped on them; every landing of a body that bounces kicks up dust. The body itself
// is VATRenderer's: AddFallen with the gag plans the path, and the landings are read from that plan. Seeded from the
// death's record, so a replay throws the same things.
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        static readonly Color Leather = new Color(0.20f, 0.15f, 0.11f);
        /// <summary>Metres from the middle of the picture beyond which a gag throws its dust no more (the body still flies).</summary>
        public const float GagDustReach = 120f;

        /// <summary>A gagged death (OnDeath hands it here instead of the plain AddFallen): the body, and what comes off it.</summary>
        void GagDeath(in SimEvent e, in DeathRecord rec, Vector3 p, float yaw, byte team, byte archetype, int variant, Clip clip, Clip from, float fromPhase, float fade, Vector3 fly, int gib, float grime, int density, int chr)
        {
            var gag = rec.Gag;
            float scale = FigureScale();
            bool pieces = debris != null && debris.Ready, gore = pieces && DebrisRenderer.Gore > 0f;
            // a headshot may take his head: the shader cuts limb 1 (the head and the helmet) from the body, GORE willing
            if ((gag.Flags & GagFlags.HeadOff) != 0 && DebrisRenderer.Gore >= 0.5f && gag.Dice < 0.6f) gib |= 1 << 1;
            units.AddFallen(new Vector3(p.x, p.y - 0.02f, p.z), yaw, team, variant, clip, archetype, from, fromPhase, fade, fly, gib, grime, density, chr, gag, out var path);
            if (!pieces) return;
            var rng = new DebrisRng(rec.Pos, 0x6A6u + gag.Seed);
            Vector3 head = p + Vector3.up * (1.62f * scale);
            Vector3 facing = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)), right = new Vector3(facing.z, 0f, -facing.x);
            switch (gag.Gag)
            {
                case DeathGag.HeadPop:
                {
                    // the helmet straight up, as high as the intensity goes
                    float up = Mathf.Min(22f, 16f * Mathf.Sqrt(gag.Intensity));
                    debris.Throw(DebrisRenderer.Piece.Helmet, head + Vector3.up * (0.12f * scale), new Vector3(rng.Range(-0.6f, 0.6f), up, rng.Range(-0.6f, 0.6f)), 0.32f * scale, Steel, ref rng, 40f);
                    if ((gib & (1 << 1)) != 0 && gore)
                    {
                        Vector3 hv = -facing * 2.2f + Vector3.up * 3.2f + rng.OnSphere() * 0.8f;
                        debris.Throw(DebrisRenderer.Piece.Head, head, hv, scale, Skin, ref rng, 30f);
                        int lumps = Mathf.RoundToInt(4f * DebrisRenderer.Gore);
                        for (int k = 0; k < lumps; k++) debris.Throw(DebrisRenderer.Piece.Clod, head, hv * 0.8f + rng.OnSphere() * 2.5f, rng.Range(0.06f, 0.11f) * scale, Gore, ref rng, 8f);
                    }
                    break;
                }
                case DeathGag.Boots:
                {
                    // all that is left: his boots, standing, smoking, and his helmet dropping onto them
                    var pose = Quaternion.Euler(0f, yaw * Mathf.Rad2Deg, 0f);
                    for (int k = -1; k <= 1; k += 2)
                        debris.Throw(DebrisRenderer.Piece.Boot, p + right * (0.12f * k * scale) + Vector3.up * (0.05f * scale), Vector3.zero, scale, Leather, ref rng, 25f, 1f, pose);
                    debris.Throw(DebrisRenderer.Piece.Helmet, head, Vector3.up * 0.8f, 0.32f * scale, Charred, ref rng, 25f, 0.7f, pose);
                    AddSmoulder(p, rec.Pos);
                    debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * (0.9f * scale), 6, 2.5f, 0.09f * scale, Charred, 10f, 0.8f, 1.2f, default, e.Tick);   // ash
                    break;
                }
                case DeathGag.Pancake:
                {
                    // out from under the track, both sides, low and fast; the helmet rolls clear
                    if (gore)
                        for (int side = -1; side <= 1; side += 2)
                            debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.1f, Mathf.Max(1, Mathf.RoundToInt(3f * DebrisRenderer.Gore)), 4.5f, 0.11f * scale, Gore, 10f, 0f, 0.35f, right * (1.4f * side), e.Tick + (uint)(side + 2));
                    debris.Throw(DebrisRenderer.Piece.Helmet, p + Vector3.up * (0.3f * scale), right * 2.2f + Vector3.up * 1.6f, 0.32f * scale, Steel, ref rng, 60f);
                    break;
                }
            }
            // dust where each arc of a bouncing body comes down, timed to the landing
            if (books != null && books.Ready && path.Arcs > 0 && CameraShake.DistanceToLook(p) < GagDustReach)
                for (int k = 0; k < path.Arcs; k++)
                {
                    var a = k == 0 ? path.A0 : k == 1 ? path.A1 : path.A2;
                    books.Add(FlipbookFx.Book.Puff, a.To + Vector3.up * 0.15f, (k == 0 ? 1.6f : 1.1f) * scale, 0.8f, FlipbookFx.Kind.Upright,
                        velocity: Vector3.up * 0.6f, grow: 0.8f, alpha: k == 0 ? 0.7f : 0.5f, pop: 0.2f, delay: path.Delay + a.End);
                }
        }
    }
}
