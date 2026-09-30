// Phase: deaths (2026-09-28, implemented) — part of CombatFx: what a death gag (DeathGags) leaves on the field besides
// the body. A headshot's helmet goes straight up (and, GORE willing, the head comes off with it: the VAT shader cuts it
// from the body); a pancake squirts out sideways from under the track and its helmet rolls clear; the beam leaves two
// smoking boots with the helmet dropped on them; every landing of a body that bounces kicks up dust. The body itself
// is VATRenderer's: AddFallen with the gag plans the path, and the landings are read from that plan. Seeded from the
// death's record, so a replay throws the same things.
using System.Collections.Generic;
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
            // torn in two by the shell (CombatFx.OwnGibs): both his halves flew, so no corpse; his path is still planned,
            // and the dust and the blood go where it comes down, which is where the upper half lands
            if ((gib & GibPlan.TornBit) != 0) { gib &= ~GibPlan.TornBit; gag.Flags |= GagFlags.NoCorpse; }
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
                        debris.Throw(archetype == InfantryArchetype.Frog ? DebrisRenderer.Piece.FrogHead : DebrisRenderer.Piece.Head, head, hv, scale, Skin, ref rng, 30f);
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
            bool near = CameraShake.DistanceToLook(p) < GagDustReach;
            if (books != null && books.Ready && path.Arcs > 0 && near)
                for (int k = 0; k < path.Arcs; k++)
                {
                    var a = k == 0 ? path.A0 : k == 1 ? path.A1 : path.A2;
                    books.Add(FlipbookFx.Book.Puff, a.To + Vector3.up * 0.15f, (k == 0 ? 1.6f : 1.1f) * scale, 0.8f, FlipbookFx.Kind.Upright,
                        velocity: Vector3.up * 0.6f, grow: 0.8f, alpha: k == 0 ? 0.7f : 0.5f, pop: 0.2f, delay: path.Delay + a.End);
                }
            GagBlood(e, rec, gag, path, p, scale, near, gib, density);
        }

        // ------------------------------------------------------------------ blood (GORE scales it, 0 is none)
        struct GagMark { public Matrix4x4 At; public float Born, Life; public byte Shape; }
        readonly List<GagMark> gagMarks = new List<GagMark>(MaxGagMarks);
        /// <summary>Blood and scorch marks at once (their own pool: blood never pushes a rut or a boot print out). Past it
        /// the one nearest gone is overwritten.</summary>
        public const int MaxGagMarks = 192;
        /// <summary>A shell's smoke with the absurd deaths on (CombatFx, the burst): where its puffs start (in blast radii
        /// up), how fast they climb (m/s; 0.4 at 0) and the share of today's life and opacity they keep.</summary>
        public const float SmokeLiftBase = 1.2f, SmokeLiftRise = 2.2f, SmokeLiftFade = 0.45f;   // frog round 4: at 0.9, 1.6, 0.6 it still hid the shell moment for four stills
        /// <summary>How long a splat lies on mud, and on snow (where nothing closes over it).</summary>
        public const float BloodLife = 45f, BloodLifeSnow = 150f;
        readonly Material[] gagMarkMats = new Material[2];   // shape 3 blood, 4 scorch
        readonly List<Matrix4x4>[] gagMarkBatch = { new List<Matrix4x4>(MaxGagMarks), new List<Matrix4x4>(MaxGagMarks) };   // full size: never grown mid-battle
        static readonly Color BloodColor = new Color(0.28f, 0.0f, 0.01f), ScorchColor = new Color(0.05f, 0.042f, 0.036f);
        /// <summary>FlipbookFx's blood book when the build has one (the VFX lane's BloodSpurt), by name so this compiles
        /// without it: -2 not looked yet, -1 none.</summary>
        int bloodBook = -2;

        /// <summary>What a gagged death spills: a trail of gore behind a flying body, a splat where each arc lands, a
        /// streak along a skid, a pancake's splat, the scorch under the beam's boots and a burnt man's skid.</summary>
        void GagBlood(in SimEvent e, in DeathRecord rec, in GagPlan gag, in FallenFlight.Plan path, Vector3 p, float scale, bool near, int gib, int density)
        {
            float gore = DebrisRenderer.Gore;
            float life = SceneTints.Now.Frozen ? BloodLifeSnow : BloodLife;
            var rng = new DebrisRng(rec.Pos, 0xB100u + gag.Seed);
            if (gag.Gag == DeathGag.Boots) { AddGagMark(p, 0f, new Vector2(1.5f, 1.5f) * scale, life * 2f, 4, 0f); return; }
            if (gag.Gag == DeathGag.Skid && path.SkidDur > 0f) AddGagMark(path.SkidTo, 0f, new Vector2(1.4f, 1.4f) * scale, life * 2f, 4, path.Delay + path.SkidStart + path.SkidDur);
            if (gore <= 0f) return;
            float size = Mathf.Sqrt(gore) * scale;
            for (int k = 0; k < path.Arcs; k++)
            {
                if (k > 0 && !near) break;   // far out, the first landing is all that reads
                var a = k == 0 ? path.A0 : k == 1 ? path.A1 : path.A2;
                float s = (k == 0 ? 2.6f : k == 1 ? 1.7f : 1.2f) * size;   // round 2: smaller read as dirty snow
                AddGagMark(a.To, rng.Range(0f, 360f), new Vector2(s, s * rng.Range(0.8f, 1.2f)), life, 3, path.Delay + a.End);
            }
            if (path.SkidDur > 0f && gag.Gag != DeathGag.Skid)
            {
                Vector3 along = path.SkidTo - path.SkidFrom; along.y = 0f;
                float len = along.magnitude;
                if (len > 0.3f) AddGagMark((path.SkidFrom + path.SkidTo) * 0.5f, Mathf.Atan2(along.x, along.z) * Mathf.Rad2Deg, new Vector2(0.7f * size, len + 0.6f * size), life, 3, path.Delay + path.SkidStart);
            }
            if (gag.Gag == DeathGag.Pancake) AddGagMark(p, rec.Yaw * Mathf.Rad2Deg, new Vector2(2.2f, 2.8f) * size, life, 3, 0.1f);
            if (gag.Gag == DeathGag.HeadPop && (gib & (1 << 1)) != 0) AddGagMark(p, rng.Range(0f, 360f), new Vector2(1f, 1f) * size, life, 3, 0.2f);
            // the trail: lumps thrown with the body at its own launch, a touch slower under the debris' lighter gravity, so
            // they string out behind him along the same arc (nothing per frame: each is one debris record)
            if (path.Arcs > 0 && path.Delay <= 0.2f && debris != null && debris.Ready)
            {
                int lumps = Mathf.RoundToInt(2f * gore * (1f + 0.25f * density) * (near ? DebrisRenderer.ZoomShare : 0f));   // round 6: fewer, so his parts read
                var a = path.A0;
                Vector3 v = new Vector3((a.To.x - a.From.x) / a.Dur, path.Gravity * a.Up, (a.To.z - a.From.z) / a.Dur) * Mathf.Sqrt(DebrisMath.Gravity / path.Gravity);
                for (int k = 0; k < lumps; k++)
                    debris.Throw(DebrisRenderer.Piece.Clod, a.From + Vector3.up * (1.0f * scale), v * rng.Range(0.85f, 1f) + rng.OnSphere() * 0.6f, rng.Range(0.07f, 0.12f) * scale, GoreRed, ref rng, 8f);
            }
            // a card of blood where the round or the claw struck, when the build has the book
            if (bloodBook == -2) bloodBook = System.Enum.TryParse("BloodSpurt", out FlipbookFx.Book found) ? (int)found : -1;
            if (bloodBook >= 0 && books != null && books.Ready && near && (gag.Gag == DeathGag.Punt || gag.Gag == DeathGag.Jig || gag.Gag == DeathGag.HeadPop || gag.Gag == DeathGag.Flung))
                books.Add((FlipbookFx.Book)bloodBook, p + Vector3.up * (1.3f * scale), 1.2f * scale * Mathf.Sqrt(gore), 0.5f, FlipbookFx.Kind.None, alpha: Mathf.Clamp01(gore), delay: gag.Gag == DeathGag.Jig ? 0.05f : 0f);
        }

        void AddGagMark(Vector3 at, float yawDegrees, Vector2 size, float life, byte shape, float delay)
        {
            if (Host == null || Host.Local == null) return;
            float y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 0.03f;
            var mark = new GagMark { At = Matrix4x4.TRS(new Vector3(at.x, y, at.z), Lie(at.x, at.z, yawDegrees, 0.3f), new Vector3(size.x, 1f, size.y)), Born = Time.time + delay, Life = life, Shape = shape };
            if (gagMarks.Count < MaxGagMarks) { gagMarks.Add(mark); return; }
            int worst = 0; float gone = -1f, now = Time.time;
            for (int i = 0; i < gagMarks.Count; i++) { float k = (now - gagMarks[i].Born) / gagMarks[i].Life; if (k > gone) { gone = k; worst = i; } }
            gagMarks[worst] = mark;
        }

        /// <summary>Once a frame (from TickSmoulders): the blood and scorch marks, faded by their age as the ruts are, two
        /// instanced draws at most. A mark not yet born (a landing still to come) is not drawn.</summary>
        void DrawGagMarks(float now)
        {
            if (gagMarks.Count == 0 || markQuad == null) return;
            Prune(gagMarks, now, static (m, at) => at - m.Born > m.Life);
            if (gagMarkMats[0] == null)
            {
                var shader = Shader.Find("TW/GroundMark (URP)");
                if (shader == null) return;
                for (int k = 0; k < 2; k++)
                {
                    gagMarkMats[k] = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, name = k == 0 ? "Blood mark" : "Scorch mark" };
                    gagMarkMats[k].SetFloat("_Shape", 3 + k); gagMarkMats[k].SetFloat("_Alpha", k == 0 ? 1f : 0.75f);   // round 4: blood read as pink paint on snow
                    gagMarkMats[k].SetColor("_Color", k == 0 ? BloodColor : ScorchColor);
                    gagMarkMats[k].SetFloat("_FadeFrom", 90f); gagMarkMats[k].SetFloat("_FadeOver", 30f);
                    gagMarkMats[k].SetFloat("_DetailFrom", 12f); gagMarkMats[k].SetFloat("_DetailOver", 16f);
                }
            }
            for (int k = 0; k < 2; k++) gagMarkBatch[k].Clear();
            for (int i = 0; i < gagMarks.Count; i++)
            {
                var m = gagMarks[i];
                if (now < m.Born) continue;
                float age = (now - m.Born) / m.Life;
                float left = 1f - 0.82f * (0.35f * age + 0.65f * age * age);   // the ruts' fade (CombatFx.Ground)
                var at = m.At; at.m01 *= left; at.m11 *= left; at.m21 *= left;
                gagMarkBatch[m.Shape - 3].Add(at);
            }
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));
            for (int k = 0; k < 2; k++)
                if (gagMarkBatch[k].Count > 0)
                    Flush(markQuad, gagMarkBatch[k], new RenderParams(gagMarkMats[k]) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
        }
    }
}
