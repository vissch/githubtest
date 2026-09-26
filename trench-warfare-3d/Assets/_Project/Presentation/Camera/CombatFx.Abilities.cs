// Phase: C4 / docs/21 phase 5 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared
// pools): what the line abilities look like. The aim: the disc or the corridor the player is dragging (AbilityAim's
// Shape, through SceneHooks.AimPreview: the effects do not know who owns the aim), with a tick at every lift of a
// stepping pattern. The strafe: an aircraft's run-in from 200 m out, timed to
// cross the corridor's start as the warm-up ends (the kit's biplane through SceneHooks.Biplane, a box without it),
// and two tracers from its guns to every burst, which is drawn light (dust, not a shell's column). The beam: a glow
// that gathers over the start during the charge, then a tall additive column at the head as it walks the corridor,
// a flash card every few frames, a low shake; the scorch under it (the sim's Explosion with dir.y = BlastShape.Beam)
// is a dark puff and sparks. The smoke screen: the sim's smoke field drawn as pale cards over each thick cell, the
// way the gas is drawn. Creeping gas needs nothing new: a GasCloudSpawned per step. The aircraft and the beam are
// timed by the sim's clock (the tick and the fraction of the next, SimNow), not the wall clock: their payloads land
// on ticks, so paused or at any speed the picture holds with the bursts.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        struct Flyover { public Vector3 Start, Dir; public float Length, Fired, Warm; }        // Fired, Warm: sim seconds
        struct Sweep { public Vector3 Start, Dir; public float Length, HalfWidth, T0, T1; }     // T0, T1: sim seconds
        readonly List<Flyover> flyovers = new List<Flyover>(4);
        readonly List<Sweep> sweeps = new List<Sweep>(4);
        readonly List<Matrix4x4> smokeCards = new List<Matrix4x4>(2048);
        public const float PlaneSpeed = 40f, PlaneRunIn = 200f, PlaneRunOut = 120f, PlaneHigh = 45f, PlaneLow = 25f;
        public const float BeamChargeSeconds = 4f, BeamFlashEvery = 0.08f, ScorchShakeEvery = 0.3f;
        float nextBeamFlash, nextScorchShake;

        /// <summary>The sim's clock in seconds (the tick plus the fraction of the next one the host has accumulated):
        /// what the aircraft and the beam are timed by, so a paused or slowed match holds them with the bursts.</summary>
        float SimNow => Host != null && Host.Local != null ? (Host.Local.World.Tick + Host.Alpha) * Host.Local.World.Config.TickSeconds : 0f;

        /// <summary>Metres along the corridor the aircraft is at a sim time: negative on the run-in, past the length on the
        /// run-out, zero over the corridor's start as the warm-up ends.</summary>
        public static float PlaneAlong(float simNow, float firedSeconds, float warmSeconds) => (simNow - firedSeconds - warmSeconds) * PlaneSpeed;
        /// <summary>The aircraft's height over the ground at a point of its run: high on the run-in, low along the corridor, climbing away.</summary>
        public static float PlaneAltitude(float along, float length)
            => along < 0f ? Mathf.Lerp(PlaneHigh, PlaneLow, Mathf.InverseLerp(-PlaneRunIn, 0f, along))
             : along > length ? Mathf.Lerp(PlaneLow, PlaneHigh, Mathf.InverseLerp(length, length + PlaneRunOut, along)) : PlaneLow;
        /// <summary>How far along its corridor a sweep is at a sim time: 0 as the warm-up ends, 1 as the sweep ends, the
        /// fraction BeamSystem.HeadOf walks tick by tick.</summary>
        public static float SweepFraction(float simNow, float t0, float t1) => Mathf.Clamp01((simNow - t0) / Mathf.Max(0.01f, t1 - t0));

        /// <summary>What the banner calls a support ability.</summary>
        static string AbilityWord(int id)
        {
            switch ((OffMapAbilityId)id)
            {
                case OffMapAbilityId.ChlorineGas: return "gas";
                case OffMapAbilityId.SmokeScreen: return "smoke";
                case OffMapAbilityId.StrafeRun: return "strafe";
                case OffMapAbilityId.Beam: return "beam";
                case OffMapAbilityId.CreepingBarrage: return "creeping barrage";
                default: return "barrage";
            }
        }

        /// <summary>AbilityFired: what the sim is about to do is set going here (the aircraft's run-in, the beam's charge).
        /// dir is the heading times the length for a line ability, zero for a point.</summary>
        void OnAbilityFired(SimEvent e)
        {
            if (Host == null || Host.Local == null || !OffMapAbilitySystem.TryGetStats(e.A, out var stats)) return;
            float tick = Host.Local.World.Config.TickSeconds, fired = e.Tick * tick;   // sim seconds: the payloads land on ticks
            var dir = new Vector3(e.Dir.x, 0f, e.Dir.z);
            float length = dir.magnitude;
            if (length < 1e-3f) return;
            dir /= length;
            var start = new Vector3(e.Pos.x, 0f, e.Pos.z);
            if (e.A == (int)OffMapAbilityId.StrafeRun)
            {
                if (flyovers.Count >= 4) flyovers.RemoveAt(0);
                flyovers.Add(new Flyover { Start = start, Dir = dir, Length = length, Fired = fired, Warm = stats.WarmupTicks * tick });
            }
            else if (e.A == (int)OffMapAbilityId.Beam)
            {
                if (sweeps.Count >= 4) sweeps.RemoveAt(0);
                sweeps.Add(new Sweep { Start = start, Dir = dir, Length = length, HalfWidth = Mathf.Max(0.5f, stats.HalfWidth), T0 = fired + stats.WarmupTicks * tick, T1 = fired + (stats.WarmupTicks + stats.SpreadTicks) * tick });
            }
        }

        /// <summary>Where the aircraft of a run is now: 200 m out and high as the ability fires, over the corridor's
        /// start as the warm-up ends, low along the corridor, climbing away past its end.</summary>
        bool PlaneAt(in Flyover f, float simNow, float now, out Vector3 at, out Quaternion rot)
        {
            float along = PlaneAlong(simNow, f.Fired, f.Warm);
            at = default; rot = Quaternion.identity;
            if (along < -PlaneRunIn || along > f.Length + PlaneRunOut) return false;
            float alt = PlaneAltitude(along, f.Length);
            var over = f.Start + f.Dir * Mathf.Clamp(along, 0f, f.Length);
            at = f.Start + f.Dir * along;
            at.y = RenderGround.Sample(Host.Local.Map, over.x, over.z) + alt;
            float bank = Mathf.Sin(now * 1.7f) * 4f;
            rot = Quaternion.LookRotation(f.Dir) * Quaternion.Euler(along < 0f ? 6f : along > f.Length ? -8f : 0f, 0f, bank);
            return true;
        }

        bool TryPlane(float now, out Vector3 at, out Quaternion rot)
        {
            float simNow = SimNow;
            for (int i = flyovers.Count - 1; i >= 0; i--) if (PlaneAt(flyovers[i], simNow, now, out at, out rot)) return true;
            at = default; rot = Quaternion.identity; return false;
        }

        void DrawFlyovers(float now, Bounds bounds)
        {
            if (flyovers.Count == 0) return;
            var kit = SceneHooks.Biplane?.Invoke();
            float simNow = SimNow;
            for (int i = flyovers.Count - 1; i >= 0; i--)
            {
                var f = flyovers[i];
                float along = PlaneAlong(simNow, f.Fired, f.Warm);
                if (along > f.Length + PlaneRunOut) { flyovers.RemoveAt(i); continue; }
                if (!PlaneAt(f, simNow, now, out var at, out var rot)) continue;
                if (kit.HasValue && kit.Value.mesh != null && kit.Value.material != null)
                    FrameBudget.Draw(new RenderParams(kit.Value.material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On, receiveShadows = false }, kit.Value.mesh, 0, Matrix4x4.TRS(at, rot, kit.Value.scale));
                else
                {
                    // no kit in the scene (a bare greybox): a box the size of the aircraft, so the pass still reads
                    batch.Clear();
                    batch.Add(Matrix4x4.TRS(at, rot, new Vector3(7f, 1f, 6f)));
                    Flush(cube, new RenderParams(dirtMat) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On });
                }
            }
        }

        void DrawSweeps(float now, Bounds bounds)
        {
            if (sweeps.Count == 0) return;
            var map = Host.Local.Map;
            float simNow = SimNow;
            bool running = Host.TimeScale > 0f;   // paused: the column stands where it is, the cards and the shake wait
            for (int i = sweeps.Count - 1; i >= 0; i--)
            {
                var s = sweeps[i];
                if (simNow > s.T1 + 0.5f) { sweeps.RemoveAt(i); continue; }
                float groundY = RenderGround.Sample(map, s.Start.x, s.Start.z);
                if (simNow < s.T0)
                {
                    // the charge: a glow gathering over the start of the corridor, brighter and larger as it comes
                    float u = Mathf.InverseLerp(s.T0 - BeamChargeSeconds, s.T0, simNow);
                    var glow = sparkMat != null ? sparkMat : aimMat;
                    if (glow == null) continue;
                    batch.Clear();
                    batch.Add(Matrix4x4.TRS(new Vector3(s.Start.x, groundY + 1f + u * 2f, s.Start.z), Quaternion.identity, Vector3.one * Mathf.Lerp(0.6f, 3f, u)));
                    Flush(sphere, new RenderParams(glow) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off });
                    continue;
                }
                float t = SweepFraction(simNow, s.T0, s.T1);
                var head = s.Start + s.Dir * (s.Length * t);
                head.y = RenderGround.Sample(map, head.x, head.z);
                var column = flashMat != null ? flashMat : sparkMat != null ? sparkMat : aimMat;
                if (column != null)
                {
                    // the column of fire from above: tall, thin, the width of the corridor; and the glow where it meets the ground
                    batch.Clear();
                    batch.Add(Matrix4x4.TRS(head + Vector3.up * 30f, Quaternion.LookRotation(s.Dir), new Vector3(s.HalfWidth * 1.2f, 60f, s.HalfWidth * 1.2f)));
                    Flush(cube, new RenderParams(column) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off });
                    batch.Clear();
                    batch.Add(Matrix4x4.TRS(head + Vector3.up * 0.8f, Quaternion.identity, Vector3.one * (s.HalfWidth * 2.4f)));
                    Flush(sphere, new RenderParams(column) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off });
                }
                if (running && now >= nextBeamFlash && books != null && books.Ready)
                {
                    nextBeamFlash = now + BeamFlashEvery;
                    books.Add(FlipbookFx.Book.Flash, head + Vector3.up * 1.2f, s.HalfWidth * 3f, 0.12f, FlipbookFx.Kind.Upright, glow: SceneMood.Night ? 3f : 1.6f);
                }
                if (running && now >= nextScorchShake) { nextScorchShake = now + ScorchShakeEvery; CameraShake.Add(head, 0.25f); }
            }
        }

        /// <summary>The aircraft's run and the beam's sweep, every frame.</summary>
        void TickAbilities(float now, Bounds bounds)
        {
            DrawFlyovers(now, bounds);
            DrawSweeps(now, bounds);
        }

        /// <summary>The bursts that are not a shell's: a strafe's rounds striking the ground, and the scorch under a
        /// beam's head. True when this drew it and the shell's own burst must not.</summary>
        bool LightBurst(SimEvent e, Vector3 p, float now)
        {
            bool strafe = e.A == (int)OffMapAbilityId.StrafeRun, scorch = e.Dir.y == (float)BlastShape.Beam;
            if (!strafe && !scorch) return false;
            if (books == null || !books.Ready) return true;
            if (strafe)
            {
                // the rounds come from the aircraft: two tracers from its guns to the burst, and the dust they kick up
                if (TryPlane(now, out var at, out var rot))
                    for (int g = -1; g <= 1; g += 2)
                        tracers.Add(new Tracer { From = at + rot * new Vector3(g * 0.9f, -0.3f, 1.5f), To = p, Born = now, Hit = true, Team = (byte)(e.B & 1) });
                for (int k = 0; k < 3; k++)
                    books.Add(FlipbookFx.Book.Spurt, p + new Vector3(Mathf.Lerp(-1.2f, 1.2f, Hash01(p.x, p.z, 10 + k)), 0.1f, Mathf.Lerp(-1.2f, 1.2f, Hash01(p.x, p.z, 20 + k))), 1.3f, 0.45f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 2.5f, grow: 0.4f);
                books.Add(FlipbookFx.Book.Puff, p + Vector3.up * 0.4f, 2.2f, 0.9f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.9f, grow: 0.6f, alpha: 0.7f);
                return true;
            }
            // the scorch: what the beam's head does to the ground, dark and quick, with sparks
            books.Add(FlipbookFx.Book.Puff, p + Vector3.up * 0.3f, 2.4f, 1.1f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 1.2f, grow: 0.7f, alpha: 0.55f);
            SceneHooks.Sparks?.Invoke(p + Vector3.up * 0.6f, 10);
            return true;
        }

        /// <summary>The smoke screen: the sim's smoke field as pale cards over each thick cell, boiling slowly, the way
        /// the gas is drawn (with the puff's drawing, which is the pale one).</summary>
        void DrawSmokeScreen(GasSmokeSystem gas, float now, Bounds bounds)
        {
            if (gas == null || !gas.SmokeActive || books == null || !books.Ready) return;
            float cs = MapData.FieldCellSize;
            var map = Host.Local.Map;
            smokeCards.Clear();
            for (int z = 0; z < gas.Length; z++)
            for (int x = 0; x < gas.Width; x++)
            {
                float c = gas.Smoke[z * gas.Width + x];
                if (c < 1.5f) continue;
                uint h = (uint)(x * 83492791 ^ z * 29349663);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f, h3 = ((h >> 20) & 1023) / 1023f;
                float thick = Mathf.Clamp01(c / 20f);
                float wx = (x + 0.5f) * cs + (h1 - 0.5f) * 2.2f, wz = (z + 0.5f) * cs + (h2 - 0.5f) * 2.2f;
                float ground = RenderGround.Sample(map, wx, wz);
                float slow = now * 0.18f + h3 * 6.2832f;
                float frame = 1f + 2.4f * (0.5f + 0.5f * Mathf.Sin(slow));
                float width = (4.2f + 2.6f * thick) * (0.9f + 0.2f * h2);
                var kind = FlipbookFx.Kind.Upright | (((h >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                smokeCards.Add(FlipbookFx.Pack(new Vector3(wx, ground + width * 0.30f + 0.25f * Mathf.Sin(slow * 0.6f), wz), width, width, frame, 1f, 1f, (h1 - 0.5f) * 0.5f + now * 0.02f, kind, 0.18f + 0.5f * thick));
            }
            if (smokeCards.Count > 0) books.DrawPacked(FlipbookFx.Book.Puff, smokeCards, bounds);
        }

        /// <summary>What the player is aiming (AbilityAim.Shape): a disc for a point ability; for a line the corridor
        /// from the start along the heading, an arrowhead at its end, a tick at every lift of a stepping pattern.</summary>
        void DrawAim(Bounds bounds)
        {
            var preview = SceneHooks.AimPreview;   // whoever owns the aim (TestPanel today) says what is aimed; nobody: nothing to draw
            if (aimMat == null || preview == null || !preview(out var shape)) return;
            var map = Host.Local.Map;
            var rp = new RenderParams(aimMat) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off };
            batch.Clear();
            if (!shape.Line)
            {
                var at = shape.Start; at.y = RenderGround.Sample(map, at.x, at.z) + 0.2f;
                batch.Add(Matrix4x4.TRS(at, Quaternion.identity, new Vector3(shape.Radius * 2f, 0.05f, shape.Radius * 2f)));
                Flush(sphere, rp);
                return;
            }
            var rot = Quaternion.LookRotation(shape.Dir);
            var mid = shape.Start + shape.Dir * (shape.Length * 0.5f); mid.y = RenderGround.Sample(map, mid.x, mid.z) + 0.25f;
            batch.Add(Matrix4x4.TRS(mid, rot, new Vector3(shape.HalfWidth * 2f, 0.05f, shape.Length)));
            if (shape.StepMetres > 0f)
                for (float s = 0f; s <= shape.Length + 0.01f; s += shape.StepMetres)
                {
                    var tick = shape.Start + shape.Dir * s; tick.y = RenderGround.Sample(map, tick.x, tick.z) + 0.35f;
                    batch.Add(Matrix4x4.TRS(tick, rot, new Vector3(shape.HalfWidth * 2.4f, 0.05f, 0.3f)));
                }
            Flush(cube, rp);
            var end = shape.End; end.y = RenderGround.Sample(map, end.x, end.z) + 0.3f;
            batch.Add(Matrix4x4.TRS(end, Quaternion.identity, Vector3.one * Mathf.Max(1.2f, shape.HalfWidth * 1.2f)));
            var origin = shape.Start; origin.y = RenderGround.Sample(map, origin.x, origin.z) + 0.3f;
            batch.Add(Matrix4x4.TRS(origin, Quaternion.identity, Vector3.one * 0.8f));
            Flush(sphere, rp);
        }
    }
}
