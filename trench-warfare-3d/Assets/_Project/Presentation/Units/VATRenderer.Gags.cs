// Phase: deaths (2026-09-28, implemented) — part of VATRenderer: a body with a death gag (DeathGags). AddFallen plans his
// whole path once (FallenFlight: the hold, the throw, the bounces, the skid, where he comes to rest, all on the drawn
// ground) and the draw reads it back every frame; his roll, squash and feet pivot ride in the instance's Tint (VatTint).
// A heap stacks the gagged by when they come DOWN, not by when they died: a man whose short flight ends first lies
// under one still in the air, instead of the late one hovering over a gap. A body with no gag never comes here.
using UnityEngine;
using TW.Sim;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Presentation.Units
{
    public sealed partial class VATRenderer
    {
        struct DrawnGround : IGroundHeight
        {
            public MapData Map;
            public float At(float x, float z) => Map != null ? RenderGround.Sample(Map, x, z) : 0f;
        }

        /// <summary>
        /// A man died with a gag: as AddFallen, and his path is planned from the gag (path is what was planned: CombatFx
        /// times its dust and blood by it). A gag that leaves no corpse (the beam's boots) lays nothing down. No gag, or no
        /// match: exactly the plain AddFallen.
        /// </summary>
        public void AddFallen(Vector3 pos, float yaw, int team, int variant, Clip clip, int archetype, Clip fromClip, float fromPhase, float fade, Vector3 fly, int gib, float grime, int density, int chr, in GagPlan gag, out FallenFlight.Plan path)
        {
            path = default;
            if (!gag.Any || Host == null || Host.Local == null)
            {
                AddFallen(pos, yaw, team, variant, clip, archetype, fromClip, fromPhase, fade, fly, gib, grime, density, chr);
                return;
            }
            var map = Host.Local.Map;
            var ground = new DrawnGround { Map = map };
            // the burning man's skid goes the way he ran; every other skid carries on the way he was thrown
            Vector3 skidDir = gag.Gag == DeathGag.Skid ? new Vector3(Mathf.Sin(gag.SkidYaw), 0f, Mathf.Cos(gag.SkidYaw)) : Vector3.zero;
            var size = map.SizeMeters;
            path = FallenFlight.Make(pos, fly, gag.Flips, gag.Rolls, gag.Bounces, gag.Skid, skidDir, gag.Delay, gag.HoldLift, gag.Jig, ThrowGravity, new Vector2(size.x, size.y), map.WaterLevel, ref ground);
            path.Topple = gag.Topple; path.ToppleDur = gag.ToppleDur;
            path.PulseQ = gag.PulseQ; path.PulseDur = gag.PulseDur; path.RestQ = gag.RestQ;
            path.RestRoll = gag.RestRoll; path.TwitchRoll = gag.TwitchRoll; path.TwitchHz = gag.TwitchHz; path.TwitchDur = gag.TwitchDur;
            if (gag.PulseDur > 0f && path.Arcs == 0 && path.SkidDur <= 0f) path.Settled = Mathf.Max(path.Settled, gag.PulseDur);
            if (gag.Topple > 0) path.Settled = Mathf.Max(path.Settled, gag.ToppleDur);
            if (gag.TwitchDur > 0f) path.Settled = Mathf.Max(path.Settled, gag.TwitchDur + FallenFlight.RollSettle);
            if ((gag.Flags & GagFlags.NoCorpse) != 0) return;

            if (fallenMen.Count >= MaxFallen) Remove(SoonestGone());
            var man = NewFallen(pos, yaw, team, variant, clip, archetype, fromClip, fromPhase, fade, gib, grime, chr, out bool near);
            man.Pos = path.Rest; man.Gag = (byte)gag.Gag; man.GagFlags = gag.Flags; man.Absurd = gag.Intensity; man.Path = path; man.Flight = path.ArcsEnd;
            // he lands his death clip on his back as his first arc comes down (as the plain throw does)
            if (near && clip == Clip.DeathThrown && path.Arcs > 0) man.Rate = Mathf.Clamp(AnimationController.ThrownLands / Mathf.Max(0.05f, path.A0.Dur), 0.8f, 1.8f);
            if (gag.ClipRate > 0f) man.Rate = gag.ClipRate;
            if (path.Arcs > 0) man.Spin = (Hash01(pos) < 0.5f ? -1f : 1f) * Mathf.Lerp(1.1f, 2.6f, Hash01(pos + new Vector3(7.3f, 0f, 3.1f))) * gag.SpinScale;

            Heap(ref man);
            path = man.Path;
        }

        /// <summary>The yaw a gagged body is drawn at: turning while he flies, twitching while a machine gun jigs him.</summary>
        static float GagYaw(in FallenMan f, float age)
        {
            float yaw = f.Yaw + f.Spin * FallenFlight.Airborne(f.Path, age);
            if (age < f.Path.Delay && f.Path.Jig != 0f) yaw += f.Path.Jig * Mathf.Sin(2f * Mathf.PI * FallenFlight.JigHz * age);
            return yaw;
        }

        /// <summary>A gagged body's Tint: his turns, his squash and whether it all happens about his feet (VatTint).</summary>
        static float GagTint(in FallenMan f, int pitch, float age)
        {
            int roll = FallenFlight.RollStep(f.Path, age);
            int q = FallenFlight.Squash(f.Path, age, f.Absurd);
            bool feet = (f.GagFlags & GagFlags.Feet) != 0 || (q != 0 && age >= f.Path.Delay && FallenFlight.ArcAt(f.Path, age) < 0);
            return VatTint.Pack(f.Team, pitch, roll, q, feet, VatTint.FallenWound);
        }
    }
}
