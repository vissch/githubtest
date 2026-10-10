// Phase: deaths (2026-09-28, implemented) — part of VATRenderer: a body with a death gag (DeathGags). AddFallen plans his
// whole path once (FallenFlight: the hold, the throw, the bounces, the skid, where he comes to rest, all on the drawn
// ground) and the draw reads it back every frame; his roll, squash and feet pivot ride in the instance's Tint (VatTint).
// A balloon's hops are their own plan (MakeHops) and he is drawn smaller on each (FallenFlight.SizeAt).
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
            if (gag.Hops > 0)
            {
                // a balloon whizzes off in hops of its own, shrinking on each (DeathGags): no bounce, no skid
                path = FallenFlight.MakeHops(pos, fly, gag.Hops, gag.HopTurn, gag.Delay, ThrowGravity, new Vector2(size.x, size.y), map.WaterLevel, ref ground);
                path.Size0 = DeathGags.BalloonSize1; path.Size1 = DeathGags.BalloonSize2; path.Size2 = DeathGags.BalloonSize3; path.SizeRest = DeathGags.BalloonSize3;
            }
            else path = FallenFlight.Make(pos, fly, gag.Flips, gag.Rolls, gag.Bounces, gag.Skid, skidDir, gag.Delay, gag.HoldLift, gag.Jig, ThrowGravity, new Vector2(size.x, size.y), map.WaterLevel, ref ground);
            path.Topple = gag.Topple; path.ToppleDur = gag.ToppleDur;
            path.PulseQ = gag.PulseQ; path.PulseDur = gag.PulseDur; path.RestQ = gag.RestQ;
            if (gag.PulseDur > 0f && path.Arcs == 0 && path.SkidDur <= 0f) path.Settled = Mathf.Max(path.Settled, gag.PulseDur);
            if (gag.Topple > 0) path.Settled = Mathf.Max(path.Settled, gag.ToppleDur);
            if ((gag.Flags & GagFlags.NoCorpse) != 0) return;

            if (fallenMen.Count >= MaxFallen) Remove(SoonestGone());
            ushort farRow = (ushort)((int)AnimRow.Death0 + (variant & 3));
            int figure = figures != null ? Mathf.Clamp(FigureOfArchetype(archetype), 0, figures.Length - 1) : 0;
            bool near = clipAtlas && clip != Clip.None;
            float seconds = near ? figures[figure].Asset.RowSeconds[(int)clip] : FallSeconds;
            bool blend = near && fromClip != Clip.None && fade > 0.01f;
            float lies = chr >= 2 ? CharredLies * (0.85f + 0.3f * Hash01(pos)) : FallenSeconds * (0.7f + 0.6f * Hash01(pos));
            var man = new FallenMan
            {
                Pos = path.Rest, From = pos, Yaw = yaw, Born = Time.time, Seconds = Mathf.Max(0.1f, seconds), Lies = lies, Team = (byte)team, Figure = (byte)figure,
                Gib = (byte)(gib & 0xFF), Grime = grime, Char = (byte)Mathf.Clamp(chr, 0, 3), Row = near ? (ushort)clip : farRow, FarRow = farRow,
                FromRow = blend ? (ushort)fromClip : (ushort)0, FromT = fromPhase, Fade = blend ? fade : 0f, Rate = 1f, Cell = -1,
                Gag = (byte)gag.Gag, GagFlags = gag.Flags, Absurd = gag.Intensity, Path = path, Flight = path.ArcsEnd,
            };
            // he lands his death clip on his back as his first arc comes down (as the plain throw does)
            if (near && clip == Clip.DeathThrown && path.Arcs > 0) man.Rate = Mathf.Clamp(AnimationController.ThrownLands / Mathf.Max(0.05f, path.A0.Dur), 0.8f, 1.8f);
            if (gag.ClipRate > 0f) man.Rate = gag.ClipRate;
            if (path.Arcs > 0) man.Spin = (Hash01(pos) < 0.5f ? -1f : 1f) * Mathf.Lerp(1.1f, 2.6f, Hash01(pos + new Vector3(7.3f, 0f, 3.1f))) * gag.SpinScale;

            // the heap, by who comes down first
            int cell = PileCell(man.Pos);
            float arrive = man.Born + path.Arrive;
            int under = 0;
            for (int k = 0; k < fallenMen.Count; k++) if (fallenMen[k].Cell == cell && ArriveOf(fallenMen[k]) <= arrive) under++;
            if (under > 0)
            {
                float h = Hash01(man.Pos + new Vector3(1.7f, 0f, 9.2f));
                float a = h * 6.2831853f;
                var rest = man.Pos + new Vector3(Mathf.Sin(a), 0f, Mathf.Cos(a)) * PileNudge;
                rest.y += PileStep * UnitScale * Mathf.Min(under, PileMax);
                FallenFlight.EndAt(ref man.Path, rest);
                man.Pos = rest; man.Pitch = (sbyte)(h < 0.5f ? -1 : 1);
                for (int k = 0; k < fallenMen.Count; k++)
                {
                    var below = fallenMen[k];
                    if (below.Cell != cell || ArriveOf(below) > arrive) continue;
                    below.Lies = LiesUnder(below.Born, below.Lies > 0.01f ? below.Lies : FallenSeconds, man.Born, man.Lies);
                    fallenMen[k] = below;
                }
            }
            pile[cell] = (byte)Mathf.Min(255, pile[cell] + 1);
            man.Cell = cell;
            fallenMen.Add(man);
            path = man.Path;
        }

        /// <summary>When a body comes to rest: the plain ones at the end of their one arc, the gagged at the end of their path.</summary>
        float ArriveOf(in FallenMan f) => f.Born + (f.Gag != 0 ? f.Path.Arrive : f.Flight);

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
