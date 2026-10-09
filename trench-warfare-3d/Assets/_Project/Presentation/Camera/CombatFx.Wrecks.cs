// Phase: wrecks (2026-09-28, implemented) — part of CombatFx: a round fired at a wreck. A gun with nobody to shoot at
// keeps the heads down behind a wreck (DirectFire.Wrecks); its Shot names the prop in b (PropTarget, <= -2). It is drawn
// as any tracer, from the shooter's muzzle to a point on the wreck's side, spread over it by a hash of the tick and the
// shooter (so two peers draw the same, decisions.md: presentation seeded, never UnityEngine.Random).
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>A Shot at a wreck (PropTarget.IsProp(e.B)): its tracer, from the muzzle to the wreck's side.</summary>
        void ShotAtWreck(SimEvent e)
        {
            var w = Host.Local.World; var map = Host.Local.Map;
            int p = PropTarget.Decode(e.B);
            if (tracers.Count >= 1500 || p < 0 || p >= map.Props.Length || e.A < 0 || e.A >= w.HighWater) return;
            var prop = map.Props[p];
            float scale = FigureScale();
            Vector3 from, barrel;
            if (units == null || !units.Sockets(e.A, out from, out barrel, out _)) EstimateMuzzle(e.A, -1, scale, out from, out barrel);
            float size = prop.Scale > 0f ? prop.Scale : 1f;
            Vector3 at = new Vector3(prop.Pos.x, 0f, prop.Pos.z)
                         + new Vector3((Hash01(prop.Pos.x + e.Tick, prop.Pos.z, e.A) - 0.5f) * 2.4f * size, 0f, (Hash01(prop.Pos.z, prop.Pos.x + e.Tick, e.A + 3) - 0.5f) * 2.4f * size);
            at.y = RenderGround.Sample(map, at.x, at.z) + (0.5f + 1.3f * Hash01(prop.Pos.x, prop.Pos.z + e.Tick, e.A + 7)) * size;
            float delay = ShotStagger.Delay(e.A, e.Tick, w.Config.TickSeconds, shotStagger);
            tracers.Add(WreckTracer(from, at, Time.time + delay, w.Team[e.A], TracerSeconds));
        }

        /// <summary>The round at a wreck as a tracer: a usual round (Width and Streak 0 are the usual round's), on screen
        /// for the usual time. Built without its Life it was pruned on its first frame (TracerSpent) and never drawn.</summary>
        static Tracer WreckTracer(Vector3 from, Vector3 at, float born, byte team, float seconds)
            => new Tracer { From = from, To = at, Born = born, Team = team, Life = seconds };

        /// <summary>Whether the round at a wreck is still drawn `since` seconds after it is born: the tracer ShotAtWreck
        /// builds, held to the prune's own rule. Pure, for the test.</summary>
        public static bool WreckTracerDrawn(float seconds, float since)
        {
            var t = WreckTracer(Vector3.zero, Vector3.forward, 0f, 0, seconds);
            return !TracerSpent(t.Born + since, t.Born, t.Life);
        }
    }
}
