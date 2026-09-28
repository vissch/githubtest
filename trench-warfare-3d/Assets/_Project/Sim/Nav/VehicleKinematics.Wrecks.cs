// Phase: wrecks (2026-09-28, implemented) — part of VehicleKinematicsSystem — depends on: PropHarm, PropRules, MapData
// What tracks do to wreckage (owner, 2026-09-28: wrecks are worn down by ramming too). A wreck blocks its cell, so the
// flow field routes round it, but a hull is wider than its path:
//  - a heavy machine (PushesTrees) grinds down a wreck or a broken wreck it brushes or pushes against, GrindPerSecond;
//  - any machine flattens scrap it drives over (scrap no longer blocks), FlattenPerSecond.
// Wear goes through PropHarm, as a blast's does: PropWorn (b = 1, wear) while the stage stands, the next stage when its
// hit points run out. It is applied every WearEvery ticks for each machine (staggered by slot), WearEvery ticks' worth at
// once, so there is no new state to hash and no event every tick; VehicleCrushed b = 3 says a machine ground wreckage.
// Ramming costs the machine nothing (owner default, plan 2026-09-28), and no machine seeks a wreck out.
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public sealed partial class VehicleKinematicsSystem
    {
        public const float GrindPerSecond = 75f, FlattenPerSecond = 150f;
        public const int WearEvery = 10;
        /// <summary>How far a wreck reaches from its middle, per unit of its size, for a hull brushing it: most of its
        /// nav cell (a size-1 wreck is about a Tusk, 2 m off its middle).</summary>
        public const float WreckBody = 1f;
        public int WrecksGround;

        /// <summary>Machine i's wear on the wreckage it touches this tick, on its turn (every WearEvery ticks). Main thread,
        /// in vehicle slot order and prop order. True when a nav cell changed (a broken wreck became scrap).</summary>
        bool GrindWrecks(SimWorld w, int i, float3 p, VehicleProfile prof, float speed)
        {
            if ((w.Tick + (uint)i) % WearEvery != 0u) return false;
            bool moving = speed > 0.2f, pushing = Pushing(i);
            if (!moving && !pushing) return false;   // a machine parked against a wreck does not wear it
            float yaw = w.Yaw[i];
            float2 fwd = new float2(SimMath.Sin(yaw), SimMath.Cos(yaw)), right = new float2(fwd.y, -fwd.x);
            float dt = w.Config.TickSeconds * WearEvery;
            bool nav = false;
            for (int k = 0; k < map.Props.Length; k++)
            {
                var prop = map.Props[k];
                if (!PropRules.IsWreckage(prop.Kind) || prop.Hp <= 0f) continue;
                bool scrap = prop.Kind == PropKind.Scrap;
                if (scrap ? !moving : !prof.PushesTrees) continue;
                // scrap is flattened under the hull; a standing wreck is ground where the hull meets its body
                float body = scrap ? 0.3f : 0.3f + WreckBody * (prop.Scale > 0f ? prop.Scale : 1f);
                float2 d = prop.Pos.xz - p.xz;
                float reach = prof.HalfLength + prof.HalfWidth + body;
                if (math.abs(d.x) > reach || math.abs(d.y) > reach) continue;
                if (math.abs(math.dot(d, fwd)) >= prof.HalfLength + body || math.abs(math.dot(d, right)) >= prof.HalfWidth + body) continue;
                float damage = (scrap ? FlattenPerSecond : GrindPerSecond) * dt;
                var harm = PropHarm.Harm(w, map, k, damage, new float3(math.normalizesafe(d).x, 0f, math.normalizesafe(d).y), 1, ref checksum, out bool changed);
                if (harm == PropHarm.Outcome.None) continue;
                WrecksGround++;
                nav |= changed;
                checksum = SimHash.Value(new int2(i, k), checksum);
                w.Events.Add(w.Tick, SimEventType.VehicleCrushed, i, 3, prop.Pos);
            }
            return nav;
        }

        /// <summary>Machine i was stopped by a cell it could not enter this tick: it is pushing against whatever is there.</summary>
        bool Pushing(int i)
        {
            for (int b = 0; b < blocked.Length; b++) if (blocked[b].x == i) return true;
            return false;
        }
    }
}
