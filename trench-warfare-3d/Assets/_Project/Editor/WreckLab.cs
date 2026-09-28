// Phase: wrecks (2026-09-28, tooling) — depends on: SimHost, BlastSystem, MapData
// Editor-only helpers to look at a wreck breaking in stages (TankRenderer.WreckStages), driven from the command line (tw
// eval) or from Tests/Stills/WreckStills. A shell queued into every world's BlastSystem bursts inside the next tick, so
// what it kills leaves its wreck prop and what it wears is worn by the sim (a Despawn inside WriteWorlds would lose the
// events the next Step clears).
//  - Shell(x, z, damage, radius): one burst at a point, next tick;
//  - Wreck(x, z): the wreck prop nearest a point, its kind and hit points.
// None of this is part of the game; it exists for the capture-and-critique loop.
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Editor
{
    public static class WreckLab
    {
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>One shell at a point in every world, bursting next tick.</summary>
        public static string Shell(float x, float z, float damage = 500f, float radius = 6f)
        {
            var h = Host; if (h == null) return "no SimHost";
            var impact = new Impact { Pos = new float3(x, 0f, z), Damage = damage, Radius = radius, Suppression = 30f, Source = (int)OffMapAbilityId.HeBarrage, Player = -1 };
            return h.WriteWorlds(m => m.World.GetSystem<BlastSystem>()?.Queue(impact)) ? $"shell {damage:0} at {x:0.0}, {z:0.0} next tick" : "worlds a tick apart: try again";
        }

        /// <summary>The wreck prop nearest a point within 8 m: "prop N Kind hp H", or "none".</summary>
        public static string Wreck(float x, float z)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var props = h.Local.Map.Props;
            int best = -1; float bestD = 64f;
            for (int i = 0; i < props.Length; i++)
            {
                var k = props[i].Kind;
                if (!PropRules.IsWreckage(k) && k != PropKind.Cleared) continue;
                float d = math.distancesq(props[i].Pos.xz, new float2(x, z));
                if (d < bestD) { bestD = d; best = i; }
            }
            return best < 0 ? "none" : $"prop {best} {props[best].Kind} hp {props[best].Hp:0}";
        }
    }
}
