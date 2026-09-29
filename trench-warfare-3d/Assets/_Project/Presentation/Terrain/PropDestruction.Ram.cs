// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: PropDestruction (rules, Finish, Chip),
// VehicleProfile (PushesTrees, Covers, Reach), Knobs, SceneHooks.LampOut (NightLights.Machines.cs).
// A heavy machine drives through what stands in its way (props.ram, off by default: today a Maw breaks nothing but the
// light things Crush flattens within 2.4 m of its centre, about a quarter of its hull). Once a sim tick, from the tick
// state, up to MaxRammers moving machines that push trees over (VehicleProfile.PushesTrees) wear what their footprint
// and RamMargin round it cover, at RamWear a second times the machine's weight (its footprint against the Maw's) and
// speed, until Finish ends it as its kind ends: a house's ground-storey chunk is thrown ahead of the hull and what it
// carried comes down after it, a storey at a time (Shaken, Settle); stone, iron, sandbag walls and gabions collapse where
// they stood. Never a shelter, the trench lining, a boulder, a stump, the light things Crush flattens, what a man drops,
// or what the sim owns (trees, wrecks, the bridge: they have no rule here).
// And a lantern post that goes, by any cause, puts its lamp out (SceneHooks.LampOut): before, its light stayed lit over
// the flattened post. Presentation only; seeded from the tick and the slot, so a replay breaks the same walls.
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Presentation.Terrain
{
    public sealed partial class PropDestruction
    {
        public const int MaxRammers = 6;
        /// <summary>Metres round the footprint a machine reaches; hp a second at the Maw's weight and 1 m/s; the speed below
        /// which it only leans on a wall; how far from a lantern post its lamp is looked for.</summary>
        public const float RamMargin = 0.5f, RamWear = 4f, RamMinSpeed = 0.5f, LampOutReach = 0.9f;
        readonly List<BattlefieldKit.Module> ramable = new List<BattlefieldKit.Module>(128);
        bool ramOn; int ramKnobs = -1;
        /// <summary>Props a machine has driven through, for the capture tools.</summary>
        public int Rammed { get; private set; }

        /// <summary>The props a machine drives through: every rule but the shelters, the lining, what Crush flattens and what
        /// a man drops; less the boulder and the stumps (a tank rolls over them, not through), and the upper storeys of a
        /// house, which fall when what carries them has gone.</summary>
        void BuildRamable()
        {
            ramable.Clear();
            var kit = props.Kit;
            foreach (var kv in rules)
            {
                var module = kv.Key; var rule = kv.Value;
                if (rule.Shelter || rule.Crush || rule.Kick || IsLining(rule)) continue;
                if (module == kit.boulder || module == kit.stumpTall || module == kit.stumpSplit || module == kit.stumpMoss || module == kit.fork) continue;
                if (kit.HouseChunkOf.TryGetValue(module, out var chunk) && !chunk.Grounded) continue;
                ramable.Add(module);
            }
        }

        /// <summary>The heavy machines on the move, through what their footprint covers. Read from the tick state.</summary>
        void Ram(SimWorld w)
        {
            if (ramKnobs != Knobs.Generation) { ramKnobs = Knobs.Generation; ramOn = Knobs.Get("props.ram", false); }
            if (!ramOn || rules == null || props.Kit == null) return;
            var kin = Host.Local.Vehicles;
            if (kin == null || !kin.Profiles.IsCreated) return;
            if (ramable.Count == 0) BuildRamable();
            if (ramable.Count == 0) return;
            var debris = DebrisRenderer.Instance;
            var maw = VehicleProfile.Maw;
            float mawArea = maw.HalfLength * maw.HalfWidth, tick = w.Config.TickSeconds;
            uint vehicle = (uint)UnitFlags.Vehicle | (uint)UnitFlags.Alive;
            int rammers = 0;
            for (int v = 0; v < w.HighWater && rammers < MaxRammers; v++)
            {
                if ((w.Flags[v] & vehicle) != vehicle) continue;
                var prof = kin.Profiles[w.Archetype[v]];
                if (!prof.PushesTrees) continue;
                var vel = w.Velocity[v];
                float speed = math.sqrt(vel.x * vel.x + vel.z * vel.z);
                if (speed < RamMinSpeed) continue;
                rammers++;
                var p = w.Position[v]; float yaw = w.Yaw[v];
                var centre = new Vector2(p.x, p.z);
                Vector3 heading = new Vector3(vel.x, 0f, vel.z) / speed;
                float weight = Mathf.Clamp(prof.HalfLength * prof.HalfWidth / mawArea, 0.5f, 2f);
                float wear = RamWear * weight * speed * tick;
                bool broke = false;
                for (int k = 0; k < ramable.Count; k++)
                {
                    var module = ramable[k]; var rule = rules[module];
                    found.Clear();
                    props.Within(module, centre, prof.Reach + RamMargin + 3f, found);   // 3 m: a prop's origin may sit off its middle
                    for (int i = 0; i < found.Count; i++)
                    {
                        var (page, slot, drawn) = found[i];
                        var m = Home(module, page, slot, drawn);
                        Measure(module, m, out var mid, out var size, out _, out _);
                        float margin = RamMargin + 0.5f * Mathf.Min(size.x, size.z);   // its near face, not its middle, meets the hull
                        if (!prof.Covers(yaw, p, new float3(mid.x, p.y, mid.z), margin)) continue;
                        long key = KeyOf(module, rule, m);
                        if (destroyed.Contains(key)) continue;
                        uint salt = w.Tick * 47u + (uint)(v * 17 + i);
                        float hp = damage.TryGetValue(key, out float left) ? left : rule.Hp;
                        bool first = hp >= rule.Hp;
                        hp -= wear;
                        Vector3 origin = new Vector3(mid.x, GroundAt(mid.x, mid.z), mid.z) - heading * 2f;   // behind it: it goes on ahead of the hull
                        if (hp > 0f) { damage[key] = hp; if (first) Chip(module, rule, m, origin, wear, debris, salt); continue; }
                        Finish(module, rule, page, slot, key, m, origin, 0.5f + 0.25f * speed, rule.Hp, debris, salt, true);
                        Rammed++;
                        broke = true;
                    }
                }
                if (broke) CameraShake.Add(new Vector3(p.x, p.y, p.z), 2f * weight);
            }
        }

        /// <summary>A lantern post that has gone takes its lamp with it: NightLights puts out the light hung over it and its glow.</summary>
        void LampOut(BattlefieldKit.Module module, in Matrix4x4 m)
        {
            if (props.Kit == null || module != props.Kit.lantern) return;
            SceneHooks.LampOut?.Invoke(m.GetPosition(), LampOutReach);
        }
    }
}
