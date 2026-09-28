// Phase: wrecks (2026-09-28, implemented) — depends on: MapData, PropRules, SimEvents.PropWorn/PropChanged
// Harm to one prop, the same whoever did it: a blast (DeformationSystem.Shake), a machine grinding a wreck or running
// over scrap (VehicleKinematicsSystem), rounds a wreck's cover absorbed. Its hit points go down; while any are left it
// stands at its stage (a wreck says so with PropWorn), and at none it goes one stage on (PropRules.Next, PropChanged)
// with the next stage's hit points, however hard the hit: nothing skips a stage. Main thread (it changes the map).
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public static class PropHarm
    {
        public enum Outcome : byte { None, Worn, Changed }

        /// <summary>Takes <paramref name="damage"/> off prop <paramref name="p"/>. A wreck that still stands raises PropWorn
        /// (b = <paramref name="cause"/>: 0 a blast, 1 wear; dir = <paramref name="way"/>); a prop at the end of its hit
        /// points goes one stage on, mixed into <paramref name="checksum"/>, with PropChanged. <paramref name="nav"/> is
        /// true when that changed a nav cell. A prop with no hit points (a stump, a log, a bridge, a cleared wreck) is
        /// left alone.</summary>
        public static Outcome Harm(SimWorld w, MapData map, int p, float damage, float3 way, int cause, ref ulong checksum, out bool nav)
        {
            nav = false;
            var prop = map.Props[p];
            if (prop.Hp <= 0f) return Outcome.None;
            prop.Hp -= damage;
            if (prop.Hp > 0f)
            {
                map.Props[p] = prop; map.Touch();
                if (PropRules.IsWreckage(prop.Kind))
                    w.Events.Add(w.Tick, SimEventType.PropWorn, p, cause, prop.Pos, way, prop.Hp / PropRules.StartHp(prop.Kind, prop.Scale));
                return Outcome.Worn;
            }
            var next = PropRules.Next(prop.Kind);   // one stage a hit: a wreck goes wreck, broken, scrap, gone
            nav = map.SetPropKind(p, next);
            checksum = SimHash.Value(new int2(p, (int)next), checksum);
            w.Events.Add(w.Tick, SimEventType.PropChanged, p, (int)next, prop.Pos);
            return Outcome.Changed;
        }
    }
}
