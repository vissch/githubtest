// Phase: A3 (implemented 2026-09-25) — the medic and the repair engineer. Owner's brief: "A healer unit. A repair
// unit."; decision: the medic heals LIVING men only (no downed state).
// Both walk and garrison like riflemen; what they do is a sweep every SupportEvery ticks, staggered by slot the way
// the Banner's standard is (VehicleModulesSystem), in slot order on the main thread because it writes other slots.
//  - A medic (InfantrySpec.HealPerSecond) takes the NEAREST wounded man of his own side within HealRadius (ties to
//    the lower slot) and gives him HealPerSecond of hit points, one patient at a time, never past MaxHp, never a
//    vehicle. UnitHealed each sweep.
//  - An engineer (InfantrySpec.RepairPerSecond) tends every friendly machine within RepairRadius of its hull:
//    the hull's structure comes back at RepairPerSecond (VehicleHullMended), a fire is beaten down at
//    FirePerSecond, and every MendEverySeconds the worst broken module is mended the way the crew would mend it
//    (VehicleModulesSystem.MendWorst: VehicleRepaired) — without the crew's need for RepairQuietTicks of peace,
//    which is the point of him. Crew and legs are not restored.
// State: MendTimer per vehicle slot (hashed), reset when the slot is re-used.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;

namespace TW.Sim.Units
{
    public sealed class SupportSystem : ISimSystem
    {
        public const uint SupportEvery = 5;
        public int Order => SimSystemOrder.Support;

        public NativeArray<int> MendTimer;     // per vehicle slot: ticks of an engineer's attention toward the next mend
        NativeArray<ushort> gen;
        VehicleModulesSystem modules;

        public void Initialize(SimWorld world)
        {
            int n = world.Config.MaxSlots;
            MendTimer = new NativeArray<int>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
            modules = world.GetSystem<VehicleModulesSystem>();
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            float dt = w.Config.TickSeconds * SupportEvery;
            for (int i = 0; i < n; i++)
            {
                if (gen[i] != w.Generation[i]) { gen[i] = w.Generation[i]; MendTimer[i] = 0; }
                if (w.Tick % SupportEvery != (uint)i % SupportEvery) continue;
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                var spec = InfantrySpec.For(w.Archetype[i]);
                if (spec.HealPerSecond > 0f) Heal(w, i, spec, dt);
                if (spec.RepairPerSecond > 0f) Repair(w, i, spec, dt);
            }
        }

        void Heal(SimWorld w, int i, in InfantrySpec spec, float dt)
        {
            int patient = -1; float best = spec.HealRadius * spec.HealRadius;
            byte team = w.Team[i]; float3 at = w.Position[i];
            for (int j = 0; j < w.HighWater; j++)
            {
                if (j == i) continue;   // he does not patch himself
                uint fj = w.Flags[j];
                if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.Vehicle) != 0 || w.Team[j] != team) continue;
                if (w.Hp[j] >= w.MaxHp[j]) continue;
                float3 d = w.Position[j] - at; d.y = 0f;
                float d2 = math.lengthsq(d);
                if (d2 < best || (d2 == best && patient >= 0 && j < patient)) { best = d2; patient = j; }
            }
            if (patient < 0) return;
            float before = w.Hp[patient];
            w.Hp[patient] = math.min(w.MaxHp[patient], before + spec.HealPerSecond * dt);
            w.Events.Add(w.Tick, SimEventType.UnitHealed, i, patient, w.Position[patient], default, w.Hp[patient] - before);
        }

        void Repair(SimWorld w, int i, in InfantrySpec spec, float dt)
        {
            byte team = w.Team[i]; float3 at = w.Position[i];
            for (int v = 0; v < w.HighWater; v++)
            {
                uint fv = w.Flags[v];
                if ((fv & (uint)UnitFlags.Alive) == 0 || (fv & (uint)UnitFlags.Vehicle) == 0 || w.Team[v] != team) continue;
                if ((fv & (uint)UnitFlags.KnockedOut) != 0 || !VehicleArchetype.IsArmoured(w.Archetype[v])) continue;
                float reach = spec.RepairRadius + VehicleProfile.ForArchetype(w.Archetype[v]).HalfLength;
                float3 d = w.Position[v] - at; d.y = 0f;
                if (math.lengthsq(d) > reach * reach) continue;
                float before = w.Hp[v];
                if (before < w.MaxHp[v])
                {
                    w.Hp[v] = math.min(w.MaxHp[v], before + spec.RepairPerSecond * dt);
                    w.Events.Add(w.Tick, SimEventType.VehicleHullMended, v, i, w.Position[v], default, w.Hp[v] - before);
                }
                if (modules != null)
                {
                    if (modules.Fire[v] > 0f) modules.Fire[v] = math.max(0f, modules.Fire[v] - spec.FirePerSecond * dt);
                    MendTimer[v] += (int)SupportEvery;
                    if (MendTimer[v] * w.Config.TickSeconds >= spec.MendEverySeconds) { MendTimer[v] = 0; modules.MendWorst(w, v); }
                }
            }
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(MendTimer, h);
            return SimHash.Array(gen, h);
        }

        public void Dispose()
        {
            if (MendTimer.IsCreated) MendTimer.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
