// Phase: tooling (VFX pass, 2026-09-28: "have everything customized, add the extra effort zoom and quality level") - the
// bench's lineup scenario: every class's shot, every machine's gun and every kind of burst, side by side in the open at
// the held view, at a known moment, so the per-class looks can be judged up close and A/B'd. In the stress battle the
// muzzles are buried in a crowd and a Kettle is never in view when it fires (cls1, cls2).
// Staging: one man of every armed class a side, in two rows facing each other across the view's focus, a bomber beside
// the Tusk, and the Tusk, Pavise, Kettle and Salvo behind player 0's row, all spawned through SimHost.WriteWorlds (the
// tooling write, as TankCapture spawns). The FIRE is presentation only: around shot_tick the lineup queues Shot, Hit,
// VehicleFired and Explosion events into the host's EventPump frame, which every renderer hears exactly as it hears the
// sim's, and the sim never does (its hash moves only by the spawns and by what the spawned men do on their own).
using System.Collections.Generic;
using System.Globalization;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Combat;

namespace TW.Perf
{
    public sealed class BenchLineup
    {
        /// <summary>The classes, in row order, and the machines behind player 0's row.</summary>
        public static readonly byte[] Men =
        {
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
            InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Jetpack,
        };
        public static readonly byte[] Machines = { VehicleArchetype.Tusk, VehicleArchetype.Pavise, VehicleArchetype.Kettle, VehicleArchetype.Salvo };
        /// <summary>The bursts in a row beyond the lineup: a shell (the HE barrage), a mine, a tripwire (Explosion.a sources).</summary>
        public static readonly int[] Bursts = { (int)TW.Sim.Match.OffMapAbilityId.HeBarrage, MineSystem.SourceBase + (int)MineKind.Mine, MineSystem.SourceBase + (int)MineKind.Tripwire };
        public const float Gap = 16f, Spacing = 2.5f, MachinesBack = 10f, MachineSpacing = 7f, BurstRow = 16f, TargetsBeyond = 16f;
        /// <summary>Knob: what the lineup fires. 1 the small arms only (a still of the men), 2 the guns and the bursts only,
        /// anything else both.</summary>
        public const string FireKnob = "bench.lineupFire";

        /// <summary>Where man `i` of `team`'s row stands (x, z): the rows run along z, Gap apart across x, player 0 to the west.</summary>
        public static Vector2 Spot(Vector2 focus, byte team, int i) =>
            focus + new Vector2(team == 0 ? -Gap * 0.5f : Gap * 0.5f, (i - (Men.Length - 1) * 0.5f) * Spacing);

        public static Vector2 MachineSpot(Vector2 focus, int i) =>
            focus + new Vector2(-Gap * 0.5f - MachinesBack, (i - (Machines.Length - 1) * 0.5f) * MachineSpacing);

        public static Vector2 BurstSpot(Vector2 focus, int i) => focus + new Vector2(Gap * 0.5f + TargetsBeyond, (i - (Bursts.Length - 1) * 0.5f) * 10f + BurstRow);

        /// <summary>Where machine `i`'s round lands: well past player 1's row, so its burst does not hide the men.</summary>
        public static Vector2 TargetSpot(Vector2 focus, int i) => focus + new Vector2(Gap * 0.5f + TargetsBeyond, (i - (Machines.Length - 1) * 0.5f) * 10f - BurstRow);

        /// <summary>The window tick (after t0) each thing goes off, about shot_tick `s`: the small arms every tick from s-3 to
        /// s+5; the guns at s-12 and their bursts and the row's at s-11, so the trails, the smoke and the columns have grown by
        /// the still (at s-4 the cards were a frame old and their smoke not yet out: lin7).</summary>
        public static bool ArmsAt(int t, int s) => t >= s - 3 && t <= s + 5;
        public static bool GunsAt(int t, int s) => t == s - 12;
        public static bool BurstsAt(int t, int s) => t == s - 11;

        readonly int[] row0 = new int[Men.Length], row1 = new int[Men.Length], machines = new int[Machines.Length];
        int bomber = -1;
        Vector2 focus;
        int lastT = -1;
        int fire;

        /// <summary>Spawn the lineup at `view` (the held focus) and log it; null if the worlds would not take the write.</summary>
        public static BenchLineup Stage(SimHost host, Vector2 view, BenchScenarios.Log log)
        {
            if (host == null || host.Local == null) { log.Warnings.Add("lineup: no match to stage it on"); return null; }
            var show = new BenchLineup { focus = view, fire = Mathf.RoundToInt(Knobs.Get(FireKnob, 0f)) };
            var inv = CultureInfo.InvariantCulture;
            bool wrote = host.WriteWorlds(m =>
            {
                var w = m.World; bool local = m == host.Local;
                int Put(byte team, byte a, Vector2 at, bool vehicle, float yaw)
                {
                    var e = w.Units.Roster[a];
                    int slot = w.Spawn(team, a, new float3(at.x, 0f, at.y), e.Hp, e.Speed, vehicle);
                    if (slot >= 0) w.Yaw[slot] = yaw;
                    if (local) log.Writes.Add(string.Format(inv, "spawn p{0} archetype {1} at ({2:0.0}, {3:0.0}): slot {4}", team, a, at.x, at.y, slot));
                    return slot;
                }
                for (int i = 0; i < Men.Length; i++)
                {
                    int a = Put(0, Men[i], Spot(view, 0, i), false, Mathf.PI * 0.5f), b = Put(1, Men[i], Spot(view, 1, i), false, -Mathf.PI * 0.5f);
                    if (local) { show.row0[i] = a; show.row1[i] = b; }
                }
                for (int i = 0; i < Machines.Length; i++)
                {
                    int s = Put(0, Machines[i], MachineSpot(view, i), true, Mathf.PI * 0.5f);
                    if (local) show.machines[i] = s;
                }
                int k = Put(1, InfantryArchetype.Rifle, MachineSpot(view, 0) + new Vector2(6f, 0f), false, -Mathf.PI * 0.5f);   // beside the Tusk: its close assault
                if (local) show.bomber = k;
            });
            if (!wrote) { log.Warnings.Add("lineup: SimHost.WriteWorlds refused (the canary is waiting on the network): nobody spawned"); return null; }
            log.Presentation.Add("lineup: Shot/Hit every tick from shot_tick-3 to +5, VehicleFired at -4, Explosions at -3 (EventPump, presentation only)");
            return show;
        }

        /// <summary>Queue this window tick's staged events (once per tick). `t` is the window tick, `s` shot_tick.</summary>
        public void Tick(SimHost host, int t, int s)
        {
            if (t == lastT || host == null || host.Local == null) return;
            lastT = t;
            var w = host.Local.World; var frame = host.Events.Frame;
            uint tick = w.Tick;
            bool arms = fire != 2, guns = fire != 1;
            if (arms && ArmsAt(t, s))
                for (int i = 0; i < Men.Length; i++)
                {
                    Fire(frame, w, tick, row0[i], row1[i], i);
                    Fire(frame, w, tick, row1[i], row0[i], i + 7);
                }
            if (arms && t == s - 3 && Alive(w, bomber) && Alive(w, machines[0]))   // the bundle onto the Tusk
                frame.Add(new SimEvent { Tick = tick, Type = SimEventType.Shot, A = bomber, B = machines[0], Pos = w.Position[bomber], Dir = new float3(-1f, 0f, 0f), Scalar = 1f });
            if (guns && GunsAt(t, s))
                for (int i = 0; i < Machines.Length; i++)
                {
                    if (!Alive(w, machines[i])) continue;
                    var to = TargetSpot(focus, i);
                    float3 from = w.Position[machines[i]], land = new float3(to.x, 0f, to.y), d = land - from; d.y = 0f;
                    frame.Add(new SimEvent { Tick = tick, Type = SimEventType.VehicleFired, A = machines[i], B = 0, Pos = land, Dir = math.normalizesafe(d), Scalar = 1f });
                }
            if (guns && BurstsAt(t, s))
            {
                for (int i = 0; i < Machines.Length; i++)
                {
                    if (!Alive(w, machines[i])) continue;
                    var to = TargetSpot(focus, i);
                    bool indirect = Machines[i] == VehicleArchetype.Kettle || Machines[i] == VehicleArchetype.Salvo;
                    frame.Add(new SimEvent { Tick = tick, Type = SimEventType.Explosion, A = SourceId.Unit(Machines[i]), B = 0, Pos = new float3(to.x, 0f, to.y),
                                             Dir = indirect ? float3.zero : new float3(1f, 0f, 0f), Scalar = Machines[i] == VehicleArchetype.Tusk ? 2.5f : 4f });
                }
                for (int i = 0; i < Bursts.Length; i++)
                {
                    var at = BurstSpot(focus, i);
                    bool mine = Bursts[i] >= MineSystem.SourceBase;
                    frame.Add(new SimEvent { Tick = tick, Type = SimEventType.Explosion, A = Bursts[i], B = 0, Pos = new float3(at.x, 0f, at.y),
                                             Dir = new float3(0f, mine ? (float)BlastShape.Mine : 0f, 0f), Scalar = mine ? 4f : 5f });
                }
            }
        }

        static bool Alive(SimWorld w, int slot) => slot >= 0 && slot < w.HighWater && (w.Flags[slot] & (uint)UnitFlags.Alive) != 0;

        /// <summary>One round from `a` at `b`, and every other tick it strikes him (a sniper's harder).</summary>
        static void Fire(List<SimEvent> frame, SimWorld w, uint tick, int a, int b, int salt)
        {
            if (!Alive(w, a) || b < 0 || b >= w.HighWater) return;   // a fallen target is still shot at where he lies
            float3 d = w.Position[b] - w.Position[a]; d.y = 0f; d = math.normalizesafe(d);
            frame.Add(new SimEvent { Tick = tick, Type = SimEventType.Shot, A = a, B = b, Pos = w.Position[a], Dir = d, Scalar = 0f });
            if (Alive(w, b) && ((tick + (uint)salt) & 1u) == 0u)
                frame.Add(new SimEvent { Tick = tick, Type = SimEventType.Hit, A = a, B = b, Pos = w.Position[b], Dir = d, Scalar = w.Archetype[a] == InfantryArchetype.Sniper ? 95f : 25f });
        }
    }
}
