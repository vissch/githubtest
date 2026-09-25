// Phase: tooling (AOSA, 2026-09-25) - what the bench's measured window stages on top of the stress battle
// (docs/reference/aosa/README.md "Scenarios"). PerfBench calls Issue once, the moment hash_start has been taken and
// before the paused match resumes, so the battle before the window is untouched and hash_end then fixes what the
// scenario did. Everything that touches the sim goes through the paths the game already uses:
//   orders    SimHost.Issue (player 0, the HUD's path) and SimHost.IssuePeer (player 1, the enemy seat's path), so
//             each lands InputDelay ticks later on every world, exactly like a click or the scripted enemy's order;
//   top-ups   there is no command that grants silver or clears a cooldown, so the scenario's own cost is added to
//             each side's silver, and the cooldowns of the abilities and slots it uses are cleared, through
//             SimHost.WriteWorlds: the sanctioned tooling write (TankCapture's), one world or both in the canary.
// Targets are fixed by the map and the held view, never by Random or the clock: a barrage falls on the cell of the
// opposing front trench nearest the view's focus, and the vfx stack lands on the focus itself, so it is on screen.
//   barrage  each side: HE barrage on the other side's front trench
//   armour   each side: every vehicle slot in its roster (tank and walkers)
//   vfx      each side: HE barrage and chlorine gas round the view's focus; plus a star shell (presentation only)
// Anything that does not exist in this build is skipped with a warning in the report, never an exception.
using System;
using System.Collections.Generic;
using System.Globalization;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Perf
{
    public static class BenchScenarios
    {
        /// <summary>What a scenario did, for the report: the sim commands issued, the tooling writes, the
        /// presentation-only triggers, and what it could not do.</summary>
        public sealed class Log
        {
            public readonly List<string> Commands = new List<string>();
            public readonly List<string> Writes = new List<string>();
            public readonly List<string> Presentation = new List<string>();
            public readonly List<string> Warnings = new List<string>();
        }

        struct Order { public byte Player; public SimCommand Command; public string Text; }

        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        const int Players = 2;

        /// <summary>Stage `scenario` on the host's match. `view` is the held camera's focus (x, z). Call it while the
        /// match is paused on the window's first tick, after hash_start.</summary>
        public static void Issue(BenchScenario scenario, SimHost host, Vector2 view, Log log)
        {
            if (scenario == BenchScenario.None || log == null) return;
            if (host == null || host.Local == null) { log.Warnings.Add("scenario " + BenchOptions.ScenarioName(scenario) + ": no match to stage it on"); return; }
            var m = host.Local;
            var orders = new List<Order>();
            var abilitiesUsed = new List<int>();   // player * AbilitySlots + ability id
            var slotsUsed = new List<int>();       // player * SlotCount + slot
            int[] cost = new int[Players];

            switch (scenario)
            {
                case BenchScenario.Barrage:
                    for (byte p = 0; p < Players; p++)
                    {
                        byte foe = (byte)(1 - p);
                        short trench = FrontTrench(m, foe);
                        if (trench < 0 || !TrenchCellNear(m, trench, view, out var at)) { log.Warnings.Add($"barrage: player {foe} has no front trench, so player {p} fires nothing"); continue; }
                        Support(m, orders, abilitiesUsed, cost, log, p, OffMapAbilityId.HeBarrage, at, $"player {foe}'s front trench {trench}, the cell nearest the view");
                    }
                    break;

                case BenchScenario.Armour:
                    for (byte p = 0; p < Players; p++)
                    {
                        int fielded = 0;
                        for (int s = 0; s < RosterEntry.SlotCount; s++)
                        {
                            int ri = p * RosterEntry.SlotCount + s;
                            var e = m.World.Roster[ri];
                            if (!e.IsVehicle) continue;
                            if (m.World.SlotUnlocked[ri] == 0) { log.Warnings.Add($"armour: player {p}'s slot {s} ({VehicleName(e.Archetype)}) is locked on this map: not deployed"); continue; }
                            cost[p] += e.Cost;
                            slotsUsed.Add(ri);
                            string kind = VehicleArchetype.IsTank(e.Archetype) ? "tank" : VehicleArchetype.IsWalker(e.Archetype) ? "walker" : "vehicle";
                            orders.Add(new Order
                            {
                                Player = p, Command = SimCommand.Deploy(0, p, s),
                                Text = $"p{p} DeployUnit slot {s}: {VehicleName(e.Archetype)} ({kind}, archetype {e.Archetype}, {e.Cost} silver)",
                            });
                            fielded++;
                        }
                        if (fielded == 0) log.Warnings.Add($"armour: player {p} has no vehicle slot in its roster");
                    }
                    break;

                case BenchScenario.Vfx:
                {
                    // both barrages and both clouds on the held view: a barrage scatters 25 m round its point and a
                    // cloud drifts downwind, so the gas is let go upwind of the focus and the two of each side apart
                    Vector2 up = Upwind(m);
                    for (byte p = 0; p < Players; p++)
                    {
                        float side = p == 0 ? -1f : 1f;
                        Support(m, orders, abilitiesUsed, cost, log, p, OffMapAbilityId.HeBarrage, Clamp(m, view + new Vector2(8f * side, 0f)), "the held view's focus");
                        Support(m, orders, abilitiesUsed, cost, log, p, OffMapAbilityId.ChlorineGas, Clamp(m, view + up * 12f + new Vector2(10f * side, 0f)), "12 m upwind of the held view's focus");
                    }
                    StarShell(log);
                    break;
                }
            }
            if (orders.Count == 0) return;

            // pay for the scenario and clear what it uses: one write at the tick the window opens, the same in every run
            var w = m.World;
            for (int p = 0; p < Players; p++) if (cost[p] > 0) log.Writes.Add($"silver[p{p}] += {cost[p]} (was {w.Silver[p]})");
            if (m.Abilities != null)
                foreach (int a in abilitiesUsed)
                    log.Writes.Add($"ability cooldown[p{a / OffMapAbilitySystem.AbilitySlots}][{(OffMapAbilityId)(a % OffMapAbilitySystem.AbilitySlots)}] = 0 (was {m.Abilities.Cooldown[a]})");
            foreach (int ri in slotsUsed)
                log.Writes.Add($"slot cooldown[p{ri / RosterEntry.SlotCount}][{ri % RosterEntry.SlotCount}] = 0 (was {w.SlotCooldown[ri]})");
            bool wrote = host.WriteWorlds(world =>
            {
                for (int p = 0; p < Players; p++) if (cost[p] > 0) world.World.Silver[p] += cost[p];
                if (world.Abilities != null) foreach (int a in abilitiesUsed) world.Abilities.Cooldown[a] = 0;
                foreach (int ri in slotsUsed) world.World.SlotCooldown[ri] = 0;
            });
            if (!wrote)
            {
                log.Writes.Clear();
                log.Warnings.Add("scenario: SimHost.WriteWorlds refused (the canary is waiting on the network): no silver top-up, so orders a side cannot afford will be rejected");
            }

            // the orders: player 0 as the HUD issues them, player 1 through the enemy seat as the scripted enemy does.
            // Tick and Player are overwritten by the driver / seat: the order lands InputDelay ticks after the next send.
            uint localTick = m.World.Tick;
            var enemyView = host.EnemyView;
            uint enemyTick = enemyView != null ? enemyView.World.Tick : localTick;
            foreach (var o in orders)
            {
                var c = o.Command;
                c.Player = o.Player;
                c.Tick = o.Player == 0 ? localTick : enemyTick;
                if (o.Player == 0) host.Issue(c); else host.IssuePeer(c);
                log.Commands.Add(o.Text + $" [issued at tick {c.Tick}]");
            }
        }

        static void Support(MatchSim m, List<Order> orders, List<int> used, int[] cost, Log log, byte p, OffMapAbilityId ability, Vector2 at, string what)
        {
            if (m.Abilities == null) { log.Warnings.Add($"{ability}: this match has no OffMapAbilitySystem: skipped"); return; }
            if (!OffMapAbilitySystem.TryGetStats((int)ability, out var stats)) { log.Warnings.Add($"{ability}: no such ability in this build: skipped"); return; }
            cost[p] += stats.Cost;
            used.Add(p * OffMapAbilitySystem.AbilitySlots + (int)ability);
            orders.Add(new Order
            {
                Player = p,
                Command = new SimCommand { Player = p, Type = CommandType.SupportFire, A = (int)ability, Pos = new Unity.Mathematics.float3(at.x, 0f, at.y) },
                Text = string.Format(Inv, "p{0} SupportFire {1} at ({2:0.0}, {3:0.0}): {4} ({5} silver)", p, ability, at.x, at.y, what, stats.Cost),
            });
        }

        /// <summary>The fire trench `team` holds furthest forward; if it holds none now, the one the map gave it.</summary>
        static short FrontTrench(MatchSim m, byte team)
        {
            short now = m.Fields != null ? m.Fields.FrontTrench(team) : (short)-1;
            if (now >= 0) return now;
            short best = -1; float bestZ = team == 0 ? float.MinValue : float.MaxValue;
            for (int t = 0; t < m.Map.Trenches.Length; t++)
            {
                var def = m.Map.Trenches[t];
                if (def.OwnerTeam != team || def.Kind != 0 || def.CellCount == 0) continue;
                float z = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart]).z;
                if (team == 0 ? z > bestZ : z < bestZ) { bestZ = z; best = (short)t; }
            }
            return best;
        }

        /// <summary>The centre of the trench's nav cell nearest `near` (x, z): a point on the line, where the camera is.</summary>
        static bool TrenchCellNear(MatchSim m, short trench, Vector2 near, out Vector2 at)
        {
            at = default;
            if (trench < 0 || trench >= m.Map.Trenches.Length) return false;
            var def = m.Map.Trenches[trench];
            float best = float.MaxValue;
            for (int c = 0; c < def.CellCount; c++)
            {
                var p = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + c]);
                float dx = p.x - near.x, dz = p.z - near.y, d = dx * dx + dz * dz;
                if (d < best) { best = d; at = new Vector2(p.x, p.z); }
            }
            return best < float.MaxValue;
        }

        /// <summary>A unit vector (x, z) pointing into the wind, or zero on a still map.</summary>
        static Vector2 Upwind(MatchSim m)
        {
            var wind = new Vector2(m.Map.Wind.x, m.Map.Wind.y);
            return wind.sqrMagnitude > 1e-6f ? -wind.normalized : Vector2.zero;
        }

        /// <summary>Inside the map with a metre to spare: OffMapAbilitySystem rejects a target outside it.</summary>
        static Vector2 Clamp(MatchSim m, Vector2 p) =>
            new Vector2(Mathf.Clamp(p.x, 1f, m.Map.SizeMeters.x - 1f), Mathf.Clamp(p.y, 1f, m.Map.SizeMeters.y - 1f));

        /// <summary>The star shell is presentation only: NightLights fires one on its own timer, ahead of the camera;
        /// FireStarShell brings it forward to the next frame. Nothing in the sim sees it.</summary>
        static void StarShell(Log log)
        {
            var night = UnityEngine.Object.FindFirstObjectByType<NightLights>();
            if (night == null) { log.Warnings.Add("vfx: no star shell: this ground's profile has no NightLights (no lamps, so no flares)"); return; }
            night.FireStarShell();
            log.Presentation.Add("NightLights.FireStarShell() at the window's first frame");
        }

        static string VehicleName(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Maw: return "Maw";
                case VehicleArchetype.Tusk: return "Tusk";
                case VehicleArchetype.Pincer: return "Pincer";
                case VehicleArchetype.Kettle: return "Kettle";
                case VehicleArchetype.Censer: return "Censer";
                case VehicleArchetype.Pavise: return "Pavise";
                case VehicleArchetype.Banner: return "Banner";
                case VehicleArchetype.Redoubt: return "Redoubt";
                default: return "archetype " + archetype.ToString(Inv);
            }
        }
    }
}
