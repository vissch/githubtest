// Phase: tooling (the gym, 2026-09-28) — stages one GymCatalogue entry at a time in the running battle, on the battle's own
// drawing (VAT men, TankRenderer machines, CombatFx), so what the gym shows is what a match shows. Everything that
// touches the sim goes through the paths the game and the benches already use: SimHost.WriteWorlds for tooling writes
// (spawn, hit points, silver, cooldowns, a fire), SimHost.Issue / IssuePeer for orders. Nothing writes world arrays
// by hand (TankCapture.Spawn does; the gym does not copy it). Events raised inside a WriteWorlds are cleared by the next
// SimWorld.Step, so a death or an effect always comes from a system stepping inside a tick: the gym stages the cause
// (a rifle line, a barrage, a fire) and waits for the sim. Presentation-only previews (an event replayed into
// EventPump.Frame) reach the effects but never the men (the controller latches the world's events), so they are
// counted apart and marked "preview (effects only)".
// Scenes are the owner's "your actions affect the battlefield": men in a trench or fresh craters, an enemy line, and
// the thing that hits them; the result counts what happened to the watched men (hits, near misses, deaths).
// Entries are staged at one of five places 60 m apart in turn, so a gas cloud or a fire left by one entry does not
// land in the next one's frame, and each entry's proof is its own (a death entry counts only its victim's Death).
// Quiet first: no scripted enemy, no enemy attacks or support fire, no ambient shells, so nothing strays into the frame.
// TW.Perf ships, so the director is compiled only in the editor and development builds: no release player carries a
// tool that writes the worlds.
#if UNITY_EDITOR || DEVELOPMENT_BUILD
using System.Collections;
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Perf
{
    public sealed class GymDirector : MonoBehaviour
    {
        /// <summary>What one trigger did, for the window and the batch sidecar.</summary>
        public sealed class Result
        {
            public GymEntry Entry;
            public uint TriggerTick;
            public readonly List<int> Slots = new List<int>();
            public readonly Dictionary<SimEventType, int> Events = new Dictionary<SimEventType, int>();
            public int Previews, Rejects, Errors, Warnings;
            public readonly List<string> Log = new List<string>();
            public bool Desync, Canary;
            public float3 Focus;
            /// <summary>A death entry's victim; VictimDied is set only by HIS Death event.</summary>
            public int Victim = -1; public bool VictimDied;
            /// <summary>The men a scene watches, and what happened to them.</summary>
            public readonly HashSet<int> Watch = new HashSet<int>();
            public int WatchedHits, WatchedNearMisses, WatchedDeaths, WatchedSuppressed;
            public int Count(SimEventType t) => Events.TryGetValue(t, out int n) ? n : 0;
        }

        public SimHost Host { get; private set; }
        public Result Current { get; private set; }
        /// <summary>Where the current entry is staged (x, z).</summary>
        public Vector2 Stage;
        Vector2 stageBase; int stageTurn;
        bool quiet; bool wasScripted, wasAttacks, wasSupport; float wasShells;

        public static GymDirector Attach(SimHost host)
        {
            if (host == null) return null;
            var d = host.GetComponent<GymDirector>();
            if (d == null) d = host.gameObject.AddComponent<GymDirector>();   // Unity's fake null: never `??` on a component
            d.Host = host;
            return d;
        }

        void OnEnable() => Application.logMessageReceived += OnLog;
        void OnDisable() { Application.logMessageReceived -= OnLog; if (Host != null) Host.Events.OnEvent -= OnEvent; hooked = false; }
        bool hooked;

        void OnLog(string msg, string stack, LogType type)
        {
            if (Current == null) return;
            if (type == LogType.Error || type == LogType.Exception || type == LogType.Assert) { Current.Errors++; if (Current.Log.Count < 20) Current.Log.Add(type + ": " + msg); }
            else if (type == LogType.Warning) Current.Warnings++;
        }

        void OnEvent(SimEvent e)
        {
            var r = Current;
            if (r == null) return;
            r.Events.TryGetValue(e.Type, out int n); r.Events[e.Type] = n + 1;
            switch (e.Type)
            {
                case SimEventType.CommandRejected: r.Rejects++; break;
                case SimEventType.Death:
                    if (e.A == r.Victim) r.VictimDied = true;
                    if (r.Watch.Contains(e.A)) r.WatchedDeaths++;
                    break;
                case SimEventType.Hit: if (r.Watch.Contains(e.B)) r.WatchedHits++; break;
                case SimEventType.NearMiss: if (r.Watch.Contains(e.A)) r.WatchedNearMisses++; break;
                case SimEventType.Suppressed: if (r.Watch.Contains(e.A)) r.WatchedSuppressed++; break;
            }
        }

        /// <summary>Stop the scripted enemy, its attacks and support fire, and the ambient shells; pick the stage.</summary>
        public bool Quiet()
        {
            if (Host == null || Host.Local == null) return false;
            if (!hooked) { Host.Events.OnEvent += OnEvent; hooked = true; }
            if (!quiet) { wasScripted = Host.ScriptedPeer; wasAttacks = Host.PeerAttacks; wasSupport = Host.PeerUsesSupport; wasShells = Host.Local.Bombardment != null ? Host.Local.Bombardment.ShellsPerMinute : 0f; }
            Host.ScriptedPeer = false; Host.PeerAttacks = false; Host.PeerUsesSupport = false;
            var w = Host.Local.World;
            var size = Host.Local.Map.SizeMeters;
            var rally = w.Rally[0];
            stageBase = Vector2.Lerp(new Vector2(rally.x, rally.z), new Vector2(size.x * 0.5f, size.y * 0.5f), 0.35f);
            Stage = stageBase;
            quiet = Host.WriteWorlds(m => { if (m.Bombardment != null) m.Bombardment.ShellsPerMinute = 0f; });
            return quiet;
        }

        /// <summary>Put the battle back as Quiet found it.</summary>
        public void Unquiet()
        {
            if (!quiet || Host == null || Host.Local == null) return;
            Host.ScriptedPeer = wasScripted; Host.PeerAttacks = wasAttacks; Host.PeerUsesSupport = wasSupport;
            float shells = wasShells;
            if (Host.WriteWorlds(m => { if (m.Bombardment != null) m.Bombardment.ShellsPerMinute = shells; })) quiet = false;
        }

        /// <summary>The next of five places 60 m apart along x: what one entry left burning stays out of the next.</summary>
        public Vector2 NextStage()
        {
            var size = Host.Local.Map.SizeMeters;
            float x = stageBase.x + ((stageTurn++ % 5) - 2) * 60f;
            Stage = new Vector2(Mathf.Clamp(x, 20f, size.x - 20f), stageBase.y);
            return Stage;
        }

        public Result Begin(GymEntry entry)
        {
            Current = new Result { Entry = entry, TriggerTick = Host != null && Host.Local != null ? Host.Local.World.Tick : 0u, Focus = new float3(Stage.x, 0f, Stage.y) };
            return Current;
        }

        /// <summary>Close the current result: note the canary and a desync. The caller reads Current before the next Begin.</summary>
        public Result End()
        {
            if (Current != null && Host != null) { Current.Desync = Host.Desync; Current.Canary = Host.CanaryActive; }
            return Current;
        }

        // ------------------------------------------------------------------------------------------------ staging
        /// <summary>A unit at (x, z) in every world, through WriteWorlds: the slot, or -1 (worlds unaligned, or the
        /// worlds disagreed on the slot).</summary>
        public int Spawn(int team, int archetype, float x, float z, float yawDeg = 0f)
        {
            if (Host == null || Host.Local == null || archetype < 0 || archetype >= Archetypes.Count) return -1;
            int first = -1; bool split = false;
            bool ok = Host.WriteWorlds(m =>
            {
                var w = m.World;
                var entry = w.Units.Roster[archetype];
                if (entry.Hp <= 0f) entry = w.Roster[team * RosterEntry.SlotCount];
                bool vehicle = ChassisKind.IsArmoured(w.ChassisOf((byte)archetype));
                int s = w.Spawn((byte)team, (byte)archetype, new float3(x, 0f, z), entry.Hp, entry.Speed, vehicle);
                if (s >= 0) w.Yaw[s] = yawDeg * Mathf.Deg2Rad;
                if (first < 0) first = s; else if (s != first) split = true;
            });
            if (!ok || split) { Current?.Log.Add(ok ? "spawn: the worlds gave different slots" : "spawn: worlds unaligned (network)"); return -1; }
            if (first >= 0) { Current?.Slots.Add(first); spawned.Add((first, Host.Local.World.Generation[first])); }
            return first;
        }

        /// <summary>A row of `count` men (or machines) for `team`, `spacing` metres apart, facing the other side.</summary>
        public List<int> Row(int team, int archetype, int count, Vector2 at, float spacing)
        {
            var slots = new List<int>();
            for (int k = 0; k < count; k++)
            {
                int s = Spawn(team, archetype, at.x + (k - (count - 1) * 0.5f) * spacing, at.y, team == 0 ? 0f : 180f);
                if (s >= 0) slots.Add(s);
            }
            return slots;
        }

        /// <summary>Is the unit spawned in `slot` (with that generation) alive now?</summary>
        public bool Alive(int slot)
        {
            if (Host == null || Host.Local == null || slot < 0) return false;
            var w = Host.Local.World;
            foreach (var (s, g) in spawned) if (s == slot) return w.Generation[s] == g && (w.Flags[s] & (uint)UnitFlags.Alive) != 0;
            return false;
        }

        /// <summary>Clear the stage: the units the gym spawned (and only those: the battle's own garrisons stay, or the
        /// match could end) are removed, pins let go. Despawn inside WriteWorlds raises events the next Step clears, but
        /// the controller still sees a man stop living and plays his fall: let the stage settle before the next entry.</summary>
        public void Clear()
        {
            if (Host == null || Host.Local == null) return;
            Host.Animation?.UnpinAll();
            var gone = spawned.ToArray(); spawned.Clear();
            Host.WriteWorlds(m =>
            {
                var w = m.World;
                foreach (var (slot, gen) in gone)
                    if (slot < w.HighWater && w.Generation[slot] == gen && (w.Flags[slot] & (uint)UnitFlags.Alive) != 0) w.Despawn(slot);
            });
        }
        readonly List<(int slot, ushort gen)> spawned = new List<(int, ushort)>();

        /// <summary>Pin `clip` on a man of `figure` at `at`: the clips tab.</summary>
        public int PlayClip(int figure, Clip clip, Vector2 at, float rate = 1f)
        {
            int s = Spawn(0, GymCatalogue.ArchetypeForFigure(figure), at.x, at.y, 0f);
            if (s >= 0) Hold(s);
            if (s >= 0 && Host.Animation != null) Host.Animation.Pin(s, clip, Host.Local.World.Generation[s], rate);
            return s;
        }

        /// <summary>Keep a spawned unit where it stands: its goal is its own nav cell, so it has arrived. A unit with no
        /// goal is given its team's front trench, and a clip's man walked out of the close shot while it played (the
        /// longer half of the clips, deaths, fidgets, the walk, were photographed on empty ground), a rifleman out of
        /// the Units tab's (2026-10-01).</summary>
        public void Hold(int slot)
        {
            if (slot < 0 || Host == null || Host.Local == null) return;
            Host.WriteWorlds(m =>
            {
                var w = m.World; var c = m.Map.NavCellOf(w.Position[slot]);
                var mode = (w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0 ? TW.Sim.Nav.NavMode.Tracked : TW.Sim.Nav.NavMode.Infantry;
                w.GoalId[slot] = m.Fields.GetGoal(TW.Sim.Nav.GoalKey.Cell(m.Map.NavIndex(c.x, c.y), mode));
            });
        }

        /// <summary>Call an off-map ability from `seat` (-1: the seat that may call it: Brass for ParaDrop, else 0). Top up
        /// that side's silver and clear its cooldown (WriteWorlds, as BenchScenarios does), then the order through the
        /// seat's own path.</summary>
        public bool Ability(OffMapAbilityId id, Vector2 at, int headingDeg = 0, int pattern = 0, int seat = -1)
        {
            if (Host == null || Host.Local == null || Host.Local.Abilities == null) return false;
            byte p = seat >= 0 ? (byte)seat : id == OffMapAbilityId.ParaDrop ? FactionSeat((byte)FactionId.Brass) : (byte)0;
            int cost = OffMapAbilitySystem.TryGetStats((int)id, out var stats) ? stats.Cost : 0;
            int key = p * OffMapAbilitySystem.AbilitySlots + (int)id;
            bool paid = Host.WriteWorlds(m =>
            {
                m.World.Silver[p] += cost;
                if (m.Abilities != null && key >= 0 && key < m.Abilities.Cooldown.Length) m.Abilities.Cooldown[key] = 0;
            });
            if (!paid) { Current?.Log.Add($"{id}: worlds unaligned, no top-up: not called"); return false; }   // the caller retries next frame
            var c = new SimCommand { Player = p, Type = CommandType.SupportFire, A = (int)id, B = AbilityArgs.Pack(headingDeg, pattern, 0), Pos = new float3(at.x, 0f, at.y) };
            c.Tick = p == 0 ? Host.Local.World.Tick : (Host.EnemyView != null ? Host.EnemyView.World.Tick : Host.Local.World.Tick);
            if (p == 0) Host.Issue(c); else Host.IssuePeer(c);
            Current?.Log.Add($"p{p} SupportFire {id} at ({at.x:0.0}, {at.y:0.0}) heading {headingDeg}");
            return true;
        }

        /// <summary>The seat (0 or 1) whose faction is `faction`, or 0.</summary>
        public byte FactionSeat(byte faction) => Host != null && Host.FactionB == faction && Host.FactionA != faction ? (byte)1 : (byte)0;

        /// <summary>Kill a man of `archetype` by `kind`, staging the cause; the sim does the killing inside a tick.
        /// Returns the victim's slot (also Current.Victim), or -1.</summary>
        public int Death(DeathKind kind, int archetype, Vector2 at)
        {
            int victim = Spawn(1, archetype, at.x, at.y, 180f);   // the enemy side, so our fire and our abilities find him
            if (victim < 0) return -1;
            Hold(victim);   // where the shells and the gas are sent: he walked for his front trench and the barrage missed him
            if (Current != null) Current.Victim = victim;
            switch (kind)
            {
                case DeathKind.Shot:
                    Host.WriteWorlds(m => m.World.Hp[victim] = 1f);
                    Row(0, 0, 4, at + new Vector2(0f, -30f), 2f);
                    break;
                case DeathKind.Blast:
                    // the 16 m box (pattern 2), on a man with 1 hp left as Shot does: twelve shells over the 25 m disc put
                    // about one within 8 m of him and he lived through it; any shell that reaches him now kills him
                    Host.WriteWorlds(m => m.World.Hp[victim] = 1f);
                    Ability(OffMapAbilityId.HeBarrage, at, 0, AbilityPattern.Box);
                    break;
                case DeathKind.Gas: Ability(OffMapAbilityId.ChlorineGas, at); break;
                case DeathKind.Beam: Ability(OffMapAbilityId.Beam, at + new Vector2(0f, -20f), 0); break;
                case DeathKind.Burning:
                    // a tooling write into the burning system (Burning.cs says presentation never calls it; flagged to the
                    // SIM lane): the fire then kills him inside its own step, so his Death event is a real one
                    Host.WriteWorlds(m => { if (m.Burning != null) m.Burning.Ignite(m.World, victim, 30f); });
                    break;
                case DeathKind.Crushed:
                    // there is no move order for one vehicle: a Maw of ours is put 25 m short of him, nose on, and drives
                    // for the enemy on its own; he may not be under its tracks (the entry then flags "he did not die")
                    Spawn(0, VehicleArchetype.Maw, at.x, at.y - 25f, 0f);
                    break;
            }
            return victim;
        }

        /// <summary>Replay one event into this frame's presentation (never the sim, never the men): for events too
        /// costly to stage. b = -1 so it names no real man.</summary>
        public void Preview(SimEventType type, int a, Vector2 at, float3 dir, float scalar)
        {
            if (Host == null || Host.Local == null) return;
            Host.Events.Frame.Add(new SimEvent { Tick = Host.Local.World.Tick, Type = type, A = a, B = -1, Pos = new float3(at.x, 0f, at.y), Dir = dir, Scalar = scalar });
            if (Current != null) Current.Previews++;
        }

        // ------------------------------------------------------------------------------------------------- scenes
        /// <summary>The fire-trench cell of `team` nearest `near`, and the next few along it; false when it holds none.</summary>
        public bool TrenchCells(byte team, Vector2 near, int count, List<Vector2> cells)
        {
            cells.Clear();
            var map = Host.Local.Map;
            int bestT = -1, bestC = -1; float best = float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                if (def.OwnerTeam != team || def.Kind != 0) continue;
                for (int c = 0; c < def.CellCount; c++)
                {
                    var p = map.NavCellCenter(map.TrenchCells[def.CellStart + c]);
                    float d = (p.x - near.x) * (p.x - near.x) + (p.z - near.y) * (p.z - near.y);
                    if (d < best) { best = d; bestT = t; bestC = c; }
                }
            }
            if (bestT < 0) return false;
            var tr = map.Trenches[bestT];
            for (int k = 0; k < count && bestC + k < tr.CellCount; k++)
            {
                var p = map.NavCellCenter(map.TrenchCells[tr.CellStart + bestC + k]);
                cells.Add(new Vector2(p.x, p.z));
            }
            return cells.Count > 0;
        }

        /// <summary>Up to `count` shell-hole cells (NavLayer.Crater, open ground) within 30 m of `near`, the nearest
        /// first, `spacing` metres or more apart: where CraterMen puts its men.</summary>
        public List<Vector2> Craters(Vector2 near, int count, float spacing)
        {
            var map = Host.Local.Map;
            var found = new List<(float, Vector2)>();
            int r = (int)(30f / TW.Sim.Terrain.MapData.NavCellSize);
            var c0 = map.NavCellOf(new float3(near.x, 0f, near.y));
            for (int z = c0.y - r; z <= c0.y + r; z++)
            for (int x = c0.x - r; x <= c0.x + r; x++)
            {
                if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                int i = map.NavIndex(x, z);
                var l = (TW.Sim.Terrain.NavLayer)map.NavLayers[i];
                if ((l & TW.Sim.Terrain.NavLayer.Crater) == 0) continue;
                if ((l & (TW.Sim.Terrain.NavLayer.Trench | TW.Sim.Terrain.NavLayer.Blocked | TW.Sim.Terrain.NavLayer.Wire)) != 0) continue;
                var p = map.NavCellCenter(i);
                var q = new Vector2(p.x, p.z);
                float d = (q - near).sqrMagnitude;
                if (d <= 900f) found.Add((d, q));
            }
            found.Sort((a, b) => a.Item1.CompareTo(b.Item1));
            var picked = new List<Vector2>();
            foreach (var (_, q) in found)
            {
                if (picked.Count >= count) break;
                bool clear = true;
                foreach (var o in picked) if ((o - q).sqrMagnitude < spacing * spacing) { clear = false; break; }
                if (clear) picked.Add(q);
            }
            return picked;
        }

        /// <summary>Of eight ways out of `from`, `prefer` first, the first whose point `distance` on stands well clear
        /// of every man of ours the gym did not spawn (else the clearest), inside the map: a line of theirs there has
        /// the gym's men as its nearest targets.</summary>
        public Vector2 Clearest(Vector2 from, float distance, Vector2 prefer)
        {
            var w = Host.Local.World; var map = Host.Local.Map;
            float sizeX = map.NavWidth * TW.Sim.Terrain.MapData.NavCellSize, sizeZ = map.NavLength * TW.Sim.Terrain.MapData.NavCellSize;
            float start = Mathf.Atan2(prefer.x, prefer.y);
            Vector2 best = prefer.sqrMagnitude > 1e-6f ? prefer.normalized : Vector2.up; float bestScore = -1f;
            for (int k = 0; k < 8; k++)
            {
                float a = start + k * Mathf.PI / 4f;
                var dir = new Vector2(Mathf.Sin(a), Mathf.Cos(a));
                var q = from + dir * distance;
                if (q.x < 6f || q.y < 6f || q.x > sizeX - 6f || q.y > sizeZ - 6f) continue;
                float nearest = float.MaxValue;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || w.Team[i] != 0) continue;
                    bool ours = false;
                    foreach (var (s, g) in spawned) if (s == i && w.Generation[i] == g) { ours = true; break; }
                    if (ours) continue;
                    nearest = Mathf.Min(nearest, Vector2.Distance(q, new Vector2(w.Position[i].x, w.Position[i].z)));
                }
                if (nearest > distance + 25f) return dir;   // the first way (the preferred one, if it will do) where they are clearly nearest
                if (nearest > bestScore + 1f) { bestScore = nearest; best = dir; }
            }
            return best;
        }

        /// <summary>Stage a scene over time (the window starts it as a coroutine; the gym run waits on it). Sets
        /// Current.Focus and Current.Watch.</summary>
        public IEnumerator Scene(GymScene scene)
        {
            var r = Current;
            var cells = new List<Vector2>();
            var enemyRally = Host.Local.World.Rally[1];
            switch (scene)
            {
                case GymScene.TrenchLine:
                case GymScene.BarrageOnTrench:
                case GymScene.GasOnTrench:
                {
                    if (!TrenchCells(0, Stage, 6, cells)) { r?.Log.Add("scene: no fire trench of ours"); yield break; }
                    foreach (var c in cells) { int s = Spawn(0, 0, c.x, c.y, 0f); if (s >= 0) r?.Watch.Add(s); }
                    var mid = cells[cells.Count / 2];
                    var toward = (new Vector2(enemyRally.x, enemyRally.z) - mid).normalized;
                    Row(1, 0, 6, mid + toward * 80f, 3f);
                    if (r != null) r.Focus = new float3(mid.x, 0f, mid.y);
                    yield return new WaitForSeconds(3f);
                    if (scene == GymScene.BarrageOnTrench) Ability(OffMapAbilityId.HeBarrage, mid, seat: 1);
                    if (scene == GymScene.GasOnTrench) Ability(OffMapAbilityId.ChlorineGas, mid - toward * 12f, seat: 1);
                    break;
                }
                case GymScene.CraterMen:
                {
                    var at = Stage;
                    Ability(OffMapAbilityId.HeBarrage, at);   // make the craters first
                    yield return new WaitForSeconds(11f);
                    // a man in each of the holes it made, the nearest first (a row stood beside them, on the grass);
                    // a row makes up any it did not make
                    var holes = Craters(at, 6, 3f);
                    var men = new List<int>();
                    foreach (var c in holes) { int s = Spawn(0, 0, c.x, c.y, 0f); if (s >= 0) men.Add(s); }
                    if (men.Count < 6) men.AddRange(Row(0, 0, 6 - men.Count, at, 3f));
                    foreach (var s in men) r?.Watch.Add(s);
                    // their enemy where they are the nearest of ours: toward the enemy's rally the line found our
                    // front trench's garrison first and shot at it (2026-10-01: 9 rounds, none at the men in the holes)
                    var centre = at;
                    if (holes.Count > 0) { centre = Vector2.zero; foreach (var c in holes) centre += c; centre /= holes.Count; }
                    Row(1, 0, 6, centre + Clearest(centre, 80f, new Vector2(enemyRally.x, enemyRally.z) - centre) * 80f, 3f);
                    if (r != null) r.Focus = new float3(centre.x, 0f, centre.y);
                    yield return new WaitForSeconds(3f);   // as the trench scenes: the line finds them and opens fire
                    break;
                }
            }
        }

        /// <summary>Seconds a scene needs after Scene() returns before its pictures mean something.</summary>
        public static float SceneSettle(GymScene scene) => scene == GymScene.GasOnTrench ? 18f : scene == GymScene.BarrageOnTrench ? 12f : 6f;

        // ------------------------------------------------------------------------------------------------- camera
        public static void Look(Vector2 focus, float zoom, float yawDeg = 30f)
        {
            var cam = FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            if (cam != null) cam.FrameFrom(focus, zoom, yawDeg);
        }
    }
}
#endif
