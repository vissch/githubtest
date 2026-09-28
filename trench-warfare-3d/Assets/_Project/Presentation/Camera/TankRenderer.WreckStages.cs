// Phase: wrecks (2026-09-28, implemented) — part of TankRenderer: a wreck breaking in stages and then gone (owner,
// 2026-09-28). The sim wears the wreck prop down (DeformationSystem: Wreck -> BrokenWreck -> Scrap -> Cleared, with
// hit points in between); this draws it. A dead machine's root part is drawn as its carcass (WreckModel: the same hull
// cut into chunks at load) with the chunks that are gone masked away in TW/Tank (WreckStageRules says which), so a
// wreck is still one instance of one mesh whatever state it is in:
//   - between stages, as the prop's hit points fall (read once a sim tick), chunks come away from the top down and fly
//     as pieces of hull (Shard: its own flight, bounce and rest, drawn as the carcass with every other chunk masked);
//   - to a broken wreck, everything still bolted to it goes (turret, tracks, wheels, legs), the crown of the hull with
//     it, a burst of plates and dust, and the hull slumps;
//   - to scrap, the rest topples in and what is left is the keel flattened into a heap;
//   - cleared, the heap bursts low and sinks into the mud, and the view is gone.
// The map's own wrecks (the generator's, never a live machine) are adopted as husks at the start of a match, drawn
// from the Maw or the Tusk, so they break the same way; the composer's four grey boxes are only the fallback now.
// Seeded throws (DebrisRng from the prop's position), no UnityEngine.Random.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        sealed partial class View
        {
            /// <summary>The sim's wreck prop this hull is drawn for (-1: none yet), its stage (WreckStageRules), the chunks
            /// of its carcass that are gone, how flat the scrap is, when it was cleared, and whether it never was a live
            /// machine (a husk: one of the map's own wrecks, adopted).</summary>
            public int Prop = -1; public int Stage; public uint Hidden; public float Squash = 1f, ClearedAt = -1f; public bool Husk;
            /// <summary>The way the last harm went (PropWorn's dir): what comes away flies that way.</summary>
            public Vector3 Harm;
        }

        sealed class Shard
        {
            public View Owner; public Carcass Carcass; public int Chunk;
            public Vector3 Pos, Vel, Spin; public Quaternion Rot; public bool Resting; public float Burn, RestedAt;
        }

        /// <summary>Chunks of hull in the air or on the ground at once (the carcass's own pieces); past it a chunk that
        /// comes away is masked and thrown as plates only. They lie ShardLie seconds, then sink over ShardSink.</summary>
        public const int MaxShards = 64;
        public const float ShardLie = 25f, ShardSink = 3f, ClearSinkSeconds = 5f;
        static readonly int ChunksId = Shader.PropertyToID("_Chunks");
        readonly Dictionary<Mesh, Carcass> carcasses = new Dictionary<Mesh, Carcass>();
        readonly List<Shard> shards = new List<Shard>(MaxShards);
        TW.Sim.Match.MatchSim stagesMatch;
        uint lastWearTick = uint.MaxValue;

        /// <summary>The carcass of a model's root part at a LOD, cut once (the far LOD on the near LOD's grid).</summary>
        Carcass CarcassOf(TankModel m, int lod)
        {
            var l = m.Lods[lod];
            if (l == null || l.Parts.Count == 0 || l.Parts[0].Mesh == null) return null;
            var root = l.Parts[0].Mesh;
            if (carcasses.TryGetValue(root, out var c)) return c;
            Carcass near = null;
            if (lod == 1 && m.Lods[0] != null && m.Lods[0].Parts.Count > 0 && m.Lods[0].Parts[0].Mesh != root) near = CarcassOf(m, 0);
            uint seed = (uint)m.Name.GetHashCode() * 2654435761u;
            c = near != null ? WreckModel.Build(root, seed, near, m.Lods[0].Parts[0].Mesh.bounds) : WreckModel.Build(root, seed);
            carcasses[root] = c;   // null too: a mesh that cannot be cut is asked once
            return c;
        }

        /// <summary>A wreck prop changed stage (PropChanged with a later kind). True when it was a wreck's: handled here.</summary>
        bool WreckStageEvent(in SimEvent e)
        {
            var kind = (PropKind)e.B;
            int stage = WreckStageRules.StageOf(kind);
            if (stage <= WreckStageRules.Whole) return false;   // a tree's, or a wreck appearing: the link below takes it
            View v = null;
            for (int k = wrecks.Count - 1; k >= 0; k--) if (wrecks[k].Prop == e.A) { v = wrecks[k]; break; }
            if (v == null || stage <= v.Stage) return true;   // not drawn by a hull (the composer's stand-in), or already there
            float now = Time.time;
            v.Stage = stage;
            var rng = new DebrisRng(v.PropPos, 0xB0A0u + (uint)stage);
            Vector3 at = v.Pos + Vector3.up * (v.Heave.Value + v.Model.Height * 0.5f);
            if (stage == WreckStageRules.Broken)
            {
                // everything still bolted on goes: up and out, spinning, the tracks sideways
                var parts = v.Model.Lods[0].Parts;
                for (int i = 1; i < parts.Count; i++)
                {
                    if (v.Off[i] || parts[i].Parent != 0) continue;
                    var d = Detach(v, i, v.World[i]);
                    Vector3 away = (Vector3)v.World[i].GetColumn(3) - v.Pos; away.y = 0f;
                    bool low = parts[i].Role == TankPartRole.Track || parts[i].Role == TankPartRole.Wheel || parts[i].Role == TankPartRole.Leg || parts[i].Role == TankPartRole.Thigh;
                    d.Vel = away.normalized * rng.Range(low ? 2f : 2f, low ? 4f : 4f) + Vector3.up * (low ? rng.Range(2f, 4f) : rng.Range(6f, 10f)) + v.Harm * 2f;
                    d.Spin = rng.OnSphere() * rng.Range(2f, 7f);
                    d.Burn = Mathf.Max(d.Burn, v.Burn);
                }
                v.Heave.Value -= v.Model.Height * 0.08f;   // it slumps where it lost its running gear
                ThrowChunks(v, WreckStageRules.Hidden(CarcassOf(v.Model, 0), stage, 1f), 7f, rng);
                Scrap(at, 10, 9f, 0.35f, Mathf.Max(0.2f, v.Burn), 30f, v.Harm, (uint)e.A * 7u + 1u);
                if (v.Burn > 0.3f) fireballs.Add(new Fireball { At = at, Born = now, Life = 1.1f, Width = 2.2f, Height = 4f, Phase = v.Slot });
                if (books != null && books.Ready)
                {
                    books.Add(FlipbookFx.Book.Wings, new Vector3(v.Pos.x, Ground(v.Pos.x, v.Pos.z), v.Pos.z), v.Model.HalfLength * 2.6f, 0.9f, FlipbookFx.Kind.Upright, grow: 0.4f, alpha: 0.8f, pop: 0.2f);
                    books.Add(FlipbookFx.Book.Smoke, at, v.Model.HalfLength * 1.6f, 6f, velocity: Vector3.up * 1.2f, grow: 1.4f, alpha: 0.7f);
                }
                CameraShake.Add(v.Pos, 7f);
            }
            else if (stage == WreckStageRules.Scrap)
            {
                // the rest topples in: low and slow, and what is left is the keel pressed into a heap
                ThrowChunks(v, WreckStageRules.Hidden(CarcassOf(v.Model, 0), stage, 1f), 2.5f, rng);
                v.Squash = WreckStageRules.ScrapHeight;
                Scrap(at, 8, 5f, 0.3f, 0f, 40f, v.Harm, (uint)e.A * 7u + 2u);
                if (books != null && books.Ready)
                    for (int k = 0; k < 4; k++)
                        books.Add(FlipbookFx.Book.Puff, v.Pos + rng.OnSphere() * v.Model.HalfLength * 0.6f + Vector3.up * 0.4f, 2.4f, 1.4f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.6f, grow: 0.8f, alpha: 0.7f, pop: 0.2f);
                CameraShake.Add(v.Pos, 4f);
            }
            else if (stage == WreckStageRules.Cleared)
            {
                // the heap bursts low and the mud takes what is left
                v.ClearedAt = now;
                Scrap(new Vector3(v.Pos.x, Ground(v.Pos.x, v.Pos.z) + 0.3f, v.Pos.z), 12, 5f, 0.3f, 0f, 20f, v.Harm, (uint)e.A * 7u + 3u);
                if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, v.Pos + Vector3.up * 0.3f, v.Model.HalfLength * 1.8f, 1.2f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.5f, grow: 0.7f, alpha: 0.7f);
            }
            v.Hidden |= WreckStageRules.Hidden(CarcassOf(v.Model, 0), stage, 1f);
            return true;
        }

        /// <summary>Harm a wreck stood (PropWorn): which way it went, for what comes away next.</summary>
        void WreckWorn(in SimEvent e)
        {
            for (int k = wrecks.Count - 1; k >= 0; k--)
                if (wrecks[k].Prop == e.A) { wrecks[k].Harm = (Vector3)e.Dir; wrecks[k].Flash = 0.6f; break; }
        }

        /// <summary>Once a frame, after the wrecks smoulder: the map's own wrecks adopted at a new match, each wreck's chunks
        /// following its hit points once a sim tick, the chunks in the air, and cleared heaps sinking out of sight.</summary>
        void WreckStagesFrame(float dt, float now, TW.Sim.Match.MatchSim match)
        {
            if (match != stagesMatch) { stagesMatch = match; AdoptHusks(match, now); }
            var map = match.Map;
            if (match.World.Tick != lastWearTick)
            {
                lastWearTick = match.World.Tick;
                foreach (var v in wrecks)
                {
                    if (v.Prop < 0 || v.Prop >= map.Props.Length || v.Stage >= WreckStageRules.Scrap) continue;
                    var prop = map.Props[v.Prop];
                    int stage = WreckStageRules.StageOf(prop.Kind);
                    if (stage != v.Stage) continue;   // the stage change's event is on its way
                    float full = PropRules.StartHp(prop.Kind, prop.Scale);
                    uint want = WreckStageRules.Hidden(CarcassOf(v.Model, 0), stage, full > 0f ? prop.Hp / full : 0f);
                    uint newly = WreckStageRules.Newly(v.Hidden, want);
                    if (newly == 0u) continue;
                    var rng = new DebrisRng(v.PropPos, 0xC0A0u + match.World.Tick);
                    ThrowChunks(v, newly, 4.5f, rng);
                    v.Hidden |= newly;
                    Scrap(v.Pos + Vector3.up * (v.Heave.Value + v.Model.Height * 0.7f), 3, 6f, 0.25f, v.Burn * 0.5f, 20f, v.Harm, match.World.Tick);
                }
            }
            FlyShards(dt, now);
            for (int k = wrecks.Count - 1; k >= 0; k--)
            {
                var v = wrecks[k];
                if (v.ClearedAt < 0f) continue;
                float age = now - v.ClearedAt;
                v.Heave.Value -= dt * v.Model.Height * WreckStageRules.ScrapHeight / ClearSinkSeconds;
                foreach (var d in v.Pieces) if (d.Resting) d.World.m13 -= dt * 0.25f;
                if (age < ClearSinkSeconds) continue;
                foreach (var p in v.Pieces) debris.Remove(p);
                shards.RemoveAll(s => s.Owner == v);
                wrecks.RemoveAt(k);
            }
        }

        /// <summary>The chunks in `bits` come away from v's hull as shards (while there is room), flying out at speed
        /// (and the way the harm went), tumbling.</summary>
        void ThrowChunks(View v, uint bits, float speed, DebrisRng rng)
        {
            var c = CarcassOf(v.Model, 0);
            if (c == null || bits == 0u) return;
            var root = v.World[0];
            Quaternion rot = root.rotation;
            for (int k = 1; k <= c.Chunks; k++)
            {
                if ((bits & (1u << (k - 1))) == 0u || c.Radius[k - 1] <= 0f || (v.Hidden & (1u << (k - 1))) != 0u) continue;
                if (shards.Count >= MaxShards) break;
                Vector3 centre = root.MultiplyPoint3x4(c.Centre[k - 1]);
                Vector3 away = centre - (v.Pos + Vector3.up * (v.Heave.Value + v.Model.Height * 0.3f)); away.y = Mathf.Max(0.2f, away.y);
                shards.Add(new Shard
                {
                    Owner = v, Carcass = c, Chunk = k, Pos = centre, Rot = rot,
                    Vel = away.normalized * speed * rng.Range(0.6f, 1.2f) + Vector3.up * rng.Range(2f, 5f) + v.Harm * 2.5f,
                    Spin = rng.OnSphere() * rng.Range(1.5f, 5f), Burn = v.Burn,
                });
            }
        }

        void FlyShards(float dt, float now)
        {
            for (int i = shards.Count - 1; i >= 0; i--)
            {
                var s = shards[i];
                s.Burn = Mathf.Max(0f, s.Burn - dt * 0.03f);
                if (s.Resting)
                {
                    float lain = now - s.RestedAt;
                    if (lain > ShardLie) s.Pos.y -= dt * s.Carcass.Radius[s.Chunk - 1] * 2f / ShardSink;
                    if (lain > ShardLie + ShardSink) shards.RemoveAt(i);
                    continue;
                }
                s.Vel += Vector3.down * 9.81f * dt;
                s.Pos += s.Vel * dt;
                if (s.Spin.sqrMagnitude > 1e-6f) s.Rot = Quaternion.AngleAxis(s.Spin.magnitude * Mathf.Rad2Deg * dt, s.Spin.normalized) * s.Rot;
                float ground = Ground(s.Pos.x, s.Pos.z), bottom = s.Pos.y - Mathf.Min(s.Carcass.Radius[s.Chunk - 1], 1.5f) * 0.45f;
                if (bottom < ground)
                {
                    s.Pos.y += ground - bottom;
                    if (s.Vel.y < -2f && books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, new Vector3(s.Pos.x, ground + 0.2f, s.Pos.z), 1.8f, 1.2f, velocity: Vector3.up * 0.5f, grow: 1f, alpha: 0.7f);
                    s.Vel = new Vector3(s.Vel.x * 0.45f, -s.Vel.y * 0.25f, s.Vel.z * 0.45f);
                    s.Spin *= 0.5f;
                    if (s.Vel.magnitude < 0.6f) { s.Resting = true; s.RestedAt = now; s.Vel = Vector3.zero; s.Spin = Vector3.zero; }
                }
            }
        }

        /// <summary>A shard: the carcass with every chunk but its own masked, turned about that chunk's middle.</summary>
        void DrawShards()
        {
            foreach (var s in shards)
            {
                var v = s.Owner;
                var m = Matrix4x4.TRS(s.Pos, s.Rot, Vector3.one) * Matrix4x4.Translate(-s.Carcass.Centre[s.Chunk - 1]);
                var dmg = new Vector4(1f, Mathf.Max(s.Burn, v.Burn * 0.5f), 0f, 0f);
                var tint = v.Team == 1 ? TeamTintB : new Vector4(1f, 1f, 1f, 0f);
                Queue(s.Carcass.Mesh, MaterialFor(v.Archetype, 0), m, 0f, dmg, tint, default, s.Carcass.All & ~(1u << (s.Chunk - 1)));
            }
        }

        /// <summary>A dead machine's root part as its carcass, the gone chunks masked (and flattened into a heap once it is
        /// scrap). False when the model cannot be cut: the root is drawn as it always was.</summary>
        bool QueueCarcass(View v, int lod, Matrix4x4 world, Vector4 damage, Vector4 tint, Vector4 team)
        {
            var c = CarcassOf(v.Model, lod);
            if (c == null) return false;
            if (v.Squash < 0.999f) world *= Matrix4x4.Scale(new Vector3(WreckStageRules.ScrapWiden, v.Squash, WreckStageRules.ScrapWiden));
            Queue(c.Mesh, MaterialFor(v.Archetype, lod), world, 0f, damage, tint, team, v.Hidden & c.All);
            return true;
        }

        /// <summary>The map's own wrecks, there before anything died, drawn as husks of the Maw or the Tusk (seeded by the
        /// prop), so they break like any other; a machine that dies later links to its own prop by its event.</summary>
        void AdoptHusks(TW.Sim.Match.MatchSim match, float now)
        {
            for (int k = wrecks.Count - 1; k >= 0; k--) if (wrecks[k].Husk) { foreach (var p in wrecks[k].Pieces) debris.Remove(p); wrecks.RemoveAt(k); }
            shards.Clear();
            var props = match.Map.Props;
            for (int i = 0; i < props.Length; i++)
            {
                var prop = props[i];
                int stage = WreckStageRules.StageOf(prop.Kind);
                if (stage < 0 || stage == WreckStageRules.Cleared) continue;
                var rng = new DebrisRng((Vector3)prop.Pos, 0x4B5Au);
                var model = rng.Next() < 0.5f && tusk != null ? tusk : maw;
                if (model == null) continue;
                var v = new View
                {
                    Slot = -1 - i, Team = (byte)(rng.Next() < 0.5f ? 0 : 1), Model = model, Archetype = model.Archetype, Born = now,
                    Pos = new Vector3(prop.Pos.x, 0f, prop.Pos.z), Yaw = prop.Yaw, Dead = true, DiedAt = now - 600f, Scorch = 1f, Hatch = 1f,
                    Linked = true, PropPos = (Vector3)prop.Pos, Prop = i, Husk = true, Stage = stage,
                };
                v.LastPos = v.Pos; v.LastYaw = v.Yaw;
                v.Off = new bool[model.Lods[0].Parts.Count];
                v.World = new Matrix4x4[model.Lods[0].Parts.Count];
                v.Heave.Value = Ground(v.Pos.x, v.Pos.z) - 0.2f;
                Pose(v, model.Lods[0], v.World);
                // a turret blown off and lying canted beside it, as the map's wrecks always had
                int turret = model.Lods[0].Find("Turret");
                if (turret < 0) turret = model.Lods[0].Find("Cupola");
                if (turret >= 0 && rng.Next() < 0.6f)
                {
                    var d = Detach(v, turret, v.World[turret]);
                    float side = prop.Yaw + (rng.Next() < 0.5f ? 1.4f : -1.4f);
                    var lying = v.Pos + new Vector3(Mathf.Sin(side), 0f, Mathf.Cos(side)) * (model.HalfLength + 1.2f);
                    lying.y = Ground(lying.x, lying.z) + 0.2f;
                    d.World = Matrix4x4.TRS(lying, Quaternion.Euler(rng.Range(-12f, 12f), rng.Range(0f, 360f), rng.Range(-20f, 20f)), Vector3.one);
                    d.Resting = true; d.Vel = Vector3.zero;
                }
                if (stage >= WreckStageRules.Broken) { v.Hidden = WreckStageRules.Hidden(CarcassOf(model, 0), stage, 1f); for (int p = 1; p < v.Off.Length; p++) v.Off[p] = true; }
                if (stage == WreckStageRules.Scrap) v.Squash = WreckStageRules.ScrapHeight;
                wrecks.Add(v);
            }
        }
    }
}
