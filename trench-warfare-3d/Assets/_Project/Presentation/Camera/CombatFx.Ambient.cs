// Phase: B1 / C4 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools):
// birds put up by shell bursts, and the ambient smoke of the field. Pool: birds (MaxBirds).
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        struct Bird { public Vector3 Pos, Vel; public float Born, Phase; }
        readonly List<Bird> birds = new List<Bird>();
        const int MaxBirds = 80; const float BirdLife = 8f;
        /// <summary>A shell burst puts up the crows from the nearest standing timber: they climb away from the blast and are gone.</summary>
        void Startle(Vector3 burst)
        {
            if (Time.time - lastFlock < 0.8f || birds.Count > MaxBirds - 10) return;
            var props = Host.Local.Map.Props;
            int best = -1; float bestSq = 45f * 45f;
            for (int i = 0; i < props.Length; i++)
            {
                var kind = props[i].Kind;
                if (kind != TW.Sim.Terrain.PropKind.Tree && kind != TW.Sim.Terrain.PropKind.BrokenTree) continue;
                float dx = props[i].Pos.x - burst.x, dz = props[i].Pos.z - burst.z, sq = dx * dx + dz * dz;
                if (sq < bestSq && sq > 9f) { bestSq = sq; best = i; }
            }
            if (best < 0) return;
            lastFlock = Time.time;
            Vector3 perch = new Vector3(props[best].Pos.x, RenderGround.Sample(Host.Local.Map, props[best].Pos.x, props[best].Pos.z) + 4.5f, props[best].Pos.z);
            Vector3 away = perch - burst; away.y = 0f; away = away.sqrMagnitude > 0.01f ? away.normalized : Vector3.forward;
            int flock = UnityEngine.Random.Range(5, 10);
            for (int k = 0; k < flock; k++)
            {
                Vector3 dir = Quaternion.Euler(0f, UnityEngine.Random.Range(-40f, 40f), 0f) * away;
                birds.Add(new Bird { Pos = perch + UnityEngine.Random.insideUnitSphere * 1.2f, Vel = dir * UnityEngine.Random.Range(6f, 10f) + Vector3.up * UnityEngine.Random.Range(3f, 6f),
                    Born = Time.time + k * 0.06f, Phase = UnityEngine.Random.value * 6.28f });
            }
        }

        void DrawBirds(float now, Bounds bounds)
        {
            Prune(birds, now - BirdLife, static (b, cut) => b.Born < cut);
            if (birds.Count == 0) return;
            float dt = Time.deltaTime;
            batch.Clear();
            var rp = new RenderParams(birdMat) { worldBounds = new Bounds(bounds.center, bounds.size + new Vector3(200f, 120f, 200f)), shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < birds.Count; i++)
            {
                var b = birds[i];
                float age = now - b.Born; if (age < 0f) continue;
                b.Vel = Vector3.Lerp(b.Vel, new Vector3(b.Vel.x, 1.2f, b.Vel.z).normalized * 9f, dt * 0.8f);   // the climb flattens into flight
                b.Pos += b.Vel * dt; birds[i] = b;
                float flap = Mathf.Sin(age * 15f + b.Phase) * 48f, size = Mathf.Clamp01((BirdLife - age) * 0.7f);
                var body = Quaternion.LookRotation(b.Vel);
                for (int wing = -1; wing <= 1; wing += 2)
                    batch.Add(Matrix4x4.TRS(b.Pos, body * Quaternion.Euler(0f, 0f, wing * flap), Vector3.one) * Matrix4x4.TRS(new Vector3(wing * 0.2f, 0f, 0f), Quaternion.identity, new Vector3(0.40f, 0.025f, 0.15f) * size));
                batch.Add(Matrix4x4.TRS(b.Pos, body, new Vector3(0.07f, 0.07f, 0.30f) * size));
                if (batch.Count >= 1020) Flush(cube, rp);
            }
            if (batch.Count > 0) Flush(cube, rp);
        }

        /// <summary>Running men kick up mud, and chimneys and rained-on fires smoke: a few small chunks a step, near the view only.</summary>
        void Ambient(float now)
        {
            var cam = Camera.main; if (cam == null) return;
            Vector3 look = cam.transform.position + cam.transform.forward * (cam.transform.position.y / Mathf.Max(0.15f, -cam.transform.forward.y));
            if (now >= nextKick)
            {
                nextKick = now + 0.2f;
                var w = Host.Local.World; int found = 0;
                for (int n = 0; n < w.HighWater && n < 300 && found < 5 && chunks.Count < 480; n++)
                {
                    int i = (kickCursor + n) % w.HighWater;
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    var v = w.Velocity[i]; if (v.x * v.x + v.z * v.z < 4f) continue;
                    var p = w.Position[i]; float dx = p.x - look.x, dz = p.z - look.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                    Vector3 at = new Vector3(p.x, RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.05f, p.z);
                    if (SceneHooks.IsWater != null && SceneHooks.IsWater(at.x, at.z)) Throw(at + Vector3.up * 0.3f, 1, 4, 2.2f, 0.04f); else Throw(at, 1, 0, 1.7f, 0.045f);
                    found++; kickCursor = i + 1;
                }
                if (found < 5 && w.HighWater > 0) kickCursor = (kickCursor + 300) % w.HighWater;
            }
            if (now >= nextSmoke)
            {
                nextSmoke = now + 0.45f;
                for (int s = 0; s < SceneHooks.SmokeSources.Count && chunks.Count < 460; s++)
                {
                    Vector3 at = SceneHooks.SmokeSources[s];
                    if ((at - look).sqrMagnitude > 110f * 110f) continue;
                    chunks.Add(new Chunk { Pos = at, Vel = new Vector3(UnityEngine.Random.Range(-0.2f, 0.2f), 0.9f, UnityEngine.Random.Range(-0.2f, 0.2f)), Born = now, Life = UnityEngine.Random.Range(2.4f, 3.6f), Size = 0.22f, Kind = 2 });
                }
            }
        }
    }
}
