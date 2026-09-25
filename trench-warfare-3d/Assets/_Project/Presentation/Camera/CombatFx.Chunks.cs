// Phase: B1 / C4 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools):
// thrown chunks: dirt, splinters, smoke balls, sparks, water, brass drawn as instanced cubes and puffs, and a dud
// cooking off. Pool: chunks (MaxChunks, MaxAmbientChunks). Wires: SceneHooks.CookOff (set in Start).
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        struct Chunk { public Vector3 Pos, Vel; public float Born, Life, Size; public byte Kind; }   // 0 dirt, 1 splinter, 2 smoke, 3 spark (night), 4 water, 5 brass, 6 helmet, 7 vapour
        readonly List<Chunk> chunks = new List<Chunk>(768);
        const int MaxChunks = 940;
        const int MaxAmbientChunks = 300;   // kinds 2, 5, 7: rifle smoke, breath, exhaust, crater steam
        int ambientChunks;                  // counted in DrawChunks, so Throw never has to scan the pool
        /// <summary>A shell going off where it lay (SceneHooks.CookOff): a shell burst's flash, fire and smoke at a fraction of
        /// its size, its sparks and clods, and the kick of it. No column: it was lying on the ground, not buried by its fall.</summary>
        void CookOff(Vector3 p, float radius)
        {
            if (Host == null || Host.Local == null) return;
            p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
            float r = Mathf.Clamp(radius, 0.8f, 4f);
            if (books != null && books.Ready)
            {
                Vector4 wind = Shader.GetGlobalVector(WindGlobalId); Vector3 drift = new Vector3(wind.x, 0f, wind.y) * 3.5f + Vector3.up * 0.55f;
                books.Add(FlipbookFx.Book.Flash, p + Vector3.up * (r * 0.3f), r * 3.2f, 0.16f, roll: UnityEngine.Random.value * 6.2832f, glow: (SceneMood.Night ? 7f : 2.5f) * SceneTints.Now.Glow, pop: 0.5f);
                books.Add(FlipbookFx.Book.Burst, p + Vector3.up * (r * 0.5f), r * 2.4f, 1.4f, FlipbookFx.Kind.Upright,
                    velocity: Vector3.up * (r * 0.5f) + drift, grow: 0.5f, roll: UnityEngine.Random.Range(-0.15f, 0.15f), glow: (SceneMood.Night ? 3.4f : 1.6f) * SceneTints.Now.Glow, pop: 0.3f);
                for (int k = 0; k < 3; k++)
                    books.Add(FlipbookFx.Book.Smoke, p + new Vector3(UnityEngine.Random.Range(-0.4f, 0.4f), 0.3f + k * 0.2f, UnityEngine.Random.Range(-0.4f, 0.4f)) * r, r * UnityEngine.Random.Range(1.1f, 1.5f), UnityEngine.Random.Range(3.5f, 5f),
                        (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None, velocity: drift * 1.6f + Vector3.up * 0.4f, grow: 2.2f, alpha: 0.6f, pop: 0.3f, delay: 0.3f + k * 0.15f);
            }
            else if (bursts.Count < 64) bursts.Add(new Burst { Pos = p, Radius = r, Born = Time.time, Variant = (Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 3 });
            Throw(p + Vector3.up * 0.3f, 14, 3, 7f, 0.05f);   // sparks
            if (debris != null && debris.Ready)
                debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.3f, Mathf.RoundToInt(5f + r * 2f), 6f + r, 0.14f, Mud, 30f, 0f, 1.8f, default, (uint)(p.x * 131f + p.z * 17f));
            SceneHooks.Flash?.Invoke(p + Vector3.up * 0.8f, new Color(1f, 0.62f, 0.3f), 6f, r * 5f, 0.25f);
            Startle(p);
            CameraShake.Add(p, r * 1.5f);
        }

        /// <summary>Throw debris: dirt and splinters fly and fall, smoke rises, swells and thins.</summary>
        void Throw(Vector3 at, int count, byte kind, float speed, float size, Vector3 bias = default)
        {
            bool ambient = kind == 2 || kind == 5 || kind == 7;
            if (ambient && ambientChunks >= MaxAmbientChunks) return;
            for (int k = 0; k < count && chunks.Count < MaxChunks && (!ambient || ambientChunks + k < MaxAmbientChunks); k++)
            {
                // the cone leans up, not out, and dirt lives long enough to come down again (gravity stays at 9.8: floaty reads as cheap)
                Vector3 dir = UnityEngine.Random.onUnitSphere; dir.y = Mathf.Abs(dir.y) * (kind == 2 ? 0.4f : 2.2f) + (kind == 2 ? 0.2f : 0.45f);
                dir += bias;   // a directional burst throws its dirt on down the line; bias is zero for everything else
                // every chunk lives exactly its own arc (2 vy / g, from the speed it actually got), so none is deleted at
                // the top of its flight and none goes on sinking through the mud after it lands; smoke and sparks keep
                // their own clocks. The spread in the cone and in the 0.5-1.2 gives the variety, so no extra jitter.
                Vector3 vel = dir.normalized * speed * UnityEngine.Random.Range(0.5f, 1.2f);
                chunks.Add(new Chunk { Pos = at, Vel = vel, Born = Time.time, Life = kind == 2 ? UnityEngine.Random.Range(3.5f, 6f) : kind == 3 ? UnityEngine.Random.Range(0.45f, 1.1f) : 2f * Mathf.Max(0f, vel.y) / 9.8f + 0.45f,
                    Size = size * UnityEngine.Random.Range(0.6f, 1.5f), Kind = kind });
            }
        }

        void DrawChunks(float now, Bounds bounds)
        {
            if (smokeMat == null) smokeMat = Transparent(Shader.Find("Universal Render Pipeline/Unlit"), new Color(0.16f, 0.15f, 0.14f, 0.30f));
            Prune(chunks, now, static (c, at) => at - c.Born > c.Life);
            float dt = Time.deltaTime; int landings = 0;
            ambientChunks = 0;
            for (int i = 0; i < chunks.Count; i++) { byte k = chunks[i].Kind; if (k == 2 || k == 5 || k == 7) ambientChunks++; }
            Vector4 wv = Shader.GetGlobalVector(WindGlobalId);   // _TWWind: the breeze at 0.034 per m/s (Atmosphere), as the drawn smoke reads it
            Vector3 downwind = new Vector3(wv.x, 0f, wv.y) * 6f;
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind == 2) { c.Vel = Vector3.Lerp(c.Vel, downwind + Vector3.up * 1.2f, dt * 1.5f); }   // drifts up and down wind
                else if (c.Kind == 7) { c.Vel = Vector3.Lerp(c.Vel, downwind * 0.45f + Vector3.up * 0.45f, dt * 1.2f); }
                else c.Vel += Vector3.down * 9.8f * dt;
                c.Pos += c.Vel * dt;
                if (c.Kind == 0 && c.Size > 0.12f && c.Vel.y < -2f && books != null && books.Ready && landings < 6)
                {
                    // a big clod thrown by a burst lands: a little spurt of dust where it hits, and it is done
                    float floor = RenderGround.Sample(Host.Local.Map, c.Pos.x, c.Pos.z);
                    if (c.Pos.y <= floor + 0.05f)
                    {
                        landings++;
                        if (SceneHooks.IsWater != null && SceneHooks.IsWater(c.Pos.x, c.Pos.z)) SceneHooks.AddRing?.Invoke(c.Pos.x, c.Pos.z, 0.5f);
                        else books.Add(FlipbookFx.Book.Spurt, new Vector3(c.Pos.x, floor, c.Pos.z), c.Size * 3.5f, 0.35f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | ((i & 1) == 0 ? FlipbookFx.Kind.Mirror : 0), alpha: 0.7f, pop: 0.3f);
                        c.Life = 0f;
                    }
                }
                if (c.Kind == 5 || c.Kind == 6)
                {
                    float floor = RenderGround.Sample(Host.Local.Map, c.Pos.x, c.Pos.z) + (c.Kind == 5 ? 0.012f : 0.06f);
                    if (c.Pos.y <= floor && c.Vel.y < 0f)
                    {
                        c.Pos.y = floor;
                        if (c.Vel.y < -1.4f) c.Vel = new Vector3(c.Vel.x * 0.45f, -c.Vel.y * 0.32f, c.Vel.z * 0.45f);   // one bounce
                        else
                        {
                            bool water = SceneHooks.IsWater != null && SceneHooks.IsWater(c.Pos.x, c.Pos.z);
                            if (water) SceneHooks.AddRing?.Invoke(c.Pos.x, c.Pos.z, c.Kind == 5 ? 0.35f : 0.9f);
                            else if (c.Kind == 5) AddRest(Matrix4x4.TRS(c.Pos, Lie(c.Pos.x, c.Pos.z, c.Born * 733f, 0.15f), new Vector3(0.020f, 0.020f, 0.085f)), 9f, 0);
                            else AddRest(Matrix4x4.TRS(c.Pos + Vector3.up * 0.02f, Lie(c.Pos.x, c.Pos.z, c.Born * 733f) * Quaternion.Euler(UnityEngine.Random.Range(-16f, 16f), 0f, UnityEngine.Random.Range(150f, 210f)), new Vector3(0.33f, 0.15f, 0.35f)), 150f, 1);
                            c.Life = 0f;
                        }
                    }
                }
                chunks[i] = c;
            }
            // the burst's low smoke is a ball: from the standard view it thickens the drawn cloud, from close by it was a glass
            // sphere with a hard rim (seen in Play). With the drawn cloud there it goes a shade fainter a quarter of the way
            // in, and is gone among the men.
            int thin = books != null && books.Ready ? Mathf.FloorToInt(Mathf.Clamp01(SceneHooks.CloseUp) * 3.99f) : 0;
            for (int pass = 0; pass < 5; pass++)
            {
                byte kind = (byte)Mathf.Min(pass, 2);
                batch.Clear();
                var rp = new RenderParams(pass == 0 ? dirtMat : pass == 1 ? woodMat : pass == 2 ? smokeMat : pass == 3 ? smokeThin : smokeFaint) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < chunks.Count; i++)
                {
                    var c = chunks[i];
                    if (c.Kind != kind) continue;
                    float k = (now - c.Born) / c.Life;
                    if (kind == 2 && Mathf.Min(2, Mathf.FloorToInt(k * 3f)) + thin != pass - 2) continue;
                    float s = kind == 2 ? c.Size * (1f + 2f * k) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.55f, 1f, k))) : c.Size;
                    var rot = kind == 2 ? Quaternion.identity : Quaternion.Euler(c.Born * 997f + now * 300f, c.Born * 613f, now * 200f);
                    batch.Add(Matrix4x4.TRS(c.Pos, rot, kind == 1 ? new Vector3(s * 0.4f, s * 0.4f, s * 3f) : new Vector3(s, s, s)));
                    if (batch.Count == 1023) Flush(kind == 2 ? puff : kind == 0 ? clod : cube, rp);
                }
                if (batch.Count > 0) Flush(kind == 2 ? puff : kind == 0 ? clod : cube, rp);
            }
            // water thrown up by rounds, shells and boots: pale drops under gravity
            batch.Clear();
            // the biome may have loaded after this material was built, and it costs one comparison a frame
            var rpW = new RenderParams(waterMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind != 4) continue;
                float s = c.Size * (1f - 0.6f * (now - c.Born) / c.Life);
                batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.identity, new Vector3(s, s * 1.6f, s)));
                if (batch.Count == 1023) Flush(sphere, rpW);
            }
            if (batch.Count > 0) Flush(sphere, rpW);
            // in flight: brass cases and helmets tumble; breath, muzzle threads and crater steam are a pale vapour
            for (int kind = 5; kind <= 7; kind++)
            {
                batch.Clear();
                var rpC = new RenderParams(kind == 5 ? brassMat : kind == 6 ? helmetMat : vapourMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < chunks.Count; i++)
                {
                    var c = chunks[i];
                    if (c.Kind != kind) continue;
                    float k = (now - c.Born) / Mathf.Max(0.01f, c.Life);
                    if (kind == 7) { float s = c.Size * (1f + 3.2f * k) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.45f, 1f, k))); batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.identity, new Vector3(s, s, s))); }
                    else batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.Euler(now * 640f + c.Born * 997f, c.Born * 613f, now * 410f), kind == 5 ? new Vector3(0.020f, 0.020f, 0.085f) : new Vector3(0.33f, 0.15f, 0.35f)));
                    if (batch.Count == 1023) Flush(kind == 5 ? cube : kind == 6 ? sphere : puff, rpC);
                }
                if (batch.Count > 0) Flush(kind == 5 ? cube : kind == 6 ? sphere : puff, rpC);
            }
            DrawBirds(now, bounds);
            Ambient(now);
            var lens = Camera.main; if (lens != null) CloseLife(now, lens);
            impactsThisFrame = 0;
            // sparks: a bright streak along its own flight, shrinking as it burns out
            batch.Clear();
            var rpS = new RenderParams(sparkMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind != 3 || c.Vel.sqrMagnitude < .01f) continue;
                float burn = 1f - (now - c.Born) / c.Life;
                batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.LookRotation(c.Vel), new Vector3(c.Size * burn, c.Size * burn, c.Size + c.Vel.magnitude * .035f)));
                if (batch.Count == 1023) Flush(cube, rpS);
            }
            if (batch.Count > 0) Flush(cube, rpS);
        }
    }
}
