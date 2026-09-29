// Phase: deaths (2026-09-28, implemented) — part of TankRenderer: a dead Salvo's last rockets fizz off (fx.deathAbsurd
// above 0; TankRenderer.Deaths starts them). Four to seven leave the rack out of their own tubes (Socket_Tube##), the
// rack riding the turret as it leaps, a beat apart, and loop off on wandering corkscrews (VehicleGags.FizzStep), skipping
// off the ground, until each pops in the air: a flash, a star, a puff, sparks. They are the renderer's alone: the sim
// fired nothing, they hurt no one, and one that strays FizzReach from where it left pops there. Drawn as the rack's
// flying rockets are (TankRenderer.Salvo): the rocket body in this frame's batches, the motor's flare, a smoke ribbon.
// Seeded by where the machine died (DebrisRng), never UnityEngine.Random.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        sealed class Fizzing
        {
            public View Owner; public Debris Rack;     // the rack's piece in the air (null: it stayed on the hull)
            public Matrix4x4 Frame;                    // the rack's frame at death, when it stayed on the hull
            public Vector3 Mouth, Out;                 // its tube's mouth and way, in the rack's frame
            public Vector3 Pos, Vel, Axis, Home;
            public float Turn, Wander, LaunchAt, PopAt, NextPoint, NextPuff;
            public bool Flying;
            public Ribbon Trail;
        }

        readonly List<Fizzing> fizzers = new List<Fizzing>(16);
        TW.Sim.Match.MatchSim fizzMatch;
        /// <summary>Seconds between the points of a fizzer's smoke (its loops are too tight for RibbonStep's metres).</summary>
        const float FizzPointEvery = 0.05f;
        /// <summary>Seconds between the puffs a fizzer leaves (critic round 1: its ribbon alone did not read by day).</summary>
        const float FizzPuffEvery = 0.07f;
        const float FizzScale = 1.4f;       // a fizzer's body against a flying rocket's (bigger: it must read by day)
        /// <summary>Seconds after the death before the first can leave: the first stills lost them in the fireball.</summary>
        const float FizzAfter = 0.5f;

        /// <summary>The rack's last rockets, as the machine dies (TankRenderer.Deaths): which tubes, and when each leaves.</summary>
        void Fizzers(View v, int top, ref DebrisRng rng, float a, float now)
        {
            var m = v.Model;
            if (!m.IsRack || m.Tubes == null || m.Tubes.Length == 0 || m.GunPart[0] < 0) return;
            int n = VehicleGags.FizzCount(a, m.Tubes.Length, rng.Next());
            Debris rack = null;
            if (top >= 0) foreach (var p in v.Pieces) if (p.Part == top) rack = p;
            Matrix4x4 frame = rack != null ? rack.World : v.World[m.GunPart[0]];
            var inverse = frame.inverse;
            int first = (int)(rng.Next() * m.Tubes.Length);
            for (int k = 0; k < n; k++)
            {
                int tube = (first + k * 5) % m.Tubes.Length;   // spread over the rack
                Vector3 mouth = TubeMouth(v, tube, out Vector3 dir);
                var f = VehicleGags.Fizz(dir, rng.Next(), rng.Next(), rng.Next(), rng.Next(), rng.Next());
                fizzers.Add(new Fizzing
                {
                    Owner = v, Rack = rack, Frame = frame, Mouth = inverse.MultiplyPoint3x4(mouth), Out = inverse.MultiplyVector(dir),
                    Axis = f.Axis, Turn = f.Turn, Wander = f.Wander, LaunchAt = now + FizzAfter + f.Delay, PopAt = now + FizzAfter + f.Delay + f.Life,
                });
            }
        }

        /// <summary>Once a frame (GagsFrame): each fizzer leaves, flies and pops.</summary>
        void FizzersFrame(float dt, float now)
        {
            if (fizzMatch != lastMatch) { fizzers.Clear(); fizzMatch = lastMatch; }   // a new match: its trails went back to the pool
            if (fizzers.Count == 0) return;
            if (rocketMesh == null) BuildRocket();
            bool fx = books != null && books.Ready;
            var cam = Camera.main;
            for (int i = fizzers.Count - 1; i >= 0; i--)
            {
                var f = fizzers[i];
                if (!f.Flying)
                {
                    if (now < f.LaunchAt) continue;
                    var frame = f.Rack != null ? f.Rack.World : f.Frame;
                    f.Pos = f.Home = frame.MultiplyPoint3x4(f.Mouth);
                    Vector3 dir = frame.MultiplyVector(f.Out);
                    f.Vel = (dir.sqrMagnitude > 1e-6f ? dir.normalized : Vector3.up) * VehicleGags.FizzSpeed;
                    f.Flying = true;
                    f.Trail = ribbonPool.Count > 0 ? ribbonPool[ribbonPool.Count - 1] : new Ribbon();
                    if (ribbonPool.Count > 0) ribbonPool.RemoveAt(ribbonPool.Count - 1);
                    f.Trail.P.Clear(); f.Trail.T.Clear(); f.Trail.Live = true;
                    f.Trail.P.Add(f.Pos); f.Trail.T.Add(now);
                    ribbons.Add(f.Trail);
                    f.NextPoint = now + FizzPointEvery;
                    if (fx) books.Add(FlipbookFx.Book.Flash, f.Pos, 1.4f, 0.08f, roll: f.Turn, glow: SceneMood.Night ? 3f : 1.8f);
                }
                bool near = VehicleGags.FizzStep(ref f.Pos, ref f.Vel, ref f.Axis, f.Turn, f.Wander, dt, Ground(f.Pos.x, f.Pos.z), f.Home);
                if (!near || now >= f.PopAt)
                {
                    f.Pos += VehicleGags.Helix(f.Vel.sqrMagnitude > 1e-6f ? f.Vel.normalized : Vector3.up, now - f.LaunchAt, f.Turn, out _);
                    FizzOut(f, fx, now);
                    fizzers.RemoveAt(i);
                    continue;
                }
                Vector3 path = f.Vel.sqrMagnitude > 1e-6f ? f.Vel.normalized : Vector3.up;
                // drawn on its corkscrew about the path (VehicleGags.Helix)
                Vector3 off = VehicleGags.Helix(path, now - f.LaunchAt, f.Turn, out Vector3 turning);
                Vector3 at = f.Pos + off, spun = f.Vel + turning;
                Vector3 way = spun.sqrMagnitude > 1e-6f ? spun.normalized : path;
                Vector3 tail = at - way * (RocketLength * FizzScale);
                if (rocketMesh != null && rocketMat != null)
                    Queue(rocketMesh, rocketMat, Matrix4x4.TRS(tail, Quaternion.LookRotation(way), Vector3.one * FizzScale), 0f, Vector4.zero, new Vector4(1f, 1f, 1f, 0f));
                if (fx)
                {
                    float roll = cam != null ? FlipbookFx.ScreenRoll(cam, -way) : 0f;
                    books.Add(FlipbookFx.Book.Muzzle, tail - way * 0.5f, 1.6f, 0.05f, roll: roll, glow: SceneMood.Night ? 3f : 2.6f);
                }
                if (now >= f.NextPoint) { f.Trail.P.Add(tail); f.Trail.T.Add(now); f.NextPoint = now + FizzPointEvery; }
                if (fx && now >= f.NextPuff)
                {
                    f.NextPuff = now + FizzPuffEvery;
                    books.Add(FlipbookFx.Book.Smoke, tail, 1.3f, 1.6f, velocity: Vector3.up * 0.3f, grow: 1.4f, alpha: 0.75f);   // dark enough for snow (round 2)
                }
            }
        }

        /// <summary>A fizzer's end: a small burst in the air, and its smoke left to fade.</summary>
        void FizzOut(Fizzing f, bool fx, float now)
        {
            if (f.Trail != null) { f.Trail.P.Add(f.Pos); f.Trail.T.Add(now); f.Trail.Live = false; }
            if (!fx) return;
            float spin = f.Turn * 1.3f;
            books.Add(FlipbookFx.Book.Flash, f.Pos, 3.0f, 0.25f, roll: spin, glow: SceneMood.Night ? 3.5f : 2.6f, pop: 0.4f);
            books.Add(FlipbookFx.Book.Star, f.Pos, 2.4f, 0.3f, roll: spin * 2f, glow: 2.6f);
            books.Add(FlipbookFx.Book.Smoke, f.Pos, 2.4f, 2.6f, velocity: Vector3.up * 0.6f, grow: 1.4f, alpha: 0.7f);   // it lingers: a still can catch it
            SceneHooks.Sparks?.Invoke(f.Pos, 8);
        }

        /// <summary>For the capture tools and the tests: fizzers waiting or in the air.</summary>
        public int FizzerCount => fizzers.Count;
    }
}
