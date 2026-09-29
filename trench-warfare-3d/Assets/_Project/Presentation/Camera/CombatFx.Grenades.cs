// Phase: A2 look (2026-09-29) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools): the
// hand grenade. The sim (DirectFireSystem, replay v21) keeps a thrown bomb in the air CombatTables.GrenadeFlightTicks
// and sets it off where it lands; GrenadeThrown (a thrower, b his target, pos his feet, dir the flight, scalar metres)
// comes first. Here the bomb is a dark lump drawn each frame on its arc from his hand to where the burst will be, with
// its fuse sputtering sparks behind it (the lump alone was not to be seen on the night field, in Play). The arc runs on
// the sim's clock (SimNow), so it lands with the burst at any speed and holds when paused: timed by the wall clock, at
// a tenth of the speed it was a lob a hundred metres high. The burst is a grenade's: a sharp flash, a spurt of earth
// and a puff of smoke, a few clods, a small kick. It was drawn as a shell (a column, wings, a rolling cloud) on the tick
// of the throw, at the shell's size for its 4.5 m radius. The man's arm is AnimationController's (Clip.Throw).
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>A bomb in the air: where it left the hand, its launch velocity, and the sim seconds it left and flies.</summary>
        struct Bomb { public Vector3 From, Velocity; public float Born, Air, Size; }
        readonly System.Collections.Generic.List<Bomb> bombs = new System.Collections.Generic.List<Bomb>(16);

        /// <summary>Where a bomb thrown from <paramref name="from"/> at <paramref name="velocity"/> is <paramref name="t"/> seconds on.</summary>
        public static Vector3 BombAt(Vector3 from, Vector3 velocity, float t) => from + velocity * t + Vector3.down * (0.5f * DebrisMath.Gravity * t * t);

        /// <summary>The velocity that takes a bomb from <paramref name="from"/> to <paramref name="to"/> in <paramref name="air"/> seconds.</summary>
        public static Vector3 BombVelocity(Vector3 from, Vector3 to, float air)
            => new Vector3((to.x - from.x) / air, (to.y - from.y + 0.5f * DebrisMath.Gravity * air * air) / air, (to.z - from.z) / air);

        /// <summary>Each bomb in the air, where it is by the sim's clock: the lump for this frame, and a spark off its fuse.</summary>
        void DrawBombs()
        {
            if (bombs.Count == 0) return;
            float now = SimNow;
            for (int k = bombs.Count - 1; k >= 0; k--)
            {
                var b = bombs[k];
                float t = now - b.Born;
                if (t > b.Air) { bombs.RemoveAt(k); continue; }
                if (t < 0f || chunks.Count >= MaxChunks - 2) continue;
                Vector3 at = BombAt(b.From, b.Velocity, t);
                chunks.Add(new Chunk { Pos = at, Vel = Vector3.zero, Born = Time.time, Life = 0f, Size = b.Size, Kind = 0 });   // drawn this frame only
                chunks.Add(new Chunk { Pos = at, Vel = UnityEngine.Random.insideUnitSphere * 0.8f + Vector3.up * 0.4f, Born = Time.time, Life = UnityEngine.Random.Range(0.15f, 0.35f), Size = 0.05f, Kind = 3 });
            }
        }

        /// <summary>The bomb leaves his hand for where the sim will set it off, and comes down there as it does.</summary>
        void OnGrenadeThrown(SimEvent e)
        {
            if (Host == null || Host.Local == null || bombs.Count >= 64) return;
            var w = Host.Local.World;
            var map = Host.Local.Map;
            float scale = FigureScale();
            Vector3 flight = new Vector3(e.Dir.x, 0f, e.Dir.z);
            Vector3 to = (Vector3)e.Pos + flight;
            to.y = RenderGround.Sample(map, to.x, to.z);
            Vector3 feet = e.A >= 0 && e.A < w.HighWater && Host.Presenter != null ? (Vector3)Host.Presenter.Drawn(e.A) : (Vector3)e.Pos;
            Vector3 way = flight.sqrMagnitude > 1e-4f ? flight.normalized : Vector3.forward;
            // over his shoulder, a little ahead of him: where the arm lets go
            Vector3 hand = new Vector3(feet.x, RenderGround.Sample(map, feet.x, feet.z) + 1.7f * scale, feet.z) + way * (0.3f * scale);
            float air = CombatTables.GrenadeFlightTicks(e.Scalar, w.Config.TickSeconds) * w.Config.TickSeconds;
            bombs.Add(new Bomb { From = hand, Velocity = BombVelocity(hand, to, air), Born = e.Tick * w.Config.TickSeconds, Air = air, Size = 0.14f * scale });
        }

        /// <summary>A grenade's burst, in place of a shell's. True: drawn.</summary>
        bool GrenadeBurst(SimEvent e, Vector3 p)
        {
            if (books != null && books.Ready)
            {
                float closeUp = SceneHooks.CloseUp;
                Vector4 wind = Shader.GetGlobalVector(WindGlobalId);
                Vector3 drift = new Vector3(wind.x, 0f, wind.y) * 3.5f;
                books.Add(FlipbookFx.Book.Flash, p + Vector3.up * 0.5f, Mathf.Lerp(3.4f, 2.2f, closeUp), 0.12f, roll: UnityEngine.Random.value * 6.2832f,
                    glow: (SceneMood.Night ? 6f : 2.2f) * SceneTints.Now.Glow, pop: 0.5f);
                bool mirror = ((Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 1) == 0;
                books.Add(FlipbookFx.Book.Spurt, p, 2.6f, 0.6f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0),
                    velocity: Vector3.up * 1.5f, grow: 0.4f, alpha: 0.9f, pop: 0.3f);
                books.Add(FlipbookFx.Book.Puff, p + Vector3.up * 0.7f, 3.4f, 1.8f, FlipbookFx.Kind.Upright | (mirror ? 0 : FlipbookFx.Kind.Mirror),
                    velocity: Vector3.up * 0.9f + drift, grow: 0.9f, alpha: 0.75f, pop: 0.3f);
            }
            if (debris != null && debris.Ready)
                debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.2f, 6, 7f, 0.1f, Mud, 12f, 0f, 1.6f, default, e.Tick);
            if (SceneMood.Night) Throw(p + Vector3.up * 0.3f, 10, 3, 12f, 0.045f);   // a few sparks
            lastBlast = p; lastBlastAt = Time.time;   // the dead are thrown away from it (CombatFx.Bodies)
            Startle(p);
            CameraShake.Add(p, 1.2f);
            return true;
        }
    }
}
