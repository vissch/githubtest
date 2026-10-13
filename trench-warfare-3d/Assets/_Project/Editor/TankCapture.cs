// Phase: A5b (tooling) — depends on: SimHost, TacticalCamera, TankRenderer, VehicleModulesSystem, TankGunnerySystem
// Editor-only helpers for looking at the tanks in Play, driven from the command line (unity command eval):
//  - Spawn: put a tank (or a man) at an exact place in BOTH lockstep worlds at once (Local and Peer step the same
//    code, so doing it to both keeps them in step; tests do the same to one world). Every change to the worlds first
//    brings them to the same tick (SimHost.AlignWorlds): between frames one can be a tick ahead;
//  - Silver: top both worlds up so deploys are not rejected;
//  - Follow: frame one slot every frame (FrameFrom with a yaw), so a moving tank stays in the picture;
//  - Shot: write the main camera's picture to a PNG at the end of the frame, after the tanks and the effects were drawn;
//  - Status: one line per tank (slot, kind, team, position, state, crew, fire, structure, modules, guns).
// None of this is part of the game; it exists for the capture-and-critique loop.
using System.IO;
using System.Text;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Units;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Editor
{
    public static class TankCapture
    {
        [DefaultExecutionOrder(30000)]
        public sealed class Shooter : MonoBehaviour
        {
            public string Pending; public int W = 1600, H = 900;
            public int FollowSlot = -1; public float FollowZoom = 22f, FollowBearing = 30f;
            public bool FollowFacing;              // the bearing is worked out from the subject's own facing, every frame
            public int FollowGeneration = -1;      // the spawn this slot held when Follow was called
            public Vector2 Held;                   // the last place the subject was really seen (a wreck stays framed)
            public bool HasHeld;
            public string Last;
            public string TracePath; public int TraceFrame; System.Text.StringBuilder trace;

            void LateUpdate()
            {
                var host = FindFirstObjectByType<SimHost>();
                var cam = FindFirstObjectByType<TacticalCamera>();
                float facing = 0f, bearing = FollowBearing; bool onSubject = false;
                if (FollowSlot >= 0 && host != null && cam != null && host.Local != null)
                {
                    var w = host.Local.World;
                    // Follow the UNIT, not the slot number: a dead man's slot is handed out again (SimWorld.Spawn bumps
                    // Generation), and Drawn returns zero (the map corner) for a slot past the presenter's count. Either
                    // one used to cut the film to a stranger or to an empty beach. When the subject is gone we hold the
                    // last place we really saw it, so its wreck stays in the frame.
                    bool sameUnit = FollowSlot < w.HighWater && w.IsAlive(FollowSlot) && (FollowGeneration < 0 || w.Generation[FollowSlot] == FollowGeneration);
                    var p = host.Presenter.Drawn(FollowSlot);
                    bool drawn = new Vector2(p.x, p.z).sqrMagnitude > 1f;   // not within 1 m of (0,0)
                    onSubject = sameUnit && drawn;
                    if (onSubject) { Held = new Vector2(p.x, p.z); HasHeld = true; }
                    if (FollowSlot < w.HighWater) facing = w.Yaw[FollowSlot] * Mathf.Rad2Deg;
                    if (FollowFacing && onSubject) bearing = FilmAim.QuarterFront(facing);
                    if (HasHeld) cam.FrameFrom(Held, FollowZoom, FilmAim.YawFor(bearing, cam.BaseYaw, cam.AutoYaw));
                }
                lastFacing = facing; lastOnSubject = onSubject;
                if (string.IsNullOrEmpty(Pending) || Camera.main == null) return;
                var c = Camera.main;
                var rt = RenderTexture.GetTemporary(W, H, 24, RenderTextureFormat.ARGB32);
                var before = c.targetTexture;
                c.targetTexture = rt; c.Render(); c.targetTexture = before;
                var was = RenderTexture.active; RenderTexture.active = rt;
                var tex = new Texture2D(W, H, TextureFormat.RGB24, false);
                tex.ReadPixels(new Rect(0, 0, W, H), 0, 0); tex.Apply();
                RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);
                Directory.CreateDirectory(Path.GetDirectoryName(Pending));
                File.WriteAllBytes(Pending, tex.EncodeToPNG());
                Object.Destroy(tex);
                Last = Pending; Pending = null;
            }

            float lastFacing; bool lastOnSubject;

            // A trace row is written when the FILM takes a frame (gamefilm.py's recorder calls Mark right after it
            // renders the camera into its picture), not on every editor update: an update pass with no picture behind it
            // reads a camera matrix mid-ease that no frame of the film was ever taken with. Those stale samples said the
            // subject was far outside a frame that in truth held it steadily.
            public string Mark()
            {
                if (string.IsNullOrEmpty(TracePath) || Camera.main == null) return "not tracing";
                trace ??= new System.Text.StringBuilder();   // a domain reload empties it; the path is serialized
                var v = Camera.main.WorldToViewportPoint(new Vector3(Held.x, 1f, Held.y));
                // [A3] judge the guns from where the camera is REALLY looking, not the requested bearing: an
                // orbit's spin and FrameFrom's ease put the lens somewhere else by the time a frame is taken.
                float look = FilmAim.BearingOf(Camera.main.transform.forward);
                trace.Append($"{TraceFrame} {v.x:0.000} {v.y:0.000} {Held.x:0.0} {Held.y:0.0} {(lastOnSubject ? 1 : 0)} {(FilmAim.ShowsGuns(look, lastFacing) ? 1 : 0)}\n");
                TraceFrame++;
                if (trace.Length > 4096) { File.AppendAllText(TracePath, trace.ToString()); trace.Clear(); }
                return "marked";
            }

            public void BeginTrace() { trace = new System.Text.StringBuilder(); }

            public string EndTrace()
            {
                if (string.IsNullOrEmpty(TracePath)) return "not tracing";
                if (trace != null && trace.Length > 0) File.AppendAllText(TracePath, trace.ToString());
                trace = null; var p = TracePath; TracePath = null;
                return "traced " + TraceFrame + " frames " + p;
            }
        }

        static Shooter Get()
        {
            var s = Object.FindFirstObjectByType<Shooter>();
            // NOT HideFlags.DontSave: every `cmd eval` compiles and reloads the domain, and a DontSave object is
            // not carried through it. The Shooter died between the call that set Follow and the next one, so the
            // camera never followed anything and every trace read slot -1 at the map corner.
            if (s == null) s = new GameObject("TankCapture").AddComponent<Shooter>();
            return s;
        }

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        public static string Shot(string path, int w = 1600, int h = 900) { var s = Get(); s.W = w; s.H = h; s.Pending = path; return "queued " + path; }

        /// <summary>Frame one unit every frame. `bearing` is a WORLD bearing the camera looks along (FilmAim takes the
        /// camera's own two angles back out); with `onFacing` it is worked out from the subject's facing instead, so a
        /// machine is always seen from its front quarter and its guns are in the picture.</summary>
        public static string Follow(int slot, float zoom = 22f, float bearing = 30f, bool onFacing = false)
        {
            var cam = Object.FindFirstObjectByType<TacticalCamera>();
            if (cam != null) cam.HoldAuto = slot >= 0;
            var s = Get(); s.FollowSlot = slot; s.FollowZoom = zoom; s.FollowBearing = bearing; s.FollowFacing = onFacing;
            s.HasHeld = false; s.FollowGeneration = -1;
            var h = Host;
            if (h != null && h.Local != null && slot >= 0 && slot < h.Local.World.HighWater) s.FollowGeneration = h.Local.World.Generation[slot];
            return "following " + slot + " gen " + s.FollowGeneration;
        }

        /// <summary>One line per rendered frame into `path`: frame, viewport x/y of the subject, its world x/z, whether it
        /// is still the unit we started on, and whether the view shows its guns. The proof a film kept its subject.</summary>
        public static string Trace(string path)
        {
            var s = Get(); s.TracePath = path; s.TraceFrame = 0;
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            File.WriteAllText(path, "frame vx vy x z onsubject showsguns\n");
            s.BeginTrace();
            return "tracing " + path;
        }

        public static string TraceStop() { var s = Object.FindFirstObjectByType<Shooter>(); if (s == null) return "no shooter"; return s.EndTrace(); }

        public static string Silver(int amount = 100000)
        {
            var h = Host; if (h == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart (waiting on the network): try again";
            h.WriteWorlds(m => { for (int p = 0; p < SimConfig.MaxPlayers; p++) m.World.Silver[p] = amount; });   // one world, or both in the canary
            return "silver " + amount;
        }

        /// <summary>A unit at an exact place in both worlds, with the stats the match table gives its archetype.</summary>
        public static string Spawn(int team, int archetype, float x, float z, float yawDeg = -999f)
        {
            var h = Host; if (h == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart (waiting on the network): try again";
            // The table of THIS match, not a chain of ternaries: the chain listed six machines and stopped, so a Banner,
            // a Redoubt or a Breaker spawned here got a rifleman's hit points and nothing said so.
            var world = h.Local.World;
            bool vehicle = ChassisKind.IsArmoured(world.ChassisOf((byte)archetype));
            var entry = archetype >= 0 && archetype < Archetypes.Count ? world.Units.Roster[archetype] : default;
            if (entry.Hp <= 0f) entry = world.Roster[team * RosterEntry.SlotCount + Mathf.Clamp(archetype, 0, 3)];
            int a = h.Local.World.Spawn((byte)team, (byte)archetype, new Unity.Mathematics.float3(x, 0f, z), entry.Hp, entry.Speed, vehicle);
            int b = h.Peer != null ? h.Peer.World.Spawn((byte)team, (byte)archetype, new Unity.Mathematics.float3(x, 0f, z), entry.Hp, entry.Speed, vehicle) : a;   // the canary's second world, when it runs
            if (yawDeg > -900f) { h.Local.World.Yaw[a] = yawDeg * Mathf.Deg2Rad; if (h.Peer != null) h.Peer.World.Yaw[b] = yawDeg * Mathf.Deg2Rad; }
            return a == b ? "slot " + a : $"MISMATCH local {a} peer {b}";
        }

        /// <summary>Set something burning in both worlds (to watch a fire, a bail-out and a cook-off).</summary>
        public static string Ignite(int slot, float fire = 0.62f)
        {
            var h = Host; if (h == null || h.Local.Modules == null) return "no modules";
            if (!h.AlignWorlds()) return "worlds a tick apart (waiting on the network): try again";
            h.WriteWorlds(m => m.Modules.Fire[slot] = fire);
            return "fire " + fire;
        }

        public static string Status()
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var w = h.Local.World; var m = h.Local.Modules; var g = h.Local.Gunnery; var k = h.Local.Vehicles;
            var sb = new StringBuilder();
            sb.Append($"tick {w.Tick} desync {h.Desync} alive {w.AliveCount}\n");
            int M = (int)VehicleModule.Count;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) != ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) continue;
                sb.Append($"#{i} {TW.Presentation.UnitLook.Name(w.Archetype[i])} t{w.Team[i]} pos({w.Position[i].x:0.0},{w.Position[i].z:0.0}) yaw {w.Yaw[i] * Mathf.Rad2Deg:0} v {Unity.Mathematics.math.length(w.Velocity[i]):0.00}");
                sb.Append($" hp {w.Hp[i]:0}/{w.MaxHp[i]:0} flags 0x{w.Flags[i]:X}");
                if (m != null) sb.Append($" state {m.State[i]} crew {m.Crew[i]}/{m.CrewMax[i]} fire {m.Fire[i]:0.00} trkL {m.Module[i * M + 1]:0.0} trkR {m.Module[i * M + 5]:0.0} eng {m.Module[i * M + 2]:0.0}");
                if (k != null) sb.Append($" ditch {k.DitchTicks[i]} bog {k.BogTicks[i]} cross {k.CrossTrench[i]}");
                if (g != null) sb.Append($" gun0 {g.GunYaw[i * 2] * Mathf.Rad2Deg:0}°→{g.GunTarget[i * 2]} gun1 {g.GunYaw[i * 2 + 1] * Mathf.Rad2Deg:0}°→{g.GunTarget[i * 2 + 1]}");
                sb.Append('\n');
            }
            if (m != null) sb.Append($"penetrations {m.Penetrations} ricochets {m.Ricochets} knockouts {m.KnockOuts} cookoffs {m.CookOffs} bailed {m.BailedOut}\n");
            if (k != null) sb.Append($"wire {k.WireCrushed} trees {k.TreesPushed} men {k.MenCrushed}\n");
            return sb.ToString();
        }
    }
}
