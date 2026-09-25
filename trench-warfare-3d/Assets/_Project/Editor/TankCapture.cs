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
            public int FollowSlot = -1; public float FollowZoom = 22f, FollowYaw = 30f;
            public string Last;

            void LateUpdate()
            {
                var host = FindFirstObjectByType<SimHost>();
                var cam = FindFirstObjectByType<TacticalCamera>();
                if (FollowSlot >= 0 && host != null && cam != null && host.Local != null && host.Local.World.IsAlive(FollowSlot))
                {
                    var p = host.Presenter.Drawn(FollowSlot);
                    cam.FrameFrom(new Vector2(p.x, p.z), FollowZoom, FollowYaw);
                }
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
        }

        static Shooter Get()
        {
            var s = Object.FindFirstObjectByType<Shooter>();
            if (s == null) s = new GameObject("TankCapture") { hideFlags = HideFlags.DontSave }.AddComponent<Shooter>();
            return s;
        }

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        public static string Shot(string path, int w = 1600, int h = 900) { var s = Get(); s.W = w; s.H = h; s.Pending = path; return "queued " + path; }

        public static string Follow(int slot, float zoom = 22f, float yaw = 30f) { var s = Get(); s.FollowSlot = slot; s.FollowZoom = zoom; s.FollowYaw = yaw; return "following " + slot; }

        public static string Silver(int amount = 100000)
        {
            var h = Host; if (h == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart (waiting on the network): try again";
            h.WriteWorlds(m => { for (int p = 0; p < SimConfig.MaxPlayers; p++) m.World.Silver[p] = amount; });   // one world, or both in the canary
            return "silver " + amount;
        }

        /// <summary>A unit at an exact place in every world, with the stats its archetype has in the roster (either
        /// side's: each side fields different machines). The sim then moves it toward its deploy zone, so read
        /// World.Position back before aiming a camera at it.</summary>
        public static string Spawn(int team, int archetype, float x, float z, float yawDeg = -999f)
        {
            var h = Host; if (h == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart (waiting on the network): try again";
            bool vehicle = VehicleArchetype.IsArmoured((byte)archetype);
            // Looked up in the live roster rather than a hand list. The hand list stopped at Pavise, so Banner and
            // Redoubt came out with a rifleman's hit points on a machine's body, and every new archetype needed an
            // edit here that nothing reminded anyone to make.
            var roster = h.Local.World.Roster;
            int found = -1;
            for (int i = 0; i < roster.Length && found < 0; i++) if (roster[i].Archetype == archetype) found = i;
            if (found < 0 && vehicle) return $"archetype {archetype} is in neither side's roster: nothing spawned";
            var entry = found >= 0 ? roster[found] : roster[team * RosterEntry.SlotCount + Mathf.Clamp(archetype, 0, 3)];
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
                sb.Append($"#{i} {(w.Archetype[i] == VehicleArchetype.Tusk ? "Tusk" : "Maw")} t{w.Team[i]} pos({w.Position[i].x:0.0},{w.Position[i].z:0.0}) yaw {w.Yaw[i] * Mathf.Rad2Deg:0} v {Unity.Mathematics.math.length(w.Velocity[i]):0.00}");
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
