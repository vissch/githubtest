// Phase: riders prototype (tooling) — depends on: TankRenderer.Riders, TankCapture, SimHost, TacticalCamera
// The bench for "men on a crab's back" and "how big is a crab": driven from `unity command eval` or from a small
// IMGUI panel in Play (RiderLab.Panel(true), or ctrl+R while it is up):
//   RiderLab.Setup(6, 8)        a Pincer (archetype 6..11) and 8 real riflemen 14 m behind it, who run in and climb
//                               aboard (their sim units are hidden and held while they ride); returns the slot
//   RiderLab.Board(slot, n)     n men on it at once, no climb (for stills)
//   RiderLab.Dismount(slot, n)  n riders climb down; each is put back in the sim where he lands
//   RiderLab.Drive(slot, dz)    walk the machine dz metres along its column    RiderLab.Kill(slot)  destroy it
//   RiderLab.Enemies(slot, n)   n enemy riflemen ahead of it, so it (and its riders) have something to shoot at
//   RiderLab.Size(6, 1.3f)      that crab kind drawn 1.3x its shipped size, live
//   RiderLab.Film(dir, secs)    frames of the main camera sampled every 1/15 s of GAME time into dir (plays back at true speed)
//   RiderLab.Status()           sizes, seats and riders per machine
// Presentation only apart from what a lab must do to the sim (spawn, hold, move, destroy), which it does through
// SimHost.WriteWorlds like TankCapture. Nothing here is hashed or replayed.
using System.Collections.Generic;
using System.IO;
using System.Text;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Units;

namespace TW.Editor
{
    public static class RiderLab
    {
        static readonly string[] Names = { "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt" };
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();
        static string NameOf(byte a) => a == VehicleArchetype.Maw ? "Maw" : a == VehicleArchetype.Tusk ? "Tusk" : VehicleArchetype.IsWalker(a) ? Names[a - VehicleArchetype.Pincer] : "unit " + a;
        static TankRenderer Tanks => Object.FindFirstObjectByType<TankRenderer>();
        // Everything compiled code has to see lives on the Bench (a scene object), not in statics here: `unity command eval`
        // runs these methods in an interpreter whose static fields the compiled game never sees. A heading pin kept in a
        // static dictionary did nothing, and a flag set through a static was read back by eval but not by the renderer.
        public static int LastSlot { get => Get().LastSlot; set => Get().LastSlot = value; }
        static Dictionary<int, float> held => Get().Held;
        static Dictionary<int, float> pinnedYaw => Get().PinnedYaw;
        public static float BoardedAt { get => Get().BoardedAt; set => Get().BoardedAt = value; }
        public static float SetupAt { get => Get().SetupAt; set => Get().SetupAt = value; }

        /// <summary>A walker of `archetype` (6 Pincer .. 11 Redoubt) and `riders` riflemen who climb aboard, camera following.</summary>
        public static string Setup(int archetype = 6, int riders = 8, float x = -1f, float z = -1f, int team = 0, float yawDeg = 0f, bool climb = true)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost / not in Play";
            if (!VehicleArchetype.IsArmoured((byte)archetype)) return "archetype " + archetype + " is not a vehicle (4, 5 tanks; 6..11 walkers)";
            var size = h.Local.Map.SizeMeters;
            if (x < 0f) x = size.x * 0.5f;
            if (z < 0f) z = size.y * 0.45f;
            string spawned = TankCapture.Spawn(team, archetype, x, z, yawDeg);
            if (!spawned.StartsWith("slot ")) return spawned;
            int slot = int.Parse(spawned.Substring(5));
            LastSlot = slot; SetupAt = Time.time;
            Hold(slot, true);
            var p = h.Local.World.Position[slot];   // the sim may have moved it: read it back
            var men = new List<int>();
            if (climb)
            {
                float back = yawDeg * Mathf.Deg2Rad + Mathf.PI;
                for (int k = 0; k < riders; k++)
                {
                    float a = back + (k - (riders - 1) * 0.5f) * 0.16f, r = 13f + (k % 3) * 1.6f;
                    string m = TankCapture.Spawn(team, 0, p.x + Mathf.Sin(a) * r, p.z + Mathf.Cos(a) * r, yawDeg);
                    if (m.StartsWith("slot ")) { int s = int.Parse(m.Substring(5)); men.Add(s); Hold(s, true); }
                }
            }
            Get().Pending = new Boarding { Slot = slot, Riders = climb ? 0 : riders, Men = men.ToArray(), Frames = 3 };
            Watch();
            TankCapture.Follow(slot, 26f, 35f);
            return $"slot {slot} {NameOf((byte)archetype)} at ({p.x:0.0},{p.z:0.0}) size x{TankRenderer.SizeOf(archetype):0.00}, {(climb ? men.Count + " men climbing aboard" : riders + " riders placed")}";
        }

        /// <summary>`n` riflemen of the machine's team spawn 12-15 m behind it, held, and run in and climb aboard (for a
        /// machine that is already standing: spawn it, frame it, start filming, then call this).</summary>
        public static string Climbers(int slot, int n = 8)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var w = h.Local.World;
            if (!w.IsAlive(slot)) return "slot " + slot + " is not alive";
            var p = w.Position[slot]; float yaw = w.Yaw[slot], back = yaw + Mathf.PI;
            var men = new List<int>();
            for (int k = 0; k < n; k++)
            {
                float a = back + (k - (n - 1) * 0.5f) * 0.16f, r = 12f + (k % 3) * 1.6f;
                string m = TankCapture.Spawn(w.Team[slot], 0, p.x + Mathf.Sin(a) * r, p.z + Mathf.Cos(a) * r, yaw * Mathf.Rad2Deg);
                if (m.StartsWith("slot ")) { int s = int.Parse(m.Substring(5)); men.Add(s); Hold(s, true); }
            }
            Get().Pending = new Boarding { Slot = slot, Riders = 0, Men = men.ToArray(), Frames = 2 };
            Watch();
            return men.Count + " men running in to slot " + slot;
        }

        /// <summary>Hold a unit still in the sim (speed 0), or let it go at its old speed.</summary>
        static void Hold(int slot, bool on)
        {
            var h = Host; if (h == null || h.Local == null) return;
            var w = h.Local.World;
            bool vehicle = (w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0;
            if (on) { if (!held.ContainsKey(slot)) held[slot] = w.Speed[slot]; h.WriteWorlds(m => m.World.Speed[slot] = 0f); if (vehicle) pinnedYaw[slot] = w.Yaw[slot]; }
            else { pinnedYaw.Remove(slot); if (held.TryGetValue(slot, out float sp)) { held.Remove(slot); h.WriteWorlds(m => m.World.Speed[slot] = sp); } }
        }

        /// <summary>Real men take these riders' places: board from where they stand.</summary>
        public static string BoardMen(int slot, int[] men)
        {
            var t = Tanks; var h = Host; if (t == null || h == null) return "no TankRenderer";
            var w = h.Local.World;
            var from = new Vector3[men.Length]; var yaw = new float[men.Length];
            for (int i = 0; i < men.Length; i++)
            {
                var d = h.Presenter.Drawn(men[i]);
                from[i] = new Vector3(d.x, RenderGround.Sample(h.Local.Map, d.x, d.z), d.z); yaw[i] = w.Yaw[men[i]];
            }
            int n = t.BoardFrom(slot, from, yaw, men, 0);
            BoardedAt = Time.time;
            return $"slot {slot}: {n} riding or on their way, of {t.SeatCount(slot)} seats";
        }

        /// <summary>Riders appear seated at once (no climb, no sim men behind them).</summary>
        public static string Board(int slot, int count)
        {
            var t = Tanks; if (t == null) return "no TankRenderer";
            int n = t.Board(slot, count);
            return $"slot {slot}: {n} riding of {t.SeatCount(slot)} seats";
        }

        public static string Dismount(int slot, int count = 99)
        {
            var t = Tanks; if (t == null) return "no TankRenderer";
            Watch();
            return $"slot {slot}: {t.Dismount(slot, count)} staying aboard";
        }

        public static string Unboard(int slot, int count = int.MaxValue)
        {
            var t = Tanks; if (t == null) return "no TankRenderer";
            return $"slot {slot}: {t.Unboard(slot, count)} riding";
        }

        /// <summary>Walk the machine `dz` metres along its own column (a cell goal), releasing the hold.</summary>
        public static string Drive(int slot, float dz)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var m0 = h.Local; var p = m0.World.Position[slot];
            float gz = Mathf.Clamp(p.z + dz, 4f, m0.Map.SizeMeters.y - 4f);
            var cell = m0.Map.NavCellOf(new float3(p.x, 0f, gz));
            h.WriteWorlds(m => m.World.GoalId[slot] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(cell.x, cell.y), NavMode.Tracked)));
            Hold(slot, false);
            if (m0.World.Speed[slot] <= 0f) { var e = RosterFor(m0.World.Archetype[slot]); h.WriteWorlds(m => m.World.Speed[slot] = e.Speed); }
            return $"slot {slot} walking to z {gz:0.0}";
        }

        public static string Stop(int slot) { Hold(slot, true); return "slot " + slot + " held"; }

        /// <summary>Destroy the machine (its riders are thrown off).</summary>
        public static string Kill(int slot)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            if (!h.AlignWorlds()) return "worlds a tick apart: try again";
            h.WriteWorlds(m => m.World.Despawn(slot));
            return "slot " + slot + " destroyed";
        }

        /// <summary>Enemy riflemen ahead of the machine, held still, so its guns and its riders have a target.</summary>
        public static string Enemies(int slot, int count = 6, float ahead = 45f, float bearingDeg = 0f)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var w = h.Local.World; var p = w.Position[slot]; float yaw = w.Yaw[slot] + bearingDeg * Mathf.Deg2Rad;   // off the nose, + to the right
            int team = 1 - w.Team[slot], made = 0;
            for (int k = 0; k < count; k++)
            {
                float a = yaw + (k - (count - 1) * 0.5f) * 0.09f;
                string m = TankCapture.Spawn(team, 0, p.x + Mathf.Sin(a) * ahead, p.z + Mathf.Cos(a) * ahead, yaw * Mathf.Rad2Deg + 180f);
                if (m.StartsWith("slot ")) { Hold(int.Parse(m.Substring(5)), true); made++; }
            }
            return $"{made} enemies {ahead:0} m ahead of slot {slot}";
        }

        /// <summary>Remove every living enemy of that machine within `radius` metres (so its guns turn to a new group).</summary>
        public static string ClearEnemies(int slot, float radius = 120f)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            var w = h.Local.World; var p = w.Position[slot]; byte team = w.Team[slot]; var gone = new List<int>();
            for (int i = 0; i < w.HighWater; i++)
                if (w.IsAlive(i) && w.Team[i] != team && math.distance(w.Position[i].xz, p.xz) <= radius) gone.Add(i);
            foreach (int i in gone) { held.Remove(i); pinnedYaw.Remove(i); h.WriteWorlds(m => m.World.Despawn(i)); }
            return gone.Count + " enemies removed";
        }

        static RosterEntry RosterFor(byte a) =>
            a == VehicleArchetype.Maw ? RosterEntry.Maw : a == VehicleArchetype.Tusk ? RosterEntry.Tusk :
            a == VehicleArchetype.Pincer ? RosterEntry.Pincer : a == VehicleArchetype.Kettle ? RosterEntry.Kettle : a == VehicleArchetype.Censer ? RosterEntry.Censer :
            a == VehicleArchetype.Pavise ? RosterEntry.Pavise : a == VehicleArchetype.Banner ? RosterEntry.Banner : RosterEntry.Redoubt;

        /// <summary>Listen for riders landing, and put each back in the sim there (or make a new man for one who had none).</summary>
        static void Watch() => Get().Watching = true;

        /// <summary>Draw one crab kind at `factor` times its shipped size (VehicleSize.Walker * factor). Live.</summary>
        public static string Size(int archetype, float factor)
        {
            var t = Tanks; if (t == null) return "no TankRenderer";
            if (!t.Resize(archetype, factor)) return "could not resize " + archetype;
            return $"{Names[archetype - VehicleArchetype.Pincer]} x{factor:0.00} of shipped = x{TankRenderer.SizeOf(archetype):0.00} of the sculpt";
        }

        /// <summary>Riders kept out from under the guns' sweep (true) or seated anywhere (false). Re-seats every machine.</summary>
        public static string ClearOfGuns(bool on) { Get().WantClearOfGuns = on ? 1 : 0; return "clear of guns " + on + " (applied next frame)"; }

        public static string Fire(bool on) { var t = Tanks; if (t == null) return "no TankRenderer"; t.RidersFire = on; return "riders fire " + on; }

        public static string Freeze(bool on)
        {
            var h = Host; if (h == null) return "no SimHost";
            h.TimeScale = on ? 0f : 1f;
            return "time scale " + h.TimeScale;
        }

        public static string Panel(bool on) { Get().Show = on; return "panel " + on; }

        /// <summary>Frame the camera on a slot: zoom, yaw, pitch (the follow keeps it framed while it walks).</summary>
        public static string Camera(int slot, float zoom, float yaw, float pitch)
        {
            var c = Object.FindFirstObjectByType<TacticalCamera>(); if (c == null) return "no camera";
            c.ZoomMin = Mathf.Min(c.ZoomMin, 4f);
            c.Pitch = pitch; c.ClosePitch = pitch;
            return TankCapture.Follow(slot, zoom, yaw);
        }

        /// <summary>Record `seconds` of GAME time as `fps` frames a second into `dir` (a frame is repeated when the editor renders slower).</summary>
        public static string Film(string dir, float seconds = 6f, int fps = 15, int w = 960, int h = 540)
        {
            Directory.CreateDirectory(dir);
            foreach (var f in Directory.GetFiles(dir, "f_*.png")) File.Delete(f);
            var b = Get(); b.FilmDir = dir; b.FilmLeft = Mathf.RoundToInt(seconds * fps); b.FilmIndex = 0; b.FilmW = w; b.FilmH = h;
            b.FilmStep = 1f / Mathf.Max(1, fps); b.FilmNext = -1f; b.FilmFrom = -1f;
            return $"filming {b.FilmLeft} frames ({seconds:0.0} s of game time) into {dir}";
        }

        /// <summary>Save a still to `path` the frame `shots` more rider shots have been fired (a volley caught as it lands).</summary>
        public static string VolleyShot(string path, int shots = 6) { var b = Get(); b.VolleyPath = path; b.VolleyFrom = -1; b.VolleyAt = shots; return "waiting for a volley of " + shots; }

        public static string FilmStatus() { var b = Get(); return b.FilmLeft > 0 ? $"filming, {b.FilmLeft} left" : $"done, {b.FilmIndex} frames from t {b.FilmFrom:0.00} to {b.FilmTo:0.00}"; }

        public static string Status()
        {
            var h = Host; var t = Tanks;
            if (h == null || h.Local == null || t == null) return "not in Play";
            var w = h.Local.World; var sb = new StringBuilder();
            for (int c = 0; c < Names.Length; c++) sb.Append($"{Names[c]} x{TankRenderer.WalkerSizeFactor[c]:0.00}  ");
            sb.Append('\n');
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || !VehicleArchetype.IsArmoured(w.Archetype[i])) continue;
                sb.Append($"#{i} {NameOf(w.Archetype[i])} t{w.Team[i]} at ({w.Position[i].x:0.0},{w.Position[i].z:0.0}) riders {t.RiderCount(i)}/{t.SeatCount(i)}:");
                foreach (var r in t.RidersOf(i)) sb.Append(' ').Append(r.State.ToString().Substring(0, 3));
                sb.Append('\n');
            }
            sb.Append($"riders in all {t.RiderTotal}, setup at {SetupAt:0.0}, boarded at {BoardedAt:0.0}, now {Time.time:0.0}");
            var vat = Object.FindFirstObjectByType<VATRenderer>();
            if (vat != null) sb.Append($", extras drawn {vat.DrawnExtras}, grow {vat.CurrentGrow:0.00}");
            sb.Append($", ducking under a barrel {t.RidersDucking}");
            return sb.ToString();
        }

        // ------------------------------------------------------------------ seat sheets (no Play needed)
        /// <summary>
        /// One picture of a crab at `scale` (a multiple of the sculpt) with a kneeling-man stand-in on every seat, from
        /// three-quarter above on the left and straight down on the right. Runs in a batch editor with graphics (no
        /// -nographics): the parts are posed as modelled, so what this checks is the seat solver and the size, not the gait.
        /// </summary>
        public static string SeatSheet(string name, byte archetype, float scale, string path, int w = 1400, int h = 700)
        {
            var model = TankModel.Load(name, archetype, "Body", scale);
            if (model == null) return "no model " + name;
            var seats = RiderSeats.For(model);
            var lod = model.Lods[0];
            var world = new Matrix4x4[lod.Parts.Count];
            var root = new GameObject("SeatSheet") { hideFlags = HideFlags.HideAndDontSave };
            var lit = Shader.Find("Universal Render Pipeline/Lit");
            var hullMat = new Material(lit) { hideFlags = HideFlags.HideAndDontSave };
            var atlas = Resources.Load<Texture2D>("Vehicles/" + name + "Atlas");
            if (atlas != null) hullMat.SetTexture("_BaseMap", atlas);
            var manMat = new Material(lit) { hideFlags = HideFlags.HideAndDontSave, color = TankRenderer.TeamA };
            var wayMat = new Material(lit) { hideFlags = HideFlags.HideAndDontSave, color = new Color(1f, 0.75f, 0.2f) };
            var all = new Bounds(Vector3.zero, Vector3.zero); bool first = true;
            for (int i = 0; i < lod.Parts.Count; i++)
            {
                var p = lod.Parts[i];
                var local = Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                world[i] = p.Parent >= 0 ? world[p.Parent] * local : local;
                var go = new GameObject(p.Name); go.transform.SetParent(root.transform, false);
                go.transform.SetPositionAndRotation(world[i].GetPosition(), world[i].rotation);
                go.AddComponent<MeshFilter>().sharedMesh = p.Mesh;
                go.AddComponent<MeshRenderer>().sharedMaterial = hullMat;
                var b = p.Mesh.bounds; var wb = new Bounds(world[i].MultiplyPoint3x4(b.center), Vector3.zero);
                for (int c = 0; c < 8; c++) wb.Encapsulate(world[i].MultiplyPoint3x4(b.center + Vector3.Scale(b.extents, new Vector3((c & 1) == 0 ? -1 : 1, (c & 2) == 0 ? -1 : 1, (c & 4) == 0 ? -1 : 1))));
                if (first) { all = wb; first = false; } else all.Encapsulate(wb);
            }
            // a kneeling man at UnitScale is ~1.35 m high and ~0.7 m across: a capsule of that size, feet on the seat
            float manH = 1.35f, manW = 0.7f;
            foreach (var seat in seats.Seats)
            {
                var at = world[seats.BodyPart].MultiplyPoint3x4(seat.Local);
                var man = GameObject.CreatePrimitive(PrimitiveType.Capsule); man.name = "Rider";
                Object.DestroyImmediate(man.GetComponent<Collider>());
                man.transform.SetParent(root.transform, false);
                man.transform.localScale = new Vector3(manW, manH * 0.5f, manW);
                man.transform.position = at + Vector3.up * (manH * 0.5f);
                man.transform.rotation = Quaternion.Euler(0f, seat.Yaw * Mathf.Rad2Deg, 0f);
                man.GetComponent<MeshRenderer>().sharedMaterial = manMat;
                var edge = GameObject.CreatePrimitive(PrimitiveType.Cube); Object.DestroyImmediate(edge.GetComponent<Collider>());
                edge.transform.SetParent(root.transform, false); edge.transform.position = world[seats.BodyPart].MultiplyPoint3x4(seat.Edge); edge.transform.localScale = Vector3.one * 0.3f;
                edge.GetComponent<MeshRenderer>().sharedMaterial = wayMat;
                var footAt = world[seats.BodyPart].MultiplyPoint3x4(new Vector3(seat.Foot.x, 0f, seat.Foot.z)); footAt.y = 0f;
                var post = GameObject.CreatePrimitive(PrimitiveType.Cylinder); Object.DestroyImmediate(post.GetComponent<Collider>());
                post.transform.SetParent(root.transform, false); post.transform.position = new Vector3(footAt.x, (world[seats.BodyPart].MultiplyPoint3x4(seat.Edge).y) * 0.5f, footAt.z);
                post.transform.localScale = new Vector3(0.12f, world[seats.BodyPart].MultiplyPoint3x4(seat.Edge).y * 0.5f, 0.12f);
                post.GetComponent<MeshRenderer>().sharedMaterial = wayMat;
                // and a nose, so the way he faces reads
                var nose = GameObject.CreatePrimitive(PrimitiveType.Cube); Object.DestroyImmediate(nose.GetComponent<Collider>());
                nose.transform.SetParent(man.transform, false); nose.transform.localPosition = new Vector3(0f, 0.6f, 0.6f); nose.transform.localScale = new Vector3(0.4f, 0.25f, 0.6f);
                nose.GetComponent<MeshRenderer>().sharedMaterial = manMat;
            }
            var lightGo = new GameObject("Sun"); lightGo.transform.SetParent(root.transform, false);
            var light = lightGo.AddComponent<Light>(); light.type = LightType.Directional; light.intensity = 1.6f; lightGo.transform.rotation = Quaternion.Euler(55f, -30f, 0f);
            var camGo = new GameObject("Cam"); camGo.transform.SetParent(root.transform, false);
            var cam = camGo.AddComponent<Camera>(); cam.fieldOfView = 28f; cam.clearFlags = CameraClearFlags.SolidColor; cam.backgroundColor = new Color(0.16f, 0.17f, 0.19f); cam.nearClipPlane = 0.2f; cam.farClipPlane = 500f;
            var rt = RenderTexture.GetTemporary(w, h, 24, RenderTextureFormat.ARGB32);
            var tex = new Texture2D(w, h, TextureFormat.RGB24, false);
            float radius = all.extents.magnitude, dist = radius / Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad) * 1.05f;
            var views = new[] { new Vector3(1f, 0.85f, -1.3f).normalized, new Vector3(0.001f, 1f, 0.02f).normalized };
            for (int k = 0; k < 2; k++)
            {
                camGo.transform.position = all.center + views[k] * dist;
                camGo.transform.LookAt(all.center, k == 1 ? Vector3.forward : Vector3.up);
                cam.rect = new Rect(k * 0.5f, 0f, 0.5f, 1f);
                cam.targetTexture = rt; cam.Render();
            }
            var was = RenderTexture.active; RenderTexture.active = rt;
            tex.ReadPixels(new Rect(0, 0, w, h), 0, 0); tex.Apply(); RenderTexture.active = was;
            System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(path));
            System.IO.File.WriteAllBytes(path, tex.EncodeToPNG());
            RenderTexture.ReleaseTemporary(rt);
            Object.DestroyImmediate(tex); Object.DestroyImmediate(root); Object.DestroyImmediate(hullMat); Object.DestroyImmediate(manMat); Object.DestroyImmediate(wayMat);
            float hi = 0f; foreach (var s in seats.Seats) hi = Mathf.Max(hi, s.Local.y);
            return $"{name} x{scale:0.00}: {seats.Seats.Count} seats, deck {seats.DeckFloor:0.00}..{seats.DeckTop:0.00} m, highest seat {hi:0.00} m, body {all.size.x:0.0}x{all.size.y:0.0}x{all.size.z:0.0} m -> {path}";
        }

        /// <summary>Batch: every crab at the sizes in TW_SEATSHEET_SCALES (default "1,1.5") into TW_SEATSHEET_OUT.</summary>
        public static void SeatSheetBatch()
        {
            string dir = System.Environment.GetEnvironmentVariable("TW_SEATSHEET_OUT");
            if (string.IsNullOrEmpty(dir)) dir = System.IO.Path.Combine(System.IO.Directory.GetParent(Application.dataPath).FullName, "Captures", "riders");
            string scalesVar = System.Environment.GetEnvironmentVariable("TW_SEATSHEET_SCALES");
            if (string.IsNullOrEmpty(scalesVar)) scalesVar = "1,1.5";
            var sb = new StringBuilder();
            foreach (var sv in scalesVar.Split(','))
            {
                float f = float.Parse(sv.Trim(), System.Globalization.CultureInfo.InvariantCulture);
                for (int c = 0; c < Names.Length; c++)
                    sb.AppendLine(SeatSheet(Names[c], (byte)(VehicleArchetype.Pincer + c), VehicleSize.Walker * f, System.IO.Path.Combine(dir, $"{Names[c]}_x{f:0.00}.png")));
            }
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "seatsheets.txt"), sb.ToString());
            Debug.Log("RiderLab.SeatSheetBatch:\n" + sb);
        }

        public struct Boarding { public int Slot, Riders, Frames; public int[] Men; }

        /// <summary>The one Bench. NEVER call this from the Bench's own per-frame code: when the lookup missed (an eval
        /// made the first one), every Bench made another every frame and they doubled until the editor froze (seen
        /// twice, 2026-09-25). Its own code uses its own fields; this is for the static entry points only.</summary>
        static Bench Get()
        {
            if (Bench.Instance != null) return Bench.Instance;
            var b = Object.FindFirstObjectByType<Bench>(FindObjectsInactive.Include);
            if (b == null) b = new GameObject("RiderLab") { hideFlags = HideFlags.DontSave }.AddComponent<Bench>();
            Bench.Instance = b;
            return b;
        }

        /// <summary>The panel, the deferred boarding (the renderer sees a machine a frame after it is spawned), the
        /// delayed un-hiding of landed men, and the film recorder (last in the frame, after every renderer).</summary>
        [DefaultExecutionOrder(30001)]
        public sealed class Bench : MonoBehaviour
        {
            public static Bench Instance;
            void Awake()
            {
                // a second Bench (an eval's lookup can miss the first) goes away at once rather than running beside it
                if (Instance != null && Instance != this) { Destroy(gameObject); return; }
                Instance = this;
            }
            public bool Show;
            public Boarding? Pending;
            public string FilmDir; public int FilmLeft, FilmIndex, FilmW = 960, FilmH = 540;
            public string VolleyPath; public int VolleyFrom = -1, VolleyAt = 6;
            public float FilmStep = 1f / 15f, FilmNext = -1f, FilmFrom = -1f, FilmTo = -1f;
            public int LastSlot = -1; public float BoardedAt = -1f, SetupAt = -1f;
            public readonly Dictionary<int, float> Held = new Dictionary<int, float>(), PinnedYaw = new Dictionary<int, float>();
            public int WantClearOfGuns = -1;   // -1 leave alone, 0 / 1 set RiderSeats.ClearOfGuns (from compiled code)
            public bool Watching;
            TankRenderer watched;
            readonly List<(int slot, float at)> unhide = new List<(int, float)>();
            readonly float[] sliders = { 1f, 1f, 1f, 1f, 1f, 1f };
            int pick = 6;
            RenderTexture rt; Texture2D tex;

            public void Unhide(int slot) => unhide.Add((slot, Time.time + 0.25f));

            void LateUpdate()
            {
                if (Pending != null)
                {
                    var p = Pending.Value; p.Frames--; Pending = p;
                    if (p.Frames <= 0)
                    {
                        var t = Tanks;
                        if (t != null)
                        {
                            if (p.Riders > 0) t.Board(p.Slot, p.Riders);
                            if (p.Men != null && p.Men.Length > 0) BoardMen(p.Slot, p.Men);
                        }
                        Pending = null;
                    }
                }
                if (PinnedYaw.Count > 0)
                {
                    var host = Host;
                    if (host != null && host.Local != null)
                        foreach (var kv in PinnedYaw)
                        {
                            int s = kv.Key; float y = kv.Value;
                            if (host.Local.World.IsAlive(s) && host.Local.World.Yaw[s] != y) host.WriteWorlds(m => { m.World.Yaw[s] = y; m.World.Velocity[s] = float3.zero; });
                        }
                }
                if (CarryMen) Carry();
                VolleyStill();
                if (unhide.Count > 0)
                {
                    var vat = Object.FindFirstObjectByType<VATRenderer>();
                    for (int i = unhide.Count - 1; i >= 0; i--)
                        if (Time.time >= unhide[i].at) { if (vat != null) vat.Hide(unhide[i].slot, false); unhide.RemoveAt(i); }
                }
                if (WantClearOfGuns >= 0)
                {
                    RiderSeats.ClearOfGuns = WantClearOfGuns == 1; WantClearOfGuns = -1;
                    var t = Tanks; if (t != null) t.ForgetSeats();
                }
                if (Watching)
                {
                    var t = Tanks;
                    if (t != null && t != watched) { if (watched != null) watched.RiderLanded -= OnLanded; t.RiderLanded -= OnLanded; t.RiderLanded += OnLanded; watched = t; }
                }
                // film by GAME time: one frame per step of it, repeated if the editor renders slower than the film's rate,
                // so the clip always plays back at the game's own speed (Time.captureFramerate did not hold here)
                if (FilmLeft > 0 && UnityEngine.Camera.main != null)
                {
                    if (FilmNext < 0f) { FilmNext = Time.time; FilmFrom = Time.time; }
                    bool rendered = false;
                    while (FilmLeft > 0 && Time.time >= FilmNext)
                    {
                        if (!rendered) { Render(); rendered = true; }
                        Save(); FilmNext += FilmStep; FilmTo = Time.time;
                    }
                }
            }

            void Render()
            {
                var c = UnityEngine.Camera.main;
                if (rt == null || rt.width != FilmW || rt.height != FilmH)
                {
                    if (rt != null) rt.Release();
                    rt = new RenderTexture(FilmW, FilmH, 24, RenderTextureFormat.ARGB32);
                    tex = new Texture2D(FilmW, FilmH, TextureFormat.RGB24, false);
                }
                var before = c.targetTexture;
                c.targetTexture = rt; c.Render(); c.targetTexture = before;
                var was = RenderTexture.active; RenderTexture.active = rt;
                tex.ReadPixels(new Rect(0, 0, FilmW, FilmH), 0, 0); tex.Apply();
                RenderTexture.active = was;
                png = tex.EncodeToPNG();
            }

            byte[] png;
            /// <summary>A still the frame a volley lands: RiderShots up by VolleyAt since the call.</summary>
            void VolleyStill()
            {
                var t = Tanks;
                if (VolleyPath == null || t == null || UnityEngine.Camera.main == null) return;
                if (VolleyFrom < 0) { VolleyFrom = t.RiderShots; return; }
                if (t.RiderShots - VolleyFrom < VolleyAt) return;
                FilmW = 960; FilmH = 540; Render();
                File.WriteAllBytes(VolleyPath, png);
                VolleyPath = null; VolleyFrom = -1;
            }

            void Save()
            {
                File.WriteAllBytes(Path.Combine(FilmDir, $"f_{FilmIndex:0000}.png"), png);
                FilmIndex++; FilmLeft--;
            }

            void OnDestroy() { if (rt != null) rt.Release(); if (watched != null) watched.RiderLanded -= OnLanded; if (Instance == this) Instance = null; }

            /// <summary>A rider is on the ground: put his man back in the sim there (or make one), all from this Bench's own state.</summary>
            /// <summary>Keep each rider's hidden sim man where the rider is (roughly what a sim CarriedBy will do). Left at
            /// the spot he boarded from, he was shot there and his rider fell dead off a machine nobody was firing at.</summary>
            public bool CarryMen = true;

            void Carry()
            {
                var t = Tanks; var h = Host;
                if (t == null || h == null || h.Local == null) return;
                var w = h.Local.World;
                foreach (int s in t.RiddenSlots)
                    foreach (var r in t.RidersOf(s))
                    {
                        int m = r.SimSlot;
                        if (m < 0 || m >= w.HighWater || !w.IsAlive(m)) continue;
                        var q = w.Position[m]; float dx = q.x - r.Pos.x, dz = q.z - r.Pos.z;
                        if (dx * dx + dz * dz < 0.09f) continue;
                        var at = new float3(r.Pos.x, 0f, r.Pos.z);
                        h.WriteWorlds(x => { x.World.Position[m] = at; x.World.Velocity[m] = float3.zero; });
                    }
            }

            void OnLanded(TankRenderer.Rider r, Vector3 at, float yaw)
            {
                var h = Host; if (h == null || h.Local == null) return;
                if (r.SimSlot >= 0 && h.Local.World.IsAlive(r.SimSlot))
                {
                    int s = r.SimSlot;
                    h.WriteWorlds(m => { m.World.Position[s] = new float3(at.x, 0f, at.z); m.World.Yaw[s] = yaw; m.World.Velocity[s] = float3.zero; });
                    PinnedYaw.Remove(s);
                    if (Held.TryGetValue(s, out float sp)) { Held.Remove(s); h.WriteWorlds(m => m.World.Speed[s] = sp); }
                    Unhide(s);   // once the presenter has the new place for a few ticks, or he streaks across the field
                }
                else TankCapture.Spawn(r.Team, r.Archetype, at.x, at.z, yaw * Mathf.Rad2Deg);
            }

            void Update()
            {
                var kb = UnityEngine.InputSystem.Keyboard.current;
                if (kb != null && kb.rKey.wasPressedThisFrame && kb.leftCtrlKey.isPressed) Show = !Show;
            }

            void OnGUI()
            {
                if (!Show) return;
                var t = Tanks; var h = Host;
                GUILayout.BeginArea(new Rect(10, 10, 360, 470), GUI.skin.box);
                GUILayout.Label("Rider lab  (ctrl+R hides)");
                for (int c = 0; c < Names.Length; c++)
                {
                    GUILayout.BeginHorizontal();
                    GUILayout.Label($"{Names[c]} x{sliders[c]:0.00}", GUILayout.Width(110));
                    sliders[c] = GUILayout.HorizontalSlider(sliders[c], 0.4f, 2.5f);
                    if (GUILayout.Button("set", GUILayout.Width(36)) && t != null) t.Resize(VehicleArchetype.Pincer + c, sliders[c]);
                    GUILayout.EndHorizontal();
                }
                GUILayout.Space(6);
                GUILayout.BeginHorizontal();
                GUILayout.Label("spawn", GUILayout.Width(50));
                for (int c = 0; c < Names.Length; c++) if (GUILayout.Toggle(pick == VehicleArchetype.Pincer + c, Names[c].Substring(0, 3), GUI.skin.button)) pick = VehicleArchetype.Pincer + c;
                GUILayout.EndHorizontal();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("spawn + 8 climb")) Setup(pick, 8);
                if (GUILayout.Button("dismount") && LastSlot >= 0) Dismount(LastSlot);
                if (GUILayout.Button("walk 40 m") && LastSlot >= 0) Drive(LastSlot, 40f);
                GUILayout.EndHorizontal();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("enemies") && LastSlot >= 0) Enemies(LastSlot);
                if (GUILayout.Button("destroy") && LastSlot >= 0) Kill(LastSlot);
                if (t != null) t.RidersFire = GUILayout.Toggle(t.RidersFire, "riders fire");
                if (h != null) { bool frozen = h.TimeScale <= 0f; bool now = GUILayout.Toggle(frozen, "freeze"); if (now != frozen) h.TimeScale = now ? 0f : 1f; }
                GUILayout.EndHorizontal();
                GUILayout.Label(Status());
                GUILayout.EndArea();
            }
        }
    }
}
