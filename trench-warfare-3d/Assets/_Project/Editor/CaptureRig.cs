// Phase: tooling (2026-09-22). One correct path for every still this project judges itself on, because the old one
// was wrong in three ways at once and we spent an evening arguing about pictures that did not show the game.
//
// What was wrong. The scripts set Camera.main's transform and called Render() inline from a `unity command eval`,
// outside the frame loop. VATRenderer culls the men in its own LateUpdate against Camera.main's frustum, so every
// still rendered LAST FRAME'S cull set from a NEW pose: men at the edges missing, or there when they should not be.
// Worse, VATRenderer takes `grow` from IZoomSource.CurrentZoom — the tactical camera's zoom — and not from the field
// of view the capture overrode, so a "super zoom" still drew men at the tactical camera's scale. Close-up judgements
// about how big the men are were made on frames where the men were the wrong size.
//
// What this does instead. A shot is posed on one frame through the camera's own API (so zoom, grow, culling and the
// close-up band all agree with what a player at that zoom would see) and rendered on the NEXT one, after every
// LateUpdate that reads the camera has run against the new pose. The camera is put back exactly as it was.
//
// It also measures. Three of the worst faults of 2026-09-22 — soldiers lit 3.3x brighter than the world, a shell
// light washing the field yellow, a burst blowing out to white — were all "too bright" and all findable by a number
// before anybody opened a PNG. Every shot writes a .json beside the .png with the frame's luminance, how much of it
// is blown out, and how far each man stands out from the ground behind him.
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using UnityEngine;
using Unity.Profiling;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Editor
{
    public static class CaptureRig
    {
        /// <summary>
        /// Runs last in the frame (after the camera, the presenter and the renderers have all had their LateUpdate),
        /// so what it photographs is what those systems just prepared.
        /// </summary>
        [DefaultExecutionOrder(30000)]
        public sealed class Rig : MonoBehaviour
        {
            public sealed class Shot
            {
                public string Path;
                public Vector2 Focus; public float Zoom, Yaw, Pitch;
                public int W = 1600, H = 900;
                public bool Posed;
                public Vector3 Want;   // the pose the rig computed, so a still can prove it was taken from where it claims
            }

            /// <summary>Logs each pose and render with its frame number. Off by default; it is how the two-rigs-in-
            /// one-frame fault was found, and it is the first thing to turn on when a still looks wrong.</summary>
            public static bool Verbose;

            public readonly Queue<Shot> Queue = new Queue<Shot>();
            public string Last = "", LastJson = "";
            bool running;

            void LateUpdate()
            {
                if (!running && Queue.Count > 0) StartCoroutine(Run());
            }

            /// <summary>
            /// Asking the tactical camera to pose itself does not work: FrameFrom's yaw is ADDED to BaseYaw and to the
            /// angle FollowBattle is eaasing towards, the public Pitch field is not what LateUpdate uses (autoPitch is),
            /// and the camera rewrites its own transform every frame regardless. So for the length of a set the camera
            /// is switched off and the rig computes the pose itself, with exactly the arithmetic TacticalCamera uses —
            /// same close term, same fov blend, same distance, same focus height — with the battle-following angle
            /// neutralised so a shot is repeatable. Zoom is still written to the camera, because that is what the
            /// renderers read to size the men and to turn the close-up detail band on.
            /// </summary>
            System.Collections.IEnumerator Run()
            {
                running = true;
                var tc = FindFirstObjectByType<TacticalCamera>();
                var cam = Camera.main;
                if (tc == null || cam == null) { Queue.Clear(); running = false; yield break; }

                bool keepEnabled = tc.enabled;
                float keepZoom = tc.Zoom; Vector2 keepFocus = tc.Focus; float keepFov = cam.fieldOfView;
                Vector3 keepPos = cam.transform.position; Quaternion keepRot = cam.transform.rotation;
                tc.enabled = false;

                while (Queue.Count > 0)
                {
                    var shot = Queue.Dequeue();
                    Pose(tc, cam, shot);
                    yield return null;                        // a whole frame, so everything that culls or scales
                    yield return new WaitForEndOfFrame();     // against the camera has seen this pose
                    Render(shot);
                }

                tc.Zoom = keepZoom; tc.Focus = keepFocus; cam.fieldOfView = keepFov;
                cam.transform.SetPositionAndRotation(keepPos, keepRot);
                tc.enabled = keepEnabled;
                running = false;
            }

            void Pose(TacticalCamera tc, Camera cam, Shot shot)
            {
                tc.Zoom = Mathf.Clamp(shot.Zoom, tc.ZoomMin, tc.ZoomMax);
                tc.Focus = shot.Focus;
                float close = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, tc.Zoom));
                float fov = Mathf.Lerp(tc.Fov, tc.CloseFov, close);
                cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
                float pitch = Mathf.Clamp(shot.Pitch, tc.PitchMin, tc.PitchMax);
                var rot = Quaternion.Euler(pitch, tc.BaseYaw + shot.Yaw, 0f);
                float distance = tc.Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
                var host = FindFirstObjectByType<SimHost>();
                float ground = host != null && host.Local != null ? host.Local.Map.Height.Sample(shot.Focus.x, shot.Focus.y) : 0f;
                var aim = new Vector3(shot.Focus.x, ground * close + 1.1f * close, shot.Focus.y);
                shot.Want = aim - rot * Vector3.forward * distance;
                cam.transform.SetPositionAndRotation(shot.Want, rot);
                if (Verbose) Debug.Log($"[rig] posed {Path.GetFileName(shot.Path)} frame={Time.frameCount} want={shot.Want} zoom={tc.Zoom} fov={fov}");
            }

            void Render(Shot shot)
            {
                var c = Camera.main; if (c == null) return;
                if (Verbose) Debug.Log($"[rig] render {Path.GetFileName(shot.Path)} frame={Time.frameCount} at={c.transform.position} want={shot.Want}");
                var rt = RenderTexture.GetTemporary(shot.W, shot.H, 24, RenderTextureFormat.ARGB32);
                var before = c.targetTexture;
                c.targetTexture = rt; c.Render(); c.targetTexture = before;
                var was = RenderTexture.active; RenderTexture.active = rt;
                var tex = new Texture2D(shot.W, shot.H, TextureFormat.RGB24, false);
                tex.ReadPixels(new Rect(0, 0, shot.W, shot.H), 0, 0); tex.Apply();
                RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);

                Directory.CreateDirectory(Path.GetDirectoryName(shot.Path));
                File.WriteAllBytes(shot.Path, tex.EncodeToPNG());
                LastJson = Measure(tex, shot, c);
                File.WriteAllText(Path.ChangeExtension(shot.Path, ".json"), LastJson);
                Object.Destroy(tex);
                Last = shot.Path;
            }
        }

        // ---- measurement -------------------------------------------------------------------------------------

        static float Luma(Color32 p) => (0.2126f * p.r + 0.7152f * p.g + 0.0722f * p.b) / 255f;

        /// <summary>
        /// The numbers worth having about a frame. Luminance tells us whether anything is blown out (the yellow wash
        /// and the white burst would both have tripped `blown`), and the figure/ground contrast answers the question
        /// we keep asking by eye — do the men stand out from the mud, or are they lost in it, or are they pasted on
        /// top of it three times too bright.
        /// </summary>
        static string Measure(Texture2D tex, Rig.Shot shot, Camera cam)
        {
            var px = tex.GetPixels32();
            int w = tex.width, h = tex.height;
            var lum = new float[px.Length];
            double sum = 0; int blown = 0, black = 0;
            for (int i = 0; i < px.Length; i++)
            {
                float l = Luma(px[i]); lum[i] = l; sum += l;
                if (l > 0.90f) blown++;
                if (l < 0.05f) black++;
            }
            var sorted = (float[])lum.Clone(); System.Array.Sort(sorted);
            float mean = (float)(sum / px.Length);
            float p50 = sorted[px.Length / 2], p95 = sorted[(int)(px.Length * 0.95f)];

            // every man the game drew, measured where he is drawn: a disc on his chest against a ring of the ground
            // and kit behind him. Contrast is |man - background| / (background + 0.02), so it is a ratio, not a
            // difference, and 0.4 means "you can find him", 0.1 means "he is mud".
            var contrasts = new List<float>();
            int inFrame = 0;
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host != null && host.Local != null && cam != null)
            {
                var world = host.Local.World;
                for (int s = 0; s < world.HighWater; s++)
                {
                    if ((world.Flags[s] & 1u) == 0) continue;
                    Vector3 chest = (Vector3)host.Presenter.Drawn(s) + Vector3.up * 1.3f;
                    if (Vector3.Dot(chest - cam.transform.position, cam.transform.forward) <= 0f) continue;
                    Vector3 sp = cam.WorldToScreenPoint(chest);
                    int cx = Mathf.RoundToInt(sp.x * w / cam.pixelWidth), cy = Mathf.RoundToInt(sp.y * h / cam.pixelHeight);
                    // how tall he is drawn, in this image's pixels. The ring that samples the ground has to sit OUTSIDE
                    // him, and he is 80 px tall at the tactical camera and several hundred up close — a fixed radius
                    // measured the man against himself and called every frame unreadable.
                    Vector3 footS = cam.WorldToScreenPoint((Vector3)host.Presenter.Drawn(s));
                    Vector3 headS = cam.WorldToScreenPoint((Vector3)host.Presenter.Drawn(s) + Vector3.up * 2.6f);
                    float tall = Mathf.Abs(headS.y - footS.y) * h / cam.pixelHeight;
                    if (tall < 6f) continue;                                  // too small to say anything honest about
                    int rMan = Mathf.Max(2, Mathf.RoundToInt(tall * 0.10f));
                    int rIn = Mathf.RoundToInt(tall * 0.62f), rOut = Mathf.RoundToInt(tall * 1.00f);
                    if (cx < rOut || cy < rOut || cx >= w - rOut || cy >= h - rOut) continue;
                    inFrame++;
                    float man = Disc(lum, w, h, cx, cy, 0, rMan);
                    float bg = Ring(lum, w, h, cx, cy, rIn, rOut);            // median, so a neighbour in the ring does not skew it
                    contrasts.Add(Mathf.Abs(man - bg) / (bg + 0.02f));
                }
            }
            contrasts.Sort();
            float cMed = contrasts.Count > 0 ? contrasts[contrasts.Count / 2] : -1f;
            float c10 = contrasts.Count > 0 ? contrasts[Mathf.Min(contrasts.Count - 1, (int)(contrasts.Count * 0.10f))] : -1f;

            var vat = Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
            var sb = new StringBuilder();
            sb.Append("{\n");
            F(sb, "path", Path.GetFileName(shot.Path));
            N(sb, "zoom", shot.Zoom); N(sb, "yaw", shot.Yaw); N(sb, "pitch", shot.Pitch);
            N(sb, "focus_x", shot.Focus.x); N(sb, "focus_z", shot.Focus.y);
            // where the camera actually ended up, not where we asked it to go: the two are not the same thing while
            // the tactical camera still has opinions of its own, and a still taken from the wrong pose is worthless
            N(sb, "cam_x", cam.transform.position.x); N(sb, "cam_y", cam.transform.position.y); N(sb, "cam_z", cam.transform.position.z);
            N(sb, "cam_pitch", cam.transform.eulerAngles.x); N(sb, "cam_yaw", cam.transform.eulerAngles.y);
            N(sb, "frame", Time.frameCount);
            N(sb, "want_x", shot.Want.x); N(sb, "want_y", shot.Want.y); N(sb, "want_z", shot.Want.z);
            N(sb, "pose_error_m", Vector3.Distance(shot.Want, cam.transform.position));
            N(sb, "luma_mean", mean); N(sb, "luma_p50", p50); N(sb, "luma_p95", p95);
            N(sb, "blown_frac", blown / (float)px.Length); N(sb, "black_frac", black / (float)px.Length);
            N(sb, "men_in_frame", inFrame); N(sb, "contrast_median", cMed); N(sb, "contrast_p10", c10);
            // which squall the shot was taken in: rain wanders over a ~50 s cycle and nothing used to record it, so
            // two captures minutes apart were compared in different weather without anybody knowing
            N(sb, "rain", TW.Presentation.Terrain.Atmosphere.RainNow);
            N(sb, "wind", TW.Presentation.Terrain.Atmosphere.WindNow.magnitude);
            if (vat != null)
            {
                N(sb, "drawn_infantry", vat.DrawnInfantry); N(sb, "drawn_near", vat.DrawnNear); N(sb, "drawn_far", vat.DrawnFar);
                N(sb, "vertices", vat.VerticesThisFrame); N(sb, "shadows", vat.ShadowsThisFrame ? 1 : 0);
                N(sb, "fallen", vat.FallenCount);
            }
            // the warnings the evening of 2026-09-22 would have wanted shouted at it
            var warn = new List<string>();
            if (blown / (float)px.Length > 0.02f) warn.Add("more than 2% of the frame is blown out");
            if (mean > 0.45f) warn.Add("frame is brighter than a night scene should be");
            if (cMed >= 0f && cMed > 2.0f) warn.Add("men stand more than 2x off their background: pasted on, not lit by the scene");
            if (cMed >= 0f && cMed < 0.12f) warn.Add("men barely separate from the ground: unreadable");
            sb.Append("  \"warnings\": [");
            for (int i = 0; i < warn.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(warn[i]).Append('"');
            sb.Append("]\n}\n");
            return sb.ToString();
        }

        /// <summary>The middle value of the ring around a man: the ground and kit behind him, unmoved by a neighbour
        /// or a lantern taking up part of it, which a mean would let drag the number anywhere.</summary>
        static float Ring(float[] lum, int w, int h, int cx, int cy, int rIn, int rOut)
        {
            var vals = new List<float>();
            for (int y = -rOut; y <= rOut; y += 2)
                for (int x = -rOut; x <= rOut; x += 2)
                {
                    int d2 = x * x + y * y;
                    if (d2 > rOut * rOut || d2 < rIn * rIn) continue;
                    int px = cx + x, py = cy + y;
                    if (px < 0 || py < 0 || px >= w || py >= h) continue;
                    vals.Add(lum[py * w + px]);
                }
            if (vals.Count == 0) return 0f;
            vals.Sort();
            return vals[vals.Count / 2];
        }

        static float Disc(float[] lum, int w, int h, int cx, int cy, int rIn, int rOut)
        {
            double s = 0; int n = 0;
            for (int y = -rOut; y <= rOut; y++)
                for (int x = -rOut; x <= rOut; x++)
                {
                    int d2 = x * x + y * y;
                    if (d2 > rOut * rOut || d2 < rIn * rIn) continue;
                    int px = cx + x, py = cy + y;
                    if (px < 0 || py < 0 || px >= w || py >= h) continue;
                    s += lum[py * w + px]; n++;
                }
            return n > 0 ? (float)(s / n) : 0f;
        }

        static void F(StringBuilder sb, string k, string v) => sb.Append("  \"").Append(k).Append("\": \"").Append(v).Append("\",\n");
        static void N(StringBuilder sb, string k, double v) => sb.Append("  \"").Append(k).Append("\": ").Append(v.ToString("0.#####", CultureInfo.InvariantCulture)).Append(",\n");

        // ---- the entry points the shell scripts call --------------------------------------------------------

        // Held here, not looked up: FindFirstObjectByType does not return a DontSave object, so a second call made a
        // SECOND rig. Each took one shot of the set, both posed the same camera in the same frame, and each then
        // photographed the other's pose — every still in a two-shot set came out from the wrong place. Found by
        // logging the frame numbers rather than reasoning about them.
        static Rig rig;

        static Rig Get()
        {
            if (rig == null)
                rig = new GameObject("TW Capture Rig") { hideFlags = HideFlags.DontSave }.AddComponent<Rig>();
            return rig;
        }

        /// <summary>Queue one still. Posed this frame, photographed the next, camera put back when the queue drains.</summary>
        public static string Shot(string path, float x, float z, float zoom, float yaw, float pitch = 25f, int w = 1600, int h = 900)
        {
            Get().Queue.Enqueue(new Rig.Shot { Path = path, Focus = new Vector2(x, z), Zoom = zoom, Yaw = yaw, Pitch = pitch, W = w, H = h });
            return "queued " + path;
        }

        /// <summary>How many shots are still to be taken; 0 means the set is finished and the camera is back.</summary>
        public static string Pending() => rig != null ? rig.Queue.Count.ToString() : "0";

        /// <summary>The measurements from the last still, so a script can read them without opening the file.</summary>
        public static string LastReport() => rig != null ? rig.LastJson : "";

        // ---- the footprint half of the scorecard ------------------------------------------------------------

        /// <summary>
        /// Samples the frame cost over `frames` frames and writes it as json. Ten lines of the scorecard have never
        /// been measured once, on any machine, because nothing in this project has ever opened a ProfilerRecorder.
        /// This is the smallest thing that turns those zeroes into numbers.
        /// </summary>
        public static string Profile(string path, int frames = 300)
        {
            var host = Get().gameObject.GetComponent<Profiler>() ?? Get().gameObject.AddComponent<Profiler>();
            host.Begin(path, frames);
            return "profiling " + frames + " frames into " + path;
        }

        [DefaultExecutionOrder(31000)]
        public sealed class Profiler : MonoBehaviour
        {
            ProfilerRecorder main, gpu, setPass, batches, gc;
            string path; int left; readonly List<double> mainMs = new List<double>(), gpuMs = new List<double>();
            readonly List<double> passes = new List<double>(), draws = new List<double>(), alloc = new List<double>();

            public void Begin(string p, int frames)
            {
                path = p; left = frames;
                main = ProfilerRecorder.StartNew(ProfilerCategory.Internal, "Main Thread");
                gpu = ProfilerRecorder.StartNew(ProfilerCategory.Render, "GPU Frame Time");
                setPass = ProfilerRecorder.StartNew(ProfilerCategory.Render, "SetPass Calls Count");
                batches = ProfilerRecorder.StartNew(ProfilerCategory.Render, "Draw Calls Count");
                gc = ProfilerRecorder.StartNew(ProfilerCategory.Memory, "GC Allocated In Frame");
            }

            void LateUpdate()
            {
                if (left <= 0) return;
                if (main.Valid && main.CurrentValue > 0) mainMs.Add(main.CurrentValue * 1e-6);
                if (gpu.Valid && gpu.CurrentValue > 0) gpuMs.Add(gpu.CurrentValue * 1e-6);
                if (setPass.Valid) passes.Add(setPass.LastValue);
                if (batches.Valid) draws.Add(batches.LastValue);
                if (gc.Valid) alloc.Add(gc.LastValue);
                if (--left > 0) return;

                var sb = new StringBuilder("{\n");
                Stat(sb, "main_ms", mainMs, 3.0); Stat(sb, "gpu_ms", gpuMs, 13.0);
                Stat(sb, "setpass", passes, 300); Stat(sb, "draw_calls", draws, 300);
                Stat(sb, "gc_bytes_per_frame", alloc, 0);
                sb.Append("  \"frames\": ").Append(mainMs.Count).Append("\n}\n");
                Directory.CreateDirectory(Path.GetDirectoryName(path));
                File.WriteAllText(path, sb.ToString());
                Debug.Log("Capture rig: frame cost written to " + path + "\n" + sb);
                main.Dispose(); gpu.Dispose(); setPass.Dispose(); batches.Dispose(); gc.Dispose();
            }

            static void Stat(StringBuilder sb, string key, List<double> v, double budget)
            {
                if (v.Count == 0) { sb.Append("  \"").Append(key).Append("\": null,\n"); return; }
                var s = new List<double>(v); s.Sort();
                double p50 = s[s.Count / 2], p95 = s[Mathf.Min(s.Count - 1, (int)(s.Count * .95))], p99 = s[Mathf.Min(s.Count - 1, (int)(s.Count * .99))];
                sb.Append("  \"").Append(key).Append("\": { \"p50\": ").Append(p50.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"p95\": ").Append(p95.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"p99\": ").Append(p99.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"budget\": ").Append(budget.ToString("0.###", CultureInfo.InvariantCulture))
                  .Append(", \"over\": ").Append(p95 > budget ? "true" : "false").Append(" },\n");
            }
        }
    }
}
