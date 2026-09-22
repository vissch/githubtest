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
using UnityEditor;
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
                public int Gap;        // frames to let run after this shot before the next one: how a series is spaced
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
                    for (int k = 0; k < shot.Gap; k++) yield return null;   // let the world move on between stills
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

        /// <summary>
        /// Where the men actually are: the middle of the thickest knot of them, not the average of all of them.
        /// Every capture script this project has written aimed at the centroid, and with two sides facing each other
        /// across a field the centroid is no-man's-land — which is why so many stills came back showing mud and
        /// wire and one man the size of a thumbnail, and why a whole critique round argued about pictures with
        /// nobody in them. Buckets the living into `cell`-metre squares and returns the centre of the fullest.
        /// </summary>
        public static Vector2 Crowd(float cell = 20f)
        {
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null || host.Local == null) return Vector2.zero;
            var w = host.Local.World;
            var count = new Dictionary<int, int>(); var sum = new Dictionary<int, Vector2>();
            int bestKey = 0, bestCount = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & 1u) == 0) continue;
                var p = (Vector3)host.Presenter.Drawn(i);
                int key = Mathf.FloorToInt(p.x / cell) * 4096 + Mathf.FloorToInt(p.z / cell);
                count.TryGetValue(key, out int n); count[key] = n + 1;
                sum.TryGetValue(key, out Vector2 s); sum[key] = s + new Vector2(p.x, p.z);
                if (n + 1 > bestCount) { bestCount = n + 1; bestKey = key; }
            }
            return bestCount > 0 ? sum[bestKey] / bestCount : Vector2.zero;
        }

        /// <summary>A still of the thickest knot of men, so a capture contains soldiers without anyone guessing at
        /// coordinates. Returns the focus it chose and how many men were standing in that square.</summary>
        public static string ShotCrowd(string path, float zoom, float yaw, float pitch = 25f, int w = 1600, int h = 900, float cell = 20f)
        {
            var at = Crowd(cell);
            Shot(path, at.x, at.y, zoom, yaw, pitch, w, h);
            return "queued " + Path.GetFileName(path) + " on the crowd at " + at.x.ToString("0.0", CultureInfo.InvariantCulture) + ", " + at.y.ToString("0.0", CultureInfo.InvariantCulture);
        }

        /// <summary>
        /// A run of stills from one pose, `everyFrames` apart, so a thing that only exists over time — a shell's
        /// smoke column climbing and leaning off, a body sinking, a muzzle flash fading — can be looked at as a
        /// sequence instead of guessed at from one lucky frame. Writes stem_00.png, stem_01.png ... and a .json each.
        /// </summary>
        public static string Series(string dir, string stem, float x, float z, float zoom, float yaw,
                                    float pitch = 25f, int count = 8, int everyFrames = 6, int w = 1600, int h = 900)
        {
            for (int i = 0; i < count; i++)
                Get().Queue.Enqueue(new Rig.Shot
                {
                    Path = Path.Combine(dir, stem + "_" + i.ToString("00") + ".png"),
                    Focus = new Vector2(x, z), Zoom = zoom, Yaw = yaw, Pitch = pitch,
                    W = w, H = h, Gap = i == count - 1 ? 0 : everyFrames,
                });
            return "queued " + count + " frames of " + stem;
        }

        /// <summary>
        /// Tiles a series into one image, because eight PNGs opened one after another is not a sequence you can see.
        /// Scales each still down to fit and lays them left to right, top to bottom, in filename order.
        /// </summary>
        public static string Sheet(string dir, string stem, string outPath, int cols = 4, int cellW = 400)
        {
            var files = new List<string>(Directory.GetFiles(dir, stem + "_*.png"));
            files.Sort(System.StringComparer.Ordinal);
            if (files.Count == 0) return "no stills matching " + stem + "_*.png in " + dir;
            var first = Load(files[0]); if (first == null) return "cannot read " + files[0];
            int cellH = Mathf.Max(1, Mathf.RoundToInt(cellW * first.height / (float)first.width));
            int rows = (files.Count + cols - 1) / cols;
            Object.DestroyImmediate(first);

            var sheet = new Texture2D(cellW * cols, cellH * rows, TextureFormat.RGB24, false);
            var blank = new Color32[sheet.width * sheet.height];
            sheet.SetPixels32(blank);
            for (int i = 0; i < files.Count; i++)
            {
                var t = Load(files[i]); if (t == null) continue;
                int ox = (i % cols) * cellW, oy = (rows - 1 - i / cols) * cellH;   // row 0 at the top
                for (int y = 0; y < cellH; y++)
                    for (int xp = 0; xp < cellW; xp++)
                        sheet.SetPixel(ox + xp, oy + y, t.GetPixelBilinear((xp + .5f) / cellW, (y + .5f) / cellH));
                Object.DestroyImmediate(t);
            }
            sheet.Apply();
            Directory.CreateDirectory(Path.GetDirectoryName(outPath));
            File.WriteAllBytes(outPath, sheet.EncodeToPNG());
            Object.DestroyImmediate(sheet);
            return "sheet of " + files.Count + " -> " + outPath;
        }

        static Texture2D Load(string path)
        {
            if (!File.Exists(path)) return null;
            var t = new Texture2D(2, 2, TextureFormat.RGB24, false);
            if (t.LoadImage(File.ReadAllBytes(path))) return t;
            Object.DestroyImmediate(t);
            return null;
        }

        // ---- holding the world still, so two stills can differ by one thing ----------------------------------

        static float heldScale = -1f;

        /// <summary>
        /// Stops the clock and pins the weather. Two stills taken either side of a change then differ by the change
        /// and by nothing else: no gust, no squall, no man having taken a step, no shell half a metre further on.
        /// Every before-and-after this project has judged until now was confounded by all four at once — which is
        /// why "is it better?" has always been an argument rather than a measurement. Default clock 120 s is a
        /// middling squall; pass another to hold the sky somewhere else on its cycle.
        /// </summary>
        public static string Hold(float weatherClock = 120f)
        {
            if (heldScale < 0f) heldScale = Time.timeScale;
            Time.timeScale = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = weatherClock;
            return "held at weather clock " + weatherClock.ToString("0.#", CultureInfo.InvariantCulture);
        }

        /// <summary>Lets the world run again and gives the weather back its own clock.</summary>
        public static string Release()
        {
            Time.timeScale = heldScale > 0f ? heldScale : 1f;
            heldScale = -1f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            return "released";
        }

        // ---- what actually changed between two stills --------------------------------------------------------

        /// <summary>
        /// Compares two stills pixel for pixel and says what moved. This is the phase-3 instrument: "is what we did
        /// better, yes or no" stops being a matter of opinion when the answer includes how much of the frame the
        /// change touched, where it touched it, and which way each measured number went. A change that was supposed
        /// to be local and comes back touching 90% of the frame is a mistake, whatever it looks like.
        /// </summary>
        public static string Diff(string pathA, string pathB, string outPath = null)
        {
            var a = Load(pathA); var b = Load(pathB);
            if (a == null || b == null) { if (a != null) Object.DestroyImmediate(a); if (b != null) Object.DestroyImmediate(b); return "cannot read both stills"; }
            if (a.width != b.width || a.height != b.height) { Object.DestroyImmediate(a); Object.DestroyImmediate(b); return "stills are different sizes"; }

            int w = a.width, h = a.height;
            var pa = a.GetPixels32(); var pb = b.GetPixels32();
            double sumAbs = 0, dr = 0, dg = 0, db = 0;
            int changed = 0; int minX = w, minY = h, maxX = -1, maxY = -1;
            var absList = new List<float>(pa.Length / 16 + 1);
            for (int i = 0; i < pa.Length; i++)
            {
                float d = Luma(pb[i]) - Luma(pa[i]);
                float ad = Mathf.Abs(d);
                sumAbs += ad;
                dr += (pb[i].r - pa[i].r) / 255.0; dg += (pb[i].g - pa[i].g) / 255.0; db += (pb[i].b - pa[i].b) / 255.0;
                if ((i & 15) == 0) absList.Add(ad);
                if (ad <= 0.02f) continue;                     // below this a difference is encoding noise, not a change
                changed++;
                int x = i % w, y = i / w;
                if (x < minX) minX = x; if (x > maxX) maxX = x;
                if (y < minY) minY = y; if (y > maxY) maxY = y;
            }
            absList.Sort();
            float p99 = absList.Count > 0 ? absList[Mathf.Min(absList.Count - 1, (int)(absList.Count * .99f))] : 0f;

            var sb = new StringBuilder("{\n");
            F(sb, "a", Path.GetFileName(pathA)); F(sb, "b", Path.GetFileName(pathB));
            N(sb, "changed_frac", changed / (float)pa.Length);
            N(sb, "luma_mean_abs", sumAbs / pa.Length);
            N(sb, "luma_p99_abs", p99);
            N(sb, "red_delta", dr / pa.Length); N(sb, "green_delta", dg / pa.Length); N(sb, "blue_delta", db / pa.Length);
            if (maxX >= 0)
            {
                N(sb, "box_x", minX); N(sb, "box_y", minY); N(sb, "box_w", maxX - minX + 1); N(sb, "box_h", maxY - minY + 1);
                N(sb, "box_frac", (maxX - minX + 1) * (maxY - minY + 1) / (float)pa.Length);
            }
            // and the measured numbers either side, so a change of look and a change of cost are read together
            foreach (var key in new[] { "luma_mean", "blown_frac", "black_frac", "men_in_frame", "contrast_median", "contrast_p10", "vertices", "rain", "pose_error_m" })
            {
                double va = Number(Path.ChangeExtension(pathA, ".json"), key), vb = Number(Path.ChangeExtension(pathB, ".json"), key);
                if (double.IsNaN(va) || double.IsNaN(vb)) continue;
                N(sb, key + "_delta", vb - va);
            }
            var notes = new List<string>();
            if (changed / (float)pa.Length < 0.001f) notes.Add("the two stills are the same picture: the change did not reach the frame");
            if (maxX >= 0 && (maxX - minX + 1) * (maxY - minY + 1) / (float)pa.Length > 0.85f) notes.Add("the change touches the whole frame, so it is global, not local");
            double rainA = Number(Path.ChangeExtension(pathA, ".json"), "rain"), rainB = Number(Path.ChangeExtension(pathB, ".json"), "rain");
            if (!double.IsNaN(rainA) && !double.IsNaN(rainB) && System.Math.Abs(rainA - rainB) > 0.01)
                notes.Add("different weather in the two stills: hold the world first or this comparison means nothing");
            sb.Append("  \"notes\": [");
            for (int i = 0; i < notes.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(notes[i]).Append('"');
            sb.Append("]\n}\n");

            if (!string.IsNullOrEmpty(outPath))
            {
                var map = new Texture2D(w, h, TextureFormat.RGB24, false);
                var px = new Color32[pa.Length];
                for (int i = 0; i < pa.Length; i++)
                {
                    float d = Luma(pb[i]) - Luma(pa[i]);
                    byte grey = (byte)(Luma(pa[i]) * 60f);                    // the old frame, dimmed, for bearings
                    byte up = (byte)Mathf.Clamp(d * 900f, 0f, 255f);          // brighter now: green
                    byte down = (byte)Mathf.Clamp(-d * 900f, 0f, 255f);       // darker now: red
                    px[i] = new Color32((byte)Mathf.Min(255, grey + down), (byte)Mathf.Min(255, grey + up), grey, 255);
                }
                map.SetPixels32(px); map.Apply();
                Directory.CreateDirectory(Path.GetDirectoryName(outPath));
                File.WriteAllBytes(outPath, map.EncodeToPNG());
                File.WriteAllText(Path.ChangeExtension(outPath, ".json"), sb.ToString());
                Object.DestroyImmediate(map);
            }
            Object.DestroyImmediate(a); Object.DestroyImmediate(b);
            return sb.ToString();
        }

        /// <summary>Pulls one number out of a measurement file, NaN if it is not there. Our own json, our own reader.</summary>
        static double Number(string json, string key)
        {
            if (!File.Exists(json)) return double.NaN;
            string text = File.ReadAllText(json);
            int at = text.IndexOf("\"" + key + "\":", System.StringComparison.Ordinal);
            if (at < 0) return double.NaN;
            int start = at + key.Length + 3;
            // comma or newline only: a closing-brace CHAR literal would be counted as a real brace by validate.py,
            // which strips string literals but not char ones, and every number we write ends in one of these two
            int end = text.IndexOfAny(new[] { ',', '\n' }, start);
            if (end < 0) return double.NaN;
            return double.TryParse(text.Substring(start, end - start).Trim(), System.Globalization.NumberStyles.Float, CultureInfo.InvariantCulture, out double v) ? v : double.NaN;
        }

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

        // ---- the cost with an army on the field --------------------------------------------------------------

        // The first profile this project ever took was of ten men. Draw calls came back at 936 against a budget of
        // 300, which is alarming until you remember that an almost empty field has no business being near the
        // ceiling at all — the number that decides whether the budget holds is the one taken with the army out.
        // SimHost reads StressUnits once, in Awake, and works the silver out from it, so the preset cannot be
        // switched on in a running game: the run has to set the field in the scene, enter play, wait for both sides
        // to deploy and go over the top, profile, and put the field back. That is what this does, across the domain
        // reload in the middle, which is why the request lives in SessionState and not in a static.
        const string Request = "tw.rig.stress", Restore = "tw.rig.stress.restore";

        /// <summary>
        /// Profiles the game with `unitsPerSide` riflemen deployed by EACH side (so 1000 is the documented 2,000-man
        /// stress preset), then puts the scene back as it was. Call it with the editor in edit mode; it drives play
        /// mode itself and stops it when the numbers are written.
        /// </summary>
        public static string Stress(int unitsPerSide = 1000, string path = "stress.json", int frames = 300, float settleSeconds = 120f)
        {
            if (EditorApplication.isPlaying) return "stop play first: SimHost reads the stress preset in Awake";
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null) return "no SimHost in the open scene: open Scenes/GreyboxCorridor.unity first";
            SessionState.SetString(Restore, host.StressUnits.ToString(CultureInfo.InvariantCulture));
            SessionState.SetString(Request, path + "|" + frames + "|" + settleSeconds.ToString("0.#", CultureInfo.InvariantCulture) + "|" + unitsPerSide);
            host.StressUnits = unitsPerSide;
            EditorUtility.SetDirty(host);           // dirty, never saved: the scene on disk keeps its own setting
            EditorApplication.EnterPlaymode();
            return "entering play with " + (unitsPerSide * 2) + " men; " + path + " will be written and play stopped";
        }

        [InitializeOnLoadMethod]
        static void Hook()
        {
            EditorApplication.playModeStateChanged -= OnPlayMode;   // once per reload, not once per reload ever since
            EditorApplication.playModeStateChanged += OnPlayMode;
        }

        static void OnPlayMode(PlayModeStateChange change)
        {
            if (change == PlayModeStateChange.EnteredPlayMode)
            {
                string req = SessionState.GetString(Request, "");
                if (string.IsNullOrEmpty(req)) return;
                SessionState.EraseString(Request);                  // a crash must not leave the editor looping
                var bits = req.Split('|');
                var run = Get().gameObject.AddComponent<StressRun>();
                run.Path = bits[0];
                run.Frames = int.Parse(bits[1], CultureInfo.InvariantCulture);
                run.Settle = float.Parse(bits[2], NumberStyles.Float, CultureInfo.InvariantCulture);
                run.Wanted = int.Parse(bits[3], CultureInfo.InvariantCulture) * 2;
            }
            else if (change == PlayModeStateChange.EnteredEditMode)
            {
                string was = SessionState.GetString(Restore, "");
                if (string.IsNullOrEmpty(was)) return;
                SessionState.EraseString(Restore);
                var host = Object.FindFirstObjectByType<SimHost>();
                if (host != null) { host.StressUnits = int.Parse(was, CultureInfo.InvariantCulture); EditorUtility.SetDirty(host); }
            }
        }

        /// <summary>Waits for the army to be on the field, profiles it, and stops play.</summary>
        public sealed class StressRun : MonoBehaviour
        {
            public string Path; public int Frames, Wanted; public float Settle;

            System.Collections.IEnumerator Start()
            {
                float until = Time.realtimeSinceStartup + Settle;
                int alive = 0, steady = 0;
                while (Time.realtimeSinceStartup < until)
                {
                    yield return new WaitForSecondsRealtime(1f);
                    int now = Alive();
                    // done when the deployment has stopped growing for five seconds, not merely when it is big:
                    // a run profiled mid-deployment measures a field that is still filling up
                    steady = now > 0 && now == alive ? steady + 1 : 0;
                    alive = now;
                    if (steady >= 5 || alive >= Wanted) break;
                }
                alive = Alive();
                var prof = Get().gameObject.GetComponent<Profiler>() ?? Get().gameObject.AddComponent<Profiler>();
                prof.Extra = "  \"men_alive\": " + alive + ",\n  \"men_wanted\": " + Wanted + ",\n";
                prof.Begin(Path, Frames);
                while (!prof.Done) yield return null;
                Debug.Log("Capture rig: stress profile of " + alive + " men written to " + Path);
                yield return null;
                EditorApplication.ExitPlaymode();
            }

            static int Alive()
            {
                var host = Object.FindFirstObjectByType<SimHost>();
                if (host == null || host.Local == null) return 0;
                var w = host.Local.World;
                int n = 0;
                for (int i = 0; i < w.HighWater; i++) if ((w.Flags[i] & 1u) != 0) n++;
                return n;
            }
        }

        [DefaultExecutionOrder(31000)]
        public sealed class Profiler : MonoBehaviour
        {
            ProfilerRecorder main, gpu, setPass, batches, gc;
            string path; int left; readonly List<double> mainMs = new List<double>(), gpuMs = new List<double>();
            readonly List<double> passes = new List<double>(), draws = new List<double>(), alloc = new List<double>();
            /// <summary>Lines written into the report as they stand: what the run wants said about what it measured.</summary>
            public string Extra = "";
            /// <summary>False until the file is on disk, so a run can wait for it instead of guessing at a duration.</summary>
            public bool Done;

            public void Begin(string p, int frames)
            {
                path = p; left = frames; Done = false;
                mainMs.Clear(); gpuMs.Clear(); passes.Clear(); draws.Clear(); alloc.Clear();
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
                sb.Append(Extra);
                Stat(sb, "main_ms", mainMs, 3.0); Stat(sb, "gpu_ms", gpuMs, 13.0);
                Stat(sb, "setpass", passes, 300); Stat(sb, "draw_calls", draws, 300);
                Stat(sb, "gc_bytes_per_frame", alloc, 0);
                sb.Append("  \"frames\": ").Append(mainMs.Count).Append("\n}\n");
                Directory.CreateDirectory(Path.GetDirectoryName(path));
                File.WriteAllText(path, sb.ToString());
                Debug.Log("Capture rig: frame cost written to " + path + "\n" + sb);
                main.Dispose(); gpu.Dispose(); setPass.Dispose(); batches.Dispose(); gc.Dispose();
                Done = true;
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
