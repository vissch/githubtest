// In Play, held: mean GPU ms per render sampler over N frames (Recorder.gpuElapsedNanoseconds), plus the GPU frame time
// (FrameTimingManager). EditorPrefs "tw.gpuprobe.label" names the run; the result is appended to EditorPrefs "tw.gpuprobe"
// (and "running" while it runs). The camera is posed on the bench's standard view every frame, TacticalCamera off.
string label = UnityEditor.EditorPrefs.GetString("tw.gpuprobe.label", "run");
int want = UnityEditor.EditorPrefs.GetInt("tw.gpuprobe.frames", 240);
var all = new System.Collections.Generic.List<string>();
UnityEngine.Profiling.Sampler.GetNames(all);
var rx = new System.Text.RegularExpressions.Regex("(?i)draw|shadow|ink|copy|blit|depth|opaque|transparent|post|uber|final|skybox|full ?screen|render|color|lut|prepass|gbuffer|lights");
var names = new System.Collections.Generic.List<string>();
var recs = new System.Collections.Generic.List<UnityEngine.Profiling.Recorder>();
foreach (var n in all)
{
    if (!rx.IsMatch(n)) continue;
    var r = UnityEngine.Profiling.Sampler.Get(n).GetRecorder();
    if (r == null || !r.isValid) continue;
    r.enabled = true; names.Add(n); recs.Add(r);
}
var sums = new double[recs.Count]; var hits = new int[recs.Count];
double gpuSum = 0; int gpuN = 0;
var ft = new UnityEngine.FrameTiming[1];
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
var cam = UnityEngine.Camera.main;
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (tc == null || cam == null || host == null) return "no camera/host";
tc.enabled = false;
var focus = new UnityEngine.Vector2(46.3951f, 51.5661f);
System.Action pose = () =>
{
    tc.Zoom = UnityEngine.Mathf.Clamp(30f, tc.ZoomMin, tc.ZoomMax); tc.Focus = focus;
    TW.Presentation.SceneHooks.CloseUp = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.DetailFullZoom, tc.DetailGoneZoom, tc.Zoom));
    UnityEngine.Shader.SetGlobalFloat("_TWClose", TW.Presentation.SceneHooks.CloseUp);
    float close = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, tc.Zoom));
    float fov = UnityEngine.Mathf.Lerp(tc.Fov, tc.CloseFov, close);
    cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
    var rot = UnityEngine.Quaternion.Euler(UnityEngine.Mathf.Clamp(25f, tc.PitchMin, tc.PitchMax), tc.BaseYaw + 21f, 0f);
    float distance = tc.Zoom * UnityEngine.Mathf.Tan(30f * UnityEngine.Mathf.Deg2Rad) / UnityEngine.Mathf.Tan(fov * 0.5f * UnityEngine.Mathf.Deg2Rad);
    float ground = host.Local.Map.Height.Sample(focus.x, focus.y);
    var aim = new UnityEngine.Vector3(focus.x, ground * close + 1.1f * close, focus.y);
    cam.transform.SetPositionAndRotation(aim - rot * UnityEngine.Vector3.forward * distance, rot);
};
pose();
int lastFrame = UnityEngine.Time.frameCount, frames = 0;
UnityEditor.EditorPrefs.SetString("tw.gpuprobe.state", "running");
UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    if (!UnityEditor.EditorApplication.isPlaying || cam == null || tc == null || host == null) { UnityEditor.EditorApplication.update -= tick; UnityEditor.EditorPrefs.SetString("tw.gpuprobe.state", "lost the scene"); return; }
    if (UnityEngine.Time.frameCount == lastFrame) return;
    lastFrame = UnityEngine.Time.frameCount; frames++;
    pose();
    UnityEngine.FrameTimingManager.CaptureFrameTimings();
    if (frames <= 12) return;   // settle: the recorders report three frames late, and the pose must have been seen
    if (UnityEngine.FrameTimingManager.GetLatestTimings(1, ft) > 0 && ft[0].gpuFrameTime > 0) { gpuSum += ft[0].gpuFrameTime; gpuN++; }
    for (int k = 0; k < recs.Count; k++)
    {
        long ns = recs[k].gpuElapsedNanoseconds;
        if (ns > 0) { sums[k] += ns / 1e6; hits[k]++; }
    }
    if (frames < want + 12) return;
    UnityEditor.EditorApplication.update -= tick;
    int n = frames - 12;
    var order = new System.Collections.Generic.List<int>();
    for (int k = 0; k < recs.Count; k++) if (sums[k] / n >= 0.01) order.Add(k);
    order.Sort((a, b) => (sums[b]).CompareTo(sums[a]));
    var sb = new System.Text.StringBuilder();
    sb.Append("== ").Append(label).Append(": gpu frame ").Append((gpuSum / System.Math.Max(1, gpuN)).ToString("0.000")).Append(" ms over ").Append(gpuN).Append(" frames\n");
    foreach (var k in order) sb.Append("  ").Append(names[k]).Append(": ").Append((sums[k] / n).ToString("0.000")).Append(" ms").Append(hits[k] < n ? " (" + hits[k] + "/" + n + ")" : "").Append('\n');
    foreach (var r in recs) r.enabled = false;
    UnityEditor.EditorPrefs.SetString("tw.gpuprobe", UnityEditor.EditorPrefs.GetString("tw.gpuprobe", "") + sb);
    UnityEditor.EditorPrefs.SetString("tw.gpuprobe.state", "done " + label);
};
UnityEditor.EditorApplication.update += tick;
return "probing " + recs.Count + " samplers for " + want + " frames as " + label;
