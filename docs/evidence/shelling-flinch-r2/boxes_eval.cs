// run via Tools/tw run eval_file -- --file <this path>
// Reconstructs each still's camera pose from its own sidecar (cam_x/y/z, cam_pitch/cam_yaw, zoom) and the
// Map's own height sample, re-poses Camera.main to exactly that transform and fov, and reads the man's screen
// rect at his feet and feet+1.6m off the SAME focus point the still was shot at (focus_x/focus_z), widened
// half a metre either side and, for the walking cases, by an extra pad metres (his travel during the capture).
// Restores the camera after.
string dir = "C:/Users/PC/Documents/GitHub/githubtest-relay-work3/docs/evidence/shelling-flinch-r2";
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var cam = UnityEngine.Camera.main;
float keepFov = cam.fieldOfView;
UnityEngine.Vector3 keepPos = cam.transform.position; UnityEngine.Quaternion keepRot = cam.transform.rotation;

float close = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.NearZoom, tc.CloseZoom, 7.5f));
float fov = UnityEngine.Mathf.Lerp(tc.Fov, tc.CloseFov, close);
cam.fieldOfView = fov;

System.Func<System.Collections.Generic.Dictionary<string, string>, string, double> Num =
    (d, k) => double.Parse(d[k], System.Globalization.CultureInfo.InvariantCulture);

System.Func<string, System.Collections.Generic.Dictionary<string, string>> ReadJson = (path) =>
{
    var txt = System.IO.File.ReadAllText(path);
    var map = new System.Collections.Generic.Dictionary<string, string>();
    foreach (var line in txt.Split('\n'))
    {
        var l = line.Trim().TrimEnd(',');
        int c = l.IndexOf(':');
        if (c < 0 || !l.StartsWith("\"")) continue;
        string key = l.Substring(1, l.IndexOf('"', 1) - 1);
        string val = l.Substring(c + 1).Trim();
        map[key] = val;
    }
    return map;
};

System.Func<System.Collections.Generic.Dictionary<string, string>, float, float[]> Box = (d, padMetres) =>
{
    var camPos = new UnityEngine.Vector3((float)Num(d, "cam_x"), (float)Num(d, "cam_y"), (float)Num(d, "cam_z"));
    var rot = UnityEngine.Quaternion.Euler((float)Num(d, "cam_pitch"), (float)Num(d, "cam_yaw"), 0f);
    cam.transform.SetPositionAndRotation(camPos, rot);
    float fx = (float)Num(d, "focus_x"), fz = (float)Num(d, "focus_z");
    float ground = host.Local.Map.Height.Sample(fx, fz);
    var feet = new UnityEngine.Vector3(fx, ground, fz);
    var head = new UnityEngine.Vector3(fx, ground + 1.6f, fz);
    var side = cam.transform.right * 0.5f;
    var corners = new[] { feet - side, feet + side, head - side, head + side };
    float minX = float.MaxValue, maxX = float.MinValue, minY = float.MaxValue, maxY = float.MinValue;
    foreach (var c in corners)
    {
        var sp = cam.WorldToScreenPoint(c);
        float sx = sp.x * 1600f / cam.pixelWidth, sy = (cam.pixelHeight - sp.y) * 900f / cam.pixelHeight;
        if (sx < minX) minX = sx; if (sx > maxX) maxX = sx;
        if (sy < minY) minY = sy; if (sy > maxY) maxY = sy;
    }
    float metrePx = maxX - minX;
    float pad = padMetres * metrePx;
    return new[] { minX - pad, minY - pad, maxX + pad, maxY + pad };
};

var cases = new (string stem, int n, float pad)[]
{
    ("stand-base", 6, 0f), ("stand", 2, 0f), ("walk-base", 6, 0.6f), ("walk", 2, 0.6f)
};
var sb = new System.Text.StringBuilder("{\n");
bool first = true;
foreach (var c in cases)
{
    for (int i = 0; i < c.n; i++)
    {
        string stem = c.stem + "_" + i.ToString("00");
        string path = System.IO.Path.Combine(dir, stem + ".json");
        if (!System.IO.File.Exists(path)) continue;
        var d = ReadJson(path);
        var box = Box(d, c.pad);
        if (!first) sb.Append(",\n");
        first = false;
        sb.Append("  \"").Append(stem).Append("\": [")
          .Append(box[0].ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
          .Append(box[1].ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
          .Append(box[2].ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
          .Append(box[3].ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append("]");
    }
}
sb.Append("\n}\n");
System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "boxes.json"), sb.ToString());

cam.fieldOfView = keepFov;
cam.transform.SetPositionAndRotation(keepPos, keepRot);
return "wrote boxes.json";
