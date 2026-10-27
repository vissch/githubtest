// Where the man is drawn, in each still's own pixels. Run in the editor, in Play, with the man of this evidence
// set still standing in the scene:
//     Tools/tw run eval_file -- --file <abs path to this file>
// It writes boxes.json beside the stills: one [x0, y0, x1, y1] per stem, in the stills' 2400x1350 pixels.
//
// WHY it projects by hand instead of calling Camera.WorldToScreenPoint: the capture camera only exists for the
// length of a CaptureRig set, and it is NOT the scene camera's pose — CaptureRig.Pose computes its own. Every
// sidecar records that pose (cam_x/y/z, cam_pitch, cam_yaw), so the box is rebuilt from the sidecar, which means
// a still and its box can never drift apart, and anyone can redo this from the committed files alone.
//
// The box is his feet-to-helmet rect, then grown 18 px all round (the probe's search window, so a block that
// starts just outside him is still matched) and a further 25 % of its height downward, because a duck moves him
// DOWN and out of a box drawn around a standing man.
var dir = @"C:/Users/PC/Documents/GitHub/githubtest-relay-work5/docs/evidence/shelling-flinch-g2/";
const int W = 2400, H = 1350;
const float FOV = 25f;          // zoom 15 is at CloseZoom, so CaptureRig.Pose's fov blend is 0: the standard lens
const float MAN_H = 1.8f;       // a man, helmet to boots
const float HALF_W = 0.75f;     // his shoulders and rifle, each side of his feet
const float PAD = 18f;          // the probe's search window
const float DROP = 0.25f;       // and a quarter of his height below, for the duck
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
float mx = 89.5f, mz = 120f;
float ground = host.Local.Map.Height.Sample(mx, mz);
var sb = new System.Text.StringBuilder("{\n");
var files = new System.Collections.Generic.List<string>(System.IO.Directory.GetFiles(dir, "stand_*.json"));
files.Sort(System.StringComparer.Ordinal);
for (int i = 0; i < files.Count; i++)
{
    var txt = System.IO.File.ReadAllText(files[i]);
    System.Func<string, float> f = key => {
        int k = txt.IndexOf("\"" + key + "\":") + key.Length + 4;
        int e = txt.IndexOfAny(new[] { ',', '\n', '}' }, k);
        return float.Parse(txt.Substring(k, e - k).Trim(), System.Globalization.CultureInfo.InvariantCulture);
    };
    var cam = new UnityEngine.Vector3(f("cam_x"), f("cam_y"), f("cam_z"));
    var rot = UnityEngine.Quaternion.Euler(f("cam_pitch"), f("cam_yaw"), 0f);
    var right = rot * UnityEngine.Vector3.right; var up = rot * UnityEngine.Vector3.up; var fwd = rot * UnityEngine.Vector3.forward;
    float t = UnityEngine.Mathf.Tan(FOV * 0.5f * UnityEngine.Mathf.Deg2Rad), aspect = (float)W / H;
    System.Func<UnityEngine.Vector3, UnityEngine.Vector2> project = p => {
        var rel = p - cam;
        float vx = UnityEngine.Vector3.Dot(rel, right), vy = UnityEngine.Vector3.Dot(rel, up), vz = UnityEngine.Vector3.Dot(rel, fwd);
        return new UnityEngine.Vector2((vx / (vz * t * aspect) * 0.5f + 0.5f) * W, (1f - (vy / (vz * t) * 0.5f + 0.5f)) * H);
    };
    var feet = project(new UnityEngine.Vector3(mx, ground, mz));
    var head = project(new UnityEngine.Vector3(mx, ground + MAN_H, mz));
    var flat = new UnityEngine.Vector3(right.x, 0f, right.z).normalized;   // along the SCREEN, not along world X: at yaw 291 world +X is mostly depth
    var side = project(new UnityEngine.Vector3(mx, ground, mz) + flat * HALF_W);
    float halfPx = UnityEngine.Mathf.Abs(side.x - feet.x);
    float x0 = feet.x - halfPx - PAD, x1 = feet.x + halfPx + PAD;
    float y0 = head.y - PAD, y1 = feet.y + PAD;
    y1 += (y1 - y0) * DROP;
    string stem = System.IO.Path.GetFileNameWithoutExtension(files[i]);
    sb.Append("  \"").Append(stem).Append("\": [")
      .Append(x0.ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
      .Append(y0.ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
      .Append(x1.ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append(", ")
      .Append(y1.ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)).Append("]")
      .Append(i == files.Count - 1 ? "\n" : ",\n");
}
sb.Append("}\n");
System.IO.File.WriteAllText(dir + "boxes.json", sb.ToString());
return sb.ToString();
