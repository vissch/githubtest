// AOSA: queue the three-tier stills of one cycle (docs/reference/aosa/README.md "Tiers"). Statement script for
//   unity command --timeout 90 eval_file <abs path to this file>
// In Play, with the battle stepped at least once (holding before the first step draws every vehicle at the origin).
// Inputs through EditorPrefs, because eval_file takes no arguments:
//   aosa.dir   absolute output folder (docs/reference/aosa/runs/<cycle>/shots)   REQUIRED
//   aosa.stem  file stem, e.g. "before" or "c12-after"                             default "shot"
//   aosa.hold  1 = CaptureRig.Hold(120) first (pins weather, stops time)           default 1
// Writes <dir>/<stem>_T1.png, _T2.png, _T3.png plus the rig's .json sidecars once CaptureRig.Pending() drains.
// The focus is the densest square of men (CaptureRig.Crowd), so A and B shots of a held frame frame the same men.
string dir = UnityEditor.EditorPrefs.GetString("aosa.dir", "");
string stem = UnityEditor.EditorPrefs.GetString("aosa.stem", "shot");
bool hold = UnityEditor.EditorPrefs.GetInt("aosa.hold", 1) == 1;
if (string.IsNullOrEmpty(dir)) return "set EditorPrefs aosa.dir first";
if (!UnityEngine.Application.isPlaying) return "not in Play";
if (UnityEditor.EditorApplication.isPaused) return "editor is paused: Update does not run, queued shots never fire; unpause and use Hold";
System.IO.Directory.CreateDirectory(dir);
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (tc != null) tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
string held = hold ? TW.Editor.CaptureRig.Hold(120f) : "not held";
var at = TW.Editor.CaptureRig.Crowd(20f);
string t1 = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + "_T1.png"), at.x, at.y, 30f, 21f, 25f, 1920, 1080);
string t2 = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + "_T2.png"), at.x, at.y, 16f, 21f, 22f, 1920, 1080);
string t3 = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + "_T3.png"), at.x, at.y, 7.5f, 21f, 13f, 1920, 1080);
return "hold: " + held + " | focus " + at + " | T1 " + t1 + " | T2 " + t2 + " | T3 " + t3 + " | poll TW.Editor.CaptureRig.Pending() until 0";
