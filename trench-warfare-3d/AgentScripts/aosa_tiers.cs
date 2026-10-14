// AOSA: queue the three-tier stills of one cycle (docs/reference/aosa/README.md "Tiers"). Statement script for
//   unity command --timeout 90 eval_file <abs path to this file>
// In Play, with the battle stepped at least once (holding before the first step draws every vehicle at the origin).
// Inputs through EditorPrefs, because eval_file takes no arguments. The keys are namespaced per checkout
// (EditorPrefs is per Windows user, not per Unity project, so two editors on two checkouts would otherwise
// cross-write); an operator who does not already know the prefix can get it with one eval:
//   return "aosa." + ((uint)UnityEngine.Application.dataPath.GetHashCode()).ToString("x8") + ".";
// and set the required key with one eval (the same expression, so no prefix is typed by hand):
//   UnityEditor.EditorPrefs.SetString("aosa." + ((uint)UnityEngine.Application.dataPath.GetHashCode())
//       .ToString("x8") + ".dir", @"<abs output folder>"); return "set";
//   aosa.<ns>.dir   absolute output folder (docs/reference/aosa/runs/<cycle>/shots)   REQUIRED
//   aosa.<ns>.stem  file stem, e.g. "before" or "c12-after"                             default "shot"
//   aosa.<ns>.hold  1 = CaptureRig.Hold(120) first (pins weather, stops time)           default 1
// Writes <dir>/<stem>_T1.png, _T2.png, _T3.png plus the rig's .json sidecars once CaptureRig.Pending() drains.
// The focus is the densest square of men (CaptureRig.Crowd), so A and B shots of a held frame frame the same men.
string ns = "aosa." + ((uint)UnityEngine.Application.dataPath.GetHashCode()).ToString("x8") + ".";
string dir = UnityEditor.EditorPrefs.GetString(ns + "dir", "");
string stem = UnityEditor.EditorPrefs.GetString(ns + "stem", "shot");
bool hold = UnityEditor.EditorPrefs.GetInt(ns + "hold", 1) == 1;
if (string.IsNullOrEmpty(dir)) return "set EditorPrefs " + ns + "dir first";
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
