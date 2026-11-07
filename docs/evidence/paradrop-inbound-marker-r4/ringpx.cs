// Measures where each of the drop's 8 ring points actually projects on screen, from CaptureRig's own
// projection (Rig.Mark -> mark_px_x/y in the sidecar), at the same pose the committed stills were shot at
// (centre point reproduces mark_px within 1 px; cam_x/y/z match the stills' (290.2854, 132.07288, -59.4156)).
// Run once per point (Rig.Mark changes are only read when a queued Shot renders, so each point must render
// and drain (Pending() == 0) before the next is set), via `Tools/tw run eval_file`. k matches CombatFx.Barrage's
// own loop: a = k/8 * 2pi, d = 7m (even k, DropStreakRingNear) or 11.5m (odd k, DropStreakRingFar),
// at = (45, ground, 82.2). The private +-0.15 rad jitter is not reproduced here; it is covered by ringpx's own
// 20 px tolerance in streakprobe.py.
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var map = host.Local.Map;
float at_x = 45f, at_z = 82.2f;
int k = /* 0..7, set per call */ 0;
double a = (double)k / 8.0 * 6.283185307179586;
float d = (k % 2 == 0) ? 7f : 11.5f;
float px = at_x + (float)System.Math.Cos(a) * d;
float pz = at_z + (float)System.Math.Sin(a) * d;
float py = TW.Presentation.RenderGround.Sample(map, px, pz);
TW.Editor.CaptureRig.Rig.Mark = new UnityEngine.Vector3(px, py, pz);
string path = "<abs path>/ring" + k + ".png";
TW.Editor.CaptureRig.Shot(path, at_x, at_z, 120f, 30f, 25f, 1600, 900);
return px + " " + pz + " " + py;
