#!/usr/bin/env python
"""Shoot the three plate-mask stills this folder's probe.py measures (sentry-look-r2, step 7).

The harsh critic's finding r1 was that the Sentry's plate floated 0.455 m up, swung round with his gun as he
traversed, and vanished the frame he died. An EditMode test can only prove the arithmetic of the new geometry, so
these three stills are shot in the battle itself (GreyboxCorridor, the owner's night look) with the plate drawn in
the flat magenta/cyan mask mode CombatFx.PlateMask turns on, and probe.py then measures them in pixels:

    plate_t.png      he is firing at an enemy group dead ahead
    plate_t60.png    the same camera pose, the group moved ~60 degrees round, so his gun has traversed
    plate_dead.png   the same camera pose again, after he is dead

All three share one camera pose (T3: zoom 7.5, yaw 21, pitch 13, 1920x1080) pinned through EditorPrefs, because
plate_shift_px is only meaningful when the pose did not move. Each gets the .json sidecar CaptureRig writes.

Needs this checkout's editor open and in Play in GreyboxCorridor. On a machine with no desktop (Windows session 0)
that is a batch-mode editor, started without -nographics so the camera still renders:

    "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
        -projectPath <checkout>/trench-warfare-3d -logFile <checkout>/trench-warfare-3d/Logs/batch-editor.log &

Then, from the repo root:  python docs/evidence/sentry-look-r2/shoot.py
"""
import json
import os
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
PROJ = os.path.join(ROOT, "trench-warfare-3d")
TW = os.path.join(PROJ, "Tools", "tw")
SCENE = "Assets/_Project/Scenes/GreyboxCorridor.unity"

SENTRY = 26          # InfantryArchetype.Sentry (RosterEntry.cs)
RIFLEMAN = 0         # the enemy group that gives him something to traverse onto
ZOOM, YAW, PITCH, W, H = 7.5, 21.0, 13.0, 1920, 1080
RANGE = 25.0         # m: close enough that he acquires the group and fires, far enough that he stays put
LOG = os.path.join(HERE, "shoot.log")


def log(line):
    print(line, flush=True)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def tw(*args, **kw):
    """One Tools/tw call. Returns its output; the pipeline answers JSON with a "result" field."""
    out = subprocess.run(["bash", TW] + list(args), capture_output=True, cwd=PROJ,
                         timeout=kw.get("timeout", 180))
    return (out.stdout or b"").decode("utf-8", "replace") + (out.stderr or b"").decode("utf-8", "replace")


def result(text):
    """The `result` field of a pipeline answer, or the raw text when it is not JSON."""
    try:
        return str(json.loads(text[text.index("{"):]).get("result", text)).strip()
    except Exception:
        return text.strip()


def ev(name, body, timeout=180):
    """Run a C# statement body in the editor through eval_file (no usings: everything fully qualified)."""
    path = os.path.join(tempfile.gettempdir(), "sentry_look_r2_%s.cs" % name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    answer = result(tw("run", "eval_file", "--", "--file", path, timeout=timeout))
    log("  %-10s -> %s" % (name, answer))
    return answer


def one_liner(code, timeout=120):
    return result(tw("eval", code, timeout=timeout))


def drain(limit=90):
    """Poll CaptureRig.Pending() until the queue is empty: a shot is posed one frame and photographed the next."""
    for _ in range(limit):
        if one_liner("return TW.Editor.CaptureRig.Pending();").strip('"') == "0":
            return True
        time.sleep(1.0)
    return False


# ---------------------------------------------------------------- the stages

PLACE = """
// Put one Sentry on the field with an enemy group ahead of him, and pin the camera on him at T3 so the near tier
// gives him sockets (VATRenderer.Sockets is false beyond LodDistance, and without a socket no plate is emplaced).
if (!UnityEngine.Application.isPlaying) return "not in Play";
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null || h.Local == null) return "no host";
int before = h.Local.World.HighWater;
int men = 0, foes = 0;
bool ok = h.WriteWorlds(m => {
    men += TW.Presentation.ProvingGround.Place(m, 0, %(sentry)d, 1);
    foes += TW.Presentation.ProvingGround.Place(m, 1, %(rifle)d, 8);
});
if (!ok) return "worlds a tick apart: retry";
var w = h.Local.World;
int slot = -1;
for (int i = before; i < w.HighWater; i++)
    if (w.Archetype[i] == %(sentry)d && w.Team[i] == 0) { slot = i; break; }
if (slot < 0) for (int i = 0; i < w.HighWater; i++)
    if (w.Archetype[i] == %(sentry)d && w.Team[i] == 0) { slot = i; break; }
if (slot < 0) return "no Sentry was placed (men=" + men + ")";
var p = w.Position[slot];
UnityEditor.EditorPrefs.SetInt("sentry.slot", slot);
UnityEditor.EditorPrefs.SetFloat("sentry.x", p.x);
UnityEditor.EditorPrefs.SetFloat("sentry.z", p.z);
// the camera has to be on him before the plate can be emplaced, and FrameFrom is the only move that survives a frame
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (tc == null) return "no TacticalCamera";
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(p.x, p.z), %(zoom)sf, %(yaw)sf);
return "slot=" + slot + " men=" + men + " foes=" + foes + " at=" + p + " tick=" + w.Tick;
""" % dict(sentry=SENTRY, rifle=RIFLEMAN, zoom=ZOOM, yaw=YAW)

# How things stand: are his sockets answering, has the plate been emplaced, and where does his barrel point?
# The barrel yaw is logged at t and at t+60 so the pair is only accepted when the gun really did traverse.
STATE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || units == null || fx == null) return "host/units/fx missing";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var w = h.Local.World;
if (slot < 0 || slot >= w.HighWater) return "no slot";
bool sock = units.Sockets(slot, out var muzzle, out var barrel, out var chest);
float byaw = sock ? UnityEngine.Mathf.Atan2(barrel.x, barrel.z) * UnityEngine.Mathf.Rad2Deg : float.NaN;
var spot = fx.SpotOf(slot);
return "tick=" + w.Tick + " alive=" + w.IsAlive(slot) + " hp=" + w.Hp[slot].ToString("0.0")
     + " sockets=" + sock + " barrel_yaw=" + byaw.ToString("0.0")
     + " unit_yaw=" + (w.Yaw[slot] * UnityEngine.Mathf.Rad2Deg).ToString("0.0")
     + " target=" + w.TargetSlot[slot]
     + " spot_set=" + spot.Set + " spot_foot=" + spot.Foot + " spot_fwd=" + spot.Fwd
     + " pos=" + w.Position[slot] + " mask=" + fx.PlateMask;
"""

# Hold the clock and the weather, turn the mask on, and queue one still from the pinned pose.
SHOT = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || fx == null || tc == null) return "host/fx/camera missing";
if (UnityEditor.EditorApplication.isPaused) return "editor is paused: queued shots never fire";
string dir = UnityEditor.EditorPrefs.GetString("sentry.dir", "");
string stem = UnityEditor.EditorPrefs.GetString("sentry.stem", "plate");
float ax = UnityEditor.EditorPrefs.GetFloat("sentry.x", 0f), az = UnityEditor.EditorPrefs.GetFloat("sentry.z", 0f);
System.IO.Directory.CreateDirectory(dir);
fx.PlateMask = true;                                   // flat magenta plate + cyan 0.50 m bar, so pixels mean metres
string held = TW.Editor.CaptureRig.Hold(120f);          // the clock and the sky pinned identically for all three
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), %(zoom)sf, %(yaw)sf);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     %(zoom)sf, %(yaw)sf, %(pitch)sf, %(w)d, %(h)d);
return "held: " + held + " | " + q;
""" % dict(zoom=ZOOM, yaw=YAW, pitch=PITCH, w=W, h=H)

# Let the world run again, so he can traverse or die between two stills.
RELEASE = """
return TW.Editor.CaptureRig.Release();
"""

# Stand the enemy group on a ring round the Sentry at a given bearing, close enough that he picks them up and fires.
# This is what makes the traverse real: the first still has them dead ahead, the second the same group 60 degrees
# round, so his gun has swung 60 degrees while the plate must not have moved a pixel. Placing them at their rally
# left them 60 m off, he never acquired a target, and his barrel only drifted 0.1 degrees with his idle.
RING = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var w0 = h.Local.World;
if (slot < 0 || slot >= w0.HighWater) return "no slot";
var c = w0.Position[slot];
float a = %(deg)sf * UnityEngine.Mathf.Deg2Rad;
float r = %(radius)sf;
int moved = 0;
bool ok = h.WriteWorlds(m => {
    var w = m.World;
    int k = 0;
    for (int i = 0; i < w.HighWater; i++)
    {
        if (!w.IsAlive(i) || w.Team[i] == 0 || (w.Flags[i] & (uint)TW.Sim.UnitFlags.Vehicle) != 0) continue;
        if (k >= 8) break;                                        // one group, not the whole enemy army
        float spread = (k - 3.5f) * 1.5f;                         // a rank 1.5 m apart, like ProvingGround.ManStep
        float bx = UnityEngine.Mathf.Sin(a), bz = UnityEngine.Mathf.Cos(a);
        w.Position[i] = new Unity.Mathematics.float3(c.x + bx * r + bz * spread, c.y, c.z + bz * r - bx * spread);
        w.TargetSlot[i] = -1;
        k++; moved++;
    }
});
return ok ? ("stood " + (moved / (h.Peer != null ? 2 : 1)) + " foes at " + %(deg)sf + " deg, " + r + " m from " + c)
          : "worlds a tick apart: retry";
"""

# Kill him, through WriteWorlds and the world's own Despawn (never by writing an array from an eval). Despawn leaves
# Archetype alone, which is what lets his plate stay standing: DrawPlate keeps a spot while the slot is still a Sentry.
KILL = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
if (slot < 0) return "no slot";
bool ok = h.WriteWorlds(m => { if (m.World.IsAlive(slot)) m.World.Despawn(slot); });
if (!ok) return "worlds a tick apart: retry";
string held = TW.Editor.CaptureRig.Hold(120f);   // at once: a freed slot the next wave spawns into stops being a
var w = h.Local.World;                           // Sentry, and DrawPlate's reuse guard would then clear his plate
return "killed slot=" + slot + " alive=" + w.IsAlive(slot) + " hp=" + w.Hp[slot]
     + " archetype=" + w.Archetype[slot] + " stance=" + w.StanceOf[slot] + " " + held;
"""


def barrel_yaw(state):
    """The barrel_yaw= field of a STATE answer, so the pair is only accepted when the gun really did traverse."""
    for part in state.split():
        if part.startswith("barrel_yaw="):
            try:
                return float(part.split("=", 1)[1])
            except ValueError:
                return float("nan")
    return float("nan")


def shot(stem):
    """One still: hold, queue it, wait for the queue to drain, and check the file and its sidecar arrived."""
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.stem", "%s"); return "ok";' % stem)
    ev("shot_" + stem, SHOT)
    if not drain():
        log("  !! %s: the capture queue never drained" % stem)
        return False
    png, side = os.path.join(HERE, stem + ".png"), os.path.join(HERE, stem + ".json")
    if not os.path.exists(png):
        log("  !! %s: no PNG arrived" % stem)
        return False
    note = "no sidecar"
    if os.path.exists(side):
        with open(side, encoding="utf-8") as f:
            d = json.load(f)
        note = "pose_error_m=%s men_in_frame=%s" % (d.get("pose_error_m"), d.get("men_in_frame"))
    log("  %-10s -> %s (%d KB) %s" % (stem, stem + ".png", os.path.getsize(png) // 1024, note))
    return True


def main():
    open(LOG, "w", encoding="utf-8").close()
    log("sentry-look-r2 capture  %s" % time.strftime("%Y-%m-%d %H:%M:%S"))
    log("scene %s  pose zoom=%s yaw=%s pitch=%s %dx%d" % (SCENE, ZOOM, YAW, PITCH, W, H))

    log("open the battle scene and enter Play")
    one_liner("TW.Editor.CaptureRig.Release(); UnityEditor.EditorApplication.isPlaying = false; return 1;")
    for _ in range(20):                       # OpenScene refuses in Play, so a stale session must stop first
        time.sleep(2.0)
        if "False" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  scene      -> %s" % one_liner(
        'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("%s"); return "opened";' % SCENE, timeout=300))
    one_liner("UnityEditor.EditorApplication.isPlaying = true; return 1;", timeout=300)
    for _ in range(40):                       # the first seconds of Play build the battlefield
        time.sleep(3.0)
        if "True" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  loaded     -> %s" % one_liner(
        "return UnityEngine.SceneManagement.SceneManager.GetActiveScene().name;"))
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.dir", @"%s"); return "ok";' % HERE)
    one_liner("TW.Editor.CaptureRig.Release(); return 1;")   # never hold before the first step

    log("stage the Sentry")
    placed = ev("place", PLACE)
    if "slot=" not in placed:
        log("FAILED: %s" % placed)
        return 1

    log("let him step, aim and set his plate down")
    for _ in range(12):
        time.sleep(2.5)
        if "spot_set=True" in ev("state", STATE):
            break
    else:
        log("FAILED: the plate was never emplaced (no sockets, or the near tier never took him)")
        return 1

    log("still 1 of 3: firing at the group dead ahead")
    ev("ring_0", RING % dict(deg=0.0, radius=RANGE))
    time.sleep(6.0)                            # he picks them up and comes onto them
    yaw_t = barrel_yaw(ev("state_t", STATE))
    if not shot("plate_t"):
        return 1

    log("still 2 of 3: the same group 60 deg round, so his gun traverses")
    ev("release", RELEASE)
    ev("ring_60", RING % dict(deg=60.0, radius=RANGE))
    time.sleep(8.0)                            # about 60 ticks for him to swing onto them
    yaw_60 = barrel_yaw(ev("state_t60", STATE))
    if not shot("plate_t60"):
        return 1
    swing = abs((yaw_60 - yaw_t + 180.0) % 360.0 - 180.0)
    log("  traverse   -> barrel yaw %.1f -> %.1f deg = %.1f deg of swing" % (yaw_t, yaw_60, swing))
    if swing < 20.0:
        log("  !! the gun barely traversed (%.1f deg): the pair does not test what it is meant to" % swing)

    log("still 3 of 3: after he is dead")
    ev("release", RELEASE)
    ev("kill", KILL)                           # holds the clock itself, so his slot cannot be let again
    ev("state_dead", STATE)
    if not shot("plate_dead"):
        return 1

    ev("release", RELEASE)
    log("done: plate_t.png, plate_t60.png, plate_dead.png and their sidecars are in %s" % HERE)
    return 0


if __name__ == "__main__":
    sys.exit(main())
