#!/usr/bin/env python
"""Shoot the three Sentry-plate stills this folder's probe.py measures (sentry-look-r4, fix 2).

The harsh critic's r1 finding fix 2 on sentry-look-r2: its plate_dead.json read drawn_infantry 1 and fallen 0 after
the Despawn, so no death ever played - r2's KILL stage called CaptureRig.Hold in the same eval that despawned him,
Time.timeScale went to 0, and no tick ever reached CombatFx.OnDeath. And the plate had only ever been seen as a flat
magenta mask, never as the night look it ships as.

So here: one Sentry alone on the field (no enemy group - drawn_infantry has to reach 0, and a live foe in frame would
hold it above 0), killed through the world's own SimWorld.Despawn inside SimHost.WriteWorlds with the clock running,
then 30 ticks (20 Hz, ~1.5 s) of real world time polled until VATRenderer.FallenCount >= 1 and DrawnInfantry == 0
while his slot is still a Sentry and his plate spot is still set.

    plate_t.png      the plate while he lives, mask on   -> probe.json plate_px_t
    plate_dead.png   the same pose after the death ran    -> plate_px_after_death, fallen, drawn_infantry
    plate_look.png   the same pose, mask off              -> the unmasked T3 night look the critic asked for

The three stills share one camera pose (zoom 7.5, yaw 21, pitch 13, 1920x1080), because a pixel count is only
comparable from one pose. CaptureRig clamps a shot to TacticalCamera.NearZoom, which is derived (not a field, so an
eval cannot lower it) and on this branch stands the camera 14 m up: the plate is some 1,300 px of the frame rather
than r2's 10,100. probe.py therefore also cuts plate_look_crop.png out of the unmasked still, around the plate. Each gets the .json sidecar CaptureRig writes.

Needs this checkout's editor open. On a machine with no desktop (Windows session 0) that is a batch-mode editor,
started without -nographics (so the camera still renders) and without -quit:

    "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
        -projectPath <checkout>/trench-warfare-3d -logFile <checkout>/trench-warfare-3d/Logs/batch-editor.log &

Then, from the repo root:  python docs/evidence/sentry-look-r4/shoot.py
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
ZOOM, YAW, PITCH, W, H = 7.5, 21.0, 13.0, 1920, 1080
TICKS = 30           # the death has to have this many ticks to play out, at 20 Hz
WAIT_S = 3.0         # the clock runs free this long after the kill: 30 ticks is 1.5 s, and the freed slot is re-let
                     # a second or two later, so the window is narrow at both ends
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
    path = os.path.join(tempfile.gettempdir(), "sentry_look_r4_%s.cs" % name)
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
// One Sentry alone on the field, and the camera pinned on him at T3 so the near tier gives him sockets
// (VATRenderer.Sockets is false beyond LodDistance, and without a socket no plate is emplaced). No enemy group:
// drawn_infantry_after_death must reach 0, and any other living man in frame would hold it above 0.
if (!UnityEngine.Application.isPlaying) return "not in Play";
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null || h.Local == null) return "no host";
int before = h.Local.World.HighWater;
int men = 0;
bool ok = h.WriteWorlds(m => { men += TW.Presentation.ProvingGround.Place(m, 0, %(sentry)d, 1); });
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
return "slot=" + slot + " men=" + men + " at=" + p + " tick=" + w.Tick;
""" % dict(sentry=SENTRY, zoom=ZOOM, yaw=YAW)

# How things stand, so every wait in this script is measured instead of slept through: the tick, whether his sockets
# answer and his plate spot is set, whether the slot is still a Sentry, and what the renderer drew last frame
# (fallen = VATRenderer.FallenCount, drawn = DrawnInfantry - the two numbers the critic caught r2 on).
STATE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || units == null || fx == null) return "host/units/fx missing";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var w = h.Local.World;
if (slot < 0 || slot >= w.HighWater) return "no slot";
bool sock = units.Sockets(slot, out var muzzle, out var barrel, out var chest);
var spot = fx.SpotOf(slot);
return "tick=" + w.Tick + " alive=" + w.IsAlive(slot) + " hp=" + w.Hp[slot].ToString("0.0")
     + " archetype=" + w.Archetype[slot] + " sockets=" + sock
     + " spot_set=" + spot.Set + " spot_foot=" + spot.Foot + " spot_fwd=" + spot.Fwd
     + " fallen=" + units.FallenCount + " drawn=" + units.DrawnInfantry
     + " pos=" + w.Position[slot] + " mask=" + fx.PlateMask;
"""

# Hold the clock and the weather, set the mask as asked, and queue one still from the pinned pose.
SHOT = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || fx == null || tc == null) return "host/fx/camera missing";
if (UnityEditor.EditorApplication.isPaused) return "editor is paused: queued shots never fire";
string dir = UnityEditor.EditorPrefs.GetString("sentry.dir", "");
string stem = UnityEditor.EditorPrefs.GetString("sentry.stem", "plate");
bool wantMask = UnityEditor.EditorPrefs.GetBool("sentry.mask", true);
float ax = UnityEditor.EditorPrefs.GetFloat("sentry.x", 0f), az = UnityEditor.EditorPrefs.GetFloat("sentry.z", 0f);
System.IO.Directory.CreateDirectory(dir);
fx.PlateMask = wantMask;                               // flat magenta plate + cyan 0.50 m bar, so pixels mean metres
string held = TW.Editor.CaptureRig.Hold(120f);          // the clock and the sky pinned identically for every still
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), %(zoom)sf, %(yaw)sf);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     %(zoom)sf, %(yaw)sf, %(pitch)sf, %(w)d, %(h)d);
return "mask=" + wantMask + " held: " + held + " | " + q;
""" % dict(zoom=ZOOM, yaw=YAW, pitch=PITCH, w=W, h=H)

# Let the world run again. Nothing may hold the clock while the death plays: that was r2's whole bug.
RELEASE = """
return TW.Editor.CaptureRig.Release();
"""

# Pin the camera point on the plate's own spot. The man walks; his emplaced plate does not, so framing the spot is
# what makes the two stills share one pose - and it keeps the plate in the middle of the frame.
AIM = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || fx == null) return "host/fx missing";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var spot = fx.SpotOf(slot);
if (!spot.Set) return "no spot to aim at";
UnityEditor.EditorPrefs.SetFloat("sentry.x", spot.Foot.x);
UnityEditor.EditorPrefs.SetFloat("sentry.z", spot.Foot.z);
return "aimed at " + spot.Foot;
"""

# Kill him: Release first, then the world's own Despawn inside WriteWorlds (never an array write from an eval), and
# no Hold in this eval - the ticks that follow are what carry the Death event to CombatFx.OnDeath. Despawn leaves
# Archetype alone, which is what lets his plate stay standing: CombatFx.KeepSpot keeps a spot while the slot is
# still a Sentry.
KILL = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
if (slot < 0) return "no slot";
TW.Editor.CaptureRig.Release();
uint tick0 = h.Local.World.Tick;
bool ok = h.WriteWorlds(m => {
    if (m.World.IsAlive(slot))
        m.World.Despawn(slot, -1, new Unity.Mathematics.float3(0f, 0f, 0f), 0f);
});
if (!ok) return "worlds a tick apart: retry";
// SimWorld.Events is a one-tick ring, and SimWorld.Step clears it before every tick: a Death added between frames,
// as this one is, would be thrown away before SimHost's own Events.Collect ever saw it (that is why r2's
// plate_dead.json read fallen 0). So the event is collected here, in the same eval, straight off the ring; the
// pump dispatches it to CombatFx.OnDeath in the next frame's Update, and the fallen man is added there.
h.Events.Collect(h.Local.World);
var w = h.Local.World;
return "killed slot=" + slot + " tick0=" + tick0 + " alive=" + w.IsAlive(slot)
     + " hp=" + w.Hp[slot] + " archetype=" + w.Archetype[slot];
"""


HOLD_NOW = """
// The freed slot is re-let within a second or so, and CombatFx.KeepSpot clears the plate the moment the archetype
// changes - so the clock is held here, in its own eval, as soon as the death has had its ticks.
var held = TW.Editor.CaptureRig.Hold(120f);
return held;
"""


def field(state, key, cast=str):
    """One `key=value` field of a STATE answer."""
    for part in state.split():
        if part.startswith(key + "="):
            try:
                return cast(part.split("=", 1)[1])
            except ValueError:
                return None
    return None


def shot(stem, mask=True):
    """One still: hold, queue it, wait for the queue to drain, and check the file and its sidecar arrived."""
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.stem", "%s");'
              ' UnityEditor.EditorPrefs.SetBool("sentry.mask", %s); return "ok";'
              % (stem, "true" if mask else "false"))
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
        note = "pose_error_m=%s fallen=%s drawn_infantry=%s men_in_frame=%s" % (
            d.get("pose_error_m"), d.get("fallen"), d.get("drawn_infantry"), d.get("men_in_frame"))
    log("  %-10s -> %s (%d KB) %s" % (stem, stem + ".png", os.path.getsize(png) // 1024, note))
    return True


def main():
    open(LOG, "w", encoding="utf-8").close()
    log("sentry-look-r4 capture  %s" % time.strftime("%Y-%m-%d %H:%M:%S"))
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

    log("stage the Sentry, alone")
    placed = ev("place", PLACE)
    if "slot=" not in placed:
        log("FAILED: %s" % placed)
        return 1

    log("let him step and set his plate down")
    state = ""
    for _ in range(12):
        time.sleep(2.5)
        state = ev("state", STATE)
        if "spot_set=True" in state:
            break
    else:
        log("FAILED: the plate was never emplaced (no sockets, or the near tier never took him)")
        return 1

    ev("aim", AIM)   # both stills are framed on the plate's own spot, so the pose cannot drift with the man

    log("still 1 of 3: the plate while he lives (mask)")
    if not shot("plate_t"):
        return 1

    log("kill him through the world's own Despawn, with the clock running")
    ev("release", RELEASE)
    before = ev("state_before", STATE)
    tick0 = field(before, "tick", int)
    fallen0 = field(before, "fallen", int)
    log("  fallen before the kill: %s (the battle round him has its own dead)" % fallen0)
    killed = ev("kill", KILL)
    if "killed slot=" not in killed:
        log("FAILED: %s" % killed)
        return 1
    tick0 = field(killed, "tick0", int)
    if tick0 is None:
        log("FAILED: no tick before the kill")
        return 1

    log("let the death play for %.1f s (>= %d ticks at 20 Hz), then hold the clock at once" % (WAIT_S, TICKS))
    time.sleep(WAIT_S)
    ev("hold", HOLD_NOW)
    state = ev("state_dead", STATE)
    tick = field(state, "tick", int)
    fallen = field(state, "fallen", int)
    drawn = field(state, "drawn", int)
    arch = field(state, "archetype", int)
    if arch != SENTRY or "spot_set=True" not in state:
        log("FAILED: his slot was re-let (archetype=%s) or his plate spot cleared: re-run the capture" % arch)
        return 1
    if tick is None or tick - tick0 < TICKS:
        log("FAILED: only %s ticks ran after the kill: the clock was still held" % (
            None if tick is None else tick - tick0))
        return 1
    if fallen is None or fallen < 1 or drawn != 0:
        log("FAILED: the death never showed: fallen=%s drawn=%s" % (fallen, drawn))
        return 1
    log("  death     -> tick %d -> %d (+%d), fallen=%d, drawn=%d" % (tick0, tick, tick - tick0, fallen, drawn))

    log("still 2 of 3: the same pose, after the death ran (mask)")
    if not shot("plate_dead"):
        return 1

    log("still 3 of 3: the same held frame, mask off - the T3 night look")
    if not shot("plate_look", mask=False):
        return 1

    ev("release", RELEASE)
    log("done: plate_t.png, plate_dead.png, plate_look.png and their sidecars are in %s" % HERE)
    return 0


if __name__ == "__main__":
    sys.exit(main())
