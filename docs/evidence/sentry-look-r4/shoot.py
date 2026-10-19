#!/usr/bin/env python
"""Shoot the stills this folder's probe.py measures (sentry-look-r4, step 5).

r2 proved the plate stands still and survives a Despawn, but its `plate_dead.json` read `drawn_infantry 1,
fallen 0`: no death ever played. SimWorld.Step() clears Events at its start and SimHost.LocalStepped collects them
only after a step, so r2's Despawn was never collected, CombatFx.OnDeath never ran, VATRenderer.AddFallen was never
called, and the presenter still held his standing pose.

Here the Death is a real one and it is delivered. SimWorld.Despawn adds a SimEventType.Death with a killer slot,
exactly as a bullet would; the same eval collects it into the pump before any step could clear it, and SimHost.Update
dispatches it on the next frame, so CombatFx.OnDeath runs and the fallen body is added.

All of it happens with the clock stopped. Slow motion was tried first and does not exist here: MatchClock.Update
writes SimHost.TimeScale back every frame and clamps it to 0.25, and Unity's own Time.timeScale, measured, changed
the tick rate not at all (32 ticks a second at timeScale 0.05). At full speed one Tools/tw call is worth a hundred
ticks, and six runs died in that gap - the battle shot him before the live still, or his slot went to the next wave
and DrawPlate dropped his plate with it (a reused slot keeps no dead man's plate). So the clock is stopped the moment
his plate is down and never started again, and SimPresenter.Capture is called by hand where a step would have called
it. Both masked stills are then the same frozen instant, from the same camera, and the only difference between them
is his death - which is exactly the claim they are evidence for.

    plate_t.png      mask on, alive behind his plate
    plate_dead.png   mask on, the same instant, after the Death was dispatched
    night_dead.png   mask off, the same frame again: the unmasked T3 night still, the plate standing over his body

One camera pose for all three, and it is picked by measuring, not guessing: pick_yaw sweeps the circle in 30 degree
steps and keeps the yaw that sees the most of the plate, because a plate half hidden by the man in front of it cannot
be compared to itself within 2 per cent. Each still gets the .json sidecar CaptureRig writes.

Needs this checkout's editor open and in Play in GreyboxCorridor. On a machine with no desktop (Windows session 0)
that is a batch-mode editor, started without -nographics so the camera still renders:

    "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode         -projectPath <checkout>/trench-warfare-3d -logFile <checkout>/trench-warfare-3d/Logs/batch-editor.log &

Then, from the repo root:  python docs/evidence/sentry-look-r4/shoot.py
"""
import glob
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
STAND_BACK = 2.5     # m: how far behind his plate he stands. A body lies about 1.8 m: closer than this
                     # and it falls across the plate and hides the very thing the still is of
RIFLEMAN = 0         # the enemy group that gives him something to traverse onto
ZOOM, YAW, PITCH, W, H = 7.5, 21.0, 13.0, 1920, 1080
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
     + " fallen=" + units.FallenCount + " drawn=" + units.DrawnInfantry
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
// mask on: flat magenta plate + cyan 0.50 m bar, so pixels mean metres. Off: the plain night look the owner asked for.
fx.PlateMask = UnityEditor.EditorPrefs.GetBool("sentry.mask", true);
string held = TW.Editor.CaptureRig.Hold(120f);          // the clock and the sky pinned identically for all three
float yaw = UnityEditor.EditorPrefs.GetFloat("sentry.yaw", %(yaw)sf);   // one pose for all three stills
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), %(zoom)sf, yaw);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     %(zoom)sf, yaw, %(pitch)sf, %(w)d, %(h)d);
return "held: " + held + " | yaw=" + yaw.ToString("0.0") + " | " + q;
""" % dict(zoom=ZOOM, yaw=YAW, pitch=PITCH, w=W, h=H)

# Let the world run again, so he can traverse or die between two stills.
RELEASE = """
return TW.Editor.CaptureRig.Release();
"""


# The world runs about a hundred ticks a wall-clock second here, and one Tools/tw call costs a second or more: the
# second run placed the enemy ring and found him dead, his slot already let to the next wave, nine hundred ticks later.
# Slow motion was tried first and does not exist here. SimHost.TimeScale is written back every frame by
# MatchClock.Update and clamped to 0.25; Unity's own Time.timeScale, measured, changed nothing at all - at 0.05 the
# world still ran 32 ticks a second. So the clock is stopped outright the moment his plate is down, and everything
# after that happens in one frozen instant: nothing moves, nothing spawns, no slot is let again, no body ages out.
#
# A frozen world is not a dead one. SimHost.Update still runs every frame, so Events.Dispatch() still delivers, and
# SimPresenter.Capture can be called by hand to show the presenter what WriteWorlds just changed - which is the one
# thing a stopped clock otherwise hides (Capture normally only runs on a step).
STAGE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
string held = TW.Editor.CaptureRig.Hold(120f);   // from here nothing moves: see STAGE
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var spot = fx != null && slot >= 0 ? fx.SpotOf(slot) : default;
if (spot.Set)                                // the ring stands round the plate, not round a man who may walk off
{
    UnityEditor.EditorPrefs.SetFloat("sentry.spot_x", spot.Foot.x);
    UnityEditor.EditorPrefs.SetFloat("sentry.spot_z", spot.Foot.z);
    UnityEditor.EditorPrefs.SetFloat("sentry.fwd_x", spot.Fwd.x);
    UnityEditor.EditorPrefs.SetFloat("sentry.fwd_z", spot.Fwd.z);
    UnityEditor.EditorPrefs.SetFloat("sentry.x", spot.Foot.x);     // the pinned pose looks at the plate itself, so
    UnityEditor.EditorPrefs.SetFloat("sentry.z", spot.Foot.z);     // both stills frame it the same way
    // and it looks along the line from the man to his plate. At the stock yaw he stood between the camera and his own
    // plate while he lived and lay out of the way once he was dead, so the same plate measured 5429 px alive and
    // 11232 px dead - a 2 %% band cannot be read through a man. On this line the plate is in front of him in both.
    var him = h.Local.World.Position[slot];
    var toPlate = new UnityEngine.Vector2(spot.Foot.x - him.x, spot.Foot.z - him.z);
    float face = toPlate.sqrMagnitude > 0.01f
               ? UnityEngine.Mathf.Atan2(toPlate.x, toPlate.y) * UnityEngine.Mathf.Rad2Deg
               : UnityEngine.Mathf.Atan2(spot.Fwd.x, spot.Fwd.z) * UnityEngine.Mathf.Rad2Deg;
    UnityEditor.EditorPrefs.SetFloat("sentry.yaw", face);
}
// and he is put behind it, STAND_BACK m back along the way it faces - which is where a Sentry firing over his plate
// stands. He emplaces it where he first stops and then walks on to his post, which left the plate alone in one still
// and him alone in the next; and pinned on the plate itself his body fell across it and hid it (11616 magenta pixels
// became 336). Behind it he dies where the one pinned camera pose sees both him and the whole plate.
if (spot.Set)
    h.WriteWorlds(m => {
        var w = m.World;
        if (!w.IsAlive(slot)) return;
        w.Position[slot] = new Unity.Mathematics.float3(spot.Foot.x - spot.Fwd.x * %(back)sf, w.Position[slot].y,
                                                       spot.Foot.z - spot.Fwd.z * %(back)sf);
        w.PostCell[slot] = -1;
    });
return held + " yaw=" + UnityEditor.EditorPrefs.GetFloat("sentry.yaw", %(yaw)sf).ToString("0.0") + " spot=" + spot.Foot
     + " at=" + (slot >= 0 ? h.Local.World.Position[slot].ToString() : "?");
""" % dict(yaw=YAW, back=STAND_BACK)

# What the world looks like at a given moment: alive, the archetype in his slot, whether his plate spot still stands,
# and the two counters the critic read. It holds the clock the instant it finds him dead - which by the kill phase it
# already is, so there it only reports.
WATCH = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || units == null || fx == null) return "host/units/fx missing";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var w = h.Local.World;
if (slot < 0 || slot >= w.HighWater) return "no slot";
float sx = UnityEditor.EditorPrefs.GetFloat("sentry.spot_x", 0f), sz = UnityEditor.EditorPrefs.GetFloat("sentry.spot_z", 0f);
bool alive = w.IsAlive(slot);
int death = UnityEditor.EditorPrefs.GetInt("sentry.death_tick", -1);
if (alive)
    h.WriteWorlds(m => {                     // keep him on the spot: a walk off would leave his plate alone in frame
        var ww = m.World;
        if (!ww.IsAlive(slot)) return;
        ww.Position[slot] = new Unity.Mathematics.float3(sx, ww.Position[slot].y, sz);
        ww.PostCell[slot] = -1;
    });
else if (death < 0) { death = (int)w.Tick; UnityEditor.EditorPrefs.SetInt("sentry.death_tick", death); }
string act = "running";
if (!alive) act = TW.Editor.CaptureRig.Hold(120f);   // at once, should it ever find him alive
var spot = fx.SpotOf(slot);
return "tick=" + w.Tick + " alive=" + alive + " death_tick=" + death + " since=" + (death < 0 ? -1 : (int)w.Tick - death)
     + " archetype=" + w.Archetype[slot] + " spot_set=" + spot.Set
     + " fallen=" + units.FallenCount + " drawn=" + units.DrawnInfantry + " act=" + act;
"""

# Everybody but the Sentry, 80 m away, and him standing at his own plate. The plate's pixel count only means "the
# plate is still standing" when nothing else is in front of it, and drawn_infantry only means "nobody is standing
# behind this plate" when the frame is his alone: one run failed its own test because a living rifleman happened to
# stand between the camera and the plate in the second still (7157 magenta pixels became 1286, drawn_infantry 1).
# The clock is already stopped here, so SimPresenter.Capture is called by hand: it is what a step would have done.
CLEAR = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
float sx = UnityEditor.EditorPrefs.GetFloat("sentry.spot_x", 0f), sz = UnityEditor.EditorPrefs.GetFloat("sentry.spot_z", 0f);
float fx = UnityEditor.EditorPrefs.GetFloat("sentry.fwd_x", 0f), fz = UnityEditor.EditorPrefs.GetFloat("sentry.fwd_z", 1f);
int moved = 0;
bool ok = h.WriteWorlds(m => {
    var w = m.World;
    for (int i = 0; i < w.HighWater; i++)
    {
        if (i == slot || !w.IsAlive(i)) continue;
        float a = (moved %% 36) * 10f * UnityEngine.Mathf.Deg2Rad;
        w.Position[i] = new Unity.Mathematics.float3(sx + UnityEngine.Mathf.Sin(a) * 80f, w.Position[i].y,
                                                     sz + UnityEngine.Mathf.Cos(a) * 80f);
        w.TargetSlot[i] = -1; w.PostCell[i] = -1;
        moved++;
    }
    if (slot >= 0 && w.IsAlive(slot))                                      // and he stands behind his own plate
        w.Position[slot] = new Unity.Mathematics.float3(sx - fx * %(back)sf, w.Position[slot].y, sz - fz * %(back)sf);
});
if (!ok) return "worlds a tick apart: retry";
h.Presenter.Capture(h.Local.World);          // the clock is stopped, so nothing else will show the move
return "stood " + (moved / (h.Peer != null ? 2 : 1)) + " men 80 m off";
""" % dict(back=STAND_BACK)


# The kill: the sim's own death, not a pose. SimWorld.Despawn adds a SimEventType.Death with a killer slot and an
# impulse, exactly as a bullet would, and the three things r2 got wrong are repaired in this one eval - the Death is
# collected before any Step() could clear it, the presenter is shown the empty slot, and the clock never runs, so his
# slot cannot be let to the next wave (which would drop his plate) and his body cannot age out of a list whose clock
# is Time.time. SimHost.Update runs every frame whatever the clock does, so Events.Dispatch() still delivers the Death
# and CombatFx.OnDeath still calls VATRenderer.AddFallen - which is the whole thing r2 never managed.
DESPAWN = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
if (slot < 0) return "no slot";
var w0 = h.Local.World;
if (!w0.IsAlive(slot)) return "he is already dead: nothing to do";
int killer = -1;
float best = float.MaxValue;
for (int i = 0; i < w0.HighWater; i++)
{
    if (!w0.IsAlive(i) || w0.Team[i] == 0) continue;
    float d = Unity.Mathematics.math.distance(w0.Position[i], w0.Position[slot]);
    if (d < best) { best = d; killer = i; }
}
if (killer < 0) return "no living enemy to credit the kill to";
bool ok = h.WriteWorlds(m => {
    if (m.World.IsAlive(slot))
        m.World.Despawn(slot, killer, default, 0f);   // shot, not thrown: no impulse, so he drops where he stood
});
if (!ok) return "worlds a tick apart: retry";
h.Events.Collect(h.Local.World);                 // before any Step() could clear it
h.Presenter.Capture(h.Local.World);              // and the presenter is shown the slot he no longer fills
string held = TW.Editor.CaptureRig.Hold(120f);   // and in the same eval, so no wave can be let into his slot
var w = h.Local.World;
return "despawned slot=" + slot + " killer=" + killer + " at=" + best.ToString("0.0") + " m"
     + " alive=" + w.IsAlive(slot) + " archetype=" + w.Archetype[slot] + " " + held;
"""


def magenta_px(stem):
    """The flat magenta pixels of a still just shot: how much of the plate this pose can actually see."""
    import numpy as np
    from PIL import Image
    rgb = np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB")).astype(np.int16)
    return int(((np.abs(rgb - np.array([255, 0, 255], dtype=np.int16)) <= 40).all(axis=2)).sum())


def pick_yaw(face):
    """Pin the one camera yaw by measuring, not by guessing.

    The plate must be seen whole in both masked stills or a 2 per cent band means nothing, and what hides it is the
    Sentry himself: at the stock yaw he stood in front of his own plate alive (5429 px) and lay out of the way dead
    (11232 px). The yaw that sees the most magenta is the one he is least in the way of - and the same number also
    catches a yaw that has the plate out of frame altogether (the man-to-plate line came out at 175 deg once, with
    the plate behind the camera: no magenta, no cyan bar, nothing to measure). So the whole circle is swept in 30 deg
    steps, plus the stock yaw, and the best is pinned for all three stills. The first trial shot holds the clock, so
    the sweep costs the battle nothing: it is the same frozen instant seen from thirteen sides.
    """
    best, best_px = None, -1
    for cand in [face + 30.0 * k for k in range(12)] + [YAW]:
        one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.yaw", %sf); return "ok";' % cand)
        if not shot("pick", quiet=True):
            continue
        px = magenta_px("pick")
        log("  yaw %7.1f -> %6d magenta px" % (cand, px))
        if px > best_px:
            best, best_px = cand, px
    for leftover in glob.glob(os.path.join(HERE, "pick.*")):
        os.remove(leftover)
    if best is None or best_px <= 0:
        return None
    one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.yaw", %sf); return "ok";' % best)
    log("  pinned yaw -> %.1f deg (%d magenta px)" % (best, best_px))
    return best


def field(state, name, cast=float):
    """One `name=value` field of a STATE answer, or None when it is not there."""
    for part in state.split():
        if part.startswith(name + "="):
            try:
                return cast(part.split("=", 1)[1])
            except ValueError:
                return None
    return None


def shot(stem, mask=True, quiet=False):
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
        note = "pose_error_m=%s men_in_frame=%s" % (d.get("pose_error_m"), d.get("men_in_frame"))
    if quiet:
        return True
    log("  %-11s -> %s (%d KB) mask=%s %s" % (stem, stem + ".png", os.path.getsize(png) // 1024, mask, note))
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
    one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.spot_x", 0f);'          # an earlier run's spot would stand
              ' UnityEditor.EditorPrefs.SetFloat("sentry.spot_z", 0f);'         # this run's enemy ring in the wrong place
              ' UnityEditor.EditorPrefs.SetInt("sentry.death_tick", -1); return "cleared";')

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

    log("stop the clock and clear the frame: everything after this is one frozen instant")
    staged_yaw = ev("stage", STAGE)            # holds the clock, pins him at his plate, works out the facing yaw
    ev("clear", CLEAR)                         # everyone but him 80 m off, and the presenter told about it
    staged = ev("state_staged", STATE)
    if "alive=True" not in staged or "spot_set=True" not in staged:
        log("FAILED: he did not reach the frozen instant alive with his plate down: %s" % staged)
        return 1

    log("pin the one camera pose, by measuring")
    face = field(staged_yaw, "yaw")
    if pick_yaw(face if face is not None else YAW) is None:
        log("FAILED: no yaw round the plate shows any of it: is CombatFx.PlateMask reaching the batch?")
        return 1

    # One frozen instant is what makes the pair worth anything: between plate_t and plate_dead nothing moves, nothing
    # spawns, no slot is let again and no body ages, so the only difference between the two stills is his death. Every
    # run that let the clock run in between lost him to something - the battle shot him at 95 HP, or his slot went to
    # the next wave and DrawPlate dropped the plate with it (a reused slot keeps no dead man's plate).
    log("still 1 of 3: alive, his plate down, the frame his alone")
    alive_state = ev("state_t", STATE)
    if "alive=True" not in alive_state:
        log("FAILED: he was already dead before the live still")
        return 1
    log("  before     -> fallen=%s drawn=%s" % (field(alive_state, "fallen"), field(alive_state, "drawn")))
    if not shot("plate_t"):
        return 1

    log("kill him for real: the sim's own Death event, collected in the same eval")
    kill_path = "despawn_collect"
    answer = ev("despawn", DESPAWN)
    if "despawned" not in answer:
        log("FAILED: the kill did not land: %s" % answer)
        return 1
    time.sleep(2.0)                            # held frames still dispatch: SimHost.Update runs every frame whatever
    log("  kill_path  -> %s" % kill_path)      # the clock does, so OnDeath and AddFallen happen here
    ev("watch", WATCH)

    dead_state = ev("state_dead", STATE)
    log("  after      -> fallen=%s drawn=%s" % (field(dead_state, "fallen"), field(dead_state, "drawn")))
    if field(dead_state, "fallen") == 0:
        log("  !! fallen is still 0: the death never reached VATRenderer.AddFallen")
    if "spot_set=True" not in dead_state:
        log("  !! his plate spot was dropped: something let his slot again")

    log("still 2 of 3: the plate, masked, after the death")
    if not shot("plate_dead"):
        return 1

    log("still 3 of 3: the same frame unmasked, the night look")
    if not shot("night_dead", mask=False):
        return 1

    ev("release", RELEASE)
    one_liner("UnityEngine.Time.timeScale = 1f; return UnityEngine.Time.timeScale;")   # the battle's own speed back
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.kill_path", "%s"); return "ok";' % kill_path)
    with open(os.path.join(HERE, "kill_path.txt"), "w", encoding="utf-8") as f:
        f.write(kill_path + chr(10))
    log("done: plate_t.png, plate_dead.png, night_dead.png and their sidecars are in %s" % HERE)
    return 0


if __name__ == "__main__":
    sys.exit(main())
