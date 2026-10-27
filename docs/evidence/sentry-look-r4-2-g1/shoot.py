#!/usr/bin/env python
"""Shoot the stills this folder's probe.py measures (sentry-look-r4-2-g1).

What r4 left wrong, in the owner's words: "the unmasked night still still shows a gunner at the gun. The man who
vanished was kneeling off to the side." r4 stood him STAND_BACK = 2.5 m behind his plate so his body could not fall
across it, and 2.5 m behind a plate is not at the gun: the figure that disappears between the two night stills is a
man off to the side, while another gunner is still at the plate. So this run changes four things and keeps
everything r4 got right.

Kept from r4: the clock is stopped the moment his plate is down and never started again (slow motion does not exist
here; a running clock cost six runs - his slot let to the next wave drops the plate); CLEAR puts every other man
80 m off so nothing else is in frame; the kill is the sim's own Death - SimWorld.Despawn, h.Events.Collect and
h.Presenter.Capture in one eval, so CombatFx.OnDeath runs and VATRenderer.AddFallen is called; and
SimPresenter.Capture is called by hand after every WriteWorlds, because a stopped clock never steps.

Changed here:
  * STAND_BACK = 0.6 m - where a gunner firing over his own plate stands, so he dies at the gun.
  * --seed N (1, 2, 3): one whole fresh run each, into its own s<N>/. The seed is the enemy group's size
    (6, 8, 10 riflemen), so he traverses differently, emplaces somewhere else and at a different tick.
    ProvingGround.Place is deterministic off the rally point, so the fresh battle is what makes the three
    independent. s<N>/meta.json records the tick, his position, the plate foot and the yaw.
  * The yaw is picked side-on, not by maximum plate. Maximum magenta put the camera where the man is out of the
    way - the opposite of what this still has to show. The four candidates are the plate's facing +-90 degrees
    (side-on, the plate whole and the man beside it) and 30 degrees in from each of those; they are scored as r4
    scored them, by the magenta pixels of a trial masked shot, which still rejects a yaw with the plate out of frame.
  * The order inside the frozen instant: ClearFallen() - nobody lying before the kill - then masked plate_t, then
    three unmasked night stills with nothing at all changed between them (night_t, night_t2, night_t3: that spread
    is the current build's noise), then the kill, then unmasked night_dead.

Needs this checkout's editor open (batch mode is fine, started without -nographics). From the repo root:

    python docs/evidence/sentry-look-r4-2-g1/shoot.py --seed 1
"""
import argparse
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
RIFLEMAN = 0         # the enemy group that gives him something to traverse onto
STAND_BACK = 0.6     # m behind his plate: where a gunner firing over it stands, so he dies at the gun
FOES = {1: 6, 2: 8, 3: 10}    # the seed: a different battle, so a different traverse, place and tick
ZOOM, YAW, PITCH, W, H = 7.5, 21.0, 13.0, 1920, 1080

LOG = [None]


def seed_dir(seed):
    return os.path.join(HERE, "s%d" % seed)


def log(line):
    print(line, flush=True)
    if LOG[0]:
        with open(LOG[0], "a", encoding="utf-8") as f:
            f.write(line + "\n")


def tw(*args, **kw):
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
    path = os.path.join(tempfile.gettempdir(), "sentry_g1_%s.cs" % name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    answer = result(tw("run", "eval_file", "--", "--file", path, timeout=timeout))
    log("  %-12s -> %s" % (name, answer))
    return answer


def one_liner(code, timeout=120):
    return result(tw("eval", code, timeout=timeout))


def drain(limit=90):
    """Poll CaptureRig.Pending(): a shot is posed one frame and photographed the next."""
    for _ in range(limit):
        if one_liner("return TW.Editor.CaptureRig.Pending();").strip('"') == "0":
            return True
        time.sleep(1.0)
    return False


# ---------------------------------------------------------------- the stages

PLACE = """
// One Sentry with an enemy group ahead of him, and the camera pinned on him at T3 so the near tier gives him
// sockets (VATRenderer.Sockets is false beyond LodDistance, and without a socket no plate is emplaced).
if (!UnityEngine.Application.isPlaying) return "not in Play";
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null || h.Local == null) return "no host";
int before = h.Local.World.HighWater;
int men = 0, foes = 0;
bool ok = h.WriteWorlds(m => {
    men += TW.Presentation.ProvingGround.Place(m, 0, %(sentry)d, 1);
    foes += TW.Presentation.ProvingGround.Place(m, 1, %(rifle)d, %(foes)d);
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
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (tc == null) return "no TacticalCamera";
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(p.x, p.z), %(zoom)sf, %(yaw)sf);
return "slot=" + slot + " men=" + men + " foes=" + foes + " at=" + p + " tick=" + w.Tick;
"""

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
return "tick=" + w.Tick + " slot=" + slot + " alive=" + w.IsAlive(slot) + " hp=" + w.Hp[slot].ToString("0.0")
     + " sockets=" + sock + " barrel_yaw=" + byaw.ToString("0.0")
     + " unit_yaw=" + (w.Yaw[slot] * UnityEngine.Mathf.Rad2Deg).ToString("0.0")
     + " target=" + w.TargetSlot[slot]
     + " spot_set=" + spot.Set + " spot_fwd=" + spot.Fwd
     + " fallen=" + units.FallenCount + " drawn=" + units.DrawnInfantry
     + " px=" + w.Position[slot].x.ToString("0.00") + " pz=" + w.Position[slot].z.ToString("0.00")
     + " fx=" + spot.Foot.x.ToString("0.00") + " fz=" + spot.Foot.z.ToString("0.00")
     + " mask=" + fx.PlateMask;
"""

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
fx.PlateMask = UnityEditor.EditorPrefs.GetBool("sentry.mask", true);
string held = TW.Editor.CaptureRig.Hold(120f);
float yaw = UnityEditor.EditorPrefs.GetFloat("sentry.yaw", %(yaw)sf);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), %(zoom)sf, yaw);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     %(zoom)sf, yaw, %(pitch)sf, %(w)d, %(h)d);
return "held: " + held + " | yaw=" + yaw.ToString("0.0") + " | " + q;
""" % dict(zoom=ZOOM, yaw=YAW, pitch=PITCH, w=W, h=H)

# Stop the clock, pin the pose on the plate, remember the plate's own facing (the side-on yaw is read off it), and
# stand him STAND_BACK m behind the plate along the way it faces - at 0.6 m, at the gun.
STAGE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (h == null) return "no host";
string held = TW.Editor.CaptureRig.Hold(120f);   // from here nothing moves
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
int slot = UnityEditor.EditorPrefs.GetInt("sentry.slot", -1);
var spot = fx != null && slot >= 0 ? fx.SpotOf(slot) : default;
if (!spot.Set) return "no plate spot";
UnityEditor.EditorPrefs.SetFloat("sentry.spot_x", spot.Foot.x);
UnityEditor.EditorPrefs.SetFloat("sentry.spot_z", spot.Foot.z);
UnityEditor.EditorPrefs.SetFloat("sentry.fwd_x", spot.Fwd.x);
UnityEditor.EditorPrefs.SetFloat("sentry.fwd_z", spot.Fwd.z);
UnityEditor.EditorPrefs.SetFloat("sentry.x", spot.Foot.x);     // both stills frame the plate the same way
UnityEditor.EditorPrefs.SetFloat("sentry.z", spot.Foot.z);
float face = UnityEngine.Mathf.Atan2(spot.Fwd.x, spot.Fwd.z) * UnityEngine.Mathf.Rad2Deg;  // the plate's own facing
UnityEditor.EditorPrefs.SetFloat("sentry.face", face);
h.WriteWorlds(m => {
    var w = m.World;
    if (!w.IsAlive(slot)) return;
    w.Position[slot] = new Unity.Mathematics.float3(spot.Foot.x - spot.Fwd.x * %(back)sf, w.Position[slot].y,
                                                    spot.Foot.z - spot.Fwd.z * %(back)sf);
    w.PostCell[slot] = -1;
});
h.Presenter.Capture(h.Local.World);
return held + " face=" + face.ToString("0.0") + " spot=" + spot.Foot
     + " at=" + h.Local.World.Position[slot].ToString();
""" % dict(back=STAND_BACK)

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
    if (slot >= 0 && w.IsAlive(slot))
        w.Position[slot] = new Unity.Mathematics.float3(sx - fx * %(back)sf, w.Position[slot].y, sz - fz * %(back)sf);
});
if (!ok) return "worlds a tick apart: retry";
h.Presenter.Capture(h.Local.World);          // the clock is stopped, so nothing else will show the move
return "stood " + (moved / (h.Peer != null ? 2 : 1)) + " men 80 m off";
""" % dict(back=STAND_BACK)

# Nobody lying before the kill: r4's night diff had a body down already, so a difference in the frame did not have
# to be his. VATRenderer.ClearFallen empties the list and its heap counts in one sweep.
CLEAR_FALLEN = """
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
if (units == null) return "no VATRenderer";
int was = units.FallenCount;
units.ClearFallen();
return "fallen " + was + " -> " + units.FallenCount;
"""

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
string held = TW.Editor.CaptureRig.Hold(120f);   // in the same eval, so no wave can be let into his slot
var w = h.Local.World;
return "despawned slot=" + slot + " killer=" + killer + " at=" + best.ToString("0.0") + " m"
     + " alive=" + w.IsAlive(slot) + " " + held;
"""

RELEASE = "return TW.Editor.CaptureRig.Release();\n"


# ---------------------------------------------------------------- helpers

def magenta_px(path):
    import numpy as np
    from PIL import Image
    rgb = np.asarray(Image.open(path).convert("RGB")).astype(np.int16)
    return int(((np.abs(rgb - np.array([255, 0, 255], dtype=np.int16)) <= 40).all(axis=2)).sum())


def field(state, name, cast=float):
    for part in state.split():
        if part.startswith(name + "="):
            try:
                return cast(part.split("=", 1)[1])
            except ValueError:
                return None
    return None


def shot(outdir, stem, mask=True, quiet=False):
    """One still: queue it, wait for the queue to drain, check the PNG and its sidecar arrived."""
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.stem", "%s");'
              ' UnityEditor.EditorPrefs.SetBool("sentry.mask", %s); return "ok";'
              % (stem, "true" if mask else "false"))
    ev("shot_" + stem, SHOT)
    if not drain():
        log("  !! %s: the capture queue never drained" % stem)
        return False
    png = os.path.join(outdir, stem + ".png")
    if not os.path.exists(png):
        log("  !! %s: no PNG arrived" % stem)
        return False
    if quiet:
        return True
    side, note = os.path.join(outdir, stem + ".json"), "no sidecar"
    if os.path.exists(side):
        with open(side, encoding="utf-8") as f:
            d = json.load(f)
        note = "pose_error_m=%s men_in_frame=%s" % (d.get("pose_error_m"), d.get("men_in_frame"))
    log("  %-11s -> %s (%d KB) mask=%s %s" % (stem, stem + ".png", os.path.getsize(png) // 1024, mask, note))
    return True


def pick_yaw(outdir, face):
    """Pin the one camera yaw: side-on to the plate, scored by a trial masked shot.

    r4 swept the whole circle and kept the most magenta, which is by definition the yaw the man is least in the way
    of - the opposite of what a still of a gunner dying at his gun has to show. Side-on to the plate's own facing
    keeps the plate whole and the man beside it, so both are in frame. Four candidates: +-90 degrees off the facing,
    and 30 degrees in from each. They are still scored by magenta pixels, which rejects a yaw that has the plate out
    of frame altogether or hidden behind a bank of earth.
    """
    best, best_px = None, -1
    for cand in (face + 90.0, face + 60.0, face - 90.0, face - 60.0):
        one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.yaw", %sf); return "ok";' % cand)
        if not shot(outdir, "pick", quiet=True):
            continue
        px = magenta_px(os.path.join(outdir, "pick.png"))
        log("  yaw %7.1f -> %6d magenta px" % (cand, px))
        if px > best_px:
            best, best_px = cand, px
    for leftover in glob.glob(os.path.join(outdir, "pick.*")):
        os.remove(leftover)
    if best is None or best_px <= 0:
        return None
    one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.yaw", %sf); return "ok";' % best)
    log("  pinned yaw -> %.1f deg (%d magenta px, side-on)" % (best, best_px))
    return best


def enter_play():
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, required=True, choices=sorted(FOES))
    args = ap.parse_args()
    seed = args.seed
    outdir = seed_dir(seed)
    os.makedirs(outdir, exist_ok=True)
    LOG[0] = os.path.join(outdir, "shoot.log")
    open(LOG[0], "w", encoding="utf-8").close()
    log("sentry-look-r4-2-g1 capture, seed %d (%d foes)  %s"
        % (seed, FOES[seed], time.strftime("%Y-%m-%d %H:%M:%S")))
    log("scene %s  zoom=%s pitch=%s %dx%d stand_back=%s m" % (SCENE, ZOOM, PITCH, W, H, STAND_BACK))

    enter_play()
    one_liner('UnityEditor.EditorPrefs.SetString("sentry.dir", @"%s"); return "ok";' % outdir)
    one_liner("TW.Editor.CaptureRig.Release(); return 1;")   # never hold before the first step
    one_liner('UnityEditor.EditorPrefs.SetFloat("sentry.spot_x", 0f);'
              ' UnityEditor.EditorPrefs.SetFloat("sentry.spot_z", 0f); return "cleared";')

    log("stage the Sentry")
    placed = ev("place", PLACE % dict(sentry=SENTRY, rifle=RIFLEMAN, foes=FOES[seed], zoom=ZOOM, yaw=YAW))
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
    staged_yaw = ev("stage", STAGE)
    ev("clear", CLEAR)
    staged = ev("state_staged", STATE)
    if "alive=True" not in staged or "spot_set=True" not in staged:
        log("FAILED: he did not reach the frozen instant alive with his plate down: %s" % staged)
        return 1

    log("pin the one camera pose, side-on to the plate")
    face = field(staged_yaw, "face")
    yaw = pick_yaw(outdir, face if face is not None else YAW)
    if yaw is None:
        log("FAILED: no side-on yaw shows any of the plate: is CombatFx.PlateMask reaching the batch?")
        return 1

    log("nobody lying before the kill")
    ev("clear_fallen", CLEAR_FALLEN)

    before = ev("state_t", STATE)
    if "alive=True" not in before:
        log("FAILED: he was already dead before the live still")
        return 1
    log("  before     -> fallen=%s drawn=%s" % (field(before, "fallen"), field(before, "drawn")))

    log("still 1: masked, alive behind his plate (the cyan bar makes pixels metres)")
    if not shot(outdir, "plate_t"):
        return 1
    log("stills 2-4: unmasked, nothing changed between them - that spread is this build's noise")
    for stem in ("night_t", "night_t2", "night_t3"):
        if not shot(outdir, stem, mask=False):
            return 1

    log("kill him for real: the sim's own Death event, collected in the same eval")
    kill_path = "despawn_collect"
    answer = ev("despawn", DESPAWN)
    if "despawned" not in answer:
        log("FAILED: the kill did not land: %s" % answer)
        return 1
    time.sleep(2.0)            # held frames still dispatch: SimHost.Update runs whatever the clock does
    after = ev("state_dead", STATE)
    log("  after      -> fallen=%s drawn=%s" % (field(after, "fallen"), field(after, "drawn")))
    if field(after, "fallen") == 0:
        log("  !! fallen is still 0: the death never reached VATRenderer.AddFallen")
    if "spot_set=True" not in after:
        log("  !! his plate spot was dropped: something let his slot again")

    log("still 5: the same unmasked frame after the death")
    if not shot(outdir, "night_dead", mask=False):
        return 1

    ev("release", RELEASE)
    one_liner("UnityEngine.Time.timeScale = 1f; return UnityEngine.Time.timeScale;")

    meta = dict(seed=seed, foes=FOES[seed], stand_back_m=STAND_BACK, kill_path=kill_path,
                zoom=ZOOM, pitch=PITCH, width=W, height=H, yaw=round(yaw, 2),
                plate_face_deg=None if face is None else round(face, 2),
                tick=field(before, "tick", int), slot=field(before, "slot", int),
                man_x=field(before, "px"), man_z=field(before, "pz"),
                plate_foot=[field(before, "fx"), field(before, "fz")],
                fallen_before=field(before, "fallen", int), drawn_before=field(before, "drawn", int),
                fallen_after=field(after, "fallen", int), drawn_after=field(after, "drawn", int))
    with open(os.path.join(outdir, "meta.json"), "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)
        f.write("\n")
    log("done: five stills and meta.json in %s" % outdir)
    return 0


if __name__ == "__main__":
    sys.exit(main())
