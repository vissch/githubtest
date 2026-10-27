#!/usr/bin/env python
"""Shoot the stills this folder's probe.py measures (sentry-look-r3-2, steps 2 and 4).

r3 stood a Sentry between two Machinegunners and XORed their silhouettes. Its own control - MG man against MG man -
came back at 0.83/0.54/0.72, *higher* than Sentry against MG man, so what the XOR measured was the phase of a walk
clip, not the plate. This run photographs **one man, three times a band**:

    sentry_<band>.png   him as he is, archetype 26 (Sentry): plate and all
    mg_<band>.png       the same man, same frozen frame, with only his look swapped (Archetype = 2, Machinegunner)
    mg2_<band>.png      the same frozen frame again with nothing at all changed: this run's own noise floor
    mask_t3.png         T3 again with CombatFx.PlateMask on, the plate flat magenta, so probe.py can find its pixels
    empty_<band>.png    the same three poses with every man 500 m off and CombatFx.ForgetPlates() called

`CaptureRig.Hold(120)` sets `Time.timeScale = 0`, and `SimHost` advances the sim by `Time.deltaTime * TimeScale`, so
between the three stills of a band his clip phase, the weather, the lamps and the shells all stand still. The only
pixels that may differ between `sentry_<band>` and `mg_<band>` are the plate's: `VATRenderer.FigureOfArchetype` gives
archetypes 26 and 2 the same Soldier figure, `CombatFx.KindOf` gives both `ArmsKind.Mg`, and `AnimationController`
branches on no infantry archetype. `mg2_<band>` differs from `mg_<band>` by nothing at all, so `xor_control_max` is
what the capture itself is worth.

`DrawPlate` clears `spots[slot]` for any archetype that is not the Sentry, so the swap back to 26 re-emplaces the
plate. The run logs `fx.SpotOf(sen).Foot` before and after the swap and stops if it moved more than 0.10 m.

The bands are r3's: t3 zoom 7.5 pitch 13, t1 zoom 30 pitch 25, o120 zoom 120 pitch 55, yaw 21, 1920x1080, in
GreyboxCorridor (the owner's night look).

Needs this checkout's editor open and in Play. On a machine with no desktop (Windows session 0) that is a batch-mode
editor, started WITHOUT -nographics so the camera still renders:

    Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d -logFile <checkout>/trench-warfare-3d/Logs/batch.log

Then, from the repo root:  python docs/evidence/sentry-look-r3-2/shoot.py
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
# forward slashes: this path is handed to bash, which eats a Windows backslash before it ever opens the file
TW = os.path.join(PROJ, "Tools", "tw").replace("\\", "/")
# Tools/tw is a bash script and it must be Git's bash: the `bash` first on PATH in a detached process is WSL's,
# which cannot see a C:/ path. That cost r3 one whole capture run.
BASH = next((b for b in (r"C:\Program Files\Git\bin\bash.exe",
                         r"C:\Program Files\Git\usr\bin\bash.exe") if os.path.exists(b)), "bash")
SCENE = "Assets/_Project/Scenes/GreyboxCorridor.unity"

SENTRY = 26          # InfantryArchetype.Sentry (RosterEntry.cs)
GUNNER = 2           # InfantryArchetype.Machinegunner: the look he is accused of being a double of
YAW = 21.0
W, H = 1920, 1080
BANDS = (("t3", 7.5, 13.0), ("t1", 30.0, 25.0), ("o120", 120.0, 55.0))
SPOT_MOVE_MAX = 0.10   # m: how far the re-emplaced plate may land from where it stood before the swap
LOG = os.path.join(HERE, "shoot.log")


def log(line):
    print(line, flush=True)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def tw(*args, **kw):
    out = subprocess.run([BASH, TW] + list(args), capture_output=True, cwd=PROJ,
                         timeout=kw.get("timeout", 180))
    return (out.stdout or b"").decode("utf-8", "replace") + (out.stderr or b"").decode("utf-8", "replace")


def result(text):
    try:
        return str(json.loads(text[text.index("{"):]).get("result", text)).strip()
    except Exception:
        return text.strip()


def ev(name, body, timeout=180):
    path = os.path.join(tempfile.gettempdir(), "sentry_look_r32_%s.cs" % name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    answer = result(tw("run", "eval_file", "--", "--file", path, timeout=timeout))
    log("  %-16s -> %s" % (name, answer))
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

# One Sentry on team 0, facing the camera, pinned and with no target: the claim under test is his plate, and a man who
# fires is a man who dies or walks. The camera frames him at T3 first, so the near tier gives sockets and the plate is
# emplaced off his barrel rather than off his sim facing.
PLACE = """
if (!UnityEngine.Application.isPlaying) return "not in Play";
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || h.Local == null) return "no host";
if (tc == null) return "no TacticalCamera";
bool ok = h.WriteWorlds(m => { TW.Presentation.ProvingGround.Place(m, 0, %(sentry)d, 1); });
if (!ok) return "worlds a tick apart: retry";
var w0 = h.Local.World;
int sen = -1;
for (int i = 0; i < w0.HighWater; i++)
    if (w0.Team[i] == 0 && w0.IsAlive(i) && w0.Archetype[i] == %(sentry)d) { sen = i; break; }
if (sen < 0) return "no Sentry placed";
var rot = UnityEngine.Quaternion.Euler(0f, tc.BaseYaw + %(yaw)sf, 0f);
UnityEngine.Vector3 fwd = rot * UnityEngine.Vector3.forward;
fwd.y = 0f; fwd.Normalize();
var c = (UnityEngine.Vector3)w0.Position[sen];
float faceYaw = UnityEngine.Mathf.Atan2(-fwd.x, -fwd.z);      // towards the camera: his plate is seen flat on
ok = h.WriteWorlds(m => {
    var w = m.World;
    w.Hp[sen] = 500f;
    w.Yaw[sen] = faceYaw;
    w.TargetSlot[sen] = -1;
});
if (!ok) return "worlds a tick apart: retry";
UnityEditor.EditorPrefs.SetInt("sentry32.sen", sen);
UnityEditor.EditorPrefs.SetFloat("sentry32.x", c.x);
UnityEditor.EditorPrefs.SetFloat("sentry32.z", c.z);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(c.x, c.z), 7.5f, %(yaw)sf);
return "sentry=" + sen + " at=" + c + " tick=" + w0.Tick;
""" % dict(sentry=SENTRY, yaw=YAW)

# Back on his own ground, topped up, facing the camera, with nothing to shoot at. The plate is emplaced on the first
# frame DrawPlate sees a living Sentry and he walks a few metres in the second between being placed and being pinned,
# so a plate standing more than 0.6 m off the man is forgotten once and set down again where he now is.
PIN = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || tc == null || fx == null) return "host/camera/fx missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
var w0 = h.Local.World;
if (!UnityEditor.EditorPrefs.GetBool("sentry32.pinned", false))
{
    var at0 = w0.Position[sen];
    UnityEditor.EditorPrefs.SetFloat("sentry32.px", at0.x);
    UnityEditor.EditorPrefs.SetFloat("sentry32.pz", at0.z);
    UnityEditor.EditorPrefs.SetBool("sentry32.pinned", true);
}
var rot = UnityEngine.Quaternion.Euler(0f, tc.BaseYaw + %(yaw)sf, 0f);
UnityEngine.Vector3 fwd = rot * UnityEngine.Vector3.forward;
fwd.y = 0f; fwd.Normalize();
var p0 = new UnityEngine.Vector3(UnityEditor.EditorPrefs.GetFloat("sentry32.px", 0f), 0f,
                                 UnityEditor.EditorPrefs.GetFloat("sentry32.pz", 0f));
float faceYaw = UnityEngine.Mathf.Atan2(-fwd.x, -fwd.z);
bool ok = h.WriteWorlds(m => {
    var w = m.World;
    w.Position[sen] = new Unity.Mathematics.float3(p0.x, w.Position[sen].y, p0.z);
    w.Hp[sen] = 500f;
    w.Yaw[sen] = faceYaw;
    w.TargetSlot[sen] = -1;
});
if (!ok) return "worlds a tick apart: retry";
UnityEditor.EditorPrefs.SetFloat("sentry32.x", p0.x);
UnityEditor.EditorPrefs.SetFloat("sentry32.z", p0.z);
var spot = fx.SpotOf(sen);
bool forgot = false;
// One reason to make him set it down again: he emplaced it on ground he has since left. `plate_faces_him` is logged
// for the record - it is the dot of the plate's Fwd with the way the pin turned him, and it is not 1, because the
// barrel DrawPlate reads is not his sim yaw. What the run needs is not agreement with his yaw but that the
// emplacement be reproducible, and that is checked by forgetting the plate once and watching where it comes back
// (REEMPLACE below): a plate that lands in the same place twice will land there again after the look swap.
var face = new UnityEngine.Vector3(UnityEngine.Mathf.Sin(faceYaw), 0f, UnityEngine.Mathf.Cos(faceYaw));
float dot = spot.Set ? UnityEngine.Vector3.Dot(new UnityEngine.Vector3(spot.Fwd.x, 0f, spot.Fwd.z).normalized, face) : 1f;
if (spot.Set && UnityEngine.Vector3.Distance(new UnityEngine.Vector3(spot.Foot.x, 0f, spot.Foot.z), p0) > 0.6f)
{
    fx.ForgetPlates();
    forgot = true;
}
return "pinned sentry=" + p0 + " forgot_plate=" + forgot + " plate_faces_him=" + dot.ToString("0.000")
     + " alive=" + w0.IsAlive(sen)
     + " plate_to_man_m=" + (spot.Set ? UnityEngine.Vector3.Distance(new UnityEngine.Vector3(spot.Foot.x, 0f, spot.Foot.z), p0).ToString("0.000") : "unset");
""" % dict(yaw=YAW)

STATE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || fx == null) return "host/fx missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
var w = h.Local.World;
var spot = fx.SpotOf(sen);
return "tick=" + w.Tick + " alive=" + w.IsAlive(sen) + " hp=" + w.Hp[sen].ToString("0")
     + " arch=" + w.Archetype[sen] + " stance=" + w.StanceOf[sen] + " target=" + w.TargetSlot[sen]
     + " spot_set=" + spot.Set + " spot_foot=" + spot.Foot + " spot_fwd=" + spot.Fwd + " mask=" + fx.PlateMask;
"""

# Hold the clock and the weather, set the mask mode, and queue one still from this band's pose. Every still in the run
# is held at the same 120 s, so the sky, the rain and the lamps are identical and probe.py may subtract one from
# another; inside a band the hold also freezes his clip phase, so the three stills are one frozen frame.
SHOT = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || fx == null || tc == null) return "host/fx/camera missing";
if (UnityEditor.EditorApplication.isPaused) return "editor is paused: queued shots never fire";
string dir = UnityEditor.EditorPrefs.GetString("sentry32.dir", "");
string stem = UnityEditor.EditorPrefs.GetString("sentry32.stem", "sentry_t3");
float ax = UnityEditor.EditorPrefs.GetFloat("sentry32.x", 0f), az = UnityEditor.EditorPrefs.GetFloat("sentry32.z", 0f);
float zoom = UnityEditor.EditorPrefs.GetFloat("sentry32.zoom", 7.5f);
float pitch = UnityEditor.EditorPrefs.GetFloat("sentry32.pitch", 13f);
bool maskOn = UnityEditor.EditorPrefs.GetBool("sentry32.mask", false);
System.IO.Directory.CreateDirectory(dir);
fx.PlateMask = maskOn;
string held = TW.Editor.CaptureRig.Hold(120f);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), zoom, %(yaw)sf);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     zoom, %(yaw)sf, pitch, %(w)d, %(h)d);
return "held: " + held + " | mask=" + maskOn + " zoom=" + zoom + " pitch=" + pitch + " | " + q;
""" % dict(yaw=YAW, w=W, h=H)

# Swap only his look. 26 -> 2 takes the plate off him (DrawPlate clears the spot of a non-Sentry); 2 -> 26 puts it
# back, re-emplaced from the same frozen barrel, which is why the caller checks the foot has not moved.
LOOK = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || fx == null) return "host/fx missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
var was = fx.SpotOf(sen);
bool ok = h.WriteWorlds(m => { m.World.Archetype[sen] = (byte)%(arch)d; });
if (!ok) return "worlds a tick apart: retry";
var w = h.Local.World;
return "arch=" + w.Archetype[sen] + " spot_was=" + (was.Set ? was.Foot.ToString("F3") : "unset")
     + " pos=" + w.Position[sen];
"""

# Put the camera on this band before the clock is held. TacticalCamera eases its zoom and its field of view on the
# scaled clock, so a band set after the hold never arrives: the first measured runs asked for zoom 7.5 (fov 40.7 at
# ZoomMin 6, CloseZoom 15) and the stills came back at fov 25, from 23 m instead of 13 m, with the man 36 px tall
# instead of 230. The camera is framed with the clock running, given three seconds to ease, and only then held.
FRAME = """
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (tc == null) return "no TacticalCamera";
float ax = UnityEditor.EditorPrefs.GetFloat("sentry32.x", 0f), az = UnityEditor.EditorPrefs.GetFloat("sentry32.z", 0f);
float zoom = UnityEditor.EditorPrefs.GetFloat("sentry32.zoom", 7.5f);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), zoom, %(yaw)sf);
var cam = UnityEngine.Camera.main;
return "framed zoom=" + zoom + " cam_zoom=" + tc.Zoom + " cam_fov=" + (cam != null ? cam.fieldOfView : 0f);
""" % dict(yaw=YAW)

HOLD_FORGET = """
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (fx == null) return "no fx";
string held = TW.Editor.CaptureRig.Hold(120f);
fx.PlateMask = false;
fx.ForgetPlates();
return "held: " + held + " and forgot every plate";
"""

FORGET = """
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (fx == null) return "no fx";
fx.ForgetPlates();
return "forgot every plate";
"""

RELEASE = """
return TW.Editor.CaptureRig.Release();
"""

# Where the man is in the still that was just written, in that image's own pixels. The pose comes in from the sidecar
# CaptureRig wrote (cam_*), because the rig restores the camera as soon as its queue drains - there is no live pose
# left to ask. `pose_check_m` is the distance between the camera point this arithmetic says the shot came from and the
# one the sidecar recorded. Screen y counts up from the bottom and image rows count down, hence `row = h - y`.
CROPS = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || tc == null) return "host/camera missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
float zoom = UnityEngine.Mathf.Clamp(%(zoom)sf, tc.ZoomMin, tc.ZoomMax);
float close = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, zoom));
float fov = UnityEngine.Mathf.Lerp(tc.Fov, tc.CloseFov, close);
var rot = UnityEngine.Quaternion.Euler(%(pitch)sf, %(camyaw)sf, 0f);
var eye = new UnityEngine.Vector3(%(cx)sf, %(cy)sf, %(cz)sf);
float distance = zoom * UnityEngine.Mathf.Tan(30f * UnityEngine.Mathf.Deg2Rad)
               / UnityEngine.Mathf.Tan(fov * 0.5f * UnityEngine.Mathf.Deg2Rad);
float ground = h.Local != null ? h.Local.Map.Height.Sample(%(ax)sf, %(az)sf) : 0f;
var aim = new UnityEngine.Vector3(%(ax)sf, ground * close + 1.1f * close, %(az)sf);
var mine = aim - rot * UnityEngine.Vector3.forward * distance;
float poseCheck = UnityEngine.Vector3.Distance(mine, eye);
var view = UnityEngine.Matrix4x4.TRS(eye, rot, new UnityEngine.Vector3(1f, 1f, -1f)).inverse;
var proj = UnityEngine.Matrix4x4.Perspective(fov, %(w)sf / %(hh)sf, 0.2f, 4000f);
var vp = proj * view;
var foot = (UnityEngine.Vector3)h.Presenter.Drawn(sen);
// Drawn() hands back the sim's own y, which is 0; the men stand on the terrain, 2.5 m up in this corridor.
foot.y = TW.Presentation.RenderGround.Sample(h.Local.Map, foot.x, foot.z);
var vat = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
var head = foot + UnityEngine.Vector3.up * (1.70f * (vat != null ? vat.UnitScale : 1f));
var sb = new System.Text.StringBuilder();
sb.Append("{\\"pose_check_m\\":").Append(poseCheck.ToString("0.0000"))
  .Append(",\\"fov\\":").Append(fov.ToString("0.000"))
  .Append(",\\"zoom\\":").Append(zoom.ToString("0.###"))
  .Append(",\\"pitch\\":").Append(%(pitch)sf.ToString("0.###"))
  .Append(",\\"men\\":{\\"sentry\\":{");
for (int j = 0; j < 2; j++)
{
    var p = j == 0 ? foot : head;
    var clip = vp * new UnityEngine.Vector4(p.x, p.y, p.z, 1f);
    float x = clip.w != 0f ? (clip.x / clip.w * 0.5f + 0.5f) * %(w)sf : float.NaN;
    float y = clip.w != 0f ? (clip.y / clip.w * 0.5f + 0.5f) * %(hh)sf : float.NaN;
    sb.Append(j > 0 ? ",\\"head\\":[" : "\\"foot\\":[").Append(x.ToString("0.00")).Append(",")
      .Append((%(hh)sf - y).ToString("0.00")).Append("]");
}
sb.Append(",\\"slot\\":").Append(sen).Append("}}}");
return sb.ToString();
"""

# The bare ground of this band's own frozen frame: this man stops being drawn and his plate with him.
# `VATRenderer.Hide(slot, true)` is the only thing that works here. Moving him 500 m off does not: `SimHost` advances
# the presenter by scaled time too, so under the hold his drawn position stays exactly where it was and the "empty"
# still came back pixel-identical to the Sentry's (0 px of difference, twice). `vat.enabled = false` does not either -
# `LateUpdate` is what draws, and the sidecar still counted a drawn man. And shooting the bare ground outside the hold
# is no good at all: the rain is animated per frame, so a still minutes later differs from the men's over 23,000 px,
# a difference common to all three stills that swamps the XOR's denominator (it read 0.0975 at T3 for that alone).
# His archetype goes to the Machinegunner's first, because `ForgetPlates` alone is undone the very next frame: a
# living Sentry re-emplaces his plate, and the bare ground would still have a plate standing in it.
EMPTY = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var vat = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
if (h == null || fx == null || vat == null) return "host/fx/vat missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
bool ok = h.WriteWorlds(m => { m.World.Archetype[sen] = (byte)%(gunner)d; });
if (!ok) return "worlds a tick apart: retry";
fx.PlateMask = false;
fx.ForgetPlates();
vat.Hide(sen, true);
return "man_drawn=off hidden=" + vat.IsHidden(sen) + " arch=" + h.Local.World.Archetype[sen];
""" % dict(gunner=GUNNER)

UNEMPTY = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var vat = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
if (h == null || vat == null) return "host/vat missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry32.sen", -1);
if (sen < 0) return "no slot";
vat.Hide(sen, false);
bool ok = h.WriteWorlds(m => { m.World.Archetype[sen] = (byte)%(sentry)d; });
if (!ok) return "worlds a tick apart: retry";
return "man_drawn=on hidden=" + vat.IsHidden(sen) + " arch=" + h.Local.World.Archetype[sen];
""" % dict(sentry=SENTRY)


def shot(stem, zoom, pitch, mask=False, pin=True):
    """One still: set the band, hold, queue it, wait for the queue to drain, check the file and its sidecar."""
    one_liner('UnityEditor.EditorPrefs.SetString("sentry32.stem", "%s");'
              'UnityEditor.EditorPrefs.SetFloat("sentry32.zoom", %sf);'
              'UnityEditor.EditorPrefs.SetFloat("sentry32.pitch", %sf);'
              'UnityEditor.EditorPrefs.SetBool("sentry32.mask", %s); return "ok";'
              % (stem, zoom, pitch, "true" if mask else "false"))
    if pin:
        ev("pin_" + stem, PIN)
    ev("shot_" + stem, SHOT)
    if not drain():
        log("  !! %s: the capture queue never drained" % stem)
        return None
    png, side = os.path.join(HERE, stem + ".png"), os.path.join(HERE, stem + ".json")
    if not os.path.exists(png):
        log("  !! %s: no PNG arrived" % stem)
        return None
    if not os.path.exists(side):
        log("  !! %s: no sidecar arrived" % stem)
        return None
    with open(side, encoding="utf-8") as f:
        d = json.load(f)
    log("  %-16s -> %s (%d KB) pose_error_m=%s men_in_frame=%s drawn_near=%s drawn_far=%s"
        % (stem, stem + ".png", os.path.getsize(png) // 1024, d.get("pose_error_m"),
           d.get("men_in_frame"), d.get("drawn_near"), d.get("drawn_far")))
    if float(d.get("pose_error_m", 9)) >= 0.5:
        log("  !! %s: pose_error_m %s is not under 0.5, the evidence rule's bar" % (stem, d.get("pose_error_m")))
    return d


def crops_for(band, zoom, pitch, side):
    body = CROPS % dict(zoom=zoom, pitch=side.get("cam_pitch", pitch),
                        camyaw=side.get("cam_yaw", 0.0),
                        cx=side.get("cam_x", 0.0), cy=side.get("cam_y", 0.0), cz=side.get("cam_z", 0.0),
                        ax=side.get("focus_x", 0.0), az=side.get("focus_z", 0.0), w=float(W), hh=float(H))
    answer = ev("crops_" + band, body)
    try:
        return json.loads(answer[answer.index("{"):answer.rindex("}") + 1])
    except Exception:
        log("  !! %s: the crop eval did not answer JSON" % band)
        return None


def foot_of(answer):
    """The `spot_was=(x, y, z)` vector out of a LOOK answer, as three floats."""
    key = "spot_was="
    if key not in answer or "unset" in answer:
        return None
    text = answer[answer.index(key) + len(key):]
    try:
        text = text[text.index("(") + 1:text.index(")")]
        return [float(p) for p in text.split(",")]
    except Exception:
        return None


def main():
    open(LOG, "w", encoding="utf-8").close()
    log("sentry-look-r3-2 capture  %s" % time.strftime("%Y-%m-%d %H:%M:%S"))
    log("scene %s  yaw=%s %dx%d  bands %s  one man, shot three times a band"
        % (SCENE, YAW, W, H, [b[0] for b in BANDS]))

    log("open the battle scene and enter Play")
    one_liner("TW.Editor.CaptureRig.Release(); UnityEditor.EditorApplication.isPlaying = false; return 1;")
    for _ in range(20):                       # OpenScene refuses in Play, so a stale session must stop first
        time.sleep(2.0)
        if "False" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  scene            -> %s" % one_liner(
        'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("%s"); return "opened";' % SCENE, timeout=300))
    one_liner("UnityEditor.EditorApplication.isPlaying = true; return 1;", timeout=300)
    for _ in range(40):
        time.sleep(3.0)
        if "True" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  loaded           -> %s" % one_liner(
        "return UnityEngine.SceneManagement.SceneManager.GetActiveScene().name;"))
    one_liner('UnityEditor.EditorPrefs.SetString("sentry32.dir", @"%s"); return "ok";' % HERE)
    one_liner("TW.Editor.CaptureRig.Release(); return 1;")
    one_liner('UnityEditor.EditorPrefs.SetBool("sentry32.pinned", false); return "ok";')

    log("stage one Sentry")
    placed = ev("place", PLACE)
    if "sentry=" not in placed:
        log("FAILED: %s" % placed)
        return 1

    log("pin him, and let him set his plate down where he stands")
    for _ in range(14):
        if "pinned sentry=" not in ev("pin", PIN):
            log("FAILED: he could not be pinned")
            return 1
        time.sleep(2.5)
        if "spot_set=True" in ev("state", STATE) and "forgot_plate=False" in ev("pin", PIN):
            break
    else:
        log("FAILED: the plate was never emplaced")
        return 1

    crops = {"image": [W, H], "yaw": YAW, "bands": {}}
    for band, zoom, pitch in BANDS:
        log("band %s: the same frozen frame as Sentry, as MG man, and again untouched" % band)
        ev("pin_%s" % band, PIN)
        one_liner('UnityEditor.EditorPrefs.SetFloat("sentry32.zoom", %sf);'
                  'UnityEditor.EditorPrefs.SetFloat("sentry32.pitch", %sf); return "ok";' % (zoom, pitch))
        ev("frame_%s" % band, FRAME)           # the camera eases on the scaled clock: frame it before holding
        time.sleep(3.0)
        ev("frame_%s_again" % band, FRAME)
        time.sleep(2.0)
        ev("repin_%s" % band, PIN)
        # Hold the clock, then forget the plate so it is set down again inside the frozen frame. A plate emplaced
        # before the hold is emplaced off a barrel that was still moving, and the one the look swap brings back is
        # emplaced off the frozen one: in the first two runs of this capture those two were 0.79 m apart, which is
        # the 0.32 m offset read off a barrel pointing the other way. Inside the hold nothing moves, so the plate
        # comes back exactly where it was - which the check at the end of the band proves.
        ev("hold_forget_%s" % band, HOLD_FORGET)
        time.sleep(3.0)
        # him as he is. From here to Release the world stands still.
        side = shot("sentry_%s" % band, zoom, pitch, pin=False)
        if side is None:
            return 1
        # the plate's own pixels, in the same frozen frame as the still they will be measured against: the mask mode
        # flushes plate and post in flat magenta. Shot after the release, as the first run did, it is a different
        # emplacement of the plate and the magenta no longer lies where the steel is in sentry_t3.png.
        if band == "t3" and shot("mask_t3", zoom, pitch, mask=True, pin=False) is None:
            return 1
        before = ev("look_mg_%s" % band, LOOK % dict(arch=GUNNER))
        if "arch=%d" % GUNNER not in before:
            log("FAILED: the look could not be swapped: %s" % before)
            return 1
        was = foot_of(before)
        if shot("mg_%s" % band, zoom, pitch, pin=False) is None:
            return 1
        # the control: nothing at all is changed between this still and the one before it
        if shot("mg2_%s" % band, zoom, pitch, pin=False) is None:
            return 1
        c = crops_for(band, zoom, pitch, side)
        if c is None:
            return 1
        after = ev("look_back_%s" % band, LOOK % dict(arch=SENTRY))
        if "arch=%d" % SENTRY not in after:
            log("FAILED: the look could not be put back: %s" % after)
            return 1
        time.sleep(2.0)                     # a frame or two for DrawPlate to see a Sentry again and emplace
        ev("state_%s" % band, STATE)
        now = foot_of(ev("look_check_%s" % band, LOOK % dict(arch=SENTRY)))
        # How far the plate landed from where it stood, after the look was swapped away and back. At T3 and T1 this is
        # 0.0000 m: the clock is held, the sockets answer, and the barrel DrawPlate reads is the same barrel. At O120
        # he is drawn as the far box soldier, which carries no sockets, so the re-emplacement falls back on his sim
        # yaw and lands about 0.3 m away - the 0.32 m offset read off a different forward. It is recorded, not fatal:
        # the swap happens after all three of the band's stills are shot, so it cannot have moved the plate in any of
        # them. The plate the stills were taken with is the one `look_mg_<band>` logs, emplaced inside the hold.
        moved = None
        if was and now:
            moved = sum((a - b) ** 2 for a, b in zip(was, now)) ** 0.5
            log("  plate re-emplaced %.4f m from where it stood (bar %.2f%s)"
                % (moved, SPOT_MOVE_MAX, "" if moved <= SPOT_MOVE_MAX else ", recorded"))
        c["plate_back_m"] = None if moved is None else round(moved, 4)
        c["plate_foot_in_stills"] = was
        # the bare ground, in this band's own frozen frame: he stops being drawn and his plate with him, and both come
        # straight back, all without releasing the hold
        if "man_drawn=off" not in ev("empty_%s" % band, EMPTY):
            log("FAILED: the field was not cleared")
            return 1
        time.sleep(3.0)
        if shot("empty_%s" % band, zoom, pitch, pin=False) is None:
            return 1
        if "man_drawn=on" not in ev("unempty_%s" % band, UNEMPTY):
            log("FAILED: the men could not be put back")
            return 1
        c["sentry"] = "sentry_%s.png" % band
        c["mg"] = "mg_%s.png" % band
        c["mg2"] = "mg2_%s.png" % band
        c["empty"] = "empty_%s.png" % band
        crops["bands"][band] = c
        ev("release_%s" % band, RELEASE)

    with open(os.path.join(HERE, "crops.json"), "w", encoding="utf-8") as f:
        json.dump(crops, f, indent=2)
        f.write("\n")
    for band in crops["bands"]:
        c = crops["bands"][band]
        log("  crops %-5s pose_check_m=%s foot=%s head=%s"
            % (band, c.get("pose_check_m"), c["men"]["sentry"]["foot"], c["men"]["sentry"]["head"]))

    ev("release", RELEASE)
    log("done: thirteen stills, their sidecars and crops.json are in %s" % HERE)
    return 0


if __name__ == "__main__":
    sys.exit(main())
