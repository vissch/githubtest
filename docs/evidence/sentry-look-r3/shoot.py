#!/usr/bin/env python
"""Shoot the stills this folder's probe.py measures (sentry-look-r3, step 9).

The harsh critic's fix 3 on sentry-look was that the plate is only ever drawn at the near tier and flushed in
`dirtMat`, the night ground's own colour - so at T1 and O120 the Sentry is an MG man double. r2's own evidence could
not catch that: it shot one Sentry alone, at T3 only. So this run stands a **Sentry and two Machinegunners side by
side**, 4.5 m apart on a line across the camera, and photographs them at all three bands in the battle itself
(GreyboxCorridor, the owner's night look). The second Machinegunner is the control: probe.py measures him against the
first exactly as it measures the Sentry, which is the floor pose noise alone puts under the number - without it a
reader cannot tell a plate that reads from two men caught mid-stride.

    pair_t3.png     T3   zoom 7.5   pitch 13
    pair_t1.png     T1   zoom 30    pitch 25
    pair_o120.png   O120 zoom 120   pitch 55
    mask_t3.png     T3 again, with CombatFx.PlateMask on: the plate flat magenta, so probe.py can find its pixels
    empty_<band>.png  the same three poses with every man moved 500 m off and CombatFx.ForgetPlates() called

probe.py subtracts `empty_<band>` from `pair_<band>` to get each man's silhouette, and XORs the Sentry's against the
first MG man's: if the plate does not read at a band, the two silhouettes are the same baked soldier and the XOR
collapses to the control's value.

All three are pinned - put back on their line, topped up to 500 hp and set to one stance - before every still, from
the tick before the plate is emplaced. The first run let them choose their own ground and lost both the pair and the
plate to it: they walked 45 m apart, which threw the MG man 8540 px off the side of a 1920 px picture, the MG man was
killed before the plate was even down, and the plate - emplaced once and never again - ended up 4.9 m to one side of
the man it belongs to.

`crops.json` says where each man is in each still - his feet and his head as screen points - so probe.py never has to
guess at a projection. Those points are computed from the pose the sidecar of that still actually recorded
(cam_x/cam_y/cam_z/cam_pitch/cam_yaw), not from a camera that has since been restored: CaptureRig puts the camera
back the moment its queue drains, so there is no pose left to query afterwards. The eval rebuilds the view and
projection matrices from the sidecar and checks its own camera point against the recorded one (`pose_check_m`).

There is deliberately **no enemy group**. Two runs put one 25 m off so all three would fire, and both runs lost the
Machinegunner inside 2.5 seconds - even topped up to 500 hp - which leaves a falling body whose drawn position runs
right off the side of the picture, and no pair left to compare. The claim under test is whether the plate makes the
Sentry read as himself; the muzzle flash is the unit `muzzle-flash-scale`'s, not this one's. So the three of them
stand alone and pinned, facing the camera, and the only thing that differs between them is the plate.

Needs this checkout's editor open and in Play in GreyboxCorridor. On a machine with no desktop (Windows session 0)
that is a batch-mode editor, started without -nographics so the camera still renders:

    "%LOCALAPPDATA%/unity/bin/unity.exe" ...  or the editor directly:
    "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
        -projectPath <checkout>/trench-warfare-3d -logFile <checkout>/trench-warfare-3d/Logs/batch-editor.log &

Then, from the repo root:  python docs/evidence/sentry-look-r3/shoot.py
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
# Tools/tw is a bash script, and it must be Git's bash: the `bash` first on PATH in a detached process is
# C:\Windows\System32\bash.exe, WSL's, which cannot see a C:/ path and answers "No such file or directory" for a file
# that is plainly there. That cost one whole capture run.
BASH = next((b for b in (r"C:\Program Files\Git\bin\bash.exe",
                         r"C:\Program Files\Git\usr\bin\bash.exe") if os.path.exists(b)), "bash")
SCENE = "Assets/_Project/Scenes/GreyboxCorridor.unity"

SENTRY = 26          # InfantryArchetype.Sentry (RosterEntry.cs)
GUNNER = 2           # InfantryArchetype.Machinegunner: the man he is accused of being a double of
# m between each of them, across the camera. 3.0 put them 23 px apart at O120, closer than probe.py's own crop boxes
# are wide out there, so one man's box held a piece of his neighbour's plate. 4.5 keeps the boxes clear at every band
# and still fits all three in a 1920 px picture at T3.
APART = 4.5
YAW = 21.0
W, H = 1920, 1080
# the three bands the critic named, each its own zoom and pitch
BANDS = (("t3", 7.5, 13.0), ("t1", 30.0, 25.0), ("o120", 120.0, 55.0))
LOG = os.path.join(HERE, "shoot.log")


def log(line):
    print(line, flush=True)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def tw(*args, **kw):
    """One Tools/tw call. Returns its output; the pipeline answers JSON with a "result" field."""
    out = subprocess.run([BASH, TW] + list(args), capture_output=True, cwd=PROJ,
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
    path = os.path.join(tempfile.gettempdir(), "sentry_look_r3_%s.cs" % name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(body)
    answer = result(tw("run", "eval_file", "--", "--file", path, timeout=timeout))
    log("  %-12s -> %s" % (name, answer))
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

# Stand one Sentry and one Machinegunner on team 0 and an enemy group they can both fire at, then pin the camera on
# the midpoint between the two of them. Their line runs across the camera (the camera's own right vector at this yaw),
# so neither hides the other, and the enemy is 45 degrees towards the camera so the plate faces it rather than away.
PLACE = """
if (!UnityEngine.Application.isPlaying) return "not in Play";
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || h.Local == null) return "no host";
if (tc == null) return "no TacticalCamera";
int before = h.Local.World.HighWater;
bool ok = h.WriteWorlds(m => {
    TW.Presentation.ProvingGround.Place(m, 0, %(sentry)d, 1);
    TW.Presentation.ProvingGround.Place(m, 0, %(gunner)d, 2);   // the second is the control
});
if (!ok) return "worlds a tick apart: retry";
var w0 = h.Local.World;
int sen = -1, gun = -1, gun2 = -1;
for (int i = 0; i < w0.HighWater; i++)
{
    if (w0.Team[i] != 0 || !w0.IsAlive(i)) continue;
    if (sen < 0 && w0.Archetype[i] == %(sentry)d) sen = i;
    else if (w0.Archetype[i] == %(gunner)d) { if (gun < 0) gun = i; else if (gun2 < 0) gun2 = i; }
}
if (sen < 0 || gun < 0 || gun2 < 0)
    return "not all three placed: sentry=" + sen + " gunner=" + gun + " control=" + gun2 + " (new slots from " + before + ")";

// the camera's own axes at this yaw, exactly as CaptureRig.Pose builds them: Euler(pitch, BaseYaw + yaw, 0)
var rot = UnityEngine.Quaternion.Euler(0f, tc.BaseYaw + %(yaw)sf, 0f);
UnityEngine.Vector3 fwd = rot * UnityEngine.Vector3.forward, right = rot * UnityEngine.Vector3.right;
fwd.y = 0f; right.y = 0f; fwd.Normalize(); right.Normalize();
var c = (UnityEngine.Vector3)w0.Position[sen];                      // the line is centred where the Sentry landed
// On the line in the same breath they are placed: DrawPlate emplaces the plate on the first frame it sees a living
// Sentry, once and for ever, so a line pinned even one eval later leaves the plate standing 3 m off its man.
int[] three = new int[] { sen, gun, gun2 };
float faceYaw = UnityEngine.Mathf.Atan2(-fwd.x, -fwd.z);
ok = h.WriteWorlds(m => {
    var w = m.World;
    for (int k = 0; k < 3; k++)
    {
        var at = c + right * (%(apart)sf * k);
        w.Position[three[k]] = new Unity.Mathematics.float3(at.x, w.Position[three[k]].y, at.z);
        w.Hp[three[k]] = 500f;
        w.Yaw[three[k]] = faceYaw;
        w.TargetSlot[three[k]] = -1;
    }
});
if (!ok) return "worlds a tick apart: retry";
UnityEditor.EditorPrefs.SetInt("sentry3.sen", sen);
UnityEditor.EditorPrefs.SetInt("sentry3.gun", gun);
UnityEditor.EditorPrefs.SetInt("sentry3.gun2", gun2);
UnityEditor.EditorPrefs.SetFloat("sentry3.x", c.x);
UnityEditor.EditorPrefs.SetFloat("sentry3.z", c.z);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(c.x, c.z), 7.5f, %(yaw)sf);     // on them at T3, so the near tier gives sockets
return "sentry=" + sen + " gunner=" + gun + " control=" + gun2 + " centre=" + c + " tick=" + w0.Tick;
""" % dict(sentry=SENTRY, gunner=GUNNER, yaw=YAW, apart=APART)

# Both of them walk. The first run let them pick their own ground after the plate was emplaced and they ended 45 m
# apart, which put the Machinegunner 8540 px off the side of a 1920 px picture at T3 - there was no pair to compare.
# So the line is pinned: once the plate is down, the Sentry's standing place is recorded and from then on, before
# every still and before every crop is read, the two of them and the group they fire at are put back on it. The
# Sentry goes back to where he was when he set his plate down, so the plate is still in front of him.
PIN = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || tc == null || fx == null) return "host/camera/fx missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry3.sen", -1);
int gun = UnityEditor.EditorPrefs.GetInt("sentry3.gun", -1);
int gun2 = UnityEditor.EditorPrefs.GetInt("sentry3.gun2", -1);
if (sen < 0 || gun < 0 || gun2 < 0) return "no slots";
var w0 = h.Local.World;
if (!UnityEditor.EditorPrefs.GetBool("sentry3.pinned", false))
{
    var at = w0.Position[sen];                       // the line is the ground the Sentry himself picked
    UnityEditor.EditorPrefs.SetFloat("sentry3.px", at.x);
    UnityEditor.EditorPrefs.SetFloat("sentry3.pz", at.z);
    UnityEditor.EditorPrefs.SetBool("sentry3.pinned", true);
}
var rot = UnityEngine.Quaternion.Euler(0f, tc.BaseYaw + %(yaw)sf, 0f);
UnityEngine.Vector3 fwd = rot * UnityEngine.Vector3.forward, right = rot * UnityEngine.Vector3.right;
fwd.y = 0f; right.y = 0f; fwd.Normalize(); right.Normalize();
var p0 = new UnityEngine.Vector3(UnityEditor.EditorPrefs.GetFloat("sentry3.px", 0f), 0f,
                                 UnityEditor.EditorPrefs.GetFloat("sentry3.pz", 0f));
int[] three = new int[] { sen, gun, gun2 };
// Facing the camera, so the Sentry's plate - emplaced from whichever way he faces - is seen flat on rather than as
// its own 0.03 m edge. A man turned away hides his plate behind himself and the pair reads the same at every band
// for a reason that has nothing to do with the fix.
float faceYaw = UnityEngine.Mathf.Atan2(-fwd.x, -fwd.z);
bool ok = h.WriteWorlds(m => {
    var w = m.World;
    for (int k = 0; k < 3; k++)
    {
        int i = three[k];
        var at = p0 + right * (%(apart)sf * k);
        w.Position[i] = new Unity.Mathematics.float3(at.x, w.Position[i].y, at.z);
        // Topped up every pin, because the first run lost the Machinegunner to the enemy before the plate was even
        // down - and a corpse's silhouette says nothing about whether the Sentry reads as himself.
        w.Hp[i] = 500f;
        w.Yaw[i] = faceYaw;
        w.StanceOf[i] = w.StanceOf[sen];                     // one stance for all three: only the plate may differ
        w.TargetSlot[i] = -1;
    }
});
if (!ok) return "worlds a tick apart: retry";
var mid = p0 + right * %(apart)sf;                           // the camera frames the middle man, so all three fit
UnityEditor.EditorPrefs.SetFloat("sentry3.x", mid.x);
UnityEditor.EditorPrefs.SetFloat("sentry3.z", mid.z);
// The plate is emplaced on the first frame DrawPlate sees a living Sentry, and he walks about 4 m in the second
// between being placed and being pinned - so the plate is set down on ground he has already left. Forgetting it puts
// it back down where he now stands, and once he is pinned he never leaves again, so this fires once.
// Emplace offsets the foot 0.32 m * scale in front of him, so anything under 0.6 m is the plate being where it belongs.
var spot = fx.SpotOf(sen);
bool forgot = false;
if (spot.Set && UnityEngine.Vector3.Distance(new UnityEngine.Vector3(spot.Foot.x, 0f, spot.Foot.z), p0) > 0.6f)
{
    fx.ForgetPlates();
    forgot = true;
}
return "pinned sentry=" + p0 + " mid=" + mid + " forgot_plate=" + forgot
     + " alive=" + w0.IsAlive(sen) + "/" + w0.IsAlive(gun) + "/" + w0.IsAlive(gun2)
     + " plate_to_man_m=" + (spot.Set ? UnityEngine.Vector3.Distance(new UnityEngine.Vector3(spot.Foot.x, 0f, spot.Foot.z), p0).ToString("0.000") : "unset");
""" % dict(yaw=YAW, apart=APART)

# How things stand: is the plate emplaced yet, and is each of the two actually shooting at the group?
STATE = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || fx == null) return "host/fx missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry3.sen", -1);
int gun = UnityEditor.EditorPrefs.GetInt("sentry3.gun", -1);
int gun2 = UnityEditor.EditorPrefs.GetInt("sentry3.gun2", -1);
var w = h.Local.World;
if (sen < 0 || gun < 0 || gun2 < 0) return "no slots";
var spot = fx.SpotOf(sen);
var sb = new System.Text.StringBuilder();
sb.Append("tick=").Append(w.Tick);
int[] three = new int[] { sen, gun, gun2 };
string[] names = new string[] { "sentry", "gunner", "control" };
for (int k = 0; k < 3; k++)
    sb.Append(" | ").Append(names[k]).Append(" alive=").Append(w.IsAlive(three[k]))
      .Append(" hp=").Append(w.Hp[three[k]].ToString("0"))
      .Append(" stance=").Append(w.StanceOf[three[k]])
      .Append(" target=").Append(w.TargetSlot[three[k]]);
sb.Append(" | spot_set=").Append(spot.Set).Append(" spot_foot=").Append(spot.Foot)
  .Append(" spot_fwd=").Append(spot.Fwd).Append(" mask=").Append(fx.PlateMask);
return sb.ToString();
"""

# Hold the clock and the weather, set the mask mode, and queue one still from this band's pose. Every still in the run
# is held at the same 120 s, so the sky, the rain and the lamps are identical and probe.py may subtract one from
# another. Anything else - a shot under a different Hold - would subtract the weather, not the men.
SHOT = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || fx == null || tc == null) return "host/fx/camera missing";
if (UnityEditor.EditorApplication.isPaused) return "editor is paused: queued shots never fire";
string dir = UnityEditor.EditorPrefs.GetString("sentry3.dir", "");
string stem = UnityEditor.EditorPrefs.GetString("sentry3.stem", "pair");
float ax = UnityEditor.EditorPrefs.GetFloat("sentry3.x", 0f), az = UnityEditor.EditorPrefs.GetFloat("sentry3.z", 0f);
float zoom = UnityEditor.EditorPrefs.GetFloat("sentry3.zoom", 7.5f);
float pitch = UnityEditor.EditorPrefs.GetFloat("sentry3.pitch", 13f);
bool maskOn = UnityEditor.EditorPrefs.GetBool("sentry3.mask", false);
System.IO.Directory.CreateDirectory(dir);
fx.PlateMask = maskOn;
string held = TW.Editor.CaptureRig.Hold(120f);
tc.ZoomMin = UnityEngine.Mathf.Min(tc.ZoomMin, 6f);
tc.FrameFrom(new UnityEngine.Vector2(ax, az), zoom, %(yaw)sf);
string q = TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, stem + ".png"), ax, az,
                                     zoom, %(yaw)sf, pitch, %(w)d, %(h)d);
return "held: " + held + " | mask=" + maskOn + " zoom=" + zoom + " pitch=" + pitch + " | " + q;
""" % dict(yaw=YAW, w=W, h=H)

RELEASE = """
return TW.Editor.CaptureRig.Release();
"""

# Where the two of them are in the still that was just written, in that image's own pixels. The pose comes in from the
# sidecar CaptureRig wrote (cam_*), because the rig restores the camera as soon as its queue drains - there is no live
# pose left to ask. The view and projection matrices are rebuilt from that pose and the band's fov, which is computed
# the way CaptureRig.Pose computes it; `pose_check_m` is the distance between the camera point this arithmetic says the
# shot was taken from and the one the sidecar recorded, so a wrong fov or a clamped zoom cannot pass unnoticed.
# Screen y counts up from the bottom and image rows count down from the top, hence `row = h - y`.
CROPS = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (h == null || tc == null) return "host/camera missing";
int sen = UnityEditor.EditorPrefs.GetInt("sentry3.sen", -1);
int gun = UnityEditor.EditorPrefs.GetInt("sentry3.gun", -1);
int gun2 = UnityEditor.EditorPrefs.GetInt("sentry3.gun2", -1);
if (sen < 0 || gun < 0 || gun2 < 0) return "no slots";
float zoom = UnityEngine.Mathf.Clamp(%(zoom)sf, tc.ZoomMin, tc.ZoomMax);
float close = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, zoom));
float fov = UnityEngine.Mathf.Lerp(tc.Fov, tc.CloseFov, close);
var rot = UnityEngine.Quaternion.Euler(%(pitch)sf, %(camyaw)sf, 0f);
var eye = new UnityEngine.Vector3(%(cx)sf, %(cy)sf, %(cz)sf);
// the camera point this arithmetic says the shot came from, to compare with the sidecar's
float distance = zoom * UnityEngine.Mathf.Tan(30f * UnityEngine.Mathf.Deg2Rad)
               / UnityEngine.Mathf.Tan(fov * 0.5f * UnityEngine.Mathf.Deg2Rad);
float ground = h.Local != null ? h.Local.Map.Height.Sample(%(ax)sf, %(az)sf) : 0f;
var aim = new UnityEngine.Vector3(%(ax)sf, ground * close + 1.1f * close, %(az)sf);
var mine = aim - rot * UnityEngine.Vector3.forward * distance;
float poseCheck = UnityEngine.Vector3.Distance(mine, eye);
// view from the sidecar's own pose, projection for this image's aspect: nothing about the live camera is used
var view = UnityEngine.Matrix4x4.TRS(eye, rot, new UnityEngine.Vector3(1f, 1f, -1f)).inverse;
var proj = UnityEngine.Matrix4x4.Perspective(fov, %(w)sf / %(hh)sf, 0.2f, 4000f);
var vp = proj * view;
var sb = new System.Text.StringBuilder();
sb.Append("{\\"pose_check_m\\":").Append(poseCheck.ToString("0.0000"))
  .Append(",\\"fov\\":").Append(fov.ToString("0.000"))
  .Append(",\\"zoom\\":").Append(zoom.ToString("0.###"))
  .Append(",\\"pitch\\":").Append(%(pitch)sf.ToString("0.###"))
  .Append(",\\"men\\":{");
int[] slots = new int[] { sen, gun, gun2 };
string[] names = new string[] { "sentry", "gunner", "control" };
for (int k = 0; k < 3; k++)
{
    var foot = (UnityEngine.Vector3)h.Presenter.Drawn(slots[k]);
    // Drawn() hands back the sim's own y, which is 0; the men stand on the terrain, 2.5 m up in this corridor. The
    // first measured run built every crop box on y=0 and put all three about 300 px under their man at T3, which is
    // why the control XOR came back as high as the Sentry's: neither box held a man at all.
    foot.y = TW.Presentation.RenderGround.Sample(h.Local.Map, foot.x, foot.z);
    // the man's own height, in the same metres CombatFx.PlatePieces is written in: his plate's top is 1.35 m, his
    // head is above it. 1.70 m times the figure scale, and the crop pads it further.
    var vat = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
    var head = foot + UnityEngine.Vector3.up * (1.70f * (vat != null ? vat.UnitScale : 1f));
    sb.Append(k > 0 ? ",\\"" : "\\"").Append(names[k]).Append("\\":{");
    for (int j = 0; j < 2; j++)
    {
        var p = j == 0 ? foot : head;
        var clip = vp * new UnityEngine.Vector4(p.x, p.y, p.z, 1f);
        float x = clip.w != 0f ? (clip.x / clip.w * 0.5f + 0.5f) * %(w)sf : float.NaN;
        float y = clip.w != 0f ? (clip.y / clip.w * 0.5f + 0.5f) * %(hh)sf : float.NaN;
        sb.Append(j > 0 ? ",\\"head\\":[" : "\\"foot\\":[").Append(x.ToString("0.00")).Append(",")
          .Append((%(hh)sf - y).ToString("0.00")).Append("]");
    }
    sb.Append(",\\"slot\\":").Append(slots[k]).Append("}");
}
sb.Append("}}");
return sb.ToString();
"""

# Take every man off the field and forget every plate, so the same three poses photograph the bare ground probe.py
# subtracts. Moving them (rather than despawning) keeps the sim's own bookkeeping out of it, and ForgetPlates is what
# stops a plate standing where its man no longer is.
EMPTY = """
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
if (h == null || fx == null) return "host/fx missing";
int moved = 0;
bool ok = h.WriteWorlds(m => {
    var w = m.World;
    for (int i = 0; i < w.HighWater; i++)
    {
        if ((w.Flags[i] & (uint)TW.Sim.UnitFlags.Vehicle) != 0) continue;
        var p = w.Position[i];
        w.Position[i] = new Unity.Mathematics.float3(p.x + 500f, p.y, p.z + 500f);
        w.TargetSlot[i] = -1;
        moved++;
    }
});
if (!ok) return "worlds a tick apart: retry";
fx.PlateMask = false;
fx.ForgetPlates();
var w0 = h.Local.World;
int sen = UnityEditor.EditorPrefs.GetInt("sentry3.sen", -1);
return "moved=" + (moved / (h.Peer != null ? 2 : 1)) + " sentry_now=" + w0.Position[sen]
     + " spot_set=" + fx.SpotOf(sen).Set;
"""


def field(state, key):
    for part in state.replace("|", " ").split():
        if part.startswith(key + "="):
            return part[len(key) + 1:]
    return ""


def shot(stem, zoom, pitch, mask=False, pin=True):
    """One still: set the band, hold, queue it, wait for the queue to drain, check the file and its sidecar."""
    one_liner('UnityEditor.EditorPrefs.SetString("sentry3.stem", "%s");'
              'UnityEditor.EditorPrefs.SetFloat("sentry3.zoom", %sf);'
              'UnityEditor.EditorPrefs.SetFloat("sentry3.pitch", %sf);'
              'UnityEditor.EditorPrefs.SetBool("sentry3.mask", %s); return "ok";'
              % (stem, zoom, pitch, "true" if mask else "false"))
    if pin:
        ev("pin_" + stem, PIN)                 # back on the line: all three walk off it between stills
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
    log("  %-12s -> %s (%d KB) pose_error_m=%s men_in_frame=%s drawn_near=%s drawn_far=%s"
        % (stem, stem + ".png", os.path.getsize(png) // 1024, d.get("pose_error_m"),
           d.get("men_in_frame"), d.get("drawn_near"), d.get("drawn_far")))
    if float(d.get("pose_error_m", 9)) >= 0.5:
        log("  !! %s: pose_error_m %s is not under 0.5, the evidence rule's bar" % (stem, d.get("pose_error_m")))
    return d


def crops_for(band, zoom, pitch, side):
    """The two men's feet and heads in the still just shot, from the pose its sidecar recorded."""
    body = CROPS % dict(zoom=zoom, pitch=side.get("cam_pitch", pitch),
                        camyaw=side.get("cam_yaw", 0.0),
                        cx=side.get("cam_x", 0.0), cy=side.get("cam_y", 0.0), cz=side.get("cam_z", 0.0),
                        ax=side.get("focus_x", 0.0), az=side.get("focus_z", 0.0), w=float(W), hh=float(H))
    ev("pin_crops_" + band, PIN)               # the same line the still was shot on, before Drawn() is read
    answer = ev("crops_" + band, body)
    try:
        return json.loads(answer[answer.index("{"):answer.rindex("}") + 1])
    except Exception:
        log("  !! %s: the crop eval did not answer JSON" % band)
        return None


def main():
    open(LOG, "w", encoding="utf-8").close()
    log("sentry-look-r3 capture  %s" % time.strftime("%Y-%m-%d %H:%M:%S"))
    log("scene %s  yaw=%s %dx%d  bands %s" % (SCENE, YAW, W, H, [b[0] for b in BANDS]))

    log("open the battle scene and enter Play")
    one_liner("TW.Editor.CaptureRig.Release(); UnityEditor.EditorApplication.isPlaying = false; return 1;")
    for _ in range(20):                       # OpenScene refuses in Play, so a stale session must stop first
        time.sleep(2.0)
        if "False" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  scene        -> %s" % one_liner(
        'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("%s"); return "opened";' % SCENE, timeout=300))
    one_liner("UnityEditor.EditorApplication.isPlaying = true; return 1;", timeout=300)
    for _ in range(40):                       # the first seconds of Play build the battlefield
        time.sleep(3.0)
        if "True" in one_liner("return UnityEngine.Application.isPlaying;"):
            break
    log("  loaded       -> %s" % one_liner(
        "return UnityEngine.SceneManagement.SceneManager.GetActiveScene().name;"))
    one_liner('UnityEditor.EditorPrefs.SetString("sentry3.dir", @"%s"); return "ok";' % HERE)
    one_liner("TW.Editor.CaptureRig.Release(); return 1;")   # never hold before the first step
    one_liner('UnityEditor.EditorPrefs.SetBool("sentry3.pinned", false); return "ok";')   # a fresh line each run

    log("stage the Sentry and the Machinegunner side by side")
    placed = ev("place", PLACE)
    if "sentry=" not in placed or "not both placed" in placed:
        log("FAILED: %s" % placed)
        return 1

    log("pin the line, then let them aim and let the Sentry set his plate down")
    # The pin runs from the first tick: the plate is emplaced once and for ever, so it has to be set down on the line
    # the stills are shot on. The first run pinned afterwards and left the plate 4.9 m to one side of its man.
    for _ in range(14):
        if "pinned sentry=" not in ev("pin", PIN):
            log("FAILED: the line could not be pinned")
            return 1
        time.sleep(2.5)
        # both: set, and set where he is standing. A plate he left behind him is not the plate under test.
        if "spot_set=True" in ev("state", STATE) and "forgot_plate=False" in ev("pin", PIN):
            break
    else:
        log("FAILED: the plate was never emplaced")
        return 1

    crops = {"image": [W, H], "yaw": YAW, "bands": {}}
    log("the pair, at each band")
    for band, zoom, pitch in BANDS:
        side = shot("pair_" + band, zoom, pitch)
        if side is None:
            return 1
        c = crops_for(band, zoom, pitch, side)
        if c is None:
            return 1
        c["pair"] = "pair_%s.png" % band
        c["empty"] = "empty_%s.png" % band
        crops["bands"][band] = c
        ev("release_" + band, RELEASE)

    log("the plate's own pixels, at T3, in the flat mask mode")
    if shot("mask_t3", BANDS[0][1], BANDS[0][2], mask=True) is None:
        return 1
    ev("release_mask", RELEASE)

    log("the bare ground: every man 500 m off, every plate forgotten")
    if "moved=" not in ev("empty", EMPTY):
        log("FAILED: the field was not cleared")
        return 1
    time.sleep(3.0)
    for band, zoom, pitch in BANDS:
        if shot("empty_" + band, zoom, pitch, pin=False) is None:   # never pin: the field must stay empty
            return 1
        ev("release_e_" + band, RELEASE)

    with open(os.path.join(HERE, "crops.json"), "w", encoding="utf-8") as f:
        json.dump(crops, f, indent=2)
        f.write("\n")
    for band in crops["bands"]:
        c = crops["bands"][band]
        log("  crops %-5s pose_check_m=%s sentry foot=%s head=%s | gunner foot=%s head=%s"
            % (band, c.get("pose_check_m"), c["men"]["sentry"]["foot"], c["men"]["sentry"]["head"],
               c["men"]["gunner"]["foot"], c["men"]["gunner"]["head"]))

    ev("release", RELEASE)
    log("done: seven stills, their sidecars and crops.json are in %s" % HERE)
    return 0


if __name__ == "__main__":
    sys.exit(main())
