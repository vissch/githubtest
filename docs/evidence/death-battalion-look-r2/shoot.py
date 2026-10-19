#!/usr/bin/env python3
"""Shoot a Death Battalion man and two riflemen SIDE BY SIDE in one frame, three seeds.

Round 1 shot each man at his own spot on his own frame, so its silhouette_diff 0.73 sat against a
rifleman-vs-rifleman noise of 0.40: most of what it measured was pose and ground, not the man. Here the three men
stand in one line, spawned by ONE eval on ONE frame with the clock held, so they share clip, tick, light and
weather; the only thing that differs between the Death Battalion man and his neighbours is what he is.

Per seed s in 1, 2, 3, at its own spot 40 m along x from the last (so an earlier seed's men are out of frame):

    seed<s>_men.png     rifleman, rifleman, Death Battalion at 1.6 m spacing, all yaw 0, Speed 0
    seed<s>_bg.png      the same held frame with the three written out of the world and the presenter re-captured:
                        same ground, same light, same weather, nobody standing on it
    seed<s>_fire.png    the three again with a Tusk of their own side burning 8 m away: the cloth under fire light

`AnimationController.Advance` counts `State[i].Frame` from the spawn, so three men spawned on ONE frame share Row
and Phase. They are spawned with the clock still running, given a moment for Advance to reach them, and only then
is the clock held; the script reads `Animation.Row/Phase` back for the three and refuses the seed if they differ.

    python docs/evidence/death-battalion-look-r2/shoot.py [--prefix before_]

It drives the batch-mode editor of this checkout over the pipeline port (the unity CLI by full path, as
Tools/assetboard/gamefilm.py does), writes the raw 2560x1440 stills into Captures/dbr2/, then copies each one
cropped to the lineup into this folder with its CaptureRig .json sidecar kept exactly as the rig wrote it.
"""
import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

from PIL import Image

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2] / 'trench-warfare-3d'
RAW = ROOT / 'Captures' / 'dbr2'
CLI = Path(os.environ.get('LOCALAPPDATA', '')) / 'unity' / 'bin' / 'unity.exe'
SCENE = 'Assets/_Project/Scenes/GreyboxCorridor.unity'

W, H = 2560, 1440                 # the whole frame the rig shoots
CROP_W, CROP_H = 900, 620         # the strip round the lineup that is committed (under the 400 KB evidence limit)
# The camera looks along +z, square on to a line of men running along x (the scene's BaseYaw is 270, so a shot yaw
# of 90 is dead ahead): at the standard 21 the line runs away from the camera and the three men overlap. A low
# pitch gives a silhouette with a readable head and helmet instead of three helmets seen from above, and 2.4 m
# apart they never touch (2.4 still merged the first two men at the darkest spot). AIM_Y lifts the aim off y = 0 so the men sit in the middle of the frame.
ZOOM, YAW, PITCH, AIM_Y = 14.9, 90.0, 14.0, 1.0
SPACING = 3.2
RIFLE, DEATH_BN, TUSK = 0, 28, 5
SEEDS = (1, 2, 3)


def run(code, timeout=90):
    env = dict(os.environ, UNITY_PROJECT_PATH=str(ROOT))
    r = subprocess.run([str(CLI), '--no-banner', 'cmd', 'eval', '--result-only', '--timeout', str(timeout),
                        '--', '--code', code], capture_output=True, text=True, env=env, timeout=timeout + 60)
    try:
        return json.loads(r.stdout).get('result')
    except ValueError:
        return r.stdout.strip()[-400:] or r.stderr.strip()[-400:]


# The gym runs itself on entering Play when a request is left in SessionState, and it drives the camera and queues
# its own CaptureRig shots. Nine stills of this unit's first run were a gym run's frames, not the lineup's (found
# 2026-10-19). Every run clears the request and the runner before it poses anything.
def quiet_the_gym():
    return run('UnityEditor.SessionState.EraseString("TW.Gym.Request");'
               ' var g = UnityEngine.Object.FindFirstObjectByType<TW.Editor.GymRun>();'
               ' if (g != null) UnityEngine.Object.DestroyImmediate(g);'
               ' UnityEngine.Time.captureFramerate = 0; return "gym was running: " + (g != null);', 60)


def ready():
    return run('var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();'
               ' return UnityEditor.EditorApplication.isPlaying && h != null && h.Local != null ? "ready" : "no";', 30) == 'ready'


def up():
    """An editor of this checkout, in Play in GreyboxCorridor with a few ticks on the clock."""
    quiet_the_gym()
    if ready():
        run('UnityEditor.EditorApplication.isPlaying = false; return "left play";', 60)   # a clean world every run
        time.sleep(6)
    if run('return 1;', 30) != 1:
        print('shoot: no editor answering on this checkout. Start one:')
        print('  Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d -logFile <log>')
        return False
    run('UnityEditor.SceneManagement.EditorSceneManager.OpenScene("' + SCENE + '");'
        ' UnityEditor.EditorApplication.isPlaying = true; return "play";', 180)
    for _ in range(45):
        time.sleep(4)
        if ready():
            time.sleep(8)       # let the sim run some ticks, or the presenter has nothing to interpolate
            print('  ' + str(quiet_the_gym()))
            return True
    print('shoot: Play did not start')
    return False


def shot(path, x, z):
    """One still, waited for: the rig poses this frame and photographs the next."""
    got = run('return TW.Editor.CaptureRig.Shot(@"%s", %sf, %sf, %sf, %sf, %sf, %d, %d, %sf);'
              % (path, x, z, ZOOM, YAW, PITCH, W, H, AIM_Y))
    if not str(got).startswith('queued'):
        raise SystemExit('shoot: the rig refused %s: %s' % (path, got))
    for _ in range(120):
        time.sleep(0.5)
        if run('return TW.Editor.CaptureRig.Pending();', 30) == '0' and Path(path).exists():
            time.sleep(0.5)
            return
    raise SystemExit('shoot: %s never landed' % path)


SPAWN = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
UnityEngine.Random.InitState(%(seed)d);
var slots = new System.Collections.Generic.List<int>();
int[] kinds = { %(rifle)d, %(rifle)d, %(death)d };
for (int k = 0; k < kinds.Length; k++)
{
    string s = null;
    for (int attempt = 0; attempt < 40 && (s == null || !s.StartsWith("slot ")); attempt++)
        s = TW.Editor.TankCapture.Spawn(0, kinds[k], %(x)sf + k * %(spacing)sf, %(z)sf, 0f);
    if (!s.StartsWith("slot ")) return "spawn failed: " + s;
    slots.Add(int.Parse(s.Substring(5)));
}
foreach (int slot in slots) { int sl = slot; for (int a = 0; a < 40 && !h.WriteWorlds(m => m.World.Speed[sl] = 0f); a++) { } }
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
return string.Join(",", slots);
'''

# A freshly spawned slot keeps the animation state of whoever stood there before until AnimationController.Advance
# has run once on it; with the clock held the pose then stays put. So the slots are read back a frame or two later,
# and the seed only goes ahead once all three agree on Row and Phase.
POSE = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
var sb = new System.Text.StringBuilder();
foreach (string part in "%(slots)s".Split(','))
{
    int slot = int.Parse(part);
    sb.Append(slot).Append(':').Append(h.Animation != null ? h.Animation.Row[slot].ToString() : "-")
      .Append('/').Append(h.Animation != null ? h.Animation.Phase[slot].ToString("0.0000") : "-").Append(' ');
}
return sb.ToString().Trim();
'''

# One clip and one tick, by construction, and the clock is already held when the men are spawned.
# Two things forced this. The sim slides a freshly spawned unit back into its side's deploy zone, so a lineup asked
# for at x 140 was drawn in a heap at x 89.5; and `AnimationController.Start` offsets a loop's frame by
# `Hash(s.Seed, ...)` so a squad is not six of the same man, while Decide picks its idle variant per man. With the
# clock held nothing ticks: the men stay where they were put, and Advance only recomputes Row and Phase from the
# state below. All three get the SAME clean Idle state, so the stills differ by the man and nothing else.
SAME = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
var a = h.Animation; if (a == null) return "no AnimationController";
var parts = "%(slots)s".Split(',');
foreach (string part in parts)
{
    int slot = int.Parse(part);
    var s = a.State[slot];
    s.Clip = TW.Presentation.Clip.Idle; s.PrevClip = TW.Presentation.Clip.Idle;
    s.Frame = 0f; s.PrevFrame = 0f; s.Blend = 0f; s.Fade = 0f; s.Rate = 1f;
    s.Rung = TW.Presentation.Rung.Idle; s.Stance = 0; s.WantStance = 0;
    s.AimYaw = 0f; s.BodyYaw = 0f; s.ShownYaw = 0f; s.TurnTo = 0f;
    s.Aimed = false; s.Gassed = false; s.Down = false; s.Dead = false;
    s.Speed = 0f; s.HopHeight = 0f;
    a.State[slot] = s;
}
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
return "one idle pose for " + parts.Length;
'''

FIRE = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
string s = null;
for (int a = 0; a < 40 && (s == null || !s.StartsWith("slot ")); a++)
    s = TW.Editor.TankCapture.Spawn(0, %(tusk)d, %(x)sf, %(z)sf, 90f);
if (!s.StartsWith("slot ")) return "tusk failed: " + s;
int tusk = int.Parse(s.Substring(5));
TW.Editor.RiderLab.Stop(tusk);
string lit = null;
for (int a = 0; a < 40 && (lit == null || !lit.StartsWith("fire")); a++) lit = TW.Editor.TankCapture.Ignite(tusk, 0.9f);
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
return "tusk " + tusk + " " + lit;
'''

# The background: the men written out of the world and the presenter re-captured, without a tick of the clock, so
# `_bg` is the same ground under the same light and weather with nobody standing on it (round 1's trick).
MOVE = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
var sb = new System.Text.StringBuilder();
foreach (string part in "%(slots)s".Split(','))
{
    int slot = int.Parse(part);
    var p = h.Local.World.Position[slot];
    sb.Append(slot).Append('=').Append(p.x.ToString("R")).Append('|').Append(p.y.ToString("R")).Append('|').Append(p.z.ToString("R")).Append(';');
    var away = new Unity.Mathematics.float3(p.x, p.y, p.z + %(dz)sf);
    for (int a = 0; a < 40 && !h.WriteWorlds(m => m.World.Position[slot] = away); a++) { }
}
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
return sb.ToString();
'''

PUT = '''
var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); if (h == null) return "no SimHost";
foreach (string part in "%(where)s".Split(';'))
{
    if (part.Length == 0) continue;
    var bits = part.Split('=');
    int slot = int.Parse(bits[0]);
    var xyz = bits[1].Split('|');
    var back = new Unity.Mathematics.float3(float.Parse(xyz[0]), float.Parse(xyz[1]), float.Parse(xyz[2]));
    for (int a = 0; a < 40 && !h.WriteWorlds(m => m.World.Position[slot] = back); a++) { }
}
h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f);
return "put back";
'''


def phases_agree(answer):
    """Three men spawned on one held frame must share Row and Phase, or the frame is not one clip and one tick."""
    poses = set(p.split(':')[1] for p in answer.split())
    return len(poses) == 1


def crop(src, dst):
    im = Image.open(src)
    left, top = (im.width - CROP_W) // 2, (im.height - CROP_H) // 2
    im.crop((left, top, left + CROP_W, top + CROP_H)).save(dst, optimize=True)


def seed_run(s, prefix):
    x0 = 140.0 + (s - 1) * 40.0        # 100 sits in a light pool with gear in it: its mask came back merged
    z0 = 120.0
    cx = x0 + SPACING                              # the middle man: the camera's aim for all three stills
    print('seed %d: lineup at x %.1f..%.1f, z %.1f' % (s, x0, x0 + 2 * SPACING, z0))
    print('  ' + str(run('return TW.Editor.CaptureRig.Hold(120f);')))     # held FIRST: no tick slides them away

    slots = run(SPAWN % dict(seed=s, rifle=RIFLE, death=DEATH_BN, x=repr(x0), z=repr(z0), spacing=repr(SPACING)))
    print('  spawned slots %s' % (slots,))
    if not isinstance(slots, str) or 'failed' in slots or ',' not in slots:
        raise SystemExit('seed %d: the spawn failed: %s' % (s, slots))
    time.sleep(0.5)
    print('  ' + str(run(SAME % dict(slots=slots))))
    for attempt in range(30):
        time.sleep(0.5)
        answer = run(POSE % dict(slots=slots))
        if isinstance(answer, str) and ':' in answer and phases_agree(answer):
            break
    else:
        raise SystemExit('seed %d: the three men are not on one clip and one tick: %s' % (s, answer))
    print('  pose %s' % answer)
    shot(str(RAW / ('seed%d_men.png' % s)), cx, z0)

    where = run(MOVE % dict(slots=slots, dz=repr(-500.0)))     # out of the world without a tick of it
    print('  background: %s' % where)
    if not isinstance(where, str) or ';' not in where:
        raise SystemExit('seed %d: could not take the men out of frame: %s' % (s, where))
    shot(str(RAW / ('seed%d_bg.png' % s)), cx, z0)
    print('  back: %s' % run(PUT % dict(where=where)))

    print('  ' + str(run(FIRE % dict(tusk=TUSK, x=repr(x0 + 8.0), z=repr(z0 - 2.0)))))
    time.sleep(1.0)                                # the fire light registers over a render frame or two
    run('var h = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();'
        ' h.Presenter.Capture(h.Local.World); h.Presenter.Interpolate(1f, 100f); return "recaptured";')
    shot(str(RAW / ('seed%d_fire.png' % s)), cx, z0)

    for stem in ('men', 'bg', 'fire'):
        src = RAW / ('seed%d_%s.png' % (s, stem))
        crop(src, HERE / ('%sseed%d_%s.png' % (prefix, s, stem)))
        side = src.with_suffix('.json')
        if side.exists():
            shutil.copyfile(side, HERE / ('%sseed%d_%s.json' % (prefix, s, stem)))
        kb = (HERE / ('%sseed%d_%s.png' % (prefix, s, stem))).stat().st_size // 1024
        print('  %sseed%d_%s.png  %d KB' % (prefix, s, stem, kb))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--prefix', default='', help='"before_" for the captures of the unchanged build')
    args = ap.parse_args()
    if not up():
        return 1
    RAW.mkdir(parents=True, exist_ok=True)
    try:
        for s in SEEDS:
            seed_run(s, args.prefix)
    finally:
        print(run('return TW.Editor.CaptureRig.Release();'))
    return 0


sys.exit(main())
