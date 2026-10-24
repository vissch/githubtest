#!/usr/bin/env python3
"""Stage three men on ONE patch of ground, one at a time, and shoot each of them three times.

Why this exists. The first round (docs/evidence/death-battalion-look/) stood the three men where the sim had put
them: `focus_x` 27 / 40.5 / 54, `luma_mean` 0.225 / 0.205 / 0.214, `black_frac` 0.021 / 0.042 / 0.052, and 15-18
other men in frame. Different ground, different light and different occluders change how much of a man clears the
probe's TOL, so the masks came back at 3460, 2300 and 4337 px -- a 1.9x spread between two men who are the SAME
rifleman. The silhouette difference it reported (0.728) sat on a noise floor of 0.404, because the measurement was
largely of where each man stood. The fix is staging, not a looser probe.

What this does instead. The three men are spawned together and parked 120 m behind the stage spot, off frame. The
clock is then HELD (CaptureRig.Hold: game time stops, the editor keeps rendering, the weather is pinned). With the
world frozen, each man in turn is written to the one stage spot (40.5, 96.6) through SimHost.WriteWorlds -- never
into the world arrays, see CLAUDE.md, the seam -- the presenter is re-captured twice so prevPos and curPos both
hold the new spot (otherwise Drawn lerps him in from the park), and the same camera shoots him. Then he is parked
again. With the spot empty, one background still is taken from the same pose: ONE background serves all three men
of that instant, because it is the same ground, the same light, the same weather and the same held instant. The
only difference between the three `_t1_` stills of an instant is which man stood there.

Three instants, i = 0,1,2, with about three seconds of play between them, so pose varies the way it really varies.
A filter, not a retouch: an instant is only shot when all three men are in the SAME animation clip. Pose was most
of the old noise; the same clip at three different instants is a fair spread, not a hand-picked frame. Skipped
instants are printed as they happen.

Twelve stills and their twelve JSON sidecars land beside this file:

    rifle_t1_0..2.png   rifle2_t1_0..2.png   deathbn_t1_0..2.png   bg_0..2.png

Beside this file they are CROPS: 640x480 cut from the middle of each 3840x2160 frame, which is the aim point and
so the man (he is about 70 px tall). The whole frames stay in `trench-warfare-3d/Captures/g2/`, which git ignores --
twelve 10 MB PNGs do not belong in the repo. The crop is the same window in every one of the twelve, so a mask made
from a crop pair is the same mask the whole frames would give.

    python docs/evidence/death-battalion-look-g2/stage.py            capture (needs this checkout's editor)
    python docs/evidence/death-battalion-look-g2/stage.py --crop     re-cut the twelve crops from Captures/g2
    python docs/evidence/death-battalion-look-g2/stage.py --verify   check the twelve pairs on disk, exit 0/1
"""
import json
import re
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent.parent              # the repo root
PROJ = ROOT / 'trench-warfare-3d'
TW = PROJ / 'Tools' / 'tw'
OUT = PROJ / 'Captures' / 'g2'                # Captures/ is ignored by git; the keepers are copied to HERE

SCENE = 'Assets/_Project/Scenes/GreyboxCorridor.unity'
PARK = (40.5, 216.6)        # 120 m behind the stage spot, off frame
SPOT = (40.5, 96.6)         # the one patch every man is photographed on
CAM = dict(zoom=30.0, yaw=21.0, pitch=25.0, w=3840, h=2160)
INSTANTS = 3
TEAM = 1                    # the side still holding GreyboxCorridor by the time the editor is up: a man spawned on
                            # the beaten side is shot dead within seconds and all three men film as corpses
MEN = [('rifle', 0), ('rifle2', 0), ('deathbn', 28)]     # archetype 0 rifleman, 28 Death Battalion (Sim/Core/RosterEntry.cs)
STEMS = [m[0] for m in MEN]
MAX_TRIES = 12              # instants attempted before giving up on the same-clip filter
CROP = (640, 480)           # the committed window, centred on the frame centre (= the aim point = the man)


# ---- driving this checkout's editor -----------------------------------------------------------------------

def tw(*args, timeout=180):
    r = subprocess.run(['bash', str(TW), *args], capture_output=True, text=True, timeout=timeout, cwd=str(PROJ))
    return (r.stdout or '') + (r.stderr or '')


def ev(code, timeout=60):
    """One eval. Returns the result as a string; raises on an answer with no result in it."""
    out = tw('eval', code, str(timeout), timeout=timeout + 60)
    m = re.search(r'"result"\s*:\s*("(?:[^"\\]|\\.)*"|[^,\n}]+)', out, re.S)
    if not m:
        raise RuntimeError('eval gave no result:\n' + code + '\n' + out[:800])
    v = m.group(1).strip()
    return json.loads(v) if v.startswith('"') else v


def ev_retry(code, tries=8, timeout=60):
    """AlignWorlds answers "worlds a tick apart" now and then (workflow.md section 6): try again."""
    last = ''
    for _ in range(tries):
        last = str(ev(code, timeout))
        if 'tick apart' not in last and 'no SimHost' not in last:
            return last
        time.sleep(1.5)
    raise RuntimeError('eval kept answering ' + repr(last) + ':\n' + code)


HOST = 'var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();'


def place(slot, x, z):
    """Put a man somewhere and make the picture believe he has always been there."""
    return ev_retry(
        HOST + 'var p=h.Local.World.Position[' + str(slot) + '];'
        'if(!h.WriteWorlds(m=>m.World.Position[' + str(slot) + ']=new Unity.Mathematics.float3('
        + str(x) + 'f,p.y,' + str(z) + 'f))) return "worlds a tick apart";'
        'h.Presenter.Capture(h.Local.World);h.Presenter.Capture(h.Local.World);'
        'var q=h.Local.World.Position[' + str(slot) + '];return q.x+" "+q.z;')


def alive(slot):
    return ev(HOST + 'return (h.Local.World.Flags[' + str(slot) + '] & (uint)TW.Sim.UnitFlags.Alive) != 0 ? "yes" : "no";') == 'yes'


def clip_of(slot):
    return ev(HOST + 'return h.Animation.State[' + str(slot) + '].Clip.ToString();')


def match_pose(ref_slot, slot):
    """Copy the reference man's whole AnimState onto slot, keeping slot's own identity (step 4)."""
    ev(HOST + 'var r=h.Animation.State[' + str(ref_slot) + '];'
       'var s=h.Animation.State[' + str(slot) + '];var g=s.Generation;var d=s.Dead;s=r;'
       's.Generation=g;s.Dead=d;h.Animation.State[' + str(slot) + ']=s;h.Animation.Advance(0f);'
       'return "ok";')


def frame_yaw_of(slot):
    v = ev(HOST + 'var s=h.Animation.State[' + str(slot) + '];'
           'return s.Frame+" "+s.ShownYaw;')
    f, y = v.split(' ')
    return (float(f), float(y))


def shot(path, x, z):
    ev('return TW.Editor.CaptureRig.Shot(@"' + path + '", ' + str(x) + 'f, ' + str(z) + 'f, '
       + str(CAM['zoom']) + 'f, ' + str(CAM['yaw']) + 'f, ' + str(CAM['pitch']) + 'f, '
       + str(CAM['w']) + ', ' + str(CAM['h']) + ');')
    for _ in range(60):
        time.sleep(1.0)
        if ev('return TW.Editor.CaptureRig.Pending();') == '0':
            break
    else:
        raise RuntimeError('the shot queue never drained for ' + path)
    png = Path(path)
    for _ in range(30):
        if png.exists() and png.with_suffix('.json').exists():
            return
        time.sleep(1.0)
    raise RuntimeError(png.name + ': the rig said done but the file is not there')


# ---- the capture ------------------------------------------------------------------------------------------

def capture():
    OUT.mkdir(parents=True, exist_ok=True)
    print(tw('up', timeout=900).strip())
    tw('run', 'open_scene', '--', '--path', SCENE, timeout=300)
    print(tw('run', 'editor_play', timeout=300).strip())
    for _ in range(40):                                  # the first seconds of Play build the battlefield
        time.sleep(3)
        try:
            if ev(HOST + 'return h!=null && h.Local!=null ? "yes" : "no";') == 'yes':
                break
        except Exception:
            pass
    else:
        raise RuntimeError('no SimHost after Play')

    slots = {}
    for stem, arch in MEN:
        a = ev_retry('return TW.Editor.TankCapture.Spawn(' + str(TEAM) + ', ' + str(arch) + ', '
                     + str(PARK[0]) + 'f, ' + str(PARK[1]) + 'f);')
        m = re.search(r'slot (\d+)', a)
        if not m:
            raise RuntimeError(stem + ': spawn said ' + repr(a))
        slots[stem] = int(m.group(1))
        print(stem + ': archetype ' + str(arch) + ', slot ' + str(slots[stem]))
    time.sleep(6)                                        # each man into a real clip, not his spawn frame

    done, tries = 0, 0
    while done < INSTANTS and tries < MAX_TRIES:
        tries += 1
        ev('return TW.Editor.CaptureRig.Hold(120f);')
        dead = [s for s in STEMS if not alive(slots[s])]
        if dead:
            ev('return TW.Editor.CaptureRig.Release();')
            raise RuntimeError('killed before the instant could be shot: ' + ', '.join(dead))
        clips = dict((s, clip_of(slots[s])) for s in STEMS)
        if len(set(clips.values())) != 1:
            print('instant skipped (try ' + str(tries) + '): clips ' + str(clips))
            ev('return TW.Editor.CaptureRig.Release();')
            time.sleep(3)
            continue
        print('instant ' + str(done) + ': all three in ' + clips[STEMS[0]])
        ref = slots[STEMS[0]]
        for stem in STEMS[1:]:
            match_pose(ref, slots[stem])
        frames = dict((s, frame_yaw_of(slots[s])) for s in STEMS)
        print('instant ' + str(done) + ' poses: ' + str(frames))
        base = frames[STEMS[0]]
        drift = [s for s in STEMS[1:]
                 if abs(frames[s][0] - base[0]) > 0.001 or abs(frames[s][1] - base[1]) > 0.001]
        if drift:
            raise RuntimeError('pose drifted before the shot for: ' + ', '.join(drift) + ' ' + str(frames))
        for stem in STEMS:
            place(slots[stem], *SPOT)
            shot(str(OUT / (stem + '_t1_' + str(done) + '.png')), *SPOT)
            place(slots[stem], *PARK)
        shot(str(OUT / ('bg_' + str(done) + '.png')), *SPOT)     # the empty spot, same held instant
        done += 1
        ev('return TW.Editor.CaptureRig.Release();')
        time.sleep(3)

    if done < INSTANTS:
        raise RuntimeError('only ' + str(done) + ' of ' + str(INSTANTS)
                           + ' instants had all three men in one clip after ' + str(tries) + ' tries')

    crop()
    return verify()


def keepers():
    return [s + '_t1_' + str(i) for i in range(INSTANTS) for s in STEMS] + ['bg_' + str(i) for i in range(INSTANTS)]


def crop():
    """Cut the committed 640x480 crops out of the whole frames in Captures/g2, sidecar beside each."""
    from PIL import Image
    for name in keepers():
        src = OUT / (name + '.png')
        if not src.exists():
            raise RuntimeError(str(src) + ': no whole frame to crop (run the capture first)')
        im = Image.open(src)
        cx, cy = im.width // 2, im.height // 2
        box = (cx - CROP[0] // 2, cy - CROP[1] // 2, cx + CROP[0] // 2, cy + CROP[1] // 2)
        im.crop(box).save(HERE / (name + '.png'))
        d = json.loads((OUT / (name + '.json')).read_text(encoding='utf-8'))
        d['path'] = name + '.png'
        d['whole_frame'] = 'Captures/g2/' + name + '.png'
        d['crop_box_in_whole_frame'] = list(box)
        (HERE / (name + '.json')).write_text(json.dumps(d, indent=2), encoding='utf-8')
    print('cropped ' + str(len(keepers())) + ' stills to ' + str(CROP[0]) + 'x' + str(CROP[1]) + ' in ' + str(HERE))


def verify():
    bad = []
    for name in keepers():
        png, js = HERE / (name + '.png'), HERE / (name + '.json')
        if not png.exists() or png.stat().st_size == 0:
            bad.append(png.name + ': missing or empty')
        if not js.exists():
            bad.append(js.name + ': missing')
            continue
        from PIL import Image
        if Image.open(png).size != CROP:
            bad.append(png.name + ': ' + str(Image.open(png).size) + ', not the committed crop size')
        d = json.loads(js.read_text(encoding='utf-8'))
        if abs(d.get('focus_x', 0) - SPOT[0]) > 0.01 or abs(d.get('focus_z', 0) - SPOT[1]) > 0.01:
            bad.append(js.name + ': aimed at ' + str(d.get('focus_x')) + ',' + str(d.get('focus_z'))
                       + ', not the stage spot')
        if d.get('zoom') != CAM['zoom'] or d.get('yaw') != CAM['yaw'] or d.get('pitch') != CAM['pitch']:
            bad.append(js.name + ': camera ' + str(d.get('zoom')) + '/' + str(d.get('yaw')) + '/'
                       + str(d.get('pitch')) + ', not the staged one')
    for line in bad:
        print('FAIL ' + line)
    print(str(len(keepers())) + ' stills and sidecars: ' + ('ok' if not bad else str(len(bad)) + ' problems'))
    return 1 if bad else 0


if '--verify' in sys.argv:
    sys.exit(verify())
if '--crop' in sys.argv:
    crop()
    sys.exit(verify())
sys.exit(capture())
