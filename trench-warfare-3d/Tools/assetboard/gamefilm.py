#!/usr/bin/env python3
"""Films of the game itself, for the asset board: each model in the asset playground (docs/22), drawn by the game's own
shaders, walking, turning its guns, firing, being shot to pieces; each building shelled until it is down.

    python Tools/assetboard/gamefilm.py up              start a batch-mode editor on this checkout, in Play in the playground
    python Tools/assetboard/gamefilm.py film [NAME ..]  film every playground model and building (or the named ones)
    python Tools/assetboard/gamefilm.py battle [NAME ..] film every battle machine in a match (GreyboxCorridor): it
                                                        turns on the spot, drives at enemy riflemen and fires, is destroyed
    python Tools/assetboard/gamefilm.py check [F ..]    judge films already made (a .mp4, a folder of them, or none
                                                        for all of Captures/assetfilm): is the subject in the frame
                                                        from the first frame to the last, does a `moves` film move,
                                                        is a machine seen from a side that shows its guns. Exit 1 on
                                                        a failure. `film` and `battle` run it after every take
    python Tools/assetboard/gamefilm.py down            leave Play and close that editor

The board's build picks the films up from Captures/assetfilm (films.py). They are harvested, not derived: a station
without them keeps the ones already on the Drive.

The editor is a batch-mode one (no window), driven over the pipeline port by the unity CLI, the way Tools/tw does it.
It still renders: batch mode has the graphics card unless -nographics is given. Frames are taken with
Time.captureFramerate, so a film is the same whatever the machine's frame rate: every frame is 1/24 s of game time,
and the playground commands of a shot are queued on exact frames.

The unity CLI is called by its full path (%LOCALAPPDATA%/unity/bin/unity.exe). On this machine a `unity` shim earlier
on PATH starts a full editor instead, which then waits on a dialog nobody sees (2026-10-03).
"""
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
P = ROOT / 'Assets' / '_Project'
OUT = ROOT / 'Captures' / 'assetfilm'
CLI = Path(os.environ.get('LOCALAPPDATA', '')) / 'unity' / 'bin' / 'unity.exe'
EDITOR = Path(os.environ.get('TW_UNITY', r'C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe'))
FFMPEG = os.environ.get('TW_FFMPEG') or shutil.which('ffmpeg')
FPS, W, H = 24, 960, 540
SCENE = 'Assets/_Project/Playground/Playground.unity'
STAGE = 'panel 0; labels 0; timescale 1; lod -1; ground grid; team -1'

RECORDER = r'''
string dir = @"%(dir)s"; int frames = %(frames)d, w = %(w)d, h = %(h)d; float spin = %(spin)sf;
var cues = new System.Collections.Generic.List<System.Collections.Generic.KeyValuePair<int, string>>();
%(cues)s
var host = TW.Playground.PlaygroundHost.Instance; if (host == null) return "NO HOST";
System.IO.Directory.CreateDirectory(dir);
UnityEngine.Time.captureFramerate = %(fps)d;
int i = 0, last = -1; float yaw0 = host.Cam.Yaw;
UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    if (!UnityEditor.EditorApplication.isPlaying || host == null) { UnityEditor.EditorApplication.update -= tick; UnityEngine.Time.captureFramerate = 0; return; }
    if (UnityEngine.Time.frameCount == last) return; last = UnityEngine.Time.frameCount;
    var c = UnityEngine.Camera.main; if (c == null) return;
    var rt = UnityEngine.RenderTexture.GetTemporary(w, h, 24, UnityEngine.RenderTextureFormat.ARGB32);
    var before = c.targetTexture; c.targetTexture = rt; c.Render(); c.targetTexture = before;
    var was = UnityEngine.RenderTexture.active; UnityEngine.RenderTexture.active = rt;
    var tex = new UnityEngine.Texture2D(w, h, UnityEngine.TextureFormat.RGB24, false);
    tex.ReadPixels(new UnityEngine.Rect(0, 0, w, h), 0, 0); tex.Apply();
    UnityEngine.RenderTexture.active = was; UnityEngine.RenderTexture.ReleaseTemporary(rt);
    System.IO.File.WriteAllBytes(dir + "/" + i.ToString("00000") + ".jpg", UnityEngine.ImageConversion.EncodeToJPG(tex, 93));
    UnityEngine.Object.Destroy(tex);
    i++;
    foreach (var cue in cues) if (cue.Key == i) host.Queue(cue.Value);
    if (spin != 0f) host.Cam.Yaw = yaw0 + spin * i / frames;
    if (i >= frames)
    {
        UnityEditor.EditorApplication.update -= tick; UnityEngine.Time.captureFramerate = 0; host.Cam.Yaw = yaw0;
        System.IO.File.WriteAllText(dir + "/done.txt", i.ToString());
    }
};
UnityEditor.EditorApplication.update += tick;
return "filming " + frames;
'''


BATTLE_RECORDER = r'''
string dir = @"%(dir)s"; int frames = %(frames)d, w = %(w)d, h = %(h)d, slot = %(slot)d; float spin = %(spin)sf, zoom = %(zoom)sf, yaw0 = %(yaw)sf;
System.IO.Directory.CreateDirectory(dir);
UnityEngine.Time.captureFramerate = %(fps)d;
int i = 0, last = -1;
UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    if (!UnityEditor.EditorApplication.isPlaying) { UnityEditor.EditorApplication.update -= tick; UnityEngine.Time.captureFramerate = 0; return; }
    if (UnityEngine.Time.frameCount == last) return; last = UnityEngine.Time.frameCount;
    var c = UnityEngine.Camera.main; if (c == null) return;
    var rt = UnityEngine.RenderTexture.GetTemporary(w, h, 24, UnityEngine.RenderTextureFormat.ARGB32);
    var before = c.targetTexture; c.targetTexture = rt; c.Render(); c.targetTexture = before;
    var was = UnityEngine.RenderTexture.active; UnityEngine.RenderTexture.active = rt;
    var tex = new UnityEngine.Texture2D(w, h, UnityEngine.TextureFormat.RGB24, false);
    tex.ReadPixels(new UnityEngine.Rect(0, 0, w, h), 0, 0); tex.Apply();
    UnityEngine.RenderTexture.active = was; UnityEngine.RenderTexture.ReleaseTemporary(rt);
    System.IO.File.WriteAllBytes(dir + "/" + i.ToString("00000") + ".jpg", UnityEngine.ImageConversion.EncodeToJPG(tex, 93));
    UnityEngine.Object.Destroy(tex);
    { var sh = UnityEngine.Object.FindFirstObjectByType<TW.Editor.TankCapture.Shooter>(); if (sh != null) sh.Mark(); }   // one trace row per filmed frame
    i++;
%(cues)s
    if (spin != 0f) { var sh = UnityEngine.Object.FindFirstObjectByType<TW.Editor.TankCapture.Shooter>(); if (sh != null) sh.FollowBearing = yaw0 + spin * i / frames; }
    if (i >= frames)
    {
        UnityEditor.EditorApplication.update -= tick; UnityEngine.Time.captureFramerate = 0;
        System.IO.File.WriteAllText(dir + "/done.txt", i.ToString());
    }
};
UnityEditor.EditorApplication.update += tick;
return "filming " + frames;
'''


def run(code, timeout=60):
    env = dict(os.environ, UNITY_PROJECT_PATH=str(ROOT))
    r = subprocess.run([str(CLI), '--no-banner', 'cmd', 'eval', '--result-only', '--timeout', str(timeout), '--', '--code', code],
                       capture_output=True, text=True, env=env, timeout=timeout + 60)
    try:
        return json.loads(r.stdout).get('result')
    except ValueError:
        return None if not r.stdout.strip() else r.stdout.strip()[-300:]


def do(commands):
    code = 'var h = TW.Playground.PlaygroundHost.Instance; if (h == null) return "NO HOST";'
    code += ''.join(f' h.Queue("{c.strip()}");' for c in commands.split(';') if c.strip())
    return run(code + ' return "queued";')


def ready():
    return run('var h = TW.Playground.PlaygroundHost.Instance; return UnityEditor.EditorApplication.isPlaying && h != null ? "ready" : "not in play";', 20) == 'ready'


def up():
    if ready():
        print('gamefilm: the editor is up and in Play in the playground')
        return 0
    if run('return 1;', 20) != 1:
        log = ROOT / 'Logs' / 'assetfilm-editor.log'
        log.parent.mkdir(exist_ok=True)
        subprocess.Popen([str(EDITOR), '-batchmode', '-projectPath', str(ROOT), '-logFile', str(log)], creationflags=0x00000008)   # detached
        print(f'gamefilm: starting a batch-mode editor (log: {log}); a first import takes some minutes')
        for _ in range(180):
            time.sleep(10)
            if run('return 1;', 20) == 1:
                break
        else:
            print('gamefilm: the editor did not answer in 30 minutes')
            return 1
    run(f'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("{SCENE}"); UnityEditor.EditorApplication.isPlaying = true; return "play";', 120)
    for _ in range(30):
        time.sleep(4)
        if ready():
            print('gamefilm: in Play in the playground')
            return 0
    print('gamefilm: Play did not start')
    return 1


def down():
    print(run('UnityEditor.EditorApplication.isPlaying = false; return "left play";', 60))
    time.sleep(5)
    print(run('UnityEditor.EditorApplication.Exit(0); return "closing";', 30))
    return 0


def record(name, seconds, cues=(), spin=0.0, battle=None):
    """Film `seconds` of game time into OUT/<name>.mp4. In the playground, cues are [(second, 'playground commands')];
    in a match (battle = dict(slot, zoom, yaw)) they are [(second, 'a C# statement;')]."""
    frames_dir = OUT / '_frames' / name
    shutil.rmtree(frames_dir, ignore_errors=True)
    frames = int(round(seconds * FPS))
    if battle:
        # the proof the camera kept its subject: one line a frame, read back by check() when the take is in
        trace_path(name).unlink(missing_ok=True)
        run(f'return TW.Editor.TankCapture.Trace(@"{trace_path(name).as_posix()}");')
        lines = ''.join(f'    if (i == {max(1, int(round(t * FPS)))}) {{ {stmt} }}\n' for t, stmt in cues)
        got = run(BATTLE_RECORDER % dict(dir=frames_dir.as_posix(), frames=frames, w=W, h=H, fps=FPS, spin=repr(float(spin)), cues=lines,
                                         slot=battle['slot'], zoom=repr(float(battle['zoom'])), yaw=repr(float(battle['yaw']))))
    else:
        lines = ''.join(f'cues.Add(new System.Collections.Generic.KeyValuePair<int, string>({max(1, int(round(t * FPS)))}, "{c.strip()}"));\n'
                        for t, cmds in cues for c in cmds.split(';') if c.strip())
        got = run(RECORDER % dict(dir=frames_dir.as_posix(), frames=frames, w=W, h=H, fps=FPS, spin=repr(float(spin)), cues=lines))
    if not str(got).startswith('filming'):
        print(f'  {name}: the recorder did not start: {got}')
        if battle:
            run('return TW.Editor.TankCapture.TraceStop();')
        return False
    for _ in range(int(seconds * 20) + 240):
        if (frames_dir / 'done.txt').exists():
            break
        time.sleep(0.5)
    else:
        print(f'  {name}: timed out with {len(list(frames_dir.glob("*.jpg")))} of {frames} frames')
        if battle:
            run('return TW.Editor.TankCapture.TraceStop();')
        return False
    if battle:
        run('return TW.Editor.TankCapture.TraceStop();')
    mp4 = OUT / f'{name}.mp4'
    r = subprocess.run([FFMPEG, '-y', '-loglevel', 'error', '-r', str(FPS), '-i', str(frames_dir / '%05d.jpg'), '-an', '-c:v', 'libx264', '-preset', 'slow',
                        '-crf', '24', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', str(mp4)], capture_output=True, text=True)
    if r.returncode != 0:
        print(f'  {name}: ffmpeg: {r.stderr[-200:]}')
        return False
    shutil.copyfile(frames_dir / f'{frames // 3:05d}.jpg', OUT / f'{name}.jpg')
    shutil.rmtree(frames_dir, ignore_errors=True)
    print(f'  {name}.mp4  {seconds:.0f} s  {mp4.stat().st_size // 1024} KB')
    return not check_one(mp4)


def settle(seconds=1.5):
    time.sleep(seconds)


def library():
    """The playground library's vehicles and figures, in its order (the index `model v N` / `model u N` takes)."""
    text = (P / 'Playground' / 'PlaygroundLibrary.asset').read_text(encoding='utf-8')
    block = lambda a, b: text[text.index(a):text.index(b)] if a in text and b in text else ''
    return (re.findall(r'- Name: (\w+)', block('Vehicles:', 'Units:')), re.findall(r'- Name: (\w+)', block('Units:', 'Clips:')))


def buildings():
    """[(set, house)] for every chunked building: the names in each set's houses.json."""
    out = []
    for man in sorted((P / 'Resources' / 'Env').glob('*/houses.json')):
        seen = []
        for row in json.loads(man.read_text(encoding='utf-8'))['chunks']:
            if row['house'] not in seen:
                seen.append(row['house'])
        out += [(man.parent.name, h) for h in seen]
    return out


def film(only):
    if not ready() and not scene(SCENE, 'TW.Playground.PlaygroundHost.Instance != null'):
        print('gamefilm: no editor in Play in the playground (gamefilm.py up)')
        return 1
    OUT.mkdir(parents=True, exist_ok=True)
    want = lambda n: not only or n in only
    vehicles, units = library()
    fails = 0
    for k, name in enumerate(vehicles):
        if not want(name):
            continue
        print(name)
        # a flying machine circles 14 m up, out of the stage's frame: the camera has to go with it
        man = json.loads((P / 'Playground' / 'Art' / 'Tanks' / name / 'tank3.json').read_text(encoding='utf-8'))
        follow = '; cam follow 0 16 30 35' if man.get('flyer') else ''
        # `walk 2 1` is a pace on the spot, and only a walker, hopper or flyer rig answers it. A wheeled or tracked
        # machine (Mercy, Brute) has none of those, so it used to stand still for the whole of its `moves` film:
        # it is sent round a circle instead, which its ground drive does answer.
        pace = 'walk 2 1' if (man.get('walker') or man.get('hopper') or man.get('flyer')) else 'walk 2'
        do(f'{STAGE}; cookdelay 5; model v {k}; vehicle'); settle(2.5)
        do(f'vehicle; repair; walk 0; fly 0{follow}'); settle()
        if not record(f'{name}.trial.game-turn', 8, spin=360):
            fails += 1
        do(f'vehicle; {pace}; fly 8{follow}'); settle()
        if not record(f'{name}.trial.game-moves', 12, cues=[(0.2, 'traverse'), (3.0, 'fire'), (4.5, 'fire'), (6.0, 'fire'), (7.5, 'fire'), (9.0, 'traverse'), (10.0, 'fire')]):
            fails += 1
        do(f'vehicle; walk 0; fly 3{follow}'); settle()
        if not record(f'{name}.trial.game-destroyed', 16, cues=[(0.5, 'seq')]):
            fails += 1
        do('repair')
    for k, name in enumerate(units):
        if not want(name):
            continue
        print(name)
        do(f'{STAGE}; model u {k}; unit; clip Rifle Idle'); settle(2.5)
        do('unit; clip Rifle Idle'); settle()
        if not record(f'{name}.trial.game-turn', 8, spin=360):
            fails += 1
        do('unit; clip Rifle Walk; face 40'); settle()
        if not record(f'{name}.trial.game-moves', 20, cues=[(3, 'clip Rifle Run (1)'), (5.5, 'clip Rifle Crouch Walk'), (8, 'clip Firing Rifle'), (10.5, 'clip Fire Rifle'),
                                                     (12.5, 'clip Reloading'), (15, 'clip Toss Grenade'), (17.5, 'kill')]):
            fails += 1
        do('unit; clip Rifle Run (1); face 40'); settle()
        if not record(f'{name}.trial.game-burning', 10, cues=[(1.0, 'ignite')]):
            fails += 1
    for set_name, house in buildings():
        if not want(house):
            continue
        print(house)
        do(f'{STAGE}; set {set_name}; house {house}'); settle(2.5)
        if not record(f'{house}.game-turn', 8, spin=360):
            fails += 1
        do('rebuild'); settle(0.5)
        if not record(f'{house}.game-shelled', 14, cues=[(0.5 + 0.7 * i, 'shell') for i in range(14)]):
            fails += 1
        do('rebuild')
    if fails:
        print(f'gamefilm: {fails} takes failed their check')
    return 1 if fails else 0


def scene(path, check):
    """Leave Play, open a scene, enter Play, wait until `check` (a C# expression) is true."""
    run('UnityEditor.EditorApplication.isPlaying = false; return 0;', 60)
    for _ in range(30):
        time.sleep(2)
        if run('return UnityEditor.EditorApplication.isPlaying ? 1 : 0;', 20) == 0:
            break
    run(f'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("{path}"); UnityEditor.EditorApplication.isPlaying = true; return 0;', 120)
    for _ in range(45):
        time.sleep(4)
        if run(f'return UnityEditor.EditorApplication.isPlaying && ({check}) ? 1 : 0;', 20) == 1:
            return True
    return False


def machines():
    """[(name, archetype id)] of the vehicles that have a battle model: the constants of VehicleArchetype, in id order."""
    sys.path.insert(0, str(HERE))
    import src_code
    code = src_code.read_all(P)
    out = [(n, v[0]) for n, v in code['vehicle'].items() if (P / 'Resources' / 'Vehicles' / n).is_dir()]
    return sorted(out, key=lambda x: x[1])


def reach(name):
    """How big a machine is, for the camera: the largest of its model's width, length and (weighted, it stands on legs
    and the camera looks down) height, read from the FBX's bounds as the board's preview measured them, else 5 m."""
    for site in (Path(os.environ.get('TW_ASSETBOARD_OUT', '')), Path('G:/My Drive/TW3D-pipeline/assets')):
        f = site / 'data' / 'thumbs.json'
        if str(site) not in ('', '.') and f.exists():
            size = json.loads(f.read_text(encoding='utf-8')).get(f'{name}.battle', {}).get('size')
            if size:
                return max(size[0], size[1] * 1.4, size[2])
    return 5.0


def figures():
    """[(name, archetype id)] of the men who have a battle figure of their own (the board's own rule)."""
    sys.path.insert(0, str(HERE))
    import model
    import src_code
    assets, _ = model.build(P, src_code.read_all(P), model.load_notes(ROOT.parent / 'docs' / 'reference' / 'asset-notes.json'))
    out = [(a['id'], a['archetype']) for a in assets.values()
           if a['category'] == 'character' and a['archetype'] is not None and not a['drawn_as'] and any(m['form'] == 'battle' for m in a['models'])]
    return sorted(out, key=lambda x: x[1])


def battle(only):
    if run('return 1;', 20) != 1:
        print('gamefilm: no editor (gamefilm.py up)')
        return 1
    host = 'UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>()'
    if not scene('Assets/_Project/Scenes/GreyboxCorridor.unity', f'{host} != null && {host}.Local != null && {host}.Local.World.Tick > 30'):
        print('gamefilm: the match did not start')
        return 1
    OUT.mkdir(parents=True, exist_ok=True)
    size = str(run(f'var s = {host}.Local.Map.SizeMeters; return s.x + " " + s.y;')).split()
    width, depth = float(size[0]), float(size[1])
    todo = [(n, k) for n, k in machines() if not only or n in only]
    fails = 0
    for i, (name, arch) in enumerate(todo):
        print(name)
        # each on ground of its own, two rows deep, so the last one's wreck is not in the next one's film
        per = (len(todo) + 1) // 2
        x, z = width * (0.12 + 0.76 * (i // 2 + 0.5) / per), depth * (0.22 if i % 2 == 0 else 0.42)
        got = str(run(f'return TW.Editor.RiderLab.Setup({arch}, 0, {x:.1f}f, {z:.1f}f, 0, 0f, false);'))
        if not got.startswith('slot '):
            print(f'  {name}: {got}')
            continue
        slot = int(got.split()[1])
        time.sleep(2.0)
        # the game draws a machine smaller than it is modelled: 16 frames a walker, 22 the largest hulls (2026-10-03,
        # by eye: 13 cut the Maw's roof off, 32 made it a toy)
        near = min(22.0, max(16.0, 2.6 * reach(name)))
        far = near * 1.3
        # the front quarter of THIS machine, from its own facing: a raw yaw is not a bearing (FilmAim), and 150
        # degrees filmed the Maw from behind with its guns hidden (the critique of 2026-10-06)
        bear = run(f'var w = {host}.Local.World; return TW.Presentation.Tactical.FilmAim.QuarterFront(w.Yaw[{slot}] * UnityEngine.Mathf.Rad2Deg);')
        bear = float(bear) if isinstance(bear, (int, float)) else 125.0
        run(f'TW.Editor.RiderLab.Panel(false); TW.Editor.RiderLab.Camera({slot}, {near:.1f}f, {bear:.1f}f, 22f); return TW.Editor.TankCapture.Follow({slot}, {near:.1f}f, {bear:.1f}f, false);')
        time.sleep(1.5)
        cam = dict(slot=slot, zoom=near, yaw=bear)
        # a turn film orbits: the spin ADDS to that bearing frame by frame, it does not replace it
        if not record(f'{name}.battle.game-turn', 8, spin=360, battle=cam):
            fails += 1
        # the machine drives and turns from here on, so the bearing follows its facing every frame (onFacing)
        run(f'TW.Editor.RiderLab.Camera({slot}, {far:.1f}f, {bear:.1f}f, 24f); return TW.Editor.TankCapture.Follow({slot}, {far:.1f}f, {bear:.1f}f, true);')
        if not record(f'{name}.battle.game-moves', 16, battle=dict(cam, zoom=far),
               cues=[(0.2, f'TW.Editor.RiderLab.Enemies({slot}, 8, 55f, 0f); TW.Editor.RiderLab.Drive({slot}, 45f);')]):
            fails += 1
        run(f'TW.Editor.RiderLab.Stop({slot}); return TW.Editor.RiderLab.ClearEnemies({slot}, 120f);')
        run(f'return TW.Editor.TankCapture.Follow({slot}, {far:.1f}f, {bear:.1f}f, true);')
        if not record(f'{name}.battle.game-destroyed', 10, battle=dict(cam, zoom=far),
               cues=[(1.0, f'TW.Editor.TankCapture.Ignite({slot}, 1f);'), (4.0, f'TW.Editor.RiderLab.Kill({slot});')]):
            fails += 1
    # the men: a section of each kind, enemy riflemen ahead of it, filmed as the sim fights them
    men = [(n, k) for n, k in figures() if not only or n in only]
    for i, (name, arch) in enumerate(men):
        print(name)
        # a row of their own, away from the machines' wrecks; filmed from above (20 degrees put houses in front of them)
        x, z = width * (0.25 + 0.5 * (i + 0.5) / len(men)), depth * 0.68
        slots = []
        for k in range(6):
            got = str(run(f'return TW.Editor.TankCapture.Spawn(0, {arch}, {x + (k - 2.5) * 2.2:.1f}f, {z - (k % 2) * 2.5:.1f}f, 0f);'))
            if got.startswith('slot '):
                slots.append(int(got.split()[1]))
        if not slots:
            print(f'  {name}: no man spawned')
            continue
        lead = slots[len(slots) // 2]
        time.sleep(2.0)
        run(f'TW.Editor.RiderLab.Panel(false); TW.Editor.RiderLab.Camera({lead}, 7f, 150f, 42f); return TW.Editor.TankCapture.Follow({lead}, 7f, 150f, false);')
        time.sleep(1.0)
        if not record(f'{name}.battle.game-battle', 18, battle=dict(slot=lead, zoom=7, yaw=150), cues=[(0.5, f'TW.Editor.RiderLab.Enemies({lead}, 8, 38f, 0f);')]):
            fails += 1
        run(f'return TW.Editor.RiderLab.ClearEnemies({lead}, 120f);')
    if fails:
        print(f'gamefilm: {fails} takes failed their check')
    return 1 if fails else 0


# --- the check: did the film keep its subject, and did the subject move? -----------------------------------------
# Two ways of judging, because a film from the Drive has no trace beside it.
#  - With a trace (TankCapture.Trace, written frame by frame while we film): the truth, in world and viewport terms.
#  - Without one: ffmpeg only. The numbers below were measured over all 46 game films of the critique of 2026-10-06
#    (Tools/assetboard/gamefilm.py check on that folder prints them again).
IN_FRAME = (0.08, 0.92)      # a subject outside this part of the viewport is not really in the picture
MOVED_M = 3.0                # a film named `moves` whose subject travels less than this stood still
GUNS_SHARE = 0.1             # a machine may be seen from behind in a tenth of its frames, no more
ORBIT_GUNS = 0.5             # [A2] a `turn` film orbits the machine on purpose, so a third of it is behind by design
GLIMPSE = 6                  # the camera eases between bearings, so the subject can swing out of the frame for a
OUT_SHARE = 0.02             # moment: up to GLIMPSE filmed frames in a row (a quarter second) and OUT_SHARE of the
                             # film is a glimpse, not a lost subject. A film that really cuts away stays away.
ON_SHARE = 0.33              # [A4] a subject that leaves fewer than this share of frames on-subject is a dead or lost one
STILL_MEAN = 0.15            # mean frame difference, 0-255: the still Mercy sat at 0.024, the next quietest film (Brute, moving) at 0.627
CUT_JUMP = 40.0              # a change that STAYS (6 frames after against 6 before): the four men's battle films that cut away
                             # measured 42..66, every other film of 2026-10-06 at most 38 (Banner.turn, a camera orbit)
FREEZE_SHARE = 0.5           # ... or ffmpeg freezedetect calls more than half the film frozen


def trace_path(name):
    return OUT / '_trace' / f'{name}.json'


def read_trace(path):
    rows = []
    for line in Path(path).read_text(encoding='utf-8').splitlines()[1:]:
        f = line.split()
        if len(f) == 7:
            rows.append(dict(frame=int(f[0]), vx=float(f[1]), vy=float(f[2]), x=float(f[3]), z=float(f[4]),
                             on=f[5] == '1', guns=f[6] == '1'))
    return rows


def check_trace(name, rows, machine=False):
    """The failures of one film, read off its trace: [] when it is a good film."""
    if not rows:
        return ['NO FRAMES: the trace is empty']
    bad = []
    lo, hi = IN_FRAME
    out = [r for r in rows if not (lo <= r['vx'] <= hi and lo <= r['vy'] <= hi)]
    runs, longest = [], 0
    for r in out:
        if runs and r['frame'] == runs[-1][-1]['frame'] + 1:
            runs[-1].append(r)
        else:
            runs.append([r])
        longest = max(longest, len(runs[-1]))
    if out and (longest > GLIMPSE or len(out) > OUT_SHARE * len(rows)):
        r = out[0]
        bad.append(f'LOST SUBJECT: {len(out)} of {len(rows)} frames have it outside the frame, '
                   f'{longest} in a row (first at frame {r["frame"]}, viewport {r["vx"]:.2f} {r["vy"]:.2f})')
    if 'moves' in name:
        x0, z0 = rows[0]['x'], rows[0]['z']
        span = max(((r['x'] - x0) ** 2 + (r['z'] - z0) ** 2) ** 0.5 for r in rows)
        if span < MOVED_M:
            bad.append(f'STILL: the subject moved {span:.1f} m, under {MOVED_M:.0f} m, in a film named moves')
    if machine:
        # [A2] a `turn` film orbits the machine on purpose: it is behind for a third of its frames by design
        share = ORBIT_GUNS if 'turn' in name else GUNS_SHARE
        hidden = [r for r in rows if not r['guns']]
        if len(hidden) > share * len(rows):
            bad.append(f'FROM BEHIND: {len(hidden)} of {len(rows)} frames do not show its guns')
    # [A4] the `on` column was read and never judged: a subject that died early left the camera on an empty spot
    on = [r for r in rows if r['on']]
    if not rows[0]['on']:
        bad.append('NO SUBJECT: the camera never held it')
    elif len(on) < ON_SHARE * len(rows):
        bad.append(f'SUBJECT LOST EARLY: {len(on)} of {len(rows)} frames hold the real unit')
    return bad


def grey(path):
    """Every frame of a film as a 64x36 grey picture (0-255), the way the critique's filmsheet.py reads motion."""
    import numpy as np
    raw = subprocess.run([FFMPEG, '-v', 'error', '-i', str(path), '-f', 'rawvideo', '-pix_fmt', 'gray',
                          '-vf', 'scale=64:36', '-'], capture_output=True).stdout
    return np.frombuffer(raw, np.uint8).reshape(-1, 36, 64).astype(np.float32)


def frozen_seconds(path):
    """How many seconds ffmpeg's freezedetect calls frozen."""
    r = subprocess.run([FFMPEG, '-v', 'info', '-i', str(path), '-vf', 'freezedetect=n=-60dB:d=2', '-f', 'null', '-'],
                       capture_output=True, text=True)
    return sum(float(m) for m in re.findall(r'freeze_duration: ([\d.]+)', r.stderr))


def sustained(g):
    """Per frame, how much the picture CHANGES AND STAYS changed: the mean of the 6 frames after against the 6 before.
    A shell flash comes back; a cut to another place does not, which is how one is told from the other."""
    import numpy as np
    k = 6
    if len(g) < 2 * k + 2:
        return np.zeros(1), 0
    pre = np.stack([g[i - k:i].mean(axis=0) for i in range(k, len(g) - k)])
    post = np.stack([g[i + 1:i + 1 + k].mean(axis=0) for i in range(k, len(g) - k)])
    s = np.abs(post - pre).mean(axis=(1, 2))
    return s, k


def check_pixels(name, path):
    """The failures of one film with no trace: ffmpeg and arithmetic only."""
    import numpy as np
    g = grey(path)
    if len(g) < 4:
        return [f'NO FRAMES: ffmpeg read {len(g)} frames']
    bad = []
    d = np.abs(g[1:] - g[:-1]).mean(axis=(1, 2))
    if 'moves' in name:
        froze = frozen_seconds(path)
        if float(d.mean()) < STILL_MEAN or froze > FREEZE_SHARE * len(g) / FPS:
            bad.append(f'STILL: the picture changes by {d.mean():.3f} of 255 a frame '
                       f'(under {STILL_MEAN}){f", frozen {froze:.0f} s" if froze else ""}, in a film named moves')
    s, k = sustained(g)
    if float(s.max()) >= CUT_JUMP:
        bad.append(f'LOST SUBJECT: the picture jumps by {s.max():.0f} of 255 and stays changed '
                   f'at {(int(s.argmax()) + k) / FPS:.2f} s (a cut away from the subject)')
    return bad


def machine_names():
    """The names of the battle machines, for the guns rule. Empty when the code cannot be read (then no guns rule)."""
    try:
        return {n for n, _ in machines()}
    except Exception as e:
        print(f'gamefilm: the machine names could not be read ({e}): no guns rule')
        return set()


def trace_for(film):
    """[A8] The trace path for `film`, or None when `film` does not sit in OUT: a same-named film copied in from
    elsewhere (the Drive, a temp folder) has no trace of its own, and ffmpeg's only is a sounder judge of it."""
    film = Path(film)
    if film.resolve().parent != OUT.resolve():
        return None
    return trace_path(film.stem)


def check_one(film, machines_known=None):
    """[] or the failures of one .mp4. Prints the verdict."""
    film = Path(film)
    name = film.stem
    subject = name.split('.')[0]
    tp = trace_for(film)
    if tp is not None and tp.exists() and tp.stat().st_size > 60:
        rows = read_trace(tp)
        known = machine_names() if machines_known is None else machines_known
        bad = check_trace(name, rows, machine=subject in known)
        how = f'traced, {len(rows)} frames'
    else:
        bad = check_pixels(name, film)
        how = 'no trace: ffmpeg'
    print(f'  {name}: {"FAIL" if bad else "OK"} ({how})')
    for b in bad:
        print(f'    {b}')
    return bad


def check(args):
    films = []
    for a in (args or [OUT]):
        p = Path(a)
        films += sorted(p.glob('*.mp4')) if p.is_dir() else [p]
    if not FFMPEG:
        print('gamefilm: no ffmpeg (TW_FFMPEG)')
        return 1
    known = machine_names()
    fails = [f for f in films if check_one(f, known)]
    print(f'gamefilm: {len(films) - len(fails)} of {len(films)} films kept their subject and moved')
    return 1 if fails else 0

if __name__ == '__main__':
    cmd = sys.argv[1] if len(sys.argv) > 1 else ''
    if not CLI.exists() and cmd != 'check':      # check reads films, so it needs no editor
        print(f'gamefilm: no unity CLI at {CLI}')
        sys.exit(1)
    sys.exit(up() if cmd == 'up' else down() if cmd == 'down' else film(set(sys.argv[2:])) if cmd == 'film'
             else battle(set(sys.argv[2:])) if cmd == 'battle' else check(sys.argv[2:]) if cmd == 'check'
             else (print(__doc__) or 2))
