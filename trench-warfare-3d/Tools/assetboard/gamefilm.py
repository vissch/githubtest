#!/usr/bin/env python3
"""Films of the game itself, for the asset board: each model in the asset playground (docs/22), drawn by the game's own
shaders, walking, turning its guns, firing, being shot to pieces; each building shelled until it is down.

    python Tools/assetboard/gamefilm.py up              start a batch-mode editor on this checkout, in Play in the playground
    python Tools/assetboard/gamefilm.py film [NAME ..]  film every playground model and building (or the named ones)
    python Tools/assetboard/gamefilm.py battle [NAME ..] film every battle machine in a match (GreyboxCorridor): it
                                                        turns on the spot, drives at enemy riflemen and fires, is destroyed
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
    i++;
%(cues)s
    if (spin != 0f) TW.Editor.TankCapture.Follow(slot, zoom, yaw0 + spin * i / frames);
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
        lines = ''.join(f'    if (i == {max(1, int(round(t * FPS)))}) {{ {stmt} }}\n' for t, stmt in cues)
        got = run(BATTLE_RECORDER % dict(dir=frames_dir.as_posix(), frames=frames, w=W, h=H, fps=FPS, spin=repr(float(spin)), cues=lines,
                                         slot=battle['slot'], zoom=repr(float(battle['zoom'])), yaw=repr(float(battle['yaw']))))
    else:
        lines = ''.join(f'cues.Add(new System.Collections.Generic.KeyValuePair<int, string>({max(1, int(round(t * FPS)))}, "{c.strip()}"));\n'
                        for t, cmds in cues for c in cmds.split(';') if c.strip())
        got = run(RECORDER % dict(dir=frames_dir.as_posix(), frames=frames, w=W, h=H, fps=FPS, spin=repr(float(spin)), cues=lines))
    if not str(got).startswith('filming'):
        print(f'  {name}: the recorder did not start: {got}')
        return False
    for _ in range(int(seconds * 20) + 240):
        if (frames_dir / 'done.txt').exists():
            break
        time.sleep(0.5)
    else:
        print(f'  {name}: timed out with {len(list(frames_dir.glob("*.jpg")))} of {frames} frames')
        return False
    mp4 = OUT / f'{name}.mp4'
    r = subprocess.run([FFMPEG, '-y', '-loglevel', 'error', '-r', str(FPS), '-i', str(frames_dir / '%05d.jpg'), '-an', '-c:v', 'libx264', '-preset', 'slow',
                        '-crf', '24', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', str(mp4)], capture_output=True, text=True)
    if r.returncode != 0:
        print(f'  {name}: ffmpeg: {r.stderr[-200:]}')
        return False
    shutil.copyfile(frames_dir / f'{frames // 3:05d}.jpg', OUT / f'{name}.jpg')
    shutil.rmtree(frames_dir, ignore_errors=True)
    print(f'  {name}.mp4  {seconds:.0f} s  {mp4.stat().st_size // 1024} KB')
    return True


def settle(seconds=1.5):
    time.sleep(seconds)


def library():
    """The playground library's vehicles and figures, in its order (the index `model v N` / `model u N` takes)."""
    text = (P / 'Playground' / 'PlaygroundLibrary.asset').read_text(encoding='utf-8')
    block = lambda a, b: text[text.index(a):text.index(b)] if a in text and b in text else ''
    import re
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
    for k, name in enumerate(vehicles):
        if not want(name):
            continue
        print(name)
        # a flying machine circles 14 m up, out of the stage's frame: the camera has to go with it
        man = json.loads((P / 'Playground' / 'Art' / 'Tanks' / name / 'tank3.json').read_text(encoding='utf-8'))
        follow = '; cam follow 0 16 30 35' if man.get('flyer') else ''
        do(f'{STAGE}; cookdelay 5; model v {k}; vehicle'); settle(2.5)
        do(f'vehicle; repair; walk 0; fly 0{follow}'); settle()
        record(f'{name}.trial.game-turn', 8, spin=360)
        do(f'vehicle; walk 2 1; fly 8{follow}'); settle()
        record(f'{name}.trial.game-moves', 12, cues=[(0.2, 'traverse'), (3.0, 'fire'), (4.5, 'fire'), (6.0, 'fire'), (7.5, 'fire'), (9.0, 'traverse'), (10.0, 'fire')])
        do(f'vehicle; walk 0; fly 3{follow}'); settle()
        record(f'{name}.trial.game-destroyed', 16, cues=[(0.5, 'seq')])
        do('repair')
    for k, name in enumerate(units):
        if not want(name):
            continue
        print(name)
        do(f'{STAGE}; model u {k}; unit; clip Rifle Idle'); settle(2.5)
        do('unit; clip Rifle Idle'); settle()
        record(f'{name}.trial.game-turn', 8, spin=360)
        do('unit; clip Rifle Walk; face 40'); settle()
        record(f'{name}.trial.game-moves', 20, cues=[(3, 'clip Rifle Run (1)'), (5.5, 'clip Rifle Crouch Walk'), (8, 'clip Firing Rifle'), (10.5, 'clip Fire Rifle'),
                                                     (12.5, 'clip Reloading'), (15, 'clip Toss Grenade'), (17.5, 'kill')])
        do('unit; clip Rifle Run (1); face 40'); settle()
        record(f'{name}.trial.game-burning', 10, cues=[(1.0, 'ignite')])
    for set_name, house in buildings():
        if not want(house):
            continue
        print(house)
        do(f'{STAGE}; set {set_name}; house {house}'); settle(2.5)
        record(f'{house}.game-turn', 8, spin=360)
        do('rebuild'); settle(0.5)
        record(f'{house}.game-shelled', 14, cues=[(0.5 + 0.7 * i, 'shell') for i in range(14)])
        do('rebuild')
    return 0


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
        run(f'TW.Editor.RiderLab.Panel(false); return TW.Editor.RiderLab.Camera({slot}, {near:.1f}f, 35f, 22f);')
        time.sleep(1.5)
        cam = dict(slot=slot, zoom=near, yaw=35)
        record(f'{name}.battle.game-turn', 8, spin=360, battle=cam)
        run(f'return TW.Editor.RiderLab.Camera({slot}, {far:.1f}f, 150f, 24f);')
        record(f'{name}.battle.game-moves', 16, battle=dict(cam, zoom=far, yaw=150),
               cues=[(0.2, f'TW.Editor.RiderLab.Enemies({slot}, 8, 55f, 0f); TW.Editor.RiderLab.Drive({slot}, 45f);')])
        run(f'TW.Editor.RiderLab.Stop({slot}); return TW.Editor.RiderLab.ClearEnemies({slot}, 120f);')
        record(f'{name}.battle.game-destroyed', 10, battle=dict(cam, zoom=far, yaw=150),
               cues=[(1.0, f'TW.Editor.TankCapture.Ignite({slot}, 1f);'), (4.0, f'TW.Editor.RiderLab.Kill({slot});')])
    # the men: a section of each kind, enemy riflemen ahead of it, filmed as the sim fights them
    men = [(n, k) for n, k in figures() if not only or n in only]
    for i, (name, arch) in enumerate(men):
        print(name)
        x, z = width * (0.25 + 0.5 * (i + 0.5) / len(men)), depth * 0.42
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
        run(f'TW.Editor.RiderLab.Panel(false); return TW.Editor.RiderLab.Camera({lead}, 8f, 150f, 20f);')
        time.sleep(1.0)
        record(f'{name}.battle.game-battle', 18, battle=dict(slot=lead, zoom=8, yaw=150), cues=[(0.5, f'TW.Editor.RiderLab.Enemies({lead}, 8, 38f, 0f);')])
        run(f'return TW.Editor.RiderLab.ClearEnemies({lead}, 120f);')
    return 0


if __name__ == '__main__':
    cmd = sys.argv[1] if len(sys.argv) > 1 else ''
    if not CLI.exists():
        print(f'gamefilm: no unity CLI at {CLI}')
        sys.exit(1)
    sys.exit(up() if cmd == 'up' else down() if cmd == 'down' else film(set(sys.argv[2:])) if cmd == 'film'
             else battle(set(sys.argv[2:])) if cmd == 'battle' else (print(__doc__) or 2))
