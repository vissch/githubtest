#!/usr/bin/env python3
"""Driver for sapper-look-3-g1's evidence: three seeds, each one pair of PNGs (an Engineer and a Sapper, same two
men, same pose) with a `men` array in the sidecar .json that was measured from the engine's own boxes, not typed.

    python docs/evidence/sapper-look-3-g1/capture.py

Copies gamefilm.py's way of driving a batch-mode editor over the pipeline port (Tools/assetboard/gamefilm.py):
the CLI is called by its full path, UNITY_PROJECT_PATH points at this checkout, the editor is started detached
with -batchmode -projectPath ... -logFile ... (no -nographics, no -quit) so it still renders.

Archetypes (Sim/Core/RosterEntry.cs:102-109): 15 = Repair (the Engineer), 35 = Sapper.
"""
import json
import math
import os
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2] / 'trench-warfare-3d'
CLI = Path(os.environ.get('LOCALAPPDATA', '')) / 'unity' / 'bin' / 'unity.exe'
EDITOR = Path(os.environ.get('TW_UNITY', r'C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe'))
LOG = ROOT / 'Logs' / 'sapper-g1-editor.log'
SCENE = 'Assets/_Project/Scenes/GreyboxCorridor.unity'
ENGINEER, SAPPER = 15, 35


def run(code, timeout=60):
    env = dict(os.environ, UNITY_PROJECT_PATH=str(ROOT))
    r = subprocess.run([str(CLI), '--no-banner', 'cmd', 'eval', '--result-only', '--timeout', str(timeout), '--', '--code', code],
                       capture_output=True, text=True, env=env, timeout=timeout + 60)
    try:
        return json.loads(r.stdout).get('result')
    except ValueError:
        return None if not r.stdout.strip() else r.stdout.strip()[-400:]


HOST = 'UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>()'


def playing():
    return run(f'return UnityEditor.EditorApplication.isPlaying && {HOST} != null && {HOST}.Local != null '
               f'&& {HOST}.Local.World.Tick > 30 ? 1 : 0;', 20) == 1


def up():
    if not playing():
        if run('return 1;', 20) != 1:
            LOG.parent.mkdir(parents=True, exist_ok=True)
            subprocess.Popen([str(EDITOR), '-batchmode', '-projectPath', str(ROOT), '-logFile', str(LOG)], creationflags=0x00000008)
            print(f'capture: starting a batch-mode editor (log: {LOG})')
            for _ in range(180):
                time.sleep(10)
                if run('return 1;', 20) == 1:
                    break
            else:
                print('capture: the editor did not answer in 30 minutes')
                return False
        run(f'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("{SCENE}"); UnityEditor.EditorApplication.isPlaying = true; return 0;', 120)
        for _ in range(45):
            time.sleep(4)
            if playing():
                break
        else:
            print('capture: the match did not start')
            return False
    print('capture: in Play on GreyboxCorridor, tick > 30')
    return True


def down():
    run('UnityEditor.EditorApplication.isPlaying = false; return 0;', 60)
    time.sleep(2)
    run('UnityEditor.EditorApplication.Exit(0); return 0;', 30)


def map_info():
    out = run(f'var s = {HOST}.Local.Map.SizeMeters; var c = TW.Editor.CaptureRig.Crowd(); '
              f'return s.x + " " + s.y + " " + c.x + " " + c.y;')
    sx, sz, cx, cz = (float(v) for v in str(out).split())
    return sx, sz, cx, cz


def ground_height(x, z):
    return float(run(f'return {HOST}.Local.Map.Height.Sample({x:.2f}f, {z:.2f}f);'))


def pick_site(sx, sz, cx, cz, avoid):
    """A corner at least 60 m from Crowd() and from any site already used this run."""
    candidates = [(sx * f, sz * g) for f in (0.12, 0.88) for g in (0.12, 0.88)]
    candidates.sort(key=lambda p: -math.hypot(p[0] - cx, p[1] - cz))
    for p in candidates:
        if math.hypot(p[0] - cx, p[1] - cz) < 60:
            continue
        if all(math.hypot(p[0] - q[0], p[1] - q[1]) >= 30 for q in avoid):
            return p
    return candidates[0]


def spawn_pair(mid_x, mid_z, yaw_deg):
    """Two Engineers 2.2 m apart across the view, along the camera's own right vector at this yaw (pitch does not
    change a horizontal offset). Returns (left_slot, right_slot, left_x, left_z, right_x, right_z)."""
    yaw = math.radians(yaw_deg)
    rx, rz = math.cos(yaw), -math.sin(yaw)
    lx, lz = mid_x - rx * 1.1, mid_z - rz * 1.1
    rxp, rzp = mid_x + rx * 1.1, mid_z + rz * 1.1
    got_l = str(run(f'return TW.Editor.TankCapture.Spawn(0, {ENGINEER}, {lx:.2f}f, {lz:.2f}f, {yaw_deg:.1f}f);'))
    got_r = str(run(f'return TW.Editor.TankCapture.Spawn(0, {ENGINEER}, {rxp:.2f}f, {rzp:.2f}f, {yaw_deg:.1f}f);'))
    if not (got_l.startswith('slot ') and got_r.startswith('slot ')):
        raise RuntimeError(f'spawn failed: left={got_l!r} right={got_r!r}')
    return int(got_l.split()[1]), int(got_r.split()[1]), lx, lz, rxp, rzp


def shot_only(path, mid_x, mid_z, zoom, yaw, pitch, aim_y):
    """Queues and waits out one still. Does NOT Hold/Release: a pose search holds the world ONCE across every
    attempt (see do_seed) so an autonomous Repair-archetype man does not walk off during the search itself."""
    run(f'TW.Editor.CaptureRig.Shot(@"{path}", {mid_x:.2f}f, {mid_z:.2f}f, {zoom:.2f}f, {yaw:.1f}f, {pitch:.1f}f, 1280, 720, {aim_y:.2f}f); return 0;', 30)
    for _ in range(60):
        if str(run('return TW.Editor.CaptureRig.Pending();', 20)) == '0':
            break
        time.sleep(0.5)


def shoot(path, mid_x, mid_z, zoom, yaw, pitch, aim_y):
    run('TW.Editor.CaptureRig.Hold(120f); return 0;', 30)
    shot_only(path, mid_x, mid_z, zoom, yaw, pitch, aim_y)
    run('return TW.Editor.CaptureRig.Release();', 30)


def sidecar(png_path):
    j = Path(str(png_path).replace('.png', '.json'))
    return json.loads(j.read_text(encoding='utf-8')) if j.exists() else {}


def framing_ok(data, want_slots):
    """Readable means found by the SAME rule CaptureRig.Measure uses to count `men_in_frame`: a real contrast,
    not merely 'somewhere in the frustum'. Every other live man on the field (the battle lines, a leftover pair
    from an earlier seed) sits well outside the margin at these zooms and always reports contrast -1, so this
    is the achievable form of "exactly the two spawned slots": exactly two READABLE men, and they are ours."""
    readable = {m['slot']: m for m in data.get('men', []) if m.get('contrast', -1) != -1}
    if set(readable) != set(want_slots):
        return False
    return all(readable[s]['tall'] >= 120 for s in want_slots)


def find_pose(mid_x, mid_z, yaw_deg, zoom0, pitch0, path, aim_y, left_slot, right_slot):
    """Re-shoots the (still-both-Engineer) pair at this spot, lowering zoom until both men are >=120 px tall and
    raising pitch (toward 42, gamefilm.py's figure pitch) until both have a real contrast and are the only two
    in frame. The last shot taken IS the kept engineer still: no throwaway frames."""
    zoom, pitch = zoom0, pitch0
    data = {}
    for attempt in range(8):
        shot_only(str(path), mid_x, mid_z, zoom, yaw_deg, pitch, aim_y)
        data = sidecar(path)
        men = {m['slot']: m for m in data.get('men', [])}
        print(f'  attempt {attempt}: zoom={zoom:.1f} pitch={pitch:.1f} men={men}')
        if framing_ok(data, (left_slot, right_slot)):
            return zoom, pitch
        if any(s not in men for s in (left_slot, right_slot)):
            zoom = min(20.0, zoom + 3.0)              # missing entirely: pull back, not in
            continue
        shortest = min((men.get(s, {}).get('tall', 0) for s in (left_slot, right_slot)), default=0)
        if shortest < 120 and zoom > 5:
            zoom = max(5.0, zoom - 1.0)
        elif pitch < 42:
            pitch = min(42.0, pitch + 4.0)
        else:
            break
    return zoom, pitch


def do_seed(n, mid_x, mid_z, yaw_deg, zoom0, pitch0):
    print(f'seed {n}: focus ({mid_x:.1f}, {mid_z:.1f}) yaw {yaw_deg}')
    left_slot, right_slot, lx, lz, rx, rz = spawn_pair(mid_x, mid_z, yaw_deg)
    time.sleep(2.0)   # let the sim tick and the renderer build their draw entries, BEFORE the world is held
    # Re-centre on where the two men ACTUALLY ended up, not where Spawn was asked to put them: a spawn point
    # inside a prop or trench wall is snapped elsewhere by the sim's own placement, and the first run of this
    # script photographed one man dead centre and the other off the edge of the frame because the midpoint it
    # aimed at was never the midpoint of the two real positions.
    ax, az, bx, bz = (float(v) for v in str(run(
        f'var a=(UnityEngine.Vector3){HOST}.Presenter.Drawn({left_slot}); var b=(UnityEngine.Vector3){HOST}.Presenter.Drawn({right_slot}); '
        f'return a.x+","+a.z+","+b.x+","+b.z;')).split(','))
    mid_x, mid_z = (ax + bx) / 2, (az + bz) / 2
    aim_y = ground_height(mid_x, mid_z) + 1.2   # chest height off THIS site's terrain, not sea level
    # Held for the whole search and the repeat shot: an autonomous Repair-archetype man walks on his own orders,
    # and a Release between search attempts (the first two runs of this script) let him wander off in the time
    # python took between calls, so a later attempt photographed an empty pose that an earlier one had found good.
    run('TW.Editor.CaptureRig.Hold(120f); return 0;', 30)
    eng_path = HERE / f'pair-engineer-{n}.png'
    zoom, pitch = find_pose(mid_x, mid_z, yaw_deg, zoom0, pitch0, eng_path, aim_y, left_slot, right_slot)
    print(f'  kept zoom={zoom:.1f} pitch={pitch:.1f}')
    run('return TW.Editor.CaptureRig.Release();', 30)
    # Two further stills of the SAME still-Engineer pose - nothing changed: not the pose, the camera, the
    # archetype or the weather clock. Each one goes through a full Release -> 0.25 s wait -> Hold -> shot cycle,
    # the identical regime the Sapper shot below is taken in, so each repeat costs the man the same one tick of
    # his own motion the kit shot costs him and the two numbers are comparable. Inside one unbroken Hold the
    # repeats are pixel-identical by construction (the 0.0 floor of the older evidence, and the same empty zero
    # the owner threw out), which is an easier control than the thing it judges.
    for suffix in ('', '-b'):
        time.sleep(0.25)
        shoot(str(HERE / f'noise-{n}{suffix}.png'), mid_x, mid_z, zoom, yaw_deg, pitch, aim_y)
    # the sanctioned door for a presentation tool to change the sim: SimHost.WriteWorlds, never a direct world write
    run(f'return {HOST}.WriteWorlds(m => m.World.Archetype[{right_slot}] = {SAPPER});', 30)
    time.sleep(0.25)  # the shortest real wait that reaches a tick (Hold() has not run yet): the draw entry only
                      # rebuilds on one, and near a firefight (seed 3) a full second is enough for both men to
                      # react and drift past the margin that framed them a moment before
    sap_path = HERE / f'pair-sapper-{n}.png'
    # retried in place (same Hold, same pose) rather than once: close to a firefight (seed 3) a man can react
    # and drift past the margin inside a single real tick, and the NEXT tick often finds him settled again
    for _ in range(3):
        run('TW.Editor.CaptureRig.Hold(120f); return 0;', 30)
        shot_only(str(sap_path), mid_x, mid_z, zoom, yaw_deg, pitch, aim_y)
        run('return TW.Editor.CaptureRig.Release();', 30)
        if framing_ok(sidecar(sap_path), (left_slot, right_slot)):
            break
        time.sleep(0.25)
    # both stills are on disk: clear this seed's two men (UnitFlags.Alive off, through the sanctioned WriteWorlds
    # door) so they stop wandering and cannot clutter a later, more distant seed's frame or its slot numbers
    run(f'return {HOST}.WriteWorlds(m => {{ m.World.Flags[{left_slot}] &= ~1u; m.World.Flags[{right_slot}] &= ~1u; }});', 30)
    return {'seed': n, 'zoom': zoom, 'pitch': pitch, 'yaw': yaw_deg, 'focus': [mid_x, mid_z],
            'left_slot': left_slot, 'right_slot': right_slot}


def main():
    if not CLI.exists():
        print(f'capture: no unity CLI at {CLI}')
        return 1
    if not up():
        return 1
    try:
        # Pose() reads BaseYaw (the tactical camera's own, which eases every frame) unless YawPin overrides it; the
        # pair is spawned along a world offset computed from OUR yaw, so the camera must be pinned to that same
        # yaw or the men land wherever BaseYaw happened to be, not where the offset math put them.
        run('TW.Editor.CaptureRig.Rig.YawPin = 0f; return 0;', 20)
        sx, sz, cx, cz = map_info()
        print(f'map {sx:.0f} x {sz:.0f}, crowd at ({cx:.1f}, {cz:.1f})')
        site1 = pick_site(sx, sz, cx, cz, avoid=[])
        site2 = pick_site(sx, sz, cx, cz, avoid=[site1])
        poses = []
        poses.append(do_seed(1, site1[0], site1[1], 0.0, 10.0, 25.0))
        poses.append(do_seed(2, site2[0], site2[1], 90.0, 10.0, 25.0))
        # seed 3: the owner's own view, the old sapper-t3 pose among the stakes
        poses.append(do_seed(3, 50.0, 50.0, 21.0, 7.0, 25.0))
        print(json.dumps(poses, indent=2))
    finally:
        run('TW.Editor.CaptureRig.Rig.YawPin = float.NaN; return 0;', 20)
        down()
    missing = [p for p in HERE.glob('pair-*-*.png') if not p.with_suffix('.json').exists()]
    ok = len(list(HERE.glob('pair-*-*.png'))) == 6 and not missing
    print('capture: wrote 6 PNGs and their sidecars' if ok else f'capture: incomplete, missing json for {missing}')
    return 0 if ok else 1


if __name__ == '__main__':
    raise SystemExit(main())
