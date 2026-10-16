#!/usr/bin/env python3
"""The game's time-lapse: one frame for every landing, the same field, same seed, same tick, so the board can join
them into a film and show the game growing (and the day it got worse).

    python Tools/timelapse.py shoot [--sha SHA]   shoot the standard view at the given commit (default: the
                                                   integration branch's tip); refuses while the editor is held
    python Tools/timelapse.py list                every frame on disk, landing order, with its hash_start
    python Tools/timelapse.py missing [--limit N] landings on the integration branch with no frame yet; exit 1 if any
    python Tools/timelapse.py film                joins the frames into timelapse.mp4, one caption burnt in each

Frames (and their sidecar .json) go to TW_TIMELAPSE, else `G:/My Drive/TW3D-pipeline/timelapse` when that Drive is
mounted, else `%LOCALAPPDATA%/TrenchWarfare/timelapse` (the pattern build.py's out_dir uses). Two frames are the
same battle only when their sidecar's hash_start matches; a landing is "shot" once its frame exists.

The shot already exists: TW.Editor.CaptureRig.Bench runs TW.Perf.PerfBench on the open GreyboxCorridor scene, the
same fixed battle, the standard view by default. `shot_tick=N shot_hud=0` holds the clock so the still repeats bit
for bit. No `fx=`/`fz=` (PerfBench warns "not the standard view") and no `ground=` (the default field) are ever
passed, so every frame this tool shoots is the standard view, bit for bit comparable.
"""
import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
REPO = ROOT.parent
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
SCENE = 'Assets/_Project/Scenes/GreyboxCorridor.unity'
CLI = Path(os.environ.get('LOCALAPPDATA', '')) / 'unity' / 'bin' / 'unity.exe'
EDITOR = Path(os.environ.get('TW_UNITY', r'C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe'))
FFMPEG = os.environ.get('TW_FFMPEG') or shutil.which('ffmpeg')
DRIVE = Path('G:/My Drive/TW3D-pipeline/timelapse')
LOCAL = Path(os.environ.get('LOCALAPPDATA', '/tmp')) / 'TrenchWarfare' / 'timelapse'
# the standard view (BenchOptions.cs): a fixed stress battle, held clock so the still repeats, the HUD hidden
BENCH_ARGS = 'stress=1500 settle_ticks=1800 ticks=400 shot_tick=200 shot_hud=0 quality=5 canary=0 weather=120 label=timelapse quit=1'
BENCH_TIMEOUT_S = 20 * 60


def out_dir():
    if os.environ.get('TW_TIMELAPSE'):
        return Path(os.environ['TW_TIMELAPSE'])
    return DRIVE if DRIVE.is_dir() else LOCAL


def git(*args):
    r = subprocess.run(['git', *args], cwd=REPO, capture_output=True, text=True)
    return r.stdout.strip()


def commit_info(rev):
    """sha, short sha, commit date (yyyy-mm-dd) and subject of `rev`."""
    out = git('log', '-1', '--format=%H%x1f%ad%x1f%s', '--date=short', rev)
    if not out:
        return None
    sha, date, subject = out.split('\x1f', 2)
    return dict(sha=sha, sha12=sha[:12], date=date, subject=subject)


def landings(base=f'origin/{INTEGRATION}'):
    """Every commit on `base`, oldest (first landed) first."""
    out = git('log', '--format=%H%x1f%ad%x1f%s', '--date=short', '--reverse', base)
    rows = []
    for line in out.splitlines():
        if not line:
            continue
        sha, date, subject = line.split('\x1f', 2)
        rows.append(dict(sha=sha, sha12=sha[:12], date=date, subject=subject))
    return rows


def frame_paths(folder, c):
    stem = f"{c['date']}-{c['sha12']}"
    return folder / f'{stem}.png', folder / f'{stem}.json'


def sidecar(folder, c):
    _, j = frame_paths(folder, c)
    if not j.exists():
        return None
    try:
        return json.loads(j.read_text(encoding='utf-8'))
    except ValueError:
        return None


def run(code, timeout=60):
    """One `cmd eval` round-trip against the open editor, as gamefilm.py does it."""
    env = dict(os.environ, UNITY_PROJECT_PATH=str(ROOT))
    r = subprocess.run([str(CLI), '--no-banner', 'cmd', 'eval', '--result-only', '--timeout', str(timeout), '--', '--code', code],
                       capture_output=True, text=True, env=env, timeout=timeout + 60)
    try:
        return json.loads(r.stdout).get('result')
    except ValueError:
        return None if not r.stdout.strip() else r.stdout.strip()[-300:]


def editor_held():
    sys.path.insert(0, str(HERE))
    import editor_lock
    return editor_lock.probe(ROOT)['state'] != 'free'


def start_editor():
    """Starts (or reuses) a batch-mode editor on this checkout and opens GreyboxCorridor, as gamefilm.py's up()."""
    if run('return 1;', 20) != 1:
        log = ROOT / 'Logs' / 'timelapse-editor.log'
        log.parent.mkdir(parents=True, exist_ok=True)
        subprocess.Popen([str(EDITOR), '-batchmode', '-projectPath', str(ROOT), '-logFile', str(log)], creationflags=0x00000008)
        for _ in range(180):
            time.sleep(10)
            if run('return 1;', 20) == 1:
                break
        else:
            print('timelapse: the editor did not answer in 30 minutes')
            return False
    opened = run(f'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("{SCENE}"); return "opened";', 120)
    if opened != 'opened':
        print(f'timelapse: could not open {SCENE}: {opened}')
        return False
    return True


def stop_editor():
    run('UnityEditor.EditorApplication.isPlaying = false; return "left play";', 60)
    time.sleep(5)
    run('UnityEditor.EditorApplication.Exit(0); return "closing";', 30)


def wait_report(path, timeout_s=BENCH_TIMEOUT_S, poll=0.5):
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        if path.exists():
            try:
                return json.loads(path.read_text(encoding='utf-8'))
            except ValueError:
                pass
        time.sleep(poll)
    return None


def shoot(sha=None, raw_out=None):
    c = commit_info(sha or f'origin/{INTEGRATION}')
    if c is None:
        print(f'timelapse: no such commit: {sha}')
        return 1
    if editor_held():
        print('timelapse: the project is held (another editor, or a gate): see python Tools/editor_lock.py status')
        return 1
    folder = out_dir()
    folder.mkdir(parents=True, exist_ok=True)
    png, sidecar_json = frame_paths(folder, c)
    raw = Path(raw_out) if raw_out else folder / f'.{c["sha12"]}.bench.json'
    if not start_editor():
        return 1
    args = f'{BENCH_ARGS} shot={png.as_posix()} out={raw.as_posix()}'
    armed = run(f'return TW.Editor.CaptureRig.Bench(@"{args}");', 30)
    if not str(armed).startswith('bench armed'):
        print(f'timelapse: the bench did not arm: {armed}')
        stop_editor()
        return 1
    report = wait_report(raw)
    stop_editor()
    if report is None:
        print(f'timelapse: no report after {BENCH_TIMEOUT_S // 60} minutes: {raw}')
        return 1
    warnings = report.get('warnings', [])
    if not png.exists() or any('never reached inside the window' in w for w in warnings):
        print(f'timelapse: the still was never taken: {warnings}')
        return 1
    scenario = report.get('scenario', {})
    window = report.get('window', {})
    sidecar_json.write_text(json.dumps(dict(
        sha=c['sha'], date=c['date'], subject=c['subject'],
        hash_start=window.get('hash_start'), seed=scenario.get('seed'), battlefield_seed=scenario.get('battlefield_seed'),
        shot_tick=window.get('tick_start'), args=args, host=os.environ.get('COMPUTERNAME', ''),
    ), indent=2), encoding='utf-8')
    raw.unlink(missing_ok=True)
    print(f'timelapse: {png}')
    return 0


def cmd_list(_args):
    folder = out_dir()
    for c in landings():
        s = sidecar(folder, c)
        if s:
            print(f"{c['date']} {c['sha12']} {c['subject']}  hash_start={s.get('hash_start')}")
    return 0


def cmd_missing(args):
    folder = out_dir()
    owed = [c for c in landings() if sidecar(folder, c) is None]
    if args.limit:
        owed = owed[:args.limit]
    for c in owed:
        print(f"{c['date']} {c['sha12']} {c['subject']}")
    if owed:
        print(f'{len(owed)} landing(s) with no frame')
        return 1
    print('every landing has a frame')
    return 0


def cmd_shoot(args):
    return shoot(args.sha)


def cmd_film(_args):
    folder = out_dir()
    frames = []
    for c in landings():
        s = sidecar(folder, c)
        if s is None:
            continue
        png, _ = frame_paths(folder, c)
        if png.exists():
            frames.append((c, s, png))
    if not frames:
        print('timelapse: no frames to film')
        return 1
    newest = frames[-1][1]
    key = lambda s: (s.get('args', '').split('ground=')[-1].split()[0] if 'ground=' in s.get('args', '') else None,
                      s.get('shot_tick'))
    newest_key = key(newest)
    kept, left_out = [], []
    for c, s, png in frames:
        (kept if key(s) == newest_key else left_out).append((c, png))
    if left_out:
        print('timelapse: left out (not the newest view): ' + ', '.join(c['sha12'] for c, _ in left_out))
    if not FFMPEG:
        print('timelapse: no ffmpeg on this machine; frames are kept, no mp4 made')
        return 0
    try:
        from PIL import Image, ImageDraw, ImageFont
    except ImportError:
        print('timelapse: no PIL on this machine; frames are kept, no mp4 made')
        return 0
    stage = folder / '_film'
    shutil.rmtree(stage, ignore_errors=True)
    stage.mkdir(parents=True)
    try:
        big = ImageFont.truetype('segoeuib.ttf', 20)
    except OSError:
        big = ImageFont.load_default()
    for i, (c, png) in enumerate(kept):
        img = Image.open(png).convert('RGB')
        d = ImageDraw.Draw(img)
        d.text((16, img.height - 34), f"{c['date']}  {c['sha12']}  {c['subject']}", font=big, fill=(240, 236, 224))
        img.save(stage / f'{i:05d}.png')
    mp4 = folder / 'timelapse.mp4'
    r = subprocess.run([FFMPEG, '-y', '-loglevel', 'error', '-framerate', str(1 / 0.6), '-i', str(stage / '%05d.png'),
                       '-an', '-c:v', 'libx264', '-crf', '24', '-pix_fmt', 'yuv420p', str(mp4)], capture_output=True, text=True)
    shutil.rmtree(stage, ignore_errors=True)
    if r.returncode != 0:
        print(f'timelapse: ffmpeg: {r.stderr[-300:]}')
        return 1
    print(f'{mp4}  {mp4.stat().st_size // 1024} KB')
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description="the game's time-lapse: one frame for every landing")
    sub = ap.add_subparsers(dest='cmd', required=True)
    p = sub.add_parser('shoot'); p.add_argument('--sha', default=None)
    sub.add_parser('list')
    m = sub.add_parser('missing'); m.add_argument('--limit', type=int, default=None)
    sub.add_parser('film')
    a = ap.parse_args(argv)
    return dict(shoot=cmd_shoot, list=cmd_list, missing=cmd_missing, film=cmd_film)[a.cmd](a)


if __name__ == '__main__':
    sys.exit(main())
