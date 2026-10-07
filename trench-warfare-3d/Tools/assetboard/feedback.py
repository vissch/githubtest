#!/usr/bin/env python3
"""The feedback captures from the game: taken in, kept where both stations see them, listed as tasks.

    python Tools/assetboard/feedback.py                 from trench-warfare-3d/: the captures, newest first
    python Tools/assetboard/feedback.py show ID         one capture: his words, where its picture and state file are

WHY (the owner, 2026-10-07: "inside the game i want a button i can press f10 that will pause the game, take a
screenshot and save the variables and metadata of the game so i can give feedback. it should connect to the tasks
board"). The game writes a capture as a folder on the machine it runs on (inbox(): capture.json, the state, and
shot.png, the screen with the HUD; UI/Shell/FeedbackCapture.cs). While his box for words is open the folder also
holds a file named writing, and the capture waits: his words reach the state file when the box closes. A machine's
own folder is seen by nobody else, so
each read of the board moves the finished captures into the tasks' folder (store(): the Drive both stations read),
and there a capture is a task from its first minute (src_tasks.py).

take_in() copies, compares every byte, and only then removes the game's copy. It also writes down the checkout the
game ran from as it is at that moment (board.json): its branch, its commit and how many files were changed. The game
says its own branch and commit at the press (build.branch, build.commit), and those are what a row shows: the
checkout may have moved on by the time the board takes the capture in, and the row says so when it did.
A folder the game left that never becomes a capture (no state file half an hour on, or more bytes than a capture
is) is not skipped in silence: broken() lists it and the board shows it as an F10 that failed.
The format is feedback.example.json, which the game's tests and these tests both read.
"""
import argparse
import datetime
import json
import os
import shutil
import socket
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import src_tasks  # noqa: E402

SCHEMA = 'tw-feedback/1'
SETTLE = 5              # seconds a capture's files must have been left alone before it is taken in
NO_SHOT = 30            # ... and how long a capture with no picture waits for one (a game with no screen writes none)
MOST = 64 * 2 ** 20     # bytes of one capture: more is not a capture
OPEN = 'writing'        # a file the game leaves in a capture while his box for words is open: his words are not in it yet
OPEN_FOR = 30 * 60      # ... and one older than this is from a game that ended with the box open: what there is, is taken


def inbox():
    """Where the game writes: TW_FEEDBACK, else %LOCALAPPDATA%\\TrenchWarfare\\feedback (the same two in the game)."""
    if os.environ.get('TW_FEEDBACK'):
        return Path(os.environ['TW_FEEDBACK'])
    return Path(os.environ.get('LOCALAPPDATA', str(Path.home()))) / 'TrenchWarfare' / 'feedback'


def store():
    return src_tasks.root() / 'feedback'


def read(folder: Path):
    try:
        c = json.loads((folder / 'capture.json').read_text(encoding='utf-8-sig'))
    except (OSError, ValueError):
        return None
    return c if isinstance(c, dict) and c.get('schema') == SCHEMA and c.get('id') else None


def finished(folder: Path, now):
    """Whether the game is done with a capture: its state file reads, he is not still writing his words (OPEN), and
    its picture is there and has been left alone, or no picture came in NO_SHOT seconds."""
    if read(folder) is None:
        return False
    try:
        if (folder / OPEN).exists() and now - (folder / OPEN).stat().st_mtime < OPEN_FOR:
            return False
        wrote = (folder / 'capture.json').stat().st_mtime
        shot = folder / 'shot.png'
        if shot.exists():
            return shot.stat().st_size > 0 and now - max(wrote, shot.stat().st_mtime) >= SETTLE
        return now - wrote >= NO_SHOT
    except OSError:
        return False


def broken(src: Path = None, now=None):
    """The folders in the game's folder that are no capture and will not become one: no state file that reads after
    OPEN_FOR seconds, or more than MOST bytes. [{name, folder, why, at}]."""
    src, now, out = src or inbox(), now or time.time(), []
    for folder in sorted(p for p in src.iterdir() if p.is_dir()) if src.is_dir() else []:
        try:
            at = folder.stat().st_mtime
            if read(folder) is None:
                if now - at < OPEN_FOR or not any(folder.iterdir()) or (folder / 'capture.json').exists():
                    continue                        # still being written, an empty folder that is nobody's, or another schema's file
                why = 'the game made its folder and wrote no state file'
            elif sum(f.stat().st_size for f in folder.iterdir() if f.is_file()) > MOST:
                why = f'it is larger than {MOST // 2 ** 20} MB, which is no capture'
            else:
                continue
        except OSError:
            continue
        out.append(dict(name=folder.name, folder=str(folder), why=why, at=at))
    return out


def waiting(src: Path = None, now=None):
    """How many finished captures wait in the game's folder (what the page says while the Drive is away)."""
    src, now = src or inbox(), now or time.time()
    return sum(1 for p in src.iterdir() if p.is_dir() and finished(p, now)) if src.is_dir() else 0


def checkout_of(project):
    """The branch and commit of the checkout the game ran from, as they are now: {} when it is no checkout."""
    if not project or not Path(project).is_dir():
        return {}

    def git(*a):
        p = subprocess.run(['git', '-C', str(project), *a], capture_output=True)
        return p.stdout.decode('utf-8', 'replace').strip() if p.returncode == 0 else ''
    head = git('rev-parse', 'HEAD')
    if not head:
        return {}
    return dict(branch=git('rev-parse', '--abbrev-ref', 'HEAD'), commit=head[:8], dirty=len([l for l in git('status', '--porcelain').splitlines() if l.strip()]))


def take_in(src: Path = None, dst: Path = None, host=None, now=None, checkout=checkout_of):
    """Move every finished capture from the game's folder into the tasks' folder, as <id>-<host>. A capture is
    copied, every file compared with its copy, and only then removed from the game's folder: one that does not
    compare stays where it is and is tried again on the next read. Returns the ids taken in."""
    src, dst, host, now = src or inbox(), dst or store(), host or socket.gethostname(), now or time.time()
    took = []
    for folder in sorted(p for p in src.iterdir() if p.is_dir()) if src.is_dir() else []:
        if not finished(folder, now):
            continue
        files = [f for f in folder.iterdir() if f.is_file() and not f.name.endswith('.tmp') and f.name != OPEN]
        if sum(f.stat().st_size for f in files) > MOST:
            continue
        c = read(folder)
        to = dst / f'{folder.name}-{host}'
        try:
            to.mkdir(parents=True, exist_ok=True)
            for f in files:
                shutil.copyfile(f, to / f.name)
            if any((to / f.name).read_bytes() != f.read_bytes() for f in files):
                continue
            meta = dict(host=host, taken_at=f'{datetime.datetime.fromtimestamp(now):%Y-%m-%d %H:%M:%S}', project=(c.get('build') or {}).get('project', ''))
            meta.update(checkout((c.get('build') or {}).get('project', '')) if (c.get('build') or {}).get('editor') else {})
            (to / 'board.json').write_text(json.dumps(meta, indent=1, sort_keys=True) + '\n', encoding='utf-8')
            for f in files:
                f.unlink()
            for f in folder.iterdir():          # a .tmp or a stale marker the game left behind
                f.unlink()
            folder.rmdir()
        except OSError:
            continue
        took.append(to.name)
    return took


def clock(seconds):
    s = max(0, int(seconds or 0))
    return f'{s // 60:02d}:{s % 60:02d}'


def lines(c, meta):
    """A capture in at most five short lines: where and when in the match, what stood on the field, which game."""
    m = (c.get('match') or {}) if c.get('in_match') else {}        # the game writes an empty match on a menu: in_match says which
    out = []
    if m:
        req, rep = m.get('request') or {}, m.get('report') or {}
        out.append(f'In {req.get("Title") or c.get("scene", "the battle")}, {clock(rep.get("DurationSeconds"))} into the match (tick {m.get("tick", 0)}), match seed {req.get("MatchSeed", "?")}, ground seed {req.get("BattlefieldSeed", "?")}.')
        alive = [sum(u.get('count', 0) for u in m.get('units') or [] if u.get('team') == t) for t in (0, 1)]
        silver = m.get('silver') or [0, 0]
        out.append(f'On the field: {alive[0]} of ours and {alive[1]} of theirs alive; silver {silver[0]} and {silver[1 if len(silver) > 1 else 0]}.')
        if m.get('holds_before') and m['holds_before'] != 'None':
            out.append(f'The match was already held ({m["holds_before"]}) when you pressed F10.')
        v, picked, errs = c.get('view') or {}, len(str(m.get('selected') or '').split()), c.get('errors') or []
        saw = ([f'the cursor on the ground at {v["cursor_ground"].get("x", 0):.0f}, {v["cursor_ground"].get("z", 0):.0f}'] if v.get('cursor_on_ground') and isinstance(v.get('cursor_ground'), dict) else []) \
            + ([f'{v["armed"]} armed'] if v.get('armed') and v['armed'] != 'None' else []) + ([f'{picked} selected'] if picked else []) \
            + ([f'{len(errs)} error{"" if len(errs) == 1 else "s"} in the console before it, the last: {str(errs[-1])[:120]}'] if errs else [])
        if saw:
            out.append('At the press: ' + '; '.join(saw) + '.')
    else:
        out.append(f'On a menu ({c.get("scene", "")}): no match was running.')
    b = c.get('build') or {}
    if b.get('editor'):
        # the game's own word at the press, when it gave one; the board's later look at the checkout otherwise, and beside it when the two differ
        branch, commit = b.get('branch') or meta.get('branch', '?'), b.get('commit') or meta.get('commit', '?')
        game = f'the editor, {branch} at {commit}' + (f' with {meta["dirty"]} files changed' if meta.get('dirty') else '')
        if b.get('commit') and meta.get('commit') and meta['commit'] != b['commit']:
            game += f' (the checkout had moved to {meta["commit"]} when the board took the capture in)'
    else:
        game = f'a build, version {b.get("version", "?")}' + (f', built from {built_from(b)}' if built_from(b) else '')
    out.append(f'Captured {c.get("when_local", "")} on {meta.get("host", "?")}, in {game}.')
    return out[:1] + out[1:-1][:3] + out[-1:]


def built_from(b):
    """The commit a build was made from, as its build-info.json says (the game copies that file into build_info)."""
    try:
        info = json.loads(b.get('build_info') or '{}')
    except ValueError:
        return ''
    if not isinstance(info, dict):
        return ''
    commit = next((str(info[k]) for k in ('commit', 'sha', 'git', 'revision') if info.get(k)), '')
    return (f'{info["branch"]} at ' if info.get('branch') and commit else '') + commit[:8]


def place(c):
    """Where and when in the game a capture was taken, in a few words: what tells two captures without words apart."""
    if not c.get('in_match'):
        return f'on a menu ({c.get("scene", "")})'
    m = c.get('match') or {}
    return f'{(m.get("request") or {}).get("Title") or c.get("scene", "the battle")}, {clock((m.get("report") or {}).get("DurationSeconds"))} in'


def read_all(where: Path = None):
    """Every capture taken in, newest first: its words, where its files are, and what a task row needs of it."""
    where, out = where or store(), []
    for folder in sorted((p for p in where.iterdir() if p.is_dir()), reverse=True) if where.is_dir() else []:
        c = read(folder)
        if c is None:
            continue
        try:
            meta = json.loads((folder / 'board.json').read_text(encoding='utf-8'))
        except (OSError, ValueError):
            meta = {}
        shot = folder / 'shot.png'
        try:
            at = time.mktime(time.strptime(str(c.get('when_local', ''))[:19], '%Y-%m-%d %H:%M:%S'))
        except ValueError:
            at = (folder / 'capture.json').stat().st_mtime
        note = str(c.get('note') or '').strip()
        ask = (f'{note or "(He left no words: the picture and the state are the feedback.)"}\n\nThe capture: {folder}\n'
               f'- {shot.name if shot.exists() else "no picture"}: the screen as he saw it, HUD included\n- capture.json: the match, the view, the settings and the build at that moment\n'
               + '\n'.join(lines(c, meta)))
        out.append(dict(id=folder.name, note=note, when=str(c.get('when_local', ''))[:16], at=at, host=meta.get('host', ''), scene=c.get('scene', ''), folder=str(folder), place=place(c),
                        shot=str(shot) if shot.exists() else '', lines=lines(c, meta), ask=ask, meta=meta))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='the feedback captures from the game')
    ap.add_argument('verb', nargs='?', default='list', choices=('list', 'show'))
    ap.add_argument('id', nargs='?', default='')
    args = ap.parse_args(argv)
    every = read_all()
    if args.verb == 'show':
        hits = [c for c in every if c['id'] == args.id] or [c for c in every if c['id'].startswith(args.id)]
        if len(hits) != 1:
            print(f'{args.id}: {"no such capture" if not hits else "that is the start of " + str(len(hits)) + " captures"}')
            return 1
        print(hits[0]['ask'])
        return 0
    print(f'{len(every)} capture{"" if len(every) == 1 else "s"} in {store()}')
    for c in every:
        print(f'  {c["id"]}  {c["note"][:90] or "(no words)"}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
