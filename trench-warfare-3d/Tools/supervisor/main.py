#!/usr/bin/env python3
"""Run the supervisor's modules (Tools/supervisor/modules/), each on its own turn and switch.

    python Tools/supervisor/main.py              list the modules, their switch and when each is due; runs nothing
    python Tools/supervisor/main.py --once       give every due module its turn, once
    python Tools/supervisor/main.py --watch 60   keep going, a look every 60 s; the file supervisor.stop in the
                                                 home makes it stand by; one --watch a home (lock.json,
                                                 beat.json: Tools/supervisor/ops/start-supervisor.ps1 starts it)
    python Tools/supervisor/main.py --only NAME  with --once: that module only, due or not

The switches, the state files and the log (supervisor.log) live in the home (Tools/supervisor/core.py).
"""
import argparse
import json
import os
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import core  # noqa: E402


def once(h, only=None, now=None):
    now = now or time.time()        # one clock a pass: the loop_* modules of one pass share one World (looprun.py)
    mods, broken = core.discover()
    out = [f'broken module {b}' for b in broken]
    for m in mods:
        if only and m.NAME != only:
            continue
        if only or core.due(m, h, now):
            out += [f'{m.NAME}: {l}' for l in core.turn(m, h, now)]
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description="the supervisor's modules")
    ap.add_argument('--once', action='store_true')
    ap.add_argument('--watch', type=int, default=0)
    ap.add_argument('--only')
    a = ap.parse_args(argv)
    h = core.home()
    if not (a.once or a.watch):
        mods, broken = core.discover()
        for m in mods:
            print(f'{m.NAME:16} {core.mode(m, h):4} every {getattr(m, "EVERY_S", 300)} s'
                  f'{"  due" if core.due(m, h) else ""}')
        for b in broken:
            print('broken', b)
        return 0
    if a.watch:
        why = take_lock(h)
        if why:
            print(why, flush=True)
            return 0
    try:
        while True:
            if (h / 'supervisor.stop').exists():
                print('supervisor.stop is in its folder: standing by', flush=True)
            else:
                for l in once(h, a.only):
                    print(l, flush=True)
            if not a.watch:
                return 0
            core.write_json(h / 'beat.json', dict(at=time.time(), pid=os.getpid()))
            if (core.read_json(h / 'lock.json', {}) or {}).get('pid') != os.getpid():
                print('another supervisor has the lock: this one leaves', flush=True)
                return 0
            time.sleep(a.watch)
    finally:
        if a.watch and (core.read_json(h / 'lock.json', {}) or {}).get('pid') == os.getpid():
            try:
                os.remove(h / 'lock.json')
            except OSError:
                pass


STALE = 900     # a lock whose supervisor has not beaten for this long is taken over


def take_lock(h, now=None):
    """One --watch supervisor a home (as autoloop.take_lock): lock.json is made exclusively; one whose supervisor has
    not beaten for STALE is taken over. '' when taken, else why not."""
    now = now or time.time()
    h.mkdir(parents=True, exist_ok=True)
    f = h / 'lock.json'
    for _ in range(2):
        try:
            fd = os.open(str(f), os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            with os.fdopen(fd, 'w', encoding='utf-8') as out:
                out.write(json.dumps(dict(pid=os.getpid(), at=now)))
            return ''
        except FileExistsError:
            beat = max((core.read_json(h / 'beat.json', {}) or {}).get('at', 0), (core.read_json(f, {}) or {}).get('at', 0))
            if now - beat < STALE:
                return f'another supervisor holds {f} (pid {(core.read_json(f, {}) or {}).get("pid")}, last beat {int(now - beat)} s ago)'
            try:
                os.remove(f)
            except OSError:
                return f'{f} is stale and could not be removed'
    return f'{f} could not be taken'


if __name__ == '__main__':
    sys.exit(main())
