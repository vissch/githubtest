#!/usr/bin/env python3
"""Move a unit of the relay's queue from the Priorities page (the owner, 2026-10-21: "a button there which says move up
or move down ... up means on the top, down means on the bottom"). Until then the page was read-only and he told the master.

The page sends /priority/move {id, to} to the listener ops.py --watch keeps (notes.py). `to` is the number the page worked
out from the rows it shows: one under the lowest of the others for the top, one over the highest for the bottom (0 to 99,
the lower runs first). This runs what the master runs, `relay.py prio <id> <n>`, on the desktop over ssh: it writes the unit's
priority and commits and pushes the pipeline board, which only the desktop does (this laptop's board checkout is not pushed).
The relay still takes work by its own rules (a share a kind of work, a wide turn): a unit on top is the first of its kind,
not always the next to run.
"""
import re
import subprocess

DESKTOP = 'emtd-desktop'                  # the ssh host of the desktop (the owner's ~/.ssh/config)
RELAY = 'C:/Users/PC/Documents/GitHub/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py'
ROUTES = ('/priority/move',)
UNIT = re.compile(r'^[A-Za-z0-9][A-Za-z0-9._-]{0,90}$')       # a unit id is a file name on the board: nothing the remote shell could read as more


def ssh(cmd, timeout=90):
    """One command on the desktop: (exit code, output). The remote shell is Windows PowerShell 5.1."""
    try:
        p = subprocess.run(['ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=10', DESKTOP, cmd], capture_output=True, text=True, timeout=timeout)
        return p.returncode, (p.stdout + p.stderr).strip()
    except (OSError, subprocess.SubprocessError) as e:
        return 255, f'{type(e).__name__}: {e}'


def move(uid, to, run=ssh):
    """Set one queued unit's priority on the board, on the desktop. Returns what the relay said. Raises ValueError for what is refused."""
    if not isinstance(uid, str) or not UNIT.match(uid):
        raise ValueError('that is not a unit id')
    if isinstance(to, bool) or not isinstance(to, int) or not 0 <= to <= 99:
        raise ValueError('the priority is a whole number from 0 to 99')
    code, out = run(f'python {RELAY} prio {uid} {to}')
    lines = [x.strip() for x in out.splitlines() if x.strip()]
    if code != 0:
        raise ValueError('the desktop did not move it: ' + (lines[-1] if lines else f'exit {code}')[:300])
    return dict(unit=uid, priority=to, said=lines[-1][:300] if lines else '')


def post(path, d, run=ssh):
    if path == '/priority/move':
        return move(d.get('id'), d.get('to'), run)
    raise ValueError('nothing here')
