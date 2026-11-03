#!/usr/bin/env python3
"""One lock on this machine for the gates the supervisor starts (the canary, the landing stage): one at a time.

WHY. On 2026-11-02 several gates ran at once in this repo: a selftest's scratch worktree was pruned by another, and
test_relay took 93 minutes instead of about 20. Two Unity gates on one machine slow each other and make false reds.
So a supervisor module that starts a gate (or what a landing runs, land.py's tool check) takes this lock first,
and a second one waits its turn.

    ok, rec = gatelock.acquire('landing', pid=gate_pid)   # (True, the lock) or (False, who holds it)
    gatelock.handover('landing', pid=session_pid)         # same holder, a new process carries it on
    gatelock.release('landing')

The lock is a file, <supervisor home>/gate.lock: {who, pid, since, host}. It is made with O_EXCL, so two takers
can not both win. It is stale, and the next taker takes it over, when its process is gone or it is older than
STALE_S (3 hours: a full gate takes 25 to 95 minutes). Gates a person starts by hand do not take it.
"""
import json
import os
import socket
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import core  # noqa: E402

STALE_S = 3 * 3600


def lock_path(p=None):
    return Path(p) if p else core.home() / 'gate.lock'


def pid_alive(pid):
    """True while the process runs. Windows: OpenProcess and its exit code (os.kill(pid, 0) would end it there)."""
    try:
        pid = int(pid)
    except (TypeError, ValueError):
        return False
    if pid <= 0:
        return False
    if os.name == 'nt':
        import ctypes
        k = ctypes.windll.kernel32
        h = k.OpenProcess(0x1000, False, pid)            # PROCESS_QUERY_LIMITED_INFORMATION
        if not h:
            return False
        try:
            code = ctypes.c_ulong()
            ok = k.GetExitCodeProcess(h, ctypes.byref(code))
            return bool(ok) and code.value == 259         # STILL_ACTIVE
        finally:
            k.CloseHandle(h)
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def read(p=None):
    try:
        with open(lock_path(p), encoding='utf-8') as f:
            rec = json.load(f)
        return rec if isinstance(rec, dict) else {}
    except (OSError, ValueError):
        return None


def stale(rec, now=None):
    """Why this lock no longer holds ('' while it does)."""
    if not rec or not rec.get('who'):
        return 'it does not read'
    if not pid_alive(rec.get('pid')):
        return f'its process {rec.get("pid")} is gone'
    age = (now or time.time()) - float(rec.get('since') or 0)
    if age > STALE_S:
        return f'it is {int(age // 60)} minutes old'
    return ''


def _create(p, rec):
    fd = os.open(str(p), os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    with os.fdopen(fd, 'w', encoding='utf-8') as f:
        json.dump(rec, f)


def acquire(who, pid=None, p=None, now=None):
    """(True, the lock) when `who` holds it now, else (False, the holder's record). A stale lock is taken over."""
    p = lock_path(p)
    p.parent.mkdir(parents=True, exist_ok=True)
    rec = dict(who=str(who), pid=int(pid or os.getpid()), since=float(now or time.time()), host=socket.gethostname())
    for _ in range(2):
        try:
            _create(p, rec)
            return True, rec
        except FileExistsError:
            held = read(p)
            if held is None and p.exists():
                time.sleep(0.05)                          # a taker is writing it this moment: read once more
                held = read(p)
            if held is not None and not stale(held, now):
                return False, held
            # stale: move it aside under our own name, so of two takers only one removes it
            aside = p.with_name(f'{p.name}.stale-{os.getpid()}')
            try:
                os.replace(p, aside)
                os.remove(aside)
            except OSError:
                pass
    held = read(p)
    return False, held or {}


def handover(who, pid, p=None, now=None):
    """The holder hands the lock to a new process of its own (the gate ended, the landing session starts). False
    when `who` does not hold it."""
    p = lock_path(p)
    rec = read(p)
    if not rec or rec.get('who') != who:
        return False
    rec.update(pid=int(pid), since=float(now or time.time()))
    core.write_json(p, rec)
    return True


def release(who, p=None):
    """Free the lock when `who` holds it. Returns True when it was freed (or was not there)."""
    p = lock_path(p)
    rec = read(p)
    if rec is None:
        return not p.exists()
    if rec.get('who') != who:
        return False
    try:
        os.remove(p)
    except OSError:
        return False
    return True
