#!/usr/bin/env python3
"""Run the full gate so that it outlives the session that started it.

WHY. The full gate takes 25 minutes. Started as a background job of an agent session it dies with that session: on
2026-10-07 a gate died in its PlayMode part when its session ended, with no verdict and a half hour lost. This starts
gate.ps1 as a process of its own (no window, its own process group, outside the caller's job) and writes everything
it prints to tw-gate.log in this checkout's git dir, beside the tw-gate-green marker the gate itself writes.

    python Tools/gate_bg.py            from trench-warfare-3d/: start the full gate, print the log's path, return
    python Tools/gate_bg.py --status   what the last run started here says: running, green, or how it ended
    python Tools/gate_bg.py --wait     the same, after waiting for the run to end (its exit code: 0 green)

Only the full gate (no arguments to gate.ps1): a run before a commit is short and wants no detaching. One run per
checkout: a second start while the first is alive is refused, because two gates on one project lock each other out.
"""
import os
import pathlib
import subprocess
import sys
import time

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parent.parent


def git_path(name):
    out = subprocess.run(['git', 'rev-parse', '--path-format=absolute', '--git-path', name], cwd=REPO,
                         capture_output=True, text=True).stdout.strip()
    return pathlib.Path(out)


def alive(pid):
    if os.name != 'nt':
        try:
            os.kill(pid, 0)
            return True
        except OSError:
            return False
    out = subprocess.run(['tasklist', '/FI', f'PID eq {pid}', '/NH'], capture_output=True, text=True).stdout
    return str(pid) in out


def read_pid(pidfile):
    try:
        return int(pidfile.read_text().split()[0])
    except (OSError, ValueError, IndexError):
        return None


def verdict(log):
    """The gate's own last words: 'Gate green. Tested tree ...', or the last line it printed."""
    try:
        lines = [l for l in log.read_text(encoding='utf-8', errors='replace').splitlines() if l.strip()]
    except OSError:
        return 'no log'
    for l in reversed(lines):
        if l.startswith('Gate green') or l.startswith('gate exit '):
            return l
    return lines[-1] if lines else 'empty log'


def status(log, pidfile):
    pid = read_pid(pidfile)
    if pid is None:
        print('no gate was started here with gate_bg.py')
        return 1
    if alive(pid):
        print(f'running (pid {pid}); log {log}')
        return 2
    v = verdict(log)
    print(v if v.startswith('Gate green') else f'ended without a green verdict: {v}  (log {log})')
    return 0 if v.startswith('Gate green') else 8


def main():
    log, pidfile = git_path('tw-gate.log'), git_path('tw-gate.pid')
    if '--status' in sys.argv:
        sys.exit(status(log, pidfile))
    if '--wait' in sys.argv:
        pid = read_pid(pidfile)
        while pid is not None and alive(pid):
            time.sleep(15)
        sys.exit(status(log, pidfile))
    pid = read_pid(pidfile)
    if pid is not None and alive(pid):
        sys.exit(f'a gate started here is still running (pid {pid}); log {log}')
    gate = REPO / 'gate.ps1'
    # the wrapper adds the exit code as the log's last line, so a run that ended red is told from one that was killed
    cmd = ['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-Command',
           f"& '{gate}' *>&1 | Out-File -FilePath '{log}' -Encoding utf8; "
           f"Add-Content -Path '{log}' -Value \"gate exit $LASTEXITCODE\" -Encoding utf8"]
    flags = 0
    if os.name == 'nt':
        # CREATE_NO_WINDOW, CREATE_NEW_PROCESS_GROUP, CREATE_BREAKAWAY_FROM_JOB. Not DETACHED_PROCESS: with no console
        # at all PowerShell exits 0 having run nothing (tried 2026-10-07)
        flags = 0x08000000 | 0x00000200 | 0x01000000
    try:
        p = subprocess.Popen(cmd, cwd=REPO, creationflags=flags, stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, close_fds=True)
    except OSError:
        # a job that forbids breaking away: start it inside the job; it still has no console to lose
        p = subprocess.Popen(cmd, cwd=REPO, creationflags=flags & ~0x01000000, stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, close_fds=True)
    pidfile.write_text(f'{p.pid} {time.strftime("%Y-%m-%dT%H:%M:%S")}\n')
    print(f'gate started (pid {p.pid}); log {log}')
    print('python Tools/gate_bg.py --wait   waits for it and prints the verdict')


if __name__ == '__main__':
    main()
