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

WHY a pid alone is not enough. Windows reuses process ids: once the gate's process has died, its pid can be
reassigned to an unrelated program. A liveness check on the pid alone then reports the old gate as still running
forever. A pid only counts as our gate when it is alive *and* matches what we started: its start time is close to
the stamp written beside the pid (one minute either way, local time), or its command line is PowerShell running
gate.ps1. When neither can be read (no CIM/wmic, odd permissions), we say so and fall back to the old pid-only
answer rather than guess a dead gate away.
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


def read_pidfile(pidfile):
    """(pid, stamp_epoch): stamp is None when missing or unparsable, not an error -- old pidfiles still work."""
    try:
        parts = pidfile.read_text().split()
        pid = int(parts[0])
    except (OSError, ValueError, IndexError):
        return None, None
    stamp = None
    if len(parts) > 1:
        try:
            stamp = time.mktime(time.strptime(parts[1], '%Y-%m-%dT%H:%M:%S'))
        except ValueError:
            stamp = None
    return pid, stamp


def probe(pid):
    """(start_epoch, cmdline), either half None when it could not be read."""
    if os.name == 'nt':
        script = (f"$p = Get-CimInstance Win32_Process -Filter 'ProcessId={pid}'; "
                  "if ($p) { Write-Output ('start=' + ([datetime]$p.CreationDate).ToString('yyyy-MM-ddTHH:mm:ss')); "
                  "Write-Output ('cmd=' + $p.CommandLine) }")
        try:
            out = subprocess.run(['powershell', '-NoProfile', '-NonInteractive', '-Command', script],
                                  capture_output=True, text=True, timeout=30)
        except (OSError, subprocess.TimeoutExpired):
            return None, None
        if out.returncode != 0:
            return None, None
        start, cmd = None, None
        for line in out.stdout.splitlines():
            if line.startswith('start='):
                try:
                    start = time.mktime(time.strptime(line[len('start='):], '%Y-%m-%dT%H:%M:%S'))
                except ValueError:
                    start = None
            elif line.startswith('cmd='):
                cmd = line[len('cmd='):].strip() or None
        return start, cmd
    try:
        with open(f'/proc/{pid}/stat') as f:
            fields = f.read().rsplit(')', 1)[1].split()
        start_ticks = int(fields[19])  # field 22 overall, 20th after the comm field
        hertz = os.sysconf('SC_CLK_TCK')
        with open('/proc/stat') as f:
            btime = next(int(l.split()[1]) for l in f if l.startswith('btime'))
        start = btime + start_ticks / hertz
    except (OSError, IndexError, ValueError, StopIteration):
        start = None
    try:
        with open(f'/proc/{pid}/cmdline', 'rb') as f:
            cmd = f.read().replace(b'\0', b' ').decode(errors='replace').strip() or None
    except OSError:
        cmd = None
    return start, cmd


def is_gate(pid, stamp):
    """Whether pid is alive and matches the gate we started (by start time or command line). (bool, reason)."""
    if not alive(pid):
        return False, 'not running'
    start, cmdline = probe(pid)
    evidence = []
    if start is not None and stamp is not None:
        evidence.append(abs(start - stamp) <= 60)
    if cmdline is not None:
        evidence.append('gate.ps1' in cmdline.lower())
    if not evidence:
        return True, f"could not read pid {pid}'s start time or command line: trusting the pid alone"
    if any(evidence):
        return True, f'pid {pid} is the gate we started'
    return False, f'pid {pid} is alive but is not the gate we started (started {start}; cmd {cmdline})'


def verdict(log):
    """The gate's own last words: 'Gate green. Tested tree ...', or the last line it printed."""
    try:
        lines = [l for l in log.read_text(encoding='utf-8', errors='replace').splitlines() if l.strip()]
    except OSError:
        return 'no log'
    # the wrapper's 'gate exit N' comes after the gate's own verdict: green is read first, wherever it stands
    for l in reversed(lines):
        if l.startswith('Gate green'):
            return l
    for l in reversed(lines):
        if l.startswith('gate exit '):
            return l
    return lines[-1] if lines else 'empty log'


def status(log, pidfile):
    pid, stamp = read_pidfile(pidfile)
    if pid is None:
        print('no gate was started here with gate_bg.py')
        return 1
    gate, reason = is_gate(pid, stamp)
    if gate:
        print(f'running (pid {pid}); log {log} -- {reason}')
        return 2
    print(reason)
    v = verdict(log)
    print(v if v.startswith('Gate green') else f'ended without a green verdict: {v}  (log {log})')
    return 0 if v.startswith('Gate green') else 8


def main():
    log, pidfile = git_path('tw-gate.log'), git_path('tw-gate.pid')
    if '--status' in sys.argv:
        sys.exit(status(log, pidfile))
    if '--wait' in sys.argv:
        pid, stamp = read_pidfile(pidfile)
        while pid is not None and is_gate(pid, stamp)[0]:
            time.sleep(15)
        sys.exit(status(log, pidfile))
    pid, stamp = read_pidfile(pidfile)
    if pid is not None and is_gate(pid, stamp)[0]:
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
