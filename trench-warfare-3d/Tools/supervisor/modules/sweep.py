"""When the breaker is open on a Unity fault, end the census's strays by PID and ask the canary for a fresh gate.

Acts only when ALL hold: the breaker's own switch is on (a dry breaker still writes open: true, and holds nothing)
and its state (state/breaker.json) is open with kind licence or unity-wont-start; the census
(state/unity_census.json) is at most CENSUS_FRESH_S old; and the row still passes the census's rules here, checked
again with the legs running NOW (a leg may have started since the census). It never ends anything on
empire-og-client, an editor window in session 1, or a Unity on a checkout a leg runs in. In dry (the default; a week
before on, the plan says) it logs each PID it would end. In on it runs `taskkill /PID n /T /F` for each, then writes
canary_request into its own state; canary.py reads it with ctx.other('sweep') and starts a new gate. /T also ends
the stray's child processes, its Unity.Licensing.Client.exe among them: that client is what held the pipe on 10-31.
Whether a live editor that shared that client gets a new one is not yet seen on this machine: watch the first
week the sweep is on.

WHY. The stray Unity of 10-27 (footage project, session 0) held the licence pipe until a person killed it on 11-02
23:25. The breaker stops the bleeding; this ends the cause, by PID only (never by a name pattern: a pattern kill
also ends the shell that holds the pattern).
"""
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import core  # noqa: E402
import relays  # noqa: E402

NAME = 'sweep'
EVERY_S = 300
DEFAULT_MODE = 'dry'
KINDS = ('licence', 'unity-wont-start')
CENSUS_FRESH_S = 900
KEEP = ('empire-og-client',)


def safe_to_end(row, live):
    """The census's verdict, checked again: a stray, not kept, not a window in session 1, not on a live leg."""
    project = row.get('project') or ''
    if not row.get('stray') or any(k in project or k in str(row.get('why')) for k in KEEP):
        return False
    if not row.get('batch') and not row.get('status_call') and row.get('session') == 1:
        return False
    if (row.get('name') or '').lower() != 'unity.exe':
        return False
    return not any(project and (project == w or project.startswith(w + '/')) for w in live)


def decide(breaker, census, now, live, breaker_on):
    """[(pid, started, why)] to end now. Pure."""
    breaker, census = breaker or {}, census or {}
    if not breaker_on or not breaker.get('open') or breaker.get('kind') not in KINDS:   # a dry breaker holds nothing
        return []
    if now - (census.get('at') or 0) > CENSUS_FRESH_S:
        return []
    return [(r['pid'], r.get('started'), r.get('why', '')) for r in census.get('census') or [] if safe_to_end(r, live)]


def same_process(pid, started):
    """True when pid is still a Unity.exe that began at `started` (a minute either way): Windows reuses PIDs."""
    ask = (f"$p = Get-CimInstance Win32_Process -Filter 'ProcessId={int(pid)}'; if ($p) {{ '{{0}}|{{1}}' -f $p.Name, "
           "([DateTimeOffset]$p.CreationDate).ToUnixTimeSeconds() }")
    r = subprocess.run(['powershell', '-NoProfile', '-Command', ask], capture_output=True, text=True, errors='replace',
                       stdin=subprocess.DEVNULL, timeout=60, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    name, _, began = (r.stdout or '').strip().partition('|')
    return name.lower() == 'unity.exe' and began.isdigit() and abs(int(began) - int(started or 0)) <= 60


def end(pid):
    r = subprocess.run(['taskkill', '/PID', str(int(pid)), '/T', '/F'], capture_output=True, text=True,
                       errors='replace', stdin=subprocess.DEVNULL, timeout=60,
                       creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    return r.returncode, (r.stdout + r.stderr).strip()


def run(ctx):
    breaker, census = ctx.other('breaker'), ctx.other('unity_census')
    on = (core.read_json(ctx.home / 'modules.json', {}) or {}).get('breaker', 'dry') == 'on'
    todo = decide(breaker, census, ctx.now, relays.running_worktrees(ctx.now), on)
    st = ctx.state() or {}
    if not todo:
        return []
    if ctx.dry:
        return [f'would end Unity pid {pid} ({why}); breaker {breaker.get("kind")}' for pid, _, why in todo]
    lines, ended = [], []
    for pid, started, why in todo:
        if not same_process(pid, started):
            lines.append(f'pid {pid} is gone or is another process now: left alone')
            continue
        code, said = end(pid)
        lines.append(f'ended pid {pid} ({why})' if code == 0 else f'pid {pid} did not end: {said[:160]}')
        if code == 0:
            ended.append(pid)
    st.update(last={'at': ctx.now, 'ended': ended, 'kind': breaker.get('kind')})
    if ended:
        st['canary_request'] = {'at': ctx.now, 'why': f'ended {len(ended)} stray Unity'}
        lines.append('asked the canary for a fresh gate')
    ctx.save(st)
    return lines
