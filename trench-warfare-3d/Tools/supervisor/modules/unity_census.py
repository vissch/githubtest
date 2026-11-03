"""Every Unity on this machine, in every session, and which of them are strays. Reads only; kills nothing (sweep.py
does, by the PIDs this writes).

A row: pid, name, session, started (epoch), age_s, project, batch (has -batchmode), status_call (a `unity status`
call, no -projectPath), ppid, parent, stray, why. A STRAY is older than 3 h AND batch-mode or a `unity status` call AND not on a
relay work checkout where a leg runs right now (Tools/supervisor/relays.py: a leg.json that says RUNNING). Never a
stray: anything on empire-og-client (KEEP), an interactive editor (no -batchmode) in session 1, a
Unity.Licensing.Client.exe (it goes with the Unity that started it: taskkill /T on that one takes it along).

WHY. A batch-mode Unity on githubtest-footage, started through WMI in session 0 on 10-27 18:30, held the machine's
licence pipe for six days; from 10-31 every gate was red. Tools/relay/launch.py sweep_unity only looks at the
checkout of a leg that just ended, and Tools/assetboard/autoloop.py end_editors only at relay checkouts: a Unity on
any other project was nobody's to see. This sees all of them; only the sweep acts, and only when the breaker says
the machine is broken.
"""
import json
import re
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import relays  # noqa: E402

NAME = 'unity_census'
EVERY_S = 300
DEFAULT_MODE = 'dry'
STRAY_AGE_S = 3 * 3600
KEEP = ('empire-og-client',)        # the owner's other game: never flagged, never killed
NAMES = ('unity.exe', 'unity.licensing.client.exe')
PS_LIST = ("$all = @{}; Get-CimInstance Win32_Process | ForEach-Object { $all[[int]$_.ProcessId] = $_ }; "
           "$all.Values | Where-Object { $_.Name -in 'Unity.exe','Unity.Licensing.Client.exe' } | ForEach-Object { "
           "$p = $all[[int]$_.ParentProcessId]; $gone = (-not $p) -or ($p.CreationDate -gt $_.CreationDate); "
           "[pscustomobject]@{pid=[int]$_.ProcessId; ppid=[int]$_.ParentProcessId; name=$_.Name; "
           "session=[int]$_.SessionId; cmd=[string]$_.CommandLine; "
           "started=([DateTimeOffset]$_.CreationDate).ToUnixTimeSeconds(); "
           "parent=$(if ($gone) {''} else {$p.Name})} } | ConvertTo-Json -Compress")


def processes():
    """The raw rows ({pid, ppid, name, session, cmd, started, parent}) from Windows. The one place that asks the OS:
    the tests feed rows of their own to assess()."""
    r = subprocess.run(['powershell', '-NoProfile', '-Command', PS_LIST], capture_output=True, text=True,
                       encoding='utf-8', errors='replace', stdin=subprocess.DEVNULL, timeout=120,
                       creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    if r.returncode != 0:
        raise RuntimeError(f'the process list could not be read: {(r.stderr or "").strip()[:200]}')
    got = json.loads(r.stdout or '[]') if (r.stdout or '').strip() else []
    return [got] if isinstance(got, dict) else got


def project_of(cmd):
    """The -projectPath of a command line, normalised (lower case, forward slashes), or ''."""
    m = re.search(r'-projectpath\s+(?:"([^"]+)"|(\S+))', cmd or '', re.I)
    return relays.norm(m.group(1) or m.group(2)) if m else ''


def is_status_call(cmd):
    """The unity CLI's `unity status`, which has no -projectPath. Not -hubIPC or -useHub: the Hub starts every editor
    window with those."""
    return bool(re.search(r'unity(\.exe)?"?\s+status\b', cmd or '', re.I)) and not project_of(cmd)


def assess(rows, now, live):
    """The census: every row with its age, project and verdict. `live`: the checkouts a leg runs in (normalised)."""
    out = []
    for r in rows:
        cmd = r.get('cmd') or ''
        name = (r.get('name') or '').lower()
        project = project_of(cmd)
        age = max(0, int(now - (r.get('started') or now)))
        batch = '-batchmode' in cmd.lower()
        status = is_status_call(cmd)
        row = dict(pid=r.get('pid'), name=r.get('name'), session=r.get('session'), started=r.get('started'),
                   age_s=age, project=project, batch=batch, status_call=status, ppid=r.get('ppid'),
                   parent=r.get('parent') or '', stray=False, why='')
        keep = next((k for k in KEEP if k in cmd.lower() or k in project), '')
        on_leg = next((w for w in live if project and (project == w or project.startswith(w + '/'))), '')
        if keep:
            row['why'] = f'kept: {keep}'
        elif name == 'unity.licensing.client.exe':
            row['why'] = 'a licensing client: it goes with its Unity'
        elif not batch and not status and r.get('session') == 1:     # whatever its other flags (-hubIPC, -useHub)
            row['why'] = 'an editor window in the owner\'s session'
        elif on_leg:
            row['why'] = f'a leg runs on {on_leg} now'
        elif age < STRAY_AGE_S:
            row['why'] = f'younger than {STRAY_AGE_S // 3600} h'
        elif not (batch or status):
            row['why'] = 'not batch-mode and not a status call'
        else:
            row['stray'] = True
            row['why'] = (f"{'batch-mode' if batch else 'a unity status call'} for {age // 3600} h in session "
                          f"{r.get('session')}, no leg on {project or 'its project'}")
        out.append(row)
    return out


def run(ctx):
    rows = assess(processes(), ctx.now, relays.running_worktrees(ctx.now))
    strays = [r for r in rows if r['stray']]
    ctx.save({'at': ctx.now, 'census': rows, 'strays': [r['pid'] for r in strays]})
    return [f'{len(rows)} Unity processes, {len(strays)} strays'] + \
        [f"stray pid {r['pid']} ({r['name']}, session {r['session']}): {r['why']}" for r in strays]
