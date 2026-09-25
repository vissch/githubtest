#!/usr/bin/env python3
"""Is this checkout sane? Run this first when you land, and before you commit. About ten seconds, no Unity needed.

    python Tools/health.py            from trench-warfare-3d/
    python Tools/health.py --compile  also compile the changed assemblies offline (needs Tools/aosa/occ.py)

It checks, in order, and prints one line each:
  1. lock     who holds THIS checkout's Unity project (Tools/editor_lock.py). HELD means an editor or a batch run
              has it: do not gate, and do not write into Assets/ unless that editor is yours.
  2. editor   whether an editor for THIS checkout is connected to the unity CLI, and whether its last compile failed.
  3. validate python validate.py, which also runs Tools/codemap.py --check (docs match the code).
  4. branch   which lane you are in (from the branch name), how far you are behind the integration branch, and any
              uncommitted file outside your lane.
  5. compile  only with --compile.

Exit code: 0 all good, 1 something to fix before committing. HELD and "behind" are reported, not failed: they
tell you what not to do, they are not errors in your tree.
"""
import json
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
REPO = ROOT.parent
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
SIM_PATHS = ('trench-warfare-3d/Assets/_Project/Sim/', 'trench-warfare-3d/Assets/_Project/Net/',
             'trench-warfare-3d/Assets/_Project/Data/')


def run(cmd, cwd=ROOT, timeout=120):
    try:
        p = subprocess.run(cmd, cwd=cwd, capture_output=True, timeout=timeout)
        return p.returncode, p.stdout.decode('utf-8', 'replace') + p.stderr.decode('utf-8', 'replace')
    except FileNotFoundError as e:
        return 127, str(e)
    except subprocess.TimeoutExpired:
        return 124, 'timed out'


def unity_cli():
    for c in ('unity', os.path.join(os.environ.get('LOCALAPPDATA', ''), 'unity', 'bin', 'unity.exe')):
        code, _ = run([c, '--version'])
        if code == 0:
            return c
    return None


def main():
    bad = False

    sys.path.insert(0, str(ROOT / 'Tools'))
    import editor_lock
    state = editor_lock.probe(ROOT)['state']
    print(f'lock     {state}' + ('  (an editor or batch run holds this checkout: no gate, no writes into Assets/'
                                  ' unless it is yours)' if state != editor_lock.FREE else ''))

    code, out = run(['powershell', '-NoProfile', '-Command',
                     '[math]::Round((Get-CimInstance Win32_PerfFormattedData_PerfOS_Memory).AvailableMBytes/1024,1)'])
    try:
        free = float(out.strip().split()[-1])
        print(f'memory   {free} GB available' + ('  LOW: do not open another editor (an editor in Play holds 5-9 GB)'
                                             if free < 4 else ''))
    except (ValueError, IndexError):
        print('memory   unknown')

    cli = unity_cli()
    if not cli:
        print('editor   unity CLI not found (expected %LOCALAPPDATA%/unity/bin/unity.exe)')
    else:
        code, out = run([cli, 'status', '--json', '--no-banner', '--project-path', str(ROOT)])
        try:
            inst = json.loads(out[out.index('{'):])['data']['instances']
        except (ValueError, KeyError):
            inst = []
        if not inst:
            print('editor   none connected for this checkout (Tools/tw up opens one)')
        else:
            e = inst[0]
            line = f"editor   pid {e.get('pid')} state {e.get('state')} port {e.get('port')}"
            if e.get('state') == 'ready':
                env = dict(os.environ, UNITY_PROJECT_PATH=str(ROOT))
                p = subprocess.run([cli, '--no-banner', 'cmd', 'console_status', '--result-only'], cwd=ROOT,
                                   capture_output=True, env=env, timeout=60)
                s = p.stdout.decode('utf-8', 'replace')
                if '"compilationFailed": true' in s:
                    line += '  COMPILE FAILED: Tools/tw build shows the errors'
                    bad = True
            print(line)

    code, out = run([sys.executable, 'validate.py'])
    last = [l for l in out.strip().split('\n') if l][-1:] or ['']
    print(f'validate {"OK" if code == 0 else "FAILED"}' + ('' if code == 0 else '\n' + out.strip()))
    bad |= code != 0

    code, branch = run(['git', 'branch', '--show-current'], cwd=REPO)
    branch = branch.strip()
    lane = 'sim' if branch.startswith('lane/sim/') else 'show' if branch.startswith('lane/show/') else None
    _, counts = run(['git', 'rev-list', '--left-right', '--count', f'HEAD...{INTEGRATION}'], cwd=REPO)
    ahead, behind = (counts.split() + ['?', '?'])[:2]
    _, status = run(['git', 'status', '--porcelain'], cwd=REPO)
    dirty = [l[3:] for l in status.split('\n') if l.strip()]
    msg = f'branch   {branch or "(detached)"}  lane {lane or "NONE (see CLAUDE.md: work out your lane first)"}' \
          f'  ahead {ahead} behind {behind} of {INTEGRATION}  {len(dirty)} uncommitted'
    print(msg)
    if lane:
        foreign = [f for f in dirty if (f.startswith(SIM_PATHS)) != (lane == 'sim') and f.startswith('trench-warfare-3d/Assets/_Project/')]
        for f in foreign:
            print(f'         outside your lane: {f}')
        bad |= bool(foreign)

    if '--compile' in sys.argv:
        occ = ROOT / 'Tools' / 'aosa' / 'occ.py'
        if occ.exists():
            code, out = run([sys.executable, str(occ), '--changed'], timeout=900)
            print(f'compile  {"OK" if code == 0 else "FAILED"}' + ('' if code == 0 else '\n' + out[-3000:]))
            bad |= code != 0
        else:
            print('compile  Tools/aosa/occ.py is not on this branch (it lives on lane/show/aosa); '
                  'use `Tools/tw build` in your own editor, or the gate')

    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
