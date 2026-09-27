#!/usr/bin/env python3
"""Is this checkout sane? Run this first when you land, and before you commit. About 15 s (validate is most of it),
no Unity needed.

    python Tools/health.py            from trench-warfare-3d/
    python Tools/health.py --compile  also compile the changed assemblies offline (needs Tools/aosa/occ.py)
    python Tools/health.py --lanes    only list the other checkouts on this machine (see below)

--lanes answers "who else is working, on what, and will we collide". For every git worktree of this repo it prints
the branch, its last commit (date and subject: what it is doing), how far it is ahead of and behind the integration
branch, whether it has uncommitted files, and the files a merge of it with YOUR branch would conflict on (a trial
merge in memory, `git merge-tree`; nothing is checked out). It replaces the hand-kept "In flight" list, which every
lane edited and so conflicted in every merge. A conflict it shows is one to talk about before it grows: a note in
a file in docs/inbox/, or ask the owner which lane lands first.

It checks, in order, and prints one line each:
  1. lock     who holds THIS checkout's Unity project (Tools/editor_lock.py). HELD means an editor or a batch run
              has it: do not gate, and do not write into Assets/ unless that editor is yours.
  2. editor   whether an editor for THIS checkout is connected to the unity CLI, and whether its last compile failed.
  3. validate python validate.py, which also runs Tools/codemap.py --check (docs match the code).
  4. branch   which lane you are in (from the branch name), how far you are ahead of and behind the integration
              branch (origin's, when fetched), and any file outside your lane that this branch changed or has
              uncommitted: a SHOW branch carrying Sim/ files fails here.
  5. inbox    the notes in docs/inbox/, on your branch and on the integration branch, marking those for you.
  6. compile  only with --compile.

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


def integration_ref():
    """origin's integration branch when this clone has fetched it (the shared truth), else the local one."""
    code, _ = run(['git', 'rev-parse', '--verify', '-q', f'origin/{INTEGRATION}'], cwd=REPO)
    return f'origin/{INTEGRATION}' if code == 0 else INTEGRATION


def inbox(branch):
    """Notes are files in docs/inbox/<date>-<to>-<topic>.md; <to> is a lane with / as - (show-aosa) or 'all'."""
    to_me = branch.replace('lane/', '', 1).replace('/', '-') if branch.startswith('lane/') else None
    here = {p.name for p in (REPO / 'docs' / 'inbox').glob('*.md') if p.name != 'README.md'}
    _, listed = run(['git', 'ls-tree', '--name-only', integration_ref(), 'docs/inbox/'], cwd=REPO)
    upstream = {Path(l).name for l in listed.split() if l.endswith('.md') and not l.endswith('README.md')}
    names = sorted(here | upstream)
    mine = [n for n in names if (to_me and f'-{to_me}-' in n) or '-all-' in n]
    print(f'inbox    {len(names)} notes, {len(mine)} for you' + ('' if names else ''))
    for n in names:
        where = '' if n in here else '  (on the integration branch only: rebase to get it)'
        print(f'         {"FOR YOU " if n in mine else "        "}docs/inbox/{n}{where}')


def unity_cli():
    for c in ('unity', os.path.join(os.environ.get('LOCALAPPDATA', ''), 'unity', 'bin', 'unity.exe')):
        code, _ = run([c, '--version'])
        if code == 0:
            return c
    return None


def lanes():
    _, me = run(['git', 'branch', '--show-current'], cwd=REPO)
    me = me.strip()
    _, porcelain = run(['git', 'worktree', 'list', '--porcelain'], cwd=REPO)
    trees = []
    for block in porcelain.strip().split('\n\n'):
        f = dict((l.split(' ', 1) + [''])[:2] for l in block.split('\n') if l)
        branch = f.get('branch', '').replace('refs/heads/', '') or '(detached)'
        trees.append((f.get('worktree', '?'), branch))
    print(f'{len(trees)} checkouts; conflicts are against your branch {me or "(detached)"}')
    for path, branch in trees:
        if not Path(path).exists():
            print(f'\n{Path(path).name}  [{branch}]  folder is gone: `git worktree prune` forgets it')
            continue
        _, last = run(['git', 'log', '-1', '--format=%cd  %s', '--date=format:%m-%d %H:%M', branch], cwd=REPO)
        _, counts = run(['git', 'rev-list', '--left-right', '--count', f'{branch}...{integration_ref()}'], cwd=REPO)
        ahead, behind = (counts.split() + ['?', '?'])[:2]
        _, dirty = run(['git', 'status', '--porcelain', '--untracked-files=no'], cwd=path)
        n = len([l for l in dirty.split('\n') if l.strip()])
        print(f'\n{Path(path).name}  [{branch}]  +{ahead} -{behind} vs integration'
              + (f'  {n} uncommitted' if n else '') + ('  (you)' if branch == me else ''))
        print(f'  {last.strip()[:110]}')
        if branch in (me, '(detached)') or not me:
            continue
        code, out = run(['git', 'merge-tree', '--write-tree', '--name-only', '--no-messages', me, branch], cwd=REPO)
        files = [l for l in out.strip().split('\n')[1:] if l.strip()]
        if code == 0:
            print('  merges cleanly with yours')
        elif files:
            names = ', '.join(Path(x).name for x in files[:8]) + (f' (+{len(files) - 8})' if len(files) > 8 else '')
            print(f'  CONFLICTS with yours in {len(files)}: {names}')
        else:
            print('  could not trial-merge: ' + out.strip()[:100])


def main():
    if '--lanes' in sys.argv:
        lanes()
        return
    bad = False

    sys.path.insert(0, str(ROOT / 'Tools'))
    import editor_lock
    state = editor_lock.probe(ROOT)['state']
    print(f'lock     {state}' + ('  (an editor or batch run holds this checkout: no gate, no writes into Assets/'
                                  ' unless it is yours)' if state != editor_lock.FREE else ''))

    # Commit headroom (limit minus committed, RAM + page file) is what runs out and kills editors. Low available RAM
    # alone only means paging: on 2026-09-27 the full gate passed with 0.6 GB available and 13 GB of headroom.
    code, out = run(['powershell', '-NoProfile', '-Command',
                     '$m=Get-CimInstance Win32_PerfFormattedData_PerfOS_Memory; "{0:N1} {1:N1}" -f '
                     '($m.AvailableMBytes/1024), (($m.CommitLimit-$m.CommittedBytes)/1GB)'])
    try:
        free, headroom = (float(v) for v in out.strip().split()[-2:])
        warn = ('  LOW: no editor and no gate; find the process holding commit (a leaking explorer.exe held 7 GB '
                'on 2026-09-26)' if headroom < 6 else
                '  enough for a batch gate, not for another editor (an editor in Play holds 5-9 GB)' if headroom < 10
                else '')
        print(f'memory   {headroom} GB commit headroom, {free} GB RAM available{warn}')
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
    integ = integration_ref()
    _, counts = run(['git', 'rev-list', '--left-right', '--count', f'HEAD...{integ}'], cwd=REPO)
    ahead, behind = (counts.split() + ['?', '?'])[:2]
    _, status = run(['git', 'status', '--porcelain'], cwd=REPO)
    dirty = [l[3:] for l in status.split('\n') if l.strip()]
    _, committed = run(['git', 'diff', '--name-only', f'{integ}...HEAD'], cwd=REPO)
    msg = f'branch   {branch or "(detached)"}  lane {lane or "NONE (see CLAUDE.md: work out your lane first)"}' \
          f'  ahead {ahead} behind {behind} of {integ}  {len(dirty)} uncommitted'
    print(msg)
    if lane:
        def foreign(f):
            return f.startswith('trench-warfare-3d/Assets/_Project/') and f.startswith(SIM_PATHS) != (lane == 'sim')
        outside = sorted({f for f in dirty + committed.split('\n') if f.strip() and foreign(f)})
        for f in outside[:12]:
            print(f'         outside your lane: {f}')
        if len(outside) > 12:
            print(f'         ... and {len(outside) - 12} more outside your lane')
        bad |= bool(outside)
    inbox(branch)

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
