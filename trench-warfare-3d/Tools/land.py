#!/usr/bin/env python3
"""Land this lane on the integration branch: one command, so no step can be skipped, raced or half done.

    python Tools/land.py                       from trench-warfare-3d/, on your lane branch, tree clean
    python Tools/land.py --dry-run             every check, and the push it would make, without pushing
    python Tools/land.py --carry-sim "<why>"   a SHOW lane that carries SIM commits, by an owner decision: say which

WHY. Landing by hand was: rebase, gate, fast-forward the local integration ref, push two branches. A second lane
landing during your gate made the fast-forward fail, and "rebase again" then landed a combination no gate had seen;
a push that failed after the local fast-forward left the local integration ref ahead of origin for good.

It fetches origin, then refuses, saying why, when:
- the branch is not lane/sim/* or lane/show/*, or the tree has uncommitted changes;
- HEAD does not contain origin's integration branch: rebase onto it (and gate again);
- origin's copy of your lane moved when it was fetched (someone pushed to it): look before overwriting it;
- the lane changes code (anything under trench-warfare-3d/ outside Tools/, gate.ps1, or any file whose name a test
  quotes, such as docs/17-ui-art-spec.md or Tools/envatlas.py, which EditMode tests read) and the last full gate
  that went green did not test exactly this tree. gate.ps1 records the tree it tested (tw-gate-green in this checkout's
  git dir). A rebase, an amend or one more commit makes a new tree, so the gate runs again. A lane that changes only
  docs and Tools/ is checked here instead: validate.py, and Tools/selftest.py when Tools/ changed;
- a SHOW lane's commits change Sim/, Net/ or Data/ (the SIM part lands first, on its own lane), unless --carry-sim
  names the owner decision that allows it.
Then it pushes the integration branch and the lane in one atomic push: both move or neither does. The integration
branch only fast-forwards; the lane is pushed with a lease, because the rebase rewrote it. Refused (someone landed
first): fetch, rebase, gate, land again. The local integration ref is moved only after the push succeeded.
Exit 0 landed (or would, with --dry-run), 1 refused, 2 the push failed.
"""
import argparse
import subprocess
import sys
from pathlib import Path

INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
SIM_PATHS = ('trench-warfare-3d/Assets/_Project/Sim/', 'trench-warfare-3d/Assets/_Project/Net/',
             'trench-warfare-3d/Assets/_Project/Data/')


def git(*args, check=False):
    p = subprocess.run(['git', *args], capture_output=True)
    out = (p.stdout + p.stderr).decode('utf-8', 'replace').strip()
    if check and p.returncode != 0:
        sys.exit(f'git {" ".join(args)} failed: {out}')
    return p.returncode, out


def refuse(why):
    print(f'REFUSED: {why}')
    sys.exit(1)


def is_code(path, tested_names):
    return ((path.startswith('trench-warfare-3d/') and not path.startswith('trench-warfare-3d/Tools/'))
            or path == 'gate.ps1' or Path(path).name in tested_names)


def names_tests_read(top):
    '''File names quoted in the test sources ("envatlas.py", "17-ui-art-spec.md"): a test reads those files, so a
    change to one needs the gate even when it lives in docs/ or Tools/.'''
    import re
    names = set()
    for cs in (top / 'trench-warfare-3d' / 'Assets' / '_Project' / 'Tests').rglob('*.cs'):
        names |= set(re.findall(r'"([\w.-]+\.\w+)"', cs.read_text(encoding='utf-8-sig', errors='replace')))
    return names


def main():
    ap = argparse.ArgumentParser(description='land this lane on the integration branch')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--carry-sim', metavar='WHY', default='')
    a = ap.parse_args()

    top = Path(git('rev-parse', '--show-toplevel', check=True)[1])
    branch = git('rev-parse', '--abbrev-ref', 'HEAD', check=True)[1]
    lane = 'sim' if branch.startswith('lane/sim/') else 'show' if branch.startswith('lane/show/') else None
    if not lane:
        refuse(f'{branch} is not a lane branch (lane/sim/* or lane/show/*)')
    dirty = git('status', '--porcelain')[1]
    if dirty:
        refuse('uncommitted changes (commit them, or they are not what the gate tested):\n' + dirty)

    # the lease is what this clone last knew of origin's copy of the lane, read BEFORE fetching: leasing on the
    # value just fetched would overwrite whatever someone else pushed to the lane meanwhile
    code, lease = git('rev-parse', '--verify', '-q', f'origin/{branch}')
    lease = lease if code == 0 else ''
    git('fetch', '-q', 'origin', check=True)
    code, now = git('rev-parse', '--verify', '-q', f'origin/{branch}')
    if (now if code == 0 else '') != lease:
        refuse(f'origin/{branch} moved when fetched: someone pushed to your lane. See git log HEAD..origin/{branch}, '
               f'bring it into your work, gate, land.')
    base = f'origin/{INTEGRATION}'
    if git('rev-parse', '--verify', '-q', base)[0] != 0:
        refuse(f'{base} does not exist: fetch the integration branch first')
    if git('merge-base', '--is-ancestor', base, 'HEAD')[0] != 0:
        refuse(f'HEAD does not contain {base}: someone landed. git rebase {base}, gate again, land again')
    ahead = git('rev-list', '--count', f'{base}..HEAD', check=True)[1]
    if ahead == '0':
        print(f'nothing to land: {branch} is already in {base}')
        return
    # --no-renames: a move lists its old path too, so a file moved out of Sim/ or out of Assets/ is still seen
    changed = [l for l in git('diff', '--name-only', '--no-renames', base, 'HEAD', check=True)[1].split('\n') if l]
    print(f'{branch}: {ahead} commits, {len(changed)} files on top of {base}')

    sim = [f for f in changed if f.startswith(SIM_PATHS)]
    if lane == 'show' and sim:
        commits = git('log', '--no-renames', '--format=  %h %s', f'{base}..HEAD', '--', *SIM_PATHS)[1]
        if not a.carry_sim:
            refuse(f'a SHOW lane carrying {len(sim)} SIM files; the SIM part lands first, on its own lane. If the '
                   f'owner decided otherwise, pass --carry-sim "<the decision>". The commits:\n{commits}')
        print(f'carrying {len(sim)} SIM files by: {a.carry_sim}\n{commits}')

    tested_names = names_tests_read(top)
    if any(is_code(f, tested_names) for f in changed):
        tree = git('rev-parse', 'HEAD^{tree}', check=True)[1]
        marker = Path(git('rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green', check=True)[1])
        green = marker.read_text(encoding='utf-8').split() if marker.exists() else []
        if not green or green[0] != tree:
            seen = f'the last green full gate tested tree {green[0][:10]} ({" ".join(green[1:])})' if green else \
                'no full gate has gone green in this checkout'
            refuse(f'code changed and {seen}; HEAD is tree {tree[:10]}. Run the full gate on this commit, then land.')
        print(f'full gate green on this exact tree ({" ".join(green[1:])})')
    else:
        proj = top / 'trench-warfare-3d'
        checks = [['validate.py']] + ([['Tools/selftest.py']] if any(f.startswith('trench-warfare-3d/Tools/') for f in changed) else [])
        for c in checks:
            p = subprocess.run([sys.executable, *c], cwd=proj, capture_output=True)
            if p.returncode != 0:
                refuse(f'docs/tools only, and {c[0]} failed:\n' + (p.stdout + p.stderr).decode('utf-8', 'replace')[-1500:])
            print(f'{c[0]} OK')

    push = ['push', '--atomic', 'origin', f'HEAD:refs/heads/{INTEGRATION}',
            f'--force-with-lease=refs/heads/{branch}:{lease}', f'HEAD:refs/heads/{branch}']
    if a.dry_run:
        print('would run: git ' + ' '.join(push))
        return
    code, out = git(*push)
    if code != 0:
        print(out)
        print(f'PUSH FAILED, nothing moved on origin. Someone probably landed: git fetch, git rebase {base}, gate, land.')
        sys.exit(2)
    local = git('fetch', '.', f'HEAD:{INTEGRATION}')
    print(f'landed {branch} on {INTEGRATION} at {git("rev-parse", "--short", "HEAD")[1]}' +
          ('' if local[0] == 0 else f' (local {INTEGRATION} not moved: {local[1]})'))


if __name__ == '__main__':
    main()
