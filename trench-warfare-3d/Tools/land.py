#!/usr/bin/env python3
"""Land this lane on the integration branch: one command, so no step can be skipped, raced or half done.

    python Tools/land.py                       from trench-warfare-3d/, on your lane branch, tree clean
    python Tools/land.py --dry-run             every check, and the push it would make, without pushing
    python Tools/land.py --carry-sim "<why>"   a SHOW lane that carries SIM commits, by an owner decision: say which

After a landing it prints the `timelapse.py shoot` command for the commit that just landed, and nothing else: it
never waits on an editor itself (that would make every landing wait twenty minutes), so the shot is left for
whoever has an editor free, or a station picking it up from `timelapse.py missing`.

WHY. Landing by hand was: rebase, gate, fast-forward the local integration ref, push two branches. A second lane
landing during your gate made the fast-forward fail, and "rebase again" then landed a combination no gate had seen;
a push that failed after the local fast-forward left the local integration ref ahead of origin for good.

It fetches origin, then refuses, saying why, when:
- the branch is not lane/sim/* or lane/show/*, or the tree has uncommitted changes;
- HEAD does not contain origin's integration branch: rebase onto it (and gate again);
- origin's copy of your lane holds a commit your HEAD does not (someone pushed to it): look before overwriting it;
- the lane changes code (anything under trench-warfare-3d/ outside Tools/, gate.ps1, or any file whose name a test
  quotes, such as docs/17-ui-art-spec.md or Tools/envatlas.py, which EditMode tests read) and the last full gate
  that went green did not test exactly this tree. gate.ps1 records the tree it tested (tw-gate-green in this checkout's
  git dir). A rebase, an amend or one more commit makes a new tree, so the gate runs again. A lane that changes only
  docs is checked here instead, with validate.py;
- the lane changes a tool (Tools/, validate.py, gate.ps1, .github/) and Tools/toolcheck.py is not green: validate.py,
  Tools/selftest.py and every tool's own tests. It runs whether or not the lane also changes code: a green gate says
  nothing about the tools, and until 2026-10-04 a lane that changed both landed with no tool check at all;
- a SHOW lane's commits change Sim/, Net/ or Data/ (the SIM part lands first, on its own lane), unless --carry-sim
  names the owner decision that allows it;
- HEAD moved, or the tree went dirty, while the checks ran. It reads HEAD once, at the start, and every check and
  the push speak of that one commit id. Looking the name HEAD up again at the push is how a second writer in this
  checkout got an untested commit landed.
Then it pushes the integration branch and the lane in one atomic push: both move or neither does. The integration
branch only fast-forwards; the lane is pushed with a lease, because the rebase rewrote it. Refused (someone landed
first): fetch, rebase, gate, land again. The local integration ref is moved only after the push succeeded.
Exit 0 landed (or would, with --dry-run), 1 refused, 2 the push failed.
"""
import argparse
import os
import subprocess
import sys
from pathlib import Path

INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
SIM_PATHS = ('trench-warfare-3d/Assets/_Project/Sim/', 'trench-warfare-3d/Assets/_Project/Net/',
             'trench-warfare-3d/Assets/_Project/Data/')
# the tools: a change under one of these is checked by Tools/toolcheck.py (gate_scope.py tells gate.ps1 the same)
TOOL_PATHS = ('trench-warfare-3d/Tools/', '.github/')
TOOL_FILES = ('trench-warfare-3d/validate.py', 'gate.ps1')


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


def is_tool(path):
    return path.startswith(TOOL_PATHS) or path in TOOL_FILES


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

    # a relay leg never lands: the runner marks the checkout a leg works in (Tools/relay/prepush.py)
    if os.environ.get('TW_RELAY') or (Path(git('rev-parse', '--absolute-git-dir', check=True)[1]) / 'relay-leg.json').exists():
        refuse('a relay leg holds this checkout: landing is the owner\'s call, never a leg\'s')
    top = Path(git('rev-parse', '--show-toplevel', check=True)[1])
    branch = git('rev-parse', '--abbrev-ref', 'HEAD', check=True)[1]
    lane = 'sim' if branch.startswith('lane/sim/') else 'show' if branch.startswith('lane/show/') else None
    if not lane:
        refuse(f'{branch} is not a lane branch (lane/sim/* or lane/show/*)')
    dirty = git('status', '--porcelain')[1]
    if dirty:
        refuse('uncommitted changes (commit them, or they are not what the gate tested):\n' + dirty)

    # HEAD is a name, and a name can move while the checks run. Read the commit once here and speak of
    # that one id from now on, so the commit that is checked is the commit that is pushed.
    sha = git('rev-parse', 'HEAD', check=True)[1]

    git('fetch', '-q', 'origin', check=True)
    code, lease = git('rev-parse', '--verify', '-q', f'origin/{branch}')
    lease = lease if code == 0 else ''        # empty: the lease then means "must not exist"
    base = f'origin/{INTEGRATION}'
    if git('rev-parse', '--verify', '-q', base)[0] != 0:
        refuse(f'{base} does not exist: fetch the integration branch first')
    # the push rewrites origin's copy of the lane, so every commit on it must already be in HEAD: reachable,
    # or carried by the rebase (git cherry compares patches). Comparing origin/<lane> before and after this
    # fetch did not do it: any earlier fetch made the two equal and the push overwrote someone else's commit.
    if lease and git('merge-base', '--is-ancestor', lease, sha)[0] != 0:
        extra = [l for l in git('cherry', sha, lease, base)[1].split('\n') if l.startswith('+')]
        if extra:
            refuse(f'origin/{branch} holds {len(extra)} commit(s) your HEAD does not: someone pushed to your '
                   f'lane. See git log HEAD..origin/{branch}, bring it into your work, gate, land.\n'
                   + '\n'.join('  ' + e for e in extra[:10]))
    if git('merge-base', '--is-ancestor', base, sha)[0] != 0:
        refuse(f'HEAD does not contain {base}: someone landed. git rebase {base}, gate again, land again')
    ahead = git('rev-list', '--count', f'{base}..{sha}', check=True)[1]
    if ahead == '0':
        print(f'nothing to land: {branch} is already in {base}')
        return
    # --no-renames: a move lists its old path too, so a file moved out of Sim/ or out of Assets/ is still seen
    changed = [l for l in git('diff', '-z', '--name-only', '--no-renames', base, sha,
                              check=True)[1].split('\0') if l]
    print(f'{branch}: {ahead} commits, {len(changed)} files on top of {base}')

    sim = [f for f in changed if f.startswith(SIM_PATHS)]
    if lane == 'show' and sim:
        # ':(top)' makes the pathspecs repo-root relative: land.py runs from trench-warfare-3d/, so plain
        # repo-root paths matched nothing and this list was always empty
        commits = git('log', '--no-renames', '--format=  %h %s', f'{base}..{sha}', '--',
                      *(f':(top){p}' for p in SIM_PATHS))[1]
        if not a.carry_sim:
            refuse(f'a SHOW lane carrying {len(sim)} SIM files; the SIM part lands first, on its own lane. If the '
                   f'owner decided otherwise, pass --carry-sim "<the decision>". The commits:\n{commits}')
        print(f'carrying {len(sim)} SIM files by: {a.carry_sim}\n{commits}')

    tested_names = names_tests_read(top)
    proj = top / 'trench-warfare-3d'
    code_changed = any(is_code(f, tested_names) for f in changed)
    if code_changed:
        tree = git('rev-parse', sha + '^{tree}', check=True)[1]
        marker = Path(git('rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green', check=True)[1])
        green = marker.read_text(encoding='utf-8').split() if marker.exists() else []
        if green and green[0].startswith('stand-in:'):
            refuse('the last green full gate ran with TW_GATE_UNITY set, so a stand-in tested the gate itself, not '
                   'the game. Run the full gate with the real Unity (unset TW_GATE_UNITY), then land.')
        if not green or green[0] != tree:
            seen = f'the last green full gate tested tree {green[0][:10]} ({" ".join(green[1:])})' if green else \
                'no full gate has gone green in this checkout'
            refuse(f'code changed and {seen}; HEAD is tree {tree[:10]}. Run the full gate on this commit, then land.')
        print(f'full gate green on this exact tree ({" ".join(green[1:])})')
    tools = [f for f in changed if is_tool(f)]
    if tools:
        p = subprocess.run([sys.executable, str(Path(__file__).resolve().parent / 'toolcheck.py'), '--project', str(proj)],
                           cwd=proj, capture_output=True)
        out = (p.stdout + p.stderr).decode('utf-8', 'replace').strip()
        if p.returncode != 0:
            refuse(f'the lane changes the tools ({len(tools)} files: {", ".join(tools[:3])}{" ..." if len(tools) > 3 else ""}) '
                   f'and Tools/toolcheck.py is not green:\n' + out[-2500:])
        print(out)
    elif not code_changed:
        p = subprocess.run([sys.executable, 'validate.py'], cwd=proj, capture_output=True)
        if p.returncode != 0:
            refuse('docs only, and validate.py failed:\n' + (p.stdout + p.stderr).decode('utf-8', 'replace')[-1500:])
        print('validate.py OK')

    push = ['push', '--atomic', 'origin', f'{sha}:refs/heads/{INTEGRATION}',
            f'--force-with-lease=refs/heads/{branch}:{lease}', f'{sha}:refs/heads/{branch}']
    if a.dry_run:
        print('would run: git ' + ' '.join(push))
        return
    # toolcheck.py and the gate read the working tree, not sha, so this re-read is what ties the checks to the
    # commit about to be pushed: a second writer in this checkout moves HEAD or dirties the tree, and we refuse.
    now = git('rev-parse', 'HEAD', check=True)[1]
    if now != sha:
        refuse(f'HEAD moved while the checks ran: they checked {sha[:10]}, HEAD is now {now[:10]}. Nothing was '
               f'pushed. Someone else is working in this checkout; gate the commit you mean, then land.')
    dirty = git('status', '--porcelain')[1]
    if dirty:
        refuse('the tree went dirty while the checks ran, so they did not check it. Nothing was pushed. Commit or '
               'clean these, gate again, land again:\n' + dirty)
    code, out = git(*push)
    if code != 0:
        print(out)
        print(f'PUSH FAILED, nothing moved on origin. Someone probably landed: git fetch, git rebase {base}, gate, land.')
        sys.exit(2)
    local = git('fetch', '.', f'{sha}:{INTEGRATION}')
    print(f'landed {branch} on {INTEGRATION} at {git("rev-parse", "--short", sha)[1]}' +
          ('' if local[0] == 0 else f' (local {INTEGRATION} not moved: {local[1]})'))
    print(f'this landing owes a frame: python Tools/timelapse.py shoot --sha {sha}')


if __name__ == '__main__':
    main()
