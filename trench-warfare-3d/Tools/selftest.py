#!/usr/bin/env python3
"""Test the tools that keep this repo honest. Run it after changing any tool here.

WHY. validate.py trusts codemap.py --check, and a merge trusts port_split.py. If an edit to either quietly stops it
catching anything, every session keeps trusting a check that no longer works. This breaks things on purpose and
asserts each break is caught, in a throwaway worktree (your checkout is never touched), about a minute.

    python Tools/selftest.py        from trench-warfare-3d/; exit 0 = every case behaved
    python Tools/selftest.py --only land,relay     only these groups (codemap, port_split, scorecard, land,
                                                   relay, health): a land.py check in 20 s, not 130

Cases: codemap --check passes on a clean tree, then fails on each of: a command that runs a missing Tools/ script, a
cited file that does not exist, a folder with no purpose line, an undocumented command-line flag, a test class
tasks.md never names, an agent-memory.md over its cap. port_split.py, on a small repo built here: an edit to moved
code lands in the new file, an edit to code that stayed lands in the old one, and an edit whose lines both sides
changed (or whose lines the other side changed in one of two identical copies) goes to the .rej file. health.py
--lanes runs and lists this checkout. scorecard.py keeps reporting a regression until it is fixed or accepted, and
counts an unmeasured metric as one. The relay (Tools/relay): land.py and pipeline.py refuse inside a leg, git's
pre-push hook refuses a held checkout every push but its own lane and does nothing without the marker, and the
leg's command rules refuse a landing. The relay's full tests are Tools/relay/test_relay.py.
"""
import pathlib
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
PROJ = HERE.parent
REPO = PROJ.parent
results = []


def run(cmd, cwd):
    p = subprocess.run(cmd, cwd=cwd, capture_output=True)
    return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')


def case(name, ok, detail=''):
    results.append(ok)
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + detail.strip().replace('\n', '\n      ')[:600]))


def edit(path: pathlib.Path, old, new):
    b = path.read_bytes().decode('utf-8')
    assert old in b, f'{path.name}: fixture text not found: {old!r}'
    path.write_bytes(b.replace(old, new, 1).encode('utf-8'))


def codemap_cases(wt: pathlib.Path):
    proj = wt / 'trench-warfare-3d'
    check = [sys.executable, 'Tools/codemap.py', '--check']

    def expect(name, needle, breaker, regen=False):
        breaker()
        if regen:
            run([sys.executable, 'Tools/codemap.py'], proj)
        code, out = run(check, proj)
        case(f'codemap catches {name}', code != 0 and needle in out, f'exit {code}, wanted "{needle}" in:\n{out}')
        run(['git', 'checkout', '-q', '--', '.'], wt)
        run(['git', 'clean', '-qfd'], wt)

    code, out = run(check, proj)
    case('codemap --check passes on the clean tree', code == 0, out)

    # the other direction: ordinary edits must not fail the check, or every lane regenerates and conflicts
    sim_host = proj / 'Assets/_Project/Presentation/Core/SimHost.cs'
    sim_host.write_bytes(b'// a harmless comment\n' + sim_host.read_bytes())
    tests = proj / 'Assets/_Project/Tests/EditMode/EnvAtlasTests.cs'
    edit(tests, 'public void The_Packer_And_The_Kit_List_The_Same_Sets_In_The_Same_Order()',
         'public void A_New_Case() { }\n        [Test]\n        public void The_Packer_And_The_Kit_List_The_Same_Sets_In_The_Same_Order()')
    code, out = run(check, proj)
    case('codemap --check passes a harmless edit (a comment, one more test in a class)', code == 0, out)
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)

    wf = wt / 'docs/reference/workflow.md'
    expect('a command running a missing tool', 'Tools/shotstatz.py',
           lambda: edit(wf, 'python Tools/shotstats.py', 'python Tools/shotstatz.py'))
    expect('a cited file that does not exist', 'NoSuchFile.cs',
           lambda: wf.write_bytes(wf.read_bytes() + b'\nSee `Presentation/Core/NoSuchFile.cs`.\n'))

    def new_folder():
        d = proj / 'Assets/_Project/Presentation/NewThing'
        d.mkdir(parents=True)
        (d / 'X.cs').write_text('namespace TW { class X {} }\n')
    expect('a folder with no purpose line', 'NewThing/ has no purpose line', new_folder)

    expect('an undocumented flag', '"-twselftest" is undocumented',
           lambda: (proj / 'Assets/_Project/Presentation/Core/SelfTestFlag.cs').write_text(
               'namespace TW { static class F { static bool On => System.Array.IndexOf('
               'System.Environment.GetCommandLineArgs(), "-twselftest") >= 0; } }\n'), regen=True)
    expect('a test class tasks.md never names', 'never names test SelfTestProbeTests',
           lambda: (proj / 'Assets/_Project/Tests/EditMode/SelfTestProbeTests.cs').write_text(
               'using NUnit.Framework;\nnamespace TW.Tests { public class SelfTestProbeTests { [Test] public void A() {} } }\n'),
           regen=True)
    expect('a production file no agent page names', 'NewHelper.cs is named on no agent page',
           lambda: (proj / 'Assets/_Project/Presentation/Core/NewHelper.cs').write_text(
               'namespace TW.Presentation { static class NewHelper { } }\n'))
    head_subject = run(['git', 'log', '-1', '--format=%s'], wt)[1].strip().replace('"', '')[:60]
    tasks = wt / 'docs/reference/tasks.md'
    expect('an "(until ... lands)" line whose commit has landed', 'has landed',
           lambda: tasks.write_bytes(tasks.read_bytes() + f'\nA pending fix (until "{head_subject}" lands).\n'.encode()))
    tag = 'Selftest quoted subject that never landed'
    tasks.write_bytes(tasks.read_bytes() + f'\nA pending fix (until "{tag}" lands).\n'.encode())
    run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qam',
         f'quote it\n\nThe body says (until "{tag}" lands).'], wt)
    code, out = run(check, proj)
    case('codemap does not call a tag landed when a commit message only quotes it', tag not in out, out)
    run(['git', 'reset', '-q', '--hard', 'HEAD~1'], wt)
    sim_lines = len((proj / 'Assets/_Project/Presentation/Core/SimHost.cs').read_text(encoding='utf-8-sig').splitlines())
    expect('a cited line one past the end of the file', 'past the end',
           lambda: wf.write_bytes(wf.read_bytes() + f'\nSee `Presentation/Core/SimHost.cs:{sim_lines + 1}`.\n'.encode()))
    expect('a tool whose name only appears inside a longer tool name', 'Tools/map.py is in neither',
           lambda: (proj / 'Tools/map.py').write_text('print(1)\n'))
    mem = wt / 'docs/reference/agent-memory.md'
    expect('agent-memory.md over its cap', 'agent-memory.md is',
           lambda: mem.write_bytes(mem.read_bytes() + b''.join(b'- filler %d\n' % i for i in range(200))))


def port_split_cases(tmp: pathlib.Path):
    r = tmp / 'split-repo'
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    big = r / 'A/Big.cs'
    lines = ['class Big', '{'] + [f'    int keep{i} = {i};' for i in range(12)] + \
            [f'    int move{i} = {i};' for i in range(12)] + ['}', '']
    big.write_text('\n'.join(lines))
    g('add', '.'); g('commit', '-qm', 'base')
    g('checkout', '-qb', 'edits')
    edit(big, 'int keep3 = 3;', 'int keep3 = 30;')      # stays in Big.cs
    edit(big, 'int move8 = 8;', 'int move8 = 80;')      # moves to Big.Moved.cs
    edit(big, 'int move2 = 2;', 'int move2 = 20;')      # both sides change it: a real conflict
    edit(big, 'int keep9 = 9;', 'int keep9 = 90;')      # the split side changes the line above: git calls it a conflict
    g('commit', '-qam', 'edits')
    g('checkout', '-q', 'main')
    keep = [l if 'keep8' not in l else '    int keep8 = 8; // split side changed this' for l in lines if 'move' not in l]
    big.write_text('\n'.join(keep))
    moved = ['partial class Big', '{'] + [f'    int move{i} = {i};' for i in range(12)] + ['}', '']
    moved[2 + 2] = '    int move2 = 2; // split side changed this'
    (r / 'A/Big.Moved.cs').write_text('\n'.join(moved))
    g('add', '.'); g('commit', '-qm', 'split')
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--from', 'main~1', '--to', 'edits'], r)
    b_old = big.read_text(); b_new = (r / 'A/Big.Moved.cs').read_text()
    case('port_split carries an edit into the file the code moved to', 'int move8 = 80;' in b_new, out)
    case('port_split keeps an edit to code that stayed in the old file', 'int keep3 = 30;' in b_old, out)
    rej = r / 'A/Big.cs.port.rej'
    rej_text = rej.read_text() if rej.exists() else ''
    case('port_split leaves a both-sides edit in .rej and exits 1',
         code == 1 and 'move2 = 20' in rej_text and 'move2 = 20' not in b_new, out)
    case('port_split rejects an edit whose context the other side changed (git would conflict)',
         'keep9 = 90' in rej_text and 'keep9 = 90' not in b_old, out)

    # the lane rule is rebase: a lane cut before the split rebases onto it and ports each stopped commit
    g('checkout', '-q', '-f', 'main'); g('clean', '-qfd')
    g('checkout', '-qb', 'lane', 'main~1')
    edit(big, 'int move9 = 9;', 'int move9 = 99;')
    g('commit', '-qam', 'lane edit')
    code, out = g('rebase', 'main')
    stopped = code != 0
    conflicted = (r / 'A/Big.cs').read_bytes()
    code, dry = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--rebase', '--dry-run'], r)
    case('port_split --dry-run says where edits go and leaves every file as it was',
         stopped and 'applied' in dry and (r / 'A/Big.cs').read_bytes() == conflicted
         and 'int move9 = 99;' not in (r / 'A/Big.Moved.cs').read_text(), dry)
    code, out2 = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--rebase'], r)
    moved_now = (r / 'A/Big.Moved.cs').read_text()
    g('add', '-A')
    code2, out3 = run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', '-c', 'core.editor=true', 'rebase', '--continue'], r)
    case('port_split --rebase carries a rebased commit into the split and the rebase completes',
         stopped and code == 0 and 'int move9 = 99;' in moved_now and code2 == 0, out + out2 + out3)


def port_split_twin_case(tmp: pathlib.Path):
    # Two methods with the same body. The lane edits B; upstream moves B out and changes a line of it. The edit's
    # lines still match A exactly once, so a placement by text alone lands it in the wrong method.
    r = tmp / 'twin-repo'
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    big = r / 'A/Big.cs'
    method = lambda name, reset: [f'    void {name}()', '    {', '        Init();', f'        {reset}();', '    }']
    big.write_text('\n'.join(['class Big', '{'] + method('A', 'Reset') + method('B', 'Reset') +
                             ['    int keep0 = 0;', '}', '']))
    g('add', '.'); g('commit', '-qm', 'base')
    g('checkout', '-qb', 'edits')
    b = big.read_text().split('\n')
    b.insert(b.index('    void B()') + 4, '        Log();')
    big.write_text('\n'.join(b))
    g('commit', '-qam', 'edits')
    g('checkout', '-q', 'main')
    big.write_text('\n'.join(['class Big', '{'] + method('A', 'Reset') + ['    int keep0 = 0;', '}', '']))
    (r / 'A/Big.Moved.cs').write_text('\n'.join(['partial class Big', '{'] + method('B', 'ResetAll') + ['}', '']))
    g('add', '.'); g('commit', '-qm', 'split')
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--from', 'main~1', '--to', 'edits'], r)
    case('port_split rejects an edit whose lines the other side changed in one of two identical copies',
         code == 1 and 'Log();' not in big.read_text() and 'Log();' not in (r / 'A/Big.Moved.cs').read_text(), out)


def land_cases(tmp: pathlib.Path):
    # A bare origin with the integration branch, and a clone with a lane on it. land.py runs from the clone.
    integ = 'claude/trench-warfare-2d-3d-plan-idt7lf'
    origin, work = tmp / 'origin.git', tmp / 'land-work'
    run(['git', 'init', '-q', '--bare', str(origin)], tmp)
    run(['git', 'clone', '-q', str(origin), str(work)], tmp)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], work)
    proj = work / 'trench-warfare-3d'
    (proj / 'Tools').mkdir(parents=True)
    (proj / 'validate.py').write_text('print("validation OK")\n')
    (proj / 'Tools/selftest.py').write_text('print("0 of 0 cases behaved")\n')
    (work / 'docs').mkdir()
    (work / 'docs/a.md').write_text('a\n')
    g('checkout', '-qb', integ); g('add', '.'); g('commit', '-qm', 'base'); g('push', '-q', 'origin', integ)
    land = lambda *extra: run([sys.executable, str(HERE / 'land.py'), *extra], proj)
    head = lambda ref: run(['git', 'rev-parse', ref], work)[1].strip()

    g('checkout', '-qb', 'lane/show/t')
    (work / 'docs/a.md').write_text('a\nb\n'); g('commit', '-qam', 'docs')
    code, out = land()
    case('land.py lands a docs-only lane after validate, moving origin and the lane together',
         code == 0 and head(f'origin/{integ}') == head('HEAD') == head('origin/lane/show/t'), out)

    (proj / 'Code.cs').write_text('class C {}\n'); g('add', '.'); g('commit', '-qm', 'code')
    code, out = land()
    case('land.py refuses code that no green full gate tested', code == 1 and head(f'origin/{integ}') != head('HEAD'), out)
    marker = pathlib.Path(run(['git', 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green'], work)[1].strip())
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    (proj / 'Code.cs').write_text('class C { int x; }\n'); g('commit', '-qam', 'one more')
    code, out = land()
    case('land.py refuses when HEAD is not the tree the gate tested (one more commit after the gate)', code == 1, out)
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land()
    case('land.py lands code whose exact tree went green', code == 0 and head(f'origin/{integ}') == head('HEAD'), out)

    sim = proj / 'Assets/_Project/Sim'
    sim.mkdir(parents=True); (sim / 'S.cs').write_text('class S {}\n'); g('add', '.'); g('commit', '-qm', 'sim on show')
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land()
    code2, out2 = land('--dry-run', '--carry-sim', 'owner, 2026-09-26: one session on both lanes')
    case('land.py refuses a SHOW lane carrying SIM files unless --carry-sim names the decision',
         code == 1 and 'SIM' in out and code2 == 0 and 'would run' in out2, out + out2)

    # a move out of Sim/ is still SIM work: with rename detection the diff lists only the new path
    g('reset', '-q', '--hard', f'origin/{integ}')
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')
    sim.mkdir(parents=True, exist_ok=True); (sim / 'Moved.cs').write_text('class Moved { int a, b, c; }\n')
    g('add', '.'); g('commit', '-qm', 'sim file'); g('push', '-q', 'origin', f'HEAD:{integ}'); g('fetch', '-q')
    (proj / 'Assets/_Project/Presentation').mkdir(parents=True, exist_ok=True)
    g('mv', 'trench-warfare-3d/Assets/_Project/Sim/Moved.cs', 'trench-warfare-3d/Assets/_Project/Presentation/Moved.cs')
    g('commit', '-qm', 'move it out of Sim')
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land('--dry-run')
    case('land.py sees a file a SHOW lane moved out of Sim/ (renames do not hide the old path)',
         code == 1 and 'SIM' in out, out)

    # a docs file a test reads is code: a docs-only lane that edits it needs the gate
    g('reset', '-q', '--hard', f'origin/{integ}')
    tests = proj / 'Assets/_Project/Tests'
    tests.mkdir(parents=True, exist_ok=True)
    (tests / 'SpecTests.cs').write_text('class SpecTests { string p = "spec.md"; }\n')
    (work / 'docs/spec.md').write_text('spec\n')
    g('add', '.'); g('commit', '-qm', 'a test reads docs/spec.md'); g('push', '-q', 'origin', f'HEAD:{integ}'); g('fetch', '-q')
    (work / 'docs/spec.md').write_text('spec changed\n'); g('commit', '-qam', 'docs only, but a test reads it')
    code, out = land('--dry-run')
    case('land.py gates a docs change to a file a test reads', code == 1 and 'gate' in out, out)

    # someone pushed to origin's copy of the lane: the lease must not overwrite it
    g('reset', '-q', '--hard', f'origin/{integ}')
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')
    other = tmp / 'land-other'
    run(['git', 'clone', '-q', '-b', 'lane/show/t', str(origin), str(other)], tmp)
    (other / 'docs/theirs.md').write_text('theirs\n')
    run(['git', '-c', 'user.name=o', '-c', 'user.email=o@o', 'add', '.'], other)
    run(['git', '-c', 'user.name=o', '-c', 'user.email=o@o', 'commit', '-qm', 'theirs'], other)
    run(['git', 'push', '-q', 'origin', 'lane/show/t'], other)
    (work / 'docs/mine.md').write_text('mine\n'); g('add', '.'); g('commit', '-qm', 'mine')
    code, out = land()
    theirs_kept = 'theirs' in run(['git', 'log', '--format=%s', 'origin/lane/show/t'], work)[1]
    case('land.py refuses when someone pushed to origin\'s copy of the lane, and their commit survives',
         code == 1 and theirs_kept, out)

    g('checkout', '-q', integ); g('reset', '-q', '--hard', f'origin/{integ}')
    (work / 'docs/b.md').write_text('b\n'); g('add', '.'); g('commit', '-qm', 'someone else lands'); g('push', '-q', 'origin', integ)
    g('checkout', '-q', 'lane/show/t')
    code, out = land()
    case('land.py refuses a lane that does not contain origin integration (someone landed first)',
         code == 1 and 'rebase' in out, out)


def relay_cases(tmp: pathlib.Path):
    # What holds a relay leg (Tools/relay) sits outside the model. Each stop is switched on here and must refuse,
    # and the same push without the marker must pass, so a hook that refuses everything is caught too.
    import json
    import os
    sys.path.insert(0, str(HERE / 'pipeline'))
    sys.path.insert(0, str(HERE / 'relay'))
    import cmdrules
    import gitio
    from pipeline import proc_start
    origin, work, board = tmp / 'relay-origin.git', tmp / 'relay-work', tmp / 'relay-board'
    run(['git', 'init', '-q', '--bare', str(origin)], tmp)
    run(['git', 'clone', '-q', str(origin), str(work)], tmp)
    for sub in ('items', 'results', 'feedback', 'claims'):
        (board / sub).mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], work)
    (work / 'a.txt').write_text('a\n')
    g('checkout', '-qb', 'lane/show/t'); g('add', '.'); g('commit', '-qm', 'base'); g('push', '-q', 'origin', 'lane/show/t')

    def tool(script, *args, **env):
        p = subprocess.run([sys.executable, str(HERE / script), *args], cwd=work, capture_output=True,
                           env=dict(os.environ, TW_BOARD=str(board), TW_STATION='desktop', **env))
        return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')

    code, out = tool('land.py', '--dry-run', TW_RELAY='1')
    case('land.py refuses inside a relay leg', code == 1 and 'relay leg' in out, out)
    code, out = tool('pipeline/pipeline.py', 'claim', 'thing--shots--0', TW_RELAY='1')
    case('pipeline.py refuses to claim inside a relay leg', code != 0 and 'relay leg' in out, out)

    marker = gitio.marker_path(work)
    marker.write_text(json.dumps({'who': 'selftest', 'pid': os.getpid(), 'pid_start': proc_start(os.getpid()),
                                  'lane': 'lane/show/t'}), encoding='utf-8')
    gitio.install_prepush(work)
    code, out = tool('land.py', '--dry-run')
    case('land.py refuses in a checkout a relay leg holds (the marker, no variable)', code == 1 and 'relay leg' in out, out)
    (work / 'a.txt').write_text('b\n'); g('commit', '-qam', 'more')
    code, out = g('push', '-q', 'origin', 'HEAD:refs/heads/other')
    case('git refuses a held checkout\'s push to another branch (the relay\'s pre-push hook)', code != 0 and 'relay' in out, out)
    code, out = g('push', '-q', 'origin', 'lane/show/t')
    case('the same hook lets the leg\'s own lane through', code == 0, out)
    marker.unlink()
    code, out = g('push', '-q', 'origin', 'HEAD:refs/heads/other')
    case('without the marker the hook does nothing', code == 0, out)

    leg = {'lane': 'lane/show/t', 'worktree': str(work), 'board': str(board)}
    case('the leg\'s command rules refuse land.py and a push to main, and pass a push of its own lane',
         bool(cmdrules.never('python Tools/land.py', leg)) and bool(cmdrules.never('git push origin main', leg))
         and cmdrules.never('git push origin lane/show/t', leg) is None)


def scorecard_cases():
    sys.path.insert(0, str(HERE))
    import scorecard
    good = {'codemap_errors': 0, 'editmode_tests': 343, 'editmode_failed': 0, 'statics_explained': 30}
    bad = dict(good, codemap_errors=3)
    worse, _ = scorecard.compare([good], bad)
    case('scorecard flags a metric that got worse', any('codemap_errors' in w for w in worse), str(worse))
    worse, _ = scorecard.compare([good, dict(bad, regressed=['codemap_errors'])], bad)
    case('scorecard keeps flagging a regression on the next run (it never becomes the baseline)',
         any('codemap_errors' in w for w in worse), str(worse))
    worse, _ = scorecard.compare([good], dict(good, statics_explained=-1, editmode_failed=-1))
    case('scorecard flags a metric that reads -1 (its source was unreadable)', len(worse) == 2, str(worse))
    missing = dict(good); del missing['editmode_tests']
    worse, _ = scorecard.compare([good], missing)
    case('scorecard flags a metric that was not measured', any('editmode_tests' in w for w in worse), str(worse))
    worse, _ = scorecard.compare([good, dict(bad, regressed=['codemap_errors'], accepted=True)], bad)
    case('scorecard takes an accepted run as the new baseline', not worse, str(worse))


GROUPS = ('codemap', 'port_split', 'scorecard', 'land', 'relay', 'health')


def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument('--only', default='', help='run only these groups, comma separated: ' + ', '.join(GROUPS))
    a = ap.parse_args()
    only = [g.strip() for g in a.only.split(',') if g.strip()] or list(GROUPS)
    unknown = [g for g in only if g not in GROUPS]
    if unknown:
        sys.exit('selftest: no group named %s (have: %s)' % (', '.join(unknown), ', '.join(GROUPS)))
    # Run by a relay leg, the tools tested here would refuse (land.py and pipeline.py do, inside a leg). They run
    # on throwaway repos, so the leg's mark is taken off; relay_cases puts it back where it tests the refusal.
    __import__('os').environ.pop('TW_RELAY', None)
    tmp = pathlib.Path(tempfile.mkdtemp(prefix='tw-selftest-'))
    wt = tmp / 'wt'
    try:
        if 'codemap' in only:                     # the one group that needs a copy of this checkout
            run(['git', 'worktree', 'prune'], REPO)   # a run killed half way leaves its worktree registered
            code, out = run(['git', 'worktree', 'add', '-q', '--detach', str(wt), 'HEAD'], REPO)
            if code:
                sys.exit('could not create a worktree: ' + out)
            # test the tree as it is on disk (the change you are about to commit): copy every changed and new file over
            changed = run(['git', 'diff', '--name-only', '--diff-filter=AMR', 'HEAD'], REPO)[1].split('\n')
            changed += run(['git', 'ls-files', '--others', '--exclude-standard'], REPO)[1].split('\n')
            for rel in filter(None, (c.strip() for c in changed)):
                (wt / rel).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(REPO / rel, wt / rel)
            for rel in filter(None, run(['git', 'diff', '--name-only', '--diff-filter=D', 'HEAD'], REPO)[1].split('\n')):
                (wt / rel.strip()).unlink(missing_ok=True)
            run(['git', 'add', '-A'], wt)
            run(['git', '-c', 'user.name=selftest', '-c', 'user.email=selftest@local', 'commit', '-qm', 'selftest: the working tree as it is on disk',
                 '--allow-empty', '--no-verify'], wt)
            codemap_cases(wt)
        if 'port_split' in only:
            port_split_cases(tmp)
            port_split_twin_case(tmp)
        if 'scorecard' in only:
            scorecard_cases()
        if 'land' in only:
            land_cases(tmp)
        if 'relay' in only:
            relay_cases(tmp)
        if 'health' in only:
            code, out = run([sys.executable, str(HERE / 'health.py'), '--lanes'], PROJ)
            case('health.py --lanes lists this checkout', code == 0 and '(you)' in out, out)
    finally:
        if wt.exists():
            run(['git', 'worktree', 'remove', '--force', str(wt)], REPO)
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved' + ('' if len(only) == len(GROUPS) else ' (only: %s)' % ', '.join(only)))
    sys.exit(0 if all(results) else 1)


if __name__ == '__main__':
    main()
