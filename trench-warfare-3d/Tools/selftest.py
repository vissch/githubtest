#!/usr/bin/env python3
"""Test the tools that keep this repo honest. Run it after changing codemap.py, port_split.py or health.py.

WHY. validate.py trusts codemap.py --check, and a merge trusts port_split.py. If an edit to either quietly stops it
catching anything, every session keeps trusting a check that no longer works. This breaks things on purpose and
asserts each break is caught, in a throwaway worktree (your checkout is never touched), about a minute.

    python Tools/selftest.py        from trench-warfare-3d/; exit 0 = every case behaved

Cases: codemap --check passes on a clean tree, then fails on each of: a command that runs a missing Tools/ script, a
cited file that does not exist, a folder with no purpose line, an undocumented command-line flag, a test class
tasks.md never names, an agent-memory.md over its cap. port_split.py, on a small repo built here: an edit to moved
code lands in the new file, an edit to code that stayed lands in the old one, and an edit whose lines both sides
changed goes to the .rej file. health.py --lanes runs and lists this checkout.
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
    g('commit', '-qam', 'edits')
    g('checkout', '-q', 'main')
    keep = [l for l in lines if 'move' not in l]
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
    case('port_split leaves a both-sides edit in .rej and exits 1',
         code == 1 and rej.exists() and 'move2 = 20' in rej.read_text() and 'move2 = 20' not in b_new, out)


def main():
    tmp = pathlib.Path(tempfile.mkdtemp(prefix='tw-selftest-'))
    wt = tmp / 'wt'
    code, out = run(['git', 'worktree', 'add', '-q', '--detach', str(wt), 'HEAD'], REPO)
    if code:
        sys.exit('could not create a worktree: ' + out)
    try:
        # test the tree as it is on disk (the change you are about to commit): copy every changed and new file over
        changed = run(['git', 'diff', '--name-only', '--diff-filter=AMR', 'HEAD'], REPO)[1].split('\n')
        changed += run(['git', 'ls-files', '--others', '--exclude-standard'], REPO)[1].split('\n')
        for rel in filter(None, (c.strip() for c in changed)):
            (wt / rel).parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(REPO / rel, wt / rel)
        for rel in filter(None, run(['git', 'diff', '--name-only', '--diff-filter=D', 'HEAD'], REPO)[1].split('\n')):
            (wt / rel.strip()).unlink(missing_ok=True)
        run(['git', 'add', '-A'], wt)
        run(['git', '-c', 'user.name=selftest', '-c', 'user.email=selftest@local', 'commit', '-qm', 'working tree',
             '--allow-empty', '--no-verify'], wt)
        codemap_cases(wt)
        port_split_cases(tmp)
        code, out = run([sys.executable, str(HERE / 'health.py'), '--lanes'], PROJ)
        case('health.py --lanes lists this checkout', code == 0 and '(you)' in out, out)
    finally:
        run(['git', 'worktree', 'remove', '--force', str(wt)], REPO)
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)


if __name__ == '__main__':
    main()
