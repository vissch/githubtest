#!/usr/bin/env python3
"""Test the tools that keep this repo honest. Run it after changing any tool here.

WHY. The gate trusts validate.py, validate.py trusts its checks (Tools/checks/, codemap.py --check among them), and a
merge trusts port_split.py. If an edit to any of them quietly stops it catching anything, every session keeps trusting
a check that no longer works. This breaks things on purpose and asserts each break is caught, in a throwaway worktree
(your checkout is never touched), a few minutes.

    python Tools/selftest.py        from trench-warfare-3d/; exit 0 = every case behaved

Cases: codemap --check passes on a clean tree, then fails on each of: a command that runs a missing Tools/ script, a
cited file that does not exist, a folder with no purpose line, an undocumented command-line flag, a test class
tasks.md never names, an agent-memory.md over its cap. validate.py prints the two lines its callers read on a clean
tree, and each check in Tools/checks catches the break it is for, alone (--only), without hiding another check or
stopping it when it crashes; an asmdef with no references key does not take it down, a test in an
[Explicit]-only assembly that is not [Explicit] is caught, and an [Explicit] one there is left alone.
port_split.py, on a small repo built here: an edit to moved code lands in the new file, an edit to code that stayed lands in the old one, and an edit whose lines both sides
changed (or whose lines the other side changed in one of two identical copies) goes to the .rej file. health.py
--lanes runs and lists this checkout. scorecard.py keeps reporting a regression until it is fixed or accepted, and
counts an unmeasured metric as one. gate_scope.py skips a slow test module only when no changed path can reach it:
a sim file, a build input, an unknown folder, a file moved out of Sim/ and a missing integration ref all run everything.
gate.ps1, with a stand-in for Unity that writes canned results: a scoped run is green only when its results hold every
assembly it asked for, keeps its results apart and never records a tree; only the full run records one, it is no
verdict when its results miss a module the list holds, and under the stand-in its marker says so; a run before
a commit leaves the Long tests out, says how long it took and, over its budget, which tests to tag; a lane that
changes a tool gets toolcheck.py in place of validate.py. land.py runs toolcheck.py for such a lane, with or without
code in it, and refuses on a red tool test; it refuses a marker a stand-in Unity wrote; a second run still refuses
when someone pushed to the lane, the
SHOW-carries-SIM refusal names the commits, and a non-ASCII path counts as code.
Tools/aosa/land.ps1, in a throwaway git repo with stand-ins for Unity and aosa.py: a missing unity.exe and a
report holding no test both fail the land instead of passing on validate.py's exit code, a try 2 that writes no
report cannot pass on try 1's file while the env-only-twice rule still holds, a card's own change to a settings
file survives the run and the log names what it reverted, an Assets/Resources that was already there is kept,
a failed snapshot stops the land before any build, and a green run claims no landing.
"""
import pathlib
import re
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
    tests = next((proj / 'Assets/_Project/Tests').rglob('EnvAtlasTests.cs'))   # by name: a test's folder is its module
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
           lambda: (tests.parent / 'SelfTestProbeTests.cs').write_text(
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


def validate_cases(wt: pathlib.Path):
    proj = wt / 'trench-warfare-3d'
    P = proj / 'Assets/_Project'
    validate = lambda *a: run([sys.executable, 'validate.py', *a], proj)
    checks = sorted(p.stem for p in (proj / 'Tools/checks').glob('*.py'))

    code, out = validate()
    lines = out.strip().replace('\r', '').split('\n')
    case('validate passes on the clean tree, with the two lines its callers read',
         code == 0 and len(lines) == 2 and lines[0].endswith(' C# files') and ' assemblies, ' in lines[0]
         and lines[1] == 'validation OK', out)
    code, out = validate('--list')
    listed = [l.split()[0] for l in out.strip().split('\n') if l.strip()]
    case('validate --list names every file in Tools/checks, once', code == 0 and sorted(listed) == checks, out)
    code, out = validate('--only', 'nope')
    case('validate --only refuses a check that does not exist', code == 2 and 'no check named nope' in out, out)

    def expect(check, name, needle, breaker):
        breaker()
        code, out = validate('--only', check)
        case(f'validate/{check} catches {name}', code == 1 and needle in out, f'exit {code}, wanted "{needle}" in:\n{out[:1500]}')
        run(['git', 'checkout', '-q', '--', '.'], wt)
        run(['git', 'clean', '-qfd'], wt)

    def add_ref(asmdef, after, ref):
        edit(P / asmdef, f'"{after}"', f'"{after}", "{ref}"')
    new_cs = lambda rel, body: (P / rel).write_text(body)

    expect('asmdef_json', 'an asmdef that is not JSON', 'invalid JSON',
           lambda: (P / 'Presentation/VFX/TW.Presentation.VFX.asmdef').write_text('{ not json'))
    expect('asmdef_refs', 'a reference to an assembly that does not exist', 'TW.Net: unknown reference TW.Nope',
           lambda: add_ref('Net/TW.Net.asmdef', 'TW.Sim.Core', 'TW.Nope'))
    expect('sim_isolation', 'a sim assembly referencing presentation', 'TW.Sim.Core: sim assembly must not reference TW.Presentation.Core',
           lambda: add_ref('Sim/Core/TW.Sim.Core.asmdef', 'Unity.Mathematics', 'TW.Presentation.Core'))

    def cycle():
        add_ref('Sim/Core/TW.Sim.Core.asmdef', 'Unity.Mathematics', 'TW.Sim.Match')
    cycle()
    code, out = validate('--only', 'asmdef_cycles')
    case('validate/asmdef_cycles catches a cycle, and names each once rather than once per route into it',
         code == 1 and 'cycle: ' in out and out.count('cycle: ') < 20, f'exit {code}, {out.count("cycle: ")} cycle lines:\n{out[:800]}')
    run(['git', 'checkout', '-q', '--', '.'], wt)

    expect('sim_purity', 'UnityEngine under Sim/', 'UnityEngine used inside a Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nusing UnityEngine;\nnamespace TW.Sim { class Bad { } }\n'))
    expect('sim_purity', 'a non-deterministic call under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nnamespace TW.Sim { class Bad { float f = Mathf.PI; } }\n'))
    expect('phase_header', 'a file with no Phase line', 'NoHeader.cs: missing "// Phase:" header',
           lambda: new_cs('Presentation/Core/NoHeader.cs', 'namespace TW.Presentation { class NoHeader { } }\n'))
    expect('brace_balance', 'a brace left open', 'Unbalanced.cs: unbalanced braces',
           lambda: new_cs('Presentation/Core/Unbalanced.cs', '// Phase: x\nnamespace TW.Presentation { class Unbalanced {\n'))
    expect('using_namespace', 'an assembly name in a using', '`using TW.Sim.Core;` names no namespace that exists',
           lambda: new_cs('Presentation/Core/WrongUsing.cs', '// Phase: x\nusing TW.Sim.Core;\nnamespace TW.Presentation { class WrongUsing { } }\n'))
    expect('audio_volume', 'the project muted in its settings asset', 'm_Volume is 0',
           lambda: edit(proj / 'ProjectSettings/AudioManager.asset', 'm_Volume: 1', 'm_Volume: 0'))

    (P / 'Presentation/VFX/TW.Presentation.VFX.asmdef').write_text('{ "name": "TW.Presentation.VFX" }\n')
    code, out = validate()
    case('[G9] an asmdef with no "references" key does not crash validate, or blame a check for it',
         'Traceback' not in out and 'crashed (KeyError' not in out and 'assemblies, ' in out, f'exit {code}:\n{out[:800]}')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)
    test = lambda body: 'using NUnit.Framework;\n' + body + '\nnamespace TW.Tests { public class StrayTests { [Test] public void A() {} } }\n'
    T = 'Assets/_Project/Tests'
    expect('test_modules', 'a sim test left in the landing folder, and says where it goes',
           f'git mv {T}/EditMode/StrayTests.cs {T}/Sim/StrayTests.cs',
           lambda: new_cs('Tests/EditMode/StrayTests.cs', '// Phase: x\n' + test('using TW.Sim;')))
    expect('test_modules', 'a UI test left in the landing folder', f'{T}/UI/StrayTests.cs',
           lambda: new_cs('Tests/EditMode/StrayTests.cs', '// Phase: x\n' + test('using TW.Presentation;\nusing TW.UI;')))
    expect('test_modules', 'a match test left in the landing folder', f'{T}/Match/StrayTests.cs',
           lambda: new_cs('Tests/EditMode/StrayTests.cs', '// Phase: x\n' + test('using TW.Presentation;\n// LockstepSession')))
    expect('test_modules', 'the sim tests referencing an assembly the scoped gate does not watch', 'TW.Tests.Sim reaches TW.UI',
           lambda: add_ref('Tests/Sim/TW.Tests.Sim.asmdef', 'TW.Data', 'TW.UI'))
    expect('test_modules', 'a sim test reading project files nobody declared', 'StrayTests.cs: reads project files',
           lambda: new_cs('Tests/Sim/StrayTests.cs', '// Phase: x\n' + test('// var t = Resources.Load("x");')))
    expect('test_modules', 'two test files with one name', 'two test files are named MineTests.cs',
           lambda: new_cs('Tests/Show/MineTests.cs', '// Phase: x\n' + test('')))
    still = lambda attrs: ('// Phase: x\nusing NUnit.Framework;\nnamespace TW.Tests {\npublic class StillStrayTests {\n'
                           '[' + attrs + ']\npublic void A() {}\n} }\n')
    expect('test_modules', 'a test in an [Explicit]-only assembly that is not [Explicit]',
           f'{T}/Stills/StillStrayTests.cs',
           lambda: new_cs('Tests/Stills/StillStrayTests.cs', still('Test')))
    new_cs('Tests/Stills/StillStrayTests.cs', still('Test, Explicit("by name")'))
    code, out = validate('--only', 'test_modules')
    case('[TU6] an [Explicit] test in Tests/Stills is left alone', code == 0, out)
    run(['git', 'clean', '-qfd'], wt)
    wf = wt / 'docs/reference/workflow.md'
    expect('codemap_docs', 'what codemap --check finds', 'codemap: ',
           lambda: wf.write_bytes(wf.read_bytes() + b'\nSee `Presentation/Core/NoSuchFile.cs`.\n'))

    # one check must not hide or stop another
    new_cs('Presentation/Core/Unbalanced.cs', '// Phase: x\nnamespace TW.Presentation { class Unbalanced {\n')
    code, out = validate('--only', 'phase_header')
    case('validate --only runs just the checks it names (a brace error does not fail phase_header)', code == 0, out)
    (proj / 'Tools/checks/audio_volume.py').write_text("WHY = 'x'\ndef run(ctx):\n    raise RuntimeError('boom')\n")
    code, out = validate('--only', 'audio_volume,brace_balance')
    case('validate reports a check that crashes as one line, and the other checks still run',
         code == 1 and 'check audio_volume crashed (RuntimeError: boom)' in out and 'unbalanced braces' in out
         and 'Traceback' not in out, out)
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)
    (proj / 'Tools/checks/extra.py').write_text("WHY = 'x'\ndef run(ctx):\n    return []\n")
    code, out = validate('--only', 'phase_header')
    case('validate fails on a check file nobody listed in ORDER', code == 1 and 'extra.py is not in ORDER' in out, out)
    run(['git', 'clean', '-qfd'], wt)


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



def split_repo(tmp: pathlib.Path, name, lane_edit):
    """A repo whose main branch split A/Big.cs into A/Big.Moved.cs, plus a branch off the base with one edit."""
    r = tmp / name
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    big = r / 'A/Big.cs'
    lines = ['class Big', '{'] + [f'    int keep{i} = {i};' for i in range(12)] + \
            [f'    int move{i} = {i};' for i in range(12)] + ['}', '']
    big.write_text('\n'.join(lines))
    g('add', '.'); g('commit', '-qm', 'base')
    g('checkout', '-qb', 'edits')
    lane_edit(big)
    g('commit', '-qam', 'edits')
    g('checkout', '-q', 'main')
    big.write_text('\n'.join([l for l in lines if 'move' not in l]))
    (r / 'A/Big.Moved.cs').write_text('\n'.join(['partial class Big', '{'] +
                                                [f'    int move{i} = {i};' for i in range(12)] + ['}', '']))
    g('add', '.'); g('commit', '-qm', 'split')
    return r, g, big


def port_split_no_conflict_case(tmp: pathlib.Path):
    # A rebase that stops on another file leaves the split file clean. Porting it must refuse, not revert it.
    r = tmp / 'no-conflict-repo'
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    big, other = r / 'A/Big.cs', r / 'A/Other.cs'
    lines = ['class Big', '{'] + [f'    int keep{i} = {i};' for i in range(12)] + \
            [f'    int move{i} = {i};' for i in range(12)] + ['}', '']
    big.write_text('\n'.join(lines))
    other.write_text('\n'.join(['class Other', '{', '    int shared = 0;', '}', '']))
    g('add', '.'); g('commit', '-qm', 'base')
    g('checkout', '-qb', 'lane')
    edit(other, 'int shared = 0;', 'int shared = 1;')
    g('commit', '-qam', 'lane edit')
    g('checkout', '-q', 'main')
    edit(other, 'int shared = 0;', 'int shared = 2;')
    big.write_text('\n'.join([l for l in lines if 'move' not in l]))
    (r / 'A/Big.Moved.cs').write_text('\n'.join(['partial class Big', '{'] +
                                                [f'    int move{i} = {i};' for i in range(12)] + ['}', '']))
    g('add', '.'); g('commit', '-qm', 'split')
    g('checkout', '-q', 'lane')
    code, reb = g('rebase', 'main')
    edit(big, 'int keep3 = 3;', 'int keep3 = 30;')      # an unstaged edit to a file git has no conflict on
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--rebase'], r)
    case('[H1] port_split --rebase keeps the unstaged edits of a file git has no conflict on',
         'int keep3 = 30;' in big.read_text() and 'no conflict' in out and 'Traceback' not in out, reb + out)


def port_split_rerun_case(tmp: pathlib.Path):
    add = lambda big: edit(big, 'int move5 = 5;', 'int move5 = 5;\n    int added = 1;')

    # mid-rebase: the first run writes the addition into the sibling, so a second run must refuse
    r, g, big = split_repo(tmp, 'rerun-repo', add)
    g('checkout', '-qb', 'lane', 'main~1')
    add(big)
    g('commit', '-qam', 'lane edit')
    code, reb = g('rebase', 'main')
    moved = r / 'A/Big.Moved.cs'
    code1, out1 = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--rebase'], r)
    once = moved.read_text().count('int added = 1;')
    code2, out2 = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--rebase'], r)
    case('[H2] a second port_split --rebase at the same stop refuses instead of porting again',
         code1 == 0 and once == 1 and code2 != 0 and 'Big.Moved.cs' in out2
         and moved.read_text().count('int added = 1;') == 1, reb + out1 + out2)

    # --from/--to cannot refuse on the siblings, so the edit itself must be recognised as already in place
    r2, g2, big2 = split_repo(tmp, 'rerun2-repo', add)
    moved2 = r2 / 'A/Big.Moved.cs'
    code3, out3 = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--from', 'main~1', '--to', 'edits'], r2)
    code4, out4 = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--from', 'main~1', '--to', 'edits'], r2)
    case('[H2] an addition already in a target is skipped, not inserted twice',
         code3 == 0 and code4 == 0 and moved2.read_text().count('int added = 1;') == 1
         and 'already in place' in out4, out3 + out4)


def port_split_merge_case(tmp: pathlib.Path):
    # Both sides already have A/Big.Extra.cs at the merge base, so only the side that ADDED A/Big.Moved.cs is split.
    r = tmp / 'merge-repo'
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    big = r / 'A/Big.cs'
    lines = ['class Big', '{'] + [f'    int keep{i} = {i};' for i in range(12)] + \
            [f'    int move{i} = {i};' for i in range(12)] + ['}', '']
    big.write_text('\n'.join(lines))
    (r / 'A/Big.Extra.cs').write_text('\n'.join(['partial class Big', '{', '    int extra0 = 0;', '}', '']))
    g('add', '.'); g('commit', '-qm', 'base')
    g('checkout', '-qb', 'split')
    big.write_text('\n'.join([l for l in lines if 'move' not in l]))
    (r / 'A/Big.Moved.cs').write_text('\n'.join(['partial class Big', '{'] +
                                                [f'    int move{i} = {i};' for i in range(12)] + ['}', '']))
    g('add', '.'); g('commit', '-qm', 'split')
    g('checkout', '-q', 'main')
    edit(big, 'int move9 = 9;', 'int move9 = 99;')
    g('commit', '-qam', 'main edit')
    code, mrg = g('merge', 'split')
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--merge'], r)
    case('[H3] port_split --merge takes the side that added the partials, not any side that has one',
         code == 0 and 'the side you are merging in' in out
         and 'int move9 = 99;' in (r / 'A/Big.Moved.cs').read_text()
         and 'move' not in big.read_text(), mrg + out)

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

    (proj / 'Code.cs').write_text('class C { int y; }\n'); g('commit', '-qam', 'stand-in gate')
    marker.write_text('stand-in:' + head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land()
    case('[G3] land.py refuses a marker a stand-in Unity wrote (TW_GATE_UNITY)',
         code == 1 and 'stand-in' in out and head(f'origin/{integ}') != head('HEAD'), out)
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land()
    case('land.py lands that same commit once the real gate has gone green on it',
         code == 0 and head(f'origin/{integ}') == head('HEAD'), out)

    sim = proj / 'Assets/_Project/Sim'
    sim.mkdir(parents=True); (sim / 'S.cs').write_text('class S {}\n'); g('add', '.'); g('commit', '-qm', 'sim on show')
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    code, out = land()
    code2, out2 = land('--dry-run', '--carry-sim', 'owner, 2026-09-26: one session on both lanes')
    case('land.py refuses a SHOW lane carrying SIM files unless --carry-sim names the decision',
         code == 1 and 'SIM' in out and code2 == 0 and 'would run' in out2, out + out2)
    case('[G7] land.py names the SIM commits in that refusal (pathspecs are repo-root, cwd is the project)',
         'sim on show' in out, out)

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
    # land.py's own fetch must not become the lease: the old check compared origin/<lane> before and after it,
    # so the first run's fetch made them equal and the second run overwrote their commit
    code, out = land()
    theirs_kept = 'theirs' in run(['git', 'log', '--format=%s', 'origin/lane/show/t'], work)[1]
    case('[G1] land.py still refuses on a second run (its own fetch must not become the lease)',
         code == 1 and theirs_kept, out)
    # the lane check now refuses earlier, so reset the fixture: the next case is about the integration ref
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')

    g('checkout', '-q', integ); g('reset', '-q', '--hard', f'origin/{integ}')
    (work / 'docs/b.md').write_text('b\n'); g('add', '.'); g('commit', '-qm', 'someone else lands'); g('push', '-q', 'origin', integ)
    g('checkout', '-q', 'lane/show/t')
    code, out = land()
    case('land.py refuses a lane that does not contain origin integration (someone landed first)',
         code == 1 and 'rebase' in out, out)

    # the tools are checked too: a lane that changes Tools/, validate.py, gate.ps1 or .github/ runs toolcheck.py
    # (validate, selftest, every tool's own tests), also when it changes code and the gate went green on its tree
    g('fetch', '-q'); g('reset', '-q', '--hard', f'origin/{integ}')
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')
    (proj / 'Tools/x').mkdir()
    (proj / 'Tools/x/test_x.py').write_text('print("1 of 1 cases behaved")\n')
    (proj / 'Tools/checks').mkdir()
    (proj / 'Tools/checks/test_shape.py').write_text('raise SystemExit("one of validate.py\'s checks, not a test")\n')
    g('add', '.'); g('commit', '-qm', 'a tool with its test')
    code, out = land()
    case('land.py runs the tools\' own tests for a tools-only lane and lands a green one (Tools/checks holds no tests)',
         code == 0 and 'Tools/x/test_x.py' in out and 'test_shape' not in out and head(f'origin/{integ}') == head('HEAD'), out)
    (work / '.github/workflows').mkdir(parents=True)
    (work / '.github/workflows/checks.yml').write_text('name: checks\n'); g('add', '.'); g('commit', '-qm', 'ci only')
    code, out = land()
    case('land.py runs them for a lane that changes only .github/', code == 0 and 'Tools/x/test_x.py' in out, out)
    (proj / 'Tools/x/test_x.py').write_text('raise SystemExit("x is broken")\n'); g('commit', '-qam', 'break the tool')
    code, out = land()
    case('land.py refuses a tools-only lane whose tool test is red, and names the test',
         code == 1 and 'Tools/x/test_x.py' in out and head(f'origin/{integ}') != head('HEAD'), out)
    (proj / 'Code.cs').write_text('class C { int y; }\n'); g('add', '.'); g('commit', '-qm', 'and code')
    marker.write_text(head('HEAD^{tree}') + ' 2026-10-04T00:00:00\n')
    code, out = land()
    case('land.py refuses a lane that changes code and breaks a tool test, though the gate went green on its tree',
         code == 1 and 'Tools/x/test_x.py' in out and head(f'origin/{integ}') != head('HEAD'), out)

    # a non-ASCII path: unquoted (-z) it is code and needs the gate; quoted it was neither SIM nor code
    g('fetch', '-q'); g('reset', '-q', '--hard', f'origin/{integ}')
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')
    g('config', 'core.quotepath', 'true')      # git's default; say it, so the case does not read a global
    (proj / 'Assets/_Project/Presentation').mkdir(parents=True, exist_ok=True)
    (proj / 'Assets/_Project/Presentation/Café.cs').write_text('class Cafe {}\n', encoding='utf-8')
    g('add', '-A'); g('commit', '-qm', 'a non-ASCII file name')
    code, out = land()
    case('[G8] land.py sees a non-ASCII path as code and asks for the gate (no quoted path slips through)',
         code == 1 and 'gate' in out and head(f'origin/{integ}') != head('HEAD'), out)


def gate_scope_cases(tmp: pathlib.Path):
    sys.path.insert(0, str(HERE))
    import gate_scope
    P = gate_scope.P
    mods = {m: ('TW.Tests.' + m, P + 'Tests/' + m + '/') for m in ('EditMode', 'Match', 'Project', 'Show', 'Sim', 'Stills', 'UI')}
    fast = ['EditMode', 'Project', 'Show', 'UI']

    def skipped(*changed):
        ran, skip = gate_scope.scope(list(changed), mods)
        assert 'Stills' not in ran and all(f in ran for f in fast), ran   # the fast modules always run, Stills never
        return sorted(skip)

    table = [
        ('a sim file runs every module', [P + 'Sim/Core/SimWorld.cs'], []),
        ('a sim test runs the sim tests and skips the match tests', [P + 'Tests/Sim/CombatTests.cs'], ['Match']),
        ('a Presentation/Core file skips only the sim tests', [P + 'Presentation/Core/SimHost.cs'], ['Sim']),
        ('UI and docs changes skip both slow modules', [P + 'UI/HudView.cs', 'docs/reference/tasks.md'], ['Match', 'Sim']),
        ('a tool other than the gate skips both', ['trench-warfare-3d/Tools/codemap.py'], ['Match', 'Sim']),
        ('a CI workflow skips both', ['.github/workflows/checks.yml'], ['Match', 'Sim']),
        ('one sim file among many others still runs everything', [P + 'UI/HudView.cs', P + 'Net/CommandSeat.cs'], []),
        ('the .meta of a sim file counts as the file', [P + 'Sim/Core/SimWorld.cs.meta'], []),
        ('an asmdef anywhere runs everything', [P + 'UI/TW.UI.asmdef'], []),
        ('a package change runs everything', ['trench-warfare-3d/Packages/manifest.json'], []),
        ('a project setting runs everything', ['trench-warfare-3d/ProjectSettings/GraphicsSettings.asset'], []),
        ('the gate itself runs everything', ['gate.ps1'], []),
        ('a folder the rule has never heard of runs everything', ['trench-warfare-3d/Assets/Plugins/New.cs'], []),
        ('an unknown file at the repo root runs everything', ['.gitattributes'], []),
        ('an empty change set runs everything', [], []),
    ]
    for name, changed, want in table:
        got = skipped(*changed)
        case(f'gate_scope: {name}', got == want, f'skipped {got}, wanted {want}')
    ran, skip = gate_scope.scope(None, mods)
    case('gate_scope: an unknown change set runs everything', not skip and 'Sim' in ran and 'Match' in ran, str(skip))

    # against a real repo: the working tree, untracked files and all, measured from the merge-base with integration
    integ = gate_scope.INTEGRATION
    origin, work = tmp / 'scope-origin.git', tmp / 'scope-work'
    run_git = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], work)
    run(['git', 'init', '-q', '--bare', str(origin)], tmp)
    run(['git', 'clone', '-q', str(origin), str(work)], tmp)
    proj = work / 'trench-warfare-3d'
    (proj / 'Tools').mkdir(parents=True)
    for tool in ('gate_scope.py', 'land.py'):
        shutil.copy2(HERE / tool, proj / 'Tools' / tool)
    for m in ('Sim', 'Match', 'Show'):
        d = proj / 'Assets/_Project/Tests' / m
        d.mkdir(parents=True)
        (d / f'TW.Tests.{m}.asmdef').write_text('{"name": "TW.Tests.%s", "includePlatforms": ["Editor"]}' % m)
        (d / f'{m}Tests.cs').write_text('namespace TW.Tests { class %sTests { [Test] void A() {} } }\n' % m)
    for folder in ('Sim', 'UI', 'Presentation/Camera'):
        (proj / 'Assets/_Project' / folder).mkdir(parents=True)
        (proj / 'Assets/_Project' / folder / 'A.cs').write_text('class A {}\n')
    (work / '.gitignore').write_text('__pycache__/\n')   # as in the real repo: running the tool must not count as a change
    run_git('checkout', '-qb', integ); run_git('add', '.'); run_git('commit', '-qm', 'base')
    run_git('push', '-q', 'origin', integ); run_git('checkout', '-qb', 'lane/show/t')
    ask = lambda *extra: run([sys.executable, 'Tools/gate_scope.py', *extra], proj)[1]

    out = ask()
    case('gate_scope: a lane with no change runs everything', 'scoped: no' in out and 'TW.Tests.Sim' in out, out)
    (proj / 'Assets/_Project/UI/New.cs').write_text('class New {}\n')   # untracked: the working tree counts, not HEAD
    out = ask()
    case('gate_scope: an untracked UI file skips the sim and match tests and still runs the rest',
         'scoped: yes' in out and 'assemblies: TW.Tests.Show\n' in out.replace('\r', '') and 'classes: TW.Tests.ShowTests' in out, out)
    run_git('add', '.'); run_git('commit', '-qm', 'ui')
    run_git('mv', 'trench-warfare-3d/Assets/_Project/Sim/A.cs', 'trench-warfare-3d/Assets/_Project/Presentation/Camera/B.cs')
    out = ask()
    case('gate_scope: a file moved out of Sim/ still runs the sim tests (renames do not hide the old path)',
         'scoped: no' in out and 'TW.Tests.Sim' in out, out)
    run_git('reset', '-q', '--hard')
    out = ask('--modules', 'sim')
    case('gate_scope: --modules runs exactly what it names', 'assemblies: TW.Tests.Sim\n' in out.replace('\r', ''), out)
    case('gate_scope: a lane that changes no tool says so', 'tools: no' in out, out)
    for tool in ('trench-warfare-3d/Tools/new_tool.py', 'trench-warfare-3d/validate.py', 'gate.ps1', '.github/workflows/checks.yml'):
        (work / tool).parent.mkdir(parents=True, exist_ok=True)
        (work / tool).write_text('# changed\n')
        yes = 'tools: yes' in ask('--modules', 'sim') and 'tools: yes' in ask()
        (work / tool).unlink()
        case(f'gate_scope: a change to {tool} is a change to the tools, whatever is asked for', yes, out)
    code, out = run([sys.executable, 'Tools/gate_scope.py', '--modules', 'Nope'], proj)
    case('gate_scope: an unknown module name is refused', code == 2 and 'no test module named Nope' in out, out)
    run_git('update-ref', '-d', f'refs/remotes/origin/{integ}')
    out = ask()
    case('gate_scope: no integration ref to measure from runs everything', 'scoped: no' in out and 'TW.Tests.Sim' in out, out)


FAKE_UNITY = r'''"""A stand-in for `unity test`: writes the results a run would, as TW_FAKE says. For Tools/selftest.py only."""
import os, sys
a = sys.argv[1:]
print('stand-in unity ' + ' '.join(a))
mode = a[a.index('--mode') + 1]
asked = a[a.index('-assemblyNames') + 1].split(';') if '-assemblyNames' in a else []
every = ['TW.Tests.Sim', 'TW.Tests.Match', 'TW.Tests.Show', 'TW.Tests.UI', 'TW.Tests.Project', 'TW.Tests.Playground']
how = os.environ.get('TW_FAKE', 'honour')
suites = ['TW.Tests.PlayMode'] if mode == 'PlayMode' else (asked if asked and how != 'ignore' else every)
if how == 'fewer':
    suites = suites[:1]
# the real wrapper Unity puts round an unexpected log: it holds the word Assert but no assertion of the test's own
NOISE = (r"Unhandled log message: '[Error] WriteToProjectRoot failed: Sharing violation on path "
         r"C:\x\Temp\.unity-pipeline-port'. Use UnityEngine.TestTools.LogAssert.Expect")
fails = []
if how in ('fail', 'softfail'):
    fails.append(('X.B', 'Expected: 1 But was: 2'))
elif how in ('noise', 'mixedfail') and '--rerun-failed' not in a:
    fails.append(('X.B', NOISE))
    if how == 'mixedfail':
        fails.append(('X.C', 'Expected: 3 But was: 4'))
odd = []
if how == 'incon':
    odd.append(('X.I', 'Inconclusive', None, 'Assume.That failed: no bake'))
elif how == 'ignored':
    odd.append(('X.G', 'Skipped', 'Ignored', 'Assert.Ignore: no GPU here'))
failed = len(fails)
cases = ''.join(f'<test-suite type="Assembly" name="{s}.dll"><test-case fullname="{s}.A" result="Passed" duration="4.5"/></test-suite>' for s in suites)
for name, msg in fails:
    cases += (f'<test-suite type="Assembly" name="X.dll"><test-case fullname="{name}" result="Failed">'
              f'<failure><message><![CDATA[{msg}]]></message></failure></test-case></test-suite>')
for name, result, label, why in odd:
    lab = f' label="{label}"' if label else ''
    cases += (f'<test-suite type="Assembly" name="X.dll"><test-case fullname="{name}" result="{result}"{lab}>'
              f'<reason><message><![CDATA[{why}]]></message></reason></test-case></test-suite>')
nincon = sum(1 for o in odd if o[1] == 'Inconclusive')
open('test-results.xml', 'w').write(f'<test-run total="{len(suites) + failed + len(odd)}" passed="{len(suites)}" failed="{failed}" '
                                    f'skipped="{len(odd) - nincon}" inconclusive="{nincon}" '
                                    f'result="{"Failed" if failed else "Passed"}">{cases}</test-run>')
sys.exit(0 if how == 'softfail' else (8 if failed else 0))
'''


def gate_cases(wt: pathlib.Path, tmp: pathlib.Path):
    """gate.ps1's own rules, with a stand-in for Unity: what a scoped run may and may not claim."""
    fake = tmp / 'fake-unity'
    fake.mkdir()
    (fake / 'fake_unity.py').write_text(FAKE_UNITY)
    (fake / 'unity.cmd').write_text(f'@"{sys.executable}" "%~dp0fake_unity.py" %*\r\n')
    proj = wt / 'trench-warfare-3d'
    marker = pathlib.Path(run(['git', 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green'], wt)[1].strip())
    scoped, full = proj / 'test-results-EditMode-scoped.xml', proj / 'test-results-EditMode.xml'

    seconds = marker.with_name('tw-gate-edit-seconds')

    def gate(how, *args, **more):
        for f in (marker, scoped, full, seconds):
            f.unlink(missing_ok=True)
        import os
        # TW_IN_TOOLCHECK: on a lane that changes a tool the gate would run toolcheck.py, which runs this file
        env = dict(os.environ, TW_GATE_UNITY=str(fake / 'unity.cmd'), TW_FAKE=how, TW_IN_TOOLCHECK='1')
        env.update(more)
        env = {k: v for k, v in env.items() if v}
        p = subprocess.run(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', str(wt / 'gate.ps1'), *args],
                           cwd=wt, capture_output=True, env=env)
        return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')

    code, out = gate('honour', '-Module', 'Show,UI')
    case('gate: a scoped run that held what was asked is green, keeps its results apart and records no tree',
         code == 0 and scoped.exists() and not full.exists() and not marker.exists(), out)
    code, out = gate('fewer', '-Module', 'Show,UI')
    case('gate: a scoped run whose results miss an assembly it asked for is no verdict (exit 6)',
         code == 6 and 'TW.Tests.UI' in out and not marker.exists(), out)
    code, out = gate('ignore', '-Module', 'Show')
    case('gate: a selection Unity ignored ran more than asked, which is still a verdict, and it says so',
         code == 0 and 'wider than asked' in out and not marker.exists(), out)
    code, out = gate('fail', '-Module', 'Show')
    case('gate: a failed test fails a scoped run (exit 8)', code == 8 and 'FAILED X.B' in out, out)
    code, out = gate('noise', '-Module', 'Show')
    case("[G2] a failure that is only Unity's unhandled-log wrapper is rerun once, and the rerun stands",
         code == 0 and 'external pipeline noise' in out and 'EditMode-scoped-rerun' in out, out)
    code, out = gate('mixedfail', '-Module', 'Show')
    case('[G2] a real assertion beside the noise is not rerun',
         code == 8 and 'Rerunning' not in out and 'FAILED X.C' in out, out)
    code, out = gate('softfail', '-Module', 'Show')
    case('[G10] a red xml under unity exit 0 prints the failed tests',
         code == 8 and 'FAILED X.B' in out and 'though unity exited 0' in out, out)
    code, out = gate('incon', '-Module', 'Show')
    case('[G11] an Inconclusive test is named and turns the gate red',
         code == 8 and 'INCONCLUSIVE X.I' in out, out)
    code, out = gate('ignored', '-Module', 'Show')
    case('[G11] an Ignored test is named, and it alone keeps the run green',
         code == 0 and 'IGNORED X.G' in out, out)
    code, out = gate('honour', '-Module', 'Nope')
    case('gate: an unknown module runs nothing and is no verdict (exit 6)', code == 6 and 'no test module named Nope' in out, out)
    code, out = gate('honour', '-EditOnly', '-All')
    case('gate: -EditOnly -All runs every module and records no tree',
         code == 0 and full.exists() and not scoped.exists() and not marker.exists() and '-assemblyNames' not in out, out)
    case('gate: a run before a commit leaves the Long tests out, and one of every module records how long it took',
         '-testCategory !Long' in out and 'EditMode took' in out and seconds.exists() and seconds.read_text().split()[0].isdigit(), out)
    code, out = gate('honour', '-Module', 'Show')
    case('gate: a scoped run leaves them out too and records no time (it is not the whole run)',
         code == 0 and '-testCategory !Long' in out and not seconds.exists(), out)
    code, out = gate('honour', '-EditOnly', '-All', '-Long')
    case('gate: -Long runs them', code == 0 and '-testCategory' not in out and not seconds.exists(), out)
    code, out = gate('honour', '-EditOnly', '-All', TW_GATE_BUDGET='-1')
    case('gate: a run over its budget names the slowest tests it ran, to tag',
         code == 0 and 'Over the budget' in out and '4.5 s  TW.Tests.Show.A' in out, out)
    (proj / 'Tools/health.py').write_bytes((proj / 'Tools/health.py').read_bytes() + b'# a change to a tool\n')
    code, out = gate('honour', '-EditOnly', '-Plan', TW_IN_TOOLCHECK='')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    case('gate: a lane that changes a tool is checked by toolcheck.py, not validate.py alone',
         code == 0 and 'validate python Tools/toolcheck.py' in out, out)
    code, out = gate('honour')
    tree = run(['git', 'rev-parse', 'HEAD^{tree}'], wt)[1].strip()
    case('gate: only the full run records the tree it tested, for land.py, and it leaves no test out',
         code == 0 and marker.exists() and marker.read_text().split()[0].endswith(tree) and '-testCategory' not in out, out)
    case('[G3] a stand-in full run marks the tree so land.py refuses it',
         code == 0 and marker.exists() and marker.read_text().split()[0] == 'stand-in:' + tree, out)
    code, out = gate('fewer')
    case('[G4] a full run whose results miss an assembly the module list holds is no verdict and records no tree',
         code == 6 and 'TW.Tests.Sim' in out and not marker.exists(), out)
    for f in (marker, scoped, full, seconds, proj / 'test-results-PlayMode.xml', proj / 'test-results.xml'):
        f.unlink(missing_ok=True)


FAKE_AOSA_UNITY = r'''"""A stand-in for `unity test` as Tools/aosa/land.ps1 calls it: writes the report to --output as
TW_FAKE_AOSA says, and dirties a churn file the way a real build does. For Tools/selftest.py only."""
import os, sys
a = sys.argv[1:]
out = a[a.index('--output') + 1]
how = os.environ.get('TW_FAKE_AOSA', 'pass')
tries = os.path.join(os.path.dirname(out), 'aosa-tries.txt')
n = (int(open(tries).read()) if os.path.exists(tries) else 0) + 1
open(tries, 'w').write(str(n))
print('stand-in aosa unity try %d: %s' % (n, ' '.join(a)))
# a real build rewrites URP's prefilter fields: churn land.ps1 must revert
open('ProjectSettings/GraphicsSettings.asset', 'a').write('  m_churn: %d\n' % n)
ENV = "Unhandled log message: '[Error] No graphic device is available'. Use UnityEngine.TestTools.LogAssert.Expect"
step = how.split('-then-')[n - 1] if '-then-' in how else how
if step == 'nothing':
    sys.exit(6)
if step in ('envfail', 'envfail-twice'):
    open(out, 'w').write('<test-run total="1" passed="0" failed="1" result="Failed">'
                         '<test-suite type="Assembly" name="X.dll"><test-case fullname="X.E" result="Failed">'
                         '<failure><message><![CDATA[%s]]></message></failure></test-case></test-suite></test-run>' % ENV)
    sys.exit(8)
total, passed = ('0', '0') if step == 'empty' else ('1', '1')
open(out, 'w').write('<test-run total="%s" passed="%s" failed="0" result="Passed">'
                     '<test-suite type="Assembly" name="X.dll"><test-case fullname="X.A" result="Passed" duration="1"/>'
                     '</test-suite></test-run>' % (total, passed))
sys.exit(0)
'''


def aosa_land_cases(tmp: pathlib.Path):
    """Tools/aosa/land.ps1's own rules, with a stand-in for Unity and for aosa.py. The real land.ps1 never runs: the
    copy below lives in a throwaway git repo, so $PSScriptRoot makes that temp tree the project."""
    import os
    root = tmp / 'aland'
    proj = root / 'trench-warfare-3d'
    (proj / 'Tools/aosa').mkdir(parents=True)
    (proj / 'ProjectSettings').mkdir()
    (proj / 'Assets/_Project/Settings').mkdir(parents=True)
    (proj / 'Assets/_AosaLocal').mkdir(parents=True)
    shutil.copy2(PROJ / 'Tools/aosa/land.ps1', proj / 'Tools/aosa/land.ps1')
    (proj / 'validate.py').write_text('print("validation OK")\n')
    # a stand-in aosa.py: only `snapshot` is reached here, and its exit code is TW_FAKE_SNAP
    (proj / 'Tools/aosa/aosa.py').write_text('import os, sys\nprint("stand-in aosa " + " ".join(sys.argv[1:]))\n'
                                             'sys.exit(int(os.environ.get("TW_FAKE_SNAP", "0")))\n')
    (proj / 'Assets/UniversalRenderPipelineGlobalSettings.asset').write_text('urp: 1\n')
    (proj / 'Assets/_Project/Settings/TW-URP.asset').write_text('twurp: 1\n')
    (proj / 'ProjectSettings/GraphicsSettings.asset').write_text('gfx: 1\n')
    run(['git', 'init', '-q', str(root)], tmp)
    run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'add', '.'], root)
    run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qm', 'base'], root)
    (proj / 'Assets/_AosaLocal/PipelineServerOff.asset').write_text('off\n')      # untracked by design (A08)
    fake = tmp / 'fake-aosa-unity'
    fake.mkdir()
    (fake / 'fake_aosa_unity.py').write_text(FAKE_AOSA_UNITY)
    (fake / 'unity.cmd').write_text(f'@"{sys.executable}" "%~dp0fake_aosa_unity.py" %*\r\n')
    log = tmp / 'aland.log'
    churn = ['Assets/UniversalRenderPipelineGlobalSettings.asset', 'Assets/_Project/Settings/TW-URP.asset',
             'ProjectSettings/GraphicsSettings.asset']

    def land(how, *args, cli=None, **more):
        log.unlink(missing_ok=True)
        for f in list(tmp.glob('aosa-*')):
            f.unlink(missing_ok=True)
        env = dict(os.environ, TW_AOSA_UNITY=cli if cli is not None else str(fake / 'unity.cmd'),
                   TW_FAKE_AOSA=how, TEMP=str(tmp), TMP=str(tmp), TW_FAKE_SNAP='0')
        env.update(more)
        p = subprocess.run(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
                            str(proj / 'Tools/aosa/land.ps1'), '-Log', str(log), *args],
                           cwd=proj, capture_output=True, env=env)
        text = log.read_text(encoding='utf-8', errors='replace') if log.exists() else ''
        return p.returncode, text + (p.stdout + p.stderr).decode('utf-8', 'replace')

    code, out = land('pass', '-EditOnly', '-NoBuild', cli=str(tmp / 'no-such-unity.exe'))
    case("[A2] a missing unity.exe fails the land instead of passing on validate.py's exit code",
         code == 1 and 'unity.exe not found' in out and 'gate green' not in out, out)

    code, out = land('empty', '-EditOnly', '-NoBuild')
    case('[A2] a report that holds no test is no verdict (exit 6), not a pass',
         code == 6 and 'no passed test' in out and 'gate green' not in out, out)

    code, out = land('envfail-then-nothing', '-EditOnly', '-NoBuild')
    case("[A3] a try 2 that writes no report cannot pass on try 1's file",
         code != 0 and 'gate green' not in out, out)

    code, out = land('envfail-twice', '-EditOnly', '-NoBuild')
    case("[A3] the env-only-twice rule still accepts a run whose failures are all the environment's",
         code == 0 and 'ENV-ONLY twice' in out, out)

    run(['git', 'checkout', '-q', '--', *churn], proj)   # the runs above left the stand-in's churn behind
    (proj / 'Assets/_Project/Settings/TW-URP.asset').write_text("twurp: 2   # the card's own change\n")
    code, out = land('pass', '-EditOnly', '-NoBuild')
    kept = (proj / 'Assets/_Project/Settings/TW-URP.asset').read_text()
    reverted = [l for l in out.splitlines() if 'reverted build churn:' in l]
    mine = [l for l in out.splitlines() if "kept the card's own change:" in l]
    case("[A4] a card's own change to a churn file survives the land, and the log names what it reverted",
         code == 0 and "card's own change" in kept
         and len(reverted) == 1 and reverted[0].endswith('GraphicsSettings.asset')
         and len(mine) == 1 and mine[0].endswith('TW-URP.asset'), out + '\n---\n' + kept)
    run(['git', 'checkout', '-q', '--', *churn], proj)

    res = proj / 'Assets/Resources'
    res.mkdir()
    (res / 'mine.txt').write_text('a card put this here\n')
    code, out = land('pass', '-EditOnly', '-NoBuild')
    case('[A4] an untracked Assets/Resources that was there before the run is left alone',
         code == 0 and (res / 'mine.txt').exists() and 'kept Assets/Resources' in out, out)
    shutil.rmtree(res)
    run(['git', 'checkout', '-q', '--', *churn], proj)

    code, out = land('pass', '-EditOnly', TW_FAKE_SNAP='2')
    case('[A13] a failed snapshot stops the land before any build starts',
         code == 1 and 'FAILED snapshot (2)' in out and 'build release' not in out, out)
    run(['git', 'checkout', '-q', '--', *churn], proj)

    text = (proj / 'Tools/aosa/land.ps1').read_text(encoding='utf-8')
    usage = [m.lower() for m in re.findall(r'\[-(\w+)', text.split('\n')[2])]
    pstart = text.index('param(')
    declared = [m.lower() for m in re.findall(r'\$(\w+)', text[pstart:text.index('\n', pstart)])]
    code, out = land('pass', '-EditOnly', '-NoBuild')
    case('[A15] the usage line offers only declared switches, and a green run claims no landing',
         not [u for u in usage if u not in declared] and code == 0 and 'LANDED' not in out
         and 'OK: gate green, nothing landed' in out, str(usage) + ' vs ' + str(declared) + '\n' + out)
    run(['git', 'checkout', '-q', '--', *churn], proj)


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


def main():
    run(['git', 'worktree', 'prune'], REPO)   # a run killed half way leaves its worktree registered
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
        run(['git', '-c', 'user.name=selftest', '-c', 'user.email=selftest@local', 'commit', '-qm', 'selftest: the working tree as it is on disk',
             '--allow-empty', '--no-verify'], wt)
        codemap_cases(wt)
        validate_cases(wt)
        gate_cases(wt, tmp)
        aosa_land_cases(tmp)
        port_split_cases(tmp)
        port_split_twin_case(tmp)
        port_split_no_conflict_case(tmp)
        port_split_rerun_case(tmp)
        port_split_merge_case(tmp)
        scorecard_cases()
        gate_scope_cases(tmp)
        land_cases(tmp)
        code, out = run([sys.executable, str(HERE / 'health.py'), '--lanes'], PROJ)
        case('health.py --lanes lists this checkout', code == 0 and '(you)' in out, out)
    finally:
        run(['git', 'worktree', 'remove', '--force', str(wt)], REPO)
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)


if __name__ == '__main__':
    main()
