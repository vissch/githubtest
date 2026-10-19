#!/usr/bin/env python3
"""Test the tools that keep this repo honest. Run it after changing any tool here.

WHY. The gate trusts validate.py, validate.py trusts its checks (Tools/checks/, codemap.py --check among them), and a
merge trusts port_split.py. If an edit to any of them quietly stops it catching anything, every session keeps trusting
a check that no longer works. This breaks things on purpose and asserts each break is caught, in a throwaway worktree
(your checkout is never touched), a few minutes.

    python Tools/selftest.py        from trench-warfare-3d/; exit 0 = every case behaved
    python Tools/selftest.py --only land,relay     only these groups (codemap, validate, gate, aosa_land,
                                                   port_split, scorecard, gate_scope, land, relay, health):
                                                   a land.py check in 20 s, not 130

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
file survives the run while the build's churn is really gone from the tree, an Assets/Resources that was already there is kept,
a failed snapshot stops the land before any build, and a green run claims no landing. The relay (Tools/relay): land.py and pipeline.py refuse inside a leg, git's
pre-push hook refuses a held checkout every push but its own lane and does nothing without the marker, and the
leg's command rules refuse a landing. The relay's full tests are Tools/relay/test_relay.py.
[H8] editor_lock.py, against a throwaway project folder: a free project's status and guard both exit 0, claim then
shows in status and guard still exits 0 (advisory only), and release clears it so a later probe no longer reports
it. perfcmp.py, on two hand-written reports: differing hash_start refuses (exit 1, prints WARNING) and --force
still prints the comparison table.
"""
import contextlib
import io
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import time

HERE = pathlib.Path(__file__).resolve().parent
PROJ = HERE.parent
REPO = PROJ.parent
results = []


def run(cmd, cwd):
    # [H5] scrub any inherited GIT_DIR/GIT_WORK_TREE so a destructive call (reset --hard, clean -qfd, push -f)
    # cannot be aimed at the real repo by a stray env var, and cap discovery at the temp root so a fixture whose
    # init/clone failed cannot fall through to a repo above it.
    env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
    env['GIT_CEILING_DIRECTORIES'] = tempfile.gettempdir()
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, env=env)
    return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')


def must(cmd, cwd):
    code, out = run(cmd, cwd)
    if code:
        sys.exit(f'{cmd} in {cwd} failed ({code}):\n{out}')
    return out


def out_lines(cmd, cwd):
    """The lines a git command printed on stdout, with its stderr left out: a path built from a run that also
    carried git's own warning ("LF will be replaced by CRLF ...") made the copy raise WinError 123."""
    env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
    env['GIT_CEILING_DIRECTORIES'] = tempfile.gettempdir()
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, env=env)
    return [l.strip() for l in p.stdout.decode('utf-8', 'replace').split('\n') if l.strip()]


def changed_paths(repo):
    # [H7] --no-renames on both sides: without it a rename reports only the new path, so the old file is never
    # removed from the copy of the tree under test.
    copy = out_lines(['git', 'diff', '--name-only', '--no-renames', '--diff-filter=AMR', 'HEAD'], repo)
    copy += out_lines(['git', 'ls-files', '--others', '--exclude-standard'], repo)
    delete = out_lines(['git', 'diff', '--name-only', '--no-renames', '--diff-filter=D', 'HEAD'], repo)
    return copy, delete


def snapshot_copies(repo, copy, cap=1_000_000):
    # [H20] main() used to copy every untracked file into the throwaway worktree and commit it, so untracked art
    # went into the shared object store on every run. A tracked path (it came from the diff) is always kept; an
    # untracked path over `cap` bytes is skipped.
    others = set(out_lines(['git', 'ls-files', '--others', '--exclude-standard'], repo))
    keep, skipped = [], []
    for rel in copy:
        try:
            big = rel in others and (pathlib.Path(repo) / rel).stat().st_size > cap
        except OSError:
            big = False
        (skipped if big else keep).append(rel)
    return keep, skipped


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
    # [H6] capture the snapshot's own sha before this extra commit: if the commit is refused (hook, gpgsign) the
    # cases must still test the tree that was on disk, not silently fall back to testing HEAD.
    before = run(['git', 'rev-parse', 'HEAD'], wt)[1].strip()
    must(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qam',
          f'quote it\n\nThe body says (until "{tag}" lands).'], wt)
    code, out = run(check, proj)
    case('[H16] codemap does not call a tag landed when a commit message only quotes it (and exits 0)',
         code == 0 and tag not in out, f'exit {code}: {out}')
    run(['git', 'reset', '-q', '--hard', before], wt)
    sim_lines = len((proj / 'Assets/_Project/Presentation/Core/SimHost.cs').read_text(encoding='utf-8-sig').splitlines())
    expect('a cited line one past the end of the file', 'past the end',
           lambda: wf.write_bytes(wf.read_bytes() + f'\nSee `Presentation/Core/SimHost.cs:{sim_lines + 1}`.\n'.encode()))
    expect('a tool whose name only appears inside a longer tool name', 'Tools/map.py is in neither',
           lambda: (proj / 'Tools/map.py').write_text('print(1)\n'))
    cm = wt / 'docs/reference/code-map.md'
    opener = '<!-- gen:folders -->'
    kept = 'A hand-written sentence a regenerate must never eat.'

    def double_opener():
        b = cm.read_bytes().decode('utf-8')
        cm.write_bytes(b.replace(opener + '\n', f'{opener}\n\n{kept}\n\n{opener}\n', 1).encode('utf-8'))

    # [H19] one opener twice (a hand-resolved merge conflict) used to pass --check, and a regenerate then
    # deleted everything between the two openers.
    expect('[H19] a duplicated gen opener', '2 <!-- gen:folders -->', double_opener)
    double_opener()
    code, out = run([sys.executable, 'Tools/codemap.py'], proj)
    case('[H19] a regenerate refuses a duplicated gen opener and keeps the text between the two openers',
         code != 0 and kept in cm.read_bytes().decode('utf-8'),
         f'exit {code}: {out}\nkept sentence present: {kept in cm.read_bytes().decode("utf-8")}')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)

    mem = wt / 'docs/reference/agent-memory.md'
    expect('agent-memory.md over its cap', 'agent-memory.md is',
           lambda: mem.write_bytes(mem.read_bytes() + b''.join(b'- filler %d\n' % i for i in range(200))))

    # [H19.1] write_all used to check and write each doc in the same loop iteration: a bad marker in a later doc
    # left the earlier, already-rewritten docs on disk even though the regenerate as a whole failed.
    cm_text = cm.read_bytes().decode('utf-8')
    cm.write_bytes(cm_text.replace(opener + '\n', f'{opener}\n{kept}\n', 1).encode('utf-8'))
    wf2 = wt / 'docs/reference/workflow.md'
    wf2_opener = '<!-- gen:eval-api -->'
    wf2_text = wf2.read_text(encoding='utf-8')
    wf2.write_bytes(wf2_text.replace(wf2_opener + '\n', wf2_opener + '\n\n' + wf2_opener + '\n', 1).encode('utf-8'))
    code, out = run([sys.executable, 'Tools/codemap.py'], proj)
    case('[H19.1] a regenerate writes every doc or none: a bad marker in the last doc leaves the first one alone',
         code != 0 and 'wrote' not in out and kept in cm.read_bytes().decode('utf-8') and 'workflow.md' in out,
         f'exit {code}: {out}')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)


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
    add_ref('Net/TW.Net.asmdef', 'TW.Sim.Core', 'GUID:0123456789abcdef0123456789abcdef')
    code, out = validate('--only', 'asmdef_refs')
    case('[G6] a GUID reference in a TW asmdef is caught by asmdef_refs', code == 1 and 'GUID reference' in out, out)
    run(['git', 'checkout', '-q', '--', '.'], wt)
    run(['git', 'clean', '-qfd'], wt)
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
    expect('sim_purity', '[G5] a non-deterministic call under Net/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Net/Bad.cs', '// Phase: x\nnamespace TW.Net { class Bad { float f = System.Math.PI; } }\n'))
    expect('sim_purity', '[G5] a non-deterministic call under Data/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Data/Bad.cs', '// Phase: x\nnamespace TW.Data { class Bad { double d = System.Math.PI; } }\n'))
    expect('sim_purity', '[G5] a raw math.* call under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nnamespace TW.Sim { class Bad { float f = math.sin(1f); } }\n'))
    expect('sim_purity', '[G5] a DateTime. read under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nnamespace TW.Sim { class Bad { object t = System.DateTime.UtcNow; } }\n'))
    expect('sim_purity', '[G5] a Stopwatch under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nnamespace TW.Sim { class Bad { object s = new System.Diagnostics.Stopwatch(); } }\n'))
    expect('sim_purity', '[G5] a System.Linq using under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nusing System.Linq;\nnamespace TW.Sim { class Bad { } }\n'))
    expect('sim_purity', '[G5] an unseeded new Random() under Sim/', 'non-deterministic API in Sim assembly',
           lambda: new_cs('Sim/Core/Bad.cs', '// Phase: x\nnamespace TW.Sim { class Bad { object r = new Random(); } }\n'))
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
    must(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'init', '-q', '-b', 'main'], r)
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

def port_split_no_merge_case(tmp: pathlib.Path):
    # [H20] --merge in a repo with no MERGE_HEAD used to raise CalledProcessError (a git traceback) before the
    # helpful "cannot tell which side split" message could ever be printed.
    r = tmp / 'no-merge-repo'
    (r / 'A').mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    g('init', '-q', '-b', 'main')
    (r / 'A/Big.cs').write_text('class Big\n{\n    int keep0 = 0;\n}\n')
    g('add', '.'); g('commit', '-qm', 'base')
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--merge'], r)
    case('[H20] port_split --merge outside a merge says so instead of a traceback',
         code == 1 and 'not in a merge' in out and 'Traceback' not in out, f'[H20] exit {code}: {out}')


def port_split_unrelated_merge_case(tmp: pathlib.Path):
    # [H20.3] MERGE_HEAD exists (we are really in a merge) but the two histories share no common ancestor, so
    # merge-base itself fails. That used to fall into the same except as "no MERGE_HEAD" and print "not in a
    # merge", which is simply wrong: we are in a merge.
    r = tmp / 'unrelated-merge-repo'
    r.mkdir(parents=True)
    g = lambda *a: run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *a], r)
    (r / 'A').mkdir()
    g('init', '-q', '-b', 'main')
    (r / 'A/Big.cs').write_text('class Big\n{\n    int keep0 = 0;\n}\n')
    g('add', '.'); g('commit', '-qm', 'main root')
    g('checkout', '--orphan', 'other')
    (r / 'A/Big.cs').write_text('class Big\n{\n    int keep0 = 1;\n}\n')
    g('add', '.'); g('commit', '-qm', 'other root')
    g('checkout', '-q', 'main')
    code, mrg = g('merge', '--allow-unrelated-histories', '--no-commit', 'other')
    code, out = run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs', '--merge'], r)
    case('[H20.3] port_split --merge in a merge of unrelated histories does not claim "not in a merge"',
         code != 0 and 'not in a merge' not in out and 'common ancestor' in out and 'Traceback' not in out,
         f'exit {code}: {mrg}\n{out}')


def skill_docs_cases(tmp: pathlib.Path):
    # [K6s.1] / [K5s.1] exercise test_skill_docs.py's K6s and K5s checks against a small fake repo, rather than
    # against the lane's real .claude/skills, so the case stays stable no matter what the real docs say.
    root = tmp / 'skilldocs-repo'
    skills = root / '.claude/skills'
    (skills / 'pipeline').mkdir(parents=True)
    (skills / 'tw-master').mkdir(parents=True)
    tools = root / 'trench-warfare-3d/Tools'
    (tools / 'pipeline').mkdir(parents=True)
    (tools / 'pipeline/stations.json').write_text(json.dumps({'laptop': {'hosts': [], 'caps': []}}))
    shutil.copy2(HERE / 'test_skill_docs.py', tools / 'test_skill_docs.py')

    def run_case(name, method):
        return run([sys.executable, '-m', 'unittest', f'test_skill_docs.SkillDocsTest.{method}'], tools)

    pipeline_doc = skills / 'pipeline/SKILL.md'
    master_doc = skills / 'tw-master/SKILL.md'

    pipeline_doc.write_text(
        'See the role skill under .claude/skills/ and .claude/agents/tw-critic.md.\n'
        'TW_STATION=laptop\n'
    )
    master_doc.write_text('TW_STATION=laptop\n')
    (root / '.claude/agents').mkdir(parents=True)
    (root / '.claude/agents/tw-critic.md').write_text('x\n')
    code, out = run_case(
        'ok', 'test_k6s_pipeline_skill_cites_no_missing_claude_path')
    case('[K6s.1] a skill doc may cite .claude/agents when that folder exists',
         code == 0, f'exit {code}: {out}')

    pipeline_doc.write_text(
        'See the role skill under .claude/skills/ and .claude/ghosts/x.md.\n'
        'TW_STATION=laptop\n'
    )
    code, out = run_case(
        'ghost', 'test_k6s_pipeline_skill_cites_no_missing_claude_path')
    case('[K6s.1] a cited .claude/<dir> that is not there is still caught',
         code != 0 and 'does not exist' in out, f'exit {code}: {out}')

    pipeline_doc.write_text('See the role skill under .claude/skills/.\nTW_STATION and laptop are both used here.\n')
    master_doc.write_text('TW_STATION and laptop are both used here, on different lines.\n')
    code, out = run_case(
        'words', 'test_k5s_docs_say_to_set_tw_station_on_a_hostless_station')
    case('[K5s.1] the TW_STATION check refuses a doc that only mentions the words',
         code != 0, f'exit {code}: {out}')


def land_cases(tmp: pathlib.Path):
    # A bare origin with the integration branch, and a clone with a lane on it. land.py runs from the clone.
    integ = 'claude/trench-warfare-2d-3d-plan-idt7lf'
    origin, work = tmp / 'origin.git', tmp / 'land-work'
    must(['git', 'init', '-q', '--bare', str(origin)], tmp)
    must(['git', 'clone', '-q', str(origin), str(work)], tmp)
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
    case('[H16] land.py refuses code that no green full gate tested, saying so',
         code == 1 and 'no full gate has gone green' in out and head(f'origin/{integ}') != head('HEAD'), out)
    marker = pathlib.Path(run(['git', 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green'], work)[1].strip())
    marker.write_text(head('HEAD^{tree}') + ' 2026-09-27T00:00:00\n')
    (proj / 'Code.cs').write_text('class C { int x; }\n'); g('commit', '-qam', 'one more')
    code, out = land()
    case('[H16] land.py refuses when HEAD is not the tree the gate tested (one more commit after the gate)',
         code == 1 and 'tested tree' in out and 'Run the full gate on this commit' in out, out)
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

    # [U068.3] a second writer commits on the lane while toolcheck.py runs: land.py checked one commit and used
    # to push the name HEAD, which by then was that newer, untested commit.
    g('fetch', '-q'); g('reset', '-q', '--hard', f'origin/{integ}')
    g('push', '-q', '-f', 'origin', 'HEAD:refs/heads/lane/show/t'); g('fetch', '-q')
    (proj / 'Tools/x/test_x.py').write_text(
        'import pathlib, subprocess\n'
        'root = pathlib.Path(__file__).resolve().parents[3]\n'
        '(root / "docs" / "sneak.md").write_text("a second writer\\n")\n'
        'id = ["-c", "user.name=t", "-c", "user.email=t@t"]\n'
        'subprocess.run(["git", *id, "add", "docs/sneak.md"], cwd=root)\n'
        'subprocess.run(["git", *id, "commit", "-qm", "second writer"], cwd=root)\n'
        'print("1 of 1 cases behaved")\n')
    g('add', '-A'); g('commit', '-qm', 'a tool test that commits while it runs')
    start, before = head('HEAD'), head(f'origin/{integ}')
    code, out = land()
    sneak, landed = head('HEAD'), head(f'origin/{integ}')
    case('[U068.3] land.py pushes the commit it checked, not a newer HEAD a second writer made during toolcheck',
         sneak != start and landed in (start, before), f'start {start} sneak {sneak} landed {landed}\n{out}')


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
if how == 'dirty':
    # [H8] simulates a tracked project file changing mid-run; the caller restores it with `git checkout -- .`
    with open('Tools/health.py', 'a') as f:
        f.write('# dirty (TW_FAKE=dirty, for Tools/selftest.py only)\n')
suites = ['TW.Tests.PlayMode'] if mode == 'PlayMode' else (asked if asked and how != 'ignore' else every)
if how == 'fewer':
    suites = suites[:1]
elif how == 'empty':
    suites = []
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
elif how == 'playfail' and mode == 'PlayMode':
    fails.append(('X.B', 'Expected: 1 But was: 2'))
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

    code, out = gate('empty', '-EditOnly', '-All')
    case('[H8] a run that found no tests is no verdict (exit 6), not a pass',
         code == 6 and 'ran no tests' in out, out)
    code, out = gate('playfail')
    case('[H8] EditMode green but PlayMode failing is exit 8, printed, and records no tree',
         code == 8 and 'FAILED X.B' in out and not marker.exists(), out)
    code, out = gate('honour', TW_GATE_UNITY=str(fake / 'missing.cmd'))
    case('[H8] TW_GATE_UNITY pointing at a missing file is exit 1, "unity.exe not found"',
         code == 1 and 'unity.exe not found' in out, out)
    (proj / 'Tools/editor_lock.py').write_text('import sys\nsys.exit(1)\n')
    code, out = gate('honour', '-EditOnly', '-All')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    case('[H8] editor_lock.py refusing the guard makes the gate exit 3, "the project is held"',
         code == 3 and 'project is held' in out.lower(), out)
    (proj / 'validate.py').write_text('raise SystemExit(1)\n')
    code, out = gate('honour', '-EditOnly', '-All')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    case('[H8] validate.py failing makes the gate exit 5',
         code == 5 and 'validate.py failed (exit 5)' in out, out)
    code, out = gate('dirty')
    run(['git', 'checkout', '-q', '--', '.'], wt)
    case('[H8] a tracked file changing during the run leaves the gate green but records no tree',
         code == 0 and not marker.exists() and 'changed during the run' in out, out)

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
    must(['git', 'init', '-q', str(root)], tmp)
    run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'add', '.'], root)
    must(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qm', 'base'], root)
    (proj / 'Assets/_AosaLocal/PipelineServerOff.asset').write_text('off\n')      # untracked by design (A08)
    fake = tmp / 'fake-aosa-unity'
    fake.mkdir()
    (fake / 'fake_aosa_unity.py').write_text(FAKE_AOSA_UNITY)
    (fake / 'unity.cmd').write_text(f'@"{sys.executable}" "%~dp0fake_aosa_unity.py" %*\r\n')
    # A4b: a stand-in for the Unity editor land.ps1 builds with. It only leaves a marker and fails, so a
    # regression that reaches a build in a selftest cannot start the real editor.
    (fake / 'unity-editor.cmd').write_text('@echo started >> "%TEMP%\\aosa-editor-started.txt"'
                                           + '\r\n@exit /b 1\r\n')
    log = tmp / 'aland.log'
    churn = ['Assets/UniversalRenderPipelineGlobalSettings.asset', 'Assets/_Project/Settings/TW-URP.asset',
             'ProjectSettings/GraphicsSettings.asset']

    def land(how, *args, cli=None, **more):
        log.unlink(missing_ok=True)
        for f in list(tmp.glob('aosa-*')):
            f.unlink(missing_ok=True)
        env = dict(os.environ, TW_AOSA_UNITY=cli if cli is not None else str(fake / 'unity.cmd'),
                   TW_AOSA_EDITOR=str(fake / 'unity-editor.cmd'),
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
    gfx = (proj / 'ProjectSettings/GraphicsSettings.asset').read_text()
    reverted = [l for l in out.splitlines() if 'reverted build churn:' in l]
    mine = [l for l in out.splitlines() if "kept the card's own change:" in l]
    case("[A4b] the build's churn is really gone from the tree and the card's own change is kept",
         code == 0 and gfx == 'gfx: 1\n' and 'm_churn' not in gfx and "card's own change" in kept
         and len(reverted) == 1 and reverted[0].endswith('GraphicsSettings.asset')
         and len(mine) == 1 and mine[0].endswith('TW-URP.asset'),
         out + '\n---\n' + kept + '\n---\n' + gfx)
    text = (proj / 'Tools/aosa/land.ps1').read_text(encoding='utf-8')
    case("[A4b] the churn revert reads git's exit code instead of hiding it",
         'git checkout -- $theirs 2>$null' not in text and 'FAILED to revert build churn' in text, text)
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
    case('[A13] [A4b] a failed snapshot stops the land before any build starts, and no editor is startable there',
         code == 1 and 'FAILED snapshot (2)' in out and 'build release' not in out
         and not (tmp / 'aosa-editor-started.txt').exists(), out)
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
    g('commit', '-q', '--amend', '-m', 'rewritten')          # the lane's history rewritten, as after a rebase
    code, out = g('push', '--force', '-q', 'origin', 'lane/show/t')
    case('the hook lets a forced push of the leg\'s own lane through', code == 0, out)
    code, out = g('push', '--force', '-q', 'origin', 'HEAD:refs/heads/' + gitio.INTEGRATION)
    case('the hook still refuses a forced push to the trunk', code != 0 and 'relay' in out, out)
    code, out = g('push', '--force', '-q', 'origin', 'HEAD:refs/heads/lane/show/other')
    case('the hook still refuses a forced push to another lane', code != 0 and 'relay' in out, out)
    marker.unlink()
    code, out = g('push', '-q', 'origin', 'HEAD:refs/heads/other')
    case('without the marker the hook does nothing', code == 0, out)

    leg = {'lane': 'lane/show/t', 'worktree': str(work), 'board': str(board)}
    case('the leg\'s command rules refuse land.py and a push to main, and pass a push of its own lane',
         bool(cmdrules.never('python Tools/land.py', leg)) and bool(cmdrules.never('git push origin main', leg))
         and cmdrules.never('git push origin lane/show/t', leg) is None
         and cmdrules.never('git push --force origin lane/show/t', leg) is None
         and bool(cmdrules.never('git push --force origin main', leg)))


def editor_lock_cases(tmp: pathlib.Path):
    # [H8] a throwaway project folder, never the real checkout: editor_lock.py finds it by Assets/_Project.
    proj = tmp / 'editor-lock-proj'
    (proj / 'Assets/_Project').mkdir(parents=True)
    (proj / 'Temp').mkdir()
    env = dict(os.environ, LOCALAPPDATA=str(tmp / 'localappdata'))
    tool = lambda *a: subprocess.run([sys.executable, str(HERE / 'editor_lock.py'), *a], cwd=proj,
                                      capture_output=True, env=env)
    r = tool('status')
    case('editor_lock.py status exits 0 on a free project (no lockfile)',
         r.returncode == 0 and 'FREE' in r.stdout.decode(), r.stdout.decode() + r.stderr.decode())
    r = tool('guard')
    case('editor_lock.py guard is silent and exits 0 on a free project', r.returncode == 0 and not r.stdout, r.stderr.decode())
    r = tool('claim', 'selftest-who', '--minutes', '5', '--why', 'selftest case')
    case('editor_lock.py claim exits 0 and reports who claimed it', r.returncode == 0 and 'selftest-who' in r.stdout.decode(), r.stdout.decode())
    r = tool('status')
    case('editor_lock.py status shows the claim, though the project is still free (advisory only)',
         r.returncode == 0 and 'selftest-who' in r.stdout.decode() and 'FREE' in r.stdout.decode(), r.stdout.decode())
    r = tool('release', 'selftest-who')
    case('editor_lock.py release of your own claim exits 0', r.returncode == 0, r.stdout.decode() + r.stderr.decode())
    r = tool('status')
    case('editor_lock.py status no longer shows a released claim', 'selftest-who' not in r.stdout.decode(), r.stdout.decode())

    # [H11] two different checkouts on the same machine (same LOCALAPPDATA) must not share one slot file.
    proj_b = tmp / 'editor-lock-proj-b'
    (proj_b / 'Assets/_Project').mkdir(parents=True)
    (proj_b / 'Temp').mkdir()
    tool_b = lambda *a: subprocess.run([sys.executable, str(HERE / 'editor_lock.py'), *a], cwd=proj_b,
                                        capture_output=True, env=env)
    r = tool('claim', 'session-a', '--minutes', '5', '--why', 'proj a')
    case('editor_lock.py claim on proj_a as session-a exits 0', r.returncode == 0, r.stdout.decode() + r.stderr.decode())
    r = tool_b('claim', 'session-b', '--minutes', '5', '--why', 'proj b')
    case('[H11] editor_lock claims on two different checkouts do not collide',
         r.returncode == 0 and 'session-b' in r.stdout.decode(), r.stdout.decode() + r.stderr.decode())
    tool('release', 'session-a')
    tool_b('release', 'session-b')


def tw_cases(tmp: pathlib.Path, tw_path: pathlib.Path):
    # [H13] a fake `unity` on PATH ahead of the real one, so `tw build` never touches a real editor.
    def make_fake(compilation_failed, console_message):
        fakebin = tmp / ('fakebin-fail' if compilation_failed else 'fakebin-ok')
        fakebin.mkdir(exist_ok=True)
        script = fakebin / 'unity'
        body = '''#!/usr/bin/env bash
# unity --no-banner cmd <subcommand> ...: the subcommand we care about is the 3rd positional argument.
case "$2 $3" in
  "cmd recompile") echo '{"status": "ok"}' ;;
  "cmd recompile_status") echo '{"status": "completed"}' ;;
  "cmd console_status") echo '{"compilationFailed": %s}' ;;
  # [H15] a clock that answers 0 first and 9.9 after: `tw stepto` is done on its first round.
  "cmd eval") n=$(cat "$0.calls" 2>/dev/null || echo 0); echo $((n+1)) > "$0.calls"
              if [ "$n" = "0" ]; then echo '{"result": 0.0}'; else echo '{"result": 9.9}'; fi ;;
  "cmd console") %s ;;
  *) echo '{}' ;;
esac
''' % ('true' if compilation_failed else 'false',
       'echo \'{"message": "boom"}\'' if console_message else 'true')
        script.write_text(body)
        posix = str(fakebin / 'unity').replace('\\', '/')
        # NTFS carries no real exec bit; bash itself has to be asked to set the one it checks.
        subprocess.run(['bash', '-c', 'chmod +x "%s"' % posix], cwd=tmp)
        return fakebin

    def run_tw_build(fakebin):
        # a real unity.exe at %LOCALAPPDATA%/unity/bin would otherwise be put ahead of the fake one on PATH.
        env = dict(os.environ, PATH=str(fakebin) + os.pathsep + os.environ.get('PATH', ''),
                   LOCALAPPDATA=str(tmp / 'no-such-localappdata'))
        p = subprocess.run(['bash', str(tw_path), 'build'], cwd=tmp, capture_output=True, env=env)
        return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')

    def run_tw(fakebin, *args):
        env = dict(os.environ, PATH=str(fakebin) + os.pathsep + os.environ.get('PATH', ''),
                   LOCALAPPDATA=str(tmp / 'no-such-localappdata'))
        p = subprocess.run(['bash', str(tw_path), *args], cwd=tmp, capture_output=True, env=env)
        return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')

    fail_bin = make_fake(compilation_failed=True, console_message=True)
    code, out = run_tw_build(fail_bin)
    case('[H13] tw build exits non-zero when compilation failed', code != 0, out)

    ok_bin = make_fake(compilation_failed=False, console_message=False)
    code, out = run_tw_build(ok_bin)
    case('[H13] tw build still exits 0 on a healthy build', code == 0, out)

    # [H15] a raw newline inside the awk string made awk die with "unterminated string" on every success.
    code, out = run_tw(ok_bin, 'stepto', '1.0')
    case('[H15] tw stepto prints how far game time advanced without an awk error',
         code == 0 and 'advanced' in out and 'awk' not in out and 'unterminated' not in out, f'exit {code}: {out}')

    # [T3.2.1] the refusal message of `tw stepabs` had the same raw newline inside its awk program as [H15]: awk
    # died with "unterminated string", so a shot taken on a late clock was refused without ever saying why.
    code, out = run_tw(ok_bin, 'stepabs', '1.0')
    case('[T3.2.1] tw stepabs says why it refuses a clock already past the mark, without an awk error',
         code == 1 and 'clock already at' in out and 'unterminated' not in out and 'awk:' not in out,
         f'exit {code}: {out}')

    # [H20] the header NOTE used to claim `tw shot` passes an absolute path outside Assets; the shot case
    # passes the relative _shots/<name>.png and moves the PNG out afterwards.
    header = tw_path.read_text(encoding='utf-8').splitlines()[:20]
    header = '\n'.join(header)
    case("[H20] tw's header does not claim shot passes an absolute path",
         'absolute path' not in header and '_shots' in header, header)


def perfcmp_cases(tmp: pathlib.Path):
    # [H8] two hand-written reports, shaped like perfcmp.py's own reader expects.
    def report(hash_start, label):
        return {
            'run': {'label': label, 'build': 'editor'},
            'window': {'hash_start': hash_start, 'tick_start': 0, 'tick_end': 100, 'alive_start': 10, 'alive_end': 10,
                       'fps_mean': 60.0, 'gc_collections': 0, 'hitches_over_33ms': []},
            'series': {k: {'p50': 1.0, 'p95': 2.0, 'p99': 3.0} for k in
                       ['cpu_frame_ms', 'main_ms', 'main_ms_tick_frames', 'main_ms_idle_frames', 'render_ms',
                        'gpu_ms', 'draw_calls', 'setpass', 'gc_bytes', 'gc_count', 'vat_vertices', 'vat_shadows_on']},
            'per_tick_ms': {},
        }
    a = tmp / 'perf-a.json'; b = tmp / 'perf-b.json'
    a.write_text(json.dumps(report('h1', 'before')))
    b.write_text(json.dumps(report('h2', 'after')))
    r = subprocess.run([sys.executable, str(HERE / 'perfcmp.py'), str(a), str(b)], capture_output=True)
    case('perfcmp.py refuses two reports with a different hash_start', r.returncode == 1 and b'WARNING' in r.stdout + r.stderr,
         (r.stdout + r.stderr).decode())
    r = subprocess.run([sys.executable, str(HERE / 'perfcmp.py'), str(a), str(b), '--force'], capture_output=True)
    case('perfcmp.py --force still prints the table despite the hash_start mismatch',
         r.returncode == 0 and b'before' in r.stdout and b'after' in r.stdout, (r.stdout + r.stderr).decode())

    # [H20] perfcmp used to read r['window'] straight off the json, so any other json died with a traceback.
    bad = tmp / 'perf-not-a-report.json'
    nope = report('h1', 'nope'); del nope['window']
    bad.write_text(json.dumps(nope))
    r = subprocess.run([sys.executable, str(HERE / 'perfcmp.py'), str(a), str(bad)], capture_output=True)
    out = (r.stdout + r.stderr).decode()
    case('[H20] perfcmp names a report that is not a PerfBench report instead of a traceback',
         r.returncode == 1 and 'not a PerfBench report' in out and 'Traceback' not in out,
         f'[H20] exit {r.returncode}: {out}')

    # [H20.2] json.load's own error (not valid JSON at all) and a report whose per_tick_ms is missing or not a
    # dict both used to reach the same lines and traceback; both are now named instead.
    not_json = tmp / 'perf-not-json.json'
    not_json.write_text('not json at all')
    r = subprocess.run([sys.executable, str(HERE / 'perfcmp.py'), str(a), str(not_json)], capture_output=True)
    out = (r.stdout + r.stderr).decode()
    case('[H20.2] perfcmp names a file that is not JSON instead of a traceback',
         r.returncode == 1 and 'perf-not-json.json' in out and 'JSON' in out and 'Traceback' not in out,
         f'[H20.2] exit {r.returncode}: {out}')

    no_ticks = tmp / 'perf-no-per-tick.json'
    no_ticks_report = report('h1', 'no-ticks')
    no_ticks_report['per_tick_ms'] = None
    no_ticks.write_text(json.dumps(no_ticks_report))
    r = subprocess.run([sys.executable, str(HERE / 'perfcmp.py'), str(no_ticks), str(no_ticks)], capture_output=True)
    out = (r.stdout + r.stderr).decode()
    case('[H20.2] perfcmp names a report whose per_tick_ms is missing instead of a traceback',
         r.returncode == 0 and 'no per_tick_ms' in out and 'Traceback' not in out,
         f'[H20.2] exit {r.returncode}: {out}')


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


LONG = chr(92) * 2 + '?' + chr(92)   # Windows' prefix for a path over 260 characters


def scrub(path):
    """Delete a folder of ours for real. git writes its objects read-only, and on Windows rmtree stops at the first
    of them: with errors ignored every run left its folder behind (151 of them, 770 MB, by 2026-10-07). A long path
    there needs the long-path prefix (LONG)."""
    import os, stat
    p = str(path)
    if os.name == 'nt' and not p.startswith(LONG):
        p = LONG + os.path.abspath(p)

    def writable(fn, target, _exc):
        try:
            os.chmod(target, stat.S_IWRITE)
            fn(target)
        except OSError:
            pass
    shutil.rmtree(p, onerror=writable)


rmtree_hard = scrub        # one deleter under two names (test_selftest.py reads both)


STALE_HOURS = 6   # no run takes an hour: a folder this old is a killed run's, or one from before scrub()


def sweep_stale(repo):
    """The folders older runs left in the temp folder, and the worktrees a killed run left registered (one with a
    broken index made git's own upkeep fail on every fetch). [H4] prune only forgets a worktree record whose folder
    is already gone, so the stale folders go first."""
    import time
    root = pathlib.Path(tempfile.gettempdir())
    for d in root.glob('tw-selftest-*'):
        try:
            if time.time() - d.stat().st_mtime < STALE_HOURS * 3600:
                continue
        except OSError:
            continue
        if (d / 'wt').exists():
            run(['git', 'worktree', 'remove', '--force', str(d / 'wt')], repo)
        scrub(d)
    run(['git', 'worktree', 'prune'], repo)


GROUPS = ('codemap', 'validate', 'gate', 'aosa_land', 'port_split', 'scorecard', 'gate_scope', 'land', 'relay',
          'editor_lock', 'perfcmp', 'health')
NEEDS_WORKTREE = ('codemap', 'validate', 'gate', 'health')   # these groups test a copy of this checkout


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
    sweep_stale(REPO)   # a run killed half way leaves its temp folder and worktree registered
    tmp = pathlib.Path(tempfile.mkdtemp(prefix='tw-selftest-'))
    wt = tmp / 'wt'
    try:
        if any(g in only for g in NEEDS_WORKTREE):
            run(['git', 'worktree', 'prune'], REPO)   # a run killed half way leaves its worktree registered
            code, out = run(['git', 'worktree', 'add', '-q', '--detach', str(wt), 'HEAD'], REPO)
            if code:
                sys.exit('could not create a worktree: ' + out)
            # test the tree as it is on disk (the change you are about to commit): copy every changed and new file over
            copy, delete = changed_paths(REPO)
            copy, skipped = snapshot_copies(REPO, copy)
            if skipped:
                print('snapshot skipped %d large untracked file(s): %s' % (len(skipped), ', '.join(skipped[:5])))
            for rel in copy:
                (wt / rel).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(REPO / rel, wt / rel)
            for rel in delete:
                (wt / rel).unlink(missing_ok=True)
            run(['git', 'add', '-A'], wt)
            must(['git', '-c', 'user.name=selftest', '-c', 'user.email=selftest@local', 'commit', '-qm', 'selftest: the working tree as it is on disk',
                  '--allow-empty', '--no-verify'], wt)
        if 'codemap' in only:
            codemap_cases(wt)
        if 'validate' in only:
            validate_cases(wt)
        if 'gate' in only:
            gate_cases(wt, tmp)
        if 'aosa_land' in only:
            aosa_land_cases(tmp)
        if 'port_split' in only:
            port_split_cases(tmp)
            port_split_twin_case(tmp)
            port_split_no_conflict_case(tmp)
            port_split_rerun_case(tmp)
            port_split_merge_case(tmp)
        if 'scorecard' in only:
            port_split_no_merge_case(tmp)
            port_split_unrelated_merge_case(tmp)
            skill_docs_cases(tmp)
            scorecard_cases()
        if 'gate_scope' in only:
            gate_scope_cases(tmp)
        if 'land' in only:
            land_cases(tmp)
        if 'relay' in only:
            relay_cases(tmp)
        if 'editor_lock' in only:
            editor_lock_cases(tmp)
        if 'perfcmp' in only:
            perfcmp_cases(tmp)
        if 'health' in only:
            code, out = run([sys.executable, str(HERE / 'health.py'), '--lanes'], PROJ)
            case('health.py --lanes lists this checkout', code == 0 and '(you)' in out, out)

            wt_proj = wt / 'trench-warfare-3d'
            _, index_path = run(['git', 'rev-parse', '--path-format=absolute', '--git-path', 'index'], wt)
            lock_path = pathlib.Path(index_path.strip() + '.lock')
            lock_path.write_text('')
            try:
                code, out = run([sys.executable, str(wt_proj / 'Tools/health.py'), '--lanes'], wt_proj)
            finally:
                lock_path.unlink(missing_ok=True)
            # Not [H9]: git's cmd_status takes the index lock with LOCK_NONE, so an existing index.lock changes
            # neither its output nor its exit code. This is a smoke test; the [H9] case below reads the argv.
            case('health.py --lanes does not choke on another checkout\'s index.lock',
                 'fatal' not in out.lower() and 'Unable to create' not in out, out)
            case('[H10] health.py --lanes marks the detached checkout running it as (you), no git error',
                 code == 0 and '(you)' in out and 'fatal' not in out.lower(), out)


            # [H18] the real fault (a domain reload drops the sampler) only happens inside Unity, so this reads the
            # script's text: the placeholder it writes must carry a UtcNow stamp, so a stale "running" is visible.
            budget_text = (HERE.parent / 'AgentScripts/aosa_budget.cs').read_text(encoding='utf-8')
            placeholder = re.search(r'SetString\(ns \+ "budget", "running(.*?)\);', budget_text, re.S)
            case('[H18] aosa_budget marks its running value with a time, so a lost sampler is visible',
                 placeholder is not None and 'DateTime.UtcNow' in budget_text
                 and 'deadline' in (placeholder.group(1) if placeholder else ''),
                 '[H18] placeholder: ' + (placeholder.group(0) if placeholder else 'no SetString(ns + "budget", "running'))

            # [H20] health.py probed console_status with a raw subprocess.run(timeout=60): a hung CLI raised
            # TimeoutExpired and health.py died with a traceback instead of printing the editor line.
            sys.path.insert(0, str(HERE))
            import health
            hang = tmp / 'hang.py'
            hang.write_text('import time\ntime.sleep(5)\n')
            try:
                failed, note = health.compile_failed([sys.executable, str(hang)], timeout=1)
                ok, detail = (failed is False and 'timed out' in note), f'[H20] ({failed!r}, {note!r})'
            except Exception as e:
                ok, detail = False, f'[H20] compile_failed raised {type(e).__name__}: {e}'
            case('[H20] health.compile_failed survives a CLI that hangs', ok, detail)

            # [H9] --lanes shells into every other checkout. Without --no-optional-locks git status takes that
            # checkout's index.lock, so a commit running there at that moment fails. No output-level case can show
            # this: cmd_status locks with LOCK_NONE, so output and exit code are the same either way (and health.py
            # only counts the lines), and the harm is a race between two processes. So the case reads the argv.
            seen = []

            def recorder(cmd, cwd=None, timeout=120, env=None):
                seen.append(list(cmd))
                sub = ' '.join(cmd[1:])
                if 'branch --show-current' in sub:
                    return 0, 'lane/x'
                if 'worktree list' in sub:
                    return 0, 'worktree %s\nHEAD abc\nbranch refs/heads/lane/x' % PROJ.parent
                if 'rev-list' in sub:
                    return 0, '0 0'
                if 'log' in cmd[:2]:
                    return 0, '01-01 00:00  x'
                return 0, ''

            real_run = health.run
            try:
                health.run = recorder
                with contextlib.redirect_stdout(io.StringIO()):
                    health.lanes()
            finally:
                health.run = real_run
            status_calls = [c for c in seen if 'status' in c]
            case('[H9] health.py --lanes reads another checkout status without taking its index.lock',
                 bool(status_calls) and all('--no-optional-locks' in c for c in status_calls),
                 '[H9] status calls: %r' % (status_calls,))

            for cs_name in ('aosa_tiers.cs', 'aosa_budget.cs'):
                cs_text = (HERE.parent / 'AgentScripts' / cs_name).read_text(encoding='utf-8')
                case(f'[H12] {cs_name} EditorPrefs keys are namespaced per checkout, not shared literals',
                     not re.search(r'EditorPrefs\.(Get|Set)\w+\("aosa\.(dir|stem|hold|budget)"', cs_text), cs_text)
                # [H12] Math.Abs(int.MinValue) throws OverflowException, and a dataPath hash can be int.MinValue,
                # so the namespace prefix must come from an unsigned cast instead.
                case(f'[H12] {cs_name} builds its namespace with (uint), never Math.Abs(GetHashCode())',
                     'Math.Abs' not in cs_text and '(uint)' in cs_text, cs_text)

            # [H12] the refusal has to name the key the script really reads, or no agent can ever satisfy it
            tiers_text = (HERE.parent / 'AgentScripts' / 'aosa_tiers.cs').read_text(encoding='utf-8')
            case('[H12] aosa_tiers.cs names the namespaced key it reads when it refuses',
                 'set EditorPrefs aosa.dir first' not in tiers_text
                 and 'return "set EditorPrefs " + ns + "dir first"' in tiers_text, tiers_text)

            tw_cases(tmp, wt_proj / 'Tools/tw')
    finally:
        if wt.exists():
            code, out = run(['git', 'worktree', 'remove', '--force', str(wt)], REPO)
            if code:
                print('could not remove worktree ' + str(wt) + ': ' + out)
        scrub(tmp)
        run(['git', 'worktree', 'prune'], REPO)
        case('the run leaves no folder behind', not tmp.exists(), str(tmp))
    print(f'{sum(results)} of {len(results)} cases behaved' + ('' if len(only) == len(GROUPS) else ' (only: %s)' % ', '.join(only)))
    sys.exit(0 if all(results) else 1)


if __name__ == '__main__':
    main()
