#!/usr/bin/env python3
"""Which EditMode test assemblies a gate run needs: the one place that knows the test modules and what each reads.

WHY. The EditMode tests are one assembly per module (TW.Tests.Sim, Match, Show, UI, Project; TW.Tests.EditMode is the
landing folder for lanes cut before the split). The sim and match tests are nearly all of the run time, and a lane that
changed no file they can reach learns nothing from running them. `gate.ps1 -EditOnly` asks this script which modules
the lane's changes can reach. The full gate (no arguments) never asks: it runs everything, and it alone lets a lane land.

    python Tools/gate_scope.py                  from trench-warfare-3d/: the modules this lane's changes need
    python Tools/gate_scope.py --all            every module (what the full gate runs)
    python Tools/gate_scope.py --modules Sim,UI exactly these
    python Tools/gate_scope.py --tree <id>      compare this tree instead of taking the working tree's own

THE RULE is deny-by-default. A slow module (NEEDS below) is skipped only when EVERY changed path is known not to
reach it: a path under one of its NEEDS prefixes, a build input (EVERYTHING), or a path this file has never heard of
runs it. Every other module always runs. "Changed" is the working tree (untracked files included) against the
merge-base with origin's integration branch, so a lane that touched the sim runs the sim tests on every commit.
Nothing changed, no merge-base, or git failing: everything runs.

A NEW SLOW MODULE needs a NEEDS line listing every folder its tests can reach: the assemblies its asmdef references,
its own folder, and any file its tests read by path. Tools/checks/test_modules.py holds the slow modules to that.

Output, one `key: value` per line, read by gate.ps1: `scoped` (yes when something is skipped), `assemblies`
(`;`-joined, what to run), `expect` (those of them that hold a test: what the results must show), `classes`
(`;`-joined full class names, for a runner that selects by name), `tools` (yes when the lane changes a tool, so
the gate runs Tools/toolcheck.py in place of validate.py alone), then one `note`.
Exit 0; 2 on an unknown module name.
"""
import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent          # trench-warfare-3d
REPO = ROOT.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from land import INTEGRATION, is_tool  # noqa: E402  (one definition of the integration branch, and of a tool)

P = 'trench-warfare-3d/Assets/_Project/'
PREFIX = 'TW.Tests.'

# Assemblies whose every test is [Explicit] (run by name, with a graphics device): no gate run selects them
# (the `test_modules` check holds every test there to it).
EXPLICIT_ONLY = {'Stills'}

# The slow modules, and the path prefixes whose change means the module must run.
NEEDS = {
    'Sim': (P + 'Sim/', P + 'Net/', P + 'Data/', P + 'Tests/Sim/'),
    'Match': (P + 'Sim/', P + 'Net/', P + 'Data/', P + 'Presentation/Core/', P + 'Tests/Match/'),
}

# Slow-module tests that read project files by path, and the folder each reads (it must be one of the module's NEEDS).
READS = {
    'MineTests': P + 'Sim/',   # one system alone clears the blast list: it greps the sim sources
    # only its Explicit report reads a file, the sweep's spec, named when Tools/sweep.py runs it; the two plain tests
    # read nothing. It plays whole matches through MatchLoopTests and AssaultLadderTests, so it is a Match test.
    'BalanceSweepTests': P + 'Tests/Match/',
}

# A change here can reach any assembly: the build's inputs, and the gate itself.
EVERYTHING_PREFIXES = ('trench-warfare-3d/Packages/', 'trench-warfare-3d/ProjectSettings/')
EVERYTHING_FILES = ('gate.ps1', 'trench-warfare-3d/Tools/gate_scope.py')
EVERYTHING_SUFFIXES = ('.asmdef', '.asmref', '.rsp', '.dll')

# Paths no slow module reads (checked after NEEDS, so Presentation/Core/ still runs Match). Anything in neither list
# is unknown, and unknown runs everything.
SAFE_PREFIXES = (P + 'Presentation/', P + 'UI/', P + 'Editor/', P + 'Perf/', P + 'Resources/', P + 'Art/',
                 P + 'Shaders/', P + 'Settings/', P + 'Scenes/', P + 'Playground/', P + 'Tests/',
                 'trench-warfare-3d/Tools/', 'docs/', '.claude/', '.github/', 'github-test1/')
SAFE_FILES = ('CLAUDE.md', 'README.md', 'trench-warfare-3d/validate.py')


def modules(root=ROOT):
    """Every EditMode test assembly: name without the TW.Tests. prefix -> (assembly name, folder relative to the repo).
    EditMode means the asmdef is Editor-only; TW.Tests.PlayMode builds for every platform and is not one."""
    out = {}
    for p in sorted((root / 'Assets' / '_Project').rglob('*.asmdef')):
        d = json.loads(p.read_text(encoding='utf-8-sig'))
        if d['name'].startswith(PREFIX) and d.get('includePlatforms') == ['Editor']:
            out[d['name'][len(PREFIX):]] = (d['name'], p.parent.relative_to(root.parent).as_posix() + '/')
    return out


def reaches(path, module):
    """Whether a change to this repo path can change what the slow module's tests see. Unknown paths can."""
    stem = path[:-5] if path.endswith('.meta') else path
    if stem in EVERYTHING_FILES or stem.endswith(EVERYTHING_SUFFIXES) or stem.startswith(EVERYTHING_PREFIXES):
        return True
    if stem.startswith(NEEDS[module]):
        return True
    return not (stem in SAFE_FILES or stem.startswith(SAFE_PREFIXES))


def scope(changed, mods):
    """(modules to run, {skipped module: why}). `changed` is a list of repo paths, or None when it is not known."""
    run = [m for m in mods if m not in EXPLICIT_ONLY]
    if not changed:
        return run, {}
    skipped = {}
    for m in [m for m in run if m in NEEDS]:
        if not any(reaches(c, m) for c in changed):
            skipped[m] = 'no change under ' + ', '.join(x[len(P):] if x.startswith(P) else x for x in NEEDS[m])
    return [m for m in run if m not in skipped], skipped


def git(*args, env=None):
    p = subprocess.run(['git', '-C', str(REPO), *args], capture_output=True, env=env)
    return p.returncode, p.stdout.decode('utf-8', 'replace').strip()


def working_tree():
    """The tree a commit of the working tree would record (untracked files included), through a scratch index: the
    same id gate.ps1 takes. None when git cannot say."""
    index = Path(tempfile.gettempdir()) / f'tw-scope-index-{os.getpid()}'
    env = dict(os.environ, GIT_INDEX_FILE=str(index))
    try:
        if git('read-tree', 'HEAD', env=env)[0] or git('add', '-A', env=env)[0]:
            return None
        code, tree = git('write-tree', env=env)
        return tree if code == 0 and re.fullmatch(r'[0-9a-f]{40}', tree) else None
    finally:
        index.unlink(missing_ok=True)


def changed_paths(tree=None):
    """(paths changed on this lane, the base they are measured from); (None, why) when that cannot be known."""
    code, base = git('merge-base', 'HEAD', f'origin/{INTEGRATION}')
    if code or not base:
        return None, f'no merge-base with origin/{INTEGRATION}'
    tree = tree or working_tree()
    if not tree:
        return None, 'git could not read the working tree'
    # --no-renames: a file moved out of Sim/ must still list its old path
    code, out = git('diff', '--name-only', '--no-renames', '-z', base, tree)
    if code:
        return None, 'git diff failed'
    return [f for f in out.split('\0') if f], base[:10]


def classes(names, mods):
    """Full names of the test classes in these modules: one class per file, named after it, in the file's namespace."""
    out = []
    for m in names:
        folder = REPO / mods[m][1]
        nested = [REPO / f for _, f in mods.values() if (REPO / f) != folder and folder in (REPO / f).parents]
        for cs in sorted(folder.rglob('*.cs')):
            if any(n in cs.parents for n in nested):
                continue
            src = cs.read_text(encoding='utf-8-sig', errors='replace')
            if re.search(r'\[(?:Test|TestCase|UnityTest)\b', src):
                ns = re.search(r'^\s*namespace\s+([\w.]+)', src, re.M)
                out.append((ns.group(1) + '.' if ns else '') + cs.stem)
    return out


def main():
    ap = argparse.ArgumentParser(description='which EditMode test assemblies a gate run needs')
    ap.add_argument('--all', action='store_true')
    ap.add_argument('--modules', default='')
    ap.add_argument('--tree', default='')
    a = ap.parse_args()
    mods = modules()
    everything = [m for m in mods if m not in EXPLICIT_ONLY]
    changed, base = changed_paths(a.tree)
    if a.modules:
        want = [m.strip() for m in a.modules.split(',') if m.strip()]
        by_lower = {m.lower(): m for m in mods}
        bad = [m for m in want if m.lower() not in by_lower]
        if bad:
            print(f'gate_scope: no test module named {", ".join(bad)}. The modules: {", ".join(sorted(mods))}')
            return 2
        run = [by_lower[m.lower()] for m in want]
        note = 'only ' + ', '.join(run) + ' (asked for by name)'
    elif a.all:
        run, note = everything, 'every module (asked for)'
    else:
        run, skipped = scope(changed, mods)
        if changed is None:
            note = f'every module: {base}'
        elif not changed:
            note = f'every module: nothing differs from the merge-base {base}'
        elif skipped:
            note = '; '.join(f'skipping {m} ({why} since {base})' for m, why in skipped.items())
        else:
            note = f'every module: the changes since {base} reach all of them'
    print('scoped: ' + ('yes' if set(run) != set(everything) else 'no'))
    print('assemblies: ' + ';'.join(mods[m][0] for m in run))
    print('expect: ' + ';'.join(mods[m][0] for m in run if classes([m], mods)))   # an assembly with no test is no suite
    print('classes: ' + ';'.join(classes(run, mods)))
    print('tools: ' + ('yes' if any(is_tool(c) for c in changed or []) else 'no'))
    print('note: ' + note)
    return 0


if __name__ == '__main__':
    sys.exit(main())
