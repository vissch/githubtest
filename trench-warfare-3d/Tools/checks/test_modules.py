import re
import sys

WHY = ('The test modules hold: the landing folder is empty, a slow module reaches only what gate_scope.py lists for it, '
       'and every test in an [Explicit]-only assembly is [Explicit].')

READS_PROJECT = re.compile(r'\b(Resources\.Load|AssetDatabase|Application\.dataPath|StreamReader|'
                           r'File\.(ReadAll|ReadLines|Open|Exists)|Directory\.(GetFiles|EnumerateFiles|GetDirectories))')


TEST_ATTR = re.compile(r'\b(?:Test|TestCase|UnityTest)\b')   # \bTest\b misses [TestFixture]: good


def explicit_gaps(txt):
    """Line and attribute text of each test in this file that no [Explicit] covers ([Explicit] on the fixture counts)."""
    gaps, type_explicit, block = [], False, []
    for n, line in enumerate(txt.splitlines(), 1):
        s = line.strip()
        if s.startswith('['):
            block.append((n, s))
            continue
        if block:
            joined = ' '.join(t for _, t in block)
            if re.search(r'\b(class|struct)\b', s):
                type_explicit = 'Explicit' in joined
            elif TEST_ATTR.search(joined) and 'Explicit' not in joined and not type_explicit:
                gaps.append((block[0][0], joined))
            block = []
    return gaps


def home(txt):
    """The module folder a test file belongs in, by what it names."""
    if re.search(r'\bTW\.Editor\b', txt):
        return 'Project'
    if re.search(r'\bTW\.UI\b', txt):
        return 'UI'
    if re.search(r'\bTW\.(Presentation|Perf)\b|\bUnity(Engine|Editor)\b', txt):
        drives_a_match = re.search(r'\b(LockstepSession|ScriptedEnemy)\b', txt)
        beyond_core = re.search(r'\bTW\.(Presentation\.\w|Perf\b)|\bUnity(Engine|Editor)\b', txt)
        return 'Match' if drives_a_match and not beyond_core else 'Show'
    return 'Sim'


def run(ctx):
    """Tools/gate_scope.py skips a slow test module (Sim, Match) when no changed path is under that module's NEEDS.
    That is only sound while the module's tests can reach nothing else, so this holds them to it. It also holds
    EXPLICIT_ONLY to its word: every test in an [Explicit]-only assembly is [Explicit]."""
    root = ctx.root.resolve()
    sys.path.insert(0, str(root / 'Tools'))
    import gate_scope
    errors = []
    tests = root / 'Assets/_Project/Tests'
    rel = lambda p: p.resolve().relative_to(root).as_posix()
    repo_rel = lambda p: p.resolve().relative_to(root.parent).as_posix()
    sources = {cs.resolve(): txt for cs, txt in ctx.sources.items()}
    asm = ctx.asmdefs

    # 1. the landing folder: a lane cut before the split lands its new tests here; they move on before they are gated
    landing = tests / 'EditMode'
    for cs, txt in sorted(sources.items()):
        if cs.parent == landing and cs.name != 'LandingFolder.cs':
            to = home(txt)
            errors.append(f'{rel(cs)}: the EditMode tests are one folder per module (docs/reference/workflow.md, section 5), '
                          f'and Tests/EditMode only catches the files of lanes cut before the split. By what it uses, this one '
                          f'belongs in Tests/{to}/. Run: git mv {rel(cs)} {rel(tests / to / cs.name)} && '
                          f'git mv {rel(cs)}.meta {rel(tests / to / cs.name)}.meta && python Tools/codemap.py')

    mods = gate_scope.modules(root)
    for m, needs in gate_scope.NEEDS.items():
        if m not in mods:
            continue
        name, folder = mods[m]
        # 2. every assembly the module can reach, through its references and theirs, lives under its NEEDS
        reach, todo = set(), [name]
        while todo:
            for r in asm.get(todo.pop(), (None, []))[1]:
                if r.startswith('TW.') and r in asm and r not in reach:
                    reach.add(r)
                    todo.append(r)
        for r in sorted(reach):
            where = repo_rel(asm[r][0].parent) + '/'
            if not where.startswith(needs):
                errors.append(f'{name} reaches {r} ({where}), which NEEDS in Tools/gate_scope.py does not list for {m}: '
                              f'the scoped gate would skip these tests when that code changes. Drop the reference, or '
                              f'add the folder to NEEDS["{m}"].')
        if not folder.startswith(needs):
            errors.append(f'NEEDS["{m}"] in Tools/gate_scope.py does not list the module\'s own folder {folder}')
        # 3. a test that reads project files says which folder, and it is one of NEEDS
        for cs, txt in sorted(sources.items()):
            if (root.parent / folder) not in cs.parents or not READS_PROJECT.search(txt):
                continue
            reads = gate_scope.READS.get(cs.stem)
            if reads is None:
                errors.append(f'{rel(cs)}: reads project files, and READS in Tools/gate_scope.py does not say which folder: '
                              f'the scoped gate could skip it when what it reads changes. Add "{cs.stem}" there with a '
                              f'folder NEEDS["{m}"] lists, or move the test to a module that always runs (Tests/Show).')
            elif not reads.startswith(needs):
                errors.append(f'{rel(cs)}: READS in Tools/gate_scope.py says it reads {reads}, which NEEDS["{m}"] does not list')

    # 4. a test class is found by its file name (tasks.md, --filter TW.Tests.<Class>): one file per name
    seen = {}
    for cs in sorted(sources):
        if tests in cs.parents:
            if cs.stem in seen:
                errors.append(f'two test files are named {cs.name} ({rel(seen[cs.stem])}, {rel(cs)}): a test class is found by its file name')
            seen.setdefault(cs.stem, cs)

    # 5. EXPLICIT_ONLY drops an assembly from every scoped run on its word that every test there is [Explicit]: hold it to that
    for m in sorted(gate_scope.EXPLICIT_ONLY):
        if m not in mods:
            continue
        name, folder = mods[m]
        for cs, txt in sorted(sources.items()):
            if (root.parent / folder) not in cs.parents:
                continue
            for n, _attrs in explicit_gaps(txt):
                errors.append(f'{rel(cs)}:{n}: {name} is in EXPLICIT_ONLY in Tools/gate_scope.py, so no gate run selects it, and this '
                              f'test is not [Explicit]: nothing runs it. Mark it [Explicit("...")], or drop "{m}" from EXPLICIT_ONLY.')
    return errors
