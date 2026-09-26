#!/usr/bin/env python3
"""Keeps the navigation docs true. Regenerates the tables that can be derived from the code, and checks the ones
that cannot.

WHY THIS EXISTS. By 2026-09-25 every entry-point doc in this repo had rotted: docs/04 listed four sim systems that
did not exist, README called the VAT renderer a stub, HANDOFF told the next agent to "work M1". Nobody lied; the
code moved and prose does not. So anything a script can derive is derived here, and anything a person wrote is
checked here against the tree, and validate.py runs this on every gate in every lane.

USAGE (from trench-warfare-3d/)
    python Tools/codemap.py            rewrite the generated blocks in the docs
    python Tools/codemap.py --check    exit 1 if a generated block is stale or a hand-written doc is wrong

Generated blocks sit between <!-- gen:NAME --> and <!-- /gen:NAME --> markers. Never edit inside them: edit the
tables below (PURPOSE, FLAG_EFFECT) or the code, then rerun.

WHAT --check FAILS ON
  * a generated block that differs from what the code says now
  * a folder under Assets/_Project (two levels deep) with no line in PURPOSE, or a PURPOSE line for a folder
    that no longer exists
  * a runtime switch (command-line arg, PlayerPrefs/EditorPrefs key, environment variable) with no line in
    FLAG_EFFECT
  * a file cited in backticks in CLAUDE.md or docs/reference/*.md (except the dated log) that does not exist,
    or a `File.cs:NN` whose line is past the end of the file
  * a test file, or a SceneHooks member, that docs/reference/tasks.md never names outside its generated blocks
  * a Tools/*.py script or `tw` subcommand that neither pipelines.md nor workflow.md names
  * docs/reference/agent-memory.md over 150 lines, or CLAUDE.md over 110
It WARNS (does not fail) when CLAUDE.md's "In flight" block is more than 7 days old.
"""
from __future__ import annotations

import datetime as _dt
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent          # trench-warfare-3d
REPO = ROOT.parent
PROJ = ROOT / 'Assets' / '_Project'
REF = REPO / 'docs' / 'reference'

CODE_MAP = REF / 'code-map.md'
FLAGS = REF / 'feature-flags.md'
TASKS = REF / 'tasks.md'
WORKFLOW = REF / 'workflow.md'
PIPELINES = REF / 'pipelines.md'
MEMORY = REF / 'agent-memory.md'
CLAUDE = REPO / 'CLAUDE.md'

MEMORY_MAX_LINES = 150
CLAUDE_MAX_LINES = 110
IN_FLIGHT_MAX_DAYS = 7

# ---- hand-maintained tables -------------------------------------------------------------------------------------

# One line per folder, two levels under Assets/_Project. A new folder fails --check until it has a line here.
PURPOSE = {
    'Art': 'Source art Unity imports but the game does not load by name (figures, clip FBX).',
    'Art/Characters': "Owner's Mixamo-rigged figures (Soldier, Sniper) and the ~100 clip FBX the VAT baker reads.",
    'Data': 'ScriptableObject schemas and the baker that turns them into sim tables (SIM lane).',
    'Editor': 'Editor-only tools: scene builder, VAT baker, importers, CaptureRig, Windows build, prop editor.',
    'Editor/UI': 'Editor tools for the UI skin: placeholder painter, importer, verifier, HUD capture.',
    'Net': 'Lockstep driver, loopback transport, command seat; UTP/hash exchange/snapshot are stubs (SIM lane).',
    'Perf': 'PerfBench (editor + player benchmark), AllocProbe, bench options.',
    'Presentation': 'Everything drawn. Reads the sim, never writes it (except through SimHost.WriteWorlds).',
    'Presentation/Audio': 'Stub: event audio router (B7). No audio assets exist yet.',
    'Presentation/Camera': 'NOT just the camera: TacticalCamera plus CombatFx, TankRenderer, WalkerGait, Flamethrower, '
                           'DebrisRenderer, FlipbookFx, the IMGUI BattleHud and TestPanel. Namespace TW.Presentation.Tactical.',
    'Presentation/Core': 'SimHost (owns the match), SimPresenter, EventPump, AnimationController, SceneHooks/RenderGround, '
                         'MatchClock, KeyMap, settings, HudBridge.',
    'Presentation/Terrain': 'Ground mesh, battlefield composer and prop kit, destruction and wear, houses, weather, night, sea.',
    'Presentation/Units': 'VATRenderer (all infantry), VAT codec/asset data, ProceduralSoldier far-tier fallback.',
    'Presentation/VFX': 'Stub assembly (B5 router). Real effects live in Presentation/Camera.',
    'Resources': 'Everything loaded by name at runtime (Resources.Load). Moving a file here breaks a string somewhere.',
    'Resources/Env': 'Imported environment sets (Tripo), one folder per set, plus EnvAtlas.jpg built by Tools/envatlas.py.',
    'Resources/Layouts': "PropLayout asset: the owner's hand edits over the composed props.",
    'Resources/ShaderKeep': 'Materials that keep Shader.Find shaders in player builds (ShaderInclusionTests).',
    'Resources/Units': 'Baked VAT atlases (.bytes) per figure. Rebuilt by TW/VAT/Bake Infantry.',
    'Resources/VFX': 'Flipbook textures (Asset Store packs + firebooks.py output).',
    'Resources/Vehicles': 'Tank and walker FBX + atlases, one folder per machine (tanksplit.py / crabsplit.py output).',
    'Scenes': 'Bootstrap (entry), MainMenu, GreyboxCorridor (the dev battle scene: open this one to Play).',
    'Settings': 'URP pipeline and renderer assets (TW-URP, TW-Renderer).',
    'Shaders': 'All TW/* shaders and the shared .hlsl includes.',
    'Sim': 'Deterministic lockstep simulation. References nothing outside TW.Sim.* (SIM lane).',
    'Sim/Combat': 'Target acquisition, direct fire, blast, suppression, gas, tank gunnery, armour.',
    'Sim/Core': 'SimWorld (state + hash), commands, events, config, RNG, math, roster, system order.',
    'Sim/Match': 'MatchSim (system registration), sector control, off-map abilities, bombardment, deformation, sea landing.',
    'Sim/Nav': 'Flow fields, movement, separation, spatial hash, vehicle kinematics (VehicleSize lives here).',
    'Sim/Terrain': 'MapData, heightfield, battlefield generator, craters, wire, mud, props.',
    'Sim/Units': 'Trench garrison and orders, vehicle modules, stats; stance/grenades/abilities are stubs.',
    'Tests': 'NUnit tests. EditMode is the bulk; PlayMode spins real SimHosts.',
    'Tests/EditMode': 'EditMode tests (references every assembly incl. TW.Editor).',
    'Tests/PlayMode': 'PlayMode tests: lockstep loopback, match clock, launch, HUD layout, shell router.',
    'UI': 'UI Toolkit: the battle HUD (HudController/HudView) and its parts.',
    'UI/Missions': 'Mission card assets for the mission select.',
    'UI/Resources': 'UXML/USS/PanelSettings loaded by name (Hud/, Shell/, UI/).',
    'UI/Selection': 'Unit selection: picker, model, markers, hover card, selection panel.',
    'UI/Shell': 'Menus: router, main menu, mission select, pause, settings, debrief, armoury.',
    'UI/Skin': 'Dust Front skin: SkinSpec (the sprite table), USS tokens, fonts.',
}

# What each runtime switch does. A switch found in the code with no line here fails --check.
FLAG_EFFECT = {
    '-twCanary': 'Player/editor arg: run the second (peer) sim world as a determinism canary. Off in single player.',
    '-twbench': 'Player/editor arg: run PerfBench with "key=value ..." options and quit. See workflow.md, Benchmark.',
    'TW_BENCH': 'Environment variable, same as -twbench. Also fires in editor Play if set, so unset it after use.',
    '-twdev': 'Arg to the batch Windows build: make a Development build.',
    'tw.hud.toolkit': 'PlayerPrefs int (registry, per machine): 1 = UI Toolkit HUD, 0 = IMGUI BattleHud. F9 flips it. '
                      'Default 1. A machine where someone pressed F9 shows the other HUD.',
    'tw.rig.stress': 'SessionState (this editor session only): CaptureRig stress request carried across a domain reload.',
    'tw.rig.stress.restore': 'SessionState: the StressUnits value CaptureRig puts back afterwards.',
    'TW.EnvProps.Edit': 'EditorPrefs bool: hand placement of props in the Scene view during Play (EnvPropEditor). Default on.',
    'TW_AUDIT_OUT': 'Environment variable: where -executeMethod TW.Editor.AssetScaleAudit.Run writes the asset scale audit. '
                    'Default docs/reference/asset-scale.md.',
}

# Code-level switches worth knowing. Each must still be declared where it says, or --check fails.
STATIC_SWITCHES = [
    ('Presentation/Core/SimHost.cs', 'CanaryOverride', 'Test/bench override of the canary (null = use arg/inspector).'),
    ('Presentation/Core/SimHost.cs', 'BombardmentOverride', 'Ambient shells/min override set by TestPanel presets; -1 = off.'),
    ('Presentation/Core/SimHost.cs', 'StressOverride', 'StressUnits override for CaptureRig/PerfBench; -1 = off.'),
    ('Presentation/Core/SimHost.cs', 'GeneratedBattlefield', 'Inspector: generated battlefield (true) or flat playtest map.'),
    ('Presentation/Core/SimHost.cs', 'PlaytestMap', 'Inspector: two-line playtest layout when not generated.'),
    ('Presentation/Core/SimHost.cs', 'Ground', 'Inspector: which terrain preset the generator builds. Does NOT change the '
                                                'biome look (GreyboxTerrainView / SceneMood do). Set both for a visual test.'),
    ('Presentation/Core/SimHost.cs', 'UseAnimationController', 'Inspector: per-man clip ladder on (default) or raw atlas rows.'),
    ('Presentation/Core/SimHost.cs', 'StressUnits', 'Inspector: deploy N per side and send both over the top.'),
    ('Presentation/Core/SimHost.cs', 'DeterminismCanary', 'Inspector: same as -twCanary.'),
    ('Presentation/Camera/CameraShake.cs', 'Strength', 'CameraShake.Strength: 0 turns shake off (settings screen writes it).'),
    ('Presentation/Camera/DebrisRenderer.cs', 'Gore', 'Gore slider, 0..1 (settings screen writes it).'),
    ('Presentation/Core/EventPump.cs', 'ProfileSubscribers', 'Profiler marker per event subscriber (perf work only).'),
    ('UI/HudBootstrap.cs', 'Disabled', 'HudBootstrap.Disabled: tests set it so no HUD is added to their scenes.'),
    ('UI/Shell/ShellBoot.cs', 'Disabled', 'ShellBoot.Disabled: tests set it so no menu shell is added.'),
    ('UI/HudHotkeys.cs', 'LegacyOverlayActive', 'Whether the IMGUI debug overlay still takes F-keys.'),
    ('Presentation/Terrain/Atmosphere.cs', 'PinnedClock', 'Freeze weather/rain at a clock value for repeatable captures; -1 = live.'),
]

# Paths that docs may cite although they are not in the repo (machine-local or produced at run time).
ALLOW_MISSING = {
    'settings.json', 'editor-slot.json', 'Captures/', 'Builds/', 'Tools/flame-shots/', 'test-results.xml',
    'Library/', 'Temp/', 'Temp/UnityLockfile', 'Library/BurstCache', 'Library/ScriptAssemblies', 'Assets/_shots/',
    'Editor.log',
}

# ---- helpers ----------------------------------------------------------------------------------------------------

def read(p: Path) -> str:
    return p.read_text(encoding='utf-8-sig').replace('\r\n', '\n')


def rel(p: Path, base: Path = PROJ) -> str:
    return p.relative_to(base).as_posix()


def cs_files(base: Path = PROJ):
    return sorted(base.rglob('*.cs'))


def strip_comments(src: str) -> str:
    src = re.sub(r'/\*.*?\*/', '', src, flags=re.S)
    return re.sub(r'//[^\n]*', '', src)


GEN = re.compile(r'(<!-- gen:(?P<name>[\w-]+) -->\n)(?P<body>.*?)(<!-- /gen:(?P=name) -->)', re.S)


def blocks(text: str) -> dict:
    return {m.group('name'): m.group('body') for m in GEN.finditer(text)}


def outside_blocks(text: str) -> str:
    return GEN.sub('', text)


def replace_blocks(text: str, new: dict) -> str:
    def sub(m):
        name = m.group('name')
        return m.group(1) + new[name] + m.group(4) if name in new else m.group(0)
    return GEN.sub(sub, text)


def table(header, rows) -> str:
    out = ['| ' + ' | '.join(header) + ' |', '|' + '---|' * len(header)]
    for r in rows:
        out.append('| ' + ' | '.join(str(c).replace('|', '\\|') for c in r) + ' |')
    return '\n'.join(out) + '\n'

# ---- assemblies -------------------------------------------------------------------------------------------------

def assemblies():
    found = []
    for p in sorted(PROJ.rglob('*.asmdef')):
        d = json.loads(read(p))
        found.append((d['name'], p.parent, d.get('references', []), d.get('rootNamespace', '')))
    folders = {f: n for n, f, _, _ in found}

    def owner(cs: Path):
        for parent in cs.parents:
            if parent in folders:
                return folders[parent]
        return None

    files, namespaces = {}, {}
    for cs in cs_files():
        o = owner(cs)
        files[o] = files.get(o, 0) + 1
        for m in re.finditer(r'^\s*namespace\s+([\w.]+)', read(cs), re.M):
            namespaces.setdefault(o, set()).add(m.group(1))
    rows = []
    for name, folder, refs, _ in found:
        tw = [r.replace('TW.', '') for r in refs if r.startswith('TW.')]
        rows.append((f'`{name}`', f'`{rel(folder)}/`', ', '.join(f'`{n}`' for n in sorted(namespaces.get(name, []))),
                     ', '.join(tw) or '(none)', files.get(name, 0)))
    total = sum(files.values())
    return (table(['Assembly', 'Folder', 'Namespaces declared', 'References (TW.*)', '.cs'], rows)
            + f'\n{len(found)} assemblies, {total} C# files. Assembly names and namespaces differ on purpose: '
              '`using` takes the namespace.\n')

# ---- folders ----------------------------------------------------------------------------------------------------

def project_folders():
    out = []
    for d in sorted(PROJ.iterdir()):
        if d.is_dir():
            out.append(rel(d))
            for s in sorted(d.iterdir()):
                if s.is_dir():
                    out.append(rel(s))
    return out


def folders_block():
    rows = []
    for f in project_folders():
        n = sum(1 for _ in (PROJ / f).rglob('*.cs'))
        rows.append((f'`{f}/`', PURPOSE.get(f, '**NO PURPOSE LINE: add one to Tools/codemap.py PURPOSE**'), n or ''))
    return table(['Folder (under Assets/_Project)', 'What lives there', '.cs'], rows)

# ---- sim system order -------------------------------------------------------------------------------------------

def sim_order():
    consts = {m.group(1): int(m.group(2)) for m in
              re.finditer(r'public const int (\w+) = (\d+);', read(PROJ / 'Sim/Core/ISimSystem.cs'))}
    systems = {}   # class -> (order, expr, file)
    for cs in cs_files(PROJ / 'Sim'):
        src = read(cs)
        for m in re.finditer(r'int Order => SimSystemOrder\.(\w+)(?:\s*([+-])\s*(\d+))?', src):
            cls = re.findall(r'class (\w+)', src[:m.start()])[-1]
            v = consts[m.group(1)] + (int(m.group(3)) * (1 if m.group(2) == '+' else -1) if m.group(2) else 0)
            expr = m.group(1) + (f' {m.group(2)} {m.group(3)}' if m.group(2) else '')
            systems[cls] = (v, expr, rel(cs))
    lines = read(PROJ / 'Sim/Match/MatchSim.cs').split('\n')
    start = next(i for i, l in enumerate(lines) if 'public MatchSim(SimConfig' in l)
    status, cond = {}, None
    for i in range(start, len(lines)):
        l = lines[i]
        s = l.strip()
        if s.startswith('public void Step'):
            break
        if re.match(r'if \((\w+[.\w]*)\)\s*$', s):
            cond = re.match(r'if \((\w+[.\w]*)\)', s).group(1)
            continue
        if s == '}' and cond:
            cond = None
            continue
        line_cond = cond
        m1 = re.match(r'if \(([\w.]+)\)\s*\{', s)
        if m1:
            line_cond = m1.group(1)
        for m in re.finditer(r'new (?:[\w]+\.)*(\w+)\(', l):
            name = m.group(1)
            if name not in systems:
                continue
            if s.startswith('//'):
                status.setdefault(name, 'stub: commented out in MatchSim')
            else:
                status[name] = 'registered' + (f' (only if `{line_cond}`)' if line_cond else '')
    rows = []
    for cls, (v, expr, f) in sorted(systems.items(), key=lambda kv: kv[1][0]):
        rows.append((v, f'`{cls}`', f'`{expr}`', f'`{f}`', status.get(cls, '**not referenced by MatchSim**')))
    used = {e.split()[0] for _, e, _ in systems.values()}
    unused = [f'`{k}` ({v})' for k, v in sorted(consts.items(), key=lambda kv: kv[1]) if k not in used]
    return (table(['Order', 'System', 'SimSystemOrder', 'File (under Assets/_Project)', 'Status'], rows)
            + '\nStep order follows `Order`; `Initialize` order follows the `AddSystem` calls in '
              '`Sim/Match/MatchSim.cs`. Order constants no system uses: ' + (', '.join(unused) or 'none') + '.\n')

# ---- SceneHooks wiring ------------------------------------------------------------------------------------------

def scene_hook_members():
    src = read(PROJ / 'Presentation/Core/RenderGround.cs')
    body = src[src.index('public static class SceneHooks'):]
    body = body[:body.index('\n    }\n')]
    return re.findall(r'public static (?:readonly )?[\w.<>, ]+? (\w+)\s*(?:;|=)', body)


def hooks_block():
    members = scene_hook_members()
    sources = {rel(cs): strip_comments(read(cs)) for cs in cs_files() if '/Tests/' not in cs.as_posix()}
    rows = []
    for mname in members:
        setters, users = [], []
        for f, src in sources.items():
            hits = list(re.finditer(r'SceneHooks\.' + mname + r'\b\s*(\+=|-=|=(?!=)|\.(?:Add|Clear|Remove)\()?', src))
            if not hits:
                continue
            short = f.split('/')[-1].replace('.cs', '')
            (setters if any(h.group(1) for h in hits) else users).append(short)
        rows.append((f'`{mname}`', ', '.join(sorted(setters)) or '-', ', '.join(sorted(users)) or '-'))
    return table(['SceneHooks member', 'Set by', 'Read / called by'], rows)

# ---- tests index ------------------------------------------------------------------------------------------------

def tests_block():
    prod_types = set()
    for cs in cs_files():
        if '/Tests/' in cs.as_posix():
            continue
        for m in re.finditer(r'\b(?:class|struct|enum|interface)\s+(\w+)', strip_comments(read(cs))):
            if len(m.group(1)) > 3:
                prod_types.add(m.group(1))
    rows = []
    for cs in cs_files(PROJ / 'Tests'):
        src = strip_comments(read(cs))
        n = len(re.findall(r'\[(?:Test|UnityTest)\b', src))
        counts = {}
        for t in prod_types:
            c = len(re.findall(r'\b' + t + r'\b', src))
            if c:
                counts[t] = c
        top = sorted(counts, key=lambda t: (-counts[t], t))[:6]
        rows.append((f'`{cs.stem}`', cs.parent.name, n, ', '.join(top)))
    return table(['Test class', 'Mode', 'Tests', 'Production types it touches most'], rows)

# ---- flags ------------------------------------------------------------------------------------------------------

def find_flags():
    flags = {}   # name -> (kind, file:line)
    for cs in cs_files():
        if '/Tests/' in cs.as_posix():
            continue
        src = read(cs)
        consts = dict(re.findall(r'const string (\w+)\s*=\s*"([^"]+)"', src))
        for m in re.finditer(r'const string (\w+)\s*=\s*"([^"]+)"\s*,\s*(\w+)\s*=\s*"([^"]+)"', src):
            consts[m.group(3)] = m.group(4)
        for i, line in enumerate(src.split('\n'), 1):
            at = f'{rel(cs)}:{i}'
            for m in re.finditer(r'"(-tw\w+)"', line):
                flags.setdefault(m.group(1), ('command-line arg', at))
            for m in re.finditer(r'(PlayerPrefs|EditorPrefs|SessionState)\.\w+\((\w+|"[^"]+")', line):
                key = m.group(2).strip('"') if m.group(2).startswith('"') else consts.get(m.group(2))
                if key:
                    flags.setdefault(key, (m.group(1), at))
            for m in re.finditer(r'GetEnvironmentVariable\((\w+|"[^"]+")', line):
                key = m.group(1).strip('"') if m.group(1).startswith('"') else consts.get(m.group(1))
                if key:
                    flags.setdefault(key, ('environment variable', at))
        # constants declared on one line and used elsewhere (e.g. "tw.rig.stress" / "TW_BENCH")
        for name, value in consts.items():
            if re.fullmatch(r'(-tw\w+|tw\.[\w.]+|TW_\w+|TW\.[\w.]+)', value) and value not in flags:
                line = src[:src.index(f'"{value}"')].count('\n') + 1
                kind = 'EditorPrefs' if 'EditorPrefs' in src else 'PlayerPrefs' if 'PlayerPrefs' in src else \
                    'environment variable' if value.startswith('TW_') else 'command-line arg' if value.startswith('-') else 'key'
                flags[value] = (kind, f'{rel(cs)}:{line}')
    return flags


def flags_block():
    flags = find_flags()
    rows = [(f'`{k}`', kind, f'`{at}`', FLAG_EFFECT.get(k, '**undocumented: add to FLAG_EFFECT in Tools/codemap.py**'))
            for k, (kind, at) in sorted(flags.items(), key=lambda kv: kv[0].lower())]
    out = table(['Switch', 'Kind', 'Read at (under Assets/_Project)', 'Effect'], rows)
    srows = []
    for f, name, effect in STATIC_SWITCHES:
        src = read(PROJ / f)
        m = re.search(r'public (?:static )?[\w?<>.]+ ' + name + r'\b', src)
        line = src[:m.start()].count('\n') + 1 if m else '?'
        srows.append((f'`{name}`', f'`{f}:{line}`', effect))
    out += '\nCode and inspector switches (static fields or `SimHost` inspector fields):\n\n'
    out += table(['Field', 'Declared at (under Assets/_Project)', 'Effect'], srows)
    return out

# ---- generate / check -------------------------------------------------------------------------------------------

def generated():
    return {
        CODE_MAP: {'assemblies': assemblies(), 'folders': folders_block(), 'sim-order': sim_order()},
        FLAGS: {'flags': flags_block()},
        TASKS: {'hooks': hooks_block(), 'tests': tests_block()},
    }


def write_all():
    for doc, new in generated().items():
        text = read(doc)
        missing = set(new) - set(blocks(text))
        if missing:
            sys.exit(f'{doc.name}: no <!-- gen:{", ".join(sorted(missing))} --> marker to write into')
        doc.write_text(replace_blocks(text, new), encoding='utf-8', newline='\n')
        print(f'wrote {rel(doc, REPO)}')


PATH_TOKEN = re.compile(r'^[\w.\-/]+\.(cs|py|md|shader|hlsl|uss|uxml|tss|asmdef|ps1|unity|asset|json|sh|txt|csv|png|jpg|'
                        r'jpeg|bytes|fbx|xml|yml)(:\d+(?:-\d+)?)?$|^[\w.\-]+(/[\w.\-]+)+/$')
TOOL_WORD = re.compile(r'^Tools/[\w.\-]+$')
SEARCH_BASES = [REPO, ROOT, PROJ, REPO / 'docs', REF]
SKIP_DIRS = {'.git', 'Library', 'Temp', 'Logs', 'obj', 'github-test1', 'UserSettings', 'Builds', 'Captures'}
_index = None


def by_name(name: str):
    global _index
    if _index is None:
        _index = {}
        for p in REPO.rglob('*'):
            if any(part in SKIP_DIRS for part in p.relative_to(REPO).parts):
                continue
            _index.setdefault(p.name, []).append(p)
    return _index.get(name, [])


def resolve(token: str):
    path = token.split(':')[0].rstrip('/')
    hits = [b / path for b in SEARCH_BASES if (b / path).exists()]
    if not hits and '/' not in path:
        hits = by_name(path)
    if not hits and '/' in path:   # a path relative to some folder we did not guess: match by suffix
        tail = path.split('/')
        hits = [p for p in by_name(tail[-1]) if list(p.parts[-len(tail):]) == tail]
    return hits


def check_citations(errors):
    docs = [CLAUDE, REPO / 'docs' / 'README.md'] + sorted(p for p in REF.glob('*.md') if p != MEMORY)
    for doc in docs:
        if not doc.exists():
            continue
        text = outside_blocks(read(doc))
        fenced = False
        for ln, line in enumerate(text.split('\n'), 1):
            # Commands: a script run in a code block or in a multi-word `python Tools/x.py args` must exist, or a
            # renamed tool leaves every recipe that runs it silently wrong. Only Tools/<name>: deeper paths are outputs.
            if line.lstrip().startswith('```'):
                fenced = not fenced
                continue
            words = line.split() if fenced else [w for t in re.findall(r'`([^`\n]+)`', line) if ' ' in t.strip()
                                                 for w in t.split()]
            for w in words:
                w = w.strip('`\'",;:()')
                w = w[2:] if w.startswith('./') else w
                if TOOL_WORD.match(w) and w not in ALLOW_MISSING and not resolve(w):
                    errors.append(f'{rel(doc, REPO)}:{ln}: runs `{w}`, which does not exist')
            for tok in re.findall(r'`([^`\n]+)`', line):
                tok = tok.strip()
                if tok in ALLOW_MISSING or '..' in tok or any(c in tok for c in '<>*{}~\\% $') or not PATH_TOKEN.match(tok):
                    continue
                hits = resolve(tok)
                if not hits:
                    errors.append(f'{rel(doc, REPO)}:{ln}: cites `{tok}`, which does not exist')
                    continue
                m = re.search(r':(\d+)(?:-(\d+))?$', tok)
                if m:
                    last = int(m.group(2) or m.group(1))
                    if not any(h.is_file() and last <= len(read(h).split('\n')) for h in hits):
                        errors.append(f'{rel(doc, REPO)}:{ln}: cites `{tok}`, past the end of the file')


def check(errors, warnings):
    for doc, new in generated().items():
        if not doc.exists():
            errors.append(f'{rel(doc, REPO)} is missing')
            continue
        have = blocks(read(doc))
        for name, body in new.items():
            if name not in have:
                errors.append(f'{rel(doc, REPO)}: no <!-- gen:{name} --> block')
            elif have[name] != body:
                errors.append(f'{rel(doc, REPO)}: generated block "{name}" is stale. Run: python Tools/codemap.py')
    folders = set(project_folders())
    for f in sorted(folders - set(PURPOSE)):
        errors.append(f'Assets/_Project/{f}/ has no purpose line: add it to PURPOSE in Tools/codemap.py')
    for f in sorted(set(PURPOSE) - folders):
        errors.append(f'PURPOSE in Tools/codemap.py names Assets/_Project/{f}/, which no longer exists')
    for k in sorted(set(find_flags()) - set(FLAG_EFFECT)):
        errors.append(f'runtime switch "{k}" is undocumented: add it to FLAG_EFFECT in Tools/codemap.py')
    for f, name, _ in STATIC_SWITCHES:
        if not re.search(r'public (?:static )?[\w?<>.]+ ' + name + r'\b', read(PROJ / f)):
            errors.append(f'STATIC_SWITCHES in Tools/codemap.py: {name} is no longer declared in {f}')
    check_citations(errors)

    if TASKS.exists():
        hand = outside_blocks(read(TASKS))
        for cs in cs_files(PROJ / 'Tests'):
            if not re.search(r'\b' + cs.stem + r'\b', hand):
                errors.append(f'tasks.md never names test {cs.stem}: put it in the row of the area it guards')
        for mname in scene_hook_members():
            if not re.search(r'\b' + mname + r'\b', hand):
                errors.append(f'tasks.md never names SceneHooks.{mname}: put it in the row of the area it wires')
    tool_docs = (read(PIPELINES) if PIPELINES.exists() else '') + (read(WORKFLOW) if WORKFLOW.exists() else '')
    for py in sorted((ROOT / 'Tools').glob('*.py')):
        if py.name not in tool_docs:
            errors.append(f'Tools/{py.name} is in neither pipelines.md nor workflow.md')
    tw = read(ROOT / 'Tools' / 'tw')
    for sub in re.findall(r'^  (\w+)\)', tw, re.M):
        if f'tw {sub}' not in tool_docs:
            errors.append(f'`tw {sub}` is in neither pipelines.md nor workflow.md')

    if MEMORY.exists():
        n = len(read(MEMORY).rstrip('\n').split('\n'))
        if n > MEMORY_MAX_LINES:
            errors.append(f'agent-memory.md is {n} lines (cap {MEMORY_MAX_LINES}). Move facts to their reference page, '
                          'fold or delete old incidents.')
    if CLAUDE.exists():
        text = read(CLAUDE)
        n = len(text.rstrip('\n').split('\n'))
        if n > CLAUDE_MAX_LINES:
            errors.append(f'CLAUDE.md is {n} lines (cap {CLAUDE_MAX_LINES}): it is read by every session, keep it short')
        m = re.search(r'In flight \(as of (\d{4}-\d{2}-\d{2})\)', text)
        if not m:
            errors.append('CLAUDE.md has no "In flight (as of YYYY-MM-DD)" block')
        else:
            age = (_dt.date.today() - _dt.date.fromisoformat(m.group(1))).days
            if age > IN_FLIGHT_MAX_DAYS:
                warnings.append(f'CLAUDE.md "In flight" block is {age} days old: update it (branches, who, what)')


def main():
    if '--check' in sys.argv:
        errors, warnings = [], []
        check(errors, warnings)
        for w in warnings:
            print('warning: ' + w)
        if errors:
            print('\n'.join('codemap: ' + e for e in errors))
            sys.exit(1)
        print('codemap OK')
    else:
        write_all()


if __name__ == '__main__':
    main()
