"""What the C# says about the assets: ids, rosters, which model draws what, where buildings are placed, the levels.

Every reader here is a PROBE: it names the file, the anchor it looks under and the pattern it expects, and it raises
ProbeError when it finds too little or misses a value it must find. A table that moved or changed shape stops the
build with the place to look, rather than quietly producing a board with a hole in it.
"""
import re
from pathlib import Path


class ProbeError(Exception):
    pass


def read(p: Path) -> str:
    return p.read_text(encoding='utf-8-sig')


def body_after(src: str, anchor: str, where: str, open_ch='{', close_ch='}') -> str:
    """The text between the first brace after `anchor` and its match."""
    at = src.find(anchor)
    if at < 0:
        raise ProbeError(f'{where}: no "{anchor}"')
    start = src.find(open_ch, at)
    depth, i = 0, start
    while i < len(src):
        if src[i] == open_ch:
            depth += 1
        elif src[i] == close_ch:
            depth -= 1
            if depth == 0:
                return src[start + 1:i]
        i += 1
    raise ProbeError(f'{where}: the block after "{anchor}" never closes')


def need(found, least, where, what, sentinels=()):
    if len(found) < least:
        raise ProbeError(f'{where}: found {len(found)} {what}, expected at least {least}')
    for s in sentinels:
        if s not in found:
            raise ProbeError(f'{where}: {what} do not include {s!r}')
    return found


def consts(src, cls, where, sentinels):
    """name -> (id, the comment on its line) for the byte constants of an archetype class."""
    body = body_after(src, f'public static class {cls}', where)
    out = {}
    for line in body.split('\n'):
        if 'public const byte' not in line:
            continue
        code, _, comment = line.partition('//')
        pairs = re.findall(r'(\w+)\s*=\s*(\d+)\s*[,;]', code)
        for name, value in pairs:
            out[name] = (int(value), comment.strip() if len(pairs) == 1 else '')
    out.pop('Max', None)
    need(out, len(sentinels), where, f'{cls} constants', [n for n, _ in sentinels])
    for n, v in sentinels:
        if out[n][0] != v:
            raise ProbeError(f'{where}: {cls}.{n} is {out[n][0]}, the board expected {v}')
    return out


def arch_refs(text):
    """The archetype names in a piece of code, in order: (class, name)."""
    return re.findall(r'\b(InfantryArchetype|VehicleArchetype)\.(\w+)', text)


def read_all(P: Path):
    """Every table the board needs, as plain data. P is Assets/_Project."""
    t = {}
    f = P / 'Sim/Core/RosterEntry.cs'
    src = read(f)
    t['infantry'] = consts(src, 'InfantryArchetype', f.name, [('Rifle', 0), ('Sniper', 3)])
    t['vehicle'] = consts(src, 'VehicleArchetype', f.name, [('Maw', 4), ('Tusk', 5)])
    shipped = [n for _, n in arch_refs(body_after(src, 'RosterEntry ForArchetype(byte archetype)', f.name))]
    t['shipped'] = need(shipped, 10, f.name, 'ForArchetype cases', ['Rifle', 'Maw'])
    known = set(t['infantry']) | set(t['vehicle'])

    f = P / 'Sim/Match/UnitDefinitions.cs'
    defined = re.findall(r'\b([A-Z]\w+)\b', re.sub(r'//.*', '', body_after(read(f), 'UnitDef[] All =', f.name)))
    t['defined'] = need(defined, 2, f.name, 'units in UnitDefinitions.All')
    for n in t['defined'] + t['shipped']:
        if n not in known:
            raise ProbeError(f'{f.name}: {n} is fielded but is not an archetype constant in RosterEntry.cs')

    f = P / 'Sim/Core/Faction.cs'
    factions = re.findall(r'(\w+)\s*=\s*\d+', body_after(read(f), 'enum FactionId', f.name))
    t['factions'] = need(factions, 2, f.name, 'factions', ['Iron', 'Brass'])
    f = P / 'Sim/Core/FactionRoster.cs'
    src = read(f)
    slots = [n for _, n in arch_refs(body_after(src, 'byte[] Slots =', f.name))]
    if not slots or len(slots) % len(factions):
        raise ProbeError(f'{f.name}: {len(slots)} roster slots do not divide over {len(factions)} factions')
    per = len(slots) // len(factions)
    t['slots'] = {fac: slots[i * per:(i + 1) * per] for i, fac in enumerate(factions)}
    pools = re.findall(r'new byte\[\]\s*\{([^}]*)\}', body_after(src, 'byte[][] Pools =', f.name))
    if len(pools) != len(factions):
        raise ProbeError(f'{f.name}: {len(pools)} pools for {len(factions)} factions')
    t['pools'] = {fac: [n for _, n in arch_refs(pools[i])] for i, fac in enumerate(factions)}
    for n in slots + [x for p in t['pools'].values() for x in p]:
        if n not in known:
            raise ProbeError(f'{f.name}: the roster names {n}, which is not an archetype constant')

    f = P / 'Presentation/Camera/TankRenderer.cs'
    src = read(f)
    rows = re.findall(r'\("(\w+)",\s*VehicleArchetype\.(\w+),\s*"(\w+)",\s*([\w.]+),\s*(true|false)\)',
                      body_after(src, 'Machines =', f.name))
    need([r[0] for r in rows], 1, f.name, 'rows in Machines', ['Pincer'])
    t['machines'] = {arch: dict(model=model, root=root, scale=scale, hover=hover == 'true') for model, arch, root, scale, hover in rows}
    fallback = body_after(src, 'TankModel ModelFor(byte archetype)', f.name)
    if 'VehicleArchetype.Tusk' not in fallback or 'maw' not in fallback:
        raise ProbeError(f'{f.name}: ModelFor no longer falls back to the Tusk and the Maw; the board\'s "drawn as" rule needs a look')

    f = P / 'Presentation/Units/VATRenderer.cs'
    src = read(f)
    m = re.search(r'FigureNames\s*=\s*\{([^}]*)\}', src)
    if not m:
        raise ProbeError(f'{f.name}: no FigureNames table')
    t['figures'] = need(re.findall(r'"(\w+)"', m.group(1)), 1, f.name, 'figure names', ['Soldier'])
    m = re.search(r'FigureOfArchetype\(int archetype\)\s*=>\s*archetype == (\d+) \? (\d+) : (\d+);', src)
    if not m:
        raise ProbeError(f'{f.name}: FigureOfArchetype is no longer "archetype == N ? a : b"; teach the board the new rule')
    t['figure_rule'] = dict(archetype=int(m.group(1)), then=int(m.group(2)), other=int(m.group(3)))

    f = P / 'Presentation/Core/UnitLook.cs'
    src = read(f)
    portraits = {}
    m = re.search(r'InfantryPortraits\s*=\s*\{([^}]*)\}', src)
    if not m:
        raise ProbeError(f'{f.name}: no InfantryPortraits table')
    by_id = {v[0]: n for n, v in t['infantry'].items()}
    for i, name in enumerate(re.findall(r'"(\w+)"', m.group(1))):
        portraits[by_id[i]] = name
    for fn in ('static string FootName(byte archetype)', 'public static string VehicleName(byte archetype)'):
        for arch, name in re.findall(r'case \w+Archetype\.(\w+):\s*return "(\w+)"', body_after(src, fn, f.name)):
            portraits[arch] = name
    t['portraits'] = need(portraits, 10, f.name, 'portrait names', ['Maw', 'Officer'])

    f = P / 'Presentation/Terrain/BattlefieldKit.cs'
    src = read(f)
    kit = {field: (s, n) for field, s, n in re.findall(r'(\w+)\s*=\s*(?:Small\()?Imported\("(\w+)",\s*"(\w+)"', src)}
    t['kit_fields'] = need(kit, 10, f.name, 'Imported(...) props', ['mgNest'])
    m = re.search(r'foreach \(var set in new\[\] \{([^}]*)\}\) all\.AddRange\(BuildingSet', src)
    if not m:
        raise ProbeError(f'{f.name}: no list of building sets handed to BuildingSet')
    t['building_sets'] = need(re.findall(r'"(\w+)"', m.group(1)), 2, f.name, 'building sets', ['Houses', 'Military'])

    placed_sets, placed_fields = {}, {}
    for cs in sorted((P / 'Presentation/Terrain').glob('BattlefieldComposer*.cs')):
        src = re.sub(r'//.*', '', read(cs))
        for s in re.findall(r'h\.Set == "(\w+)"\)\s*;', src) + re.findall(r'FindAll\(kit\.Houses, h => h\.Set == "(\w+)"\)', src):
            placed_sets.setdefault(s, cs.name)
        for field in re.findall(r'\bkit\.(\w+)\b', src):
            if field in kit:
                placed_fields.setdefault(field, cs.name)
    need(placed_sets, 1, 'BattlefieldComposer*.cs', 'building sets a composer places', ['Houses'])
    t['placed_sets'], t['placed_fields'] = placed_sets, placed_fields
    hamlets = body_after(read(P / 'Presentation/Terrain/BattlefieldComposer.Buildings.cs'), 'void PlaceHamlets', 'BattlefieldComposer.Buildings.cs')
    if 'PropKind.Bridge' not in hamlets or 'WaterLevel' not in hamlets:
        raise ProbeError('BattlefieldComposer.Buildings.cs: PlaceHamlets no longer asks for water and a bridge; '
                         'the board\'s "Houses only where there is a river" rule needs a look')

    f = P / 'Presentation/Core/MatchLaunch.cs'
    grounds = re.findall(r'(\w+)\s*=\s*\d+', body_after(read(f), 'public enum Ground', f.name))
    need(grounds, 1, f.name, 'grounds', ['ShelledForest'])
    f = P / 'Sim/Terrain/BattlefieldGenerator.cs'
    src = read(f)
    t['grounds'] = {}
    for g in grounds:
        m = re.search(r'BattlefieldParams ' + g + r'\(uint seed\)\s*=>\s*new BattlefieldParams\s*\{([^}]*)\}', src)
        if not m:
            raise ProbeError(f'{f.name}: no preset for the ground {g}')
        flags = dict(re.findall(r'(River|Sea)\s*=\s*(true|false)', m.group(1)))
        t['grounds'][g] = dict(river=flags.get('River') == 'true', sea=flags.get('Sea') == 'true')

    f = P / 'UI/Campaign/CampaignGraph.cs'
    missions = re.findall(r'M\("([^"]+)",\s*"([^"]+)",\s*Ground\.(\w+),\s*(\d+)', read(f))
    t['missions'] = [dict(name=n, node=node, ground=g, seed=int(seed)) for n, node, g, seed in need(missions, 3, f.name, 'campaign missions')]

    f = P / 'UI/Campaign/FactionBuildings.cs'
    hf = re.findall(r'Id = "([\w-]+)",\s*Name = "([^"]+)",\s*Faction = (\w+),\s*Set = "(\w+)",\s*Model = "(\w+)"', read(f))
    t['home_front'] = [dict(id=i, name=n, faction=fa, set=s, model=mo) for i, n, fa, s, mo in need(hf, 2, f.name, 'Home Front buildings')]

    f = P / 'Sim/Core/SimEvents.cs'
    t['events'] = []
    if f.exists():
        body = re.sub(r'/\*.*?\*/', '', body_after(read(f), 'enum SimEventType', f.name), flags=re.S)
        t['events'] = [e for e in re.findall(r'^\s*(\w+)\s*(?:=\s*\d+\s*)?,', re.sub(r'//.*', '', body), re.M) if e != 'None']

    f = P / 'Presentation/Core/AnimationController.cs'
    t['clips'] = clips(read(f), f.name)
    return t


def clips(src, where):
    """The men's clips in the order of the enum, which is the order of a baked atlas's rows: [(name, row, group)].
    The group is the comment line above (idle, locomotion, fire, ...), up to its first bracket."""
    out, group = [], ''
    for line in body_after(src, 'public enum Clip', where).split('\n'):
        code, _, comment = line.partition('//')
        if not code.strip() and comment.strip():
            group = comment.split('(')[0].strip()
        for name in re.findall(r'\b([A-Za-z_]\w*)\b', code):
            out.append((name, len(out), group))
    need([n for n, _, _ in out], 20, where, 'Clip names', ['None', 'Idle', 'Walk', 'FireStand', 'Count'])
    if out[0][0] != 'None' or out[1][0] != 'Idle' or out[-1][0] != 'Count':
        raise ProbeError(f'{where}: the Clip enum no longer runs None, Idle, ... Count')
    return out[:-1]


def mentions(P: Path, roots, pattern):
    """file name -> hits of a regex, over the C# under the given folders of Assets/_Project."""
    out = {}
    rx = re.compile(pattern)
    for root in roots:
        for cs in sorted((P / root).rglob('*.cs')):
            n = len(rx.findall(read(cs)))
            if n:
                out[cs.relative_to(P).as_posix()] = n
    return out
