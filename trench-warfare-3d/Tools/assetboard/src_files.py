"""What the files say about the assets: models per LOD, manifests, sockets inside the FBX, portraits, tests."""
import json
import re
from pathlib import Path

NODE = re.compile(rb'([A-Za-z_]\w*)\x00\x01Model')


def load(p: Path):
    return json.loads(p.read_text(encoding='utf-8-sig'))


def fbx_nodes(p: Path):
    """The node names of a binary FBX, read from its bytes: (parts, sockets). No Blender needed."""
    names = sorted({m.decode('ascii', 'replace') for m in NODE.findall(p.read_bytes())})
    sockets = [n for n in names if n.startswith('Socket_')]
    parts = [n for n in names if not n.startswith('Socket_') and '_LOD' not in n and n != p.stem]
    return parts, sockets


def rel(p: Path, P: Path) -> str:
    return p.relative_to(P).as_posix()


def vehicle_models(P: Path, name: str, crabs: dict):
    """The model forms a machine has on disk: 'battle' (Resources/Vehicles) and 'trial' (Playground/Art/Tanks)."""
    out = []
    folder = P / 'Resources/Vehicles' / name
    lods = sorted(folder.glob(f'{name}_LOD*.fbx')) if folder.is_dir() else []
    if lods:
        atlas = P / 'Resources/Vehicles' / f'{name}Atlas.jpg'
        if not atlas.exists():
            atlas = P / 'Resources/Vehicles/TankAtlas_LOD0.jpg'   # the Maw and the Tusk share one
        parts, sockets = fbx_nodes(lods[0])
        man = crabs.get(name)
        tris = {l['lod']: l['tris'] for l in man['lods']} if man else {}
        out.append(dict(form='battle', lods=[dict(lod=i, path=rel(f, P), tris=tris.get(i)) for i, f in enumerate(lods)],
                        texture=rel(atlas, P) if atlas.exists() else None, manifest='Resources/Vehicles/crabs.json' if man else None,
                        parts=parts, sockets=sorted(man['sockets']) if man else sockets, size_m=man.get('size_m') if man else None))
    folder = P / 'Playground/Art/Tanks' / name
    man_file = folder / 'tank3.json'
    lods = sorted(folder.glob(f'{name}_LOD*.fbx')) if folder.is_dir() else []
    if lods:
        man = load(man_file) if man_file.exists() else {}
        tris = {l['lod']: l['tris'] for l in man.get('lodList', [])}
        tex = folder / f'{name}_LOD0_Base.jpg'
        out.append(dict(form='trial', lods=[dict(lod=i, path=rel(f, P), tris=tris.get(i)) for i, f in enumerate(lods)],
                        texture=rel(tex, P) if tex.exists() else None, manifest=rel(man_file, P) if man else None,
                        parts=[p['name'] for p in man.get('partList', [])], sockets=sorted(man.get('sockets', {})),
                        source=man.get('source'), size_m=None))
    return out


def figure_models(P: Path, figure: str):
    """A figure's forms: the source FBX, the baked battle figure (Resources/Units), a playground rig."""
    out = []
    baked = sorted((P / 'Resources/Units').glob(f'Figure{figure}*'))
    baked = [f for f in baked if f.suffix != '.meta']
    src = P / 'Art/Characters' / f'{figure}.fbx'
    if baked:
        out.append(dict(form='battle', lods=[dict(lod=0, path=rel(f, P), tris=None) for f in baked],
                        texture=None, manifest=None, parts=[], sockets=[], source=rel(src, P) if src.exists() else None, size_m=None))
    rig = P / 'Playground/Art/Units' / figure
    man_file = rig / 'frogrig.json'
    if man_file.exists():
        man = load(man_file)
        fbx = rig / f'{figure}.fbx'
        tex = rig / f'{figure}_LOD0_Base.jpg'
        out.append(dict(form='trial', lods=[dict(lod=l['lod'], path=rel(fbx, P), tris=l['tris']) for l in man['lods']],
                        texture=rel(tex, P) if tex.exists() else None, manifest=rel(man_file, P), parts=[], sockets=[],
                        source=man.get('source'), size_m=[None, man.get('height_m'), None], bones=len(man.get('skeleton', []))))
    return out


def houses(P: Path, sets):
    """house name -> dict(set, chunks, tris, verts, mats, rows) from each set's houses.json."""
    out = {}
    for s in sets:
        f = P / 'Resources/Env' / s / 'houses.json'
        if not f.exists():
            continue
        for row in load(f)['chunks']:
            h = out.setdefault(row['house'], dict(set=s, chunks=0, tris=0, verts=0, mats={}, manifest=rel(f, P), rows=[]))
            h['chunks'] += 1
            h['tris'] += row['tris']
            h['verts'] += row['verts']
            h['mats'][row.get('mat', '?')] = h['mats'].get(row.get('mat', '?'), 0) + 1
            h['rows'].append(row)
    return out


def chunk_file(P: Path, set_name: str, chunk: str):
    for cand in (P / 'Resources/Env' / set_name / f'{chunk}.fbx', P / 'Resources/Env' / set_name / 'Chunks' / f'{chunk}.fbx'):
        if cand.exists():
            return cand
    return None


def pictures(P: Path):
    """What the UI holds per name: card portraits, cut-outs, mood busts, and which portraits are generated placeholders."""
    skin = P / 'UI/Skin'
    placeholders = set()
    ph = skin / 'placeholders.json'
    if ph.exists():
        placeholders = {Path(f['file']).stem for f in load(ph)['files'] if f['file'].startswith('Portraits/')}
    moods = {}
    for f in sorted((P / 'UI/Resources/UnitArt/States').glob('*.png')):
        moods.setdefault(f.stem.rsplit('_', 1)[0], []).append(rel(f, P))
    return dict(portraits={f.stem: rel(f, P) for f in sorted((skin / 'Portraits').glob('*.png'))},
                unitart={f.stem: rel(f, P) for f in sorted((P / 'UI/Resources/UnitArt').glob('*.png'))},
                moods=moods, placeholders=placeholders)


def library(P: Path):
    """Names in the playground library before its clip list starts (vehicles and units on trial)."""
    f = P / 'Playground/PlaygroundLibrary.asset'
    if not f.exists():
        return []
    text = f.read_text(encoding='utf-8', errors='replace')
    head = text.split('Clips:')[0] if 'Clips:' in text else text
    return re.findall(r'^\s*- Name: (\w+)\s*$', head, re.M)


def tests_naming(P: Path, names):
    """name -> [(test file, hits)] for the tests that name an asset as an archetype constant or a quoted string."""
    files = {cs: cs.read_text(encoding='utf-8-sig', errors='replace') for cs in sorted((P / 'Tests').rglob('*.cs'))}
    files.update({cs: cs.read_text(encoding='utf-8-sig', errors='replace') for cs in sorted((P / 'Playground/Tests').rglob('*.cs'))})
    out = {}
    for n in names:
        rx = re.compile(r'Archetype\.' + re.escape(n) + r'\b|"' + re.escape(n) + r'"')
        hits = [(rel(cs, P), len(rx.findall(src))) for cs, src in files.items()]
        out[n] = sorted([h for h in hits if h[1]], key=lambda h: -h[1])
    return out
