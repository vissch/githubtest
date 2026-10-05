#!/usr/bin/env python3
"""The gate on imported art: every model FBX under Assets/_Project measured from the file itself. No Unity, no Blender.

    python Tools/assetgate.py                from trench-warfare-3d/: check the tree against Tools/assetgate.json
    python Tools/assetgate.py --record       measure everything again, rewrite Tools/assetgate.json, print what moved
    python Tools/assetgate.py --file F [--kind vehicle|bench|figure|prop]
                                             report one FBX and its LOD siblings, in the tree or not (a fresh split)
    python Tools/assetgate.py --project DIR  another checkout

WHY. A split script's mistakes were all silent: a part not under the Hull is never drawn (TankModel walks from the
root part), a lower LOD decimated too far tears into spikes and holes, a pivot that moves between LODs makes the part
jump at the switch, and nothing counted triangles against the owner's budget (decisions.md, 2026-09-21). They were
found by eye in Play, days later. The files are small and plain, so this reads them and says so before the commit.

HARD RULES fail the check unless Tools/assetgate.waivers.json carries a line with the reason (a waiver that no
longer matches anything fails too, so the list cannot rot):
    root        vehicle          one node named Hull or Body, and every mesh and Socket_* is it or under it
                                 (the bench's FBXs are flat: its rig reads the parents from tank3.json)
    lod-names   vehicle, bench   a lower LOD has no node LOD0 lacks
    lod-pivot   vehicle, bench   a node keeps its parent, position and rotation at every LOD
    lod-shape   vehicle, bench   a part at a lower LOD stays on LOD0's surface: no vertex further off than SPIKE of
                                 the part's size, and no LOD0 vertex further from it than HOLE
    uv-missing  all              a mesh without a UV set
    uv-bounds   all              a UV outside 0..1 (the atlases have no wrap)
    budget      vehicle, figure  LOD0 triangles of a battle vehicle, vertices of a battle figure
and a model that Tools/splitlog.py has a run on record for must still be the model that run wrote.
RECORDED NUMBERS (triangles, vertices, open and non-manifold edges, zero-area triangles, zero-area UV triangles, the
spread of texel density between parts) have no pass mark: Tripo's meshes are open by nature. They are written to
Tools/assetgate.json, one line per file, and the check fails when a file's bytes no longer match its line, so new
numbers always arrive in the same commit as the art and show in its diff. `--record` prints every number that got
worse. `shape` is a hash of the model itself (nodes, placements, vertices, faces, UVs): Blender stamps the time into
every FBX, so a split run twice gives new bytes and the same shape, and `--record` says so.

Animation clips (Art/Characters/Clips) are not models: Tools/clipcheck.py judges those.
"""
import argparse
import array
import hashlib
import json
import math
import os
import re
import sys
from collections import Counter
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
from fbxscan import read_fbx  # noqa: E402  (the one binary-FBX reader)

PROJECT = Path(__file__).resolve().parent.parent
ASSETS = 'Assets/_Project'
SKIP = ('Assets/_Project/Art/Characters/Clips/',)
BASELINE = 'Tools/assetgate.json'
WAIVERS = 'Tools/assetgate.waivers.json'

# Bump when a rule, a threshold or a recorded number changes meaning: the check then asks for a fresh --record.
RULESET = 1

ROOT_PARTS = ('Hull', 'Body')       # TankModel.Load's root argument: tanks and machines, walkers
UV_SLACK = 0.002
# lod-shape, as a share of the LOD0 part's bounding diagonal. Calibrated 2026-10-05 (docs/reference/pipelines.md).
SPIKE = 0.12
HOLE = 0.25
SMALL_PART = 24                     # a LOD0 part under this many triangles is too coarse to compare
BUDGET_VEHICLE_TRIS = 5000          # decisions.md 2026-09-21: vehicles 3,000-5,000
BUDGET_FIGURE_VERTS = 2000          # full model 1,200-1,500 verts (max 2,000)


# ---------------------------------------------------------------------------------------------------- reading a file

class Node:
    __slots__ = ('name', 'kind', 'parent', 't', 'r', 's', 'mesh')


class Mesh:
    __slots__ = ('v', 'polys', 'uv', 'uvpolys')


def _clean(s, kind):
    return s.split('\x00')[0].replace(kind + '::', '')


def _polys(index):
    out, cur = [], []
    for i in index:
        if i < 0:
            cur.append(-i - 1)
            out.append(cur)
            cur = []
        else:
            cur.append(i)
    return out


def load(path):
    """An FBX as {node name: Node}, in file order. A mesh's vertices are in its node's own space."""
    return build(read_fbx(str(path)))


def build(tree):
    """The same from an already parsed tree (the tests break a parsed tree, not a file)."""
    objects = next(n for n in tree if n[0] == 'Objects')[2]
    conns = next(n for n in tree if n[0] == 'Connections')[2]
    models, geoms = {}, {}
    for n in objects:
        if n[0] == 'Model':
            node = Node()
            node.name, node.kind, node.parent, node.mesh = _clean(n[1][1], 'Model'), n[1][2], None, None
            node.t, node.r, node.s = [0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [1.0, 1.0, 1.0]
            for c in n[2]:
                if c[0] != 'Properties70':
                    continue
                for p in c[2]:
                    if p[1][0] == 'Lcl Translation':
                        node.t = list(p[1][4:7])
                    elif p[1][0] == 'Lcl Rotation':
                        node.r = list(p[1][4:7])
                    elif p[1][0] == 'Lcl Scaling':
                        node.s = list(p[1][4:7])
            models[n[1][0]] = node
        elif n[0] == 'Geometry':
            m = Mesh()
            m.v, m.polys, m.uv, m.uvpolys = [], [], None, None
            uv_index = None
            for c in n[2]:
                if c[0] == 'Vertices':
                    m.v = c[1][0]
                elif c[0] == 'PolygonVertexIndex':
                    m.polys = _polys(c[1][0])
                elif c[0] == 'LayerElementUV' and m.uv is None:
                    for d in c[2]:
                        if d[0] == 'UV':
                            m.uv = d[1][0]
                        elif d[0] == 'UVIndex':
                            uv_index = d[1][0]
            if m.uv is not None:
                flat = uv_index if uv_index is not None else list(range(len(m.uv) // 2))
                m.uvpolys, k = [], 0
                for p in m.polys:
                    m.uvpolys.append(flat[k:k + len(p)])
                    k += len(p)
                if k != len(flat):
                    m.uvpolys = None            # not one UV per polygon corner: counted as missing
            geoms[n[1][0]] = m
    for c in conns:
        p = c[1]
        if p[0] != 'OO':
            continue
        src, dst = p[1], p[2]
        if src in models and dst in models:
            models[src].parent = models[dst].name
        elif src in geoms and dst in models:
            models[dst].mesh = geoms[src]
    return {m.name: m for m in models.values()}


def fingerprint(nodes):
    """One hash over what the file says: every node's name, parent and placement, every mesh's vertices, faces and
    UVs. Blender stamps the time into each FBX it writes, so two runs of one split differ in bytes and agree here."""
    h = hashlib.sha256()
    for name in sorted(nodes):
        n = nodes[name]
        h.update(repr((name, n.kind, n.parent, n.t, n.r, n.s, n.mesh is not None)).encode('utf-8'))
        if n.mesh is not None:
            h.update(array.array('d', n.mesh.v).tobytes())
            h.update(repr(n.mesh.polys).encode('utf-8'))
            h.update(array.array('d', n.mesh.uv or []).tobytes())
            h.update(repr(n.mesh.uvpolys).encode('utf-8'))
    return h.hexdigest()[:16]


# ------------------------------------------------------------------------------------------------- measuring a mesh

def _pt(v, i):
    return (v[3 * i], v[3 * i + 1], v[3 * i + 2])


def triangles(mesh):
    """Fan triangles as vertex-index triples."""
    return [(p[0], p[j], p[j + 1]) for p in mesh.polys for j in range(1, len(p) - 1)]


def diagonal(v):
    if not v:
        return 0.0
    xs, ys, zs = v[0::3], v[1::3], v[2::3]
    return math.dist((min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs)))


def tri_area(a, b, c):
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    return 0.5 * math.sqrt((uy * vz - uz * vy) ** 2 + (uz * vx - ux * vz) ** 2 + (ux * vy - uy * vx) ** 2)


def measure_mesh(mesh):
    """One part's numbers. Edges are counted on vertex indices, as Unity's importer sees the part."""
    v, diag = mesh.v, diagonal(mesh.v)
    edges = Counter()
    for p in mesh.polys:
        for a, b in zip(p, p[1:] + p[:1]):
            edges[(a, b) if a < b else (b, a)] += 1
    tiny = 1e-12 * max(diag, 1e-9) ** 2
    geo, uv_area, flat, uv_flat, out_of_bounds = 0.0, 0.0, 0, 0, 0
    has_uv = mesh.uv is not None and mesh.uvpolys is not None
    if has_uv:
        us, vs = mesh.uv[0::2], mesh.uv[1::2]
        out_of_bounds = sum(1 for a, b in zip(us, vs)
                            if a < -UV_SLACK or a > 1 + UV_SLACK or b < -UV_SLACK or b > 1 + UV_SLACK)
    for k, p in enumerate(mesh.polys):
        q = mesh.uvpolys[k] if has_uv else None
        for j in range(1, len(p) - 1):
            area = tri_area(_pt(v, p[0]), _pt(v, p[j]), _pt(v, p[j + 1]))
            if area <= tiny:
                flat += 1
                continue
            geo += area
            if q:
                a = abs((us[q[j]] - us[q[0]]) * (vs[q[j + 1]] - vs[q[0]])
                        - (us[q[j + 1]] - us[q[0]]) * (vs[q[j]] - vs[q[0]])) / 2
                uv_area += a
                if a <= 1e-12:
                    uv_flat += 1
    return dict(verts=len(v) // 3, tris=sum(len(p) - 2 for p in mesh.polys),
                open=sum(1 for c in edges.values() if c == 1), nonmanifold=sum(1 for c in edges.values() if c > 2),
                flat=flat, uv=has_uv, uv_out=out_of_bounds, uv_flat=uv_flat, geo=geo, uv_area=uv_area, diag=diag)


# ------------------------------------------------------------------------------ how far one part is from another's skin

def _closest_sq(p, a, b, c):
    """Squared distance from point p to triangle abc (Ericson, Real-Time Collision Detection 5.1.5)."""
    abx, aby, abz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    acx, acy, acz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    apx, apy, apz = p[0] - a[0], p[1] - a[1], p[2] - a[2]
    d1 = abx * apx + aby * apy + abz * apz
    d2 = acx * apx + acy * apy + acz * apz
    if d1 <= 0 and d2 <= 0:
        return apx * apx + apy * apy + apz * apz
    bpx, bpy, bpz = p[0] - b[0], p[1] - b[1], p[2] - b[2]
    d3 = abx * bpx + aby * bpy + abz * bpz
    d4 = acx * bpx + acy * bpy + acz * bpz
    if d3 >= 0 and d4 <= d3:
        return bpx * bpx + bpy * bpy + bpz * bpz
    vc = d1 * d4 - d3 * d2
    if vc <= 0 and d1 >= 0 and d3 <= 0:
        t = d1 / (d1 - d3)
        return (apx - t * abx) ** 2 + (apy - t * aby) ** 2 + (apz - t * abz) ** 2
    cpx, cpy, cpz = p[0] - c[0], p[1] - c[1], p[2] - c[2]
    d5 = abx * cpx + aby * cpy + abz * cpz
    d6 = acx * cpx + acy * cpy + acz * cpz
    if d6 >= 0 and d5 <= d6:
        return cpx * cpx + cpy * cpy + cpz * cpz
    vb = d5 * d2 - d1 * d6
    if vb <= 0 and d2 >= 0 and d6 <= 0:
        t = d2 / (d2 - d6)
        return (apx - t * acx) ** 2 + (apy - t * acy) ** 2 + (apz - t * acz) ** 2
    va = d3 * d6 - d5 * d4
    if va <= 0 and (d4 - d3) >= 0 and (d5 - d6) >= 0:
        t = (d4 - d3) / ((d4 - d3) + (d5 - d6))
        return (bpx - t * (c[0] - b[0])) ** 2 + (bpy - t * (c[1] - b[1])) ** 2 + (bpz - t * (c[2] - b[2])) ** 2
    denom = va + vb + vc
    if denom == 0:                                  # a zero-area triangle: its first corner will do
        return apx * apx + apy * apy + apz * apz
    s, t = vb / denom, vc / denom
    return (apx - s * abx - t * acx) ** 2 + (apy - s * aby - t * acy) ** 2 + (apz - s * abz - t * acz) ** 2


class Skin:
    """A part's triangles in a grid, to ask how far a point is from the surface."""

    def __init__(self, mesh):
        v = mesh.v
        self.tris = [(_pt(v, a), _pt(v, b), _pt(v, c)) for a, b, c in triangles(mesh)]
        self.cell = max(diagonal(v) / 12.0, 1e-9)
        self.grid = {}
        for i, (a, b, c) in enumerate(self.tris):
            lo = [int(math.floor(min(a[k], b[k], c[k]) / self.cell)) for k in range(3)]
            hi = [int(math.floor(max(a[k], b[k], c[k]) / self.cell)) for k in range(3)]
            for x in range(lo[0], hi[0] + 1):
                for y in range(lo[1], hi[1] + 1):
                    for z in range(lo[2], hi[2] + 1):
                        self.grid.setdefault((x, y, z), []).append(i)
        keys = list(self.grid)
        self.lo = [min(k[d] for k in keys) for d in range(3)] if keys else [0, 0, 0]
        self.hi = [max(k[d] for k in keys) for d in range(3)] if keys else [0, 0, 0]

    def distance(self, p):
        if not self.tris:
            return float('inf')
        c = [int(math.floor(p[k] / self.cell)) for k in range(3)]
        reach = max(max(self.hi[k] - c[k], c[k] - self.lo[k]) for k in range(3)) + 1
        best, seen, r = float('inf'), set(), 0
        while r <= reach:
            for x in range(c[0] - r, c[0] + r + 1):
                for y in range(c[1] - r, c[1] + r + 1):
                    edge = abs(x - c[0]) == r or abs(y - c[1]) == r
                    for z in (range(c[2] - r, c[2] + r + 1) if edge else (c[2] - r, c[2] + r)):
                        for i in self.grid.get((x, y, z), ()):
                            if i not in seen:
                                seen.add(i)
                                d = _closest_sq(p, *self.tris[i])
                                if d < best:
                                    best = d
            # everything within r cells of the point's cell is searched: a hit closer than that cannot be beaten
            if best <= (r * self.cell) ** 2:
                break
            r += 1
        return math.sqrt(best)


def shape_drift(low, ref):
    """(spike, hole) of a lower-LOD part against LOD0's, each as a share of LOD0's bounding diagonal: the furthest a
    vertex of the lower part stands off LOD0's surface, and the furthest a LOD0 vertex is from the lower part's."""
    diag = diagonal(ref.v)
    if diag <= 0 or not low.polys or not ref.polys:
        return (0.0, 0.0)
    ref_skin, low_skin = Skin(ref), Skin(low)
    used_low = sorted({i for p in low.polys for i in p})
    used_ref = sorted({i for p in ref.polys for i in p})
    spike = max(ref_skin.distance(_pt(low.v, i)) for i in used_low)
    hole = max(low_skin.distance(_pt(ref.v, i)) for i in used_ref)
    return (spike / diag, hole / diag)


# ------------------------------------------------------------------------------------------------ judging a file

def kind_of(rel):
    if rel.startswith(ASSETS + '/Resources/Vehicles/'):
        return 'vehicle'
    if rel.startswith(ASSETS + '/Playground/Art/Tanks/'):
        return 'bench'
    if rel.startswith(ASSETS + '/Art/Characters/'):
        return 'figure'
    return 'prop'


LOD = re.compile(r'^(.*)_LOD(\d+)\.fbx$', re.I)


def lod_of(rel):
    """(group key, lod number) for Name_LOD<k>.fbx, else (None, None)."""
    m = LOD.match(rel)
    return (m.group(1), int(m.group(2))) if m else (None, None)


def _under(nodes, name, root):
    seen = set()
    while name is not None and name not in seen:
        if name == root:
            return True
        seen.add(name)
        name = nodes[name].parent if name in nodes else None
    return False


def judge(rel, nodes, kind, ref=None):
    """One file's recorded numbers and the hard rules it breaks: (numbers, [(rule, message)]). `ref` is the LOD0
    file's nodes when this is a lower LOD of a vehicle or bench model."""
    found, total = [], Counter()
    parts, densities = {}, []
    for node in nodes.values():
        if node.mesh is None:
            continue
        m = measure_mesh(node.mesh)
        parts[node.name] = m
        for key in ('verts', 'tris', 'open', 'nonmanifold', 'flat', 'uv_flat'):
            total[key] += m[key]
        if not m['uv']:
            found.append(('uv-missing', f'{node.name} has no UV set'))
        elif m['uv_out']:
            found.append(('uv-bounds', f'{node.name} has {m["uv_out"]} UVs outside 0..1'))
        if m['uv'] and m['geo'] > 0 and m['uv_area'] > 0 and m['tris'] >= SMALL_PART:
            densities.append(math.sqrt(m['uv_area'] / m['geo']))
    numbers = dict(kind=kind, shape=fingerprint(nodes), meshes=len(parts), verts=total['verts'], tris=total['tris'],
                   open=total['open'],
                   nonmanifold=total['nonmanifold'], flat=total['flat'], uv_flat=total['uv_flat'],
                   density_spread=round(max(densities) / min(densities), 2) if densities else 0)

    if kind == 'vehicle':
        roots = [n for n in nodes if n in ROOT_PARTS]
        if len(roots) != 1:
            found.append(('root', f'needs exactly one node named {" or ".join(ROOT_PARTS)}, has {len(roots)}: '
                                  'TankModel.Load starts from it'))
        else:
            loose = sorted(n.name for n in nodes.values()
                           if (n.mesh is not None or n.name.startswith('Socket_')) and not _under(nodes, n.name, roots[0]))
            if loose:
                found.append(('root', f'not under the {roots[0]}, so never drawn or never found: {", ".join(loose)}'))
    if kind in ('vehicle', 'bench'):
        if ref is not None:
            # the file's own top node is named after the file (Salvo_LOD1): one and the same node at every LOD
            top = {n.name for n in nodes.values() if n.parent is None}
            ref_top = {n.name for n in ref.values() if n.parent is None}
            extra = sorted(n for n in nodes if n not in ref and n not in top)
            if extra:
                found.append(('lod-names', f'not at LOD0: {", ".join(extra)}'))
            size = max((diagonal(n.mesh.v) for n in ref.values() if n.mesh is not None), default=1.0) or 1.0
            moved, spikes, holes, worst = [], [], [], (0.0, 0.0)
            for name, node in nodes.items():
                if name not in ref or name in top:
                    continue
                r = ref[name]
                if ((node.parent in top) != (r.parent in ref_top)
                        or (node.parent not in top and node.parent != r.parent)):
                    moved.append(f'{name} (parent {node.parent}, {r.parent} at LOD0)')
                elif math.dist(node.t, r.t) > 1e-4 * size or max(abs(a - b) for a, b in zip(node.r, r.r)) > 0.01:
                    moved.append(f'{name} ({math.dist(node.t, r.t):.3f} off)')
                if node.mesh is not None and r.mesh is not None and parts[name]['tris'] and \
                        sum(len(p) - 2 for p in r.mesh.polys) >= SMALL_PART:
                    spike, hole = shape_drift(node.mesh, r.mesh)
                    worst = (max(worst[0], spike), max(worst[1], hole))
                    if spike > SPIKE:
                        spikes.append(f'{name} {spike:.2f}')
                    if hole > HOLE:
                        holes.append(f'{name} {hole:.2f}')
            if moved:
                found.append(('lod-pivot', f'moved against LOD0: {", ".join(moved)}'))
            if spikes:
                found.append(('lod-shape', f'vertices off LOD0\'s surface by more than {SPIKE} of the part '
                                           f'(a torn decimation or another sculpt looks like this): {", ".join(spikes)}'))
            if holes:
                found.append(('lod-shape', f'LOD0 surface the part no longer covers, by more than {HOLE} of the '
                                           f'part: {", ".join(holes)}'))
            numbers['spike'], numbers['hole'] = round(worst[0], 3), round(worst[1], 3)

    lod = lod_of(rel)[1]
    if kind == 'vehicle' and lod == 0 and total['tris'] > BUDGET_VEHICLE_TRIS:
        found.append(('budget', f'{total["tris"]} triangles, the vehicle budget is {BUDGET_VEHICLE_TRIS}'))
    if kind == 'figure':
        fat = [f'{n} {m["verts"]}' for n, m in parts.items() if m['verts'] > BUDGET_FIGURE_VERTS]
        if fat:
            found.append(('budget', f'over {BUDGET_FIGURE_VERTS} vertices: {", ".join(fat)}'))
    return numbers, found


# ------------------------------------------------------------------------------------------------ the tree as a whole

def model_files(project):
    base = project / ASSETS
    out = []
    for p in sorted(base.rglob('*')):
        if p.suffix.lower() != '.fbx' or not p.is_file():
            continue
        rel = p.relative_to(project).as_posix()
        if not rel.startswith(SKIP):
            out.append(rel)
    return out


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()[:16]


def reference_of(rel, files):
    """The LOD0 file a lower LOD is judged against, or None."""
    key, lod = lod_of(rel)
    if key is None or lod == 0:
        return None
    ref = f'{key}_LOD0.fbx'
    return ref if ref in files else None


def measure_tree(project, only=None):
    """{rel: line} for every model file (or just `only`), each line the numbers, the rules broken and the hash."""
    files = model_files(project)
    cache, out = {}, {}

    def nodes_of(rel):
        if rel not in cache:
            cache[rel] = load(project / rel)
        return cache[rel]

    for rel in files:
        if only is not None and rel not in only:
            continue
        kind = kind_of(rel)
        ref = reference_of(rel, files) if kind in ('vehicle', 'bench') else None
        try:
            numbers, found = judge(rel, nodes_of(rel), kind, nodes_of(ref) if ref else None)
        except Exception as e:  # an unreadable file is a finding, not a crash of the whole check
            numbers, found = dict(kind=kind), [('unreadable', f'{type(e).__name__}: {e}')]
        line = dict(sha=sha(project / rel))
        if ref:
            line['ref'] = sha(project / ref)
        line.update(numbers)
        line['broken'] = [list(f) for f in found]
        out[rel] = line
    return out


def read_json(path, default):
    try:
        return json.loads(Path(path).read_bytes().decode('utf-8'))
    except FileNotFoundError:
        return default


def write_baseline(project, lines):
    body = ',\n'.join(f'  {json.dumps(rel)}: {json.dumps(lines[rel], sort_keys=True)}' for rel in sorted(lines))
    text = '{\n "ruleset": %d,\n "files": {\n%s\n }\n}\n' % (RULESET, body)
    (project / BASELINE).write_bytes(text.encode('utf-8'))


def waived(project, lines):
    """(errors, waived count): every broken rule without a waiver, and every waiver that waives nothing."""
    waivers = read_json(project / WAIVERS, [])
    errors, used = [], set()
    for rel in sorted(lines):
        for rule, message in lines[rel].get('broken', []):
            hit = next((i for i, w in enumerate(waivers) if w.get('file') == rel and w.get('rule') == rule), None)
            if hit is None:
                errors.append(f'{rel}: [{rule}] {message}')
            else:
                used.add(hit)
                if not str(waivers[hit].get('why', '')).strip():
                    errors.append(f'{WAIVERS}: the waiver for {rel} [{rule}] gives no reason (why)')
    for i, w in enumerate(waivers):
        if i not in used:
            errors.append(f'{WAIVERS}: the waiver for {w.get("file")} [{w.get("rule")}] matches nothing now: remove it')
    return errors, len(used)


def check(project):
    """Error lines for the tree as it stands. Fast: a file whose bytes match its recorded line is not measured again."""
    project = Path(project)
    base = read_json(project / BASELINE, None)
    if base is None:
        return [f'{BASELINE} is missing: python Tools/assetgate.py --record']
    if base.get('ruleset') != RULESET:
        return [f'{BASELINE} was recorded with ruleset {base.get("ruleset")}, the tool is at {RULESET}: '
                'python Tools/assetgate.py --record']
    recorded, files = base.get('files', {}), model_files(project)
    hashes = {rel: sha(project / rel) for rel in files}
    stale = set()
    for rel in files:
        line = recorded.get(rel)
        ref = reference_of(rel, files) if kind_of(rel) in ('vehicle', 'bench') else None
        if line is None or line.get('sha') != hashes[rel] or (ref and line.get('ref') != hashes[ref]):
            stale.add(rel)
    gone = sorted(set(recorded) - set(files))
    lines = {rel: recorded[rel] for rel in files if rel not in stale}
    if stale:
        lines.update(measure_tree(project, only=stale))
    errors, _ = waived(project, lines)
    import splitlog
    errors += splitlog.verify(project)
    if stale or gone:
        names = sorted(stale) + [g + ' (deleted)' for g in gone]
        errors.append(f'{BASELINE} is out of date for {len(names)} file(s) ({", ".join(names[:4])}'
                      f'{", ..." if len(names) > 4 else ""}): python Tools/assetgate.py --record, and commit it with the art')
    return errors


WORSE = ('tris', 'verts', 'open', 'nonmanifold', 'flat', 'uv_flat', 'density_spread', 'spike', 'hole')


def record(project):
    project = Path(project)
    old = (read_json(project / BASELINE, None) or {}).get('files', {})
    lines = measure_tree(project)
    write_baseline(project, lines)
    for rel in sorted(lines):
        before, now = old.get(rel), lines[rel]
        if before is None:
            print(f'new    {rel}  {now.get("tris", "?")} tris')
        elif before.get('sha') != now['sha'] or before.get('ref') != now.get('ref'):
            moved = [f'{k} {before.get(k)} -> {now.get(k)}{" WORSE" if (now.get(k) or 0) > (before.get(k) or 0) else ""}'
                     for k in WORSE if before.get(k) != now.get(k)]
            same = before.get('shape') is not None and before.get('shape') == now.get('shape')
            print(f'moved  {rel}  {"the same model, new bytes" if same else "; ".join(moved) or "same numbers"}')
    for rel in sorted(set(old) - set(lines)):
        print(f'gone   {rel}')
    errors, n_waived = waived(project, lines)
    import splitlog
    errors += splitlog.verify(project)
    for e in errors:
        print('FAIL  ' + e)
    print(f'assetgate: recorded {len(lines)} files, {n_waived} waived, {len(errors)} failing')
    return 1 if errors else 0


def report(path, kind):
    path = Path(path).resolve()
    key, lod = lod_of(path.name)
    ref_path = path.with_name(f'{key}_LOD0.fbx') if key is not None and lod else None
    nodes = load(path)
    ref = load(ref_path) if ref_path is not None and ref_path.exists() and kind in ('vehicle', 'bench') else None
    numbers, found = judge(path.name, nodes, kind, ref)
    print(f'{path.name}  as {kind}' + (f'  against {ref_path.name}' if ref else ''))
    print('  ' + '  '.join(f'{k} {v}' for k, v in numbers.items() if k != 'kind'))
    for node in nodes.values():
        if node.mesh is not None:
            m = measure_mesh(node.mesh)
            print(f'  {node.name:<24} under {str(node.parent):<16} {m["tris"]:>6} tris  open {m["open"]:>5}  '
                  f'nonmanifold {m["nonmanifold"]:>4}  flat {m["flat"]:>3}')
    for rule, message in found:
        print(f'  BROKEN [{rule}] {message}')
    if not found:
        print('  no hard rule broken')
    return 1 if found else 0


def main(argv=None):
    ap = argparse.ArgumentParser(description='the gate on imported art')
    ap.add_argument('--record', action='store_true')
    ap.add_argument('--file')
    ap.add_argument('--kind', choices=('vehicle', 'bench', 'figure', 'prop'))
    ap.add_argument('--project', default=str(PROJECT))
    a = ap.parse_args(argv)
    project = Path(a.project)
    if a.file:
        p = Path(a.file).resolve()
        try:
            guess = kind_of(p.relative_to(project.resolve()).as_posix())
        except ValueError:
            guess = 'vehicle' if lod_of(p.name)[0] else 'prop'
        return report(p, a.kind or guess)
    if a.record:
        return record(project)
    errors = check(project)
    for e in errors:
        print(e)
    print('assetgate FAILED' if errors else 'assetgate OK')
    return 1 if errors else 0


if __name__ == '__main__':
    sys.exit(main())
