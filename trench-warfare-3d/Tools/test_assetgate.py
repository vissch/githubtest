#!/usr/bin/env python3
"""Tests for assetgate.py: python Tools/test_assetgate.py. Stdlib only, no Unity, no Blender.

Each hard rule is shown to pass on a sound model and to fail on the one fault it is for, on models built here (so
the tests do not move when the art does). The surface-distance search is held to a brute-force answer. The tree-level
check runs on a throwaway project made of two shipped files. Last, the committed Tools/assetgate.json is measured
again from scratch: a hand-edited line, or a rule changed without bumping RULESET, fails here.
"""
import json
import math
import random
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import assetgate as A

PROJECT = Path(__file__).resolve().parent.parent


def grid_box(size=(1.0, 1.0, 1.0), n=3, at=(0.0, 0.0, 0.0)):
    """A closed box, each face n x n quads, vertices shared across faces. UVs: one 0..1 square per face."""
    index, v, polys, uv, uvpolys = {}, [], [], [], []

    def vert(p):
        key = tuple(round(c, 6) for c in p)
        if key not in index:
            index[key] = len(v) // 3
            v.extend(p)
        return index[key]

    def face(origin, du, dv):
        for i in range(n):
            for j in range(n):
                quad, uvq = [], []
                for a, b in ((i, j), (i + 1, j), (i + 1, j + 1), (i, j + 1)):
                    quad.append(vert([origin[k] + du[k] * a / n + dv[k] * b / n for k in range(3)]))
                    uvq.append(len(uv) // 2)
                    uv.extend((a / n, b / n))
                polys.append(quad)
                uvpolys.append(uvq)

    x, y, z = size
    o = at
    face((o[0], o[1], o[2]), (0, y, 0), (x, 0, 0))            # bottom
    face((o[0], o[1], o[2] + z), (x, 0, 0), (0, y, 0))        # top
    face((o[0], o[1], o[2]), (x, 0, 0), (0, 0, z))            # front
    face((o[0], o[1] + y, o[2]), (0, 0, z), (x, 0, 0))        # back
    face((o[0], o[1], o[2]), (0, 0, z), (0, y, 0))            # left
    face((o[0] + x, o[1], o[2]), (0, y, 0), (0, 0, z))        # right
    m = A.Mesh()
    m.v, m.polys, m.uv, m.uvpolys = v, polys, uv, uvpolys
    return m


def node(name, parent, mesh=None, t=(0.0, 0.0, 0.0)):
    n = A.Node()
    n.name, n.kind, n.parent, n.mesh = name, 'Mesh' if mesh else 'Null', parent, mesh
    n.t, n.r, n.s = list(t), [0.0, 0.0, 0.0], [1.0, 1.0, 1.0]
    return n


def vehicle(lod=0, n=3):
    """Top > Hull > Turret > Gun, a muzzle socket on the gun: the shape Tools/mechsplit.py TW_BATTLE=1 writes."""
    top = f'Test_LOD{lod}'
    nodes = [node(top, None), node('Hull', top, grid_box((4, 2, 1), n)),
             node('Turret', 'Hull', grid_box((1, 1, 1), n), t=(0, 0, 1)),
             node('Gun', 'Turret', grid_box((0.2, 2, 0.2), n), t=(0, 1, 0.5)),
             node('Socket_Muzzle', 'Gun', t=(0, 2, 0))]
    return {x.name: x for x in nodes}


def rules(found):
    return sorted({r for r, _ in found})


class MeshNumbers(unittest.TestCase):
    def test_a_closed_box_has_no_open_edges(self):
        m = A.measure_mesh(grid_box())
        self.assertEqual((m['open'], m['nonmanifold'], m['flat'], m['uv_out'], m['tris']), (0, 0, 0, 0, 108))

    def test_a_missing_face_opens_its_four_edges(self):
        box = grid_box(n=1)
        box.polys.pop()
        box.uvpolys.pop()
        self.assertEqual(A.measure_mesh(box)['open'], 4)

    def test_a_third_face_on_an_edge_is_non_manifold(self):
        box = grid_box(n=1)
        a, b = box.polys[0][0], box.polys[0][1]
        box.v.extend((5.0, 5.0, 5.0))
        box.polys.append([a, b, len(box.v) // 3 - 1])
        box.uvpolys.append([0, 1, 2])
        self.assertEqual(A.measure_mesh(box)['nonmanifold'], 1)

    def test_a_zero_area_triangle_is_counted(self):
        box = grid_box(n=1)
        a = box.polys[0][0]
        box.polys.append([a, a, box.polys[0][1]])
        box.uvpolys.append([0, 0, 1])
        self.assertEqual(A.measure_mesh(box)['flat'], 1)

    def test_a_uv_outside_the_square_is_counted_and_the_slack_is_not(self):
        box = grid_box(n=1)
        box.uv[0] = 1.0 + A.UV_SLACK / 2
        self.assertEqual(A.measure_mesh(box)['uv_out'], 0)
        box.uv[0] = 1.5
        self.assertEqual(A.measure_mesh(box)['uv_out'], 1)


class SurfaceDistance(unittest.TestCase):
    def test_known_distances_to_one_triangle(self):
        a, b, c = (0, 0, 0), (1, 0, 0), (0, 1, 0)
        for p, want in (((0.2, 0.2, 3.0), 3.0),            # over the inside
                        ((0.5, -2.0, 0.0), 2.0),           # off an edge
                        ((-3.0, -4.0, 0.0), 5.0),          # off a corner
                        ((2.0, 0.0, 0.0), 1.0),
                        ((0.25, 0.25, 0.0), 0.0)):
            self.assertAlmostEqual(math.sqrt(A._closest_sq(p, a, b, c)), want, places=9, msg=str(p))

    def test_the_grid_search_agrees_with_brute_force(self):
        rng = random.Random(1917)
        box = grid_box((3, 1, 0.4), n=4)
        skin = A.Skin(box)
        for _ in range(300):
            p = tuple(rng.uniform(-4, 6) for _ in range(3))
            brute = math.sqrt(min(A._closest_sq(p, *t) for t in skin.tris))
            self.assertAlmostEqual(skin.distance(p), brute, places=9, msg=str(p))

    def test_the_grid_search_looks_past_the_first_ring_that_holds_a_triangle(self):
        # scattered scraps: the nearest is often a ring further out than the first one found (a cell's far corner
        # is further than the next cell along an axis), which a search that stops at its first hit gets wrong
        rng = random.Random(7)
        scraps = A.Mesh()
        scraps.v, scraps.polys, scraps.uv, scraps.uvpolys = [], [], None, None
        for k in range(150):
            c = [rng.uniform(0, 10) for _ in range(3)]
            for d in ((0, 0, 0), (0.1, 0, 0), (0, 0.1, 0)):
                scraps.v.extend(c[i] + d[i] for i in range(3))
            scraps.polys.append([3 * k, 3 * k + 1, 3 * k + 2])
        skin = A.Skin(scraps)
        for _ in range(400):
            p = tuple(rng.uniform(-1, 11) for _ in range(3))
            brute = math.sqrt(min(A._closest_sq(p, *t) for t in skin.tris))
            self.assertAlmostEqual(skin.distance(p), brute, places=9, msg=str(p))

    def test_drift_is_nothing_for_the_same_part_and_coarser_is_small(self):
        self.assertEqual(A.shape_drift(grid_box(n=3), grid_box(n=3)), (0.0, 0.0))
        spike, hole = A.shape_drift(grid_box(n=1), grid_box(n=4))
        self.assertLess(spike, 1e-9)
        self.assertLess(hole, 1e-9)


class HardRules(unittest.TestCase):
    def judge(self, nodes, kind='vehicle', ref=None, lod=None):
        lod = lod if lod is not None else (0 if ref is None else 1)
        return A.judge(f'Assets/_Project/Resources/Vehicles/Test/Test_LOD{lod}.fbx', nodes, kind, ref)

    def test_a_sound_vehicle_breaks_nothing_at_either_lod(self):
        numbers, found = self.judge(vehicle(0))
        self.assertEqual(found, [])
        self.assertEqual(numbers['meshes'], 3)
        numbers, found = self.judge(vehicle(1, n=2), ref=vehicle(0))
        self.assertEqual(found, [], found)
        self.assertLess(numbers['spike'], 0.01)

    def test_root_a_part_outside_the_hull(self):
        nodes = vehicle(0)
        nodes['Turret'].parent = 'Test_LOD0'          # the gun and its socket go with it
        _, found = self.judge(nodes)
        self.assertEqual(rules(found), ['root'])
        self.assertIn('Gun, Socket_Muzzle, Turret', found[0][1])

    def test_root_no_hull_at_all_and_two_of_them(self):
        nodes = vehicle(0)
        hull = nodes.pop('Hull')
        hull.name = 'Chassis'
        nodes['Chassis'] = hull
        nodes['Turret'].parent = 'Chassis'
        self.assertEqual(rules(self.judge(nodes)[1]), ['root'])
        nodes = vehicle(0)
        nodes['Body'] = node('Body', 'Test_LOD0', grid_box())
        self.assertEqual(rules(self.judge(nodes)[1]), ['root'])

    def test_root_is_not_asked_of_the_bench_whose_files_are_flat(self):
        nodes = vehicle(0)
        for n in nodes.values():
            if n.parent is not None:
                n.parent = 'Test_LOD0'
        self.assertEqual(self.judge(nodes, kind='bench')[1], [])

    def test_lod_names_a_node_lod0_lacks(self):
        low = vehicle(1)
        low['Mudguard'] = node('Mudguard', 'Hull', grid_box())
        self.assertEqual(rules(self.judge(low, ref=vehicle(0))[1]), ['lod-names'])

    def test_lod_pivot_a_part_that_moves_turns_or_changes_parent(self):
        for change in ('move', 'turn', 'parent'):
            low = vehicle(1)
            if change == 'move':
                low['Turret'].t[0] += 0.05
            elif change == 'turn':
                low['Turret'].r[2] = 5.0
            else:
                low['Gun'].parent = 'Hull'
            _, found = self.judge(low, ref=vehicle(0))
            self.assertEqual(rules(found), ['lod-pivot'], change)
        self.assertEqual(self.judge(vehicle(1), ref=vehicle(0))[1], [])

    def test_lod_shape_a_vertex_pulled_out_of_the_part(self):
        low = vehicle(1)
        box = low['Hull'].mesh
        box.v[2] -= 1.5                                # one corner of a 4 x 2 x 1 hull, 1.5 down
        _, found = self.judge(low, ref=vehicle(0))
        self.assertEqual(rules(found), ['lod-shape'])
        self.assertIn('Hull', found[0][1])
        low = vehicle(1)
        low['Hull'].mesh.v[2] -= 0.05                  # 1 % of the part: under the mark
        self.assertEqual(self.judge(low, ref=vehicle(0))[1], [])

    def test_lod_shape_a_part_that_lost_half_its_surface(self):
        low = vehicle(1)
        box = low['Hull'].mesh
        keep = [k for k, p in enumerate(box.polys) if max(box.v[3 * i] for i in p) <= 2.0 + 1e-6]
        box.polys = [box.polys[k] for k in keep]
        box.uvpolys = [box.uvpolys[k] for k in keep]
        _, found = self.judge(low, ref=vehicle(0))
        self.assertEqual(rules(found), ['lod-shape'])
        self.assertIn('no longer covers', found[0][1])

    def test_uv_missing_and_uv_bounds(self):
        nodes = vehicle(0)
        nodes['Gun'].mesh.uv = nodes['Gun'].mesh.uvpolys = None
        self.assertEqual(rules(self.judge(nodes)[1]), ['uv-missing'])
        nodes = vehicle(0)
        nodes['Gun'].mesh.uv[1] = -0.3
        self.assertEqual(rules(self.judge(nodes)[1]), ['uv-bounds'])
        self.assertEqual(rules(self.judge(nodes, kind='prop')[1]), ['uv-bounds'])

    def test_budget_counts_a_battle_vehicle_at_lod0_only(self):
        fat = vehicle(0, n=21)                         # 3 parts x 6 faces x 441 quads x 2
        self.assertGreater(self.judge(fat)[0]['tris'], A.BUDGET_VEHICLE_TRIS)
        self.assertEqual(rules(self.judge(fat)[1]), ['budget'])
        self.assertEqual(self.judge(fat, kind='bench')[1], [])
        self.assertEqual(rules(self.judge(fat, ref=vehicle(0, n=21), lod=1)[1]), [])

    def test_budget_counts_a_figures_vertices(self):
        nodes = {'Soldier': node('Soldier', None, grid_box(n=19))}      # 6 x 19 x 19 quads: 2,168 vertices
        self.assertEqual(rules(A.judge('Assets/_Project/Art/Characters/Soldier.fbx', nodes, 'figure')[1]), ['budget'])
        nodes = {'Soldier': node('Soldier', None, grid_box(n=8))}
        self.assertEqual(A.judge('Assets/_Project/Art/Characters/Soldier.fbx', nodes, 'figure')[1], [])


class Paths(unittest.TestCase):
    def test_kinds_and_lods(self):
        P = A.ASSETS
        self.assertEqual(A.kind_of(P + '/Resources/Vehicles/Maw/Maw_LOD0.fbx'), 'vehicle')
        self.assertEqual(A.kind_of(P + '/Playground/Art/Tanks/Mercy/Mercy_LOD2.fbx'), 'bench')
        self.assertEqual(A.kind_of(P + '/Art/Characters/Soldier.fbx'), 'figure')
        self.assertEqual(A.kind_of(P + '/Resources/Env/Houses/Chunks/a.fbx'), 'prop')
        self.assertEqual(A.lod_of('a/Maw_LOD1.fbx'), ('a/Maw', 1))
        self.assertEqual(A.lod_of('a/Crate.fbx'), (None, None))


def clean_vehicle():
    """A shipped vehicle whose two LODs break no rule, to copy into a throwaway project."""
    lines = json.loads((PROJECT / A.BASELINE).read_bytes().decode('utf-8'))['files']
    for rel in sorted(lines):
        key, lod = A.lod_of(rel)
        if lines[rel]['kind'] == 'vehicle' and lod == 1 and not lines[rel]['broken'] \
                and not lines.get(key + '_LOD0.fbx', {'broken': [1]})['broken']:
            return PROJECT / (key + '_LOD0.fbx'), PROJECT / rel
    raise AssertionError('no shipped vehicle is clean at both LODs any more: pick the test files another way')


class TheTree(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.proj = Path(self.tmp.name)
        self.dir = self.proj / A.ASSETS / 'Resources/Vehicles/Test'
        self.dir.mkdir(parents=True)
        (self.proj / 'Tools').mkdir()
        self.lod0, self.lod1 = clean_vehicle()
        shutil.copyfile(self.lod0, self.dir / 'Test_LOD0.fbx')
        shutil.copyfile(self.lod1, self.dir / 'Test_LOD1.fbx')
        clips = self.proj / A.ASSETS / 'Art/Characters/Clips'
        clips.mkdir(parents=True)
        (clips / 'Run.fbx').write_bytes(b'not a model')         # clips are not the gate's
        self.rel0 = A.ASSETS + '/Resources/Vehicles/Test/Test_LOD0.fbx'
        self.rel1 = A.ASSETS + '/Resources/Vehicles/Test/Test_LOD1.fbx'

    def tearDown(self):
        self.tmp.cleanup()

    def record(self):
        lines = A.measure_tree(self.proj)
        A.write_baseline(self.proj, lines)
        return lines

    def test_a_recorded_tree_checks_clean_and_clips_are_left_out(self):
        self.assertEqual(A.check(self.proj), [f'{A.BASELINE} is missing: python Tools/assetgate.py --record'])
        lines = self.record()
        self.assertEqual(sorted(lines), [self.rel0, self.rel1])
        self.assertEqual(A.check(self.proj), [])

    def test_changed_art_needs_a_new_record(self):
        self.record()
        shutil.copyfile(self.lod0, self.dir / 'Test_LOD1.fbx')
        errors = A.check(self.proj)
        self.assertEqual(len(errors), 1, errors)
        self.assertIn('out of date for 1 file(s)', errors[0])
        self.assertIn('Test_LOD1.fbx', errors[0])

    def test_a_new_lod0_makes_its_lower_lods_stale_too(self):
        self.record()
        shutil.copyfile(self.lod1, self.dir / 'Test_LOD0.fbx')
        errors = A.check(self.proj)
        self.assertIn('out of date for 2 file(s)', errors[-1])

    def test_a_new_file_and_a_deleted_one(self):
        self.record()
        shutil.copyfile(self.lod0, self.dir / 'Other_LOD0.fbx')
        self.assertIn('Other_LOD0.fbx', A.check(self.proj)[-1])
        (self.dir / 'Other_LOD0.fbx').unlink()
        (self.dir / 'Test_LOD1.fbx').unlink()
        self.assertIn('Test_LOD1.fbx (deleted)', A.check(self.proj)[-1])

    def test_a_ruleset_bump_asks_for_a_record(self):
        self.record()
        path = self.proj / A.BASELINE
        path.write_bytes(path.read_bytes().replace(b'"ruleset": %d' % A.RULESET, b'"ruleset": 0'))
        self.assertIn('ruleset 0', A.check(self.proj)[0])

    def test_a_broken_rule_fails_until_waived_with_a_reason(self):
        lines = self.record()
        lines[self.rel1]['broken'] = [['lod-shape', 'made up for the test']]
        A.write_baseline(self.proj, lines)
        self.assertEqual(A.check(self.proj), [f'{self.rel1}: [lod-shape] made up for the test'])
        waivers = self.proj / A.WAIVERS
        waivers.write_text(json.dumps([{'file': self.rel1, 'rule': 'lod-shape', 'why': ' '}]))
        self.assertIn('gives no reason', A.check(self.proj)[0])
        waivers.write_text(json.dumps([{'file': self.rel1, 'rule': 'lod-shape', 'why': 'the test says so'}]))
        self.assertEqual(A.check(self.proj), [])

    def test_a_waiver_that_matches_nothing_fails(self):
        self.record()
        (self.proj / A.WAIVERS).write_text(json.dumps([{'file': self.rel1, 'rule': 'budget', 'why': 'left behind'}]))
        errors = A.check(self.proj)
        self.assertEqual(len(errors), 1)
        self.assertIn('matches nothing now', errors[0])

    def test_an_unreadable_model_is_a_finding_not_a_crash(self):
        (self.dir / 'Junk.fbx').write_bytes(b'Kaydara FBX Binary  \x00\x1a\x00' + b'\x00' * 8)
        lines = A.measure_tree(self.proj)
        self.assertEqual([r for r, _ in lines[A.ASSETS + '/Resources/Vehicles/Test/Junk.fbx']['broken']], ['unreadable'])


class TheRecords(TheTree):
    def test_a_model_that_is_not_what_its_split_recorded_fails_the_check(self):
        lines = self.record()
        splits = self.proj / 'Tools/splits'
        splits.mkdir()
        good = dict(path=self.rel1, sha=lines[self.rel1]['sha'], shape=lines[self.rel1]['shape'])
        (splits / 'mechsplit-Resources_Vehicles_Test.json').write_text(json.dumps({'outputs': [good]}))
        self.assertEqual(A.check(self.proj), [])
        (splits / 'mechsplit-Resources_Vehicles_Test.json').write_text(
            json.dumps({'outputs': [dict(good, sha='0' * 16, shape='0' * 16)]}))
        errors = A.check(self.proj)
        self.assertEqual(len(errors), 1, errors)
        self.assertIn('Test_LOD1.fbx is no longer what this run wrote', errors[0])

    def test_the_shape_ignores_the_bytes_blender_stamps(self):
        b = bytearray((self.dir / 'Test_LOD1.fbx').read_bytes())
        at = b.index(b'20', b.index(b'CreationTime'))
        b[at + 2:at + 4] = b'99' if b[at + 2:at + 4] != b'99' else b'98'
        (self.dir / 'Stamped.fbx').write_bytes(bytes(b))
        self.assertNotEqual(A.sha(self.dir / 'Stamped.fbx'), A.sha(self.dir / 'Test_LOD1.fbx'))
        self.assertEqual(A.fingerprint(A.load(self.dir / 'Stamped.fbx')), A.fingerprint(A.load(self.dir / 'Test_LOD1.fbx')))
        self.assertNotEqual(A.fingerprint(A.load(self.dir / 'Test_LOD0.fbx')), A.fingerprint(A.load(self.dir / 'Test_LOD1.fbx')))


class TheCommittedBaseline(unittest.TestCase):
    def test_every_line_is_what_the_tool_measures_today(self):
        committed = json.loads((PROJECT / A.BASELINE).read_bytes().decode('utf-8'))
        self.assertEqual(committed['ruleset'], A.RULESET)
        fresh = json.loads(json.dumps(A.measure_tree(PROJECT)))
        self.assertEqual(sorted(fresh), sorted(committed['files']))
        wrong = [rel for rel in fresh if fresh[rel] != committed['files'][rel]]
        self.assertEqual(wrong, [], 'these lines differ from a fresh measurement: python Tools/assetgate.py --record '
                                    '(and bump RULESET if a rule changed)')
        self.assertGreater(len(fresh), 100)

    def test_the_tree_passes(self):
        self.assertEqual(A.check(PROJECT), [])


if __name__ == '__main__':
    unittest.main(verbosity=1)
