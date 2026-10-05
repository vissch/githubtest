#!/usr/bin/env python3
"""Tests for splitlog.py: python Tools/test_splitlog.py. Stdlib only, no Blender.

A throwaway project stands in for a split's run: source files written before the "run", outputs after it. The models
are two shipped FBXs (the record hashes what a model says, so it needs real ones)."""
import json
import os
import re
import shutil
import sys
import tempfile
import time
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import splitlog as S
from test_assetgate import clean_vehicle


class Records(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        t = Path(self.tmp.name)
        self.proj, self.src, self.scratch = t / 'proj', t / 'src', t / 'scratch'
        (self.proj / 'Tools').mkdir(parents=True)
        self.script = self.proj / 'Tools' / 'mechsplit.py'
        self.script.write_text('# stand-in\n')
        self.out = self.proj / 'Assets/_Project/Resources/Vehicles/Test'
        self.render = self.scratch / 'render'
        for d in (self.out, self.render, self.src / 'm.fbm'):
            d.mkdir(parents=True)
        self.lod0, self.lod1 = clean_vehicle()
        (self.src / 'm.fbx').write_bytes(b'the source model')
        (self.src / 'm.fbm' / 'base.jpg').write_bytes(b'its texture')
        (self.out / 'Old.fbx').write_bytes(b'left by an earlier run')
        old = time.time() - 3600
        for p in (self.src / 'm.fbx', self.out / 'Old.fbx'):
            os.utime(p, (old, old))
        self.since = time.time()

    def tearDown(self):
        self.tmp.cleanup()

    def run_split(self, out=None):
        out = out or self.out
        shutil.copyfile(self.lod0, out / 'Test_LOD0.fbx')
        shutil.copyfile(self.lod1, out / 'Test_LOD1.fbx')
        (out.parent / 'TestAtlas.jpg').write_bytes(b'atlas')              # mechsplit.py writes it one level up
        (self.render / 'Test_parts.png').write_bytes(b'render')
        return ['Test', str(self.src / 'm.fbx'), str(out), str(self.render)]

    def write(self, argv):
        record = S.build(self.script, argv, self.proj, since=self.since, env={'TW_KIND': 'hover', 'TW_SCALE': '7.0',
                                                                             'PATH': 'x', 'TW_IN_TOOLCHECK': '1'})
        path = S.where(self.script, argv, self.proj, since=self.since)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(record))
        return record, path

    def test_a_run_into_the_project_is_recorded_outside_assets(self):
        record, path = self.write(self.run_split())
        self.assertEqual(path, self.proj.resolve() / 'Tools/splits/mechsplit-Resources_Vehicles_Test.json')
        self.assertEqual(record['script'], 'Tools/mechsplit.py')
        self.assertEqual(record['env'], {'TW_KIND': 'hover', 'TW_SCALE': '7.0'})
        self.assertEqual([(Path(i['path']).name, i['bytes'], 'textures' in i) for i in record['inputs']], [('m.fbx', 16, True)])
        outs = {o['path']: o for o in record['outputs']}
        P = 'Assets/_Project/Resources/Vehicles/'
        self.assertEqual(sorted(k for k in outs if not Path(k).is_absolute()),
                         [P + 'Test/Test_LOD0.fbx', P + 'Test/Test_LOD1.fbx', P + 'TestAtlas.jpg'])
        self.assertIn('shape', outs[P + 'Test/Test_LOD0.fbx'])
        self.assertNotIn('shape', outs[P + 'TestAtlas.jpg'])
        self.assertTrue(any(k.endswith('render/Test_parts.png') for k in outs))
        self.assertFalse(any('Old.fbx' in k for k in outs), 'a file from an earlier run is not this run\'s output')
        self.assertIn('TW_KIND=hover TW_SCALE=7.0', record['command'])
        self.assertIn('Tools/mechsplit.py -- Test', record['command'])

    def test_a_run_elsewhere_leaves_its_record_in_its_out_folder(self):
        out = self.scratch / 'Vehicles/Test'
        out.mkdir(parents=True)
        _, path = self.write(self.run_split(out))
        self.assertEqual(path, out.resolve() / 'split.json')

    def test_an_out_file_argument_counts_as_an_output_not_a_source(self):
        fbx = self.out / 'Frog.fbx'
        shutil.copyfile(self.lod0, fbx)
        record, path = self.write([str(self.src / 'm.fbx'), str(fbx), str(self.render)])
        self.assertEqual([Path(i['path']).name for i in record['inputs']], ['m.fbx'])
        self.assertIn('Assets/_Project/Resources/Vehicles/Test/Frog.fbx', [o['path'] for o in record['outputs']])
        self.assertEqual(path.name, 'mechsplit-Resources_Vehicles_Test.json')

    def test_the_check_holds_a_model_to_its_record(self):
        self.write(self.run_split())
        self.assertEqual(S.verify(self.proj), [])
        (self.out.parent / 'TestAtlas.jpg').write_bytes(b'graded since')          # textures are not held to their hash
        self.assertEqual(S.verify(self.proj), [])
        shutil.copyfile(self.lod0, self.out / 'Test_LOD1.fbx')                    # another model under the name
        errors = S.verify(self.proj)
        self.assertEqual(len(errors), 1, errors)
        self.assertIn('Test_LOD1.fbx is no longer what this run wrote', errors[0])

    def test_new_bytes_of_the_same_model_pass(self):
        self.write(self.run_split())
        p = self.out / 'Test_LOD1.fbx'
        b = bytearray(p.read_bytes())
        at = b.index(b'CreationTime')                       # the stamp Blender writes: a second run differs here
        stamp = b.index(b'20', at)
        b[stamp + 2:stamp + 4] = b'99' if b[stamp + 2:stamp + 4] != b'99' else b'98'
        p.write_bytes(bytes(b))
        self.assertNotEqual(S.sha(p), S.sha(self.lod1))
        self.assertEqual(S.verify(self.proj), [])

    def test_a_deliberate_edit_is_adopted_with_a_reason(self):
        self.write(self.run_split())
        shutil.copyfile(self.lod0, self.out / 'Test_LOD1.fbx')
        rel = 'Assets/_Project/Resources/Vehicles/Test/Test_LOD1.fbx'
        self.assertIn('needs --why', S.adopt(rel, ' ', self.proj)[0])
        self.assertIn('no record', S.adopt('Assets/_Project/Nope.fbx', 'x', self.proj)[0])
        self.assertEqual(S.adopt(rel, 'the Gun re-parented in the file', self.proj), [])
        self.assertEqual(S.verify(self.proj), [])

    def test_a_recorded_model_that_is_gone(self):
        self.write(self.run_split())
        (self.out / 'Test_LOD1.fbx').unlink()
        self.assertIn('which is gone', S.verify(self.proj)[0])

    def test_a_witness_is_taken_only_when_every_model_matches(self):
        scratch_out = self.scratch / 'Vehicles/Test'
        scratch_out.mkdir(parents=True)
        _, run = self.write(self.run_split(scratch_out))
        shutil.copyfile(self.lod0, self.out / 'Test_LOD0.fbx')
        errors = S.witness(run, self.out, self.proj)
        self.assertEqual(len(errors), 1)
        self.assertIn('Test_LOD1.fbx, which', errors[0])
        self.assertEqual(S.records(self.proj), [])
        shutil.copyfile(self.lod0, self.out / 'Test_LOD1.fbx')                    # the wrong model under the name
        self.assertIn('is not the model the run wrote', S.witness(run, self.out, self.proj)[0])
        self.assertEqual(S.records(self.proj), [])
        shutil.copyfile(self.lod1, self.out / 'Test_LOD1.fbx')
        self.assertEqual(S.witness(run, self.out, self.proj), [])
        record = json.loads(S.records(self.proj)[0].read_text())
        self.assertEqual(sorted(o['path'] for o in record['outputs']),
                         ['Assets/_Project/Resources/Vehicles/Test/Test_LOD0.fbx', 'Assets/_Project/Resources/Vehicles/Test/Test_LOD1.fbx'])
        self.assertIn('witness', record)
        self.assertEqual(S.verify(self.proj), [])
        self.assertIn('not under this project', S.witness(run, self.scratch, self.proj)[0])

    def test_write_never_fails_the_split(self):
        self.assertIsNone(S.write(self.proj / 'Tools' / 'missing.py', ['x'], self.proj))


class TheScripts(unittest.TestCase):
    def test_every_blender_split_records_its_run_on_every_way_out(self):
        tools = Path(__file__).resolve().parent
        # a split is a Blender script (it imports bpy at the top) that writes model FBXs
        splits = sorted(p for p in tools.glob('*.py')
                        if re.search(r'^import bpy', p.read_text(encoding='utf-8'), re.M)
                        and 'bpy.ops.export_scene.fbx(' in p.read_text(encoding='utf-8'))
        self.assertGreaterEqual(len(splits), 8, [p.name for p in splits])
        for p in splits:
            lines = p.read_text(encoding='utf-8').rstrip().split('\n')
            self.assertIn('import splitlog', '\n'.join(lines), p.name)
            self.assertEqual(lines[-1].strip(), 'splitlog.write(__file__, argv)', f'{p.name}: the record is its last line')
            for k, line in enumerate(lines):
                if line.strip().startswith(('sys.exit(', 'quit(', 'exit(')):
                    self.assertEqual(lines[k - 1].strip(), 'splitlog.write(__file__, argv)',
                                     f'{p.name}:{k + 1} leaves without writing the record')


if __name__ == '__main__':
    unittest.main(verbosity=1)
