#!/usr/bin/env python3
"""[found-critiques-py-write-critique-json-whole-or-no-20261016-2] critique.json is written whole or not at all.
Run from trench-warfare-3d/: python Tools/assetboard/test_critiquewrite.py

A write cut half way used to leave a critique.json that read_all drops without a word, so a whole kept critic loop
vanishes from the listing and from ideas.py context.
"""
import json
import os
import pathlib
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folder: TW_CRITIQUES points at a temp folder before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-critiquewrite-'))
os.environ['TW_CRITIQUES'] = str(TEMP)

import sys
sys.path.insert(0, str(HERE))
import critiques   # noqa: E402  (after TW_CRITIQUES, for the same reason)


class Critique(unittest.TestCase):

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='critiques-', dir=TEMP))

    def test_a_write_cut_half_way_keeps_the_kept_loop(self):
        """[found-critiques-py-write-critique-json-whole-or-no-20261016-2] a write cut half way must not lose
        an already-kept loop: critique.json changes only by a whole rename, never by writing over itself."""
        kept = critiques.save(self.where, [('VERDICT: 80/100\n', 'r1')], 'Kept loop', 'The kept subject', by='test')
        kept_bytes = (self.where / kept['id'] / 'critique.json').read_bytes()

        other = critiques.save(self.where, [('VERDICT: 50/100\n', 'r1')], 'Other loop', 'The other subject', by='test')
        f = other['id'] and (self.where / other['id'] / 'critique.json')
        f.write_bytes(f.read_bytes()[:len(f.read_bytes()) // 2])
        self.assertEqual([c['id'] for c in critiques.read_all(self.where)], [kept['id']],
                          '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                          'a critique.json cut in half must be dropped by read_all, not raise')

        real_write_text = pathlib.Path.write_text

        def flaky(self_path, text, *a, **kw):
            if self_path.name.startswith('critique.json'):
                real_write_text(self_path, text[:len(text) // 2], *a, **kw)
                raise OSError('the disk is full')
            return real_write_text(self_path, text, *a, **kw)

        pathlib.Path.write_text = flaky
        try:
            with self.assertRaises(OSError):
                critiques.save(self.where, [('VERDICT: 90/100\n', 'r2')], 'Kept loop', 'The kept subject', by='test',
                                cid=kept['id'])
        finally:
            pathlib.Path.write_text = real_write_text

        self.assertEqual((self.where / kept['id'] / 'critique.json').read_bytes(), kept_bytes,
                          '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                          'a write cut half way must not touch the already-kept critique.json')
        self.assertIn(kept['id'], [c['id'] for c in critiques.read_all(self.where)],
                       '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                       'a whole kept critic loop must still be found after a write cut half way')

    def test_critique_json_is_written_through_a_temp_file(self):
        """[found-critiques-py-write-critique-json-whole-or-no-20261016-2] critique.json changes by a rename of a
        temp file beside it (the pattern briefs.py put and notes.py write_whole already use), not a write over itself."""
        renames = []
        real_replace = pathlib.Path.replace

        def recording(self_path, target):
            renames.append((self_path.name, Path(target).name))
            return real_replace(self_path, target)

        pathlib.Path.replace = recording
        try:
            c = critiques.save(self.where, [('VERDICT: 70/100\n', 'r1')], 'Renamed loop', 'The subject', by='test')
        finally:
            pathlib.Path.replace = real_replace

        hits = [(n, t) for n, t in renames if t == 'critique.json']
        self.assertTrue(hits and all(n != 'critique.json' and n.startswith('critique.json') for n, _ in hits),
                         '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                         'critique.json must be written by a rename from a temp file beside it, not in place')

        raw = (self.where / c['id'] / 'critique.json').read_bytes()
        self.assertNotIn(b'\r', raw,
                          '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                          'the temp-file write must keep the exact bytes (newline="\\n"), not turn them into CRLF')
        self.assertTrue(raw.endswith(b'\n'),
                         '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                         'critique.json must still end with a newline')
        self.assertEqual(json.loads(raw.decode('utf-8'))['id'], c['id'],
                          '[found-critiques-py-write-critique-json-whole-or-no-20261016-2] '
                          'critique.json on disk must read back as the saved loop')


if __name__ == '__main__':
    unittest.main()
