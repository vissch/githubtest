#!/usr/bin/env python3
"""Tests for nightly.py's balance-band section: python Tools/test_nightly.py. Stdlib only.

A hand-written sweep report.json under a temp TW_SWEEPS root, read by band_part() with no editor and no sweep run.
"""
import json, os, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import nightly as N


def write_report(root, name, now_row, scenario='match'):
    run = os.path.join(root, name)
    os.makedirs(run, exist_ok=True)
    data = {
        'meta': {'commit': 'abc12345', 'scenario': scenario, 'sides': {'a': 'Iron', 'b': 'Brass'}},
        'factions': {'band': [0.4, 0.6], 'rows': [now_row] if now_row else []},
    }
    json.dump(data, open(os.path.join(run, 'report.json'), 'w'))
    return run


class BandPartTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = self.tmp.name

    def tearDown(self):
        self.tmp.cleanup()

    def test_out_of_band_is_a_line_and_a_look_item(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, '20261013-0100-abc', row)
        md, look = N.band_part(self.root)
        self.assertTrue(any('Iron won 72' in l for l in md))
        self.assertEqual(len(look), 1)
        self.assertIn('Iron won 72', look[0])

    def test_inside_the_band_is_a_line_and_no_look_item(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, '20261013-0100-abc', row)
        md, look = N.band_part(self.root)
        self.assertTrue(any('Iron won 50' in l for l in md))
        self.assertEqual(look, [])

    def test_empty_root_is_one_line_and_no_crash(self):
        md, look = N.band_part(self.root)
        self.assertEqual(look, [])
        self.assertTrue(any('No sweep' in l for l in md))

    def test_no_match_ended_is_reported_and_unjudged(self):
        row = {'name': 'now', 'judged': False, 'played': 4, 'ended': 0, 'unended': 4,
               'share_a': None, 'share_b': None, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, '20261013-0100-abc', row)
        md, look = N.band_part(self.root)
        self.assertTrue(any('No match ended' in l for l in md))
        self.assertEqual(look, [])

    def test_a_report_with_no_factions_rows_is_skipped(self):
        write_report(self.root, '20261013-0100-old', None)
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, '20261013-0200-new', row)
        md, look = N.band_part(self.root)
        self.assertTrue(any('Iron won 50' in l for l in md))


if __name__ == '__main__':
    unittest.main()
