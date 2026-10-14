#!/usr/bin/env python3
"""Tests for nightly.py's balance-band section: python Tools/test_nightly.py. Stdlib only.

A hand-written sweep report.json under a temp TW_SWEEPS root, read by band_part() with no editor and no sweep run.
"""
import datetime, json, os, sys, tempfile, unittest
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import nightly as N


def write_report(root, name, now_row, scenario='match', spec='factions.json', dirty=False, not_run=None, sha='abc12345'):
    run = os.path.join(root, name)
    os.makedirs(run, exist_ok=True)
    data = {
        'meta': {'commit': sha, 'spec': spec, 'dirty': dirty, 'scenario': scenario, 'sides': {'a': 'Iron', 'b': 'Brass'}},
        'not_run': not_run or [],
        'factions': {'band': [0.4, 0.6], 'rows': [now_row] if now_row else []},
    }
    json.dump(data, open(os.path.join(run, 'report.json'), 'w'))
    return run


def stamp(days_ago=0, sha='abc'):
    when = datetime.datetime.now() - datetime.timedelta(days=days_ago)
    return when.strftime('%Y%m%d-%H%M') + '-' + sha


class BandPartTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = self.tmp.name

    def tearDown(self):
        self.tmp.cleanup()

    def test_out_of_band_is_a_line_and_a_look_item(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, stamp(), row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('Iron won 72' in l for l in md))
        self.assertEqual(len(look), 1)
        self.assertIn('Iron won 72', look[0])

    def test_inside_the_band_is_a_line_and_no_look_item(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, stamp(), row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('Iron won 50' in l for l in md))
        self.assertEqual(look, [])

    def test_empty_root_is_one_line_and_no_crash(self):
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertEqual(look, [])
        self.assertTrue(any('No sweep' in l for l in md))

    def test_no_match_ended_is_reported_and_unjudged(self):
        row = {'name': 'now', 'judged': False, 'played': 4, 'ended': 0, 'unended': 4,
               'share_a': None, 'share_b': None, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, stamp(), row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('No match ended' in l for l in md))
        self.assertEqual(look, [])

    def test_B4_unknown_seatings_is_reported_and_unjudged(self):
        # [B4] a row whose swapSeats (and so its match count) is unknown must never be judged as "no match ended".
        row = {'name': 'now', 'judged': False, 'seatings_unknown': True, 'played': None, 'ended': None,
               'unended': None, 'share_a': None, 'share_b': None, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, stamp(), row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('swapSeats' in l and 'unknown' in l for l in md))
        self.assertEqual(look, [])
        self.assertFalse(info['judged'])

    def test_a_report_with_no_factions_rows_is_skipped(self):
        write_report(self.root, stamp(days_ago=1, sha='old'), None, sha='abc12345')
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, stamp(sha='new'), row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('Iron won 50' in l for l in md))

    def test_B1_a_half_written_report_does_not_raise_and_the_older_good_one_is_read(self):
        # [B1] a mid-write report.json must not throw; the older readable sweep is used instead.
        run = os.path.join(self.root, stamp(days_ago=1, sha='old'))
        os.makedirs(run, exist_ok=True)
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(self.root, os.path.basename(run), row, sha='abc12345')
        broken = os.path.join(self.root, stamp(sha='broken'))
        os.makedirs(broken, exist_ok=True)
        open(os.path.join(broken, 'report.json'), 'w').write('{"meta": {')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(any('could not be read' in l for l in md))
        self.assertTrue(any('Iron won 50' in l for l in md))
        self.assertEqual(info['sweep'], os.path.basename(run))

    def test_B2_wrong_commit_is_named_and_not_judged(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        name = stamp(days_ago=2)
        write_report(self.root, name, row, sha='deadbeef')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertEqual(look, [])
        self.assertFalse(info['judged'])
        self.assertTrue(any(name in l for l in md))
        self.assertTrue(any('2' in l for l in md))

    def test_B2_dirty_tree_is_named_and_not_judged(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        name = stamp()
        write_report(self.root, name, row, sha='abc12345', dirty=True)
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertEqual(look, [])
        self.assertFalse(info['judged'])
        self.assertTrue(any(name in l for l in md))

    def test_B2_not_run_variants_is_named_and_not_judged(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        name = stamp()
        write_report(self.root, name, row, sha='abc12345', not_run=['variant_x'])
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertEqual(look, [])
        self.assertFalse(info['judged'])
        self.assertTrue(any(name in l for l in md))

    def test_B2_wrong_spec_is_named_and_not_judged(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        name = stamp()
        write_report(self.root, name, row, sha='abc12345', spec='other.json')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertEqual(look, [])
        self.assertFalse(info['judged'])
        self.assertTrue(any(name in l for l in md))

    def test_B2_judged_sweep_names_its_run_and_age(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        name = stamp(days_ago=3)
        write_report(self.root, name, row, sha='abc12345')
        md, look, info = N.band_part(self.root, sha='abc12345')
        self.assertTrue(info['judged'])
        self.assertTrue(any(name in l for l in md))
        self.assertEqual(info['age_days'], 3)


def fake_bench(night, seeds):
    data = {'mean': {'machine_jam_s': 1.0}, '1': {'machine_jam_s': 1.0}, '2': {'machine_jam_s': 1.0}}
    json.dump(data, open(os.path.join(night, 'bench.json'), 'w'))
    return data


class MainExitTest(unittest.TestCase):
    """[B3] main's exit must follow the band only when a finding is new since last night."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.cwd = os.getcwd()
        self.old_env = {k: os.environ.get(k) for k in ('LOCALAPPDATA', 'TW_SWEEPS')}
        os.environ['LOCALAPPDATA'] = os.path.join(self.tmp.name, 'local')
        os.environ['TW_SWEEPS'] = os.path.join(self.tmp.name, 'sweeps')
        os.makedirs(os.environ['TW_SWEEPS'], exist_ok=True)
        self.sha = N.git('rev-parse', '--short=8', 'HEAD') or 'nogit'

    def tearDown(self):
        os.chdir(self.cwd)
        for k, v in self.old_env.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v
        self.tmp.cleanup()

    def _run(self, argv):
        with mock.patch.object(sys, 'argv', ['nightly.py'] + argv), mock.patch.object(N, 'bench_part', fake_bench):
            try:
                N.main()
            except SystemExit as e:
                return e.code
        return None

    def test_fresh_in_band_sweep_exits_0(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.5, 'share_b': 0.5, 'out': [], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(os.environ['TW_SWEEPS'], stamp(), row, sha=self.sha)
        self.assertEqual(self._run(['--skip-gym']), 0)

    def test_fresh_out_of_band_sweep_exits_2(self):
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        write_report(os.environ['TW_SWEEPS'], stamp(), row, sha=self.sha)
        self.assertEqual(self._run(['--skip-gym']), 2)

    def test_same_out_of_band_sweep_already_reported_last_night_exits_0(self):
        root = os.environ['LOCALAPPDATA']
        nightly_root = os.path.join(root, 'TrenchWarfare', 'nightly')
        os.makedirs(nightly_root, exist_ok=True)
        row = {'name': 'now', 'judged': True, 'played': 10, 'ended': 10, 'unended': 0,
               'share_a': 0.72, 'share_b': 0.28, 'out': ['a'], 'label_a': 'Iron', 'label_b': 'Brass'}
        sweep_name = stamp()
        write_report(os.environ['TW_SWEEPS'], sweep_name, row, sha=self.sha)
        last_night = os.path.join(nightly_root, stamp(days_ago=1, sha='old'))
        os.makedirs(last_night, exist_ok=True)
        json.dump({'night': 'last', 'look': [], 'ran': True,
                   'band': {'sweep': sweep_name, 'age_days': 0, 'commit': self.sha, 'judged': True,
                             'reason': '', 'out': ['a']}},
                  open(os.path.join(last_night, 'report.json'), 'w'))
        self.assertEqual(self._run(['--skip-gym']), 0)

    def test_docstring_mentions_balance_band(self):
        self.assertIn('Balance band', N.__doc__)


if __name__ == '__main__':
    unittest.main()
