#!/usr/bin/env python3
"""Tests for abtest.py's report() and nightly.py's compare_bench() with no editor: python Tools/test_abtest.py.

A metric missing on a seed must be left out of the movement, not read as 0 [N13.11]. Stdlib only.
"""
import contextlib, io, json, os, sys, tempfile, unittest, warnings
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import abtest as A
import nightly as N


class ReportTest(unittest.TestCase):
    def setUp(self):
        warnings.simplefilter('ignore', ResourceWarning)
        self.tmp = tempfile.TemporaryDirectory()

    def tearDown(self):
        self.tmp.cleanup()

    def write(self, name, data):
        json.dump(data, open(os.path.join(self.tmp.name, name + '.json'), 'w'))

    def test_a_metric_missing_on_a_seed_is_not_a_movement(self):
        # "now" has t on seed1 only; "v" has it on all three: only seed1 is a real pair.
        self.write('now', {'seed1': {'t': 10.0}, 'seed2': {}, 'seed3': {}, 'mean': {'t': 10.0}})
        self.write('v', {'seed1': {'t': 14.0}, 'seed2': {'t': 30.0}, 'seed3': {'t': 20.0}, 'mean': {'t': 21.3}})
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            A.report(self.tmp.name, ['now', 'v'])
        out = buf.getvalue()
        self.assertIn('v: up, 1 of 3', out, '[N13.11] only seed1 has the metric on both sides: not "all up"')
        self.assertNotIn('all up', out, '[N13.11] a cell must never claim every seed when only one has the number')

    def test_a_variant_missing_a_seed_does_not_break_the_table(self):
        self.write('now', {'seed1': {'t': 10.0}, 'seed2': {'t': 10.0}, 'mean': {'t': 10.0}})
        self.write('v', {'seed1': {'t': 11.0}, 'mean': {'t': 11.0}})  # v has no seed2 at all
        buf = io.StringIO()
        try:
            with contextlib.redirect_stdout(buf):
                A.report(self.tmp.name, ['now', 'v'])
        except Exception as e:
            self.fail(f'[N13.11] a variant report missing a seed must not raise: {e!r}')
        self.assertIn('v: up, 1 of 2', buf.getvalue())


class CompareBenchTest(unittest.TestCase):
    def test_a_night_metric_missing_on_a_seed_is_not_a_movement(self):
        m = 'machine_jam_s'  # a WORSE_UP metric: a wrongly-full "all up" would also wrongly flag "worse"
        now = {'seed1': {m: 10.0}, 'seed2': {m: 10.0}, 'seed3': {m: 10.0}, 'mean': {m: 10.0}}
        then = {'seed1': {m: 5.0}, 'seed2': {}, 'seed3': {m: 5.0}, 'mean': {m: 5.0}}
        lines, worse = N.compare_bench(now, then)
        row = [l for l in lines if f'| {m} |' in l][0]
        self.assertIn('2 of 3', row, '[N13.11] only seed1 and seed3 have the metric on both nights')
        self.assertNotIn('all up', row, '[N13.11] a missing seed must not be read as a movement into "all up"')
        self.assertEqual(worse, [], "[N13.11] 'worse' only fires on a full-coverage 'all up'")


if __name__ == '__main__':
    unittest.main(verbosity=1)
