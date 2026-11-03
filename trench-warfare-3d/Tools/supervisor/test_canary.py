#!/usr/bin/env python3
"""Tests of the canary (modules/canary.py): when it starts a gate, how it reads one, and that dry starts nothing.
Run: python Tools/supervisor/test_canary.py"""
import importlib.util
import os
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402

spec = importlib.util.spec_from_file_location('tw_sup_test_canary', HERE / 'modules' / 'canary.py')
canary = importlib.util.module_from_spec(spec)
spec.loader.exec_module(canary)
H = 3600


class Decide(unittest.TestCase):
    def test_no_verdict_starts_one_and_a_fresh_one_is_kept_for_an_hour(self):
        self.assertEqual(canary.decide({}, 1000, None)[0], 'start')
        green = {'verdict': 'green', 'at': 1000, 'started': 0}
        self.assertEqual(canary.decide(green, 1000 + H - 1, None)[0], 'none')
        self.assertEqual(canary.decide(green, 1000 + H, None)[0], 'start')

    def test_a_request_from_the_sweep_starts_one_at_once(self):
        green = {'verdict': 'green', 'at': 1000, 'started': 900}
        self.assertEqual(canary.decide(green, 1100, 1050)[0], 'start')
        self.assertEqual(canary.decide(green, 1100, 800)[0], 'none', 'a request older than the gate is answered')

    def test_reading_a_running_gate(self):
        run = {'verdict': 'running', 'started': 1000, 'at': 1000}
        self.assertEqual(canary.decide(run, 2000, None)[0], 'read')
        self.assertEqual(canary.decide(run, 2000, None, (2, 'running (pid 5)'))[1]['verdict'], 'running')
        st = canary.decide(run, 2000, None, (0, 'not running\nGate green. Tested tree abc'))[1]
        self.assertEqual((st['verdict'], st['started']), ('green', 1000))
        st = canary.decide(run, 2000, None, (8, '[license] Error: Timed-out waiting for Licensing\nended without'))[1]
        self.assertEqual((st['verdict'], st['kind']), ('red', 'licence'))
        st = canary.decide(run, 2000, None, (8, 'FAILED TW.Tests.Foo\nended without a green verdict'))[1]
        self.assertEqual((st['verdict'], st['kind']), ('red', 'test'))
        st = canary.decide(run, 1000 + 4 * H, None, (2, 'running (pid 5)'))[1]
        self.assertEqual((st['verdict'], st['kind']), ('red', 'gate-stall'))
        st = canary.decide(run, 1000 + 4 * H, None, (1, 'the gate could not be read'))[1]
        self.assertEqual((st['verdict'], st['kind']), ('red', 'gate-stall'), 'an unreadable gate must stall too')
        st = canary.decide(run, 2000, None, (1, 'no gate was started here with gate_bg.py'))[1]
        self.assertEqual(st['verdict'], 'unknown', 'no gate there is no verdict, not a red')
        self.assertEqual(canary.decide(st, 2001, None)[0], 'start')

    def test_a_failed_start_waits_before_the_next_try(self):
        st = {'error': 'dirty', 'tried': 1000}
        self.assertEqual(canary.decide(st, 1000 + 60, None)[0], 'none')
        self.assertEqual(canary.decide(st, 1000 + canary.RETRY_S, None)[0], 'start')


class Shell(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = Path(self.tmp.name)
        self.started = []
        self.old = canary.start
        canary.start = lambda co: (self.started.append(co), ('tree1', 'log1'))[1]
        os.environ.pop('TW_CANARY_CHECKOUT', None)

    def tearDown(self):
        canary.start = self.old
        self.tmp.cleanup()

    def turn(self, mode, now=10_000):
        core.write_json(self.home / 'modules.json', {'canary': mode})
        return core.turn(canary, self.home, now=now)

    def test_dry_says_what_it_would_do_and_starts_nothing(self):
        lines = self.turn('dry')
        self.assertEqual(self.started, [])
        self.assertIn('would start a gate', lines[0])

    def test_on_starts_one_and_records_it(self):
        self.turn('on')
        self.assertEqual(self.started, [Path(canary.DEFAULT)])
        st = core.read_state('canary', self.home)
        self.assertEqual((st['verdict'], st['tree'], st['started']), ('running', 'tree1', 10_000))

    def test_never_the_landing_checkout_nor_any_other_not_named_canary(self):
        for other in ('githubtest-desk-gate', 'githubtest-relay-work3', 'tw3d-board-2'):
            core.write_json(self.home / 'canary.json', {'checkout': f'C:/Users/PC/Documents/GitHub/{other}'})
            lines = self.turn('on')
            self.assertEqual(self.started, [], other)
            self.assertIn('not a canary checkout', lines[0])


if __name__ == '__main__':
    unittest.main(verbosity=2)
