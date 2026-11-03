#!/usr/bin/env python3
"""Tests of the retry rule (retry.py, modules/retry.py) and its hook in loopsteps.attempts(). Run:
python Tools/supervisor/test_retry.py

The replay proofs, on the records of 10-19..11-02: fix-job-ordering-tests ran 20 times with no pass under today's
rule and is parked after its 2nd try here; the relays' 3+-in-a-row streaks do not happen with the cool-down; units
whose every failed try was a machine fault (the licence of 10-31..11-02) are not parked, where counting every
FAIL/BLOCKED alike (today's count, which also took machine faults) parks them."""
import importlib.util
import os
import sys
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402
import retry  # noqa: E402
from replay import records  # noqa: E402
from signatures import is_machine  # noqa: E402


def put_back(key, old):
    """An environment variable as it was before a test set it (None: it was not set)."""
    if old is None:
        os.environ.pop(key, None)
    else:
        os.environ[key] = old

spec = importlib.util.spec_from_file_location('tw_sup_mod_retry', HERE / 'modules' / 'retry.py')
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


def T(kind, verdict='FAIL', run='r', at='2026-10-20T00:00:00Z', relay='1'):
    return dict(kind=kind, verdict=verdict, run=run, at=at, relay=relay)


class Rule(unittest.TestCase):
    def test_two_of_one_kind_park(self):
        d = retry.decide([T('push-refused', run='a'), T('unit', run='b'), T('push-refused', run='c')])
        self.assertTrue(d['park'])
        self.assertEqual((d['kind'], d['run']), ('push-refused', 'c'))

    def test_two_different_kinds_do_not(self):
        self.assertFalse(retry.decide([T('push-refused'), T('unit')])['park'])

    def test_a_machine_fault_is_no_try(self):
        self.assertTrue(is_machine('licence'))
        self.assertFalse(retry.decide([T('licence')] * 6 + [T('gate-stall')] * 3 + [T('unit')])['park'])

    def test_a_pass_and_since_start_the_count_over(self):
        self.assertFalse(retry.decide([T('unit'), T('', 'PASS'), T('unit')])['park'])
        ts = [T('unit', at='2026-10-20T01:00:00Z'), T('unit', at='2026-10-20T03:00:00Z')]
        self.assertTrue(retry.decide(ts)['park'])
        self.assertFalse(retry.decide(ts, since='2026-10-20T02:00:00Z')['park'])

    def test_cooling(self):
        self.assertTrue(retry.cooling({'x': 'FAIL', 'y': 'PASS'}, 'x'))
        self.assertFalse(retry.cooling({'x': 'FAIL', 'y': 'PASS'}, 'y'))
        self.assertFalse(retry.cooling({}, 'x'))


class Replay(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rs = records.load()
        cls.tries = retry.tries_of(cls.rs.runs, cls.rs.legs)

    def test_fix_job_ordering_tests_is_parked_within_four_tries(self):
        ts = self.tries['fix-job-ordering-tests']
        self.assertEqual(len(ts), 20, 'the old behaviour: 20 tries, no pass, never parked')
        d = retry.decide(ts)
        print(f'\n  retry: fix-job-ordering-tests parked after try {d["index"] + 1} of 20 ({d["kind"]}); '
              f'{20 - d["index"] - 1} tries not started')
        self.assertTrue(d['park'])
        self.assertLessEqual(d['index'] + 1, 4)

    def test_no_unit_runs_twice_in_a_row_on_a_relay_with_the_cool_down(self):
        old = retry.streaks(self.rs.runs, least=3)
        new = retry.streaks(retry.with_cooldown(self.rs.runs), least=2)
        print(f'\n  retry: {len(old)} streaks of 3+ failed runs of one unit on one relay; with the cool-down {len(new)} of even 2')
        self.assertGreaterEqual(len(old), 15, 'the old behaviour')
        self.assertEqual(new, [])

    def test_units_held_only_by_machine_faults_are_not_parked(self):
        machine_only = {u for u, ts in self.tries.items()
                        if sum(1 for t in ts if t['verdict'] != 'PASS') >= 2
                        and all(is_machine(t['kind']) for t in ts if t['verdict'] != 'PASS')}
        lic = {u for u in machine_only if any(t['kind'] == 'licence' for t in self.tries[u])}
        parked_new = {u for u in machine_only if retry.decide(self.tries[u])['park']}
        parked_old = {u for u in machine_only if retry.decide(self.tries[u], machine_counts=True, by_kind=False)['park']}
        print(f'\n  retry: {len(machine_only)} units failed only on machine faults ({len(lic)} with the licence); '
              f'parked by counting every fail: {len(parked_old)}; by the retry rule: {len(parked_new)}')
        self.assertGreaterEqual(len(lic), 5)
        self.assertEqual(parked_new, set())
        self.assertEqual(parked_old, machine_only, 'the old count parks them: the proof fails on it')

    def test_the_same_units_own_fault_still_parks(self):
        """Must still fail: a unit with two tries of its own kind amid machine faults is parked."""
        ts = [T('licence'), T('unit', run='a'), T('licence'), T('gate-stall'), T('unit', run='b')]
        self.assertEqual(retry.decide(ts)['run'], 'b')


class Module(unittest.TestCase):
    def test_step_logs_a_new_park_once_and_starts_over_after_the_loop_parked_it(self):
        ts = {'x': [T('unit', run='a', at='2026-10-20T01:00:00Z'), T('unit', run='b', at='2026-10-20T02:00:00Z')]}
        units, lines = mod.step({}, ts, {'x'}, {}, dry=True)
        self.assertTrue(units['x']['park'])
        self.assertEqual(lines, ['x would be parked: 2 tries ended unit (a, b)'])
        units, lines = mod.step(units, ts, {'x'}, {}, dry=True)
        self.assertEqual(lines, [], 'said once')
        parks = {'x': '2026-10-20T03:00:00Z'}           # the loop parked it (autoloop's parked/x.json)
        units, _ = mod.step(units, ts, {'x'}, parks, dry=False)
        self.assertFalse(units['x']['park'], 'back on a queue: the tries that parked it do not park it again')
        ts['x'].append(T('unit', run='c', at='2026-10-20T04:00:00Z'))
        units, _ = mod.step(units, ts, {'x'}, parks, dry=False)
        self.assertFalse(units['x']['park'], 'one new try')
        ts['x'].append(T('unit', run='d', at='2026-10-20T05:00:00Z'))
        units, lines = mod.step(units, ts, {'x'}, parks, dry=False)
        self.assertTrue(units['x']['park'])
        self.assertEqual(lines, ['x: the loop parks it: 2 tries ended unit (c, d)'])
        units, _ = mod.step(units, {}, {'x'}, parks, dry=False)
        self.assertFalse(units['x']['park'], 'its tries left the window: a unit queued again is not parked on them')

    def test_the_cool_down_hook(self):
        import hooks
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        home, board = Path(tmp.name) / 'sup', Path(tmp.name) / 'tw3d-board-2'
        self.addCleanup(put_back, 'TW_SUP_HOME', os.environ.get('TW_SUP_HOME'))
        os.environ['TW_SUP_HOME'] = str(home)
        stops = board / 'relay' / 'desktop' / 'stops'
        stops.mkdir(parents=True)
        core.write_json(stops / '20261020-010000-1.json', {'units': {'x': 'FAIL', 'y': 'PASS'}})
        self.assertEqual(hooks.skip('x', board, 'lane/show/x'), '', 'retry dry: no cool-down')
        core.write_json(home / 'modules.json', {'retry': 'on'})
        self.assertEqual(hooks.skip('x', board, 'lane/show/x'), 'cool-down: the newest run on this board ended x FAIL')
        self.assertEqual(hooks.skip('y', board, 'lane/show/y'), '')
        core.write_json(stops / '20261020-020000-1.json', {'units': {}, 'reason': 'nothing left to do'})
        self.assertEqual(hooks.skip('x', board, 'lane/show/x'), '', 'a run in between ends the cool-down: never kept off for good')


class Kept(unittest.TestCase):
    """The hook reads a fresh state only (S.3), and a test leaves TW_SUP_HOME as it found it (M.1)."""

    def test_retry_park_does_not_act_on_a_stale_state(self):
        import time
        import hooks
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        h = Path(tmp.name)
        core.write_json(h / 'modules.json', {'retry': 'on'})
        core.write_state('retry', dict(units={'x': dict(park=True, why='2 tries ended unit')}, at=time.time() - 3 * 3600), h)
        self.assertIsNone(hooks.retry_park('x', h), 'a retry state of a stopped supervisor parks nothing: a unit its lane '
                                                    'brought back would be parked again every round (S.3)')
        core.write_state('retry', dict(units={'x': dict(park=True, why='2 tries ended unit')}, at=time.time()), h)
        self.assertEqual(hooks.retry_park('x', h), (True, '2 tries ended unit'))

    def test_the_tests_put_tw_sup_home_back(self):
        old = os.environ.get('TW_SUP_HOME')
        os.environ['TW_SUP_HOME'] = 'C:/sentinel-sup-home'
        try:
            for t in (Module('test_the_cool_down_hook'), Hook('test_retry_on_does_not_park_a_unit_it_does_not_call_due')):
                r = unittest.TestResult()
                t.run(r)
                self.assertTrue(r.wasSuccessful(), r.errors + r.failures)
                self.assertEqual(os.environ.get('TW_SUP_HOME'), 'C:/sentinel-sup-home', f'{t.id()} did not put TW_SUP_HOME back (M.1)')
        finally:
            if old is None:
                os.environ.pop('TW_SUP_HOME', None)
            else:
                os.environ['TW_SUP_HOME'] = old


class Hook(unittest.TestCase):
    """The hook in loopsteps.attempts(): today's rule while retry is not 'on'; retry's verdict when it is."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = Path(self.tmp.name) / 'sup'
        self.old_home = os.environ.get('TW_SUP_HOME')       # before the imports below set one of their own
        sys.path.insert(0, str(HERE.parent / 'assetboard'))
        import test_loopsteps, test_autoloop  # noqa: F401,E401  they set a TW_SUP_HOME of their own on import: first
        os.environ['TW_SUP_HOME'] = str(self.home)

    def tearDown(self):
        put_back('TW_SUP_HOME', self.old_home)
        self.tmp.cleanup()

    def run_loop(self, name, mode):
        import autoloop
        import test_loopsteps as tl
        from test_autoloop import put
        if mode:
            core.write_json(self.home / 'modules.json', {'retry': mode})
        core.write_state('retry', dict(units={'moving-1': dict(park=True, why='2 tries ended push-refused (a, b)')},
                                       at=time.time()), self.home)      # fresh: hooks.retry_park reads no stale state (S.3)
        w = tl.world(Path(self.tmp.name), name)
        put(w.main, 'queue', 'moving-1', lane='lane/show/moving', priority=40)

        class Moving(type(w)):          # its lane moved between its two tries: today's rule gives it one more
            def lane_sha(self, lane, at=''):
                return {'': 'L2', '2026-10-15T04:15:29Z': 'L1', '2026-10-15T05:33:21Z': 'L2'}.get(at, 'L2')
        w.__class__ = Moving
        w.stop_records = [(r, at, 'moving-1', v, y) for r, at, _, v, y in tl.STOPS[:2]]
        return w, autoloop.round_(w)

    def test_retry_on_parks_what_todays_rule_lets_run_again(self):
        w, lines = self.run_loop('dry', 'dry')
        self.assertEqual(w.unqueued, [], 'retry dry: today\'s rule, which gives a moved unit one more try')
        w, lines = self.run_loop('on', 'on')
        self.assertIn(('main', 'moving-1'), w.unqueued, lines)
        self.assertTrue(any('parked by the retry rule: 2 tries ended push-refused' in l for l in lines), lines)
        self.assertEqual(len(w.briefs_written), 1)

    def test_retry_on_does_not_park_a_unit_it_does_not_call_due(self):
        core.write_json(self.home / 'modules.json', {'retry': 'on'})
        import autoloop
        import test_loopsteps as tl
        from test_autoloop import put
        core.write_state('retry', dict(units={'relay-harness-rebase-b': dict(park=False)}, at=time.time()), self.home)
        w = tl.world(Path(self.tmp.name), 'machine')
        put(w.main, 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness', priority=40)
        w.stop_records = tl.STOPS           # three BLOCKED with nothing changed: today's rule parks it
        autoloop.round_(w)
        self.assertEqual(w.unqueued, [], 'retry on replaces the fail count: its tries were not two of one kind')


if __name__ == '__main__':
    unittest.main()
