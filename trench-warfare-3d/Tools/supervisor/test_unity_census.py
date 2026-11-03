#!/usr/bin/env python3
"""Tests of the Unity census (modules/unity_census.py) and the sweep (modules/sweep.py) on made-up process rows.
Run: python Tools/supervisor/test_unity_census.py"""
import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402


def load(name):
    spec = importlib.util.spec_from_file_location(f'tw_sup_test_{name}', HERE / 'modules' / f'{name}.py')
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


census, sweep = load('unity_census'), load('sweep')
NOW = 1_793_700_000
GH = 'C:\\Users\\PC\\Documents\\GitHub\\'
UNITY = '"C:\\Program Files\\Unity\\Hub\\Editor\\6000.0.58f2\\Editor\\Unity.exe"'
LIVE = {'c:/users/pc/documents/github/githubtest-relay-work3'}


def row(pid, cmd, session=0, age_h=144, name='Unity.exe'):
    return {'pid': pid, 'ppid': 1, 'name': name, 'session': session, 'cmd': cmd, 'started': NOW - age_h * 3600,
            'parent': ''}


ROWS = [
    # the stray of 10-27: batch mode on the footage project, session 0, six days old
    row(115660, f'{UNITY} -batchmode -projectPath {GH}githubtest-footage\\trench-warfare-3d -runTests'),
    # the owner's other game, batch mode and old: kept
    row(2001, f'{UNITY} -batchmode -projectPath "{GH}empire-og-client\\client"'),
    # the owner's editor window in his session: kept
    row(2002, f'{UNITY} -projectPath {GH}githubtest-frog-house\\trench-warfare-3d', session=1),
    # an editor window the Hub opened (-useHub -hubIPC), eight hours old: kept
    row(2005, f'{UNITY} -projectpath "{GH}githubtest-frog-test\\trench-warfare-3d" -useHub -hubIPC '
              '-cloudEnvironment production -licensingIpc LicenseClient-PC', session=1, age_h=8),
    # a running leg's batch editor, four hours in: kept
    row(2003, f'{UNITY} -batchmode -projectPath {GH}githubtest-relay-work3\\trench-warfare-3d', age_h=4),
    # a young batch run on another checkout: not yet
    row(2004, f'{UNITY} -batchmode -projectPath {GH}githubtest-sup-core\\trench-warfare-3d', age_h=1),
    # a hung `unity status` for a day: a stray
    row(99236, 'C:\\Users\\PC\\AppData\\Local\\unity\\bin\\unity.exe status', age_h=31),
    # the licensing client: it goes with its Unity
    row(3001, '"C:\\Program Files\\Unity\\Hub\\Editor\\6000.0.58f2\\Editor\\Data\\Resources\\Licensing\\Client\\'
              'Unity.Licensing.Client.exe" --namedPipe Unity-LicenseClient-PC', name='Unity.Licensing.Client.exe'),
]


class Census(unittest.TestCase):
    def setUp(self):
        self.got = {r['pid']: r for r in census.assess(ROWS, NOW, LIVE)}

    def test_the_footage_unity_of_10_27_is_a_stray(self):
        r = self.got[115660]
        self.assertTrue(r['stray'], r['why'])
        self.assertEqual(r['project'], 'c:/users/pc/documents/github/githubtest-footage/trench-warfare-3d')
        self.assertTrue(r['batch'])
        self.assertEqual(r['session'], 0)

    def test_kept_rows_are_not_strays(self):
        for pid, word in ((2001, 'empire-og-client'), (2002, 'window'), (2005, 'window'), (2003, 'leg runs'), (2004, 'younger'),
                          (3001, 'licensing')):
            self.assertFalse(self.got[pid]['stray'], pid)
            self.assertIn(word, self.got[pid]['why'], pid)

    def test_a_hung_status_call_is_a_stray(self):
        self.assertTrue(self.got[99236]['stray'])

    def test_the_rule_still_flags_the_leg_editor_once_its_leg_is_over(self):
        # an input that must still fail: the same editor, with no leg running on its checkout
        r = {x['pid']: x for x in census.assess(ROWS, NOW + 3600, set())}[2003]
        self.assertTrue(r['stray'], r['why'])


class Sweep(unittest.TestCase):
    def setUp(self):
        self.census = {'at': NOW, 'census': census.assess(ROWS, NOW, LIVE)}
        self.open = {'open': True, 'kind': 'licence'}

    def pids(self, breaker, now=NOW, live=LIVE, on=True):
        return sorted(p for p, _, _ in sweep.decide(breaker, self.census, now, live, on))

    def test_it_ends_only_the_strays_when_the_breaker_is_open_on_a_unity_fault(self):
        self.assertEqual(self.pids(self.open), [99236, 115660])
        self.assertEqual(self.pids({'open': True, 'kind': 'unity-wont-start'}), [99236, 115660])
        self.assertEqual(self.pids({'open': True, 'kind': 'disk'}), [])
        self.assertEqual(self.pids({'open': False, 'kind': 'licence'}), [])
        self.assertEqual(self.pids(self.open, now=NOW + 3600), [], 'a stale census must not be acted on')

    def test_never_empire_never_a_window_never_a_live_leg_even_if_the_census_said_stray(self):
        for r in self.census['census']:          # a census gone wrong: every row a stray
            r['stray'] = True
        got = self.pids(self.open)
        for pid in (2001, 2002, 2003, 2005, 3001):
            self.assertNotIn(pid, got)
        # a leg that started on the stray's checkout since the census: left alone
        live = LIVE | {'c:/users/pc/documents/github/githubtest-footage/trench-warfare-3d'}
        self.assertNotIn(115660, self.pids(self.open, live=live))

    def run_module(self, mode, breaker='on'):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        h = Path(tmp.name)
        core.write_state('breaker', self.open, h)
        core.write_state('unity_census', self.census, h)
        core.write_json(h / 'modules.json', {'sweep': mode, 'breaker': breaker})
        ended = []
        old = sweep.end, sweep.same_process, sweep.relays.running_worktrees
        sweep.end = lambda pid: (ended.append(pid), (0, 'SUCCESS'))[1]
        sweep.same_process = lambda pid, started: True
        sweep.relays.running_worktrees = lambda now=None: LIVE
        try:
            lines = core.turn(sweep, h, now=NOW + 60)
        finally:
            sweep.end, sweep.same_process, sweep.relays.running_worktrees = old
        return lines, ended, core.read_state('sweep', h)

    def test_dry_names_the_pids_and_ends_nothing(self):
        lines, ended, st = self.run_module('dry')
        self.assertEqual(ended, [])
        self.assertTrue(any('115660' in l for l in lines), lines)
        self.assertNotIn('canary_request', st)

    def test_on_ends_them_and_asks_the_canary(self):
        lines, ended, st = self.run_module('on')
        self.assertEqual(sorted(ended), [99236, 115660])
        self.assertEqual(st['canary_request']['at'], NOW + 60)

    def test_on_ends_nothing_while_the_breaker_itself_is_only_dry(self):
        self.assertEqual(self.pids(self.open, on=False), [])
        lines, ended, st = self.run_module('on', breaker='dry')
        self.assertEqual((ended, lines), ([], []))


if __name__ == '__main__':
    unittest.main(verbosity=2)
