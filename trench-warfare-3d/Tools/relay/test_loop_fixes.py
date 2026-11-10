#!/usr/bin/env python3
"""Tests of the loop fixes of 2026-11-10 (the loop review: plan also-review-the-vfx-dapper-wirth). Run:
python Tools/relay/test_loop_fixes.py

Each case fails on 0aacc2bca:
  - a relay start after autoloop.stop appears is refused (11-08: started 8 min after the stop);
  - a failed unit's leftover files are put aside, the checkout is clean after (15 of 79 runs stopped on them);
  - the queue's trust is asked in two git calls, not two a file (665 files on the main board);
  - the retro leg is not on Haiku (it starts in default mode: probe of 11-10, three runs ended there)."""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import gitio  # noqa: E402
from sources import lane  # noqa: E402


def git(cwd, *a):
    subprocess.run(['git', *a], cwd=str(cwd), check=True, capture_output=True)


def repo(tmp):
    git(tmp, 'init', '-q')
    git(tmp, 'config', 'user.email', 't@t')
    git(tmp, 'config', 'user.name', 't')
    git(tmp, 'config', 'core.autocrlf', 'false')
    (Path(tmp) / 'a.txt').write_text('one\n')
    git(tmp, 'add', '-A')
    git(tmp, 'commit', '-q', '-m', 'base')


class StopFile(unittest.TestCase):
    def test_no_start_once_the_stop_file_is_there(self):
        import autoloop
        with tempfile.TemporaryDirectory() as home:
            old = os.environ.get('TW_SUP_HOME')
            os.environ['TW_SUP_HOME'] = home          # no modules.json: the breaker allows
            try:
                w = autoloop.World(main=home, extras=(), home=home, acting=False)
                self.assertEqual(w.start(2), 'would start')
                (Path(home) / 'autoloop.stop').write_text('owner: stop')
                self.assertTrue(w.start(2).startswith('not started: autoloop.stop'))
            finally:
                if old is None:
                    os.environ.pop('TW_SUP_HOME', None)
                else:
                    os.environ['TW_SUP_HOME'] = old


class TakeOff(unittest.TestCase):
    """queue_gc 2026-11-10: 646 paths in one `git add` = WinError 206; the files were deleted and never put back."""
    def test_many_queue_files_come_off_in_one_commit(self):
        import autoloop
        with tempfile.TemporaryDirectory() as tmp:
            repo(tmp)
            q = Path(tmp) / 'relay' / 'queue'
            q.mkdir(parents=True)
            uids = ['a-very-long-unit-name-of-the-sort-the-loop-writes-%04d-20261110' % i for i in range(700)]
            for u in uids:
                (q / f'{u}.json').write_text('{}')
            git(tmp, 'add', '-A')
            git(tmp, 'commit', '-q', '-m', 'queue')
            w = autoloop.World(main=tmp, extras=(), home=tmp, acting=True)
            said = autoloop.take_off(w, tmp, uids, push=False)
            self.assertEqual(said, 'committed (kept local)', said)
            self.assertEqual(list(q.glob('*.json')), [])
            self.assertEqual(gitio.dirty(tmp), [])

    def test_a_git_that_cannot_start_puts_the_files_back(self):
        import autoloop
        with tempfile.TemporaryDirectory() as tmp:
            repo(tmp)
            q = Path(tmp) / 'relay' / 'queue'
            q.mkdir(parents=True)
            (q / 'u1.json').write_text('{"id": "u1"}')
            git(tmp, 'add', '-A')
            git(tmp, 'commit', '-q', '-m', 'queue')
            w = autoloop.World(main=tmp, extras=(), home=tmp, acting=True)
            real = w.git

            def broken(board, *a):
                if a and a[0] == 'add':
                    raise FileNotFoundError(206, 'The filename or extension is too long')
                return real(board, *a)
            w.git = broken
            said = autoloop.take_off(w, tmp, ['u1'], push=False)
            self.assertIn('put back', said)
            self.assertEqual((q / 'u1.json').read_text(), '{"id": "u1"}')
            self.assertEqual(gitio.dirty(tmp), [])


class PutAside(unittest.TestCase):
    def test_leftover_work_is_saved_and_the_checkout_is_clean(self):
        with tempfile.TemporaryDirectory() as tmp, tempfile.TemporaryDirectory() as leg:
            repo(tmp)
            (Path(tmp) / 'a.txt').write_text('two\n')
            (Path(tmp) / 'new.txt').write_text('new\n')
            self.assertEqual(gitio.put_aside(tmp, leg, 'run unit'), '')
            self.assertEqual(gitio.dirty(tmp), [])
            patch = (Path(leg) / 'red.patch').read_bytes()
            self.assertIn(b'new.txt', patch)
            self.assertIn(b'+two', patch)
            stashes = subprocess.run(['git', 'stash', 'list'], cwd=tmp, capture_output=True, text=True).stdout
            self.assertIn('put aside by the relay: run unit', stashes)

    def test_a_clean_checkout_is_left_alone(self):
        with tempfile.TemporaryDirectory() as tmp, tempfile.TemporaryDirectory() as leg:
            repo(tmp)
            self.assertEqual(gitio.put_aside(tmp, leg, 'x'), '')
            self.assertFalse((Path(leg) / 'red.patch').exists())


class QueueTrust(unittest.TestCase):
    def board(self, tmp):
        repo(tmp)
        q = Path(tmp) / 'relay' / 'queue'
        q.mkdir(parents=True)
        for name in ('ok', 'changed', 'renamed'):
            (q / f'{name}.json').write_text(json.dumps({'id': name}))
        git(tmp, 'add', '-A')
        git(tmp, 'commit', '-q', '-m', 'queue')
        (q / 'changed.json').write_text(json.dumps({'id': 'changed', 'priority': 1}))
        (q / 'loose.json').write_text(json.dumps({'id': 'loose'}))
        git(tmp, 'mv', 'relay/queue/renamed.json', 'relay/queue/moved.json')
        return q

    def test_same_verdicts_as_one_file_at_a_time(self):
        with tempfile.TemporaryDirectory() as tmp:
            q = self.board(tmp)
            ok, skipped = lane.files(tmp)
            self.assertEqual([p.name for p in ok], ['ok.json'])
            self.assertEqual(sorted(p.name for p in skipped), ['changed.json', 'loose.json', 'moved.json'])
            for p in q.glob('*.json'):
                self.assertEqual(lane.trusted(tmp, p), p in ok, p.name)

    def test_two_git_calls_for_the_whole_queue(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.board(tmp)
            calls, real = [], gitio.git_raw

            def counting(args, cwd, env=None):
                calls.append(args)
                return real(args, cwd, env)
            gitio.git_raw = counting
            try:
                lane.files(tmp)
            finally:
                gitio.git_raw = real
            self.assertLessEqual(len(calls), 2, calls)


class Ceiling(unittest.TestCase):
    """With no finish work queued, fix and critique used to get the whole day (a kind with nothing lends its room)."""
    def setUp(self):
        import sources
        self.sources = sources
        self.real = sources.load

        class Fake:
            def __init__(self, kinds):
                self.kinds = kinds

            def next(self, ctx, kind=None):
                return {'id': 'u-' + kind, 'kind': kind} if kind in self.kinds else None
        self.fake = Fake(('fix', 'critique'))           # no finish work at all
        sources.load = lambda name: self.fake

    def tearDown(self):
        self.sources.load = self.real

    def ctx(self, spent, cap):
        return dict(shares={'finish': 60, 'fix': 25, 'critique': 15}, spent_kinds=spent, cap_usd=cap)

    def test_fix_runs_under_its_share_of_the_cap(self):
        u = self.sources.next_unit(['lane'], self.ctx({'fix': 10, 'critique': 10}, 100))
        self.assertEqual(u['kind'], 'fix')

    def test_nothing_runs_once_both_have_spent_their_share_of_the_cap(self):
        self.assertIsNone(self.sources.next_unit(['lane'], self.ctx({'fix': 25, 'critique': 15}, 100)))

    def test_critique_alone_left_under_its_share(self):
        u = self.sources.next_unit(['lane'], self.ctx({'fix': 30, 'critique': 5}, 100))
        self.assertEqual(u['kind'], 'critique')


class RetroModel(unittest.TestCase):
    def test_the_retro_leg_is_not_on_haiku(self):
        ph = json.loads((HERE / 'phases.json').read_text(encoding='utf-8'))
        self.assertNotEqual(ph['retro']['model'], 'haiku',
                            'a headless Haiku start gets permissionMode default and the run stops at its retro')


if __name__ == '__main__':
    unittest.main(verbosity=2)
