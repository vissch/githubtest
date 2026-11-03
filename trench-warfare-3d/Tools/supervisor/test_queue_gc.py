#!/usr/bin/env python3
"""Tests of modules/queue_gc.py. Run: python Tools/supervisor/test_queue_gc.py

On temp boards that are real git repos (never a real board): a done unit's queue file goes in one commit, the others
stay, a running relay's board is not touched, and an answer the loop's log shows refused twice for the same reason
is closed with it. Today's loop (autoloop.sweep) takes nothing off the main board and closes no answer: the proof
fails on it."""
import importlib.util
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
import autoloop  # noqa: E402
import core  # noqa: E402

spec = importlib.util.spec_from_file_location('tw_sup_mod_queue_gc', HERE / 'modules' / 'queue_gc.py')
gc = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gc)

RUN = 'RUNNING: relay 20261102-120000-1 x, lane lane/show/x\n'
IDLE = 'NOT RUNNING. Last run 20261102-110000-1: nothing left to do (3 legs).\n'


def git(repo, *a):
    r = subprocess.run(['git', '-C', str(repo), *a], capture_output=True, text=True)
    assert r.returncode == 0, (a, r.stderr)
    return r.stdout.strip()


def board(path, queued, done):
    (path / 'relay' / 'queue').mkdir(parents=True)
    (path / 'relay' / 'done').mkdir(parents=True)
    git(path, 'init', '-q')
    for k, v in (('user.email', 't@t'), ('user.name', 't'), ('commit.gpgsign', 'false')):
        git(path, 'config', k, v)
    for u in queued:
        (path / 'relay' / 'queue' / f'{u}.json').write_text(json.dumps(dict(id=u, lane=f'lane/show/{u}')), encoding='utf-8')
    for u in done:
        (path / 'relay' / 'done' / f'{u}.json').write_text('{}', encoding='utf-8')
    git(path, 'add', '-A')
    git(path, 'commit', '-q', '--allow-empty', '-m', 'start')
    return path


class World(autoloop.World):
    """Real git on temp boards; the relay's status, the pull and the Decide page are the test's."""

    def __init__(self, tmp, text, answers=()):
        super().__init__(main=tmp / 'main', extras=(), home=tmp / 'home', acting=True)
        self.extras = {2: tmp / 'b2', 3: tmp / 'b3'}
        self.text, self.ans, self.taken, self.rows = text, list(answers), [], []

    def relay(self, b, *a):
        assert a == ('status',), a
        return 0, self.text[Path(b)]

    def pull(self, b):
        return 0, ''

    def answers(self):
        return list(self.ans)

    def take(self, a, queued='', outcome='', option=''):
        self.taken.append((a['id'], outcome))

    def log_decision(self, line):
        self.rows.append(line)


class Ctx:
    def __init__(self, home, dry):
        self.home, self.dry, self.now = home, dry, 0


class QueueGC(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        t = Path(self.tmp.name)
        os.environ['TW_SUP_HOME'] = str(t / 'sup')
        board(t / 'main', ['a-done', 'b-done-on-2', 'c-open'], ['a-done'])
        board(t / 'b2', ['d-open', 'b-done-on-2', 'a-done'], ['b-done-on-2'])
        board(t / 'b3', ['a-done', 'e-open'], [])
        self.t = t
        self.text = {t / 'main': IDLE, t / 'b2': IDLE, t / 'b3': RUN}

    def tearDown(self):
        os.environ.pop('TW_SUP_HOME', None)
        self.tmp.cleanup()

    def queued(self, name):
        return sorted(p.stem for p in (self.t / name / 'relay' / 'queue').glob('*.json'))

    def test_dry_lists_and_changes_nothing(self):
        w = World(self.t, self.text)
        lines = gc.run(Ctx(self.t / 'sup', True), w, log_lines=[])
        self.assertTrue(any(l.startswith('relay 1: 2 queue files of done units would come off') for l in lines), lines)
        self.assertEqual(self.queued('main'), ['a-done', 'b-done-on-2', 'c-open'])
        self.assertEqual(git(self.t / 'main', 'rev-list', '--count', 'HEAD'), '1')

    def test_on_takes_done_files_off_idle_boards_only(self):
        w = World(self.t, self.text)
        lines = gc.run(Ctx(self.t / 'sup', False), w, log_lines=[])
        self.assertEqual(self.queued('main'), ['c-open'])
        self.assertEqual(self.queued('b2'), ['d-open'])
        self.assertEqual(self.queued('b3'), ['a-done', 'e-open'], 'relay 3 runs: its board is untouched')
        self.assertEqual(git(self.t / 'b3', 'status', '--porcelain'), '')
        self.assertEqual(git(self.t / 'b3', 'rev-list', '--count', 'HEAD'), '1')
        for b in ('main', 'b2'):
            self.assertEqual(git(self.t / b, 'status', '--porcelain'), '', 'committed, nothing left in the index')
            self.assertEqual(git(self.t / b, 'log', '-1', '--format=%s'), 'relay: 2 queue files of done units taken off')
        self.assertTrue(any('relay 3' in l and 'its relay runs' in l for l in lines), lines)

    def test_todays_sweep_leaves_the_main_board_as_it_is(self):
        """The old behaviour: autoloop.sweep cleans only the extra boards."""
        w = World(self.t, self.text)
        autoloop.sweep(w)
        self.assertEqual(self.queued('main'), ['a-done', 'b-done-on-2', 'c-open'])

    def test_a_status_that_does_not_answer_is_running(self):
        self.text[self.t / 'main'] = 'Traceback: something broke'
        w = World(self.t, self.text)
        gc.run(Ctx(self.t / 'sup', False), w, log_lines=[])
        self.assertEqual(self.queued('main'), ['a-done', 'b-done-on-2', 'c-open'])

    def test_a_held_or_just_started_relay_counts_as_running(self):
        """The loop starts the main relay with a hold, then its start script waits: status still says NOT RUNNING."""
        self.text[self.t / 'main'] = IDLE + 'The relay build is held by autoloop until 00:15 (relay.py hold).\n'
        w = World(self.t, self.text)
        autoloop.save(w.home / 'starts.json', {'2': dict(at=w.now() - 60, reason='', wait=1800)})
        lines = gc.run(Ctx(self.t / 'sup', False), w, log_lines=[])
        self.assertEqual(self.queued('main'), ['a-done', 'b-done-on-2', 'c-open'])
        self.assertEqual(self.queued('b2'), ['a-done', 'b-done-on-2', 'd-open'])
        self.assertTrue(any('relay 1' in l and 'held' in l for l in lines) and any('relay 2' in l and 'started its relay lately' in l for l in lines), lines)

    def test_a_failed_commit_puts_the_files_back(self):
        (self.t / 'b2' / '.git' / 'index.lock').write_text('', encoding='utf-8')     # a relay's git holds the index
        gc.run(Ctx(self.t / 'sup', False), World(self.t, self.text), log_lines=[])
        (self.t / 'b2' / '.git' / 'index.lock').unlink()
        self.assertEqual(self.queued('b2'), ['a-done', 'b-done-on-2', 'd-open'])
        self.assertEqual(git(self.t / 'b2', 'status', '--porcelain'), '', 'nothing left deleted and uncommitted')

    def test_registry_done_counts_only_when_its_switch_is_on(self):
        core.write_state('registry_refresh', dict(lanes={'lane/show/grok-c-open': {'c-open': 'abc1234'}}), self.t / 'sup')
        gc.run(Ctx(self.t / 'sup', False), World(self.t, self.text), log_lines=[])
        self.assertEqual(self.queued('main'), ['c-open'])
        core.write_json(self.t / 'sup' / 'modules.json', {'registry_refresh': 'on'})
        gc.run(Ctx(self.t / 'sup', False), World(self.t, self.text), log_lines=[])
        self.assertEqual(self.queued('main'), [])


class Answers(unittest.TestCase):
    LOG = ['10-20 16:18:01 answers: 2026-10-20-keep-only-model-ids: the relay refused found-keep-0-3: relay: found-keep-0-3 is done already',
           '10-20 16:23:01 answers: 2026-10-20-keep-only-model-ids: the relay refused found-keep-0-3: relay: found-keep-0-3 is done already',
           '10-20 16:23:02 answers: other-brief: the relay refused u1: not on origin yet (12 s)',
           '10-20 16:28:02 answers: other-brief: the relay refused u1: the lane is held',
           '10-20 16:28:03 answers: once-brief: the relay refused u2: no',
           '10-20 16:30:00 answers: closed-brief: the relay refused u3: same',
           '10-20 16:35:00 answers: closed-brief: the relay refused u3: same']

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.t = Path(self.tmp.name)
        for name in ('main', 'b2', 'b3'):
            board(self.t / name, [], [])
        self.text = {self.t / n: IDLE for n in ('main', 'b2', 'b3')}
        self.ans = [dict(id=b, title=b, note='n', when='2026-10-20 10:00') for b in
                    ('2026-10-20-keep-only-model-ids', 'other-brief', 'once-brief')]

    def tearDown(self):
        self.tmp.cleanup()

    def test_refusals_pairs_only_the_same_reason(self):
        got = gc.refusals(self.LOG)
        self.assertEqual(set(got), {'2026-10-20-keep-only-model-ids', 'closed-brief'})

    def test_dry_says_it_and_on_closes_with_the_reason(self):
        w = World(self.t, self.text, self.ans)
        lines = gc.run(Ctx(self.t / 'sup', True), w, log_lines=self.LOG)
        self.assertEqual([l for l in lines if l.startswith('answers:')],
                         ['answers: 2026-10-20-keep-only-model-ids would be closed: the relay refuses found-keep-0-3 '
                          '(relay: found-keep-0-3 is done already); closed by the supervisor after 2 refusals with the same reason'])
        self.assertEqual(w.taken, [])
        lines = gc.run(Ctx(self.t / 'sup', False), w, log_lines=self.LOG)
        self.assertEqual([b for b, _ in w.taken], ['2026-10-20-keep-only-model-ids'], 'once-brief (one refusal), other-brief (two reasons) and the closed one stay')
        self.assertIn('is done already', w.taken[0][1])
        self.assertEqual(len(w.rows), 1)


if __name__ == '__main__':
    unittest.main()
