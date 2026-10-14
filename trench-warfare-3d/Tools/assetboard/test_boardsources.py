#!/usr/bin/env python3
"""Tests of 5 findings of the second code review in the asset board's sources (CODE_REVIEW_2026-10-07.md: N11.1,
N11.2, N11.3, N11.4, N11.6). Run from trench-warfare-3d/: python Tools/assetboard/test_boardsources.py
"""
import json
import os
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-boardsources-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

import sys
sys.path.insert(0, str(HERE))
import src_git     # noqa: E402
import src_ops     # noqa: E402
import src_queue   # noqa: E402
import src_floor   # noqa: E402


def small_repo(name):
    """A one-commit git repo, trunk is HEAD, with 'refs/heads/trunk' as its only ref."""
    repo = TEMP / name
    repo.mkdir(parents=True)
    env = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t')

    def git(*args):
        p = subprocess.run(['git', '-C', str(repo), *args], capture_output=True, env=env)
        assert p.returncode == 0, (args, p.stderr)
        return p.stdout.decode().strip()
    git('init', '-q', '-b', 'trunk')
    (repo / 'f.txt').write_text('x', encoding='utf-8')
    git('add', '-A')
    git('commit', '-q', '-m', 'first')
    return repo, git


class GitRunKeepsGitsContract(unittest.TestCase):
    """[N11.6] src_git.run() tells a caller a git call failed; src_git.git() still answers '' as its callers expect."""

    def test_a_failed_call_is_ok_false_and_git_still_answers_empty(self):
        repo, git = small_repo('repo-n116-run')
        out, ok = src_git.run(repo, 'rev-parse', '--verify', 'refs/heads/does-not-exist')
        self.assertFalse(ok, '[N11.6] a non-zero git exit must be reported as ok=False')
        self.assertEqual('', src_git.git(repo, 'rev-parse', '--verify', 'refs/heads/does-not-exist'),
                          "[N11.6] git() must still answer '' for the ~60 callers that read that as an honest empty answer")

    def test_a_slow_call_times_out_instead_of_hanging_the_read(self):
        repo, git = small_repo('repo-n116-timeout')
        out, ok = src_git.run(repo, 'rev-parse', 'HEAD', timeout=0.0001)
        self.assertFalse(ok, '[N11.6] a call over its time limit must come back as ok=False, not hang')


class LanesAheadCountFailed(unittest.TestCase):
    """[N11.6] lanes(): a failed ahead count must not drop the branch, since it may well have commits."""

    def test_a_branch_with_a_failed_count_is_kept_unknown_not_dropped(self):
        repo, git = small_repo('repo-n116-lanes')
        git('branch', 'feature')
        git('checkout', '-q', 'feature')
        (repo / 'f.txt').write_text('y', encoding='utf-8')
        git('add', '-A')
        git('commit', '-q', '-m', 'second')
        old_integ, old_run = src_git.INTEGRATION, src_git.run

        def flaky(r, *args, **kw):
            if 'rev-list' in args and '--count' in args:
                return '', False
            return old_run(r, *args, **kw)
        src_git.INTEGRATION, src_git.run = 'refs/heads/trunk', flaky
        try:
            out = src_git.lanes(repo)
        finally:
            src_git.INTEGRATION, src_git.run = old_integ, old_run
        row = next((r for r in out if r['branch'] == 'feature'), None)
        self.assertIsNotNone(row, '[N11.6] a failed ahead count must not drop the branch from the list at all')
        self.assertTrue(row.get('ahead_unknown'), '[N11.6] a branch whose ahead count failed must say so, not say it has none')
        self.assertEqual(0, row['ahead'], '[N11.6] unknown is not the same as 0, but it must not be a negative or stale number either')


class QueueDriftFailed(unittest.TestCase):
    """[N11.6] src_queue.drift() and collect(): a failed drift count is unknown, and does not land a lane."""

    def test_drift_reports_unknown_on_a_failed_call(self):
        repo, git = small_repo('repo-n116-drift')
        old_run = src_git.run
        src_git.run = lambda r, *a, **kw: ('', False)
        try:
            behind, ahead = src_queue.drift(repo, 'refs/heads/trunk', 'refs/heads/trunk')
        finally:
            src_git.run = old_run
        self.assertEqual((None, None), (behind, ahead), '[N11.6] a failed drift call must read as unknown, not as 0 behind and 0 ahead')

    def test_collect_does_not_land_a_lane_whose_drift_could_not_be_read(self):
        repo, git = small_repo('repo-n116-collect')
        ops = dict(lanes=[dict(branch='lane/show/x', path=str(repo), checkout='c', dirty=0)])
        keep_gate, keep_drift = src_queue.gate_state, src_queue.drift
        src_queue.gate_state = lambda path: ('h', ['h', '2026-01-01T00:00:00'], None)
        src_queue.drift = lambda repo, integ, ref: (None, None)
        try:
            q = src_queue.collect(repo, ops, board=None, cache=dict(ci=dict(at=time.time(), run=None)),
                                   now=time.time(), integration='refs/heads/trunk')
        finally:
            src_queue.gate_state, src_queue.drift = keep_gate, keep_drift
        self.assertEqual([], q['land'], '[N11.6] a lane whose drift is unknown must not be judged landable')
        self.assertEqual([], q['said'], '[N11.6] nor listed as said, nor owed: unknown is not nothing')
        self.assertEqual([], q['owed'])
        self.assertTrue(any(b.get('kind') == 'git' and b.get('lane') == 'lane/show/x' for b in q['broken']),
                         "[N11.6] it must be said, as a Broken row naming the lane")


class CheckoutsNoOptionalLocks(unittest.TestCase):
    """[N11.4] checkouts(): status is read with --no-optional-locks, and a failed read is unknown, not clean."""

    def test_status_is_run_with_no_optional_locks(self):
        src = (HERE / 'src_ops.py').read_text(encoding='utf-8')
        self.assertIn("'--no-optional-locks', 'status'", src,
                      '[N11.4] checkouts() must read status with --no-optional-locks, so it does not take the index lock')

    def test_a_failed_status_is_unknown_not_clean(self):
        repo, git = small_repo('repo-n114-checkouts')
        old_run = src_git.run
        src_git.run = lambda r, *a, **kw: ('', False) if 'status' in a else old_run(r, *a, **kw)
        try:
            out = src_ops.checkouts(repo)
        finally:
            src_git.run = old_run
        row = next((w for w in out.values() if w['branch'] == 'trunk'), None)
        self.assertIsNotNone(row, '[N11.4] the checkout itself must still be listed')
        self.assertEqual([], row['dirty'], '[N11.4] a failed status read must not be told apart from an empty one by its own list')
        self.assertTrue(row.get('dirty_unknown'), '[N11.4] but it must say its status could not be read, not say it is clean')


class BoardReadsItemsOneByOne(unittest.TestCase):
    """[N11.3] board(): one malformed item file does not empty the whole board."""

    def test_a_broken_item_leaves_the_good_one_and_says_why(self):
        board_dir = TEMP / 'board-n113'
        (board_dir / 'items').mkdir(parents=True)
        (board_dir / 'items' / 'good.json').write_text(json.dumps(dict(
            id='good', title='A good item', lane='lane/show/known', stages=[dict(id='s1', station='desktop')])), encoding='utf-8')
        (board_dir / 'items' / 'bad.json').write_text('{ not json at all', encoding='utf-8')
        old = os.environ.get('TW_BOARD')
        os.environ['TW_BOARD'] = str(board_dir)
        try:
            out, claims = src_ops.board(HERE.parents[1], [])
        except (SystemExit, Exception) as e:
            self.fail(f'[N11.3] one malformed item file must not end the whole read of the board: {e}')
        finally:
            if old is None:
                os.environ.pop('TW_BOARD', None)
            else:
                os.environ['TW_BOARD'] = old
        by_id = {it['id']: it for it in out}
        self.assertIn('good', by_id, '[N11.3] the good item beside the bad one must still come back')
        self.assertEqual('lane/show/known', by_id['good']['lane'])
        self.assertIn('bad', by_id, '[N11.3] the bad item must be said, not silently dropped')
        self.assertEqual('Broken', by_id['bad']['lane'], '[N11.3] it is shown as a Broken row')


class UnknownLaneItemRow(unittest.TestCase):
    """[N11.2] a board item whose lane is not a key of `lanes` gets a row of its own, not a silent drop."""

    def test_an_item_on_an_unknown_lane_gets_its_own_lane_row(self):
        site = TEMP / 'site-n112'
        repo = TEMP / 'repo-n112'
        item = dict(id='x', title='X', lane='lane/show/mystery', stages=[])
        keep = (src_ops.checkouts, src_ops.roster, src_ops.board, src_ops.relay, src_ops.floor, src_git.lanes, src_floor.others)
        src_ops.checkouts = lambda repo: {}
        src_ops.roster = lambda repo: []
        src_ops.board = lambda root, skills: ([item], {})
        src_ops.relay = lambda root, now: dict(runs=[], last=None, does='')
        src_ops.floor = lambda trees, now, home=None, rel=None: []
        src_git.lanes = lambda repo: []
        src_floor.others = lambda now: ([], [])
        try:
            data = src_ops.collect(repo, site)
        except (KeyError, AttributeError) as e:
            self.fail(f'[N11.2] an item on a lane src_ops does not know must not end the read of the floor: {e}')
        finally:
            (src_ops.checkouts, src_ops.roster, src_ops.board, src_ops.relay, src_ops.floor, src_git.lanes,
             src_floor.others) = keep
        mystery = next((l for l in data['lanes'] if l['branch'] == 'lane/show/mystery'), None)
        self.assertIsNotNone(mystery, '[N11.2] the lane must be on the floor, not dropped for being a lane nobody here owns')
        self.assertEqual(['x'], [it['id'] for it in mystery['items']], '[N11.2] its item must be on that row')


class LandWordMatchesFullLane(unittest.TestCase):
    """[N11.1] his word to land a lane is matched on the note's own `lane` field, not the lane's short name shared
    by a SHOW and a SIM lane of the same name."""

    def test_show_and_sim_lanes_of_the_same_name_do_not_share_a_note(self):
        repo, git = small_repo('repo-n111')
        ops = dict(lanes=[dict(branch='lane/show/x', path=str(repo), checkout='c1', dirty=0),
                          dict(branch='lane/sim/x', path=str(repo), checkout='c2', dirty=0)])
        note = dict(id='n1', when='2026-01-01 00:00:00', state='open', kind='queue', **{'from': 'owner'},
                    about='land: x', lane='lane/show/x', text='land it')
        keep_gate, keep_drift = src_queue.gate_state, src_queue.drift
        src_queue.gate_state = lambda path: ('h', ['h', '2026-01-01T00:00:00'], None)
        src_queue.drift = lambda repo, integ, ref: (0, 1)
        try:
            q = src_queue.collect(repo, ops, board=None, cache=dict(ci=dict(at=time.time(), run=None)),
                                   now=time.time(), integration='refs/heads/trunk', notes=[note])
        finally:
            src_queue.gate_state, src_queue.drift = keep_gate, keep_drift
        said_lanes = [l['lane'] for l in q['said']]
        owed_lanes = [l['lane'] for l in q['owed']]
        self.assertEqual(['lane/show/x'], said_lanes, '[N11.1] only the lane his note named must be said land')
        self.assertEqual(['lane/sim/x'], owed_lanes, '[N11.1] the SIM lane of the same short name must not be listed under "You said land" too')


if __name__ == '__main__':
    unittest.main()
