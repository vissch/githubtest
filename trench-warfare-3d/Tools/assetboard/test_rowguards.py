#!/usr/bin/env python3
"""Tests of the rows a read of the board may meet broken (Tools/assetboard/src_ops.py, src_queue.py), the finding
N11.5 of the second code review. Run from trench-warfare-3d/:
python Tools/assetboard/test_rowguards.py

The watcher reads the same sources every tick. One transcript with a `<command-name>/` and no name, one half-written
data/assets.json, one approval that is a JSON list, one null date or one brief without an id used to end the read:
every page then went stale. Each of those rows is now skipped and said, and the rest of the rows still come back.
"""
import json
import os
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-rowguards-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

import sys
sys.path.insert(0, str(HERE))
import src_ops     # noqa: E402
import src_queue   # noqa: E402


def transcript(folder: Path, sid: str, lines):
    folder.mkdir(parents=True, exist_ok=True)
    (folder / f'{sid}.jsonl').write_text('\n'.join(json.dumps(d) for d in lines) + '\n', encoding='utf-8')


class CommandNameRow(unittest.TestCase):
    """[N11.5] a transcript whose user text is `<command-name>/` with no name beside one with `/pipeline`."""

    def test_a_command_with_no_name_is_passed_over(self):
        projects = TEMP / 'projects'
        when = '2026-10-12T00:00:00.000Z'
        for sid, text in (('aaaa-no-name', '<command-name>/'), ('bbbb-pipeline', '<command-name>/pipeline</command-name>')):
            transcript(projects / sid, sid, [
                dict(type='user', cwd=str(TEMP), timestamp=when, message=dict(content=[dict(type='text', text=text)])),
            ])
        old = src_ops.PROJECTS
        src_ops.PROJECTS = projects
        try:
            got = src_ops.sessions({}, time.time())
        except AttributeError as e:
            self.fail(f'[N11.5] a `<command-name>/` with no name ended the whole read of the sessions: {e}')
        finally:
            src_ops.PROJECTS = old
        by_id = {s['id']: s for s in got}
        self.assertIn('bbbb-pipeline', by_id, '[N11.5] the session beside the broken row did not come back')
        self.assertIn('pipeline', by_id['bbbb-pipeline']['skills'], '[N11.5] the good session lost its skill')
        self.assertEqual({}, by_id.get('aaaa-no-name', {}).get('skills', {}),
                         '[N11.5] a `<command-name>/` with no name must leave no skill behind')


class AssetsJsonRow(unittest.TestCase):
    """[N11.5] data/assets.json, which build.py rewrites in place, with an asset row that has no models or status."""

    def test_a_broken_asset_row_leaves_the_other_lanes_their_assets(self):
        site = TEMP / 'site-assets'
        (site / 'data').mkdir(parents=True, exist_ok=True)
        (site / 'data' / 'assets.json').write_text(json.dumps(dict(assets=[
            dict(id='broken', name='Half written', lanes=[dict(branch='lane/show/good', touches_art=True)]),
            dict(id='maw', name='The Maw', status='merged', models=[dict(thumb='img/maw.png', films=[])],
                 lanes=[dict(branch='lane/show/good', touches_art=True)]),
        ])), encoding='utf-8')
        lanes = {'lane/show/good': dict(branch='lane/show/good', assets=[])}
        fn = getattr(src_ops, 'lane_assets', None)
        self.assertTrue(fn, '[N11.5] src_ops has no lane_assets: the assets.json read is still unguarded')
        try:
            fn(site, lanes)
        except (KeyError, TypeError) as e:
            self.fail(f'[N11.5] an asset row with no models ended the read of assets.json: {e}')
        self.assertEqual(['maw'], [a['id'] for a in lanes['lane/show/good']['assets']],
                         '[N11.5] the good asset row must still reach its lane')


class ApprovalRows(unittest.TestCase):
    """[N11.5] an approvals folder with a file that is a JSON list, and one whose date is null."""

    def test_a_list_and_a_null_date_leave_the_good_approvals(self):
        board = TEMP / 'board'
        (board / 'approvals').mkdir(parents=True, exist_ok=True)
        (board / 'approvals' / 'a-list.json').write_text('["lane/show/x", 2]', encoding='utf-8')
        (board / 'approvals' / 'b-null.json').write_text(json.dumps(dict(lane='lane/show/b', date=None, words='land it')), encoding='utf-8')
        (board / 'approvals' / 'c-good.json').write_text(json.dumps(dict(lane='lane/show/c', date='2026-10-01', words='land it')), encoding='utf-8')

        class Git:
            INTEGRATION = 'origin/integ'

            @staticmethod
            def branch_refs(repo):
                return {'lane/show/b': ('origin/lane/show/b',), 'lane/show/c': ('origin/lane/show/c',)}

            @staticmethod
            def git(repo, *args):
                return '1'

            @staticmethod
            def run(repo, *args, timeout=None):
                return '1', True

            @staticmethod
            def family(name):
                return name

        keep = (src_queue.src_git, src_queue.drift, src_queue.merged_into, src_queue.gate_state)
        src_queue.src_git, src_queue.drift = Git, lambda repo, integ, ref: (0, 1)
        src_queue.merged_into, src_queue.gate_state = (lambda repo, ref: set()), (lambda path: ('', [], None))
        try:
            got = src_queue.approvals(TEMP, 'origin/integ', board, [])
        except (TypeError, KeyError) as e:
            self.fail(f'[N11.5] a broken approval row ended the read of every approval: {e}')
        finally:
            src_queue.src_git, src_queue.drift, src_queue.merged_into, src_queue.gate_state = keep
        self.assertEqual(['lane/show/b', 'lane/show/c'], sorted(a['lane'] for a in got),
                         '[N11.5] the approvals beside the JSON list must still come back')


class BriefWithoutId(unittest.TestCase):
    """[N11.5] a brief whose JSON carries no id at all."""

    def test_a_brief_with_no_id_is_a_row_and_not_an_error(self):
        try:
            row = src_queue.brief_row(dict(title='No id at all', options=[], evidence=[]))
        except KeyError as e:
            self.fail(f'[N11.5] a brief with no id ended the read of the queue: {e}')
        self.assertEqual('', row['brief'], '[N11.5] a brief with no id has no id to lead to')
        self.assertEqual('', row['shot'], '[N11.5] a brief with no id can have no picture folder either')


if __name__ == '__main__':
    unittest.main()
