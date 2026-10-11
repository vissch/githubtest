#!/usr/bin/env python3
"""Tests of the states a decision brief passes through (Tools/assetboard/briefs.py), the findings N10.1, N10.3,
N10.4 and N10.13 of the second code review. Run from trench-warfare-3d/:
python Tools/assetboard/test_briefstates.py

His answer on the Decide page is the decision, so a brief that is shown as decided when nobody was told, closed as
queued when nothing was queued, or lost to a write cut half way, is his word dropped.
"""
import json
import os
import pathlib
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-briefstates-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

import sys
sys.path.insert(0, str(HERE))
import briefs      # noqa: E402  (after TW_BRIEFS: it must never see the live folder)


def a_brief(where, title='The gas ability', bid=''):
    return briefs.add(where, title, 'It needs a yes before it is built.', ['Build it', 'Leave it'],
                      'It is cheap and it reads at every zoom.', by='test', bid=bid,
                      no_evidence='nothing is built yet: there is nothing to show')


class Unreadable(unittest.TestCase):
    """[N10.4] read_all passes over a brief.json that does not read, or whose id is not its folder's name; what it
    passed over must be told, not swallowed."""

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=TEMP))

    def test_unreadable_names_a_cut_brief_and_a_renamed_folder(self):
        # [N10.4] a brief.json cut in half, and a brief whose folder was renamed
        good = a_brief(self.where, 'The one that reads')
        cut = a_brief(self.where, 'The one cut in half')
        f = self.where / cut['id'] / 'brief.json'
        f.write_text(f.read_text(encoding='utf-8')[:len(f.read_text(encoding='utf-8')) // 2], encoding='utf-8')
        moved = a_brief(self.where, 'The one that was renamed')
        (self.where / moved['id']).rename(self.where / (moved['id'] + '-old'))
        if not hasattr(briefs, 'unreadable'):
            self.fail('[N10.4] read_all has no way to say what it passed over: briefs.unreadable is missing')
        bad = {x['id']: x['why'] for x in briefs.unreadable(self.where)}
        self.assertEqual(sorted(bad), sorted([cut['id'], moved['id'] + '-old']),
                         '[N10.4] the briefs that did not read are not named: %s' % sorted(bad))
        self.assertIn('does not read', bad[cut['id']], '[N10.4] a brief.json cut in half is passed over without a word')
        self.assertIn('id', bad[moved['id'] + '-old'], '[N10.4] a renamed brief folder is dropped without a word')
        self.assertEqual([b['id'] for b in briefs.read_all(self.where)], [good['id']],
                         '[N10.4] read_all must still return the briefs that do read')

    def test_read_all_takes_a_list_of_what_it_passed_over(self):
        # [N10.4] read_all(where, bad) appends what it passed over, so no caller has to change its call
        import inspect
        if 'bad' not in inspect.signature(briefs.read_all).parameters:
            self.fail('[N10.4] read_all cannot report what it passed over: it has no `bad` argument')
        b = a_brief(self.where)
        (self.where / b['id'] / 'brief.json').write_text('{ not json', encoding='utf-8')
        bad = []
        self.assertEqual(briefs.read_all(self.where, bad), [], '[N10.4] a brief that does not read is not a brief')
        self.assertEqual([x['id'] for x in bad], [b['id']], '[N10.4] read_all did not say what it passed over')


class Writes(unittest.TestCase):
    """[N10.4] brief.json is written whole or not at all: a temp file and a rename."""

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=TEMP))

    def test_failed_write_keeps_the_brief(self):
        # [N10.4] a write that fails half way must lose nothing: the open brief is still there
        b = a_brief(self.where)
        f = self.where / b['id'] / 'brief.json'
        before = f.read_text(encoding='utf-8')
        real = pathlib.Path.replace

        def no_rename(self, target):
            raise OSError('the disk is full')

        pathlib.Path.replace = no_rename
        try:
            try:
                briefs.answer(self.where, b['id'], 'A', said='Build it', by='test')
                raised = False
            except OSError:
                raised = True
        finally:
            pathlib.Path.replace = real
        self.assertTrue(raised, '[N10.4] brief.json is written in place: a write cut half way loses the brief')
        self.assertEqual(f.read_text(encoding='utf-8'), before,
                         '[N10.4] the open brief is gone after a write that failed')
        self.assertEqual(json.loads(before)['state'], 'open', '[N10.4] the brief read for was not the open one')

    def test_site_tells_the_page_what_did_not_read(self):
        # [N10.4] site() writes window.BRIEFS_BAD, so the Decide page can say a brief could not be read
        a_brief(self.where, 'The one that reads')
        cut = a_brief(self.where, 'The one cut in half')
        (self.where / cut['id'] / 'brief.json').write_text('{ not json', encoding='utf-8')
        out = Path(tempfile.mkdtemp(prefix='out-', dir=TEMP))
        briefs.site(self.where, out)
        text = (out / 'data' / 'briefs.js').read_text(encoding='utf-8')
        self.assertIn('window.BRIEFS_BAD', text, '[N10.4] the page is not told that a brief could not be read')
        self.assertIn(cut['id'], text.split('window.BRIEFS_BAD', 1)[1],
                      '[N10.4] BRIEFS_BAD does not name the brief that did not read')


if __name__ == '__main__':
    unittest.main()
