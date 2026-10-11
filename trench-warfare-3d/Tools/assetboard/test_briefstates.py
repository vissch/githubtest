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
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
DECIDE = HERE / 'static' / 'decide.js'

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


# the page half of decide.js under node: `module` is undefined, so it runs on past its module.exports, and there is
# no #decide element, so draw() is never wired. Only what a card is made of is looked at.
HARNESS = r"""
const fs = require('fs');
const src = fs.readFileSync(process.argv[2], 'utf8');
const note = JSON.parse(process.argv[3]);
const brief = JSON.parse(process.argv[4]);

function node(tag, cls, text) {
  return { tag: tag, className: cls || '', textContent: text == null ? '' : String(text), children: [], dataset: {},
    appendChild: function (n) { this.children.push(n); return n; },
    addEventListener: function () {}, setAttribute: function () {}, contains: function () { return false; } };
}
function text(n) { return String(n.textContent || '') + (n.children || []).map(text).join(''); }

var root = { Crew: { el: node, live: function () {}, nowPill: function () {} },
  Board: { notes: function () { return [note]; }, note: function () {}, button: node, onchange: function () {} } };
var document = { getElementById: function () { return null; }, body: { dataset: {} },
  createTextNode: function (t) { return node('#text', '', t); } };

var f = new Function('root', 'module', 'window', 'document', src);
f(root, undefined, root, document);

var out = { has: typeof root.Briefs.closedNote === 'function' };
if (out.has) {
  out.said = root.Briefs.said(brief);
  out.closed = !!root.Briefs.closedNote(brief);
}
out.card = text(root.Briefs.card(brief));
console.log(JSON.stringify(out));
"""


class ThePage(unittest.TestCase):
    """[N10.1] the Decide page takes his newest note about a brief as his answer whatever the note's state, so a note
    closed any other way than by `briefs.py take` leaves an open brief that reads "You decided ... taken up" and that
    no session is told about."""

    def run_page(self, note, brief):
        out = Path(tempfile.mkdtemp(prefix='page-', dir=TEMP)) / 'run.js'
        out.write_text(HARNESS, encoding='utf-8')
        r = subprocess.run([self.node, str(out), str(DECIDE), json.dumps(note), json.dumps(brief)],
                           capture_output=True, text=True)
        if r.returncode != 0:
            self.fail('[N10.1] the page half of decide.js did not run: %s' % (r.stderr or r.stdout)[:600])
        return json.loads(r.stdout.strip().splitlines()[-1])

    def setUp(self):
        self.node = shutil.which('node')
        if not self.node:
            self.skipTest('node is not installed here')
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=TEMP))
        b = a_brief(self.where)
        self.brief = json.loads((self.where / b['id'] / 'brief.json').read_text(encoding='utf-8'))
        self.brief.setdefault('evidence', [])
        self.note = {'id': 'n-1', 'about': 'brief:' + b['id'], 'when': '2026-10-09 11:20',
                     'from': 'owner', 'text': 'A: Build it', 'state': 'done'}

    def test_a_done_note_is_no_answer_on_an_open_brief(self):
        # [N10.1] his note was answered without the brief being closed: the brief is still to decide
        self.assertEqual(self.brief['state'], 'open', 'the brief read for was not the open one')
        got = self.run_page(self.note, self.brief)
        if not got['has']:
            self.fail('[N10.1] the page cannot tell a closed note from an answer: Briefs.closedNote is missing')
        self.assertIsNone(got['said'], '[N10.1] a done note on an open brief is taken as his answer: %s' % (got['said'],))
        self.assertTrue(got['closed'], '[N10.1] the note answered without closing the brief is not found')
        self.assertIn('without closing', got['card'],
                      '[N10.1] the card does not say his answer was closed without a session being told: %s' % got['card'][:300])
        self.assertNotIn('You decided', got['card'],
                         '[N10.1] the card says "You decided" for a brief nobody was told about: %s' % got['card'][:300])
        self.assertNotIn('taken up', got['card'],
                         '[N10.1] the card says the brief was taken up, and no session was told: %s' % got['card'][:300])

    def test_an_open_note_is_still_his_answer(self):
        # [N10.1] the fix must not drop a live note: an open note is his answer as before
        live = dict(self.note, state='', id='n-2')
        got = self.run_page(live, self.brief)
        self.assertEqual((got.get('said') or {}).get('id'), 'n-2',
                         '[N10.1] an open note about the brief is no longer taken as his answer')
        self.assertIn('You decided', got['card'], '[N10.1] the card no longer says what he decided')


if __name__ == '__main__':
    unittest.main()
