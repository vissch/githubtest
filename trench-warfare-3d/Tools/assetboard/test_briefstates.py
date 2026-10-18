#!/usr/bin/env python3
"""Tests of the states a decision brief passes through (Tools/assetboard/briefs.py), the findings N10.1, N10.3,
N10.4 and N10.13 of the second code review. Run from trench-warfare-3d/:
python Tools/assetboard/test_briefstates.py

His answer on the Decide page is the decision, so a brief that is shown as decided when nobody was told, closed as
queued when nothing was queued, or lost to a write cut half way, is his word dropped.
"""
import datetime
import inspect
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
import notes       # noqa: E402  (after TW_NOTES, for the same reason)


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


class Taking(unittest.TestCase):
    """[N10.3] take closed a brief as "Queued as <unit>" on say-so, with nothing looking at the relay's queue, and
    [N10.13] it closed the brief before it answered his notes, so a failure between the two left them open for good."""

    T0 = datetime.datetime(2026, 10, 11, 12, 0)     # a fixed clock: before and after the close is not real time

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=TEMP))
        self.box = Path(tempfile.mkdtemp(prefix='notes-', dir=TEMP))
        self.board = Path(tempfile.mkdtemp(prefix='board-', dir=TEMP))
        (self.board / 'relay' / 'queue').mkdir(parents=True)
        (self.board / 'relay' / 'done').mkdir(parents=True)
        self.git('init', '-q')
        self.unit = dict(id='unit-cover', lane='lane/sim/cover', goal='Men behind a building take less damage.',
                         done_when=['python', 'Tools/otr.py', 'CoverTests'])
        self.brief = a_brief(self.where, 'Do houses give cover')
        briefs.then(self.where, notes.read_all(self.box), self.brief['id'], 'A',
                    'Queues a sim lane: men behind a building take less damage', self.unit)
        self.note = self.click('A')

    def git(self, *args):
        subprocess.run(['git', '-C', str(self.board), *args], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def write_unit(self, unit, folder='queue', name=None):
        """A unit file in the board's working tree, and nothing else: nobody committed or pushed it."""
        f = self.board / 'relay' / folder / ((name or unit['id']) + '.json')
        f.write_text(json.dumps(unit), encoding='utf-8')
        return f

    def queue(self, unit, folder='queue', name=None):
        """The unit as the relay sees it: written, committed on the board and pushed (origin/main here is a ref of
        this repo, so the test needs no remote)."""
        f = self.write_unit(unit, folder, name)
        self.git('add', '-A')
        self.git('-c', 'user.email=t@t', '-c', 'user.name=t', 'commit', '-q', '-m', 'queue ' + f.name)
        self.git('update-ref', 'refs/remotes/origin/main', 'HEAD')
        return f

    def click(self, key, now=None, text=None):
        """What the page leaves on a click: the option's text, and the stamp the Then line has now."""
        b = [x for x in briefs.read_all(self.where) if x['id'] == self.brief['id']][0]
        o = [x for x in b['options'] if x['key'] == key][0]
        return notes.write(self.box, text or '%s: %s' % (key, o['text']), kind='page', about='brief:' + b['id'],
                           then=(o.get('then') or {}).get('stamp', ''), now=now or self.T0)

    def state(self):
        return [x for x in briefs.read_all(self.where) if x['id'] == self.brief['id']][0]

    def take(self, **kw):
        """take, with its refusal as a string. Named so a run on the old code says the id, not a TypeError."""
        for need in ('board', 'unchecked'):
            if need not in inspect.signature(briefs.take).parameters:
                self.fail('[N10.3] take has no %s: it closes a brief as queued without looking at the board\'s '
                          'relay queue' % need)
        try:
            kw.setdefault('now', self.T0)
            return briefs.take(self.where, self.box, self.brief['id'], by='master', note=self.note['id'], **kw), ''
        except ValueError as e:
            return None, str(e)

    def test_a_unit_that_is_not_on_the_queue_is_no_queued_answer(self):
        # [N10.3] his yes is not taken up as "Queued as unit-cover" while the relay's queue is empty
        got, why = self.take(board=self.board)
        self.assertIsNone(got, '[N10.3] the brief closed as queued with nothing on the relay\'s queue')
        self.assertIn('unit-cover', why, '[N10.3] the refusal does not name the unit nobody queued: %s' % why)
        self.assertIn('--out', why, '[N10.3] the refusal does not name the command that queues it: %s' % why)
        self.assertIn('--unchecked', why, '[N10.3] the refusal does not name --unchecked: %s' % why)
        self.assertEqual(self.state().get('state'), 'open',
                         '[N10.3] the brief is closed although nothing was queued')
        self.assertNotEqual(notes.read_all(self.box)[0].get('state'), 'done',
                            '[N10.3] his note is answered although nothing was queued')

    def test_no_board_at_all_is_the_same_refusal(self):
        # [N10.3] a board folder that is not there is no proof either
        got, why = self.take(board=self.board / 'not-there')
        self.assertIsNone(got, '[N10.3] the brief closed as queued with no board to read')
        self.assertEqual(self.state().get('state'), 'open', '[N10.3] the brief is closed with no board to read')

    def test_the_unit_on_the_queue_closes_the_brief(self):
        # [N10.3] the fix must not refuse the real case: the unit stands on the relay's queue, so it closes
        self.queue(self.unit)
        got, why = self.take(board=self.board)
        self.assertEqual(why, '', '[N10.3] a unit standing on the relay\'s queue was refused: %s' % why)
        self.assertEqual((self.state().get('answer') or {}).get('queued'), 'unit-cover',
                         '[N10.3] the brief did not close with the unit that was queued')
        self.assertEqual([n.get('state') for n in notes.read_all(self.box)], ['done'],
                         '[N10.3] his note about the closed brief is left open')

    def test_a_unit_in_the_done_folder_counts_too(self):
        # [N10.3] a unit the runner has finished is queued work all the same
        self.queue(dict(id='unit-cover'), folder='done')
        got, why = self.take(board=self.board)
        self.assertEqual(why, '', '[N10.3] a unit in the relay\'s done folder was refused: %s' % why)
        # [N10.3b] a done record of that id may be an earlier unit's, so the brief and his note must say it was there
        a = self.state().get('answer') or {}
        self.assertTrue((a.get('done_already') or '').strip(),
                        '[N10.3b] the answer does not say the relay had a done record of that id already: %s' % a)
        text = notes.read_all(self.box)[0]['answers'][-1]['text']
        self.assertIn('done record', text,
                      '[N10.3b] what his note is answered with does not say the done record was there already: %s' % text)

    def test_a_unit_written_and_not_pushed_is_no_queued_answer(self):
        # [N10.3b] the relay picks units off the board's origin/main: a local, uncommitted file is nothing to it
        self.write_unit(self.unit)
        got, why = self.take(board=self.board)
        self.assertIsNone(got, '[N10.3b] the brief closed as queued on a unit file nobody committed or pushed')
        self.assertIn('push', why, '[N10.3b] the refusal does not say the board must be committed and pushed: %s' % why)
        self.assertEqual(self.state().get('state'), 'open',
                         '[N10.3b] the brief is closed on a unit file nobody pushed')

    def test_a_file_whose_name_only_looks_right_is_not_the_unit(self):
        # [N10.3b] the id inside the file decides, not the file name
        self.queue(dict(self.unit, id='unit-cover-2'), name='unit-cover')
        got, why = self.take(board=self.board)
        self.assertIsNone(got, '[N10.3b] a queue file named unit-cover.json whose id is unit-cover-2 passed as unit-cover')
        self.assertEqual(self.state().get('state'), 'open',
                         '[N10.3b] the brief closed on a queue file that holds another unit')
        got, why = self.take(board=self.board, queued='unit-cover-2')
        self.assertEqual(why, '', '[N10.3b] the unit queued as unit-cover-2 was not found by its own id: %s' % why)
        self.assertEqual((self.state().get('answer') or {}).get('queued'), 'unit-cover-2',
                         '[N10.3b] the brief did not close with the unit that was queued')

    def test_unchecked_closes_it_and_says_so(self):
        # [N10.3] the board cannot always be read; then the answer must say that nobody looked
        got, why = self.take(board=None, unchecked='G: is not mounted on this station')
        self.assertEqual(why, '', '[N10.3] --unchecked did not close the brief: %s' % why)
        a = (self.state().get('answer') or {})
        self.assertEqual(a.get('unchecked'), 'G: is not mounted on this station',
                         '[N10.3] the answer does not say the board was not read: %s' % a)
        text = notes.read_all(self.box)[0]['answers'][-1]['text']
        self.assertIn('not mounted', text,
                      '[N10.3] what his note is answered with does not say the board was not read: %s' % text)

    def test_take_finishes_what_an_earlier_take_left(self):
        # [N10.13] a crash between closing the brief and answering his notes left them open for ever
        briefs.answer(self.where, self.brief['id'], 'A', said='', by='master', now=self.T0, queued='unit-cover')
        self.assertNotEqual(notes.read_all(self.box)[0].get('state'), 'done',
                            'the note read for was answered already')
        kw = dict(board=self.board) if 'board' in inspect.signature(briefs.take).parameters else {}
        try:
            got = briefs.take(self.where, self.box, self.brief['id'], by='master', note=self.note['id'], now=self.T0, **kw)
        except Exception as e:
            self.fail('[N10.13] take cannot finish what an earlier take left: %s: %s' % (type(e).__name__, e))
        got_b, got_n = got
        self.assertEqual(got_b['id'], self.brief['id'], '[N10.13] take gave back another brief')
        self.assertEqual([n.get('state') for n in notes.read_all(self.box)], ['done'],
                         '[N10.13] his note is left open for good after a take that stopped half way')
        self.assertIn('unit-cover', notes.read_all(self.box)[0]['answers'][-1]['text'],
                      '[N10.13] the note is not answered with what became of the brief')

    def test_a_word_of_his_after_the_close_is_not_answered_with_the_old_outcome(self):
        # [N10.13b] left_over answered EVERY open note about a closed brief with what became of it, --note or not:
        # "No, leave it, I changed my mind", written after the close, was answered "Taken up: the brief is closed
        # with A" and closed unread. A later word of his is a decision of its own.
        self.queue(self.unit)
        got, why = self.take(board=self.board)
        self.assertEqual(why, '', '[N10.13b] the first take was refused: %s' % why)
        later = self.click('A', now=self.T0 + datetime.timedelta(minutes=5),
                           text='No, leave it, I changed my mind')
        got, why = self.take(board=self.board, now=self.T0 + datetime.timedelta(minutes=6))
        self.assertIsNone(got, '[N10.13b] a note he wrote after the close was taken up as part of that answer')
        self.assertIn(later['id'], why, '[N10.13b] the refusal does not name his later note: %s' % why)
        mine = [n for n in notes.read_all(self.box) if n['id'] == later['id']][0]
        self.assertEqual(mine.get('state'), 'open',
                         '[N10.13b] his note written after the close is closed: %s' % (mine.get('answers') or []))
        self.assertEqual(mine.get('answers'), [],
                         '[N10.13b] his note written after the close is answered with the old outcome: %s' % mine['answers'])
        pairs = briefs.waiting(briefs.read_all(self.where), notes.read_all(self.box))
        self.assertEqual([n['id'] for _, n in pairs], [later['id']],
                         '[N10.13b] waiting does not list the word he left after the brief was closed')

    def test_a_left_over_take_does_not_swallow_the_unit_it_is_given(self):
        # [found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] a left-over take given a different outcome
        # must refuse it or record it, never swallow it and answer his note with the old outcome
        briefs.answer(self.where, self.brief['id'], 'A', said='', by='master', now=self.T0,
                      outcome='Nothing was queued: he wants it by hand')
        self.queue(self.unit)
        got, why = self.take(board=self.board, queued='unit-cover')
        a = self.state().get('answer') or {}
        if got is None:
            self.assertIn('unit-cover', why,
                          '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] the refusal does not '
                          'name the unit it was given: %s' % why)
            self.assertEqual(a.get('queued', ''), '',
                             '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] the unit it was given '
                             'was written although the take was refused: %s' % a)
            self.assertEqual(a.get('outcome'), 'Nothing was queued: he wants it by hand',
                             '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] the old outcome on '
                             'record was overwritten: %s' % a)
            self.assertNotEqual(notes.read_all(self.box)[0].get('state'), 'done',
                                '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] his note was '
                                'answered although the take naming a new unit was refused')
        else:
            self.assertEqual(a.get('queued'), 'unit-cover',
                             '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] the unit it was given '
                             'was not recorded: %s' % a)

    def test_a_left_over_take_with_the_same_record_finishes_his_notes(self):
        # [found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] a legitimate rerun naming what is already
        # on record must still finish answering his notes
        briefs.answer(self.where, self.brief['id'], 'A', said='', by='master', now=self.T0, queued='unit-cover')
        self.queue(self.unit)
        got, why = self.take(board=self.board, queued='unit-cover')
        self.assertEqual(why, '',
                         '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] a left-over take naming '
                         'the same unit as the record was refused: %s' % why)
        self.assertEqual([n.get('state') for n in notes.read_all(self.box)], ['done'],
                         '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] his note is left open '
                         'although the take named the same unit already on record')
        text = notes.read_all(self.box)[0]['answers'][-1]['text']
        self.assertIn('unit-cover', text,
                      '[found-take-a-left-over-brief-must-not-swallow-queu-20261016-0] his note is not answered '
                      'with the unit on record: %s' % text)

    def test_a_closed_brief_with_no_open_note_is_still_refused(self):
        # [N10.13] finishing a left-over take is not a second close
        self.queue(self.unit)
        self.take(board=self.board)
        got, why = self.take(board=self.board)
        self.assertIsNone(got, '[N10.13] a brief that is closed with his notes answered was taken a second time')
        self.assertTrue(why, '[N10.13] a second take says nothing')


class GateBriefWrite(unittest.TestCase):
    """[N10.4b] A gate brief's brief.json was written in place by idearoute.gates: a write cut half way left a brief
    the page, waiting() and steps() drop without a word. It must go through briefs.put, like every other write."""

    def setUp(self):
        sys.path.insert(0, str(HERE.parent / 'pipeline'))
        global idearoute, ideas, pipeline
        import idearoute, ideas, pipeline       # noqa: E401,E402
        self.tmp = Path(tempfile.mkdtemp(prefix='tw-gatewrite-'))
        self.where, self.bwhere = self.tmp / 'ideas', self.tmp / 'briefs'
        self.nwhere, self.board = self.tmp / 'notes', self.tmp / 'board'
        (self.board / 'items').mkdir(parents=True)
        self.day = datetime.datetime(2026, 10, 7, 12, 0, 0)
        pic = self.tmp / 'p.png'
        pic.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
        self.pic = pic
        P = [dict(path=str(pic), caption='What it is', kind='capture')]
        i = ideas.add(self.where, 'A whistle before the assault', 'A thing on the field that does a thing.',
                      'It serves a goal.', 'mechanic', P, 'M', 'R1 C1', entries=[], now=self.day)
        ideas.answer(self.where, i['id'], 'accepted', now=self.day)
        idearoute.route(self.where, self.board)
        self.iid = 'idea-a-whistle-before-the-assault'
        self.B = pipeline.Board(self.board)
        for stage in ('game-design', 'concept'):
            self.passes(stage)

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def passes(self, stage):
        info = pipeline.evaluate(self.B.items()[self.iid], self.B)[stage]
        f = self.board / 'evidence' / self.iid / stage / 'shown.png'
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_bytes(self.pic.read_bytes())
        pipeline.write_json(self.board / 'results' / f'{info["job"]}--1.json',
                            dict(job=info['job'], item=self.iid, stage=stage, attempt=1, verdict='PASS', station='desktop',
                                 token='t', consumed=info['consumed'], upstream_rev=info['upstream_rev'],
                                 evidence=dict(shown=f.relative_to(self.board).as_posix()), note='made', feedback=[],
                                 finished_at=pipeline.now()))

    def test_a_gate_brief_is_written_whole_or_not_at_all(self):
        # [N10.4b] a write straight onto brief.json is the half-written brief N10.4 took out of briefs.put
        real = pathlib.Path.write_text

        def held(self_, *a, **k):
            if self_.name == 'brief.json':
                raise OSError(5, 'another program holds brief.json')
            return real(self_, *a, **k)

        pathlib.Path.write_text = held
        try:
            got = idearoute.gates(self.where, self.bwhere, self.nwhere, self.board, now=self.day)
        finally:
            pathlib.Path.write_text = real
        # [U055.2] the id carries the revision of the steps before it: ask idearoute for it, as gates() does
        bid = idearoute.gate_id(self.iid, 'you-pick', pipeline.evaluate(self.B.items()[self.iid], self.B)['you-pick'])
        self.assertIn(bid, got['asked'], '[N10.4b] the gate brief was not asked at all: %s' % (got,))
        b = briefs.find(self.bwhere, bid)
        self.assertEqual(b['options'][0].get('then', {}).get('says'), idearoute.GATE_THEN,
                         '[N10.4b] the gate brief on disk has no Then line: it was not written through briefs.put')
        self.assertEqual(b.get('step', {}).get('stage'), 'you-pick',
                         '[N10.4b] the gate brief on disk does not say which step it is: %s' % b.get('step'))

if __name__ == '__main__':
    unittest.main()
