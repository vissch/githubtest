#!/usr/bin/env python3
"""Tests of a brief.json whose shape is wrong (Tools/assetboard/briefs.py), the finding N10.5 of the second code
review. Run from trench-warfare-3d/:
python Tools/assetboard/test_briefshape.py

A brief.json that reads as JSON but lacks title, options or evidence used to raise KeyError deep in answers() or
site(), which takes the whole Decide page down: every decision of his goes stale over one bad folder. Such a brief
must be passed over and named on the page instead.
"""
import json
import os
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-briefshape-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

import sys
sys.path.insert(0, str(HERE))
import briefs      # noqa: E402  (after TW_BRIEFS: it must never see the live folder)
import notes       # noqa: E402  (after TW_NOTES, for the same reason)

# a brief.json that is valid JSON with the right id, each missing one key the readers index
MALFORMED = {
    'no-title': dict(options=[dict(key='A', text='Build it')], evidence=[], state='open'),
    'no-options': dict(title='No options at all', evidence=[], state='open'),
    'no-evidence': dict(title='No evidence at all', options=[dict(key='A', text='Build it')], state='open'),
}


class BriefShape(unittest.TestCase):

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=str(TEMP)))
        self.out = Path(tempfile.mkdtemp(prefix='site-', dir=str(TEMP)))
        (self.out / 'data').mkdir(parents=True, exist_ok=True)
        self.good = briefs.add(self.where, 'The gas ability', 'It needs a yes before it is built.',
                               ['Build it', 'Leave it'], 'It is cheap and it reads at every zoom.', by='test',
                               no_evidence='nothing is built yet: there is nothing to show')
        for bid, body in MALFORMED.items():
            home = self.where / bid
            home.mkdir(parents=True, exist_ok=True)
            b = dict(body, id=bid)
            (home / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')

    def test_read_all_passes_over_a_brief_of_the_wrong_shape(self):
        """[N10.5] a brief.json with no title, no options or no evidence is passed over and named, not indexed"""
        bad = []
        got = briefs.read_all(self.where, bad)
        self.assertEqual([b['id'] for b in got], [self.good['id']],
                         '[N10.5] read_all handed on a brief of the wrong shape: %s' % [b.get('id') for b in got])
        self.assertEqual(sorted(x['id'] for x in bad), sorted(MALFORMED),
                         '[N10.5] read_all did not name the briefs it passed over: %s' % bad)
        for x in bad:
            self.assertTrue(x.get('why'), '[N10.5] the brief %s was passed over with no why' % x['id'])

    def test_answers_and_site_come_back_with_a_brief_of_the_wrong_shape_there(self):
        """[N10.5] the Decide page's two reads return instead of raising KeyError over one bad folder"""
        bad = []
        got = briefs.read_all(self.where, bad)
        mine = notes.read_all(Path(os.environ['TW_NOTES']))
        try:
            answers = briefs.answers(got, mine)
        except Exception as e:                                  # noqa: BLE001 - the old code raises KeyError here
            self.fail('[N10.5] answers() raised over a brief of the wrong shape: %r' % e)
        self.assertEqual(answers, [], '[N10.5] nobody answered, yet answers() found one: %s' % answers)
        try:
            listed = briefs.site(self.where, self.out)
        except Exception as e:                                  # noqa: BLE001 - the old code raises KeyError here
            self.fail('[N10.5] site() raised over a brief of the wrong shape: %r' % e)
        self.assertEqual([b['id'] for b in listed], [self.good['id']],
                         '[N10.5] the page was given a brief of the wrong shape: %s' % [b.get('id') for b in listed])
        page = (self.out / 'data' / 'briefs.js').read_text(encoding='utf-8')
        self.assertIn('BRIEFS_BAD', page, '[N10.5] the page does not say a brief did not read')
        for bid in MALFORMED:
            self.assertIn(bid, page, '[N10.5] the page does not name the brief %s it passed over' % bid)


class PlainTitleAndPicture(unittest.TestCase):
    """The owner, 2026-11-01: titles of jargon and no pictures; "i need something visual to tell what im deciding on"."""

    def setUp(self):
        self.where = Path(tempfile.mkdtemp(prefix='briefs-', dir=str(TEMP)))
        self.png = Path(tempfile.mkdtemp(prefix='ev-', dir=str(TEMP))) / 'after.png'
        self.png.write_bytes(b'\x89PNG\r\n\x1a\n' + b'\0' * 32)
        self.txt = self.png.with_name('notes.txt')
        self.txt.write_text('numbers', encoding='utf-8')

    def brief(self, title='The sapper looks like a sapper', **kw):
        base = dict(evidence=[], no_evidence='', lane='', role='', files=())
        base.update(kw)
        return briefs.add(self.where, title, 'Whether the sapper reads as a sapper at play zoom.',
                          ['Keep it', 'Send it back'], 'It reads at every zoom.', base['evidence'], base['no_evidence'],
                          lane=base['lane'], by='test', role=base['role'], files=base['files'],
                          raised_by=kw.get('raised_by'))

    def test_a_visual_brief_without_a_picture_is_refused(self):
        for kw in (dict(role='character-simulator'), dict(files=['Assets/_Project/Art/Units/Sapper.png']),
                   dict(lane='lane/show/sapper-look')):
            with self.assertRaises(ValueError, msg=f'a visual brief ({kw}) with no picture was written') as e:
                self.brief(no_evidence='the editor was busy', **kw)
            self.assertIn('visual work', str(e.exception))
            with self.assertRaises(ValueError, msg=f'a visual brief ({kw}) with only a text file was written'):
                self.brief(evidence=[(str(self.txt), 'the numbers')], **kw)
        b = self.brief(role='character-simulator', evidence=[(str(self.png), 'The sapper after')])
        self.assertEqual(len(b['evidence']), 1)

    def test_a_title_of_ids_chains_or_paths_is_refused(self):
        for title in ('ability-fx-captures-r2-2-g1 needs you', 'Sapper look 3 g2 -g1-g2 chain',
                      'Flamer-look-r2: keep it?', 'docs/evidence/sapper/after.png', 'The sapper (lane/show/sapper-look)',
                      'Look every unit close 20261019', 'The ' + 'very ' * 20 + 'long title'):
            with self.assertRaises(ValueError, msg=f'the title "{title}" was let through') as e:
                self.brief(title, evidence=[(str(self.png), 'The sapper after')])
            self.assertIn('title', str(e.exception))
        with self.assertRaises(ValueError, msg='a title naming its own unit id was let through'):
            self.brief('Is sapper-look done', evidence=[(str(self.png), 'The sapper after')],
                       raised_by=dict(run='r1', unit='sapper-look', leg=2))

    def test_a_program_written_title_is_made_plain_not_refused(self):
        """the loop's park brief and found.py have nobody to answer a refusal: fit() makes their titles plain"""
        import briefrules
        for t in ('ability-fx-captures-r2-2-g1-g1-g1: chain of fix units 5 deep, 62 units',
                  'relay-proof-to-drive: tried 4 times, no progress',
                  'Tools/relay/relay.py leaks the board lock when docs/evidence/x/a.png is missing',
                  'look-look-every-unit-close-20261019: refused by the API safety filter 4 times',
                  'A ' + 'very ' * 30 + 'long finding'):
            f = briefrules.fit(t, ['relay-proof-to-drive'])
            self.assertEqual(briefrules.title_bad(f, ['relay-proof-to-drive']), [], f'{t!r} -> {f!r}')
            self.brief(f, evidence=[(str(self.png), 'The sapper after')])
        self.assertEqual(briefrules.fit('The gas ability'), 'The gas ability')
        self.assertTrue(briefrules.fit('ability-fx-captures-r2-2-g1: tried 4 times').startswith('Ability fx captures: tried 4'))

    def test_an_old_style_non_visual_brief_still_passes(self):
        b = briefs.add(self.where, 'The gas ability', 'It needs a yes before it is built.', ['Build it', 'Leave it'],
                       'It is cheap and it reads at every zoom.', by='test',
                       no_evidence='nothing is built yet: there is nothing to show', lane='lane/sim/gas-ability')
        self.assertEqual(b['title'], 'The gas ability')
        b = briefs.add(self.where, "The house's look: near-black where nobody works", 'What the house is drawn like.',
                       ['Near-black where nobody works', 'Every room light'], "It is the site's own look", by='test',
                       no_evidence='a decision of words')
        self.assertTrue(b['id'])


if __name__ == '__main__':
    unittest.main()
