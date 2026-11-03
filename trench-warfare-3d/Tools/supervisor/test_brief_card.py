#!/usr/bin/env python3
"""Tests of brief_card.py: a brief from a unit, in plain words, with pictures. Run: python Tools/supervisor/test_brief_card.py"""
import os
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
TEMP = Path(tempfile.mkdtemp(prefix='tw-briefcard-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):        # never the live folders
    (TEMP / _k.lower()).mkdir(exist_ok=True)
    os.environ[_k] = str(TEMP / _k.lower())
os.environ['TW_SUP_HOME'] = str(TEMP / 'sup')                   # the switch briefrules: never the machine's own
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import brief_card  # noqa: E402
import briefrules  # noqa: E402
import briefs  # noqa: E402

REPORT = """Done-when check exits 0.

RESULT: done - ability-fx-captures-r2-2-g1 lane committed, gated green, and pushed.
CHANGED:
- `FlipbookFx` GasBank row and the night tint; the shader no longer erases the bank.
- Area 0.13% -> 3.25% of frame (bar 3%).
NEXT: none - leg closed.
"""


def png(path, colour):
    try:
        from PIL import Image
        Image.new('RGB', (64, 40), colour).save(path)
    except ImportError:
        path.write_bytes(b'\x89PNG\r\n\x1a\n' + b'\0' * 32)
    return str(path)


class Card(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp(dir=str(TEMP)))
        self.unit = dict(id='ability-fx-captures-r2-2-g1', lane='lane/show/ability-fx-captures-r2-2-g1',
                         role='destruction-vfx-simulator', goal='Make the gas ability read at night from play zoom',
                         report=REPORT, score=22, fixes=['Give the gas a bright rim', 'Show it at night', 'Cut the noise'],
                         before=[png(self.dir / 'before-night.png', 'black')],
                         after=[png(self.dir / 'after-night.png', 'green'), png(self.dir / 'after-day.png', 'white')],
                         run='20261020-190432-80600', leg=8)

    def test_a_held_unit_reads_in_plain_words(self):
        c = brief_card.card(self.unit, 'held', work=self.dir / 'work')
        self.assertEqual(briefrules.title_bad(c['title'], [self.unit['id'], self.unit['lane']]), [], c['title'])
        self.assertTrue(c['title'].startswith('Ability fx captures'), c['title'])
        self.assertIn('22', c['title'])
        self.assertIn('Make the gas ability read at night', c['what_for'])
        self.assertIn('22/100', c['what_for'])
        self.assertEqual(c['done'][0][:4], 'done')
        self.assertTrue(any('GasBank' in d for d in c['done']))
        self.assertEqual(c['fixes'], self.unit['fixes'])
        self.assertEqual(len(c['options']), 3)
        self.assertIn('fix', c['options'][0])
        self.assertEqual(len(c['evidence']), 2, 'one before/after pair and one after alone')
        self.assertTrue(c['evidence'][0][1].startswith(('Before (left) and after (right)', 'Before:')))
        self.assertIn('after', c['evidence'][-1][1].lower())
        self.assertIn('What was done', brief_card.text(c))

    def test_the_card_is_a_brief_the_shape_rules_take(self):
        c = brief_card.card(self.unit, 'held', work=self.dir / 'work')
        b = brief_card.post(Path(os.environ['TW_BRIEFS']), c, by='test')
        self.assertEqual(b['card']['score'], 22)
        self.assertEqual(b['raised_by']['unit'], self.unit['id'])
        self.assertTrue(b['evidence'])
        self.assertEqual([x['id'] for x in briefs.read_all(Path(os.environ['TW_BRIEFS'])) if x['id'] == b['id']], [b['id']])

    def test_a_card_with_no_picture_is_refused_for_visual_work(self):
        sup = Path(os.environ['TW_SUP_HOME'])
        sup.mkdir(exist_ok=True)
        (sup / 'modules.json').write_text('{"briefrules": "on"}', encoding='utf-8')    # the rule refuses only when on
        self.addCleanup((sup / 'modules.json').unlink)
        c = brief_card.card(dict(self.unit, before=[], after=[]), 'held')
        with self.assertRaises(ValueError) as e:
            brief_card.post(Path(os.environ['TW_BRIEFS']), c)
        self.assertIn('visual work', str(e.exception))

    def test_landing_and_family_kinds(self):
        c = brief_card.card(dict(self.unit, score=78), 'landing')
        self.assertTrue(c['title'].endswith('ready to land'))
        self.assertEqual(len(c['options']), 2)
        c = brief_card.card(dict(self.unit, score=None, id='sapper-look-3-g2-g1-g2'), 'family')
        self.assertEqual(briefrules.title_bad(c['title']), [], c['title'])
        self.assertTrue(c['title'].startswith('Sapper look'), c['title'])
        self.assertIn('could not score', c['what_for'])
        with self.assertRaises(ValueError):
            brief_card.card(self.unit, 'other')

    def test_a_long_name_is_cut_to_the_title_limit(self):
        c = brief_card.card(dict(self.unit, id='the-very-long-name-of-a-unit-that-goes-on-and-on-and-on-forever-more-r2'), 'held')
        self.assertLessEqual(len(c['title']), briefrules.TITLE_MAX)
        self.assertEqual(briefrules.title_bad(c['title']), [], c['title'])


if __name__ == '__main__':
    unittest.main()
