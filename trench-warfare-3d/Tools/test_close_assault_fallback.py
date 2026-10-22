#!/usr/bin/env python3
"""The close-assault fallback stays deleted, as the owner answered on 2026-10-22.

    python Tools/test_close_assault_fallback.py

The 2026-10-18 question asked whether to delete DirectFire's ownHits path or make it
reachable. The owner said land the fix as built: the attack is gone, and every such
hit goes through the armour model. That answer belongs in How units move and fight,
not under Open. The code on integration already throws when TankGunnerySystem is
missing; this test keeps that, and refuses the open question being left waiting.
"""
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[2]
DECISIONS = ROOT / 'docs' / 'reference' / 'decisions.md'
DIRECT_FIRE = ROOT / 'trench-warfare-3d' / 'Assets' / '_Project' / 'Sim' / 'Combat' / 'DirectFire.cs'
COMBAT_TESTS = ROOT / 'trench-warfare-3d' / 'Assets' / '_Project' / 'Tests' / 'Sim' / 'CombatTests.cs'

OPEN_HEAD = '## Open: waiting on the owner'
FIGHT_HEAD = '## How units move and fight'
OPEN_TITLE = 'The close-assault fallback in `DirectFire.cs` is dead code'
OWNER = 'Land the fix as built: the attack is gone, every such hit now goes through the armor model'
THROW = 'DirectFireSystem needs TankGunnerySystem registered'


def split_decisions(text):
    head, sep, open_part = text.partition(OPEN_HEAD)
    if not sep:
        raise AssertionError('decisions.md has no open section')
    if FIGHT_HEAD not in head:
        raise AssertionError('decisions.md has no How units move and fight section')
    fight = head.split(FIGHT_HEAD, 1)[1]
    return fight, open_part


class CloseAssaultFallback(unittest.TestCase):
    def test_owner_answer_is_moved_up(self):
        fight, open_part = split_decisions(DECISIONS.read_text(encoding='utf-8'))
        self.assertNotIn(OPEN_TITLE, open_part, 'the close-assault question is still open')
        self.assertNotIn('Option (b) stays open', open_part)
        row = next((line for line in fight.splitlines()
                    if line.startswith('| 2026-10-22 |') and 'close-assault fallback' in line), '')
        self.assertTrue(row, 'How units move and fight has no 2026-10-22 close-assault row')
        self.assertIn(OWNER, row)
        self.assertIn('option (a)', row)

    def test_step_still_refuses_a_world_with_no_armour_model(self):
        src = DIRECT_FIRE.read_text(encoding='utf-8')
        self.assertNotIn('ownHits', src)
        self.assertIn(THROW, src)
        self.assertIn('gunnery.PendingHits', src)
        tests = COMBAT_TESTS.read_text(encoding='utf-8')
        self.assertIn('DirectFireWithoutTankGunneryRefusesToStep', tests)


if __name__ == '__main__':
    unittest.main(verbosity=2)
