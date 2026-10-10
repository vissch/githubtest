#!/usr/bin/env python3
"""Tests of the day's card (daycard.py). Run from trench-warfare-3d/: python Tools/assetboard/test_daycard.py

The figures are those of 2026-10-10 in small: the day the card was written for."""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import daycard    # noqa: E402

results = []
DAY = '2026-10-10'


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


def leg(hour, source, role, usd=None, kind=None, day=DAY):
    rec = dict(started_at=f'{day}T{hour:02d}:00:00Z', source=source, role=role)
    rec.update({k: v for k, v in (('cost_usd', usd), ('kind', kind)) if v is not None})
    return rec


def main():
    legs = [leg(8, 'pipeline', 'character-simulator', 6.0), leg(9, 'lane', 'review-fix', 2.0), leg(10, 'lane', 'lane', kind='critique'), leg(11, 'retro', 'retro', 9.0),
            leg(8, 'pipeline', 'x', 50.0, day='2026-10-08'), leg(12, 'lane', 'destruction-vfx-simulator', 1.0, kind='fix')]
    usd, n, guessed = daycard.spend(legs, DAY)
    case('tokens: the day\'s legs are added up by the kind of work each was paid as (its record, else its source and role); a retrospective is nobody\'s share, '
         'another day\'s legs are not counted, and a leg with no cost is counted at the usual one and said',
         usd == dict(finish=6.0, fix=3.0, critique=3.0) and n == 4 and guessed == 1, (usd, n, guessed))
    landed = [dict(when=f'{DAY} 23:18', lane='lane/show/landing-2026-10-09-narrows', head='f19b38ba11', on=dict(click='land-x-1', note='n')),
              dict(when=f'{DAY} 23:40', lane='lane/show/docs-a', head='aaaaaaaa11', on=dict(alone=True)), dict(when='2026-10-09 10:00', lane='lane/show/old', head='b', on=dict(alone=True))]
    taken = [dict(when=f'{DAY} 12:00', look='look-1', went='queue'), dict(when=f'{DAY} 12:00', look='look-1', went='card'), dict(when=f'{DAY} 15:00', look='look-2', went='dropped'),
             dict(when='2026-10-09 12:00', look='look-0', went='queue')]
    briefs = [dict(state='answered', answer=dict(when=f'{DAY} 23:20')), dict(state='answered', answer=dict(when='2026-10-09 09:00')), dict(state='open')]
    untaken = [dict(when=f'{DAY} 10:52:06', title='later'), dict(when=f'{DAY} 10:22:50', title='earlier')]
    text = '\n'.join(daycard.card(DAY, landed=landed, legs=legs, taken=taken, briefs=briefs, untaken=untaken, idle={'a Unity held the checkout': 19800, 'between two runs': 40}))
    case('the card says, in five lines: what landed and how (alone or on which click), the tokens against the split, what the looks found and what became of it, '
         'what was decided and what still waits since when, and how long the loop stood still and why',
         'Landed: 2** (1 by themselves, 1 on your click)' in text and 'landing-2026-10-09-narrows at `f19b38ba` (card land-x-1)' in text and 'docs-a at `aaaaaaaa`' in text
         and 'Tokens: 4 legs' in text and 'finish 50% (its share is 60%)' in text and 'critique 25% (its share is 15%)' in text and '1 legs had no cost' in text
         and 'Found: 3 tasks** from 2 looks' in text and '1 queued as fixes, 1 are cards for you, 1 dropped' in text
         and 'Decided: 1 of your answers were carried out**; 2 wait, the oldest since 10-10 10:22' in text and 'Stood still: 5.5 hours**: 5.5 h a Unity held the checkout' in text
         and len(text.splitlines()) == 5 and 'between two runs' not in text, text)
    empty = '\n'.join(daycard.card(DAY))
    case('a day on which nothing happened says so in every line, and does not fall over',
         'Landed: nothing' in empty and 'no leg ran' in empty and 'no look at old work' in empty and '0 of your answers' in empty and 'not at all' in empty, empty)
    off = '\n'.join(daycard.card(DAY, legs=[leg(8, 'pipeline', 'x', 9.0), leg(9, 'lane', 'review-fix', 1.0)]))
    case('a day that is more than fifteen points off the split says which kinds, and only those', off.splitlines()[1].endswith('Off the split by more than 15 points: finish'), off)
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
