#!/usr/bin/env python3
"""Tests of modules/families.py, the goal family rule (spend.goal_root/family_of) and its park hook (spend.capped),
against the replay kit of 2026-10-19..11-02. Run: python Tools/supervisor/test_families.py

The proof: ability-fx-captures stops at about $100 instead of $990, and every other family that would have hit the
owner's cap ($100 or 3 fix generations) is named. It is also run against the rule that was live (loopsteps: a chain
of more than CHAIN_DEPTH -gN links, counted per chain_family) and must fail there.
"""
import importlib.util
import os
import re
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import core  # noqa: E402
import spend  # noqa: E402
from replay import records  # noqa: E402

spec = importlib.util.spec_from_file_location('tw_sup_test_families', HERE / 'modules' / 'families.py')
families = importlib.util.module_from_spec(spec)
spec.loader.exec_module(families)

RS = records.load()
LEGS = sorted(RS.legs, key=lambda l: l.get('started_at') or '')
FX = 'ability-fx-captures'


def old_rule(legs, chain_depth=4):
    """Stub of the live rule (loopsteps.depth/chain_family, CHAIN_DEPTH 4): {chain family: (usd, usd after its stop)}."""
    out = {}
    for l in legs:
        u = l.get('unit') or ''
        fam = re.split(r'-g\d+', u)[0]
        usd, after, stop = out.get(fam, (0.0, 0.0, False))
        c = float(l.get('cost_usd') or 0)
        stop = stop or len(re.findall(r'-g\d+(?=-g\d+|$)', u)) > chain_depth
        out[fam] = (usd + c, after + (c if stop else 0.0), stop)
    return out


class Rule(unittest.TestCase):
    def test_the_real_ability_fx_ids_are_one_family(self):
        ids = sorted({l['unit'] for l in LEGS if l.get('unit', '').startswith('ability-fx-captures')})
        roots = {spend.goal_root(u) for l in LEGS for u in [l.get('unit') or ''] if u}
        fams = {u: spend.family_of(u, roots) for u in ids}
        self.assertEqual(set(fams.values()), {FX}, {u: f for u, f in fams.items() if f != FX})
        self.assertEqual(spend.goal_root('ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2-g2'), FX)
        self.assertEqual(spend.goal_root('ability-fx-captures-r3-2-2'), FX)

    def test_other_goals_stay_apart(self):
        roots = {spend.goal_root(l['unit']) for l in LEGS if l.get('unit')}
        self.assertNotEqual(spend.family_of('ability-fx-remove-fire-smoke-2', roots), FX)
        self.assertEqual(spend.family_of('atrifle-look-r3', roots), spend.family_of('atrifle-look-r2', roots))
        self.assertNotEqual(spend.family_of('sapper-lay-mine-anim', roots), spend.family_of('sapper-look-3', roots))
        self.assertEqual(spend.goal_root('fix-job-ordering-tests'), 'fix-job-ordering-tests')
        self.assertFalse(spend.joins('look-x', 'look'), 'a one-word root joins nothing: this must still fail')
        self.assertNotEqual(spend.family_of('look-review-z004', roots), spend.family_of('look-review-z005', roots),
                            'two looks reviewed are two goals')
        merged = {}
        for u in {l['unit'] for l in LEGS if l.get('unit')}:
            merged.setdefault(spend.family_of(u, roots), set()).add(spend.goal_root(u))
        many = {f: sorted(r) for f, r in merged.items() if len(r) > 1}
        print('  families that join more than one goal root:')
        for f, r in sorted(many.items()):
            print(f'    {f}: {", ".join(x for x in r if x != f)}')
        for f, r in many.items():
            self.assertTrue(all(x == f or x.startswith(f + '-') for x in r), (f, r))


class Replay(unittest.TestCase):
    def test_ability_fx_stops_near_100_dollars(self):
        fams = families.decide(LEGS)
        f = fams[FX]
        print(f'  {FX}: ${f["usd"]:.0f} in {len(f["units"])} units, {f["depth"]} generations; over at {f["over_at"]} '
              f'({f["over"]}); ${f["usd_after"]:.0f} in {f["legs_after"]} legs followed it')
        self.assertTrue(f['usd'] > 900 and f['over'], f)
        self.assertLessEqual(f['over_at']['usd'], 125, 'it must stop at about $100')
        self.assertGreater(f['usd_after'], 750)
        old = old_rule(LEGS)
        old_after = sum(a for k, (_, a, _) in old.items() if spend.family_of(k, {FX}) == FX)
        old_spent = f['usd'] - old_after
        print(f'  the live rule (chain depth > 4 per chain family, as if live all along) lets ${old_spent:.0f} be spent '
              f'and saves ${old_after:.0f}')
        self.assertGreater(old_spent, 3 * families.CAP_USD, 'the old rule must fail this proof: it does not stop at ~$100')

    def test_every_family_over_the_cap_is_named(self):
        fams = families.decide(LEGS)
        over = {r: f for r, f in fams.items() if f['over']}
        print('  families over the cap ($100 or 3 generations), $ after the stop:')
        for r, f in sorted(over.items(), key=lambda x: -x[1]['usd']):
            print(f'    {r}: ${f["usd"]:.0f}, {f["depth"]} gen, {len(f["units"])} units; {f["over"]} at '
                  f'{f["over_at"]["unit"]} {f["over_at"]["at"][:16]}; ${f["usd_after"]:.0f} after')
        print(f'  saved in all: ${sum(f["usd_after"] for f in over.values()):.0f}')
        self.assertIn(FX, over)
        self.assertNotIn('fix-job-ordering-tests', over, '$36 and no generations: under the cap')

    def test_a_queued_fourth_generation_is_over(self):
        fams = families.decide([], queued=['goal-a-g1-g2-g1-g2'])
        self.assertTrue(fams['goal-a']['over'])
        fams = families.decide([], queued=['goal-a-g1-g2-g1'])
        self.assertFalse(fams['goal-a']['over'], 'three generations are allowed: this must still fail')


class Module(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.h = Path(self.tmp.name)
        self.legs = [dict(unit='goal-x-g1', cost_usd=60, started_at='2026-10-20T10:00:00Z', report='saw C:/nope/a.png 20/100'),
                     dict(unit='goal-x-r2-g1', cost_usd=50, started_at='2026-10-20T11:00:00Z')]
        self.written = []
        self.real = families.write_brief
        families.write_brief = lambda root, f: self.written.append(root) or '2026-11-02-goal-x'

    def tearDown(self):
        families.write_brief = self.real
        self.tmp.cleanup()

    def test_dry_logs_and_writes_no_brief(self):
        lines = families.run(core.Ctx(self.h, 1000.0, True, 'families'), self.legs, {'goal-x-r2-g1-g2'})
        self.assertTrue(any('would park 1 queued' in l for l in lines), lines)
        self.assertEqual(self.written, [])
        st = core.read_state('families', self.h)
        self.assertEqual(st['families']['goal-x']['brief'], '')
        core.write_json(self.h / 'modules.json', {'families': 'on'})
        self.assertIsNone(spend.capped('goal-x-r2-g1-g2', self.h, now=1001), 'no brief written: nothing is parked')

    def test_on_writes_one_brief_and_the_hook_parks_the_family(self):
        core.write_json(self.h / 'modules.json', {'families': 'on'})
        ctx = core.Ctx(self.h, 1000.0, False, 'families')
        families.run(ctx, self.legs, {'goal-x-r2-g1-g2', 'goal-x-visual-review-3', 'other-goal'})
        families.run(ctx, self.legs, {'goal-x-r2-g1-g2', 'goal-x-visual-review-3', 'other-goal'})
        self.assertEqual(self.written, ['goal-x'], 'one brief, not one a turn or a unit')
        cap = spend.capped('goal-x-r2-g1-g2', self.h, now=1001)
        self.assertEqual((cap['root'], cap['brief']), ('goal-x', '2026-11-02-goal-x'))
        self.assertTrue(spend.capped('goal-x-visual-review-3', self.h, now=1001))
        self.assertIsNone(spend.capped('other-goal', self.h, now=1001))
        core.write_json(self.h / 'modules.json', {'families': 'dry'})
        self.assertIsNone(spend.capped('goal-x-r2-g1-g2', self.h, now=1001), 'dry parks nothing')
        core.write_json(self.h / 'modules.json', {'families': 'on'})
        real = families.answered
        families.answered = lambda bid: bid == '2026-11-02-goal-x'
        try:
            lines = families.run(ctx, self.legs, {'goal-x-r3'})
        finally:
            families.answered = real
        self.assertTrue(any('answered' in l for l in lines), lines)
        self.assertIsNone(spend.capped('goal-x-r3', self.h, now=1001), 'after his answer a unit queued from it runs')
        self.assertEqual(self.written, ['goal-x'], 'and no second brief')

    def test_the_brief_passes_the_brief_shape_check(self):
        import briefs
        f = families.decide(self.legs)['goal-x']
        title, what_for, options, why, ev, none = families.brief_of('goal-x', f)
        self.assertEqual(briefs.check(title, what_for, options, why, ev, none), [])
        self.assertEqual(ev, [], 'a still that is not on disk is no evidence')
        self.assertTrue(options[0].startswith('Go on') and options[1].startswith('Stop'),
                        'A puts its units back and B stops it, as on a parked unit\'s brief (S.2): %s' % options)

    def test_the_brief_says_what_a_and_b_do_and_names_no_id(self):
        import briefs
        t = Path(self.tmp.name)
        old = {k: os.environ.get(k) for k in ('TW_BRIEFS', 'TW_NOTES')}
        for k in old:
            (t / k).mkdir()
            os.environ[k] = str(t / k)
        try:
            bid = self.real('ability-fx-captures', families.decide(self.legs)['goal-x'])
            b = briefs.find(briefs.folder(), bid)
        finally:
            for k, v in old.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v
        self.assertEqual(b['about'], 'goal family ability-fx-captures')
        self.assertTrue(b['options'][0]['text'].startswith('Go on') and (b['options'][0].get('then') or {}).get('says'),
                        'his A must have a Then line: without one his click is no plain answer the loop takes up')
        self.assertTrue((b['options'][1].get('then') or {}).get('says'), b['options'])
        self.assertNotIn('ability-fx-captures', b['title'])


if __name__ == '__main__':
    os.environ.pop('TW_SUP_HOME', None)
    unittest.main(verbosity=1)
