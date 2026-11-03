#!/usr/bin/env python3
"""Tests of modules/capacity.py and its feed hook (spend.capacity_room), against the replay kit of 2026-10-19..11-02.
Run: python Tools/supervisor/test_capacity.py

The proofs replay the relays' own records: on the 5-6% days (10-20, 10-24) the budget carries 1-2 relays, on the
night of 10-26/27 (15% over 10 h, cap 36) it carries 6, and most of the 357 runs that stopped on the pace or the
budget with 0 legs would not have started. Each proof is also run against today's rule (a stub: every extra relay
may start) and must fail there.
"""
import datetime
import importlib.util
import os
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import budget  # noqa: E402
import core  # noqa: E402
import spend  # noqa: E402
from replay import records  # noqa: E402

spec = importlib.util.spec_from_file_location('tw_sup_test_capacity', HERE / 'modules' / 'capacity.py')
capacity = importlib.util.module_from_spec(spec)
spec.loader.exec_module(capacity)

RS = records.load()
I = capacity._iso
DST = datetime.datetime(2026, 10, 25, 1)       # the desktop went from UTC+2 to UTC+1


def local(u):
    return u + datetime.timedelta(hours=2 if u < DST else 1)


def utc(lt):
    u = lt - datetime.timedelta(hours=1)
    return u if u >= DST else lt - datetime.timedelta(hours=2)


def spent_at(u):
    """What the day had used by UTC time u, % of the week: the ledger's figure in the last stop record that day,
    or the legs' dollars when they say more."""
    lt = local(u)
    d0 = lt.replace(hour=0, minute=0, second=0)
    legs = sum(float(l.get('cost_usd') or 0) for l in RS.legs
               if l.get('started_at') and d0 <= local(I(l['started_at'])) < lt) / capacity.USD_PER_PCT
    runs = [float(r.get('day_pct') or 0) for r in RS.runs if r.get('stopped_at') and d0 <= local(I(r['stopped_at'])) < lt]
    return max([legs] + runs)


def allowed_at(u, cap, end=None, rule=None):
    """The module's decision at UTC time u on the day's cap `cap` (a window ending at local `end`, else midnight)."""
    lt = local(u)
    end = end or lt.replace(hour=0, minute=0, second=0) + datetime.timedelta(days=1)
    r, _ = capacity.rate(RS.runs, RS.legs, u)
    return (rule or capacity.decide)(dict(cap=cap, spent=spent_at(u), hours=(end - lt).total_seconds() / 3600.0, rate=r))


def today_rule(s):
    """Stub of the rule before this module: feed starts every extra relay that has work."""
    return dict(allowed=capacity.MOST, why='every relay')


def running_at(u, me):
    """The relays (by number) whose run was going at UTC time u, the run `me` left out."""
    out = set()
    for r in RS.runs:
        if r is me or not (r.get('started_at') and r.get('stopped_at')):
            continue
        if I(r['started_at']) <= u < I(r['stopped_at']):
            out.add(str(r['relay']))
    return out


def prevented(rule=None, worked=False):
    """(prevented, of): the 0-leg runs stopped on the pace or the budget that feed would not have started. worked:
    the extra relays' runs that did legs instead (what the rule costs)."""
    if worked:
        empty = [r for r in RS.runs if r.get('legs') and str(r['relay']) != '1']
    else:
        empty = [r for r in RS.runs if r.get('reason_kind') in ('pace', 'budget') and not r.get('legs')]
    n = 0
    for r in empty:
        if str(r['relay']) == '1':
            continue                                     # the main relay is not held by capacity (its own pace is)
        u = I(r['started_at'])
        taken = len(running_at(u, r) | {'1'})            # relay 1 holds the first slot, running or not
        if taken >= allowed_at(u, float(r.get('day_budget_pct') or 11), rule=rule)['allowed']:
            n += 1
    return n, len(empty)


class Decide(unittest.TestCase):
    def test_the_formula(self):
        self.assertEqual(capacity.decide(dict(cap=36, spent=0, hours=8, rate=0.25))['allowed'], 6)
        self.assertEqual(capacity.decide(dict(cap=11, spent=5, hours=10, rate=0.2))['allowed'], 3)
        self.assertEqual(capacity.decide(dict(cap=5, spent=3, hours=14, rate=0.2))['allowed'], 1,
                         '0.71 relays: relay 1 still holds the first slot while anything is left')
        self.assertEqual(capacity.decide(dict(cap=5, spent=5.2, hours=14, rate=0.2))['allowed'], 0,
                         'a spent day carries no relay: this input must still fail')

    def test_rate_is_measured_from_the_legs(self):
        r, how = capacity.rate(RS.runs, RS.legs, I('2026-10-20T12:00:00'))
        self.assertTrue(0.1 < r < 0.35 and 'working relay-hours' in how, (r, how))
        r0, how0 = capacity.rate([], [], I('2026-10-20T12:00:00'))
        self.assertEqual(r0, capacity.RATE_PCT, how0)


class Replay(unittest.TestCase):
    def hours(self, day, cap, rule=None):
        """{hour: allowed} over the hours the day's cap was not spent yet, and the hours it was."""
        out, spent = {}, []
        for h in range(6, 22):
            u = utc(datetime.datetime(2026, 10, day, h))
            if spent_at(u) >= cap:
                spent.append(h)
            else:
                out[h] = allowed_at(u, cap, rule=rule)['allowed']
        return out, spent

    def test_five_percent_days_carry_one_or_two(self):
        for day, cap in ((20, 5.0), (24, 6.0)):
            got, spent = self.hours(day, cap)
            print(f'  10-{day} cap {cap:g}%: allowed by hour {got}; cap spent from {spent[:1] or "-"}')
            self.assertTrue(len(got) >= 8 and set(got.values()) <= {1, 2}, (day, got))
            self.assertTrue(all(allowed_at(utc(datetime.datetime(2026, 10, day, h)), cap)['allowed'] == 0 for h in spent),
                            'a spent day carries none')
            old, _ = self.hours(day, cap, today_rule)
            self.assertFalse(set(old.values()) <= {1, 2}, 'today\'s rule must fail this proof')

    def test_the_good_night_carries_six(self):
        with tempfile.TemporaryDirectory() as h:
            # his words that night: 15% for 10 hours, from 22:00; budget.py gives the cap 36 he was given by hand
            budget.set_(15, hours=10, used=0, now=datetime.datetime(2026, 10, 26, 22, 0), h=h)
            got = {}
            for lt in (datetime.datetime(2026, 10, 26, 22, 30), datetime.datetime(2026, 10, 27, 0, 30),
                       datetime.datetime(2026, 10, 27, 3, 0), datetime.datetime(2026, 10, 27, 6, 0)):
                cap = budget.day_pct(lt, h)
                end = capacity.window_end(lt, h)
                got[lt.strftime('%m-%d %H:%M')] = (cap, allowed_at(utc(lt), cap, end)['allowed'])
            print(f'  night 10-26/27 (cap, allowed): {got}')
            self.assertEqual(budget.day_pct(datetime.datetime(2026, 10, 27, 3, 0), h), 36.0)
            self.assertTrue(all(a == 6 for _, a in got.values()), got)

    def test_empty_runs_it_prevents(self):
        n, of = prevented()
        old, _ = prevented(today_rule)
        print(f'  0-leg pace/budget runs: {of}; feed would not have started {n} ({100 * n / of:.0f}%); '
              f'today\'s rule: {old}')
        self.assertEqual(of, 357)
        self.assertGreaterEqual(n / of, 0.5, (n, of))
        self.assertLess(old / of, 0.2, 'today\'s rule must fail this proof')
        w, wof = prevented(worked=True)
        wusd = sum(float(r.get('run_usd') or 0) for r in RS.runs if r.get('legs') and str(r['relay']) != '1')
        print(f'  extra-relay runs that did legs: {wof} (${wusd:.0f}); the rule would have held back {w} of them')
        self.assertLess(w, wof, 'a rule that holds every extra back is no capacity rule: this must still fail')


class Hook(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.h = Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_dry_and_off_hold_nothing_back(self):
        core.write_state('capacity', dict(allowed=1, why='x', at=1000), self.h)
        self.assertEqual(spend.capacity_room(5, self.h, now=1001), (True, ''), 'dry by default: feed unchanged')
        core.write_json(self.h / 'modules.json', {'capacity': 'off'})
        self.assertTrue(spend.capacity_room(5, self.h, now=1001)[0])

    def test_on_holds_the_extra_back_when_the_slots_are_taken(self):
        core.write_json(self.h / 'modules.json', {'capacity': 'on'})
        core.write_state('capacity', dict(allowed=2, why='x', at=1000), self.h)
        self.assertTrue(spend.capacity_room(1, self.h, now=1001)[0])
        ok, why = spend.capacity_room(2, self.h, now=1001)
        self.assertFalse(ok)
        self.assertIn('2 of 2', why)
        self.assertTrue(spend.capacity_room(2, self.h, now=1000 + spend.STALE_S + 1)[0], 'a stale figure holds nothing')

    def test_run_in_dry_writes_only_its_state(self):
        ctx = core.Ctx(self.h, 1000.0, True, 'capacity')
        real = budget.used_now
        budget.used_now = lambda: None
        try:
            lines = capacity.run(ctx)
        finally:
            budget.used_now = real
        self.assertIn('could not be read', lines[0])
        self.assertEqual(sorted(p.name for p in self.h.rglob('*') if p.is_file()), ['capacity.json'])


if __name__ == '__main__':
    os.environ.pop('TW_SUP_HOME', None)
    unittest.main(verbosity=1)
