#!/usr/bin/env python3
"""Tests of budget.py (the owner's words for the day) and its readers (spend.day_figure, spend.day_cap).
Run: python Tools/supervisor/test_budget.py

Each figure is one the owner gave by chat between 10-13 and 11-02 and a session turned into TW_DAY_PCT by hand;
the hand sum is the expected value. Today's rule (a stub: the env var or 11, whatever he said) must fail them.
"""
import datetime
import io
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from types import SimpleNamespace

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import budget  # noqa: E402
import spend  # noqa: E402

D = datetime.datetime


def today_rule(env):
    """Stub of the rule before budget.py: the cap is TW_DAY_PCT when the loop started, else 11."""
    return float(env or 11)


class Maths(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.h = Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_no_file_is_the_standing_rule_and_the_env_stands(self):
        self.assertEqual(budget.day_pct(D(2026, 11, 2, 10), self.h), 11.0)
        self.assertIsNone(spend.day_cap(D(2026, 11, 2, 10), self.h))
        self.assertEqual(spend.day_figure('26', D(2026, 11, 2, 10), self.h), '26', 'no budget.json: TW_DAY_PCT as before')
        self.assertEqual(spend.day_figure('11', D(2026, 11, 2, 10), self.h), '11')

    def test_x_more_until_t(self):
        # "another 4% until 9am" at 01:00 with 2.0% used: the pace must allow 6.0% by 09:00 -> 6 * 24 / 9 = 16
        now = D(2026, 10, 21, 1, 0)
        budget.set_(4, at=budget.until('9am', now), used=2.0, now=now, h=self.h)
        self.assertAlmostEqual(budget.day_pct(D(2026, 10, 21, 5), self.h), 16.0)
        self.assertEqual(spend.day_figure('11', D(2026, 10, 21, 5), self.h), '16')
        self.assertEqual(budget.day_pct(D(2026, 10, 21, 9, 0), self.h), 11.0, 'the window ended: the standing 11%')
        self.assertEqual(spend.day_figure('15', D(2026, 10, 21, 9, 0), self.h), '15',
                         'after his window a run\'s own -DayPct or TW_DAY_PCT stands again')
        self.assertNotEqual(today_rule('11'), 16.0, 'today\'s rule must fail this')

    def test_only_x_before_t_and_its_pace(self):
        # "only 2% before 18:00" at 12:00 with 3.1% used: (3.1 + 2) * 24 / 18 = 6.8
        now = D(2026, 10, 24, 12, 0)
        budget.set_(2, at=budget.until('18:00', now), used=3.1, now=now, h=self.h)
        cap = budget.day_pct(D(2026, 10, 24, 12, 30), self.h)
        self.assertAlmostEqual(cap, 6.8)
        self.assertAlmostEqual(cap * 18 / 24, 3.1 + 2, msg='what the pace allows by 18:00 is used + X')

    def test_x_percent_today(self):
        now = D(2026, 10, 19, 10, 29)
        budget.set_(26, now=now, h=self.h)
        self.assertEqual(budget.day_pct(D(2026, 10, 19, 23, 59), self.h), 26.0)
        self.assertEqual(budget.day_pct(D(2026, 10, 20, 0, 1), self.h), 11.0, 'gone at midnight')

    def test_for_hours_across_midnight(self):
        # "15% for 10 hours" at 22:00 with 9.0% used today: 3% of it before midnight, 12% from 00:00 to 08:00
        now = D(2026, 10, 26, 22, 0)
        budget.set_(15, hours=10, used=9.0, now=now, h=self.h)
        self.assertAlmostEqual(budget.day_pct(D(2026, 10, 26, 23), self.h), 12.0, msg='(9 + 3) * 24 / 24')
        self.assertAlmostEqual(budget.day_pct(D(2026, 10, 27, 3), self.h), 36.0, msg='12 * 24 / 8: the 36 he was given')
        self.assertEqual(budget.day_pct(D(2026, 10, 27, 8, 0), self.h), 11.0)

    def test_set_11_and_clear_end_a_window(self):
        now = D(2026, 10, 20, 9, 0)
        budget.set_(5, now=now, h=self.h)
        self.assertEqual(budget.day_pct(D(2026, 10, 20, 10), self.h), 5.0)
        budget.set_(11, now=D(2026, 10, 20, 11), h=self.h)
        self.assertEqual(budget.day_pct(D(2026, 10, 20, 12), self.h), 11.0)
        self.assertEqual(budget.day_pct(D(2026, 10, 20, 10, 30), self.h), 5.0, 'the past is kept as it was')
        self.assertEqual(spend.day_figure('26', D(2026, 10, 20, 12), self.h), '11', 'with budget.json the env is not read')

    def test_a_newer_window_replaces_the_open_one(self):
        budget.set_(26, now=D(2026, 10, 19, 10), h=self.h)
        budget.set_(2, at=D(2026, 10, 19, 18), used=4.0, now=D(2026, 10, 19, 12), h=self.h)
        self.assertAlmostEqual(budget.day_pct(D(2026, 10, 19, 13), self.h), 8.0)
        self.assertEqual(budget.day_pct(D(2026, 10, 19, 20), self.h), 11.0, 'not back to 26 after 18:00')

    def test_words_it_refuses(self):
        for bad in ('lots', '-3', '7%%'):
            self.assertRaises(ValueError, budget.pct, bad)
        self.assertRaises(ValueError, budget.until, '25:00', D(2026, 10, 19, 10))
        self.assertRaises(ValueError, budget.hours_of, 'soon')
        self.assertEqual(budget.until('9', D(2026, 10, 19, 10)), D(2026, 10, 20, 9), 'a time gone today is tomorrow')
        self.assertEqual(budget.pct('7%'), 7.0)

    def test_a_broken_file_keeps_the_env(self):
        budget.path(self.h).parent.mkdir(parents=True, exist_ok=True)
        budget.path(self.h).write_text('{"windows": [{"kind": "more"}]}', encoding='utf-8')
        self.assertEqual(budget.day_pct(D(2026, 10, 19, 10), self.h), 11.0)
        self.assertEqual(spend.day_figure('26', D(2026, 10, 19, 10), self.h), '26', 'a window that does not read: the env')
        budget.set_(7, at=D(2026, 10, 19, 18), used=1, now=D(2026, 10, 19, 9), h=self.h)
        self.assertEqual(len(budget.load(self.h)['windows']), 1, 'a broken window is left out, never a crash')
        budget.path(self.h).write_text('not json', encoding='utf-8')
        self.assertEqual(spend.day_figure('26', D(2026, 10, 19, 10), self.h), '26', 'a file that does not read: the env')

    def test_cli(self):
        os.environ['TW_SUP_HOME'] = str(self.h)
        try:
            out = io.StringIO()
            with redirect_stdout(out):
                code = budget.main(['set', '7%', '--until', '23:59', '--used', '1'])
            self.assertEqual(code, 0)
            self.assertIn('7% more until', out.getvalue())
            self.assertEqual(budget.main(['set', 'lots']), 2)
            with redirect_stdout(io.StringIO()):
                self.assertEqual(budget.main(['clear']), 0)
            self.assertEqual(budget.day_pct(h=self.h), 11.0)
            self.assertEqual(spend.day_figure('26', h=self.h), '26', 'clear: the old figures stand again')
        finally:
            os.environ.pop('TW_SUP_HOME', None)


class Runner(unittest.TestCase):
    """The runner's hook (Tools/relay/runner.py owner_cap, read in Run.allot between units)."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.h = Path(self.tmp.name)
        os.environ['TW_SUP_HOME'] = str(self.h)
        sys.path.insert(0, str(HERE.parent / 'relay'))
        import config
        import runner
        self.runner = runner
        self.fake = SimpleNamespace(home=self.h / 'relayhome', base_pct=11.0, board=self.h / 'board', ctx={},
                                    lim=config.limits(overrides={'day_budget_pct': 11}))

    def tearDown(self):
        os.environ.pop('TW_SUP_HOME', None)
        self.tmp.cleanup()

    def test_a_running_relay_takes_his_figure_at_its_next_unit(self):
        self.assertIsNone(self.runner.owner_cap(), 'no budget.json: the run keeps its own figure')
        with redirect_stdout(io.StringIO()):
            self.runner.Run.allot(self.fake)
        self.assertEqual(self.fake.lim['day_budget_pct'], 11)
        budget.set_(26, now=datetime.datetime.now(), h=self.h)
        real = self.runner.owner_cap
        try:
            self.runner.owner_cap = lambda: None                 # today's rule: nothing outside the run is read
            with redirect_stdout(io.StringIO()):
                self.runner.Run.allot(self.fake)
            self.assertEqual(self.fake.lim['day_budget_pct'], 11, 'today\'s rule fails this proof')
        finally:
            self.runner.owner_cap = real
        out = io.StringIO()
        with redirect_stdout(out):
            self.runner.Run.allot(self.fake)
        self.assertEqual(self.fake.lim['day_budget_pct'], 26)
        self.assertIn("the owner's budget", out.getvalue())


if __name__ == '__main__':
    os.environ.pop('TW_SUP_HOME', None)
    unittest.main(verbosity=1)
