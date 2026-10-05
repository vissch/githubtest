#!/usr/bin/env python3
"""Tests for the weekly-limit readings (Tools/relay/usage.py). Run: python Tools/relay/test_usage.py
Each test works in a temporary folder. Nothing here calls anybody: a reading is handed in, as a source would.
The ledger's side (the day in percent) is tested in test_ledger.py, the leg record in test_relay.py (class Runs).
"""
import contextlib, io, json, os, shutil, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import config, relay, usage   # noqa: E402

ANSWER = {"five_hour": {"utilization": 33.0, "resets_at": "2026-10-05T19:00:00.528743+00:00"},
          "seven_day": {"utilization": 13.0, "resets_at": "2026-10-08T16:00:00.951713+00:00"},
          "seven_day_opus": None,
          "seven_day_sonnet": {"utilization": 1.0, "resets_at": "2026-10-09T03:00:00+00:00"},
          "extra_usage": {"is_enabled": False, "utilization": None}}
STATUSLINE = {"model": {"display_name": "Opus"},
              "rate_limits": {"five_hour": {"used_percentage": 23.5, "resets_at": 1791226800},
                              "seven_day": {"used_percentage": 41.2, "resets_at": 1791475200}}}


class Usage(unittest.TestCase):
    def setUp(self):
        self.home = Path(tempfile.mkdtemp(prefix="usage-test-"))
        self.old, self.old_board = os.environ.get("TW_RELAY_HOME"), os.environ.get("TW_BOARD")
        os.environ["TW_RELAY_HOME"] = str(self.home)
        os.environ["TW_BOARD"] = str(self.home / "board")
        self.lim = config.limits()

    def tearDown(self):
        os.environ.pop("TW_RELAY_HOME", None) if self.old is None else os.environ.__setitem__("TW_RELAY_HOME", self.old)
        os.environ.pop("TW_BOARD", None) if self.old_board is None else os.environ.__setitem__("TW_BOARD", self.old_board)
        shutil.rmtree(self.home, ignore_errors=True)

    def main(self, text, *argv):
        old, buf = sys.stdin, io.StringIO()
        sys.stdin = io.StringIO(text)
        try:
            with contextlib.redirect_stdout(buf):
                code = relay.main(list(argv))
        finally:
            sys.stdin = old
        return code, buf.getvalue().strip()

    def test_both_shapes_give_the_week_in_percent(self):
        a = usage.shape(ANSWER, 1000)
        self.assertEqual((a["week"], a["five_hour"], a["resets"], a["at_s"]), (13.0, 33.0, "2026-10-08 16:00", 1000))
        self.assertEqual(a["windows"], {"five_hour": 33.0, "seven_day": 13.0, "seven_day_sonnet": 1.0})
        s = usage.shape(STATUSLINE, 1000)
        self.assertEqual((s["week"], s["five_hour"]), (41.2, 23.5))
        self.assertRegex(s["resets"], r"^2026-10-\d\d \d\d:\d\d$")       # epoch seconds became a UTC stamp

    def test_an_answer_with_no_weekly_figure_is_no_reading(self):
        for raw in (None, {}, [], "x", {"five_hour": {"utilization": 5}}, {"seven_day": None},
                    {"seven_day": {"utilization": "13"}}, {"seven_day": {"utilization": True}},
                    {"rate_limits": {"five_hour": {"used_percentage": 5}}}):
            self.assertIsNone(usage.shape(raw, 1000), raw)
            self.assertIsNone(usage.put(self.home, raw))
        self.assertFalse((self.home / usage.FILE).exists())               # nothing was kept

    def test_a_reading_is_kept_and_read_back_until_it_is_too_old(self):
        self.assertIsNone(usage.read(self.home, self.lim))                # nothing was ever read
        usage.put(self.home, ANSWER, clock=lambda: 1000)
        age = self.lim["usage_max_age_seconds"]
        self.assertEqual(usage.read(self.home, self.lim, clock=lambda: 1000 + age)["week"], 13.0)
        self.assertIsNone(usage.read(self.home, self.lim, clock=lambda: 1001 + age))
        self.assertIsNone(usage.read(self.home, self.lim, clock=lambda: 999))     # a reading from the future is none
        usage.put(self.home, ANSWER, clock=lambda: 1000.9)                # put and read inside one second
        self.assertEqual(usage.read(self.home, self.lim, clock=lambda: 1000.95)["week"], 13.0)

    def test_a_bad_answer_does_not_wipe_the_reading_that_was_there(self):
        usage.put(self.home, ANSWER, clock=lambda: 1000)
        self.assertIsNone(usage.put(self.home, {"seven_day": None}, clock=lambda: 1100))
        self.assertEqual(usage.read(self.home, self.lim, clock=lambda: 1100)["at_s"], 1000)

    def test_a_broken_file_is_no_reading(self):
        self.home.mkdir(exist_ok=True)
        for text in ("{", "[1]", json.dumps({"reading": "x"}), json.dumps({"reading": {"week": "13", "at_s": 1}})):
            (self.home / usage.FILE).write_text(text, encoding="utf-8")
            self.assertIsNone(usage.read(self.home, self.lim, clock=lambda: 2))

    def test_what_a_leg_used_is_the_difference_of_two_readings(self):
        def r(at_s, week, resets="2026-10-08 16:00"):
            return {"at_s": at_s, "week": week, "resets": resets}
        self.assertEqual(usage.delta(r(1, 13.0), r(2, 13.75)), 0.75)
        self.assertEqual(usage.delta(r(1, 13.0), r(2, 13.0)), 0.0)        # measured, and it used nothing
        self.assertEqual(usage.delta(r(1, 13.0), r(2, 12.0)), 0.0)        # the figure never runs backwards in a week
        self.assertEqual(usage.delta(r(1, 96.0), r(2, 1.5, "2026-10-15 16:00")), 1.5)   # the week turned over
        for start, end in ((None, r(2, 1)), (r(1, 1), None), (None, None),
                           (r(5, 13.0), r(5, 13.0)),                      # the same reading twice measures nothing
                           (r(5, 13.0), r(4, 14.0))):
            self.assertIsNone(usage.delta(start, end))

    def test_the_command_keeps_a_reading_and_says_where_the_week_stands(self):
        code, out = self.main("", "usage")
        self.assertEqual((code, out[:19]), (1, "Week: not measured."))
        code, out = self.main(json.dumps(ANSWER), "usage", "put")
        self.assertEqual(code, 0)
        self.assertRegex(out, r"^Week: 13% used, 5-hour window 33% \(read \d\d:\d\d UTC\), "
                              r"starts over 2026-10-08 16:00 UTC\.$")
        code, again = self.main("", "usage")
        self.assertEqual((code, again), (0, out))
        self.assertEqual(self.main("not json", "usage", "put")[0], 1)
        self.assertEqual(self.main("", "usage")[0], 0)                     # the good reading is still there

    def test_as_a_status_line_it_prints_one_short_line_and_never_fails(self):
        self.assertEqual(self.main(json.dumps(STATUSLINE), "usage", "put", "--statusline"), (0, "week 41%"))
        self.assertEqual(usage.read(self.home, self.lim)["week"], 41.2)
        for text in ("", "not json", json.dumps({"model": {}})):           # no rate limits yet: early in a session
            self.assertEqual(self.main(text, "usage", "put", "--statusline"), (0, "week 41%"))


if __name__ == "__main__":
    unittest.main(verbosity=1)
