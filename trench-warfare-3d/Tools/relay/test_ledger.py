#!/usr/bin/env python3
"""Tests for the day's spend (Tools/relay/ledger.py). Run: python Tools/relay/test_ledger.py
Each test works in a temporary folder with a board of its own. The run loop's side of the day budget is tested in
test_relay.py (class Budget).
"""
import datetime, json, os, shutil, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import config, ledger   # noqa: E402


def stamp(days_ago=0):
    t = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(days=days_ago)
    return t.strftime("%Y-%m-%dT%H:%M:%SZ")


def day(days_ago=0):
    return (datetime.datetime.now() - datetime.timedelta(days=days_ago)).strftime("%Y-%m-%d")


class Ledger(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="ledger-test-"))
        self.board, self.home, self.n = self.tmp / "board", self.tmp / "home", 0
        self.old = os.environ.get("TW_RELAY_HOME")
        os.environ["TW_RELAY_HOME"] = str(self.home)
        self.lim = config.limits()

    def tearDown(self):
        os.environ.pop("TW_RELAY_HOME", None) if self.old is None else os.environ.__setitem__("TW_RELAY_HOME", self.old)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def leg(self, cost, phase="plan", days_ago=0, station="desktop", unit="u1", model="opus", effort="high", **kw):
        """One leg record on the board, named the way the runner names it. cost None: the record holds no cost."""
        self.n += 1
        run = (datetime.datetime.now() - datetime.timedelta(days=days_ago)).strftime("%Y%m%d-%H%M%S") + "-1"
        rec = dict({"run": run, "leg": self.n, "unit": unit, "phase": phase, "model": model, "effort": effort,
                    "started_at": stamp(days_ago)}, **kw)
        if cost is not None:
            rec["cost_usd"] = cost
        p = self.board / "relay" / station / "legs" / ("%s-%02d.json" % (run, self.n))
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(rec), encoding="utf-8")
        return rec

    def test_a_stamp_gives_its_local_day_and_a_bad_one_gives_none(self):
        self.assertEqual(ledger.day_of(stamp()), day())
        self.assertEqual(ledger.day_of(stamp(1)), day(1))
        for bad in (None, "", "yesterday", 5):
            self.assertIsNone(ledger.day_of(bad))

    def test_an_empty_board_has_spent_nothing(self):
        s = ledger.spent(self.board)
        self.assertEqual((s["day"], s["usd"], s["legs"], s["estimated"], s["units"]), (day(), 0.0, 0, 0, {}))

    def test_today_is_the_sum_of_todays_legs_on_every_station(self):
        self.leg(2.5)
        self.leg(1.5, "execute", station="laptop", unit="u2")
        self.leg(9, days_ago=1)
        s = ledger.spent(self.board)
        self.assertEqual((s["usd"], s["legs"], s["estimated"]), (4.0, 2, 0))
        self.assertEqual(s["units"], {"u1": {"plan": 2.5}, "u2": {"execute": 1.5}})
        self.assertEqual(ledger.spent(self.board, day(1))["usd"], 9.0)

    def test_a_leg_with_no_cost_in_its_record_takes_it_from_its_leg_folder(self):
        rec = self.leg(None)
        own = self.home / "runs" / rec["run"] / "legs" / ("%02d" % rec["leg"]) / "leg.json"
        own.parent.mkdir(parents=True)
        own.write_text(json.dumps({"cost_usd": 3.25}), encoding="utf-8")
        s = ledger.spent(self.board)
        self.assertEqual((s["usd"], s["estimated"]), (3.25, 0))

    def test_a_leg_with_no_cost_anywhere_counts_at_the_usual_cost_of_its_phase(self):
        for c in (2, 4, 6):
            self.leg(c, "execute", days_ago=1)
        self.leg(1, "plan", days_ago=1)
        self.leg(None, "execute")
        s = ledger.spent(self.board)
        self.assertEqual((s["usd"], s["legs"], s["estimated"]), (4.0, 1, 1))

    def test_with_no_cost_on_the_board_at_all_a_leg_counts_at_the_shipped_usual_cost(self):
        self.leg(None)
        self.assertEqual(ledger.spent(self.board)["usd"], float(self.lim["usual_leg_usd"]))
        self.assertEqual(ledger.usual(self.board, "plan"), float(self.lim["usual_leg_usd"]))

    def test_the_usual_cost_prefers_legs_of_the_same_phase_model_and_effort(self):
        for c in (1, 2, 3):
            self.leg(c, "plan", model="sonnet", effort="medium")
        for c in (10, 20):
            self.leg(c, "plan")
        self.leg(7, "execute", effort="low")
        price = ledger.usuals(self.board)
        self.assertEqual(price("plan", "sonnet", "medium"), 2.0)       # three of the same kind: their median
        self.assertEqual(price("plan", "opus", "high"), 3.0)           # two are too few: every plan leg
        self.assertEqual(price("execute", "opus", "low"), 7.0)
        self.assertEqual(price("critic"), 5.0)                         # none of the phase: every leg

    def test_only_the_newest_legs_set_the_usual_cost(self):
        self.leg(100, days_ago=3)
        for _ in range(3):
            self.leg(2)
        self.assertEqual(ledger.usuals(self.board, lim=dict(self.lim, price_legs=3))("plan"), 2.0)

    def test_a_broken_record_is_skipped(self):
        self.leg(2)
        bad = self.board / "relay" / "desktop" / "legs" / "zz-broken.json"
        bad.write_text("{not json", encoding="utf-8")
        (bad.parent / "zz-list.json").write_text("[1]", encoding="utf-8")
        self.assertEqual(ledger.spent(self.board)["usd"], 2.0)

    def test_true_is_not_a_cost(self):
        self.leg(True)
        self.assertEqual(ledger.spent(self.board)["estimated"], 1)

    def test_history_has_one_row_a_day_oldest_first(self):
        self.leg(1, days_ago=2)
        self.leg(2)
        self.leg(3)
        h = ledger.history(self.board, 3)
        self.assertEqual([(d["day"], d["usd"], d["legs"]) for d in h], [(day(2), 1.0, 1), (day(1), 0.0, 0), (day(), 5.0, 2)])

    def test_the_report_puts_today_first_and_leaves_out_days_with_no_leg(self):
        self.leg(2.5, "plan")
        self.leg(1.25, "execute")
        self.leg(None, "execute", unit="u2")
        self.leg(4, days_ago=2)
        out = ledger.lines(self.board, 50, 5)
        self.assertEqual(out[0], "Today %s: $5.00 spent of $50.00, $45.00 left. 3 legs, 1 estimated." % day())
        self.assertIn("plan $2.50  execute $1.25", out[1])              # the order the legs ran in, not the alphabet
        self.assertTrue(out[1].lstrip().startswith("u1"))
        self.assertEqual([l.split()[0] for l in out if l.startswith("  2")], [day(2)])
        self.assertTrue(all(l.isascii() for l in out))

    def test_the_report_and_the_one_line_say_when_no_budget_is_set_or_none_is_left(self):
        self.leg(8)
        self.assertIn("No day budget is set.", ledger.lines(self.board, 0)[0])
        self.assertEqual(ledger.one_line(self.board, 0), "Today: $8.00 spent (no day budget).")
        self.assertEqual(ledger.one_line(self.board, 5), "Today: $8.00 of $5.00 spent, $0.00 left.")
        self.assertEqual(ledger.one_line(self.board, 50), "Today: $8.00 of $50.00 spent, $42.00 left.")

    def test_the_one_line_says_how_many_legs_are_counted_at_the_usual_cost(self):
        self.leg(8)
        self.leg(None)
        self.assertEqual(ledger.one_line(self.board, 50),
                         "Today: $16.00 of $50.00 spent, $34.00 left. 1 of 2 legs counted at the usual cost.")
        self.assertEqual(ledger.one_line(self.board, 0),
                         "Today: $16.00 spent (no day budget). 1 of 2 legs counted at the usual cost.")


class Settings(unittest.TestCase):
    def test_the_day_budget_ships_at_50_and_a_flag_is_held_to_its_bounds(self):
        self.assertEqual(config.limits()["day_budget_usd"], 50)
        self.assertEqual(config.limits(overrides={"day_budget_usd": 9999})["day_budget_usd"], 500)
        self.assertEqual(config.limits(overrides={"day_budget_usd": 0})["day_budget_usd"], 0)

    def test_a_retrospective_cannot_move_the_day_budget(self):
        self.assertNotIn("day_budget_usd", config.RETRO_TUNES)


if __name__ == "__main__":
    unittest.main(verbosity=1)
