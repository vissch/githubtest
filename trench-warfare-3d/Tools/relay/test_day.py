#!/usr/bin/env python3
"""Tests for the master's one screen (Tools/relay/day.py, `relay.py day`) and the queue's order
(`relay.py prio`, sources/lane.py). Run: python Tools/relay/test_day.py
Each test works in a temporary folder with a board and a relay home of its own.
"""
import contextlib, io, json, os, shutil, subprocess, sys, tempfile, time, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import config, day, gitio, relay   # noqa: E402
from sources import lane            # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_BOARD", "TW_STATION")


def git(args, cwd):
    return subprocess.run(["git"] + args, cwd=str(cwd), capture_output=True, text=True, check=True).stdout


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="day-test-"))
        self.board, self.home = self.tmp / "board", self.tmp / "home"
        (self.board / "relay" / "queue").mkdir(parents=True)
        self.old = {k: os.environ.get(k) for k in ENV}
        os.environ.update(TW_RELAY_HOME=str(self.home), TW_BOARD=str(self.board), TW_STATION="desktop")
        self.lim, self.ph = config.limits(), config.phases()

    def tearDown(self):
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def queue(self, uid, priority=None, lane_name="lane/show/x", raw=None):
        u = {"id": uid, "lane": lane_name, "role": "lane", "goal": "Add a.txt", "done_when": ["git", "status"]}
        if priority is not None:
            u["priority"] = priority
        p = self.board / "relay" / "queue" / (uid + ".json")
        p.write_text(raw if raw is not None else json.dumps(u), encoding="utf-8")
        return p

    def done(self, uid):
        p = self.board / "relay" / "done" / (uid + ".json")
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text("{}", encoding="utf-8")

    def order(self):
        """The ids in the order the runner would take them."""
        out, ctx = [], {"board": str(self.board), "skip": set()}
        while True:
            u = lane.next(ctx)
            if not u:
                return out
            out.append(u["id"])
            ctx["skip"].add(u["id"])

    def leg(self, cost, phase):
        d = self.board / "relay" / "desktop" / "legs"
        d.mkdir(parents=True, exist_ok=True)
        n = len(list(d.glob("*.json"))) + 1
        run = time.strftime("%Y%m%d-%H%M%S") + "-1"
        (d / ("%s-%02d.json" % (run, n))).write_text(json.dumps(
            {"run": run, "leg": n, "unit": "earlier", "phase": phase, "model": self.ph[phase]["model"],
             "effort": self.ph[phase]["effort"], "cost_usd": cost,
             "started_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}), encoding="utf-8")

    def stop(self, run, station="desktop", **kw):
        p = self.board / "relay" / station / "stops" / (run + ".json")
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(dict({"run": run, "reason": "nothing left to do", "legs": 2, "units": {}}, **kw)),
                     encoding="utf-8")

    def screen(self, holder=None):
        return day.lines(self.board, self.home, self.lim, self.ph, holder)

    def main(self, *argv):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = relay.main(list(argv))
        return code, buf.getvalue()


class Order(Base):
    """The queue's order: the lower priority first, then the name."""
    def test_with_no_priority_the_queue_runs_by_name_as_before(self):
        for uid in ("b", "c", "a"):
            self.queue(uid)
        self.assertEqual(self.order(), ["a", "b", "c"])

    def test_a_lower_priority_runs_first_and_the_name_breaks_a_tie(self):
        self.queue("a", 60)
        self.queue("b")                                         # none named: the shipped 50
        self.queue("c", 10)
        self.queue("d", 10)
        self.assertEqual(self.lim["queue_priority"], 50)
        self.assertEqual(self.order(), ["c", "d", "b", "a"])

    def test_a_done_unit_is_never_taken_whatever_its_priority(self):
        self.queue("a", 0)
        self.queue("b")
        self.done("a")
        self.assertEqual(self.order(), ["b"])

    def test_a_priority_that_is_not_a_whole_number_in_range_is_refused(self):
        top = self.lim["queue_priority_max"]
        for bad in ("high", 1.5, True, -1, top + 1):
            p = self.queue("a", bad)
            with self.assertRaises(SystemExit) as e:
                lane.load(p)
            self.assertIn("priority must be a whole number from 0 to %d" % top, str(e.exception))
        for good in (0, top):
            self.assertEqual(lane.load(self.queue("a", good))["priority"], good)

    def test_a_broken_file_stops_the_queue_only_when_its_turn_comes(self):
        self.queue("a")
        self.queue("z", raw="{not json")
        ctx = {"board": str(self.board), "skip": set()}
        self.assertEqual(lane.next(ctx)["id"], "a")             # the good one ahead of it is still taken
        ctx["skip"].add("a")
        with self.assertRaises(SystemExit):
            lane.next(ctx)


class Prio(Base):
    """`relay.py prio <id> <n>`: the owner's (or the master's) way to move a unit in the queue."""
    def test_prio_writes_the_priority_keeps_the_rest_and_moves_the_unit(self):
        self.queue("a")
        before = json.loads(self.queue("b").read_text(encoding="utf-8"))
        code, out = self.main("prio", "b", "5")
        self.assertEqual(code, 0)
        self.assertIn("b now has priority 5", out)
        after = json.loads((self.board / "relay" / "queue" / "b.json").read_text(encoding="utf-8"))
        self.assertEqual(after, dict(before, priority=5))
        self.assertEqual(self.order(), ["b", "a"])

    def test_prio_refuses_an_unknown_unit_a_done_one_and_a_number_out_of_range(self):
        with self.assertRaises(SystemExit) as e:
            self.main("prio", "nope", "5")
        self.assertIn("nothing is queued as nope", str(e.exception))
        p = self.queue("a", 20)
        text = p.read_text(encoding="utf-8")
        with self.assertRaises(SystemExit):
            self.main("prio", "a", str(self.lim["queue_priority_max"] + 1))
        self.assertEqual(p.read_text(encoding="utf-8"), text)   # a refused number leaves the file as it was
        self.done("a")
        with self.assertRaises(SystemExit) as e:
            self.main("prio", "a", "5")
        self.assertIn("a is done already", str(e.exception))
        self.assertEqual(p.read_text(encoding="utf-8"), text)

    def test_prio_commits_the_change_on_the_board(self):
        git(["init", "-q"], self.board)
        git(["config", "user.email", "t@example.com"], self.board)
        git(["config", "user.name", "t"], self.board)
        self.queue("a")
        git(["add", "-A"], self.board)
        git(["commit", "-q", "-m", "queue a"], self.board)
        code, out = self.main("prio", "a", "7")
        self.assertEqual(code, 0)
        self.assertEqual(git(["status", "--porcelain"], self.board), "")          # committed: the runner trusts it
        self.assertIn("relay: priority a 7", git(["log", "-1", "--format=%s"], self.board))
        self.assertEqual(self.order(), ["a"])

    def test_prio_refuses_a_queue_file_nobody_committed(self):
        git(["init", "-q"], self.board)
        git(["config", "user.email", "t@example.com"], self.board)
        git(["config", "user.name", "t"], self.board)
        git(["commit", "-q", "--allow-empty", "-m", "start"], self.board)
        p = self.queue("a")                                     # written, never committed: a leg could have done it
        text = p.read_text(encoding="utf-8")
        with self.assertRaises(SystemExit) as e:
            self.main("prio", "a", "7")
        self.assertIn("not committed on the board", str(e.exception))
        self.assertEqual(p.read_text(encoding="utf-8"), text)
        self.assertEqual(git(["log", "--format=%s"], self.board).strip(), "start")


class Day(Base):
    """`relay.py day`: one screen the master starts every turn from."""
    def test_an_empty_board_still_prints_a_full_screen(self):
        out = self.screen()
        self.assertEqual(out, ["Today: $0.00 of $50.00 spent, $50.00 left.", "No run going. No run yet.",
                               "Queue: nothing is queued.", "Needs you: nothing."])

    def test_the_queue_is_shown_in_the_runners_order_with_the_usual_cost(self):
        self.queue("slow", 80)
        self.queue("first", 5, "lane/show/landing-tools")
        self.queue("gone")
        self.done("gone")
        out = self.screen()
        usual = 2 * self.lim["usual_leg_usd"]                    # no cost on the board yet: plan and execute, shipped
        self.assertIn("Queue: 2 units, about $%.2f at the usual cost of $%.2f a unit (plan and execute)."
                      % (2 * usual, usual), out)
        rows = [l.split() for l in out if l.startswith("  ") and "." in l.split()[0]]
        self.assertEqual([(r[1], r[3], r[4]) for r in rows],
                         [("first", "5", "lane/show/landing-tools"), ("slow", "80", "lane/show/x")])
        self.assertEqual([u["id"] for u in day.queue(self.board)[0]], self.order())

    def test_the_usual_cost_of_a_unit_comes_from_the_legs_on_the_board(self):
        self.queue("a")
        for c in (1, 2, 3):
            self.leg(c, "plan")
            self.leg(c * 2, "execute")
        out = self.screen()
        self.assertEqual(out[0], "Today: $18.00 of $50.00 spent, $32.00 left.")
        self.assertIn("Queue: 1 unit, about $6.00 at the usual cost of $6.00 a unit (plan and execute).", out)

    def test_a_long_queue_still_fits_one_screen(self):
        for n in range(40):
            self.queue("unit-%02d" % n)
        self.stop("20261005-100000-1", units={("u%02d" % n): "FAIL" for n in range(40)})
        out = self.screen({"who": "pc-10", "until": "2026-10-05 12:00"})
        rows = self.lim["day_queue_rows"]
        self.assertEqual(len(out), 2 * rows + 8)                 # six single lines, two capped lists with a tail
        self.assertEqual(sum(1 for l in out if l == "  ... and %d more" % (40 - rows)), 2)
        self.assertTrue(all(len(l) <= self.lim["day_line_chars"] for l in out), out)
        self.assertEqual(out[-1], "The relay build is held by pc-10 until 2026-10-05 12:00.")

    def test_no_line_is_wider_than_the_screen_and_a_stop_reason_is_never_cut(self):
        width = self.lim["day_line_chars"]
        self.queue("unit-" + "x" * width, lane_name="lane/show/" + "y" * width)
        reason = "the day's budget has $1.00 left of $5.00, and a unit usually costs $4.00 " + "word " * 40
        self.stop("20261005-100000-1", reason=reason, units={"u" * 2 * width: "FAIL"})
        out = self.screen({"who": "w" * 2 * width, "until": "2026-10-05 12:00"})
        self.assertTrue(all(len(l) <= width for l in out), out)
        self.assertTrue(all(l.isascii() for l in out))
        self.assertIn(" ".join(reason.split()), " ".join(" ".join(out).split()))
        self.assertEqual(sum(1 for l in out if l.endswith("...")), 2)        # the two rows: the unit, what needs you

    def test_the_newest_stop_of_any_station_is_shown_and_a_unit_that_did_not_pass_needs_the_owner(self):
        self.stop("20261004-090000-1", "laptop", reason="the leg cap (2) is reached")
        self.stop("20261005-161419-2", "desktop", legs=9, units={"u1": "PASS", "u2": "FAIL", "u3": "BLOCKED"})
        out = self.screen()
        self.assertEqual(out[1:3], ["No run going. Last run 20261005-161419-2 (desktop): 9 legs, 3 units: 1 PASS, "
                                    "1 FAIL, 1 BLOCKED.", "  It stopped: nothing left to do."])
        at = out.index("Needs you:")
        self.assertEqual(out[at + 1:at + 3], ["  - u2 ended FAIL in the last run", "  - u3 ended BLOCKED in the last run"])

    def test_a_queue_file_the_runner_will_not_take_needs_the_owner(self):
        self.queue("bad", raw=json.dumps({"id": "bad", "lane": "lane/show/x"}))
        out = self.screen()
        self.assertIn("Queue: nothing is queued.", out)
        self.assertTrue(any(l.startswith("  - bad.json has no role") for l in out), out)

    def test_a_run_going_on_this_machine_is_shown_with_its_progress(self):
        work = self.tmp / "work"
        work.mkdir()
        git(["init", "-q"], work)                                # the lock also marks the checkout's .git
        gitio.take_lock(work, self.home, "relay 20261005-200000-9 u2", "lane/show/x")   # as the runner names it
        prog = self.home / "runs" / "20261005-200000-9" / "progress.json"
        prog.parent.mkdir(parents=True)
        prog.write_text(json.dumps({"started_by": "master", "legs": 3, "now_on": "u2, leg 03 (plan)",
                                    "units": {"u1": "FAIL"}}), encoding="utf-8")
        self.stop("20261005-100000-1")                           # an older stop is not the news while a run is going
        out = self.screen()
        self.assertEqual(out[1:3], ["Run going: 20261005-200000-9 on lane/show/x, started by master.",
                                    "  3 legs so far, now on u2, leg 03 (plan)."])
        self.assertFalse(any(l.startswith("No run going") for l in out))
        self.assertIn("  - u1 ended FAIL in the run going now", out)

    def test_the_holder_of_the_relay_build_is_shown(self):
        self.assertEqual(self.main("hold", "pc-10")[0], 0)
        code, out = self.main("day")
        self.assertEqual(code, 0)
        self.assertIn("The relay build is held by pc-10 until ", out)
        self.assertIn("Today: $0.00 of $50.00 spent, $50.00 left.", out)

    def test_day_writes_nothing(self):
        self.queue("a")
        self.stop("20261005-100000-1")
        before = sorted((str(p), p.stat().st_mtime_ns) for p in self.tmp.rglob("*"))
        self.main("day")
        self.assertEqual(sorted((str(p), p.stat().st_mtime_ns) for p in self.tmp.rglob("*")), before)


if __name__ == "__main__":
    unittest.main(verbosity=1)
