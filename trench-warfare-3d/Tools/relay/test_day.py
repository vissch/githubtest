#!/usr/bin/env python3
"""Tests for the master's one screen (Tools/relay/day.py, `relay.py day`), the queue's order
(`relay.py prio`, sources/lane.py), the owner's answers on the screen (answers.py) and a unit queued from a file
(`relay.py add --unit`). Run: python Tools/relay/test_day.py
Each test works in a temporary folder with a board, a relay home and the two folders of the owner's answers of its
own: none reads the Drive.
"""
import contextlib, datetime, io, json, os, shutil, subprocess, sys, tempfile, time, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import answers, config, day, gitio, relay, usage   # noqa: E402
from sources import lane            # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_BOARD", "TW_STATION", "TW_BRIEFS", "TW_NOTES")


def git(args, cwd):
    return subprocess.run(["git"] + args, cwd=str(cwd), capture_output=True, text=True, check=True).stdout


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="day-test-"))
        self.board, self.home = self.tmp / "board", self.tmp / "home"
        (self.board / "relay" / "queue").mkdir(parents=True)
        self.briefs, self.notes = self.tmp / "decisions", self.tmp / "notes"    # what the owner answered: empty folders
        self.briefs.mkdir()
        self.notes.mkdir()
        self.old = {k: os.environ.get(k) for k in ENV}
        os.environ.update(TW_RELAY_HOME=str(self.home), TW_BOARD=str(self.board), TW_STATION="desktop",
                          TW_BRIEFS=str(self.briefs), TW_NOTES=str(self.notes))
        self.limits = config.limits                         # these tests count in dollars or in measured legs:
        config.limits = lambda *a, **k: dict(self.limits(*a, **k), week_usd=0,   # no guessed week (see Guess),
                                             day_budget_pct=0, pace_to_hour=0)   # no percent cap, no pace (see Pace)
        self.lim, self.ph = config.limits(), config.phases()

    def tearDown(self):
        config.limits = self.limits
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

    def brief(self, bid, title="A question", state="open", keys="AB", crlf=False):
        """A decision put to the owner, as the asset board writes it."""
        d = self.briefs / bid
        d.mkdir(parents=True)
        text = json.dumps({"id": bid, "title": title, "state": state,
                           "options": [{"key": k, "text": "Option " + k} for k in keys]}, indent=1) + "\n"
        (d / "brief.json").write_bytes(text.replace("\n", "\r\n" if crlf else "\n").encode("utf-8"))

    def note(self, name, about, text, when="2026-10-06 12:00:00", state="open", crlf=False):
        """A note the owner left on a page, as the asset board writes it: a click is "A: the option's text"."""
        raw = ("---\nid: %s\nwhen: %s\nfrom: owner\nkind: page\nabout: %s\ntitle: T\npage: decide.html\nstate: %s\n"
               "---\n%s\n" % (name, when, about, state, text))
        (self.notes / (name + ".md")).write_bytes(raw.replace("\n", "\r\n" if crlf else "\n").encode("utf-8"))

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


class Role(Base):
    """`relay.py role <role> <id> ...`: give queued units the role whose brief their legs get."""
    def read(self, uid):
        return json.loads((self.board / "relay" / "queue" / (uid + ".json")).read_text(encoding="utf-8"))

    def start_board(self):
        git(["init", "-q"], self.board)
        git(["config", "user.email", "t@example.com"], self.board)
        git(["config", "user.name", "t"], self.board)
        git(["commit", "-q", "--allow-empty", "-m", "start"], self.board)

    def test_role_writes_the_role_on_every_unit_named_and_keeps_the_rest(self):
        before = {uid: json.loads(self.queue(uid, 20).read_text(encoding="utf-8")) for uid in ("a", "b", "c")}
        code, out = self.main("role", "review-fix", "a", "c")
        self.assertEqual(code, 0)
        self.assertIn("a, c now have the role review-fix", out)
        self.assertEqual({uid: self.read(uid) for uid in before},
                         {"a": dict(before["a"], role="review-fix"), "b": before["b"],
                          "c": dict(before["c"], role="review-fix")})
        self.assertEqual(self.order(), ["a", "b", "c"])                          # the queue's order is not touched

    def test_role_refuses_a_role_nobody_knows_an_unknown_unit_and_a_done_one_and_then_changes_nothing(self):
        p = self.queue("a")
        text = p.read_text(encoding="utf-8")
        self.queue("b")
        self.done("b")
        for argv, said in ((("painter", "a"), "no role named painter"),
                           (("review-fix", "a", "nope"), "nothing is queued as nope"),
                           (("review-fix", "a", "b"), "b is done already")):
            with self.assertRaises(SystemExit) as e:
                self.main("role", *argv)
            self.assertIn(said, str(e.exception))
            self.assertEqual(p.read_text(encoding="utf-8"), text)                # all of them, or none

    def test_role_commits_the_change_on_the_board_once_and_refuses_a_file_nobody_committed(self):
        self.start_board()
        self.queue("a")
        self.queue("b")
        git(["add", "-A"], self.board)
        git(["commit", "-q", "-m", "queue a b"], self.board)
        code, out = self.main("role", "vehicle-simulator", "a", "b")
        self.assertEqual(code, 0)
        self.assertEqual(git(["status", "--porcelain"], self.board), "")          # committed: the runner trusts it
        self.assertEqual(git(["log", "--format=%s"], self.board).splitlines()[:2],
                         ["relay: role vehicle-simulator a, b", "queue a b"])
        p = self.queue("c")                                     # written, never committed: a leg could have done it
        text = p.read_text(encoding="utf-8")
        with self.assertRaises(SystemExit) as e:
            self.main("role", "review-fix", "a", "c")
        self.assertIn("not committed on the board", str(e.exception))
        self.assertEqual((p.read_text(encoding="utf-8"), self.read("a")["role"]), (text, "vehicle-simulator"))


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

    def test_with_a_measured_leg_the_screen_is_in_percent_of_the_week(self):
        self.queue("a")
        for c in (1, 2, 3):
            self.leg(c, "plan")
            self.leg(c * 2, "execute")
        d = self.board / "relay" / "desktop" / "legs"
        f = sorted(d.glob("*.json"))[-1]                        # the newest leg, $6: it used 1.2 points of the week
        rec = json.loads(f.read_text(encoding="utf-8"))
        f.write_text(json.dumps(dict(rec, week_used=1.2, week_end={
            "at": "2026-10-05T16:54:00Z", "at_s": 1, "week": 41.0, "resets": "2026-10-08 16:00"})), encoding="utf-8")
        out = self.screen()
        self.assertEqual(out[0] + " " + out[1].strip(),         # a sentence too long for the screen goes on below
                         "Today: 3.6% of the week used by the relay, 6.4% left of a day's cap of about 10.0%. "
                         "5 of 6 legs estimated from their cost.")
        self.assertEqual(out[2], "Week: 41% used at a leg's last reading (2026-10-05 16:54 UTC), "
                                 "starts over 2026-10-08 16:00 UTC.")
        self.assertIn("Queue: 1 unit, about 1.2% of the week at the usual 1.2% a unit (plan and execute).", out)
        self.assertFalse([l for l in out if "$" in l])

    def test_a_reading_on_this_machine_outranks_the_one_a_leg_left(self):
        self.leg(1, "plan")
        f = next((self.board / "relay" / "desktop" / "legs").glob("*.json"))
        f.write_text(json.dumps(dict(json.loads(f.read_text(encoding="utf-8")), week_end={
            "at": "2026-10-05T16:54:00Z", "at_s": 1, "week": 41.0})), encoding="utf-8")
        self.assertTrue(self.screen()[1].startswith("Week: 41% used at a leg's last reading"))
        usage.put(self.home, {"seven_day": {"utilization": 55.0, "resets_at": "2026-10-08T16:00:00+00:00"}})
        out = self.screen()
        self.assertTrue(out[1].startswith("Week: 55% used (read "), out[1])
        self.assertTrue(out[0].startswith("Today: $1.00 of"), out[0])   # a reading alone measures no leg

    def test_as_shipped_the_screen_is_in_percent_from_the_first_leg_on_and_says_it_is_a_guess(self):
        config.limits = self.limits                             # the file as shipped: it holds a guessed week
        self.queue("a")
        self.leg(18.3, "plan")
        code, out = self.main("day")
        out = out.splitlines()
        self.assertEqual(code, 0)
        self.assertTrue(out[0].startswith("Today: about 1.0% of the week used by the relay, "), out[0])
        self.assertIn("A guess: no leg is measured yet, so a full week is taken as $%d of leg cost."
                      % self.limits()["week_usd"], " ".join(l.strip() for l in out[:3]))
        self.assertTrue([l for l in out if l.startswith("Queue: 1 unit, about ") and "% of the week" in l], out)
        self.assertFalse([l for l in out if len(l) > self.lim["day_line_chars"]], out)

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


class Answers(Base):
    """What the owner answered on the Decide page and nobody took up is on the master's screen (answers.py)."""
    def day(self):
        code, out = self.main("day")
        self.assertEqual(code, 0)
        return out.splitlines()

    def test_an_answer_nobody_took_up_is_on_the_screen_between_what_needs_him_and_the_holder(self):
        self.brief("2026-10-06-the-house-s-look", "The house's look")
        self.note("2026-10-06-120000-brief-a", "brief:2026-10-06-the-house-s-look", "A: Option A")
        self.assertEqual(self.main("hold", "pc-10")[0], 0)
        out = self.day()
        at = out.index("Your answers: 1 not taken up. What each leads to: briefs.py waiting.")
        self.assertEqual(out[at + 1], "  - A: The house's look")
        self.assertEqual(out[at - 1], "Needs you: nothing.")
        self.assertTrue(out[at + 2].startswith("The relay build is held by pc-10"), out)

    def test_with_no_answer_waiting_the_screen_says_so(self):
        self.brief("b1")                                        # a brief he has not answered waits on him, not on us
        self.assertIn("Your answers: nothing waits.", self.day())

    def test_the_lines_alone_say_nothing_of_answers_nobody_looked_for(self):
        self.brief("b1")
        self.note("n1", "brief:b1", "A: Option A")
        self.assertFalse([l for l in self.screen() if "Your answers" in l])

    def test_folders_that_are_not_there_are_said_and_fail_nothing(self):
        os.environ["TW_BRIEFS"] = str(self.tmp / "gone")
        out = " ".join(l.strip() for l in self.day())
        self.assertIn("Your answers: not read (", out)
        self.assertIn("is not there).", out)
        self.assertNotIn("Your answers: nothing waits.", out)   # a Drive that is not mounted is not "nothing waits"
        os.environ.update(TW_BRIEFS=str(self.briefs), TW_NOTES=str(self.tmp / "gone"))
        self.assertIn("Your answers: not read (", " ".join(self.day()))

    def test_his_last_word_is_his_answer_and_a_brief_counts_once(self):
        self.brief("b1", "Clicked twice")                       # the notes' names sort the other way than their times:
        self.note("n9", "brief:b1", "A: Option A", "2026-10-06 11:44:41")      # it is the time that says which is last
        self.note("n8", "brief:b1", "B: Option B", "2026-10-06 11:44:46")
        self.brief("b2", "In his words")
        self.note("n1", "brief:b2", "neither, do it later", "2026-10-06 11:50:00")
        self.brief("b3", "No such option", keys="AB")
        self.note("n4", "brief:b3", "D: Option D", "2026-10-06 11:55:00")
        rows, why = answers.read(self.briefs, self.notes)
        self.assertEqual(why, "")
        self.assertEqual([(r["id"], r["picked"], r["when"]) for r in rows],
                         [("b1", "B", "2026-10-06 11:44:46"), ("b2", "your own words", "2026-10-06 11:50:00"),
                          ("b3", "your own words", "2026-10-06 11:55:00")])
        out = self.day()
        self.assertIn("Your answers: 3 not taken up. What each leads to: briefs.py waiting.", out)
        self.assertIn("  - B: Clicked twice", out)
        self.assertIn("  - your own words: In his words", out)

    def test_what_is_taken_up_or_about_no_brief_waits_on_nobody(self):
        self.brief("closed", state="answered")
        self.note("n1", "brief:closed", "a late word")           # the brief is closed
        self.brief("done")
        self.note("n2", "brief:done", "A: Option A", state="done")     # his note is answered
        self.note("n3", "brief:gone", "A: Option A")             # no such brief
        self.note("n4", "brief:../board", "A: Option A")         # not a brief's name: nothing outside is looked up,
        (self.board / "brief.json").write_text(json.dumps(       # though a file that reads as a brief is there
            {"id": "../board", "title": "Outside", "state": "open", "options": []}), encoding="utf-8")
        self.note("n5", "land: proving-ground", "land it")       # a note about something else
        self.brief("b9")
        self.note("n7", "notes:b9", "A: Option A")               # about something else whose name ends like a brief's
        (self.notes / "readme.md").write_text("not a note", encoding="utf-8")
        (self.briefs / "broken").mkdir()
        (self.briefs / "broken" / "brief.json").write_text("{ not json", encoding="utf-8")
        self.note("n6", "brief:broken", "A: Option A")
        self.assertEqual(answers.read(self.briefs, self.notes), ([], ""))
        self.assertIn("Your answers: nothing waits.", self.day())

    def test_files_written_on_windows_read_the_same(self):
        self.brief("b1", "Lines that end both ways", crlf=True)
        self.note("n1", "brief:b1", "B: Option B\nand a word", crlf=True)
        self.brief("b2", "Unix lines")
        self.note("n2", "brief:b2", "A: Option A", "2026-10-06 12:00:01")
        self.assertEqual([(r["id"], r["picked"]) for r in answers.read(self.briefs, self.notes)[0]],
                         [("b1", "B"), ("b2", "A")])

    def test_the_rows_are_ascii_fit_the_screen_and_a_long_list_has_a_tail(self):
        width, rows = self.lim["day_line_chars"], self.lim["day_queue_rows"]
        for n in range(rows + 3):
            self.brief("b%02d" % n, "The enemy\u2019s barrage \u2014 Normal too? " + "long " * (40 if n == 0 else 0))
            self.note("n%02d" % n, "brief:b%02d" % n, "A: Option A", "2026-10-06 12:%02d:00" % n)
        out = self.day()
        at = out.index("Your answers: %d not taken up. What each leads to: briefs.py waiting." % (rows + 3))
        mine = out[at + 1:at + rows + 2]
        self.assertEqual(mine[-1], "  ... and 3 more")
        self.assertTrue(all(l.startswith("  - A: The enemy?s barrage ? Normal too?") for l in mine[:-1]), mine)
        self.assertTrue(mine[0].endswith("..."), mine[0])       # what he picked comes first: the title is what is cut
        self.assertTrue(all(l.isascii() and len(l) <= width for l in out), out)

    def test_reading_never_raises(self):
        f = self.tmp / "a-file"
        f.write_text("x", encoding="utf-8")
        self.assertEqual(answers.read(f, self.notes)[0], [])
        self.assertIn("is not there", answers.read(f, self.notes)[1])
        (self.briefs / "b1").mkdir()
        (self.briefs / "b1" / "brief.json").write_text("[1, 2]", encoding="utf-8")     # JSON, not a brief
        self.note("n1", "brief:b1", "A: Option A")
        self.assertEqual(answers.read(self.briefs, self.notes), ([], ""))

    def test_the_folders_are_the_drives_unless_named(self):
        os.environ.pop("TW_BRIEFS")
        os.environ.pop("TW_NOTES")
        self.assertEqual([p.as_posix() for p in answers.folders()],
                         ["G:/My Drive/TW3D-pipeline/decisions", "G:/My Drive/TW3D-pipeline/notes"])
        os.environ.update(TW_BRIEFS=str(self.briefs), TW_NOTES=str(self.notes))
        self.assertEqual(answers.folders(), (self.briefs, self.notes))


class AddUnit(Base):
    """`relay.py add --unit FILE`: a unit queued from a file, as an answer of the owner's names it."""
    UNIT = {"id": "house-cover", "lane": "lane/sim/house-cover", "goal": 'Men behind a building take "less" damage; $5 says so.',
            "done_when": ["python", "Tools/otr.py", "CoverTests"]}

    def file(self, unit=None, raw=None, name="unit.json"):
        p = self.tmp / name
        p.write_text(raw if raw is not None else json.dumps(self.UNIT if unit is None else unit), encoding="utf-8")
        return str(p)

    def queued(self, uid="house-cover"):
        return self.board / "relay" / "queue" / (uid + ".json")

    def test_a_unit_from_a_file_is_queued_as_the_file_says(self):
        code, out = self.main("add", "--unit", self.file())
        self.assertEqual(code, 0)
        self.assertEqual(out.strip(), "queued house-cover on lane/sim/house-cover. Board: no board repo.")
        self.assertEqual(json.loads(self.queued().read_text(encoding="utf-8")), dict(self.UNIT, role="lane"))
        self.assertEqual(self.order(), ["house-cover"])
        self.assertTrue(any("house-cover" in l for l in self.screen()))

    def test_the_same_unit_again_is_queued_once_and_ends_0(self):
        self.main("add", "--unit", self.file())
        self.main("prio", "house-cover", "5")                   # moved since: it is still the same unit
        before = self.queued().read_bytes()
        code, out = self.main("add", "--unit", self.file())
        self.assertEqual(code, 0)
        self.assertIn("house-cover is queued already, the same unit.", out)
        self.assertEqual(self.queued().read_bytes(), before)
        self.assertEqual(len(list((self.board / "relay" / "queue").glob("*.json"))), 1)

    def test_a_unit_given_a_role_since_is_still_the_same_unit(self):
        self.main("add", "--unit", self.file())
        self.main("role", "balance-simulator", "house-cover")
        before = self.queued().read_bytes()
        code, out = self.main("add", "--unit", self.file())
        self.assertEqual(code, 0)
        self.assertIn("house-cover is queued already, the same unit.", out)
        self.assertEqual(self.queued().read_bytes(), before)

    def test_a_unit_may_name_its_role_and_a_role_nobody_knows_is_refused_before_a_file_is_written(self):
        code, _ = self.main("add", "--unit", self.file(dict(self.UNIT, role="balance-simulator")))
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(self.queued().read_text(encoding="utf-8"))["role"], "balance-simulator")
        for argv in (["--unit", self.file(dict(self.UNIT, id="u3", role="balance-sim"))],
                     ["u3", "--lane", "lane/show/x", "--goal", "g", "--role", "balance-sim", "--done-when", "git", "status"]):
            with self.assertRaises(SystemExit) as e:
                self.main("add", *argv)
            self.assertIn("no role named balance-sim", str(e.exception))
            self.assertIn("balance-simulator", str(e.exception))                 # the error lists the names there are
        self.assertFalse(self.queued("u3").exists())
        code, _ = self.main("add", "u3", "--lane", "lane/show/x", "--goal", "g", "--role", "review-fix",
                            "--done-when", "git", "status")
        self.assertEqual((code, json.loads(self.queued("u3").read_text(encoding="utf-8"))["role"]), (0, "review-fix"))

    def test_another_unit_under_an_id_that_is_taken_is_refused(self):
        self.main("add", "--unit", self.file())
        before = self.queued().read_bytes()
        with self.assertRaises(SystemExit) as e:
            self.main("add", "--unit", self.file(dict(self.UNIT, goal="Something else.")))
        self.assertIn("house-cover is queued already", str(e.exception))
        self.assertNotIn("the same unit", str(e.exception))
        self.assertEqual(self.queued().read_bytes(), before)

    def test_a_unit_the_queue_would_not_take_is_refused_and_leaves_no_file(self):
        gone = {k: v for k, v in self.UNIT.items() if k != "done_when"}
        for bad in (dict(raw="{ not json"), dict(unit=gone), dict(unit=dict(self.UNIT, lane="feature/x")),
                    dict(unit=dict(self.UNIT, done_when="python Tools/otr.py")), dict(unit=dict(self.UNIT, id="has a space")),
                    dict(unit=dict(self.UNIT, id="../up")), dict(unit=dict(self.UNIT, priority=1)), dict(unit=["a", "list"]),
                    dict(unit={k: v for k, v in self.UNIT.items() if k != "id"})):
            with self.assertRaises(SystemExit, msg=str(bad)):
                self.main("add", "--unit", self.file(**bad))
        with self.assertRaises(SystemExit):
            self.main("add", "--unit", str(self.tmp / "no-such-file.json"))
        with self.assertRaises(SystemExit) as e:                 # an id that is a path is refused before a file is written
            self.main("add", "--unit", self.file(dict(self.UNIT, id="../up")))
        self.assertIn("names no id", str(e.exception))
        self.assertEqual(list((self.board / "relay").rglob("*.json")), [])

    def test_a_file_and_words_together_are_refused_and_the_words_alone_still_queue(self):
        for more in (["u2"], ["--lane", "lane/show/x"], ["--goal", "x"], ["--done-when", "git", "status"]):
            with self.assertRaises(SystemExit, msg=str(more)):
                self.main("add", "--unit", self.file(), *more)
        with self.assertRaises(SystemExit) as e:
            self.main("add", "u2", "--lane", "lane/show/x", "--done-when", "git", "status")
        self.assertIn("--goal", str(e.exception))
        self.assertEqual(list((self.board / "relay" / "queue").glob("*.json")), [])
        code, out = self.main("add", "u2", "--lane", "lane/show/x", "--goal", "Add a.txt", "--done-when", "git", "status")
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(self.queued("u2").read_text(encoding="utf-8")),
                         {"id": "u2", "lane": "lane/show/x", "role": "lane", "goal": "Add a.txt", "done_when": ["git", "status"]})

    def test_a_push_that_failed_goes_out_when_the_same_unit_is_added_again(self):
        origin = self.tmp / "origin.git"
        git(["init", "-q", "--bare", str(origin)], self.tmp)
        git(["init", "-q"], self.board)
        git(["config", "user.email", "t@example.com"], self.board)
        git(["config", "user.name", "t"], self.board)
        git(["commit", "-q", "--allow-empty", "-m", "start"], self.board)
        branch = git(["rev-parse", "--abbrev-ref", "HEAD"], self.board).strip()
        git(["remote", "add", "origin", str(origin)], self.board)
        git(["push", "-q", "-u", "origin", branch], self.board)
        git(["remote", "set-url", "origin", str(self.tmp / "unplugged.git")], self.board)
        code, out = self.main("add", "--unit", self.file())
        self.assertEqual(code, 0)
        self.assertIn("Board: push failed: the commit is kept locally.", out)
        self.assertEqual(git(["log", "--format=%s"], origin).strip(), "start")
        git(["remote", "set-url", "origin", str(origin)], self.board)
        code, out = self.main("add", "--unit", self.file())
        self.assertEqual(code, 0)
        self.assertIn("house-cover is queued already, the same unit. Board: pushed.", out)
        self.assertEqual(git(["log", "-1", "--format=%s"], origin).strip(), "relay: queue house-cover")
        self.assertIn("Board: nothing to push.", self.main("add", "--unit", self.file())[1])


class Pace(Base):
    """The screen says what the day's pace allows by now, and when the next unit may start."""
    def at(self, hour, minute=0):
        return datetime.datetime.now().replace(hour=hour, minute=minute, second=0, microsecond=0)

    def test_the_screen_has_a_pace_line_under_the_day(self):
        lim = dict(self.limits(), week_usd=1000)                # as shipped: 11% a day, even over 24 hours
        self.queue("a")
        self.leg(10.0, "plan")
        out = day.lines(self.board, self.home, lim, self.ph, now=self.at(12))
        self.assertTrue(out[0].startswith("Today: about 1.0% of the week used by the relay, 10.0% left of a day's "
                                          "cap of 11.0%."), out[0])
        pace = [l for l in out if l.startswith("Pace: ")]
        self.assertEqual(pace, ["Pace: 5.5% of the week allowed by 12:00, about 4.5% of it free."])
        self.assertLess(out.index(pace[0]), [i for i, l in enumerate(out) if l.startswith("No run going")][0])
        self.assertFalse([l for l in out if len(l) > lim["day_line_chars"]], out)

    def test_ahead_of_the_pace_the_screen_says_when_the_next_unit_may_start(self):
        lim = dict(self.limits(), week_usd=1000)
        self.queue("a")
        self.leg(10.0, "plan")                                  # a unit usually costs 10 + 10: 2.0 points
        out = " ".join(l.strip() for l in day.lines(self.board, self.home, lim, self.ph, now=self.at(1)))
        self.assertIn("Pace: 0.5% of the week allowed by 01:00, and the day is about 0.5% ahead of that. "
                      "The next unit may start at 06:33.", out)
        self.done("a")                                          # nothing queued: no time is named
        out = " ".join(l.strip() for l in day.lines(self.board, self.home, lim, self.ph, now=self.at(1)))
        self.assertNotIn("The next unit", out)

    def test_with_no_pace_there_is_no_pace_line(self):
        self.leg(10.0, "plan")
        self.assertFalse([l for l in day.lines(self.board, self.home, self.lim, self.ph) if l.startswith("Pace")])


if __name__ == "__main__":
    unittest.main(verbosity=1)
