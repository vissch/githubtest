#!/usr/bin/env python3
"""Tests for the order of the relay's turns (sources.next_unit): the wide turn, stepping aside, parking, and the
screen that shows them. They play turns on a board that is a plain folder: the pick is the real one, a turn is the
plan-leg record the runner would write, and a try that does not pass is recorded by the lane source itself. No leg
runs, so a hundred turns take seconds.

The owner, 2026-10-12: "if things arent moving on the items that have priority the relays will move onto lower
priority items", "a system that covers the overall state of the game", "even the lower priority items should come
into light at some point".
Run: python -B -m unittest test_turns
"""
import json, os, random, shutil, sys, tempfile, time, unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import config, day, ledger, runner, sources      # noqa: E402
from sources import lane                 # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_BOARD", "TW_STATION", "TW_BRIEFS", "TW_NOTES", "TW_BOARD_ALSO")
T0 = 1767225600                          # 2026-01-01 00:00 UTC: the tests' own clock, a minute a step
FX, RULES, UNITS = "destruction-vfx-simulator", "balance-simulator", "character-simulator"


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="turns-test-"))
        self.board, self.home = self.tmp / "board", self.tmp / "home"
        (self.board / "relay" / "queue").mkdir(parents=True)
        self.old = {k: os.environ.get(k) for k in ENV}
        os.environ.pop("TW_BOARD_ALSO", None)
        os.environ.update(TW_RELAY_HOME=str(self.home), TW_BOARD=str(self.board), TW_STATION="desktop",
                          TW_BRIEFS=str(self.tmp / "decisions"), TW_NOTES=str(self.tmp / "notes"))
        self.lim, self.clock, self.legs = dict(config.limits()), 0, 0

    def tearDown(self):
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def tick(self):
        self.clock += 1
        return T0 + 60 * self.clock

    def queue(self, uid, priority=None, role="lane", lane_name="lane/show/x", kind=None, goal="Add a.txt"):
        u = {"id": uid, "lane": lane_name, "role": role, "goal": goal, "done_when": ["git", "status"]}
        if priority is not None:
            u["priority"] = priority
        if kind:
            u["kind"] = kind
        p = self.board / "relay" / "queue" / (uid + ".json")
        p.write_text(json.dumps(u), encoding="utf-8")
        at = self.tick()
        os.utime(p, (at, at))                               # when it was queued, on the tests' clock

    def ctx(self, run="r1", **kw):
        return dict({"board": self.board, "skip": set(), "run": run, "lim": self.lim,
                     "queued": lane.queued_at(self.board)}, **kw)

    def turn(self, unit, run="r1"):
        """The plan-leg record the runner writes when a unit's turn begins."""
        self.legs += 1
        d = self.board / "relay" / "desktop" / "legs"
        d.mkdir(parents=True, exist_ok=True)
        (d / ("%s-%04d.json" % (run, self.legs))).write_text(json.dumps(
            {"run": run, "leg": self.legs, "unit": unit["id"], "source": unit["source"], "role": unit["role"],
             "lane": unit["lane"], "kind": unit.get("kind"), "turn": unit.get("turn") or "", "phase": "plan",
             "started_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(self.tick()))}), encoding="utf-8")

    def end(self, unit, how, run="r1"):
        """PASS: the done file. FAIL or BLOCKED: the record the lane source keeps of the try."""
        if how == "PASS":
            p = self.board / "relay" / "done" / (unit["id"] + ".json")
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_text("{}", encoding="utf-8")
            rec = lane.tried_path(self.board, unit["id"])
            if rec.exists():
                rec.unlink()
        else:
            lane.finish(unit, self.ctx(run), ["it did not pass"], "blocked" if how == "BLOCKED" else "failed")

    def play(self, n, ends=None, run="r1", **kw):
        """n turns as a run would take them: [(unit id, "wide" or "")]. ends: unit id -> how each of its turns ends
        (default PASS), or a function of the unit."""
        out = []
        for _ in range(n):
            if kw.get("state"):                              # what the runner reads before a pick, the split too
                kw = dict(runner.pick_state(self.board, self.home, self.lim), state=True)
            unit = sources.next_unit(["lane"], self.ctx(run, **{k: v for k, v in kw.items() if k != "state"}))
            if not unit:
                break
            self.turn(unit, run)
            how = ends(unit) if callable(ends) else (ends or {}).get(unit["id"], "PASS")
            self.end(unit, how, run)
            out.append((unit["id"], unit.get("turn") or ""))
        return out


class Wide(Base):
    def test_every_third_turn_goes_to_a_game_area_whatever_its_priority_and_the_areas_take_it_in_turn(self):
        for n in range(1, 7):
            self.queue("fix%d" % n, 5, "review-fix")
        self.queue("tracers", 90, FX)
        self.queue("sweep", 95, RULES)
        self.assertEqual(self.play(8), [("fix1", ""), ("fix2", ""), ("tracers", "wide"), ("fix3", ""), ("fix4", ""),
                                        ("sweep", "wide"), ("fix5", ""), ("fix6", "")])
        self.assertEqual(self.lim["wide_every"], 3)          # as shipped; the owner, 2026-10-12

    def test_with_wide_turns_off_the_queue_runs_by_priority_alone(self):
        self.lim["wide_every"] = 0
        for n in range(1, 5):
            self.queue("fix%d" % n, 5, "review-fix")
        self.queue("tracers", 90, FX)
        self.assertEqual([u for u, _ in self.play(5)], ["fix1", "fix2", "fix3", "fix4", "tracers"])

    def test_inside_an_area_the_unit_that_waited_longest_goes_first_and_one_queued_later_never_passes_it(self):
        self.queue("old-smoke", 90, FX)
        for n in range(1, 4):
            self.queue("fix%d" % n, 5, "review-fix")
        self.queue("new-sparks", 1, FX, kind="fix")          # queued last, with the best priority there is
        got = self.play(3, ends={"new-sparks": "PASS"})
        self.assertEqual(got[2], ("old-smoke", "wide"))
        self.assertEqual(got[0][0], "new-sparks")            # by priority it is first, as before

    def test_an_area_that_turns_by_priority_keep_busy_is_not_passed_over_for_its_wide_turn(self):
        # the area's clock counts wide turns only: counted on every turn, an area that gets priority turns would
        # never be the one that waited longest, and its low units would never come up
        self.queue("fx-a", 1, FX)
        self.queue("fx-b", 1, FX)
        self.queue("fx-c", 99, FX)
        self.queue("fx-d", 99, FX)
        self.queue("rule-a", 99, RULES)
        self.queue("rule-b", 99, RULES)
        got = self.play(6)
        self.assertEqual(got[:2], [("fx-a", ""), ("fx-b", "")])
        self.assertEqual(got[2], ("fx-c", "wide"))           # effects first: its unit has waited longest
        self.assertEqual(got[5][1], "wide")
        self.assertEqual(got[5][0][:4], "rule")              # then the area whose wide turn is longest ago

    def test_review_fixes_and_tool_work_get_a_wide_turn_only_when_no_game_area_has_work(self):
        self.queue("tool-old", 99)                           # role lane: tools and other
        for n in range(1, 4):
            self.queue("fix%d" % n, 5, "review-fix")
        self.queue("squad", 50, UNITS)
        got = self.play(5)
        self.assertEqual(got[2], ("squad", "wide"))
        self.assertEqual(got[4], ("tool-old", ""))           # nothing else is left: it runs, by priority
        self.queue("fix8", 5, "review-fix")
        self.queue("fix9", 5, "review-fix")
        self.queue("tool-b", 99)
        self.assertEqual(self.play(3)[0], ("fix8", "wide"))  # no game work: the wide turn goes to the others

    def test_work_with_no_role_on_a_sim_lane_counts_as_the_games_rules(self):
        self.assertEqual(sources.area_of("lane", "lane", "lane/sim/frog-hopper-flight"), "rules")
        self.assertEqual(sources.area_of("lane", "lane", "lane/show/board"), "tools and other")
        self.assertEqual(sources.area_of("lane", "review-fix", "lane/sim/review-fixes-2"), "review fixes")
        self.assertEqual(sources.area_of("pipeline", "concept-artist", "lane/show/x"), "ideas")
        self.assertEqual(sources.area_of("lane", FX, "lane/show/tracers-side-by-side"), "effects")

    def test_the_wide_turn_takes_nothing_of_a_kind_with_no_share(self):
        self.queue("fix1", 5, "review-fix")
        self.queue("fix2", 5, "review-fix")
        self.queue("fix3", 5, "review-fix")
        self.queue("look", 90, FX, kind="critique")
        shares = {"finish": 60.0, "fix": 40.0, "critique": 0.0}
        got = self.play(4, shares=shares, spent_kinds={})
        self.assertNotIn("look", [u for u, _ in got])
        self.assertEqual(len(got), 3)

    def test_a_wide_turn_is_no_kinds_share_so_it_takes_no_turn_from_the_work_being_finished(self):
        # on a copy of the real board (2026-10-12) the wide turns, booked as finishing, used up finishing's 20
        # percent, and landing-small-lanes at priority 5 got no turn in thirty
        self.clock = (int(time.time()) - T0) // 60 - 10      # these legs are today's: the split counts a day
        self.turn({"id": "landing", "source": "lane", "role": "lane", "lane": "lane/show/landing"})
        self.turn({"id": "tracers", "source": "lane", "role": FX, "lane": "lane/show/tracers", "turn": "wide"})
        self.turn({"id": "fix1", "source": "lane", "role": "review-fix", "lane": "lane/show/review-fixes"})
        kinds = ledger.spent(self.board, lim=self.lim)["kinds"]
        self.assertEqual(sorted(kinds), ["finish", "fix", "wide"])
        self.assertEqual(kinds["wide"], kinds["finish"])     # the landing's turn alone is finishing's

    def test_a_turn_that_cost_no_leg_is_no_turn_and_the_wide_turn_is_still_owed(self):
        for n in range(1, 5):
            self.queue("fix%d" % n, 5, "review-fix")
        self.queue("tracers", 90, FX)
        self.queue("smoke", 91, FX)
        self.play(2)
        first = sources.next_unit(["lane"], self.ctx())
        self.assertEqual((first["id"], first["turn"]), ("tracers", "wide"))
        self.end(first, "PASS")                              # it was done already: no leg, so no turn was taken
        again = sources.next_unit(["lane"], self.ctx())
        self.assertEqual((again["id"], again["turn"]), ("smoke", "wide"))

    def test_a_queue_file_that_cannot_be_read_stops_no_wide_turn_and_still_stops_the_queue_at_its_own_turn(self):
        self.queue("fix1", 5, "review-fix")
        self.queue("fix2", 5, "review-fix")
        (self.board / "relay" / "queue" / "broken.json").write_text("{", encoding="utf-8")
        self.queue("tracers", 90, FX)
        self.assertEqual(self.play(3)[2], ("tracers", "wide"))
        with self.assertRaises(SystemExit):
            sources.next_unit(["lane"], self.ctx())


class Aside(Base):
    def fill(self, n):
        for i in range(n):
            self.queue("work%02d" % i, 50)

    def test_a_unit_that_does_not_pass_steps_aside_for_three_then_six_then_twelve_turns_and_parks_after_four_tries(self):
        self.lim["wide_every"] = 0                           # this one is about the count alone
        self.queue("stuck", 1)
        self.fill(40)
        got = [u for u, _ in self.play(40, ends={"stuck": "FAIL"})]
        self.assertEqual([i + 1 for i, u in enumerate(got) if u == "stuck"], [1, 5, 12, 25])
        rec = lane.tried(self.board, "stuck")
        self.assertEqual((rec["tries"], rec["verdict"]), (4, "FAIL"))
        self.assertEqual(lane.standing(rec, lane.load(self.board / "relay" / "queue" / "stuck.json"), self.lim,
                                       ledger.turns(self.board)), "parked")
        self.assertEqual((self.lim["aside_turns"], self.lim["park_after"]), (3, 4))

    def test_the_work_behind_a_unit_that_does_not_pass_runs_at_once_and_in_the_same_run(self):
        self.queue("stuck-a", 1)
        self.queue("stuck-b", 2)
        self.fill(3)
        got = [u for u, _ in self.play(4, ends={"stuck-a": "FAIL", "stuck-b": "BLOCKED"})]
        self.assertEqual(got, ["stuck-a", "stuck-b", "work00", "work01"])

    def test_a_unit_whose_leg_said_blocked_gets_one_more_try_and_is_parked_at_the_second(self):
        self.lim["wide_every"] = 0
        self.queue("needs-a-window", 1)
        self.fill(12)
        got = [u for u, _ in self.play(12, ends={"needs-a-window": "BLOCKED"})]
        self.assertEqual([i + 1 for i, u in enumerate(got) if u == "needs-a-window"], [1, 5])
        self.assertEqual(self.lim["park_after_blocked"], 2)

    def test_a_unit_that_passes_after_a_failed_try_leaves_no_record(self):
        self.lim["wide_every"] = 0
        self.queue("flaky", 1)
        self.fill(6)
        tries = iter(["FAIL", "PASS"])
        got = [u for u, _ in self.play(7, ends=lambda u: next(tries) if u["id"] == "flaky" else "PASS")]
        self.assertEqual([i + 1 for i, u in enumerate(got) if u == "flaky"], [1, 5])
        self.assertEqual(lane.tried(self.board, "flaky"), {})

    def test_when_only_units_stepping_aside_are_left_each_gets_one_try_in_a_run_and_never_two(self):
        self.queue("stuck-a", 1)
        self.queue("stuck-b", 2)
        ends = {"stuck-a": "FAIL", "stuck-b": "FAIL"}
        self.assertEqual([u for u, _ in self.play(9, ends, run="r1")], ["stuck-a", "stuck-b"])
        self.assertEqual([u for u, _ in self.play(9, ends, run="r2")], ["stuck-a", "stuck-b"])   # no other work: no
        self.assertEqual([u for u, _ in self.play(9, ends, run="r3")], ["stuck-a", "stuck-b"])   # turn would ever pass
        self.assertEqual([u for u, _ in self.play(9, ends, run="r4")], ["stuck-a", "stuck-b"])
        self.assertEqual(self.play(9, ends, run="r5"), [])   # four tries each: both are parked

    def test_a_changed_unit_file_takes_a_parked_unit_again_and_a_move_in_the_queue_does_not(self):
        self.queue("stuck", 1)
        for run in ("r1", "r2", "r3", "r4"):
            self.play(1, {"stuck": "FAIL"}, run=run)
        self.assertEqual(self.play(1, run="r5"), [])
        self.queue("stuck", 0)                               # relay.py prio: a move in the queue is no new unit
        self.assertEqual(self.play(1, run="r5"), [])
        self.queue("stuck", 0, goal="Add a.txt, split in two as he answered")
        self.assertEqual([u for u, _ in self.play(1, run="r5")], ["stuck"])


class Screen(Base):
    def lines(self, **kw):
        limits = config.limits                               # the screen in dollars, as test_day reads it
        config.limits = lambda *a, **k: dict(limits(*a, **k), week_usd=0, day_budget_pct=0, pace_to_hour=0)
        try:
            return day.lines(self.board, self.home, config.limits(), config.phases(), **kw)
        finally:
            config.limits = limits

    def test_the_screen_names_the_next_turns_as_the_run_takes_them_and_says_why(self):
        for n in range(1, 6):
            self.queue("fix%d" % n, 5, "review-fix", "lane/show/review-fixes")
        self.queue("tracers", 90, FX, "lane/show/tracers-side-by-side", kind="fix")
        out = self.lines()
        rows = [l.split() for l in out if l[:5].strip().rstrip(".").isdigit()]
        shown = [(r[1], "wide" if r[2] == "wide:" else "") for r in rows]
        self.assertIn("   3. tracers                            wide: effects lane/show/tracers-side-by-side", out)
        self.assertIn("Areas: effects 1 waiting (no wide turn yet); review fixes 5 waiting (no wide turn yet).", out)
        self.assertEqual(shown, self.play(6, state=True))    # and then the run takes them in that order

    def test_reading_the_next_turns_changes_nothing_on_the_board(self):
        for n in range(1, 4):
            self.queue("fix%d" % n, 5, "review-fix")
        before = sorted(str(p) for p in self.board.rglob("*"))
        ctx = self.ctx()
        got = sources.forecast(["lane"], ctx, 5)
        self.assertEqual([u["id"] for u in got], ["fix1", "fix2", "fix3"])
        self.assertEqual((ctx["skip"], sorted(str(p) for p in self.board.rglob("*"))), (set(), before))

    def test_a_unit_stepping_aside_is_listed_with_the_turns_until_it_returns_and_is_not_under_needs_you(self):
        self.queue("stuck", 1)
        for n in range(1, 4):
            self.queue("work%d" % n, 50)
        self.play(2, {"stuck": "FAIL"})
        out = self.lines()
        self.assertIn("Stepping aside (each comes back by itself): 1.", out)
        self.assertIn("  - stuck: FAIL, try 1, back after 2 more turns of other work", out)
        self.assertIn("Needs you: nothing.", out)

    def test_a_parked_unit_is_under_needs_you_once_and_in_no_list_of_turns(self):
        self.queue("stuck", 1)
        for run in ("r1", "r2", "r3", "r4"):
            self.play(1, {"stuck": "FAIL"}, run=run)
        self.queue("work1", 50)
        out = self.lines()
        self.assertEqual([l for l in out if "stuck" in l],
                         ["  - stuck did not pass; parked after 4 tries: rewrite, split or drop"])


class Bound(Base):
    """The rule's promise, played: while review fixes keep arriving in front, no unit of a game area waits longer
    than wide_every x the game areas x its place in its area. With wide turns off the same queue breaks it, so
    the check can fail."""
    AREAS = (FX, RULES, UNITS, "vehicle-simulator", "env-simulator")

    def waits(self, seed, turns=120):
        shutil.rmtree(self.board / "relay")                  # a board of its own for every seed
        (self.board / "relay" / "queue").mkdir(parents=True)
        rng = random.Random(seed)
        born, ahead, first, done = {}, {}, {}, set()

        def add(uid, role, priority):                        # ahead of it: every unit of its area that is not done
            ahead[uid] = sum(1 for u in born if u.split("-")[0] == uid.split("-")[0] and u not in done) \
                if role != "review-fix" else 0
            born[uid] = len(taken)
            self.queue(uid, priority, role)
        taken = []
        for i in range(10):
            add("fix-%03d" % i, "review-fix", 5)
        for i, role in enumerate(self.AREAS * 3):
            add("%s-%02d" % (role.split("-")[0], i), role, 60 + i)
        for n in range(turns):
            unit = sources.next_unit(["lane"], self.ctx())
            self.turn(unit)
            first.setdefault(unit["id"], len(taken))
            taken.append(unit["id"])
            how = "PASS" if rng.random() < 0.8 else "FAIL"
            if how == "PASS":
                done.add(unit["id"])
            self.end(unit, how)
            add("fix-%03d" % (100 + n), "review-fix", 5)     # the front of the queue never empties
            if n % 10 == 0:
                role = rng.choice(self.AREAS)
                add("%s-late%03d" % (role.split("-")[0], n), role, 60)
        game = [u for u in born if not u.startswith("fix-")]
        return [(u, (first[u] if u in first else turns) - born[u], ahead[u], u in first) for u in game]

    def test_no_unit_of_a_game_area_waits_longer_than_wide_every_times_the_areas_times_its_place(self):
        every, areas = self.lim["wide_every"], len(self.AREAS)
        for seed in (1, 2):
            for unit, waited, ahead, ran in self.waits(seed):
                bound = every * areas * (ahead + 1)
                if ran or waited > bound:                    # a unit queued near the end has not had its time yet
                    self.assertLessEqual(waited, bound, "%s (seed %d): %d turns, %d ahead of it" % (unit, seed, waited, ahead))

    def test_with_wide_turns_off_the_same_queue_leaves_the_game_areas_waiting_past_that_bound(self):
        every, areas = self.lim["wide_every"], len(self.AREAS)
        self.lim["wide_every"] = 0
        over = [u for u, waited, ahead, ran in self.waits(1) if waited > every * areas * (ahead + 1)]
        self.assertGreater(len(over), 10)


if __name__ == "__main__":
    unittest.main()
