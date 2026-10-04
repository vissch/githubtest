#!/usr/bin/env python3
"""The run loop: pick a unit, plan it (one leg), execute the plan (one leg per part), check the result by script,
record it, and go on until a stop rule fires. Every decision here is a script's; the model only does the legs.

Stop rules: the run's time is up, the leg cap, nothing left to do, the owner's stop (relay.py stop), the checkout is
missing, busy or left dirty, a leg that cannot be trusted (timeout, compaction trip, not auto mode, no hooks, no
result), limits.json no_progress_units units in a row with no result, or any error. Whatever stops it, the stop is
recorded and a pipeline claim is released. The checkout is held (lock and leg marker) from the first unit to the stop
record, so git's push guard also covers a job a leg left running.
Stdlib only. ASCII only.
"""
import os, re, sys, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                                                # noqa: E402
import boardio, config, gitio, launch, legdir, papers, sources      # noqa: E402

NO_WINDOW = ("\n- This run cannot open a window (it was not started from the owner's desktop). A windowed Unity "
             "editor hangs here. Unity in batch mode works. Work that needs a window: end BLOCKED and say so.")
VERDICT = re.compile(r"^\W*RESULT\W+(done|blocked|failed)\b", re.I)


class Stop(Exception):
    pass


def said(report):
    """done | blocked | failed, from the first line of a leg's report (markdown around it is fine); else None."""
    m = VERDICT.match((report or "").lstrip())
    return m.group(1).lower() if m else None


class Run:
    def __init__(self, a):
        self.a = a
        over = {k: v for k, v in (("run_hours", a.hours), ("leg_minutes", a.leg_minutes),
                                  ("leg_budget_usd", a.leg_budget)) if v is not None}
        self.lim = config.limits(overrides=over)
        for k, v in over.items():
            if self.lim[k] != v:
                print("note: %s %s is outside its bounds; using %s" % (k, v, self.lim[k]))
        self.ph, self.style = config.phases(), config.style()
        self.station = P.station()
        self.board = P.board_dir()
        self.work = Path(a.work).resolve()
        self.home = legdir.home()
        self.run = "%s-%d" % (time.strftime("%Y%m%d-%H%M%S"), os.getpid())
        self.deadline = time.time() + self.lim["run_hours"] * 3600
        self.ctx = {"board": self.board, "work": self.work, "station": self.station, "skip": set(), "since": 0}
        self.legs, self.idle, self.claimed, self.ready = 0, 0, None, False
        self.heads, self.lanes, self.stop_file = {}, set(), stop_path(self.home)
        self.units, self.no_window = {}, launch.no_window()     # unit id -> PASS | FAIL | BLOCKED

    def preflight(self):
        """Once, before the run's first git write: nobody else is in the checkout, and git guards the pushes."""
        why = gitio.busy_reason(self.work, self.home, quiet=0 if self.a.no_quiet else self.lim["quiet_seconds"])
        if why:
            raise Stop("the work checkout cannot be used: " + why)
        gitio.install_prepush(self.work)
        self.heads = gitio.remote_heads(self.work)
        if self.no_window:
            print("note: this terminal cannot open a window, so a leg that needs a Unity window will end BLOCKED. "
                  "Start the run from a normal terminal for that work.", flush=True)
        if self.stop_file.exists():                         # a stop asked of an earlier run
            self.stop_file.unlink()
        self.ready = True

    def owner_stop(self):
        if self.stop_file.exists():
            raise Stop("stopped by the owner (relay.py stop)")

    # ----- what a leg may not have touched, checked by script after every leg -----
    def watch(self):
        board_skip = (".git", "__pycache__", "evidence")
        return {"heads": gitio.remote_heads(self.work), "door": gitio.door_state(self.work),
                "board": gitio.tree_hashes(self.board, board_skip),
                "tools": dict(gitio.tree_hashes(HERE), **{"pipeline/" + k: v for k, v in
                                                          gitio.tree_hashes(HERE.parent / "pipeline").items()})}

    def moved(self, before, after):
        """Branches on origin that moved, other than the lanes this run works on. Git's push guard refuses a leg
        every push but its lane, and other sessions push all day: this is a note for the record, not a stop."""
        own = {"refs/heads/" + l for l in self.lanes}
        return [r.replace("refs/heads/", "") for r in sorted(set(before) | set(after))
                if r not in own and before.get(r) != after.get(r)]

    def audit(self, before, unit):
        after, bad = self.watch(), []
        for what, label in (("board", "the board's"), ("tools", "the relay's own"), ("door", "git's push guard:")):
            bad += ["%s %s changed" % (label, k) for k in sorted(set(before[what]) | set(after[what]))
                    if before[what].get(k) != after[what].get(k)]
        return bad, self.moved(before["heads"], after["heads"])

    # ----- one leg -----
    def leg(self, unit, phase, body, plan=None):
        if time.time() > self.deadline:
            raise Stop("the run's %.3g hours are up" % self.lim["run_hours"])
        if self.a.max_legs and self.legs >= self.a.max_legs:
            raise Stop("the leg cap (%d) is reached" % self.a.max_legs)
        self.owner_stop()
        self.legs += 1
        d = launch.make_leg(self.run, self.legs, unit, phase, self.work, unit["lane"], self.board,
                            body + (NO_WINDOW if self.no_window else ""), self.lim, plan)
        print("leg %02d  %-7s %s" % (self.legs, phase, unit["id"]), flush=True)
        left, before = self.deadline - time.time(), self.watch()
        leg = launch.run_leg(d, self.lim, max(1, min(self.lim["leg_minutes"] * 60, left)), self.stop_file)
        broke, moved = self.audit(before, unit)
        boardio.leg_record(self.board, self.station, leg, moved_on_origin=moved,
                           style_problems=config.report_problems(leg.get("report") or "", self.style))
        why = ("broke a rule: " + "; ".join(broke[:4])) if broke else launch.ran_clean(leg)
        if why:
            snap = gitio.snapshot_dirty(self.work, d)
            if leg["state"] == "TIMEOUT" and time.time() >= self.deadline:
                why = "the run's %.3g hours are up (mid-leg)" % self.lim["run_hours"]
            if leg["state"] == "STOPPED" and self.stop_file.exists():
                raise Stop("stopped by the owner (relay.py stop --now), mid-leg %02d" % self.legs,
                           "uncommitted work saved in %s" % d if snap else "")
            raise Stop("leg %02d %s" % (self.legs, why), "uncommitted work saved in %s" % d if snap else "")
        return d, leg

    # ----- one unit: plan, execute, check -----
    def unit(self, unit):
        src = sources.load(unit["source"])
        why = gitio.busy_reason(self.work, self.home, quiet=0)        # the quiet check ran once, in preflight
        if why:
            raise Stop("the work checkout cannot be used: " + why)
        self.owner_stop()
        self.lanes.add(unit["lane"])
        gitio.take_lock(self.work, self.home, "relay %s %s" % (self.run, unit["id"]), unit["lane"])
        print("unit %s: lane %s (%s)" % (unit["id"], unit["lane"], gitio.switch_lane(self.work, unit["lane"])))
        self.ctx["since"] = time.time()
        unit = src.refresh(unit, self.ctx)
        if not unit:
            return
        if src.already_done(unit, self.ctx):
            print("unit %s: already done, no leg needed" % unit["id"])
            self.finish(src, unit, [], "done")
            return
        src.claim(unit)
        self.claimed = src
        remote_before, body = gitio.remote_head(self.work, unit["lane"]), src.body(unit)
        d, leg = self.leg(unit, "plan", body)
        plan_file = legdir.desk(d) / self.ph["plan"]["output"]
        plan = plan_file.read_text(encoding="utf-8") if plan_file.exists() else ""
        problems = papers.check_plan(plan, self.lim["plan_max_bytes"], self.work, self.lim["max_plan_parts"]) \
            if plan else ["the plan leg wrote no plan.md"]
        outcome = "failed" if problems else "done"
        if said(leg.get("report")) in ("blocked", "failed"):                  # the plan leg's own word counts
            outcome = said(leg.get("report"))
            problems = ["the plan leg reported %s" % outcome]
        if not problems:
            parts = papers.plan_steps(plan)
            for i, part in enumerate(parts, 1):
                d, leg = self.leg(unit, "execute", body, papers.plan_for_part(plan, parts, i))
                outcome = said(leg.get("report")) or "failed"
                if outcome != "done":
                    problems = ["execute leg %d of %d reported %s" % (i, len(parts), outcome)]
                    break
            if outcome == "done":
                problems = src.verify(unit, self.ctx)
        if gitio.dirty(self.work):
            gitio.snapshot_dirty(self.work, d)
            src.keep_note(unit, self.ctx, legdir.desk(d), self.lim)
            problems.append("uncommitted work was left behind (saved beside leg %02d)" % self.legs)
            self.finish(src, unit, problems, "failed")
            raise Stop("unit %s left uncommitted work in the checkout" % unit["id"], "patch: %s" % (d / "red.patch"))
        src.keep_note(unit, self.ctx, legdir.desk(d), self.lim)
        verdict = self.finish(src, unit, problems, outcome)
        moved = gitio.code_changed(self.work, remote_before, gitio.remote_head(self.work, unit["lane"]))
        self.idle = 0 if (verdict == "PASS" or moved) else self.idle + 1      # only pushed code counts
        print("unit %s: %s%s" % (unit["id"], verdict, "".join("\n  - " + p for p in problems)), flush=True)
        if self.idle >= self.lim["no_progress_units"]:
            raise Stop("%d units in a row brought no result and no pushed code" % self.idle)

    def finish(self, src, unit, problems, outcome):
        verdict = src.finish(unit, self.ctx, problems, outcome)
        self.claimed = None
        self.units[unit["id"]] = verdict
        if verdict != "PASS":
            self.ctx["skip"].add(unit["id"])
        return verdict

    # ----- the loop -----
    def loop(self):
        try:
            return self.run_units()
        finally:                                            # the checkout stays held until the stop is recorded
            if self.work.exists():
                gitio.release_lock(self.work, self.home)

    def run_units(self):
        reason, detail, code = "nothing left to do", "", 0
        try:
            while True:
                unit = sources.next_unit(self.a.sources, self.ctx)
                if not unit:
                    break
                if self.a.dry_run:
                    print("would run: %s %s (%s) in %s on %s" % (unit["source"], unit["id"], unit.get("todo", unit["role"]),
                                                                self.work.name, unit["lane"]))
                    return 0
                if not self.ready:
                    self.preflight()
                self.unit(unit)
        except Stop as s:
            reason, detail = s.args[0], (s.args[1] if len(s.args) > 1 else "")
        except KeyboardInterrupt:
            reason, code = "stopped by the owner (Ctrl+C)", 130
        except (SystemExit, Exception) as e:                # noqa: BLE001 - a run always ends with a record
            reason, detail, code = "error: %s" % (str(e) or type(e).__name__), type(e).__name__, 1
        if self.a.dry_run:
            print("would run: nothing (%s)" % reason)
            return code
        if self.claimed:                                    # a unit that never got a verdict: free the job
            try:
                self.claimed.release()
            except (SystemExit, Exception):                 # noqa: BLE001
                pass
        moved = []
        if self.ready:                                      # once more, after the last leg: what moved during the run
            try:
                moved = self.moved(self.heads, gitio.remote_heads(self.work))
            except Exception:                               # noqa: BLE001 - a run always ends with a record
                pass
        boardio.stop_record(self.board, self.station, self.run, reason, self.legs, detail, moved_on_origin=moved,
                            units=self.units)
        pushed = "not sent (--no-push)" if self.a.no_push else \
            boardio.push(self.board, "relay: run %s, %d legs, %s" % (self.run, self.legs, reason))
        print("STOP: %s. %d legs ran%s. Record on the board: %s."
              % (reason.rstrip("."), self.legs, tally(self.units), pushed), flush=True)
        if detail:
            print("  " + detail, flush=True)
        return code


def tally(units):
    """", 2 units: 1 PASS, 1 FAIL" for the stop line: how the units ended, so a failed unit is not hidden behind
    "nothing left to do"."""
    if not units:
        return ""
    kinds = sorted(set(units.values()), key=("PASS", "FAIL", "BLOCKED").index)
    return ", %d unit%s: %s" % (len(units), "" if len(units) == 1 else "s",
                                ", ".join("%d %s" % (list(units.values()).count(k), k) for k in kinds))


def stop_path(home):
    """The owner's stop request (relay.py stop): the runner ends before its next leg, or at once with --now."""
    return Path(home) / "stop.json"


def add_args(p):
    p.add_argument("--work", required=True, help="the work checkout legs run in (never a busy lane's checkout)")
    p.add_argument("--sources", default=list(sources.ORDER), type=lambda s: [x for x in s.split(",") if x])
    p.add_argument("--hours", type=float, help="stop the run after this long (default: limits.json run_hours)")
    p.add_argument("--leg-minutes", type=float, dest="leg_minutes")
    p.add_argument("--leg-budget", type=float, dest="leg_budget")
    p.add_argument("--max-legs", type=int, dest="max_legs", default=0)
    p.add_argument("--dry-run", action="store_true", dest="dry_run", help="say what would run; claim and start nothing")
    p.add_argument("--no-push", action="store_true", dest="no_push", help="do not commit and push the board")
    p.add_argument("--no-quiet", action="store_true", dest="no_quiet",
                   help="skip the check that the checkout saw no git activity lately (you know nobody is in it)")
