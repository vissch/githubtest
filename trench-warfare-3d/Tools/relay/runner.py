#!/usr/bin/env python3
"""The run loop: pick a unit, plan it (one leg), execute the plan (one leg per part), check the result by script,
record it, and go on until a stop rule fires. Every decision here is a script's; the model only does the legs.

Stop rules: the run's time is up, the leg cap, the day's budget (limits.json day_budget_pct percent points of the
week, or day_budget_usd dollars while nothing can be said in percent; counted by ledger.py over every leg started
today and every agent booked for it), the day's pace when waiting for it would outlast the run, nothing left to do,
the owner's stop (relay.py stop), the checkout is missing, busy or left dirty, a leg that cannot be trusted
(timeout, compaction trip, not auto mode, no hooks, no result), limits.json no_progress_units units in a row with
no result, or any error. Whatever stops it, the stop is recorded and a pipeline claim is released. The checkout is
held (lock and leg marker) from the first unit to the stop record, so git's push guard also covers a job a leg left
running. A run that is ahead of the day's pace waits for it between legs, and keeps the checkout while it waits.
So that another machine can follow a run that is going, the board is committed and pushed at the run's start, after
every leg and at a unit's verdict (one short try: a push that fails is a note), with a live record of what the run
is on (boardio.live_record), which the stop record replaces.
Stdlib only. ASCII only.
"""
import datetime, json, os, re, subprocess, sys, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                                                # noqa: E402
import agents, boardio, config, gitio, launch, ledger, legdir, papers, sources   # noqa: E402

NO_WINDOW = ("\n- This run cannot open a window (it was not started from the owner's desktop). Only work that "
             "needs a visible window is blocked: end BLOCKED and say so. Unity itself works in batch mode; a "
             "windowed editor hangs here. "
             "Talk to that editor with the Unity CLI at %LOCALAPPDATA%/unity/bin/unity.exe, which is the one "
             "Tools/tw is written for. A different unity earlier on PATH refuses while Temp/UnityLockfile exists "
             "unless UNITY_CLI_ALLOW_LOCKED=1, and then it starts a second editor. unity status on the CLI in "
             "%LOCALAPPDATA%/unity/bin does list a batch editor.")
PACE_STEP = 30                          # seconds between two looks at the clock while a run waits for the pace
clock = datetime.datetime.now           # the wall clock the day's pace is read from
sleep = time.sleep
said = launch.said                      # the one rule for a report's verdict; run_leg reads it too


# The family of a stop reason, for the stop record's reason_kind: a page reads the word, not the sentence.
KINDS = ("done", "hours", "legs", "asked", "budget", "pace", "checkout", "leg", "uncommitted", "no-result", "lane",
         "error")


class Why(str):
    """A stop reason that knows its kind (KINDS). Every reason in this file is one: Stop takes nothing else."""
    def __new__(cls, kind, text):
        if kind not in KINDS:
            raise ValueError("no kind of stop named %s" % kind)
        self = super().__new__(cls, text)
        self.kind = kind
        return self


class Stop(Exception):
    def __init__(self, why, detail=""):
        if not isinstance(why, Why):
            raise TypeError("a stop reason needs its kind: Stop(Why(kind, text))")
        super().__init__(why, detail)


class Run:
    def __init__(self, a):
        self.a = a
        over = {k: v for k, v in (("run_hours", a.hours), ("leg_minutes", a.leg_minutes),
                                  ("leg_budget_usd", a.leg_budget),
                                  ("day_budget_usd", getattr(a, "day_budget", None)),
                                  ("day_budget_pct", getattr(a, "day_pct", None))) if v is not None}
        if "day_budget_usd" in over:
            over.setdefault("day_budget_pct", 0)                # a day given in dollars is counted in dollars
        self.ph, self.style = config.phases(), config.style()
        self.station = P.station()
        self.board = P.board_dir()
        tuned = {k: v for k, v in boardio.tuning(self.board, self.station).items() if k in config.RETRO_TUNES}
        self.lim = config.limits(overrides=dict(tuned, **over))     # a flag outranks the last retrospective
        for k, v in over.items():
            if self.lim[k] != v:
                print("note: %s %s is outside its bounds; using %s" % (k, v, self.lim[k]))
        self.tuned = {k: self.lim[k] for k in tuned if k not in over}
        self.base_pct = self.lim["day_budget_pct"]              # the run's own day figure: allot() may put another
        self.work = Path(a.work).resolve()
        self.home = legdir.home()
        self.run = "%s-%d" % (time.strftime("%Y%m%d-%H%M%S"), os.getpid())
        self.deadline = time.time() + self.lim["run_hours"] * 3600
        self.ctx = {"board": self.board, "work": self.work, "station": self.station, "skip": set(), "since": 0}
        self.legs, self.idle, self.claimed, self.ready = 0, 0, None, False
        self.heads, self.lanes, self.stop_file = {}, set(), stop_path(self.home)
        self.units, self.no_window = {}, launch.no_window()     # unit id -> PASS | FAIL | BLOCKED
        self.retro_at = 0                                       # the leg count at the last retrospective
        self.refused = 0                                        # commands the guard refused, over all legs
        self.who = getattr(a, "who", None) or os.environ.get("USERNAME") or "someone"
        self.code = ""                                          # the commit the relay's own code is at
        self.waited = False                                     # the last budget check waited for the day's pace
        self.stuck = 0                                          # units in a row whose lane could not be switched to
        self.started_at = P.now()
        self.problems = {}                                      # unit id -> why it ended FAIL or BLOCKED
        self.on, self.at = "", None                             # the unit in flight; what the live record says of it
        self.ended, self.run_usd, self.unpriced = 0, 0.0, 0     # legs that ended, their cost, how many named none
        self.asked_of = {}                                      # the why and the who of a stop somebody asked for

    def preflight(self):
        """Once, before the run's first git write: nobody else is in the checkout, and git guards the pushes."""
        why = gitio.busy_reason(self.work, self.home, quiet=0 if self.a.no_quiet else self.lim["quiet_seconds"])
        if why:
            raise Stop(Why("checkout", "the work checkout cannot be used: " + why))
        self.code, dirty = gitio.code_state(HERE)
        if dirty and not getattr(self.a, "allow_dirty", False):
            raise Stop(Why("checkout", "the relay's own code has uncommitted changes (%s): a run uses committed code "
                           "only. Run from the frozen copy (githubtest-relay-run), or commit them"
                           % ", ".join(dirty[:3])))
        gitio.install_prepush(self.work)
        self.heads = gitio.remote_heads(self.work)
        print("run %s, started by %s, relay code %s" % (self.run, self.who, self.code[:10] or "not in git"), flush=True)
        if self.tuned:
            print("note: limits set by the last retrospective: %s"
                  % ", ".join("%s %g" % kv for kv in sorted(self.tuned.items())), flush=True)
        if self.no_window:
            print("note: this terminal cannot open a window. Unity batch mode still works; only work that needs a "
                  "visible window will end BLOCKED (start the run from a normal terminal for that).", flush=True)
        if self.stop_file.exists():                         # a stop asked of an earlier run
            self.stop_file.unlink()
        self.ready = True
        self.live()
        self.send("started")                                # so the run shows elsewhere before its first leg ends

    def book_agents(self):
        """Between units: count what the agents cost that this machine's sessions spawned outside the relay today,
        keep the sum on the board, and take up the bookings other machines handed in (agents.py). The day's budget
        is every agent's. It is never a reason to stop a run."""
        try:
            agents.take_in(self.board, self.home)
            agents.book(self.board, self.station, by="run " + self.run)
        except (SystemExit, Exception) as e:                # noqa: BLE001
            print("note: the agents outside the relay were not booked (%s)" % (str(e) or type(e).__name__), flush=True)

    def allot(self):
        """Between units, before the next one is picked: the day's figure and the split as they stand NOW, and what
        each kind of work has spent today. The figure is the run's own unless <home>/day.json names today and
        another one ({"day": "2026-10-11", "pct": 21}): a raise the owner gives for one day starts at the next unit
        and is gone at midnight with nobody taking it back. The split is limits.json share_* unless
        <home>/shares.json holds a whole one ({"finish": 60, "fix": 25, "critique": 15})."""
        def read(name):
            try:
                rec = P.read_json(self.home / name)
            except (OSError, ValueError):
                rec = None
            return rec if isinstance(rec, dict) else {}
        rec, pct = read("day.json"), self.base_pct
        if rec.get("day") == time.strftime("%Y-%m-%d") and isinstance(rec.get("pct"), (int, float)) \
                and not isinstance(rec.get("pct"), bool):
            pct = config.limits(overrides={"day_budget_pct": rec["pct"]})["day_budget_pct"]
        if pct != self.lim["day_budget_pct"]:
            print("note: the day's cap is %g%% from here (%s)"
                  % (pct, "day.json names today" if pct != self.base_pct else "the run's own again"), flush=True)
            self.lim["day_budget_pct"] = pct
        self.ctx["shares"] = config.shares(self.lim, read("shares.json"))
        try:
            self.ctx["spent_kinds"] = dict(ledger.spent(self.board, lim=self.lim).get("kinds") or {})
        except (SystemExit, Exception):                     # noqa: BLE001 - a day that cannot be read splits nothing
            self.ctx["spent_kinds"] = {}

    def card_round(self):
        """Between units: what the owner's page needs of the board while a run is going. TW_BETWEEN_UNITS is a JSON
        list of commands (each a list of words), run with the board as the working folder; what they change under
        items/, results/ and feedback/ is committed and pushed. A run lasts up to twelve hours, and the board is
        not to be written under a leg: on 2026-10-10 a whole night of passed steps became the owner's cards in one
        round, and his answers waited for the run to end. It is never a reason to stop a run."""
        try:
            cmds = json.loads(os.environ.get("TW_BETWEEN_UNITS") or "[]")
        except ValueError:
            cmds = []
        if not cmds or self.a.no_push:
            return
        try:
            for cmd in cmds:
                r = subprocess.run([str(w) for w in cmd], cwd=str(self.board), capture_output=True, text=True,
                                   stdin=subprocess.DEVNULL, encoding="utf-8", errors="replace", timeout=600)
                last = ((r.stdout or "").strip().splitlines() or [""])[-1]
                print("card round: %s: %s" % (Path(str(cmd[1 if len(cmd) > 1 else 0])).name + " " + " ".join(
                    str(w) for w in cmd[2:4]), last[:160] if r.returncode == 0 else "exit %d: %s" % (
                        r.returncode, ((r.stderr or "").strip().splitlines() or [last])[-1][:160])), flush=True)
            gitio.git(["add", "--", "items", "results", "feedback"], self.board, check=False)
            if gitio.git_raw(["diff", "--cached", "--quiet"], self.board).returncode:
                gitio.git(["commit", "-q", "-m", "ideas: the card round of run %s" % self.run], self.board)
                print("card round: board %s" % boardio.push_kept(self.board), flush=True)
        except (SystemExit, Exception) as e:                # noqa: BLE001
            print("note: the card round was not finished (%s)" % (str(e) or type(e).__name__), flush=True)

    def asked(self, flag="", tail=""):
        """The reason for a stop somebody asked for (relay.py stop). With no why it reads as it always has: scripts
        match those words. A why is named in it, with who asked: a watcher whose time is up is not the owner."""
        try:
            rec = P.read_json(self.stop_file)
        except (OSError, ValueError):
            rec = None
        rec = rec if isinstance(rec, dict) else {}
        why, by = (" ".join(str(rec.get(k) or "").split())[:300] for k in ("why", "by"))
        self.asked_of = {"asked_why": why, "asked_by": by}
        how = "(relay.py stop%s)%s" % (flag, tail)
        if not why:
            return Why("asked", "stopped by the owner " + how)
        return Why("asked", "stopped %s %s: %s" % ("by " + by if by else "on request", how, why))

    def owner_stop(self):
        if self.stop_file.exists():
            raise Stop(self.asked())

    # ----- what another machine sees of the run while it is going -----
    def live(self, paced=""):
        """Write the run's live record on the board: how far it is and what it is on (self.at). Only ever between
        two legs' watches, like every write of the runner's on the board: a leg is blamed for a board file that
        changes while it works. Never a reason to stop a run."""
        if not self.ready:
            return
        try:
            boardio.live_record(self.board, self.station, self.run, started_at=self.started_at,
                                started_by=self.who, code=self.code, hours=self.lim["run_hours"],
                                leg_minutes=self.lim["leg_minutes"], day_budget_pct=self.lim["day_budget_pct"],
                                legs=self.ended, units=self.units, now_on=self.at, paced_until=paced)
        except Exception as e:                              # noqa: BLE001
            print("note: the run's live record was not written (%s)" % (str(e) or type(e).__name__), flush=True)

    def send(self, what):
        """Commit and push the board between two legs, so the run can be followed from another machine: one short
        try. A push that fails is a note: the commit stays here and goes out with the next push."""
        if self.a.no_push:
            return
        try:
            said = boardio.push(self.board, "relay: run %s, %s" % (self.run, what), tries=1, wait=boardio.QUICK_S,
                                leave=("relay/queue",))
        except Exception as e:                              # noqa: BLE001
            said = str(e) or type(e).__name__
        if said not in ("pushed", "nothing to push", "no board repo"):
            print("note: the board was not sent (%s): it goes out with the next push" % said, flush=True)

    def keep(self, what):
        """Commit the board on this machine and send nothing. As a leg starts: the board clone then holds no
        changed file of the runner's while the leg works (git refuses anybody's pull there over one), and what the
        run is on goes out with whatever is pushed from this clone next."""
        if self.a.no_push:
            return
        try:
            boardio.keep(self.board, "relay: run %s, %s" % (self.run, what), leave=("relay/queue",))
        except Exception as e:                              # noqa: BLE001
            print("note: the board was not committed (%s)" % (str(e) or type(e).__name__), flush=True)

    def lane_head(self, unit):
        """The head of the unit's lane in the work checkout, '' when the unit has no lane or it cannot be read."""
        try:
            return gitio.git(["rev-parse", "-q", "--verify", "HEAD"], self.work, check=False) if unit.get("lane") else ""
        except OSError:
            return ""

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
                    if before[what].get(k) != after[what].get(k)
                    and not (what == "board" and k.startswith("relay/queue/"))]   # the owner may queue mid-run:
        # a queue file counts only once it is committed on the board (sources/lane.py), which a leg cannot do unseen
        return bad, self.moved(before["heads"], after["heads"])

    def progress(self, now_on=""):
        """What `relay.py status` shows while the run is going: who started it, the units so far, what it is on."""
        P.write_json(legdir.leg_path(self.run, 1).parents[1] / "progress.json",
                     {"run": self.run, "started_by": self.who, "code": self.code, "legs": self.legs, "now_on": now_on,
                      "units": self.units, "refusals": self.refused})

    def day(self):
        """The day's budget as ledger.py counts it now: cap, spent, left, what the pace allows. None: no budget."""
        return ledger.day_budget(self.board, self.lim, now=clock())

    def day_left(self):
        """What is left of the day's budget, in dollars of cost, or None when no day budget is set."""
        b = self.day()
        return b["left_usd"] if b else None

    def no_budget(self, phases, unit=None):
        """Why the day's budget does not let legs of these phases start at their usual cost, or None. Asked before
        a leg starts: a leg that runs out of budget half way leaves work nobody can use.
        The day's cap is spent evenly over the day (ledger.pace_share). When the day covers the legs and the pace
        does not yet, the run waits here for the pace, when that time comes before the run's own time is up, and
        then asks the day again. A dry run does not wait, and a wait that would outlast the run does not start:
        both say the time."""
        price = ledger.usuals(self.board, lim=self.lim)
        need = ledger.need(price, config.routed(self.ph, unit) if unit else self.ph, phases)   # a routed leg at its own price
        what = "a %s leg" % phases[0] if len(phases) == 1 else "a unit"
        told = False
        while True:
            b = self.day()
            if b is None:
                return None
            if b["left"] <= 0:
                return Why("budget", "the day's budget is spent (%s of %s)"
                           % (ledger.amount(b, b["spent"]), ledger.amount(b, b["cap"], of=True)))
            if b["left_usd"] < need:
                return Why("budget", "the day's budget has %s left of %s, and %s usually costs %s%s"
                           % (ledger.amount(b, b["left"]), ledger.amount(b, b["cap"], of=True), what,
                              "about " if b["unit"] == "pct" else "", ledger.amount(b, need, usd=True)))
            at = ledger.pace_at(b, need, self.lim, clock()) if phases else None
            if at is None or b["free_usd"] >= need:             # no pace, or it covers the legs now
                if told and self.ready:                         # the wait is over: say that elsewhere too
                    self.live()
                    self.send("the wait for the day's pace is over")
                return None
            wait, when = (at - clock()).total_seconds(), at.strftime("%H:%M")
            if self.a.dry_run:
                return Why("pace", "the day's pace lets %s start at %s" % (what, when))
            if time.time() + wait > self.deadline:
                return Why("pace", "the day's pace lets %s start at %s, after this run's %.3g hours are up"
                           % (what, when, self.lim["run_hours"]))
            if self.stop_file.exists():
                return self.asked()
            if not told:
                told = True
                print("paced: %s may start at %s; the day's budget is spent evenly over the day" % (what, when),
                      flush=True)
                if self.ready:
                    self.progress("waiting for the day's pace until %s" % when)
                    self.live(paced=when)                       # a wait can last hours: say so elsewhere too
                    self.send("waiting for the day's pace until %s" % when)
            self.waited = True
            sleep(max(1.0, min(PACE_STEP, wait)))

    def no_room(self, phase=None, unit=None):
        """Why no further leg may start (the time, the leg cap, the owner's stop, the day's budget), or None.
        With a phase, the budget must also cover the usual cost of such a leg."""
        if time.time() > self.deadline:
            return Why("hours", "the run's %.3g hours are up" % self.lim["run_hours"])
        if self.a.max_legs and self.legs >= self.a.max_legs:
            return Why("legs", "the leg cap (%d) is reached" % self.a.max_legs)
        if self.stop_file.exists():
            return self.asked()
        return self.no_budget([phase] if phase else [], unit)

    # ----- one leg -----
    def leg(self, unit, phase, body, plan=None, fill=None):
        """One leg. fill (a function): the leg works in a folder of its own under its desk, which fill fills, and
        is not pointed at the board: a blind critic."""
        why = self.no_room(phase, unit)
        if why:
            raise Stop(why)
        self.legs += 1
        work, board = (legdir.desk_path(self.run, self.legs) / "bundle", "") if fill else (self.work, self.board)
        d = launch.make_leg(self.run, self.legs, unit, phase, work, unit["lane"], board,
                            body + (NO_WINDOW if self.no_window and not fill else ""), self.lim, plan)
        if phase in ("plan", "execute"):
            place = getattr(sources.load(unit["source"]), "place_brief", None)
            if place:
                place(unit["role"], legdir.desk(d))
        if fill:
            fill(work)
        print("leg %02d  %-7s %s" % (self.legs, phase, unit["id"]), flush=True)
        self.progress("%s, leg %02d (%s)" % (unit["id"], self.legs, phase))
        self.on, on = unit["id"], {"unit": unit["id"], "role": unit["role"], "lane": unit["lane"]}
        self.at = dict(on, phase=phase, leg=self.legs, since=P.now())
        self.live()                                         # written before the watch below, never after, and
        self.keep("leg %02d %s %s starts" % (self.legs, phase, unit["id"]))    # committed here: no push as a leg starts
        if getattr(self.a, "view", False) and not self.no_window:
            launch.open_view(d)
        left, head, before = self.deadline - time.time(), self.lane_head(unit), self.watch()
        lim, day = self.lim, self.day_left()
        if day is not None and not 0 < lim["leg_budget_usd"] <= day:     # the leg may spend what the day has left
            lim = dict(lim, leg_budget_usd=round(max(day, 0.01), 2))
        leg = launch.run_leg(d, lim, max(1, min(self.lim["leg_minutes"] * 60, left)), self.stop_file)
        broke, moved = self.audit(before, unit)
        self.refused += leg.get("guard_refusals") or 0
        self.ended += 1
        cost = leg.get("cost_usd")
        if isinstance(cost, (int, float)) and not isinstance(cost, bool):
            self.run_usd += cost
        else:
            self.unpriced += 1
        after = self.lane_head(unit)
        new, more = gitio.commits(self.work, head, after)
        leg = dict(leg, head_before=head, head_after=after, commits=new, commits_more=more,
                   needs_you=config.needs_you(leg.get("report") or "", self.style))
        boardio.leg_record(self.board, self.station, leg, moved_on_origin=moved,
                           style_problems=config.report_problems(leg.get("report") or "", self.style))
        bad = ("broke a rule: " + "; ".join(broke[:4])) if broke else launch.ran_clean(leg)
        if bad:
            saved = "uncommitted work saved in %s" % d if gitio.snapshot_dirty(self.work, d) else ""
            if leg["state"] == "STOPPED" and self.stop_file.exists():
                raise Stop(self.asked(" --now", ", mid-leg %02d" % self.legs), saved)
            if leg.get("subtype") == "error_max_budget_usd" and lim is not self.lim:
                b = self.day()
                raise Stop(Why("budget", "leg %02d the day's budget is spent (%s, mid-leg)"
                               % (self.legs, ledger.amount(b, b["cap"], of=True) if b else "$0")), saved)
            if leg["state"] == "TIMEOUT" and time.time() >= self.deadline:
                raise Stop(Why("hours", "leg %02d the run's %.3g hours are up (mid-leg)"
                               % (self.legs, self.lim["run_hours"])), saved)
            raise Stop(Why("leg", "leg %02d %s" % (self.legs, bad)), saved)
        if phase == "retro":                                # a retrospective is no unit: nothing is in flight after it
            self.on, self.at = "", None
        else:                                               # between two legs of the unit: no phase, leg 0
            self.at = dict(on, phase="", leg=0, since=P.now())
        self.live()
        self.send("leg %02d %s %s" % (self.legs, phase, unit["id"]))
        return d, leg

    # ----- one unit: plan, execute, check -----
    def unit(self, unit):
        src = sources.load(unit["source"])
        busy = gitio.busy_reason(self.work, self.home, quiet=0)        # the quiet check ran once, in preflight
        if busy:
            raise Stop(Why("checkout", "the work checkout cannot be used: " + busy))
        self.owner_stop()
        self.lanes.add(unit["lane"])
        gitio.take_lock(self.work, self.home, "relay %s %s" % (self.run, unit["id"]), unit["lane"])
        try:
            how = gitio.switch_lane(self.work, unit["lane"])
        except gitio.GitError as e:
            # A lane this checkout cannot switch to (another checkout has the branch out) fails its own unit, not
            # the run: nothing is claimed and no leg has run, so the next unit in the queue is taken. Not through
            # src.finish: a pipeline job that was never claimed cannot be completed.
            self.units[unit["id"]] = "FAIL"
            self.ctx["skip"].add(unit["id"])
            self.keep_problems(unit, ["its lane %s cannot be switched to: %s" % (unit["lane"], e)])
            self.progress()
            self.live()
            self.stuck += 1
            print("unit %s: FAIL\n  - its lane %s cannot be switched to: %s" % (unit["id"], unit["lane"], e), flush=True)
            if self.stuck >= self.lim["stuck_lanes"]:
                raise Stop(Why("lane", "%d units in a row whose lane cannot be switched to" % self.stuck), str(e))
            return
        self.stuck = 0
        print("unit %s: lane %s (%s)" % (unit["id"], unit["lane"], how))
        self.ctx["since"] = time.time()
        unit = src.refresh(unit, self.ctx)
        if not unit:
            return
        if src.already_done(unit, self.ctx):
            print("unit %s: already done, no leg needed" % unit["id"])
            self.finish(src, unit, [], "done")
            return
        self.waited = False
        why = self.no_budget(["plan", "execute"], unit)      # asked before the plan: a plan with no execute is lost
        if why:
            raise Stop(why)
        if self.waited:                                     # the queue's order may have changed while it waited:
            print("unit %s: the wait is over, the queue is read again" % unit["id"], flush=True)
            return                                           # the loop picks the unit that is first now
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
            if outcome == "done" and not problems:
                problems = self.critique(src, unit, body, plan)
        if gitio.dirty(self.work):
            gitio.snapshot_dirty(self.work, d)
            src.keep_note(unit, self.ctx, legdir.desk(d), self.lim)
            problems.append("uncommitted work was left behind (saved beside leg %02d)" % self.legs)
            self.finish(src, unit, problems, "failed")
            raise Stop(Why("uncommitted", "unit %s left uncommitted work in the checkout" % unit["id"]),
                       "patch: %s" % (d / "red.patch"))
        src.keep_note(unit, self.ctx, legdir.desk(d), self.lim)
        verdict = self.finish(src, unit, problems, outcome)
        moved = gitio.code_changed(self.work, remote_before, gitio.remote_head(self.work, unit["lane"]))
        self.idle = 0 if (verdict == "PASS" or moved) else self.idle + 1      # only pushed code counts
        print("unit %s: %s%s" % (unit["id"], verdict, "".join("\n  - " + p for p in problems)), flush=True)
        if self.idle >= self.lim["no_progress_units"]:
            raise Stop(Why("no-result", "%d units in a row brought no result and no pushed code" % self.idle))

    def critique(self, src, unit, body, plan):
        """Critic rounds on work the script checks passed. A blind leg scores the evidence; under the target, one
        execute leg does the three mandated fixes and the critic scores again (limits.json critic_rounds in all).
        The verdict stays the script's: the scores go in the result's note, the rounds beside the evidence, a row
        per round in the lessons. Returns the problems a fix round left (empty = still a PASS)."""
        target, last, scores, note = self.lim["critic_target"], self.lim["critic_rounds"], [], ""
        problems = []
        for r in range(1, last + 1):
            brief = src.critic(unit, self.ctx, r)
            if not brief:
                return []
            if self.no_room("critic"):                      # work that passed its checks keeps its verdict
                note = "no critic round %d: %s" % (r, self.no_room("critic"))
                break
            d, leg = self.leg(unit, "critic", brief["body"], fill=brief["fill"])
            f = legdir.desk(d) / self.ph["critic"]["output"]
            text = f.read_text(encoding="utf-8") if f.exists() else ""
            bad = papers.check_critic(text, self.lim["critic_max_bytes"]) if text else ["the critic leg wrote no critic.md"]
            if bad:
                note = "critic round %d gave no score (%s)" % (r, bad[0])
                break
            score, fixes = papers.critic_score(text), papers.critic_fixes(text)
            src.keep_critic(unit, self.ctx, r, text)
            boardio.lesson(self.board, self.station, unit, r, score, target, fixes[0])
            scores.append(score)
            if score >= target or r == last:
                break
            if self.no_room("execute"):
                note = "no fix round: %s" % self.no_room("execute")
                break
            self.ctx["since"] = time.time()                 # a fix round makes its evidence again
            d, leg = self.leg(unit, "execute", body, papers.plan_for_fixes(plan, fixes, r, score, target))
            if said(leg.get("report")) != "done":
                problems = ["the fix round after critic round %d reported %s" % (r, said(leg.get("report")) or "nothing")]
                break
            problems = src.verify(unit, self.ctx)
            if problems:
                break
        if scores:
            note = ("critic %s (target %d)" % (", then ".join("%d/100" % s for s in scores), target)
                    + ("; the fix round scored lower: round %d was the best" % (scores.index(max(scores)) + 1)
                       if scores[-1] < max(scores) else "") + ("; " + note if note else ""))
        if note:
            unit["critic_note"] = note
            print("unit %s: %s" % (unit["id"], note), flush=True)
        return problems

    # ----- the retrospective: between units, every limits.json retro_every_legs legs -----
    def retro(self):
        """One leg reads this run's leg records, refusals and critic rounds, and writes retro.md. Its tuning moves
        amber, red and the leg time inside their bounds, for the rest of this run and the next ones on this
        station; its proposals go to the board for the owner. A paper that fails its check changes nothing."""
        n, run_root = self.legs, legdir.leg_path(self.run, 1).parent
        shipped = HERE / "limits.json"

        def fill(dst):
            (dst / "legs").mkdir(parents=True)
            (dst / "denials").mkdir()
            for f in sorted((boardio.folder(self.board, self.station) / "legs").glob(self.run + "-*.json")):
                (dst / "legs" / f.name).write_bytes(f.read_bytes())
            for f in sorted(run_root.glob("*/denials.jsonl")):
                (dst / "denials" / (f.parent.name + ".jsonl")).write_bytes(f.read_bytes())
            lessons = boardio.folder(self.board, self.station) / "lessons.md"
            if lessons.exists():
                (dst / "lessons.md").write_bytes(lessons.read_bytes())
            (dst / "limits.json").write_bytes(shipped.read_bytes())
            P.write_json(dst / "limits-now.json", {k: self.lim[k] for k in config.RETRO_TUNES})

        unit = {"id": "retro-%s-%02d" % (self.run, n + 1), "source": "retro", "role": "retro", "lane": ""}
        body = "\n".join([
            "# Retrospective after %d legs of run %s" % (n, self.run),
            "Your working folder holds: legs/ (one record per leg of this run), denials/ (what the guard refused, "
            "per leg), lessons.md (critic rounds, if any), limits.json (the settings and their bounds) and "
            "limits-now.json (the values in force now).",
            "You may tune only: %s." % ", ".join(config.RETRO_TUNES)])
        d, leg = self.leg(unit, "retro", body, fill=fill)
        self.retro_at = self.legs
        f = legdir.desk(d) / self.ph["retro"]["output"]
        text = f.read_text(encoding="utf-8") if f.exists() else ""
        bad = papers.check_retro(text, self.lim["retro_max_bytes"]) if text else ["the leg wrote no retro.md"]
        if bad:
            print("retrospective: nothing taken (%s)" % bad[0], flush=True)
            return
        tune = papers.retro_tuning(text, config.RETRO_TUNES)
        if tune:
            try:
                new = config.limits(overrides=dict({k: self.lim[k] for k in config.TUNABLE}, **tune))
            except SystemExit as e:                         # amber over red: keep what we have
                print("retrospective: tuning refused (%s)" % e, flush=True)
            else:
                moved = {k: new[k] for k in tune if new[k] != self.lim[k]}
                self.lim.update({k: new[k] for k in config.RETRO_TUNES})
                boardio.keep_tuning(self.board, self.station, self.run, {k: self.lim[k] for k in config.RETRO_TUNES})
                print("retrospective: %s" % (", ".join("%s is now %g" % kv for kv in sorted(moved.items()))
                                             or "the tuning it asked for is what is in force"), flush=True)
        words = papers.sections(text).get("Proposals", "")
        if words.strip():
            p = boardio.proposals(self.board, self.run, self.legs, "# Proposals from the retrospective of run %s, "
                                  "leg %02d\n\n%s" % (self.run, self.legs, words))
            print("retrospective: proposals for the owner in %s" % p, flush=True)

    def keep_problems(self, unit, problems):
        """Why a unit ended FAIL or BLOCKED, for the stop record: the lines the run prints, kept short."""
        self.problems[unit["id"]] = [str(p)[:300] for p in problems[:8]]

    def finish(self, src, unit, problems, outcome):
        verdict = src.finish(unit, self.ctx, problems, outcome)
        self.claimed = None
        self.units[unit["id"]] = verdict
        ran, self.on, self.at = self.on == unit["id"], "", None     # it has its verdict: no longer in flight
        self.progress()
        if verdict != "PASS":
            self.ctx["skip"].add(unit["id"])
            self.keep_problems(unit, problems)
        self.live()
        if ran:                                              # a unit that cost no leg goes out with the next push
            self.send("unit %s %s" % (unit["id"], verdict))
        return verdict

    # ----- the loop -----
    def loop(self):
        try:
            return self.run_units()
        finally:                                            # the checkout stays held until the stop is recorded
            if self.work.exists():
                gitio.release_lock(self.work, self.home)

    def run_units(self):
        reason, detail, code = Why("done", "nothing left to do"), "", 0
        try:
            while True:
                if self.ready and not self.a.dry_run:
                    self.card_round()                       # before the pick: his answer of this hour changes it
                self.allot()
                unit = sources.next_unit(self.a.sources, self.ctx)
                if not unit:
                    break
                if self.a.dry_run:
                    why = self.no_budget(["plan", "execute"], unit)
                    if why:
                        raise Stop(why)
                    print("would run: %s %s (%s) in %s on %s" % (unit["source"], unit["id"], unit.get("todo", unit["role"]),
                                                                self.work.name, unit["lane"]))
                    return 0
                if not self.ready:
                    self.preflight()
                self.book_agents()
                self.unit(unit)
                if self.legs - self.retro_at >= self.lim["retro_every_legs"] and not self.no_room("retro"):
                    self.retro()
        except Stop as s:
            reason, detail = s.args
        except KeyboardInterrupt:
            reason, code = Why("asked", "stopped by the owner (Ctrl+C)"), 130
        except (SystemExit, Exception) as e:                # noqa: BLE001 - a run always ends with a record
            reason, detail, code = Why("error", "error: %s" % (str(e) or type(e).__name__)), type(e).__name__, 1
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
        b = self.day()
        in_pct = dict(day_pct=round(b["spent"], 2), day_budget_pct=b["cap"]) if b and b["unit"] == "pct" else {}
        more = dict(in_pct, **(self.asked_of if reason.kind == "asked" else {}))    # who asked, and what for
        if self.a.max_legs:
            more["max_legs"] = self.a.max_legs
        boardio.stop_record(self.board, self.station, self.run, str(reason), self.legs, detail, moved_on_origin=moved,
                            units=self.units, refusals=self.refused, started_by=self.who, code=self.code,
                            day_usd=round(ledger.spent(self.board, lim=self.lim)["usd"], 2),
                            day_budget_usd=self.lim["day_budget_usd"], reason_kind=reason.kind,
                            started_at=self.started_at, hours=self.lim["run_hours"],
                            run_usd=round(self.run_usd, 4), run_usd_unpriced=self.unpriced, now_on=self.on,
                            problems=self.problems, **more)
        pushed = "not sent (--no-push)" if self.a.no_push else \
            boardio.push(self.board, "relay: run %s, %d legs, %s" % (self.run, self.legs, reason))
        print("STOP: %s. %d legs ran%s. Record on the board: %s."
              % (reason.rstrip("."), self.legs, tally(self.units), pushed), flush=True)
        if detail:
            print("  " + detail, flush=True)
        print("  " + ledger.one_line(self.board, self.lim["day_budget_usd"], lim=self.lim), flush=True)
        if not self.no_window:
            launch.notify("Relay stopped: %s. %d legs%s." % (" ".join(reason.split()[:12]), self.legs,
                                                            tally(self.units)))
        if self.refused:
            print("  the guard refused %d command%s: read them with `relay.py refusals`; one that was normal work "
                  "becomes a test and a rule fix." % (self.refused, "" if self.refused == 1 else "s"), flush=True)
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
    p.add_argument("--day-budget", type=float, dest="day_budget",
                   help="the day's cap in dollars of cost, for this run: no leg starts once today's legs cost this "
                        "much, and the percent cap is not used (0: no cap at all)")
    p.add_argument("--day-pct", type=float, dest="day_pct",
                   help="the day's cap in percent of the plan's week, for this run (default: limits.json "
                        "day_budget_pct): only when the owner named another figure for the day")
    p.add_argument("--max-legs", type=int, dest="max_legs", default=0)
    p.add_argument("--dry-run", action="store_true", dest="dry_run", help="say what would run; claim and start nothing")
    p.add_argument("--no-push", action="store_true", dest="no_push", help="do not commit and push the board")
    p.add_argument("--who", help="who starts this run (a session name): shown by `relay.py status`")
    p.add_argument("--allow-dirty", action="store_true", dest="allow_dirty",
                   help="run although the relay's own code has uncommitted changes (developing the relay)")
    p.add_argument("--view", action="store_true", help="open a Windows Terminal tab per leg that shows its output")
    p.add_argument("--no-quiet", action="store_true", dest="no_quiet",
                   help="skip the check that the checkout saw no git activity lately (you know nobody is in it)")
