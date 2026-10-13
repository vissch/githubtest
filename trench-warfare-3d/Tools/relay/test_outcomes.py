#!/usr/bin/env python3
"""Tests for what became of a unit after it passed (Tools/relay/outcomes.py). Run: python Tools/relay/test_outcomes.py
Each test works in a temporary folder: a git repo of its own with an integration branch, and a board of its own.
"""
import contextlib, datetime, io, json, os, shutil, subprocess, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import gitio, outcomes, relay   # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_BOARD", "TW_BOARD_ALSO", "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_COMMITTER_NAME",
       "GIT_COMMITTER_EMAIL")
INTO = gitio.INTEGRATION


def stamp(days_ago=0.0):
    t = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(days=days_ago)
    return t.strftime("%Y-%m-%dT%H:%M:%SZ")


class Outcomes(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="outcomes-test-"))
        self.board, self.repo, self.n = self.tmp / "board", self.tmp / "repo", 0
        self.old = {k: os.environ.get(k) for k in ENV}
        os.environ.pop("TW_BOARD_ALSO", None)
        os.environ.update(TW_RELAY_HOME=str(self.tmp / "home"), TW_BOARD=str(self.board),
                          GIT_AUTHOR_NAME="t", GIT_COMMITTER_NAME="t", GIT_AUTHOR_EMAIL="t@t", GIT_COMMITTER_EMAIL="t@t")
        self.repo.mkdir()
        self.g("init", "-q")
        self.commit("README.md", "x\n", "init")
        self.g("branch", "-M", INTO)

    def tearDown(self):
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    # ----- the repo -----
    def g(self, *args):
        r = subprocess.run(["git"] + list(args), cwd=str(self.repo), capture_output=True)
        if r.returncode:
            raise AssertionError("git %s: %s" % (" ".join(args), r.stderr.decode("utf-8", "replace")))
        return r.stdout.decode("utf-8", "replace").strip()

    def commit(self, name, text, message=None):
        p = self.repo / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8", newline="\n")
        self.g("add", "-A")
        self.g("commit", "-q", "-m", message or name)
        return self.g("rev-parse", "HEAD")

    def lane(self, name, *files):
        """A lane off integration with one commit per file; (the head it started at, its head). Leaves the repo on
        integration."""
        self.g("switch", "-q", "-c", name, INTO)
        before = self.g("rev-parse", "HEAD")
        for f in files:
            self.commit(f, "made on %s\n" % name)
        after = self.g("rev-parse", "HEAD")
        self.g("switch", "-q", INTO)
        return before, after

    def land(self, lane, most=None):
        """Land the way land.py and a combined landing branch do: integration moves on first, then the lane's
        commits (the first `most` of them) are picked onto it, so every hash is another one."""
        self.n += 1
        self.commit("other-%d.txt" % self.n, "someone else\n")
        for sha in self.g("rev-list", "--reverse", "%s..%s" % (INTO, lane)).split()[:most]:
            self.g("cherry-pick", sha)

    def is_ancestor(self, sha):
        return subprocess.run(["git", "merge-base", "--is-ancestor", sha, INTO], cwd=str(self.repo)).returncode == 0

    # ----- the board -----
    def leg(self, unit, phase="execute", cost=1.0, model="opus", days_ago=10.0, heads=None, board=None, **kw):
        """One leg record on the board, named the way the runner names it. heads: (head_before, head_after)."""
        self.n += 1
        run = "20261001-%06d-1" % self.n
        rec = dict({"run": run, "leg": 1, "unit": unit, "source": "lane", "role": "lane", "phase": phase,
                    "model": model, "started_at": stamp(days_ago), "finished_at": stamp(days_ago)}, **kw)
        if cost is not None:
            rec["cost_usd"] = cost
        if heads:
            rec["head_before"], rec["head_after"] = heads
        p = Path(board or self.board) / "relay" / "desktop" / "legs" / ("%s-01.json" % run)
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(rec), encoding="utf-8")
        return rec

    def done(self, unit, lane, head, days_ago=9.0):
        p = self.board / "relay" / "done" / (unit + ".json")
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps({"id": unit, "lane": lane, "head": head, "done_at": stamp(days_ago)}), encoding="utf-8")

    def unit(self, unit, lane, heads, cost=(1.0, 2.0), models=("opus", "opus"), days_ago=10.0, passed=True, **kw):
        """A unit as a run leaves it: a plan leg that left the lane where it was, an execute leg that moved it, and
        its done file."""
        self.leg(unit, "plan", cost[0], models[0], days_ago, (heads[0], heads[0]), lane=lane, **kw)
        self.leg(unit, "execute", cost[1], models[1], days_ago - 0.01, heads, lane=lane, **kw)
        if passed:
            self.done(unit, lane, heads[1], days_ago - 0.02)

    def read(self, **kw):
        return outcomes.read(self.board, self.repo, **kw)

    def row(self, unit, **kw):
        return {r["unit"]: r for r in self.read(**kw)["units"]}[unit]

    # ----- the outcome of one unit -----
    def test_a_unit_with_no_done_file_is_open_and_its_cost_is_its_legs(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u-open", "lane/show/a", heads, cost=(1.5, 2.25), models=("opus", "sonnet"), passed=False)
        r = self.row("u-open")
        self.assertEqual((r["outcome"], r["landed"], r["usd"], r["legs"], r["unpriced"]), ("open", None, 3.75, 2, 0))
        self.assertEqual((r["plan"], r["execute"], r["tier"], r["lane"]),
                         ("opus", "sonnet", "plan opus, execute sonnet", "lane/show/a"))

    def test_a_lane_that_was_rebased_and_then_landed_reads_landed_though_no_hash_is_the_same(self):
        heads = self.lane("lane/show/a", "a.txt", "b.txt")
        self.unit("u1", "lane/show/a", heads)
        self.assertEqual(self.row("u1")["outcome"], "passed")
        self.land("lane/show/a")
        self.assertFalse(self.is_ancestor(heads[1]))            # an ancestor test alone still says: not landed
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["landed"]), ("landed", True))
        self.assertIn("every one of its 2 commits has its patch on integration", r["why"])

    def test_a_lane_with_one_commit_that_is_not_upstream_reads_passed(self):
        heads = self.lane("lane/show/a", "a.txt", "b.txt", "c.txt")
        self.unit("u1", "lane/show/a", heads)
        self.land("lane/show/a", most=2)                        # a combined landing took two of the three
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["landed"]), ("passed", False))
        self.assertIn("1 of its 3 commits is not on integration", r["why"])

    def test_a_lane_whose_head_is_on_integration_is_landed_when_its_legs_added_commits(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads)
        self.g("merge", "-q", "--ff-only", "lane/show/a")       # land.py pushes the rebased lane as integration
        self.assertTrue(self.is_ancestor(heads[1]))
        self.assertEqual((self.row("u1")["outcome"], self.row("u1")["why"]), ("landed", "its head is on integration"))

    def test_a_unit_that_passed_at_an_older_head_is_judged_there_not_at_the_lanes_tip(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads)
        self.land("lane/show/a")
        self.g("switch", "-q", "lane/show/a")
        self.commit("later.txt", "the next unit's, not landed\n")
        self.g("switch", "-q", INTO)
        self.assertEqual(self.row("u1")["outcome"], "landed")

    def test_a_lane_with_no_commit_of_its_own_is_empty_never_landed(self):
        before, after = self.lane("lane/show/a")
        self.assertEqual(before, after)
        self.unit("u1", "lane/show/a", (before, after))
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["landed"], r["why"]), ("empty", False, "its legs added no commit"))
        t = self.read()["tiers"][0]
        self.assertEqual((t["passed"], t["landed"], t["empty"], t["usd_per_landed"]), (1, 0, 1, None))

    def test_what_the_records_or_the_checkout_cannot_say_is_unknown_never_guessed(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.g("merge", "-q", "--ff-only", "lane/show/a")
        self.leg("old", "plan", lane="lane/show/a")             # a record from before the heads were kept
        self.leg("old", "execute", lane="lane/show/a", model=None)
        self.done("old", "lane/show/a", heads[1])
        r = self.row("old")
        self.assertEqual((r["outcome"], r["landed"], r["execute"]), ("unknown", None, "unknown"))
        self.assertIn("do not say whether the legs added a commit", r["why"])
        self.leg("far", "execute", lane="lane/show/far")
        self.done("far", "lane/show/far", "0123456789012345678901234567890123456789")
        self.assertEqual((self.row("far")["outcome"], self.row("far")["why"]),
                         ("unknown", "neither its head nor its lane is in this checkout"))
        self.leg("bare", "execute")
        self.done("bare", None, None)
        self.assertEqual(self.row("bare")["why"], "the records name no lane and no head for it")
        t = {t["tier"]: t for t in self.read()["tiers"]}
        self.assertEqual(t["plan none, execute opus"]["unknown"], 2)
        self.assertEqual((t["plan opus, execute unknown"]["unknown"], t["plan opus, execute unknown"]["landed"]), (1, 0))

    def test_a_head_that_is_gone_is_judged_at_the_lane_and_the_line_says_so(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads)
        self.done("u1", "lane/show/a", "0123456789012345678901234567890123456789")
        r = self.row("u1")
        self.assertEqual(r["outcome"], "passed")
        self.assertIn("judged at lane/show/a: the head it passed at is not in this checkout", r["why"])

    def test_with_no_integration_branch_nothing_is_said_about_a_landing(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads)
        got = self.read(into="origin/nowhere")
        self.assertEqual((got["into_sha"], got["units"][0]["outcome"]), ("", "unknown"))
        self.assertIn("no integration branch", outcomes.lines(got)[0])

    # ----- rework -----
    def fix(self, unit, name, days_ago, **kw):
        """A later unit that changed one file on a lane of its own; a review fix unless told otherwise."""
        heads = self.lane("lane/sim/" + unit, name)
        self.unit(unit, "lane/sim/" + unit, heads, days_ago=days_ago, **dict({"role": "review-fix"}, **kw))

    def test_a_landed_unit_a_later_fix_changed_the_same_file_of_is_reworked_and_names_the_fix(self):
        heads = self.lane("lane/show/a", "src/a.txt", "src/b.txt")
        self.unit("u1", "lane/show/a", heads, days_ago=10)
        self.land("lane/show/a")
        self.fix("rv-9", "src/b.txt", days_ago=7)
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["reworked_by"], r["shared"], r["landed"]), ("reworked", "rv-9", ["src/b.txt"], True))
        self.assertEqual(self.row("rv-9")["outcome"], "passed")         # the fix itself is not its own rework
        t = {t["tier"]: t for t in self.read()["tiers"]}["plan opus, execute opus"]
        self.assertEqual((t["passed"], t["landed"], t["reworked"], t["rework_rate"]), (2, 1, 1, 0.5))
        self.assertTrue(any("u1" in l and "reworked" in l and "rv-9 changed src/b.txt" in l and "(it had landed)" in l
                            for l in outcomes.lines(self.read())))

    def test_a_passed_unit_can_be_reworked_before_it_ever_lands(self):
        heads = self.lane("lane/show/a", "src/a.txt")
        self.unit("u1", "lane/show/a", heads, days_ago=10)
        self.fix("task-7", "src/a.txt", days_ago=8, role="lane", kind="fix")    # a found task that says it is a fix
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["reworked_by"], r["landed"]), ("reworked", "task-7", False))

    def test_what_is_no_rework(self):
        heads = self.lane("lane/show/a", "src/a.txt", "docs/reference/decisions.md")
        self.unit("u1", "lane/show/a", heads, days_ago=30)
        self.fix("rv-early", "src/a.txt", days_ago=31)                  # before the pass
        self.fix("rv-late", "src/a.txt", days_ago=30 - 15)              # after the fourteen days
        self.fix("rv-other", "src/z.txt", days_ago=29)                  # another file
        self.fix("rv-docs", "docs/reference/decisions.md", days_ago=29)  # every leg may write the decisions
        self.fix("feature", "src/a.txt", days_ago=29, role="lane")      # later work that is no fix
        self.fix("rv-open", "src/q.txt", days_ago=29)
        self.assertEqual((self.row("u1")["outcome"], self.row("u1")["reworked_by"]), ("passed", None))
        self.assertEqual(self.row("u1", days=16)["reworked_by"], "rv-late")
        self.leg("u-open", "execute", days_ago=40, heads=self.lane("lane/show/o", "src/q.txt"), lane="lane/show/o")
        self.assertEqual(self.row("u-open")["outcome"], "open")          # a unit that never passed is not reworked

    def test_a_unit_whose_commits_are_not_here_is_not_said_to_be_reworked(self):
        heads = self.lane("lane/show/a", "src/a.txt")
        self.unit("u1", "lane/show/a", ("0123456789012345678901234567890123456789", heads[1]), days_ago=10)
        self.fix("rv-9", "src/a.txt", days_ago=7)
        r = self.row("u1")
        self.assertEqual((r["outcome"], r["files_known"]), ("passed", False))

    # ----- the sums -----
    def test_the_sums_are_per_tier_and_no_landed_unit_gives_none_landed_not_a_division(self):
        self.unit("s1", "lane/show/s1", self.lane("lane/show/s1", "s1.txt"), cost=(1.0, 3.0), models=("opus", "sonnet"))
        self.unit("s2", "lane/show/s2", self.lane("lane/show/s2", "s2.txt"), cost=(1.0, 5.0), models=("opus", "sonnet"),
                  passed=False)
        self.unit("o1", "lane/show/o1", self.lane("lane/show/o1", "o1.txt"), cost=(2.0, 8.0))
        self.unit("o2", "lane/show/o2", self.lane("lane/show/o2", "o2.txt"), cost=(2.0, 6.0))
        self.unit("o3", "lane/show/o3", self.lane("lane/show/o3", "o3.txt"), cost=(2.0, 12.0), passed=False)
        self.land("lane/show/o1")
        got = self.read()
        opus, sonnet = got["tiers"]
        self.assertEqual((opus["tier"], opus["units"], opus["passed"], opus["landed"], opus["open"]),
                         ("plan opus, execute opus", 3, 2, 1, 1))
        self.assertEqual((opus["usd"], opus["usd_per_passed"], opus["usd_per_landed"], opus["rework_rate"]),
                         (32.0, 16.0, 32.0, 0.0))                       # every dollar the tier spent, over what got there
        self.assertEqual((sonnet["tier"], sonnet["usd"], sonnet["passed"], sonnet["landed"]),
                         ("plan opus, execute sonnet", 10.0, 1, 0))
        self.assertEqual((sonnet["usd_per_passed"], sonnet["usd_per_landed"]), (10.0, None))
        out = outcomes.lines(got)
        self.assertEqual(out[-2].split(), "opus/opus 3 units, 2 passed, 1 landed, $32.00 spent: $16.00 per passed "
                                          "unit, $32.00 per landed unit, rework 0 of 2 (0%)".split())
        self.assertEqual(out[-1].split(), "opus/sonnet 2 units, 1 passed, 0 landed, $10.00 spent: $10.00 per passed "
                                          "unit, none landed, rework 0 of 1 (0%)".split())

    def test_a_tier_in_which_nothing_passed_divides_by_nothing(self):
        self.unit("o1", "lane/show/o1", self.lane("lane/show/o1", "o1.txt"), passed=False)
        t = self.read()["tiers"][0]
        self.assertEqual((t["usd_per_passed"], t["usd_per_landed"], t["rework_rate"]), (None, None, None))
        self.assertIn("none passed, none landed, no rework to count", outcomes.lines(self.read())[-1])

    def test_a_leg_that_moved_up_shows_both_models_and_a_leg_with_no_cost_counts_as_nothing_and_is_said(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.leg("u1", "plan", 2.0, lane="lane/show/a", days_ago=10)
        self.leg("u1", "execute", 1.0, "sonnet", lane="lane/show/a", days_ago=9.9)
        self.leg("u1", "execute", None, "opus", heads=heads, lane="lane/show/a", days_ago=9.8)
        self.leg("u1", "critic", 0.5, lane="", days_ago=9.7)
        self.leg("retro-r1-05", "retro", 9.0, source="retro", role="retro")     # a retrospective is no unit
        got = self.read()
        r = got["units"][0]
        self.assertEqual([u["unit"] for u in got["units"]], ["u1"])
        self.assertEqual((r["execute"], r["usd"], r["legs"], r["unpriced"]), ("sonnet+opus", 3.5, 4, 1))
        self.assertTrue(any("1 leg named no cost" in l for l in outcomes.lines(got)))
        self.assertIn("1 leg with no cost named", outcomes.lines(got)[-1])

    # ----- where the records come from -----
    def test_a_board_job_passed_when_its_newest_result_is_a_pass(self):
        heads = self.lane("lane/show/pipe-thing", "out.txt")
        for job in ("thing--shots--aaaa1111", "thing--shots--bbbb2222"):
            self.leg(job, "execute", heads=heads, lane="lane/show/pipe-thing", source="pipeline", role="env-simulator")
        res = self.board / "results"
        res.mkdir(parents=True)
        for job, attempt, verdict in (("thing--shots--aaaa1111", 1, "PASS"), ("thing--shots--aaaa1111", 2, "FAIL"),
                                      ("thing--shots--bbbb2222", 1, "FAIL"), ("thing--shots--bbbb2222", 2, "PASS")):
            (res / ("%s--%d.json" % (job, attempt))).write_text(json.dumps(
                {"job": job, "attempt": attempt, "verdict": verdict, "finished_at": stamp(9)}), encoding="utf-8")
        self.assertEqual(self.row("thing--shots--aaaa1111")["outcome"], "open")     # a later FAIL outranks the PASS
        r = self.row("thing--shots--bbbb2222")
        self.assertEqual((r["outcome"], r["lane"]), ("passed", "lane/show/pipe-thing"))
        self.land("lane/show/pipe-thing")
        self.assertEqual(self.row("thing--shots--bbbb2222")["outcome"], "landed")

    def test_the_legs_of_a_second_relays_board_are_read_too_each_once(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads, cost=(1.0, 2.0))
        also = self.tmp / "board-2"
        self.leg("u2", "execute", 4.0, board=also, heads=self.lane("lane/show/b", "b.txt"), lane="lane/show/b")
        shutil.copytree(self.board / "relay" / "desktop" / "legs", also / "relay" / "desktop" / "legs", dirs_exist_ok=True)
        self.assertEqual([r["unit"] for r in self.read()["units"]], ["u1"])
        os.environ["TW_BOARD_ALSO"] = str(also)
        got = self.read()
        self.assertEqual([(r["unit"], r["usd"], r["legs"]) for r in got["units"]], [("u1", 3.0, 2), ("u2", 4.0, 1)])

    def test_a_unit_that_passed_with_no_leg_record_is_counted_apart(self):
        self.done("ghost", "lane/show/g", None)
        got = self.read()
        self.assertEqual((got["units"], got["passed_without_legs"]), ([], 1))
        self.assertTrue(any("1 unit passed with no leg record here" in l for l in outcomes.lines(got)))

    def test_nothing_is_written_to_the_board_or_the_repo(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads)
        self.land("lane/show/a")
        before = (gitio.tree_hashes(self.board, ()), self.g("for-each-ref"), self.g("status", "--porcelain"))
        self.read()
        self.assertEqual((gitio.tree_hashes(self.board, ()), self.g("for-each-ref"), self.g("status", "--porcelain")), before)

    # ----- the command -----
    def test_the_command_prints_lines_for_a_person_and_json_for_a_page(self):
        heads = self.lane("lane/show/a", "a.txt")
        self.unit("u1", "lane/show/a", heads, cost=(1.0, 2.0), models=("sonnet", "opus"))

        def cmd(*args):
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                code = relay.main(["outcomes", "--repo", str(self.repo)] + list(args))
            return code, buf.getvalue()
        code, out = cmd()
        self.assertEqual(code, 0)
        self.assertIn("Outcomes of 1 unit with leg records", out.splitlines()[0])
        self.assertIn("against %s at %s" % (INTO, self.g("rev-parse", INTO)[:10]), out.splitlines()[0])
        self.assertEqual(out.splitlines()[1].split()[:5], ["u1", "passed", "$", "3.00", "sonnet/opus"])
        self.assertIn("none landed", out.splitlines()[-1])
        code, out = cmd("--json")
        got = json.loads(out)
        self.assertEqual((code, got["into"], got["rework_days"]), (0, INTO, 14))
        self.assertEqual(sorted(got["units"][0]), sorted(
            ["unit", "source", "role", "kind", "lane", "legs", "usd", "unpriced", "plan", "execute", "tier",
             "started_at", "passed_at", "outcome", "landed", "why", "reworked_by", "shared", "files_known"]))
        self.assertEqual((got["tiers"][0]["tier"], got["tiers"][0]["usd_per_landed"]), ("plan sonnet, execute opus", None))


if __name__ == "__main__":
    unittest.main(verbosity=1)
