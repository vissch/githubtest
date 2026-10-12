#!/usr/bin/env python3
"""Tests for the token fixes of 2026-10-12: a plan leg no longer trims its plan blind, a leg carries only the tools
it uses, the week a leg's stream names is kept, and a review fix is executed on Sonnet.
Run: python Tools/relay/test_tokens.py    Nothing here starts a session or reads the machine's logs.
"""
import json, re, sys, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import config, launch, papers, usage   # noqa: E402

PLAN = """## Goal
Add a.txt to the repo.
## Steps
1. Create `a.txt` with one line.
%s
## Files
- `a.txt` (new)
## Checks
- `git status --porcelain` prints nothing.
## Done when
a.txt is in HEAD.
## Risks
%s
"""
# The last line a leg's stream held on 2026-10-12 (the desktop, run 20261012-151502-44224), ids cut.
EVENT = {"type": "rate_limit_event", "rate_limit_info": {
    "status": "allowed", "resetsAt": 1791826200, "rateLimitType": "five_hour",
    "unifiedWindows": {"five_hour": {"utilization": 0.13, "resetsAt": 1791826200},
                       "seven_day": {"utilization": 0.69, "resetsAt": 1791993600}}}}
LEG = {"run": "r1", "leg": 1, "model": "opus", "effort": "low", "desk": "D", "board": ""}


def plan_of(n_bytes, risks=""):
    text = PLAN % ("", risks)
    return PLAN % ("   " + "x" * (n_bytes - len(text.encode("utf-8")) - 3), risks)


class PlanSize(unittest.TestCase):
    def test_the_plan_most_legs_write_first_passes_the_shipped_limit(self):
        """106 plan legs: the first write was 8,863 bytes at the median and 11,007 at the ninth tenth, against a
        limit of 8,192, so three in four were sent round to trim."""
        lim = config.limits()
        for size in (8863, 11007):
            self.assertEqual(papers.check_plan(plan_of(size), lim["plan_max_bytes"]), [], size)
        self.assertTrue(any("bytes" in b for b in papers.check_plan(plan_of(14000), lim["plan_max_bytes"])))

    def test_a_plan_that_is_too_long_is_told_how_much_to_cut_and_where(self):
        text = plan_of(9000, risks="r" * 2000)                     # Steps is the biggest section, Risks the second
        hint = papers.plan_cut(text, 8192)
        over = len(text.encode("utf-8")) - 8192
        self.assertGreaterEqual(int(re.search(r"cut (\d+) bytes", hint).group(1)), over)   # one edit is enough
        self.assertLess(hint.index("Steps"), hint.index("Risks"))
        self.assertLess(hint.index("Risks"), hint.index("Goal"))
        self.assertEqual(papers.plan_cut(plan_of(8000), 8192), "")

    def test_the_size_the_plan_leg_is_told_fits_the_limit(self):
        """The role text names words, the check counts bytes: a plan of the most words it allows must pass. A plan
        is heavy with paths: 106 first writes held under 8 bytes a word."""
        text = (HERE / "roles" / "_phase_plan.md").read_text(encoding="utf-8")
        aim, top = (int(x.replace(",", "")) for x in re.search(r"aim for ([\d,]+) words, never over ([\d,]+)", text).groups())
        self.assertLess(aim, top)
        self.assertLessEqual(top * 8, config.limits()["plan_max_bytes"])
        self.assertNotIn("KB", text)                               # a model cannot count bytes


class Carried(unittest.TestCase):
    def test_a_leg_is_started_with_its_phases_tools_and_without_the_skills(self):
        lim = dict(config.limits(), leg_budget_usd=0)
        a = launch.argv("d", dict(LEG, tools=["Bash", "Read", "Write"], skills=False), lim)
        self.assertEqual(a[a.index("--tools") + 1], "Bash,Read,Write")
        self.assertIn("--disable-slash-commands", a)
        old = launch.argv("d", dict(LEG), lim)                     # a leg record from before: as it was
        self.assertNotIn("--tools", old)
        self.assertNotIn("--disable-slash-commands", old)

    def test_every_shipped_phase_names_the_tools_legs_use(self):
        """332 legs, 13,339 calls: Bash, Edit, Write, Read, Grep, Glob and PowerShell made all but 28 of them."""
        for name, p in config.phases().items():
            self.assertEqual(set(p["tools"]), {"Bash", "PowerShell", "Read", "Grep", "Glob", "Edit", "Write"}, name)
            self.assertIs(p["skills"], False, name)

    def test_a_call_to_a_tool_the_leg_was_not_given_is_not_an_unguarded_call(self):
        """Claude Code answers such a call itself, so the guard never sees it and it never ran. A tool the leg does
        have, missing from the guard's record, still is one."""
        import tempfile
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp)
            use = lambda i, name: json.dumps({"type": "assistant", "message": {"content": [
                {"type": "tool_use", "id": i, "name": name, "input": {}}]}})
            (d / launch.legdir.OUT).write_text("\n".join([use("t1", "Bash"), use("t2", "TodoWrite"), use("t3", "Read")]) + "\n",
                                               encoding="utf-8")
            (d / "calls.jsonl").write_text(json.dumps({"id": "t1", "tool": "Bash"}) + "\n", encoding="utf-8")
            (d / launch.legdir.LEG).write_text(json.dumps({"tools": ["Bash", "Read", "Write"]}), encoding="utf-8")
            self.assertEqual(launch.unguarded(d), ["Read"])
            (d / launch.legdir.LEG).write_text(json.dumps({}), encoding="utf-8")       # a leg with every tool
            self.assertEqual(launch.unguarded(d), ["TodoWrite", "Read"])

    def test_a_phase_whose_tools_cannot_do_a_leg_is_refused(self):
        import tempfile
        for bad, word in (({"tools": ["Read"]}, "Bash, Read and Write"), ({"tools": "Bash"}, "list of tool names"),
                          ({"skills": "no"}, "true or false")):
            with tempfile.TemporaryDirectory() as tmp:
                ph = json.loads((HERE / "phases.json").read_text(encoding="utf-8"))
                ph["plan"].update(bad)
                (Path(tmp) / "phases.json").write_text(json.dumps(ph), encoding="utf-8")
                with self.assertRaises(SystemExit) as e:
                    config.phases(tmp)
                self.assertIn(word, str(e.exception))


class Week(unittest.TestCase):
    def test_the_week_in_a_legs_stream_is_read_in_percent(self):
        r = usage.from_stream(EVENT, 1791820000)
        self.assertEqual((r["week"], r["five_hour"], r["resets"]), (69.0, 13.0, "2026-10-14 16:00"))
        self.assertEqual(usage.line(usage.brief(r), old=True)[:14], "Week: 69% used")

    def test_a_line_with_no_weekly_figure_is_no_reading(self):
        for raw in (None, {}, {"rate_limit_info": None}, {"rate_limit_info": {"unifiedWindows": {"five_hour": {"utilization": 0.1}}}},
                    {"rate_limit_info": {"unifiedWindows": {"seven_day": {"utilization": "0.6"}}}},
                    {"rate_limit_info": {"unifiedWindows": {"seven_day": {"utilization": 69}}}}):     # not 0 to 1
            self.assertIsNone(usage.from_stream(raw, 1), raw)

    def test_a_full_week_is_taken_as_what_was_measured(self):
        """2026-10-12: $28.40 of list cost was 1% of the plan's week, give or take a tenth."""
        self.assertTrue(2550 <= config.limits()["week_usd"] <= 3130)


class Sonnet(unittest.TestCase):
    def test_every_review_fix_is_executed_on_sonnet_and_planned_on_opus(self):
        for unit in ("rv-12-hud-f9", "rv2-08-board-sources", "one-not-queued-yet"):
            self.assertEqual(config.route("review-fix", "execute", unit=unit).get("model"), "sonnet", unit)
            self.assertEqual(config.route("review-fix", "plan", unit=unit), {})
        self.assertEqual(config.route("lane", "execute", unit="rv-12-hud-f9"), {})


class Card(unittest.TestCase):
    """A card too long for the session-start hook reached the leg as a preview and a file path: 105 of 337 legs
    spent a call reading their own card (2026-10-12)."""

    def test_a_long_card_is_the_first_message_and_a_short_one_comes_by_the_hook(self):
        import relay_hook
        short, long_ = "# Leg card\n" + "x" * 5000, "# Leg card\n" + "y" * 11000 + "\n## Risks\nthe last line\n"
        card, first = launch.card_and_prompt(short)
        self.assertEqual((card, first, relay_hook.card_in_prompt(short)),
                         (short, "Read your leg card and do the leg.\n", False))
        card, first = launch.card_and_prompt(long_)
        self.assertEqual(card, long_)                              # card.md still holds it, for the record
        self.assertTrue(first.startswith(long_.rstrip()) and "the last line" in first and first.endswith("Do the leg.\n"))
        self.assertTrue(relay_hook.card_in_prompt(long_))          # so the hook hands nothing over twice
        self.assertLess(relay_hook.CARD_HOOK_MAX, 10000)

    def test_ten_small_review_fixes_are_planned_on_sonnet_and_ten_on_opus(self):
        row = [r for r in config.routes() if (r["role"], r["phase"]) == ("review-fix", "plan")][0]
        self.assertEqual((row["model"], len(row["units"]), len(set(row["units"]))), ("sonnet", 10, 10))
        control = re.findall(r"rv-\d+", row["why"])
        self.assertEqual(len(control), 10)
        self.assertFalse([u for u in row["units"] if u.split("-p2")[0] in control])     # no unit in both arms
        self.assertEqual(config.route("review-fix", "plan", unit=row["units"][0]), {"model": "sonnet"})


class Tiers(unittest.TestCase):
    """Haiku for the easy work, Sonnet for the medium, Opus for the hard (the owner, 2026-10-12)."""

    def test_the_grade_is_the_first_word_under_difficulty(self):
        for body, grade in (("easy: one file.", "easy"), ("**Hard** - the seam.", "hard"), ("`medium`", "medium"),
                            ("Medium. Two files.", "medium"), ("quite easy", None), ("", None)):
            self.assertEqual(papers.plan_grade(PLAN % ("", "") + "## Difficulty\n%s\n" % body), grade, body)
        self.assertIsNone(papers.plan_grade(PLAN % ("", "")))
        self.assertEqual(papers.check_plan(PLAN % ("", "") + "## Difficulty\neasy: one file.\n", 12288), [])

    def test_the_table_is_a_setting_and_a_route_wins_over_it(self):
        ph = config.phases()
        self.assertEqual([config.graded(ph, "lane", "u1", g) for g in ("easy", "medium", "hard", None)],
                         ["sonnet", "sonnet", None, None])
        self.assertIn("model", config.route("review-fix", "execute", unit="u1"))       # the owner's route is there
        self.assertIsNone(config.graded(ph, "review-fix", "u1", "easy"))
        self.assertIsNone(config.graded(ph, "review-fix", "u1", None))
        self.assertEqual(config.graded(ph, "review-fix", "u1", "hard"), ph["execute"]["model"])   # hard work beats a route
        self.assertIn("`easy`, `medium` or `hard`", (HERE / "roles" / "_phase_plan.md").read_text(encoding="utf-8"))

    def test_haiku_reads_and_never_works(self):
        import tempfile
        ph = config.phases()
        self.assertEqual((ph["retro"]["model"], ph["retro"]["mode"]), ("haiku", "read_only"))
        for change, word in ((("execute", {"model": "haiku"}), "only for a phase that only reads"),
                             (("execute", {"by_grade": {"easy": "haiku"}}), "by_grade"),
                             (("execute", {"by_grade": {"simple": "sonnet"}}), "by_grade")):
            with tempfile.TemporaryDirectory() as tmp:
                raw = json.loads((HERE / "phases.json").read_text(encoding="utf-8"))
                raw[change[0]].update(change[1])
                (Path(tmp) / "phases.json").write_text(json.dumps(raw), encoding="utf-8")
                with self.assertRaises(SystemExit) as e:
                    config.phases(tmp)
                self.assertIn(word, str(e.exception))


class Watch(unittest.TestCase):
    """leakwatch.py: a leg that fought the plan's size is named, and a leg that did not is not."""

    def leg(self, root, n, refusals, batch):
        d = root / "runs" / "r1" / "legs" / ("%02d" % n)
        d.mkdir(parents=True)
        (d / "leg.json").write_text(json.dumps({"run": "r1", "leg": n, "unit": "u%d" % n, "phase": "plan",
                                                "cost_usd": 2.0}), encoding="utf-8")
        lines, k = [], 0

        def answer(*uses):
            nonlocal k
            k += 1
            lines.append(json.dumps({"type": "assistant", "message": {
                "id": "m%d" % k, "usage": {"input_tokens": 10, "cache_read_input_tokens": 30000},
                "content": [{"type": "tool_use", "id": "t%d-%d" % (k, i), "name": name, "input": inp}
                            for i, (name, inp) in enumerate(uses)]}}))
            return "t%d-0" % k

        for _ in range(4):
            answer(*[("Bash", {"command": "grep -n x Assets/A%d.cs" % i}) for i in range(batch)])
        answer(("Write", {"file_path": "C:/home/desk/01/plan.md", "content": "x"}))
        for i in range(refusals + 1):
            tid = answer(("Bash", {"command": "python \"C:/r/Tools/relay/relay.py\" leg done"}))
            said = "NOT DONE: plan.md is 9000 bytes, the limit is 8192" if i < refusals else "done: plan.md passes"
            lines.append(json.dumps({"type": "user", "message": {"content": [
                {"type": "tool_result", "tool_use_id": tid, "content": said}]}}))
            if i < refusals:
                answer(("Edit", {"file_path": "C:/home/desk/01/plan.md", "old_string": "a", "new_string": "b"}))
        (d / "out.jsonl").write_text("\n".join(lines) + "\n", encoding="utf-8")
        return d

    def test_a_leg_that_fought_the_size_is_flagged_and_a_clean_leg_is_not(self):
        import tempfile, leakwatch
        with tempfile.TemporaryDirectory() as tmp:
            bad = leakwatch.measure(self.leg(Path(tmp), 1, refusals=7, batch=1))
            good = leakwatch.measure(self.leg(Path(tmp), 2, refusals=0, batch=3))
            r = leakwatch.rules()
            self.assertEqual((bad["done_refused"], bad["done_calls"], bad["late_calls"], bad["first_ctx"]), (7, 8, 15, 30010))
            self.assertEqual({f["flag"] for f in leakwatch.flags(bad, r)}, {"size-fight", "late-calls", "same-file"})
            self.assertEqual((good["done_refused"], good["late_calls"], good["calls_per_answer"]), (0, 1, 2.33))
            self.assertEqual(leakwatch.flags(good, r), [])
            got = leakwatch.read_runs(tmp)
            self.assertEqual(leakwatch.summary(got["r1"])["plan"]["done_refused_mean"], 3.5)


if __name__ == "__main__":
    unittest.main(verbosity=1)
