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


if __name__ == "__main__":
    unittest.main(verbosity=1)
