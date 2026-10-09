#!/usr/bin/env python3
"""Tests for second opinions by another vendor (Tools/relay/second.py, providers/). Run: python Tools/relay/test_second.py
Every test works in a temporary relay home against fake_vendor.py: no model, no network. That the real rails hold on
this machine is `second.py proof --vendor <v>`, and that the proof can fail is `second.py proof --vendor <v> --open`.
"""
import json, os, shutil, subprocess, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import providers, second   # noqa: E402
from providers import codex, grok   # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_FAKE_SECOND", "TW_FAKE_SECOND_ARGV", grok.ENV, codex.ENV)
PAPER = ("VERDICT: look ROUND 1: 72/100 the smoke reads, the fire does not\n\nSCORES: coverage 20, look 22\n\n"
         "TOP-3 MANDATED FIXES:\n1. Lift the fire's core two stops.\n2. Cut the sparks by half.\n"
         "3. Hold the smoke for a second longer.\n")


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="second-test-"))
        self.old = {k: os.environ.get(k) for k in ENV}
        self.script, self.started = self.tmp / "fake.json", self.tmp / "argv.jsonl"
        os.environ.update({"TW_RELAY_HOME": str(self.tmp / "home"), "TW_FAKE_SECOND": str(self.script),
                           "TW_FAKE_SECOND_ARGV": str(self.started)})
        for v in (grok, codex):
            os.environ[v.ENV] = json.dumps([sys.executable, str(HERE / "fake_vendor.py"), v.__name__.split(".")[-1]])
        self.fake(report=PAPER)

    def tearDown(self):
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def fake(self, **script):
        self.script.write_text(json.dumps(script), encoding="utf-8")

    def fill(self, dst):
        (dst / "stage.json").write_text('{"stage": "look"}', encoding="utf-8")
        (dst / "A.jpg").write_bytes(b"\xff\xd8\xff\xe0 not a real picture")
        (dst / "B.jpg").write_bytes(b"\xff\xd8\xff\xe0 nor this")

    def critic(self, vendor="grok", **kw):
        return second.critic(vendor, self.fill, "# Critic round 1: a test", max_bytes=8192, **kw)

    def starts(self):
        return [json.loads(l) for l in self.started.read_text(encoding="utf-8").splitlines()] if self.started.exists() else []


class Papers(Base):
    def test_a_trusted_paper_gives_its_score(self):
        for vendor in providers.NAMES:
            rec = self.critic(vendor)
            self.assertIsNone(second.usable(rec), rec)
            self.assertEqual((rec["score"], len(rec["fixes"]), rec["state"]), (72, 3, "DONE"))
            self.assertTrue((Path(rec["home"]) / "record.json").is_file())

    def test_a_paper_the_relays_parser_cannot_read_gives_no_score(self):
        self.fake(report="I think this is about a 72. Fix the fire.")
        rec = self.critic()
        self.assertIsNone(rec["score"])
        self.assertIn("VERDICT", second.usable(rec))

    def test_no_paper_gives_no_score(self):
        self.fake(report="")
        rec = self.critic("codex")
        self.assertIsNone(rec["score"])
        self.assertEqual(second.usable(rec), "it wrote no paper")

    def test_the_prompt_carries_the_text_files_and_names_the_pictures(self):
        rec = self.critic("codex")
        prompt = (Path(rec["home"]) / "prompt.txt").read_text(encoding="utf-8")
        self.assertTrue(prompt.startswith(second.ONLY_READ))
        self.assertIn('{"stage": "look"}', prompt)
        self.assertIn("A.jpg, B.jpg", prompt)


class Rails(Base):
    def test_each_vendor_is_started_held_to_reading(self):
        self.critic("grok")
        self.critic("codex")
        g, c = self.starts()
        self.assertEqual(g[g.index("--permission-mode") + 1], "dontAsk")
        self.assertEqual(g[g.index("--tools") + 1], "read_file,list_dir,grep")
        self.assertNotIn("--always-approve", g)
        self.assertEqual(c[c.index("-s") + 1], "read-only")
        self.assertEqual(c[-1], "-")                                   # the prompt comes on stdin
        self.assertEqual(sum(1 for w in c if w == "-i"), 2)            # one per picture
        self.assertLess(max(i for i, w in enumerate(c) if w == "-i"), c.index("-o"))

    def test_a_command_line_that_does_not_ask_for_read_only_is_not_started(self):
        for vendor, mod, drop in (("grok", grok, "--tools"), ("codex", codex, "-s")):
            real = mod.argv

            def loose(job, real=real, drop=drop):
                a = real(job)
                del a[a.index(drop):a.index(drop) + 2]
                return a
            mod.argv = loose
            try:
                rec = self.critic(vendor)
            finally:
                mod.argv = real
            self.assertEqual(rec["state"], "REFUSED", vendor)
            self.assertIn("not started", second.usable(rec))
        self.assertEqual(self.starts(), [])

    def test_a_run_that_changed_a_file_is_untrusted(self):
        for vendor in providers.NAMES:
            self.fake(report=PAPER, write="made.txt")
            rec = self.critic(vendor)
            self.assertIn("files in its working folder changed", rec["untrusted"], vendor)
            self.assertTrue(second.usable(rec).startswith("untrusted"), vendor)

    def test_a_grok_run_that_used_a_tool_beyond_reading_is_untrusted(self):
        self.fake(report=PAPER, calls=[{"tool": "read_file"}, {"tool": "run_terminal_command", "what": "del x"}])
        self.assertIn("it ran run_terminal_command", self.critic()["untrusted"])
        self.fake(report=PAPER, calls=[{"tool": "use_tool", "what": "liteapi__post_rates_book"}])
        self.assertIn("it ran use_tool", self.critic()["untrusted"])

    def test_a_call_the_vendor_itself_refused_is_not_held_against_the_run(self):
        self.fake(report=PAPER, calls=[{"tool": "run_terminal_command", "ran": False}])
        self.assertEqual(self.critic()["untrusted"], [])
        self.fake(report=PAPER, calls=[{"tool": "file_change", "ran": False}])
        self.assertEqual(self.critic("codex")["untrusted"], [])

    def test_a_grok_run_that_says_it_had_more_than_it_was_given_is_untrusted(self):
        self.fake(report=PAPER, tools=["read_file", "grep", "run_terminal_command"])
        self.assertTrue(any("wider" in u for u in self.critic()["untrusted"]))
        self.fake(report=PAPER, mode="bypassPermissions")
        self.assertTrue(any("mode" in u for u in self.critic()["untrusted"]))

    def test_a_codex_run_that_changed_files_by_patch_is_untrusted(self):
        self.fake(report=PAPER, calls=[{"tool": "command", "what": "cmd /c type note.txt"}, {"tool": "file_change"}])
        self.assertEqual(self.critic("codex")["untrusted"], ["it ran file_change"])
        self.fake(report=PAPER, calls=[{"tool": "command", "what": "cmd /c type note.txt"}])
        self.assertEqual(self.critic("codex")["untrusted"], [])


class Failures(Base):
    """A second opinion that fails is a record that says so: nothing here raises."""
    def test_a_run_out_of_time_is_stopped_and_gives_no_score(self):
        self.fake(report=PAPER, sleep=60)
        rec = self.critic(minutes=0.03)
        self.assertEqual(rec["state"], "TIMEOUT")
        self.assertLess(rec["seconds"], 45)
        self.assertIn("ran out", second.usable(rec))

    def test_a_run_with_no_closing_record_gives_no_score(self):
        for vendor in providers.NAMES:
            self.fake(report=PAPER, no_result=True, exit=1)
            rec = self.critic(vendor)
            self.assertFalse(rec["ok"], vendor)
            self.assertIsNotNone(second.usable(rec), vendor)

    def test_a_grok_turn_that_failed_gives_no_score_though_it_exits_zero(self):
        self.fake(report=PAPER, subtype="error_during_execution")
        rec = self.critic()
        self.assertEqual((rec["exit_code"], rec["ok"]), (0, False))
        self.assertIn("error_during_execution", second.usable(rec))

    def test_a_vendor_that_is_not_installed_is_a_record_not_an_error(self):
        os.environ[grok.ENV] = json.dumps([str(self.tmp / "no-such-grok.exe")])
        rec = self.critic()
        self.assertEqual(rec["state"], "ERROR")
        self.assertIsNotNone(second.usable(rec))


class Money(Base):
    def test_grok_says_its_own_cost_and_codex_is_priced_from_its_tokens(self):
        g, c = self.critic("grok"), self.critic("codex")
        self.assertEqual((g["cost_usd"], g["cost_from"]), (0.02, "vendor"))
        row = second.prices()["codex"]
        want = (10000 * row["in"] + 90000 * row["cached"] + 1000 * row["out"]) / 1e6
        self.assertEqual((c["cost_usd"], c["cost_from"]), (round(want, 4), "list price"))


class Review(Base):
    def repo(self):
        r = self.tmp / "repo"
        (r / "Assets").mkdir(parents=True)
        run = lambda *a: subprocess.run(["git", "-C", str(r)] + list(a), check=True, capture_output=True)
        run("init", "-q")
        run("config", "user.email", "t@example.com")
        run("config", "user.name", "t")
        (r / "Assets" / "A.cs").write_text("class A { int n = 1; }\n", encoding="utf-8")
        (r / "Assets" / "art.png").write_bytes(b"\x89PNG" + b"0" * 64)
        run("add", "-A")
        run("commit", "-q", "-m", "base")
        (r / "Assets" / "A.cs").write_text("class A { int n = 2; }\n", encoding="utf-8")
        run("commit", "-q", "-am", "A counts from two")
        return r

    def test_a_review_gets_the_diff_the_log_and_the_code_at_the_head_commit(self):
        self.fake(report="VERDICT: PASS\n[A1] pass: Assets/A.cs:1 counts from two.\nNot checked or unsure: none")
        rec = second.review("codex", self.repo(), "HEAD~1..HEAD", about="a test")
        self.assertIsNone(second.usable(rec), rec)
        b = Path(rec["home"]) / "bundle"
        self.assertIn("int n = 2", (b / "diff.patch").read_text(encoding="utf-8"))
        self.assertIn("A counts from two", (b / "log.txt").read_text(encoding="utf-8"))
        self.assertEqual((b / "tree" / "Assets" / "A.cs").read_text(encoding="utf-8"), "class A { int n = 2; }\n")
        self.assertIn("    1  class A { int n = 2; }", (b / "touched.md").read_text(encoding="utf-8"))
        self.assertIn("    1  class A { int n = 2; }", (Path(rec["home"]) / "prompt.txt").read_text(encoding="utf-8"))
        self.assertFalse((b / "tree" / "Assets" / "art.png").exists())      # code and docs only
        self.assertFalse((b / ".git").exists())                             # nothing there to commit or push from

    def test_a_review_can_be_made_where_the_repo_is_and_read_where_the_vendor_is(self):
        self.fake(report="VERDICT: PASS\n[A1] pass: Assets/A.cs:1.\nNot checked or unsure: none")
        made = self.tmp / "made"
        self.assertEqual(second.main(["bundle", "--checkout", str(self.repo()), "--commits", "HEAD~1..HEAD",
                                      "--out", str(made)]), 0)
        self.assertEqual((made / "commits.txt").read_text(encoding="utf-8"), "HEAD~1..HEAD\n")
        rec = second.review("grok", bundle=made, about="a test")
        self.assertIsNone(second.usable(rec), rec)
        prompt = (Path(rec["home"]) / "prompt.txt").read_text(encoding="utf-8")
        self.assertIn("commits HEAD~1..HEAD", prompt)
        self.assertIn("    1  class A { int n = 2; }", prompt)

    def test_a_report_with_no_verdict_is_not_used(self):
        self.fake(report="Looks fine to me.")
        self.assertIn("VERDICT", second.usable(second.review("grok", self.repo(), "HEAD~1..HEAD")))


if __name__ == "__main__":
    unittest.main(verbosity=1)
