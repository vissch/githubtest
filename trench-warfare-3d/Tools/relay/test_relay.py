#!/usr/bin/env python3
"""Tests for the relay (Tools/relay). Run: python Tools/relay/test_relay.py
Each test works in a temporary folder: no real board, checkout or Claude session is touched. Runs use fake_claude.py,
which calls the leg's real hooks, so a passing run also proves the guards and the meter were on.
"""
import argparse, ast, contextlib, datetime, io, json, os, shutil, subprocess, sys, tempfile, threading, time
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import boardio, cmdrules, config, gitio, launch, ledger, legcmd, legdir, papers, relay, relay_hook as H, runner   # noqa: E402
from sources import pipeline as SP                                                 # noqa: E402

UNIT = {"id": "house5--evidence--d1192f67", "source": "pipeline", "role": "destruction-vfx-simulator"}
LANE = "lane/show/pipe-house5"
ENV = ("TW_RELAY_HOME", "TW_RELAY_LEG", "TW_BOARD", "TW_STATION", "TW_RELAY_CLAUDE", "TW_FAKE_SCRIPT", "TW_AGENT_LOGS",
       "TW_RELAY_NO_QUIET", "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL",
       "TW_WORKER_PID", "TW_RUNS", "TW_RELAY_GATE", "TW_RELAY", "TW_RELAY_NO_WINDOW", "TW_RELAY_WT", "TW_RELAY_NOTIFY",
       "TW_RELAY_PLAY")


def jpeg(width, height, size=2000):
    return (b"\xff\xd8\xff\xe0\x00\x10JFIF\x00" + b"\x00" * 9 + b"\xff\xc0\x00\x11\x08"
            + height.to_bytes(2, "big") + width.to_bytes(2, "big") + b"\x00" * (size - 30))


def model_call(tokens, sidechain=False, out=0):
    return json.dumps({"type": "assistant", "isSidechain": sidechain,
                       "message": {"usage": {"input_tokens": 5, "cache_read_input_tokens": tokens - 5,
                                             "cache_creation_input_tokens": 0, "output_tokens": out}}})


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="relay-test-"))
        self.old = {k: os.environ.get(k) for k in ENV}
        os.environ.pop("TW_RELAY", None)                     # run by a relay leg, the tests are still not one
        self.cwd = os.getcwd()
        os.chdir(self.tmp)                                   # nor are they in the checkout a leg holds
        os.environ["TW_RELAY_HOME"] = str(self.tmp / "home")
        os.environ["TW_RELAY_NO_WINDOW"] = "0"               # the same whatever terminal runs the tests
        os.environ["TW_RELAY_NOTIFY"] = json.dumps([sys.executable, "-c", "pass"])   # no real notification
        os.environ["TW_AGENT_LOGS"] = str(self.tmp / "agent-logs")    # nor this machine's real sessions (agents.py)
        self.limits = config.limits                          # these tests count in dollars or in measured legs:
        config.limits = lambda *a, **k: dict(self.limits(*a, **k), week_usd=0,   # no guessed week (see Guess),
                                             day_budget_pct=0, pace_to_hour=0)   # no percent cap, no pace (see Pace)
        self.lim, self.ph = config.limits(), config.phases()
        self.board, self.d = self.tmp / "board", None

    def tearDown(self):
        config.limits = self.limits
        os.chdir(self.cwd)
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def leg(self, phase="execute", nn=1):
        d = legdir.new_leg("r1", nn, UNIT, phase, self.ph[phase], self.lim, self.tmp / "work", LANE, self.board)
        self.d = d
        return d

    def transcript(self, lines):
        p = self.tmp / "t.jsonl"
        with open(p, "a", encoding="utf-8", newline="\n") as f:
            f.write("".join(x + "\n" for x in lines))
        return p

    def hook(self, event, inp):
        """Run the hook as Claude Code does: JSON on stdin, JSON (or nothing) on stdout."""
        old, buf = sys.stdin, io.StringIO()
        sys.stdin = io.StringIO(json.dumps(inp))
        try:
            with contextlib.redirect_stdout(buf), contextlib.redirect_stderr(io.StringIO()):
                code = H.main([event] + ([str(self.d)] if self.d else []))
        finally:
            sys.stdin = old
        text = buf.getvalue().strip()
        return code, (json.loads(text)["hookSpecificOutput"] if text else None)

    def denied(self, name, agent=False, **tin):
        inp = {"tool_name": name, "tool_input": tin}
        if agent:
            inp["agent_id"] = "a1"
        o = self.hook("pre-tool", inp)[1]
        return bool(o) and o["permissionDecision"] == "deny"

    def red(self):
        t = self.transcript([model_call(301000)])
        self.hook("meter", {"transcript_path": str(t)})


class Settings(Base):
    def test_shipped_settings_load(self):
        self.assertEqual((self.lim["amber_tokens"], self.lim["red_tokens"]), (240000, 300000))
        self.assertEqual((self.ph["plan"]["effort"], self.ph["execute"]["effort"]), ("high", "low"))
        self.assertEqual(self.ph["plan"]["model"], self.ph["execute"]["model"])
        self.assertIn("RESULT:", config.style_text(config.style()))

    def test_overrides_are_clamped_to_the_bounds(self):
        lim = config.limits(overrides={"red_tokens": 900000, "run_hours": 99})
        self.assertEqual((lim["red_tokens"], lim["run_hours"]), (300000, 12))

    def test_a_bad_limits_file_stops_the_run(self):
        bad = self.tmp / "cfg"
        shutil.copytree(HERE, bad, ignore=shutil.ignore_patterns("*.py", "roles", "sources", "__pycache__"))
        raw = json.loads((bad / "limits.json").read_text(encoding="utf-8"))
        raw["amber_tokens"].update(value=300000, max=300000)        # amber no longer below red
        (bad / "limits.json").write_text(json.dumps(raw), encoding="utf-8")
        with self.assertRaises(SystemExit):
            config.limits(bad)

    def test_a_long_or_shapeless_report_is_named(self):
        st = config.style()
        self.assertEqual(config.report_problems("RESULT: done. Pictures are on the board.", st), [])
        self.assertEqual(len(config.report_problems("word " * 200, st)), 2)

    def test_a_verdict_is_read_from_its_own_line_also_under_a_line_of_words(self):
        """rv-12-hud-f9, 2026-10-08: "Leg complete." above the RESULT line turned a done into a failed."""
        self.assertEqual(runner.said("Leg complete.\n\nRESULT: done - I3 fixed, committed and pushed."), "done")
        self.assertEqual(runner.said("**RESULT:** blocked, the owner must pick."), "blocked")
        self.assertEqual(runner.said("Summary first.\nRESULT: failed - the gate is red.\nRESULT: done later"), "failed")
        for nothing in ("I'll pause here and wait for the background PlayMode test run to finish.",
                        "The result: done, I think.", "As a result done is near.", "", None):
            self.assertIsNone(runner.said(nothing), nothing)

    def test_a_review_fix_leg_is_told_its_standing_rules_and_every_execute_leg_the_gate_rules(self):
        import prompt
        st, top = config.style(), self.lim["prompt_max_bytes"]       # system_text refuses a prompt over the limit
        fix, plain = (prompt.system_text(role, "execute", st, top) for role in ("review-fix", "lane"))
        self.assertIn("fails on the old code", fix)
        self.assertIn("`[ID] test: Class.Method`", fix)
        self.assertNotIn("[ID] test:", plain)                        # the role file rides only with its role
        for text in (fix, plain):
            self.assertIn("Never start the full gate", text)
            self.assertIn("Never end your turn while a gate", text)
        self.assertNotIn("full gate", prompt.system_text("lane", "plan", st, top))    # a plan leg runs no gate

    def test_every_setting_is_used_somewhere(self):
        code = "".join(p.read_text(encoding="utf-8") for p in list(HERE.glob("*.py")) + list((HERE / "sources").glob("*.py"))
                       if p.name != "test_relay.py")
        raw = json.loads((HERE / "limits.json").read_text(encoding="utf-8"))
        unused = [k for k in raw if code.count('"%s"' % k) < 2]                # config.py names each once
        self.assertEqual(unused, [])


class NobodyWakesALeg(Base):
    """rv-12-hud-f9, 2026-10-08: the leg put its waits in the background, a command that outlasted its two minutes
    was "moved to the background" with "you will be notified", and the leg ended its turn to wait for the notice."""
    def test_a_leg_runs_with_background_tasks_off_and_its_own_waits_fit_in_one_call(self):
        d = self.leg()
        env = launch.leg_env(d, legdir.read(d))
        self.assertEqual(env.get("CLAUDE_CODE_DISABLE_BACKGROUND_TASKS"), "1")
        ap = argparse.ArgumentParser()
        legcmd.add_args(ap)
        for words in (["gate", "wait"], ["play", "TW.Tests.X"]):               # each waits this long by default
            self.assertLess(ap.parse_args(words).max * 1000, int(env["BASH_DEFAULT_TIMEOUT_MS"]), words)
        self.assertLessEqual(int(env["BASH_DEFAULT_TIMEOUT_MS"]), int(env["BASH_MAX_TIMEOUT_MS"]))

    def test_the_guard_refuses_what_would_wait_for_a_notice(self):
        self.leg()
        self.assertTrue(self.denied("Bash", command="python Tools/codemap.py", run_in_background=True))
        why = json.loads((self.d / "denials.jsonl").read_text(encoding="utf-8").splitlines()[0])["why"]
        self.assertIn("nobody wakes a leg", why)
        self.assertTrue(self.denied("PowerShell", command="Get-Date", run_in_background=True))
        self.assertTrue(self.denied("Agent", prompt="look for the test", run_in_background=True))
        for tool in ("ScheduleWakeup", "CronCreate", "Monitor"):
            self.assertTrue(self.denied(tool, delaySeconds=60, reason="waiting for the gate"), tool)
        relay_py = str(HERE / "relay.py").replace("\\", "/")
        for tin in ({"command": "python Tools/codemap.py"}, {"command": "python Tools/codemap.py", "run_in_background": False},
                    {"command": 'python "%s" leg play TW.Tests.HudTogglePlayTests' % relay_py},
                    {"command": 'python "%s" leg gate wait' % relay_py, "timeout": 600000}):
            self.assertFalse(self.denied("Bash", **tin), tin)
        self.assertFalse(self.denied("Agent", prompt="look for the test"))

    def test_every_leg_is_told_its_turn_is_its_end_and_an_execute_leg_how_to_run_a_playmode_test(self):
        import prompt
        st, top = config.style(), self.lim["prompt_max_bytes"]
        for phase in ("plan", "execute"):
            text = prompt.system_text("review-fix", phase, st, top)
            self.assertIn("The end of your turn is the end of the leg", text)
            self.assertIn("never wait for, or stop, a process you\n  did not start", text)
        self.assertIn("`leg play", prompt.system_text("lane", "execute", st, top))
        self.assertNotIn("you run yourself", prompt.system_text("review-fix", "execute", st, top))
        card = prompt.card_text(legdir.read(self.leg()), "the goal")
        self.assertIn("leg play <its class or full name>", card)
        self.assertNotIn("leg play", prompt.card_text(legdir.read(self.leg("plan", 2)), "the goal"))


class Meter(Base):
    def test_amber_is_said_once_and_red_every_batch(self):
        d = self.leg()
        t = self.transcript([model_call(100000)])
        self.assertIsNone(self.hook("meter", {"transcript_path": str(t)})[1])
        self.transcript([model_call(250000)])
        self.assertIn("amber", self.hook("meter", {"transcript_path": str(t)})[1]["additionalContext"])
        self.transcript([model_call(255000)])
        self.assertIsNone(self.hook("meter", {"transcript_path": str(t)})[1])
        for n in (301000, 305000):
            self.transcript([model_call(n)])
            self.assertIn("red", self.hook("meter", {"transcript_path": str(t)})[1]["additionalContext"])
        trips = [json.loads(x)["level"] for x in (d / "trips.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual(trips, ["amber", "red"])

    def test_red_stays_red_and_the_reply_counts(self):
        d = self.leg()
        t = self.transcript([model_call(280000, out=25000)])                  # 305k with the reply
        self.hook("meter", {"transcript_path": str(t)})
        self.transcript([model_call(100000)])                                 # a smaller call later
        self.hook("meter", {"transcript_path": str(t)})
        self.assertEqual(json.loads((d / "meter.json").read_text(encoding="utf-8"))["level"], "red")

    def test_subagent_batches_and_sidechain_calls_do_not_count(self):
        d = self.leg()
        t = self.transcript([model_call(100000), model_call(999999, sidechain=True)])
        self.hook("meter", {"transcript_path": str(t), "agent_id": "a1"})
        self.assertFalse((d / "meter.json").exists())
        self.hook("meter", {"transcript_path": str(t)})
        self.assertEqual(json.loads((d / "meter.json").read_text(encoding="utf-8"))["tokens"], 100000)

    def test_a_huge_line_a_half_line_and_a_bad_number_are_skipped(self):
        self.leg()
        big = json.dumps({"type": "user", "blob": "x" * 6_000_000, "usage": 1, "who": "assistant"})
        bad = json.dumps({"type": "assistant", "message": {"usage": {"input_tokens": "many"}}})
        t = self.transcript([model_call(50000), big, bad])
        with open(t, "a", encoding="utf-8") as f:
            f.write(model_call(290000)[:40])                                  # the writer is mid-line
        tokens, off, ok = H.context_tokens(str(t))
        self.assertEqual((tokens, ok), (50000, True))
        with open(t, "a", encoding="utf-8") as f:
            f.write(model_call(290000)[40:] + "\n")
        self.assertEqual(H.context_tokens(str(t), off, tokens)[0], 290000)

    def test_a_transcript_that_cannot_be_read_is_logged_and_turns_amber(self):
        d = self.leg()
        for _ in range(3):
            code, o = self.hook("meter", {"transcript_path": str(self.tmp / "nope.jsonl")})
        self.assertIn("amber", o["additionalContext"])
        self.assertTrue((d / "hook-errors.log").exists())

    def test_without_a_leg_the_hook_does_nothing(self):
        self.d = None
        self.assertEqual(self.hook("meter", {"transcript_path": "nope"}), (0, None))
        self.assertEqual(self.hook("pre-tool", {"tool_name": "Bash", "tool_input": {"command": "python land.py"}}), (0, None))


class Guards(Base):
    """Each list is an attack the critic ran against round 1."""
    def test_a_leg_never_lands_claims_or_rewrites(self):
        self.leg()
        board = str(self.board)
        for cmd in ("python Tools/land.py", "python Tools\\land.py", "python Tools/LAND.PY", "py -3 Tools/land.py",
                    "python -m land", 'cd Tools; python -c "import land; land.main()"',
                    "python Tools/pipeline/pipeline.py claim j1", "python -m pipeline complete j --verdict PASS",
                    "git push --force", "git push origin lane/show/other", "git push --force origin " + LANE,
                    "git push origin HEAD:claude/trench-warfare-2d-3d-plan-idt7lf # " + board,
                    "git push origin --delete lane/show/other; ls " + board, "git push --all",
                    "git push origin %s:other" % LANE, "git  push  -f origin " + LANE, "git.exe push origin main",
                    '"git" push origin main', "git -C %s push origin main" % board, "git p --force origin main",
                    "$g='pu'+'sh'; git $g --force origin main", "git commit -m x --amend", "git commit --amend -m x",
                    "git reset --hard HEAD~1", "git reset --keep HEAD~3", "git branch -f %s HEAD~5" % LANE,
                    "git rebase -i HEAD~3", "git switch lane/show/other", "git checkout main",
                    "git -c alias.p=push p", "git config alias.p push", "git clean -fdx", "gh pr merge 12 --squash",
                    "gh api -X PUT repos/x/y/pulls/1/merge", "git remote set-url origin https://x/y.git",
                    "git merge main", "git pull origin lane/show/other", "git rebase origin/lane/show/other",
                    "git stash drop", "git worktree add ../x", "git checkout -B %s origin/main" % LANE,
                    "git reset HEAD~3", "git -C ../githubtest commit -am x", "git fetch origin x:main",
                    "python -m Tools.land", "pythonw Tools/land.py", "Tools/land.py",
                    'python -c "exec(open(\'Tools/land.py\').read())"', 'python -c "from land import main"'):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)

    def test_a_wrapped_command_is_still_seen(self):
        self.leg()
        bad = "git push --force origin main"
        for cmd in ('bash -c "%s"' % bad, "sh -c '%s'" % bad, "env %s" % bad, "FOO=1 %s" % bad, "cmd /c %s" % bad,
                    'powershell -Command "%s"' % bad, "echo x | xargs %s" % bad, 'eval "%s"' % bad, "time %s" % bad,
                    "nohup %s" % bad, "command %s" % bad, "exec %s" % bad, "if true; then %s; fi" % bad,
                    "(%s)" % bad, "{ %s; }" % bad, "! %s" % bad, 'g"i"t push --force origin main',
                    "git.cmd push origin main", "C:/Program\\ Files/Git/cmd/git.exe push origin main",
                    "iex '%s'" % bad, "Start-Process git push origin main",
                    'bash -c "bash -c \'%s\'"' % bad, "env -i python Tools/land.py",
                    "python Tools/pipeline/run_detached.py start x --timeout 9 -- %s" % bad):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        self.assertTrue(self.denied("Monitor", command=bad))                       # any tool that runs a command
        for cmd in ("env -u FOO %s" % bad, "timeout 5 %s" % bad, "timeout -k 3 5s %s" % bad, 'env -S "%s"' % bad,
                    "git diff $(%s)" % bad, "git status `%s`" % bad, "echo \\' ; %s ; echo \\'" % bad,
                    "GIT_EXTERNAL_DIFF=rm git diff", "GIT_DIR=../githubtest/.git git commit -m x",
                    "export GIT_SSH_COMMAND=evil; git fetch origin", "unset TW_RELAY; python x.py",
                    "env -u TW_RELAY python x.py", "TW_RELAY= python x.py", "$env:TW_RELAY=''; python x.py",
                    "Remove-Item Env:\\TW_RELAY"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        for cmd in ("git push --no-verify origin " + LANE, "rm .git/hooks/pre-push", "del .git\\relay-leg.json",
                    "python x.py --no-verify"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)

    def test_ordinary_work_is_not_refused(self):
        self.leg()
        for cmd in ("git push origin " + LANE, "git push -u origin " + LANE, "git -C %s push" % self.board,
                    'git commit -m "fix the push ordering"', 'git commit -m "note about land.py"',
                    "git log --grep push", "cat Tools/land.py", "git rebase origin/claude/trench-warfare-2d-3d-plan-idt7lf",
                    "git add -A", "python Tools/codemap.py", "git checkout -- a.cs", "git fetch origin",
                    "git commit -m \"$(cat <<'EOF'\nFix the gate; git push --force later\n\nit's \"quoted\"\nEOF\n)\"",
                    "git add -A && git commit -m \"$(cat <<'EOF'\nmsg\nEOF\n)\"",
                    "git push origin %s 2>&1 | tail -3" % LANE,
                    "git add -A && git commit -m x && git -C %s push" % self.board,
                    "git -C %s push 2>&1 | tail -2" % self.board, 'git commit --message="fix; git push later"',
                    "git diff $(git merge-base HEAD origin/claude/trench-warfare-2d-3d-plan-idt7lf)",
                    "git -c core.autocrlf=false add -A", "git config user.name", "git config --get core.autocrlf",
                    "git checkout main -- docs/x.md", "git reflog", "git ls-remote origin", "gh pr list --search merge",
                    "powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly",
                    "python Tools/pipeline/run_detached.py start gate --timeout 1800 -- powershell -File gate.ps1 -EditOnly",
                    "git pull --rebase origin " + LANE, "git stash list", "git rebase --continue",
                    "git push", "git push origin HEAD", "git push -u origin HEAD", "git push origin HEAD:" + LANE,
                    "git -C . status", "git -C . commit -m x", "git -C %s add -A" % (self.tmp / "work"),
                    "git -C . push origin " + LANE, 'git commit -m "explain --no-verify and the pre-push hook"',
                    'git commit -m "hooks/pre-push is the guard" -m "relay-leg.json is the marker"',
                    "GIT_EDITOR=true git rebase --continue", "FOO=1 python Tools/codemap.py",
                    "timeout 600 python Tools/codemap.py", "ls ../githubtest", "cd ../.. && ls",
                    "git commit -m 'costs $(nothing) here'"):
            self.assertFalse(self.denied("Bash", command=cmd), cmd)

    def test_a_leg_cannot_write_into_git_or_reach_another_legs_folder(self):
        d = self.leg()
        self.leg(nn=2)
        self.d = d
        for path in (self.tmp / "work" / ".git" / "hooks" / "pre-push", self.tmp / "work" / ".git" / "config"):
            self.assertTrue(self.denied("Write", file_path=str(path)), path)
        self.assertTrue(self.denied("Bash", command="cat ../../legs/02/leg.json"))
        self.assertTrue(self.denied("Bash", command="cp x ..\\..\\legs\\02\\card.md"))

    def test_a_board_push_in_git_bash_form_is_the_board(self):
        leg = {"lane": LANE, "board": "C:/Users/PC/Documents/GitHub/tw3d-board"}
        self.assertIsNone(cmdrules.never("git -C /c/Users/PC/Documents/GitHub/tw3d-board push", leg))

    def test_a_leg_cannot_edit_its_own_rules(self):
        d = self.leg("plan")
        for name in ("leg.json", "meter.json", "hooks.json", "card.md"):
            self.assertTrue(self.denied("Write", file_path=str(d / name)), name)
        self.assertTrue(self.denied("Bash", command='python -c "open(r\'%s\',\'w\')"' % (d / "leg.json")))
        self.assertTrue(self.denied("Bash", command="cat %s" % str(d / "leg.json").replace("\\", "/")))
        self.assertTrue(self.denied("Bash", command="cat note.md>%s" % str(d / "leg.json").replace("\\", "/")))
        self.assertTrue(self.denied("Write", file_path="\\\\?\\" + str(d / "leg.json")))

    def test_a_leg_cannot_edit_the_relay_or_the_board_outside_evidence(self):
        self.leg()
        for path in (HERE / "relay_hook.py", HERE / "cmdrules.py", HERE.parent / "pipeline" / "pipeline.py",
                     self.board / "results" / "thing--shots--aaaa--9.json", self.board / "claims" / "desktop.json",
                     self.board / "relay" / "queue" / "zz.json", self.board / "items" / "x.json"):
            self.assertTrue(self.denied("Write", file_path=str(path)), path)
            self.assertTrue(self.denied("MultiEdit", file_path=str(path)), path)
        self.assertFalse(self.denied("Write", file_path=str(self.board / "evidence" / "thing" / "shots" / "near.jpg")))
        self.assertFalse(self.denied("Edit", file_path=str(self.tmp / "work" / "a.cs")))

    def test_the_hook_as_a_process_refuses_when_its_rules_are_broken(self):
        d = self.leg()
        broken = self.tmp / "relaycopy"
        shutil.copytree(HERE, broken, ignore=shutil.ignore_patterns("__pycache__", "test_relay.py"))
        (broken / "cmdrules.py").write_text("this is not python (", encoding="utf-8")
        r = subprocess.run([sys.executable, str(broken / "relay_hook.py"), "pre-tool", str(d)], capture_output=True,
                           input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "git status"}}).encode())
        self.assertEqual(json.loads(r.stdout)["hookSpecificOutput"]["permissionDecision"], "deny")
        ok = subprocess.run([sys.executable, str(HERE / "relay_hook.py"), "pre-tool", str(d)], capture_output=True,
                            input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "git status"}}).encode())
        self.assertEqual((ok.returncode, ok.stdout.strip()), (0, b""))
        self.assertEqual(len((d / "calls.jsonl").read_text(encoding="utf-8").splitlines()), 2)

    def test_hooks_that_log_at_the_same_moment_lose_no_line(self):
        # one batch of tool calls runs its hooks together; a lost line reads to the audit as an unguarded call
        d = self.leg()
        code = ("import sys; sys.path.insert(0, sys.argv[1]); import relay_hook as H\n"
                "for i in range(40): H.append(sys.argv[2], 'calls.jsonl', {'id': sys.argv[3] + '-%d' % i})\n")
        ps = [subprocess.Popen([sys.executable, "-c", code, str(HERE), str(d), str(k)]) for k in range(8)]
        self.assertEqual({p.wait() for p in ps}, {0})
        ids = [json.loads(x)["id"] for x in (d / "calls.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual((len(ids), len(set(ids))), (320, 320))

    def test_rules_that_cannot_be_read_refuse_everything(self):
        d = self.leg()
        (d / "leg.json").write_text("{ not json", encoding="utf-8")
        self.assertTrue(self.denied("Bash", command="git status"))
        (d / "leg.json").unlink()
        self.assertTrue(self.denied("Read", file_path="x"))

    def test_a_meter_file_that_cannot_be_read_counts_as_red(self):
        d = self.leg()
        (d / "meter.json").write_text('{"level": "gre', encoding="utf-8")
        self.assertTrue(self.denied("Edit", file_path=str(self.tmp / "work" / "a.cs")))
        self.assertFalse(self.denied("Bash", command="git status"))

    def test_a_plan_leg_only_reads_and_writes_its_plan(self):
        d = self.leg("plan")
        desk = legdir.desk(d)
        self.assertFalse(self.denied("Write", file_path=str(desk / "plan.md")))
        self.assertFalse(self.denied("Write", file_path=str(desk / "note.md")))
        for cmd in ("git log --oneline -5", "git status", "rg foo Assets | head -5", "git diff HEAD~1 --stat",
                    "python Tools/pipeline/pipeline.py status", "ls Assets", "git log 2>&1 | head", "ls 2>/dev/null",
                    "sed -n 1,20p a.cs", "cd Tools && ls", "test -f a && echo y", "git branch --contains HEAD",
                    "git remote -v", "git stash list", "jq .id items/x.json", "git config --get user.name",
                    "python Tools/editor_lock.py status 2>&1 | head -20", "python Tools/editor_lock.py guard",
                    "git branch -a --contains e23f5353 | head -10", "git branch -r --merged origin/main"):
            self.assertFalse(self.denied("Bash", command=cmd), cmd)
        for cmd in ("git branch -a new-branch", "git branch --contains HEAD -D lane/show/x", "git branch -m --contains x y"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)                 # a look at branches makes or drops none
        for cmd in ("python Tools/editor_lock.py claim leg-1 --minutes 15", "python Tools/editor_lock.py release leg-1",
                    "python Tools/editor_lock.py wait --timeout 1800"):       # a claim writes, a wait is not a look
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        for cmd in ("git status\ngit log --oneline -3", "ls Assets\n# then the tools\nls Tools",
                    "git diff $(git merge-base HEAD origin/claude/trench-warfare-2d-3d-plan-idt7lf) --stat",
                    "git log $(git rev-parse --short HEAD) -1", "Get-Item a.cs | Select-Object Name, Length",
                    "Get-ChildItem Assets | Sort-Object Name | Format-Table", "sha256sum a.cs", "printf x | wc -c"):
            self.assertFalse(self.denied("Bash", command=cmd), cmd)
        self.assertFalse(self.denied("Skill", skill="tw-env-sim"))
        for tool in ("WebFetch", "WebSearch", "Agent"):                            # looking things up is reading
            self.assertFalse(self.denied(tool, url="x", prompt="x"), tool)
        for tool in ("Monitor", "EnterWorktree", "CronCreate", "Workflow"):
            self.assertTrue(self.denied(tool, command="git status", url="x"), tool)
        for cmd in ("GIT_EXTERNAL_DIFF=rm git diff", "git log $(echo --output=f)", "git log $(git rev-parse --output=f)",
                    "echo \\' ; touch PWNED ; echo \\'", "ls\ntouch PWNED", "FOO=1 ls", "timeout 5 touch PWNED",
                    "git status $(touch PWNED)"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        self.assertTrue(self.denied("MultiEdit", file_path=str(self.tmp / "work" / "a.cs")))
        for cmd in ("echo hi>f", "git show HEAD:a.txt>f", "sort -o f a", "uniq a f", "find . -fprint0 f",
                    "echo hi # ' \ntouch PWNED # '", "git grep -Orm pattern", "git fetch . HEAD:refs/heads/main2",
                    "echo (New-Item PWNED9)", "gci | where { Remove-Item b.txt }", "python /tmp/evil/health.py",
                    "python Tools/pipeline/run_detached.py start status --timeout 9 -- git push -f origin main",
                    "sed -i s/a/b/ f", "awk '{print > \"f\"}' a", "rg --pre evil x", "tee f", "cp a b", "curl -o f http://x"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        for cmd in ("echo x > Assets/a.cs", 'python -c "open(\'a\',\'w\')"', "sed -i s/a/b/ a.cs", "rm -rf Assets",
                    "git apply p.patch", "git commit -m x", "git.exe commit -m x", "git --no-pager commit -m x",
                    "git add -A", "git diff --output=a.cs", "git stash", "python build.py", "ls; rm a",
                    "cat a `rm b`", "Get-Content a | Out-File b"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        self.assertTrue(self.denied("PowerShell", command="Set-Content a.cs x"))
        self.assertTrue(self.denied("Edit", file_path=str(self.tmp / "work" / "a.cs")))
        self.assertTrue(self.denied("Write", file_path=str(desk / "other.md")))
        self.assertTrue(self.denied("Write", file_path="plan.md"))                 # relative: not the desk
        self.assertTrue(self.denied("Edit", agent=True, file_path=str(self.tmp / "work" / "a.cs")))
        self.assertTrue(self.denied("Bash", agent=True, command="git commit -m x"))

    def test_what_real_legs_were_refused_now_passes_and_the_writers_beside_it_do_not(self):
        relay = str(HERE / "relay.py").replace("\\", "/")
        self.leg("plan")
        for cmd in ('cd Assets && for f in a.cs b.cs c.unity; do ls -la "$f"; done',
                    "ls gate.ps1 && git --version && git log --oneline -1 && git status --porcelain | head",
                    "git --version", "python --version",
                    'cd Tools && python "%s" leg done 2>&1 | head -20' % relay,
                    'sed -n 276,282p a.cs; echo "=== houses"; python -c "\nimport json;d=json.load(open(\'a.json\'))\n'
                    'print(len(d), sorted(d)[:3])\n"',
                    "if test -f a.cs; then echo yes; fi"):
            self.assertFalse(self.denied("Bash", command=cmd), cmd)
        for cmd in ('for f in a.cs b.cs; do rm "$f"; done', 'for c in "rm -rf x"; do $c; done',
                    'python -c "from pathlib import Path; Path(\'a\').write_text(\'x\')"',
                    'python -c "import subprocess; subprocess.run([\'git\', \'push\'])"',
                    'python -c "import os; os.remove(\'a\')"', 'python -c "open(\'a\', \'a\').close()"',
                    'cd Tools && python "%s" leg finish' % relay, 'python "%s" leg done > x' % relay,
                    "git --version --exec-path=/tmp", "python -m pip install x"):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)
        self.leg("execute", nn=2)                              # the unit was a fix to land.py itself
        for cmd in ('python -c "\nfrom pathlib import Path\nfor f in [\'Tools/land.py\', \'Tools/selftest.py\']:\n'
                    '    p=Path(f); p.write_bytes(p.read_bytes())\n" && file Tools/land.py',
                    'python -c "print(open(\'Tools/land.py\').read()[:80])"'):
            self.assertFalse(self.denied("Bash", command=cmd), cmd)
        for cmd in ('python -c "exec(open(\'Tools/land.py\').read())"',
                    'python -c "import runpy; runpy.run_path(\'Tools/land.py\')"',
                    'python -c "import subprocess; subprocess.run([\'python\', \'Tools/land.py\'])"'):
            self.assertTrue(self.denied("Bash", command=cmd), cmd)

    def test_a_plan_leg_may_check_its_plan_and_its_card_names_the_command(self):
        d = self.leg("plan")
        relay = str(HERE / "relay.py").replace("\\", "/")
        self.assertFalse(self.denied("Bash", command='python "%s" leg done' % relay))
        for bad in ('python "%s" leg finish' % relay, 'python "%s" leg gate start' % relay,
                    'python "%s" leg done > x' % relay, "python other/relay.py leg done",
                    'python "%s" run --work .' % relay):
            self.assertTrue(self.denied("Bash", command=bad), bad)
        import prompt
        self.assertIn("leg done", prompt.card_text(legdir.read(d), "body"))
        self.assertNotIn("leg finish", prompt.card_text(legdir.read(d), "body"))

    def test_a_critic_leg_only_reads_writes_its_paper_and_starts_no_subagent(self):
        d = self.leg("critic")
        desk = legdir.desk(d)
        relay = str(HERE / "relay.py").replace("\\", "/")
        self.assertFalse(self.denied("Write", file_path=str(desk / "critic.md")))
        self.assertFalse(self.denied("Read", file_path="near.jpg"))
        self.assertFalse(self.denied("Bash", command="ls"))
        self.assertFalse(self.denied("Bash", command='python "%s" leg done' % relay))
        self.assertTrue(self.denied("Agent", prompt="score it for me"))
        self.assertTrue(self.denied("Write", file_path=str(desk / "bundle" / "near.jpg")))
        self.assertTrue(self.denied("Edit", file_path=str(self.tmp / "work" / "a.cs")))
        self.assertTrue(self.denied("Bash", command="rm near.jpg"))
        self.leg("plan", nn=2)                                 # a plan leg may still start one
        self.assertFalse(self.denied("Agent", prompt="look this up"))

    def test_at_red_only_the_close_out_works(self):
        d = self.leg()
        self.red()
        relay = str(HERE / "relay.py").replace("\\", "/")
        for ok in ("git status", "git diff --stat", "git log --oneline -3", 'python "%s" leg finish' % relay,
                   'python "%s" leg gate wait --max 540' % relay):
            self.assertFalse(self.denied("Bash", command=ok), ok)
        self.assertFalse(self.denied("Write", file_path=str(legdir.desk(d) / "note.md")))
        self.assertFalse(self.denied("Read", file_path="x"))
        for bad in ("git status; rm -rf x", "git commit -m x", "python build.py", "git diff --output=Sim.cs",
                    "git diff HEAD>f", "git status # ' \nrm -rf x # '", "git diff -U99999",
                    "git log --output=x.txt", "git -C ../githubtest log -p", "git log -p --all",
                    'python "%s" leg finish > x' % relay, "python other/relay.py leg finish"):
            self.assertTrue(self.denied("Bash", command=bad), bad)
        self.assertTrue(self.denied("Edit", file_path=str(self.tmp / "work" / "a.cs")))
        self.assertTrue(self.denied("Agent", prompt="x"))
        self.assertTrue(self.denied("Edit", agent=True, file_path=str(self.tmp / "work" / "a.cs")))
        self.assertTrue(self.denied("Bash", agent=True, command="python build.py"))
        self.assertTrue(self.denied("WebFetch", url="x"))

    def test_compaction_is_blocked_and_recorded(self):
        d = self.leg()
        code, _ = self.hook("pre-compact", {"trigger": "auto", "session_id": "s1"})
        self.assertEqual(code, 2)
        self.assertEqual(json.loads((d / "compact.json").read_text(encoding="utf-8"))["trigger"], "auto")

    def test_session_start_hands_over_the_card(self):
        d = self.leg()
        (d / "card.md").write_text("LEG CARD: house5 evidence", encoding="utf-8")
        _, o = self.hook("session-start", {"session_id": "s1", "transcript_path": "t"})
        self.assertEqual(o["additionalContext"], "LEG CARD: house5 evidence")
        self.assertEqual(json.loads((d / "session.json").read_text(encoding="utf-8"))["session_id"], "s1")

    def test_the_hooks_template_names_every_event(self):
        raw = json.loads((HERE / "relay-hooks.json").read_text(encoding="utf-8"))["hooks"]
        self.assertEqual(sorted(raw), ["PostToolBatch", "PreCompact", "PreToolUse", "SessionStart"])


PLAN = """## Goal
Add a.txt to the repo.
## Steps
1. Create `a.txt` with one line.
## Files
- `a.txt` (new)
## Checks
- `git status --porcelain` prints nothing.
## Done when
a.txt is in HEAD.
## Risks
"""
CRITIC = """VERDICT: shots ROUND 1: %d/100 - the far band is too dark to read.
CAPTURES: valid
READABILITY: holds
FINDINGS: | MAJOR | far band dark | far.jpg, lower third | lift the haze |
RUBRIC: coverage 20/30, look 15/30, budget 15/20, repeatability 5/10, cost 5/10
TOP-3 MANDATED FIXES:
1. Fix the first thing in `out.txt`.
2. Fix the second thing.
3. Fix the third thing.
COULD NOT JUDGE: the motion, from stills.
"""
RETRO = """## What happened
Legs 1 and 2 ran clean and ended far under amber.
## Tuning
- amber_tokens: 150000 - both legs ended under 60k
- `leg_minutes`: 60 - no leg took over 20 minutes
- run_hours: 99 - not mine to move
## Proposals
- roles/_phase_plan.md: say that a plan needs no more than six steps (leg 1 wrote eleven).
"""
NOTE = """## Goal
Add a.txt to the repo.
## Done
Nothing is committed yet.
## In flight
## Next
Create the file, commit, push.
## Predictions
- `git status --porcelain` -> exit 0
- `git rev-parse --abbrev-ref HEAD` -> contains "lane/show/x"
## Dead ends
"""


class Papers(Base):
    def test_a_good_plan_passes_and_a_thin_one_is_named(self):
        self.assertEqual(papers.check_plan(PLAN, 6144), [])
        thin = "".join("## %s\nx\n" % s for s in papers.PLAN_SECTIONS)
        bad = papers.check_plan(thin, 6144)
        self.assertTrue(any("says nothing" in b for b in bad) and any("numbered" in b for b in bad), bad)
        self.assertTrue(any("bytes" in b for b in papers.check_plan(PLAN + "x" * 7000, 6144)))
        self.assertTrue(any("Checks" in b for b in papers.check_plan(PLAN.replace("`git status --porcelain`", "look at it"), 6144)))

    def test_a_critic_paper_needs_a_score_and_three_fixes(self):
        good = CRITIC % 72
        self.assertEqual((papers.check_critic(good, 8192), papers.critic_score(good)), ([], 72))
        self.assertEqual(papers.critic_fixes(good)[2], "Fix the third thing.")
        self.assertEqual(papers.critic_score("**VERDICT:** shots ROUND 2: 91 / 100 - fine"), 91)
        for bad, word in ((good.replace("72/100", "about seventy"), "VERDICT"), (CRITIC % 140, "VERDICT"),
                          (good.replace("3. Fix the third thing.\n", ""), "lists 2 fixes"),
                          ("I liked it.\n" + good, "VERDICT"), (good + "x" * 9000, "bytes")):
            self.assertTrue(any(word in b for b in papers.check_critic(bad, 8192)), (word, papers.check_critic(bad, 8192)))
        fix = papers.plan_for_fixes(PLAN, papers.critic_fixes(good), 1, 72, 85)
        self.assertIn("scored the work 72/100 in round 1, the target is 85", fix)
        self.assertIn("1. Fix the first thing", fix)
        self.assertNotIn("Create `a.txt`", fix)               # the old steps are gone, the rest of the plan stays
        self.assertIn("## Checks", fix)

    def test_a_retrospective_needs_its_sections_and_tunes_only_what_it_may(self):
        self.assertEqual(papers.check_retro(RETRO, 6144), [])
        self.assertEqual(papers.retro_tuning(RETRO, config.RETRO_TUNES), {"amber_tokens": 150000, "leg_minutes": 60})
        self.assertEqual(papers.check_retro("## What happened\nLegs ran clean.\n## Tuning\n## Proposals\n", 6144), [])
        self.assertTrue(any("Tuning" in b for b in papers.check_retro("## What happened\nLegs ran clean.\n", 6144)))
        self.assertTrue(any("bytes" in b for b in papers.check_retro(RETRO + "x" * 7000, 6144)))

    def test_a_plan_naming_a_missing_file_is_refused(self):
        for name in ("src/gone.cs", "gone.cs", "Assets\\Gone.cs"):
            plan = PLAN.replace("- `a.txt` (new)", "- `%s`" % name)
            self.assertTrue(any("does not exist" in b for b in papers.check_plan(plan, 6144, self.tmp)), name)
        self.assertEqual(papers.check_plan(PLAN, 6144, self.tmp), [])              # (new) on its own line
        (self.tmp / "trench-warfare-3d" / "Tools").mkdir(parents=True)             # what the first real plan named
        (self.tmp / "trench-warfare-3d" / "Tools" / "health.py").write_text("x", encoding="utf-8")
        for name in ("Tools/health.py", "BOARD/evidence/house5/balance.md", ".../destruction/tests.md"):
            plan = PLAN.replace("- `a.txt` (new)", "- `%s`" % name)
            self.assertEqual(papers.check_plan(plan, 6144, self.tmp), [], name)
        subprocess.run(["git", "init", "-q"], cwd=str(self.tmp), check=True)
        subprocess.run(["git", "add", "-A"], cwd=str(self.tmp), check=True, capture_output=True)
        self.assertEqual(papers.check_plan(PLAN.replace("- `a.txt` (new)", "- `health.py`"), 6144, self.tmp), [])

    def test_leg_breaks_cut_the_steps_and_too_many_is_refused(self):
        plan = PLAN.replace("1. Create `a.txt` with one line.", "1. `one`\n--- leg break ---\n2. `two`")
        parts = papers.plan_steps(plan)
        self.assertEqual(parts, ["1. `one`", "2. `two`"])
        two = papers.plan_for_part(plan, parts, 2)
        self.assertIn("part 2 of 2", two)
        self.assertNotIn("1. `one`", two)
        self.assertIn("## Checks", two)
        many = PLAN.replace("1. Create `a.txt` with one line.", "\n--- leg break ---\n".join("%d. `s`" % i for i in range(1, 9)))
        self.assertTrue(any("too big" in b for b in papers.check_plan(many, 6144)))

    def test_a_note_needs_look_only_predictions(self):
        self.assertEqual(papers.check_note(NOTE, 4096), [])
        for bad in ("git push origin x", "git diff HEAD~0 --output=pwned.txt", "git status; rm -rf x", "python build.py"):
            note = NOTE.replace("git status --porcelain", bad)
            self.assertTrue(any("look-only" in b for b in papers.check_note(note, 4096)), bad)
        self.assertTrue(papers.check_note(NOTE.replace('- `git status --porcelain` -> exit 0\n', "- it will be fine\n"), 4096))
        self.assertTrue(any("bytes" in b for b in papers.check_note(NOTE + "y" * 5000, 4096)))

    def test_a_prediction_that_writes_is_never_run(self):
        note = NOTE.replace("git status --porcelain", "git diff HEAD --output=pwned.txt")
        self.assertEqual(papers.run_predictions(note, self.tmp)[0][1], False)
        self.assertFalse((self.tmp / "pwned.txt").exists())


class Repo(Base):
    """A real git repo with an origin, a board folder and the fake claude."""
    def setUp(self):
        super().setUp()
        for k in ("GIT_AUTHOR_NAME", "GIT_COMMITTER_NAME"):
            os.environ[k] = "relay-test"
        for k in ("GIT_AUTHOR_EMAIL", "GIT_COMMITTER_EMAIL"):
            os.environ[k] = "relay@test"
        self.origin, self.work = self.tmp / "origin.git", self.tmp / "work"
        self.g(["init", "-q", "--bare", str(self.origin)], self.tmp)
        self.work.mkdir()
        self.g(["init", "-q"], self.work)
        (self.work / "README.md").write_text("x\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "init"])
        self.g(["branch", "-M", gitio.INTEGRATION])
        self.g(["remote", "add", "origin", str(self.origin)])
        self.g(["push", "-q", "origin", gitio.INTEGRATION])
        for sub in ("items", "results", "feedback", "claims", "relay/queue"):
            (self.board / sub).mkdir(parents=True)
        os.environ.update(TW_BOARD=str(self.board), TW_STATION="desktop",
                          TW_RELAY_CLAUDE=json.dumps([sys.executable, str(HERE / "fake_claude.py")]))

    def g(self, args, cwd=None):
        return subprocess.run(["git"] + args, cwd=str(cwd or self.work), check=True, capture_output=True)

    def queue(self, uid, done_when=("git", "cat-file", "-e", "HEAD:a.txt"), raw=None):
        (self.board / "relay" / "queue" / (uid + ".json")).write_text(raw if raw is not None else json.dumps(
            {"id": uid, "lane": "lane/show/x", "role": "lane", "goal": "Add a.txt", "done_when": list(done_when)}),
            encoding="utf-8")

    def script(self, obj):
        p = self.tmp / "fake.json"
        p.write_text(json.dumps(obj), encoding="utf-8")
        os.environ["TW_FAKE_SCRIPT"] = str(p)

    def args(self, **kw):
        a = argparse.Namespace(work=str(self.work), sources=["lane"], hours=None, leg_minutes=None, leg_budget=None,
                               max_legs=0, dry_run=False, no_push=True, no_quiet=True, allow_dirty=True)
        vars(a).update(kw)
        return a

    def go(self, **kw):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            r = runner.Run(self.args(**kw))
            code = r.loop()
        stops = sorted((self.board / "relay" / "desktop" / "stops").glob("*.json"))
        self.code = code
        return buf.getvalue(), (json.loads(stops[-1].read_text(encoding="utf-8")) if stops else None)

    COMMIT = [{"file": "a.txt", "text": "one\n"}, {"git": ["add", "-A"]}, {"git": ["commit", "-q", "-m", "a"]},
              {"git": ["push", "-q", "origin", "lane/show/x"]}]
    GOOD = {"plan": [{"write": "plan.md", "text": PLAN}], "execute": COMMIT}


class Checkout(Repo):
    def test_one_leg_per_checkout(self):
        home = self.tmp / "home"
        self.assertIsNone(gitio.busy_reason(self.work, home, quiet=0))
        gitio.take_lock(self.work, home, "me")
        self.assertIsNone(gitio.busy_reason(self.work, home, quiet=0))                  # our own lock
        rec = json.loads(gitio.lock_path(self.work, home).read_text(encoding="utf-8"))
        rec.update(pid=4, pid_start=-1)                                        # a holder that is gone
        gitio.lock_path(self.work, home).write_text(json.dumps(rec), encoding="utf-8")
        self.assertIsNone(gitio.lock_holder(self.work, home))

    def test_a_guard_file_left_by_a_crash_blocks_only_for_a_minute(self):
        home = self.tmp / "home"
        guard = Path(str(gitio.lock_path(self.work, home)) + ".lock")
        guard.parent.mkdir(parents=True)
        guard.write_text("", encoding="utf-8")
        with self.assertRaises(gitio.GitError):
            gitio.take_lock(self.work, home, "me")
        old = time.time() - 120
        os.utime(guard, (old, old))
        gitio.take_lock(self.work, home, "me")
        self.assertEqual(gitio.lock_holder(self.work, home)["who"], "me")

    def test_dirty_names_files_exactly(self):
        (self.work / "README.md").write_text("changed\n", encoding="utf-8")        # the first line of git status
        (self.work / "new file.cs").write_text("a\n", encoding="utf-8")
        (self.work / "plain.txt").write_text("a\n", encoding="utf-8")
        self.assertEqual(gitio.dirty(self.work), ["README.md", "new file.cs", "plain.txt"])
        self.assertNotIn("gone", gitio.file_hashes(self.work, gitio.dirty(self.work)).values())

    def test_a_snapshot_restores_untracked_and_binary_work_and_notices_a_change(self):
        home, leg = self.tmp / "home", self.tmp / "legx"
        (self.work / "new file.cs").write_text("half\n", encoding="utf-8")
        (self.work / "pic.bin").write_bytes(bytes(range(256)) * 4)
        (self.work / "README.md").write_text("edited\n", encoding="utf-8")
        self.assertIn("uncommitted", gitio.busy_reason(self.work, home, quiet=0))
        snap = gitio.snapshot_dirty(self.work, leg)
        self.assertIsNone(gitio.busy_reason(self.work, home, snap, quiet=0))
        clone = self.tmp / "clone"
        self.g(["clone", "-q", str(self.work), str(clone)], self.tmp)
        self.g(["apply", str(leg / "red.patch")], clone)
        self.assertEqual((clone / "new file.cs").read_text(encoding="utf-8"), "half\n")
        self.assertEqual((clone / "pic.bin").read_bytes(), bytes(range(256)) * 4)
        (self.work / "new file.cs").write_text("changed by someone\n", encoding="utf-8")
        self.assertFalse(gitio.matches_snapshot(self.work, snap))
        self.assertIn("uncommitted", gitio.busy_reason(self.work, home, snap, quiet=0))

    def test_only_a_change_outside_docs_is_progress(self):
        a = gitio.head(self.work)
        (self.work / "docs").mkdir(); (self.work / "docs" / "n.md").write_text("n\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "docs"])
        b = gitio.head(self.work)
        self.assertEqual(gitio.code_changed(self.work, a, b), [])
        (self.work / "c.py").write_text("1\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "code"])
        self.assertEqual(gitio.code_changed(self.work, b, gitio.head(self.work)), ["c.py"])


class Runs(Repo):
    def legs(self):
        return [json.loads(l.read_text(encoding="utf-8")) for l in sorted((self.board / "relay" / "desktop" / "legs").glob("*.json"))]

    def test_a_unit_is_planned_executed_checked_and_recorded(self):
        self.queue("u1")
        self.script(self.GOOD)
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        self.assertEqual((stop["reason"], stop["legs"], self.code), ("nothing left to do", 2, 0))
        self.assertTrue((self.board / "relay" / "done" / "u1.json").exists())
        self.assertEqual([l["phase"] for l in self.legs()], ["plan", "execute"])
        self.assertEqual(gitio.branch(self.work), "lane/show/x")
        self.assertIsNone(gitio.lock_holder(self.work, legdir.home()))
        guard = legdir.leg_path(stop["run"], 2)                                 # the hooks really ran
        self.assertTrue((guard / "session.json").exists())
        self.assertIn("LEG CARD", (guard / "card.md").read_text(encoding="utf-8").upper())

    ROLED = {"id": "u1", "lane": "lane/show/x", "goal": "Add a.txt", "done_when": ["git", "cat-file", "-e", "HEAD:a.txt"]}

    def test_a_queued_unit_with_a_role_gets_the_brief_of_that_role_as_a_board_job_does(self):
        self.queue("u1", raw=json.dumps(dict(self.ROLED, role="vehicle-simulator")))
        self.script(self.GOOD)
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        runs = self.tmp / "home" / "runs"
        for nn in ("01", "02"):                                                 # the plan leg and the execute leg
            self.assertTrue((next(runs.glob("*/desk/" + nn)) / "brief" / "tw-vehicle-sim" / "SKILL.md").is_file(), nn)
            self.assertIn("Read `brief/tw-vehicle-sim/SKILL.md` in your leg folder",
                          (next(runs.glob("*/legs/" + nn)) / "card.md").read_text(encoding="utf-8"))

    def test_a_queued_unit_with_the_role_lane_or_one_nobody_knows_runs_without_a_brief(self):
        from sources import lane as SL
        self.assertIn("no brief for the role `painter`", SL.body(dict(self.ROLED, role="painter")))
        self.assertNotIn("brief", SL.body(dict(self.ROLED, role="lane")))
        self.assertNotIn("brief", SL.body(dict(self.ROLED, role="review-fix")))   # its rules ride in the system prompt
        for role in ("lane", "painter", "review-fix"):
            self.assertIsNone(SL.place_brief(role, self.tmp / "desk"))
        self.assertFalse((self.tmp / "desk").exists())
        self.queue("u1", raw=json.dumps(dict(self.ROLED, role="painter")))        # a file from before the table:
        self.script(self.GOOD)                                                  # it still runs, and passes
        self.assertIn("unit u1: PASS", self.go()[0])

    def test_a_leg_record_holds_what_the_leg_used_of_the_week(self):
        self.queue("u1")
        self.script(self.GOOD)
        asked, real = [], launch.usage.read

        def reading(home=None, lim=None, **kw):           # a new reading every time: 0.5 points more of the week
            asked.append(1)
            n = len(asked)
            return {"at": "2026-10-05T10:%02d:00Z" % n, "at_s": 1000 + 60 * n, "week": 40.0 + n / 2.0,
                    "five_hour": 3.0, "resets": "2026-10-08 16:00", "windows": {}}
        launch.usage.read = reading
        try:
            out, stop = self.go()
        finally:
            launch.usage.read = real
        self.assertIn("unit u1: PASS", out)
        plan, execute = self.legs()
        self.assertEqual((plan["week_start"]["week"], plan["week_end"]["week"], plan["week_used"]), (40.5, 41.0, 0.5))
        self.assertEqual((execute["week_start"]["week"], execute["week_used"]), (41.5, 0.5))
        self.assertEqual(sorted(plan["week_end"]), ["at", "at_s", "resets", "week"])
        self.assertEqual(ledger.standing(self.board)["week"], 42.0)
        self.assertEqual(ledger.week(ledger.spent(self.board), ledger.rate(self.board)), (1.0, 0))

    def test_a_leg_with_no_reading_or_a_reading_that_breaks_is_not_measured_and_runs_all_the_same(self):
        self.queue("u1")
        self.script(self.GOOD)
        real = launch.usage.read

        def broken(home=None, lim=None, **kw):
            raise OSError("the file is gone")
        launch.usage.read = broken
        try:
            out, stop = self.go()
        finally:
            launch.usage.read = real
        self.assertIn("unit u1: PASS", out)
        for leg in self.legs():
            self.assertEqual((leg["week_start"], leg["week_end"], leg["week_used"]), (None, None, None))
        self.assertIsNone(ledger.rate(self.board))

    def test_the_guards_are_live_inside_a_run(self):
        self.queue("u1")
        self.script({"plan": [{"tool": "Bash", "input": {"command": "git commit -m x"}},
                              {"tool": "Edit", "input": {"file_path": str(self.work / "README.md")}},
                              {"tokens": 250000}, {"write": "plan.md", "text": PLAN}], "execute": self.COMMIT})
        _, stop = self.go()
        guard = legdir.leg_path(stop["run"], 1)
        self.assertEqual(len((guard / "denials.jsonl").read_text(encoding="utf-8").splitlines()), 2)
        self.assertEqual(self.legs()[0]["level"], "amber")

    def test_a_dry_run_starts_and_claims_nothing(self):
        self.queue("u1")
        out, stop = self.go(dry_run=True)
        self.assertIn("would run: lane u1", out)
        self.assertIsNone(stop)
        self.assertFalse((legdir.home() / "runs").exists())

    def test_work_already_there_costs_no_leg(self):
        self.g(["switch", "-q", "-c", "lane/show/x"])
        (self.work / "a.txt").write_text("one\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "a"]); self.g(["push", "-q", "origin", "lane/show/x"])
        self.queue("u1")
        out, stop = self.go()
        self.assertIn("already done", out)
        self.assertEqual(stop["legs"], 0)
        self.assertTrue((self.board / "relay" / "done" / "u1.json").exists())

    def test_no_plan_means_no_execute_leg_and_two_empty_units_stop_the_run(self):
        for u in ("u1", "u2", "u3"):
            self.queue(u)
        self.script({"plan": []})
        out, stop = self.go()
        self.assertIn("wrote no plan.md", out)
        self.assertEqual(stop["legs"], 2)
        self.assertIn("2 units in a row", stop["reason"])

    def test_a_commit_that_is_not_pushed_is_not_progress(self):
        for u in ("u1", "u2", "u3"):
            self.queue(u, done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.script({"plan": [{"write": "plan.md", "text": PLAN}],
                     "execute": [{"file": "junk.py", "text": "1\n"}, {"git": ["add", "-A"]},
                                 {"git": ["commit", "-q", "--allow-empty", "-m", "junk"]}]})
        out, stop = self.go()
        self.assertIn("2 units in a row", stop["reason"])
        self.assertEqual(stop["legs"], 4)
        self.assertEqual(stop["reason_kind"], "no-result")

    def test_work_that_fails_its_check_is_not_done_and_not_picked_again(self):
        self.queue("u1", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.script(self.GOOD)
        out, stop = self.go()
        self.assertIn("unit u1: FAIL", out)
        self.assertIn("done_when exited", out)
        self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())
        self.assertEqual(stop["legs"], 2)
        self.assertEqual(stop["units"], {"u1": "FAIL"})         # "nothing left to do" does not hide it
        self.assertEqual(stop["refusals"], 0)
        self.assertIn("1 unit: 1 FAIL", out)

    def test_a_retrospective_tunes_inside_the_bounds_and_leaves_proposals_for_the_owner(self):
        self.queue("u1")
        self.script(dict(self.GOOD, retro=[{"write": "retro.md", "text": RETRO}]))
        real = config.limits
        config.limits = lambda *a, **k: dict(real(*a, **k), retro_every_legs=2)
        try:
            out, stop = self.go()
        finally:
            config.limits = real
        self.assertEqual((stop["legs"], stop["units"]), (3, {"u1": "PASS"}))     # plan, execute, retrospective
        tuned = json.loads((self.board / "relay" / "desktop" / "tuning.json").read_text(encoding="utf-8"))["limits"]
        self.assertEqual((tuned["amber_tokens"], tuned["leg_minutes"]), (200000, 60))   # 150000 is under the bound
        self.assertNotIn("run_hours", tuned)
        self.assertIn("leg_minutes is now 60", out)
        props = list((self.board / "relay" / "proposals").glob("*.md"))
        self.assertEqual(len(props), 1)
        self.assertIn("six steps", props[0].read_text(encoding="utf-8"))
        leg3 = json.loads(next((self.tmp / "home" / "runs").glob("*/legs/03/leg.json")).read_text(encoding="utf-8"))
        bundle = Path(leg3["worktree"])
        self.assertEqual((leg3["phase"], leg3["board"]), ("retro", ""))
        self.assertEqual(len(list((bundle / "legs").glob("*.json"))), 2)
        self.assertTrue((bundle / "limits.json").exists() and (bundle / "limits-now.json").exists())
        # the next run on this station starts with the tuning; a flag outranks it
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            self.assertEqual(runner.Run(self.args()).lim["leg_minutes"], 60)
            self.assertEqual(runner.Run(self.args(leg_minutes=45)).lim["leg_minutes"], 45)

    def test_a_retrospective_without_its_sections_changes_nothing(self):
        self.queue("u1")
        self.script(dict(self.GOOD, retro=[{"write": "retro.md", "text": "All good, raise amber_tokens: 260000.\n"}]))
        real = config.limits
        config.limits = lambda *a, **k: dict(real(*a, **k), retro_every_legs=2)
        try:
            out, stop = self.go()
        finally:
            config.limits = real
        self.assertIn("retrospective: nothing taken", out)
        self.assertFalse((self.board / "relay" / "desktop" / "tuning.json").exists())
        self.assertEqual(stop["reason"], "nothing left to do")

    def test_view_opens_one_tab_per_leg_through_a_command_file_and_never_where_no_window_can_open(self):
        stub, log = self.tmp / "wt_stub.py", self.tmp / "wt.log"
        stub.write_text("import json, sys\nopen(%r, 'a').write(json.dumps(sys.argv[1:]) + '\\n')\n" % str(log),
                        encoding="utf-8")
        os.environ["TW_RELAY_WT"] = json.dumps([sys.executable, str(stub)])
        self.queue("u1")
        self.script(self.GOOD)
        self.go(view=True)
        for _ in range(50):                                   # the tabs are started, not waited for
            if log.exists() and len(log.read_text(encoding="utf-8").splitlines()) == 2:
                break
            time.sleep(0.1)
        tabs = [json.loads(l) for l in log.read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(tabs), 2)
        for tab in tabs:
            self.assertEqual(tab[:3] + tab[5:8], ["-w", "tw-relay", "new-tab", "cmd.exe", "/d", "/c"])
            self.assertRegex(tab[4], r"^[A-Za-z0-9 -]+$")       # the title: nothing a shell could read as more
            text = Path(tab[8]).read_text(encoding="utf-8")
            self.assertIn("relay.py", text)
            self.assertIn("--follow", text)
        log.unlink()
        os.environ["TW_RELAY_NO_WINDOW"] = "1"
        self.queue("u2", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.go(view=True)
        time.sleep(1)
        self.assertFalse(log.exists())
        os.environ["TW_RELAY_WT"] = json.dumps(["no-such-program-anywhere"])
        os.environ["TW_RELAY_NO_WINDOW"] = "0"
        d = legdir.new_leg("r9", 1, UNIT, "execute", self.ph["execute"], self.lim, self.work, LANE, self.board)
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            self.assertFalse(launch.open_view(d))               # a tab that cannot open is a note, not an error
        self.assertIn("no viewer tab", buf.getvalue())

    def test_what_the_guard_refused_is_counted_and_can_be_read_back(self):
        self.queue("u1")
        self.script({"plan": [{"tool": "Bash", "input": {"command": "rm -rf Assets"}},
                              {"write": "plan.md", "text": PLAN}], "execute": self.COMMIT})
        out, stop = self.go()
        self.assertEqual((stop["refusals"], self.legs()[0]["guard_refusals"]), (1, 1))
        self.assertIn("the guard refused 1 command", out)
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            relay.main(["refusals"])
        self.assertIn("rm -rf Assets", buf.getvalue())
        self.assertIn("this phase only reads", buf.getvalue())
        self.assertIn("1 refused in 1 run.", buf.getvalue())

    def test_a_call_claude_code_refused_itself_is_not_a_call_the_guard_missed(self):
        d = legdir.new_leg("r7", 1, UNIT, "execute", self.ph["execute"], self.lim, self.work, LANE, self.board)
        use = lambda i, name: json.dumps({"type": "assistant", "message": {"content": [
            {"type": "tool_use", "id": i, "name": name, "input": {"command": "x"}}]}})
        result = lambda i, text, err: json.dumps({"type": "user", "message": {"content": [
            {"type": "tool_result", "tool_use_id": i, "is_error": err, "content": text}]}})
        (d / "out.jsonl").write_text("\n".join([
            use("t1", "Bash"), result("t1", "ok", False),
            use("t2", "Bash"), result("t2", "<tool_use_error>Blocked: sleep 60 followed by: tail</tool_use_error>", True),
            use("t3", "Read"), result("t3", [{"type": "text", "text": "file not found"}], True)]) + "\n", encoding="utf-8")
        (d / "calls.jsonl").write_text(json.dumps({"id": "t1", "tool": "Bash"}) + "\n"
                                       + json.dumps({"id": "t3", "tool": "Read"}) + "\n", encoding="utf-8")
        self.assertEqual(launch.unguarded(d), [])               # t2 never ran: Claude Code refused it before any hook
        good = {"state": "DONE", "exit_code": 0, "has_result": True, "subtype": "success", "ran_mode": "auto",
                "hooked": True, "guard_intact": True, "transcript_read": True, "final_tokens": 10, "red_tokens": 300000,
                "tool_uses": 3, "guard_calls": 2}
        self.assertIsNone(launch.ran_clean(dict(good, unguarded=[])))
        (d / "calls.jsonl").write_text(json.dumps({"id": "t1", "tool": "Bash"}) + "\n", encoding="utf-8")
        self.assertEqual(launch.unguarded(d), ["Read"])         # t3 ran (it has a real result) and no guard saw it
        self.assertIn("did not see 1 of its 3", launch.ran_clean(dict(good, unguarded=["Read"])))
        self.assertIn("saw 2 of its 3", launch.ran_clean(dict(good, unguarded=None)))    # no ids: by count

    def test_work_queued_during_a_leg_does_not_stop_the_run_and_other_board_changes_do(self):
        self.queue("u1")
        q = str(self.board / "relay" / "queue" / "u9.json")
        self.script(dict(self.GOOD, plan=[{"abs": q, "text": json.dumps(
            {"id": "u9", "lane": "lane/show/x", "role": "lane", "goal": "g", "done_when": ["git", "cat-file", "-e", "HEAD:a.txt"]})},
            {"write": "plan.md", "text": PLAN}]))
        out, stop = self.go()
        self.assertNotIn("broke a rule", stop["reason"])
        self.assertEqual(stop["units"]["u1"], "PASS")

    def test_a_run_uses_committed_code_and_says_who_started_it_and_how_far_it_is(self):
        self.queue("u1")
        self.script(self.GOOD)
        real = gitio.code_state
        gitio.code_state = lambda folder: ("abc1234567890", ["Tools/relay/roles/_common.md"])
        try:
            out, stop = self.go(allow_dirty=False)
            self.assertIn("uncommitted changes (Tools/relay/roles/_common.md)", stop["reason"])
            self.assertEqual(stop["reason_kind"], "checkout")
            self.assertEqual(stop["legs"], 0)
            gitio.code_state = lambda folder: ("abc1234567890", [])
            seen = []
            run_leg = launch.run_leg

            def watched(d, *a):                              # what `status` prints while a leg is going
                buf = io.StringIO()
                with contextlib.redirect_stdout(buf):
                    relay.status()
                seen.append(buf.getvalue())
                return run_leg(d, *a)
            launch.run_leg = watched
            try:
                out, stop = self.go(allow_dirty=False, who="pc-73")
            finally:
                launch.run_leg = run_leg
        finally:
            gitio.code_state = real
        self.assertEqual((stop["started_by"], stop["code"], stop["units"]), ("pc-73", "abc1234567890", {"u1": "PASS"}))
        self.assertIn("started by pc-73, relay code abc1234567", out)
        self.assertIn("started by pc-73, relay code abc1234567; so far 1 legs", seen[0])
        commit, changed = real(self.work)                       # the real check, on a real checkout
        self.assertEqual((len(commit), changed), (40, []))

    def test_one_name_holds_the_relay_build_and_a_hold_ends_by_itself(self):
        def cmd(*args):
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                code = relay.main(["hold"] + list(args))
            return code, buf.getvalue()
        self.assertEqual(cmd("pc-73")[0], 0)
        self.assertEqual(cmd("pc-73", "--hours", "2")[0], 0)        # the holder renews
        code, out = cmd("pc-70")
        self.assertEqual(code, 1)
        self.assertIn("HELD by pc-73", out)
        self.assertEqual(cmd("pc-70", "--release")[0], 1)           # only the holder gives it back
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            relay.status()
        self.assertIn("held by pc-73", buf.getvalue())
        p = self.tmp / "home" / relay.HOLD
        p.write_text(json.dumps(dict(json.loads(p.read_text(encoding="utf-8")), until_s=time.time() - 1)), encoding="utf-8")
        self.assertEqual(cmd("pc-70")[0], 0)                         # the old hold ran out
        self.assertEqual(cmd("pc-70", "--release")[0], 0)
        self.assertIsNone(relay.holder())

    def test_update_moves_only_a_frozen_copy_and_never_under_a_run(self):
        with self.assertRaises(SystemExit) as e:                    # this checkout is on a branch: it is for building
            relay.update("HEAD")
        self.assertIn("frozen copy", str(e.exception))
        gitio.take_lock(self.work, self.tmp / "home", "relay r1 u1", "lane/show/x")
        with self.assertRaises(SystemExit) as e:
            relay.update("HEAD")
        self.assertIn("a run is going", str(e.exception))

    def test_a_stop_sends_one_notification_and_none_where_no_window_can_open(self):
        log = self.tmp / "toast.log"
        stub = self.tmp / "toast_stub.py"
        stub.write_text("import os\nopen(%r, 'a').write(os.environ.get('TW_RELAY_TOAST', '') + '\\n')\n" % str(log),
                        encoding="utf-8")
        os.environ["TW_RELAY_NOTIFY"] = json.dumps([sys.executable, str(stub)])
        self.queue("u1")
        self.script(self.GOOD)
        self.go()
        for _ in range(50):
            if log.exists() and log.read_text(encoding="utf-8").strip():
                break
            time.sleep(0.1)
        self.assertEqual(log.read_text(encoding="utf-8").splitlines(),
                         ["Relay stopped: nothing left to do. 2 legs, 1 unit: 1 PASS."])
        log.unlink()
        os.environ["TW_RELAY_NO_WINDOW"] = "1"
        self.go()
        time.sleep(1)
        self.assertFalse(log.exists())

    def test_a_run_that_cannot_open_a_window_says_so_on_every_card(self):
        self.queue("u1")
        self.script(self.GOOD)
        os.environ["TW_RELAY_NO_WINDOW"] = "1"
        out, stop = self.go()
        self.assertIn("cannot open a window", out)
        cards = sorted((self.tmp / "home" / "runs").glob("*/legs/*/card.md"))
        self.assertEqual(len(cards), 2)
        self.assertTrue(all("cannot open a window" in c.read_text(encoding="utf-8") for c in cards))
        self.assertTrue(all("UNITY_CLI_ALLOW_LOCKED" in c.read_text(encoding="utf-8") for c in cards))
        self.assertTrue(all("%LOCALAPPDATA%/unity/bin/unity.exe" in c.read_text(encoding="utf-8") for c in cards))
        self.assertEqual(stop["units"], {"u1": "PASS"})
        self.assertEqual(runner.tally({"a": "PASS", "b": "FAIL", "c": "PASS"}), ", 3 units: 2 PASS, 1 FAIL")

    WAITS = "I'll pause here and wait for the background PlayMode test run to finish before proceeding."

    def results(self, stop, nn):
        """The result records of one leg's session: one per turn it was given."""
        return launch.records(legdir.leg_path(stop["run"], nn), "result")

    def test_a_leg_that_ends_with_no_result_line_gets_one_more_turn_and_its_work_counts(self):
        """rv-12-hud-f9, 2026-10-08: the leg ended its turn to wait, and the unit was recorded as failed."""
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"report": self.WAITS}],
                     "execute+resume": [{"report": "RESULT: done. The test had finished: 1 run, 1 passed."}],
                     "cost": {"plan": 1, "execute": 2}})
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        leg = self.legs()[1]
        self.assertEqual((leg["resumed"], leg["state"], leg["cost_usd"]), (1, "DONE", 4))    # both turns are paid for
        self.assertEqual(len(self.results(stop, 2)), 2)
        self.assertIn("no line that starts with RESULT", (legdir.leg_path(stop["run"], 2) / "resume.txt").read_text(encoding="utf-8"))
        self.assertEqual(self.legs()[0].get("resumed"), 0)                      # the plan leg said done: one turn

    def test_a_leg_that_still_has_no_result_after_its_extra_turn_fails_and_gets_no_third(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"report": self.WAITS}]})
        out, stop = self.go()
        self.assertIn("unit u1: FAIL", out)
        self.assertIn("execute leg 1 of 1 reported failed", out)
        self.assertEqual(len(self.results(stop, 2)), 2)

    def test_no_extra_turn_at_red_for_a_leg_that_only_reads_or_one_that_gave_its_verdict(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}, {"report": "The plan is written."}],
                     "execute": self.COMMIT + [{"tokens": 301000}, {"report": self.WAITS}]})
        out, stop = self.go()
        self.assertEqual(len(self.results(stop, 1)), 1)                         # a plan leg's paper is its result
        self.assertEqual((len(self.results(stop, 2)), self.legs()[1]["level"]), (1, "red"))   # at red it was told to close
        self.assertIn("unit u1: FAIL", out)
        self.assertEqual(launch.said("RESULT: failed. A check is red."), "failed")

    def test_a_blocked_or_failed_leg_is_never_a_pass(self):
        for word, verdict in (("**RESULT: blocked** - needs the owner", "BLOCKED"), ("RESULT: failed. A check is red.", "FAIL")):
            shutil.rmtree(self.board / "relay", ignore_errors=True)
            (self.board / "relay" / "queue").mkdir(parents=True)
            self.queue("u1")
            self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"report": word}]})
            out, _ = self.go()
            self.assertIn("unit u1: " + verdict, out)
            self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())
            self.g(["switch", "-q", gitio.INTEGRATION]); self.g(["branch", "-q", "-D", "lane/show/x"])
            self.g(["push", "-q", "origin", "--delete", "lane/show/x"])

    def test_a_failed_part_stops_the_later_parts(self):
        self.queue("u1")
        plan = PLAN.replace("1. Create `a.txt` with one line.", "1. `one`\n--- leg break ---\n2. `two`\n--- leg break ---\n3. `three`")
        self.script({"plan": [{"write": "plan.md", "text": plan}], "execute": [{"report": "RESULT: failed. Step 1 broke."}]})
        out, stop = self.go()
        self.assertEqual(stop["legs"], 2)
        self.assertIn("execute leg 1 of 3 reported failed", out)

    def test_each_part_gets_only_its_own_steps(self):
        self.queue("u1")
        plan = PLAN.replace("1. Create `a.txt` with one line.", "1. `one`\n--- leg break ---\n2. `two`")
        self.script({"plan": [{"write": "plan.md", "text": plan}], "execute#3": self.COMMIT})
        _, stop = self.go()
        card = (legdir.leg_path(stop["run"], 3) / "card.md").read_text(encoding="utf-8")
        self.assertIn("part 2 of 2", card)
        self.assertNotIn("1. `one`", card)

    def test_a_good_note_reaches_the_next_run_with_its_predictions_scored(self):
        self.queue("u1", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"write": "note.md", "text": NOTE}]})
        self.go()
        self.assertTrue((self.board / "relay" / "notes" / "u1.md").exists())
        self.g(["switch", "-q", gitio.INTEGRATION])          # the next run starts on another branch
        _, stop = self.go(max_legs=1)
        card = (legdir.leg_path(stop["run"], 1) / "card.md").read_text(encoding="utf-8")
        self.assertIn("Note from the last leg", card)
        self.assertEqual(card.count("- held:"), 2)

    def test_a_bad_note_is_not_handed_on(self):
        self.queue("u1", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        bad = NOTE.replace("git status --porcelain", "git push origin x")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"write": "note.md", "text": bad}]})
        self.go()
        self.assertFalse((self.board / "relay" / "notes" / "u1.md").exists())

    def test_uncommitted_work_left_behind_fails_the_unit_and_is_saved(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": [{"file": "half.txt", "text": "half\n"}]})
        out, stop = self.go()
        self.assertIn("left uncommitted work", stop["reason"])
        self.assertEqual(stop["reason_kind"], "uncommitted")
        self.assertIn(b"half.txt", (legdir.leg_path(stop["run"], 2) / "red.patch").read_bytes())
        self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())

    def test_a_compaction_trip_stops_the_run_and_saves_the_dirty_tree(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}],
                     "execute": [{"file": "a.txt", "text": "half\n"}, {"compact": True}]})
        _, stop = self.go()
        self.assertIn("COMPACT", stop["reason"])
        self.assertEqual(stop["reason_kind"], "leg")
        self.assertIn(b"a.txt", (legdir.leg_path(stop["run"], 2) / "red.patch").read_bytes())

    def test_a_leg_over_its_time_is_killed_and_the_run_stops(self):
        self.queue("u1")
        self.script({"plan": [{"sleep": 120}]})
        r = runner.Run(self.args())
        r.lim["leg_minutes"] = 0.05
        with contextlib.redirect_stdout(io.StringIO()):
            r.loop()
        stop = json.loads(sorted((self.board / "relay" / "desktop" / "stops").glob("*.json"))[-1].read_text(encoding="utf-8"))
        self.assertIn("TIMEOUT", stop["reason"])
        self.assertEqual(stop["reason_kind"], "leg")
        leg = legdir.read(legdir.leg_path(stop["run"], 1))
        self.assertIsNone(launch.proc_start(leg["child_pid"]))

    def test_the_run_time_cap_ends_a_leg_and_starts_no_other(self):
        self.queue("u1")
        self.script({"plan": [{"sleep": 120}]})
        r = runner.Run(self.args())
        r.deadline = time.time() + 3
        with contextlib.redirect_stdout(io.StringIO()):
            r.loop()
        stop = json.loads(sorted((self.board / "relay" / "desktop" / "stops").glob("*.json"))[-1].read_text(encoding="utf-8"))
        self.assertIn("hours are up", stop["reason"])
        self.assertEqual(stop["reason_kind"], "hours")
        self.assertEqual(stop["legs"], 1)

    def test_a_leg_that_cannot_be_trusted_stops_the_run_without_a_verdict(self):
        cases = (({"mode": "default", "plan": [{"write": "plan.md", "text": PLAN}]}, "not auto"),
                 ({"no_hooks": True, "plan": [{"write": "plan.md", "text": PLAN}]}, "hooks did not run"),
                 ({"plan": [{"exit": 2}]}, "exited 2"),
                 ({"subtype": "error_max_turns", "plan": [{"write": "plan.md", "text": PLAN}]}, "not success"),
                 ({"no_result": True, "plan": []}, "no result record"))
        for script, why in cases:
            shutil.rmtree(self.board / "relay", ignore_errors=True)
            (self.board / "relay" / "queue").mkdir(parents=True)
            self.queue("u1")
            self.script(script)
            out, stop = self.go()
            self.assertIn(why, stop["reason"])
            self.assertEqual(stop["reason_kind"], "leg")
            self.assertNotIn("unit u1: FAIL", out)
            self.assertEqual(stop["legs"], 1)

    def test_an_interrupt_kills_the_leg(self):
        self.queue("u1")
        self.script({"plan": [{"sleep": 120}]})
        d = launch.make_leg("r9", 1, dict(UNIT, role="lane"), "plan", self.work, "lane/show/x", self.board, "x", self.lim)
        real = time.sleep

        def interrupt(s):
            real(0.2)
            raise KeyboardInterrupt
        launch.time.sleep = interrupt
        try:
            with self.assertRaises(KeyboardInterrupt):
                launch.run_leg(d, self.lim, 600)
        finally:
            launch.time.sleep = real
        leg = legdir.read(d)
        self.assertEqual(leg["state"], "STOPPED")
        self.assertIsNone(launch.proc_start(leg["child_pid"]))

    def test_any_error_still_writes_a_stop_record(self):
        self.queue("u1", raw="{ not json")
        _, stop = self.go()
        self.assertIn("not valid JSON", stop["reason"])
        self.assertEqual(stop["reason_kind"], "error")
        self.assertEqual(self.code, 1)
        shutil.rmtree(self.board / "relay")
        (self.board / "relay" / "queue").mkdir(parents=True)
        self.queue("u1")
        self.script(self.GOOD)
        real = gitio.switch_lane

        def boom(*a):
            raise RuntimeError("boom")
        gitio.switch_lane = boom                              # an error nobody foresaw, with the checkout held
        try:
            _, stop = self.go()
        finally:
            gitio.switch_lane = real
        self.assertIn("error: boom", stop["reason"])
        self.assertEqual(stop["reason_kind"], "error")
        self.assertIsNone(gitio.lock_holder(self.work, legdir.home()))

    HELD = {"lane": "lane/show/held", "role": "lane", "goal": "g", "done_when": ["git", "cat-file", "-e", "HEAD:a.txt"]}

    def test_a_lane_checked_out_elsewhere_fails_its_unit_and_the_next_unit_still_runs(self):
        # 2026-10-09: a spare checkout had lane/show/banner-recut out and the whole run ended with 0 legs, twice
        self.g(["worktree", "add", "-q", str(self.tmp / "other"), "-b", "lane/show/held"])
        self.queue("u1", raw=json.dumps(dict(self.HELD, id="u1")))
        self.queue("u2")
        self.script(self.GOOD)
        out, stop = self.go()
        self.assertEqual(stop["units"], {"u1": "FAIL", "u2": "PASS"})
        self.assertEqual((stop["reason"], stop["legs"]), ("nothing left to do", 2))     # u1 cost no leg
        self.assertIn("its lane lane/show/held cannot be switched to", out)
        self.assertIsNone(gitio.lock_holder(self.work, legdir.home()))

    def test_a_row_of_lanes_that_cannot_be_switched_to_stops_the_run(self):
        self.g(["worktree", "add", "-q", str(self.tmp / "other"), "-b", "lane/show/held"])
        for i in range(1, 8):
            self.queue("u%d" % i, raw=json.dumps(dict(self.HELD, id="u%d" % i)))
        _, stop = self.go()
        self.assertIn("5 units in a row whose lane cannot be switched to", stop["reason"])
        self.assertEqual(stop["reason_kind"], "lane")
        self.assertEqual((len(stop["units"]), stop["legs"]), (5, 0))

    def test_a_queue_file_with_a_bad_id_or_a_string_command_is_refused(self):
        for raw in ({"id": "../../escaped", "lane": "lane/show/x", "role": "lane", "goal": "g", "done_when": ["true"]},
                    {"id": "u1", "lane": "lane/show/x", "role": "lane", "goal": "g", "done_when": "python Tools\\x.py && rm x"},
                    {"id": "u1", "lane": "main", "role": "lane", "goal": "g", "done_when": ["true"]}):
            self.queue("u1", raw=json.dumps(raw))
            _, stop = self.go()
            self.assertEqual(self.code, 1, raw)
            self.assertEqual(stop["legs"], 0)

    def test_a_plan_leg_that_says_blocked_gets_no_execute_leg(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}, {"report": "RESULT: blocked. The owner must decide."}],
                     "execute": self.COMMIT})
        out, stop = self.go()
        self.assertIn("unit u1: BLOCKED", out)
        self.assertEqual(stop["legs"], 1)

    def test_git_itself_refuses_a_leg_pushing_anywhere_but_its_lane(self):
        self.queue("u1")
        for push in (["push", "-q", "origin", "HEAD:refs/heads/other"], ["push", "-q", "origin", "HEAD:" + gitio.INTEGRATION],
                     ["push", "-q", "--force", "origin", "HEAD~1:lane/show/x"], ["push", "-q", "origin", ":lane/show/x"]):
            self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"git": push}]})
            before = gitio.remote_heads(self.work).get("refs/heads/" + gitio.INTEGRATION)
            _, stop = self.go()
            heads = gitio.remote_heads(self.work)
            self.assertIn("exited 1", stop["reason"], push)
            self.assertNotIn("refs/heads/other", heads)
            self.assertEqual(heads.get("refs/heads/" + gitio.INTEGRATION), before)
            self.assertEqual(heads.get("refs/heads/lane/show/x"), gitio.head(self.work))     # the good push stands
            self.g(["switch", "-q", gitio.INTEGRATION]); self.g(["branch", "-q", "-D", "lane/show/x"])
            gitio.git(["push", "-q", "origin", "--delete", "lane/show/x"], self.work)
        self.g(["push", "-q", "origin", "HEAD:refs/heads/free"])                # no leg holds it now: git lets it go
        self.assertIn("refs/heads/free", gitio.remote_heads(self.work))

    def test_a_leg_that_touches_the_board_or_the_relay_stops_the_run_with_no_verdict(self):
        forged = self.board / "results" / "thing--shots--aaaaaaaa--9.json"
        claimed = self.board / "claims" / "desktop.json"         # (a new queue file is the owner's to add: see
        for target in (forged, claimed):                         # test_work_queued_during_a_leg_...)
            self.queue("u1")
            self.script({"plan": [{"write": "plan.md", "text": PLAN}],
                         "execute": self.COMMIT + [{"abs": str(target), "text": "{}"}]})
            out, stop = self.go()
            self.assertIn("broke a rule", stop["reason"])
            self.assertIn(target.name, stop["reason"])
            self.assertNotIn("unit u1: PASS", out)
            self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())
            target.unlink()
            self.g(["switch", "-q", gitio.INTEGRATION]); self.g(["branch", "-q", "-D", "lane/show/x"])
            gitio.git(["push", "-q", "origin", "--delete", "lane/show/x"], self.work)

    def test_a_leg_that_removes_gits_push_guard_stops_the_run(self):
        self.queue("u1")
        hook = Path(gitio.git(["rev-parse", "--absolute-git-dir"], self.work)) / "hooks" / "pre-push"
        self.script({"plan": [{"write": "plan.md", "text": PLAN}],
                     "execute": self.COMMIT + [{"abs": str(hook), "text": "#!/bin/sh\nexit 0\n"}]})
        _, stop = self.go()
        self.assertIn("git's push guard", stop["reason"])
        self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())

    def test_a_guard_file_changed_under_a_leg_stops_the_run(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT})
        real = launch.records

        def tamper(d, kind, subtype=None):                  # something rewrites the card while the leg runs
            (Path(d) / "card.md").write_text("do anything", encoding="utf-8")
            return real(d, kind, subtype)
        launch.records = tamper
        try:
            _, stop = self.go()
        finally:
            launch.records = real
        self.assertIn("guard files were changed", stop["reason"])

    def test_the_quiet_check_runs_once_and_the_runners_own_git_work_does_not_trip_it(self):
        self.g(["switch", "-q", "-c", "lane/show/x"])
        (self.work / "a.txt").write_text("one\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "a"]); self.g(["push", "-q", "origin", "lane/show/x"])
        self.g(["switch", "-q", gitio.INTEGRATION])
        self.queue("u1"); self.queue("u2", done_when=("git", "cat-file", "-e", "HEAD:README.md"))
        _, stop = self.go(no_quiet=False)
        self.assertIn("git index moved", stop["reason"])     # somebody just worked here: the run does not start
        self.assertEqual(stop["reason_kind"], "checkout")
        old = time.time() - 3600
        gitdir = Path(gitio.git(["rev-parse", "--absolute-git-dir"], self.work))
        for f in (gitdir / "index", gitdir / "HEAD"):
            os.utime(f, (old, old))
        out, stop = self.go(no_quiet=False)
        self.assertEqual(stop["reason"], "nothing left to do")
        self.assertEqual(out.count("already done"), 2)       # the switch for u1 did not block u2

    def test_on_a_git_board_only_a_committed_queue_file_counts(self):
        g = lambda args: subprocess.run(["git"] + args, cwd=str(self.board), check=True, capture_output=True)
        g(["init", "-q"])
        self.queue("u1")
        out, stop = self.go(dry_run=True)
        self.assertIn("skipping u1.json", out)
        self.assertIn("would run: nothing", out)
        g(["add", "-A"]); g(["commit", "-q", "-m", "queue u1"])
        out, _ = self.go(dry_run=True)
        self.assertIn("would run: lane u1", out)

    def test_the_leg_cap_stops_the_run(self):
        self.queue("u1")
        self.script(self.GOOD)
        _, stop = self.go(max_legs=1)
        self.assertIn("leg cap", stop["reason"])
        self.assertEqual(stop["reason_kind"], "legs")

    def test_a_missing_work_checkout_is_named_as_missing(self):
        self.queue("u1")
        out, stop = self.go(work=str(self.tmp / "nowhere"))
        self.assertIn("does not exist", stop["reason"])
        self.assertIn("git worktree add", stop["reason"])
        self.assertEqual(stop["legs"], 0)

    def test_the_checkout_is_held_between_units_and_freed_at_the_stop(self):
        self.queue("u1"); self.queue("u2")
        self.script(self.GOOD)
        held, real = [], runner.sources.next_unit

        def peek(names, ctx):                               # asked between the units: is the marker still there?
            held.append(gitio.marker_path(self.work).exists())
            return real(names, ctx)
        runner.sources.next_unit = peek
        try:
            self.go()
        finally:
            runner.sources.next_unit = real
        self.assertEqual(held[:2], [False, True])           # before the first unit: free; after it: still held
        self.assertFalse(gitio.marker_path(self.work).exists())
        self.assertIsNone(gitio.lock_holder(self.work, legdir.home()))

    def test_another_sessions_push_is_noted_and_does_not_stop_the_run(self):
        self.queue("u1")
        self.script(self.GOOD)
        other = self.tmp / "elsewhere"
        self.g(["clone", "-q", str(self.origin), str(other)], self.tmp)
        run_leg = launch.run_leg

        def with_a_neighbour(d, lim, t, *more):             # somebody lands a lane while the leg runs
            leg = run_leg(d, lim, t, *more)
            self.g(["push", "-q", "origin", "origin/%s:refs/heads/lane/show/neighbour" % gitio.INTEGRATION], other)
            return leg
        launch.run_leg = with_a_neighbour
        try:
            out, stop = self.go()
        finally:
            launch.run_leg = run_leg
        self.assertIn("unit u1: PASS", out)
        self.assertEqual(self.legs()[0]["moved_on_origin"], ["lane/show/neighbour"])
        self.assertEqual(stop["moved_on_origin"], ["lane/show/neighbour"])

    def test_a_leg_may_not_push_from_any_other_checkout(self):
        gitio.install_prepush(self.work)
        env = dict(os.environ, TW_RELAY="1")
        r = subprocess.run(["git", "push", "-q", "origin", "HEAD:refs/heads/sneak"], cwd=str(self.work), env=env,
                           capture_output=True)
        self.assertIn(b"only from the checkout it holds", r.stderr)
        self.assertNotIn("refs/heads/sneak", gitio.remote_heads(self.work))

    def test_the_owners_stop_ends_the_run_before_the_next_leg_or_at_once(self):
        self.queue("u1")
        self.script(self.GOOD)
        run_leg = launch.run_leg

        def then_stop(d, lim, t, *more):
            leg = run_leg(d, lim, t, *more)
            with contextlib.redirect_stdout(io.StringIO()):
                relay.main(["stop"])
            return leg
        launch.run_leg = then_stop
        try:
            _, stop = self.go()
        finally:
            launch.run_leg = run_leg
        self.assertIn("stopped by the owner", stop["reason"])
        self.assertEqual(stop["legs"], 1)
        self.script({"plan": [{"sleep": 120}]})
        t = threading.Timer(4, lambda: relay.stop(True))
        t.start()
        began = time.time()
        _, stop = self.go()
        t.join()
        self.assertIn("stopped by the owner", stop["reason"])
        self.assertLess(time.time() - began, 60)
        self.assertEqual((stop["reason_kind"], stop["asked_why"], stop["now_on"]), ("asked", "", "u1"))
        leg = legdir.read(legdir.leg_path(stop["run"], 1))
        self.assertIsNone(launch.proc_start(leg["child_pid"]))

    def test_add_queues_committed_lane_work_and_refuses_a_bad_lane(self):
        g = lambda args: subprocess.run(["git"] + args, cwd=str(self.board), check=True, capture_output=True)
        g(["init", "-q"])
        with contextlib.redirect_stdout(io.StringIO()):
            relay.main(["add", "u9", "--lane", "lane/show/x", "--role", "lane", "--goal", "Add a.txt", "--done-when",
                        "git", "cat-file", "-e", "HEAD:a.txt"])
            out, _ = self.go(dry_run=True)
        self.assertIn("would run: lane u9", out)
        with self.assertRaises(SystemExit), contextlib.redirect_stdout(io.StringIO()):
            relay.main(["add", "u10", "--lane", "main", "--role", "lane", "--goal", "g", "--done-when", "true"])
        self.assertFalse((self.board / "relay" / "queue" / "u10.json").exists())

    def test_add_in_words_names_its_role_or_is_refused(self):
        g = lambda args: subprocess.run(["git"] + args, cwd=str(self.board), check=True, capture_output=True)
        g(["init", "-q"])
        with self.assertRaises(SystemExit) as e, contextlib.redirect_stdout(io.StringIO()):
            relay.main(["add", "u11", "--lane", "lane/show/x", "--goal", "g", "--done-when", "true"])
        self.assertIn("add needs --role", str(e.exception))
        self.assertIn("with a brief: balance-simulator", str(e.exception))   # roles whose legs get a brief, first
        self.assertIn("review-fix", str(e.exception).split("With none")[0])  # its brief is its own role text
        self.assertFalse((self.board / "relay" / "queue" / "u11.json").exists())
        with contextlib.redirect_stdout(io.StringIO()):
            relay.main(["add", "u11", "--lane", "lane/show/x", "--role", "review-fix", "--goal", "g", "--done-when", "true"])
        self.assertEqual(json.loads((self.board / "relay" / "queue" / "u11.json").read_text(encoding="utf-8"))["role"],
                         "review-fix")

    def test_a_route_moves_one_roles_legs_of_one_phase_and_no_other(self):
        config.routes()                                              # the shipped file reads, whatever it holds
        mine = self.tmp / "myroutes"                                 # the test's own routes: the shipped ones may go
        shutil.copytree(HERE, mine, ignore=shutil.ignore_patterns("*.py", "roles", "sources", "__pycache__"))
        (mine / "routes.json").write_text(json.dumps({"routes": [
            {"role": "review-fix", "phase": "execute", "model": "sonnet", "effort": "medium", "units": ["in-trial"]},
            {"role": "optimizer", "phase": "plan", "effort": "medium"}]}), encoding="utf-8")
        real = config.route
        config.route = lambda role, phase, folder=None, unit=None: real(role, phase, mine, unit)
        self.addCleanup(setattr, config, "route", real)
        ph, made = config.phases(), {}
        legs = (("review-fix", "execute", "in-trial"), ("review-fix", "execute", "control"),
                ("review-fix", "plan", "in-trial"), ("lane", "execute", "in-trial"), ("optimizer", "plan", "any"))
        for n, (role, phase, uid) in enumerate(legs, 1):
            d = launch.make_leg("r8", n, dict(UNIT, role=role, id=uid), phase, self.work, "lane/show/x", self.board,
                                "x", self.lim)
            made[(role, phase, uid)] = (legdir.read(d)["model"], legdir.read(d)["effort"])
        own = lambda phase: (ph[phase]["model"], ph[phase]["effort"])
        self.assertEqual(made[("review-fix", "execute", "in-trial")], ("sonnet", "medium"))
        self.assertEqual(made[("review-fix", "execute", "control")], own("execute"))    # same role, not a listed unit
        self.assertEqual(made[("review-fix", "plan", "in-trial")], own("plan"))         # another phase
        self.assertEqual(made[("lane", "execute", "in-trial")], own("execute"))         # another role
        self.assertEqual(made[("optimizer", "plan", "any")], (ph["plan"]["model"], "medium"))   # no units: every unit
        d = launch.make_leg("r8", 9, dict(UNIT, role="review-fix", id="in-trial"), "execute", self.work, "lane/show/x",
                            self.board, "x", self.lim, model="opus")
        self.assertEqual(legdir.read(d)["model"], "opus")                               # a caller's model still wins
        # the budget prices a routed leg by its route: need() is asked with the unit's own phases
        asked = []
        price = lambda phase, model=None, effort=None: asked.append((phase, model, effort)) or 1.0
        ledger.need(price, config.routed(ph, {"role": "review-fix", "id": "in-trial"}), ("plan", "execute"))
        ledger.need(price, config.routed(ph, {"role": "review-fix", "id": "control"}), ("execute",))
        self.assertEqual(asked, [("plan",) + own("plan"), ("execute", "sonnet", "medium"), ("execute",) + own("execute")])

    def test_a_route_that_cannot_run_is_refused(self):
        bad = self.tmp / "badroutes"
        shutil.copytree(HERE, bad, ignore=shutil.ignore_patterns("*.py", "roles", "sources", "__pycache__"))
        for rows, why in (([{"role": "lane", "phase": "execute", "model": "haiku"}], "model is"),
                          ([{"role": "review_fix", "phase": "execute", "model": "sonnet"}], "no role named review_fix"),
                          ([{"role": "lane", "phase": "execute", "model": "sonnet", "units": []}], "units is a list"),
                          ([{"role": "lane", "phase": "paint", "model": "sonnet"}], "a phase of phases.json"),
                          ([{"role": "lane", "phase": "plan"}], "changes nothing"),
                          ([{"role": "lane", "phase": "plan", "model": "sonnet"}] * 2, "twice")):
            (bad / "routes.json").write_text(json.dumps({"routes": rows}), encoding="utf-8")
            with self.assertRaises(SystemExit) as e:
                config.routes(bad)
            self.assertIn(why, str(e.exception))
        (bad / "routes.json").write_text(json.dumps({"routes": []}), encoding="utf-8")
        self.assertEqual(config.route("review-fix", "execute", bad), {})            # no route: the phase's own


class Record(Repo):
    """What a run leaves for a reader on another machine: the stop record, the leg records, and while it is going
    its live record, with the board pushed after every leg."""
    legs = Runs.legs
    REPORT = ("RESULT: done. a.txt is in.\nNEEDS YOU: pick the colour\nof the roof.\nCHANGED:\n- a.txt\n"
              "NEXT: nothing")

    def board_repo(self):
        """The board as a station has it: a clone with an origin. What is queued by now is committed."""
        self.far = self.tmp / "board-origin.git"
        self.b(["init", "-q", "--bare", str(self.far)], self.tmp)
        self.b(["init", "-q"])
        self.b(["add", "-A"]); self.b(["commit", "-q", "-m", "board"]); self.b(["branch", "-M", "main"])
        self.b(["remote", "add", "origin", str(self.far)]); self.b(["push", "-q", "-u", "origin", "main"])

    def b(self, args, cwd=None):
        return subprocess.run(["git"] + args, cwd=str(cwd or self.board), check=True, capture_output=True)

    def far_file(self, rel):
        """A file as origin's board has it now, or None."""
        r = subprocess.run(["git", "show", "main:" + rel], cwd=str(self.far), capture_output=True)
        return json.loads(r.stdout.decode("utf-8")) if r.returncode == 0 else None

    def log(self, where):
        return subprocess.run(["git", "log", "--reverse", "--format=%s", "main"], cwd=str(where), check=True,
                              capture_output=True).stdout.decode("utf-8").splitlines()

    def during(self, look):
        """Run with look(leg number, leg folder) called as each leg starts to work: after the runner's watch."""
        run_leg = launch.run_leg

        def watched(d, *a):
            look(legdir.read(d)["leg"], d)
            return run_leg(d, *a)
        launch.run_leg = watched
        try:
            return self.go(no_push=False)
        finally:
            launch.run_leg = run_leg

    def test_the_stop_record_says_when_it_started_what_it_cost_and_why_a_unit_failed(self):
        self.queue("u1", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.script(dict(self.GOOD, cost={"plan": 1.5, "execute": 2}))
        out, stop = self.go()
        self.assertRegex(stop["started_at"], r"^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ$")
        self.assertLessEqual(stop["started_at"], stop["stopped_at"])
        self.assertEqual((stop["station"], stop["hours"], stop["reason_kind"]), ("desktop", 3, "done"))
        self.assertEqual((stop["run_usd"], stop["run_usd_unpriced"], stop["now_on"]), (3.5, 0, ""))
        self.assertEqual(list(stop["problems"]), ["u1"])
        self.assertIn("  - " + stop["problems"]["u1"][0], out)          # the line the run printed
        self.assertTrue(stop["problems"]["u1"][0].startswith("done_when exited"))
        self.assertNotIn("max_legs", stop)
        self.assertNotIn("asked_why", stop)
        r = runner.Run(self.args())
        r.keep_problems({"id": "u7"}, ["x" * 400] * 10)                # short lines, and not all of a long list
        self.assertEqual((len(r.problems["u7"]), set(map(len, r.problems["u7"]))), (8, {300}))

    def test_the_unit_in_flight_at_the_stop_is_named_and_a_leg_with_no_cost_is_counted(self):
        self.queue("u1")
        self.script(self.GOOD)
        _, stop = self.go(max_legs=1, hours=2)
        self.assertEqual((stop["now_on"], stop["units"], stop["max_legs"], stop["hours"]), ("u1", {}, 1, 2))
        self.assertEqual((stop["reason_kind"], stop["problems"]), ("legs", {}))
        self.script({"no_result": True, "plan": []})
        _, stop = self.go()
        self.assertEqual((stop["run_usd"], stop["run_usd_unpriced"], stop["now_on"]), (0, 1, "u1"))
        self.assertEqual(stop["reason_kind"], "leg")

    def test_a_unit_that_is_blocked_or_whose_lane_is_held_has_its_reason_in_the_record(self):
        self.g(["worktree", "add", "-q", str(self.tmp / "other"), "-b", "lane/show/held"])
        self.queue("u1", raw=json.dumps(dict(Runs.HELD, id="u1")))
        self.queue("u2")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}, {"report": "RESULT: blocked. The owner must decide."}]})
        _, stop = self.go()
        self.assertEqual(stop["units"], {"u1": "FAIL", "u2": "BLOCKED"})
        self.assertIn("its lane lane/show/held cannot be switched to", stop["problems"]["u1"][0])
        self.assertEqual(stop["problems"]["u2"], ["the plan leg reported blocked"])

    def test_every_stop_reason_in_the_runner_names_its_kind(self):
        with self.assertRaises(TypeError):
            runner.Stop("a reason nobody gave a kind")
        with self.assertRaises(ValueError):
            runner.Why("mood", "x")
        tree = ast.parse((HERE / "runner.py").read_text(encoding="utf-8"))
        name = lambda c: getattr(c.func, "id", None) or getattr(c.func, "attr", "")
        calls = [n for n in ast.walk(tree) if isinstance(n, ast.Call)]
        kinds = [c.args[0].value if c.args and isinstance(c.args[0], ast.Constant) else None
                 for c in calls if name(c) == "Why"]
        self.assertTrue(kinds and all(k in runner.KINDS for k in kinds), kinds)   # a kind in so many words, each time
        self.assertEqual(sorted(set(kinds)), sorted(runner.KINDS))                 # and no kind that nothing uses
        made = lambda v: isinstance(v, ast.Call) and name(v) in ("Why", "asked", "no_room", "no_budget")

        def values(f, var):
            """What the function assigns to a name, also as one of several (a, b = x, y)."""
            out = []
            for n in ast.walk(f):
                for t in n.targets if isinstance(n, ast.Assign) else []:
                    if isinstance(t, ast.Name) and t.id == var:
                        out.append(n.value)
                    for i, e in enumerate(t.elts if isinstance(t, ast.Tuple) else []):
                        if isinstance(e, ast.Name) and e.id == var:
                            out.append(n.value.elts[i] if isinstance(n.value, ast.Tuple) else n.value)
            return out
        stops = reasons = returns = 0
        for f in [n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef)]:
            for c in [n for n in ast.walk(f) if isinstance(n, ast.Call) and name(n) == "Stop"]:
                first = c.args[0]
                stops += 1
                self.assertTrue(made(first) or (isinstance(first, ast.Name) and values(f, first.id)
                                                and all(made(v) for v in values(f, first.id))),
                                "%s, line %d: a Stop with a reason of no kind" % (f.name, c.lineno))
            for v in values(f, "reason"):                      # what the stop record is written from
                reasons += 1
                self.assertTrue(made(v) or (isinstance(v, ast.Attribute) and v.attr == "args"),
                                "%s, line %d: a reason of no kind" % (f.name, v.lineno))
            for r in [n for n in ast.walk(f) if isinstance(n, ast.Return)] if f.name in ("asked", "no_room", "no_budget") else []:
                returns += 1
                self.assertTrue(r.value is None or made(r.value) or (isinstance(r.value, ast.Constant) and r.value.value is None),
                                "%s, line %d: gives a reason of no kind" % (f.name, r.lineno))
        self.assertTrue(stops >= 10 and reasons >= 4 and returns >= 10, (stops, reasons, returns))   # it did walk them

    def test_a_stop_somebody_asked_for_keeps_the_why_and_the_who_and_reads_as_before_without_them(self):
        r = runner.Run(self.args())
        with contextlib.redirect_stdout(io.StringIO()):
            relay.main(["stop"])
            self.assertEqual((str(r.asked()), r.asked().kind), ("stopped by the owner (relay.py stop)", "asked"))
            self.assertEqual(r.asked_of, {"asked_why": "", "asked_by": ""})
            self.assertEqual(sorted(json.loads(r.stop_file.read_text(encoding="utf-8"))), ["asked_at", "now"])
            relay.main(["stop", "--by", "watch_run"])               # no why: the words scripts match stay
            self.assertEqual(str(r.asked()), "stopped by the owner (relay.py stop)")
            self.assertEqual(r.asked_of["asked_by"], "watch_run")
            relay.main(["stop", "--why", "a  look at\nthe queue"])
            self.assertEqual(str(r.asked()), "stopped on request (relay.py stop): a look at the queue")
            relay.main(["stop", "--now", "--why", "x" * 400])
            self.assertEqual(len(r.asked(" --now", ", mid-leg 02")),
                             len("stopped on request (relay.py stop --now), mid-leg 02: ") + 300)
        self.queue("u1")
        self.script(self.GOOD)
        run_leg = launch.run_leg

        def then_stop(d, lim, t, *more):
            leg = run_leg(d, lim, t, *more)
            with contextlib.redirect_stdout(io.StringIO()):
                relay.main(["stop", "--why", "its time is up", "--by", "watch_run"])
            return leg
        launch.run_leg = then_stop
        try:
            out, stop = self.go()
        finally:
            launch.run_leg = run_leg
        self.assertEqual(stop["reason"], "stopped by watch_run (relay.py stop): its time is up")
        self.assertEqual((stop["reason_kind"], stop["asked_why"], stop["asked_by"], stop["legs"], stop["now_on"]),
                         ("asked", "its time is up", "watch_run", 1, "u1"))
        self.assertIn("STOP: stopped by watch_run (relay.py stop): its time is up.", out)

    def test_a_leg_record_holds_its_verdict_what_it_asks_of_the_owner_and_the_commits_it_made(self):
        self.queue("u1")
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.COMMIT + [{"report": self.REPORT}]})
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        plan, execute = self.legs()
        head = gitio.head(self.work)
        self.assertEqual((execute["said"], execute["needs_you"]), ("done", "pick the colour\nof the roof."))
        self.assertEqual((execute["head_after"], execute["commits"], execute["commits_more"]),
                         (head, [{"sha": head[:10], "subject": "a"}], 0))
        self.assertEqual((len(execute["head_before"]), execute["head_before"] != head), (40, True))
        self.assertEqual((plan["said"], plan["needs_you"], plan["commits"], plan["commits_more"]), ("done", "", [], 0))
        self.assertEqual(plan["head_before"], plan["head_after"])
        self.assertEqual((plan["session_id"], execute["session_id"]), ("fake", "fake"))
        self.script({"plan": [{"report": "I looked and gave up."}]})         # no verdict, nothing asked
        self.queue("u2", done_when=("git", "cat-file", "-e", "HEAD:never.txt"))
        self.go()
        self.assertEqual((self.legs()[-1]["said"], self.legs()[-1]["needs_you"]), (None, ""))

    def test_needs_you_is_read_up_to_the_next_heading_and_nothing_asked_is_empty(self):
        st = config.style()
        for said in ("nothing", "Nothing.", "none", "None.", "no", "No.", "-", "n/a", "N/A.", "", "**nothing**"):
            self.assertEqual(config.needs_you("RESULT: done.\nNEEDS YOU: %s\nCHANGED: x" % said, st), "", said)
        self.assertEqual(config.needs_you("RESULT: done.\nCHANGED: x\nNEXT: y", st), "")
        self.assertEqual(config.needs_you("", st), "")
        self.assertEqual(config.needs_you("**RESULT:** blocked.\n**NEEDS YOU:** say which lane lands first\n"
                                          "- banner\n- narrows\n**NEXT:** wait", st),
                         "say which lane lands first\n- banner\n- narrows")
        self.assertEqual(config.needs_you("RESULT: done\nneeds you: no window here.\nNext, run it at a desk.", st),
                         "no window here.\nNext, run it at a desk.")       # a word in a sentence is no heading
        for said in ("nothing needed.", "None - the lane is pushed.", "No action.", "no action needed", "\u2014",
                     "nothing.\nAll green.", "N/A (no decision in this unit)"):
            self.assertEqual(config.needs_you("RESULT: done.\nNEEDS YOU: %s\nCHANGED: x" % said, st), "", said)
        for said in ("No window can open here: start it at a desk", "None of the lanes can land until you pick one",
                     "Nothing works without the editor, open it"):                  # these do ask
            self.assertEqual(config.needs_you("RESULT: blocked.\nNEEDS YOU: %s\nNEXT: wait" % said, st), said)
        self.assertEqual(config.needs_you("RESULT: blocked.\nNEEDS YOU: pick one\n- NEXT: A\n- Result: B\nNEXT: wait", st),
                         "pick one\n- NEXT: A\n- Result: B")                      # a bullet under it is not a heading
        self.assertEqual(config.needs_you("## RESULT\nblocked\n## NEEDS YOU\npick one\n## NEXT\nwait", st), "pick one")
        self.assertEqual(config.needs_you("1. RESULT: done\n2. NEEDS YOU: pick\n3. CHANGED: x", st), "pick")

    def test_a_long_row_of_commits_is_cut_at_twenty_oldest_first_and_the_rest_is_counted(self):
        start = gitio.head(self.work)
        for i in range(23):
            self.g(["commit", "-q", "--allow-empty", "-m", "c%02d %s" % (i, "y" * 200 if i == 0 else "")])
        got, more = gitio.commits(self.work, start, gitio.head(self.work))
        self.assertEqual((len(got), more, got[1]["subject"], got[19]["subject"]), (20, 3, "c01", "c19"))
        self.assertEqual((len(got[0]["subject"]), len(got[0]["sha"])), (120, 10))
        self.assertEqual(gitio.commits(self.work, "", start), ([], 0))
        self.assertEqual(gitio.commits(self.work, start, start), ([], 0))
        self.g(["switch", "-q", "-c", "side", start]); self.g(["commit", "-q", "--allow-empty", "-m", "side"])
        self.assertEqual(gitio.commits(self.work, gitio.head(self.work), got[-1]["sha"]), ([], 0))   # not grown: rewritten

    def test_a_going_run_shows_on_origin_after_every_leg_and_its_live_record_goes_with_the_stop(self):
        self.queue("u1")
        self.board_repo()
        self.script(self.GOOD)
        seen = {}

        def look(nn, d):
            live = "relay/desktop/live/%s.json" % legdir.read(d)["run"]
            seen[nn] = (self.far_file(live), json.loads((self.board / live).read_text(encoding="utf-8")),
                        self.log(self.far)[-1], self.far_file("relay/desktop/legs/%s-01.json" % legdir.read(d)["run"]),
                        self.b(["status", "--porcelain"]).stdout)
        out, stop = self.during(look)
        run = stop["run"]
        self.assertEqual((stop["reason"], stop["units"]), ("nothing left to do", {"u1": "PASS"}))   # no rule broken:
        far, here, last, leg1, changed = seen[1]                        # the live file is written between the watches
        self.assertEqual((changed, seen[2][4]), (b"", b""))             # and committed: nothing changed under a leg
        self.assertEqual(sorted(far), ["beat", "code", "day_budget_pct", "hours", "leg_minutes", "legs", "now_on",
                                       "paced_until", "run", "started_at", "started_by", "station", "units"])
        self.assertEqual((far["run"], far["station"], far["legs"], far["units"], far["now_on"], far["hours"]),
                         (run, "desktop", 0, {}, None, 3))
        self.assertEqual((last, leg1), ("relay: run %s, started" % run, None))
        self.assertEqual({k: here["now_on"][k] for k in ("unit", "role", "phase", "leg", "lane")},
                         {"unit": "u1", "role": "lane", "phase": "plan", "leg": 1, "lane": "lane/show/x"})
        self.assertRegex(here["now_on"]["since"], r"^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ$")
        far, here, last, leg1, changed = seen[2]                        # leg 1 is over: origin has it
        self.assertEqual((last, leg1["phase"], far["legs"]), ("relay: run %s, leg 01 plan u1" % run, "plan", 1))
        self.assertEqual((far["now_on"]["unit"], far["now_on"]["phase"], far["now_on"]["leg"]), ("u1", "", 0))
        self.assertEqual((here["now_on"]["phase"], here["now_on"]["leg"], here["legs"]), ("execute", 2, 1))
        live = "relay/desktop/live/%s.json" % run
        self.assertEqual((self.far_file(live), (self.board / live).exists()), (None, False))   # a run never has both
        self.assertEqual(self.far_file("relay/desktop/stops/%s.json" % run)["reason"], "nothing left to do")
        self.assertEqual(self.log(self.far)[1:], ["relay: run %s, %s" % (run, w) for w in (
            "started", "leg 01 plan u1 starts", "leg 01 plan u1", "leg 02 execute u1 starts", "leg 02 execute u1",
            "unit u1 PASS", "2 legs, nothing left to do")])
        self.assertIn("Record on the board: pushed.", out)
        self.assertEqual(self.b(["status", "--porcelain"]).stdout, b"")

    def test_a_push_that_fails_between_legs_does_not_stop_the_run_and_goes_out_with_the_next(self):
        self.queue("u1")
        self.board_repo()
        self.script(self.GOOD)
        seen = {}

        def look(nn, d):
            seen[nn] = (self.log(self.far)[-1], self.log(self.board)[-2:])
            self.b(["remote", "set-url", "origin", str(self.tmp / "gone.git") if nn == 1 else str(self.far)])
        out, stop = self.during(look)
        run = stop["run"]
        self.assertEqual((stop["reason"], stop["units"], stop["legs"]), ("nothing left to do", {"u1": "PASS"}, 2))
        self.assertEqual(out.count("note: the board was not sent (push failed: the commit is kept locally)"), 1)
        self.assertEqual(seen[2], ("relay: run %s, started" % run, ["relay: run %s, leg %s" % (run, w) for w in (
            "01 plan u1", "02 execute u1 starts")]))                  # kept here, with what the run is on now
        self.assertIn("relay: run %s, leg 01 plan u1" % run, self.log(self.far))     # it went out with a later push
        self.assertEqual(self.far_file("relay/desktop/legs/%s-01.json" % run)["phase"], "plan")

    def test_origin_moving_during_a_leg_is_taken_in_and_a_queue_file_a_leg_wrote_is_not_committed(self):
        self.queue("u1")
        self.board_repo()
        other = self.tmp / "board-elsewhere"
        self.b(["clone", "-q", "-b", "main", str(self.far), str(other)], self.tmp)
        q = self.board / "relay" / "queue" / "u9.json"
        self.script(dict(self.GOOD, plan=[{"abs": str(q), "text": json.dumps(
            {"id": "u9", "lane": "lane/show/x", "role": "lane", "goal": "g", "done_when": ["git", "cat-file", "-e", "HEAD:a.txt"]})},
            {"write": "plan.md", "text": PLAN}]))
        seen = {}

        def look(nn, d):
            seen[nn] = (self.log(self.far)[-3:], self.b(["status", "--porcelain", "--", "relay/queue"]).stdout.decode(),
                        (self.board / "items" / "theirs.txt").exists())
            if nn == 1:                                         # another session pushes the board while leg 1 works
                self.b(["pull", "-q", "--rebase"], other)
                (other / "items").mkdir(exist_ok=True)
                (other / "items" / "theirs.txt").write_text("x\n", encoding="utf-8")
                self.b(["add", "-A"], other); self.b(["commit", "-q", "-m", "theirs"], other); self.b(["push", "-q"], other)
        out, stop = self.during(look)
        run = stop["run"]
        self.assertEqual((stop["reason"], stop["units"]), ("nothing left to do", {"u1": "PASS"}))
        self.assertEqual(seen[2][0], ["theirs"] + ["relay: run %s, leg 01 plan u1%s" % (run, w) for w in (" starts", "")])
        self.assertEqual(seen[2][1:], ("?? relay/queue/u9.json\n", True))              # the leg's file: not committed
        self.assertIn("skipping u9.json", out)                                           # so not taken as work

    def test_a_call_to_origin_that_hangs_is_given_up(self):
        self.queue("u1")
        self.board_repo()
        nap = ["-c", "alias.nap=!\"%s\" -c \"import time; time.sleep(5)\"" % sys.executable.replace("\\", "/"), "nap"]
        began = time.time()
        self.assertEqual(boardio._origin(nap, self.board, 2), 1)
        self.assertTrue(1.9 <= time.time() - began < 30, time.time() - began)   # it did hang, and was left
        self.assertEqual(boardio._origin(["status", "-s"], self.board, 20), 0)
        asked, real = [], boardio.push
        boardio.push = lambda board, message, **kw: asked.append(kw) or "pushed"
        try:
            r = runner.Run(self.args(no_push=False))
            r.send("leg 01 plan u1")
            boardio.push = lambda *a, **kw: 1 / 0               # whatever goes wrong in it is a note
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                r.send("leg 02 execute u1")
        finally:
            boardio.push = real
        self.assertEqual(asked, [{"tries": 1, "wait": boardio.QUICK_S, "leave": ("relay/queue",)}])    # one short try
        self.assertIn("note: the board was not sent (division by zero)", buf.getvalue())
        self.assertLessEqual(boardio.QUICK_S, 60)
        time.sleep(6)                                       # the sleeper git left behind is gone: the folder can go

    def test_between_legs_only_the_fetch_and_the_push_are_timed_and_no_pull_is_left_to_rebase_under_a_leg(self):
        asked, real = [], (boardio._origin, gitio.git_raw)
        boardio._origin = lambda args, board, wait: asked.append(("timed" if wait else "plain", args[0])) or 0
        gitio.git_raw = lambda args, cwd, env=None: asked.append(("plain", args[0])) or argparse.Namespace(returncode=0)
        try:
            self.assertEqual(boardio._onto_origin(self.board, 30), 0)
            self.assertEqual(asked, [("timed", "fetch"), ("plain", "rebase")])      # a pull given up may live on
            del asked[:]
            self.assertEqual(boardio._onto_origin(self.board, None), 0)
            self.assertEqual(asked, [("plain", "pull")])                            # at the stop: as it always was
        finally:
            boardio._origin, gitio.git_raw = real

    def test_a_rebase_left_half_way_is_ended_before_the_board_is_committed(self):
        self.queue("u1")
        self.board_repo()
        other = self.tmp / "board-elsewhere"
        self.b(["clone", "-q", "-b", "main", str(self.far), str(other)], self.tmp)
        for where, text in ((other, "theirs\n"), (self.board, "ours\n")):         # the same file, both sides
            (where / "relay" / "clash.txt").write_text(text, encoding="utf-8")
            self.b(["add", "-A"], where); self.b(["commit", "-q", "-m", "clash"], where)
        self.b(["push", "-q"], other)
        r = subprocess.run(["git", "pull", "--rebase", "-q"], cwd=str(self.board), capture_output=True)
        self.assertNotEqual(r.returncode, 0)                                      # stopped half way: an interrupt
        self.assertEqual(gitio.branch(self.board), "HEAD")
        (self.board / "relay" / "desktop" / "stops").mkdir(parents=True)
        (self.board / "relay" / "desktop" / "stops" / "r1.json").write_text("{}", encoding="utf-8")
        self.assertEqual(boardio.push(self.board, "relay: run r1, the stop"), "push failed: the commit is kept locally")
        self.assertEqual((gitio.branch(self.board), self.log(self.board)[-1]), ("main", "relay: run r1, the stop"))

    def test_a_board_with_nothing_of_the_relays_is_left_alone(self):
        bare = self.tmp / "bare-board"
        bare.mkdir()
        self.b(["init", "-q"], bare)
        (bare / "items.txt").write_text("x\n", encoding="utf-8")
        self.assertEqual(boardio.keep(bare, "relay: x", leave=("relay/queue",)), "nothing to push")
        self.assertEqual(self.b(["status", "--porcelain"], bare).stdout, b"?? items.txt\n")     # nothing staged

    def test_a_dry_run_and_a_run_that_never_started_leave_no_live_record(self):
        self.queue("u1")
        self.go(dry_run=True)
        _, stop = self.go(work=str(self.tmp / "nowhere"))
        self.assertEqual(stop["reason_kind"], "checkout")
        self.assertFalse((self.board / "relay" / "desktop" / "live").exists())


class Budget(Repo):
    """The day's budget: no leg starts once today's legs cost limits.json day_budget_usd (ledger.py does the sum)."""
    def legs(self):
        return [json.loads(l.read_text(encoding="utf-8")) for l in sorted((self.board / "relay" / "desktop" / "legs").glob("*.json"))]

    def spent(self, cost, phase="plan", n=1):
        """A leg another run finished earlier today."""
        p = self.board / "relay" / "desktop" / "legs" / ("%s-1-%02d.json" % (time.strftime("%Y%m%d-%H%M%S"), n))
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps({"run": p.stem[:-3], "leg": n, "unit": "earlier", "phase": phase, "model": "opus",
                                 "effort": "high", "cost_usd": cost,
                                 "started_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}), encoding="utf-8")

    def test_ordinary_work_under_the_budget_passes_and_its_cost_is_recorded(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 2}))
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        self.assertEqual((stop["reason"], stop["legs"], self.code), ("nothing left to do", 2, 0))
        self.assertEqual([(l["phase"], l["cost_usd"], l["tokens_in"], l["tokens_out"], l["cache_read"], l["cache_write"])
                          for l in self.legs()], [("plan", 1, 10, 20, 30, 40), ("execute", 2, 10, 20, 30, 40)])
        self.assertEqual((stop["day_usd"], stop["day_budget_usd"]), (3.0, 50))
        self.assertIn("Today: $3.00 of $50.00 spent, $47.00 left.", out)

    def test_a_spent_day_starts_no_leg(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(6)
        out, stop = self.go(day_budget=5)
        self.assertEqual((stop["reason"], stop["legs"], stop["units"]),
                         ("the day's budget is spent ($6.00 of $5.00)", 0, {}))
        self.assertEqual(stop["reason_kind"], "budget")
        self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())
        self.assertIsNone(gitio.lock_holder(self.work, legdir.home()))

    def test_a_dry_run_says_the_day_is_spent(self):
        self.queue("u1")
        self.spent(6)
        out, stop = self.go(dry_run=True, day_budget=5)
        self.assertIn("would run: nothing (the day's budget is spent ($6.00 of $5.00))", out)
        self.assertIsNone(stop)

    def test_a_unit_starts_only_when_the_day_covers_its_plan_and_its_execute(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(2, "plan", 1)
        self.spent(2, "execute", 2)
        out, stop = self.go(day_budget=5)                        # 1 left, and a unit usually costs 2 + 2
        self.assertEqual(stop["reason"], "the day's budget has $1.00 left of $5.00, and a unit usually costs $4.00")
        self.assertEqual(stop["reason_kind"], "budget")
        self.assertEqual(stop["legs"], 0)

    def test_the_run_stops_between_units_when_the_next_one_no_longer_fits(self):
        self.queue("u1")
        self.queue("u2", done_when=("git", "cat-file", "-e", "HEAD:b.txt"))     # not there after u1: it needs legs
        self.script(dict(self.GOOD, cost={"plan": 2, "execute": 2}))
        out, stop = self.go(day_budget=6)                        # no cost known yet: 3 + 3 a unit, which just fits
        self.assertEqual((stop["legs"], stop["units"], stop["day_usd"]), (2, {"u1": "PASS"}, 4.0))
        self.assertEqual(stop["reason"], "the day's budget has $2.00 left of $6.00, and a unit usually costs $4.00")
        self.assertFalse((self.board / "relay" / "done" / "u2.json").exists())

    def test_a_leg_may_spend_only_what_the_day_has_left(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 9}))
        out, stop = self.go(day_budget=6)
        self.assertEqual((stop["legs"], stop["units"]), (2, {}))
        self.assertEqual(stop["reason"], "leg 02 the day's budget is spent ($6.00, mid-leg)")
        self.assertEqual(stop["reason_kind"], "budget")
        self.assertEqual(self.legs()[1]["cost_usd"], 5)          # 6, less the plan's 1: Claude stopped it there
        self.assertEqual(stop["day_usd"], 6.0)

    def test_the_legs_own_cap_still_holds_when_the_day_has_more(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 4}))
        out, stop = self.go(leg_budget=2)
        self.assertEqual(self.legs()[0]["cost_usd"], 2)
        self.assertNotIn("the day's budget", stop["reason"])
        self.assertIn("leg 01", stop["reason"])
        self.assertEqual(stop["reason_kind"], "leg")

    def test_a_day_budget_of_zero_switches_it_off(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(400)
        out, stop = self.go(day_budget=0)
        self.assertIn("unit u1: PASS", out)
        self.assertIn("(no day budget)", out)

    def test_a_flag_outside_the_bounds_is_clamped_and_said(self):
        self.queue("u1")
        self.script(self.GOOD)
        out, stop = self.go(day_budget=9999)
        self.assertIn("note: day_budget_usd 9999 is outside its bounds; using 500", out)
        self.assertEqual(stop["day_budget_usd"], 500)

    def test_the_budget_command_and_status_show_today(self):
        self.spent(2.5)
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            self.assertEqual(relay.main(["budget"]), 0)
            self.assertEqual(relay.main(["status"]), 0)
        self.assertIn("$2.50 spent of $50.00, $47.50 left. 1 leg.", buf.getvalue())
        self.assertIn("earlier", buf.getvalue())
        self.assertIn("Today: $2.50 of $50.00 spent, $47.50 left.", buf.getvalue())


class Pace(Repo):
    """The day's cap in percent of the week, and the pace: a unit the pace does not cover yet waits for it."""
    legs, spent = Budget.legs, Budget.spent

    def setUp(self):
        super().setUp()
        # limits.json as shipped (11% a day, even over 24 hours), a full week taken as $1000: a dollar is 0.1 points
        config.limits = lambda *a, **k: dict(self.limits(*a, **k), week_usd=1000)
        self.keep = (runner.clock, runner.sleep, os.environ.pop(ledger.ALSO, None))
        self.slept = []
        runner.clock = lambda: self.now
        runner.sleep = self.sleep
        self.at(23)

    def tearDown(self):
        runner.clock, runner.sleep = self.keep[:2]
        os.environ.pop(ledger.ALSO, None) if self.keep[2] is None else os.environ.__setitem__(ledger.ALSO, self.keep[2])
        super().tearDown()

    def at(self, hour, minute=0):
        self.now = datetime.datetime.now().replace(hour=hour, minute=minute, second=0, microsecond=0)

    def sleep(self, seconds):
        """Waiting moves this test's clock and takes no time."""
        self.slept.append(seconds)
        self.now += datetime.timedelta(seconds=seconds)

    def test_a_day_over_its_percent_cap_starts_no_leg(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(111)                                         # 11.1 points
        out, stop = self.go()
        self.assertEqual((stop["reason"], stop["legs"], stop["units"]),
                         ("the day's budget is spent (11.10% of 11.00% of the week)", 0, {}))
        self.assertEqual((stop["day_pct"], stop["day_budget_pct"]), (11.1, 11))
        self.assertIn("0.0% left of a day's cap of 11.0%", out)

    def test_work_under_the_cap_passes_and_the_stop_record_holds_the_day_in_percent(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 2}))
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        self.assertNotIn("paced:", out)
        self.assertEqual((stop["day_usd"], stop["day_pct"], stop["day_budget_pct"]), (3.0, 0.3, 11))
        self.assertEqual(self.slept, [])

    def test_a_unit_the_pace_does_not_cover_waits_for_it_and_then_runs(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 2}))
        self.at(0, 30)                      # 0.23 points allowed; a unit usually costs $3 + $3: 0.6 points, at 01:19
        out, stop = self.go()
        self.assertIn("paced: a unit may start at 01:19; the day's budget is spent evenly over the day", out)
        self.assertEqual(out.count("paced:"), 1)
        self.assertLess(out.index("paced:"), out.index("leg 01"))
        self.assertIn("unit u1: PASS", out)
        self.assertEqual(stop["reason"], "nothing left to do")
        self.assertTrue(49 * 60 - 30 <= sum(self.slept) <= 49 * 60 + 30, sum(self.slept))
        self.assertTrue(all(s <= runner.PACE_STEP for s in self.slept))

    board_repo, b, far_file, log = Record.board_repo, Record.b, Record.far_file, Record.log

    def test_a_run_that_waits_for_the_pace_says_so_in_its_live_record_until_it_goes_on(self):
        self.queue("u1")
        self.board_repo()
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 2}))
        self.at(0, 30)
        name = lambda: next((self.board / "relay" / "desktop" / "live").glob("*.json")).name
        here = lambda: json.loads((self.board / "relay" / "desktop" / "live" / name()).read_text(encoding="utf-8"))
        far = lambda: self.far_file("relay/desktop/live/" + name())
        seen, run_leg = [], launch.run_leg

        def waiting(seconds):
            self.sleep(seconds)
            seen.append(("waits", here()["paced_until"], far()["paced_until"]))

        def working(d, *a):
            seen.append(("works", here()["paced_until"], far()["paced_until"], self.log(self.far)[-1].split(", ", 1)[1]))
            return run_leg(d, *a)
        runner.sleep, launch.run_leg = waiting, working
        try:
            out, stop = self.go(no_push=False)
        finally:
            launch.run_leg = run_leg
        self.assertIn("unit u1: PASS", out)
        self.assertEqual(set(seen[:-2]), {("waits", "01:19", "01:19")})    # origin knows why nothing moves,
        self.assertEqual(seen[-2], ("works", "", "", "the wait for the day's pace is over"))   # and when it does again
        self.assertEqual(self.log(self.far)[2].split(", ", 1)[1], "waiting for the day's pace until 01:19")

    def test_after_a_wait_the_queue_is_read_again_and_the_unit_that_is_first_now_runs(self):
        self.queue("u2")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 2}))
        self.at(0, 30)

        def moved(seconds):                                     # while the run waits, the owner says "do u1 first"
            self.sleep(seconds)
            if not (self.board / "relay" / "queue" / "u1.json").exists():
                self.queue("u1", raw=json.dumps({"id": "u1", "lane": "lane/show/x", "role": "lane", "priority": 1,
                                                 "goal": "Add a.txt", "done_when": ["git", "cat-file", "-e", "HEAD:a.txt"]}))
        runner.sleep = moved
        out, stop = self.go()
        self.assertIn("unit u2: the wait is over, the queue is read again", out)
        self.assertIn("leg 01  plan    u1", out)
        self.assertEqual(stop["units"].get("u1"), "PASS")

    def test_a_day_that_another_relay_spent_during_the_wait_starts_nothing(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.at(0, 30)

        def spent_elsewhere(seconds):                           # the wait ends at 01:19; by then the day is gone
            self.sleep(seconds)
            if not self.legs():
                self.spent(111)
        runner.sleep = spent_elsewhere
        out, stop = self.go()
        self.assertEqual((stop["reason"], stop["legs"]), ("the day's budget is spent (11.10% of 11.00% of the week)", 0))

    def test_a_wait_that_would_outlast_the_run_stops_it_and_says_the_time(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.at(0, 30)
        out, stop = self.go(hours=0.25)
        self.assertEqual((stop["reason"], stop["legs"]),
                         ("the day's pace lets a unit start at 01:19, after this run's 0.25 hours are up", 0))
        self.assertEqual(stop["reason_kind"], "pace")
        self.assertEqual(self.slept, [])
        self.assertFalse((self.board / "relay" / "done" / "u1.json").exists())

    def test_a_dry_run_does_not_wait_and_says_the_time(self):
        self.queue("u1")
        self.at(0, 30)
        out, stop = self.go(dry_run=True)
        self.assertIn("would run: nothing (the day's pace lets a unit start at 01:19)", out)
        self.assertEqual(self.slept, [])
        self.at(1, 19)
        self.assertIn("would run: lane u1", self.go(dry_run=True)[0])

    def test_hours_nobody_used_can_be_spent_later_the_same_day(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 20, "execute": 25}))     # 4.5 points at once, late in the day
        self.at(20)
        out, stop = self.go()
        self.assertIn("unit u1: PASS", out)
        self.assertEqual((self.slept, stop["day_pct"]), ([], 4.5))
        self.assertEqual(stop["reason_kind"], "done")

    def test_a_leg_may_spend_what_the_day_has_left_not_only_what_the_pace_allows(self):
        self.queue("u1")
        self.script(dict(self.GOOD, cost={"plan": 1, "execute": 9}))
        self.at(2)                          # 0.92 points allowed by 02:00: $9.17, and the plan takes $1 of it
        out, stop = self.go()
        self.assertEqual(self.legs()[1]["cost_usd"], 9)          # the pace decides when a leg starts, not its cap
        self.assertIn("unit u1: PASS", out)

    def test_the_owner_can_stop_a_run_that_waits_for_the_pace(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.at(0, 30)

        def asked(seconds):
            self.sleep(seconds)
            runner.P.write_json(runner.stop_path(legdir.home()), {"now": False})
        runner.sleep = asked
        out, stop = self.go()
        self.assertEqual((stop["reason"], stop["legs"], len(self.slept)), ("stopped by the owner (relay.py stop)", 0, 1))
        self.assertEqual(stop["reason_kind"], "asked")

    def test_the_owner_can_name_another_figure_for_the_day(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(6)                                           # 0.6 points
        out, stop = self.go(day_pct=0.5)
        self.assertEqual(stop["reason"], "the day's budget is spent (0.60% of 0.50% of the week)")
        self.assertEqual(stop["day_budget_pct"], 0.5)
        out, stop = self.go(day_pct=500, dry_run=True)
        self.assertIn("note: day_budget_pct 500 is outside its bounds; using 100", out)

    def test_a_day_given_in_dollars_is_counted_in_dollars(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(6)
        out, stop = self.go(day_budget=5)
        self.assertEqual(stop["reason"], "the day's budget is spent ($6.00 of $5.00)")
        self.assertNotIn("day_pct", stop)

    def test_the_day_that_only_just_covers_no_unit_says_so_in_percent(self):
        self.queue("u1")
        self.script(self.GOOD)
        self.spent(52, "plan", 1)
        self.spent(52, "execute", 2)                            # 10.4 points; a unit usually costs 52 + 52
        out, stop = self.go()
        self.assertEqual(stop["reason"], "the day's budget has 0.60% left of 11.00% of the week, and a unit usually "
                                         "costs about 10.40%")

    def test_a_run_books_the_agents_sessions_spawned_outside_the_relay_before_a_unit(self):
        self.queue("u1")
        self.script(self.GOOD)
        f = Path(os.environ["TW_AGENT_LOGS"]) / "p" / "s" / "subagents" / "agent-a1.jsonl"
        f.parent.mkdir(parents=True)
        f.write_text(json.dumps({                               # 6M output tokens of Opus 5.5: $120, 12 points
            "cwd": "C:/x/githubtest-pipe", "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
            "message": {"role": "assistant", "id": "m1", "model": "claude-opus-5-5",
                        "usage": {"output_tokens": 6000000}}}), encoding="utf-8")
        out, stop = self.go()
        self.assertEqual((stop["reason"], stop["legs"]), ("the day's budget is spent (12.00% of 11.00% of the week)", 0))
        rec = json.loads(next((self.board / "relay" / "desktop" / "agents").glob("*.json")).read_text(encoding="utf-8"))
        self.assertEqual((rec["usd"], rec["agents"], rec["by"][:4]), (120.0, 1, "run "))
        self.assertIn("used by the relay and 1 agent outside it", out)

    def test_the_legs_of_a_second_relay_count_toward_the_same_day(self):
        self.queue("u1")
        self.script(self.GOOD)
        mine, self.board = self.board, self.tmp / "board-2"
        self.spent(111)                                         # the other relay's board holds the day's spend
        self.board = mine
        self.assertIn("would run: lane u1", self.go(dry_run=True)[0])
        os.environ[ledger.ALSO] = str(self.tmp / "board-2")
        out, stop = self.go()
        self.assertEqual((stop["reason"], stop["legs"]), ("the day's budget is spent (11.10% of 11.00% of the week)", 0))


class CloseOut(Repo):
    """The leg's own commands: the gate as a detached job, then commit and push by script."""
    def setUp(self):
        super().setUp()
        self.g(["switch", "-q", "-c", "lane/show/x"])
        self.d = legdir.new_leg("r1", 1, dict(UNIT, role="lane"), "execute", self.ph["execute"], self.lim, self.work,
                                "lane/show/x", self.board)
        os.environ["TW_RUNS"] = str(legdir.desk(self.d) / "jobs")
        self.gate(0)

    def gate(self, code):
        os.environ["TW_RELAY_GATE"] = json.dumps([sys.executable, "-c", "import sys; sys.exit(%d)" % code])

    def leg_cmd(self, *args):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = relay.main(["leg"] + list(args))
        return code, buf.getvalue()

    def test_a_green_gate_for_these_exact_files_lets_finish_commit_and_push(self):
        (self.work / "a.txt").write_text("one\n", encoding="utf-8")
        self.assertEqual(self.leg_cmd("gate", "status")[0], 4)                  # NONE
        self.leg_cmd("gate", "start")
        code, out = self.leg_cmd("gate", "wait", "--max", "60")
        self.assertEqual((code, out[:5]), (0, "GREEN"))
        self.assertEqual(self.leg_cmd("done")[0], 1)                            # not committed yet
        code, out = self.leg_cmd("finish", "-m", "add a.txt")
        self.assertEqual(code, 0, out)
        self.assertEqual(gitio.dirty(self.work), [])
        self.assertTrue(gitio.pushed(self.work, "lane/show/x"))
        self.assertIn("add a.txt", gitio.git(["log", "-1", "--format=%s"], self.work))
        self.assertEqual(self.leg_cmd("done")[0], 0)

    def test_files_changed_after_the_gate_or_a_red_gate_are_saved_not_committed(self):
        (self.work / "a.txt").write_text("one\n", encoding="utf-8")
        self.leg_cmd("gate", "start")
        self.leg_cmd("gate", "wait", "--max", "60")
        (self.work / "a.txt").write_text("two\n", encoding="utf-8")            # edited after the gate
        self.assertEqual(self.leg_cmd("gate", "status")[1][:5], "STALE")
        head = gitio.head(self.work)
        code, out = self.leg_cmd("finish")
        self.assertEqual((code, gitio.head(self.work)), (1, head))
        self.assertIn("NOT COMMITTED", out)
        self.assertIn(b"a.txt", (self.d / "red.patch").read_bytes())
        self.gate(8)
        self.leg_cmd("gate", "start")
        code, out = self.leg_cmd("gate", "wait", "--max", "60")
        self.assertEqual((code, out[:3]), (1, "RED"))
        self.assertEqual(self.leg_cmd("finish")[0], 1)
        self.assertEqual(gitio.head(self.work), head)

    def test_in_a_plan_leg_done_checks_the_plan_as_the_runner_will(self):
        self.d = legdir.new_leg("r1", 2, dict(UNIT, role="lane"), "plan", self.ph["plan"], self.lim, self.work,
                                "lane/show/x", self.board)
        desk = legdir.desk(self.d)
        os.environ["TW_RUNS"] = str(desk / "jobs")
        code, out = self.leg_cmd("done")
        self.assertEqual(code, 1)
        self.assertIn("plan.md is not in your leg folder", out)
        (desk / "plan.md").write_text(PLAN.replace("`a.txt` (new)", "`nowhere/b.txt`"), encoding="utf-8")
        code, out = self.leg_cmd("done")
        self.assertEqual(code, 1)
        self.assertIn("nowhere/b.txt", out)
        (desk / "plan.md").write_text(PLAN, encoding="utf-8")
        self.assertEqual(self.leg_cmd("done")[0], 0)

    def test_in_a_critic_leg_done_checks_the_paper(self):
        self.d = legdir.new_leg("r1", 3, dict(UNIT, role="lane"), "critic", self.ph["critic"], self.lim,
                                self.tmp / "bundle", "lane/show/x", "")
        desk = legdir.desk(self.d)
        os.environ["TW_RUNS"] = str(desk / "jobs")
        self.assertEqual(self.leg_cmd("done")[0], 1)
        (desk / "critic.md").write_text("It is fine.\n", encoding="utf-8")
        code, out = self.leg_cmd("done")
        self.assertEqual(code, 1)
        self.assertIn("VERDICT", out)
        (desk / "critic.md").write_text(CRITIC % 80, encoding="utf-8")
        self.assertEqual(self.leg_cmd("done")[0], 0)

    def test_outside_a_leg_the_leg_commands_refuse(self):
        os.environ.pop("TW_RUNS")
        with self.assertRaises(SystemExit):
            self.leg_cmd("done")

    def unity(self, total, passed, failed, sleep=0):
        """A stand-in for the Unity CLI's test run: it writes the NUnit report the real one would, where it is told
        to. total -1: it writes nothing and exits 1, as on a compile error."""
        stub = self.tmp / "unity_stub.py"
        stub.write_text(
            "import sys, time\n"
            "total, passed, failed, sleep = map(int, sys.argv[1:5])\n"
            "name, out = sys.argv[5], sys.argv[6]\n"
            "time.sleep(sleep)\n"
            "if total < 0:\n"
            "    sys.exit(1)\n"
            "fail = '<failure><message><![CDATA[[I1] F9 left no HUD at all\\n  Expected: 1\\n  But was:  0]]></message></failure>'\n"
            "cases = ''.join('<test-case fullname=\"%s.T%d\" result=\"%s\">%s</test-case>'\n"
            "                % (name, i, 'Failed' if i < failed else 'Passed', fail if i < failed else '') for i in range(total))\n"
            "open(out, 'w', encoding='utf-8').write('<?xml version=\"1.0\" encoding=\"utf-8\"?><test-run id=\"2\" '\n"
            "    'total=\"%d\" passed=\"%d\" failed=\"%d\" result=\"%s\"><test-suite>%s</test-suite></test-run>'\n"
            "    % (total, passed, failed, 'Failed(Child)' if failed else 'Passed', cases))\n"
            "sys.exit(2 if failed else 0)\n", encoding="utf-8")
        os.environ["TW_RELAY_PLAY"] = json.dumps([sys.executable, str(stub)] + [str(n) for n in (total, passed, failed, sleep)])

    def test_play_waits_for_the_run_and_reads_the_verdict_from_the_report(self):
        desk = legdir.desk(self.d)
        self.unity(2, 2, 0)
        code, out = self.leg_cmd("play", "TW.Tests.HudTogglePlayTests", "--max", "60")
        self.assertEqual((code, out[:41]), (0, "GREEN: total 2, passed 2, failed 0 (TW.Te"), out)
        self.assertTrue((desk / "playmode-1.xml").exists())                     # the report is the leg's paper,
        self.assertEqual(gitio.dirty(self.work), [])                            # never a file in the checkout
        self.unity(1, 0, 1)                                                     # rv-12's own run: one test, failed
        code, out = self.leg_cmd("play", "TW.Tests.HudTogglePlayTests", "--max", "60")
        self.assertEqual((code, out[:39]), (1, "RED: total 1, passed 0, failed 1 (TW.Te"), out)
        self.assertIn("FAILED TW.Tests.HudTogglePlayTests.T0: [I1] F9 left no HUD at all | Expected: 1", out)
        self.assertTrue((desk / "playmode-2.xml").exists())                     # a second run, a second report
        for total, passed in ((-1, 0), (0, 0), (2, 0)):                         # no report, no test, nothing passed
            self.unity(total, passed, 0)
            code, out = self.leg_cmd("play", "TW.Tests.Nothing", "--max", "60")
            self.assertEqual((code, out[:3]), (1, "RED"), out)
        self.assertIn("nothing", out.lower())

    def test_play_called_again_waits_on_the_same_run_and_the_gate_does_not_start_beside_it(self):
        self.unity(1, 1, 0, sleep=6)
        code, out = self.leg_cmd("play", "TW.Tests.Slow", "--max", "1")
        self.assertEqual((code, out[:7]), (2, "RUNNING"), out)
        with self.assertRaises(SystemExit):                                     # one batch run per project at a time
            self.leg_cmd("gate", "start")
        code, out = self.leg_cmd("play", "TW.Tests.Slow", "--max", "60")
        self.assertEqual((code, out[:5]), (0, "GREEN"), out)
        self.assertEqual(sorted(p.name for p in (legdir.desk(self.d) / "jobs").glob("play-*")), ["play-1"])

    def test_a_leg_that_only_reads_runs_no_playmode_test(self):
        self.d = legdir.new_leg("r1", 2, dict(UNIT, role="lane"), "plan", self.ph["plan"], self.lim, self.work,
                                "lane/show/x", self.board)
        os.environ["TW_RUNS"] = str(legdir.desk(self.d) / "jobs")
        self.unity(1, 1, 0)
        with self.assertRaises(SystemExit):
            self.leg_cmd("play", "TW.Tests.X")


class PipelineSource(Repo):
    """The board source end to end, on a throwaway board with one item."""
    def setUp(self):
        super().setUp()
        self.p_repo = SP.P.REPO
        SP.P.REPO = self.work
        (self.work / "in.txt").write_text("input\n", encoding="utf-8")
        self.g(["add", "-A"]); self.g(["commit", "-q", "-m", "input"]); self.g(["push", "-q", "origin", gitio.INTEGRATION])
        (self.board / "items" / "thing.json").write_text(json.dumps({
            "id": "thing", "title": "A thing", "lane": "lane/show/pipe-thing", "stages": [
                {"id": "shots", "station": "desktop", "role": "env-simulator", "inputs": ["in.txt"], "bands": ["near", "far"]},
                {"id": "land", "station": "desktop", "role": "master", "after": ["shots"], "inputs": ["in.txt"]}]}), encoding="utf-8")

    def tearDown(self):
        SP.P.REPO = self.p_repo
        super().tearDown()

    def evidence(self, data, sidecar='{"pose_error_m": 0}', frames="near.jpg: rear, zoom 17\nfar.jpg: rear, zoom 30\n"):
        folder = self.board / "evidence" / "thing" / "shots"
        folder.mkdir(parents=True, exist_ok=True)
        for band in ("near", "far"):
            (folder / (band + ".jpg")).write_bytes(data)
            (folder / (band + ".json")).unlink(missing_ok=True)
            if sidecar is not None:
                (folder / (band + ".json")).write_text(sidecar, encoding="utf-8")
        (folder / "frames.txt").unlink(missing_ok=True)
        if frames is not None:
            (folder / "frames.txt").write_text(frames, encoding="utf-8")

    def results(self):
        return [json.loads(p.read_text(encoding="utf-8")) for p in sorted((self.board / "results").glob("*.json"))]

    PUSH = [{"file": "out.txt", "text": "o\n"}, {"git": ["add", "-A"]}, {"git": ["commit", "-q", "-m", "o"]},
            {"git": ["push", "-q", "origin", "lane/show/pipe-thing"]}]

    def go_with_evidence(self, **kw):
        """A run in which the pictures appear after every leg, as if the leg made them."""
        real = runner.Run.unit

        def with_evidence(run, unit):
            run_leg = launch.run_leg

            def after(d, lim, t, *more):
                leg = run_leg(d, lim, t, *more)
                self.evidence(jpeg(640, 360))
                return leg
            launch.run_leg = after
            try:
                return real(run, unit)
            finally:
                launch.run_leg = run_leg
        runner.Run.unit = with_evidence
        try:
            return self.go(sources=["pipeline"], **kw)
        finally:
            runner.Run.unit = real

    def leg_file(self, nn, name, desk=False):
        return next((self.tmp / "home" / "runs").glob("*/%s/%02d" % ("desk" if desk else "legs", nn))) / name

    FIX = [{"file": "out2.txt", "text": "fixed\n"}, {"git": ["add", "-A"]}, {"git": ["commit", "-q", "-m", "fixes"]},
           {"git": ["push", "-q", "origin", "lane/show/pipe-thing"]}]

    def test_a_blind_critic_scores_the_evidence_and_a_low_score_gets_one_fix_round(self):
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute#2": self.PUSH,
                     "critic#3": [{"write": "critic.md", "text": CRITIC % 60}], "execute#4": self.FIX,
                     "critic#5": [{"write": "critic.md", "text": (CRITIC % 90).replace("ROUND 1", "ROUND 2")}]})
        out, stop = self.go_with_evidence()
        res = self.results()
        self.assertEqual([(r["stage"], r["verdict"]) for r in res], [("shots", "PASS")])
        self.assertIn("critic 60/100, then 90/100 (target 85)", res[0]["note"])
        self.assertEqual(stop["legs"], 5)
        shots = self.board / "evidence" / "thing" / "shots"
        self.assertIn("60/100", (shots / "critic-r1.md").read_text(encoding="utf-8"))
        self.assertIn("90/100", (shots / "critic-r2.md").read_text(encoding="utf-8"))
        rows = (self.board / "relay" / "desktop" / "lessons.md").read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(rows), 4)                                   # the head, the rule, one row per round
        self.assertIn("| 1 | 60 | 85 | Fix the first thing", rows[2])
        # blind: its own folder with the pictures and the stage, no board, no word from the producer
        leg3 = json.loads(self.leg_file(3, "leg.json").read_text(encoding="utf-8"))
        self.assertEqual((leg3["phase"], leg3["board"], Path(leg3["worktree"]).name), ("critic", "", "bundle"))
        self.assertEqual(sorted(f.name for f in Path(leg3["worktree"]).iterdir()),
                         ["far.jpg", "far.json", "frames.txt", "near.jpg", "near.json", "stage.json"])
        bundle5 = Path(json.loads(self.leg_file(5, "leg.json").read_text(encoding="utf-8"))["worktree"])
        self.assertFalse((bundle5 / "critic-r1.md").exists())           # round 2 does not read round 1
        card = self.leg_file(3, "card.md").read_text(encoding="utf-8")
        self.assertIn("Critic round 1", card)
        self.assertIn("Hard critic", card)                               # the rubric rides on the card
        self.assertNotIn("brief/tw-env-sim", card)                       # a critic is not handed the producer's brief
        self.assertFalse(self.leg_file(3, "brief", desk=True).exists())
        self.assertNotIn(str(self.board).replace("\\", "/"), card.replace("\\", "/"))
        for nn in (1, 2):
            self.assertTrue(self.leg_file(nn, "brief/tw-env-sim/SKILL.md", desk=True).is_file())
            self.assertTrue(self.leg_file(nn, "brief/pipeline/references/driving-and-evidence.md", desk=True).is_file())
            self.assertIn("brief/tw-env-sim/SKILL.md", self.leg_file(nn, "card.md").read_text(encoding="utf-8"))
            self.assertNotIn("UNITY_CLI_ALLOW_LOCKED", self.leg_file(nn, "card.md").read_text(encoding="utf-8"))
        fix = self.leg_file(4, "card.md").read_text(encoding="utf-8")
        self.assertIn("fix round", fix)
        self.assertIn("1. Fix the first thing", fix)

    def test_a_critic_without_a_score_or_without_room_leaves_the_pass_and_says_so(self):
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.PUSH,
                     "critic": [{"write": "critic.md", "text": "It looks fine to me.\n"}]})
        out, stop = self.go_with_evidence()
        res = self.results()
        self.assertEqual((res[0]["verdict"], stop["legs"]), ("PASS", 3))
        self.assertIn("critic round 1 gave no score", res[0]["note"])
        self.assertFalse((self.board / "evidence" / "thing" / "shots" / "critic-r1.md").exists())

    def test_the_leg_cap_skips_the_critic_and_keeps_the_verdict(self):
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.PUSH})
        out, stop = self.go_with_evidence(max_legs=2)
        res = self.results()
        self.assertEqual((res[0]["verdict"], stop["legs"]), ("PASS", 2))
        self.assertIn("no critic round 1: the leg cap (2) is reached", res[0]["note"])

    def test_a_job_is_claimed_checked_and_completed_by_the_runner_and_master_is_left_alone(self):
        self.script({"plan": [{"write": "plan.md", "text": PLAN}], "execute": self.PUSH,
                     "critic": [{"write": "critic.md", "text": CRITIC % 88}]})
        out, stop = self.go_with_evidence()
        self.assertIn("PASS", out)
        res = self.results()
        self.assertEqual([(r["stage"], r["verdict"]) for r in res], [("shots", "PASS")])
        self.assertEqual(sorted(res[0]["evidence"]), ["far", "near"])
        self.assertEqual(stop["reason"], "nothing left to do")               # the master stage is not taken

    def test_evidence_that_is_empty_not_a_picture_or_old_fails_the_job(self):
        self.evidence(b"")
        unit = SP.next({"board": self.board, "station": "desktop", "skip": set()})
        self.g(["switch", "-q", "-c", "lane/show/pipe-thing"]); self.g(["push", "-q", "origin", "lane/show/pipe-thing"])
        ctx = {"board": self.board, "work": self.work, "since": 0}
        self.assertEqual(len(SP.verify(unit, ctx)), 2)
        self.evidence(b"just text")
        self.assertTrue(all("not a JPEG" in p for p in SP.verify(unit, ctx)))
        self.evidence(jpeg(8, 8))
        self.assertTrue(all("too small" in p for p in SP.verify(unit, ctx)))
        self.evidence(jpeg(640, 360))
        self.assertEqual(SP.verify(unit, ctx), [])
        self.assertTrue(all("older" in p for p in SP.verify(unit, dict(ctx, since=time.time() + 60))))
        # a still is scored by its sidecar and frames.txt: a job without them fails here, not a critic round later
        self.evidence(jpeg(640, 360), sidecar=None)
        self.assertTrue(all("has no sidecar" in p for p in SP.verify(unit, ctx)) and len(SP.verify(unit, ctx)) == 2)
        self.evidence(jpeg(640, 360), sidecar="not json")
        self.assertTrue(all("not a JSON object" in p for p in SP.verify(unit, ctx)) and len(SP.verify(unit, ctx)) == 2)
        self.evidence(jpeg(640, 360), frames=None)
        self.assertTrue(all("frames.txt" in p for p in SP.verify(unit, ctx)) and len(SP.verify(unit, ctx)) == 2)
        self.evidence(jpeg(640, 360), frames="near.jpg: rear\n")
        self.assertEqual(SP.verify(unit, ctx), ["frames.txt has no line that starts far.jpg:"])
        self.evidence(jpeg(640, 360), sidecar=None,
                      frames="near.jpg: a mock-up, no sidecar\nfar.jpg: No sidecar, a concept sheet\n")
        self.assertEqual(SP.verify(unit, ctx), [])                      # a still the game did not render

    def test_the_job_card_asks_for_the_sidecars_the_critic_scores_by_and_they_reach_its_folder(self):
        ctx = {"board": self.board, "work": self.work, "station": "desktop", "skip": set()}
        unit = SP.next(ctx)
        card = SP.body(unit)
        for name in ("<band>.jpg", "<band>.json", "frames.txt", "`<band>.jpg:`", "`no sidecar`"):
            self.assertIn(name, card)
        self.evidence(jpeg(640, 360))
        (self.board / "evidence" / "thing" / "shots" / "critic-r1.md").write_text("VERDICT", encoding="utf-8")
        bundle = self.tmp / "bundle-check"
        SP.critic(unit, ctx, 2)["fill"](bundle)
        self.assertEqual(sorted(f.name for f in bundle.iterdir()),
                         ["far.jpg", "far.json", "frames.txt", "near.jpg", "near.json", "stage.json"])

    def test_the_job_card_names_the_skill_for_the_role_and_every_such_skill_exists(self):
        ctx = {"board": self.board, "work": self.work, "station": "desktop", "skip": set()}
        self.assertIn("Read `brief/tw-env-sim/SKILL.md` in your leg folder", SP.body(SP.next(ctx)))
        skills = HERE.parents[2] / ".claude" / "skills"
        for role, skill in SP.ROLE_SKILLS.items():
            self.assertTrue((skills / skill / "SKILL.md").exists(), "%s -> %s" % (role, skill))

    def test_the_role_table_and_the_pipeline_skills_table_name_the_same_roles(self):
        """Tools/pipeline/roles.json is what the scripts read, the table in the pipeline skill is what a session
        reads: a role in one and not in the other is a leg without a brief, or a brief nobody can name."""
        text = (HERE.parents[2] / ".claude" / "skills" / "pipeline" / "SKILL.md").read_text(encoding="utf-8")
        table = text.split("## Role skills", 1)[1].split("\n## ", 1)[0]
        said = {}
        for line in table.splitlines():
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if line.startswith("|") and len(cells) >= 2 and cells[0] != "Role" and not set(cells[0]) <= set("-"):
                for name in cells[0].split(","):
                    if not name.strip().startswith("`/"):                        # a slash command, not a role
                        said[name.strip().strip("`")] = cells[1]
        roles = SP.P.roles()
        self.assertEqual(sorted(said), sorted(roles))
        for role, skill in roles.items():
            if skill:
                self.assertIn("`%s`" % skill, said[role], role)
        self.assertEqual(SP.ROLE_SKILLS, {r: s for r, s in roles.items() if s})
        self.assertEqual((SP.ROLE_SKILLS["character"], SP.ROLE_SKILLS["critic"]), ("tw-character-sim", "tw-critic"))

    def test_inside_a_leg_the_pipeline_tool_refuses_to_claim_or_complete(self):
        env = dict(os.environ, TW_RELAY="1")
        for args in (["claim", "x"], ["complete", "x", "--verdict", "PASS"], ["release"]):
            r = subprocess.run([sys.executable, str(HERE.parent / "pipeline" / "pipeline.py")] + args, env=env,
                               capture_output=True)
            self.assertIn(b"relay leg", r.stderr, args)
            self.assertNotEqual(r.returncode, 0)
        gitio.take_lock(self.work, self.tmp / "home", "relay r1 u1", "lane/show/x")       # the marker alone is enough
        rec = json.loads(gitio.marker_path(self.work).read_text(encoding="utf-8"))
        gitio.marker_path(self.work).write_text(json.dumps(dict(rec, pid=4)), encoding="utf-8")
        r = subprocess.run([sys.executable, str(HERE.parent / "pipeline" / "pipeline.py"), "release"], cwd=str(self.work),
                           capture_output=True)
        self.assertIn(b"relay leg", r.stderr)

    def test_a_job_that_ended_blocked_is_not_taken_again_until_someone_answers(self):
        """The Ridge's battlefield job, 2026-10-08: BLOCKED on a question for the owner, and planned again by the
        next run for nothing."""
        self.script({"plan": [{"write": "plan.md", "text": PLAN}],
                     "execute": [{"report": "RESULT: blocked - this needs a SIM lane, the owner must open one."}]})
        out, stop = self.go(sources=["pipeline"])
        self.assertEqual([(r["verdict"], r["attempt"]) for r in self.results()], [("BLOCKED", 1)])
        self.g(["switch", "-q", gitio.INTEGRATION])
        out, stop = self.go(sources=["pipeline"])                               # a new run, nothing has changed
        self.assertEqual((stop["reason"], stop["legs"]), ("nothing left to do", 0))
        self.assertEqual(len(self.results()), 1)
        SP.P.cmd_feedback(SP.P.Board(self.board), argparse.Namespace(                # he answers: a new job
            item="thing", stage="shots", words="A sim lane is open: build it there.", check="RidgeTests green"))
        self.g(["switch", "-q", gitio.INTEGRATION])
        out, stop = self.go(sources=["pipeline"])
        self.assertEqual(stop["legs"], 2)
        self.assertEqual(len(self.results()), 2)

    def test_a_run_that_stops_mid_job_gives_the_claim_back_and_writes_no_result(self):
        self.script({"plan": [{"exit": 2}]})
        _, stop = self.go(sources=["pipeline"])
        self.assertIn("exited 2", stop["reason"])
        self.assertEqual(self.results(), [])
        self.assertFalse((self.board / "claims" / "desktop.json").exists())


if __name__ == "__main__":
    unittest.main(verbosity=1)
