#!/usr/bin/env python3
"""Tests for Tools/flameshots, run against a stand-in `tw` that only records its calls.

No editor and no Unity: the script's calls are logged by a fake `tw` on a temp tree, and the test reads the log.
Run: python Tools/test_flameshots.py
"""
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = os.path.dirname(os.path.abspath(__file__))


def git_bash():
    """Git's own bash. A bare "bash" on Windows can be WSL's launcher (WindowsApps), first on PATH:
    it cannot open a C: path, so the stand-in never runs and the temp tree stays locked (WinError 32
    when the folder is deleted). The first git.exe on PATH may ship with no bash beside it, so every
    git on PATH is tried, the same places selftest and test_aosa look."""
    gits = []
    for folder in os.environ.get("PATH", "").split(os.pathsep):
        for name in ("git.exe", "git"):
            cand = os.path.join(folder, name)
            if os.path.isfile(cand):
                gits.append(cand)
                break
    for git in gits:
        g = Path(git).resolve()
        for cand in (g.parent / "bash.exe", g.parent.parent / "bin" / "bash.exe",
                     g.parent.parent / "usr" / "bin" / "bash.exe"):
            if cand.is_file():
                return str(cand)
    return shutil.which("bash") or "bash"

# Prints the shapes flameshots' grep/sed lines read back, so its early checks pass and the run reaches one().
FAKE_TW = r'''#!/usr/bin/env bash
printf '%s\n' "$*" | tr '\n' ' ' >> "$TW_LOG"; printf '\n' >> "$TW_LOG"
for f in ${TW_FAIL:-}; do [ "$1" = "$f" ] && exit 1; done   # TW_FAIL: subcommands this stand-in fails on
case "$1" in
  run)
    [ "$2" = console_status ] && echo '{ "compilationFailed": false, "consoleErrors": 0 }'
    ;;
  eval)
    code="$2"
    case "$code" in
      *bp.x.ToString*) echo '{ "result": "100.00 1.000 104.00 1.00000 0.00000" }' ;;
      *DeltaAngle*)    echo '{ "result": "0.00" }' ;;
      *GetActiveScene*) echo '{ "result": "GreyboxCorridor/True" }' ;;
      *) echo '{ "result": "0" }' ;;
    esac
    ;;
esac
exit 0
'''


def run_script(tmp, fail=()):
    """Copy flameshots onto a stand-in tree, run it, return (logged `tw` argument lines, exit code).

    `fail`: `tw` subcommands the stand-in exits 1 on, so the script meets a failing step.
    """
    tools = os.path.join(tmp, "Tools")
    os.makedirs(os.path.join(tools, "flame-shots"))
    shutil.copyfile(os.path.join(HERE, "flameshots"), os.path.join(tools, "flameshots"))
    with open(os.path.join(tools, "tw"), "w", newline="\n") as f:
        f.write(FAKE_TW)
    with open(os.path.join(tools, "flamecheck.py"), "w", newline="\n") as f:
        f.write("import sys\nsys.exit(0)\n")
    bash = git_bash()
    tw = os.path.join(tools, "tw")
    # NTFS carries no real exec bit; bash itself has to be asked to set the one it checks.
    subprocess.run([bash, "-c", 'chmod +x "%s"' % tw.replace("\\", "/")], cwd=tmp)
    log = os.path.join(tmp, "tw.log")
    script = os.path.join(tools, "flameshots").replace("\\", "/")
    env = dict(os.environ, TW_LOG=log, TW_FAIL=" ".join(fail))
    # Git bash prepends /usr/bin on every new process, so a stand-in `sleep` on PATH never wins and the
    # script's sleep 8 / sleep 30 really wait. Source it here, where a function shadows sleep. $1 stays
    # the prefix the test passes (`test`).
    r = subprocess.run(
        [bash, "-c", 'sleep() { :; }; script=$1; set -- test; source "$script"', "bash", script],
        cwd=tmp, env=env, capture_output=True, text=True)
    with open(log, encoding="utf-8", errors="replace") as f:
        return [ln.strip() for ln in f if ln.strip()], r.returncode


class FlameshotsTest(unittest.TestCase):
    def test_r2_each_shot_pins_the_clock_before_creating_the_effect(self):
        """[R2] one() must call `tw stepabs $4` before the eval that creates the effect."""
        with tempfile.TemporaryDirectory() as tmp:
            calls, _ = run_script(tmp)
        # (absolute birth time from the call sites at the bottom of the script, marker in the created effect)
        for want_abs, marker in (("34", "fx.Burst"), ("44", "fx.TankCookOff"), ("54", "fx.Alight")):
            birth = next((i for i, c in enumerate(calls) if re.match(r"^stepabs %s(\b|$)" % want_abs, c)), None)
            self.assertIsNotNone(
                birth, "[R2] no `stepabs %s` call at all: the birth time is never used (calls: %r)"
                % (want_abs, calls))
            made = next((i for i, c in enumerate(calls) if c.startswith("eval ") and marker in c), None)
            self.assertIsNotNone(made, "[R2] the shot creating %s never ran" % marker)
            self.assertLess(birth, made,
                            "[R2] the clock is not pinned to %s before %s is created" % (want_abs, marker))

    def test_t32_a_failed_stepabs_cancels_the_shot(self):
        """[T3.2] one() discarded stepabs' exit code and shot an unpinned clock anyway."""
        with tempfile.TemporaryDirectory() as tmp:
            calls, rc = run_script(tmp, fail=["stepabs"])
        self.assertFalse([c for c in calls if c.startswith("shot test_jet")],
                         "[T3.2] the jet shot was taken although the clock was never pinned (calls: %r)" % calls)
        self.assertNotEqual(rc, 0, "[T3.2] flameshots exited 0 with a clock that could not be pinned")


class FlamecheckTest(unittest.TestCase):
    """[R13] flamecheck must say it cannot run instead of letting flameshots report "no fire"."""

    def test_r13_a_missing_scipy_exits_2_with_a_message(self):
        """[R13] flamecheck.py ran without scipy reported a clean "EMPTY" and exit 1, which reads as no fire."""
        with tempfile.TemporaryDirectory() as tmp:
            # a scipy package earlier on the path than the real one, whose import fails
            os.makedirs(os.path.join(tmp, "scipy"))
            with open(os.path.join(tmp, "scipy", "__init__.py"), "w", newline="\n") as f:
                f.write("raise ImportError('no scipy here', name='scipy')\n")
            env = dict(os.environ, PYTHONPATH=tmp)
            r = subprocess.run([sys.executable, os.path.join(HERE, "flamecheck.py"), "nosuch.png"],
                               env=env, capture_output=True, text=True)
        self.assertEqual(r.returncode, 2,
                         "[R13] flamecheck without scipy exited %d, not 2 (out=%r err=%r)"
                         % (r.returncode, r.stdout, r.stderr))
        self.assertIn("scipy", r.stderr, "[R13] flamecheck said nothing about scipy on stderr (%r)" % r.stderr)
        self.assertNotIn("EMPTY", r.stdout,
                         "[R13] flamecheck still reports a verdict on a capture it could not read (%r)" % r.stdout)

    def test_r13_flameshots_exits_non_zero_on_a_bad_shot(self):
        """[R13] flameshots swallowed every flamecheck failure and always exited 0."""
        src = open(os.path.join(HERE, "flameshots"), encoding="utf-8").read()
        check = src.split("check() {", 1)[1].split("\n}", 1)[0]
        self.assertNotIn("2>/dev/null", check,
                         "[R13] check() still sends flamecheck's message to /dev/null: a missing scipy is silent")
        self.assertIn("BAD=1", check, "[R13] check() does not record a failed shot")
        self.assertIn("cannot run", check,
                      "[R13] check() does not tell a flamecheck that could not run from a capture with no fire")
        self.assertIn("exit $BAD", src, "[R13] flameshots never exits with its failure flag")


if __name__ == "__main__":
    unittest.main(verbosity=2)
