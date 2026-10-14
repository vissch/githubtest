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

HERE = os.path.dirname(os.path.abspath(__file__))

# Prints the shapes flameshots' grep/sed lines read back, so its early checks pass and the run reaches one().
FAKE_TW = r'''#!/usr/bin/env bash
printf '%s\n' "$*" | tr '\n' ' ' >> "$TW_LOG"; printf '\n' >> "$TW_LOG"
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


def run_script(tmp):
    """Copy flameshots onto a stand-in tree, run it, return the list of logged `tw` argument lines."""
    tools = os.path.join(tmp, "Tools")
    os.makedirs(os.path.join(tools, "flame-shots"))
    os.makedirs(os.path.join(tmp, "bin"))
    shutil.copyfile(os.path.join(HERE, "flameshots"), os.path.join(tools, "flameshots"))
    with open(os.path.join(tools, "tw"), "w", newline="\n") as f:
        f.write(FAKE_TW)
    with open(os.path.join(tools, "flamecheck.py"), "w", newline="\n") as f:
        f.write("import sys\nsys.exit(0)\n")
    with open(os.path.join(tmp, "bin", "sleep"), "w", newline="\n") as f:  # the script's sleep 8 / sleep 30
        f.write("#!/usr/bin/env bash\nexit 0\n")
    for p in (os.path.join(tools, "tw"), os.path.join(tmp, "bin", "sleep")):
        os.chmod(p, 0o755)
    log = os.path.join(tmp, "tw.log")
    env = dict(os.environ, TW_LOG=log, PATH=os.path.join(tmp, "bin") + os.pathsep + os.environ["PATH"])
    subprocess.run(["bash", os.path.join(tools, "flameshots"), "test"],
                   cwd=tmp, env=env, capture_output=True, text=True)
    with open(log, encoding="utf-8", errors="replace") as f:
        return [ln.strip() for ln in f if ln.strip()]


class FlameshotsTest(unittest.TestCase):
    def test_r2_each_shot_pins_the_clock_before_creating_the_effect(self):
        """[R2] one() must call `tw stepabs $4` before the eval that creates the effect."""
        with tempfile.TemporaryDirectory() as tmp:
            calls = run_script(tmp)
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
