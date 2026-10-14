#!/usr/bin/env python3
"""Tests for Tools/tw, run against a stand-in `unity` CLI that only answers the evals tw reads back.

No editor and no Unity. Run: python Tools/test_tw.py
"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))

# Answers the one eval `tw stepabs` reads back: the clock, already PAST the time the shot wants.
FAKE_UNITY = r'''#!/usr/bin/env bash
for a in "$@"; do case "$a" in *Time.time*) echo '{ "result": 40.0 }'; exit 0 ;; esac; done
echo '{ "result": 0 }'
exit 0
'''


def run_tw(tmp, *args):
    """Run Tools/tw with a fake `unity` first on PATH and no CLI under LOCALAPPDATA."""
    bindir = os.path.join(tmp, "bin")
    os.makedirs(bindir)
    with open(os.path.join(bindir, "unity"), "w", newline="\n") as f:
        f.write(FAKE_UNITY)
    os.chmod(os.path.join(bindir, "unity"), 0o755)
    empty = os.path.join(tmp, "localappdata")   # no unity/bin here, so the PATH `unity` is used
    os.makedirs(empty)
    env = dict(os.environ, LOCALAPPDATA=empty,
               PATH=bindir + os.pathsep + os.environ["PATH"])
    return subprocess.run(["bash", os.path.join(HERE, "tw")] + list(args),
                          env=env, capture_output=True, text=True)


class StepabsTest(unittest.TestCase):
    def test_t32_a_clock_already_past_the_wanted_time_fails(self):
        """[T3.2] stepabs returned 0 without pinning when the clock had run past $want."""
        with tempfile.TemporaryDirectory() as tmp:
            r = run_tw(tmp, "stepabs", "34")
        self.assertNotEqual(r.returncode, 0,
                            "[T3.2] the birth cannot be pinned at 34s with the clock at 40s, "
                            "but stepabs exited 0 (out=%r err=%r)" % (r.stdout, r.stderr))
        self.assertIn("already", r.stderr,
                      "[T3.2] stepabs said nothing about the clock being past the mark (%r)" % r.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
