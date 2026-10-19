#!/usr/bin/env python3
"""Tests of Tools/toolcheck.py: a running part shows progress in the log, so a gate's quiet log means a hang.

    python Tools/test_toolcheck.py
Stdlib only. ASCII only.
"""
import contextlib, io, os, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import toolcheck   # noqa: E402


class Progress(unittest.TestCase):
    def run_script(self, code, beat):
        with tempfile.TemporaryDirectory() as tmp:
            (Path(tmp) / "part.py").write_text(code, encoding="utf-8")
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                got = toolcheck.run_part("part.py", Path(tmp), dict(os.environ, PYTHONUNBUFFERED="1"), beat=beat)
            return got, buf.getvalue()

    def test_a_part_that_keeps_printing_puts_dots_in_the_log_while_it_runs(self):
        # relay 4, 2026-10-19: test_relay.py ran 1583 s with not one line in the gate's log, so a gate wait could not
        # tell a slow tool test from a hung one
        (code, out, dots), log = self.run_script(
            "import sys, time\nfor i in range(8):\n    sys.stderr.write('.'); sys.stderr.flush(); time.sleep(0.25)\n"
            "print('done')\n", beat=0.2)
        self.assertEqual(code, 0)
        self.assertGreaterEqual(dots, 3)
        self.assertEqual(log, "." * dots)
        self.assertIn(b"done", out)

    def test_a_quiet_part_puts_no_dot_and_its_exit_code_and_output_are_kept(self):
        (code, out, dots), log = self.run_script("import time, sys\ntime.sleep(1.0)\nprint('tail')\nsys.exit(3)\n",
                                                 beat=0.2)
        self.assertEqual(code, 3)
        self.assertLessEqual(dots, 1)                                           # the print just before the exit
        self.assertEqual(log, "." * dots)
        self.assertIn(b"tail", out)


if __name__ == "__main__":
    unittest.main(verbosity=1)
