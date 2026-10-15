#!/usr/bin/env python3
"""Tests for gate_bg.py: python Tools/test_gate_bg.py. Stdlib only, no Unity, no gate started, no network."""
import contextlib
import io
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import gate_bg as G


class Status(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.where = Path(self.tmp.name)
        self.log = self.where / 'log'
        self.pidfile = self.where / 'pid'
        self.old_probe = G.probe

    def tearDown(self):
        G.probe = self.old_probe
        self.tmp.cleanup()

    def run_status(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = G.status(self.log, self.pidfile)
        return code, out.getvalue()

    def test_own_pid_with_a_2000_stamp_is_stale(self):
        import os
        self.pidfile.write_text(f'{os.getpid()} 2000-01-01T00:00:00\n')
        code, said = self.run_status()
        self.assertNotEqual(code, 2, said)

    def test_own_pid_with_a_stamp_of_now_is_running(self):
        import os
        self.pidfile.write_text(f'{os.getpid()} {time.strftime("%Y-%m-%dT%H:%M:%S")}\n')
        code, said = self.run_status()
        self.assertEqual(code, 2, said)

    def test_unreadable_start_time_falls_back_to_pid_only(self):
        import os
        G.probe = lambda pid: (None, None)
        self.pidfile.write_text(f'{os.getpid()} 2000-01-01T00:00:00\n')
        code, said = self.run_status()
        self.assertEqual(code, 2, said)
        self.assertIn('could not read', said)

    def test_a_pid_that_is_not_alive_is_not_the_gate(self):
        p = subprocess.Popen([sys.executable, '-c', 'pass'])
        dead_pid = p.pid
        p.wait()
        self.pidfile.write_text(f'{dead_pid} {time.strftime("%Y-%m-%dT%H:%M:%S")}\n')
        code, said = self.run_status()
        self.assertNotEqual(code, 2, said)

    def test_command_line_alone_finds_the_gate(self):
        import os
        G.probe = lambda pid: (None, 'powershell -File C:\\x\\gate.ps1')
        self.pidfile.write_text(f'{os.getpid()} 2000-01-01T00:00:00\n')
        code, said = self.run_status()
        self.assertEqual(code, 2, said)

    def test_an_unrelated_program_that_reused_the_pid_is_rejected(self):
        import os
        G.probe = lambda pid: (time.time(), 'notepad.exe')
        self.pidfile.write_text(f'{os.getpid()} 2000-01-01T00:00:00\n')
        code, said = self.run_status()
        self.assertNotEqual(code, 2, said)

    def test_an_old_pidfile_with_no_stamp_still_works(self):
        import os
        G.probe = lambda pid: (None, None)
        self.pidfile.write_text(f'{os.getpid()}\n')
        code, said = self.run_status()
        self.assertEqual(code, 2, said)


if __name__ == '__main__':
    sys.exit(0 if unittest.main(exit=False).result.wasSuccessful() else 1)
