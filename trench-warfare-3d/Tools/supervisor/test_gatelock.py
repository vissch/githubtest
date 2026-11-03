#!/usr/bin/env python3
"""Tests of gatelock.py: one gate the supervisor starts at a time, and a lock whose holder is gone or too old frees.

    python Tools/supervisor/test_gatelock.py
"""
import json
import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import gatelock  # noqa: E402


def dead_pid():
    p = subprocess.Popen([sys.executable, '-c', 'pass'])
    p.wait()
    return p.pid


class GateLock(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.lock = Path(self.tmp.name) / 'gate.lock'

    def tearDown(self):
        self.tmp.cleanup()

    def test_second_gate_is_refused_while_the_first_holds(self):
        ok, rec = gatelock.acquire('canary', pid=os.getpid(), p=self.lock)
        self.assertTrue(ok)
        ok2, holder = gatelock.acquire('landing', pid=os.getpid(), p=self.lock)
        self.assertFalse(ok2)
        self.assertEqual(holder['who'], 'canary')
        self.assertEqual(json.loads(self.lock.read_text(encoding='utf-8'))['who'], 'canary')

    def test_a_lock_whose_process_is_gone_is_taken_over(self):
        gatelock.acquire('canary', pid=dead_pid(), p=self.lock)
        ok, rec = gatelock.acquire('landing', pid=os.getpid(), p=self.lock)
        self.assertTrue(ok)
        self.assertEqual(gatelock.read(self.lock)['who'], 'landing')

    def test_a_lock_older_than_three_hours_is_taken_over(self):
        gatelock.acquire('canary', pid=os.getpid(), p=self.lock, now=time.time() - gatelock.STALE_S - 60)
        ok, _ = gatelock.acquire('landing', pid=os.getpid(), p=self.lock)
        self.assertTrue(ok)
        # and a young one is not
        ok, holder = gatelock.acquire('canary', pid=os.getpid(), p=self.lock)
        self.assertFalse(ok)
        self.assertEqual(holder['who'], 'landing')

    def test_only_the_holder_releases_and_hands_over(self):
        gatelock.acquire('landing', pid=os.getpid(), p=self.lock)
        self.assertFalse(gatelock.release('canary', p=self.lock))
        self.assertFalse(gatelock.handover('canary', 4, p=self.lock))
        self.assertTrue(self.lock.exists())
        self.assertTrue(gatelock.handover('landing', 1234, p=self.lock))
        self.assertEqual(gatelock.read(self.lock)['pid'], 1234)
        self.assertTrue(gatelock.release('landing', p=self.lock))
        self.assertFalse(self.lock.exists())
        ok, _ = gatelock.acquire('canary', pid=os.getpid(), p=self.lock)
        self.assertTrue(ok)

    def test_a_fresh_lock_made_by_another_taker_meanwhile_is_not_removed(self):
        # B read a stale lock; before B moves it aside, A has taken it over and made a fresh one: B must not win
        gatelock.acquire('canary', pid=os.getpid(), p=self.lock)
        real, calls = gatelock.read, []

        def read(p=None):
            calls.append(p)
            rec = real(p)
            if len(calls) == 1:
                return dict(rec, pid=dead_pid())          # what B saw a moment ago
            return rec
        gatelock.read = read
        try:
            ok, holder = gatelock.acquire('landing', pid=os.getpid(), p=self.lock)
        finally:
            gatelock.read = real
        self.assertFalse(ok)
        self.assertEqual(holder['who'], 'canary')
        self.assertEqual(gatelock.read(self.lock)['who'], 'canary')

    def test_pid_alive(self):
        self.assertTrue(gatelock.pid_alive(os.getpid()))
        self.assertFalse(gatelock.pid_alive(dead_pid()))
        self.assertFalse(gatelock.pid_alive(None))


if __name__ == '__main__':
    unittest.main(verbosity=2)
