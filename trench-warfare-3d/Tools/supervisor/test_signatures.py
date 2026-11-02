#!/usr/bin/env python3
"""Tests of signatures.py on the relays' real leg reports. Run: python Tools/supervisor/test_signatures.py"""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from signatures import classify, is_machine, MACHINE, HARNESS, KINDS  # noqa: E402
from replay import records  # noqa: E402


def not_done(legs):
    return [l for l in legs if l.get('phase') == 'execute' and l.get('said') != 'done']


class Rows(unittest.TestCase):
    def test_every_kind_is_machine_or_harness_and_unit_is_neither(self):
        self.assertEqual({k for k, _ in KINDS}, MACHINE | HARNESS)
        self.assertFalse(is_machine('unit'))

    def test_the_words_of_the_licence_fault(self):
        for text in ["RESULT: blocked - Unity cannot start on this machine (licensing pipe `Unity-LicenseClient-PC` "
                     "access denied)", "[Licensing::Module] Error: Licensing initialization failed after 75.33s",
                     "gate failed twice with the exact licensing-collision compile storm"]:
            self.assertEqual(classify(text), 'licence', text)

    def test_a_unit_fault_stays_the_units(self):
        for text in ["RESULT: blocked - the probe still fails 2 of 8 bars, so the plan's own risk clause stops here.",
                     "RESULT: failed - the prone man reads 0.81 of the kneeling man; the bar is 0.9"]:
            self.assertEqual(classify(text), 'unit', text)
        self.assertEqual(classify(''), 'unit')
        self.assertEqual(classify(None), 'unit')


class Replay(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rs = records.load()

    def share(self, a, b):
        legs = not_done(self.rs.between(a, b).legs)
        machine = [l for l in legs if is_machine(classify(l.get('report')))]
        return len(legs), len(machine)

    def test_the_licence_days_read_as_the_machines_fault(self):
        n, m = self.share('2026-10-31T12:00', '2026-11-03T00:00')
        self.assertGreater(n, 80)
        self.assertGreaterEqual(m / n, 0.85, f'{m} of {n} legs on the licence days read as machine faults')

    def test_the_good_night_mostly_reads_as_units(self):
        n, m = self.share('2026-10-26T21:00', '2026-10-27T08:40')
        self.assertLessEqual(m / max(n, 1), 0.25, f'{m} of {n} legs on the good night read as machine faults')

    def test_finished_legs_hardly_ever_read_as_a_machine_fault(self):
        done = [l for l in self.rs.legs if l.get('phase') == 'execute' and l.get('said') == 'done']
        machine = [l for l in done if is_machine(classify(l.get('report')))]
        self.assertLessEqual(len(machine) / len(done), 0.02)

    def test_the_gate_logs_of_the_licence_days_name_the_licence(self):
        gates = self.rs.between('2026-10-31T12:00', '2026-11-03T00:00').gates
        hits = [g for g in gates if classify('\n'.join(g.get('lines') or [])) == 'licence']
        self.assertGreaterEqual(len(hits), 15)


if __name__ == '__main__':
    unittest.main()
