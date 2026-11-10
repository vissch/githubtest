#!/usr/bin/env python3
"""Tests of ops/switch.py. Run: python Tools/supervisor/test_switch.py"""
import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('tw_sup_switch', HERE / 'ops' / 'switch.py')
sw = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sw)


class Switch(unittest.TestCase):
    def test_a_switch_keeps_the_others_and_is_logged(self):
        with tempfile.TemporaryDirectory() as h:
            (Path(h) / 'modules.json').write_text(json.dumps({'canary': 'on'}))
            line = sw.switch('landing', 'on', h, who='test')
            self.assertIn('landing dry -> on (by test)', line)
            self.assertEqual(json.loads((Path(h) / 'modules.json').read_text()), {'canary': 'on', 'landing': 'on'})
            self.assertIn('landing dry -> on', (Path(h) / 'supervisor.log').read_text())

    def test_no_such_module_or_mode_is_refused(self):
        with tempfile.TemporaryDirectory() as h:
            with self.assertRaises(SystemExit):
                sw.switch('landng', 'on', h)
            with self.assertRaises(SystemExit):
                sw.switch('landing', 'yes', h)
            self.assertFalse((Path(h) / 'modules.json').exists())


if __name__ == '__main__':
    unittest.main(verbosity=2)
