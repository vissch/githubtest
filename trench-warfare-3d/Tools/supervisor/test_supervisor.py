#!/usr/bin/env python3
"""Tests of the supervisor's base (core.py, main.py) and the replay kit. Run: python Tools/supervisor/test_supervisor.py"""
import os
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402
from replay import records  # noqa: E402


def write_module(folder, name, body, default='dry'):
    p = Path(folder) / f'{name}.py'
    src = (f'NAME = {name!r}\nEVERY_S = 60\nDEFAULT_MODE = {default!r}\ndef run(ctx):\n'
           + textwrap.indent(textwrap.dedent(body), '    ') + '\n')
    p.write_text(src, encoding='utf-8')
    return p


class Core(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = Path(self.tmp.name) / 'home'
        self.mods = Path(self.tmp.name) / 'mods'
        self.mods.mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def load(self):
        found, broken = core.discover(self.mods)
        return {m.NAME: m for m in found}, broken

    def test_a_module_gets_its_own_default_and_the_home_overrides_it(self):
        write_module(self.mods, 'alpha', "return ['x']", default='dry')
        m = self.load()[0]['alpha']
        self.assertEqual(core.mode(m, self.home), 'dry')
        core.write_json(self.home / 'modules.json', {'alpha': 'on'})
        self.assertEqual(core.mode(m, self.home), 'on')
        core.write_json(self.home / 'modules.json', {'alpha': 'loud'})
        self.assertEqual(core.mode(m, self.home), 'off', 'an unknown switch word must read as off')

    def test_dry_mode_is_told_to_the_module_and_marked_in_the_log(self):
        write_module(self.mods, 'beta', "ctx.save({'dry': ctx.dry})\nreturn ['would stop relay 3']")
        m = self.load()[0]['beta']
        lines = core.turn(m, self.home, now=1000)
        self.assertEqual(lines, ['[dry] would stop relay 3'])
        self.assertEqual(core.read_state('beta', self.home), {'dry': True})
        self.assertIn('beta: [dry] would stop relay 3', (self.home / 'supervisor.log').read_text(encoding='utf-8'))

    def test_off_runs_nothing(self):
        write_module(self.mods, 'gamma', "ctx.save({'ran': 1})\nreturn ['ran']", default='off')
        m = self.load()[0]['gamma']
        self.assertEqual(core.turn(m, self.home), [])
        self.assertEqual(core.read_state('gamma', self.home), {})

    def test_a_failing_module_is_logged_and_does_not_stop_the_turn(self):
        write_module(self.mods, 'delta', "raise RuntimeError('boom')", default='on')
        m = self.load()[0]['delta']
        self.assertEqual(core.turn(m, self.home), ['FAILED: RuntimeError: boom'])

    def test_a_broken_file_is_reported_and_the_rest_still_load(self):
        write_module(self.mods, 'ok_one', "return []")
        (self.mods / 'bad.py').write_text('NAME = (', encoding='utf-8')
        mods, broken = self.load()
        self.assertIn('ok_one', mods)
        self.assertTrue(any(b.startswith('bad.py') for b in broken))

    def test_due_follows_every_s(self):
        write_module(self.mods, 'eps', "return []", default='on')
        m = self.load()[0]['eps']
        self.assertTrue(core.due(m, self.home, now=1000))
        core.turn(m, self.home, now=1000)
        self.assertFalse(core.due(m, self.home, now=1030))
        self.assertTrue(core.due(m, self.home, now=1060))

    def test_modules_read_each_other_only_through_state(self):
        write_module(self.mods, 'writer', "ctx.save({'open': True})\nreturn []", default='on')
        write_module(self.mods, 'reader', "return ['breaker open' if ctx.other('writer').get('open') else 'closed']",
                     default='on')
        mods = self.load()[0]
        core.turn(mods['writer'], self.home)
        self.assertEqual(core.turn(mods['reader'], self.home), ['breaker open'])


class Replay(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rs = records.load()

    def test_the_fixture_holds_the_two_weeks(self):
        self.assertGreater(len(self.rs.runs), 1000)
        self.assertGreater(len(self.rs.legs), 1400)
        self.assertGreater(len(self.rs.gates), 900)

    def test_the_licence_days_have_no_green_gate_and_name_the_licence(self):
        days = self.rs.between('2026-10-31T00:00', '2026-11-03T00:00')
        self.assertGreater(len(days.gates), 150)
        self.assertEqual([g for g in days.gates if g.get('exit_code') == 0], [])
        self.assertTrue(any('Licens' in l for g in days.gates for l in g['lines']))

    def test_the_good_night_has_green_gates(self):
        night = self.rs.between('2026-10-26T22:00', '2026-10-27T09:00')
        green = [g for g in night.gates if g.get('exit_code') == 0]
        self.assertGreater(len(green), 40)

    def test_between_and_timeline_keep_time_order(self):
        tl = self.rs.between('2026-10-20T00:00', '2026-10-21T00:00').timeline()
        ts = [records.when(r) for r in tl]
        self.assertEqual(ts, sorted(ts))
        self.assertTrue(all('2026-10-20' <= t < '2026-10-21' for t in ts))


if __name__ == '__main__':
    unittest.main()
