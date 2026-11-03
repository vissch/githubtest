#!/usr/bin/env python3
"""Tests of the breaker (modules/breaker.py) on the relays' real records, and of breaker_gate.py and its two hooks
(Tools/relay/runner.py before a leg, Tools/assetboard/autoloop.py before a relay start).
Run: python Tools/supervisor/test_breaker.py   (TW_BREAKER_STUB=1 swaps in the old behaviour, a breaker that never
opens: the replay tests must then fail)."""
import importlib.util
import os
import sys
import tempfile
import time
import types
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import breaker_gate  # noqa: E402
import core  # noqa: E402
import relays  # noqa: E402
from replay import records  # noqa: E402


def load(name):
    spec = importlib.util.spec_from_file_location(f'tw_sup_test_{name}', HERE / 'modules' / f'{name}.py')
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


breaker = load('breaker')
DECIDE = breaker.decide
if os.environ.get('TW_BREAKER_STUB'):
    DECIDE = lambda events, canary, now, prev: {'open': False}   # noqa: E731 - the old behaviour: nothing ever opens

T = relays.epoch


def replay(rs, decide):
    """Step the decision through the fixture in time order. The canary is played by the relays' own gates: a gate
    that printed a green verdict proves Unity works (its start is the canary's start); no gate here can stand in for
    a red canary, so on replay the breaker opens on the legs alone. Returns [(opened, closed or None, state)]."""
    evs = breaker.events_of(rs.legs, rs.gates)
    greens = sorted((T(g.get('finished_at')), T(g.get('started_at'))) for g in rs.gates
                    if T(g.get('finished_at')) and any(l.startswith('Gate green') or 'EditMode green' in l
                                                        for l in g.get('lines') or []))
    ticks = sorted({e['at'] for e in evs} | {f for f, _ in greens})
    st, out, gi, canary = {}, [], 0, {}
    for t in ticks:
        while gi < len(greens) and greens[gi][0] <= t:
            canary = {'verdict': 'green', 'started': greens[gi][1]}
            gi += 1
        new = decide(evs, canary, t, st)
        if new.get('open') and not st.get('open'):
            out.append([t, None, new])
        elif st.get('open') and not new.get('open'):
            out[-1][1] = t
        st = new
    return out


def spans(rs, decide):
    return [(a, b if b is not None else T(records.when(rs.all[-1])) + 1, s) for a, b, s in replay(rs, decide)]


def saved(rs, sp):
    legs = [l for l in rs.legs if any(a <= T(l.get('started_at')) < b for a, b, _ in sp)]
    return len(legs), sum(l.get('cost_usd') or 0 for l in legs)


class Replay(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rs = records.load()
        cls.sp = spans(cls.rs, DECIDE)

    def opened_in(self, a, b):
        return [s for s in self.sp if T(a) <= s[0] < T(b)]

    def test_opens_within_an_hour_of_the_first_licence_red_gate_of_10_31(self):
        first = min(T(g['finished_at']) for g in self.rs.between('2026-10-31T00:00', '2026-11-01T00:00').gates
                    if g.get('exit_code') == 6 and any('Aborting batchmode' in l for l in g.get('lines') or []))
        got = self.opened_in('2026-10-31T00:00', '2026-11-01T00:00')
        self.assertTrue(got, 'the breaker never opened on 10-31')
        a, b, st = got[0]
        print(f"\n10-31: first licence-red gate {breaker._iso(first)}, open {breaker._iso(a)} "
              f"(+{(a - first) // 60} min) kind {st['kind']}, closed {breaker._iso(b)}; would not have started "
              '%d legs ($%.0f)' % saved(self.rs, [s for s in self.sp if T('2026-10-31') <= s[0]]))
        self.assertLessEqual(a - first, 3600)
        self.assertIn(st['kind'], ('licence', 'broken-base'))

    def test_opens_in_the_gate_hang_of_10_19(self):
        got = self.opened_in('2026-10-18T22:00', '2026-10-19T20:00')
        self.assertTrue(got, 'the breaker never opened in the gate hang of 10-19')
        a, b, st = got[0]
        print(f"\n10-19: open {breaker._iso(a)} kind {st['kind']} ({len(st['evidence'])} units), closed "
              f'{breaker._iso(b)}; would not have started %d legs ($%.0f)'
              % saved(self.rs, [s for s in self.sp if s[0] < T('2026-10-19T20:00')]))
        self.assertEqual(st['kind'], 'gate-stall')

    def test_never_open_on_the_good_night_of_10_26(self):
        a, b = T('2026-10-26T21:00'), T('2026-10-27T08:40')
        hit = [s for s in self.sp if s[0] < b and s[1] > a]
        self.assertEqual(hit, [], 'the breaker was open on the good night (45 passes): '
                         + '; '.join(f'{breaker._iso(x)}..{breaker._iso(y)} {s.get("reason")}' for x, y, s in hit))

    def test_the_rule_still_trips_on_a_made_up_pair_in_the_good_night(self):
        # an input that must still fail: two units with the same licence words inside the good night
        legs = [dict(t='leg', relay='3', run='r', leg=1, unit='a', state='DONE', said='blocked',
                     finished_at='2026-10-27T03:00:00Z', report='RESULT: blocked - machine-wide licence fault'),
                dict(t='leg', relay='4', run='r', leg=1, unit='b', state='DONE', said='blocked',
                     finished_at='2026-10-27T03:40:00Z', report='RESULT: blocked - Unity licence timed out')]
        st = breaker.decide(breaker.events_of(legs), {}, T('2026-10-27T03:40'), {})
        self.assertTrue(st['open'])
        self.assertEqual(st['kind'], 'licence')


class Decide(unittest.TestCase):
    def ev(self, unit, at, kind='licence'):
        return {'at': at, 'unit': unit, 'kind': kind, 'relay': '1', 'line': 'x'}

    def test_one_unit_twice_does_not_open_and_two_units_an_hour_apart_do_not(self):
        self.assertFalse(breaker.decide([self.ev('a', 100), self.ev('a', 200)], {}, 300, {})['open'])
        self.assertFalse(breaker.decide([self.ev('a', 100), self.ev('b', 100 + 3601)], {}, 3800, {})['open'])
        self.assertFalse(breaker.decide([self.ev('a', 100), self.ev('b', 200, 'leg-time')], {}, 300, {})['open'])

    def test_closes_only_on_a_green_canary_started_after_it_opened(self):
        st = breaker.decide([self.ev('a', 100), self.ev('b', 200)], {}, 200, {})
        self.assertTrue(st['open'])
        self.assertTrue(breaker.decide([], {'verdict': 'green', 'started': 150}, 5000, st)['open'])
        self.assertTrue(breaker.decide([], {'verdict': 'running', 'started': 300}, 5000, st)['open'])
        shut = breaker.decide([], {'verdict': 'green', 'started': 300}, 5000, st)
        self.assertFalse(shut['open'])
        # the events that opened it do not open it again after the close
        self.assertFalse(breaker.decide([self.ev('a', 100), self.ev('b', 200)], {}, 5001, shut)['open'])

    def test_a_red_canary_opens_it(self):
        st = breaker.decide([], {'verdict': 'red', 'kind': 'licence', 'tree': 't1'}, 10, {})
        self.assertTrue(st['open'])
        self.assertEqual(st['kind'], 'licence')
        self.assertFalse(breaker.decide([], {'verdict': 'red', 'kind': 'test'}, 10, {})['open'],
                         'a test that fails on integration is not the machine')


class Gate(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = Path(self.tmp.name)
        core.write_state('breaker', {'open': True, 'kind': 'licence', 'since': 1, 'reason': 'two units'}, self.home)
        self.old = os.environ.get('TW_SUP_HOME')
        os.environ['TW_SUP_HOME'] = str(self.home)

    def tearDown(self):
        if self.old is None:
            os.environ.pop('TW_SUP_HOME', None)
        else:
            os.environ['TW_SUP_HOME'] = self.old
        self.tmp.cleanup()

    def switch(self, word, canary='on'):
        core.write_json(self.home / 'modules.json', {'breaker': word, 'canary': canary} if word else {})

    def test_allow_is_true_unless_on_and_open(self):
        for word in (None, 'dry', 'off'):
            self.switch(word)
            self.assertTrue(breaker_gate.allow()[0], word)
        self.switch('on', canary='dry')
        self.assertTrue(breaker_gate.allow()[0], 'with no canary to close it, the breaker must hold nothing')
        self.switch('on')
        ok, why = breaker_gate.allow()
        self.assertFalse(ok)
        self.assertIn('two units', why)
        core.write_state('breaker', {'open': False}, self.home)
        self.assertTrue(breaker_gate.allow()[0])
        (self.home / 'state' / 'breaker.json').write_text('{half', encoding='utf-8')
        self.assertTrue(breaker_gate.allow()[0], 'an unreadable state must not stop the relays')

    def runner_room(self):
        sys.path.insert(0, str(HERE.parent / 'relay'))
        import runner
        fake = types.SimpleNamespace(deadline=time.time() + 3600, lim={'run_hours': 1}, a=types.SimpleNamespace(max_legs=0),
                                     legs=0, stop_file=self.home / 'no-such-stop', no_budget=lambda phases, unit=None: None)
        return runner.Run.no_room(fake, 'execute')

    def test_runner_refuses_a_leg_only_when_the_breaker_is_on_and_open(self):
        self.switch('dry')
        self.assertIsNone(self.runner_room(), 'with the switch at dry the hook must change nothing')
        self.switch('on')
        why = self.runner_room()
        self.assertEqual(why.kind, 'breaker')
        self.assertIn('two units', why)

    def autoloop_start(self):
        sys.path.insert(0, str(HERE.parent / 'assetboard'))
        import autoloop
        w = autoloop.World(main=self.home / 'board', extras=(), home=self.home / 'loop', acting=False)
        return w.start(3)

    def test_autoloop_starts_no_relay_only_when_the_breaker_is_on_and_open(self):
        self.switch('dry')
        self.assertEqual(self.autoloop_start(), 'would start', 'with the switch at dry the hook must change nothing')
        self.switch('on')
        self.assertTrue(self.autoloop_start().startswith('not started: breaker'))


class NoSupervisor(unittest.TestCase):
    def test_autoloop_and_runner_import_in_a_checkout_without_the_gate(self):
        """A checkout without Tools/supervisor: both hooks fall back to a gate that allows everything (M.2)."""
        import subprocess
        block = ('import sys\n'
                 'class NoGate:\n'
                 '    def find_spec(self, name, path=None, target=None):\n'
                 '        if name == "breaker_gate":\n'
                 '            raise ImportError("no Tools/supervisor in this checkout")\n'
                 'sys.meta_path.insert(0, NoGate())\n'
                 'sys.path.insert(0, sys.argv[1])\n'
                 'm = __import__(sys.argv[2])\n'
                 'print("ALLOW", m.breaker_gate.allow()[0])\n')
        for folder, name in (('assetboard', 'autoloop'), ('relay', 'runner')):
            r = subprocess.run([sys.executable, '-c', block, str(HERE.parent / folder), name], capture_output=True,
                               text=True, timeout=120, cwd=str(HERE.parent.parent),
                               env=dict(os.environ, TW_SUP_HOME=tempfile.mkdtemp(prefix='tw-sup-test-')))
            self.assertEqual(r.returncode, 0, f'{name} does not import without the supervisor (M.2): {r.stderr[-600:]}')
            self.assertIn('ALLOW True', r.stdout, r.stdout[-300:])


if __name__ == '__main__':
    unittest.main(verbosity=2)
