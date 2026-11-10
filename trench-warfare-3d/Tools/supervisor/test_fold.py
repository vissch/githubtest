#!/usr/bin/env python3
"""Tests of landing.fold_chains: the owner's brief lists one row a chain. Run: python Tools/supervisor/test_fold.py"""
import importlib.util
import sys
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('tw_sup_landing_fold', HERE / 'modules' / 'landing.py')
landing = importlib.util.module_from_spec(spec)
spec.loader.exec_module(landing)


def row(lane, tip):
    return dict(lane=lane, tip=tip, cls='look', why='', n=1, title=lane, pictures=[])


class Fold(unittest.TestCase):
    def test_a_chain_is_one_row_at_its_tip(self):
        a = 'lane/show/ability-fx-captures-r2-2'
        rows = [row(a, 'a'), row(a + '-g1', 'b'), row(a + '-g1-g2', 'c'), row(a + '-g2', 'd'), row('lane/show/other', 'e')]
        holds = {('a', 'b'), ('a', 'c'), ('b', 'c'), ('a', 'd')}         # g2 branched from r2-2 too
        out = landing.fold_chains(rows, lambda x, y: (x, y) in holds)
        self.assertEqual(sorted((r['lane'][10:], r.get('folded', 0)) for r in out),
                         [('ability-fx-captures-r2-2-g1-g2', 2), ('ability-fx-captures-r2-2-g2', 0), ('other', 0)])

    def test_siblings_of_one_family_are_one_row_at_the_biggest(self):
        a = 'lane/show/ability-fx-captures'
        rows = [dict(row(a + '-r2-2-g1-g1', 'a'), n=73), dict(row(a + '-r2-2-g1-g2', 'b'), n=80),
                dict(row(a, 'c'), n=34), row('lane/show/sentry-look-r3', 'd'), row('lane/show/sentry-look-r4', 'e'),
                dict(row('lane/sim/ability-fx-captures', 'f'), cls='sim')]
        out = landing.by_family(rows)
        got = sorted((r['cls'], r['lane'].split('/', 2)[-1], r.get('folded', 0)) for r in out)
        self.assertEqual(got, [('look', 'ability-fx-captures-r2-2-g1-g2', 2), ('look', 'sentry-look-r4', 1),
                               ('sim', 'ability-fx-captures', 0)])

    def test_the_brief_text_fits_the_brief_tool(self):
        rows = [row('lane/show/x%d' % i, str(i)) for i in range(300)]
        args = landing.brief_args(rows, '2026-11-10', 'C:/a/very/long/path/to/owner-2026-11-10.md')
        what_for = args[args.index('--for') + 1]
        self.assertLessEqual(len(what_for.split()), 45, what_for)

    def test_a_name_that_does_not_hold_the_commit_is_not_folded(self):
        rows = [row('lane/show/x', 'a'), row('lane/show/x-g1', 'b')]
        out = landing.fold_chains(rows, lambda x, y: False)
        self.assertEqual(len(out), 2)


class GateLaunch(unittest.TestCase):
    """11-10: the first 'on' turn started gate.ps1 with DETACHED_PROCESS; it ran nothing, wrote an empty log and read
    as red. This starts a stand-in gate.ps1 the way the module does and reads its Write-Host line from the log."""
    @unittest.skipUnless(sys.platform == 'win32', 'the launch flags are Windows ones')
    def test_the_gates_write_host_lines_reach_the_log(self):
        import tempfile
        import time
        with tempfile.TemporaryDirectory() as tmp:
            stage = Path(tmp) / "stage o'brien"            # a space and a quote in the path, as a guard
            stage.mkdir()
            (stage / 'gate.ps1').write_text('Write-Host "Gate green. Tested tree abc123"\nexit 0\n', encoding='utf-8')
            log = Path(tmp) / 'gate.log'
            w = landing.World(tmp, stage, tmp, tmp)
            pid = w.popen(landing.gate_cmd(stage), stage, log)
            for _ in range(120):
                if not w.alive(pid):
                    break
                time.sleep(0.5)
            text = log.read_bytes().decode('utf-8', 'replace')
            self.assertIn('Gate green. Tested tree abc123', text)


if __name__ == '__main__':
    unittest.main(verbosity=2)
