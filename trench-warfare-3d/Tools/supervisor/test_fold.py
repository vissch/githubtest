#!/usr/bin/env python3
"""Tests of landing.fold_chains: the owner's brief lists one row a chain. Run: python Tools/supervisor/test_fold.py"""
import importlib.util
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

    def test_a_name_that_does_not_hold_the_commit_is_not_folded(self):
        rows = [row('lane/show/x', 'a'), row('lane/show/x-g1', 'b')]
        out = landing.fold_chains(rows, lambda x, y: False)
        self.assertEqual(len(out), 2)


if __name__ == '__main__':
    unittest.main(verbosity=2)
