#!/usr/bin/env python3
"""Unit test for port_split.py that a node-less, editor-less machine can still run (Tools/toolcheck.py picks up any
Tools/**/test_*.py by its name; the checking script that drives a single unit's tests needs a test_... method of a
unittest.TestCase in a file ending in unittest.main()).

    python Tools/test_port_split.py
    python Tools/test_port_split.py TestPortSplit.test_twin_addition_lands_in_the_edited_method
"""
import pathlib
import subprocess
import sys
import tempfile
import unittest

HERE = pathlib.Path(__file__).resolve().parent


def git(args, cwd):
    return subprocess.run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', *args],
                           cwd=cwd, capture_output=True, check=True)


class TestPortSplit(unittest.TestCase):
    def test_twin_addition_lands_in_the_edited_method(self):
        with tempfile.TemporaryDirectory() as td:
            r = pathlib.Path(td) / 'repo'
            (r / 'A').mkdir(parents=True)
            git(['init', '-q', '-b', 'main'], r)
            big = r / 'A/Big.cs'
            method = lambda name, log: [f'    void {name}()', '    {', '        Init();', '        Step1();',
                                         '        Step2();'] + (['        Log();'] if log else []) + \
                ['        Step3();', '        Step4();', '        Reset();', '    }']
            big.write_text('\n'.join(['class Big', '{'] + method('A', True) + method('B', False) +
                                      ['    int keep0 = 0;', '}', '']))
            git(['add', '.'], r)
            git(['commit', '-qm', 'base'], r)
            git(['checkout', '-qb', 'edits'], r)
            lines = big.read_text().split('\n')
            lines.insert(lines.index('    void B()') + 5, '        Log();')
            big.write_text('\n'.join(lines))
            git(['commit', '-qam', 'edits'], r)
            git(['checkout', '-q', 'main'], r)
            big.write_text('\n'.join(['class Big', '{'] + method('A', True) + ['    int keep0 = 0;', '}', '']))
            (r / 'A/Big.Moved.cs').write_text('\n'.join(['partial class Big', '{'] + method('B', False) + ['}', '']))
            git(['add', '.'], r)
            git(['commit', '-qm', 'split'], r)

            p = subprocess.run([sys.executable, str(HERE / 'port_split.py'), 'A/Big.cs',
                                 '--from', 'main~1', '--to', 'edits'], cwd=r, capture_output=True)
            out = (p.stdout + p.stderr).decode('utf-8', 'replace')

            self.assertEqual(p.returncode, 0,
                              '[N13.4] the added line was dropped: the twin method A already had it\n' + out)
            moved = (r / 'A/Big.Moved.cs').read_text()
            self.assertEqual(moved.count('Log();'), 1,
                              '[N13.4] the added line was dropped: the twin method A already had it\n' + out)
            self.assertIn(
                '\n'.join(['        Init();', '        Step1();', '        Step2();', '        Log();',
                           '        Step3();', '        Step4();', '        Reset();']),
                moved,
                '[N13.4] the added line was dropped: the twin method A already had it\n' + out)
            self.assertEqual(big.read_text().count('Log();'), 1,
                              '[N13.4] the added line was dropped: the twin method A already had it\n' + out)
            self.assertFalse((r / 'A/Big.cs.port.rej').exists(),
                              '[N13.4] the added line was dropped: the twin method A already had it\n' + out)


if __name__ == '__main__':
    unittest.main()
