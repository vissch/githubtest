#!/usr/bin/env python3
"""Tests for fixcheck.py: python Tools/review/test_fixcheck.py. Stdlib only, no Unity.

A small git repo made here stands in for the game repo, and a stand-in for Unity answers from the "production"
file it finds in the project. Each verdict is shown both ways: a real fix is PROVED, and the same commits with the
one thing wrong (a test that cannot fail, an id nobody named, a BOM, a file outside the lane) FAIL.
"""
import contextlib, io, json, os, subprocess, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import fixcheck as F

P = 'trench-warfare-3d/'
FAKE_UNITY = r'''
import sys, re
from pathlib import Path
a = sys.argv[1:]
arg = lambda n: a[a.index(n) + 1]
proj, names = Path(arg('-projectPath')), arg('-testFilter').split(';')
src = (proj / 'Assets/_Project/Sim/Thing.cs').read_text(encoding='utf-8')
tests = (proj / 'Assets/_Project/Tests/Sim/ThingTests.cs').read_text(encoding='utf-8')
if 'NewName' in tests and 'NewName' not in src:
    Path(arg('-logFile')).write_text("ThingTests.cs(9,9): error CS0117: 'Thing' has no 'NewName'\n", encoding='utf-8')
    sys.exit(1)
def row(n):
    if 'FIXED' in src or 'Always' in n:
        return '<test-case fullname="%s" result="Passed"/>' % n
    said = 'the thing is not fixed' if 'SAYS_NO_ID' in tests else '[U1] the thing is not fixed'
    return '<test-case fullname="%s" result="Failed"><failure><message>%s</message></failure></test-case>' % (n, said)
rows = ''.join(row(n) for n in names)
Path(arg('-testResults')).write_text('<test-run>' + rows + '</test-run>', encoding='utf-8')
if 'HOLDS_LOG' in tests:      # as Unity's licensing client does: something keeps the log open after Unity has gone
    import subprocess, time
    subprocess.Popen([sys.executable, '-c', 'import sys, time; f = open(sys.argv[1], "a"); time.sleep(4)', arg('-logFile')],
                     stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(0.8)
'''
FAKE_SELFTEST = r'''
import subprocess
src = subprocess.run(['git', 'show', 'HEAD:trench-warfare-3d/Tools/thing.py'], capture_output=True).stdout.decode()
print(('ok    ' if 'return 2' in src else 'FAIL  ') + '[S1] the thing answers two')
print('ok    [S2] a case that is always green')
'''
CS_TEST = '''namespace TW.Tests
{
    public sealed class ThingTests
    {
        // [U1] the thing was never fixed
        [Test]
        public void TheThingIsFixed()
        {
            Assert.IsTrue(Thing.Fixed);
        }

        [Test]
        public void AlwaysGreen()
        {
            // [U2] a tag inside a body that cannot fail
            Assert.Pass();
        }
    }
}
'''


class Repo(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='fixcheck-test-')
        self.tree = Path(self.tmp.name) / 'tree-fixcheck'
        self.tree.mkdir()
        self.unity = Path(self.tmp.name) / 'fake_unity.py'
        self.unity.write_text(FAKE_UNITY, encoding='utf-8')
        self.git('init', '-q', '-b', 'main')
        self.git('config', 'core.autocrlf', 'false')    # fixcheck's own git calls must write what these wrote
        self.put(P + 'Tools/thing.py', 'def answer():\n    return 1\n')
        self.put(P + 'Tools/test_thing.py', 'import sys, unittest\nsys.path.insert(0, "Tools")\nimport thing\n\n\n'
                                            'class ThingTest(unittest.TestCase):\n    def test_old(self):\n'
                                            '        self.assertTrue(True)\n\n\nif __name__ == "__main__":\n'
                                            '    unittest.main()\n')
        self.put(P + 'Assets/_Project/Sim/Thing.cs', 'class Thing { }\n')
        self.put(P + 'Assets/_Project/Tests/Sim/ThingTests.cs', 'namespace TW.Tests { class ThingTests { } }\n')
        self.put(P + 'Assets/_Project/Presentation/View.cs', 'class View { }\n')
        self.put('docs/notes.md', 'notes\n')
        self.base = self.commit('the start')

    def tearDown(self):
        self.tmp.cleanup()

    def git(self, *args):
        p = subprocess.run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', '-c', 'core.autocrlf=false'] + list(args),
                           cwd=self.tree, capture_output=True)
        self.assertEqual(p.returncode, 0, p.stderr.decode('utf-8', 'replace'))
        return p.stdout.decode('utf-8', 'replace').strip()

    def put(self, path, text, raw=None):
        f = self.tree / path
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_bytes(raw if raw is not None else text.encode('utf-8'))

    def commit(self, message):
        self.git('add', '-A')
        self.git('commit', '-q', '-m', message)
        return self.git('rev-parse', 'HEAD')

    def py_fix(self, test_body="        self.assertEqual(thing.answer(), 2, '[T1] it answered one')\n",
               message='the thing answers two [T1]'):
        self.put(P + 'Tools/thing.py', 'def answer():\n    return 2\n')
        text = (self.tree / (P + 'Tools/test_thing.py')).read_text(encoding='utf-8')
        new = '    def test_answer(self):\n        # [T1] it answered one\n' + test_body + '\n'
        self.put(P + 'Tools/test_thing.py', text.replace('\n\nif __name__', '\n' + new + '\nif __name__'))
        return self.commit(message)

    def check(self, head, ids, lane='lane/show/review-tools', **kw):
        out = Path(self.tmp.name) / 'rec.json'
        argv = ['--tree', str(self.tree), '--base', self.base, '--head', head, '--lane', lane, '--ids'] + ids
        argv += ['--out', str(out), '--unit', 'rv-test']
        argv += ['--unity', kw['unity']] if kw.get('unity') is not None else ['--unity', '']
        argv += kw.get('extra') or []
        said = io.StringIO()
        with contextlib.redirect_stdout(said):
            code = F.main(argv)
        return code, json.loads(out.read_text(encoding='utf-8')), said.getvalue()


class PythonFixes(Repo):
    def test_a_real_fix_is_proved_red_then_green(self):
        code, rec, said = self.check(self.py_fix(), ['T1'])
        self.assertEqual((code, rec['verdict'], rec['ids']['T1']['verdict']), (0, 'PASS', 'PROVED'), said)
        t = rec['ids']['T1']['tests'][0]
        self.assertEqual((t['name'], t['old'], t['new']), ('ThingTest.test_answer', 'red', 'green'))

    def test_red_for_a_reason_that_does_not_name_the_finding_is_red_not_proved(self):
        code, rec, said = self.check(self.py_fix(test_body='        self.assertEqual(thing.answer(), 2)\n'), ['T1'])
        self.assertEqual((code, rec['verdict'], rec['ids']['T1']['verdict'], rec['counts']['RED']),
                         (0, 'PASS', 'RED', 1), said)
        self.assertIn('1 != 2', rec['ids']['T1']['tests'][0]['old_said'])
        self.assertIn('does not name [T1]', rec['ids']['T1']['why'])

    def test_a_test_that_cannot_fail_fails_the_unit(self):
        code, rec, said = self.check(self.py_fix(test_body='        self.assertTrue(thing.answer() > 0)\n'), ['T1'])
        self.assertEqual((code, rec['ids']['T1']['verdict']), (1, 'FAIL'), said)
        self.assertIn('cannot fail', rec['ids']['T1']['why'])

    def test_a_fix_whose_test_is_red_on_the_fix_fails(self):
        code, rec, said = self.check(self.py_fix(test_body='        self.assertEqual(thing.answer(), 3)\n'), ['T1'])
        self.assertEqual((code, rec['ids']['T1']['why']), (1, 'a tagged test is red on the fix'), said)

    def test_an_id_no_commit_names_fails(self):
        code, rec, said = self.check(self.py_fix(message='the thing answers two'), ['T1'])
        self.assertEqual(code, 1)
        self.assertIn('no commit message names [T1]', rec['ids']['T1']['why'])

    def test_no_test_needs_its_reason_in_a_commit(self):
        self.put('docs/notes.md', 'notes, corrected\n')
        head = self.commit('the note is right [D1]')
        code, rec, said = self.check(head, ['D1'])
        self.assertEqual((code, rec['ids']['D1']['verdict']), (1, 'FAIL'), said)
        self.put('docs/notes.md', 'notes, corrected again\n')
        head = self.commit('[D1] no test: it is a sentence in a doc')
        code, rec, said = self.check(head, ['D1'])
        self.assertEqual((code, rec['ids']['D1']['verdict'], rec['ids']['D1']['why']),
                         (0, 'NO TEST', 'it is a sentence in a doc'), said)

    def test_a_follow_up_that_only_mends_a_test_names_the_fix_already_in_the_base(self):
        self.base = self.py_fix(test_body='        self.assertTrue(thing.answer() > 0)\n')   # the fix, its test cannot fail
        text = (self.tree / (P + 'Tools/test_thing.py')).read_text(encoding='utf-8')
        self.put(P + 'Tools/test_thing.py',
                 text.replace('# [T1] it answered one\n        self.assertTrue(thing.answer() > 0)',
                              '# [T1b] it answered one\n        self.assertEqual(thing.answer(), 2, "[T1b] one")'))
        head = self.commit('the test can fail now [T1b]')
        code, rec, said = self.check(head, ['T1b'])
        self.assertEqual((code, rec['ids']['T1b']['verdict']), (1, 'FAIL'), said)
        self.assertIn('fix in base', rec['ids']['T1b']['why'])
        code, rec, said = self.check(head, ['T1b'], extra=['--fix-in-base', 'T1b=' + self.base])
        self.assertEqual((code, rec['ids']['T1b']['verdict']), (0, 'PROVED'), said)
        self.put('docs/notes.md', 'notes, again\n')
        head = self.commit('[T1b] fix in base: %s' % self.base[:10])
        code, rec, said = self.check(head, ['T1b'])
        self.assertEqual((code, rec['ids']['T1b']['verdict'], rec['ids']['T1b']['fix_in_base']),
                         (0, 'PROVED', self.base), said)
        self.assertEqual(self.git('status', '--porcelain'), '')

    def test_the_tree_is_back_on_the_fix_and_clean_afterwards(self):
        head = self.py_fix()
        self.check(head, ['T1'])
        self.assertEqual(self.git('rev-parse', 'HEAD'), head)
        self.assertEqual(self.git('status', '--porcelain'), '')
        self.assertIn('return 2', (self.tree / (P + 'Tools/thing.py')).read_text(encoding='utf-8'))

    def test_a_selftest_case_is_read_from_its_line_and_sees_the_old_code_through_head(self):
        self.put(P + 'Tools/selftest.py', F'# cases\nX = """\ncase("[S1] the thing answers two", ok)\n'
                                          F'case("[S2] a case that is always green", ok)\n"""\n' + FAKE_SELFTEST)
        self.put(P + 'Tools/thing.py', 'def answer():\n    return 2\n')
        head = self.commit('two [S1] [S2]')
        code, rec, said = self.check(head, ['S1', 'S2'])
        self.assertEqual(rec['ids']['S1']['verdict'], 'PROVED', said)
        self.assertEqual((rec['ids']['S2']['verdict'], code), ('FAIL', 1), 'green on the old code too: ' + said)


class WhatGitAloneSays(Repo):
    def test_a_gained_bom_fails_and_names_the_file(self):
        head_ok = self.py_fix()
        self.put(P + 'Tools/thing.py', None, raw=F.BOM + b'def answer():\n    return 2\n')
        head = self.commit('the same, saved by an editor [T1]')
        self.assertEqual(self.check(head_ok, ['T1'])[0], 0)
        code, rec, said = self.check(head, ['T1'])
        self.assertEqual((code, rec['gained_bom']), (1, [P + 'Tools/thing.py']), said)

    def test_a_file_that_had_its_bom_before_is_not_blamed(self):
        self.put('docs/notes.md', None, raw=F.BOM + b'notes\n')
        self.base = self.commit('an old BOM')
        self.put('docs/notes.md', None, raw=F.BOM + b'notes, more\n')
        head = self.commit('[D1] no test: a doc')
        code, rec, said = self.check(head, ['D1'])
        self.assertEqual((code, rec['gained_bom']), (0, []), said)

    def test_each_lane_keeps_to_its_folders(self):
        self.put(P + 'Assets/_Project/Sim/Thing.cs', 'class Thing { /* sim */ }\n')
        self.put(P + 'Assets/_Project/Presentation/View.cs', 'class View { /* show */ }\n')
        head = self.commit('[X1] no test: both lanes at once')
        sim, show = P + 'Assets/_Project/Sim/Thing.cs', P + 'Assets/_Project/Presentation/View.cs'
        for lane, outside in (('lane/sim/x', [show]), ('lane/show/x', [sim]), ('lane/show/review-tools', [show, sim])):
            code, rec, said = self.check(head, ['X1'], lane=lane)
            self.assertEqual((code, sorted(rec['outside_lane'])), (1, sorted(outside)), lane)

    def test_only_a_tree_of_its_own_and_a_clean_one(self):
        head = self.py_fix()
        (self.tree / 'stray.txt').write_text('x', encoding='utf-8')
        with self.assertRaises(SystemExit) as e:
            self.check(head, ['T1'])
        self.assertIn('uncommitted', str(e.exception))
        (self.tree / 'stray.txt').unlink()
        other = self.tree.parent / 'somebodys-checkout'
        other.mkdir()
        with self.assertRaises(SystemExit) as e:
            F.main(['--tree', str(other), '--base', 'a', '--head', 'b', '--lane', 'lane/sim/x', '--ids', 'T1'])
        self.assertIn("not a checkout of fixcheck's own", str(e.exception))


class UnityFixes(Repo):
    def cs_fix(self, source='class Thing { public static bool Fixed = true; /* FIXED */ }\n', tests=CS_TEST):
        self.put(P + 'Assets/_Project/Sim/Thing.cs', source)
        self.put(P + 'Assets/_Project/Tests/Sim/ThingTests.cs', tests)
        return self.commit('the thing is fixed [U1] [U2]')

    def test_a_tag_above_a_test_and_a_tag_in_a_body_find_their_tests(self):
        found = F.find_tests(P + 'Assets/_Project/Tests/Sim/ThingTests.cs', CS_TEST, ['U1', 'U2'])
        self.assertEqual(found['U1'][0]['name'], 'TW.Tests.ThingTests.TheThingIsFixed')
        self.assertEqual(found['U2'][0]['name'], 'TW.Tests.ThingTests.AlwaysGreen')
        self.assertEqual(found['U1'][0]['platform'], 'EditMode')
        play = F.find_tests(P + 'Assets/_Project/Tests/PlayMode/X.cs', CS_TEST, ['U1'])
        self.assertEqual(play['U1'][0]['platform'], 'PlayMode')

    def test_a_helper_class_nested_beside_the_tests_is_not_the_tests_class(self):
        text = CS_TEST.replace('        [Test]\n        public void AlwaysGreen()',
                               '        sealed class Rig\n        {\n            public int N;\n        }\n\n'
                               '        [Test]\n        public void AlwaysGreen()')
        found = F.find_tests(P + 'Assets/_Project/Tests/Sim/ThingTests.cs', text, ['U2'])
        self.assertEqual(found['U2'][0]['name'], 'TW.Tests.ThingTests.AlwaysGreen')

    def test_red_then_green_is_proved_and_green_twice_fails(self):
        code, rec, said = self.check(self.cs_fix(), ['U1', 'U2'], lane='lane/sim/x', unity=str(self.unity))
        self.assertEqual(rec['ids']['U1']['verdict'], 'PROVED', said)
        self.assertEqual((rec['ids']['U2']['verdict'], code), ('FAIL', 1), said)

    def test_a_unity_failure_that_does_not_name_the_id_is_red(self):
        head = self.cs_fix(tests=CS_TEST.replace('// [U2] a tag', '// SAYS_NO_ID [U2] a tag'))
        code, rec, said = self.check(head, ['U1'], lane='lane/sim/x', unity=str(self.unity))
        self.assertEqual((rec['ids']['U1']['verdict'], rec['ids']['U1']['tests'][0]['old_said']),
                         ('RED', 'the thing is not fixed'), said)

    def test_a_log_something_still_holds_open_does_not_cost_the_verdict(self):
        # seen 2026-10-07 on rv-11: Unity's licensing client kept unity.log open, the temp folder could not be
        # removed, and the PermissionError came before the record was written
        head = self.cs_fix(tests=CS_TEST.replace('// [U2] a tag', '// HOLDS_LOG [U2] a tag'))
        code, rec, said = self.check(head, ['U1'], lane='lane/sim/x', unity=str(self.unity))
        self.assertEqual(rec['ids']['U1']['verdict'], 'PROVED', said)

    def test_without_a_unity_it_says_unchecked_never_pass(self):
        code, rec, said = self.check(self.cs_fix(), ['U1'], lane='lane/sim/x', unity='')
        self.assertEqual((code, rec['verdict'], rec['ids']['U1']['verdict']), (2, 'UNCHECKED', 'UNCHECKED'), said)

    def test_red_only_by_a_compile_error_is_weak_not_proved(self):
        head = self.cs_fix(source='class Thing { public static bool NewName = true; /* FIXED */ }\n',
                           tests=CS_TEST.replace('Thing.Fixed', 'Thing.NewName'))
        code, rec, said = self.check(head, ['U1'], lane='lane/sim/x', unity=str(self.unity))
        self.assertEqual((code, rec['ids']['U1']['verdict'], rec['counts']['WEAK']), (0, 'WEAK', 1), said)


if __name__ == '__main__':
    unittest.main()
