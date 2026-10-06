#!/usr/bin/env python3
"""Tests for handoffs.py: python Tools/test_handoffs.py. Stdlib only.

A folder of handoffs made here stands in for the Drive (TW_HANDOFFS). Each rule of `check` is shown to pass on a
folder that is in order and to fail on the one thing that breaks it.
"""
import contextlib, io, os, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import handoffs as H


class Folder(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.where = Path(self.tmp.name)
        self.old = os.environ.get('TW_HANDOFFS')
        os.environ['TW_HANDOFFS'] = str(self.where)
        for name in ('HANDOFF_AGENT_relay.md', 'HANDOFF_AGENT_relay_2.md', 'look/HANDOFF_AGENT_look.md'):
            self.make(name)
        self.run_ok('new', 'HANDOFF_AGENT_relay.md', '--topic', 'relay', '--for', 'the first word on the relay')
        self.run_ok('new', 'HANDOFF_AGENT_relay_2.md', '--topic', 'relay', '--for', 'the relay, a day on',
                    '--replaces', 'HANDOFF_AGENT_relay.md')
        self.run_ok('new', 'look/HANDOFF_AGENT_look.md', '--topic', 'look', '--for', 'the units\' look')

    def tearDown(self):
        if self.old is None:
            os.environ.pop('TW_HANDOFFS', None)
        else:
            os.environ['TW_HANDOFFS'] = self.old
        self.tmp.cleanup()

    def make(self, name):
        p = self.where / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text('# a handoff\n', encoding='utf-8')

    def call(self, *argv):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            try:
                code = H.main(list(argv))
            except SystemExit as e:
                code, _ = 2, out.write(str(e))
        return code, out.getvalue()

    def run_ok(self, *argv):
        code, said = self.call(*argv)
        self.assertEqual(code, 0, said)
        return said


class CheckTest(Folder):
    def test_a_folder_in_order_passes(self):
        code, said = self.call('check')
        self.assertEqual(code, 0, said)
        self.assertNotIn('MEND', said)

    def test_new_with_replaces_leaves_one_current_on_the_topic(self):
        h = H.read(self.where)['handoffs']
        self.assertEqual(h['HANDOFF_AGENT_relay.md']['state'], 'replaced')
        self.assertEqual(h['HANDOFF_AGENT_relay.md']['by'], 'HANDOFF_AGENT_relay_2.md')
        self.assertEqual(h['HANDOFF_AGENT_relay_2.md']['state'], 'current')

    def test_a_handoff_nobody_listed_fails(self):
        self.make('HANDOFF_AGENT_new_thing.md')
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('HANDOFF_AGENT_new_thing.md: not in the index', said)

    def test_a_handoff_in_a_subfolder_counts_too(self):
        self.make('deep/er/HANDOFF_AGENT_far.md')
        self.assertEqual(self.call('check')[0], 1)

    def test_a_listed_file_that_is_gone_fails(self):
        (self.where / 'look' / 'HANDOFF_AGENT_look.md').unlink()
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('the file is gone', said)

    def test_two_current_on_one_topic_fail(self):
        self.make('HANDOFF_AGENT_relay_3.md')
        self.run_ok('new', 'HANDOFF_AGENT_relay_3.md', '--topic', 'relay', '--for', 'a second current one')
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('topic "relay": 2 current handoffs', said)
        self.run_ok('replace', 'HANDOFF_AGENT_relay_2.md', 'HANDOFF_AGENT_relay_3.md')
        self.assertEqual(self.call('check')[0], 0, 'replacing the older one mends it')

    def test_replaced_by_nothing_and_by_a_stranger_fail(self):
        d = H.read(self.where)
        d['handoffs']['HANDOFF_AGENT_relay.md']['by'] = ''
        H.write(self.where, d)
        self.assertIn('replaced, and by nothing', self.call('check')[1])
        d['handoffs']['HANDOFF_AGENT_relay.md']['by'] = 'HANDOFF_AGENT_nobody.md'
        H.write(self.where, d)
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('which is not a listed handoff', said)

    def test_a_doc_in_the_repo_may_replace_a_handoff(self):
        self.run_ok('replace', 'look/HANDOFF_AGENT_look.md', 'repo:docs/reference/workflow.md')
        self.assertEqual(self.call('check')[0], 0)

    def test_an_index_edited_by_hand_fails_and_render_mends_it(self):
        index = self.where / 'INDEX.md'
        index.write_text(index.read_text(encoding='utf-8') + 'a line by hand\n', encoding='utf-8')
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('INDEX.md is not what handoffs.json says', said)
        self.run_ok('render')
        self.assertEqual(self.call('check')[0], 0)

    def test_new_refuses_a_file_that_is_not_there(self):
        code, said = self.call('new', 'HANDOFF_AGENT_ghost.md', '--topic', 'x', '--for', 'y')
        self.assertEqual(code, 2)
        self.assertIn('write the handoff first', said)


class IndexTest(Folder):
    def test_the_index_puts_current_first_and_names_the_replacement(self):
        text = (self.where / 'INDEX.md').read_text(encoding='utf-8')
        self.assertLess(text.index('## Current'), text.index('## Replaced'))
        self.assertIn('| relay | `HANDOFF_AGENT_relay.md` | `HANDOFF_AGENT_relay_2.md` |', text)
        self.assertIn('| look | `look/HANDOFF_AGENT_look.md` |', text)

    def test_done_and_park_move_a_handoff_out_of_current(self):
        self.run_ok('done', 'HANDOFF_AGENT_relay_2.md')
        self.run_ok('park', 'look/HANDOFF_AGENT_look.md')
        text = (self.where / 'INDEX.md').read_text(encoding='utf-8')
        self.assertNotIn('## Current', text)
        self.assertIn('## Parked', text)
        self.assertIn('## Done', text)

    def test_the_line_for_health_counts_and_says_what_to_mend(self):
        head, bad = H.line(self.where)
        self.assertTrue(head.startswith('2 current, 0 parked, 1 replaced or done'), head)
        self.assertEqual(bad, [])
        self.make('HANDOFF_AGENT_unlisted.md')
        head, bad = H.line(self.where)
        self.assertIn('1 to mend', head)
        self.assertEqual(len(bad), 1)

    def test_a_folder_that_is_not_there_is_said_not_passed_over(self):
        os.environ['TW_HANDOFFS'] = str(self.where / 'no-such-drive')
        code, said = self.call('check')
        self.assertEqual(code, 1)
        self.assertIn('not read', said)


if __name__ == '__main__':
    unittest.main()
