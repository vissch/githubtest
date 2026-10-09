#!/usr/bin/env python3
"""Tests for what the tools know of AGENTS.md: python Tools/test_agents_md.py. Stdlib only, no Unity, no Assets/.

AGENTS.md at the repo root is what an agent of another vendor reads first (Codex reads it alone, Grok Build beside
CLAUDE.md). It points at CLAUDE.md and copies none of it. Two tools know the name: gate_scope.py counts it as a doc,
and codemap.py caps its length and checks what it cites. Each rule is shown to pass on a file in order and to fail on
the one thing that breaks it. The last class holds the file in the repo to the same rules, and to naming every agent
in .claude/agents/.
"""
import sys, tempfile, unittest
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import codemap as C
import gate_scope as G

MODS = {m: ('TW.Tests.' + m, G.P + 'Tests/' + m + '/') for m in ('EditMode', 'Match', 'Project', 'Show', 'Sim', 'Stills', 'UI')}


class ScopeTest(unittest.TestCase):
    def skipped(self, *changed):
        return sorted(G.scope(list(changed), MODS)[1])

    def test_agents_md_is_a_doc_and_skips_the_slow_modules_as_claude_md_does(self):
        self.assertEqual(self.skipped('AGENTS.md'), ['Match', 'Sim'])
        self.assertEqual(self.skipped('AGENTS.md'), self.skipped('CLAUDE.md'))

    def test_a_file_the_rule_has_never_heard_of_beside_it_still_runs_everything(self):
        self.assertEqual(self.skipped('AGENTS.txt'), [])
        self.assertEqual(self.skipped('AGENTS.md', G.P + 'Sim/Core/SimWorld.cs'), [])


class CapTest(unittest.TestCase):
    """check_caps on three files made here, so the length of the repo's own pages decides nothing."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        where = Path(self.tmp.name)
        self.patch = mock.patch.multiple(C, AGENTS=where / 'AGENTS.md', CLAUDE=where / 'CLAUDE.md',
                                         MEMORY=where / 'agent-memory.md')
        self.patch.start()

    def tearDown(self):
        self.patch.stop()
        self.tmp.cleanup()

    def caps(self, **lines):
        for name, n in lines.items():
            getattr(C, name).write_text('a line\n' * n, encoding='utf-8')
        errors = []
        C.check_caps(errors)
        return errors

    def test_agents_md_at_its_cap_passes(self):
        self.assertEqual(self.caps(AGENTS=C.AGENTS_MAX_LINES), [])

    def test_agents_md_one_line_over_its_cap_fails(self):
        errors = self.caps(AGENTS=C.AGENTS_MAX_LINES + 1)
        self.assertEqual(len(errors), 1, errors)
        self.assertIn(f'AGENTS.md is {C.AGENTS_MAX_LINES + 1} lines (cap {C.AGENTS_MAX_LINES})', errors[0])

    def test_its_cap_is_its_own_and_small(self):
        self.assertLessEqual(C.AGENTS_MAX_LINES, 40)       # a pointer: at 110 lines it would be a second CLAUDE.md
        errors = self.caps(AGENTS=C.AGENTS_MAX_LINES + 1, CLAUDE=C.AGENTS_MAX_LINES + 1)
        self.assertEqual(len(errors), 1, errors)           # the same length passes as CLAUDE.md

    def test_claude_md_and_the_memory_log_keep_their_caps(self):
        errors = self.caps(CLAUDE=C.CLAUDE_MAX_LINES + 1, MEMORY=C.MEMORY_MAX_LINES + 1)
        self.assertEqual(len(errors), 2, errors)
        self.assertTrue(any(e.startswith(f'CLAUDE.md is {C.CLAUDE_MAX_LINES + 1} lines') for e in errors), errors)
        self.assertTrue(any(e.startswith('agent-memory.md is') for e in errors), errors)

    def test_all_three_at_their_caps_pass(self):
        self.assertEqual(self.caps(AGENTS=C.AGENTS_MAX_LINES, CLAUDE=C.CLAUDE_MAX_LINES, MEMORY=C.MEMORY_MAX_LINES), [])

    def test_a_tree_with_no_agents_md_passes(self):
        self.assertEqual(self.caps(CLAUDE=10), [])


class RepoFileTest(unittest.TestCase):
    """The AGENTS.md in this repo. Only its own lines are read out of the citations check, so a tree that holds no
    Assets/ (and fails that check on every other page) can still run this."""

    def cited(self, extra=''):
        errors, read = [], C.read
        with mock.patch.object(C, 'read', lambda p: read(p) + extra if p == C.AGENTS else read(p)):
            C.check_citations(errors)
        return [e for e in errors if e.startswith('AGENTS.md:')]

    def test_it_is_there_and_under_its_cap(self):
        self.assertTrue(C.AGENTS.is_file(), 'AGENTS.md is gone from the repo root')
        n = len(C.read(C.AGENTS).rstrip('\n').split('\n'))
        self.assertLessEqual(n, C.AGENTS_MAX_LINES)

    def test_every_file_it_cites_exists(self):
        self.assertEqual(self.cited(), [])

    def test_a_cited_file_that_is_not_there_is_caught(self):
        errors = self.cited('\nSee `docs/reference/no-such-page.md`.\n')
        self.assertEqual(len(errors), 1, errors)
        self.assertIn('cites `docs/reference/no-such-page.md`, which does not exist', errors[0])

    def test_a_command_that_runs_a_missing_tool_is_caught(self):
        errors = self.cited('\nRun `python Tools/no_such_tool.py --now`.\n')
        self.assertEqual(len(errors), 1, errors)
        self.assertIn('runs `Tools/no_such_tool.py`, which does not exist', errors[0])

    def test_it_points_at_claude_md_and_names_every_agent(self):
        text = C.read(C.AGENTS)
        self.assertIn('`CLAUDE.md`', text)
        self.assertIn('`.claude/skills/', text)
        for agent in sorted((C.REPO / '.claude' / 'agents').glob('*.md')):
            self.assertIn(f'`{agent.stem}`', text, f'AGENTS.md does not name the agent {agent.stem} (.claude/agents/)')


if __name__ == '__main__':
    unittest.main()
