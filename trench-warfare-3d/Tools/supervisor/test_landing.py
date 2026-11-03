#!/usr/bin/env python3
"""Tests of the landing stage (modules/landing.py), on git repos made here in a temp folder, never the real one.

    python Tools/supervisor/test_landing.py

What they hold: a look or sim lane never goes on the stack; the stack is integration plus origin's lanes in risk
order, a conflicting lane skipped with its files; a local branch is never read; the gate's verdict is read from its
log and marker, never its exit code; a red or stale gate never starts the landing session; dry mode makes no
checkout, no branch and starts nothing; a landing is counted only when origin's integration holds the stack.
"""
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402
import gatelock  # noqa: E402

spec = importlib.util.spec_from_file_location('tw_sup_landing', HERE / 'modules' / 'landing.py')
L = importlib.util.module_from_spec(spec)
spec.loader.exec_module(L)

I = 'claude/trench-warfare-2d-3d-plan-idt7lf'
P = 'trench-warfare-3d/'
A = P + 'Assets/_Project/'
ENV = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t')


def git(cwd, *a):
    r = subprocess.run(['git', '-C', str(cwd), *a], capture_output=True, text=True, env=ENV)
    if r.returncode:
        raise RuntimeError(f'git {a}: {r.stdout}{r.stderr}')
    return r.stdout.strip()


def write(root, files):
    for path, text in files.items():
        f = Path(root) / path
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_text(text, encoding='utf-8', newline='\n')


def dead_pid():
    p = subprocess.Popen([sys.executable, '-c', 'pass'])
    p.wait()
    return p.pid


class Stub(L.World):
    """The real git, but processes and briefs are only written down. A started process is one that has ended."""

    def __init__(self, *a):
        super().__init__(*a)
        self.briefs = []
        self.pid = dead_pid()

    def popen(self, argv, cwd, log, env=None):
        self.started.append(argv)
        Path(log).parent.mkdir(parents=True, exist_ok=True)
        Path(log).touch()
        return self.pid

    def add_brief(self, args):
        self.briefs.append(args)
        return 0, 'brief added'


class Repos(unittest.TestCase):
    """origin (bare), a clone (the repo whose store is read), a board with done records, a home."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        t = Path(self.tmp.name)
        self.home, self.board = t / 'home', t / 'board'
        origin, self.repo, self.stage = t / 'origin.git', t / 'repo', t / 'stage'
        self.old_env = {k: os.environ.get(k) for k in ('TW_SUP_HOME', 'TW_BRIEFS', 'TW_LAND_CLAUDE', 'TW_LAND_REPO',
                                                       'TW_LAND_STAGE', 'TW_BOARD')}
        os.environ.update(TW_SUP_HOME=str(self.home), TW_BRIEFS=str(t / 'briefs'),
                          TW_LAND_CLAUDE=json.dumps([sys.executable, '-c', 'pass']), TW_LAND_REPO=str(self.repo),
                          TW_LAND_STAGE=str(self.stage), TW_BOARD=str(self.board))
        shutil.copytree(ORIGIN['path'], origin)              # made once (setUpModule), copied for each test
        self.base, self.tips = ORIGIN['base'], ORIGIN['tips']
        git(t, 'clone', '-q', str(origin), str(self.repo))
        # a local branch with a look change origin does not have: the stack must never read it
        git(self.repo, 'checkout', '-q', '-b', 'lane/show/tool-a', f'origin/lane/show/tool-a')
        write(self.repo, {A + 'Presentation/p.cs': 'LOCAL ONLY\n'})
        git(self.repo, 'commit', '-q', '-am', 'local only')
        git(self.repo, 'checkout', '-q', '--detach', f'origin/{I}')
        done = self.board / 'relay' / 'done'
        done.mkdir(parents=True)
        for lane, tip in self.tips.items():
            if lane != 'lane/show/notdone':
                (done / f'{lane.rsplit("/", 1)[-1]}.json').write_text(json.dumps(dict(id=lane, lane=lane, head=tip)), encoding='utf-8')
        (done / 'old.json').write_text(json.dumps(dict(id='old', lane='lane/show/landed-old', head=self.base)), encoding='utf-8')
        core.write_json(self.home / 'state' / 'landing_extra.json', ['lane/show/extra-doc', 'lane/show/no-such-lane'])
        self.W = Stub(self.repo, self.stage, self.board, self.home)
        self.now = time.mktime((2026, 11, 3, 10, 0, 0, 0, 0, -1))

    def tearDown(self):
        for k, v in self.old_env.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v
        subprocess.run(['git', '-C', str(self.repo), 'worktree', 'remove', '--force', str(self.stage)], capture_output=True)
        self.tmp.cleanup()

    def ctx(self, dry, now=None):
        return core.Ctx(self.home, now or self.now, dry, 'landing')

    def landing_refs(self):
        return git(self.repo, 'for-each-ref', '--format=%(refname)', 'refs/heads/lane/show/landing-')


ORIGIN = {}


def setUpModule():
    """The origin every test starts from: integration, nine lanes off it, one lane that is in integration already."""
    t = Path(tempfile.mkdtemp(prefix='tw-landing-test-'))
    ORIGIN['tmp'] = t
    origin, seed = t / 'origin.git', t / 'seed'
    subprocess.run(['git', 'init', '-q', '--bare', str(origin)], check=True)
    subprocess.run(['git', 'init', '-q', '-b', I, str(seed)], check=True)
    write(seed, {P + 'Tools/a.py': 'a = 1\n', 'docs/x.md': 'x\n', A + 'Presentation/p.cs': 'p\n',
                 A + 'Sim/s.cs': 's\n', A + 'Tests/t.cs': 't\n', A + 'Editor/e.cs': 'e\n'})
    git(seed, 'add', '-A')
    git(seed, 'commit', '-q', '-m', 'base')
    base = git(seed, 'rev-parse', 'HEAD')
    lanes = {
            'lane/show/tool-a': ({P + 'Tools/a.py': 'a = 2\n'}, '[tool-a] a is two'),
            'lane/show/tool-clash': ({P + 'Tools/a.py': 'a = 3\n'}, '[tool-clash] a is three'),
            'lane/show/docs-b': ({'docs/b.md': 'b\n', 'docs/c.md': 'c\n'}, '[docs-b] two docs'),
            'lane/show/review-fix-e': ({A + 'Editor/e.cs': 'e2\n', A + 'Tests/t.cs': 't2\n'}, '[rv-e] the editor fix'),
            'lane/show/look-p': ({A + 'Presentation/p.cs': 'p2\n'}, '[look-p] brighter'),
            'lane/sim/s': ({A + 'Sim/s.cs': 's2\n'}, '[sim-s] faster'),
            'lane/show/wip': ({P + 'Tools/w.py': 'w\n'}, 'WIP [wip] unit FAILED; leftover work'),
            'lane/show/notdone': ({P + 'Tools/n.py': 'n\n'}, '[nd] not passed'),
            'lane/show/extra-doc': ({'docs/e.md': 'e\n'}, '[extra] a doc'),
        }
    tips = {}
    for lane, (files, subject) in lanes.items():
        git(seed, 'checkout', '-q', '-b', lane, base)
        write(seed, files)
        git(seed, 'add', '-A')
        git(seed, 'commit', '-q', '-m', subject)
        tips[lane] = git(seed, 'rev-parse', 'HEAD')
    git(seed, 'branch', 'lane/show/landed-old', base)                                  # in integration already
    git(seed, 'remote', 'add', 'origin', str(origin))
    git(seed, 'push', '-q', 'origin', '--all')
    ORIGIN.update(path=origin, base=base, tips=tips)


def tearDownModule():
    shutil.rmtree(ORIGIN['tmp'], ignore_errors=True)


class Classify(unittest.TestCase):
    def test_classes(self):
        c = lambda paths, subjects=(), lane='lane/show/x': L.classify(paths, subjects, lane)[0]
        self.assertEqual(c([P + 'Tools/a.py', 'docs/x.md', 'README.md', '.claude/skills/x/SKILL.md', A + 'Tests/T.cs']), 'alone')
        self.assertEqual(c([P + 'Tools/a.py', A + 'Presentation/Hud.cs']), 'look')
        for d in ('Art', 'Shaders', 'Scenes', 'Resources', 'UI'):
            self.assertEqual(c([A + d + '/x.asset']), 'look', d)
        for d in ('Sim', 'Net', 'Data'):
            self.assertEqual(c([A + d + '/x.cs', 'docs/x.md']), 'sim', d)
        self.assertEqual(c([A + 'Tests/EditMode/SimHashTests.cs']), 'sim')
        self.assertEqual(c([A + 'Sim/x.cs', A + 'Presentation/y.cs', P + 'Tools/land.py']), 'guard')
        self.assertEqual(c([P + 'Tools/supervisor/modules/landing.py']), 'guard')
        self.assertEqual(c(['gate.ps1']), 'guard')
        self.assertEqual(c([A + 'Editor/E.cs', A + 'Tests/T.cs'], ['[rv-12-x] fix it']), 'fix')
        self.assertEqual(c([A + 'Editor/E.cs', A + 'Tests/T.cs'], ['[ux-1] new'], 'lane/show/review-fixes'), 'fix')
        self.assertEqual(c([A + 'Editor/E.cs'], ['[rv-12-x] fix it']), 'owner')        # no test
        self.assertEqual(c([A + 'Editor/E.cs', A + 'Tests/T.cs'], ['[ux-1] new']), 'owner')   # not a fix unit
        self.assertEqual(c([P + 'ProjectSettings/x.asset']), 'owner')
        self.assertEqual(c([P + 'Assets/odd.png']), 'owner')
        self.assertEqual(c([]), 'empty')


class Collect(Repos):
    def test_candidates_and_classes(self):
        base, rows = L.collect(self.W, self.home)
        self.assertEqual(base, self.base)
        got = {r['lane']: r['cls'] for r in rows}
        self.assertEqual(got, {'lane/show/tool-a': 'alone', 'lane/show/tool-clash': 'alone', 'lane/show/docs-b': 'alone',
                               'lane/show/review-fix-e': 'fix', 'lane/show/look-p': 'look', 'lane/sim/s': 'sim',
                               'lane/show/wip': 'wip', 'lane/show/extra-doc': 'alone'})
        self.assertEqual([r['lane'] for r in L.eligible(rows)],
                         ['lane/show/extra-doc', 'lane/show/tool-a', 'lane/show/tool-clash', 'lane/show/docs-b',
                          'lane/show/review-fix-e'])

    def test_a_done_record_whose_head_is_not_on_the_lane_does_not_count(self):
        rec = self.board / 'relay' / 'done' / 'docs-b.json'
        rec.write_text(json.dumps(dict(id='x', lane='lane/show/docs-b', head=self.tips['lane/show/tool-a'])), encoding='utf-8')
        _, rows = L.collect(self.W, self.home)
        self.assertNotIn('lane/show/docs-b', [r['lane'] for r in rows])


class Stack(Repos):
    def test_order_conflict_skip_and_origin_only(self):
        base, rows = L.collect(self.W, self.home)
        self.W.make_stage(base)
        tip, stacked, skipped = L.stack(self.W, base, L.eligible(rows))
        self.assertEqual(stacked, ['lane/show/extra-doc', 'lane/show/tool-a', 'lane/show/docs-b', 'lane/show/review-fix-e'])
        self.assertEqual([(s['lane'], s['why'], s['files']) for s in skipped],
                         [('lane/show/tool-clash', 'conflict', [P + 'Tools/a.py'])])
        files = git(self.repo, 'diff', '--name-only', base, tip).split()
        self.assertNotIn(A + 'Presentation/p.cs', files)       # the local-only commit and the look lane are not on it
        self.assertNotIn(A + 'Sim/s.cs', files)
        self.assertEqual(git(self.repo, 'show', f'{tip}:{P}Tools/a.py'), 'a = 2')
        self.assertTrue(L.World.is_anc(self.W, base, tip))
        # the forecast (dry) says the same without a checkout
        self.assertEqual(L.forecast(self.W, base, L.eligible(rows)), (stacked, skipped))

    def test_the_stack_takes_at_most_its_limit(self):
        base, rows = L.collect(self.W, self.home)
        stacked, _ = L.forecast(self.W, base, L.eligible(rows), most=2)
        self.assertEqual(stacked, ['lane/show/extra-doc', 'lane/show/tool-a'])


class Verdict(unittest.TestCase):
    T = 'abcdef1234' + '0' * 30

    def test_reading_the_gate_log(self):
        v = L.verdict
        green = 'lots\nGate green. Tested tree abcdef1234: commit exactly this and Tools/land.py will land it.\n'
        self.assertEqual(v(green, self.T, False, [self.T]), 'green')
        self.assertTrue(v(green, '1111111111' + '0' * 30, False, [self.T]).startswith('red'))          # another tree
        self.assertTrue(v(green, self.T, False, ['9' * 40]).startswith('red'))                          # marker disagrees
        self.assertTrue(v(green, self.T, False, []).startswith('red'))
        stand_in = 'Gate green, but a stand-in Unity ran it (TW_GATE_UNITY)'
        self.assertTrue(v(stand_in, self.T, False, [self.T]).startswith('red'))
        self.assertTrue(v('8 tests failed\nexit 0', self.T, False, [self.T]).startswith('red'))       # exit 0 is not green
        self.assertEqual(v('compiling', self.T, True, [self.T], started=time.time()), 'running')
        self.assertTrue(v('compiling', self.T, True, [self.T], started=time.time() - L.GATE_MOST - 1).startswith('red'))


class Turns(Repos):
    def test_dry_makes_nothing_and_starts_nothing(self):
        lines = core.turn(L, self.home, self.now)
        self.assertTrue(lines and all(l.startswith('[dry]') for l in lines), lines)
        self.assertFalse(self.stage.exists())
        self.assertEqual(self.landing_refs(), '')
        self.assertEqual(git(self.repo, 'worktree', 'list').count('\n'), 0)
        st = core.read_state('landing', self.home)
        d = st['days']['2026-11-03']
        self.assertEqual(d['stacked'], ['lane/show/extra-doc', 'lane/show/tool-a', 'lane/show/docs-b', 'lane/show/review-fix-e'])
        self.assertIn('would', d)
        self.assertIn('brief_would', d)
        self.assertEqual(st.get('phase', 'idle'), 'idle')
        self.assertFalse((self.home / 'gate.lock').exists())
        self.assertFalse((self.home / 'landing').exists())
        self.assertFalse((self.home / 'landed.json').exists())

    def test_dry_with_a_green_gate_in_its_state_does_not_start_a_session(self):
        st = dict(phase='gating', gate=dict(pid=dead_pid(), log=str(self.home / 'g.log'), started=self.now, sha='x', tree='t' * 40,
                                            base=self.base, branch='lane/show/landing-20261103', lanes=['lane/show/tool-a']))
        core.write_state('landing', st, self.home)
        (self.home / 'g.log').write_text('Gate green. Tested tree tttttttttt', encoding='utf-8')
        self.W.marker = lambda: ['t' * 40]
        L.step(self.ctx(True), self.W)
        self.assertEqual(self.W.started, [])

    def on(self):
        L.step(self.ctx(False), self.W)
        return core.read_state('landing', self.home)

    def test_on_stacks_gates_and_lands_only_on_a_green_gate_of_this_tree(self):
        st = self.on()
        self.assertEqual(st['phase'], 'gating')
        self.assertEqual(self.W.started, [L.gate_cmd(self.stage)])
        g = st['gate']
        self.assertEqual(git(self.stage, 'rev-parse', '--abbrev-ref', 'HEAD'), 'lane/show/landing-20261103')
        self.assertEqual(git(self.stage, 'rev-parse', 'HEAD'), g['sha'])
        self.assertEqual(gatelock.read()['who'], 'landing')
        self.assertEqual(len(self.W.briefs), 1)                                   # the look and the sim lane, in one brief
        self.assertIn('2 finished lanes', ' '.join(self.W.briefs[0]))
        # the gate ends with a green line for this tree, and its marker
        Path(g['log']).write_text(f'Gate green. Tested tree {g["tree"][:10]}: commit exactly this', encoding='utf-8')
        marker = git(self.stage, 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green')
        Path(marker).write_text(g['tree'] + ' full 2026-11-03', encoding='utf-8')
        st = self.on()
        self.assertEqual(st['phase'], 'landing')
        self.assertEqual(len(self.W.started), 2)
        self.assertIn('--max-turns', self.W.started[1])
        prompt = Path(self.W.started[1][self.W.started[1].index('-p') + 1].split('Read the file ')[1].split(' and do')[0])
        self.assertIn(g['sha'], prompt.read_text(encoding='utf-8'))
        # not landed: origin's integration did not move
        st = self.on()
        self.assertEqual(st['phase'], 'idle')
        self.assertNotIn('landed', st['days']['2026-11-03'])
        self.assertFalse((self.home / 'landed.json').exists())
        self.assertIsNone(gatelock.read())

    def test_landed_is_read_from_origin(self):
        st = self.on()
        g = st['gate']
        Path(g['log']).write_text(f'Gate green. Tested tree {g["tree"][:10]}', encoding='utf-8')
        Path(git(self.stage, 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green')).write_text(g['tree'], encoding='utf-8')
        self.on()
        git(self.stage, 'push', '-q', 'origin', f'{g["sha"]}:refs/heads/{I}')       # what land.py would have done
        st = self.on()
        self.assertEqual(st['days']['2026-11-03']['landed'], g['sha'])
        m = json.loads((self.home / 'landed.json').read_text(encoding='utf-8'))
        self.assertEqual(m['weeks'], {'2026-W45': 1})

    def gate_then(self, log_text, alive=False, started_ago=0, marker=True):
        st = self.on()
        g = st['gate']
        if started_ago:
            g['started'] = self.now - started_ago
            core.write_state('landing', st, self.home)
        if alive:
            self.W.alive = lambda pid: True
        Path(g['log']).write_text(log_text.replace('TREE', g['tree'][:10]), encoding='utf-8')
        if marker:
            Path(git(self.stage, 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green')).write_text(g['tree'], encoding='utf-8')
        return g, self.on()

    def test_a_red_gate_never_starts_the_session_and_halves_the_stack(self):
        g, st = self.gate_then('8 tests failed\nGate red')
        self.assertEqual(st['phase'], 'idle')
        self.assertEqual(len(self.W.started), 1)
        self.assertEqual(st['days']['2026-11-03']['limit'], len(g['lanes']) // 2)
        self.assertIsNone(gatelock.read())

    def test_a_stale_gate_never_starts_the_session(self):
        g, st = self.gate_then('still compiling', alive=True, started_ago=L.GATE_MOST + 60)
        self.assertEqual(st['phase'], 'idle')
        self.assertEqual(len(self.W.started), 1)

    def test_green_on_another_tree_or_without_marker_never_starts_the_session(self):
        _, st = self.gate_then('Gate green. Tested tree 0123456789')
        self.assertEqual(len(self.W.started), 1)
        self.assertEqual(st['phase'], 'idle')

    def test_green_but_integration_moved_is_not_landed(self):
        st = self.on()
        g = st['gate']
        Path(g['log']).write_text(f'Gate green. Tested tree {g["tree"][:10]}', encoding='utf-8')
        Path(git(self.stage, 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green')).write_text(g['tree'], encoding='utf-8')
        git(self.stage, 'push', '-q', 'origin', f'{self.tips["lane/show/look-p"]}:refs/heads/{I}')   # someone landed
        st = self.on()
        self.assertEqual(len(self.W.started), 1)
        self.assertEqual(st['phase'], 'idle')

    def test_a_held_gate_lock_makes_it_wait(self):
        gatelock.acquire('canary', pid=os.getpid())
        st = self.on()
        self.assertEqual(self.W.started, [])
        self.assertEqual(st.get('phase', 'idle'), 'idle')
        self.assertFalse(self.stage.exists())


if __name__ == '__main__':
    unittest.main(verbosity=2)
