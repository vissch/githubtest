#!/usr/bin/env python3
"""The landing stage: finished lanes reach the game by themselves, the ones the owner must see go to him.

WHY. The relays passed 236 units in two weeks of 2026-10, and the integration branch did not move from 10-24 to
11-02: landing happened only when a session did it by hand. The owner, 2026-11-02 (docs/reference/decisions.md):
the landing stage lands fixes, tests, tools and docs by itself after a second read; looks and play wait on him
(as he said on 10-10 and 10-17). Plan: PLAN_relay_harness_v2_2026-11-02.md on the Drive, "Phase 4".

A state machine, one step a turn (EVERY_S), in a checkout of its own (githubtest-land-stage, detached, made from
githubtest-autoloop's repo when missing):

  collect   origin lanes ahead of integration that hold the head of a unit the relay passed (tw3d-board/relay/done)
            or that <home>/state/landing_extra.json lists; each is classed by what it changes against integration
            (classify): alone (tools, docs, tests, skills) and fix (code outside look and sim, from a review or fix
            unit, with a test) land by themselves; look, sim, guard (what checks a landing) and owner (anything
            else) do not. SIM is the owner's for now: no test pins a played battle's hash yet, so a green gate does
            not show a battle plays out the same. A lane with a WIP commit or a merge commit is left out.
  stack     lane/show/landing-<yyyymmdd> = origin's integration + the eligible lanes as origin has them, rebased one
            by one, smallest risk first; a lane that conflicts is skipped with its files. Local branches are never read.
  gate      the full gate.ps1 (no arguments) under gatelock.py, detached, with a log. The verdict is the log's line
            "Gate green. Tested tree <tree>" for the stacked commit's tree AND the gate's marker naming that tree,
            never the exit code. Red: the next stack takes half the lanes; one red lane waits until its tip moves.
  land      ON only: one headless session (claude -p, a fixed prompt) runs the tw-reviewer second read in the
            foreground on the stacked commits and, on PASS, Tools/land.py unchanged. Landed is read from origin
            (integration holds the stacked commit), not from what the session says.
  brief     once a day, the lanes that wait on the owner go to him in one brief (briefs.py), with a list file.

In dry mode (the default) it fetches, collects, classes and forecasts the stack with `git merge-tree` (no branch, no
checkout, no process), and keeps in its state what it would run.
"""
import datetime
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import core  # noqa: E402
import gatelock  # noqa: E402

NAME = 'landing'
EVERY_S = 900
DEFAULT_MODE = 'dry'

INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
GH = Path(os.environ.get('TW_LAND_GH') or 'C:/Users/PC/Documents/GitHub')
P = 'trench-warfare-3d/'
A = P + 'Assets/_Project/'
STACK_MOST = 8              # lanes in one stack: a red gate on more says too little about which one it was
TRY_MOST = 24               # lanes tried for one stack (conflicts skipped) before it is built with what it has
LANDINGS_A_DAY = 3          # landing sessions a day: each is a second read on Opus
GATE_MOST = 3 * 3600        # a gate with no verdict after this is taken as red
SESSION_MOST = 2 * 3600     # a landing session past this is said overdue; it is waited for, never stopped
PLAN_AGAIN = 3 * 3600       # dry: the forecast is made again after this
SKIP = ('lane/show/found',)  # the lane found.py writes old-work looks on: never landed
WHO = 'landing'

# what checks a landing, or how a session may act: a lane that changes these never lands by itself. The landing
# session runs in the stacked checkout and reads its CLAUDE.md, agents and review skills, so a lane that changed
# them would rewrite the second read that judges it (review 2026-11-03, L2).
GUARD = ('gate.ps1', P + 'gate.ps1', P + 'Tools/land.py', P + 'Tools/gate', P + 'Tools/toolcheck.py',
         P + 'Tools/selftest.py', P + 'Tools/checks/', P + 'validate.py', P + 'Tools/supervisor/', '.claude/settings',
         '.claude/hooks', '.claude/agents/', '.claude/skills/tw-master/', '.claude/skills/tw-review/', '.github/',
         'CLAUDE.md', P + 'CLAUDE.md')
# the owner's words and the seam's documents (CLAUDE.md, "The seam"): his, whatever else the lane is
OWNER_DOCS = ('docs/reference/decisions.md', 'docs/02-contracts.md', 'docs/03-determinism-rules.md')
LOOK = tuple(A + d + '/' for d in ('Presentation', 'Art', 'Shaders', 'Scenes', 'Resources', 'UI', 'Settings'))
SIM = (A + 'Sim/', A + 'Net/', A + 'Data/')
SEAM = (P + 'ProjectSettings/', P + 'Packages/')
ALONE_CLAUDE = ('.claude/skills/', '.claude/agents/')
PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp')
ORDER = ('alone', 'fix')    # the classes that land by themselves, the safer first
OWNERS = ('look', 'sim', 'guard', 'owner')
FIX_TAG = re.compile(r'(review|fix|rv-|\bp1\b)', re.I)


# ---- classes ---------------------------------------------------------------------------------------------------------

def kind(p):
    """One path's kind: guard, sim, look, owner (seam or unknown), code, test or alone."""
    if p.startswith(GUARD):
        return 'guard'
    if p.startswith(SIM):
        return 'sim'
    if p.startswith(A + 'Tests/'):
        if 'SimHashTests' in p:
            return 'sim'
        return 'test' if p.endswith(('.cs', '.cs.meta', '.asmdef', '.asmdef.meta')) else 'owner'
    if p.startswith(LOOK):
        return 'look'
    if p.startswith(SEAM) or p in OWNER_DOCS:
        return 'owner'
    if p.startswith('docs/') or p.startswith(P + 'docs/') or p.startswith(P + 'Tools/') or p.startswith(ALONE_CLAUDE):
        return 'alone'
    if p.endswith('.md') and not p.startswith((P + 'Assets/', '.claude/')):
        return 'alone'
    if p.startswith(A) and p.endswith(('.cs', '.cs.meta', '.asmdef', '.asmdef.meta')):
        return 'code'
    return 'owner'


def classify(paths, subjects=(), lane=''):
    """(class, why) of a lane from the paths it changes against integration, most cautious first."""
    if not paths:
        return 'empty', 'it changes nothing against integration'
    kinds = {}
    for p in paths:
        kinds.setdefault(kind(p), p)
    for k, why in (('guard', 'it changes what checks a landing'), ('sim', 'it changes the sim, the net code or the data, '
                   'and no test pins a played battle yet'), ('look', 'it changes what the game looks like or how it plays'),
                   ('owner', 'it changes a seam file or a path the stage does not know')):
        if k in kinds:
            return k, f'{why} ({kinds[k]})'
    if 'code' in kinds:
        tagged = bool(FIX_TAG.search(lane)) or any(FIX_TAG.search(m.group(1)) for s in subjects
                                                   for m in [re.match(r'\s*\[([^\]]+)\]', s)] if m)
        if tagged and 'test' in kinds:
            return 'fix', f'code from a review or fix unit, with a test ({kinds["code"]})'
        return 'owner', ('code with no test in the lane' if tagged else 'code not from a review or fix unit') + f' ({kinds["code"]})'
    return 'alone', 'tools, docs, tests or skills only'


def plain_title(subjects):
    """The lane in plain words: its first commit's subject without the [unit] tag."""
    s = subjects[-1] if subjects else ''
    s = re.sub(r'^\s*(WIP\s+)?\[[^\]]*\]\s*', '', s)
    return ' '.join(s.split()[:14]) or '(no subject)'


# ---- git and processes: a World, so the tests can stand one up in a temp folder --------------------------------------

class World:
    """`repo` is the checkout whose object store is read (githubtest-autoloop: a worktree add from it makes the stage),
    `stage` the landing stage's own checkout, `board` the pipeline's board (relay/done)."""

    def __init__(self, repo, stage, board, home):
        self.repo, self.stage, self.board, self.home = Path(repo), Path(stage), Path(board), Path(home)
        self.started = []

    @classmethod
    def default(cls, home):
        return cls(Path(os.environ.get('TW_LAND_REPO') or GH / 'githubtest-autoloop'),
                   Path(os.environ.get('TW_LAND_STAGE') or GH / 'githubtest-land-stage'),
                   Path(os.environ.get('TW_BOARD') or GH / 'tw3d-board'), home)

    def git(self, *a, cwd=None, env=None):
        r = subprocess.run(['git', '-C', str(cwd or self.repo), *a], capture_output=True, stdin=subprocess.DEVNULL,
                           env=env)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def out(self, *a, cwd=None):
        code, o = self.git(*a, cwd=cwd)
        return o if code == 0 else ''

    def sg(self, *a):
        return self.git(*a, cwd=self.stage)

    def fetch(self):
        # --prune: a lane the owner deleted on origin must not live on as a remote-tracking ref and land (review L1)
        return self.git('fetch', '-q', '--prune', 'origin')

    def base(self):
        return self.out('rev-parse', '-q', '--verify', f'refs/remotes/origin/{INTEGRATION}^{{commit}}')

    def origin_lanes(self):
        """{lane: tip} of origin's lane/show/* and lane/sim/*, from the remote-tracking refs only."""
        out = {}
        for line in self.out('for-each-ref', '--format=%(objectname) %(refname)', 'refs/remotes/origin/lane/').splitlines():
            sha, _, ref = line.partition(' ')
            lane = ref[len('refs/remotes/origin/'):]
            if lane.startswith(('lane/show/', 'lane/sim/')):
                out[lane] = sha
        return out

    def is_anc(self, a, b):
        return self.git('merge-base', '--is-ancestor', a, b)[0] == 0

    def changed(self, base, tip):
        o = self.out('diff', '-z', '--name-only', '--no-renames', f'{base}...{tip}')
        return [p for p in o.split('\0') if p]

    def subjects(self, base, tip):
        return [l for l in self.out('log', '--format=%s', f'{base}..{tip}').splitlines() if l.strip()]

    def merges(self, base, tip):
        return int(self.out('rev-list', '--merges', '--count', f'{base}..{tip}') or 0)

    def tree(self, sha, cwd=None):
        return self.out('rev-parse', f'{sha}^{{tree}}', cwd=cwd)

    def stage_ok(self):
        return (self.stage / '.git').exists()

    def make_stage(self, base):
        return self.git('worktree', 'add', '--detach', str(self.stage), base)

    def marker(self):
        """What the stage's last green full gate recorded (gate.ps1 writes tw-gate-green in the checkout's git dir)."""
        p = self.out('rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green', cwd=self.stage)
        try:
            return Path(p).read_text(encoding='utf-8').split() if p else []
        except OSError:
            return []

    def popen(self, argv, cwd, log, env=None):
        """Start a process that outlives this turn, its output in `log`. Returns its pid."""
        Path(log).parent.mkdir(parents=True, exist_ok=True)
        flags = 0x00000008 | 0x00000200 if os.name == 'nt' else 0      # DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP
        with open(log, 'ab') as f:
            p = subprocess.Popen(argv, cwd=str(cwd), stdout=f, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL,
                                 env=env, creationflags=flags, close_fds=True)
        self.started.append(argv)
        return p.pid

    def alive(self, pid):
        return gatelock.pid_alive(pid)

    def add_brief(self, args):
        """briefs.py add, from this checkout (not the stage's): (exit code, what it said)."""
        p = subprocess.run([sys.executable, str(HERE.parents[1] / 'assetboard' / 'briefs.py'), 'add', *args],
                           cwd=str(HERE.parents[2]), capture_output=True, stdin=subprocess.DEVNULL)
        return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace').strip()


# ---- collect -----------------------------------------------------------------------------------------------------------

def done_records(board):
    out = {}
    d = Path(board) / 'relay' / 'done'
    for f in sorted(d.glob('*.json')) if d.is_dir() else []:
        rec = core.read_json(f, {}) or {}
        if rec.get('lane') and rec.get('head'):
            out.setdefault(rec['lane'], []).append(rec)
    return out


def collect(W, home, red=None, held=None):
    """Every lane that could land, with its class: [{lane, tip, cls, why, files, n, title, pictures, from}].
    `held` is the critic gate's state (modules/critic_gate.py, its 'held': {unit id: {score, ...}}): a lane whose
    passed units on it are all held there does not land. No such state: nothing is held."""
    base = W.base()
    lanes = W.origin_lanes()
    done = done_records(W.board)
    extra = core.read_json(Path(home) / 'state' / 'landing_extra.json', []) or []
    extra = [e for e in extra if isinstance(e, str)] if isinstance(extra, list) else []
    red = red or {}
    rows = []
    for lane in sorted(set(done) | set(extra)):
        if not lane.startswith(('lane/show/', 'lane/sim/')) or lane in SKIP or lane.startswith('lane/show/landing-'):
            continue
        tip = lanes.get(lane)
        if not tip or W.is_anc(tip, base):
            continue                                     # gone from origin, or landed already
        units = [r for r in done.get(lane, []) if W.is_anc(r['head'], tip)]
        if lane in done and not units:
            continue                                     # no passed unit's work is on the lane as origin has it
        files = W.changed(base, tip)
        subjects = W.subjects(base, tip)
        cls, why = classify(files, subjects, lane)
        if any(s.startswith('WIP') or 'unit FAILED' in s for s in subjects):
            cls, why = 'wip', 'it holds a WIP commit of a failed unit'
        elif W.merges(base, tip):
            cls, why = 'merges', 'it holds a merge commit; integration takes none'
        elif red.get(lane) == tip:
            cls, why = 'red', 'the gate was red on it alone at this tip'
        elif units and held and all(str(r.get('id')) in held for r in units):
            scores = [h.get('score', '?') if isinstance(h, dict) else '?' for h in (held[str(r.get('id'))] for r in units)]
            cls, why = 'held', 'held by the critic, score ' + ', '.join(str(s) for s in scores)
        rows.append(dict(lane=lane, tip=tip, cls=cls, why=why, n=len(files), title=plain_title(subjects),
                         pictures=[f for f in files if f.lower().endswith(PICTURES)][:3],
                         came='extra' if lane in extra and lane not in done else 'relay'))
    return base, rows


def eligible(rows):
    """The lanes that may land by themselves, safest first: tools and docs before fixes, then the smallest."""
    ok = [r for r in rows if r['cls'] in ORDER]
    return sorted(ok, key=lambda r: (ORDER.index(r['cls']), r['n'], r['lane']))


# ---- stack -------------------------------------------------------------------------------------------------------------

def forecast(W, base, lanes, most=STACK_MOST):
    """What the stack would be, with no branch and no checkout: a merge-tree of each lane on what is stacked so far
    (the trees and the throwaway commits it writes are loose objects no ref names)."""
    env = dict(os.environ, GIT_AUTHOR_NAME='landing stage', GIT_AUTHOR_EMAIL='landing@stage', GIT_COMMITTER_NAME='landing stage',
               GIT_COMMITTER_EMAIL='landing@stage')
    tip, stacked, skipped = base, [], []
    for r in lanes[:TRY_MOST]:
        if len(stacked) >= most:
            break
        mb = W.out('merge-base', base, r['tip'])
        code, o = W.git('merge-tree', '--write-tree', '--name-only', '--no-messages', f'--merge-base={mb}', tip, r['tip'])
        lines = o.splitlines()
        if code == 1:
            skipped.append(dict(lane=r['lane'], why='conflict', files=[l for l in lines[1:] if l.strip()][:20]))
            continue
        if code != 0 or not lines:
            skipped.append(dict(lane=r['lane'], why='merge-tree failed: ' + o[-200:], files=[]))
            continue
        if lines[0] == W.tree(tip):
            skipped.append(dict(lane=r['lane'], why='nothing new on the stack', files=[]))
            continue
        code, c = W.git('commit-tree', lines[0], '-p', tip, '-m', 'landing forecast', env=env)
        if code:
            skipped.append(dict(lane=r['lane'], why='commit-tree failed: ' + c[-200:], files=[]))
            continue
        tip = c
        stacked.append(r['lane'])
    return stacked, skipped


def stack(W, base, lanes, most=STACK_MOST):
    """In the stage checkout, detached: integration's tip, then each lane's origin tip rebased onto what is stacked.
    Returns (tip, stacked, skipped). A conflict is aborted and recorded with its files."""
    W.sg('rebase', '--abort')
    code, o = W.sg('checkout', '-q', '--detach', base)
    if code:
        return '', [], [dict(lane='-', why='checkout of integration failed: ' + o[-200:], files=[])]
    tip, stacked, skipped = base, [], []
    for r in lanes[:TRY_MOST]:
        if len(stacked) >= most:
            break
        W.sg('checkout', '-q', '--detach', r['tip'])
        code, o = W.sg('rebase', '-q', tip)
        if code:
            files = [l for l in W.git('diff', '--name-only', '--diff-filter=U', cwd=W.stage)[1].splitlines() if l.strip()]
            W.sg('rebase', '--abort')
            skipped.append(dict(lane=r['lane'], why='conflict', files=files[:20]))
            W.sg('checkout', '-q', '--detach', tip)
            continue
        new = W.out('rev-parse', 'HEAD', cwd=W.stage)
        if new == tip:
            skipped.append(dict(lane=r['lane'], why='nothing new on the stack', files=[]))
            continue
        tip = new
        stacked.append(r['lane'])
    W.sg('checkout', '-q', '--detach', tip)
    return tip, stacked, skipped


# ---- the gate ----------------------------------------------------------------------------------------------------------

GREEN = re.compile(r'Gate green\. Tested tree ([0-9a-f]{7,40})')


def verdict(text, tree, alive, marker=(), started=0.0, now=None):
    """'green', 'red: why' or 'running'. Green only when the log names this tree and the marker holds it."""
    m = GREEN.search(text or '')
    if m:
        if not tree.startswith(m.group(1)):
            return f'red: the gate tested tree {m.group(1)}, the stack is {tree[:10]}'
        if not marker or marker[0] != tree:
            return 'red: the gate marker does not name the stacked tree'
        return 'green'
    if 'Gate green' in (text or ''):
        return 'red: the gate went green with no tree recorded (a stand-in, or files changed during the run)'
    if alive and (now or time.time()) - started < GATE_MOST:
        return 'running'
    if alive:
        return f'red: no verdict after {GATE_MOST // 3600} hours'
    return 'red: the gate ended without "Gate green"'


def gate_cmd(stage):
    return ['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', str(Path(stage) / 'gate.ps1')]


# ---- the landing session -----------------------------------------------------------------------------------------------

PROMPT = """You are the landing stage of Trench Warfare 3D: the second reader and the lander of one stacked branch.
The owner decided on 2026-11-02 that fixes, tests, tools and docs land by themselves after a second read.

Checkout: {stage}
Branch:   {branch} at {sha}
Base:     origin/{integration} at {base}
Lanes stacked (each was rebased onto the one before, risk order): {lanes}
Gate:     the full gate.ps1 went green on tree {tree} in this checkout.
Result:   write {result}

Rules: do not edit, commit, rebase, reset, push or delete anything; do not run gate.ps1 or toolcheck.py by hand; do
not pass --dry-run, --carry-sim or any other flag to land.py; run land.py at most once. Do not start background tasks.

1. Load the skill tw-master.
2. In the checkout, check `git rev-parse HEAD` is {sha} and `git rev-parse --abbrev-ref HEAD` is {branch}, and
   `git status --porcelain` is empty. If not: write the result with verdict FAIL and stop.
3. Run the agent tw-reviewer IN THE FOREGROUND (never in the background) as the second reader of the commits
   {base}..{sha} in {stage}: the rules are the repo's CLAUDE.md and that these lanes may land by themselves only
   if they change tools, docs, tests or skills, or are review fixes with tests. Ask it to look hardest at: a change
   to how a battle plays or looks, a change that weakens a check, tests or tools that can no longer fail.
   PASS when it finds nothing that would break the game, the tools or the repo's rules; else FAIL.
4. On PASS only: `cd {stage}/trench-warfare-3d` and run `python Tools/land.py`. Keep its last lines.
5. Write {result} as JSON: {{"verdict": "PASS" or "FAIL", "ran_land": true or false, "land_exit": its exit code or
   null, "said": "at most 60 words", "findings": ["the worst findings, one line each"]}}. Then stop.
"""


def claude_cmd():
    stub = os.environ.get('TW_LAND_CLAUDE')
    if stub:
        return json.loads(stub)
    exe = shutil.which('claude')
    return [exe] if exe else ['claude']


def session_argv(prompt_file, name):
    # the relay legs' safe flags (Tools/relay/launch.py argv), plus a turn limit and a budget for one session
    return claude_cmd() + ['-p', f'Read the file {prompt_file} and do what it says.', '--output-format', 'stream-json',
                           '--verbose', '--name', name, '--model', 'opus', '--permission-mode', 'auto',
                           '--permission-prompts', 'none', '--disallowedTools', 'AskUserQuestion', '--strict-mcp-config',
                           '--max-turns', '60', '--max-budget-usd', '10',
                           '--tools', 'Bash,Read,Grep,Glob,Agent,Skill,Write']


def session_env():
    env = dict(os.environ, CLAUDE_CODE_DISABLE_BACKGROUND_TASKS='1', BASH_DEFAULT_TIMEOUT_MS='600000',
               BASH_MAX_TIMEOUT_MS='600000', PYTHONUTF8='1')
    for k in ('TW_RELAY', 'TW_RELAY_LEG'):
        env.pop(k, None)
    return env


# ---- the owner's brief -------------------------------------------------------------------------------------------------

def fold_chains(rows, is_anc=None):
    """The owner's rows with every chain folded into its tip: a lane whose name another waiting lane's name extends
    (ability-fx-captures-r2-2 -> ...-r2-2-g1 -> ...-r2-2-g1-g2) and whose tip that lane holds is not listed on its
    own; the tip row says how many it holds ('folded'). 11-10: 340 rows, 97 of them one family's links.
    `is_anc(a, b)`: True when commit a is in b; None trusts the names alone."""
    by = {r['lane']: r for r in rows}
    holder = {}
    for lane in sorted(by, key=len):
        kids = [o for o in by if o != lane and o.startswith(lane + '-')]
        for k in sorted(kids, key=len, reverse=True):          # the furthest link that holds it
            if is_anc is None or is_anc(by[lane]['tip'], by[k]['tip']):
                holder[lane] = k
                break
    out = []
    for lane, r in by.items():
        if lane in holder:
            continue
        n = sum(1 for l in holder if _tip_of(l, holder) == lane)
        out.append(dict(r, folded=n) if n else r)
    return sorted(out, key=lambda r: r['lane'])


def _tip_of(lane, holder):
    seen = set()
    while lane in holder and lane not in seen:
        seen.add(lane)
        lane = holder[lane]
    return lane


def owner_text(rows, day):
    """The list of lanes that wait on the owner, as the file the brief points to: one row a chain (fold_chains)."""
    wait = [r for r in rows if r['cls'] in OWNERS]
    out = [f'# Lanes that wait on you, {day}', '',
           'The relay finished these; they change what the game looks like or how it plays, the sim, or what checks a '
           'landing, so they do not land by themselves.', '']
    for c in OWNERS:
        rs = [r for r in wait if r['cls'] == c]
        if rs:
            out.append(f'## {c} ({len(rs)})')
            for r in rs:
                pic = f'; picture: {r["pictures"][0]}' if r['pictures'] else ''
                more = f'; holds {r["folded"]} earlier lanes of its chain' if r.get('folded') else ''
                out.append(f'- {r["title"]} ({r["lane"]}, {r["n"]} files{more}): {r["why"]}{pic}')
            out.append('')
    return '\n'.join(out), len(wait)


def brief_args(rows, day, list_file):
    wait = [r for r in rows if r['cls'] in OWNERS]
    counts = ', '.join(f'{sum(1 for r in wait if r["cls"] == c)} {c}' for c in OWNERS if any(r['cls'] == c for r in wait))
    first = '; '.join(r['title'] for r in wait[:2])
    return ['--title', f'{len(wait)} finished lanes wait on you to land', '--for',
            f'They change looks, play, the sim or the landing checks ({counts}), so they wait for you. First: {first}. '
            f'The list: {list_file}',
            '--option', 'Show them to me one at a time, looks first', '--option', 'Not now: they stay branches',
            '--why', 'Each one changes what a player sees or how a battle plays; one at a time keeps every look checked.',
            '--no-evidence', 'the lanes are listed in the file; their own cards show the pictures',
            '--by', 'the landing stage']


# ---- the metric ----------------------------------------------------------------------------------------------------------

def week(now):
    y, w, _ = datetime.date.fromtimestamp(now).isocalendar()
    return f'{y}-W{w:02d}'


def count_landing(home, sha, now):
    p = Path(home) / 'landed.json'
    m = core.read_json(p, {}) or {}
    weeks = m.get('weeks') or {}
    weeks[week(now)] = weeks.get(week(now), 0) + 1
    core.write_json(p, dict(weeks=weeks, last_landed_at=datetime.datetime.fromtimestamp(now).isoformat(timespec='seconds'),
                            last_sha=sha))


# ---- the turn ------------------------------------------------------------------------------------------------------------

def run(ctx):
    return step(ctx, World.default(ctx.home))


def step(ctx, W):
    st = ctx.state({}) or {}
    now = ctx.now
    day = time.strftime('%Y-%m-%d', time.localtime(now))
    days = st.setdefault('days', {})
    for k in sorted(days)[:-14]:
        days.pop(k)
    d = days.setdefault(day, {})
    phase = st.get('phase', 'idle')
    try:
        if phase == 'gating':
            lines = gating(ctx, W, st, d)
        elif phase == 'landing':
            lines = landing_wait(ctx, W, st, d)
        else:
            lines = idle(ctx, W, st, d, day)
    finally:
        ctx.save(st)
    return lines


def idle(ctx, W, st, d, day):
    if ctx.dry and d.get('planned_at') and ctx.now - d['planned_at'] < PLAN_AGAIN:
        return [f'{day}: planned at {time.strftime("%H:%M", time.localtime(d["planned_at"]))}; '
                f'{len(d.get("stacked") or [])} lanes would land']
    if not ctx.dry and len(d.get('landings') or []) >= LANDINGS_A_DAY:
        return [f'{day}: {LANDINGS_A_DAY} landing sessions today; the next stack comes tomorrow']
    if not ctx.dry and d.get('nothing') and ctx.now - d.get('planned_at', 0) < 3600:
        return [f'{day}: nothing could land at the last look; the next look is within the hour']
    d.pop('nothing', None)
    W.fetch()
    held = (ctx.other('critic_gate', {}) or {}).get('held') or {}
    base, rows = collect(W, ctx.home, st.get('red'), held if isinstance(held, dict) else {})
    if not base:
        return ['origin\'s integration branch could not be read']
    classes = {}
    for r in rows:
        classes[r['cls']] = classes.get(r['cls'], 0) + 1
    d.update(candidates=[{k: r[k] for k in ('lane', 'tip', 'cls', 'why', 'n', 'title', 'came')} for r in rows],
             classes=classes, base=base, planned_at=ctx.now)
    lines = [f'{len(rows)} candidates: ' + ', '.join(f'{v} {k}' for k, v in sorted(classes.items()))]
    lines += brief(ctx, W, d, rows, day)
    ok = eligible(rows)
    limit = d.get('limit', STACK_MOST)
    branch = f'lane/show/landing-{day.replace("-", "")}'
    if ctx.dry:
        stacked, skipped = forecast(W, base, ok, limit)
        d.update(stacked=stacked, skipped=skipped, branch=branch,
                 would=dict(stage=str(W.stage) + ('' if W.stage_ok() else ' (would be made: git worktree add --detach)'),
                            gate=gate_cmd(W.stage), session=session_argv(str(Path(ctx.home) / 'landing' / 'prompt.md'), 'landing')))
        lines.append(f'would stack {len(stacked)} of {len(ok)} eligible on {base[:8]} as {branch}: '
                     + (', '.join(stacked) or 'none') + (f'; {len(skipped)} skipped' if skipped else ''))
        return lines
    if not ok:
        d['nothing'] = True
        return lines + ['nothing may land by itself']
    got, holder = gatelock.acquire(WHO)
    if not got:
        return lines + [f'the gate lock is held by {holder.get("who")} (pid {holder.get("pid")}): waits']
    try:
        if not W.stage_ok():
            code, o = W.make_stage(base)
            if code:
                gatelock.release(WHO)
                return lines + ['the stage checkout could not be made: ' + o[-200:]]
        if W.sg('status', '--porcelain')[1].strip():
            gatelock.release(WHO)
            return lines + [f'the stage checkout {W.stage} has uncommitted files: nothing is stacked there']
        tip, stacked, skipped = stack(W, base, ok, limit)
        d.update(stacked=stacked, skipped=skipped)
        if not stacked:
            gatelock.release(WHO)
            d['nothing'] = True
            return lines + ['no lane went on the stack: ' + '; '.join(f'{s["lane"]} {s["why"]}' for s in skipped[:4])]
        if W.out('rev-parse', '-q', '--verify', f'refs/remotes/origin/{branch}'):
            branch += time.strftime('-%H%M', time.localtime(ctx.now))
        code, o = W.sg('checkout', '-q', '-B', branch, tip)
        if code:
            gatelock.release(WHO)
            return lines + [f'the branch {branch} could not be made: {o[-200:]}']
        log = Path(ctx.home) / 'landing' / f'gate-{day}-{time.strftime("%H%M", time.localtime(ctx.now))}.log'
        pid = W.popen(gate_cmd(W.stage), W.stage, log)
        gatelock.handover(WHO, pid)
    except Exception:
        gatelock.release(WHO)
        raise
    st['phase'] = 'gating'
    tips = {r['lane']: r['tip'] for r in ok}
    st['gate'] = dict(pid=pid, log=str(log), started=ctx.now, sha=tip, tree=W.tree(tip), base=base, branch=branch,
                      lanes=stacked, tips={l: tips[l] for l in stacked})
    d['gate'] = 'running'
    d['branch'] = branch
    return lines + [f'stacked {len(stacked)} lanes as {branch} at {tip[:8]}; the full gate runs (pid {pid}, {log.name})']


def gating(ctx, W, st, d):
    g = st['gate']
    try:
        text = Path(g['log']).read_text(encoding='utf-8', errors='replace')
    except OSError:
        text = ''
    v = verdict(text, g['tree'], W.alive(g['pid']), W.marker(), g['started'], ctx.now)
    if v == 'running':
        return [f'the gate runs on {g["branch"]} ({int((ctx.now - g["started"]) // 60)} min)']
    if v != 'green':
        gatelock.release(WHO)
        d['gate'] = v
        st['phase'] = 'idle'
        if len(g['lanes']) > 1:
            d['limit'] = max(1, len(g['lanes']) // 2)
            more = f'the next stack takes {d["limit"]} lanes'
        else:
            # the tip that was gated, not origin's tip now: a lane that moved meanwhile is tried again (review L6)
            st.setdefault('red', {})[g['lanes'][0]] = (g.get('tips') or {}).get(g['lanes'][0], 'unknown')
            more = f'{g["lanes"][0]} waits until its tip moves'
        return [f'gate {v} on {g["branch"]}; {more}']
    d['gate'] = 'green'
    return land(ctx, W, st, d)


def land(ctx, W, st, d):
    """The gate is green on the stack: check nothing moved, then (ON only) start the one landing session."""
    g = st['gate']
    W.fetch()
    why = ''
    if W.base() != g['base']:
        why = 'integration moved on origin while the gate ran'
    elif W.out('rev-parse', 'HEAD', cwd=W.stage) != g['sha']:
        why = 'the stage checkout is not on the gated commit'
    elif W.sg('status', '--porcelain')[1].strip():
        why = 'the stage checkout has uncommitted files'
    elif W.tree(g['sha']) != g['tree'] or (W.marker() or [''])[0] != g['tree']:
        why = 'the gated tree is not the stacked tree'
    if why:
        gatelock.release(WHO)
        st['phase'] = 'idle'
        d['landing'] = 'not started: ' + why
        return [f'gate green, landing not started: {why}; it is stacked again']
    stamp = time.strftime('%H%M', time.localtime(ctx.now))
    folder = Path(ctx.home) / 'landing'
    prompt_file, result = folder / f'prompt-{stamp}.md', folder / f'result-{stamp}.json'
    text = PROMPT.format(stage=W.stage, branch=g['branch'], sha=g['sha'], integration=INTEGRATION, base=g['base'],
                         lanes=', '.join(g['lanes']), tree=g['tree'], result=result)
    argv = session_argv(str(prompt_file), f'landing-{stamp}')
    if ctx.dry:                         # dry never stacks, so this is only reached by a state a person wrote
        gatelock.release(WHO)
        st['phase'] = 'idle'
        d['landing'] = dict(would_run=argv)
        return ['would start the landing session: ' + ' '.join(argv[:3])]
    if not gatelock.handover(WHO, os.getpid()):
        got, holder = gatelock.acquire(WHO)
        if not got:
            return [f'gate green; the gate lock is held by {holder.get("who")}: the landing waits']
    folder.mkdir(parents=True, exist_ok=True)
    prompt_file.write_text(text, encoding='utf-8', newline='\n')
    try:
        pid = W.popen(argv, W.stage, folder / f'session-{stamp}.log', env=session_env())
    except OSError as e:
        gatelock.release(WHO)
        st['phase'] = 'idle'
        d['landing'] = f'the session did not start: {e}'
        return [d['landing']]
    gatelock.handover(WHO, pid)
    st['phase'] = 'landing'
    st['session'] = dict(pid=pid, started=ctx.now, result=str(result),
                         day=time.strftime('%Y-%m-%d', time.localtime(ctx.now)))
    d.setdefault('landings', []).append(dict(at=ctx.now, sha=g['sha'], lanes=g['lanes']))
    return [f'gate green on {g["branch"]}; the landing session runs (pid {pid})']


def landing_wait(ctx, W, st, d):
    s, g = st['session'], st['gate']
    if W.alive(s['pid']):
        # still running: the stage checkout and the lock stay its own, however long (review L3); past SESSION_MOST
        # it is said, and gatelock's own three hours let another gate past it
        mins = int((ctx.now - s['started']) // 60)
        if ctx.now - s['started'] < SESSION_MOST:
            return [f'the landing session runs ({mins} min)']
        s['overdue'] = True
        return [f'the landing session is overdue ({mins} min, pid {s["pid"]}); it is waited for, not stopped']
    W.fetch()
    said = core.read_json(s['result'], {}) or {}
    landed = W.is_anc(g['sha'], W.base())
    gatelock.release(WHO)
    st['phase'] = 'idle'
    words = f'{said.get("verdict", "no result")}: {" ".join(str(said.get("said", "")).split())[:300]}'
    rec = ((st.get('days', {}).get(s.get('day')) or d).get('landings') or [{}])[-1]       # the day it started (L9)
    if landed:
        d['landed'] = g['sha']
        count_landing(ctx.home, g['sha'], ctx.now)
        rec.update(result='landed', said=words)
        return [f'LANDED {g["branch"]} at {g["sha"][:8]}: {", ".join(g["lanes"])}']
    rec.update(result='not landed', said=words)
    return [f'not landed ({words})']


def brief(ctx, W, d, rows, day):
    """Once a day, one brief for the lanes that wait on the owner (one row a chain); dry keeps only its text."""
    if d.get('brief') or ('brief_would' in d and ctx.dry):
        return []                                    # filed (or its dry text kept) today: no fold again every look
    rows = fold_chains([r for r in rows if r['cls'] in OWNERS], W.is_anc) + [r for r in rows if r['cls'] not in OWNERS]
    text, n = owner_text(rows, day)
    if not n or d.get('brief'):
        return []
    folder = Path(ctx.home) / 'landing'
    list_file = folder / f'owner-{day}.md'
    args = brief_args(rows, day, list_file)
    if ctx.dry:
        d['brief_would'] = dict(args=args, list=text[:4000])
        return [f'would brief the owner on {n} lanes']
    folder.mkdir(parents=True, exist_ok=True)
    list_file.write_text(text, encoding='utf-8', newline='\n')
    code, said = W.add_brief(args)
    d['brief'] = said[-300:] if code == 0 else f'failed: {said[-300:]}'
    return [f'brief for {n} lanes: {d["brief"][:120]}']
