#!/usr/bin/env python3
"""The loop that works by itself: when a relay's queue runs dry, it picks up work. No model is in this file.

    python Tools/assetboard/autoloop.py                  what one round would do, changing nothing
    python Tools/assetboard/autoloop.py --once           one round, for real
    python Tools/assetboard/autoloop.py --watch 300      a round every five minutes, for ever (one loop a machine)

The owner, 2026-10-15 about 02:00: "Que them all. We want an as automized and continously working system." Before
this, a relay whose queue ran dry stood still until a session filled it; his answers on the Decide page waited for a
session; an extra relay that once read empty was never started again; and what the extra relays finished stayed on
their own boards. One round does, in this order:

  back      copy the done records of the extra boards (tw3d-board-N) to the main board, so the main board knows
  answers   take up his Decide-page answers that are a plain yes to a unit (briefs.py `queue`) or to nothing being
            built (`nothing`): queue the unit, then close the brief. Answers in his own words (`write`): one cheap
            leg reads them and the loop queues the unit they lead to, or closes the brief saying why nothing
            (loopsteps.take_writes)
  attempts  a unit that went nowhere TRIES times with nothing changed between is parked and put to him once
            (loopsteps.attempts)
  ideas     his "Do it" and his steps on an idea's route go on the pipeline's board (loopsteps.route_ideas)
  feed      an extra relay that is not running and has nothing open on its board gets the open units of ONE free
            lane of the main board (no running relay works it; parked at 99 on the main board first), and is
            started. One that has open units and is not running is started too, with a growing wait when it
            stops again for the same reason (STARTS): a relay that stops at once is not started every round
  found     found.py take, then found.py rota: a finished look's findings become fix units (small) or cards for
            him (major); and when the main queue has run dry and fewer than FIX_MOST fix units wait, the next look
            is queued: first the review-index unit whose files changed most since its last review. Also after a
            landing and once a day, not only when dry (loopsteps.reviews)

Every unit goes through the relay's own `add`, and runs under the relay's day (11% of the week for all relays
together, spent evenly) and its shares (the 25% fix share for review fixes, 15% for looks): this file starts no
model and spends nothing itself. A file autoloop.stop in its folder makes it stand by.
"""
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import found  # noqa: E402
import loopsteps  # noqa: E402

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
RELAY = GH / 'githubtest-relay-run' / 'trench-warfare-3d' / 'Tools' / 'relay' / 'relay.py'
MAIN = GH / 'tw3d-board'
EXTRAS = tuple(range(2, 7))
START = Path(os.environ.get('LOCALAPPDATA') or tempfile.gettempdir()) / 'Temp' / 'relayN-start-2026-10-13.ps1'   # not TEMP: a process WMI starts gets the system's
MAIN_START = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'relay-test-start.ps1'
MAIN_WHO = 'autoloop'               # the loop's own name for the main relay's hold and runs: no session's hold is renewed
HOME = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'autoloop'
BY = 'autoloop'
DAY_PCT = os.environ.get('TW_DAY_PCT') or '11'   # the day's cap for every relay together (the owner, 2026-10-07); the start script counts all boards
PARKED = found.PARKED
STARTS = (1800, 14400)      # after a start, wait at least this long; when it stopped again for the same reason, double it, up to the second
INTEGRATION = found.INTEGRATION
NEEDS_RELAY = 'trench-warfare-3d/Tools/relay/relay.py'     # a unit that works on the relay needs it on its lane
WAITS = 'relay-harness'     # what such a unit waits for: the relay line landing on integration
QUIET = 690                 # the relay refuses a start for ten minutes after a checkout's index moves
STALE = 900                 # a lock whose loop has not beaten for this long is taken over


def load(f: Path, default):
    try:
        return json.loads(Path(f).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def save(f: Path, data):
    f.parent.mkdir(parents=True, exist_ok=True)
    tmp = f.with_name(f.name + '.tmp')
    tmp.write_text(json.dumps(data, indent=1), encoding='utf-8', newline='\n')
    os.replace(tmp, f)


def queue_of(board: Path):
    """({id: unit} queued, {id} done) of one board's relay folder."""
    r = Path(board) / 'relay'
    q = {f.stem: load(f, {}) for f in sorted((r / 'queue').glob('*.json'))} if (r / 'queue').is_dir() else {}
    done = {f.stem for f in (r / 'done').glob('*.json')} if (r / 'done').is_dir() else set()
    return q, done


def open_units(board: Path):
    q, done = queue_of(board)
    return {u: v for u, v in q.items() if u not in done}


def parse_status(text):
    """(running lanes, the last stop's words) from `relay.py status`; None for the lanes when the text is neither
    a run nor "NOT RUNNING" (a status that fell over is not "nothing runs")."""
    lanes = set(re.findall(r'^RUNNING: .*, lane (\S+)', text or '', re.M))
    if lanes:
        return lanes, ''
    m = re.search(r'^NOT RUNNING\.(.*)$', text or '', re.M)
    if not m:
        return None, ''
    why = re.match(r'\s*Last run ([^:]*): (.*?)(?: \((\d+) legs.*)?\.?$', m.group(1))
    if not why:
        return set(), m.group(1).strip()[:200]
    said = why.group(2).strip()[:200]
    if why.group(3) and int(why.group(3)) >= 2 and 'nothing left to do' not in said:     # a run that did work: one leg that fails at once still doubles     # a run that worked and then failed or timed out is one attempt, not a
        said += f' [run {why.group(1).split()[0]}]'  # relay that stops at once: its restart wait does not double (the coordinator, 20:25)
    return set(), said


def parse_pace(text):
    """The percent of the week the day's pace has free, from `relay.py day`; 0.0 when its line says none is free,
    None when there is no such line (an older relay)."""
    line = next((x for x in str(text or '').splitlines() if x.startswith('Pace:')), None)
    if line is None:
        return None
    if 'cap is spent' in line or 'ahead of that' in line:
        return 0.0
    m = re.search(r', (?:about )?\$?([\d.]+)%? of it free', line)
    return float(m.group(1)) if m else None


# ---- the outside world ---------------------------------------------------------------------------------------------------

class World:
    """Everything that reads or writes outside this file; the tests put a stand-in in its place."""

    def __init__(self, main=MAIN, extras=EXTRAS, home=HOME, acting=True):
        self.main, self.home, self.acting = Path(main), Path(home), acting
        self.extras = {n: GH / f'tw3d-board-{n}' for n in extras if (GH / f'tw3d-board-{n}').is_dir()}

    def git(self, board, *a):
        r = subprocess.run(['git', '-C', str(board), *a], capture_output=True, stdin=subprocess.DEVNULL, timeout=300)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def relay(self, board, *a):
        env = dict(os.environ, TW_BOARD=str(board))
        n = next((k for k, b in self.extras.items() if Path(b) == Path(board)), 0)
        if n:
            env['TW_RELAY_HOME'] = str(Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / f'relay{n}')
        else:
            env.pop('TW_RELAY_HOME', None)
        try:
            r = subprocess.run([sys.executable, str(RELAY), *a], capture_output=True, text=True, encoding='utf-8', errors='replace',
                               stdin=subprocess.DEVNULL, env=env, timeout=600)
        except subprocess.TimeoutExpired:
            return 124, 'the relay did not answer in ten minutes'
        return r.returncode, (r.stdout + r.stderr).strip()

    def status(self, board):
        code, text = self.relay(board, 'status')
        return parse_status(text) if code == 0 else (None, '')

    def pull(self, board):
        """git pull --rebase, tried again when another git on the board was mid-fetch ("Cannot rebase onto multiple
        branches", 19:35) or held its lock."""
        for n in range(4):
            code, out = self.git(board, 'pull', '--rebase', '-q')
            if code == 0 or not re.search(r'multiple branches|index\.lock|cannot lock ref|Unable to create', out, re.I):
                return code, out
            if n < 3:
                time.sleep(3 + 3 * n)
        return code, out

    def tracked(self, board, folder):
        """The file names git has under `folder` of a board, committed."""
        code, out = self.git(board, 'ls-files', '--', folder)
        return None if code else {Path(p).name for p in out.splitlines()}

    def commit_push(self, board, paths, message, push=True):
        """'' when nothing was to commit, else what happened; raises when git refuses, so the step says so."""
        if not self.acting:
            return 'would commit: ' + message
        for git_args in (('add', '--', *paths),):
            code, out = self.git(board, *git_args)
            if code:
                raise RuntimeError(f'git add refused: {out[:200]}')
        if self.git(board, 'diff', '--cached', '--quiet')[0] == 0:
            return ''
        code, out = self.git(board, 'commit', '-q', '-m', message)
        if code:
            raise RuntimeError(f'git commit refused: {out[:200]}')
        if not push:
            return 'committed (kept local)'
        for _ in range(3):
            if self.git(board, 'push', '-q')[0] == 0:
                return 'pushed'
            self.pull(board)
        return 'committed; the push was refused three times: it is pushed with the next one'

    def start(self, n):
        if not self.acting:
            return 'would start'
        flags = getattr(subprocess, 'CREATE_NO_WINDOW', 0) | getattr(subprocess, 'CREATE_NEW_PROCESS_GROUP', 0)   # DETACHED_PROCESS: powershell runs nothing
        if n == 1:                  # the main relay: its hold first, then a run under the usual day (no figure passed)
            code, said = self.relay(self.main, 'hold', MAIN_WHO, '--hours', '0.25')     # long enough to start; given back when idle
            if code:
                return 'not started: ' + (said.splitlines() or ['the hold was refused'])[0][:160]
            cmd = [str(MAIN_START), '-Who', MAIN_WHO, '-Sources', 'lane']
        else:
            cmd = [str(START), '-N', str(n), '-DayPct', DAY_PCT]
        p = subprocess.Popen(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', *cmd],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, creationflags=flags, close_fds=True)
        return f'started (pid {p.pid})'

    def answers(self):
        import briefs
        import notes
        return briefs.answers(briefs.read_all(briefs.folder()), notes.read_all(notes.folder()))

    def take(self, a, queued='', outcome='', option=''):
        import briefs
        import notes
        briefs.take(briefs.folder(), notes.folder(), a['id'], by=BY, note=a['note'], queued=queued, outcome=outcome, option=option, board=self.main)

    def add(self, unit):
        f = Path(tempfile.mkdtemp(prefix='tw-autoloop-')) / (unit['id'] + '.json')
        f.write_text(json.dumps(unit, indent=1), encoding='utf-8')
        code, said = self.relay(self.main, 'add', '--unit', str(f))
        return code == 0 or 'queued already, the same unit' in said, (said.splitlines() or [''])[-1][:300]

    def prio(self, uid, n):
        return self.relay(self.main, 'prio', uid, str(n))

    def log_decision(self, line):
        """His answers taken up by the loop, as rows a session moves into decisions.md (the loop has no lane)."""
        f = Path(os.environ.get('TW_AUTO_DECISIONS') or 'G:/My Drive/TW3D-pipeline/decisions/taken-by-the-loop.md')
        if self.acting:
            f.parent.mkdir(parents=True, exist_ok=True)
            with open(f, 'a', encoding='utf-8', newline='\n') as h:
                h.write(line + '\n')

    def now(self):
        return time.time()

    def repo(self, *a):
        """git in the checkout this file runs from, read only (refs and objects; never a work checkout's index)."""
        r = subprocess.run(['git', '-C', str(found.build.REPO), *a], capture_output=True, stdin=subprocess.DEVNULL, timeout=300)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def has(self, ref, path):
        return self.repo('cat-file', '-e', f'{ref}:{path}')[0] == 0

    def lane_ref(self, lane):
        """origin/<lane> when it is on origin, else integration (a new lane starts there)."""
        return f'origin/{lane}' if self.repo('rev-parse', '--verify', '-q', f'origin/{lane}')[0] == 0 else f'origin/{INTEGRATION}'

    def works(self):
        """{relay number: its work checkout}; 1 is the main relay."""
        out = {1: GH / 'githubtest-relay-work'}
        out.update({n: GH / f'githubtest-relay-work{n}' for n in self.extras})
        return {n: p for n, p in out.items() if (p / '.git').exists()}

    def rebased(self, work):
        """[(branch, sha)] of the lane branches in a work checkout that were rebased onto integration and cannot
        fast-forward their origin branch. Reads refs and objects only: the relay refuses to start for ten minutes
        after a checkout's index moves, and nothing here touches the index."""
        g = lambda *a: self.git(work, *a)       # noqa: E731
        code, out = g('for-each-ref', '--format=%(refname:short) %(objectname)', 'refs/heads/lane/')
        res = []
        for line in out.splitlines() if code == 0 else []:
            name, _, sha = line.partition(' ')
            if g('rev-parse', '--verify', '-q', f'refs/remotes/origin/{name}')[0]:
                continue                                        # not on origin: the relay pushes it itself
            if g('merge-base', '--is-ancestor', f'origin/{name}', sha)[0] == 0:
                continue                                        # it fast-forwards: nothing to rescue
            if g('for-each-ref', '--points-at', sha, 'refs/remotes/origin/')[1].strip():
                continue                                        # on origin already, under another name
            code, cherry = g('cherry', sha, f'origin/{name}')
            if code or any(l.startswith('+') for l in cherry.splitlines()):
                continue                                        # origin has work this branch lacks: stale or a dropped commit
            if g('merge-base', '--is-ancestor', f'origin/{INTEGRATION}', sha)[0] == 0:
                res.append((name, sha))
        return res

    def free_name(self, work, lane):
        for s in 'bcdefghij':
            name = f'{lane}-{s}'
            if self.git(work, 'rev-parse', '--verify', '-q', f'refs/remotes/origin/{name}')[0]:
                return name
        return ''

    def push_as(self, work, sha, name):
        """Push a commit to a NEW branch on origin; never a force push."""
        return self.git(work, 'push', '-q', 'origin', f'{sha}:refs/heads/{name}')

    def put_aside(self, work, n):
        """Leftover work of a leg that ended without committing: kept as a patch and a named stash, so the relay can
        start again (what keep_relays4.ps1 did). Returns what was done."""
        stamp = time.strftime('%Y%m%d-%H%M%S')
        d = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'Temp' / 'clear-dirty-patches'
        d.mkdir(parents=True, exist_ok=True)
        code, diff = self.git(work, '--no-optional-locks', 'diff', 'HEAD')
        if code:
            return False, f'not put aside: git diff failed ({diff[:120]})'
        (d / f'{stamp}-relay{n}.patch').write_text(diff + '\n', encoding='utf-8')
        branch = self.git(work, 'rev-parse', '--abbrev-ref', 'HEAD')[1]
        code, said = self.git(work, 'stash', 'push', '-u', '-q', '-m', f'put aside {stamp} by autoloop.py so relay {n} can start: left on {branch}')
        if code:
            return False, f'not put aside: git stash failed ({said[:120]})'
        return True, f'put aside as a stash and {d / (stamp + "-relay" + str(n) + ".patch")}'

    def checkouts(self):
        """{branch: the checkout it is checked out in} from `git worktree list --porcelain` of the run copy's repo
        and of every work checkout's (one repo today, but a work checkout cloned apart would have its own); None when
        git did not answer. A branch checked out in one worktree cannot be switched to in another: the extra relay
        fed such a lane failed at once with 0 legs, all night of 2026-10-16."""
        out, asked = {}, 0
        for where in [RELAY.parents[3], *self.works().values()]:
            if not Path(where).exists():
                continue
            code, text = self.git(where, 'worktree', 'list', '--porcelain')
            if code:
                continue
            asked += 1
            path = ''
            for line in text.splitlines():
                if line.startswith('worktree '):
                    path = line[9:].strip()
                elif line.startswith('branch refs/heads/') and path:
                    out.setdefault(line[18:].strip(), path)
        return out if asked else None

    def clean(self, work):
        """True when a checkout has no changed or new files (read only: no index refresh is written)."""
        code, out = self.git(work, '--no-optional-locks', 'status', '--porcelain')
        return code == 0 and not out.strip()

    def detach(self, work):
        """Let go of a checkout's branch (git switch --detach), so another checkout can take the lane."""
        return self.git(work, 'switch', '-q', '--detach')

    def main_on(self):
        """(unit, lane) the main relay's current run is on, from its progress.json now_on and its status line;
        ('', '') when it does not run. The status line names no lane before the run's first leg has one: the loop
        then fed relay 2 the very unit the running main relay was on (01:41, 2026-10-16)."""
        code, text = self.relay(self.main, 'status')
        m = re.search(r'^RUNNING: \S+ (\S+)[^,\n]*, lane (\S+)', text or '', re.M) if code == 0 else None
        if not m:
            return '', ''
        lane = '' if m.group(2) in ('None', '') else m.group(2)
        home = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'relay'
        now = str(load(home / 'runs' / m.group(1) / 'progress.json', {}).get('now_on') or '')
        return now.split(',')[0].strip(), lane

    def found(self):
        return found.World(found.build.REPO, self.main, self.home / 'found')

    # ---- for loopsteps.py ----
    def pace_free(self):
        """The percent of the week the day's pace has free now (relay.py day), 0.0 when none, None when unread."""
        code, text = self.relay(self.main, 'day')
        return parse_pace(text) if code == 0 else None

    def leg(self, prompt, usd):
        """One headless Sonnet answer at low effort, no tools, no project files, at most `usd`: (its text, its cost).
        Run from an empty folder with no setting sources, so no CLAUDE.md, skill or memory is loaded (with them the
        same one-line answer cost $0.44 before a word; without, $0.001)."""
        cmd = ['claude', '-p', '--model', 'sonnet', '--effort', 'low', '--max-budget-usd', str(usd), '--output-format', 'json',
               '--tools', '', '--no-session-persistence', '--disable-slash-commands', '--strict-mcp-config', '--setting-sources', '',
               '--system-prompt', 'You turn a game studio owner\'s decision into one JSON work unit. Reply with the JSON object only.']
        where = Path(tempfile.mkdtemp(prefix='tw-autoloop-leg-'))
        try:
            r = subprocess.run(cmd, input=prompt, capture_output=True, text=True, encoding='utf-8', errors='replace', cwd=str(where),
                               timeout=300, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
            d = json.loads(r.stdout or '{}')
        except (OSError, ValueError, subprocess.TimeoutExpired) as e:
            return f'the leg did not answer: {e}', 0.0
        return str(d.get('result') or ''), float(d.get('total_cost_usd') or 0.0)

    def book(self, rec):
        """Book the loop's own legs on the day (relay.py agents book --file): a run going here takes it in itself."""
        f = Path(tempfile.mkdtemp(prefix='tw-autoloop-book-')) / 'booking.json'
        f.write_text(json.dumps(rec), encoding='utf-8')
        return self.relay(self.main, 'agents', 'book', '--file', str(f))

    def lane_facts(self, lane):
        if not lane:
            return 'no lane is named'
        ref = f'origin/{lane}'
        if self.repo('rev-parse', '--verify', '-q', ref)[0]:
            return f'{lane} is not on origin'
        if self.repo('merge-base', '--is-ancestor', ref, f'origin/{INTEGRATION}')[0] == 0:
            return f'{lane} is already on the integration branch (landed)'
        n = self.repo('rev-list', '--count', f'origin/{INTEGRATION}..{ref}')[1]
        b = self.repo('rev-list', '--count', f'{ref}..origin/{INTEGRATION}')[1]
        return f'{lane} has {n} commits not on the integration branch and is {b} commits behind it'

    def ideas(self, write):
        """idearoute's route and gates on the main board: (routed, gates, error)."""
        import briefs
        import ideas
        import idearoute
        import notes
        routed, g, err = [], dict(asked=[], done=[], owed=[]), ''
        try:
            routed = idearoute.route(ideas.folder(), self.main, write=write)
            g = idearoute.gates(ideas.folder(), briefs.folder(), notes.folder(), self.main, write=write)
        except (ValueError, SystemExit) as e:
            err = str(e)
        return routed, g, err

    def changed(self, board, folders):
        code, out = self.git(board, 'ls-files', '-m', '-o', '--exclude-standard', '--', *folders)
        return [] if code else [p for p in out.splitlines() if p.strip()]

    def stops(self):
        """[(run, stopped at, unit, verdict, why)] of every board's stop records of the last SINCE_DAYS days, oldest first."""
        since = time.strftime('%Y%m%d', time.localtime(self.now() - loopsteps.SINCE_DAYS * 86400))
        out = []
        for board in [self.main, *self.extras.values()]:
            for f in Path(board).glob('relay/*/stops/*.json'):
                if f.name[:8] < since:
                    continue
                d = load(f, {})
                for uid, v in (d.get('units') or {}).items() if isinstance(d, dict) else []:
                    why = (d.get('problems') or {}).get(uid) or d.get('reason') or ''
                    out.append((d.get('run') or f.stem, d.get('stopped_at') or '', uid, v, ' '.join(map(str, why)) if isinstance(why, list) else str(why)))
        return sorted(out, key=lambda x: (x[1], x[0]))

    def lane_sha(self, lane, at=''):
        """The head of origin/<lane>, or the commit it stood at by `at` (a stop record's time); '' when none."""
        if not lane:
            return ''
        code, out = self.repo('rev-list', '-1', *([f'--before={at}'] if at else []), f'origin/{lane}')
        return '' if code else out

    def int_sha(self, at=''):
        return self.lane_sha(INTEGRATION, at)

    def lane_files(self, lane):
        code, out = self.repo('diff', '--name-only', f'origin/{INTEGRATION}...origin/{lane}')
        return [] if code else out.splitlines()

    def changed_files(self, a, b):
        code, out = self.repo('diff', '--name-only', a, b)
        return [] if code else out.splitlines()

    def unqueue(self, board, uid):
        """Take a unit off a board's queue: git rm, committed (pushed on the main board). True when it is off."""
        rel = f'relay/queue/{uid}.json'
        if not (Path(board) / rel).exists():
            return True
        if self.git(board, 'rm', '-q', '--', rel)[0]:
            return False
        code, said = self.git(board, 'commit', '-q', '-m', f'relay: {uid} parked by the loop (no progress in its tries)', '--', rel)   # that path only, never the whole index
        if code:
            self.git(board, 'reset', '-q', 'HEAD', '--', rel)
            self.git(board, 'checkout', '-q', 'HEAD', '--', rel)      # put back as it was: it is tried next round
            return False
        if Path(board) == Path(self.main):
            for _ in range(3):
                if self.git(board, 'push', '-q')[0] == 0:
                    break
                self.pull(board)
        return True

    def park_brief(self, uid, rec, nxt, unit):
        """The one brief a parked unit gets: A puts it back as it is, B drops it, C is his words to the writes leg."""
        import briefs
        import notes
        title, what_for, why = loopsteps.park_text(uid, rec, nxt)
        b = briefs.add(briefs.folder(), title=title, what_for=what_for,
                       options=['Try it once more as it stands', 'Drop it: nothing more is built', 'Change it: write in a note what to do instead'],
                       why=why, no_evidence='a unit that went nowhere has nothing to show but its stop records', lane=unit.get('lane', ''), by='autoloop.py')
        ns = notes.read_all(notes.folder())
        try:
            briefs.then(briefs.folder(), ns, b['id'], 'A', 'It goes back on the relay\'s queue as it is', unit)
        except ValueError:
            pass                                    # its id is another brief's already: A is then his words, for the writes leg
        briefs.then(briefs.folder(), ns, b['id'], 'B', 'Nothing more is built; it stays off the queue', None)
        return b['id']

    def ids(self):
        """Every unit id a board has queued or done: a new unit never takes one of them."""
        return set().union(*(set(queue_of(b)[0]) | queue_of(b)[1] for b in [self.main, *self.extras.values()]))

    def brief_answered(self, bid):
        import briefs
        hit = [b for b in briefs.read_all(briefs.folder()) if b['id'] == bid]
        return bool(hit) and hit[0].get('state') == 'answered'


# ---- the four steps -------------------------------------------------------------------------------------------------------

def forward_done(w):
    """A unit done on the main board (or another extra) that an extra board still has open gets that done record on
    the extra board, committed there (kept local, as its relay keeps its board), so its relay does not start again
    on finished work (heroes-off-bench, relay 6, four runs in the night of 2026-10-15). Not under a run: a leg that
    sees relay/done move under it ends the run."""
    out = []
    boards = [w.main, *w.extras.values()]
    for n, board in sorted(w.extras.items()):
        mine = open_units(board)
        hits = {uid: f for b in boards if Path(b) != Path(board) for uid in mine
                for f in [Path(b) / 'relay' / 'done' / f'{uid}.json'] if f.is_file()}
        if not hits:
            continue
        running, _ = w.status(board)
        if running is None or running:
            continue
        if not w.acting:
            out.append(f'back: relay {n} has {len(hits)} units open that are done elsewhere: their done records would be copied to it')
            continue
        (Path(board) / 'relay' / 'done').mkdir(parents=True, exist_ok=True)
        for uid, f in hits.items():
            shutil.copyfile(f, Path(board) / 'relay' / 'done' / f.name)
        w.commit_push(board, [f'relay/done/{uid}.json' for uid in hits], f'relay: {len(hits)} done records from the other boards ({", ".join(sorted(hits)[:6])})', push=False)
        out.append(f'back: relay {n} had {len(hits)} units open that are done elsewhere ({", ".join(sorted(hits)[:4])}): their done records copied to it')
    return out


def copy_back(w):
    """Done records of the extra boards that the main board has not committed are copied to it and pushed. Not while
    the main relay runs: a leg that sees the board's relay/done move under it ends the run with "broke a rule" (the
    main run of 2026-10-15 13:51 stopped so)."""
    out = forward_done(w)
    running, _ = w.status(w.main)
    if running is None or running:
        return out + (['back: the main relay runs: done records are copied to its board once it has stopped'] if running else [])
    if w.acting:
        code, said = w.pull(w.main)
        if code:
            return out + [f'back: the main board did not pull ({said[:120]}): tried next round']
    have = w.tracked(w.main, 'relay/done')
    if have is None:
        return out + ['back: git did not list the main board\'s done records: tried next round']
    mine = w.main / 'relay' / 'done'
    new = []
    for n, board in sorted(w.extras.items()):
        for f in sorted((Path(board) / 'relay' / 'done').glob('*.json')):
            if f.name not in have and f.name not in {g.name for _, g in new}:
                new.append((n, f))
    if not new:
        return out
    if w.acting:
        mine.mkdir(parents=True, exist_ok=True)
        for _, f in new:
            if not (mine / f.name).exists():
                shutil.copyfile(f, mine / f.name)
    said = w.commit_push(w.main, ['relay/done/' + f.name for _, f in new],
                         f'relay: {len(new)} done records of the extra relays copied back ({", ".join(f.stem for _, f in new[:6])})')
    return out + [f'back: {len(new)} done records copied from the extra boards: {said}']


def take_answers(w):
    """His plain yes on the Decide page is carried out; his own words are read by a leg (loopsteps.take_writes). Each answer on its own:
    one that fails is tried again next round and does not hold up the others; its row is written once, after the
    brief is closed."""
    out = []
    for a in w.answers():
        if a['go'] not in ('queue', 'nothing'):
            continue
        try:
            if a['go'] == 'queue':
                unit = dict(a['unit'] or {})
                unit.setdefault('role', 'lane')
                unit.pop('priority', None)
                if not w.acting:
                    out.append(f'answers: {a["id"]}: would queue {unit.get("id")} and close the brief')
                    continue
                ok, said = w.add(unit)
                if not ok:
                    out.append(f'answers: {a["id"]}: the relay refused {unit.get("id")}: {said}')
                    continue
                w.take(a, queued=unit['id'])
                loopsteps.answered(w, unit['id'])          # a parked unit he sent back: its tries start over
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Queued as `{unit["id"]}`. (owner, Decide page, note {a["note"]}) |')
                out.append(f'answers: {a["id"]}: his yes queued {unit["id"]} and the brief is closed')
            else:
                if not w.acting:
                    out.append(f'answers: {a["id"]}: would close it, nothing is built')
                    continue
                w.take(a, outcome=a.get('says') or 'his option builds nothing')
                loopsteps.dropped_by(w, a['id'])           # his B on a parked unit's brief
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Nothing is built. (owner, Decide page, note {a["note"]}) |')
                out.append(f'answers: {a["id"]}: his answer builds nothing; the brief is closed')
        except Exception as e:      # noqa: BLE001  e.g. take refuses while the unit's push has not reached the board's origin
            out.append(f'answers: {a["id"]}: not closed this round ({str(e)[:200]})')
    return out + loopsteps.take_writes(w)


def may_start(w, n, reason):
    """True when relay n may be started now: STARTS[0] after its last start, doubled each time it stopped again for
    the same reason, at most STARTS[1]. Remembered in the loop's folder."""
    starts = load(w.home / 'starts.json', {})
    s = starts.get(str(n))
    if s and w.now() - s.get('at', 0) < s.get('wait', STARTS[0]):
        return False
    same = bool(s and reason and reason == s.get('reason'))
    starts[str(n)] = dict(at=w.now(), reason=reason, wait=min(s.get('wait', STARTS[0]) * 2, STARTS[1]) if same else STARTS[0])
    save(w.home / 'starts.json', starts)
    return True


def lane_free(w, n, lane, cos, runs, out):
    """True when relay n can switch its work checkout to `lane`: no other checkout has it checked out, or the one
    that has it is the work checkout of a relay that does not run and has a clean tree, which is then detached to
    let go of it. `cos` {branch: checkout} (checkouts()), `runs` {relay number: its running lanes or None}. A lane
    the loop may not take is said in `out` and left alone."""
    if not lane:
        return True
    works = {m: Path(p) for m, p in w.works().items()}
    at = cos.get(lane)
    if not at or (n in works and os.path.normcase(os.path.abspath(at)) == os.path.normcase(os.path.abspath(works[n]))):
        return True
    m = next((m for m, p in works.items() if os.path.normcase(os.path.abspath(p)) == os.path.normcase(os.path.abspath(at))), None)
    if m is None or m == n or runs.get(m) != set() or not w.clean(at):
        why = ('it is not a relay\'s work checkout' if m is None else 'its relay runs' if runs.get(m) != set() else 'its tree has changes')
        out.append(f'feed: relay {n}: {lane} skipped: it is checked out in {at} ({why})')
        return False
    if not w.acting:
        out.append(f'feed: relay {n}: {lane} would be freed from {at} (relay {m} does not run, clean tree)')
        return True
    code, said = w.detach(at)
    if code:
        out.append(f'feed: relay {n}: {lane} skipped: {at} could not let go of it ({said[:120]})')
        return False
    cos.pop(lane, None)
    out.append(f'feed: relay {n}: {lane} freed: {at} (relay {m}, not running, clean) was detached from it')
    return True


def feed(w):
    """Every extra relay that is not running gets work and is started."""
    out = []
    lanes, runs = set(), {}
    boards = {1: w.main, **w.extras}
    for m, board in boards.items():
        got, _ = w.status(board)
        if got is None:
            return ['feed: a relay did not say whether it runs: nothing is fed this round']
        lanes |= got
        runs[m] = got
    cos = w.checkouts()
    if cos is None:
        return ['feed: git did not list the checkouts: nothing is fed this round']
    main_unit, main_lane = w.main_on()          # the main run's unit and lane, also before its status names a lane
    if main_lane:
        lanes.add(main_lane)
    tries = loopsteps.load(w.home / 'attempts.json', {})
    held = {v.get('lane') for b in w.extras.values() for v in open_units(b).values()} | lanes
    kin = {loopsteps.work_key(u, v) for b in w.extras.values() for u, v in open_units(b).items()}   # x and x-b are one piece of work
    kin_lanes = {loopsteps.family(l)[0] for l in lanes}
    q, done = queue_of(w.main)
    waiting = load(w.home / 'lane_waits.json', {})          # {unit: lane} taken off an extra relay because its lane was held
    for uid, lane in sorted(waiting.items()):
        if uid in done or uid not in q:
            waiting.pop(uid)
        elif lane not in cos and lane not in lanes and w.acting and w.prio(uid, 50)[0] == 0:
            waiting.pop(uid)
            q[uid]['priority'] = 50
            out.append(f'feed: {uid} goes back to priority 50: {lane} is free now')
    if w.acting:
        save(w.home / 'lane_waits.json', waiting)
    main_ids = {loopsteps.family(main_unit)[0]} if main_unit else set()
    if main_unit and (q.get(main_unit) or {}).get('lane'):
        held.add(q[main_unit]['lane'])
    for n, board in sorted(w.extras.items()):
        running, reason = w.status(board)
        if running:
            continue
        latest = {}
        for b in [w.main, *w.extras.values()]:
            for u, v in open_units(b).items():
                k = loopsteps.work_key(u, v)
                latest[k] = max(latest.get(k, 0), loopsteps.family(u)[1])
        mine = {u: v for u, v in open_units(board).items() if u not in done                  # done on the main board: not work
                and loopsteps.family(u)[1] >= latest.get(loopsteps.work_key(u, v), 0)}       # nor a unit a later copy (x-b) replaces
        stuck = {u: v for u, v in mine.items() if v.get('lane') and v['lane'] not in lanes and not lane_free(w, n, v['lane'], cos, runs, out)}
        if stuck and w.acting:                  # its lane is held where the loop cannot free it: the relay is not left standing on it
            for uid, v in sorted(stuck.items()):
                if w.unqueue(board, uid):
                    mine.pop(uid)
                    waiting[uid] = v['lane']
                    out.append(f'feed: relay {n}: {uid} taken off its queue: {v["lane"]} is held where the loop cannot free it; '
                               f'it waits parked on the main board until the lane is free')
            save(w.home / 'lane_waits.json', waiting)
        if not mine:
            free = {}
            for uid, u in q.items():
                if uid in done or found.prio(u) >= PARKED or not u.get('lane') or u['lane'] in held or loopsteps.work_key(uid, u) in kin \
                        or loopsteps.family(u['lane'])[0] in kin_lanes or loopsteps.family(uid)[0] in main_ids \
                        or (tries.get(uid) or {}).get('main_try'):      # sent to the main relay for its second try: not fed back
                    continue
                free.setdefault(u['lane'], []).append((found.prio(u), uid, u))
            lane = next((k for k in sorted(free, key=lambda k: (min(p for p, _, _ in free[k]), k))
                         if lane_free(w, n, k, cos, runs, out)), None)
            if lane is None:
                continue
            units = sorted(free[lane], key=lambda x: (x[0], x[1]))
            held.add(lane)
            if not w.acting:
                out.append(f'feed: relay {n} is empty: it would get {lane} ({", ".join(uid for _, uid, _ in units)})')
                continue
            parked = []
            for p, uid, _ in units:                 # park on the main board first: the main relay must not take them
                code, said = w.prio(uid, PARKED)
                if code:
                    break
                parked.append((p, uid))
            if len(parked) < len(units):
                for p, uid in parked:
                    w.prio(uid, p)
                out.append(f'feed: relay {n}: {lane} could not be parked on the main board, put back: tried next round')
                continue
            qd = Path(board) / 'relay' / 'queue'
            qd.mkdir(parents=True, exist_ok=True)
            for _, uid, u in units:
                u = dict(u)
                u.pop('priority', None)
                (qd / f'{uid}.json').write_text(json.dumps(u, indent=2, ensure_ascii=False) + '\n', encoding='utf-8', newline='\n')
            w.commit_push(board, ['relay/queue/' + f'{uid}.json' for _, uid, _ in units], f'relay: {len(units)} units of {lane} for extra relay {n}, kept local', push=False)
            out.append(f'feed: relay {n} was empty: it gets {lane} ({", ".join(uid for _, uid, _ in units)}), parked at {PARKED} on the main board')
            reason = ''
        else:
            blocked = sorted({v.get('lane') for v in mine.values()} - {None, ''})
            blocked = [l for l in blocked if l in lanes or not lane_free(w, n, l, cos, runs, out)]
            if blocked:                         # it would fail at once with 0 legs: not started
                out.append(f'feed: relay {n} is not started: {", ".join(blocked)} cannot be switched to now')
                continue
        if not w.acting:
            out.append(f'feed: relay {n} is not running and has work: it would be started')
            continue
        if 'uncommitted changes' in reason and n in w.works():
            starts = load(w.home / 'starts.json', {})
            if (starts.get(str(n)) or {}).get('reason') != 'put aside':
                ok, said = w.put_aside(w.works()[n], n)
                out.append(f'feed: relay {n} stopped on leftover files: {said}')
                if ok:          # the relay wants ten quiet minutes after the index moved: start it from the next round after that
                    starts[str(n)] = dict(at=w.now(), reason='put aside', wait=QUIET)
                    save(w.home / 'starts.json', starts)
                continue
        if not may_start(w, n, reason):
            continue
        out.append(f'feed: relay {n} is not running and has work: {w.start(n)}')
        runs[n] = {'started this round'}        # its checkout is no longer free to detach in this round
    running, reason = w.status(w.main)          # the main relay: started when it has a unit to take
    q, done = queue_of(w.main)
    extra_kin = {loopsteps.work_key(u, v) for b in w.extras.values() for u, v in open_units(b).items()}
    work = any(u not in done and found.prio(v) < PARKED and v.get('lane') not in held and loopsteps.work_key(u, v) not in extra_kin
               for u, v in q.items())                  # nor the copy of a unit an extra relay holds
    started = False
    if running == set() and work:               # the lanes it will take, held by a stopped extra relay's clean checkout, are freed first
        for lane in sorted({v.get('lane') for u, v in q.items() if u not in done and found.prio(v) < PARKED and v.get('lane')} - held):
            if lane in cos:
                lane_free(w, 1, lane, cos, runs, out)    # its 12:42 run had 4 FAILs that were only these refusals (2026-10-16)
    if running == set() and work:
        if not w.acting:
            out.append('feed: the main relay is not running and has work: it would be started')
        else:
            before = load(w.home / 'starts.json', {}).get('1')
            if may_start(w, 1, reason):
                said = w.start(1)
                out.append(f'feed: the main relay is not running and has work: {said}')
                started = not said.startswith('not started')
                if not started:                 # a start its hold refused is no start: tried again next round, the wait not doubled
                    starts = load(w.home / 'starts.json', {})
                    starts.pop('1', None) if before is None else starts.__setitem__('1', before)
                    save(w.home / 'starts.json', starts)
    if running == set() and not started and w.acting:
        w.relay(w.main, 'hold', MAIN_WHO, '--release')     # the loop's own hold only: another name's is refused, not taken
    return out


def running_lanes(w):
    """The lanes every relay works now, or None when one of them does not say."""
    lanes = set()
    for board in [w.main, *w.extras.values()]:
        got, _ = w.status(board)
        if got is None:
            return None
        lanes |= got
    return lanes


def rescue(w):
    """A leg that rebased its lane cannot push it (the relay refuses a force push) and ends BLOCKED. The rebased
    lane is pushed as <lane>-b (or the next free suffix), never forced, and said; the coordinator did it by hand
    twice on 2026-10-15. Only for a relay that is not running, so no leg is mid-way."""
    out, done, seen = [], load(w.home / 'rescued.json', {}), set()
    units = {v.get('lane') for b in [w.main, *w.extras.values()] for v in open_units(b).values()}
    busy = running_lanes(w)
    if busy is None:
        return ['rescue: a relay did not say whether it runs: nothing is pushed this round']
    for n, work in sorted(w.works().items()):
        running, _ = w.status(w.main if n == 1 else w.extras[n])
        if running is None or running:
            continue
        if w.acting:
            w.git(work, 'fetch', '-q', 'origin')         # refs only; the index is not touched
        for lane, sha in w.rebased(work):
            if lane not in units or f'{lane}@{sha}' in done or lane in busy or (lane, sha) in seen:
                continue                    # done before, a leg works on it now, or seen in another worktree of the same repo
            seen.add((lane, sha))
            name = w.free_name(work, lane)
            if not name:
                done[f'{lane}@{sha}'] = 'no free name'
                save(w.home / 'rescued.json', done)
                out.append(f'rescue: {lane} ({sha[:9]}) is rebased and cannot fast-forward; no free name -b..-j (said once)')
                continue
            if not w.acting:
                out.append(f'rescue: would push the rebased {lane} ({sha[:9]}) as {name}')
                continue
            again = running_lanes(w)                 # a relay may have started since the round began
            if again is None or lane in again or w.status(w.main if n == 1 else w.extras[n])[0]:
                continue
            code, said = w.push_as(work, sha, name)
            if code:
                done[f'{lane}@{sha}'] = 'refused: ' + said[:160]
                save(w.home / 'rescued.json', done)
                out.append(f'rescue: {lane} as {name} was refused (said once): {said[:160]}')
                continue
            done[f'{lane}@{sha}'] = name
            save(w.home / 'rescued.json', done)
            w.log_decision(f'| {time.strftime("%Y-%m-%d")} | **Done by the loop.** **The rebased {lane} could not fast-forward, so it was pushed as {name} ({sha[:9]}), with no force push.** A session moves the lane\'s units to {name}. |')
            out.append(f'rescue: the rebased {lane} pushed as {name} ({sha[:9]})')
    return out


def wait_for_relay(w):
    """A depends-on the queue has no field for: a unit about the relay (its goal names Tools/relay) whose lane has
    no Tools/relay waits parked at 99 until the relay line has landed, then goes back to its old priority."""
    out, held = [], load(w.home / 'held.json', {})
    busy = running_lanes(w)
    if busy is None:
        return ['waits: a relay did not say whether it runs: nothing is parked this round']
    landed = w.has(f'origin/{INTEGRATION}', NEEDS_RELAY)
    q, done = queue_of(w.main)
    for uid, old in sorted(held.items()):
        if uid in done or uid not in q:
            held.pop(uid)
            continue
        if found.prio(q[uid]) != PARKED:
            held.pop(uid)                       # somebody set another priority meanwhile: theirs stands
            continue
        if landed or w.has(w.lane_ref(q[uid].get('lane', '')), NEEDS_RELAY):
            if w.acting and w.prio(uid, old)[0]:
                continue
            held.pop(uid)
            out.append(f'waits: {uid} goes back to priority {old}: its lane has Tools/relay now')
    if not landed:
        for uid, u in q.items():
            if uid in done or uid in held or found.prio(u) >= PARKED or u.get('lane') in busy or 'Tools/relay' not in str(u.get('goal', '')):
                continue
            if w.has(w.lane_ref(u.get('lane', '')), NEEDS_RELAY):
                continue
            if not w.acting:
                out.append(f'waits: {uid} would wait for {WAITS} (its lane has no Tools/relay)')
                continue
            if w.prio(uid, PARKED)[0]:
                continue
            held[uid] = found.prio(u)
            out.append(f'waits: {uid} waits for {WAITS} to land (its lane has no Tools/relay): parked at {PARKED}')
    if w.acting:
        save(w.home / 'held.json', held)
    return out


def attempts(w):
    boards = [w.main, *w.extras.values()]
    dones = set().union(*(queue_of(b)[1] for b in boards))
    opens = {b: {u: v for u, v in open_units(b).items() if u not in dones} for b in boards}
    idle = [b for b in boards if w.status(b)[0] == set()]
    return loopsteps.attempts(w, opens, dones, running_lanes(w), idle, set(w.extras.values()))


def round_(w):
    lines = []
    for step in (copy_back, take_answers, attempts, wait_for_relay, rescue, loopsteps.route_ideas, feed):
        try:
            lines += step(w)
        except Exception:       # noqa: BLE001  one step that fails must not stop the others
            lines.append(f'{step.__name__}: failed: ' + traceback.format_exc().strip().splitlines()[-1][:300])
    fw = w.found()
    try:
        lines += loopsteps.reviews(w, fw)
    except Exception:           # noqa: BLE001
        lines.append('reviews: failed: ' + traceback.format_exc().strip().splitlines()[-1][:300])
    try:
        if w.acting:
            lines += found.take(fw)
            lines.append(found.rota(fw))
        else:
            left = found.takeable(fw)
            lines.append(f'found: {found.waiting(fw)} fix units wait unstarted (the most is {found.FIX_MOST}); '
                         + (f'the main queue has {len(left)} to take' if left else 'the main queue is dry: a look would be queued'))
    except Exception:           # noqa: BLE001
        lines.append('found: failed: ' + traceback.format_exc().strip().splitlines()[-1][:300])
    return lines


def take_lock(home: Path, now=None):
    """One loop a machine: the lock file is made exclusively; one whose loop has not beaten for STALE is taken over.
    Returns '' when taken, else why not."""
    now = now or time.time()
    f = home / 'lock.json'
    for _ in range(2):
        try:
            fd = os.open(str(f), os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            with os.fdopen(fd, 'w', encoding='utf-8') as h:
                h.write(json.dumps(dict(pid=os.getpid(), at=now)))
            return ''
        except FileExistsError:
            beat = max(load(home / 'beat.json', {}).get('at', 0), load(f, {}).get('at', 0))
            if now - beat < STALE:
                return f'another loop holds {f} (pid {load(f, {}).get("pid")}, last beat {int(now - beat)} s ago)'
            try:
                os.remove(f)
            except OSError:
                return f'{f} is stale and could not be removed'
    return f'{f} could not be taken'


def main(argv=None):
    ap = argparse.ArgumentParser(description='when a relay\'s queue runs dry, it picks up work by itself')
    ap.add_argument('--once', action='store_true', help='one round, for real')
    ap.add_argument('--watch', type=float, default=0, help='seconds between rounds, for ever')
    ap.add_argument('--main', default=str(MAIN), help='the main board')
    ap.add_argument('--extras', default=','.join(map(str, EXTRAS)), help='the extra relays\' numbers (their boards tw3d-board-N); none for none')
    ap.add_argument('--home', default=str(HOME))
    a = ap.parse_args(argv)
    home = Path(a.home)
    home.mkdir(parents=True, exist_ok=True)
    extras = tuple(int(x) for x in a.extras.split(',') if x.strip().isdigit())      # 'none' for none
    if not (a.once or a.watch):
        print('\n'.join(round_(World(a.main, extras, home, acting=False))))
        return 0
    log = home / 'autoloop.log'

    def say(text):
        with open(log, 'a', encoding='utf-8') as f:
            f.write(f'{time.strftime("%m-%d %H:%M:%S")} {text}\n')
    why = take_lock(home)
    if why:
        say(f'not started (pid {os.getpid()}): {why}')
        print(why)
        return 0
    try:
        say(f'autoloop starts (pid {os.getpid()}), every {a.watch or 0:.0f} s, extras {",".join(map(str, extras)) or "none"}')
        while True:
            if (home / 'autoloop.stop').exists():
                say('autoloop.stop is in its folder: standing by')
            else:
                for line in round_(World(a.main, extras, home)):
                    say(line)
                    if a.once:
                        print(line)
            save(home / 'beat.json', dict(at=time.time(), pid=os.getpid()))
            if load(home / 'lock.json', {}).get('pid') != os.getpid():
                say('another loop has the lock: this one leaves')
                return 0
            if not a.watch:
                return 0
            time.sleep(max(30, a.watch))
    finally:
        if load(home / 'lock.json', {}).get('pid') == os.getpid():
            try:
                os.remove(home / 'lock.json')
            except OSError:
                pass


if __name__ == '__main__':
    sys.exit(main())
