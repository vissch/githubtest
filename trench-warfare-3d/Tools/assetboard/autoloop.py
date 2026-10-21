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
import calendar
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
STUB_AGE = 7200             # a Unity a unity.cmd stub started is ended after this long when its stub is gone or still waits
PS_LIST = ("$all = @{}; Get-CimInstance Win32_Process | ForEach-Object { $all[[int]$_.ProcessId] = $_ }; "
           "$now = Get-Date; $all.Values | Where-Object { $_.Name -in 'Unity.exe','cmd.exe' } | ForEach-Object { "
           "$p = $all[[int]$_.ParentProcessId]; $gone = (-not $p) -or ($p.CreationDate -gt $_.CreationDate); "
           "[pscustomobject]@{pid=[int]$_.ProcessId; ppid=[int]$_.ParentProcessId; name=$_.Name; cmd=[string]$_.CommandLine; "
           "age=[int]($now - $_.CreationDate).TotalSeconds; parent=$(if ($gone) {''} else {$p.Name}); "
           "parent_cmd=$(if ($gone) {''} else {[string]$p.CommandLine})} } | ConvertTo-Json -Compress")


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

    def start(self, n, sources='lane', quiet_off=False):
        if not self.acting:
            return 'would start'
        flags = getattr(subprocess, 'CREATE_NO_WINDOW', 0) | getattr(subprocess, 'CREATE_NEW_PROCESS_GROUP', 0)   # DETACHED_PROCESS: powershell runs nothing
        if n == 1:                  # the main relay: its hold first, then a run under the usual day (no figure passed)
            code, said = self.relay(self.main, 'hold', MAIN_WHO, '--hours', '0.25')     # long enough to start; given back when idle
            if code:
                return 'not started: ' + (said.splitlines() or ['the hold was refused'])[0][:160]
            cmd = [str(MAIN_START), '-Who', MAIN_WHO, '-Sources', sources] + (['-NoQuiet'] if quiet_off else [])
        else:
            cmd = [str(START), '-N', str(n), '-DayPct', DAY_PCT] + (['-NoQuiet'] if quiet_off else [])
        p = subprocess.Popen(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', *cmd],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, creationflags=flags, close_fds=True)
        return f'started (pid {p.pid})'

    def pipeline_work(self):
        """True when the main relay's runner would take a job off the pipeline board now (its own dry run says so)."""
        code, said = self.relay(self.main, 'run', '--work', str(GH / 'githubtest-relay-work'), '--dry-run', '--sources', 'pipeline')
        return code == 0 and said.lstrip().startswith('Would run: pipeline')

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

    def main_touch(self, n=1):
        """(the newest write to relay n's work checkout's git index or HEAD, the end of its newest run that had legs),
        in epoch seconds, 0 when unread. Read only."""
        work, newest, ended = self.works().get(n), 0.0, 0.0
        if work:
            code, gd = self.git(work, 'rev-parse', '--absolute-git-dir')
            if code == 0:
                newest = max((f.stat().st_mtime for f in (Path(gd) / 'index', Path(gd) / 'HEAD') if f.exists()), default=0.0)
        for f in sorted(Path(self.main if n == 1 else self.extras[n]).glob('relay/*/stops/*.json'), key=lambda p: p.name)[-12:]:
            d = load(f, {})
            if isinstance(d, dict) and d.get('legs') and d.get('stopped_at'):
                try:
                    ended = max(ended, calendar.timegm(time.strptime(d['stopped_at'][:19], '%Y-%m-%dT%H:%M:%S')))
                except ValueError:
                    pass
        return newest, ended

    def landed(self, lane):
        """True when origin/<lane> is on the integration branch and its tip is over a day old: its work has landed.
        A lane made from integration today and not yet worked on is not "landed"."""
        ref = f'origin/{lane}'
        if not lane or self.repo('rev-parse', '--verify', '-q', ref)[0]:
            return False
        if self.repo('merge-base', '--is-ancestor', ref, f'origin/{INTEGRATION}')[0]:
            return False
        code, t = self.repo('log', '-1', '--format=%ct', ref)
        return code == 0 and t.strip().isdigit() and self.now() - int(t.strip()) > 86400

    def main_last(self):
        """(the main relay's last stopped run, who holds the relay now or '') from its status text."""
        code, text = self.relay(self.main, 'status')
        run = re.search(r'^NOT RUNNING\. Last run (\S+?):', text or '', re.M)
        who = re.search(r'held by (\S+) until', text or '')
        return (run.group(1) if run else ''), (who.group(1) if who else '')

    def found(self):
        return found.World(found.build.REPO, self.main, self.home / 'found', also=self.extras.values())

    def procs(self):
        """[{pid, ppid, name, cmd, age (s), parent (its name, '' when gone), parent_cmd}] of every Unity.exe and cmd.exe."""
        r = subprocess.run(['powershell', '-NoProfile', '-Command', PS_LIST], capture_output=True, text=True, encoding='utf-8',
                           errors='replace', stdin=subprocess.DEVNULL, timeout=120, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
        got = json.loads(r.stdout or '[]') if r.returncode == 0 else []
        return [got] if isinstance(got, dict) else got

    def kill(self, pid):
        r = subprocess.run(['taskkill', '/PID', str(pid), '/T', '/F'], capture_output=True, text=True, errors='replace',
                           stdin=subprocess.DEVNULL, timeout=60, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
        return r.returncode, (r.stdout + r.stderr).strip()

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

    def unqueue(self, board, uid, why='parked by the loop (no progress in its tries)'):
        """Take a unit off a board's queue: git rm, committed (pushed on the main board). True when it is off."""
        rel = f'relay/queue/{uid}.json'
        if not (Path(board) / rel).exists():
            return True
        if self.git(board, 'rm', '-q', '--', rel)[0]:
            return False
        code, said = self.git(board, 'commit', '-q', '-m', f'relay: {uid} {why}', '--', rel)   # that path only, never the whole index
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
            if w.acting:        # his notes as they stand now: the list read at the round's start goes stale while 34 units are queued
                now = [x for x in w.answers() if x['id'] == a['id']]   # one at a time, and two clicks seconds apart (22:21, 10-17)
                if not now:
                    continue
                a = now[0]      # its note is his newest; notes on two options make it 'write', a leg's
                if a['go'] not in ('queue', 'nothing'):
                    continue
            if a['go'] == 'queue':
                unit = dict(a['unit'] or {})
                unit.setdefault('role', 'lane')
                unit.pop('priority', None)
                if not w.acting:
                    out.append(f'answers: {a["id"]}: would queue {unit.get("id")} and close the brief')
                    continue
                ok, said = w.add(unit)
                if not ok and 'is done already' in str(said):
                    # The id is a finished unit, so the same yes is refused every round
                    # (autoloop.log 2026-10-20 16:18). Queue it as <id>-2, or <id>-3
                    # when -2 is queued or done, and close the brief on that id.
                    base = str(unit.get('id') or '')
                    held = w.ids()
                    alt = f'{base}-2' if f'{base}-2' not in held else f'{base}-3'
                    if base and alt not in held:
                        retry = dict(unit, id=alt)
                        ok, said = w.add(retry)
                        if ok:
                            unit = retry
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


def may_start(w, n, reason, fresh=False):
    """True when relay n may be started now: STARTS[0] after its last start, doubled each time it stopped again for
    the same reason, at most STARTS[1]. Remembered in the loop's folder. `fresh`: something changed this round (it was
    fed new work, or its lane was freed), so the wait its last stop earned does not hold it back: relay 2, fed at
    22:57 after empty runs had doubled its wait to 4 h, was not started until 01:43 (2026-10-18); the main relay, its
    lane freed at 22:57, not until 00:03."""
    starts = load(w.home / 'starts.json', {})
    s = starts.get(str(n))
    if s and not fresh and w.now() - s.get('at', 0) < s.get('wait', STARTS[0]):
        return False
    same = bool(s and reason and reason == s.get('reason') and not fresh)
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
    if w.acting:                            # the loop's own touch of relay m's checkout: not "somebody works in it"
        save(w.home / 'touched.json', dict(load(w.home / 'touched.json', {}), **{'main' if m == 1 else str(m): w.now()}))
    out.append(f'feed: relay {n}: {lane} freed: {at} (relay {m}, not running, clean) was detached from it')
    w.freed = getattr(w, 'freed', 0) + 1
    return True


def outside(w, lane, cos):
    """True when `lane` is checked out in a checkout that is no relay's work checkout (a session's): the loop never frees it."""
    at = cos.get(lane)
    if not at:
        return False
    key = lambda p: os.path.normcase(os.path.abspath(str(p)))      # noqa: E731
    return key(at) not in {key(p) for p in w.works().values()}


def all_done(w):
    """The ids done on any board. An extra relay's done records reach the main board only between main runs (a leg
    that sees relay/done move under it ends its run), so the loop does not wait for that copy to know them."""
    return set().union(*(queue_of(b)[1] for b in [w.main, *w.extras.values()]))


QUIET_OWN = (90, 30)    # a write this soon after the main relay's own last run ended, or after the loop detached its checkout


def own_touch(w, n=1):
    """True when the main work checkout's last git write was the relay's own (its last run's legs) or the loop's
    (a detach), so its ten quiet minutes guard against nobody: each main start after a run was refused "git index
    moved 222 s ago" by that run's own last commit (08:03, 2026-10-18), and 12 s after the loop's own detach (04:54)."""
    newest, ended = w.main_touch(n)     # the extras too: relays 2, 3 and 5 were refused so right after a feed (11:08-11:16, 2026-10-18)
    mine = load(w.home / 'touched.json', {}).get('main' if n == 1 else str(n), 0)
    if not newest or w.now() - newest >= QUIET:
        return False
    return newest <= max(ended + QUIET_OWN[0] if ended else 0, mine + QUIET_OWN[1] if mine else 0)


def sweep(w):
    """Queue files on an idle extra board whose unit is done (on that board or the main one) are taken off it, in one
    local commit: relay 2's board held about 60 of them (old rv-*, look-*, done units)."""
    out, main_done = [], queue_of(w.main)[1]
    for n, board in sorted(w.extras.items()):
        q, done = queue_of(board)
        stale = sorted(u for u in q if u in done or u in main_done)
        if not stale or w.status(board)[0] != set():
            continue
        if not w.acting:
            out.append(f'feed: relay {n}: {len(stale)} queue files of done units would be taken off its board')
            continue
        for u in stale:
            (Path(board) / 'relay' / 'queue' / f'{u}.json').unlink(missing_ok=True)
        try:
            said = w.commit_push(board, [f'relay/queue/{u}.json' for u in stale], f'relay: {len(stale)} queue files of done units taken off', push=False)
        except RuntimeError as e:
            said = str(e)[:120]
        out.append(f'feed: relay {n}: {len(stale)} queue files of done units taken off its board ({said})')
    return out


def retire(w, out):
    """{unit: why} of the open units that are no work any more: a later copy of them (x-b, or a unit on <lane>-b) is
    queued or done on any board, or their lane has landed and they failed there since. They come off the extra boards and are parked on the main
    one, once: relay-harness-rebase-b (its lane landed 10-15) and review-tools-rebase ran again on 10-18."""
    boards = [w.main, *w.extras.values()]
    seen, units = {}, {}
    for b in boards:
        q, d = queue_of(b)
        for uid, u in q.items():
            units.setdefault(uid, u)
        for uid in d:
            units.setdefault(uid, q.get(uid) or load(Path(b) / 'relay' / 'done' / f'{uid}.json', {}))
    best = {}
    for uid, u in units.items():
        lane = str((u or {}).get('lane') or '')
        fid = loopsteps.family(uid)[0]
        best[('id', fid)] = max(best.get(('id', fid), 0), loopsteps.family(uid)[1])
        if lane:                            # the ids of the units on each copy of a lane: a later lane copy supersedes only its own kin
            best.setdefault(('lanes', loopsteps.family(lane)[0]), []).append((loopsteps.family(lane)[1], fid))
    done = all_done(w)
    state, tries = load(w.home / 'retired.json', {}), load(w.home / 'attempts.json', {})
    gone, landed = {}, {}
    on = w.main_on()[0]
    for b in boards:
        if Path(b) != Path(w.main) and w.status(b)[0] != set():    # an extra relay that runs (or did not say) keeps its board as it
            continue                        # is: it may be on that unit. The main board's queue is only re-prioritised, as feed does under a run
        for uid, u in open_units(b).items():
            if uid in done or (on and loopsteps.family(uid)[0] == loopsteps.family(on)[0]):
                continue
            lane = str(u.get('lane') or '')
            fid, fl = loopsteps.family(uid), loopsteps.family(lane)
            why = ''
            if best.get(('id', fid[0]), 0) > fid[1]:
                why = 'a later copy of it is queued or done'
            elif lane and any(r > fl[1] and (f.startswith(fid[0]) or fid[0].startswith(f))       # review-tools-b supersedes
                              for r, f in best.get(('lanes', fl[0]), [])):                  # review-tools-rebase; an unrelated unit stays
                why = f'a unit on a later copy of {lane} is queued or done'
            elif lane and (tries.get(uid) or {}).get('fails'):     # tried and failed since: a new unit on an old lane is work
                if lane not in landed:
                    landed[lane] = w.landed(lane)
                if landed[lane]:
                    why = f'{lane} has landed on the integration branch'
            if not why:
                continue
            gone[uid] = why
            if not w.acting:
                continue
            if Path(b) != Path(w.main):
                w.unqueue(b, uid, why='off this relay: ' + why)
            elif found.prio(u) < PARKED:
                w.prio(uid, PARKED)
            if uid not in state:
                state[uid] = why
                out.append(f'feed: {uid} is not fed: {why}; taken off the extra boards and parked on the main one')
    if w.acting:
        save(w.home / 'retired.json', state)
    return gone


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
    out += sweep(w)
    gone = retire(w, out)
    held = {v.get('lane') for b in w.extras.values() for v in open_units(b).values()} | lanes
    kin = {loopsteps.work_key(u, v) for b in w.extras.values() for u, v in open_units(b).items()}   # x and x-b are one piece of work
    kin_lanes = {loopsteps.family(l)[0] for l in lanes}
    q, done = queue_of(w.main)
    done = done | all_done(w) | set(gone)
    waiting = load(w.home / 'lane_waits.json', {})          # {unit: lane} taken off an extra relay because its lane was held
    for uid, lane in sorted(waiting.items()):
        if uid in done or uid not in q:
            waiting.pop(uid)
        elif not outside(w, lane, cos) and lane not in lanes and lane not in held and w.acting and w.prio(uid, 50)[0] == 0:
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
        fresh = False
        if running:
            continue
        latest = {}
        for b in [w.main, *w.extras.values()]:
            for u, v in open_units(b).items():
                k = loopsteps.work_key(u, v)
                latest[k] = max(latest.get(k, 0), loopsteps.family(u)[1])
        mine = {u: v for u, v in open_units(board).items() if u not in done                  # done on the main board: not work
                and loopsteps.family(u)[1] >= latest.get(loopsteps.work_key(u, v), 0)}       # nor a unit a later copy (x-b) replaces
        stuck = {u: v for u, v in mine.items() if u in q and v.get('lane') and v['lane'] not in lanes and outside(w, v['lane'], cos)}   # a relay's checkout is freed when its relay stops; a unit with no main copy stays
        if stuck and w.acting:                  # its lane is held where the loop cannot free it: the relay is not left standing on it
            for uid, v in sorted(stuck.items()):
                if w.unqueue(board, uid, why='off this relay: its lane is held by a checkout that is no relay\'s'):
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
            reason, fresh = '', True
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
        if not may_start(w, n, reason, fresh=bool(not mine and fresh) or n in getattr(w, 'tidied', set())):
            continue
        own = own_touch(w, n)
        out.append(f'feed: relay {n} is not running and has work: {w.start(n, quiet_off=own)}'
                   + (' (its checkout was last written by its own run or the loop: the quiet wait is skipped)' if own else ''))
        runs[n] = {'started this round'}        # its checkout is no longer free to detach in this round
    running, reason = w.status(w.main)          # the main relay: started when it has a unit to take
    q, done = queue_of(w.main)
    done = done | all_done(w) | set(gone)
    extra_kin = {loopsteps.work_key(u, v) for b in w.extras.values() for u, v in open_units(b).items()}
    work = any(u not in done and found.prio(v) < PARKED and v.get('lane') not in held and loopsteps.work_key(u, v) not in extra_kin
               for u, v in q.items())                  # nor the copy of a unit an extra relay holds
    started = False
    s1 = load(w.home / 'starts.json', {}).get('1') or {}
    pipe = running == set() and not work and (not s1 or w.now() - s1.get('at', 0) >= s1.get('wait', STARTS[0])) and w.pipeline_work()   # its dry run only when a start may follow   # nothing on its queue it can take: the pipeline board's routed
    #   ideas are its work (option A, the owner 2026-10-17: everything accepted; 11 ideas waited). Pipeline only: a lane source
    #   would take the units held back above (parked, a lane an extra relay holds, an extra's copy)
    w.freed = 0
    if running == set() and work:               # the lanes it will take, held by a stopped extra relay's clean checkout, are freed first
        for lane in sorted({v.get('lane') for u, v in q.items() if u not in done and found.prio(v) < PARKED and v.get('lane')} - held):
            if lane in cos:
                lane_free(w, 1, lane, cos, runs, out)    # its 12:42 run had 4 FAILs that were only these refusals (2026-10-16)
    if running == set() and work and w.acting and 'uncommitted changes' in reason and 1 in w.works():
        starts = load(w.home / 'starts.json', {})       # a leg left files in the main checkout (a staged decisions.md, 15:25):
        run, holder = w.main_last()                     # put aside as the extras' are, once per stopped run, never under another's hold
        if holder and holder != MAIN_WHO:
            out.append(f'feed: the main relay stopped on leftover files; {holder} holds it, so they are left alone')
            work = False
        elif run and run not in load(w.home / 'asides.json', []):
            ok, said = w.put_aside(w.works()[1], 1)
            out.append(f'feed: the main relay stopped on leftover files (run {run}): {said}')
            if ok:
                save(w.home / 'asides.json', (load(w.home / 'asides.json', []) + [run])[-50:])
                starts['1'] = dict(at=w.now(), reason='put aside', wait=QUIET)
                save(w.home / 'starts.json', starts)
            work = False
    if running == set() and (work or pipe):
        what = 'has work' if work else 'has no queued unit it can take: the pipeline board has a job'
        if not w.acting:
            out.append(f'feed: the main relay is not running and {what}: it would be started')
        else:
            before = load(w.home / 'starts.json', {}).get('1')
            if not may_start(w, 1, reason, fresh=bool(w.freed)):
                s1 = before or {}
                out.append('feed: the main relay is not running and has work; its next start waits until '
                           + time.strftime('%H:%M', time.localtime(s1.get('at', 0) + s1.get('wait', STARTS[0])))
                           + f' (it stopped: {str(s1.get("reason") or "-")[:100]})')
            else:
                own = own_touch(w)
                said = w.start(1, quiet_off=own) if work else w.start(1, sources='pipeline', quiet_off=own)
                if own:
                    said += ' (the checkout was last touched by the relay or the loop itself: its quiet wait is skipped)'
                out.append(f'feed: the main relay is not running and {what}: {said}')
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


def project_of(cmd):
    """The -projectPath of a Unity or unity.cmd command line, as a lower-case path with forward slashes, or ''."""
    m = re.search(r'-projectpath\s+(?:"([^"]+)"|(\S+))', cmd or '', re.I)
    return (m.group(1) or m.group(2)).replace('\\', '/').rstrip('/').lower() if m else ''


def relay_of(project, works):
    """The relay number whose work checkout holds this project, or 0 (another project: never touched)."""
    for n, work in works.items():
        root = str(work).replace('\\', '/').rstrip('/').lower()
        if project == root or project.startswith(root + '/'):
            return n
    return 0


def end_editors(w):
    """End the Unity editors a relay leg left open: a Unity.exe on a relay's work checkout once that relay's run has
    stopped, and a Unity a unity.cmd stub started that is older than STUB_AGE while its stub is gone or still waits
    (with that stub); then the project's Temp/UnityLockfile goes. Relay 4 refused to start for a day on an editor left
    open on githubtest-relay-work4 (10-16 17:59); three stubs had hung since 10-09. Unity on any other project is left alone."""
    works = w.works()
    if not works:
        return []
    boards = {1: w.main, **w.extras}
    procs = w.procs()
    stopped = {}
    out, ended, gone = [], {}, set()

    def is_stub(p):
        return (p.get('name') or '').lower() == 'cmd.exe' and 'unity.cmd' in (p.get('cmd') or '').lower()
    for p in procs:
        if (p.get('name') or '').lower() != 'unity.exe':
            continue
        project = project_of(p.get('cmd'))
        n = relay_of(project, works) if project else 0
        if not n:
            continue
        stub = (p.get('parent') or '').lower() == 'cmd.exe' and 'unity.cmd' in (p.get('parent_cmd') or '').lower()
        why = ''
        if (p.get('age') or 0) > STUB_AGE and (stub or not p.get('parent')):
            why = f'started by a unity.cmd stub {int(p["age"]) // 3600} h ago, ' + ('the stub still waits' if stub else 'its parent is gone')
        else:
            if n not in stopped:
                b = boards.get(n)
                stopped[n] = b is not None and w.status(b)[0] == set()     # None (status unread) is not "stopped"
            if stopped[n]:
                why = f'relay {n}\'s run has stopped'
        if not why:
            continue
        code, said = w.kill(p['pid'])            # Unity first: taskkill /T on the stub would take it, and its kill would then fail
        out.append(f'editors: ended Unity {p["pid"]} on {project} ({why})' if code == 0 else f'editors: Unity {p["pid"]} on {project} did not end: {said[:200]}')
        if code == 0:
            ended[project] = n
            gone.add(p['pid'])
        if stub and w.kill(p['ppid'])[0] == 0:
            out.append(f'editors: ended the unity.cmd stub {p["ppid"]} of Unity {p["pid"]}')
    for p in procs:                              # stubs left with no Unity, waiting on a relay's project for over STUB_AGE
        project = project_of(p.get('cmd'))
        if is_stub(p) and (p.get('age') or 0) > STUB_AGE and project and relay_of(project, works) \
                and not any(q.get('ppid') == p.get('pid') and (q.get('name') or '').lower() == 'unity.exe' for q in procs):
            if w.kill(p['pid'])[0] == 0:
                out.append(f'editors: ended the unity.cmd stub {p["pid"]} on {project}, waiting {int(p["age"]) // 3600} h')
                ended[project] = relay_of(project, works)
    for project in ended:
        lock = Path(project) / 'Temp' / 'UnityLockfile'
        if any(project_of(q.get('cmd')) == project and q.get('pid') not in gone for q in procs if (q.get('name') or '').lower() == 'unity.exe'):
            continue
        try:
            if lock.exists():
                lock.unlink()
                out.append(f'editors: removed {lock}')
        except OSError as e:
            out.append(f'editors: {lock} could not be removed: {e}')
    return out


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


def held_looks(w):
    """Open units on the main board kept for the main relay's second try while it runs on something else, and on no
    extra board: no idle relay can take them now."""
    running, _ = w.status(w.main)
    if not running:
        return set()
    tries, on = loopsteps.load(w.home / 'attempts.json', {}), w.main_on()[0]
    extra = {u for b in w.extras.values() for u in open_units(b)}
    return {u for u in open_units(w.main) if (tries.get(u) or {}).get('main_try') and u != on and u not in extra}


def idle_why(w, lines):
    """'' or why a look is queued although the main queue still has units: an extra relay stands idle with an empty
    board and this round fed it nothing, so what the queue holds is nothing an idle relay can take (15:41-16:25,
    2026-10-16: one unit kept for the busy main relay's second try held five relays idle for 40 minutes)."""
    if any(': it gets ' in l or 'would get' in l for l in lines):
        return ''
    for n, board in sorted(w.extras.items()):
        if w.status(board)[0] == set() and not open_units(board):
            return f'relay {n} is idle and no queued unit is one it can take'
    return ''


def tidy(w):
    """Every stopped extra relay lets go of its lane at once (a clean checkout is detached), so the next look or unit on
    that lane is not held by it (every look is on lane/show/found; relays 2 and 5 kept theirs, 2026-10-18 17:50); a
    stopped one left dirty by a run that ended otherwise (relay 3, a TIMEOUT at 15:56, 8 files) is put aside once per
    stopped run, so it can be fed and started again. The relays it put aside are kept in w.tidied: feed starts them in the
    same round, past their quiet wait and their last start's wait (relays 3-6 put aside at 22:10 were refused "git index
    moved 28 s ago" and only started at 22:43, 2026-10-18). A detach is the loop's own write too (touched.json), so it
    skips the quiet wait, but not the start wait: a relay that stops at once would else be restarted every round."""
    w.tidied = set()
    out, cos = [], w.checkouts()
    if cos is None:
        return out
    key = lambda p: os.path.normcase(os.path.abspath(str(p)))      # noqa: E731
    on = {key(p): b for b, p in cos.items()}
    asides = load(w.home / 'asides.json', [])
    for n, work in sorted(w.works().items()):
        if n == 1 or n not in w.extras:
            continue
        running, reason = w.status(w.extras[n])
        if running != set():
            continue
        if not w.clean(work):
            m = re.search(r'\[run (\S+)\]', reason or '')
            run = f'{n}:' + (m.group(1) if m else (reason or '')[:60])
            if 'uncommitted changes' in (reason or '') or run in asides:
                continue                    # feed puts that one aside when it has work; once per stopped run here
            if not w.acting:
                out.append(f'tidy: relay {n} stopped with leftover files: they would be put aside')
                continue
            ok, said = w.put_aside(work, n)
            asides = (asides + [run])[-50:]
            save(w.home / 'asides.json', asides)
            if ok:                          # the stash is the loop's own write: feed starts it this round past the quiet wait
                save(w.home / 'touched.json', dict(load(w.home / 'touched.json', {}), **{str(n): w.now()}))
                w.tidied.add(n)
            out.append(f'tidy: relay {n} stopped with leftover files ({str(reason)[:80]}): {said}')
            continue
        lane = on.get(key(work))
        if not lane:
            continue
        if not w.acting:
            out.append(f'tidy: relay {n} would let go of {lane}')
            continue
        code, said = w.detach(work)
        if code == 0:
            save(w.home / 'touched.json', dict(load(w.home / 'touched.json', {}), **{str(n): w.now()}))
        out.append(f'tidy: relay {n} stopped: let go of {lane}' if code == 0 else f'tidy: relay {n} could not let go of {lane}: {said[:120]}')
    return out


def round_(w):
    lines = []
    for step in (copy_back, take_answers, attempts, end_editors, wait_for_relay, rescue, tidy, loopsteps.route_ideas, feed):
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
            why = idle_why(w, lines)
            lines.append(found.rota(fw, why=why, held=held_looks(w) if why else ()))
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
