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
            built (`nothing`): queue the unit, then close the brief. Answers in his own words (`write`) wait for a
            session, which must read them: they are counted, not guessed at
  feed      an extra relay that is not running and has nothing open on its board gets the open units of ONE free
            lane of the main board (no running relay works it; parked at 99 on the main board first), and is
            started. One that has open units and is not running is started too, with a growing wait when it
            stops again for the same reason (STARTS): a relay that stops at once is not started every round
  found     found.py take, then found.py rota: a finished look's findings become fix units (small) or cards for
            him (major); and when the main queue has run dry and fewer than FIX_MOST fix units wait, the next look
            is queued: first the review-index unit whose files changed most since its last review

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

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
RELAY = GH / 'githubtest-relay-run' / 'trench-warfare-3d' / 'Tools' / 'relay' / 'relay.py'
MAIN = GH / 'tw3d-board'
EXTRAS = tuple(range(2, 7))
START = Path(os.environ.get('TEMP') or tempfile.gettempdir()) / 'relayN-start-2026-10-13.ps1'
HOME = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'autoloop'
BY = 'autoloop'
DAY_PCT = os.environ.get('TW_DAY_PCT') or '11'   # the day's cap for every relay together (the owner, 2026-10-07); the start script counts all boards
PARKED = found.PARKED
STARTS = (1800, 14400)      # after a start, wait at least this long; when it stopped again for the same reason, double it, up to the second
INTEGRATION = found.INTEGRATION
NEEDS_RELAY = 'trench-warfare-3d/Tools/relay/relay.py'     # a unit that works on the relay needs it on its lane
WAITS = 'relay-harness'     # what such a unit waits for: the relay line landing on integration
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
    why = re.match(r'\s*Last run [^:]*: (.*?)(?: \(\d+ legs.*)?\.?$', m.group(1))
    return set(), (why.group(1) if why else m.group(1)).strip()[:200]


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
        return self.git(board, 'pull', '--rebase', '-q')

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
        flags = getattr(subprocess, 'DETACHED_PROCESS', 0) | getattr(subprocess, 'CREATE_NEW_PROCESS_GROUP', 0)
        p = subprocess.Popen(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', str(START), '-N', str(n), '-DayPct', DAY_PCT],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, creationflags=flags, close_fds=True)
        return f'started (pid {p.pid})'

    def answers(self):
        import briefs
        import notes
        return briefs.answers(briefs.read_all(briefs.folder()), notes.read_all(notes.folder()))

    def take(self, a, queued='', outcome=''):
        import briefs
        import notes
        briefs.take(briefs.folder(), notes.folder(), a['id'], by=BY, note=a['note'], queued=queued, outcome=outcome, board=self.main)

    def add(self, unit):
        f = Path(tempfile.mkdtemp(prefix='tw-autoloop-')) / (unit['id'] + '.json')
        f.write_text(json.dumps(unit, indent=1), encoding='utf-8')
        code, said = self.relay(self.main, 'add', '--unit', str(f))
        return code == 0 or 'queued already' in said, (said.splitlines() or [''])[-1][:300]

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

    def found(self):
        return found.World(found.build.REPO, self.main, self.home / 'found')


# ---- the four steps -------------------------------------------------------------------------------------------------------

def copy_back(w):
    """Done records of the extra boards that the main board has not committed are copied to it and pushed."""
    if w.acting:
        code, out = w.pull(w.main)
        if code:
            return [f'back: the main board did not pull ({out[:120]}): tried next round']
    have = w.tracked(w.main, 'relay/done')
    if have is None:
        return ['back: git did not list the main board\'s done records: tried next round']
    mine = w.main / 'relay' / 'done'
    new = []
    for n, board in sorted(w.extras.items()):
        for f in sorted((Path(board) / 'relay' / 'done').glob('*.json')):
            if f.name not in have and f.name not in {g.name for _, g in new}:
                new.append((n, f))
    if not new:
        return []
    if w.acting:
        mine.mkdir(parents=True, exist_ok=True)
        for _, f in new:
            if not (mine / f.name).exists():
                shutil.copyfile(f, mine / f.name)
    said = w.commit_push(w.main, ['relay/done/' + f.name for _, f in new],
                         f'relay: {len(new)} done records of the extra relays copied back ({", ".join(f.stem for _, f in new[:6])})')
    return [f'back: {len(new)} done records copied from the extra boards: {said}']


def take_answers(w):
    """His plain yes on the Decide page is carried out; his own words wait for a session. Each answer on its own:
    one that fails is tried again next round and does not hold up the others; its row is written once, after the
    brief is closed."""
    out, write = [], 0
    for a in w.answers():
        if a['go'] == 'write':
            write += 1
            continue
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
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Queued as `{unit["id"]}`. (owner, Decide page, note {a["note"]}) |')
                out.append(f'answers: {a["id"]}: his yes queued {unit["id"]} and the brief is closed')
            else:
                if not w.acting:
                    out.append(f'answers: {a["id"]}: would close it, nothing is built')
                    continue
                w.take(a, outcome=a.get('says') or 'his option builds nothing')
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Nothing is built. (owner, Decide page, note {a["note"]}) |')
                out.append(f'answers: {a["id"]}: his answer builds nothing; the brief is closed')
        except Exception as e:      # noqa: BLE001  e.g. take refuses while the unit's push has not reached the board's origin
            out.append(f'answers: {a["id"]}: not closed this round ({str(e)[:200]})')
    if write:
        out.append(f'answers: {write} in his own words wait for a session (briefs.py waiting)')
    return out


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


def feed(w):
    """Every extra relay that is not running gets work and is started."""
    out = []
    lanes = set()
    for board in [w.main, *w.extras.values()]:
        got, _ = w.status(board)
        if got is None:
            return ['feed: a relay did not say whether it runs: nothing is fed this round']
        lanes |= got
    held = {v.get('lane') for b in w.extras.values() for v in open_units(b).values()} | lanes
    q, done = queue_of(w.main)
    for n, board in sorted(w.extras.items()):
        running, reason = w.status(board)
        if running:
            continue
        mine = open_units(board)
        if not mine:
            free = {}
            for uid, u in q.items():
                if uid in done or found.prio(u) >= PARKED or not u.get('lane') or u['lane'] in held:
                    continue
                free.setdefault(u['lane'], []).append((found.prio(u), uid, u))
            if not free:
                continue
            lane = min(free, key=lambda k: (min(p for p, _, _ in free[k]), k))
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
        if not w.acting:
            out.append(f'feed: relay {n} is not running and has work: it would be started')
            continue
        if not may_start(w, n, reason):
            continue
        out.append(f'feed: relay {n} is not running and has work: {w.start(n)}')
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
            if f'{lane}@{sha}' in done or lane in busy or (lane, sha) in seen:
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


def round_(w):
    lines = []
    for step in (copy_back, take_answers, wait_for_relay, rescue, feed):
        try:
            lines += step(w)
        except Exception:       # noqa: BLE001  one step that fails must not stop the others
            lines.append(f'{step.__name__}: failed: ' + traceback.format_exc().strip().splitlines()[-1][:300])
    fw = w.found()
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
    ap.add_argument('--extras', default=','.join(map(str, EXTRAS)), help='the extra relays\' numbers (their boards tw3d-board-N); empty for none')
    ap.add_argument('--home', default=str(HOME))
    a = ap.parse_args(argv)
    home = Path(a.home)
    home.mkdir(parents=True, exist_ok=True)
    extras = tuple(int(x) for x in a.extras.split(',') if x.strip())
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
