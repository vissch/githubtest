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
            lane of the main board (no other relay holds it; parked at 99 on the main board), and is started.
            One that has open units and is not running is started too, whatever its last stop said
  found     found.py take, then found.py rota: a finished look's findings become fix units (small) or cards for
            him (major); and when the main queue has run dry and fewer than FIX_MOST fix units wait, the next look
            is queued: first the review-index unit whose files changed most since its last review

Every unit goes through the relay's own `add`, and runs under the relay's day (11% of the week, spent evenly) and
its shares (the 25% fix share for review fixes, 15% for looks): this file starts no model and spends nothing itself.
A file autoloop.stop in its folder makes it stand by.
"""
import argparse
import json
import os
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


def load(f: Path, default):
    try:
        return json.loads(Path(f).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def queue_of(board: Path):
    """({id: unit} queued, {id} done) of one board's relay folder."""
    r = Path(board) / 'relay'
    q = {f.stem: load(f, {}) for f in sorted((r / 'queue').glob('*.json'))} if (r / 'queue').is_dir() else {}
    done = {f.stem for f in (r / 'done').glob('*.json')} if (r / 'done').is_dir() else set()
    return q, done


def open_units(board: Path):
    q, done = queue_of(board)
    return {u: v for u, v in q.items() if u not in done}


# ---- the outside world ---------------------------------------------------------------------------------------------------

class World:
    """Everything that reads or writes outside this file; the tests put a stand-in in its place."""

    def __init__(self, main=MAIN, extras=EXTRAS, home=HOME, acting=True):
        self.main, self.home, self.acting = Path(main), Path(home), acting
        self.extras = {n: GH / f'tw3d-board-{n}' for n in extras if (GH / f'tw3d-board-{n}').is_dir()}

    def git(self, board, *a):
        r = subprocess.run(['git', '-C', str(board), *a], capture_output=True, stdin=subprocess.DEVNULL)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def relay(self, board, *a):
        env = dict(os.environ, TW_BOARD=str(board))
        n = next((k for k, b in self.extras.items() if Path(b) == Path(board)), 0)
        if n:
            env['TW_RELAY_HOME'] = str(Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / f'relay{n}')
        else:
            env.pop('TW_RELAY_HOME', None)
        r = subprocess.run([sys.executable, str(RELAY), *a], capture_output=True, text=True, encoding='utf-8', errors='replace',
                           stdin=subprocess.DEVNULL, env=env, timeout=600)
        return r.returncode, (r.stdout + r.stderr).strip()

    def running(self, n):
        code, text = self.relay(self.extras[n], 'status')
        return text.startswith('RUNNING')

    def main_on(self):
        """The unit the main relay works now, or ''."""
        code, text = self.relay(self.main, 'status')
        for line in text.splitlines():
            if ' now on ' in line:
                return line.split(' now on ', 1)[1].split()[0].strip(',.')
        return ''

    def pull(self, board):
        return self.git(board, 'pull', '--rebase', '-q')

    def commit_push(self, board, paths, message, push=True):
        if not self.acting:
            return 'would commit: ' + message
        self.git(board, 'add', '--', *paths)
        if self.git(board, 'diff', '--cached', '--quiet')[0] == 0:
            return ''
        self.git(board, 'commit', '-q', '-m', message)
        if not push:
            return 'committed (kept local)'
        for _ in range(3):
            if self.git(board, 'push', '-q')[0] == 0:
                return 'pushed'
            self.pull(board)
        return 'the push was refused three times: it is tried again next round'

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

    def found(self):
        return found.World(found.build.REPO, self.main, self.home / 'found')


# ---- the four steps -------------------------------------------------------------------------------------------------------

def copy_back(w):
    """Done records of the extra boards that the main board lacks are copied to it and pushed."""
    if w.acting:
        w.pull(w.main)
    mine = w.main / 'relay' / 'done'
    new = []
    for n, board in sorted(w.extras.items()):
        for f in sorted((Path(board) / 'relay' / 'done').glob('*.json')):
            if not (mine / f.name).exists():
                new.append((n, f))
    if not new:
        return []
    if w.acting:
        mine.mkdir(parents=True, exist_ok=True)
        for _, f in new:
            shutil.copyfile(f, mine / f.name)
    said = w.commit_push(w.main, ['relay/done/' + f.name for _, f in new],
                         f'relay: {len(new)} done records of the extra relays copied back ({", ".join(f.stem for _, f in new[:6])})')
    return [f'back: {len(new)} done records copied from the extra boards: {said}']


def take_answers(w):
    """His plain yes on the Decide page is carried out; his own words wait for a session."""
    out, write = [], 0
    for a in w.answers():
        if a['go'] == 'write':
            write += 1
            continue
        if a['go'] == 'queue':
            unit = dict(a['unit'] or {})
            unit.setdefault('role', 'lane')
            unit.pop('priority', None)
            ok, said = w.add(unit) if w.acting else (True, 'would queue')
            if not ok:
                out.append(f'answers: {a["id"]}: the relay refused {unit.get("id")}: {said}')
                continue
            w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Queued as `{unit["id"]}`. (owner, Decide page, note {a["note"]}) |')
            if w.acting:
                w.take(a, queued=unit['id'])
            out.append(f'answers: {a["id"]}: his yes queued {unit["id"]} and the brief is closed')
        elif a['go'] == 'nothing':
            w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a["option"]}: {a["text"]}** Nothing is built. (owner, Decide page, note {a["note"]}) |')
            if w.acting:
                w.take(a, outcome=a.get('says') or 'his option builds nothing')
            out.append(f'answers: {a["id"]}: his answer builds nothing; the brief is closed')
    if write:
        out.append(f'answers: {write} in his own words wait for a session (briefs.py waiting)')
    return out


def feed(w):
    """Every extra relay that is not running gets work and is started."""
    out = []
    held = {v.get('lane') for b in w.extras.values() for v in open_units(b).values()}
    on = w.main_on()
    q, done = queue_of(w.main)
    on_lane = (q.get(on) or {}).get('lane', '')
    for n, board in sorted(w.extras.items()):
        if w.running(n):
            continue
        mine = open_units(board)
        if not mine:
            free = {}
            for uid, u in q.items():
                if uid in done or found.prio(u) >= PARKED or not u.get('lane') or u['lane'] in held or u['lane'] == on_lane:
                    continue
                free.setdefault(u['lane'], []).append((found.prio(u), uid, u))
            if not free:
                continue
            lane = min(free, key=lambda k: (min(p for p, _, _ in free[k]), k))
            units = sorted(free[lane])
            if w.acting:
                qd = Path(board) / 'relay' / 'queue'
                qd.mkdir(parents=True, exist_ok=True)
                for _, uid, u in units:
                    u = dict(u)
                    u.pop('priority', None)
                    (qd / f'{uid}.json').write_text(json.dumps(u, indent=2, ensure_ascii=False) + '\n', encoding='utf-8', newline='\n')
                w.commit_push(board, ['relay/queue/' + f'{uid}.json' for _, uid, _ in units], f'relay: {len(units)} units of {lane} for extra relay {n}, kept local', push=False)
                for _, uid, _ in units:
                    w.prio(uid, PARKED)
            held.add(lane)
            out.append(f'feed: relay {n} was empty: it gets {lane} ({", ".join(uid for _, uid, _ in units)}), parked at {PARKED} on the main board')
        out.append(f'feed: relay {n} is not running and has work: {w.start(n)}')
    return out


def round_(w):
    lines = []
    for step in (copy_back, take_answers, feed):
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


def take_lock(home: Path):
    f = home / 'lock.json'
    pid = load(f, {}).get('pid')
    if pid and pid != os.getpid():
        try:
            out = subprocess.run(['tasklist', '/FI', f'PID eq {pid}', '/NH'], capture_output=True, text=True).stdout
            if str(pid) in out:
                return False
        except OSError:
            pass
    f.write_text(json.dumps(dict(pid=os.getpid(), at=time.time())), encoding='utf-8')
    return True


def main(argv=None):
    ap = argparse.ArgumentParser(description='when a relay\'s queue runs dry, it picks up work by itself')
    ap.add_argument('--once', action='store_true', help='one round, for real')
    ap.add_argument('--watch', type=float, default=0, help='seconds between rounds, for ever')
    ap.add_argument('--main', default=str(MAIN), help='the main board')
    ap.add_argument('--extras', default=','.join(map(str, EXTRAS)), help='the extra relays\' numbers (their boards tw3d-board-N)')
    ap.add_argument('--home', default=str(HOME))
    a = ap.parse_args(argv)
    home = Path(a.home)
    home.mkdir(parents=True, exist_ok=True)
    extras = tuple(int(x) for x in a.extras.split(',') if x.strip())
    if not (a.once or a.watch):
        print('\n'.join(round_(World(a.main, extras, home, acting=False))))
        return 0
    if a.watch and not take_lock(home):
        return 0
    log = home / 'autoloop.log'

    def say(text):
        with open(log, 'a', encoding='utf-8') as f:
            f.write(f'{time.strftime("%m-%d %H:%M:%S")} {text}\n')
    say(f'autoloop starts (pid {os.getpid()}), every {a.watch or 0:.0f} s')
    while True:
        if (home / 'autoloop.stop').exists():
            say('autoloop.stop is in its folder: standing by')
        else:
            for line in round_(World(a.main, extras, home)):
                say(line)
                if a.once:
                    print(line)
        (home / 'beat.json').write_text(json.dumps(dict(at=time.time(), pid=os.getpid())), encoding='utf-8')
        if not a.watch:
            return 0
        time.sleep(max(30, a.watch))


if __name__ == '__main__':
    sys.exit(main())
