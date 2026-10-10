#!/usr/bin/env python3
"""The steward: keeps the relay going on the desktop with nobody watching, and writes the one state file.

    python Tools/assetboard/steward.py                      look once and print the state: starts and writes nothing
    python Tools/assetboard/steward.py --watch 60           keep the loop going, again every 60 seconds
    python Tools/assetboard/steward.py --watch 60 --heal unity,dirty     ... and mend what HEALS names by itself

The owner, 2026-10-10: "I need you to build a reliable, sustainable continous loop ... it should run by itslef".
That day the relay stood still from 18:24 to midnight on its own leftover (a leg cut at its time limit left a
batch-mode Unity holding the work checkout), its watcher tried again every hour without telling anybody, and ended
by itself at 23:55. A day earlier a watch in a session did not fire once in a night.

So: no model is started here, there is no end date, and every reason a run stops has ONE answer (STOPS below):
wait for a time the relay names, mend it, start again, or say it under "Needs you" with what frees it. What it sees
goes to <Drive>/TW3D-pipeline/STATE.md (and state.json for the board), written only when something changed: a
session reads that file and writes no new handoff.

One steward a machine (a lock in its folder): start it as often as you like, the second one leaves at once. That
is how it is kept alive: a Task Scheduler entry starts it every five minutes. A file steward.stop in its folder
makes it stand by (the run that is going is left alone).
"""
import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build        # noqa: E402

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
RELAY = GH / 'githubtest-relay-run' / 'trench-warfare-3d' / 'Tools' / 'relay' / 'relay.py'      # the frozen run copy
WORK = GH / 'githubtest-relay-work'
MAIN = GH / 'githubtest'
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
LOCAL = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare'
BOARD = GH / 'tw3d-board'
# The script a run is started through, written into the steward's folder at every start (change it here). Between two
# units the runner does the card round it is handed in TW_BETWEEN_UNITS (runner.card_round): an accepted idea goes on
# the board, a passed step becomes the owner's card, his answer on a step is acted on. A relay copy from before
# 2026-10-11 does not read the variable and runs as it did.
START_PS1 = """# Written by Tools/assetboard/steward.py at every start of a run: change it there, not here.
param([string]$Hours = '12', [string]$Who = 'steward', [string]$DayPct = '')
$GH = '%(gh)s'
$env:UNITY_CLI_ALLOW_LOCKED = '1'
if (-not $env:TW_BOARD_ALSO) { $env:TW_BOARD_ALSO = "$GH/tw3d-board;$GH/tw3d-board-2" }
$env:TW_BETWEEN_UNITS = '%(between)s'
$day = if ($DayPct) { "--day-pct $DayPct" } else { '' }
$log = "$env:LOCALAPPDATA/Temp/relay-test-run-" + (Get-Date -Format 'yyyyMMdd-HHmmss') + '.log'
"started $(Get-Date -Format o) by $Who, hours $Hours, day pct '$DayPct'" | Out-File -Encoding utf8 $log
# input from NUL: the runner's git over ssh waits for ever on an inherited input
cmd /c "python $GH/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py run --work $GH/githubtest-relay-work --hours $Hours --max-legs 0 --who $Who $day < NUL >> `"$log`" 2>&1"
"ended $(Get-Date -Format o), exit $LASTEXITCODE" | Out-File -Encoding utf8 -Append $log
"""


def start_script():
    """The text of the start script, with this checkout's card round in it."""
    between = json.dumps([['python', '-B', (HERE / tool).as_posix(), step, '--board', BOARD.as_posix()]
                          for tool, step in (('idearoute.py', 'route'), ('idearoute.py', 'gates'), ('found.py', 'take'), ('found.py', 'rota'))])
    return START_PS1 % dict(gh=GH.as_posix(), between=between)
WHO = 'steward'
HOURS = 12                  # a run's own limit, and
STOP_AFTER = 10.75          # the hours after which it is asked to stop before its next leg, so the limit cuts no leg
QUIET = 660                 # the runner wants ten quiet minutes on the work checkout before a start
LONGEST = 3600              # the longest wait between two tries
HOLD_EVERY = 3 * 3600
LEFTOVER_MIN = 20 * 60      # a batch-mode Unity on the work checkout this old, with no run going, is a leftover
REPEATS = 3                 # the same reason this often in a row is said under "Needs you"
HEALS = ('unity', 'dirty')

# Why a run stopped or did not start, as the relay words it (runner.py, gitio.py, launch.py), and the one answer.
#   wait   the relay names the time, or it passes by itself: try again then, say nothing
#   heal   the steward mends it when --heal names it; else it is said under "Needs you"
#   again  start the next run after the quiet minutes (a run that ended inside ten minutes doubles the wait)
#   hand   nothing here can mend it: said under "Needs you" at once
STOPS = (
    ('unity', 'heal', r'a Unity editor has this project open', 'a Unity holds the relay\'s work checkout'),
    ('dirty', 'heal', r'uncommitted changes that are not a saved leg snapshot|left uncommitted work', 'leftover files in the relay\'s work checkout'),
    ('pace', 'wait', r'the day\'s pace lets', ''),
    ('budget', 'wait', r'the day\'s budget is spent|budget has .* left', ''),
    ('quiet', 'wait', r'its git index moved', ''),
    ('leg', 'wait', r'a relay leg holds it', ''),
    ('nowork', 'wait', r'nothing left to do|would run: nothing', ''),
    ('code', 'hand', r'the relay\'s own code has uncommitted changes', 'the relay\'s run copy (githubtest-relay-run) has uncommitted changes: commit them or put them back'),
    ('missing', 'hand', r'does not exist|is not a git checkout', 'the relay\'s work checkout is missing: `git worktree add --detach` it again'),
    ('hold', 'hand', r'hold is another|is held by', 'another session holds the relay build (relay.py hold): it must release it, or its hours must pass'),
    ('owner', 'again', r'stopped by |stopped on request', ''),
    ('again', 'again', r'ended TIMEOUT|broke a rule|hours are up|leg cap|brought no result|cannot be switched to|not auto mode|no hooks|guard changed|no result|error:', ''),
)


WHY_IDLE = dict(unity='a Unity held the checkout', dirty='leftover files in the checkout', pace='waiting for the pace of the day', budget='the tokens of the day were spent',
                nowork='nothing to do', hold='another session held the build', code='the run copy had changes', missing='the work checkout was missing')


def classify(reason):
    """(kind, answer, what to say) for a stop reason of the relay's; one nobody listed is tried again and counted."""
    for kind, answer, pattern, say in STOPS:
        if re.search(pattern, reason or ''):
            return kind, answer, say
    return 'unknown', 'again', ''


def parse_status(text):
    """`relay.py status`, read: {run, on, last, reason, legs, held_by}."""
    out = dict(run='', on='', last='', reason='', legs=None, held_by='')
    lines = (text or '').splitlines()
    first = lines[0] if lines else ''
    m = re.match(r'RUNNING: relay (\S+)\s*(.*)', first)
    if m:
        out.update(run=m.group(1), on=m.group(2).strip())
    m = re.match(r'NOT RUNNING\. Last run (\S+): (.*?)(?: \((\d+) legs?\))?\.?$', first)
    if m:
        out.update(last=m.group(1), reason=m.group(2).strip(), legs=int(m.group(3)) if m.group(3) else None)
    for l in lines:
        m = re.search(r'relay build is held by (\S+) until', l)
        if m:
            out['held_by'] = m.group(1)
    return out


def run_started(run):
    """When a run began, from its name (20261010-103442-62944); 0 when the name is not one."""
    m = re.match(r'(\d{8}-\d{6})', run or '')
    return datetime.datetime.strptime(m.group(1), '%Y%m%d-%H%M%S').timestamp() if m else 0.0


def day_pct(rec, now):
    """The day's figure the owner named, as the text the relay takes, or ''. The file names its day, so a raise for
    one day is nobody's job to take back at midnight."""
    if not isinstance(rec, dict) or rec.get('day') != time.strftime('%Y-%m-%d', time.localtime(now)):
        return ''
    try:
        return '%g' % float(rec.get('pct'))
    except (TypeError, ValueError):
        return ''


def named_time(text, now):
    """The clock time a line of the relay's names ("... start at 00:47"), as a time not before now; 0 when none."""
    m = re.search(r'start at (\d\d):(\d\d)', text or '')
    if not m:
        return 0.0
    t = datetime.datetime.fromtimestamp(now).replace(hour=int(m.group(1)), minute=int(m.group(2)), second=0, microsecond=0)
    if t.timestamp() < now - 60:
        t += datetime.timedelta(days=1)
    return t.timestamp()


def midnight(now):
    d = datetime.datetime.fromtimestamp(now).replace(hour=0, minute=0, second=0, microsecond=0) + datetime.timedelta(days=1)
    return d.timestamp()


def hm(t):
    return time.strftime('%m-%d %H:%M', time.localtime(t)) if t else '?'


# ---- the outside world: everything the steward reads or does, so a test can stand in for it -------------------------

class World:
    def __init__(self, home: Path):
        self.home = home

    def now(self):
        return time.time()

    def sleep(self, s):
        time.sleep(s)

    def sh(self, cmd, cwd=None, timeout=300):
        try:
            r = subprocess.run(cmd, capture_output=True, text=True, stdin=subprocess.DEVNULL, cwd=cwd, encoding='utf-8', errors='replace', timeout=timeout)
        except (OSError, subprocess.TimeoutExpired) as e:
            return 1, f'{type(e).__name__}: {e}'
        return r.returncode, (r.stdout + r.stderr).strip()

    def relay(self, *a):
        env_also = os.environ.get('TW_BOARD_ALSO')
        if not env_also:                                        # as the start script sets it: both boards' spend is one day
            os.environ['TW_BOARD_ALSO'] = f'{GH.as_posix()}/tw3d-board;{GH.as_posix()}/tw3d-board-2'
        return self.sh([sys.executable, str(RELAY)] + [str(x) for x in a])

    def stopped(self):
        return (self.home / 'steward.stop').exists()

    def day_file(self):
        try:
            return json.loads((LOCAL / 'relay' / 'day.json').read_text(encoding='utf-8'))
        except (OSError, ValueError):
            return None

    def unity_on_work(self):
        """The Unity processes that have the relay's work checkout open: [{pid, batch, started}]."""
        code, out = self.sh(['powershell', '-NoProfile', '-Command',
                             "Get-CimInstance Win32_Process -Filter \"Name='Unity.exe'\" | ForEach-Object { '{0}|{1}|{2}' -f $_.ProcessId, $_.CreationDate.ToString('s'), $_.CommandLine }"])
        want, rows = str(WORK / 'trench-warfare-3d').replace('/', '\\').lower(), []
        for line in out.splitlines() if code == 0 else []:
            pid, _, rest = line.partition('|')
            since, _, cmd = rest.partition('|')
            m = re.search(r'-projectpath\s+"?([^"]+?)"?(?:\s+-|\s*$)', cmd, re.I)
            if m and m.group(1).strip().replace('/', '\\').rstrip('\\').lower() == want:
                try:
                    started = datetime.datetime.strptime(since, '%Y-%m-%dT%H:%M:%S').timestamp()
                except ValueError:
                    started = 0.0
                rows.append(dict(pid=int(pid), batch='-batchmode' in cmd.lower(), started=started))
        return rows

    def kill_tree(self, pid):
        return self.sh(['taskkill', '/PID', str(pid), '/T', '/F'])[0] == 0

    def work_dirty(self):
        code, out = self.sh(['git', '-C', str(WORK), 'status', '--porcelain'])
        return [l for l in out.splitlines() if l.strip()] if code == 0 else []

    def save_and_clean(self):
        """Leftover files of a leg that was cut: kept twice (a patch file beside the relay's records, and a stash that
        holds the untracked ones too), then the checkout is clean. (ok, where it is kept or why not)."""
        stamp = time.strftime('%Y%m%d-%H%M%S')
        branch = self.sh(['git', '-C', str(WORK), 'rev-parse', '--abbrev-ref', 'HEAD'])[1].replace('/', '_')[:60]
        keep = LOCAL / 'relay' / 'leftovers'
        keep.mkdir(parents=True, exist_ok=True)
        patch = keep / f'{stamp}-{branch}.patch'
        r = subprocess.run(['git', '-C', str(WORK), 'diff', '--binary', 'HEAD'], capture_output=True, stdin=subprocess.DEVNULL)
        patch.write_bytes(r.stdout)
        code, out = self.sh(['git', '-C', str(WORK), 'stash', 'push', '--include-untracked', '-m', f'steward: leftover of a cut leg, {stamp}'])
        if code or self.work_dirty():
            return False, f'the stash did not take it ({out[-200:]}); the patch is {patch}'
        return True, f'{patch} and the stash "steward: leftover of a cut leg, {stamp}"'

    def start(self, pct):
        script = self.home / 'relay-start.ps1'
        put(script, start_script())
        line = f'powershell -NoProfile -ExecutionPolicy Bypass -File "{script}" -Hours {HOURS} -Who {WHO}' + (f' -DayPct {pct}' if pct else '')
        return self.sh(['powershell', '-NoProfile', '-Command',
                        f"(Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{{ CommandLine = '{line}' }}).ReturnValue"])

    def answers(self):
        import briefs
        import notes
        return briefs.answers(briefs.read_all(briefs.folder()), notes.read_all(notes.folder()))

    def integration(self):
        out = self.sh(['git', '-C', str(MAIN), 'ls-remote', 'origin', 'refs/heads/' + INTEGRATION], timeout=60)[1]
        return out.split()[0] if re.match(r'[0-9a-f]{40}\s', out) else ''

    def landings(self, tip):
        """The landing queue, a line a lane (landq.py): what waits, what was asked of him, what was refused and why."""
        import landq
        return landq.lines(LOCAL / 'landq')

    def day_card(self):
        """The day's card (daycard.py): what landed, what the tokens went to, what was found, what was decided."""
        import daycard
        day = time.strftime('%Y-%m-%d')
        try:
            return daycard.card(day, **daycard.read(day, board=BOARD, local=LOCAL))
        except Exception as e:      # noqa: BLE001  a card that cannot be counted is said, not the end of the state file
            return [f'- the card of the day could not be counted ({type(e).__name__}: {e})'[:200]]

    def land_work(self, tree: Path):
        """Have the landing queue worked, when no worker is at it: first the lanes the relay finished are put in
        (offer), then the queue is taken lane by lane (work), as a process of its own. A full gate takes 25
        minutes, so the steward never waits for it. Returns a line when a worker was started."""
        import landq
        home = LOCAL / 'landq'
        if landq.worker_alive(home):
            return ''
        tool = str(HERE / 'landq.py')
        self.sh([sys.executable, '-B', tool, 'offer', '--board', str(BOARD)])
        if not landq.queue(home) and not any(a['go'] == 'land' for a in self.answers()):
            return ''
        home.mkdir(parents=True, exist_ok=True)
        with open(home / 'work.log', 'a', encoding='utf-8') as out:
            p = subprocess.Popen([sys.executable, '-u', '-B', tool, 'work', '--tree', str(tree)], cwd=str(HERE.parent.parent), stdout=out, stderr=subprocess.STDOUT,
                                 stdin=subprocess.DEVNULL, creationflags=0x08000200 if os.name == 'nt' else 0)        # no window, its own group (gate_bg.py: a detached one starts nothing)
        return f'the landing queue is being worked (pid {p.pid}): {len(landq.queue(home))} lanes'


# ---- one look, and what follows from it ------------------------------------------------------------------------------

def need(mem, kind, text):
    mem.setdefault('needs', {})[kind] = text


def failed(mem, now, kind, reason, say=''):
    """A try that started no run: wait longer each time, and after REPEATS of one kind say it."""
    same = mem.get('same') or ['', 0]
    mem['same'] = [kind, same[1] + 1 if same[0] == kind else 1]
    mem['wait'] = min(max(mem.get('wait', QUIET), QUIET) * 2, LONGEST)
    mem['next_try'] = now + mem['wait']
    if say or mem['same'][1] >= REPEATS:
        need(mem, kind, (say or f'the relay did not start {mem["same"][1]} times in a row') + f': {reason[:160]}')


def preflight(w, mem, heal, did):
    """What the steward can see for itself before a start, so a run that would end with no leg is not started (each
    one is a record on the board: six of them on the evening of 2026-10-10). Returns (kind, reason) or None."""
    now = w.now()
    for u in w.unity_on_work():
        age = now - u['started']
        if not u['batch']:
            need(mem, 'unity', f'an editor window (pid {u["pid"]}, open since {hm(u["started"])}) has the relay\'s work checkout open: close it, the relay works there')
            return 'unity', 'an editor window has the work checkout open'
        if age < LEFTOVER_MIN:
            return 'unity', f'a batch-mode Unity (pid {u["pid"]}) started {int(age // 60)} minutes ago in the work checkout: left alone until it is {LEFTOVER_MIN // 60} minutes old'
        if 'unity' not in heal:
            need(mem, 'unity', f'a leftover batch-mode Unity (pid {u["pid"]}, since {hm(u["started"])}) holds the relay\'s work checkout and no run is going: `taskkill /PID {u["pid"]} /T /F` frees it')
            return 'unity', 'a leftover Unity holds the work checkout'
        ok = w.kill_tree(u['pid'])
        did.append(f'stopped the leftover batch-mode Unity {u["pid"]} (since {hm(u["started"])}) and what it started' if ok else f'COULD NOT stop the leftover Unity {u["pid"]}')
        if not ok:
            need(mem, 'unity', f'the leftover Unity {u["pid"]} could not be stopped: `taskkill /PID {u["pid"]} /T /F`')
            return 'unity', 'a leftover Unity could not be stopped'
        w.sleep(5)
    left = w.work_dirty()
    if left:
        if 'dirty' not in heal:
            need(mem, 'dirty', f'{len(left)} leftover files of a cut leg are in the relay\'s work checkout ({left[0].strip()[:70]} ...): save them and clean it, or start the steward with --heal dirty')
            return 'dirty', f'{len(left)} leftover files in the work checkout'
        ok, where = w.save_and_clean()
        did.append(f'saved {len(left)} leftover files of a cut leg and cleaned the work checkout: {where}' if ok else f'COULD NOT clean the work checkout: {where}')
        if not ok:
            need(mem, 'dirty', f'the work checkout could not be cleaned: {where}')
            return 'dirty', 'the work checkout could not be cleaned'
    (mem.get('needs') or {}).pop('unity', None)
    (mem.get('needs') or {}).pop('dirty', None)
    return None


def tick(w, mem, heal=(), acting=True):
    """One look at the relay and the one thing that follows. `mem` is what the steward remembers between looks (kept
    in a file, so a steward that is started again goes on where the last one was). `acting` False only reads.
    Returns what the state file is written from."""
    now, did = w.now(), []
    code, text = w.relay('status')
    st = parse_status(text)
    out = dict(at=now, run=st['run'], on=st['on'], last=st['last'], reason=st['reason'], legs=st['legs'], did=did, standby='')
    if not acting:
        return out
    if code or not (st['run'] or text.startswith('NOT RUNNING')):
        # a status that timed out or fell over is not "no run": acting on it would mend a checkout a leg is working in
        out['standby'] = 'the relay did not say whether a run is going: nothing is done on this look'
        return out
    if w.stopped():
        out['standby'] = 'steward.stop is in its folder: it starts nothing until the file is gone'
        return out
    if not st['run']:                               # the day's count of standing still, by why: for the day's card
        day, gap = time.strftime('%Y-%m-%d', time.localtime(now)), min(max(now - mem.get('looked', now), 0), 180)
        idle = mem.setdefault('idle', {})
        for old_day in [d for d in idle if d < day][:-3]:
            del idle[old_day]
        why = WHY_IDLE.get(classify(st['reason'])[0], 'between two runs')
        idle.setdefault(day, {})[why] = idle.setdefault(day, {}).get(why, 0) + gap
    mem['looked'] = now
    if st['run']:
        if mem.get('seen') != st['run']:
            mem.update(seen=st['run'], stop_asked='', same=['', 0], wait=QUIET, needs={})
            did.append('going: ' + st['run'])
        if now - mem.get('held', 0) > HOLD_EVERY:
            w.relay('hold', WHO)
            mem['held'] = now
        age = (now - run_started(st['run'])) / 3600 if run_started(st['run']) else 0
        if age > STOP_AFTER and mem.get('stop_asked') != st['run']:
            did.append(f'the run is {age:.1f} hours old, asked to stop before its next leg: ' + w.relay('stop')[1][:160])
            mem['stop_asked'] = st['run']
        return out
    if mem.get('seen'):                                         # a run the steward saw going has ended
        lasted = now - run_started(mem['seen']) if run_started(mem['seen']) else 0
        did.append(f'stopped: {mem["seen"]} after {lasted / 3600:.1f} hours: {st["reason"][:200]}' + (f' ({st["legs"]} legs)' if st['legs'] is not None else ''))
        mem['wait'] = QUIET if lasted > 1800 else min(mem.get('wait', QUIET) * 2, LONGEST) if lasted < 600 else mem.get('wait', QUIET)
        mem.update(seen='', next_try=now + mem['wait'])
    pct = day_pct(w.day_file(), now)
    if pct != mem.get('pct', ''):                               # he named another figure for the day: what waited on the budget tries now
        mem.update(pct=pct, next_try=min(mem.get('next_try', now), now))
    if now < mem.get('next_try', 0):
        return out
    code, text = w.relay('hold', WHO)               # first: whoever holds the build may be working in the checkout
    if code:
        failed(mem, now, 'hold', text.splitlines()[0] if text else 'the hold was refused', say=classify('hold is another')[2])
        return out
    mem['held'] = now
    block = preflight(w, mem, heal, did)
    if block:
        failed(mem, now, block[0], block[1])
        return out
    first = (w.relay('run', '--work', WORK.as_posix(), '--dry-run', *(['--day-pct', pct] if pct else []))[1].splitlines() or [''])[0]
    if 'would run: nothing' in first:
        kind, answer, say = classify(first.split('(', 1)[1] if '(' in first else first)
        mem['next_try'] = named_time(first, now) or (midnight(now) + 60 if kind == 'budget' else now + (QUIET if kind in ('quiet', 'leg') else 1800))
        mem['same'] = [kind, 0]
        out['waits'] = first[:200]
        if answer == 'hand':
            need(mem, kind, f'{say}: {first[:160]}')
        return out
    if 'would run:' not in first:
        kind, answer, say = classify(first)
        failed(mem, now, kind, first, say=say if answer == 'hand' else '')
        return out
    w.start(pct)
    w.sleep(70)
    after = parse_status(w.relay('status')[1])
    if after['run']:
        mem.update(seen=after['run'], stop_asked='', same=['', 0], wait=QUIET, needs={})
        did.append(f'started {after["run"]}: {first[:200]}')
        out.update(run=after['run'], on=after['on'])
    else:
        kind, answer, say = classify(after['reason'])
        did.append(f'a start brought no run: {after["reason"][:200]}')
        failed(mem, now, kind, after['reason'], say=say if answer == 'hand' else '')
        out.update(last=after['last'], reason=after['reason'], legs=after['legs'])
    return out


# ---- the state file --------------------------------------------------------------------------------------------------

def state_of(w, out, mem, slow):
    """Everything STATE.md says, as data. `slow` is what is read every few minutes only (the day, the answers,
    integration, the landing branches): {day, answers, tip, landings}."""
    if out['run']:
        loop = f'RUNNING: run {out["run"]}' + (f', on {out["on"]}' if out['on'] else '')
    elif out['standby']:
        loop = 'STANDING BY: ' + out['standby']
    else:
        loop = f'NOT RUNNING: {out["reason"] or "no run on record"}' + (f' ({out["legs"]} legs)' if out['legs'] is not None else '')
        if out.get('waits'):
            loop += f'. {out["waits"]}'
        if mem.get('next_try'):
            loop += f'. Next try {hm(mem["next_try"])}'
    answers = sorted(slow.get('answers') or [], key=lambda a: a['when'])
    asks = [l.split(':', 1)[1].strip() for l in slow.get('day') or [] if l.startswith('Needs you:') and 'nothing' not in l.lower()]
    return dict(loop=loop, today=slow.get('today', ''), card=slow.get('card') or [], day=[l for l in slow.get('day') or [] if not l.startswith('Needs you:')],
                needs=sorted((mem.get('needs') or {}).values()) + ['the relay: ' + a for a in asks],
                answers=[dict(id=a['id'], go=a['go'], when=a['when'], title=a['title']) for a in answers],
                tip=slow.get('tip', ''), landings=slow.get('landings') or [], pct=mem.get('pct', ''))


def text_of(s, host, heal):
    lines = ['# TW3D state', '',
             f'Written by the steward on {host} (`Tools/assetboard/steward.py`, no model). It changes only when something',
             'changed. Read this instead of a handoff; write no new handoff, add what lasts to the topic\'s own file.', '',
             '**Loop:** ' + s['loop']]
    lines += [f'**{l.split(":", 1)[0]}:**{l.split(":", 1)[1]}' if ':' in l else l for l in s['day']]
    if s['pct']:
        lines.append(f'**The day\'s figure:** {s["pct"]}% today only (day.json names its day)')
    lines += ['', '## Needs you'] + ([f'- {n}' for n in s['needs']] or ['- nothing from the loop'])
    lines += ['', f'## Your answers nobody has taken up: {len(s["answers"])}'] + [f'- since {a["when"][5:16]}, `{a["go"]}`: {a["title"]} (`{a["id"]}`)' for a in s['answers'][:12]]
    lines += ['', f'## Today, {s.get("today", "")}'] + (s.get('card') or ['- not counted yet'])
    lines += ['', '## Landing', f'- integration is `{s["tip"][:8] or "not read"}`'] + ([f'- {l}' for l in s['landings']] or ['- nothing waits in the landing queue'])
    lines += ['', f'Mends by itself: {", ".join(heal) or "nothing (started without --heal)"}. To make it stand by: a file `steward.stop` in `%LOCALAPPDATA%\\TrenchWarfare\\steward`.']
    return '\n'.join(lines) + '\n'


def put(path: Path, text):
    """Write a file whole or not at all; True when it changed."""
    try:
        if path.read_text(encoding='utf-8') == text:
            return False
    except OSError:
        pass
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + '.tmp')
    tmp.write_text(text, encoding='utf-8', newline='\n')
    os.replace(tmp, path)
    return True


def load(path: Path):
    try:
        return json.loads(path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return {}


def alive(pid):
    import ideas
    return ideas.pid_alive(dict(pid=pid))


def take_lock(home: Path):
    """True when this is the machine's one steward. A lock whose process is gone is taken over."""
    f = home / 'lock.json'
    rec = load(f)
    if rec.get('pid') and rec['pid'] != os.getpid() and alive(rec['pid']) and time.time() - load(home / 'beat.json').get('at', 0) < 600:
        return False
    put(f, json.dumps(dict(pid=os.getpid(), since=time.strftime('%Y-%m-%d %H:%M:%S'))))
    return True


def main(argv=None):
    ap = argparse.ArgumentParser(description='keeps the relay going with nobody watching, and writes the one state file')
    ap.add_argument('--watch', type=float, default=0, help='seconds between looks; 0 looks once and starts nothing')
    ap.add_argument('--heal', default='', help=f'what it may mend by itself, of: {",".join(HEALS)}')
    ap.add_argument('--land', default='', help='the landing queue\'s own checkout (a full one): with it the queue is worked (landq.py); left out, nothing lands')
    ap.add_argument('--state', default='', help='where STATE.md and state.json go (default: the Drive\'s TW3D-pipeline folder)')
    ap.add_argument('--home', default='', help='its own folder (lock, memory, log)')
    args = ap.parse_args(argv)
    heal = tuple(h for h in args.heal.split(',') if h)
    if set(heal) - set(HEALS):
        ap.error(f'--heal takes {",".join(HEALS)}')
    home = Path(args.home) if args.home else LOCAL / 'steward'
    where = Path(args.state) if args.state else build.DRIVE
    host = __import__('socket').gethostname()
    w = World(home)
    if not args.watch:
        out = tick(w, {}, acting=False)
        tip = w.integration()
        day = [l for l in w.relay('day')[1].splitlines() if re.match(r'Today:|Needs you:', l)]
        print(text_of(state_of(w, out, load(home / 'mem.json'), dict(day=day, answers=w.answers(), tip=tip, landings=w.landings(tip))), host, heal))
        return 0
    home.mkdir(parents=True, exist_ok=True)
    if not take_lock(home):
        return 0                                                # the machine has its steward
    log = home / 'steward.log'

    def say(text):
        with open(log, 'a', encoding='utf-8') as f:
            f.write(f'{time.strftime("%m-%d %H:%M:%S")} {text}\n')
    say(f'steward starts (pid {os.getpid()}), mends: {", ".join(heal) or "nothing"}')
    mem, slow, slow_at = load(home / 'mem.json'), {}, 0.0
    while True:
        try:
            out = tick(w, mem, heal)
            for d in out['did']:
                say(d)
            if time.time() - slow_at > 300 or out['did']:       # the slow reads: every five minutes, and when something happened
                if args.land:
                    started = w.land_work(Path(args.land))
                    if started:
                        say(started)
                tip = w.integration()
                slow = dict(day=[l for l in w.relay('day')[1].splitlines() if re.match(r'Today:|Needs you:', l)],
                            answers=w.answers(), tip=tip, landings=w.landings(tip), today=time.strftime('%Y-%m-%d'), card=w.day_card())
                slow_at = time.time()
            s = state_of(w, out, mem, slow)
            if put(where / 'STATE.md', text_of(s, host, heal)):
                put(where / 'state.json', json.dumps(dict(s, host=host, at=time.strftime('%Y-%m-%d %H:%M:%S')), indent=1))
            put(home / 'mem.json', json.dumps(mem))
        except Exception:       # noqa: BLE001  one look that fails must not end the steward
            say('this look failed, the next one is tried:\n' + traceback.format_exc()[-1500:])
        put(home / 'beat.json', json.dumps(dict(at=time.time(), pid=os.getpid())))
        if load(home / 'lock.json').get('pid') != os.getpid():
            say('another steward has the lock: this one leaves')
            return 0
        time.sleep(max(20, args.watch))


if __name__ == '__main__':
    sys.exit(main())
