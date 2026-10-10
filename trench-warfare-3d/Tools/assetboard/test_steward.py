#!/usr/bin/env python3
"""Tests of the steward (steward.py). Run from trench-warfare-3d/: python Tools/assetboard/test_steward.py

The relay, Unity, git and the clock are a stand-in here (Fake), so every row of the steward's table of stops is
tried against the words the relay really says (copied from runner.py, gitio.py and the desktop's logs of
2026-10-10), and nothing on this machine is started, stopped or written outside a temp folder."""
import datetime
import json
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import steward as S    # noqa: E402

results = []
T0 = datetime.datetime(2026, 10, 10, 22, 0, 0).timestamp()
GOING = 'RUNNING: relay 20261010-220200-111 idea-a-shot-frog--motion leg 03'
UNITY = 'NOT RUNNING. Last run 20261010-214738-59000: the work checkout cannot be used: a Unity editor has this project open (0 legs).'
DRY = 'would run: pipeline idea-a-shot-frog--motion--2b1ea2c5 (REGENERATE) in githubtest-relay-work on lane/show/pipe-idea-a-shot-frog'


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


class Fake:
    """The outside world, as the steward asks for it. A start makes the relay say `after_start`."""

    def __init__(self, status=UNITY, dry=DRY):
        self.t, self.status, self.dry, self.after_start = T0, status, dry, GOING
        self.unity, self.dirty, self.calls, self.day, self.stop, self.hold = [], [], [], None, False, (0, 'held')
        self.kill_ok, self.clean_ok = True, True

    def now(self):
        return self.t

    def sleep(self, s):
        self.t += s

    def relay(self, *a):
        self.calls.append(a)
        return {'status': (0, self.status), 'hold': self.hold, 'run': (0, self.dry), 'stop': (0, 'stop asked')}.get(a[0], (0, ''))

    def stopped(self):
        return self.stop

    def day_file(self):
        return self.day

    def unity_on_work(self):
        return list(self.unity)

    def kill_tree(self, pid):
        self.calls.append(('KILL', pid))
        if self.kill_ok:
            self.unity = [u for u in self.unity if u['pid'] != pid]
        return self.kill_ok

    def work_dirty(self):
        return list(self.dirty)

    def save_and_clean(self):
        self.calls.append(('CLEAN',))
        if self.clean_ok:
            self.dirty = []
        return self.clean_ok, 'a patch and a stash'

    def start(self, pct):
        self.calls.append(('START', pct))
        self.status = self.after_start

    def did(self, what):
        return [c for c in self.calls if c[0] == what]


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-steward-test-'))

    # ---- reading the relay's words ----
    a, b = S.parse_status(GOING), S.parse_status(UNITY + '\nToday: about 12.6%\nThe relay build is held by laptop-relay-test until 2026-10-11 02:48 (relay.py hold).')
    case('status: a run that is going gives its name and what it is on; one that stopped gives the last run, why, and its legs; a hold names its holder',
         a['run'] == '20261010-220200-111' and 'motion' in a['on'] and b['run'] == '' and b['last'] == '20261010-214738-59000'
         and b['reason'] == 'the work checkout cannot be used: a Unity editor has this project open' and b['legs'] == 0 and b['held_by'] == 'laptop-relay-test', (a, b))
    said = {'the work checkout cannot be used: a Unity editor has this project open': 'unity',
            'the work checkout cannot be used: it has uncommitted changes that are not a saved leg snapshot': 'dirty',
            'unit rv-12 left uncommitted work in the checkout': 'dirty',
            "the day's pace lets a unit start at 00:47, after this run's 12 hours are up": 'pace',
            "the day's budget is spent (12.6 of 11.0)": 'budget',
            'the work checkout cannot be used: its git index moved 212 s ago: somebody may be working in it': 'quiet',
            'the work checkout cannot be used: a relay leg holds it (run 2026)': 'leg',
            "the relay's own code has uncommitted changes": 'code',
            'the work checkout cannot be used: C:/x does not exist. Make it once: git worktree add': 'missing',
            'nothing left to do': 'nowork', 'stopped by the owner (relay.py stop)': 'owner',
            'stopped by the steward (relay.py stop): the run is 10.8 hours old': 'owner', 'stopped on request (relay.py stop): a restart': 'owner',
            'leg 35 ended TIMEOUT': 'again', "leg 12 broke a rule: the board's items/x.json changed": 'again',
            "the run's 12 hours are up": 'again', '2 units in a row brought no result and no pushed code': 'again',
            '5 units in a row whose lane cannot be switched to': 'again', 'error: KeyError': 'again',
            'a reason nobody has listed yet': 'unknown'}
    got = {k: S.classify(k)[0] for k in said}
    case('table: every reason the relay gives for a stop has one row, by the words the relay says; a reason nobody listed is tried again',
         got == said and S.classify('a reason nobody has listed yet')[1] == 'again', {k: (v, got[k]) for k, v in said.items() if got[k] != v})
    case('table: the three answers are fixed a row: a Unity and leftover files are mended, pace and budget are waited for, changed relay code and a missing checkout are his',
         [S.classify(k)[1] for k in ('a Unity editor has this project open', 'it has uncommitted changes that are not a saved leg snapshot', "the day's pace lets", "the day's budget is spent", "the relay's own code has uncommitted changes", 'does not exist')]
         == ['heal', 'heal', 'wait', 'wait', 'hand', 'hand'])

    # ---- the evening of 2026-10-10: a leftover Unity ----
    old = dict(pid=64552, batch=True, started=T0 - 4 * 3600)
    w, mem = Fake(), {}
    w.unity = [old]
    out = S.tick(w, mem)
    case('a leftover batch-mode Unity, no mending allowed: no run is started that would end with no leg, and "Needs you" names the process and the command that frees it',
         not w.did('START') and not w.did('run') and not w.did('KILL') and '64552' in mem['needs']['unity'] and 'taskkill /PID 64552 /T /F' in mem['needs']['unity'] and mem['next_try'] > T0, (w.calls, mem))
    w, mem = Fake(), {}
    w.unity = [old]
    out = S.tick(w, mem, heal=('unity',))
    case('a leftover batch-mode Unity, mending allowed: it is stopped with what it started, and the run starts in the same look',
         w.did('KILL') == [('KILL', 64552)] and w.did('START') == [('START', '')] and out['run'] == '20261010-220200-111' and not mem['needs'] and any('64552' in d for d in out['did']), (w.calls, out))
    w, mem = Fake(), {}
    w.unity = [dict(pid=7, batch=True, started=T0 - 300)]
    S.tick(w, mem, heal=('unity',))
    case('a batch-mode Unity five minutes old is somebody\'s work: left alone, nothing started, nothing asked of him',
         not w.did('KILL') and not w.did('START') and not mem.get('needs'), (w.calls, mem))
    w, mem = Fake(), {}
    w.unity = [dict(pid=9, batch=False, started=T0 - 9 * 3600)]
    S.tick(w, mem, heal=('unity', 'dirty'))
    case('an editor WINDOW on the work checkout is never stopped, whatever may be mended: he is told to close it',
         not w.did('KILL') and not w.did('START') and 'close it' in mem['needs']['unity'], (w.calls, mem))
    w, mem = Fake(), {}
    w.unity, w.kill_ok = [old], False
    S.tick(w, mem, heal=('unity',))
    case('a Unity that will not stop is said, and no run is started on top of it', not w.did('START') and 'could not be stopped' in mem['needs']['unity'], mem)

    # ---- leftover files of a cut leg ----
    w, mem = Fake(), {}
    w.dirty = [' M trench-warfare-3d/Assets/_Project/Editor/CaptureRig.cs', ' M docs/reference/workflow.md']
    S.tick(w, mem)
    plain = (not w.did('CLEAN') and not w.did('START') and '2 leftover files' in mem['needs']['dirty'])
    w, mem = Fake(), {}
    w.dirty = [' M a.cs']
    out = S.tick(w, mem, heal=('dirty',))
    case('leftover files in the work checkout: said when it may not mend; kept and cleaned when it may, and the run starts',
         plain and w.did('CLEAN') and w.did('START') and not mem['needs'] and any('saved 1 leftover' in d for d in out['did']), (w.calls, mem))

    # ---- the pace and the budget are waited for, by the time the relay names ----
    w, mem = Fake(dry="would run: nothing (the day's pace lets a unit start at 23:10)"), {}
    S.tick(w, mem)
    at = datetime.datetime(2026, 10, 10, 23, 10).timestamp()
    w.t, w.dry = at - 120, DRY
    S.tick(w, mem)
    early = not w.did('START')
    w.t = at + 5
    S.tick(w, mem)
    case('the pace: nothing is started and nothing asked of him; the next try is the minute the relay names, not an hour later',
         early and mem.get('needs', {}) == {} and w.did('START') == [('START', '')], (mem, w.calls))
    case('a time the relay names that is past midnight is tomorrow\'s', S.named_time('start at 00:47', T0) == datetime.datetime(2026, 10, 11, 0, 47).timestamp() and S.named_time('no time here', T0) == 0)
    w, mem = Fake(dry="would run: nothing (the day's budget is spent (12.6 of 11.0))"), {}
    S.tick(w, mem)
    waits = mem['next_try'] == S.midnight(T0) + 60 and not mem.get('needs')
    w.t, w.day, w.dry = T0 + 600, dict(day='2026-10-10', pct=21, by='his note'), DRY
    S.tick(w, mem)
    dry = [c for c in w.calls if c[0] == 'run'][-1]
    case('the budget: it waits for the new day; a figure he names for today is tried at once and is passed to the dry run AND the start (the desktop-only patch of 2026-10-09)',
         waits and w.did('START') == [('START', '21')] and dry[-2:] == ('--day-pct', '21'), (mem, w.calls))
    case('the day\'s figure names its day: yesterday\'s file is no figure, a broken one is none',
         S.day_pct(dict(day='2026-10-09', pct=21), T0) == '' and S.day_pct(dict(day='2026-10-10', pct='x'), T0) == '' and S.day_pct(None, T0) == '' and S.day_pct(dict(day='2026-10-10', pct=16.5), T0) == '16.5')

    # ---- a run that is going, and one that ended ----
    w, mem = Fake(status='RUNNING: relay 20261010-140000-5 a unit'), {}
    S.tick(w, mem)
    young = not w.did('stop') and w.did('hold')                     # eight hours old: left to run, its hold renewed
    w.t = datetime.datetime(2026, 10, 11, 1, 0).timestamp()         # eleven hours after it began
    S.tick(w, mem)
    S.tick(w, mem)
    case('a run is left to run and its hold renewed; older than its stop-after hours it is asked once to stop before its next leg',
         young and len(w.did('stop')) == 1, w.calls)
    w, mem = Fake(status='RUNNING: relay 20261010-140000-5 a unit'), {}
    S.tick(w, mem)
    w.status = 'NOT RUNNING. Last run 20261010-140000-5: leg 35 ended TIMEOUT (35 legs).'
    out = S.tick(w, mem)
    waited = not w.did('START') and mem['next_try'] == T0 + S.QUIET and any('ended TIMEOUT' in d for d in out['did'])
    w.t = T0 + S.QUIET + 1
    S.tick(w, mem)
    case('a leg cut at its limit ends the run: the next run is started after the quiet minutes, with nobody asked',
         waited and w.did('START') and not mem.get('needs'), (mem, w.calls))
    w, mem = Fake(), {}
    w.after_start = 'NOT RUNNING. Last run 20261010-220100-1: error: KeyError (0 legs).'
    waits_seen = []
    for _ in range(3):
        S.tick(w, mem)
        waits_seen.append(mem['wait'])
        w.t = mem['next_try'] + 1
    case('a start that brings no run waits longer each time, and the third in a row is said under "Needs you" with the reason',
         waits_seen == [1320, 2640, 3600] and len(w.did('START')) == 3 and 'error: KeyError' in mem['needs'].get('again', ''), (waits_seen, mem))
    w.after_start = GOING
    S.tick(w, mem)
    case('once a run goes, what was asked of him about the loop is taken back and the waits are short again', mem['needs'] == {} and mem['wait'] == S.QUIET and mem['same'] == ['', 0], mem)
    w, mem = Fake(), {}
    w.hold = (1, 'relay: the build is held by laptop-relay-test until 02:48')
    S.tick(w, mem)
    case('a hold that is another session\'s: no start, and he is told whose it is', not w.did('START') and 'laptop-relay-test' in mem['needs']['hold'], mem)
    w, mem = Fake(), {}
    w.stop = True
    out = S.tick(w, mem)
    case('steward.stop makes it stand by: nothing is started or mended', out['standby'] and not w.did('START') and not w.did('hold'), (out, w.calls))
    w = Fake()
    out = S.tick(w, {}, acting=False)
    case('a look that only looks asks the relay its status and nothing else', w.calls == [('status',)] and out['reason'].endswith('has this project open'), w.calls)

    # ---- the state file ----
    mem = dict(needs=dict(unity='a leftover batch-mode Unity (pid 64552) holds the work checkout'), next_try=T0 + 660)
    out = dict(run='', on='', last='x', reason='the work checkout cannot be used: a Unity editor has this project open', legs=0, did=[], standby='')
    slow = dict(day=['Today: about 12.6% of the week used by the relay', 'Needs you: rv-12 ended BLOCKED: which of two fixes'], tip='dd77f34fba67', landings=['lane/show/landing-x d6467344a: on integration\'s tip, gated green'],
                answers=[dict(id='b2', go='write', when='2026-10-10 10:52:06', title='Later'), dict(id='b1', go='nothing', when='2026-10-10 00:36:23', title='Earlier')])
    s = S.state_of(None, out, mem, slow)
    text = S.text_of(s, 'DESKTOP', ('unity',))
    case('the state file says, in this order: the loop and why it stands still with its next try, the day, what needs him, his untaken answers oldest first, integration and the landing branches',
         text.index('**Loop:** NOT RUNNING') < text.index('**Today:**') < text.index('## Needs you') < text.index('pid 64552') < text.index('Earlier') < text.index('Later') < text.index('dd77f34f') < text.index('gated green')
         and 'Next try 10-10 22:11' in text and 'since 10-10 00:36' in text and '- the relay: rv-12 ended BLOCKED' in text and '**Needs you:**' not in text
         and 'nothing from the loop' in S.text_of(S.state_of(None, out, {}, dict(slow, day=['Needs you: nothing.'])), 'DESKTOP', ()), text)
    f = tmp / 'STATE.md'
    case('the state file is written when it changed and left alone when it did not', S.put(f, text) is True and S.put(f, text) is False and S.put(f, text + 'x') is True and not (tmp / 'STATE.md.tmp').exists())
    ps1 = S.start_script()
    between = json.loads(ps1.split("$env:TW_BETWEEN_UNITS = '")[1].splitlines()[0].rstrip("'"))
    case('the start script hands the runner the card round for between two units: route, then gates, of this checkout, on the board; and no day figure unless one is passed',
         [c[3] for c in between] == ['route', 'gates'] and all(c[2].endswith('Tools/assetboard/idearoute.py') and c[-2:] == ['--board', S.BOARD.as_posix()] for c in between)
         and '--max-legs 0 --who $Who $day < NUL' in ps1 and "[string]$DayPct = ''" in ps1, between)
    home = tmp / 'home'
    home.mkdir()
    first = S.take_lock(home)
    S.put(home / 'lock.json', json.dumps(dict(pid=4, since='x')))        # a process number that is nobody's steward
    case('one steward a machine: the lock is taken, and a lock whose steward is gone or silent is taken over', first and S.take_lock(home) and S.load(home / 'lock.json')['pid'] != 4)
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
