#!/usr/bin/env python3
"""The autoloop's steps as supervisor modules (modules/loop_NN_<step>.py): the same round, one step a module.

    python Tools/supervisor/test_loop_modules.py          (from trench-warfare-3d/)

The world is test_autoloop.py's stand-in (Fake: real board folders in a temp directory, the relay, git and the briefs
faked), set so that most steps act in one round: done records to copy back, an answer to queue, a Unity left open on
a stopped relay's checkout, a relay left dirty by a TIMEOUT (tidy puts it aside; feed starts it in the same round only
because tidy said so), an empty relay to feed, a look for found. Two copies of it: one gets one autoloop.round_, the
other every loop_* module 'on' through main.py's pass. Same lines, same files, same calls. A third copy gets every
loop_* module in 'dry': no file changes and no call that writes, ends or starts anything.
"""
import json
import os
import shutil
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import test_autoloop as ta  # noqa: E402  (sets TW_SUP_HOME to a temp folder of its own)
import autoloop  # noqa: E402
import core  # noqa: E402
import looprun  # noqa: E402
import main as sup_main  # noqa: E402

results = []
T = 1_800_000_000.0


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:1500]))


class World(ta.Fake):
    """test_autoloop's Fake, with the calls a whole round makes that the Fake leaves to the machine."""

    def __init__(self, tmp):
        super().__init__(tmp, extras=(2, 3, 4))
        self.gits, self.asides, self.found_calls = [], [], 0

    def found(self):                # found.py's World: counted; its tip does not read, so reviews says so and returns
        self.found_calls += 1
        return type('FW', (), {'tip': staticmethod(lambda: ''), 'home': self.home / 'found'})()

    def git(self, board, *a):
        self.gits = self.gits + [(Path(board).name, a)]
        return 0, ''

    def rebased(self, work):
        return []

    def put_aside(self, work, n):
        self.asides = self.asides + [n]
        self.dirty = self.dirty - {work}
        return True, 'put aside as a stash'


def fixture(root):
    w = World(root)
    w.clock = T
    w.work_dirs = {1: Path('C:/gh/w1'), 2: Path('C:/gh/w2'), 3: Path('C:/gh/w3')}
    w.cos = {'lane/show/e2': Path('C:/gh/w2'), 'lane/show/e3': Path('C:/gh/w3')}
    w.dirty = {Path('C:/gh/w3')}
    ta.put(w.extras[2], 'done', 'banner-legs')                                   # back
    w.ans = [dict(id='b1', go='queue', note='n1', when='2026-11-03', title='T', option='A', text='yes',   # answers
                  unit=dict(id='ans-unit', lane='lane/show/e3', goal='g', done_when=['python', '-c', '1']))]   # relay 3's lane: not fed to 4
    w.ps = [dict(pid=501, ppid=1, name='Unity.exe', cmd='Unity.exe -projectPath C:/gh/w3/trench-warfare-3d', age=100,
                 parent='explorer.exe', parent_cmd='')]                         # editors: relay 3 has stopped
    w.text[w.extras[3]] = 'NOT RUNNING. Last run 20261103-010001-1: leg 03 ended TIMEOUT (3 legs).'   # tidy puts it aside
    ta.put(w.extras[3], 'queue', 'e-3', lane='lane/show/e3')                     # feed starts it: tidy said so
    (w.home / 'starts.json').write_text(json.dumps({'3': dict(at=T - 600, reason='x', wait=1800)}), encoding='utf-8')
    ta.put(w.main, 'queue', 'free-1', lane='lane/show/free', priority=20)         # feed gives relay 2 this lane
    w.text[w.main] = ta.RUN                                                      # the main relay runs (back waits)
    # relay 4 stands idle with an empty board and no lane left to feed it: found's idle_why says so unless it reads
    # feed's own line that relay 2 "gets" a lane, so found's `why` shows whether feed's lines reached it
    return w


def snapshot(root):
    return {str(p.relative_to(root)).replace('\\', '/'): p.read_bytes() for p in sorted(Path(root).rglob('*')) if p.is_file()}


def calls(w, root):
    keep = ('started', 'pushed', 'prios', 'added', 'taken', 'rows', 'killed', 'detached', 'unqueued', 'asides',
            'gits', 'legs', 'booked', 'briefs_written')
    d = {k: getattr(w, k, None) for k in keep}
    d['holds'], d['quiet_offs'] = getattr(w, 'holds', []), getattr(w, 'quiet_offs', [])
    return json.loads(json.dumps(d, default=str).replace(str(root).replace('\\', '\\\\'), '<root>'))


def found_patch(whys):
    real = (autoloop.found.take, autoloop.found.rota)
    autoloop.found.take = lambda fw: ['found: took 0 findings']
    autoloop.found.rota = lambda fw, why='', **k: whys.append(why) or 'found: queued the look x'
    return real


def switches(h, loop_mode):
    mods, _ = core.discover()
    core.write_json(Path(h) / 'modules.json', {m.NAME: (loop_mode if m.NAME.startswith('loop_') else 'off') for m in mods})


class View:
    """A new World each make_world call, as autoloop.World(...) is: the machine (the Fake's boards and records) is
    shared, what a step leaves on its World (w.tidied, the round's found World) is this World's own."""

    def __init__(self, inner, acting):
        self.__dict__.update(inner=inner, acting=acting)
        inner.acting = acting

    def __getattr__(self, name):
        return getattr(self.inner, name)


def supervisor_pass(w, h, mode, now=T):
    looprun.make_world = lambda ctx: View(w, not ctx.dry)
    out = sup_main.once(h, now=now)
    pre = '[dry] ' if mode == 'dry' else ''
    lines = []
    for l in out:
        name, _, said = l.partition(': ')
        assert name.startswith('loop_'), l
        assert said.startswith(pre), l
        lines.append(said[len(pre):])
    return lines


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-loopmods-'))
    try:
        mods = [m for m in core.discover()[0] if m.NAME.startswith('loop_')]
        steps = [n for n, _ in autoloop.STEPS]
        case('there is one loop_* module a step, in file-name order the same as autoloop.STEPS (round_\'s order), each '
             'off by default', [m.STEP for m in mods] == steps and [m.ORDER for m in mods] == list(range(1, len(steps) + 1))
             and all(m.DEFAULT_MODE == 'off' and m.NAME == 'loop_' + m.STEP for m in mods), [(m.NAME, m.STEP) for m in mods])

        # ---- on: the same as one round ----
        whys_a, whys_b = [], []
        real = found_patch(whys_a)
        try:
            wa = fixture(tmp / 'a')
            la = autoloop.round_(wa)
            autoloop.found.rota = lambda fw, why='', **k: whys_b.append(why) or 'found: queued the look x'
            wb = fixture(tmp / 'b')
            hb = tmp / 'sup-b'
            switches(hb, 'on')
            lb = supervisor_pass(wb, hb, 'on')
        finally:
            autoloop.found.take, autoloop.found.rota = real
        acted = [s for s in ('back:', 'answers:', 'editors:', 'tidy:', 'feed: relay 2', 'feed: relay 3', 'found:')
                 if not any(l.startswith(s) for l in la)]
        case('the fixture makes most steps act in one round (else the comparison proves little)', not acted and 3 in wa.started
             and wa.asides == [3] and wa.killed == [501], (acted, la, wa.started))
        case('every loop_* module on, in one supervisor pass: the same log lines, in the same order, as one round_',
             la == lb, '\n'.join(f'{x!r} | {y!r}' for x, y in zip(la + [''] * 9, lb + [''] * 9)))
        case('... and the same files on the boards and in the loop\'s home',
             snapshot(tmp / 'a') == snapshot(tmp / 'b'), sorted(set(snapshot(tmp / 'a').items()) ^ set(snapshot(tmp / 'b').items()))[:6])
        case('... and the same calls: starts (relay 3 only because tidy put it aside in the same round), pushes, '
             'priorities, queued units, kills, detaches, git', calls(wa, tmp / 'a') == calls(wb, tmp / 'b') and whys_a == whys_b,
             (calls(wa, tmp / 'a'), calls(wb, tmp / 'b'), whys_a, whys_b))
        case('... and the found step read feed\'s lines (its why is empty: relay 2 got a lane this round, though relay 4 '
             'stands idle) and the round made one found World, shared by reviews and found',
             whys_a == [''] and whys_b == [''] and wa.found_calls == 1 and wb.found_calls == 1
             and any(l.startswith('reviews: the integration branch did not read') for l in lb),
             (whys_a, whys_b, wa.found_calls, wb.found_calls))
        again = supervisor_pass(wb, hb, 'on', now=T + 60)
        case('lock-step: the next pass inside the round\'s 300 s runs no step', again == [] and wb.started == wa.started, again)

        # ---- dry: nothing changes ----
        wc = fixture(tmp / 'c')
        before = snapshot(tmp / 'c')
        hc = tmp / 'sup-c'
        switches(hc, 'dry')
        whys = []
        real = found_patch(whys)
        try:
            lc = supervisor_pass(wc, hc, 'dry')
        finally:
            autoloop.found.take, autoloop.found.rota = real
        empty = {k: v for k, v in calls(wc, tmp / 'c').items() if v}
        case('every loop_* module in dry: it says what it would do, and not one file changes and not one call writes, '
             'ends or starts anything (the print-only World alone still ended the Unity: end_editors asks no acting)',
             lc and any('would' in l for l in lc) and snapshot(tmp / 'c') == before and not empty and whys == [],
             (lc, empty, sorted(set(snapshot(tmp / 'c')) ^ set(before))))

        # ---- the switch-over: a step on in the supervisor is left out by the autoloop ----
        old = os.environ.get('TW_SUP_HOME')
        os.environ['TW_SUP_HOME'] = str(tmp / 'sup-s')
        core.write_json(tmp / 'sup-s' / 'modules.json', {'loop_back': 'on', 'loop_feed': 'dry', 'loop_nope': 'on', 'breaker': 'on'})
        got = autoloop.supervised()
        os.environ['TW_SUP_HOME'] = old         # test_autoloop set it on import: never the machine's own home
        whys = []
        real = found_patch(whys)
        try:
            wd = fixture(tmp / 'd')
            wd.text[wd.main] = ta.IDLE
            ld = autoloop.round_(wd, skip=got)
        finally:
            autoloop.found.take, autoloop.found.rota = real
        case('switch-over: the autoloop leaves out exactly the steps whose loop_* switch is on (not dry, not another '
             'module\'s) and still runs the rest', got == {'back'} and not any(l.startswith('back:') for l in ld)
             and not (wd.main / 'relay' / 'done' / 'banner-legs.json').exists() and any(l.startswith('feed:') for l in ld), (got, ld))

        # ---- autoloop.stop holds the modules as it holds the autoloop ----
        we = fixture(tmp / 'e')
        (we.home / 'autoloop.stop').write_text('', encoding='utf-8')
        before = snapshot(tmp / 'e')
        he = tmp / 'sup-e'
        switches(he, 'on')
        le = supervisor_pass(we, he, 'on')
        case('autoloop.stop (pause-relays.ps1) holds every loop_* module: it stands by and changes nothing',
             le and all('standing by' in l for l in le) and snapshot(tmp / 'e') == before and not we.started and not we.killed, le)

        # ---- 'on' never runs a step in both processes, nor a group split ----
        wg = fixture(tmp / 'g')
        before = snapshot(tmp / 'g')
        hg = tmp / 'sup-g'
        switches(hg, 'on')
        core.write_json(hg / 'modules.json', dict(core.read_json(hg / 'modules.json', {}), loop_tidy='off'))
        lg = supervisor_pass(wg, hg, 'on')
        split = [l for l in lg if l.split(':')[0] in ('feed', 'reviews', 'found')]
        case('a group switched on in part does not run: feed, reviews and found refuse while tidy is off (and nothing '
             'is started)', len(split) == 3 and all('switches over with tidy' in l for l in split) and not wg.started
             and not any(l.startswith('tidy') for l in lg), lg)

        wh = fixture(tmp / 'h')
        (wh.home / 'beat.json').write_text(json.dumps(dict(at=T - 30)), encoding='utf-8')
        before = snapshot(tmp / 'h')
        hh = tmp / 'sup-h'
        switches(hh, 'on')
        lh = supervisor_pass(wh, hh, 'on')
        held = lh and all('has not said it leaves' in l for l in lh) and snapshot(tmp / 'h') == before and not wh.started
        (wh.home / 'autoloop.log').write_text('11-03 09:59:00 feed: x\n11-03 10:00:00 left to the supervisor (its loop_* switch is on): '
                                              + ', '.join(steps) + '\n', encoding='utf-8')
        lh2 = supervisor_pass(wh, tmp / 'sup-h2', 'on') if switches(tmp / 'sup-h2', 'on') is None else []
        case('while the live autoloop beats, an \'on\' step runs only after the autoloop has logged that it leaves that '
             'step to the supervisor', held and any(l.startswith('feed: relay') for l in lh2) and 3 in wh.started, (lh, lh2))

        # ---- one supervisor a home (main.py --watch; start-supervisor.ps1 counts on it) ----
        hl = tmp / 'sup-lock'
        first = sup_main.take_lock(hl, now=5000)
        second = sup_main.take_lock(hl, now=5060)
        stale = sup_main.take_lock(hl, now=5000 + sup_main.STALE + 1)
        case('one --watch supervisor a home: a second is refused and says why; a lock that stopped beating is taken over',
             first == '' and 'another supervisor holds' in second and stale == '', (first, second, stale))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'\n{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
