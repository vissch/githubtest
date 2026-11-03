"""The autoloop's round, one step a module: what Tools/supervisor/modules/loop_NN_<step>.py share.

Each loop module calls ONE step of Tools/assetboard/autoloop.py (autoloop.step, its STEPS) and returns that step's log
lines; the step's code stays in autoloop.py, unchanged. This file keeps the round a round:

- Order: main.py gives modules their turn in file-name order; loop_01_back .. loop_11_found are numbered in
  autoloop.STEPS's order (test_loop_modules.py checks the two agree).
- One World a pass: every loop module of one main.py pass (one ctx.now) gets the same World, so what one step leaves
  on it reaches the next (tidy's w.tidied for feed's starts, the round's found World), and the lines said so far
  (found's idle_why reads feed's).
- Lock-step: a round is due ROUND_S after the last; every loop module turned in the pass that opens a round runs its
  step, in every other pass none does. A module switched on later joins the next round, not a pass of its own.
- dry: autoloop's print-only World (acting=False) inside DryWorld, which refuses every call that writes, ends or
  starts anything, whatever the step checks, and whose home is a throwaway copy of the loop's home. dry can not write
  a board, start a relay or end a process.
  (found.py's World in dry has its fetches refused too.)
- autoloop.stop (pause-relays.ps1 writes it) holds the loop modules as it holds the autoloop.
- 'on' runs a step only when its whole GROUP is on, and only when the live autoloop does not run or has logged that
  it leaves that step to the supervisor: a step never runs in both processes.

The live autoloop leaves a step to the supervisor when that step's switch is 'on' in modules.json
(autoloop.supervised()), so a step never runs in both.
"""
import shutil
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import autoloop  # noqa: E402

ROUND_S = 300           # the autoloop's own --watch 300
_PASS = {}              # {(supervisor home, now, dry): {'w': World, 'lines': [...]}} of the current pass only


def make_world(ctx):
    """The autoloop's World on the real boards and home; acting only in 'on'. Tests put a stand-in here."""
    return autoloop.World(acting=not ctx.dry)


class DryWorld:
    """A World that changes nothing: reads go to the World inside, every write is refused here. Its home is a copy."""

    READ_GIT = ('status', 'ls-files', 'rev-parse', 'for-each-ref', 'merge-base', 'cherry', 'diff', 'log', 'cat-file',
                'rev-list', 'show')
    REFUSED = {
        'kill': (1, 'dry: nothing is ended'), 'detach': (1, 'dry: nothing is detached'),
        'push_as': (1, 'dry: nothing is pushed'), 'put_aside': (False, 'dry: nothing is put aside'),
        'add': (False, 'dry: nothing is queued'), 'prio': (1, 'dry: no priority is set'),
        'pull': (1, 'dry: the board is not pulled'), 'unqueue': False, 'park_brief': '', 'take': None,
        'log_decision': None, 'leg': ('dry: no leg', 0.0), 'book': (1, 'dry: nothing is booked'),
        'start': 'would start'}

    def __init__(self, inner, home):
        self.__dict__['inner'] = inner
        self.__dict__['home'] = Path(home)
        self.__dict__['acting'] = False
        self.__dict__['refused'] = []

    def __getattr__(self, name):
        if name in self.REFUSED:
            def no(*a, **k):
                self.refused.append((name, a))
                return self.REFUSED[name]
            return no
        return getattr(self.inner, name)

    def __setattr__(self, name, value):
        if name in ('acting', 'home', 'inner'):
            raise AttributeError(f'DryWorld.{name} is fixed')
        self.__dict__[name] = value

    def commit_push(self, board, paths, message, push=True):
        self.refused.append(('commit_push', (board, message)))
        return 'would commit: ' + message

    def ideas(self, write):
        return self.inner.ideas(False)

    def found(self):
        """found.py's World, with its git fetches refused (reviews' fw.tip() fetches): it only reads in dry."""
        fw = self.inner.found()
        real = getattr(fw, 'git', None)
        if real is not None:
            def git(*a):
                if a[:1] == ('fetch',):
                    self.refused.append(('found git', a))
                    return 1, 'dry: no fetch'
                return real(*a)
            fw.git = git
        return fw

    def git(self, board, *a):
        b = a[1:] if a[:1] == ('--no-optional-locks',) else a
        if b and (b[0] in self.READ_GIT or b[:2] == ('worktree', 'list')):
            return self.inner.git(board, *a)
        self.refused.append(('git', a))
        return 1, 'dry: git ' + ' '.join(map(str, a[:2])) + ' refused'

    def relay(self, board, *a):
        if a and (a[0] in ('status', 'day') or (a[0] == 'run' and '--dry-run' in a)):
            return self.inner.relay(board, *a)
        self.refused.append(('relay', a))
        return 1, 'dry: relay ' + ' '.join(map(str, a[:2])) + ' refused'


def _copy_home(src):
    """A throwaway copy of the loop's home for a dry pass: its state is read as it is, a write lands in the copy.
    One fixed folder, made again each dry pass, so a restart leaves nothing behind."""
    dst = Path(tempfile.gettempdir()) / 'tw-sup-loopdry'
    shutil.rmtree(dst, ignore_errors=True)
    dst.mkdir(parents=True, exist_ok=True)
    if Path(src).is_dir():
        shutil.copytree(src, dst, dirs_exist_ok=True, ignore=shutil.ignore_patterns('*.log', 'lock.json', 'found'))
    return dst


def _drop(key):
    _PASS.pop(key, None)


# Steps that share state inside a round or in the loop's home switch over together: answers and attempts both write
# attempts.json (loopsteps.answered / dropped_by vs attempts' load-then-save); tidy hands feed w.tidied, feed hands
# found its lines, reviews and found share the round's found World.
GROUPS = (('answers', 'attempts'), ('tidy', 'feed', 'reviews', 'found'))
HANDED = 'left to the supervisor'       # what autoloop.main() logs when supervised() leaves steps to the supervisor
LOG_TAIL = 65536


def handed_over(home, name, now):
    """'' when the live autoloop leaves `name` to the supervisor (it said so in its log's tail) or does not run (no
    beat within autoloop.STALE); else why the step must not run here: it would run in both."""
    beat = (autoloop.load(Path(home) / 'beat.json', {}) or {}).get('at', 0)
    if now - beat >= autoloop.STALE:
        return ''
    try:
        with open(Path(home) / 'autoloop.log', 'rb') as f:
            f.seek(0, 2)
            f.seek(max(0, f.tell() - LOG_TAIL))
            tail = f.read().decode('utf-8', 'replace').splitlines()
    except OSError:
        tail = []
    said = [l for l in tail if HANDED in l]
    if said and name in [s.strip() for s in said[-1].rsplit('):', 1)[-1].split(',')]:
        return ''
    return (f'the autoloop runs (beat {int(now - beat)} s ago) and has not said it leaves {name} to the supervisor '
            '(its checkout lacks autoloop.supervised(), or it has not read the switch yet): not run, so it does not run twice')


def refuse_on(ctx, name, home):
    """Why a step switched 'on' must not run this round, or ''."""
    modes = core.read_json(Path(ctx.home) / 'modules.json', {}) or {}
    for g in GROUPS:
        if name in g:
            off = [s for s in g if modes.get('loop_' + s) != 'on']
            if off:
                return (f'not run: {name} switches over with {", ".join(off)} (they share state in a round): '
                        'switch the group on in one edit')
    return handed_over(home, name, ctx.now)


def world(ctx):
    """(World, the lines said this round so far) for this pass and mode; a new pass drops the last one's."""
    key = (str(ctx.home), ctx.now, bool(ctx.dry))
    for k in [k for k in _PASS if k[:2] != key[:2]]:
        _drop(k)
    if key not in _PASS:
        w = make_world(ctx)
        p = {'home': Path(w.home), 'lines': [], 'copy': None}
        if ctx.dry:
            p['copy'] = _copy_home(w.home)
            w = DryWorld(w, p['copy'])
        w.round_fw = autoloop._NO_FW
        p['w'] = w
        _PASS[key] = p
    return _PASS[key]


def round_due(ctx):
    """True when this pass runs a round: it opened one (ROUND_S after the last) or is the pass that did."""
    st = core.read_state('loop_round', ctx.home, {}) or {}
    if st.get('pass') == ctx.now:
        return True
    if ctx.now - (st.get('pass') or 0) < ROUND_S:
        return False
    core.write_state('loop_round', {'pass': ctx.now}, ctx.home)
    return True


def run_step(ctx, name):
    """One loop module's turn: its step of the autoloop's round, when this pass is a round."""
    if not round_due(ctx):
        return []
    p = world(ctx)
    if (p['home'] / 'autoloop.stop').exists():
        return [f'{name}: autoloop.stop is in its folder: standing by']
    if not ctx.dry:
        why = refuse_on(ctx, name, p['home'])
        if why:
            return [f'{name}: {why}']
    got = autoloop.step(p['w'], name, p['lines'])
    p['lines'] += got
    return got
