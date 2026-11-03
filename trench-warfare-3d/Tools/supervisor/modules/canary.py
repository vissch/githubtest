"""The canary: a gate on a clean checkout at integration's tip, so "is the machine broken?" has an answer that no
unit's own edit can colour. Its verdict holds for an hour; a request from the sweep (ctx.other('sweep')
['canary_request']) asks for a fresh one sooner.

State: {verdict: green|red|running|unknown, at, started, tree, log, kind, line}. `started` is when the gate began:
the breaker closes only on a green canary that started after it opened.

The gate is the checkout's own Tools/gate_bg.py (the full gate, detached, its log in the checkout's git dir, its
verdict from `gate_bg.py --status`: 0 green, 2 running, else ended without green). In on it fetches integration,
checks the tip out detached in the canary checkout (made with `git worktree add --detach` when missing) and starts
it; a checkout with uncommitted files is never touched. In dry it says what it would do and starts nothing.

The checkout: TW_CANARY_CHECKOUT, else "checkout" in <home>/canary.json, else DEFAULT. Its name must start
githubtest-canary: start() detaches the checkout it is given, so a relay or lane checkout named by mistake is refused,
and so is the landing checkout (githubtest-desk-gate), where a canary would race land.py's own gate.

WHY. From 10-31 every gate on every relay checkout was red, and each leg could only say "not my edit" about its own
tree. A gate on a tree nobody edited tells the machine's fault from the unit's, and is what lets the breaker close.
"""
import os
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import core  # noqa: E402
from signatures import classify, is_machine  # noqa: E402

NAME = 'canary'
EVERY_S = 300
DEFAULT_MODE = 'dry'
DEFAULT = 'C:/Users/PC/Documents/GitHub/githubtest-canary'
LANDING = 'githubtest-desk-gate'
OWN = 'githubtest-canary'   # the checkout's name must start so: a relay or lane checkout named by mistake is refused
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'      # Tools/assetboard/found.py INTEGRATION
FRESH_S = 3600
STALL_S = 3 * 3600          # a gate still running after this long has hung: that is a red (gate-stall)
RETRY_S = 900               # after a start that failed, wait this long before the next try
REPO = Path(__file__).resolve().parents[4]


def checkout(home):
    got = os.environ.get('TW_CANARY_CHECKOUT') or (core.read_json(Path(home) / 'canary.json', {}) or {}).get('checkout')
    return Path(got or DEFAULT)


def _last(text):
    return ((text or '').strip().splitlines() or [''])[-1][:200]


def decide(state, now, request_at, status=None):
    """('start' | 'read' | 'none', new state). Pure.

    status: (code, text) of `gate_bg.py --status` when the state says running, else None: 0 green, 2 running,
    1 no gate was started there (or it could not be read: the caller passes 1), else it ended without green.
    A red's kind is the machine fault its lines name (signatures.py), else 'test' (a test or a compile error of
    integration itself: the breaker does not open on that one, the relays are what fixes it)."""
    st = dict(state or {})
    v = st.get('verdict') or 'unknown'
    if v == 'running':
        if status is None:
            return 'read', st
        code, text = status
        if code == 0:
            st.update(verdict='green', at=now, kind='', line=_last(text))
        elif now - (st.get('started') or now) > STALL_S:
            st.update(verdict='red', at=now, kind='gate-stall', line=f'no verdict after {STALL_S // 3600} h: {_last(text)}')
        elif code == 2:
            pass
        elif code == 1:
            st.update(verdict='unknown', at=now, kind='', line=_last(text))
        else:
            kind = classify(text)
            st.update(verdict='red', at=now, kind=kind if is_machine(kind) else 'test', line=_last(text))
        return 'none', st
    if st.get('error') and now - (st.get('tried') or 0) < RETRY_S:
        return 'none', st
    asked = request_at and request_at > (st.get('started') or 0)
    if v == 'unknown' or now - (st.get('at') or 0) >= FRESH_S or asked:
        return 'start', st
    return 'none', st


def git(co, *a):
    r = subprocess.run(['git', '-C', str(co), *a], capture_output=True, text=True, errors='replace',
                       stdin=subprocess.DEVNULL, timeout=600)
    if r.returncode:
        raise RuntimeError(f"git {' '.join(a)}: {(r.stderr or r.stdout).strip()[:200]}")
    return r.stdout.strip()


def gate_bg(co, *a):
    tw = Path(co) / 'trench-warfare-3d'
    r = subprocess.run([sys.executable, str(tw / 'Tools' / 'gate_bg.py'), *a], cwd=tw, capture_output=True, text=True,
                       errors='replace', stdin=subprocess.DEVNULL, timeout=300,
                       creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    return r.returncode, (r.stdout or '') + (r.stderr or '')


def tail(log, n=200):
    try:
        data = Path(log).read_bytes()[-200000:]
    except (OSError, TypeError):
        return ''
    return '\n'.join(data.replace(b'\x00', b'').decode('utf-8', 'replace').splitlines()[-n:])


def start(co):
    """Bring the canary checkout to integration's tip and start its gate. Returns (tree, log)."""
    if not (co / '.git').exists():
        git(REPO, 'fetch', '-q', 'origin', INTEGRATION)
        git(REPO, 'worktree', 'add', '--detach', str(co), 'FETCH_HEAD')
    if git(co, 'status', '--porcelain'):
        raise RuntimeError(f'{co} has uncommitted files: a canary needs a clean checkout')
    git(co, 'fetch', '-q', 'origin', INTEGRATION)
    git(co, 'checkout', '-q', '--detach', 'FETCH_HEAD')
    tree = git(co, 'rev-parse', 'HEAD^{tree}')
    code, out = gate_bg(co)
    if code:
        raise RuntimeError(f'gate_bg.py refused: {out.strip()[:200]}')
    m = re.search(r'log (\S+)', out)
    return tree, m.group(1) if m else ''


def run(ctx):
    co = checkout(ctx.home)
    if not co.name.lower().startswith(OWN):          # its git commands detach the checkout: never someone else's
        return [f'refused: {co} is not a canary checkout (its name must start {OWN}; never the landing checkout '
                f'{LANDING})']
    req = (ctx.other('sweep') or {}).get('canary_request') or {}
    st = ctx.state() or {}
    action, st = decide(st, ctx.now, req.get('at'))
    if action == 'read':
        try:
            code, text = gate_bg(co, '--status')
        except (OSError, subprocess.TimeoutExpired) as e:
            code, text = 1, f'the gate could not be read: {e}'
        if code not in (0, 2):              # red: the log's own lines say whose fault (licence, compile, a test)
            text = tail(st.get('log')) + '\n' + text
        action, st = decide(st, ctx.now, req.get('at'), (code, text))
        ctx.save(st)
        return [f"{st['verdict']}{' (' + st['kind'] + ')' if st.get('kind') else ''} on tree {st.get('tree', '?')[:10]}"]
    if action == 'none':
        return []
    why = 'asked by the sweep' if req.get('at') and req['at'] > (st.get('started') or 0) else \
        'no verdict yet' if (st.get('verdict') or 'unknown') == 'unknown' else 'the last verdict is over an hour old'
    if ctx.dry:
        return [f'would start a gate on {co} at {INTEGRATION} ({why})']
    try:
        tree, log = start(co)
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as e:
        st.update(error=str(e)[:300], tried=ctx.now)
        ctx.save(st)
        return [f'could not start: {e}']
    st = {'verdict': 'running', 'started': ctx.now, 'at': ctx.now, 'tree': tree, 'log': log, 'kind': '', 'line': ''}
    ctx.save(st)
    return [f'started a gate on {co}, tree {tree[:10]} ({why}); log {log}']
