"""Take done units' queue files off the boards, and close a Decide answer the loop cannot take.

  queue files  a queue file whose unit has a done record on any board (or that the registry calls done, when the
               registry_refresh switch is 'on') comes off its board, only while that board's relay is NOT RUNNING
               (`relay.py status`; a relay that does not answer, one whose build is held, or one the loop started
               in the last QUIET + 2 minutes counts as running: busy()). Through the loop's own path
               (autoloop.take_off): the files go in one commit, kept local on an extra board, pulled first and
               pushed on the main board, as autoloop.sweep and copy_back do.
  answers      an answer on the Decide page that the loop's log (autoloop.log) shows refused TWICE with the same
               words ("answers: <brief>: the relay refused <unit>: <why>") is closed with that reason through
               briefs.py's take (World.take), and its row goes to the loop's decisions file. The 2026-10-20 answer
               on keep-only-model-ids was refused and logged every 5 min for two weeks.

'dry' (the default) lists what it would remove or close and changes nothing.
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'assetboard'))
import hooks  # noqa: E402

NAME = 'queue_gc'
EVERY_S = 900
DEFAULT_MODE = 'dry'
REFUSED = re.compile(r' answers: (\S+): the relay refused (\S+): (.*)$')
SAME = 2


def stale(queues, extra_done=frozenset()):
    """Pure: {board: sorted ids} of the queue files whose unit is done on any board or in `extra_done`.
    `queues` {board: ({uid: unit}, {done uid})}."""
    done = set(extra_done).union(*(d for _, d in queues.values())) if queues else set(extra_done)
    return {b: sorted(u for u in q if u in done) for b, (q, _) in queues.items() if any(u in done for u in q)}


def refusals(lines, least=SAME):
    """Pure: {brief: (unit, why)} of the answers whose last `least` refusals in the log have the same words."""
    seen = {}
    for line in lines:
        m = REFUSED.search(line)
        if m:
            why = re.sub(r'\d+', 'N', m.group(3).strip())
            seen.setdefault(m.group(1), []).append((m.group(2), why, m.group(3).strip()))
    out = {}
    for bid, hits in seen.items():
        tail = hits[-least:]
        if len(tail) >= least and len({(u, w) for u, w, _ in tail}) == 1:
            out[bid] = (tail[-1][0], tail[-1][2])
    return out


HELD = 'The relay build is held by'


def busy(w, b, n):
    """'' only when board b's relay is surely idle, else why its board is left alone this turn. NOT RUNNING is not
    enough: the loop's start of the main relay takes a hold first and its start script waits its quiet minutes
    before the run shows in `status` (the loop runs in another process), so a hold, or a start by the loop within
    QUIET + 2 minutes, counts as running."""
    import autoloop
    try:
        code, text = w.relay(b, 'status')
    except Exception as e:   # noqa: BLE001
        return f'its status failed ({str(e)[:80]})'
    lanes = autoloop.parse_status(text)[0] if code == 0 else None
    if lanes is None:
        return 'its relay did not say whether it runs'
    if lanes:
        return 'its relay runs'
    if HELD in (text or ''):
        return 'its relay build is held (a start may be on its way)'
    s = (autoloop.load(w.home / 'starts.json', {}) or {}).get(str(n)) or {}
    if w.now() - s.get('at', 0) < autoloop.QUIET + 120:
        return 'the loop started its relay lately (the run may not show yet)'
    return ''


def world(dry):
    import autoloop
    return autoloop.World(acting=not dry)


def run(ctx, w=None, log_lines=None):
    import autoloop
    w = w or world(ctx.dry)
    boards = {w.main: 1, **{b: n for n, b in w.extras.items()}}
    queues = {b: autoloop.queue_of(b) for b in boards}
    units = {u: v for q, _ in queues.values() for u, v in q.items()}
    reg = hooks.done_ids(units, ctx.home)
    out = []
    for b, uids in sorted(stale(queues, reg).items(), key=lambda kv: boards[kv[0]]):
        n = boards[b]
        why = busy(w, b, n)
        if why:
            out.append(f'relay {n}: {len(uids)} queue files of done units wait: {why}')
            continue
        if ctx.dry:
            out.append(f'relay {n}: {len(uids)} queue files of done units would come off ({", ".join(uids[:5])}{", ..." if len(uids) > 5 else ""})')
            continue
        out.append(f'relay {n}: {len(uids)} queue files of done units: ' + autoloop.take_off(w, b, uids, push=(n == 1)))
    if log_lines is None:
        try:
            log_lines = (w.home / 'autoloop.log').read_text(encoding='utf-8', errors='replace').splitlines()[-5000:]
        except OSError:
            log_lines = []
    stuck = refusals(log_lines)
    if stuck:
        try:
            open_ = {a['id']: a for a in w.answers()}
        except Exception as e:   # noqa: BLE001
            return out + [f'answers: the Decide page could not be read ({str(e)[:120]})']
        for bid, (uid, why) in sorted(stuck.items()):
            a = open_.get(bid)
            if not a:
                continue                    # closed already, or no longer an answer the loop takes
            said = f'the relay refuses {uid} ({why[:160]}); closed by the supervisor after {SAME} refusals with the same reason'
            if ctx.dry:
                out.append(f'answers: {bid} would be closed: {said}')
                continue
            try:
                w.take(a, outcome=said)
                w.log_decision(f'| {str(a.get("when", ""))[:10]} | **Closed by the supervisor.** **{a.get("title", bid)}**: {said}. (owner, Decide page, note {a.get("note", "")}) |')
                out.append(f'answers: {bid} closed: {said}')
            except Exception as e:   # noqa: BLE001
                out.append(f'answers: {bid} not closed ({str(e)[:160]})')
    return out
