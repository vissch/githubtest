"""The supervisor's one-line hooks into the relay's picker (Tools/relay/sources/lane.py) and the loop
(Tools/assetboard/autoloop.py all_done, loopsteps.attempts). Each is inert unless its module's switch is 'on'
(modules.json in the home, else the module's DEFAULT_MODE), and none ever raises into its caller: a hook that
fails answers as if the supervisor were not there.

    skip(uid, board, lane)   '' or why the picker passes the unit by: the registry calls it done (registry_refresh
                             'on'), its goal family is over the cap (families 'on', spend.capped), or this board's
                             relay failed it in its newest run (retry 'on': the cool-down)
    done_ids(units)          the ids of {uid: unit} the registry calls done (registry_refresh 'on'), else set()
    retry_park(uid)          None when retry is not 'on' or its state is older than STALE_S (today's rule in
                             loopsteps.attempts holds), else (park?, why) from retry's state

A caller loads this file by its path (load()), so no name of the supervisor's shadows one of its own.
"""
import importlib.util
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
if str(HERE) not in sys.path:
    sys.path.append(str(HERE))          # appended: never ahead of the caller's own modules
import core  # noqa: E402

STALE_S = 1800      # as spend.STALE_S: a module state older than this is not acted on, its supervisor has stopped


def load(path=None):
    """This file as a module, for a caller outside Tools/supervisor; None when it cannot be loaded."""
    try:
        p = Path(path or __file__)
        spec = importlib.util.spec_from_file_location('tw_sup_hooks', p)
        m = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(m)
        return m
    except Exception:   # noqa: BLE001
        return None


_MEMO = {}     # the picker asks for every queued unit (~600 on the main board): a switch or a board's stop records
#                are read again only when their file or folder changed (one stat a unit, not a read and a glob)


def _stamp(*paths):
    out = []
    for p in paths:
        try:
            out.append((str(p), p.stat().st_mtime_ns))
        except OSError:
            out.append((str(p), None))
    return tuple(out)


def _memo(key, stamp, fn):
    hit = _MEMO.get(key)
    if hit and hit[0] == stamp:
        return hit[1]
    val = fn()
    _MEMO[key] = (stamp, val)
    return val


def on(name, h=None):
    """True only when the module's switch reads 'on'."""
    try:
        home = Path(h or core.home())
        return _memo(('on', name, str(home)), _stamp(home / 'modules.json', HERE / 'modules' / f'{name}.py'), lambda: _on(name, home))
    except Exception:   # noqa: BLE001
        return False


def _on(name, h=None):
    try:
        over = core.read_json(Path(h or core.home()) / 'modules.json', {}) or {}
        if name in over:
            return over[name] == 'on'
        src = (HERE / 'modules' / f'{name}.py').read_text(encoding='utf-8')
        m = re.search(r"^DEFAULT_MODE\s*=\s*['\"](\w+)['\"]", src, re.M)
        return bool(m) and m.group(1) == 'on'
    except Exception:   # noqa: BLE001
        return False


def relay_of(board):
    """'1' for tw3d-board, 'N' for tw3d-board-N, '' for any other folder (a test board)."""
    m = re.fullmatch(r'tw3d-board(?:-(\d+))?', Path(str(board or '')).name)
    return (m.group(1) or '1') if m else ''


def last_units(board):
    """{uid: verdict} of the board's newest stop record (run ids sort by time), or {}. The newest, even when it
    worked no unit: a run in between (one that found nothing else to do) ends the cool-down, so a relay whose
    queue holds only that unit is kept off it once, never for good."""
    folders = sorted(Path(board).glob('relay/*/stops'))       # a new stop record changes its folder's time
    return _memo(('stops', str(board)), _stamp(*folders), lambda: _last_units(board))


def _last_units(board):
    stops = sorted(Path(board).glob('relay/*/stops/*.json'), key=lambda p: p.name)
    for p in reversed(stops):
        d = core.read_json(p, None)
        if isinstance(d, dict):
            return d.get('units') or {}
    return {}


def skip(uid, board=None, lane=None, h=None):
    try:
        if on('registry_refresh', h):
            import registry
            done, where = registry.is_done(uid, lane, h)
            if done:
                return f'done ({where})'
        try:                                    # its own try: a fault here does not skip the cool-down below
            if on('families', h):               # a goal family over the owner's cap waits on his one brief (spend.capped):
                import spend                    # the running relay's picker does not take it, and no board is written
                cap = spend.capped(uid, h)
                if cap:
                    return f'over the cap: its goal family {cap["root"]} waits on his brief {cap["brief"]}'
        except Exception:   # noqa: BLE001
            pass
        if board and on('retry', h):
            import retry
            last = last_units(board)
            if retry.cooling(last, uid):
                return f'cool-down: the newest run on this board ended {uid} {last[uid]}'
    except Exception:   # noqa: BLE001
        return ''
    return ''


def done_ids(units, h=None):
    try:
        if on('registry_refresh', h):
            import registry
            return set(registry.done_of(units, h))
    except Exception:   # noqa: BLE001
        pass
    return set()


def retry_park(uid, h=None, now=None):
    try:
        if not on('retry', h):
            return None
        st = core.read_state('retry', h) or {}
        if (now or time.time()) - float(st.get('at') or 0) > STALE_S:
            return None         # main.py stopped: an old park=True would park a unit again every round (the reviewer, S.3)
        u = (st.get('units') or {}).get(uid) or {}
        return bool(u.get('park')), str(u.get('why') or '')
    except Exception:   # noqa: BLE001
        return None
