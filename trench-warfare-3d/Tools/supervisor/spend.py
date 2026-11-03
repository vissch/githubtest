#!/usr/bin/env python3
"""The spend hooks: what the relay's runner and the loop (Tools/assetboard/autoloop.py, loopsteps.py) ask the
supervisor's spend modules, in a few lines each. Read-only: nothing here writes a file.

    day_figure(env)        the day's cap as the start script takes it: budget.json's figure when the owner set the day
                           with budget.py, else `env` (TW_DAY_PCT) as before
    day_cap(now)           the same figure as a float, or None when no budget.json: the runner then keeps its own
    capacity_room(taken)   (True|False, why): may one more relay start when `taken` relays hold a slot (relay 1 counts
                           first, running or not). True unless modules/capacity.py is switched on and says no
    capped(uid)            {root, why, brief} when the unit's goal family is over the owner's cap and
                           modules/families.py is switched on, else None
    goal_root(uid)         the goal family rule (families.py groups by it; capped() matches by it)

WHY. The owner's day figure, the relay count and the goal caps live in the supervisor's home (core.home()); the old
readers (autoloop's TW_DAY_PCT, the start script's -DayPct, feed, attempts) keep working unchanged until a figure is
set or a module is switched on. A module in 'dry' or 'off' changes nothing here: its state is read only under 'on'.
"""
import datetime
import importlib.util
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _load(name):
    """A supervisor file by its path, under a name no other Tools folder uses (relay/ and assetboard/ have their own
    budget-like names on sys.path)."""
    key = f'tw_sup_{name}'
    if key not in sys.modules:
        spec = importlib.util.spec_from_file_location(key, HERE / f'{name}.py')
        m = importlib.util.module_from_spec(spec)
        sys.modules[key] = m
        spec.loader.exec_module(m)
    return sys.modules[key]


core = _load('core')
STALE_S = 1800          # a module state older than this is not acted on: its supervisor has stopped


def switch(name, default='dry', h=None):
    over = core.read_json(Path(h or core.home()) / 'modules.json', {}) or {}
    m = over.get(name, default)
    return m if m in core.MODES else 'off'


def _fresh(st, now=None):
    return isinstance(st, dict) and (now or time.time()) - float(st.get('at') or 0) <= STALE_S


# ---- the day's figure ------------------------------------------------------------------------------------------------

def day_cap(now=None, h=None):
    """budget.json's cap for now (percent points of the week), or None when none of the owner's windows holds now
    (never set, ended, or a file that does not read): the old figure then stands. `set 11` is a window too: it holds
    11 until midnight over any -DayPct or day.json."""
    budget = _load('budget')
    now = now or datetime.datetime.now()
    if budget.window(now, h) is None:          # no window of his holds now: -DayPct, day.json and TW_DAY_PCT stand
        return None
    return budget.day_pct(now, h)


def day_figure(env, now=None, h=None):
    """What the loop passes as -DayPct: budget.json's figure, else env (TW_DAY_PCT, or the loop's '11')."""
    try:
        cap = day_cap(now, h)
    except Exception:  # noqa: BLE001 - a broken budget file must not stop a start: the old figure stands
        cap = None
    return env if cap is None else ('%g' % round(cap, 2))


# ---- the relay count -------------------------------------------------------------------------------------------------

def capacity_room(taken, h=None, now=None):
    """(may start, why). Under 'on' only: taken < allowed from state/capacity.json, when that is fresh."""
    if switch('capacity', h=h) != 'on':
        return True, ''
    st = core.read_state('capacity', h)
    if not _fresh(st, now) or st.get('allowed') is None:
        return True, 'capacity has no fresh figure'
    ok = taken < int(st['allowed'])
    return ok, f'{taken} of {st["allowed"]} relays the budget carries now ({st.get("why", "")})'


# ---- goal families ---------------------------------------------------------------------------------------------------

ROUND = re.compile(r'-(?:r\d+|\d+|[b-j])$')       # a re-run of a goal (-r2, -2) or a rescue copy (-b): the same goal
GEN = re.compile(r'-g\d+')                         # a fix of a fix (loopsteps.depth): one generation


def generations(uid):
    """How many -g<digits> fix generations the id carries (loopsteps.depth)."""
    return len(re.findall(r'-g\d+(?=-g\d+|$)', uid))


def goal_root(uid):
    """The goal an id works for: up to its first -gN (loopsteps.chain_family), then every trailing round or copy
    marker off. ability-fx-captures-r2-2-g2-g1 and ability-fx-captures-r3-2-2 are both ability-fx-captures."""
    s = GEN.split(str(uid))[0]
    while True:
        t = ROUND.sub('', s)
        if t == s or t.count('-') < 1:
            return s
        s = t


def joins(root, family):
    """True when a goal root belongs to a family's root: the same, or a follow-up named after it in two words or more
    (ability-fx-captures-r2-2-visual-review-5 -> ability-fx-captures). A family root of one word joins nothing, nor
    does one more single word: look-review-z004 and look-review-z005 are two looks, not one goal."""
    if root == family:
        return True
    return family.count('-') >= 1 and root.startswith(family + '-') and '-' in root[len(family) + 1:]


def family_of(uid, roots):
    """The shortest root in `roots` the id's goal joins, else its own goal root."""
    r = goal_root(uid)
    hit = [f for f in roots if joins(r, f)]
    return min(hit, key=len) if hit else r


def capped(uid, h=None, now=None):
    """{root, why, brief} when families.py is on, fresh, and the id's goal is in a family over the cap whose one brief
    is written (no brief, no park: the owner must see why the units left) and not answered yet (once he answered,
    what he said is what happens: a unit queued from his answer is not parked again); else None."""
    if switch('families', h=h) != 'on':
        return None
    st = core.read_state('families', h)
    if not _fresh(st, now):
        return None
    r = goal_root(uid)
    for root, f in sorted((st.get('families') or {}).items(), key=lambda x: len(x[0])):
        if f.get('over') and f.get('brief') and not f.get('answered') and joins(r, root):     # his answer stands
            return dict(root=root, why=f['over'], brief=f.get('brief') or '')
    return None
