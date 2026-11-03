"""Spend and fix generations per goal family, and the owner's cap on them (supervisor module, Tools/supervisor/core.py).

A goal family is every unit that works for one goal: the fix chain (loopsteps.chain_family: up to the first -gN), its
re-runs and rescue copies (-r2, -2, -b) and follow-ups named after it (-visual-review-5). The rule is
spend.goal_root() and spend.family_of(): ability-fx-captures-r2-2-g2-g1-g2, ability-fx-captures-r3-2-2 and
ability-fx-captures-r2-2-visual-review-5 are all ability-fx-captures.

Over the cap (CAP_USD spent, or a unit more than CAP_GENERATIONS fix generations deep, whichever comes first):
  'dry'  the log says what it would park and the brief it would write; nothing else changes
  'on'   ONE brief through Tools/assetboard/briefs.py (the family's spend, its best and worst stills when a leg's
         report names them, three options), and the family is marked over in state/families.json. The loop's own
         park path then takes its queued units off (loopsteps.attempts asks spend.capped()): only on a board whose
         relay is not running and a lane no leg works, so no board is written during a leg. This module writes no
         board.
State: {families: {root: {usd, depth, units, over, over_at, brief}}, at}.

WHY. One visual goal, ability-fx-captures, split into 120 fix units up to 14 generations deep and cost $990, 38% of
all relay spend in two weeks, while critics scored it about 22/100 (review of 2026-11-02). The owner's decision
(2026-11-02, docs/reference/decisions.md): a goal's chain stops at 3 generations or $100, whichever comes first, and
becomes one brief with pictures.
"""
import datetime
import re
import sys
from pathlib import Path

NAME = 'families'
EVERY_S = 600
DEFAULT_MODE = 'dry'

CAP_USD = 100.0             # the owner, 2026-11-02
CAP_GENERATIONS = 3         # a unit of a 4th fix generation is not started
LOOKBACK_DAYS = 14          # legs this recent count to a family's spend

SUP = Path(__file__).resolve().parent.parent
STILL = re.compile(r'[^\s"\'`<>|*?()\[\]]+\.(?:png|jpe?g|webp|gif)\b', re.I)
SCORE = re.compile(r'\b(\d{1,3})\s*/\s*100\b')


def _spend():
    sys.path.insert(0, str(SUP))
    import spend
    return spend


def over_why(f):
    if f['usd'] >= CAP_USD:
        return f'${f["usd"]:.0f} spent (cap ${CAP_USD:.0f})'
    if f['depth'] > CAP_GENERATIONS:
        return f'{f["depth"]} fix generations deep (cap {CAP_GENERATIONS})'
    return ''


def decide(legs, queued=()):
    """Pure. legs: replay-kit leg records, oldest first; queued: unit ids on a queue now. -> {root: family}.
    A family's figures are counted leg by leg, so `over_at` is the leg that took it over the cap, and `usd_after` what
    its legs cost after that (what the cap would have saved)."""
    sp = _spend()
    ids = [l.get('unit') or '' for l in legs if l.get('unit')] + [u for u in queued if u]
    roots = {sp.goal_root(u) for u in ids}
    fams = {}
    for l in legs:
        uid = l.get('unit')
        if not uid:
            continue
        root = sp.family_of(uid, roots)
        f = fams.setdefault(root, dict(usd=0.0, depth=0, units=[], over='', over_at=None, usd_after=0.0, legs_after=0,
                                       stills=[]))
        usd = float(l.get('cost_usd') or 0)
        if f['over']:
            f['usd_after'] += usd
            f['legs_after'] += 1
        f['usd'] += usd
        f['depth'] = max(f['depth'], sp.generations(uid))
        if uid not in f['units']:
            f['units'].append(uid)
        for m in STILL.finditer(l.get('report') or ''):
            sc = SCORE.search(l.get('report') or '')
            f['stills'].append((int(sc.group(1)) if sc else None, m.group(0), l.get('started_at') or ''))
        if not f['over'] and over_why(f):
            f['over'] = over_why(f)
            f['over_at'] = dict(unit=uid, at=l.get('started_at') or l.get('finished_at') or '', usd=round(f['usd'], 2))
    for u in queued:
        root = sp.family_of(u, roots)
        f = fams.setdefault(root, dict(usd=0.0, depth=0, units=[], over='', over_at=None, usd_after=0.0, legs_after=0,
                                       stills=[]))
        f['depth'] = max(f['depth'], sp.generations(u))
        if u not in f['units']:
            f['units'].append(u)
        if not f['over'] and over_why(f):
            f['over'], f['over_at'] = over_why(f), dict(unit=u, at='queued', usd=round(f['usd'], 2))
    return fams


def best_worst(f):
    """(best, worst) still paths that are files, by the score a report gave, else the first and the newest."""
    have = [s for s in f.get('stills') or [] if Path(s[1]).is_file()]
    if not have:
        return None, None
    scored = [s for s in have if s[0] is not None]
    if scored:
        return max(scored)[1], min(scored)[1]
    have.sort(key=lambda s: s[2])
    return have[-1][1], have[0][1]


def brief_of(root, f):
    """(title, what_for, options, why, evidence, no_evidence) of the one brief a family over the cap gets."""
    best, worst = best_worst(f)
    ev = [(best, 'the best still its legs made')] + ([(worst, 'the worst still')] if worst and worst != best else []) \
        if best else []
    title = f'{root}: stopped at ${f["usd"]:.0f}, {len(f["units"])} units, {f["depth"]} fix generations'
    what_for = (f'The goal {root} went over your cap ({f["over"]}), so its queued units are off the queues. '
                f'What happens to the goal now?')
    options = ['Stop here: keep the best result, build nothing more',
               'Go on: put its units back on the queue',
               'Change it: write in a note what to do instead']
    why = f'Over your cap of ${CAP_USD:.0f} or {CAP_GENERATIONS} fix generations; more rounds of the same fix rarely pass.'
    return title, what_for, options, why, ev, ('' if ev else 'no leg report of the family names a still that is on disk')


def write_brief(root, f):
    """Write the family's one brief (Tools/assetboard/briefs.py). Returns its id."""
    sys.path.insert(0, str(SUP.parent / 'assetboard'))
    import briefs
    title, what_for, options, why, ev, none = brief_of(root, f)
    b = briefs.add(briefs.folder(), title=title, what_for=what_for, options=options, why=why, evidence=ev,
                   no_evidence=none, about=f'goal family {root}', by='supervisor/families.py')
    return b['id']


def answered(bid):
    """True when the owner answered the brief (Tools/assetboard/briefs.py): from then on his answer is what happens,
    and spend.capped() parks nothing more of the family."""
    try:
        sys.path.insert(0, str(SUP.parent / 'assetboard'))
        import briefs
        hit = [b for b in briefs.read_all(briefs.folder()) if b['id'] == bid]
        return bool(hit) and hit[0].get('state') == 'answered'
    except Exception:  # noqa: BLE001 - unread is not answered
        return False


def queued_ids():
    """The unit ids on every board's queue now (read-only: no board is pulled or written)."""
    gh = Path(r'C:\Users\PC\Documents\GitHub')
    out = set()
    for q in gh.glob('tw3d-board*/relay/queue/*.json'):
        if not (q.parent.parent / 'done' / q.name).exists():
            out.add(q.stem)
    return out


def records(now):
    sys.path.insert(0, str(SUP))
    from replay import build_fixtures
    since = (now - datetime.timedelta(days=LOOKBACK_DAYS)).strftime('%Y%m%d')
    recs = build_fixtures.build(since, (now + datetime.timedelta(days=1)).strftime('%Y%m%d'))
    return [r for r in recs if r['t'] == 'leg']


def run(ctx, legs=None, queued=None):
    now = datetime.datetime.fromtimestamp(ctx.now)
    legs = records(now) if legs is None else legs
    queued = queued_ids() if queued is None else queued
    fams = decide(sorted(legs, key=lambda l: l.get('started_at') or ''), queued)
    old = (ctx.state() or {}).get('families') or {}
    out, keep = [], {}
    for root, f in sorted(fams.items()):
        waiting = sorted(u for u in f['units'] if u in queued)
        rec = dict(usd=round(f['usd'], 2), depth=f['depth'], units=len(f['units']), over=f['over'], over_at=f['over_at'],
                   queued=waiting, brief=(old.get(root) or {}).get('brief', ''),
                   answered=bool((old.get(root) or {}).get('answered')))
        if rec['brief'] and not rec['answered'] and answered(rec['brief']):
            rec['answered'] = True
            out.append(f'{root}: the owner answered its brief {rec["brief"]}: nothing more of it is parked')
        if f['over'] and waiting and not rec['brief']:
            if ctx.dry:
                title = brief_of(root, f)[0]
                out.append(f'{root} is over the cap ({f["over"]}): would park {len(waiting)} queued '
                           f'({", ".join(waiting[:4])}{" ..." if len(waiting) > 4 else ""}) and write the brief "{title}"')
            else:
                try:
                    rec['brief'] = write_brief(root, f)
                    out.append(f'{root} is over the cap ({f["over"]}): brief {rec["brief"]}; the loop parks its '
                               f'{len(waiting)} queued units')
                except Exception as e:  # noqa: BLE001 - no brief, no park (spend.capped): tried next turn
                    out.append(f'{root} is over the cap, its brief was refused: {str(e)[:160]}')
        if f['over'] or f['usd'] >= CAP_USD / 2 or waiting:
            keep[root] = rec
    ctx.save(dict(families=keep, at=ctx.now, cap_usd=CAP_USD, cap_generations=CAP_GENERATIONS))
    over = sorted(r for r, f in keep.items() if f['over'])
    return out or [f'{len(fams)} goal families; over the cap: {", ".join(over) or "none"}']
