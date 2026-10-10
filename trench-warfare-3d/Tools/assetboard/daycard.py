#!/usr/bin/env python3
"""The day's card: what the loop did today, in a few lines a person reads in a minute. No model is in it.

    python Tools/assetboard/daycard.py                today's card, printed (reads only)
    python Tools/assetboard/daycard.py 2026-10-10     that day's

WHY. The owner, on the evening of 2026-10-10: "why is there not decisions that came out of this run, what was done
exactly?" The answer was in six places and took a session to add up: 60 legs, 12.6 percent of the week, eleven first
steps, nothing of the relay landed, nine of his answers untaken. The card adds it up every few minutes for no
tokens: the steward writes it under "Today" in STATE.md.

What it counts, and from where:
  landed       the landing queue's own record (landed.jsonl): each landing, and whether it was alone or on which click
  tokens       the relay's leg records on the board, started that day, by the kind of work each was paid as, against
               the split of limits.json (finish, fix, critique); a leg with no cost in its record is counted at the
               usual cost and said so
  found        what the looks at old work handed in that day and what became of it (found.py taken.json)
  decided      the cards that were closed that day, and how many of his answers nobody has taken up yet
  stood still  the minutes the steward saw no run going, by why (its own count, kept in its memory)
"""
import datetime
import json
import os
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
KINDS = ('finish', 'fix', 'critique')
USUAL_USD = 3.0         # a leg whose record holds no cost (it was cut): the relay's own guess, limits.json usual_leg_usd


def load(f: Path, default):
    try:
        return json.loads(f.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def kind_of(rec):
    """The share a leg was paid from: what its record says, else what its source and role say (the relay's rule)."""
    if rec.get('kind') in KINDS:
        return rec['kind']
    if rec.get('source') == 'critique':
        return 'critique'
    return 'fix' if rec.get('role') == 'review-fix' else 'finish'


def local_day(stamp):
    """The local day of a leg's time ('2026-10-10T15:24:30Z'), or ''."""
    try:
        t = datetime.datetime.strptime(str(stamp)[:19], '%Y-%m-%dT%H:%M:%S').replace(tzinfo=datetime.timezone.utc)
    except ValueError:
        return ''
    return t.astimezone().strftime('%Y-%m-%d')


def spend(legs, day):
    """({kind: usd}, legs counted, of them guessed) for the legs started that day. A retrospective is nobody's share."""
    out, n, guessed = {k: 0.0 for k in KINDS}, 0, 0
    for rec in legs:
        if local_day(rec.get('started_at') or rec.get('finished_at')) != day or rec.get('source') == 'retro':
            continue
        c = rec.get('cost_usd')
        if not isinstance(c, (int, float)) or isinstance(c, bool):
            c, guessed = USUAL_USD, guessed + 1
        out[kind_of(rec)] += float(c)
        n += 1
    return out, n, guessed


def card(day, landed=(), legs=(), shares=None, taken=(), briefs=(), untaken=(), idle=None):
    """The card's lines. `landed` are the landing queue's lines, `legs` the relay's leg records, `shares` the split
    {kind: percent}, `taken` found.py's ledger, `briefs` every brief, `untaken` his answers nobody took up
    ([{when, title}]), `idle` {why: seconds} of that day."""
    shares = shares or dict(finish=60, fix=25, critique=15)
    out = []
    mine = [l for l in landed if str(l.get('when', '')).startswith(day)]
    alone = sum(1 for l in mine if (l.get('on') or {}).get('alone'))
    if mine:
        out.append(f'- **Landed: {len(mine)}** ({alone} by themselves, {len(mine) - alone} on your click): '
                   + '; '.join(f'{l["lane"].split("/", 2)[-1]} at `{str(l.get("head", ""))[:8]}`' + ('' if (l.get('on') or {}).get('alone') else f' (card {(l.get("on") or {}).get("click", "?")})') for l in mine[:8])
                   + (f' and {len(mine) - 8} more' if len(mine) > 8 else ''))
    else:
        out.append('- **Landed: nothing** through the landing queue')
    usd, n, guessed = spend(legs, day)
    total = sum(usd.values())
    if n:
        parts = ', '.join(f'{k} {usd[k] / total * 100:.0f}% (its share is {shares.get(k, 0):g}%)' for k in KINDS) if total else ''
        off = [k for k in KINDS if total and abs(usd[k] / total * 100 - shares.get(k, 0)) > 15]
        out.append(f'- **Tokens: {n} legs**, by kind of work: {parts}' + (f'; {guessed} legs had no cost on record and are counted at the usual one' if guessed else '')
                   + (f'. Off the split by more than 15 points: {", ".join(off)}' if off else ''))
    else:
        out.append('- **Tokens: no leg ran**')
    looks = sorted({t.get('look') for t in taken if str(t.get('when', '')).startswith(day) and t.get('look')})
    mine_t = [t for t in taken if str(t.get('when', '')).startswith(day)]
    if mine_t or looks:
        went = {w: sum(1 for t in mine_t if t.get('went') == w) for w in ('queue', 'card', 'dropped')}
        out.append(f'- **Found: {len(mine_t)} tasks** from {len(looks)} looks at old work: {went["queue"]} queued as fixes, {went["card"]} are cards for you, {went["dropped"]} dropped')
    else:
        out.append('- **Found: no look at old work was taken up**')
    closed = [b for b in briefs if b.get('state') == 'answered' and str((b.get('answer') or {}).get('when', '')).startswith(day)]
    oldest = min((a.get('when', '') for a in untaken), default='')
    out.append(f'- **Decided: {len(closed)} of your answers were carried out**; {len(untaken)} wait' + (f', the oldest since {oldest[5:16]}' if untaken else ''))
    idle = {k: v for k, v in (idle or {}).items() if v >= 60}
    if idle:
        out.append(f'- **Stood still: {sum(idle.values()) / 3600:.1f} hours**: ' + ', '.join(f'{v / 3600:.1f} h {k}' for k, v in sorted(idle.items(), key=lambda kv: -kv[1])))
    else:
        out.append('- **Stood still: not at all** that the steward saw')
    return out


def read(day, board: Path = None, local: Path = None):
    """Everything the card needs, read from this machine: the keyword arguments of card()."""
    import briefs as B
    import notes
    board, local = Path(board or GH / 'tw3d-board'), Path(local or build.LOCAL.parent)
    cut = (datetime.datetime.strptime(day, '%Y-%m-%d') - datetime.timedelta(days=1)).strftime('%Y%m%d')
    legs = [load(f, {}) for f in board.glob('relay/*/legs/*.json') if not (f.name[:8].isdigit() and f.name[:8] < cut)]
    landed = []
    try:
        landed = [json.loads(l) for l in (local / 'landq' / 'landed.jsonl').read_text(encoding='utf-8').splitlines() if l.strip()]
    except (OSError, ValueError):
        pass
    every = B.read_all(B.folder())
    untaken = [dict(when=a['when'], title=a['title']) for a in B.answers(every, notes.read_all(notes.folder()))]
    idle = (load(local / 'steward' / 'mem.json', {}).get('idle') or {}).get(day) or {}
    return dict(landed=landed, legs=legs, taken=load(local / 'found' / 'taken.json', []), briefs=every, untaken=untaken, idle=idle,
                shares=load(local / 'relay' / 'shares.json', None))


def main(argv=None):
    args = (argv if argv is not None else sys.argv[1:])
    day = args[0] if args else f'{datetime.datetime.now():%Y-%m-%d}'
    print(f'## {day}')
    print('\n'.join(card(day, **read(day))))
    return 0


if __name__ == '__main__':
    sys.exit(main())
