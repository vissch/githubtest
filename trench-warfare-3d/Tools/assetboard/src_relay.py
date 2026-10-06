"""The relay (Tools/relay): a runner that does work as a chain of short headless Claude sessions, "legs".

Read from what the runner leaves behind, so this needs none of its code:
- <home>/locks/*.json: one per checkout a live runner holds (its pid and start time, the run, the unit, the lane);
- <home>/runs/<run>/legs/<nn>/leg.json: every leg of that run (phase, state, when it started), and meter.json
  beside it (how full the leg's context is);
- <board>/relay/<station>/stops/<run>.json: how each run ended.
<home> is TW_RELAY_HOME, else %LOCALAPPDATA%/TrenchWarfare/relay.
"""
import datetime
import json
import os
from pathlib import Path


def home():
    return Path(os.environ.get('TW_RELAY_HOME') or Path(os.environ.get('LOCALAPPDATA', str(Path.home()))) / 'TrenchWarfare' / 'relay')


def read(p):
    try:
        return json.loads(Path(p).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return None


def minutes(stamp, now):
    try:
        t = datetime.datetime.strptime(stamp, '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=datetime.timezone.utc).timestamp()
        return max(0, int((now - t) / 60))
    except (TypeError, ValueError):
        return None


def unit_words(unit):
    """A unit id as words: 'house5--evidence--d1192f67' -> 'house5 evidence'."""
    parts = [p for p in (unit or '').split('--') if p]
    return ' '.join(parts[:2]) if len(parts) > 1 else (unit or '')


def runs(root: Path, now, alive):
    """Every run a live runner holds a checkout for, with its legs so far. alive(pid, pid_start) -> bool."""
    out = []
    for f in sorted((root / 'locks').glob('*.json')) if (root / 'locks').is_dir() else []:
        rec = read(f)
        if not rec or not alive(rec.get('pid'), rec.get('pid_start')):
            continue
        words = (rec.get('who') or '').split()
        run = words[1] if len(words) > 1 else ''
        legs = []
        for lf in sorted((root / 'runs' / run / 'legs').glob('*/leg.json')) if run else []:
            leg = read(lf) or {}
            meter = read(lf.parent / 'meter.json') or {}
            legs.append(dict(leg=leg.get('leg'), phase=leg.get('phase'), state=leg.get('state'), unit=leg.get('unit'), role=leg.get('role'),
                             minutes=minutes(leg.get('started_at'), now), tokens=meter.get('tokens') or leg.get('final_tokens') or 0,
                             level=meter.get('level', 'green')))
        out.append(dict(run=run, unit=words[2] if len(words) > 2 else '', lane=rec.get('lane') or '', checkout=Path(rec.get('worktree') or '').name,
                        minutes=minutes(rec.get('taken_at'), now), legs=legs, stopping=(root / 'stop.json').exists()))
    return out


def last_stop(board):
    """The newest stop record on the board, from any station."""
    files = sorted(Path(board).glob('relay/*/stops/*.json'), key=lambda p: p.name) if board else []
    rec = read(files[-1]) if files else None
    return dict(run=rec.get('run'), reason=rec.get('reason', ''), legs=rec.get('legs', 0), at=rec.get('stopped_at', '')) if rec else None


def leg_line(run):
    """What the relay is doing now, in a few words: 'house5 evidence: plan leg 1, 9 min, 128k tokens'."""
    cur = next((l for l in reversed(run['legs']) if l['state'] in ('NEW', 'RUNNING')), None)
    what = unit_words(run['unit'])
    if not cur:
        return f'{what}: checking leg {len(run["legs"])}' if run['legs'] else f'{what}: starting'
    bits = [f'{cur["phase"]} leg {cur["leg"]}']
    if cur['minutes'] is not None:
        bits.append(f'{cur["minutes"]} min')
    if cur['tokens']:
        bits.append(f'{round(cur["tokens"] / 1000)}k tokens' + ('' if cur['level'] == 'green' else f' ({cur["level"]})'))
    return f'{what}: ' + ', '.join(bits) + (' · stop asked' if run['stopping'] else '')


def collect(board, now, alive, root: Path = None):
    root = root or home()
    going = runs(root, now, alive)
    last = last_stop(board)
    does = 'runs work as a chain of short sessions'
    if not going and last:
        does = f'last run stopped: {last["reason"]} ({last["legs"]} leg{"" if last["legs"] == 1 else "s"})'
    return dict(runs=going, last=last, does=does)
