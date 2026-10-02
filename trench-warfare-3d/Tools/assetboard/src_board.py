"""What the two-station job board says: each item's stages and their states, and which assets an item is about.

States are not stored on the board; Tools/pipeline/pipeline.py derives them, so this asks it (Board, evaluate).
A machine with no board repo gets a warning and an empty list.
"""
import re
import sys
from pathlib import Path


def attach(root: Path, assets, warnings):
    sys.path.insert(0, str(root / 'Tools' / 'pipeline'))
    try:
        import pipeline
        board = pipeline.Board()
        items = board.items()
    except SystemExit as e:
        warnings.append(f'board: {e}')
        return []
    except Exception as e:   # a board this checkout's pipeline.py cannot read is not a reason to have no site
        warnings.append(f'board: could not be read ({type(e).__name__}: {e})')
        return []
    out = []
    for iid, item in items.items():
        try:
            states = pipeline.evaluate(item, board)
        except (SystemExit, Exception) as e:
            warnings.append(f'board: {iid} could not be evaluated ({e})')
            states = {}
        stages = [dict(id=s['id'], station=s.get('station', ''), role=s.get('role', ''), notes=s.get('notes', ''),
                       state=states.get(s['id'], {}).get('state', 'UNKNOWN'), reason=states.get(s['id'], {}).get('reason', ''))
                  for s in item['stages']]
        # an item is ABOUT an asset when its id or title names it, or a stage reads a file under the asset's art
        # roots; a mention in a stage's notes is not enough (the melee item names the officer and is not about his art)
        text = iid + ' ' + item.get('title', '')
        inputs = [i for s in item['stages'] for i in s.get('inputs', []) + s.get('outputs', [])]
        about = []
        for aid, a in assets.items():
            rx = re.compile(a['mention'], re.I) if a.get('mention') else None
            roots = tuple('trench-warfare-3d/Assets/_Project/' + r for r in a['roots'])
            under = any(i.startswith(roots) or (i.endswith('/') and any(r.startswith(i) for r in roots)) for i in inputs) if roots else False
            if (rx and rx.search(text)) or under:
                about.append(aid)
                for s in stages:
                    a['board'].append(dict(item=iid, title=item.get('title', ''), stage=s['id'], state=s['state'], station=s['station']))
        out.append(dict(id=iid, title=item.get('title', ''), lane=item.get('lane', ''), stages=stages, assets=about))
    return out
