#!/usr/bin/env python3
"""An accepted idea on the pipeline's board: its route as stages, and the owner's own steps put to him as briefs.

WHY. The owner, 2026-10-07: "once a idea is accepted ... it should route through the specific agents, a game idea
should first be fleshed out by a game design agent, concepted by a concept art agent, fleshed out even more before
being made in another decision. even after that decision has been made, the idea should already be able to travel to
the next specialist agent." Asked what waits on a decision of his in the middle: "Keep moving around me".

    python Tools/assetboard/idearoute.py                    what would be put on the board, and which steps of his are ready (writes nothing)
    python Tools/assetboard/idearoute.py route [--board DIR]      write the items and units of the accepted ideas
    python Tools/assetboard/idearoute.py gates [--board DIR]      put his ready steps to him as briefs, and act on the ones he answered

An accepted idea (ideas.py) becomes an item of the board (Tools/pipeline/pipeline.py) whose stages are its route. A
step of his own is a stage of the role master, which no worker takes: `gates` puts it to him as a brief with the
pictures of the step before, and his answer completes it (Go on), sends the step before back (feedback, which makes
it and all after it stale), or drops the idea. The steps that think (THINKS) do not wait for his pick: they come
after the step before it. The steps that build do wait. An idea for a tool has no route of specialists: it is one
unit for the relay's queue, written as a file for `relay.py add --unit`.
NOTHING IS COMMITTED OR PUSHED HERE. The board is the desktop's, and the master's to push between two legs of the
relay (a push during a leg can stop the run): run `route` and `gates` there, then commit the board as the master does.
The roles a route names (ideas.py ROUTES) must be in Tools/pipeline/roles.json where the relay runs.
"""
import argparse
import datetime
import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs     # noqa: E402
import build      # noqa: E402
import ideas      # noqa: E402

THINKS = ('game-designer', 'ux', 'balance-simulator')
LAPTOP = THINKS                 # what needs no editor and no GPU; everything else is the desktop's
DOES = {
    'game-designer': 'Flesh the idea out against the game as it is: the rule, its numbers, its edge cases, what it costs the player. One page and one sketch.',
    'concept-artist': 'Two to four concepts or references of how it looks, as pictures he can pick from.',
    'balance-simulator': 'Placeholder numbers and a sweep: does it hold over seeds, what does it do to the win rates.',
    'ux': 'How the player meets it: where it is on the screen, what he presses, what he sees first. A flow drawn as a page.',
    'ui-artist': 'The element in the skin of the game, as a mock over a real capture.',
    'lowpoly': 'The model, cut and rebuilt to the budget.',
    'character-simulator': 'Its motion in the game, checked at every zoom.',
    'destruction-vfx-simulator': 'Its effects and how it breaks or dies, at every zoom.',
    'env-simulator': 'The battlefield built and judged from the standard view.',
    'sim': 'The rule in the sim, with its tests, on a sim lane.',
    'lane': 'The build on its lane, with its tests.',
    'hard-critic': 'Score what was built from its captures; name the three things to fix.',
}
GATE_OPTIONS = ('Go on: the next steps start', 'Send it back: I say below what is wrong', 'Stop here: drop this idea')
GATE_THEN = 'The steps that waited on you start'
slug = briefs.slug


def pipeline():
    sys.path.insert(0, str(build.ROOT / 'Tools' / 'pipeline'))
    import pipeline as p
    return p


def item(i):
    """The item of the board an accepted idea becomes: its route as stages. Every step that is not his own names a
    capture (a band), so each reaches him with something to look at."""
    iid, used, stages, last, gate = 'idea-' + slug(i['title'])[:40].strip('-'), set(), [], None, None
    for n, r in enumerate(i['route']):
        sid = base = slug(r['says'])[:24] or f'step-{n + 1}'
        k = 1
        while sid in used:
            k += 1
            sid = f'{base}-{k}'
        used.add(sid)
        final = n == len(i['route']) - 1
        s = dict(id=sid, station='laptop' if r['role'] in LAPTOP else 'desktop', role=r['role'])
        after = [last] if last else []
        if not r['own'] and gate and r['role'] not in THINKS:
            after.append(gate)
        if after:
            s['after'] = after
        if r['own']:
            s['notes'] = ('The landing: the word of the owner, then tw-master.' if final else
                          f'A step of the owner ("{r["says"]}"): put to him as a brief with the pictures of the step before (idearoute.py gates); his answer completes it. No worker takes it.')
            gate = gate if final else sid
        else:
            s['bands'] = ['shown']
            s['notes'] = f'{DOES.get(r["role"], "")} The idea: {i["title"]}. {i["pitch"]} Why now: {i["why_now"]}'.strip()
            if r['role'] == 'sim':
                s['lane'] = f'lane/sim/pipe-{iid}'
            last = sid
        stages.append(s)
    return dict(id=iid, title=f'{i["title"]}: {i["pitch"]} (an idea the owner accepted, {str((i.get("answer") or {}).get("when", ""))[:10]})', lane=f'lane/show/pipe-{iid}', idea=i['id'], stages=stages)


def unit_of(i, integration='claude/trench-warfare-2d-3d-plan-idt7lf'):
    """A tool or a way of working needs no route of specialists: it is one unit of the queue."""
    uid = 'idea-' + slug(i['title'])[:40].strip('-')
    check = (f"import subprocess,sys;o=subprocess.run(['git','log','--format=%B','origin/{integration}..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             f"sys.exit(0 if '[{uid}]' in o else 1)")
    return dict(id=uid, lane=f'lane/show/{uid}', role='lane', done_when=['python', '-c', check],
                goal=f'An idea the owner accepted on the board: {i["title"]}. {i["pitch"]} Why now: {i["why_now"]} Tools and docs go in commits of their own. Commit and push the lane only: do not land. '
                     f'When it is built and its tests are green, put the tag [{uid}] in a commit message on the lane; not before.')


def route(where: Path, board: Path, write=True):
    """Put every accepted idea that is not on the board yet on it: an item (items/<id>.json) for an idea with a route
    of specialists, a unit file (units/<id>.json in the ideas folder) for a tool. The idea is marked with where it
    went. Returns [(idea id, 'item' or 'unit', its id, the file)]."""
    p, out = pipeline(), []
    for i in ideas.read_all(where):
        if i['state'] != 'accepted' or i.get('routed'):
            continue
        if i['kind'] == 'tool':
            u = unit_of(i)
            bad = briefs.check_then('queued', u)
            if bad:
                raise ValueError(f'{i["id"]}: ' + '; '.join(bad))
            f, kind, rid, body = where / 'units' / f'{u["id"]}.json', 'unit', u['id'], u
        else:
            it = item(i)
            try:
                p.lint_item(it, it['id'])
            except SystemExit as e:
                raise ValueError(f'{i["id"]}: {e}')
            f, kind, rid, body = Path(board) / 'items' / f'{it["id"]}.json', 'item', it['id'], it
            if f.exists() and json.loads(f.read_text(encoding='utf-8')).get('idea') != i['id']:
                raise ValueError(f'{i["id"]}: the board has an item {rid} already, of another idea')
        if write:
            f.parent.mkdir(parents=True, exist_ok=True)
            f.write_text(json.dumps(body, indent=2, sort_keys=True) + '\n', encoding='utf-8', newline='\n')
            i['routed'] = f'unit:{rid}' if kind == 'unit' else rid
            ideas.save(where, i)
        out.append((i['id'], kind, rid, f))
    return out


def gates(where: Path, briefs_where: Path, notes_where: Path, board: Path, evaluate=None, now=None, by='idearoute.py gates', write=True):
    """The steps of the owner's own, of the ideas on the board. One that is ready (the step before it passed) is put
    to him as a brief with the pictures of that step; one with nothing to show is owed a capture and gets no brief. A
    brief he has answered is acted on: Go on completes the step (a PASS result, as pipeline.py complete writes it), so
    what waited on it is ready; Send it back is feedback on the step before; Stop here drops the idea.
    `evaluate(item, board)` is pipeline.evaluate (the tests give their own). The last step, the landing, is not put to
    him here: that is his word to tw-master. `write` False writes nothing and says what would be asked.
    Returns dict(asked=[brief ids], done=[(brief id, what happened)], owed=[(item, stage, why)])."""
    import notes
    p = pipeline()
    b = p.Board(board)
    evaluate = evaluate or p.evaluate
    out = dict(asked=[], done=[], owed=[])
    mine = {i.get('routed'): i for i in ideas.read_all(where) if i.get('routed')}
    for iid, it in briefs.board_items(Path(board)).items():
        if iid not in mine or mine[iid].get('stopped'):
            continue
        states = evaluate(it, b)
        own = [s for s in it['stages'][:-1] if s.get('role') == 'master']
        for s in own:
            bid, info = f'gate-{iid}-{s["id"]}', states[s['id']]
            if info['state'] != 'READY' or (briefs_where / bid).exists():
                continue
            ev = []
            for a in s.get('after', []):
                r = briefs.newest_result(Path(board), iid, a)
                ev += briefs.step_evidence(Path(board), iid, a, r) if r else []
            if not ev:
                out['owed'].append((iid, s['id'], f'the step before it ({", ".join(s.get("after", []))}) passed with nothing a page can show'))
                continue
            out['asked'].append(bid)
            if not write:
                continue
            head = ' '.join(mine[iid]['title'].split()[:10])
            waits = [x['id'] for x in it['stages'] if s['id'] in x.get('after', [])]
            br = briefs.add(briefs_where, f'{head}: {s["id"].replace("-", " ")}', f'Your idea reached a step of yours. The step {", ".join(s.get("after", []))} is done; these are its pictures. Waiting on you: {", ".join(waits) or "nothing"}.',
                            GATE_OPTIONS, 'It passed its own check; the pictures are what it made.', ev[:briefs.MOST_EVIDENCE], lane=it.get('lane', ''), by=by, now=now, bid=bid)
            br['step'] = dict(item=iid, stage=s['id'], job=info['job'], gate=True)
            br['options'][0]['then'] = dict(says=GATE_THEN, stamp=briefs.stamp(GATE_THEN))
            (briefs_where / bid / 'brief.json').write_text(json.dumps(br, indent=1, sort_keys=True) + '\n', encoding='utf-8')
        if not write:
            continue
        for a in briefs.answers([x for x in briefs.read_all(briefs_where) if x['id'].startswith(f'gate-{iid}-')], notes.read_all(notes_where)):
            s = next((x for x in own if f'gate-{iid}-{x["id"]}' == a['id']), None)
            if not s:
                continue
            info, stamp_ = states[s['id']], f'{(now or datetime.datetime.now()):%Y%m%d%H%M%S}'
            if a['option'] == 'A' and info['state'] == 'READY':
                attempt = 1 + max([r['attempt'] for r in b.results(iid, s['id']) if r['job'] == info['job']] or [0])
                p.write_json(b.root / 'results' / f'{info["job"]}--{attempt}.json',
                             dict(job=info['job'], item=iid, stage=s['id'], attempt=attempt, verdict='PASS', station=s['station'], token='owner', consumed=info['consumed'], upstream_rev=info['upstream_rev'],
                                  evidence={}, note=f'The owner on the board: {a["said"] or a["text"]}', feedback=[], finished_at=p.now()))
                what = 'the step is passed; what waited on it is ready'
            elif a['option'] == 'B':
                for prev in s.get('after', []):
                    fid = f'FR-owner-{stamp_}-{slug(prev)[:8]}'
                    p.write_json(b.root / 'feedback' / f'{fid}.json', dict(id=fid, item=iid, stage=prev, words=a['said'] or 'Sent back by the owner on the board.', check='the owner looks at it again', status='open', created_at=p.now()))
                what = f'sent back: {", ".join(s.get("after", []))} is done again after your words'
            elif a['option'] == 'C':
                i = mine[iid]
                i['stopped'] = f'{(now or datetime.datetime.now()):%Y-%m-%d %H:%M}'
                ideas.save(where, i)
                what = 'the idea is dropped; its item stays on the board and no step of it is put to you again'
            else:
                continue                    # words of his own that pick no option: a session reads them (briefs.py waiting)
            briefs.take(briefs_where, notes_where, a['id'], by=by, now=now, note=a['note'], outcome=what, option=a['option'] if a['go'] == 'write' else '')
            out['done'].append((a['id'], what))
    return out


def main(argv=None):
    import notes
    import ops
    ap = argparse.ArgumentParser(description='accepted ideas on the board of the pipeline')
    ap.add_argument('what', nargs='?', default='look', choices=('look', 'route', 'gates'))
    ap.add_argument('--board', default='', help='the board of the pipeline (default: TW_BOARD, or tw3d-board beside the checkout)')
    a = ap.parse_args(argv)
    board = Path(a.board) if a.board else ops.board_root()
    try:
        if not board or not (Path(board) / 'items').is_dir():
            raise ValueError(f'no board at {board}: name it with --board')
        if a.what in ('look', 'route'):
            got = route(ideas.folder(), Path(board), write=a.what == 'route')
            print(f'idearoute: {len(got)} accepted idea{"" if len(got) == 1 else "s"} {"put" if a.what == "route" else "to put"} on the board')
            for iid, kind, rid, f in got:
                print(f'      {kind} {rid}  ({iid})  {f}')
            if got and a.what == 'route':
                print('      nothing is committed: commit the board as the master does, between two legs; a unit goes in with relay.py add --unit FILE')
        if a.what in ('look', 'gates'):
            g = gates(ideas.folder(), briefs.folder(), notes.folder(), Path(board), write=a.what == 'gates')
            print(f'idearoute: {len(g["asked"])} step{"" if len(g["asked"]) == 1 else "s"} of his {"put" if a.what == "gates" else "to put"} to him, {len(g["done"])} of his answers acted on, {len(g["owed"])} owe a capture')
            for bid in g['asked']:
                print(f'      asks {bid}')
            for bid, w in g['done']:
                print(f'      {bid}: {w}')
            for i_, s, w in g['owed']:
                print(f'      owes a capture: {i_} / {s}: {w}')
    except (ValueError, SystemExit) as e:
        print(f'idearoute: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
