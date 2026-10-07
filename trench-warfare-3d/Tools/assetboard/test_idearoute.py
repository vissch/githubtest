#!/usr/bin/env python3
"""Tests of an accepted idea's way onto the pipeline's board (idearoute.py). Run from trench-warfare-3d/:
python Tools/assetboard/test_idearoute.py

The board is a folder made here; its states are worked out by the pipeline's own evaluate, so "what waits on him and
what does not" is tested against the state machine the relay reads, not against a copy of it."""
import datetime
import json
import os
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / 'pipeline'))
import briefs      # noqa: E402
import idearoute   # noqa: E402
import ideas       # noqa: E402
import notes       # noqa: E402
import pipeline    # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:600]))


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-idearoute-test-'))
    where, bwhere, nwhere, board = tmp / 'ideas', tmp / 'briefs', tmp / 'notes', tmp / 'board'
    (board / 'items').mkdir(parents=True)
    day = datetime.datetime(2026, 10, 7, 12, 0, 0)
    pic = tmp / 'p.png'
    pic.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
    P = [dict(path=str(pic), caption='What it is', kind='capture')]

    def idea(title, kind, state='accepted'):
        i = ideas.add(where, title, 'A thing on the field that does a thing.', 'It serves a goal.', kind, P, 'M', 'R1 C1', entries=[], now=day)
        return ideas.answer(where, i['id'], state, now=day) if state != 'open' else i
    whistle, slate, tool, still_open = idea('A whistle before the assault', 'mechanic'), idea('A victory slate slams down', 'interface'), idea('A weekly digest page', 'tool'), idea('Rain fills the shell holes', 'level', 'open')

    it = idearoute.item(whistle)
    by = {s['id']: s for s in it['stages']}
    try:
        pipeline.lint_item(it, it['id'])
        lint = ''
    except SystemExit as e:
        lint = str(e)
    order = [s['id'] for s in it['stages']]
    case('item: an accepted idea is an item of the board the pipeline takes, its route as stages in order, each of his own steps a master stage no worker takes',
         not lint and order == ['game-design', 'concept', 'you-pick', 'numbers', 'ux', 'ui-art', 'you-build-it', 'sim-build', 'show-build', 'critic', 'land']
         and [s['id'] for s in it['stages'] if s['role'] == 'master'] == ['you-pick', 'you-build-it', 'land'] and it['idea'] == whistle['id'] and it['lane'].startswith('lane/show/pipe-idea-'), (lint, order))
    case('item: the steps that think do not wait for his pick, the steps that build do: "keep moving around me"',
         by['numbers']['after'] == ['concept'] and by['ux']['after'] == ['numbers'] and by['ui-art']['after'] == ['ux', 'you-pick'] and by['you-pick']['after'] == ['concept']
         and by['sim-build']['after'] == ['ui-art', 'you-build-it'] and by['show-build']['after'] == ['sim-build', 'you-build-it'] and by['land']['after'] == ['critic'], {k: v.get('after') for k, v in by.items()})
    case('item: every step that is not his own names a capture and says what it is to do with the idea; the sim build has a sim lane; the thinking is the laptop\'s',
         all(s.get('bands') == ['shown'] and whistle['title'] in s['notes'] for s in it['stages'] if s['role'] != 'master') and not any(s.get('bands') for s in it['stages'] if s['role'] == 'master')
         and by['sim-build']['lane'].startswith('lane/sim/pipe-') and by['game-design']['station'] == 'laptop' and by['concept']['station'] == 'desktop', by['sim-build'])
    roles = {r for route in ideas.ROUTES.values() for r, _, _ in route}
    case('item: every role a route names has a line that says what it does with an idea, and every kind of idea makes an item the pipeline takes',
         roles - {'master'} <= set(idearoute.DOES) and all(pipeline.lint_item(idearoute.item(dict(whistle, route=[dict(role=r, says=s, own=o) for r, s, o in ideas.ROUTES[k]])), k) is None for k in ideas.ROUTES),
         sorted(roles - {'master'} - set(idearoute.DOES)))

    dry = idearoute.route(where, board, write=False)
    nothing = not list((board / 'items').glob('*.json')) and not (where / 'units').exists()
    got = idearoute.route(where, board)
    again = idearoute.route(where, board)
    unit = json.loads((where / 'units' / 'idea-a-weekly-digest-page.json').read_text(encoding='utf-8'))
    case('route: a look writes nothing; a route writes an item for each accepted idea with specialists and marks the idea; an idea he has not said yes to stays off the board; a second route finds nothing new',
         len(dry) == 3 and nothing and sorted(k for _, k, _, _ in got) == ['item', 'item', 'unit'] and sorted(f.name for f in (board / 'items').glob('*.json')) == ['idea-a-victory-slate-slams-down.json', 'idea-a-whistle-before-the-assault.json']
         and ideas.find(where, whistle['id'])['routed'] == 'idea-a-whistle-before-the-assault' and not ideas.find(where, still_open['id']).get('routed') and again == [], (got, again))
    case('route: an idea for a tool is one unit the queue takes, not an item: its lane, its goal with the idea in it, and a check that only its own commit tag passes',
         not briefs.check_then('queued', unit) and unit['lane'] == 'lane/show/idea-a-weekly-digest-page' and 'A weekly digest page' in unit['goal'] and '[idea-a-weekly-digest-page]' in unit['done_when'][2]
         and ideas.find(where, tool['id'])['routed'] == 'unit:idea-a-weekly-digest-page', unit)
    (board / 'items' / 'idea-flares.json').write_text(json.dumps(dict(id='idea-flares', title='x', lane='lane/show/x', idea='another', stages=[])), encoding='utf-8')
    clash = idea('Flares', 'look')
    try:
        idearoute.route(where, board)
        said = ''
    except ValueError as e:
        said = str(e)
    case('route: an item of that name that is another idea\'s is not written over', 'of another idea' in said and not ideas.find(where, clash['id']).get('routed'), said)
    (board / 'items' / 'idea-flares.json').unlink()
    ideas.answer  # noqa: B018
    c = ideas.find(where, clash['id'])
    c['stopped'] = 'test'           # out of the way of the cases below
    c['routed'] = 'none'
    ideas.save(where, c)

    B = pipeline.Board(board)
    iid = 'idea-a-whistle-before-the-assault'

    def states():
        return {k: v['state'] for k, v in pipeline.evaluate(B.items()[iid], B).items()}

    def passes(stage, shown=True):
        info = pipeline.evaluate(B.items()[iid], B)[stage]
        ev = {}
        if shown:
            f = board / 'evidence' / iid / stage / 'shown.png'
            f.parent.mkdir(parents=True, exist_ok=True)
            f.write_bytes(pic.read_bytes())
            ev = dict(shown=f.relative_to(board).as_posix())
        pipeline.write_json(board / 'results' / f'{info["job"]}--1.json', dict(job=info['job'], item=iid, stage=stage, attempt=1, verdict='PASS', station='desktop', token='t', consumed=info['consumed'],
                                                                              upstream_rev=info['upstream_rev'], evidence=ev, note='made', feedback=[], finished_at=pipeline.now()))
    s0 = states()
    g0 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    passes('game-design')
    passes('concept')
    s1 = states()
    case('gates: by the pipeline\'s own states, the idea starts at game design; once the concept is made his pick is ready AND the numbers go on beside it, while the art that needs his pick waits',
         s0['game-design'] == 'READY' and s0['concept'] == 'BLOCKED' and g0 == dict(asked=[], done=[], owed=[]) and s1['you-pick'] == 'READY' and s1['numbers'] == 'READY' and s1['ux'] == 'BLOCKED' and s1['ui-art'] == 'BLOCKED', (s0, s1))
    look = idearoute.gates(where, bwhere, nwhere, board, now=day, write=False)
    g1 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    g1b = idearoute.gates(where, bwhere, nwhere, board, now=day)
    bid = f'gate-{iid}-you-pick'
    br = briefs.find(bwhere, bid) if (bwhere / bid).exists() else {}
    case('gates: his step is put to him once, as a brief with the pictures of the step before, three options, and Go on says what happens then; a look writes nothing',
         look['asked'] == [bid] and g1['asked'] == [bid] and g1b['asked'] == [] and len(br.get('evidence', [])) == 1 and br['evidence'][0]['caption'].startswith('concept') and [o['text'] for o in br['options']] == list(idearoute.GATE_OPTIONS)
         and br['options'][0]['then']['says'] == idearoute.GATE_THEN and br['step']['gate'] and 'ui-art' in br['what_for'], (look, g1, br))
    notes.write(nwhere, 'A: ' + idearoute.GATE_OPTIONS[0], kind='page', about='brief:' + bid, then=br['options'][0]['then']['stamp'], now=day)
    g2 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    s2 = states()
    passes('numbers')
    passes('ux')
    s3 = states()
    closed = briefs.find(bwhere, bid)
    case('gates: his Go on completes the step, the brief is closed with what happened, and what waited on him is ready as soon as the thinking beside it is done',
         g2['done'] == [(bid, 'the step is passed; what waited on it is ready')] and s2['you-pick'] == 'DONE' and s2['ui-art'] == 'BLOCKED' and s3['ui-art'] == 'READY' and closed['state'] == 'answered' and closed['answer']['option'] == 'A'
         and not [n for n in notes.read_all(nwhere) if n['state'] != 'done'], (g2, s2, s3))
    passes('ui-art')
    g3 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    bid2 = f'gate-{iid}-you-build-it'
    notes.write(nwhere, 'B: ' + idearoute.GATE_OPTIONS[1] + '\nthe whistle is too small on the HUD', kind='page', about='brief:' + bid2, now=day)
    g4 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    s4 = states()
    fb = [json.loads(f.read_text(encoding='utf-8')) for f in (board / 'feedback').glob('FR-*.json')]
    case('gates: Send it back is feedback on the step before with his words, which makes that step to be done again; nothing after it starts',
         g3['asked'] == [bid2] and g4['done'] and g4['done'][0][1].startswith('sent back: ui-art') and len(fb) == 1 and fb[0]['stage'] == 'ui-art' and 'too small' in fb[0]['words'] and s4['ui-art'] in ('STALE', 'READY')
         and s4['you-build-it'] == 'BLOCKED' and s4['sim-build'] == 'BLOCKED', (g4, s4, fb))
    iid2 = 'idea-a-victory-slate-slams-down'
    iid, keep = iid2, iid
    passes('ux', shown=False)
    passes('ui-art', shown=False)
    g5 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    case('gates: a step of his whose step before has nothing a page can show gets no brief and is said to owe a capture',
         g5['asked'] == [] and [(a, b) for a, b, _ in g5['owed']] == [(iid2, 'you-pick')] and not (bwhere / f'gate-{iid2}-you-pick').exists(), g5)
    f = board / 'evidence' / iid2 / 'ui-art' / 'shown.png'
    f.parent.mkdir(parents=True, exist_ok=True)
    f.write_bytes(pic.read_bytes())
    g6 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    bid3 = f'gate-{iid2}-you-pick'
    notes.write(nwhere, 'C: ' + idearoute.GATE_OPTIONS[2], kind='page', about='brief:' + bid3, now=day)
    g7 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    g8 = idearoute.gates(where, bwhere, nwhere, board, now=day)
    case('gates: Stop here drops the idea: the brief is closed, the idea says when, and no step of it is put to him again',
         g6['asked'] == [bid3] and g7['done'] and 'dropped' in g7['done'][0][1] and ideas.find(where, slate['id']).get('stopped') and g8 == dict(asked=[], done=[], owed=[]) and briefs.find(bwhere, bid3)['state'] == 'answered', (g6, g7, g8))
    iid = keep
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')
    sys.exit(main())
