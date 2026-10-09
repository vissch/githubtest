#!/usr/bin/env python3
"""Tests of the landing on the owner's click (lander.py, and the landing an option of a brief can lead to in
briefs.py). Run from trench-warfare-3d/:
python Tools/assetboard/test_lander.py
Nothing is landed and no real folder is read: the briefs, the notes and the checkout are made in a temp folder, and
land.py is a stand-in that says what the case needs."""
import datetime
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs      # noqa: E402
import lander      # noqa: E402
import notes       # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:600]))


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-lander-test-'))
    where, box, tree = tmp / 'briefs', tmp / 'notes', tmp / 'checkout' / 'trench-warfare-3d'
    (tree / 'Tools').mkdir(parents=True)
    (tree / 'Tools' / 'land.py').write_text('', encoding='utf-8')
    day = datetime.datetime(2026, 10, 9, 12, 0, 0)
    LANE, SAYS = 'lane/show/night-lamps', 'The desktop lands it within minutes'
    ran, at, says = [], ['aaa111'], [(1, 'fetching\nREFUSED: HEAD does not contain origin/integration: someone landed. git rebase, gate again, land again')]

    def run(t, land):
        ran.append((str(t), dict(land)))
        return says[0]

    def once(**kw):
        return lander.once(where, box, run=run, head=lambda t: at[0], now=day, **kw)

    def ask(title, lane=LANE):
        return briefs.add(where, title, 'Whether the lane goes into the game now.', ['Land it', 'Not yet: I say below what must change'], 'Its full test run is green.',
                          no_evidence='A landing changes nothing a picture shows', lane=lane, now=day)

    def click(q, key, sec, more=''):
        o = [x for x in briefs.find(where, q['id'])['options'] if x['key'] == key][0]
        return notes.write(box, f'{key}: {o["text"]}' + (f'\n{more}' if more else ''), kind='page', about='brief:' + q['id'], then=(o.get('then') or {}).get('stamp', ''), now=day + datetime.timedelta(seconds=sec))

    def no(f):
        try:
            f()
        except ValueError as e:
            return str(e)
        return ''

    land = dict(checkout=str(tree), lane=LANE)
    q = ask('The night with 8 real lamps: land it?')
    plain = briefs.stamp(SAYS)
    b = briefs.then(where, [], q['id'], 'A', SAYS, land=land)
    t = b['options'][0]['then']
    case('then: an option can say that his click lands a checkout; the stamp is another than the same words that land nothing, and another again for another checkout',
         t['land'] == land and t['stamp'] != plain and t['stamp'] != briefs.stamp(SAYS, None, dict(land, checkout='elsewhere')), t)
    q2 = ask('A second lane: land it?', lane='lane/show/second')
    case('then: a landing is refused that names no checkout, a branch that is no lane, a key land.py has no word for, or a unit to queue as well',
         'no checkout' in no(lambda: briefs.then(where, [], q2['id'], 'A', SAYS, land=dict(checkout='', lane=LANE)))
         and 'not a lane' in no(lambda: briefs.then(where, [], q2['id'], 'A', SAYS, land=dict(checkout=str(tree), lane='main')))
         and 'carry_sim' in no(lambda: briefs.then(where, [], q2['id'], 'A', SAYS, land=dict(checkout=str(tree), lane=LANE, force='yes')))
         and 'not both' in no(lambda: briefs.then(where, [], q2['id'], 'A', SAYS, dict(id='u', lane=LANE, goal='g', done_when=['x']), land))
         and not briefs.find(where, q2['id'])['options'][0].get('then'))

    case('once: with no click of his nothing is run', once() == [] and not ran)
    n1 = click(q, 'A', 1)
    a = [x for x in briefs.answers(briefs.read_all(where), notes.read_all(box)) if x['id'] == q['id']][0]
    look = once(write=False)
    case('answers: a click on the option with the line it showed is a yes to the landing (go land); a look runs nothing; and no session can close it as if it had landed without saying how',
         a['go'] == 'land' and a['land'] == land and look == [(q['id'], 'would', f'land {LANE} from {tree}')] and not ran
         and 'lander.py' in no(lambda: briefs.take(where, box, q['id'], by='a session', note=n1['id'])), (a, look))

    r1 = once()
    b1 = briefs.find(where, q['id'])
    r2 = once()
    case('once: a refusal of land.py is written on the brief in its own words, the brief stays his answer in progress, and the same commit is not tried a second time',
         r1[0][1] == 'refused' and len(ran) == 1 and b1.get('state') != 'answered' and b1['landing']['head'] == 'aaa111' and b1['landing']['note'] == n1['id']
         and b1['landing']['said'].startswith('fetching REFUSED: HEAD does not contain') and r2[0][1] == 'tried' and len(ran) == 1, (r1, b1.get('landing'), r2))
    shown = [x for x in briefs.site(where, tmp / 'site', now=day, got=briefs.answers(briefs.read_all(where), notes.read_all(box))) if x['id'] == q['id']][0]
    case('site: the page is told that his click waits on a landing and what the refusal said', shown['waits']['go'] == 'land' and shown['waits']['refused'].startswith('fetching REFUSED'), shown.get('waits'))

    at[0], says[0] = 'bbb222', (0, 'validate.py OK\nlanded lane/show/night-lamps on integration at bbb222')
    r3 = once()
    b3 = briefs.find(where, q['id'])
    case('once: after a rebase the new commit is tried; landed, the brief is closed with his option and where it landed, and his notes are answered',
         r3 == [(q['id'], 'landed', 'landed lane/show/night-lamps on integration at bbb222')] and len(ran) == 2 and ran[1] == (str(tree), land) and b3['state'] == 'answered' and b3['answer']['option'] == 'A'
         and b3['answer']['outcome'].startswith('Landed on your click. landed lane/show/night-lamps') and not [n for n in notes.read_all(box) if n['state'] != 'done'] and once() == [], (r3, b3.get('answer')))

    q3 = ask('A third lane: land it?', lane='lane/show/third')
    briefs.then(where, [], q3['id'], 'A', SAYS, land=dict(land, lane='lane/show/third'))
    click(q3, 'A', 2, more='but only after the weekend')
    q4 = ask('A fourth lane: land it?', lane='lane/show/fourth')
    briefs.then(where, [], q4['id'], 'A', SAYS, land=dict(land, lane='lane/show/fourth'))
    click(q4, 'B', 3)
    q5 = ask('A fifth lane: land it?', lane='lane/show/fifth')
    briefs.then(where, [], q5['id'], 'A', 'The desktop lands it')
    click(q5, 'A', 4)
    briefs.then(where, [], q2['id'], 'A', SAYS, land=dict(land, lane='lane/show/second'))
    click(q2, 'A', 5)
    b2 = briefs.find(where, q2['id'])
    b2['options'][0]['then'] = dict(b2['options'][0]['then'], land=dict(land, lane='lane/show/swapped'))        # changed after his click, the stamp left as it was
    import json
    (where / q2['id'] / 'brief.json').write_text(json.dumps(b2, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    b2 = briefs.then(where, [], ask('A sixth lane: land it?', lane='lane/show/sixth')['id'], 'A', SAYS, land=dict(checkout=str(tmp / 'not-here'), lane='lane/show/sixth'))
    click(b2, 'A', 6)
    r4 = once()
    why = {x['id']: (x['go'], x['why']) for x in briefs.answers(briefs.read_all(where), notes.read_all(box))}
    case('once: words of his own beside the click, a click on another option, an option whose line names no landing, and a landing swapped under the line he clicked land nothing; a checkout this station does not hold is passed over',
         len(ran) == 2 and [(i, w) for i, w, _ in r4] == [(b2['id'], 'elsewhere')] and why[q3['id']][0] == 'write' and why[q4['id']][0] == 'write' and why[q5['id']][0] == 'nothing'
         and why[q2['id']] == ('write', 'the Then line was changed and not stamped again'), (r4, ran[2:], why))

    repo = tmp / 'repo'
    repo.mkdir()
    subprocess.run(['git', 'init', '-q', '-b', 'lane/show/other', str(repo)], capture_output=True)
    code, said = lander.run_land(repo, land)
    case('run_land: a checkout that is on another branch than the lane he said yes to is refused before land.py is run', code == 1 and said.startswith('REFUSED: the checkout is on') and LANE in said, (code, said))
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
