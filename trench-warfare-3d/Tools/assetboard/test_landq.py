#!/usr/bin/env python3
"""Tests of the landing queue (landq.py) and of the click that lands (briefs.py, notes.py). Run from
trench-warfare-3d/: python Tools/assetboard/test_landq.py

git is real here: an origin, the worker's checkout and a second clone that plays the sessions are made in a temp
folder, and lanes are really rebased and pushed. The gate and Tools/land.py are stand-ins (a full gate takes 25
minutes and a Unity): the stand-in for land.py pushes as land.py does, or does what a case needs it to do wrong.
Each case from "a note that was merely written" on is a fault of the first try (lander.py, 2026-10-10)."""
import datetime
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs   # noqa: E402
import landq    # noqa: E402
import notes    # noqa: E402

results = []
INT = landq.INTEGRATION
ENV = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t')


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


def git(cwd, *a):
    r = subprocess.run(['git', '-c', 'core.autocrlf=false', *a], cwd=str(cwd), capture_output=True, env=ENV, stdin=subprocess.DEVNULL)
    return (r.stdout + r.stderr).decode('utf-8', 'replace').strip()


def commit(tree, path, text, msg):
    f = Path(tree) / path
    f.parent.mkdir(parents=True, exist_ok=True)
    f.write_text(text, encoding='utf-8', newline='\n')
    git(tree, 'add', '-A')
    git(tree, 'commit', '-q', '-m', msg)
    return git(tree, 'rev-parse', 'HEAD')


class W(landq.World):
    """The real git of the queue, with a gate and a land.py that a case sets."""

    def __init__(self, tree, home, other):
        super().__init__(tree, home)
        self.other, self.green, self.land_how, self.click_of, self.gates, self.lands, self.cards, self.closed, self.during_gate = other, True, 'push', {}, 0, 0, [], [], None

    def git(self, *a, cwd=None):
        r = subprocess.run(['git', '-c', 'core.autocrlf=false', *a], cwd=str(cwd or self.tree), capture_output=True, env=ENV, stdin=subprocess.DEVNULL)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def gate(self):
        self.gates += 1
        if self.during_gate:
            self.during_gate()
            self.during_gate = None
        return self.green, 'Gate green.' if self.green else 'FAILED: FallenFlightTests.A_frog_lands (expected 3 was 2)'

    def land(self):
        self.lands += 1
        lane = self.git('rev-parse', '--abbrev-ref', 'HEAD')[1]
        if self.land_how == 'nothing':
            return 0, f'nothing to land: {lane} is already in origin/{INT}'
        if self.land_how == 'refuse':
            return 1, 'REFUSED: the lane changes the tools and Tools/toolcheck.py is not green'
        code, out = self.git('push', '-q', '--atomic', 'origin', f'HEAD:refs/heads/{INT}', f'+HEAD:refs/heads/{lane}')
        return code, f'landed {lane} on {INT}' if code == 0 else out

    def clicks(self):
        return dict(self.click_of)

    def card(self, e, tip, why, paths):
        self.cards.append((e['lane'], tip, why))
        return 'land-' + landq.slug(e['lane'])

    def close(self, click, words):
        self.closed.append((click['brief'], words))
        return ''


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-landq-test-'))
    origin, dev, tree, home = tmp / 'origin.git', tmp / 'dev', tmp / 'worker', tmp / 'home'
    git(tmp, 'init', '-q', '--bare', '-b', INT, str(origin))
    git(tmp, 'clone', '-q', str(origin), str(dev))
    git(dev, 'checkout', '-q', '-b', INT)
    commit(dev, 'docs/reference/tasks.md', 'tasks\n', 'start')
    commit(dev, landq.A + 'Presentation/Flag.cs', 'class Flag {}\n', 'a flag')
    git(dev, 'push', '-q', 'origin', INT)
    git(tmp, 'clone', '-q', str(origin), str(tree))

    def lane(name, path, text, base=INT):
        """A lane with one commit, pushed; the dev clone goes back to integration."""
        git(dev, 'fetch', '-q', 'origin')
        git(dev, 'checkout', '-q', '-B', name, 'origin/' + base)
        sha = commit(dev, path, text, name)
        git(dev, 'push', '-q', '-f', 'origin', name)
        return sha

    def tip(branch=INT):
        git(dev, 'fetch', '-q', '--prune', 'origin')
        return git(dev, 'rev-parse', 'origin/' + branch)

    def land_elsewhere(path='docs/other.md'):
        """Somebody lands past the queue: integration moves on origin."""
        git(dev, 'fetch', '-q', 'origin')
        git(dev, 'checkout', '-q', '-B', 'x', 'origin/' + INT)
        commit(dev, path, 'x\n', 'another landing')
        git(dev, 'push', '-q', 'origin', 'x:' + INT)

    # ---- which lanes need no click ----
    A, T = landq.A, landq.P + 'Tools/'
    alone = [['docs/reference/tasks.md'], [T + 'sweep.py', '.claude/skills/tw-critic/SKILL.md'], [A + 'Tests/Show/FallenFlightTests.cs'],
             [A + 'Sim/Core/Match.cs', A + 'Sim/Core/Match.cs.meta', A + 'Tests/Sim/MatchTests.cs'], ['README.md', A + 'Net/Lockstep.cs']]
    his = [[A + 'Presentation/Units/VATRenderer.cs'], [A + 'Shaders/Unit.shader'], [A + 'Tests/Sim/SimHashTests.cs'], [A + 'Data/Units/Frog.asset'],
           [A + 'UI/Hud.uxml'], [landq.P + 'ProjectSettings/QualitySettings.asset'], ['some/new/place.txt'], [T + 'land.py'], [landq.P + 'gate.ps1'],
           [T + 'assetboard/landq.py'], [T + 'assetboard/notes.py'], [T + 'relay/runner.py'], ['docs/x.md', A + 'Art/Frog.fbx']]
    case('alone: docs, tools and skills, tests, and the C# of the sim, the net and the data land with no click',
         all(landq.klass(p)[0] for p in alone), [p for p in alone if not landq.klass(p)[0]])
    case('his: what draws or is drawn, a data file, the pinned sim hashes, a path nobody listed, and the gate, the landing tools, the relay and the click itself',
         not any(landq.klass(p)[0] for p in his) and 'pinned sim hashes' in landq.klass(his[2])[1] and 'the click itself' in landq.klass(his[7])[1], [p for p in his if landq.klass(p)[0]])
    case('the full gate is owed for anything of the game, not for tools or docs',
         landq.code_changed([A + 'Sim/Core/Match.cs']) and landq.code_changed([A + 'Tests/Show/X.cs']) and not landq.code_changed([T + 'sweep.py', 'docs/a.md', landq.P + 'notes.md']))

    # ---- the queue ----
    e, new = landq.add(home, 'lane/show/docs-a', 'A docs lane.', by='a session')
    again = landq.add(home, 'lane/show/docs-a')
    try:
        landq.add(home, 'main')
        bad = ''
    except ValueError as x:
        bad = str(x)
    case('a lane is in the queue once, and only a lane can be', new and not again[1] and len(landq.queue(home)) == 1 and 'not a lane' in bad, (landq.queue(home), bad))

    # ---- a docs lane lands alone, on integration's tip, though it was cut before integration moved ----
    lane('lane/show/docs-a', 'docs/a.md', 'a\n')
    land_elsewhere()
    moved = tip()
    w = W(tree, home, dev)
    did = landq.work(w, home, say=lambda s: None)
    landed = [json.loads(l) for l in (home / 'landed.jsonl').read_text(encoding='utf-8').splitlines()]
    case('a docs lane cut before integration moved is rebased on its tip and landed with no click and no gate; the landing is written down as landed alone',
         did[0][1] == 'landed' and tip() == did[0][2] and git(dev, 'merge-base', '--is-ancestor', moved, tip()) == '' and w.gates == 0 and w.lands == 1
         and landed[-1]['on'] == dict(alone=True) and landed[-1]['head'] == tip() and landq.queue(home) == [], (did, landed))

    # ---- a lane that changes the look waits for his click: one card, with the commit ----
    look = lane('lane/show/flag-red', A + 'Presentation/Flag.cs', 'class Flag { /* red */ }\n')
    landq.add(home, 'lane/show/flag-red', 'The flag is red.')
    before = tip()
    did = landq.work(w, home, say=lambda s: None)
    q = landq.queue(home)
    case('a lane that changes what a battle looks like is not landed: it is asked once, on a card that names the lane and its commit, and it stays in the queue',
         did[0][1] == 'asked' and tip() == before and w.cards == [('lane/show/flag-red', look, did[0][2])] and q[0]['state'] == 'asked' and q[0]['card'] == 'land-lane-show-flag-red' and w.lands == 1, (did, w.cards))
    landq.work(w, home, say=lambda s: None)
    case('looked at again with no click, it is still one card and nothing lands', len(w.cards) == 2 and w.cards[0] == w.cards[1] and tip() == before and w.lands == 1, w.cards)

    # ---- his click, for another commit than the lane's: not landed ----
    w.click_of = {'lane/show/flag-red': dict(brief='land-lane-show-flag-red', note='n1', tip='0' * 40)}
    did = landq.work(w, home, say=lambda s: None)
    case('his yes is to a commit: a lane that moved on after his click is not landed on it', did[0][1] == 'refused' and 'moved on after your click' in did[0][2] and tip() == before and w.lands == 1, did)

    # ---- his click for this commit, a red gate ----
    w.click_of = {'lane/show/flag-red': dict(brief='land-lane-show-flag-red', note='n1', tip=look)}
    w.green = False
    did = landq.work(w, home, say=lambda s: None)
    case('a red gate lands nothing: the lane stays in the queue with the failing test and its count of red gates',
         did[0][1] == 'red' and 'FallenFlightTests' in did[0][2] and tip() == before and landq.queue(home)[0]['reds'] == 1 and w.lands == 1, did)

    # ---- green, and integration moves while the gate runs: tested again, landed on the new tip ----
    w.green, gates = True, w.gates
    w.during_gate = lambda: land_elsewhere('docs/meanwhile.md')
    did = landq.work(w, home, say=lambda s: None)
    landed = [json.loads(l) for l in (home / 'landed.jsonl').read_text(encoding='utf-8').splitlines()]
    case('integration moved while the lane was tested: it is rebased and tested again by itself, and lands on the new tip',
         did[0][1] == 'landed' and w.gates == gates + 2 and tip() == did[0][2] and 'meanwhile.md' in git(dev, 'ls-tree', '-r', '--name-only', tip()), (did, w.gates - gates))
    case('a landing on his click is written down with the card and the note it came from, the card is closed with where it landed, and the lane leaves the queue',
         landed[-1]['on'] == dict(click='land-lane-show-flag-red', note='n1') and w.closed == [('land-lane-show-flag-red', f'Landed on your click, as integration {tip()[:8]}')] and landq.queue(home) == [], (landed[-1], w.closed))
    w.click_of = {}

    # ---- land.py says "nothing to land" and exits 0: not a landing ----
    lane('lane/show/docs-b', 'docs/b.md', 'b\n')
    landq.add(home, 'lane/show/docs-b')
    w.land_how, before = 'nothing', tip()
    did = landq.work(w, home, say=lambda s: None)
    case('land.py ending 0 without moving integration is not a landing: integration is read back and must be the lane\'s head',
         did[0][1] == 'refused' and tip() == before and landq.queue(home)[0]['state'] == 'refused' and len([l for l in (home / 'landed.jsonl').read_text(encoding='utf-8').splitlines()]) == 2, did)
    w.land_how = 'refuse'
    did = landq.work(w, home, say=lambda s: None)
    case('what land.py refuses stays refused, with its words', did[0][1] == 'refused' and 'toolcheck.py is not green' in did[0][2] and tip() == before, did)
    w.land_how = 'push'

    # ---- two lanes: one after the other, the second on the first's landing ----
    lane('lane/show/docs-c', 'docs/c.md', 'c\n')
    landq.add(home, 'lane/show/docs-c')
    did = landq.work(w, home, say=lambda s: None)
    files = git(dev, 'ls-tree', '-r', '--name-only', tip())
    case('two lanes land one after the other in the queue\'s order, the second rebased on the first\'s landing: nothing races',
         [d[:2] for d in did] == [('lane/show/docs-b', 'landed'), ('lane/show/docs-c', 'landed')] and 'docs/b.md' in files and 'docs/c.md' in files and tip() == did[1][2]
         and git(dev, 'merge-base', '--is-ancestor', did[0][2], did[1][2]) == '', did)

    # ---- a lane that does not rebase by itself ----
    lane('lane/show/clash', 'docs/a.md', 'mine\n', base=INT + '~5')        # cut before docs/a.md landed: both add the file
    landq.add(home, 'lane/show/clash')
    before = tip()
    did = landq.work(w, home, say=lambda s: None)
    case('a lane that does not go on integration\'s tip by itself is refused with the file that clashes, and the worker\'s checkout is left clean for the next lane',
         did[0][1] == 'refused' and 'docs/a.md' in did[0][2] and tip() == before and w.git('status', '--porcelain')[1] == '' and 'rebase' not in w.git('status')[1].lower(), did)
    landq.drop(home, 'lane/show/clash')

    # ---- already in, gone, held elsewhere ----
    landq.add(home, 'lane/show/docs-c')
    landq.add(home, 'lane/show/never-pushed')
    did = landq.work(w, home, say=lambda s: None)
    case('a lane that is in integration already, and one that is no longer on origin, leave the queue without a landing',
         [d[:2] for d in did] == [('lane/show/docs-c', 'in'), ('lane/show/never-pushed', 'gone')] and landq.queue(home) == [], did)
    lane('lane/show/held', 'docs/held.md', 'h\n')
    git(tree, 'fetch', '-q', 'origin')
    git(tree, 'worktree', 'add', '-q', '-b', 'lane/show/held', str(tmp / 'elsewhere'), 'origin/lane/show/held')
    landq.add(home, 'lane/show/held')
    did = landq.work(w, home, say=lambda s: None)
    case('a lane another checkout stands on waits, and says which checkout', did[0][1] == 'waiting' and 'elsewhere' in did[0][2] and landq.queue(home)[0]['state'] == 'waiting', did)
    landq.drop(home, 'lane/show/held')

    # ---- no flood of cards ----
    for i in range(landq.ASK_MOST + 2):
        lane(f'lane/show/look-{i}', A + f'Presentation/L{i}.cs', 'class L {}\n')
        landq.add(home, f'lane/show/look-{i}')
    w.cards = []
    did = landq.work(w, home, say=lambda s: None)
    case(f'at most {landq.ASK_MOST} landing cards are open at a time: the lanes behind them wait, and say what for',
         [d[1] for d in did] == ['asked'] * landq.ASK_MOST + ['waiting'] * 2 and len(w.cards) == landq.ASK_MOST and 'its card comes when' in did[-1][2], [d[:2] for d in did])
    for e in landq.queue(home):
        landq.drop(home, e['lane'])

    # ---- the lanes the relay finished ----
    board = tmp / 'board'
    (board / 'relay' / 'done').mkdir(parents=True)
    for uid, ln in (('rv-10', 'lane/sim/review-fixes-2'), ('rv-11', 'lane/sim/review-fixes-2'), ('look-01', 'lane/show/look'), ('odd', 'main')):
        (board / 'relay' / 'done' / (uid + '.json')).write_text(json.dumps(dict(id=uid, lane=ln, head='0' * 40)), encoding='utf-8')
    new = landq.offer(home, board)
    case('offer: every lane a relay unit finished is put in the queue once, and what is not a lane is not',
         sorted(new) == ['lane/show/look', 'lane/sim/review-fixes-2'] and landq.offer(home, board) == [] and 'rv-1' in landq.queue(home)[0]['why'] + landq.queue(home)[1]['why'], new)

    # ---- the click that lands: what briefs.answers calls `land` ----
    bw, nw, key = tmp / 'briefs', tmp / 'notes', 'the-listeners-key'
    Path(os.environ['TW_CLICK_KEY']).write_text(key + '\n', encoding='utf-8')         # this machine's listener signs with it
    now = datetime.datetime(2026, 10, 11, 9, 0, 0)
    b = briefs.add(bw, 'Land the red flag?', 'The flag idea is finished and tested.', ['Land it', 'Not now'], 'It is ready.', no_evidence='shown on its own cards', lane='lane/show/flag-red', now=now)
    briefs.then(bw, [], b['id'], 'A', 'The queue lands it', land=dict(lane='lane/show/flag-red', tip=look))
    st = briefs.find(bw, b['id'])['options'][0]['then']['stamp']

    def ans(when=now):
        return [a for a in briefs.answers(briefs.read_all(bw), notes.read_all(nw), now=when, keys=[key]) if a['id'] == b['id']][0]
    n1 = notes.write(nw, 'A: Land it', kind='page', about='brief:' + b['id'], then=st, now=now)
    written = ans()
    notes.answer(nw, n1['id'], 'not his click', by='the test')
    n2 = notes.write(nw, 'A: Land it', kind='page', about='brief:' + b['id'], then=st, now=now, sign=key)
    signed = ans()
    case('a note that was merely written, with `from: owner`, `kind: page` and the right stamp, lands nothing: it reads as an answer a session must look at. The same click through the listener is `land`, with the lane and the commit',
         written['go'] == 'write' and 'not signed' in written['why'] and signed['go'] == 'land' and signed['land'] == dict(lane='lane/show/flag-red', tip=look), (written, signed))
    case('a click on a landing lapses after 48 hours', ans(now + datetime.timedelta(hours=47))['go'] == 'land' and ans(now + datetime.timedelta(hours=49))['go'] == 'write' and 'older than 48 hours' in ans(now + datetime.timedelta(hours=49))['why'])
    case('a click signed with another key is not his here', [a for a in briefs.answers(briefs.read_all(bw), notes.read_all(nw), now=now, keys=['some other key']) if a['id'] == b['id']][0]['go'] == 'write')
    raw = json.loads((bw / b['id'] / 'brief.json').read_text(encoding='utf-8'))
    raw['options'][0]['then']['land']['tip'] = 'f' * 40
    (bw / b['id'] / 'brief.json').write_text(json.dumps(raw), encoding='utf-8')
    swapped = ans()
    raw['options'][0]['then']['land']['tip'] = look
    (bw / b['id'] / 'brief.json').write_text(json.dumps(raw), encoding='utf-8')
    case('another commit put under the line he clicked is no yes of his', swapped['go'] == 'write' and 'not stamped again' in swapped['why'] and ans()['go'] == 'land', swapped)
    try:
        briefs.take(bw, nw, b['id'], by='a session', note=n2['id'])
        took = ''
    except ValueError as x:
        took = str(x)
    case('a session cannot close a landing click as if it had been carried out: the queue closes it when it has landed', 'the landing queue does and closes' in took, took)
    b2 = briefs.add(bw, 'Land the red flag again?', 'Asked a second time.', ['Land it', 'Not now'], 'It is ready.', no_evidence='nothing new', lane='lane/show/flag-red', now=now)
    refused = []
    for land in (dict(lane='lane/show/flag-red', tip=look), dict(lane='lane/show/other', tip='abc'), dict(lane='main', tip=look), dict(checkout='C:/x', lane='lane/show/other')):
        try:
            briefs.then(bw, [], b2['id'], 'A', 'The queue lands it', land=land)
        except ValueError as x:
            refused.append(str(x))
    case('a second card for a lane that has one is refused (a landing is asked once), and so is a landing that names no whole commit, no lane, or a folder',
         len(refused) == 4 and 'asked once' in refused[0] and '40 characters' in refused[1] and 'not a lane' in refused[2], refused)

    # ---- the real card ----
    os.environ.update(TW_BRIEFS=str(tmp / 'cards'), TW_NOTES=str(tmp / 'cardnotes'))
    real = landq.World(tree, home)
    e = dict(lane='lane/show/flag-red', why='The relay finished the flag idea on it.')
    bid = real.card(e, look, 'it changes what a battle looks like or how it plays (Flag.cs)', ['a', 'b'])
    card = briefs.find(tmp / 'cards', bid)
    bid2 = real.card(e, 'e' * 40, 'it changes what a battle looks like', ['a'])
    card2 = briefs.find(tmp / 'cards', bid)
    case('the queue\'s own card passes the rules of a brief, is named after the lane, and its first option lands the lane at its commit; asked again after the lane moved it is the same card with the new commit',
         bid == bid2 == 'land-lane-show-flag-red' and card['options'][0]['then']['land'] == dict(lane='lane/show/flag-red', tip=look) and card2['options'][0]['then']['land']['tip'] == 'e' * 40
         and len(briefs.read_all(tmp / 'cards')) == 1 and card['lane'] == 'lane/show/flag-red', card)
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')
    os.environ['TW_BRIEFS'] = tempfile.mkdtemp(prefix='tw-briefs-test-')
    os.environ['TW_CLICK_KEY'] = str(Path(tempfile.mkdtemp(prefix='tw-click-test-')) / 'click.key')
    sys.exit(main())
