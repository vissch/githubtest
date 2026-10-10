#!/usr/bin/env python3
"""Tests of the landing queue (landq.py) and of the click that lands (briefs.py, notes.py). Run from
trench-warfare-3d/: python Tools/assetboard/test_landq.py

git is real here: an origin, the worker's checkout (a clone of its own) and a second clone that plays the sessions
are made in a temp folder, and lanes are really rebased and pushed. The gate and Tools/land.py are stand-ins (a full
gate takes 25 minutes and a Unity): the stand-in for land.py pushes as land.py does, or does what a case needs it to
do wrong. The cases are the faults of the first try (lander.py, 2026-10-10) and the findings of the review of this
queue (LQ1 to LQ13, 2026-10-10)."""
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
    """The real git of the queue, with a gate, a land.py and clicks that a case sets."""

    def __init__(self, tree, home):
        super().__init__(tree, home)
        self.green, self.land_how, self.click_of, self.gates, self.lands, self.cards, self.closed, self.during_gate, self.card_falls = True, 'push', {}, 0, 0, [], [], None, False

    def git(self, *a, cwd=None):
        r = subprocess.run(['git', '-c', 'core.autocrlf=false', *a], cwd=str(cwd or self.tree), capture_output=True, env=ENV, stdin=subprocess.DEVNULL)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def owes_gate(self, paths):
        return landq.code_changed(paths)

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
        return {k: dict(v) for k, v in self.click_of.items()}

    def card(self, e, tip, why, paths):
        if self.card_falls:
            raise ValueError('not a brief yet: what it is for takes 60 words; 45 at most')
        self.cards.append((e['lane'], tip))
        return landq.card_id(e['lane'], tip)

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
    quiet = dict(say=lambda s: None)

    def lane(name, path, text, base=INT, more=None):
        """A lane with one commit (or one more on top of it), pushed; returns its tip."""
        git(dev, 'fetch', '-q', 'origin')
        git(dev, 'checkout', '-q', '-B', name, 'origin/' + (more or base))
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
        commit(dev, path, path + '\n', 'another landing: ' + path)
        git(dev, 'push', '-q', 'origin', 'x:' + INT)

    def log():
        return [json.loads(l) for l in (home / 'landed.jsonl').read_text(encoding='utf-8').splitlines()] if (home / 'landed.jsonl').exists() else []

    # ---- which lanes need no click ----
    A, T = landq.A, landq.P + 'Tools/'
    alone = [['docs/reference/tasks.md'], [T + 'sweep.py', '.claude/skills/tw-critic/SKILL.md', '.claude/agents/tw-scout.md'], [A + 'Tests/Show/FallenFlightTests.cs', A + 'Tests/Show/FallenFlightTests.cs.meta'],
             ['README.md', T + 'assetboard/src_runs.py']]
    his = [[A + 'Presentation/Units/VATRenderer.cs'], [A + 'Shaders/Unit.shader'], [A + 'Tests/Sim/SimHashTests.cs'], [A + 'Data/Units/Frog.asset'],
           [A + 'UI/Hud.uxml'], [landq.P + 'ProjectSettings/QualitySettings.asset'], ['some/new/place.txt'], [T + 'land.py'], ['gate.ps1'],
           [T + 'assetboard/landq.py'], [T + 'assetboard/notes.py'], [T + 'relay/runner.py'], ['docs/x.md', A + 'Art/Frog.fbx'],
           [A + 'Sim/Match/UnitDefinitions.cs'], [A + 'Net/Lockstep.cs'], [A + 'Data/Units.cs'], [A + 'Tests/Show/Resources/Frog.prefab'], [A + 'Tests/Runtime.asmdef'],
           ['.claude/settings.json'], ['.claude/hooks/guard.py'], [T + 'checks/asmdef_refs.py'], [T + 'selftest.py'], [T + 'assetboard/static/decide.js'],
           [T + 'assetboard/build.py'], [T + 'assetboard/test_landq.py'], [T + 'gate_bg.py']]
    case('alone: docs, tools, skills and agents, and test code land with no click',
         all(landq.klass(p)[0] for p in alone), [p for p in alone if not landq.klass(p)[0]])
    case('his: what draws or is drawn; a data file; the code of the sim, the net and the data (no test pins how a battle plays out); anything under Tests that is not test code; '
         'settings and hooks; a path nobody listed; and the gate with what it runs, the landing tools, the relay and the click itself',
         not any(landq.klass(p)[0] for p in his) and 'no test pins' in landq.klass(his[13])[1] and 'the click itself' in landq.klass(his[7])[1], [p for p in his if landq.klass(p)[0]])
    case('the full gate is owed for anything of the game and for the gate script, not for tools or docs',
         landq.code_changed([A + 'Sim/Core/Match.cs']) and landq.code_changed([A + 'Tests/Show/X.cs']) and landq.code_changed(['gate.ps1']) and not landq.code_changed([T + 'sweep.py', 'docs/a.md', landq.P + 'notes.md']))

    # ---- the queue ----
    e, new = landq.add(home, 'lane/show/docs-a', 'A docs lane. ' + 'word ' * 60, by='a session')
    again = landq.add(home, 'lane/show/docs-a')
    try:
        landq.add(home, 'main')
        bad = ''
    except ValueError as x:
        bad = str(x)
    case('a lane is in the queue once, only a lane can be, and what it is said to be is kept short', new and not again[1] and len(landq.queue(home)) == 1 and 'not a lane' in bad and len(e['why'].split()) == 24, (landq.queue(home), bad))

    # ---- a docs lane lands alone, on integration's tip, though it was cut before integration moved ----
    lane('lane/show/docs-a', 'docs/a.md', 'a\n')
    land_elsewhere()
    moved = tip()
    w = W(tree, home)
    did = landq.work(w, home, **quiet)
    case('a docs lane cut before integration moved is rebased on its tip and landed with no click and no gate; the landing is written down as landed alone',
         did[0][1] == 'landed' and tip() == did[0][2] and git(dev, 'merge-base', '--is-ancestor', moved, tip()) == '' and w.gates == 0 and w.lands == 1
         and log()[-1]['on'] == dict(alone=True) and log()[-1]['head'] == tip() and landq.queue(home) == [], (did, log()))
    case('between two lanes the worker stands on no branch and keeps no branch of its own', w.git('rev-parse', '--abbrev-ref', 'HEAD')[1] == 'HEAD' and w.git('branch', '--list', 'lane/*')[1] == '', w.git('branch')[1])

    # ---- a lane that changes the look waits for his click: one card, with the commit ----
    look = lane('lane/show/flag-red', A + 'Presentation/Flag.cs', 'class Flag { /* red */ }\n')
    landq.add(home, 'lane/show/flag-red', 'The flag is red.')
    before = tip()
    did = landq.work(w, home, **quiet)
    q = landq.queue(home)
    case('a lane that changes what a battle looks like is not landed: it is asked, on a card whose id names the lane and the commit, and it stays in the queue',
         did[0][1] == 'asked' and tip() == before and w.cards == [('lane/show/flag-red', look)] and q[0]['state'] == 'asked' and q[0]['card'] == f'land-lane-show-flag-red-{look[:8]}' and w.lands == 1, (did, w.cards))
    landq.work(w, home, **quiet)
    case('looked at again with no click, it is the same card and nothing lands', w.cards == [('lane/show/flag-red', look)] * 2 and tip() == before and w.lands == 1, w.cards)

    # ---- his click, for another commit than the lane's: no yes to this one ----
    w.click_of = {'lane/show/flag-red': dict(brief='land-lane-show-flag-red-00000000', note='n0', tip='0' * 40)}
    did = landq.work(w, home, **quiet)
    case('his yes is to a commit: a click for another commit of the lane lands nothing, and the lane is asked again for the commit it now has',
         did[0][1] == 'asked' and tip() == before and w.lands == 1 and w.cards[-1] == ('lane/show/flag-red', look), did)

    # ---- his click for this commit, a red gate ----
    click = dict(brief=landq.card_id('lane/show/flag-red', look), note='n1', tip=look)
    w.click_of = {'lane/show/flag-red': click}
    w.green = False
    did = landq.work(w, home, **quiet)
    case('a red gate lands nothing: the lane stays in the queue with the failing test and its count of red gates',
         did[0][1] == 'red' and 'FallenFlightTests' in did[0][2] and tip() == before and landq.queue(home)[0]['reds'] == 1 and w.lands == 1, did)
    w.green, gates = True, w.gates
    did = landq.work(w, home, **quiet)
    case('a lane that was red is not tested again while neither the lane nor integration has moved: no half hour of Unity for the same answer',
         did[0][1] == 'same' and w.gates == gates and landq.queue(home)[0]['state'] == 'red' and landq.queue(home)[0]['reds'] == 1, did)
    old = dict(landq.queue(home)[0], seen_at=0)
    landq.keep(home, old)
    w.green = False
    did = landq.work(w, home, **quiet)
    case('after six hours it is tried once more though nothing moved (a test can be red for the hour of the day)', did[0][1] == 'red' and w.gates == gates + 1 and landq.queue(home)[0]['reds'] == 2, did)

    # ---- green, and integration moves before and while the gate runs: tested again, landed on the new tip ----
    w.green, gates = True, w.gates
    land_elsewhere('docs/before.md')
    w.during_gate = lambda: land_elsewhere('docs/meanwhile.md')
    did = landq.work(w, home, **quiet)
    case('integration moved while the lane was tested: it is rebased and tested again by itself, and lands on the new tip',
         did[0][1] == 'landed' and w.gates == gates + 2 and tip() == did[0][2] and 'meanwhile.md' in git(dev, 'ls-tree', '-r', '--name-only', tip()), (did, w.gates - gates))
    case('a landing on his click is written down with the card and the note it came from, the card is closed with where it landed, and the lane leaves the queue',
         log()[-1]['on'] == dict(click=click['brief'], note='n1') and w.closed == [(click['brief'], f'Landed on your click, as integration {tip()[:8]}')] and landq.queue(home) == [], (log()[-1], w.closed))

    # ---- his click changes while the lane is tested ----
    blue = lane('lane/show/flag-blue', A + 'Presentation/Flag.cs', 'class Flag { /* blue */ }\n')
    landq.add(home, 'lane/show/flag-blue')
    w.click_of = {'lane/show/flag-red': click, 'lane/show/flag-blue': dict(brief=landq.card_id('lane/show/flag-blue', blue), note='n2', tip=blue)}
    w.during_gate = lambda: w.click_of.pop('lane/show/flag-blue')
    before = tip()
    did = landq.work(w, home, **quiet)
    case('he writes on the card, or his click is taken back, while the lane is tested: nothing is landed', did[0][1] == 'refused' and 'changed while it was tested' in did[0][2] and tip() == before, did)
    landq.drop(home, 'lane/show/flag-blue')
    w.click_of = {}

    # ---- land.py says "nothing to land" and exits 0: not a landing ----
    lane('lane/show/docs-b', 'docs/b.md', 'b\n')
    landq.add(home, 'lane/show/docs-b')
    w.land_how, before = 'nothing', tip()
    did = landq.work(w, home, **quiet)
    case('land.py ending 0 without moving integration is not a landing: integration is read back and must be the lane\'s head',
         did[0][1] == 'refused' and tip() == before and landq.queue(home)[0]['state'] == 'refused' and len(log()) == 2, did)
    w.land_how = 'refuse'
    land_elsewhere('docs/again.md')
    did = landq.work(w, home, **quiet)
    case('what land.py refuses stays refused, with its words', did[0][1] == 'refused' and 'toolcheck.py is not green' in did[0][2], did)
    w.land_how = 'push'
    land_elsewhere('docs/again2.md')

    # ---- two lanes: one after the other, the second on the first's landing ----
    lane('lane/show/docs-c', 'docs/c.md', 'c\n')
    landq.add(home, 'lane/show/docs-c')
    did = landq.work(w, home, **quiet)
    files = git(dev, 'ls-tree', '-r', '--name-only', tip())
    case('two lanes land one after the other in the queue\'s order, the second rebased on the first\'s landing: nothing races',
         [d[:2] for d in did] == [('lane/show/docs-b', 'landed'), ('lane/show/docs-c', 'landed')] and 'docs/b.md' in files and 'docs/c.md' in files and tip() == did[1][2]
         and git(dev, 'merge-base', '--is-ancestor', did[0][2], did[1][2]) == '', did)

    # ---- a lane whose every commit is in integration under other commit ids ----
    git(dev, 'fetch', '-q', 'origin')
    first = git(dev, 'rev-list', '--max-parents=0', 'origin/' + INT)
    git(dev, 'checkout', '-q', '-B', 'lane/show/twin', first)
    commit(dev, 'docs/c.md', 'c\n', 'the same change as docs-c, cut from the first commit')
    git(dev, 'push', '-q', '-f', 'origin', 'lane/show/twin')
    landq.add(home, 'lane/show/twin')
    before, lands = tip(), w.lands
    did = landq.work(w, home, **quiet)
    case('a lane that rebases to nothing (it landed in a stack, under other commits) is in, not landed: land.py is not run and no landing is written down',
         did[0][1] == 'in' and tip() == before and w.lands == lands and landq.queue(home) == [] and len(log()) == 4, did)

    # ---- a lane that does not rebase by itself, and a lane the queue falls over on ----
    lane('lane/show/clash', 'docs/a.md', 'mine\n', base=INT + '~8')
    lane('lane/show/after', 'docs/after.md', 'after\n')
    landq.add(home, 'lane/show/clash')
    landq.add(home, 'lane/show/look-x', why='A look lane.')
    lane('lane/show/look-x', A + 'Presentation/X.cs', 'class X {}\n')
    landq.add(home, 'lane/show/after')
    w.card_falls, before = True, tip()
    did = landq.work(w, home, **quiet)
    case('a lane that does not go on integration\'s tip by itself is refused with the file that clashes; a lane the queue falls over on is said; and the lane behind both still lands, from a clean checkout',
         [d[:2] for d in did] == [('lane/show/clash', 'refused'), ('lane/show/look-x', 'refused'), ('lane/show/after', 'landed')] and 'docs/a.md' in did[0][2] and 'fell over' in did[1][2]
         and w.git('status', '--porcelain')[1] == '' and 'rebase in progress' not in w.git('status')[1], did)
    w.card_falls = False
    for ln in ('lane/show/clash', 'lane/show/look-x'):
        landq.drop(home, ln)

    # ---- already in, gone, held elsewhere, local commits ----
    landq.add(home, 'lane/show/docs-c')
    landq.add(home, 'lane/show/never-pushed')
    did = landq.work(w, home, **quiet)
    case('a lane that is in integration already, and one that is no longer on origin, leave the queue without a landing',
         [d[:2] for d in did] == [('lane/show/docs-c', 'in'), ('lane/show/never-pushed', 'gone')] and landq.queue(home) == [], did)
    lane('lane/show/held', 'docs/held.md', 'h\n')
    git(tree, 'fetch', '-q', 'origin')
    git(tree, 'worktree', 'add', '-q', '-b', 'lane/show/held', str(tmp / 'elsewhere'), 'origin/lane/show/held')
    landq.add(home, 'lane/show/held')
    did = landq.work(w, home, **quiet)
    case('a lane another checkout stands on waits, and says which checkout', did[0][1] == 'waiting' and 'elsewhere' in did[0][2] and landq.queue(home)[0]['state'] == 'waiting', did)
    landq.drop(home, 'lane/show/held')
    lane('lane/show/local', 'docs/local.md', 'l\n')
    git(tree, 'fetch', '-q', 'origin')
    git(tree, 'branch', 'lane/show/local', 'origin/lane/show/local')
    git(tree, 'checkout', '-q', 'lane/show/local')
    unpushed = commit(tree, 'docs/unpushed.md', 'u\n', 'not pushed')
    git(tree, 'checkout', '-q', '--detach')
    landq.add(home, 'lane/show/local')
    did = landq.work(w, home, **quiet)
    case('a local branch of the lane that holds commits origin does not have is not reset: the lane waits, and the commits are still there',
         did[0][1] == 'waiting' and 'not on origin' in did[0][2] and git(tree, 'rev-parse', 'lane/show/local') == unpushed, did)
    landq.drop(home, 'lane/show/local')

    # ---- no flood of cards ----
    for i in range(landq.ASK_MOST + 2):
        lane(f'lane/show/look-{i}', A + f'Presentation/L{i}.cs', 'class L {}\n')
        landq.add(home, f'lane/show/look-{i}')
    w.cards = []
    did = landq.work(w, home, **quiet)
    case(f'at most {landq.ASK_MOST} landing cards are open at a time: the lanes behind them wait, and say what for',
         [d[1] for d in did] == ['asked'] * landq.ASK_MOST + ['waiting'] * 2 and len(w.cards) == landq.ASK_MOST and 'its card comes when' in did[-1][2], [d[:2] for d in did])
    for e in landq.queue(home):
        landq.drop(home, e['lane'])

    # ---- the lanes the relay finished ----
    board = tmp / 'board'
    (board / 'relay' / 'done').mkdir(parents=True)
    for uid, ln in (('rv-10', 'lane/sim/review-fixes-2'), ('rv-11', 'lane/sim/review-fixes-2'), ('look-01', 'lane/show/look'), ('odd', 'main'), ('look-code-x', 'lane/show/found')):
        (board / 'relay' / 'done' / (uid + '.json')).write_text(json.dumps(dict(id=uid, lane=ln, head='0' * 40)), encoding='utf-8')
    new = landq.offer(home, board)
    case('offer: every lane a relay unit finished is put in the queue once; what is not a lane is not, nor the lane the looks at old work write on',
         sorted(new) == ['lane/show/look', 'lane/sim/review-fixes-2'] and landq.offer(home, board) == [] and 'rv-1' in landq.queue(home)[0]['why'] + landq.queue(home)[1]['why'], new)

    # ---- one worker ----
    lock = tmp / 'lockhome'
    first = landq.take_lock(lock)
    ended = subprocess.Popen([sys.executable, '-c', 'pass'])
    ended.wait()                                     # a process number that is nobody's now (4 is the System's own on Windows, and alive)
    landq.put(lock / 'lock.json', dict(pid=ended.pid, since='x'))
    gone = not landq.worker_alive(lock)
    landq.put(lock / 'lock.json', dict(pid=os.getppid(), since='x'))
    landq.put(lock / 'beat.json', dict(at=0, pid=os.getppid()))
    silent = not landq.worker_alive(lock)
    landq.beat(lock)
    landq.put(lock / 'beat.json', dict(landq.load(lock / 'beat.json', {}), pid=os.getppid()))
    case('one worker: a lock whose process is gone, or is there and has been silent for two hours, is no worker; one that is there and gives signs of life is',
         first and gone and silent and landq.worker_alive(lock) and not landq.take_lock(lock), (first, gone, silent))

    # ---- the click that lands: what briefs.answers calls `land` ----
    bw, nw, key = tmp / 'briefs', tmp / 'notes', 'the-listeners-key'
    Path(os.environ['TW_CLICK_KEY']).write_text(key + '\n', encoding='utf-8')         # this machine's listener signs with it
    now = datetime.datetime(2026, 10, 11, 9, 0, 0)

    def a_card(title, ln, commit_):
        b = briefs.add(bw, title, 'The flag idea is finished and tested.', ['Land it', 'Not now'], 'It is ready.', no_evidence='shown on its own cards', lane=ln, now=now)
        briefs.then(bw, [], b['id'], 'A', 'The queue lands it', land=dict(lane=ln, tip=commit_))
        return b['id'], briefs.find(bw, b['id'])['options'][0]['then']['stamp']

    def ans(bid, when=now, keys=None):
        return [a for a in briefs.answers(briefs.read_all(bw), notes.read_all(nw), now=when, keys=[key] if keys is None else keys) if a['id'] == bid][0]
    b1, st1 = a_card('Land the red flag?', 'lane/show/flag-red', look)
    notes.write(nw, 'A: Land it', kind='page', about='brief:' + b1, then=st1, now=now)
    written = ans(b1)
    b2, st2 = a_card('Land the blue flag?', 'lane/show/flag-blue', blue)
    n2 = notes.write(nw, 'A: Land it', kind='page', about='brief:' + b2, then=st2, now=now, sign=key)
    signed = ans(b2)
    case('a note that was merely written, with `from: owner`, `kind: page` and the right stamp, lands nothing: it reads as an answer a session must look at. The same click through the listener is `land`, with the lane and the commit',
         written['go'] == 'write' and 'not signed' in written['why'] and signed['go'] == 'land' and signed['land'] == dict(lane='lane/show/flag-blue', tip=blue) and len(st2) == 16, (written, signed))
    case('a click on a landing lapses after 48 hours', ans(b2, now + datetime.timedelta(hours=47))['go'] == 'land' and ans(b2, now + datetime.timedelta(hours=49))['go'] == 'write' and 'older than 48 hours' in ans(b2, now + datetime.timedelta(hours=49))['why'])
    case('a click signed with another key is not his here', ans(b2, keys=['some other key'])['go'] == 'write' and ans(b2, keys=[])['go'] == 'write')
    raw = json.loads((bw / b2 / 'brief.json').read_text(encoding='utf-8'))
    for swap in (dict(lane='lane/show/flag-blue', tip='f' * 40), dict(lane='lane/show/flag-blue', tip=blue, checkout='C:/x')):
        raw['options'][0]['then']['land'] = swap
        (bw / b2 / 'brief.json').write_text(json.dumps(raw), encoding='utf-8')
        swapped = ans(b2)
        if swapped['go'] != 'write':
            break
    raw['options'][0]['then']['land'] = dict(lane='lane/show/flag-blue', tip=blue)
    (bw / b2 / 'brief.json').write_text(json.dumps(raw), encoding='utf-8')
    case('another commit put under the line he clicked, or a word more in what it lands, is no yes of his', swapped['go'] == 'write' and 'not stamped again' in swapped['why'] and ans(b2)['go'] == 'land', swapped)
    try:
        briefs.take(bw, nw, b2, by='a session', note=n2['id'])
        took = ''
    except ValueError as x:
        took = str(x)
    case('a session cannot close a landing click as if it had been carried out: the queue closes it when it has landed', 'the landing queue does and closes' in took, took)
    wait = notes.write(nw, 'wait, not yet', kind='page', about='brief:' + b2, now=now + datetime.timedelta(minutes=5), sign=key)
    with_wait = ans(b2)
    notes.answer(nw, wait['id'], 'Noted.', by='a session')
    after = ans(b2)
    case('he clicks Land and then writes "wait" on the card: no landing; and a session that answers that note does not bring the click back',
         with_wait['go'] == 'write' and after['go'] == 'write' and 'nothing else on the card' in after['why'], (with_wait['why'], after['why']))
    b3 = briefs.add(bw, 'Land the red flag again?', 'Asked a second time.', ['Land it', 'Not now'], 'It is ready.', no_evidence='nothing new', lane='lane/show/flag-red', now=now)
    refused = []
    for land in (dict(lane='lane/show/flag-red', tip=look), dict(lane='lane/show/other', tip='abc'), dict(lane='main', tip=look), dict(checkout='C:/x', lane='lane/show/other')):
        try:
            briefs.then(bw, [], b3['id'], 'A', 'The queue lands it', land=land)
        except ValueError as x:
            refused.append(str(x))
    case('a second card for a lane that has one is refused (a landing is asked once), and so is a landing that names no whole commit, no lane, or a folder',
         len(refused) == 4 and 'asked once' in refused[0] and '40 characters' in refused[1] and 'not a lane' in refused[2], refused)

    # ---- the real card ----
    os.environ.update(TW_BRIEFS=str(tmp / 'cards'), TW_NOTES=str(tmp / 'cardnotes'))
    real = landq.World(tree, home)
    e = dict(lane='lane/show/flag-red', why='The relay finished the flag idea on it. ' + 'word ' * 40)
    why = 'it changes what a battle looks like or how it plays, or a path the queue does not know (trench-warfare-3d/Assets/_Project/Presentation/Flag.cs)'
    bid = real.card(e, look, why, ['a', 'b'])
    same = real.card(e, look, why, ['a', 'b'])
    card = briefs.find(tmp / 'cards', bid)
    his_click = notes.write(tmp / 'cardnotes', 'A: Land it', kind='page', about='brief:' + bid, then=card['options'][0]['then']['stamp'], sign=key)
    bid2 = real.card(e, 'e' * 40, why, ['a'])
    cards = {b['id']: b for b in briefs.read_all(tmp / 'cards')}
    old_note = [n for n in notes.read_all(tmp / 'cardnotes') if n['id'] == his_click['id']][0]
    case('the queue\'s own card passes the rules of a brief, even for a lane with a long story; its id names the lane and the commit, and its first option lands the lane at that commit; asked twice it is one card',
         bid == same == f'land-lane-show-flag-red-{look[:8]}' and card['options'][0]['then']['land'] == dict(lane='lane/show/flag-red', tip=look) and card['lane'] == 'lane/show/flag-red', card)
    case('when the lane moves on, the card of the old commit is closed in the queue\'s own name, his click on it is answered with why it did not land, and a new card asks for the new commit',
         bid2 == 'land-lane-show-flag-red-eeeeeeee' and cards[bid]['state'] == 'answered' and cards[bid]['answer']['by'] == 'the landing queue' and 'not his answer' in cards[bid]['answer']['said']
         and cards[bid2]['state'] == 'open' and cards[bid2]['options'][0]['then']['land']['tip'] == 'e' * 40 and old_note['state'] == 'done' and 'Not landed' in old_note['answers'][-1]['text'], cards[bid].get('answer'))
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')
    os.environ['TW_BRIEFS'] = tempfile.mkdtemp(prefix='tw-briefs-test-')
    os.environ['TW_CLICK_KEY'] = str(Path(tempfile.mkdtemp(prefix='tw-click-test-')) / 'click.key')
    sys.exit(main())
