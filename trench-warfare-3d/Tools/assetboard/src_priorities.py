"""Everything that is to be done, in one list, in the order it comes (the owner, 2026-10-12: "i want a priority list
with everything and its priorities"). Read-only: the page shows the order, he changes it by telling the master.

Nothing here is a new source. Each row is read from what the board already has, and says in a few plain words why
it stands where it does:

  yours     what only he can do: the open briefs and the open questions (data/queue.js)
  now       what is being worked right now: the relay's unit in hand, the sessions at work (data/runs.js, the floor)
  next      the relay's queue, as the pipeline board's origin/main has it, by the unit's own priority (lower first)
  land      branches whose work is done and that wait for a landing (data/queue.js)
  left      what agents left unfinished, and steps no agent has taken (data/tasks.js, data/queue.js)
  notyet    what is known and is nobody's work yet: the review's P1 findings no unit names, accepted ideas not routed

The relay does not simply go down its queue by priority, and the page must not say it does. It splits the day over
three kinds of work (Tools/relay/sources/__init__.py: finish, fix, critique) and takes the first unit by priority
OF THE KIND that is furthest under its share; every few turns it takes a wide turn for the area that waited longest
instead. Which kind is next depends on what the day has spent, which this station cannot see. So a unit carries its
kind and its place within that kind, and the first of each kind is marked as one the relay may take next.

Written as data/priorities.js (window.PRIORITIES), a script like every reading of the board (ops.py)."""
import json
import re
import time
from pathlib import Path

import tasks

BANDS = (('yours', 'Yours', 'only you can do these'),
         ('now', 'Being worked on now', 'in an agent\'s hands at this reading'),
         ('next', 'Next for the relay', 'the relay\'s queue, lower number first within its kind of work'),
         ('land', 'Done, waits for a landing', 'the work is finished and is not in the game yet'),
         ('left', 'Left unfinished', 'started and dropped, or ready and not taken by any agent'),
         ('notyet', 'Known, nobody\'s work yet', 'found or accepted, not queued'))
SHARES = dict(finish=60, fix=25, critique=15)       # Tools/relay/limits.json share_*, as it stood 2026-10-12
QUEUE_PRIORITY = 50                                 # limits.json queue_priority: a unit that names none
WIDE_EVERY = 3
# the area of the game a unit counts under (Tools/relay/sources/__init__.py AREAS, area_of)
AREAS = {'character-simulator': 'units', 'character': 'units', 'vehicle-simulator': 'vehicles', 'destruction-vfx-simulator': 'effects',
         'env-simulator': 'fields', 'balance-simulator': 'rules', 'game-designer': 'rules', 'sim': 'rules', 'ux': 'HUD', 'ui-artist': 'HUD',
         'optimizer': 'speed', 'review-fix': 'review fixes'}
KIND_WORDS = dict(finish='finishing started work', fix='fixing what a review found', critique='looking for what is wrong')
REVIEW = ('review-control-room', 'review_state.json')      # under the Drive's TW3D-pipeline
TITLE, WHY, ABOUT, GOAL = 140, 220, 200, 8000
MOST = dict(next=400, left=120, notyet=80)


def reading(path: Path, name):
    """One global of a data script of the site (window.NAME = <json>;), or None when it is not there or does not read."""
    try:
        text = Path(path).read_text(encoding='utf-8')
    except OSError:
        return None
    m = re.search(r'window\.%s = ' % re.escape(name), text)
    if not m:
        return None
    try:
        return json.JSONDecoder().raw_decode(text, m.end())[0]
    except ValueError:
        return None


def clip(s, n):
    s = ' '.join(str(s or '').split())
    return s if len(s) <= n else s[:n - 1].rstrip() + '…'


def short(lane):
    return re.sub(r'^lane/(show|sim)/', '', str(lane or ''))


def first_sentence(s, n=TITLE):
    """What a unit is, from its goal: his own words when the goal opens with 'The owner, <date>, to the brief "...":
    "..."', else the goal, up to its first full stop."""
    s = ' '.join(str(s or '').split())
    m = re.search(r':\s*["“]([^"”]{8,})["”]', s[:600]) if re.match(r'The owner, \d{4}-\d\d-\d\d', s) else None       # his own words, not the brief's title before them
    if m:
        s = m.group(1)
    cut = re.search(r'(?<=[a-z0-9\)])\. (?=[A-Z])', s)
    return clip(s[:cut.start() + 1] if cut else s, n)


def named(unit_id):
    """A unit's id as a name he can read: landing-small-lanes is 'Landing small lanes', rv3-07-lod-floors is
    'Review fix: lod floors'. The id itself stays on the row as a chip, since that is what the master is told."""
    s = str(unit_id or '')
    m = re.match(r'rv\d*-(?:c?\d+[a-z]?-)?(.+)', s)
    words = (m.group(1) if m else s).replace('-', ' ').replace('_', ' ').strip()
    words = words[:1].upper() + words[1:]
    return ('Review fix: ' + words[:1].lower() + words[1:]) if m else words


def kind_of(unit):
    """finish, fix or critique: the relay's own rule (sources/__init__.py kind_of)."""
    if unit.get('kind') in SHARES:
        return unit['kind']
    if unit.get('source') == 'critique':
        return 'critique'
    return 'fix' if unit.get('role') == 'review-fix' else 'finish'


def area_of(unit):
    role, lane = unit.get('role') or '', unit.get('lane') or ''
    if role in AREAS:
        return AREAS[role]
    return 'rules' if lane.startswith('lane/sim/') else 'tools and other'


def relay_queue(board, cache):
    """The relay's queue with what the task board leaves out (priority, role, kind) and what the relay tried:
    {units [..], done {id}, tried {id: record}, live [..], as_of}. Read with git off origin/main (tasks.REF), again
    only when it moved; tasks.py has fetched it on this read already."""
    empty = dict(units=[], done=[], tried={}, live=[], as_of='', read=False)
    if not board or not Path(board).is_dir():
        return empty
    seen = cache.setdefault('priorities', {})
    sha = tasks.git(board, 'rev-parse', '--verify', '--quiet', tasks.REF).decode().strip()
    if not sha:
        return empty
    if seen.get('sha') == sha and seen.get('data'):
        return seen['data']
    want = ('relay/queue/', 'relay/done/', 'relay/tried/', 'relay/desktop/live/', 'relay/laptop/live/')
    names = [n for n in tasks.git(board, 'ls-tree', '-r', '--name-only', tasks.REF, 'relay').decode('utf-8', 'replace').splitlines()
             if n.endswith('.json') and n.startswith(want)]
    out = tasks.git(board, 'cat-file', '--batch', send=''.join(f'{tasks.REF}:{n}\n' for n in names).encode('utf-8'))
    data, at = dict(empty, units=[], done=[], tried={}, live=[], read=True), 0
    data['as_of'] = tasks.git(board, 'log', '-1', '--format=%ci', tasks.REF).decode().strip()[:16]
    for n in names:
        end = out.find(b'\n', at)
        head = out[at:end].split()
        if end < 0 or len(head) != 3 or head[1] != b'blob':
            break
        size = int(head[2])
        try:
            d = json.loads(out[end + 1:end + 1 + size].decode('utf-8', 'replace'))
        except ValueError:
            d = None
        at = end + 1 + size + 1
        if not isinstance(d, dict):
            continue
        stem = Path(n).stem
        if n.startswith('relay/queue/') and d.get('id'):
            data['units'].append(dict(id=d['id'], lane=d.get('lane') or '', role=d.get('role') or '', kind=d.get('kind') or '',
                                      priority=d.get('priority'), goal=' '.join(str(d.get('goal') or '').split())[:GOAL], file=stem))
        elif n.startswith('relay/done/'):
            data['done'].append(d.get('id') or stem)
        elif n.startswith('relay/tried/'):
            data['tried'][d.get('id') or stem] = {k: d.get(k) for k in ('verdict', 'tries', 'at', 'problems')}
        elif '/live/' in n:
            data['live'].append(dict(run=d.get('run') or stem, now_on=d.get('now_on') or {}, beat=d.get('beat') or ''))
    seen.update(sha=sha, data=data)
    return data


def relay_rows(rq, in_hand=()):
    """The waiting units as rows, in the order the page lists them: by priority (a unit that names none is 50), then
    by file name, as the relay sorts them (sources/lane.py files). Each says its kind and its place in that kind."""
    done, hand = set(rq['done']), set(in_hand)
    waiting = [u for u in rq['units'] if u['id'] not in done and u['id'] not in hand]
    waiting.sort(key=lambda u: (QUEUE_PRIORITY if u['priority'] is None else int(u['priority']), u['file']))
    place, rows = {}, []
    for u in waiting:
        kind, pr = kind_of(u), QUEUE_PRIORITY if u['priority'] is None else int(u['priority'])
        place[kind] = place.get(kind, 0) + 1
        tried = rq['tried'].get(u['id']) or {}
        tries = int(tried.get('tries') or 0)
        why = f'priority {pr}' + ('' if u['priority'] is not None else ' (none set, so the usual 50)') + f' · {KIND_WORDS[kind]}, number {place[kind]} of that kind'
        if place[kind] == 1:
            why += ' · the relay may take this next'
        if tries:
            why += f' · tried {tries} time{"s" if tries != 1 else ""}, last result {str(tried.get("verdict") or "none").lower()}'
        rows.append(dict(band='next', kind='relay unit', id='unit:' + u['id'], title=named(u['id']), about=first_sentence(u['goal'], ABOUT), name=u['id'], why=why, lane=u['lane'],
                         chips=[f'priority {pr}', kind, area_of(u)] + ([f'tried {tries}×'] if tries else []), first=place[kind] == 1,
                         link='runs.html', n=pr))
    return rows


def yours(q):
    rows = []
    for b in q.get('briefs') or []:
        days = b.get('days')
        rows.append(dict(band='yours', kind='decision', id='brief:' + str(b.get('brief')), title=clip(b.get('title'), TITLE), lane=b.get('lane') or '',
                         why='your decision' + (f', asked {days} day{"s" if days != 1 else ""} ago' if days else ', asked today') + (f' · {b["options"]} options' if b.get('options') else ''),
                         chips=['decide'], link='decide.html#' + str(b.get('brief')), shot=b.get('shot') or ''))
    for d in q.get('decide') or []:
        days = d.get('days')
        rows.append(dict(band='yours', kind='question', id='ask:' + clip(d.get('title') or d.get('text'), 60), title=clip(d.get('title') or d.get('text'), TITLE), lane='',
                         why='an open question with no brief yet' + (f', {days} days old' if days else ''), chips=['question'], link='decide.html'))
    return rows


def now_rows(runs, lanes, rq):
    """What is in hand: the unit of every run that is going, then the sessions and agents at work on the floor."""
    rows, hand = [], []
    units = {u['id']: u for u in rq['units']}
    going = [r for r in (runs or {}).get('runs') or [] if r.get('state') == 'going' and r.get('now_on')]
    seen = {r['now_on'] if isinstance(r['now_on'], str) else (r['now_on'] or {}).get('unit') for r in going}
    for l in rq['live']:
        if l['now_on'].get('unit'):
            seen.add(l['now_on']['unit'])
    for uid in sorted(u for u in seen if u):
        u = units.get(uid) or {}
        hand.append(uid)
        rows.append(dict(band='now', kind='relay unit', id='unit:' + uid, title=named(uid), about=first_sentence(u.get('goal'), ABOUT), name=uid, lane=u.get('lane') or '',
                         why='the relay is on it now' + (f' · {KIND_WORDS[kind_of(u)]}' if u else ''), chips=['relay', 'at work'], link='runs.html'))
    for l in lanes or []:
        for w in l.get('workers') or []:
            if w.get('state') != 'working' or w.get('kind') not in ('session', 'agent') or str(w.get('id', '')).startswith('agent:relay'):
                continue
            who = w.get('title') or (w.get('what') if w['kind'] == 'agent' else '') or w.get('name') or w.get('id')       # a subagent has no title: what it was sent to do says more than its type
            rows.append(dict(band='now', kind=w['kind'], id='worker:' + str(w.get('uid') or w.get('id')), title=clip(who, TITLE), lane='' if l.get('made') else l.get('branch') or '',
                             why=clip('at work' + (f' since {w["since"]}' if w.get('since') else '') + (f' · {w["what"]}' if w.get('what') and w.get('what') != who else ''), WHY),
                             chips=[w['kind'], 'at work'] + ([f'on {w["host"]}'] if w.get('host') else []),
                             link='house.html?pin=' + str(w.get('uid') or w.get('id'))))
    return rows, hand


def land_rows(q):
    rows = []
    said = dict(said='you said to land it', land='the gate is green and its brief is open', approved='approved on the board', owed='the gate is green; an agent owes its brief')
    for g in ('said', 'land', 'approved', 'owed'):
        for e in q.get(g) or []:
            behind, ahead = e.get('behind'), e.get('ahead')
            why = said[g] + (f' · holds {ahead} commit{"s" if ahead != 1 else ""}' if ahead else '') + (f' · {behind} behind the game, so it is rebased and gated again first' if behind else ' · up to date with the game')
            rows.append(dict(band='land', kind='landing', id='land:' + str(e.get('lane')), title='Land ' + short(e.get('lane')), lane=e.get('lane') or '', why=why,
                             chips=[dict(said='your word', land='waits on you', approved='approved', owed='agent\'s turn')[g]], link='floor.html', n=behind or 0))
    return rows


def left_rows(t, q):
    rows = []
    kinds = dict(capture='your capture from the game', agent='an agent stopped', session='a session stopped', handoff='a handoff nobody picked up', unit='a unit written and not queued',
                 relay='the relay left it', ready='a step is ready and not taken', paused='paused')
    for r in (t or {}).get('rows') or []:
        if r.get('state') != 'left':
            continue
        idle = r.get('idle')
        age = '' if idle is None else f' · idle {int(idle // 1440)} days' if idle >= 2880 else f' · idle {int(idle // 60)} h' if idle >= 120 else f' · idle {int(idle)} min'
        rows.append(dict(band='left', kind='task', id='task:' + str(r.get('id')), title=clip(r.get('title'), TITLE), lane=r.get('lane') or '',
                         why=clip(kinds.get(r.get('kind'), str(r.get('kind'))) + age + (f' · {r["stands"]}' if r.get('stands') else ''), WHY),
                         chips=[str(r.get('kind'))], link='tasks.html', n=idle or 0, first=r.get('kind') == 'capture'))
    rows.sort(key=lambda r: (not r.get('first'), r['n']))
    for e in q.get('ready') or []:
        rows.append(dict(band='left', kind='step', id='ready:' + str(e.get('item')) + ':' + str(e.get('stage')), title=clip(e.get('title'), TITLE), lane=e.get('lane') or '',
                         why=f'its step {e.get("stage")} is ready for the {e.get("role")} role' + (f', {e["days"]} days now' if e.get('days') else '') + '; nobody has taken it',
                         chips=['ready step'], link='tasks.html'))
    for e in q.get('broken') or []:
        rows.append(dict(band='left', kind='loose end', id='broken:' + clip(e.get('title'), 50) + str(e.get('lane')), title=clip(e.get('title') or e.get('text'), TITLE), lane=e.get('lane') or '',
                         why=clip((e.get('detail') or [''])[0], WHY), chips=[str(e.get('kind') or 'loose end')], link='index.html#queue'))
    return rows


def notyet_rows(review, ideas, rq):
    rows = []
    goals = [u['goal'] for u in rq['units']]
    size = dict(S=0, M=1, L=2)

    def in_a_unit(f):
        """A unit's goal names the finding by its id, or names its file among findings it calls P1 (the review's units
        name a file and a line, not always the id). Units that are done count: the fix is then written."""
        mark, base = re.compile(r'(?<![\w.])%s(?![\w.])' % re.escape(str(f.get('id')))), Path(str(f.get('path') or '')).name
        return any(mark.search(g) or (base and base in g and re.search(r'\bP1\b', g)) for g in goals)

    p1 = [f for f in (review or {}).get('findings') or [] if f.get('severity') == 'P1' and not in_a_unit(f)]
    p1.sort(key=lambda f: (size.get(f.get('size'), 3), str(f.get('id'))))
    for f in p1:
        rows.append(dict(band='notyet', kind='review finding', id='finding:' + str(f.get('id')), title=clip(f.get('title'), TITLE), lane='',
                         why=clip(f'P1 from the code review, size {f.get("size") or "?"}, in {Path(str(f.get("path") or "")).name}; no relay unit, waiting or done, names it or its file', WHY),
                         chips=['P1', str(f.get('lane') or ''), str(f.get('kind') or '')], link=''))
    for i in (ideas or {}).get('ideas') or []:
        if i.get('state') == 'accepted' and not i.get('routed'):
            rows.append(dict(band='notyet', kind='idea', id='idea:' + str(i.get('id')), title=clip(i.get('title'), TITLE), lane='',
                             why=f'you said yes {str((i.get("answer") or {}).get("when") or "")[:10]}; it has no route on the board yet' + (f' · {i["score"]}' if i.get('score') else ''),
                             chips=['idea', str(i.get('size') or '')], link='index.html#ideas'))
    return rows


def collect(data_dir: Path, board, lanes, cache, review=None, now=None):
    """The whole list: {rows, bands, relay, at, wrong}. `data_dir` is the site's data folder as this read wrote it,
    `lanes` the floor's lanes of this read, `cache` this station's cache of the queue."""
    data_dir, wrong = Path(data_dir), []
    q = reading(data_dir / 'queue.js', 'QUEUE') or {}
    t = reading(data_dir / 'tasks.js', 'TASKS')
    ideas = reading(data_dir / 'ideas.js', 'IDEAS')
    runs = reading(data_dir / 'runs.js', 'RUNS')
    if not q:
        wrong.append('What waits on you could not be read.')
    if t is None:
        wrong.append('The tasks could not be read.')
    try:
        rq = relay_queue(board, cache)
    except Exception as e:      # noqa: BLE001  a queue that does not read is said, the rest of the list stands
        rq = dict(units=[], done=[], tried={}, live=[], as_of='', read=False)
        wrong.append(f'The relay\'s queue did not read ({type(e).__name__}).')
    if not rq.get('read'):
        wrong.append('The relay\'s queue could not be read off the pipeline board, so "Next for the relay" is empty.')
    if review is None:
        wrong.append('The code review\'s findings could not be read, so its P1s are not listed.')
    in_now, hand = now_rows(runs, lanes, rq)
    by = dict(yours=yours(q), now=in_now, next=relay_rows(rq, hand), land=land_rows(q), left=left_rows(t, q), notyet=notyet_rows(review, ideas, rq))
    rows, bands = [], []
    for key, name, says in BANDS:
        mine = by[key]
        bands.append(dict(id=key, name=name, says=says, n=len(mine)))
        for r in mine[:MOST.get(key, 200)]:
            r['rank'] = len(rows) + 1
            r['chips'] = [c for c in r.get('chips') or [] if c]
            rows.append(r)
    kinds = {}
    for r in by['next']:
        k = r['chips'][1]
        kinds[k] = kinds.get(k, 0) + 1
    return dict(rows=rows, bands=bands, wrong=wrong, at=time.strftime('%Y-%m-%d %H:%M', time.localtime(now or time.time())),
                relay=dict(as_of=rq.get('as_of') or '', waiting=len(by['next']), kinds=kinds, shares=SHARES, wide_every=WIDE_EVERY))

