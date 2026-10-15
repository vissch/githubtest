#!/usr/bin/env python3
"""Four steps of the loop that works by itself (autoloop.py), from the owner's words of 2026-10-15: "make sure that
decisions made on the page and ideas picked are taken into the relays, that code reviews are running when its
approriate. the ai doesnt go over the same items multiple times without noticable progress."

  writes    an answer of his in his own words (briefs.py `write`: his words, an option with no Then line, two options
            clicked) is read by one cheap headless leg (Sonnet, low effort, at most WRITE_USD, booked on the day as the
            station `autoloop`). It writes the unit his words lead to; the loop queues it and closes the brief with
            --queued, or closes it with --outcome when his words build nothing. He is never asked again
  ideas     an idea he said "Do it" to, and a step of his on an idea's route, go on the pipeline's board
            (idearoute.py route and gates); a tool idea's unit goes on the relay's queue. Only while the main relay
            does not run: a leg must not see the board's items move under it
  reviews   a code review is queued not only when the queue runs dry (found.py rota) but also after a landing, for
            the review units whose files the landing changed and that changed since their last review, and once a
            day while the day's pace has room. found.py's ceiling (FIX_MOST unstarted fixes) and one open look at a
            time hold; the relay's shares hold the 25% fix share
  attempts  a unit that ended FAIL or BLOCKED TRIES times, with nothing changed since its last try (no new commit on
            its lane, no landing that touched its files, no new answer of his), is parked: taken off every queue
            the runners read (a priority of 99 only sorts it last), kept in the loop's folder, and put to him in ONE
            brief with what was tried and its last leg's own Next. It goes back by itself when its lane or the files
            it works on change, or when he answers A on that brief

No step here asks him anything a brief has not already asked, and none runs a model but `writes`.
"""
import datetime
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import found  # noqa: E402

WRITE_USD = 0.25        # the most one leg that reads his words may cost
WRITE_DAY_USD = 2.0     # what those legs may cost together in a day
WRITE_TRIES = 2         # a reply that is no unit: tried again once on the same note, then said once and left
TRIES = 2               # FAIL or BLOCKED this often with nothing changed between: the unit is parked
SINCE_DAYS = 3          # stop records older than this are not counted (the boards keep weeks of them)
PARKED = found.PARKED
INTEGRATION = found.INTEGRATION
ROLES = found.ROLES
SAY_MOST = 45           # briefs.py FOR_WORDS


def load(f, default):
    return found.load(Path(f), default)


def save(f, data):
    found.put(Path(f), data)


def words(s, n):
    w = found.one(s).split()
    return ' '.join(w[:n]) + (' ...' if len(w) > n else '')


def today(w):
    return datetime.datetime.fromtimestamp(w.now()).strftime('%Y-%m-%d')


# ---- writes: his own words become a unit ---------------------------------------------------------------------------------

PROMPT = '''The owner of a game studio answered a decision on his Decide page. His answer IS the decision: he is never asked again.
Say what it leads to. Reply with ONE JSON object and nothing else.

The decision: {title}
The option he took: {option}
His words: {said}
His notes on it, oldest first:
{notes}
The lane the decision was about: {lane}
What git says of that lane now: {facts}

When it leads to work a coding agent can do in one lane:
{{"decision": "build", "id": "a-short-kebab-id", "lane": "lane/show/<name>" (lane/sim/<name> when it changes the simulation), "role": one of {roles}, "goal": "what to do, quoting his words", "done_when": "what a test or a script shows once it is done"}}
When his answer builds nothing (he keeps what is there, sets a rule, or what he wants is already on the integration branch):
{{"decision": "nothing", "why": "one sentence"}}
Rules: landing is the owner's own step, never a unit. "Land it" on a lane that is already on the integration branch builds nothing. "Land it" on a lane that is not: the unit makes the lane ready to land (rebased onto the integration branch, its tests green, pushed) and never lands it. Keep his intent; do not add work he did not ask for.'''

ID = re.compile(r'^[a-z0-9][a-z0-9-]{2,47}$')
LANE = re.compile(r'^lane/(show|sim)/[a-z0-9][a-z0-9.-]{1,80}$')


def reply(text):
    """The leg's reply as a decision: {'decision': 'build', id, lane, role, goal, done_when} or {'decision':
    'nothing', why}. A ValueError says what is wrong with it."""
    m = re.search(r'\{.*\}', str(text or ''), re.S)
    if not m:
        raise ValueError('the reply holds no JSON object')
    try:
        d = json.loads(m.group(0))
    except ValueError as e:
        raise ValueError(f'the reply is no JSON: {e}')
    if d.get('decision') == 'nothing':
        if len(found.one(d.get('why'))) < 10:
            raise ValueError('it builds nothing and does not say why')
        return dict(decision='nothing', why=found.one(d['why'])[:300])
    if d.get('decision') != 'build':
        raise ValueError(f'its decision is {d.get("decision")!r}, not build or nothing')
    bad = [k for k, ok in (('id', ID.match(str(d.get('id', '')))), ('lane', LANE.match(str(d.get('lane', '')))),
                           ('role', d.get('role') in ROLES), ('goal', len(found.one(d.get('goal')).split()) >= 6),
                           ('done_when', len(found.one(d.get('done_when')).split()) >= 3)) if not ok]
    if bad:
        raise ValueError('its ' + ', '.join(bad) + ' will not do')
    return dict(decision='build', **{k: found.one(d[k]) for k in ('id', 'lane', 'role', 'goal', 'done_when')})


def unit_of(a, d):
    """The queue file: his words first, the leg's goal, and a done-when written here (a commit on the lane names the
    unit), so no command a model wrote is ever run to check it."""
    his = a.get('said') or a.get('text') or ''
    goal = (f'The owner\'s answer on the Decide page to "{found.one(a["title"])}": "{found.one(his)}". {d["goal"]} '
            f'Done when: {d["done_when"]}. Commit and push the lane only: do not land. Put the tag [{d["id"]}] in a commit '
            f'message only once that holds, not before.')
    check = ("import subprocess,sys;o=subprocess.run(['git','log','--format=%%B','origin/%s..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             "sys.exit(0 if '[%s]' in o else 1)" % (INTEGRATION, d['id']))
    return dict(id=d['id'], lane=d['lane'], role=d['role'], goal=goal[:3000], done_when=['python', '-c', check])


def prompt_of(a, facts):
    notes = '\n'.join(f'- {n["when"]}: {found.one(n["text"])}' for n in a.get('notes') or []) or '- (none)'
    return PROMPT.format(title=found.one(a['title']), option=f'{a.get("option")}: {found.one(a.get("text"))}' if a.get('text') else 'none of the options',
                         said=found.one(a.get('said')) or '(only the click)', notes=notes, lane=a.get('lane') or '(none named)', facts=facts,
                         roles=', '.join(ROLES))


def spent_today(w):
    return (load(w.home / 'legs.json', {}).get(today(w)) or {}).get('usd', 0.0)


def book(w, usd):
    """Add one leg's cost to the day's record of the station `autoloop` and book it (relay.py agents book --file)."""
    legs = load(w.home / 'legs.json', {})
    d = legs.setdefault(today(w), dict(usd=0.0, n=0))
    d['usd'], d['n'] = round(d['usd'] + float(usd or 0), 4), d['n'] + 1
    save(w.home / 'legs.json', legs)
    w.book(dict(day=today(w), station='autoloop', usd=round(d['usd'], 2), agents=d['n'], models={'sonnet': round(d['usd'], 2)},
                unpriced=[], at=datetime.datetime.utcfromtimestamp(w.now()).strftime('%Y-%m-%dT%H:%M:%SZ'), by='autoloop'))
    return d['usd']


def option_of(a):
    """The option to close with when his notes do not settle it themselves: his last pick."""
    return a['option'] if a.get('why') == 'his notes do not all pick the same option' and a.get('option') in tuple('ABCD') else ''


def take_writes(w):
    """His answers in his own words: a leg reads them and the loop queues what they lead to, or says why nothing."""
    out, st = [], load(w.home / 'writes.json', {})
    todo = [a for a in w.answers() if a['go'] == 'write' and ((st.get(f'{a["id"]}@{a["note"]}') or {}).get('tries', 0) < WRITE_TRIES
                                                             or (st.get(f'{a["id"]}@{a["note"]}') or {}).get('unit'))]
    if not todo:
        return out
    if not w.acting:
        return [f'writes: {a["id"]}: a leg would read his words and write the unit they lead to' for a in todo]
    free = w.pace_free()
    for a in todo:
        key = f'{a["id"]}@{a["note"]}'
        if (st.get(key) or {}).get('unit'):             # queued by an earlier round whose close was refused: close it, no second leg
            try:
                w.take(a, queued=st[key]['unit'], option=option_of(a))
                dropped_by(w, a['id'])
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a.get("option")}: {found.one(a.get("said") or a.get("text"))}** Queued as `{st[key]["unit"]}`. (owner, Decide page, note {a["note"]}) |')
                st[key]['went'] = 'queued'
                out.append(f'writes: {a["id"]}: closed with {st[key]["unit"]}, queued in an earlier round')
            except Exception as e:      # noqa: BLE001
                out.append(f'writes: {a["id"]}: {st[key]["unit"]} is queued, the brief not closed yet ({str(e)[:120]})')
            save(w.home / 'writes.json', st)
            continue
        if free is None or free <= 0:
            out.append(f'writes: {len(todo)} answers in his own words wait: the day\'s pace has nothing free now')
            break
        if spent_today(w) + WRITE_USD > WRITE_DAY_USD:
            out.append(f'writes: {len(todo)} answers in his own words wait: these legs used ${spent_today(w):.2f} of their ${WRITE_DAY_USD:.2f} today')
            break
        rec = st.setdefault(key, dict(tries=0))
        rec['tries'] += 1
        text, usd = w.leg(prompt_of(a, w.lane_facts(a.get('lane'))), WRITE_USD)
        book(w, usd)
        try:
            d = reply(text)
            if d['decision'] == 'nothing':
                w.take(a, outcome=f'Read by the loop: {d["why"]}', option=option_of(a))
                dropped_by(w, a['id'])
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a.get("option")}: {found.one(a.get("said") or a.get("text"))}** Nothing is built: {d["why"]} (owner, Decide page, note {a["note"]}) |')
                rec.update(went='nothing', why=d['why'])
                out.append(f'writes: {a["id"]}: his words build nothing ({d["why"][:120]}); the brief is closed')
            else:
                taken = w.ids()                             # an id a board has queued or done is another unit's: never reused
                uid = next(x for x in [d['id']] + [f'{d["id"][:44]}-{n}' for n in range(2, 99)] if x not in taken)
                unit = unit_of(a, dict(d, id=uid))
                ok, said = w.add(unit)
                if not ok:
                    raise ValueError(f'the relay refused {unit["id"]}: {said}')
                rec['unit'] = unit['id']                     # queued: a refused close is retried with this id, never a second leg
                save(w.home / 'writes.json', st)
                w.take(a, queued=unit['id'], option=option_of(a))
                dropped_by(w, a['id'])                      # his words on a parked unit's brief: the old unit goes
                w.log_decision(f'| {a["when"][:10]} | **Taken up by the loop.** **{a["title"]}: {a.get("option")}: {found.one(a.get("said") or a.get("text"))}** Queued as `{unit["id"]}` on {unit["lane"]}. (owner, Decide page, note {a["note"]}) |')
                rec.update(went='queued', unit=unit['id'])
                out.append(f'writes: {a["id"]}: his words queued {unit["id"]} ({unit["role"]}, {unit["lane"]}); the brief is closed')
        except Exception as e:      # noqa: BLE001  a reply that is no unit, or a refusal: tried once more, then said once
            rec['last'] = str(e)[:300]
            last = rec['tries'] >= WRITE_TRIES
            out.append(f'writes: {a["id"]}: not taken up ({str(e)[:160]})' + (': left for a session, said once' if last else ': tried again next round'))
        save(w.home / 'writes.json', st)
    return out


# ---- ideas: his "Do it" and his steps go on the board -------------------------------------------------------------------

def route_ideas(w):
    running, _ = w.status(w.main)
    if running is None:
        return ['ideas: the main relay did not say whether it runs: nothing is routed this round']
    routed, g, err = w.ideas(write=False)
    if err:
        return [f'ideas: idearoute could not read the ideas: {err[:200]}']
    if not (routed or g['asked'] or g['done']):
        return []
    what = f'{len(routed)} ideas he said yes to, {len(g["asked"])} steps of his to put to him, {len(g["done"])} of his step answers'
    if running:
        return [f'ideas: {what} wait: the main relay runs, and its board is not written under a leg']
    if not w.acting:
        return [f'ideas: would route {what}']
    w.pull(w.main)
    routed, g, err = w.ideas(write=True)
    out = [f'ideas: idearoute stopped: {err[:200]}'] if err else []
    paths = w.changed(w.main, ('items', 'results', 'feedback'))
    if paths:
        said = w.commit_push(w.main, paths, f'ideas: {len(routed)} routed, {len(g["asked"])} steps put to him, {len(g["done"])} answers acted on (autoloop)')
        out.append(f'ideas: the board: {said}')
    for iid, kind, rid, f in routed:
        if kind != 'unit':
            out.append(f'ideas: {iid} is on the pipeline\'s board as {rid}')
            continue
        unit = load(f, {})
        unit.setdefault('role', 'lane')
        ok, said = w.add(unit) if unit.get('id') else (False, f'{f} does not read')
        out.append(f'ideas: {iid}: its unit {rid} ' + ('is queued' if ok else f'was refused: {said}'))
    out += [f'ideas: put to him: {b}' for b in g['asked']] + [f'ideas: his step answer acted on: {b}: {x}' for b, x in g['done']]
    return out


# ---- reviews: after a landing, and once a day ----------------------------------------------------------------------------

def gates_shut(fw):
    """Why no look may be queued now, or ''."""
    q, done = fw.queued()
    if any(v.get('kind') == 'critique' and u not in done for u, v in q.items()):
        return 'a look is queued and not done'
    n = found.waiting(fw)
    return f'{n} fix units wait unstarted (the most is {found.FIX_MOST})' if n >= found.FIX_MOST else ''


def reviews(w, fw):
    st, out = load(w.home / 'reviews.json', {}), []
    tip = fw.tip()
    if not tip:
        return ['reviews: the integration branch did not read: nothing this round']
    st.setdefault('tip', tip)                               # the first round only marks where integration stands
    shut = gates_shut(fw)
    if st['tip'] != tip:                                    # a landing: review what it touched, when it changed since its last review
        landed = set(fw.changed(st['tip'], tip))
        due = [s for s in fw.reviews('origin/' + INTEGRATION) if landed & set(s.get('files') or [])]
        if due and shut:
            return [f'reviews: a landing touched {len(due)} review units: waits ({shut})']
        line = ''
        if due:
            line = found.rota(fw, why='landing', among=due) if w.acting else f'would queue a review of {due[0]["id"]}'
            out.append(f'reviews: after the landing {st["tip"][:9]}..{tip[:9]}: {line}')
        if not due or line.startswith(('found: queued', 'found: every subject')):
            st['tip'] = tip                                 # a review that was refused is tried again next round
    day = today(w)
    looked = load(fw.home / 'state.json', {})
    if st.get('daily') != day and not any(str(v.get('queued', '')).startswith(day) for v in looked.values()):
        free = w.pace_free()
        if shut or gates_shut(fw):
            pass                                            # the ceiling holds: said by found.rota's own line
        elif free is None or free <= 0:
            out.append('reviews: no review today yet: the day\'s pace has nothing free')
        else:
            if w.acting:
                subjects = fw.subjects()
                rv = [s for s in subjects if s['kind'] == 'review']
                line = found.rota(fw, why='daily', among=rv or subjects)
                if line.startswith(('found: queued', 'found: every subject')):
                    st['daily'] = day                       # nothing due is an answer for the day too
            else:
                line = 'would queue the day\'s review'
            out.append(f'reviews: the day\'s review: {line}')
    if w.acting:
        save(w.home / 'reviews.json', st)
    return out


# ---- attempts: no unit is gone over again without progress ------------------------------------------------------------

NOT_ITS_OWN = re.compile(r'cannot be switched to|the work checkout cannot be used|index\.lock', re.I)   # a clash between relays, not the unit's try


def paths_named(text):
    return set(re.findall(r'(?:trench-warfare-3d/)?(?:Assets|Tools|docs|Packages)/[\w./-]+', str(text or '')))


def differs(w, a, b, unit):
    """What changed for a unit between two fingerprints {lane: its lane's head, base: integration's head}, or ''."""
    lane = unit.get('lane', '')
    if (a or {}).get('lane') != (b or {}).get('lane'):
        return f'new commits on {lane}'
    if a.get('base') and b.get('base') and a['base'] != b['base']:
        mine = set(w.lane_files(lane)) | {p if p.startswith('trench-warfare-3d/') else 'trench-warfare-3d/' + p for p in paths_named(unit.get('goal'))}
        hit = {f for f in mine & set(w.changed_files(a['base'], b['base'])) if not f.endswith('.md') and not f.startswith('docs/')}   # every landing writes the docs
        if hit:
            return f'a landing changed {sorted(hit)[0]}'
    return ''


def changed_since(w, fp, unit):
    return differs(w, fp or {}, dict(lane=w.lane_sha(unit.get('lane', '')), base=w.int_sha()), unit)


def note_next(w, board, uid):
    """The 'Next' section of the unit's last leg note on a board, or ''."""
    f = Path(board) / 'relay' / 'notes' / f'{uid}.md'
    try:
        text = f.read_text(encoding='utf-8')
    except OSError:
        return ''
    m = re.search(r'^#+\s*Next\s*$(.*?)(?=^#+\s|\Z)', text, re.M | re.S)
    return found.one(m.group(1)) if m else ''


def park(w, uid, rec, on, out, dropped=False):
    """Take a unit off the queues of `on` {board: its unit} and keep it in the loop's folder. True when it is off all."""
    u = next(iter(on.values()))
    olds = [found.prio(x) for x in on.values() if found.prio(x) < PARKED]
    keep = dict(u, priority=PARKED, old_priority=min(olds) if olds else 50, fp=rec.get('fp') or {}, boards=[str(b) for b in on], at=today(w))
    save(w.home / 'parked' / f'{uid}.json', keep)
    gone = [str(b) for b in on if w.unqueue(b, uid)]
    if len(gone) < len(on):
        out.append(f'attempts: {uid} could not be taken off every queue ({len(gone)} of {len(on)}): tried next round')
        return False
    rec['parked'], rec['unit'] = keep, {k: u[k] for k in ('id', 'lane', 'goal') if k in u}
    if dropped:
        rec.update(dropped=True, drop=False)
    return True


def attempts(w, opens, dones, running, idle, extras=None):
    """Count each unit's tries from the stop records of every board; park the ones that went nowhere twice; bring a
    parked one back when something changed. `opens` {board: {id: unit}}, `dones` the ids done on any board, `running`
    the lanes a relay works now (None when one did not say), `idle` the boards whose relay does not run: a queue is
    only written on those, so no run sees its board move and no git of the loop's races a runner's."""
    st, out = load(w.home / 'attempts.json', {}), []
    if running is None:
        return ['attempts: a relay did not say whether it runs: nothing is parked this round']
    idle = {Path(b) for b in idle}
    units = {}
    for b, us in opens.items():
        for uid, u in us.items():
            units.setdefault(uid, u)
    for run, at, uid, verdict, why in w.stops():
        rec = st.setdefault(uid, dict(seen=[], fails=[]))
        if run in rec['seen']:
            continue
        rec['seen'] = (rec['seen'] + [run])[-40:]
        if verdict == 'PASS':
            rec.update(fails=[], fp={})
        elif verdict in ('FAIL', 'BLOCKED') and not NOT_ITS_OWN.search(why or ''):
            u = units.get(uid) or rec.get('unit') or {}
            fp = dict(lane=w.lane_sha(u.get('lane', ''), at), base=w.int_sha(at))     # as they stood when that try ended
            if rec['fails'] and differs(w, rec.get('fp') or {}, fp, u):
                rec['fails'] = []                           # it moved since the try before: that one does not count against it
            rec['fails'] = (rec['fails'] + [f'{run} {verdict}'])[-12:]
            rec['why'], rec['fp'] = found.one(why)[:300], fp
    for uid, rec in sorted(st.items()):
        parked = rec.get('parked')
        if uid in dones:
            if parked:
                rec['parked'] = None
            continue
        on = {b: us[uid] for b, us in opens.items() if uid in us}
        if rec.get('drop') and not parked:                  # his B on its brief after it had come back by itself
            if on and all(Path(b) in idle for b in on) and w.acting and park(w, uid, rec, on, out, dropped=True):
                out.append(f'attempts: {uid} taken off the queue again: his answer on its brief was to drop it')
            continue
        if parked:
            if not rec.get('dropped') and rec.get('brief') and w.brief_answered(rec['brief']):
                rec['dropped'] = True                       # he answered its brief with something other than A: his word stands
            if rec.get('dropped') or parked.get('lane') in running or not w.acting:
                continue
            if not rec.get('brief'):                        # parked, but its brief was refused last time
                try:
                    rec['brief'] = w.park_brief(uid, rec, rec.get('next', ''), {k: v for k, v in parked.items() if k not in ('fp', 'boards', 'old_priority', 'at', 'priority')})
                    out.append(f'attempts: {uid}: its brief is written now: {rec["brief"]}')
                except Exception as e:  # noqa: BLE001
                    out.append(f'attempts: {uid} is parked, its brief was refused again: {str(e)[:160]}')
            what = changed_since(w, parked.get('fp') or {}, parked)
            if what:
                u = {k: v for k, v in parked.items() if k not in ('fp', 'boards', 'old_priority', 'at')}
                u['priority'] = parked.get('old_priority', 50)
                ok, said = w.add(u)
                if ok:
                    rec.update(parked=None, fails=[], fp={})
                    out.append(f'attempts: {uid} is back on the queue: {what}' + (f' (its brief {rec["brief"]} stays: his B on it takes it off again)' if rec.get('brief') else ''))
                else:
                    out.append(f'attempts: {uid} could not go back on the queue: {said[:160]}')
            continue
        if len(rec.get('fails') or []) < TRIES or not on:
            continue
        u = next(iter(on.values()))
        if u.get('lane') in running or not all(Path(b) in idle for b in on):
            continue                                        # a leg works on it, or a board it is on is in a run: next round
        if changed_since(w, rec.get('fp') or {}, u):
            continue                                        # one more try is fair: something changed since the last
        if not w.acting:
            out.append(f'attempts: {uid} would be parked: {len(rec["fails"])} tries with nothing changed between them')
            continue
        if not park(w, uid, rec, on, out):
            continue
        rec['next'] = note_next(w, w.main, uid) or next((note_next(w, b, uid) for b in on if note_next(w, b, uid)), '')
        try:
            rec['brief'] = w.park_brief(uid, rec, rec['next'], {k: v for k, v in u.items() if k != 'priority'})
        except Exception as e:      # noqa: BLE001  the unit stays parked; the brief is tried next round
            rec['brief'] = ''
            out.append(f'attempts: {uid} parked, its brief was refused: {str(e)[:160]}')
        out.append(f'attempts: {uid} parked after {len(rec["fails"])} tries with no progress ({", ".join(rec["fails"][-3:])}); brief {rec.get("brief") or "not written"}')
    out += superseded(w, st, opens, idle)
    out += stuck_on_extras(w, st, opens, idle, extras or set())
    if w.acting:
        save(w.home / 'attempts.json', st)
    return out


SUFFIX = re.compile(r'^(.+?)-([b-j])$')


def family(uid):
    """(the id a copy was made of, its rank): x-b is a later copy of x, x-c of x-b."""
    m = SUFFIX.match(uid)
    return (m.group(1), ' bcdefghij'.index(m.group(2))) if m else (uid, 0)


def superseded(w, st, opens, idle):
    """When a later copy of a unit (x-b, x-c) is open, the earlier one is parked for good, with no brief: two relays
    never work one piece of work (relay 3 ran relay-proof-to-drive while relay 6 had relay-proof-to-drive-b)."""
    out, best = [], {}
    ids = {uid for us in opens.values() for uid in us}
    for uid in ids:
        f, r = family(uid)
        best[f] = max(best.get(f, -1), r)
    for uid in sorted(ids):
        f, r = family(uid)
        if r >= best[f]:
            continue
        rec = st.setdefault(uid, dict(seen=[], fails=[]))
        on = {b: us[uid] for b, us in opens.items() if uid in us and (Path(b) / 'relay' / 'queue' / f'{uid}.json').exists()}
        if rec.get('parked') or not on:
            continue
        if not w.acting:
            out.append(f'attempts: {uid} would be parked: a later copy of it is queued')
            continue
        ready = {b: u for b, u in on.items() if Path(b) in idle}         # off the idle boards now, the others when idle
        if ready and len(ready) < len(on):
            for b in ready:
                if w.unqueue(b, uid):
                    out.append(f'attempts: {uid} taken off {Path(b).name}: its later copy is queued')
        elif ready and park(w, uid, rec, on, out, dropped=True):
            out.append(f'attempts: {uid} parked for good: its later copy is queued')
    return out


def stuck_on_extras(w, st, opens, idle, extras):
    """An idle extra relay whose queue holds a unit that ended FAIL or BLOCKED with nothing changed since does not
    count that unit as work: it goes off that board (the main board keeps it at 99, where its second try or its park
    is decided), so the relay is fed other work instead of standing still (relay 2 held only relay-four-checks)."""
    out = []
    for b in sorted(extras, key=str):
        if Path(b) not in idle:
            continue
        for uid, u in sorted((opens.get(b) or opens.get(Path(b)) or {}).items()):
            rec = st.get(uid) or {}
            if rec.get('parked') or not (Path(b) / 'relay' / 'queue' / f'{uid}.json').exists():
                continue                                    # parked this round already
            if not rec.get('fails') or changed_since(w, rec.get('fp') or {}, u):
                continue
            if not w.acting:
                out.append(f'attempts: {uid} would go off {Path(b).name}: it went nowhere there and nothing changed since')
                continue
            if w.unqueue(b, uid):
                out.append(f'attempts: {uid} taken off {Path(b).name}: it went nowhere there and nothing changed since, so it is not that relay\'s work')
    return out


def answered(w, uid):
    """His A on a parked unit's brief queued it again: its count starts over, and a later park gets a brief of its own."""
    st = load(w.home / 'attempts.json', {})
    if uid in st:
        st[uid].update(parked=None, fails=[], fp={}, brief='', dropped=False, drop=False)
        save(w.home / 'attempts.json', st)


def dropped_by(w, bid):
    """His answer closed brief `bid` with nothing built: when it is a parked unit's brief and the unit came back by
    itself meanwhile, the next round takes it off the queue again."""
    st = load(w.home / 'attempts.json', {})
    for rec in st.values():
        if rec.get('brief') == bid:
            if rec.get('parked'):
                rec['dropped'] = True
            else:
                rec['drop'] = True
            save(w.home / 'attempts.json', st)
            return True
    return False


def park_text(uid, rec, nxt):
    """(title, what it is for, why) of the one brief a parked unit gets."""
    kinds = {}
    for f in rec['fails']:
        k = f.split()[-1]
        kinds[k] = kinds.get(k, 0) + 1
    tried = ', '.join(f'{k} {n}x' for k, n in kinds.items())
    say = (f'The relay tried {uid} {len(rec["fails"])} times ({tried}) with nothing changed between tries, so the loop took it off the queue. '
           f'Last problem: {words(rec.get("why") or "none recorded", 12)} Its leg said next: {words(nxt or "nothing", 14)}')
    return (f'{uid}: tried {len(rec["fails"])} times, no progress', words(say, SAY_MOST - 1),
            'R0 C0. Another try as it stands costs tokens for nothing; it comes back by itself when its lane or files change.')
