#!/usr/bin/env python3
"""The relay's last runs: what each did, by which agent, what it cost, why it ended and what it left for the owner.

    python Tools/assetboard/src_runs.py            from trench-warfare-3d/: the runs as text, as a session reads them
    python Tools/assetboard/src_runs.py RUN        one run, every leg with its report

WHY (the owner, 2026-10-09: "right now the visibility on what a relay run does is low. i want to increase this
visibility, we make a new page for the control room/index. here we show the last 20 relay runs with their attached
decions to be made and a report on what has been done and by which agent"). A run was a line in a log on the desktop
and one commit message on the pipeline board; what its legs wrote was read by whoever went looking.

WHAT IS READ. The relay keeps its records on the pipeline board (Tools/relay/boardio.py): a stop record a run
(relay/<station>/stops/<run>.json), a record a leg with the leg's whole report (relay/<station>/legs/<run>-<nn>.json),
the unit a leg worked on (relay/queue, relay/done), a retrospective's proposals (relay/proposals), and, once the relay
writes one, a record of the run that is going (relay/<station>/live/<run>.json). All of it is read off origin/main
with git, as tasks.py reads the queue: a board whose checkout is behind is neither needed nor touched. A run that is
going shows only what the relay has pushed of it.

WHAT IS WORKED OUT, where a record does not say it. A leg's verdict is its `said`, else the first line of its report
that starts with RESULT (the relay's own rule, Tools/relay/launch.py said()). What it asks of the owner is its
`needs_you`, else its report's NEEDS YOU part. A run began at its stop record's `started_at`, else when its first leg
did, else at the time in its name (the desktop's clock, taken as this station's). Its cost is the sum of its legs'.
The agent of a leg is its role, its phase and the model it ran on: the relay has no other name for one.

A RUN'S DECISIONS (briefs_of). A decision brief (briefs.py) is a run's when it says so (`raised_by`: the run reader,
runreport.py, writes those, and a report can name a brief that asked it before); when it is the step of a pipeline job
the run worked ('step'); when the owner's answer to it queued a unit the run worked ('from': his own word, come back
as work); and, marked as a guess, when it is about a lane the run worked and was asked within two days after ('lane').
What a leg asked that no brief holds yet is listed as the leg wrote it (`asks`).

THE LAST TWENTY are the last twenty runs that ran a leg. A start that failed before its first leg (the work checkout
could not be used) is listed between them, short, and not counted.
"""
import datetime
import hashlib
import json
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import tasks        # noqa: E402

MOST = 20               # runs that ran a leg
MOST_ROWS = 60          # rows in all, the starts that failed between them counted
REPORT = 6000           # characters of a leg's report the page and the run reader are given (a report is 120 words; three times that is one that ran on)
GOAL = 400              # ... of a unit's goal
NEAR = 2 * 86400        # a brief about a lane is guessed to be a run's when asked this long after it, at most
LIVE_OLD = 6 * 3600     # a run that is going and has not been heard from for this long is said to be silent
VERDICT = re.compile(r'^\W*RESULT\W+(done|blocked|failed)\b', re.I | re.M)        # Tools/relay/launch.py VERDICT
# a part's head: the word in capitals with its colon, markdown around it or not. "- Next to the trench ..." is a bullet, not a part
PART = re.compile(r'^[^\w\n]*(RESULT|NEEDS YOU|CHANGED|NEXT|NOTES?)\**\s*:\**\s*')
BULLET = re.compile(r'^\s*[-*\u2022]\s+')
NOTHING = re.compile(r'^\W*(?:(?:nothing|none|n/?a)(?:\s+(?:is\s+)?needed)?\s*(?:[.,;:(—-].*)?|no\W*)$', re.I | re.S)       # "nothing. One unread message for this machine": nothing
SHA = re.compile(r'\b(?=[0-9a-f]*\d)(?=[0-9a-f]*[a-f])[0-9a-f]{7,12}\b')
PROOF = 'proof-'        # the relay's own proofs are runs by name only
# why a run ended, as one word (the relay's `reason_kind`, once it writes one): the start of its reason, the longest first
KINDS = (('nothing left to do', 'done'), ("the run's", 'hours'), ('the leg cap', 'legs'), ('stopped by the owner', 'asked'), ('stopped on request', 'asked'), ("the day's budget", 'budget'), ("the day's pace", 'pace'),
         ('the work checkout', 'checkout'), ('error:', 'error'), ('leg ', 'leg'))
TAILS = (('hours are up', 'hours'), ('left uncommitted work', 'uncommitted'), ('brought no result', 'no-result'), ('cannot be switched to', 'lane'))
CACHE = {}              # the board's records as of one commit: read again only when origin/main moved


def blobs(board, names, ref=tasks.REF):
    """The files `names` of a commit of the board, as bytes: one git call for all of them."""
    out = tasks.git(board, 'cat-file', '--batch', send=''.join(f'{ref}:{n}\n' for n in names).encode('utf-8'))
    got, at = {}, 0
    for n in names:                                 # each answer: "<sha> blob <size>\n", the bytes, "\n"; or "<name> missing\n"
        end = out.find(b'\n', at)
        head = out[at:end].split()
        if end >= 0 and head[-1:] == [b'missing']:
            at = end + 1
            continue
        if end < 0 or len(head) != 3 or head[1] != b'blob':
            break
        size = int(head[2])
        got[n] = out[end + 1:end + 1 + size]
        at = end + 1 + size + 1
    return got


def tree(board, ref=tasks.REF):
    """Everything the relay wrote on the board, as origin/main has it: (the commit, when it was made, {path: record}).
    A proposals file is its text; the rest are the records as they were written."""
    if not board or not Path(board).is_dir():
        return '', '', {}
    sha = tasks.git(board, 'rev-parse', '--verify', '--quiet', ref).decode().strip()
    if not sha:
        return '', '', {}
    if CACHE.get('sha') == sha and CACHE.get('board') == str(board):
        return sha, CACHE['as_of'], CACHE['files']
    # by the commit, not by the name: a fetch between the two calls would list one commit's files and read another's
    names = [n for n in tasks.git(board, 'ls-tree', '-r', '--name-only', sha, 'relay').decode('utf-8', 'replace').splitlines() if n.endswith('.json') or (n.startswith('relay/proposals/') and n.endswith('.md'))]
    read = blobs(board, names, sha)
    if len(read) != len(names) or (not names and not tasks.git(board, 'ls-tree', '--name-only', sha)):
        # a git call that failed gives nothing, which reads as "no runs": said, and never kept as this commit's records
        raise RuntimeError(f'the board\'s records did not all read ({len(read)} of {len(names)} files of {sha[:8]})')
    files = {}
    for n, raw in read.items():
        text = raw.decode('utf-8', 'replace')
        if n.endswith('.md'):
            files[n] = text
            continue
        try:
            d = json.loads(text)
        except ValueError:
            continue
        if isinstance(d, dict):
            files[n] = d
    as_of = tasks.git(board, 'log', '-1', '--format=%ct', ref).decode().strip()
    CACHE.update(sha=sha, board=str(board), as_of=as_of, files=files)
    return sha, as_of, files


def stamp(s):
    """A time the relay wrote (UTC, 2026-10-09T18:36:50Z), in seconds; 0 when it is none."""
    try:
        return int(datetime.datetime.strptime(str(s)[:19], '%Y-%m-%dT%H:%M:%S').replace(tzinfo=datetime.timezone.utc).timestamp())
    except ValueError:
        return 0


def named(run):
    """When a run began by its own name (20261009-185051-11300: the desktop's clock, taken as this station's); 0 when
    its name is not a time."""
    try:
        return int(time.mktime(time.strptime(str(run)[:15], '%Y%m%d-%H%M%S')))
    except ValueError:
        return 0


def said(report):
    """done | blocked | failed, from the first line of a report that starts with RESULT; else ''."""
    m = VERDICT.search(report or '')
    return m.group(1).lower() if m else ''


def parts(report):
    """A leg's report by its parts: {result, needs, changed [bullets], next}. A part runs to the next one; the words
    before the first are nobody's. RESULT is given without its verdict word."""
    got, at = {}, None
    for line in str(report or '').splitlines():
        m = PART.match(line)
        if m and (m.group(1) != 'RESULT' or 'RESULT' not in got):
            at = m.group(1)
            got.setdefault(at, []).append(line[m.end():])
        elif at:
            got[at].append(line)
    def text(k):
        return re.sub(r'\s+', ' ', ' '.join(got.get(k, []))).strip()
    bullets, cur = [], None
    for line in got.get('CHANGED', []):
        if BULLET.match(line):
            cur = [BULLET.sub('', line)]
            bullets.append(cur)
        elif line.strip() and cur is not None:
            cur.append(line.strip())
        else:
            cur = None                              # a blank line ends a bullet: the paragraph after the bullets is not one of them
    needs = text('NEEDS YOU')
    return dict(result=re.sub(r'^(done|blocked|failed)\b\W*', '', text('RESULT'), flags=re.I), needs='' if NOTHING.match(needs) else needs,
                changed=[re.sub(r'\s+', ' ', ' '.join(b)).strip() for b in bullets], next=text('NEXT'))


def kind_of(reason):
    """Why a run ended, as one word, from the words of its reason (the relay's own words, Tools/relay/runner.py)."""
    r = str(reason or '').strip()
    for tail, k in TAILS:
        if tail in r:
            return k
    for head, k in KINDS:
        if r.startswith(head):
            return k
    return 'other' if r else ''


def sig(run, stop, legs):
    """A run as it is now, in twelve characters: a report was written for one of these, and a run that changed since
    (a leg record that came in late) is read again."""
    return hashlib.sha1(f'{run}|{(stop or {}).get("stopped_at", "")}|{len(legs)}'.encode('utf-8')).hexdigest()[:12]


def leg_row(d):
    """One leg, as the page is given it."""
    report = str(d.get('report') or '')
    p = parts(report)
    if not p['result'] and said(report):               # "RESULT done" with no colon: the verdict's own line, without the verdict
        p['result'] = re.sub(r'\s+', ' ', VERDICT.sub('', next(l for l in report.splitlines() if VERDICT.match(l)), count=1)).strip(' ,-:*\u2014')
    needs = d.get('needs_you') if isinstance(d.get('needs_you'), str) else p['needs']
    commits = [dict(sha=str(c.get('sha', ''))[:10], subject=str(c.get('subject', ''))[:120]) for c in d.get('commits') or [] if isinstance(c, dict) and c.get('sha')]
    return dict(n=int(d.get('leg') or 0), phase=str(d.get('phase') or ''), role=str(d.get('role') or ''), model=str(d.get('ran_model') or d.get('model') or ''), effort=str(d.get('effort') or ''),
                state=str(d.get('state') or ''), said=str(d.get('said') or '') or said(report), started=stamp(d.get('started_at')), seconds=int(d.get('seconds') or 0),
                usd=round(float(d['cost_usd']), 2) if isinstance(d.get('cost_usd'), (int, float)) else None, resumed=int(d.get('resumed') or 0),
                result=p['result'], changed=p['changed'], needs=re.sub(r'\s+', ' ', needs).strip(), next=p['next'],
                commits=commits, more=int(d.get('commits_more') or 0), shas=[] if commits else sorted(set(SHA.findall(report)))[:8], report=report[:REPORT], cut=len(report) > REPORT)


def collect(files, now=None, most=MOST):
    """The runs, the newest first, from the board's records ({path: record}, tree()). Each: {id, station, state
    (going | ended | open: legs and no stop record), started, ended, by, code, reason, kind, asked_why, hours, legs,
    usd, unpriced, refusals, day_pct, day_budget_pct, now_on, units [...], proposals [...], asks [...], sig, empty}."""
    now = now or time.time()
    stops, legs, live, queue, done, props = {}, {}, {}, {}, {}, {}
    for n, d in files.items():
        part = n.split('/')
        if part[:2] == ['relay', 'proposals']:
            m = re.match(r'(.+)-(\d+)\.md$', part[-1])
            if m:
                props.setdefault(m.group(1), []).append(dict(leg=int(m.group(2)), text=str(d).strip()[:4000]))
        elif not isinstance(d, dict):
            continue
        elif part[1] == 'queue' and d.get('id'):
            queue[d['id']] = d
        elif part[1] == 'done':
            done[d.get('id') or Path(n).stem] = d
        elif len(part) == 4 and part[2] in ('stops', 'legs', 'live'):
            run = str(d.get('run') or (Path(n).stem if part[2] != 'legs' else Path(n).stem.rsplit('-', 1)[0]))
            if run.startswith(PROOF):
                continue
            d = dict(d, station=d.get('station') or part[1])
            if part[2] == 'legs':
                legs.setdefault(run, []).append(d)
            else:
                (stops if part[2] == 'stops' else live)[run] = d
    rows = []
    for run in set(stops) | set(legs) | set(live):
        try:
            rows.append(a_run(run, stops, legs, live, queue, done, props))
        except (ValueError, TypeError, AttributeError, KeyError):         # a record that is not one costs its run, not the page
            continue
    rows.sort(key=lambda r: (r['started'], r['id']), reverse=True)
    out, ran = [], 0
    for r in rows:
        if ran >= most or len(out) >= MOST_ROWS:
            break
        out.append(r)
        ran += 0 if r['empty'] else 1
    while out and out[-1]['empty']:                 # the list ends on a run, not on the starts that failed before the one that fell off it
        out.pop()
    return out


def a_run(run, stops, legs, live, queue, done, props):
    """One run from its records (collect())."""
    stop, mine, going = stops.get(run), sorted(legs.get(run, []), key=lambda d: int(d.get('leg') or 0)), live.get(run) if run not in stops else None
    head = stop or going or (mine[0] if mine else {})
    started = stamp(head.get('started_at') if (stop or going) else '') or named(run) or (stamp(mine[0].get('started_at')) if mine else 0)
    verdicts = dict((stop or going or {}).get('units') or {})
    now_on = (stop or {}).get('now_on') or ''
    if going and isinstance(going.get('now_on'), dict):
        now_on = going['now_on'].get('unit') or ''
    units, order = {}, []
    for d in mine:
        u = str(d.get('unit') or '')
        if u not in units:
            order.append(u)
            q = queue.get(u) or {}
            units[u] = dict(id=u, lane=str(d.get('lane') or q.get('lane') or ''), role=str(d.get('role') or ''), source=str(d.get('source') or ''), goal=str(q.get('goal') or '')[:GOAL], legs=[])
        units[u]['legs'].append(leg_row(d))
    for u in verdicts:                          # a unit with a verdict and no leg: its lane could not be switched to
        if u not in units:
            order.append(u)
            q = queue.get(u) or {}
            units[u] = dict(id=u, lane=str(q.get('lane') or ''), role=str(q.get('role') or ''), source='', goal=str(q.get('goal') or '')[:GOAL], legs=[])
    asks = []
    for u in order:
        x = units[u]
        x['verdict'] = str(verdicts.get(u) or '')
        x['why'] = [str(w)[:300] for w in ((stop or {}).get('problems') or {}).get(u) or []][:8]
        x['usd'] = round(sum(l['usd'] or 0 for l in x['legs']), 2)
        x['seconds'] = sum(l['seconds'] for l in x['legs'])
        x['head'] = str((done.get(u) or {}).get('head') or '')[:10] if x['verdict'] == 'PASS' else ''
        for l in x['legs']:
            if l['needs']:
                asks.append(dict(key=f'{l["n"]:02d}', leg=l['n'], unit=u, kind='needs', text=l['needs'][:600]))
        last = x['legs'][-1] if x['legs'] else None
        if (x['verdict'] == 'BLOCKED' or (last and last['said'] == 'blocked')) and not (last and last['needs']):
            asks.append(dict(key=f'{last["n"]:02d}' if last else u, leg=last['n'] if last else 0, unit=u, kind='blocked', text=((last and last['result']) or (x['why'] or ['it ended blocked'])[0])[:600]))
    priced = [float(d['cost_usd']) for d in mine if isinstance(d.get('cost_usd'), (int, float))]
    ended = stamp(stop.get('stopped_at')) if stop else 0
    beat = stamp(going.get('beat')) if going else 0
    return dict(
        id=run, station=str(head.get('station') or ''), state='ended' if stop else 'going' if going else 'open', started=started, ended=ended,
        heard=beat or max([stamp(d.get('finished_at')) for d in mine] or [0]), by=str(head.get('started_by') or ''), code=str(head.get('code') or '')[:8],
        reason=str((stop or {}).get('reason') or ''), detail=str((stop or {}).get('detail') or '')[:300], kind=str((stop or {}).get('reason_kind') or '') or kind_of((stop or {}).get('reason')),
        asked_why=str((stop or {}).get('asked_why') or ''), asked_by=str((stop or {}).get('asked_by') or ''), hours=head.get('hours') or 0,
        legs=len(mine) or int((stop or {}).get('legs') or 0), usd=round(sum(priced), 2), unpriced=len(mine) - len(priced), refusals=int((stop or going or {}).get('refusals') or 0),
        day_pct=(stop or {}).get('day_pct'), day_budget_pct=(stop or going or {}).get('day_budget_pct'), now_on=str(now_on), units=[units[u] for u in order],
        proposals=sorted(props.get(run, []), key=lambda p: p['leg']), asks=asks, sig=sig(run, stop, mine), empty=not mine and not verdicts, briefs=[])


def local(when):
    """A brief's own time (2026-10-09 19:37, this station's clock) in seconds; 0 when it is none."""
    try:
        return int(time.mktime(time.strptime(str(when)[:16], '%Y-%m-%d %H:%M')))
    except ValueError:
        return 0


def brief_row(b, how, shown=()):
    a = b.get('answer') or {}
    return dict(id=b['id'], title=b.get('title', ''), state=b.get('state', 'open'), asked=b.get('asked', ''), what_for=b.get('what_for', ''), how=how, shown=b['id'] in shown, lane=b.get('lane', ''),
                pick=b.get('pick', ''), options=[dict(key=o.get('key', ''), text=o.get('text', '')) for o in b.get('options') or []], pictures=len(b.get('evidence') or []),
                answer=dict(option=a.get('option', ''), said=a.get('said', ''), when=a.get('when', ''), queued=a.get('queued', ''), outcome=a.get('outcome', '')) if a else None,
                unit=str((b.get('raised_by') or {}).get('unit') or ''), leg=int((b.get('raised_by') or {}).get('leg') or 0))


def briefs_of(runs, every, reports=None, shown=()):
    """Give every run the decision briefs that are its own (the head of this file says when one is), each once, on
    the newest run it fits; and mark what a leg asked as held by a brief, or judged not the owner's, where a report
    says so. `every` is briefs.read_all(), `reports` {run: report} (runreport.py), `shown` the ids decide.html lists."""
    reports, by_id, taken = reports or {}, {b['id']: b for b in every}, set()
    for r in runs:
        rep = reports.get(r['id']) or {}
        told = {str(a.get('key')): a for a in rep.get('asks') or []} if rep.get('sig') == r['sig'] else {}
        for a in r['asks']:
            t = told.get(a['key']) or {}
            a['brief'], a['no'] = (t.get('brief') or '') if t.get('brief') in by_id else '', str(t.get('no') or '')
    def give(r, b, how):
        if b['id'] not in taken:
            taken.add(b['id'])
            r['briefs'].append(brief_row(b, how, shown))
    for r in runs:                                  # what says so itself, first: no other run's report, and no guess, takes a brief from the run that raised it
        for b in every:
            if (b.get('raised_by') or {}).get('run') == r['id']:
                give(r, b, 'raised')
    for r in runs:
        for a in r['asks']:
            if a['brief']:
                give(r, by_id[a['brief']], 'raised')
    for r in runs:
        ids = {u['id'] for u in r['units']}
        for b in every:
            step = b.get('step') or {}
            if step.get('job') in ids or (step.get('item') and any(u['source'] == 'pipeline' and u['id'].startswith(str(step['item']) + '--') for u in r['units'])):
                give(r, b, 'step')
    for r in runs:
        ids = {u['id'] for u in r['units']}
        for b in every:
            queued = {(b.get('answer') or {}).get('queued')} | {((o.get('then') or {}).get('unit') or {}).get('id') for o in b.get('options') or []}
            if ids & (queued - {None, ''}):
                give(r, b, 'from')
    for b in every:                                 # the guess: the last run on the brief's lane before it was asked
        at, lane = local(b.get('asked')), b.get('lane')
        if b['id'] in taken or not lane or not at:
            continue
        fits = [r for r in runs if r['started'] and r['started'] <= at <= (r['ended'] or r['heard'] or r['started']) + NEAR and any(u['lane'] == lane for u in r['units'])]
        # of the runs it fits (the newest first), the one where a unit on that lane asked something no brief holds: a
        # brief is written about what a leg asked, and the run that was going when it was written is often a later one
        asking = [r for r in fits if any(not a['brief'] and not a['no'] and any(u['id'] == a['unit'] and u['lane'] == lane for u in r['units']) for a in r['asks'])]
        if fits:
            give((asking or fits)[0], b, 'lane')
    for r in runs:
        r['briefs'].sort(key=lambda b: (b['state'] != 'open', ('raised', 'step', 'lane', 'from').index(b['how']), b['asked']))
        # what waits on him from this run: a brief of its own he has not answered, and what a leg asked that nothing holds yet
        r['waits'] = sum(1 for b in r['briefs'] if b['state'] != 'answered' and b['how'] in ('raised', 'step')) + sum(1 for a in r['asks'] if not a['brief'] and not a['no'])
        r['maybe'] = sum(1 for b in r['briefs'] if b['state'] != 'answered' and b['how'] == 'lane')         # open, and this run's only by a guess
    return runs


def read(board, every=(), reports=None, shown=(), now=None, most=MOST):
    """The runs for this reading: {runs, read_at, as_of (when the board last changed, seconds), commit, most}. A board
    that is not there gives no runs and says so (`board`: False)."""
    now = now or time.time()
    sha, as_of, files = tree(board)
    runs = briefs_of(collect(files, now, most), list(every), reports, shown)
    for r in runs:
        rep = (reports or {}).get(r['id'])
        r['report'] = rep if rep and rep.get('sig') == r['sig'] else None
        r['silent'] = r['state'] != 'ended' and bool(r['heard']) and now - r['heard'] > LIVE_OLD
    return dict(runs=runs, read_at=int(now), as_of=int(as_of or 0), commit=sha[:8], most=most, board=bool(sha))


def site(R, out: Path):
    """Put the runs in the site: data/runs.js."""
    return tasks.put(out / 'data' / 'runs.js', f'window.RUNS = {json.dumps(R, sort_keys=True)};\n')


def failed(out: Path, why, now=None):
    """A reading of the runs failed: the page keeps the runs of the reading before and says they are old, and why."""
    path = out / 'data' / 'runs.js'
    try:
        R = json.loads(path.read_text(encoding='utf-8')[len('window.RUNS = '):].rstrip().rstrip(';'))
    except (OSError, ValueError):
        R = dict(runs=[])
    R['failed'] = dict(at=int(now or time.time()), why=str(why)[:300])
    return tasks.put(path, f'window.RUNS = {json.dumps(R, sort_keys=True)};\n')


def clock(sec):
    return time.strftime('%m-%d %H:%M', time.localtime(sec)) if sec else '?'


def lines(R, only=''):
    """The runs as text, for a session: a line a run, its units under it; with `only`, that run with every leg's report."""
    out = [] if R.get('board') else ['no pipeline board on this station: the runs cannot be read here']
    for r in R['runs']:
        if only and r['id'] != only:
            continue
        if r['empty']:
            out.append(f'{r["id"]}  {clock(r["started"])}  no leg ran: {r["reason"]}')
            continue
        tally = ', '.join(f'{sum(1 for u in r["units"] if u["verdict"] == v)} {v}' for v in ('PASS', 'FAIL', 'BLOCKED') if any(u['verdict'] == v for u in r['units']))
        out.append(f'{r["id"]}  {clock(r["started"])} to {clock(r["ended"]) if r["ended"] else r["state"]}  {r["legs"]} legs, ${r["usd"]:.2f}, {len(r["units"])} units{": " + tally if tally else ""}'
                   + (f'; {r["reason"]}' if r['reason'] else '') + (f'; {r["waits"]} wait on the owner' if r['waits'] else ''))
        for u in r['units']:
            agents = ', '.join(sorted({f'{l["role"]} {l["phase"]} on {l["model"]}' for l in u['legs']}))
            out.append(f'    {u["verdict"] or "open":8}{u["id"]}  ({u["lane"]}; {len(u["legs"])} legs, ${u["usd"]:.2f}; {agents})')
            for l in u['legs'] if only else []:
                out.append(f'        leg {l["n"]:02d} {l["phase"]}, {l["seconds"] // 60} min: {l["said"] or "no verdict"} - {l["result"]}')
                out.extend(f'            {w}' for w in l['report'].splitlines())
        for a in r['asks']:
            out.append(f'    asks (leg {a["leg"]:02d}): {a["text"]}' + (f'  [brief {a["brief"]}]' if a['brief'] else f'  [not his: {a["no"]}]' if a['no'] else ''))
        for b in r['briefs']:
            out.append(f'    brief ({b["how"]}, {b["state"]}): {b["id"]}')
    return out


def main(argv=None):
    import briefs
    import ops
    import runreport
    argv = sys.argv[1:] if argv is None else argv
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(errors='replace')
    every = briefs.read_all(briefs.folder())
    R = read(ops.board_root(), every, runreport.read_all(runreport.folder()), [b['id'] for b in briefs.shown(every)])
    print('\n'.join(lines(R, argv[0] if argv else '')))
    return 0


if __name__ == '__main__':
    sys.exit(main())
