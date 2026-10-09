#!/usr/bin/env python3
"""What a relay run did, in plain words, and what it leaves for the owner: read by an agent when the run has ended.

    python Tools/assetboard/runreport.py                   from trench-warfare-3d/: the reports, the day's readings, what waits
    python Tools/assetboard/runreport.py context RUN       what the records say about one run (read this first)
    python Tools/assetboard/runreport.py brief RUN KEY --title .. --for .. --option .. --option .. --why ..
                                                            a decision the run leaves for him, as a brief on the Decide page
    python Tools/assetboard/runreport.py add RUN --title .. --did .. --left .. --unit ID=.. [--asked KEY=BRIEF] [--not-his KEY=why]
    python Tools/assetboard/runreport.py tick [--only RUN]  what the watcher does on every read, by hand

WHY (the owner, 2026-10-09, on the Runs page: "a report on what has been done and by which agent", with the
"attached decions to be made"; asked how, he picked "Report and briefs": one agent a finished run writes a plain
summary and turns every real question the run left into a brief on the Decide page, stamped with the run). A leg's
own report is 120 words of an engineer's shorthand, and what a leg asked of him was a line nobody had to read: of the
last twenty runs' asks, none had become a brief that names its run.

A REPORT is a folder <reports>/<run>/ with report.json: a title, what was done (`did`), what is left (`left`), a
sentence a unit, and for everything a leg asked of the owner (src_runs.py `asks`) one of three answers: the brief
that now asks it (written in this reading with `brief`, or one that asked it before: --asked), or why it is not his
to decide (--not-his: "one unread message for this machine" is a chore, not a decision). It is made for the run as
it is (src_runs.sig()). The folder is on the Drive both stations read.

THE PAGE DOES NOT WAIT FOR IT. A run is listed from its records at once (src_runs.py); its report comes in when it
is read, and until then the page shows the legs' own words.

THE BOARD STARTS THE AGENT (tick(), as taskbrief.py does for tasks). The watcher starts one headless session for one
run at a time. It runs in its own scratch folder and may write there only; beyond that it reads, and runs this tool.
It starts no agent and searches no web. Read are: a run that ended since the reading was first switched on, the
newest BACK runs from before that (the owner's pick, 2026-10-09: "Last 5 runs"), and any run he asks for on the page
(READ_SAY). The numbers are LIMITS; every reading is a line in spend.jsonl with its dollars and the host that ran it.

WHAT IS CHECKED (check()). A report is refused, with every reason, when it is not short, when it names a file or
quotes code where it should say what the thing is for, when a unit that ran has no sentence, and when something a
leg asked has no answer. Whether the words are TRUE is not checked by anything here: the skill (tw-run-report) holds
the agent to what it read.
"""
import argparse
import datetime
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs       # noqa: E402
import build        # noqa: E402
import src_runs     # noqa: E402
import src_tasks    # noqa: E402
import taskbrief    # noqa: E402
import tasks        # noqa: E402

TITLE = 70              # characters of a report's title
WORDS = dict(did=60, left=45, unit=30, no=25)       # words, at most
CAPTION_WORDS = 14
MOST_PICTURES, MOST_BRIEFS = 4, 3                   # a report shows this many pictures, a run leaves this many briefs, at most
BACK = 5                # runs from before the reading was switched on that are read
TRIES = 2               # readings that may fail on a run before it is left with its records only
CLAIM = 30 * 60         # seconds another station's claim on a run is left alone
READ_SAY = 'Read this run and write its report.'    # what a click on "Have it read" leaves as his note (runs.js READ)
BY = 'the run reader'
RUN_ID = re.compile(r'[A-Za-z0-9][\w.-]*\Z')        # a run's name is a folder's name here: nothing with a slash or dots alone
HIS = re.compile(r'\b(landed|lands|merged)\b', re.I)      # landing is the owner's own act: a unit that passed is on its lane
DRIVE_WAS = build.DRIVE.is_dir()                    # the Drive was there when this process began: when it goes, nothing is read into a folder no page reads
CODE = re.compile(r'`|\b[\w/\\.-]+\.(?:cs|py|js|json|md|ps1|uss|uxml|shader|asset|prefab)\b')
PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp')
PAPERS = ('.md', '.txt', '.json')

words, one, load, put = briefs.words, briefs.one, taskbrief.load, taskbrief.put


def folder():
    """Where the reports are: TW_RUNREPORTS, else run-reports on the Drive both stations read, else this station's own."""
    if os.environ.get('TW_RUNREPORTS'):
        return Path(os.environ['TW_RUNREPORTS'])
    return build.DRIVE / 'run-reports' if build.DRIVE.is_dir() else build.LOCAL / 'run-reports'


def state():
    """This station's own: the reading that is going and the readings' scratch folders. Never on the Drive."""
    if os.environ.get('TW_RUNREPORTS'):
        return Path(os.environ['TW_RUNREPORTS']) / '_station'
    return build.LOCAL / 'runreport'


def read_all(where: Path):
    """Every report there is: {run: report}."""
    out = {}
    for f in sorted(where.glob('*/report.json')) if where.is_dir() else []:
        d = load(f)
        if d and d.get('run') == f.parent.name:
            out[d['run']] = d
    return out


def current(where: Path, r):
    """The report of a run as it is now, or None."""
    d = load(where / r['id'] / 'report.json')
    return d if d and d.get('sig') == r['sig'] else None


def waits(where: Path, r, now=None):
    """What is written of a run that waits to be read: since when, and the readings that failed on it."""
    w = load(where / r['id'] / 'wait.json')
    if not w or w.get('sig') != r['sig']:
        w = dict(sig=r['sig'], since=int(now or time.time()), n=0, why='')
        try:
            put(where / r['id'] / 'wait.json', w)
        except OSError:
            pass
    return w


def since(where: Path, now=None):
    """When the reading was first switched on here: runs that ended after it are all read. Written on first ask."""
    d = load(where / 'since.json')
    if not d:
        d = dict(since=int(now or time.time()), host=socket.gethostname())
        try:
            put(where / 'since.json', d)
        except OSError:
            pass
    return int(d.get('since') or 0)


# ---- what the records say about a run (context) ----------------------------------------------------------------------

def given(path=''):
    """The run a reading was given, whole, with the board it was read from: {run, board}. Outside a reading: read now."""
    path = path or os.environ.get('TW_RUNREPORT_GIVEN', '')
    if path:
        return load(Path(path)) or {}
    return {}


def run_of(rid, path=''):
    """(the run, the board): what a reading was given, else the run as the board has it now."""
    if not RUN_ID.match(str(rid)) or '..' in str(rid):
        raise ValueError(f'{rid!r} is not the name of a run')
    if path and os.environ.get('TW_RUNREPORT_READING'):
        raise ValueError('--given is not for a reading the board started: it reads the run it was handed')
    g = given(path)
    if g:
        if g['run']['id'] != rid:
            raise ValueError(f'{rid} is not the run this reading was given: {g["run"]["id"]}')
        return g['run'], g.get('board') or ''
    import ops
    board = ops.board_root()
    every = briefs.read_all(briefs.folder())
    R = src_runs.read(board, every, read_all(folder()), [b['id'] for b in briefs.shown(every)], most=10 ** 6)
    hits = [r for r in R['runs'] if r['id'] == rid]
    if not hits:
        raise ValueError(f'{rid} is not a run on the board')
    return hits[0], str(board or '')


def evidence(board, unit, scratch: Path):
    """What a pipeline unit left on the board as evidence, copied into the reading's folder so it can be looked at
    and shown: ([pictures], [papers]). A lane unit leaves none."""
    item, _, rest = str(unit).partition('--')
    stage = rest.split('--')[0]
    if not board or not stage:
        return [], []
    names = [n for n in tasks.git(board, 'ls-tree', '-r', '--name-only', tasks.REF, f'evidence/{item}/{stage}').decode('utf-8', 'replace').splitlines()
             if Path(n).suffix.lower() in PICTURES + PAPERS][:24]
    shots, papers = [], []
    for n, raw in src_runs.blobs(board, names).items():
        dst = scratch / n
        try:
            dst.parent.mkdir(parents=True, exist_ok=True)
            dst.write_bytes(raw)
        except OSError:
            continue
        (shots if dst.suffix.lower() in PICTURES else papers).append(dst.as_posix())
    return shots, papers


def asked_on_lane(lane):
    """What a blocked unit wrote for the owner on its own lane: the lines its lane adds to the decisions file."""
    if not lane or not taskbrief.lane_facts(lane)[0]:
        return []
    diff = taskbrief.git('diff', '--unified=0', f'{src_tasks.INTEGRATION}...origin/{lane}', '--', 'docs/reference/decisions.md')
    return [l[1:].rstrip() for l in diff.splitlines() if l.startswith('+') and not l.startswith('+++') and l[1:].strip()][:60]


def context(r, board='', scratch=None, every=()):
    """Everything the records say about a run, as lines for the agent that writes its report. `every` is
    briefs.read_all(): the open briefs about a lane of the run are listed whichever run they are tied to, because a
    brief that names no run can sit on the wrong one (the wreck question, 2026-10-10, was asked twice for that)."""
    scratch = Path(scratch) if scratch else state() / 'context' / r['id']
    files = src_runs.tree(board)[2] if board else {}
    queue = {d['id']: d for n, d in files.items() if n.startswith('relay/queue/') and isinstance(d, dict) and d.get('id')}
    when = lambda s: time.strftime('%Y-%m-%d %H:%M', time.localtime(s)) if s else 'unknown'      # noqa: E731
    out = [f'RUN {r["id"]} on {r["station"] or "?"}: began {when(r["started"])}, ' + (f'ended {when(r["ended"])}' if r['ended'] else f'has no stop record ({r["state"]})') + f', started by {r["by"] or "?"}',
           f'  {r["legs"]} legs, ${r["usd"]:.2f}' + (f' ({r["unpriced"]} legs carry no cost)' if r['unpriced'] else '') + f', {r["refusals"]} refusals by the guard',
           f'  why it ended: {r["reason"] or "not recorded"}' + (f' ({r["asked_why"]})' if r['asked_why'] else '') + (f'; {r["detail"]}' if r['detail'] else ''),
           '  NOTE: "stopped by the owner (relay.py stop)" is also what a watcher\'s stop reads as: do not say he stopped it unless a record says why.' if r['kind'] == 'asked' and not r['asked_why'] else None]
    for u in r['units']:
        q = queue.get(u['id']) or {}
        on, ahead = taskbrief.lane_facts(u['lane']) if u['lane'] else (False, 0)
        out += ['', f'UNIT {u["id"]}',
                f'  verdict of its check: {u["verdict"] or "NONE (the run ended before it had one, or it is not a unit with a check)"}; role {u["role"] or "?"}; {len(u["legs"])} legs, {u["seconds"] // 60} min, ${u["usd"]:.2f}',
                f'  lane: {u["lane"] or "none"}' + (f' ({"on origin, " + (str(ahead) + " commits not landed" if ahead else "nothing on it that is not landed") if on else "NOT on origin as this station last fetched it"})' if u['lane'] else '')]
        if q.get('goal'):
            out.append(f'  what it was asked: {one(q["goal"])[:1500]}')
        for w in q.get('done_when') or []:
            out.append(f'  done when: {one(w)[:300]}')
        for w in u['why']:
            out.append(f'  the runner said: {w}')
        if u['head']:
            out.append(f'  its lane stood at {u["head"]} when it passed')
        for l in u['legs']:
            out.append(f'  LEG {l["n"]:02d} {l["phase"]}, by the {l["role"]} role on {l["model"]}{" " + l["effort"] if l["effort"] else ""}, {l["seconds"] // 60} min, ' + (f'${l["usd"]:.2f}' if l['usd'] is not None else 'cost unknown')
                       + f': it {"said " + l["said"] if l["said"] else "gave NO verdict line"}' + (f' (the session ended {l["state"]})' if l['state'] not in ('DONE', '') else ''))
            out += ['      | ' + w for w in l['report'].splitlines()] + (['      | (cut: the record holds more)'] if l['cut'] else [])
            for c in l['commits']:
                out.append(f'      commit {c["sha"]}  {c["subject"]}')
            for sha, subject in taskbrief.commits_of(l['shas']) if l['shas'] else []:
                out.append(f'      names commit {sha}  {subject}')
        if u['verdict'] == 'BLOCKED':
            lines = asked_on_lane(u['lane'])
            out += ['  what its lane adds to the decisions file (a blocked leg writes its question there):'] + ['      + ' + w for w in lines] if lines else ['  its lane adds nothing to the decisions file']
        shots, papers = evidence(board, u['id'], scratch) if u['source'] == 'pipeline' else ([], [])
        for p in shots:
            out.append(f'  picture (may be shown): {p}')
        for p in papers:
            out.append(f'  paper (read it): {p}')
    out += ['', 'ASKS: what a leg asked of the owner. Answer every one in `add`: a brief you write (`brief RUN KEY ...`), --asked KEY=BRIEF for a brief that asks it already, or --not-his KEY=why.'] if r['asks'] else ['', 'ASKS: no leg asked anything of the owner.']
    for a in r['asks']:
        out.append(f'  {a["key"]}  ({"the unit ended BLOCKED" if a["kind"] == "blocked" else "NEEDS YOU"}, unit {a["unit"]}): {a["text"]}')
    if r['briefs']:
        out += ['', 'BRIEFS already tied to this run (do not ask twice; `lane` is a guess from the lane and the time):']
    for b in r['briefs']:
        a = b['answer']
        out.append(f'  {b["id"]}  ({b["how"]}; {"answered " + a["option"] + (" " + a["said"] if a["said"] else "") if a else "OPEN"}): {b["title"]}. {b["what_for"]}')
    lanes, tied = {u['lane'] for u in r['units'] if u['lane']}, {b['id'] for b in r['briefs']}
    near = [b for b in every if b.get('state') != 'answered' and b['id'] not in tied and b.get('lane') in lanes and src_runs.local(b.get('asked')) >= r['started']]
    if near:
        out += ['', 'OPEN BRIEFS ON THIS RUN\'S LANES, asked since it began (if one asks what a leg asked, answer that ask with --asked KEY=ID; never write it again):']
    for b in near:
        out.append(f'  {b["id"]}  (asked {b.get("asked", "")}, {b.get("lane", "")}): {b["title"]}. {b.get("what_for", "")}  Options: ' + ' / '.join(o.get('text', '') for o in b.get('options') or []))
    for p in r['proposals']:
        out += ['', f'PROPOSALS of the retrospective, leg {p["leg"]:02d} (changes to the relay itself, for the master; not the owner\'s unless one is a decision):'] + ['  ' + w for w in p['text'].splitlines()]
    return [l for l in out if l is not None]


# ---- a report is written (check, add, brief) -------------------------------------------------------------------------

def pairs(items):
    return [(str(x).split('=', 1) + [''])[:2] for x in items]


def raised(r, every):
    """The briefs that say they come from this run: {ask key or '': [brief ids]}, by the leg they name."""
    out = {}
    for b in every:
        rb = b.get('raised_by') or {}
        if rb.get('run') == r['id']:
            out.setdefault(str(rb.get('key') or (f'{int(rb.get("leg") or 0):02d}' if rb.get('leg') else '')), []).append(b['id'])
    return out


def check(r, title, did, left, units, asked, not_his, pictures, every, scratch=None, still_open=()):
    """Why this is not a report yet: every reason, as a list (empty when it is one), and how each ask is answered
    [{key, brief | no}]. `units` is [(id, sentence)], `asked` [(key, brief id)], `not_his` [(key, why)], `pictures`
    [(path, caption)], `every` briefs.read_all()."""
    bad, title, texts = [], one(title), dict(did=one(did), left=one(left))
    if not title or len(title) > TITLE:
        bad.append(f'the title is {len(title)} characters; 1 to {TITLE}')
    for key, text in texts.items():
        if not text:
            bad.append(f'--{key} is empty' + (' (say "Nothing: ..." when nothing is left)' if key == 'left' else ''))
        elif words(text) > WORDS[key]:
            bad.append(f'--{key} takes {words(text)} words; {WORDS[key]} at most')
    ran = [u['id'] for u in r['units'] if u['legs']]
    said = {}
    for uid, text in units:
        if uid not in ran:
            bad.append(f'--unit {uid}: no unit of this run that ran a leg is called that (they are: {", ".join(ran)})')
        elif not one(text) or words(text) > WORDS['unit']:
            bad.append(f'--unit {uid} needs a sentence of at most {WORDS["unit"]} words')
        else:
            said[uid] = one(text)
    bad += [f'the unit {u} has no sentence: --unit "{u}=what was done on it, in plain words"' for u in ran if u not in said and not any(x == u for x, _ in units)]
    for name, text in [('the title', title), ('--did', texts['did']), ('--left', texts['left'])] + [(f'--unit {u}', t) for u, t in said.items()]:
        m = CODE.search(text)
        if m:
            bad.append(f'{name} names a file or quotes code ({m.group(0)!r}): say what the thing is for in his words; the legs\' own reports are a click away for the names')
        m = HIS.search(text)
        if m:
            bad.append(f'{name} says "{m.group(0)}": say passed, or pushed to its lane. Landing is his own act, and a leg that writes "landed" means a commit on its lane')
    keys, ids, mine, answers = {a['key']: a for a in r['asks']}, {b['id'] for b in every}, raised(r, every), {}
    for key, bid in asked:
        if key not in keys:
            bad.append(f'--asked {key}: this run has no ask {key} (it has: {", ".join(keys) or "none"})')
        elif bid not in ids:
            bad.append(f'--asked {key}={bid}: there is no brief called that')
        else:
            answers[key] = dict(key=key, brief=bid)
    for key, why in not_his:
        if key not in keys:
            bad.append(f'--not-his {key}: this run has no ask {key} (it has: {", ".join(keys) or "none"})')
        elif not one(why) or words(why) > WORDS['no']:
            bad.append(f'--not-his {key} needs its reason in at most {WORDS["no"]} words')
        elif key in answers:
            bad.append(f'the ask {key} is answered twice')
        else:
            answers[key] = dict(key=key, no=one(why))
    for key, why in still_open:
        if key not in keys:
            bad.append(f'--still-open {key}: this run has no ask {key} (it has: {", ".join(keys) or "none"})')
        elif not one(why) or words(why) > WORDS['no']:
            bad.append(f'--still-open {key} needs its reason in at most {WORDS["no"]} words')
        elif key in answers:
            bad.append(f'the ask {key} is answered twice')
        else:
            answers[key] = dict(key=key, open=one(why))         # his to decide, and no brief: the page keeps it as what the leg asked
    for key in keys:
        if key not in answers and mine.get(key):
            answers[key] = dict(key=key, brief=mine[key][0])
    bad += [f'the ask {k} ("{keys[k]["text"][:80]}") has no answer: write its brief (`brief {r["id"]} {k} ...`), or --asked {k}=BRIEF, or --not-his {k}=why, or --still-open {k}=why it is his and has no brief' for k in keys if k not in answers]
    if len(pictures) > MOST_PICTURES:
        bad.append(f'{len(pictures)} pictures; {MOST_PICTURES} at most')
    for path, caption in pictures[:MOST_PICTURES]:
        f = Path(path)
        if not (f.is_file() and f.suffix.lower() in PICTURES):
            bad.append(f'{path} is not a picture that is there')
        elif not scratch or Path(scratch).resolve() not in f.resolve().parents:
            bad.append(f'{f.name} is not in this reading\'s folder: a report shows what `context` put there (a unit\'s evidence), nothing from elsewhere')
        if not one(caption) or words(caption) > CAPTION_WORDS:
            bad.append(f'the picture {f.name} needs a caption of at most {CAPTION_WORDS} words that says what it shows')
    return bad, [answers[k] for k in keys if k in answers], said


def add(where: Path, r, title, did, left, units=(), asked=(), not_his=(), pictures=(), every=(), scratch=None, reading='', now=None, still_open=()):
    """Write the report of a run and return it. A ValueError says every reason it is not one."""
    units, asked, not_his, pictures = list(units), list(asked), list(not_his), list(pictures)
    if not RUN_ID.match(str(r['id'])) or '..' in str(r['id']):
        raise ValueError(f'{r["id"]!r} is not the name of a run')
    bad, answers, said = check(r, title, did, left, units, asked, not_his, pictures, list(every), scratch, list(still_open))
    if bad:
        raise ValueError('not a report yet: ' + '; '.join(bad))
    now = now or datetime.datetime.now()
    home = where / r['id']
    home.mkdir(parents=True, exist_ok=True)
    for f in home.iterdir():                        # the report before this one: its pictures go with it
        if f.name not in ('wait.json', 'claim.json') and f.is_file():
            f.unlink()
    shown = []
    for i, (path, caption) in enumerate(pictures, 1):
        src = Path(path)
        dst = briefs.keep(src, home / f'{i}-{briefs.slug(src.stem)[:40] or "picture"}{src.suffix.lower()}')
        shown.append(dict(file=dst.name, caption=one(caption)))
    d = dict(run=r['id'], sig=r['sig'], when=f'{now:%Y-%m-%d %H:%M}', host=socket.gethostname(), reading=reading, title=one(title), did=one(did), left=one(left), units=said, asks=answers, pictures=shown)
    put(home / 'report.json', d)
    return d


def brief(r, key, title, what_for, options, why, shows=(), no_evidence='', where: Path = None, scratch=None, now=None):
    """Put what a leg asked to the owner as a decision brief, stamped with the run, the unit and the leg, so the Runs
    page lists it under this run and the Decide page asks it. Returns the brief. Refused when the run has no such
    ask, when that ask has its brief already, and past MOST_BRIEFS a run: a run that leaves him more than that many
    decisions is a run to tell the master about, not three more cards."""
    where = where or briefs.folder()
    asks = {a['key']: a for a in r['asks']}
    if key not in asks:
        raise ValueError(f'the run {r["id"]} has no ask {key} (it has: {", ".join(asks) or "none"}): a brief is written for what a leg asked, and `context` lists those')
    mine = raised(r, briefs.read_all(where))
    if mine.get(key):
        raise ValueError(f'the ask {key} has its brief already: {mine[key][0]}')
    if sum(len(v) for v in mine.values()) >= MOST_BRIEFS:
        raise ValueError(f'this run has left {MOST_BRIEFS} briefs already, the most a run leaves: answer the rest with --asked, --not-his or, when it is his and has no brief, --still-open, and say in --left that more is open')
    for path, _ in shows:
        if scratch and Path(scratch).resolve() not in Path(path).resolve().parents:
            raise ValueError(f'{Path(path).name} is not in this reading\'s folder: a brief shows what `context` put there, nothing from elsewhere')
    a = asks[key]
    lane = ([u['lane'] for u in r['units'] if u['id'] == a['unit']] or [''])[0]
    return briefs.add(where, title, what_for, list(options), why, list(shows), no_evidence, lane=lane, by=f'{BY} (run {r["id"]})', now=now, raised_by=dict(run=r['id'], unit=a['unit'], leg=a['leg'], key=key))


def dress(R, where: Path, out: Path):
    """Put what the reports show in the site (img/run/<run>/), give each run's report the pictures' places, and
    remove what the reports of runs no longer listed showed."""
    listed = set()
    for r in R['runs']:
        rep = r.get('report')
        if not rep:
            continue
        listed.add(r['id'])
        shots = []
        for p in rep.get('pictures') or []:
            src, dst = where / r['id'] / p['file'], out / 'img' / 'run' / r['id'] / p['file']
            try:
                if not dst.exists() or dst.stat().st_size != src.stat().st_size:
                    dst.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(src, dst)
                shots.append(dict(src=dst.relative_to(out).as_posix(), caption=p['caption']))
            except OSError:
                continue
        r['report'] = dict(title=rep['title'], did=rep['did'], left=rep['left'], units=rep.get('units') or {}, when=rep.get('when', ''), shots=shots)
    root = out / 'img' / 'run'
    for d in root.iterdir() if root.is_dir() else []:
        if d.is_dir() and d.name not in listed:
            shutil.rmtree(d, ignore_errors=True)
    return R


# ---- the board starts the agent --------------------------------------------------------------------------------------

LIMITS = dict(runs_per_day=8, usd_per_run=2.0, minutes=20, model='sonnet')          # runreport.json beside this file overrules them
RUNS = {}               # the reading this process started: {'p': Popen, 'reading': name}
TOOL = Path(__file__).resolve().as_posix()
SKILL = (build.REPO / '.claude' / 'skills' / 'tw-run-report' / 'SKILL.md').as_posix()
IN_A_READING = ('list', 'context', 'brief', 'add')          # what a reading may ask of this tool: never another reading


def limits():
    got = load(HERE / 'runreport.json') or {}
    return {k: type(v)(got.get(k, v)) for k, v in LIMITS.items()}


def spent(where: Path, day):
    out = []
    try:
        rows = (where / 'spend.jsonl').read_text(encoding='utf-8', errors='replace').splitlines()
    except OSError:
        return out
    for row in rows:                                # a line that does not read (two stations write this file through a synced folder) costs that line, not the day's count
        try:
            r = json.loads(row)
        except ValueError:
            r = dict(when=day, unread=True) if row.strip() else {}
        if str(r.get('when', '')).startswith(day):
            out.append(r)
    return out


def asked_for(every_note):
    """The runs he asked to have read: {run: [his open notes that say so]}."""
    out = {}
    for n in every_note or []:
        if n.get('state') != 'done' and n.get('from', 'owner') == 'owner' and str(n.get('about', '')).startswith('run: ') and n.get('text', '').strip() == READ_SAY:
            out.setdefault(n['about'][len('run: '):], []).append(n['id'])
    return out


def given_up(where: Path, r):
    w = load(where / r['id'] / 'wait.json') or {}
    return f'{w["n"]} readings of it failed' + (f' ({w["why"]})' if w.get('why') else '') if w.get('sig') == r['sig'] and w.get('n', 0) >= TRIES else ''


def wanted(R, where: Path, host, now, every_note=()):
    """The runs to read next: one he asked for first, then the newest. A run is read once it has ended and ran a leg,
    when it ended after the reading was switched on, is one of the newest BACK, or he asked. One that is going, one
    a station claimed lately, and one two readings failed on are left."""
    began, his, out = since(where, now), asked_for(every_note), []
    ran = [r for r in R['runs'] if not r['empty']]
    for i, r in enumerate(ran):
        if r['state'] != 'ended' or current(where, r) or (given_up(where, r) and r['id'] not in his):       # his ask is one more try
            continue
        if not (i < BACK or r['ended'] >= began or r['id'] in his):
            continue
        c = load(where / r['id'] / 'claim.json') or {}
        if c.get('host') not in (None, host) and c.get('sig') == r['sig'] and now - c.get('at', 0) < CLAIM:
            continue
        out.append(r)
    return sorted(out, key=lambda r: (r['id'] not in his, -r['started']))


def prompt_for(rid):
    return (f'You are the run reader of Trench Warfare 3D, started by the board. First read {SKILL} and follow it. '
            f'The relay run {rid} has ended and the owner cannot tell from its records what it did or what it leaves for him. '
            f'In this order: `python {TOOL} context {rid}`, look at what it points to, write a brief for each real decision it leaves him (`python {TOOL} brief {rid} KEY ...`), '
            f'then `python {TOOL} add {rid} ...`. Run the tool with that path, as written (no quotes around it, no cd before it), one command a call. '
            'You are in your scratch folder: write there and nowhere else. Stop when the run has its report. '
            'Nobody answers a question in this session: where you are unsure, say less.')


def command(rid, lim, exe=None):
    """The headless session that reads one run (ideas.py command() says why it is started this way). It may read, and
    run this tool; it starts no agent and searches no web; it is cut off at the reading's money."""
    exe = exe or shutil.which('claude')
    if not exe:
        raise ValueError('no claude on this station\'s path: the runs cannot be read here')
    cmd = [exe, '-p', prompt_for(rid), '--output-format', 'json', '--permission-mode', 'acceptEdits', '--allowedTools', 'Read', 'Grep', 'Glob', f'Bash(python {TOOL} *)', f'Bash(python "{TOOL}" *)',
           '--disallowedTools', 'AskUserQuestion', 'Agent', 'WebSearch', 'WebFetch', '--max-budget-usd', str(lim['usd_per_run'])]
    return cmd + (['--model', lim['model']] if lim['model'] else [])


def start(where: Path, r, board='', now=None, launch=None, lim=None, host=None):
    """Start a reading of one run and write down that it runs. `launch(cmd, cwd, env, out)` starts the process (the
    tests give their own). Returns the reading's record."""
    now, lim, host = now or datetime.datetime.now(), lim or limits(), host or socket.gethostname()
    reading = f'{now:%Y%m%d-%H%M%S}'
    scratch = state() / 'runs' / reading
    scratch.mkdir(parents=True, exist_ok=True)
    cmd = command(r['id'], lim) if launch is None else ['claude', '-p', prompt_for(r['id'])]
    handed = state() / 'runs' / f'{reading}.run.json'       # beside the scratch folder, not in it: what a run is is not the reading's to rewrite
    put(handed, dict(run={k: v for k, v in r.items() if k != 'report'}, board=str(board or '')))
    try:
        put(where / r['id'] / 'claim.json', dict(host=host, at=int(now.timestamp()), sig=r['sig']))
        other = (load(where / r['id'] / 'claim.json') or {}).get('host')
        if other not in (None, host):               # both stations wrote one in the same moment: one of them reads it back as the other's, and stands down
            raise ValueError(f'{other} claimed {r["id"]} in the same moment: left to it')
    except OSError:
        pass
    rec = dict(reading=reading, since=f'{now:%Y-%m-%d %H:%M:%S}', run=r['id'], sig=r['sig'], pid=0)
    put(state() / 'running.json', rec)              # before the session is: one that started and was not written down would be started again 20 s later, uncounted
    env = dict(os.environ, TW_RUNREPORTS=str(where), TW_RUNREPORT_READING=reading, TW_RUNREPORT_GIVEN=str(handed), TW_RUNREPORT_SCRATCH=str(scratch), TW_BRIEFS=str(briefs.folder()))
    out = state() / 'runs' / f'{reading}.json'
    if launch is None:
        with open(out, 'wb') as fh, open(out.with_suffix('.err'), 'wb') as eh:         # the child keeps its own handles; what it warns of is not in its result
            p = subprocess.Popen(cmd, cwd=str(scratch), env=env, stdin=subprocess.DEVNULL, stdout=fh, stderr=eh, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    else:
        p = launch(cmd, str(scratch), env, out)
    RUNS.update(p=p, reading=reading)
    rec['pid'] = getattr(p, 'pid', 0)
    put(state() / 'running.json', rec)
    return rec


def stop(rec, p=None):
    """End a reading this process started, with everything under it: on Windows the session may hang under a shell,
    and ending the shell alone leaves it running to its money. A reading this process did not start is never killed
    by its number: after a restart that number may be another program's."""
    if p is None:
        return
    try:
        if os.name == 'nt':
            subprocess.run(['taskkill', '/T', '/F', '/PID', str(p.pid)], capture_output=True, timeout=30)
        p.kill()
    except (OSError, subprocess.SubprocessError):
        pass


def finish(where: Path, rec, why='', now=None, notes_where: Path = None, every_note=()):
    """A reading ended: its line in spend.jsonl; a run left without its report has one failed reading written down
    (TRIES of them and it stays with its records only); and his note that asked for the reading is answered."""
    now = now or datetime.datetime.now()
    usd = 0.0
    try:
        raw = (state() / 'runs' / f'{rec["reading"]}.json').read_text(encoding='utf-8', errors='replace')
        got = json.loads(raw[raw.index('{"'):] if '{"' in raw else raw)
        usd = float(got.get('total_cost_usd') or 0)
        if got.get('is_error'):
            why = why or f'the session ended on an error: {one(got.get("result"))[:160]}'
    except (OSError, ValueError):
        why = why or 'the session left no result'
    rep = load(where / rec['run'] / 'report.json')
    made = bool(rep and rep.get('sig') == rec.get('sig'))
    if not made:
        why = why or 'the reading wrote no report'
        w = load(where / rec['run'] / 'wait.json') or {}
        w = dict(sig=rec.get('sig'), since=w.get('since', int(now.timestamp())), n=(w.get('n', 0) if w.get('sig') == rec.get('sig') else 0) + 1, why=why)
        try:
            put(where / rec['run'] / 'wait.json', w)
        except OSError:
            pass
    left = [b['id'] for b in briefs.read_all(briefs.folder()) if (b.get('raised_by') or {}).get('run') == rec['run']]
    line = dict(when=f'{now:%Y-%m-%d %H:%M}', reading=rec['reading'], run=rec['run'], made=made, briefs=len(left), usd=round(usd, 2), why=why, host=socket.gethostname())
    where.mkdir(parents=True, exist_ok=True)
    with open(where / 'spend.jsonl', 'a', encoding='utf-8') as f:
        f.write(json.dumps(line, sort_keys=True) + '\n')
    if notes_where:
        import notes
        said = (f'Read: its report is on the Runs page{", with " + str(len(left)) + (" decision" if len(left) == 1 else " decisions") + " for you on the Decide page" if left else ""}.' if made
                else f'Not read: the reading failed ({why}). The run stays with its records; ask again to have it tried once more.')
        for nid in asked_for(every_note).get(rec['run'], []):
            try:
                notes.answer(notes_where, nid, said, by=BY, now=now)
            except (OSError, ValueError):
                pass
    try:
        (state() / 'running.json').unlink()
    except OSError:
        pass
    RUNS.clear()
    old = sorted(d for d in (state() / 'runs').iterdir() if d.is_dir())[:-20] if (state() / 'runs').is_dir() else []
    for d in old:                                   # the last twenty readings' folders are kept to look at, with what each was handed and what it answered
        shutil.rmtree(d, ignore_errors=True)
        for f in (state() / 'runs').glob(d.name + '.*'):
            f.unlink(missing_ok=True)
    return line


def tick(R, where: Path = None, board='', now=None, launch=None, alive=None, lim=None, host=None, only=None, every_note=(), notes_where: Path = None):
    """What the watcher does on every read: look whether the reading that is going has ended (or ran past its
    minutes: then it is stopped), and when none runs and a run waits to be read, start one. `only` reads just those
    runs. Returns what the page is told: {running, waiting [runs], left, last, off}."""
    import ideas
    where, now, host = where or folder(), now or datetime.datetime.now(), host or socket.gethostname()
    nothing = dict(running=None, waiting=[], left=0, last=None)
    if os.environ.get('TW_RUNREPORT_OFF') and launch is None:
        # the tests of other tools, and a station that reads none: nothing is started, ended, written down or answered
        return dict(nothing, off='the reading is switched off on this station (TW_RUNREPORT_OFF)')
    if launch is None and not os.environ.get('TW_RUNREPORTS') and DRIVE_WAS and not build.DRIVE.is_dir():
        return dict(nothing, off='the Drive is away: nothing is read until it is back')        # else the reports go where no page reads them, and the runs are paid for twice
    lim, day = lim or limits(), f'{now:%Y-%m-%d}'
    rec, last, off = load(state() / 'running.json'), None, ''
    if rec:
        p = RUNS.get('p') if RUNS.get('reading') == rec['reading'] else None
        age = (now - datetime.datetime.strptime(rec['since'], '%Y-%m-%d %H:%M:%S')).total_seconds() / 60
        # a reading this process did not start (the watcher was restarted) is believed alive by its number only while
        # its minutes last: past them the number may be another program's, and it is written off, never killed
        going = (p.poll() is None) if p is not None else (age < lim['minutes'] and (alive or ideas.pid_alive)(rec))
        if going and age >= lim['minutes']:
            stop(rec, p)
            last, rec = finish(where, rec, why=f'stopped after {lim["minutes"]} minutes', now=now, notes_where=notes_where, every_note=every_note), None
        elif not going:
            last, rec = finish(where, rec, now=now, notes_where=notes_where, every_note=every_note), None
    want = [r for r in wanted(R, where, host, now.timestamp(), every_note) if not only or r['id'] in only]
    left = max(0, lim['runs_per_day'] - len(spent(where, day)))
    if not rec and want:
        if not left:
            off = f'the day\'s {lim["runs_per_day"]} readings are used'
        else:
            try:
                rec = start(where, want[0], board=board, now=now, launch=launch, lim=lim, host=host)
            except (ValueError, OSError) as e:
                off = str(e)
    return dict(running=dict(since=rec['since'], run=rec['run']) if rec else None, waiting=[r['id'] for r in want if not rec or r['id'] != rec['run']], left=max(0, left - (1 if rec else 0)), last=last, off=off)


# ---- the command -----------------------------------------------------------------------------------------------------

def main(argv=None):
    ap = argparse.ArgumentParser(description='what a relay run did and what it leaves for the owner, read when it has ended')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'context', 'brief', 'add', 'tick'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--title', default='', help=f'add: the run in plain words, {TITLE} characters at most. brief: the decision\'s title')
    ap.add_argument('--did', default='', help=f'what the run got done, in his words, {WORDS["did"]} words at most')
    ap.add_argument('--left', default='', help=f'what is unfinished, failed or blocked and why, {WORDS["left"]} words at most ("Nothing: ..." when nothing is)')
    ap.add_argument('--unit', action='append', default=[], help=f'ID=what was done on that unit, {WORDS["unit"]} words at most; one for every unit that ran')
    ap.add_argument('--asked', action='append', default=[], help='KEY=BRIEF: a brief that asks this already')
    ap.add_argument('--not-his', action='append', default=[], help=f'KEY=why this is not a decision of the owner\'s, {WORDS["no"]} words at most')
    ap.add_argument('--still-open', action='append', default=[], help=f'KEY=why this is his to decide and has no brief (the run left more than {MOST_BRIEFS}), {WORDS["no"]} words at most')
    ap.add_argument('--picture', action='append', default=[], help='PATH=what it shows; a picture `context` put in the reading\'s folder')
    ap.add_argument('--for', dest='what_for', default='', help=f'brief: what the decision is for, {briefs.FOR_WORDS} words at most')
    ap.add_argument('--option', action='append', default=[], help='brief: an option; the first is the one you would take')
    ap.add_argument('--why', default='', help='brief: why the first option')
    ap.add_argument('--evidence', action='append', default=[], help='brief: PATH=what it shows')
    ap.add_argument('--no-evidence', default='', help='brief: why nothing can be shown')
    ap.add_argument('--given', default='', help='a file of the run, whole (a reading is given one)')
    ap.add_argument('--only', action='append', default=[], help='tick: read just this run')
    a = ap.parse_args(argv)
    where, reading, scratch = folder(), os.environ.get('TW_RUNREPORT_READING', ''), os.environ.get('TW_RUNREPORT_SCRATCH', '')
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(errors='replace')
    try:
        if reading and a.what not in IN_A_READING:
            raise ValueError(f'{a.what} is not for a reading the board started ({", ".join(IN_A_READING)})')
        if a.what in ('context', 'add', 'brief'):
            if len(a.args) < 1:
                raise ValueError(f'{a.what} RUN')
            r, board = run_of(a.args[0], a.given)
            if a.what == 'context':
                print('\n'.join(context(r, board, scratch or None, briefs.read_all(briefs.folder()))))
            elif a.what == 'brief':
                if len(a.args) != 2:
                    raise ValueError('brief RUN KEY --title .. --for .. --option .. --option .. --why .. [--evidence PATH=caption | --no-evidence why]')
                b = brief(r, a.args[1], a.title, a.what_for, a.option, a.why, pairs(a.evidence), a.no_evidence, scratch=scratch or None)
                print(f'runreport: the ask {a.args[1]} is put to him as the brief {b["id"]}; `add` now counts it as answered')
            else:
                d = add(where, r, a.title, a.did, a.left, pairs(a.unit), pairs(a.asked), pairs(a.not_his), pairs(a.picture), briefs.read_all(briefs.folder()), scratch=scratch or None, reading=reading, still_open=pairs(a.still_open))
                print(f'runreport: {d["run"]} has its report ({len(d["units"])} units, {len(d["asks"])} asks answered, {len(d["pictures"])} pictures), in {where / d["run"]}')
        elif a.what == 'tick':
            import notes
            import ops
            board = ops.board_root()
            every = briefs.read_all(briefs.folder())
            his = notes.read_all(notes.folder())
            R = src_runs.read(board, every, read_all(where), [b['id'] for b in briefs.shown(every)])
            s = tick(R, where, board=board, only=a.only, every_note=his, notes_where=notes.folder())
            print(f'runreport: {"a reading is going since " + s["running"]["since"] + " for " + s["running"]["run"] if s["running"] else "no reading"}; {len(s["waiting"])} wait to be read; {s["left"]} readings left today'
                  + (f'; {s["off"]}' if s['off'] else '') + (f'; the last reading {"made the report of" if s["last"]["made"] else "made NO report of"} {s["last"]["run"]} for ${s["last"]["usd"]}' if s['last'] else ''))
        else:
            every = read_all(where)
            today = spent(where, f'{datetime.datetime.now():%Y-%m-%d}')
            print(f'runreport: {len(every)} reports in {where}; today {len(today)} readings, ${sum(r.get("usd", 0) for r in today):.2f}, {sum(1 for r in today if r.get("made"))} reports made')
            for rid, d in sorted(every.items(), reverse=True):
                print(f'  {rid:26} {d["when"]}  {d["title"]}')
    except ValueError as e:
        print(f'runreport: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
