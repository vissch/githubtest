#!/usr/bin/env python3
"""What a task on the board is about: read by an agent before the task is listed, with its pictures and its links.

    python Tools/assetboard/taskbrief.py                   from trench-warfare-3d/: the briefs, the day's runs, what waits
    python Tools/assetboard/taskbrief.py context ID        what the records say about one task (read this first)
    python Tools/assetboard/taskbrief.py add ID --title .. --about .. --part-of .. --stands .. --link .. --picture ..
    python Tools/assetboard/taskbrief.py shoot PAGE.html OUT.png      a picture of a page, for a task about a page
    python Tools/assetboard/taskbrief.py tick [--only ID ..]          what the watcher does on every read, by hand

WHY (the owner, 2026-10-08, looking at the Tasks page: "currently its not clear without proper context what these
tasks are about. we need before theyre added a agent to go through and find out what the related context is, provide
screenshots/videos where possible and provide the proper links to communicate what the tasks are. everytimes
something lands on that page we first need to go through that process"). A task was listed with the first words of
the prompt its agent was given ("You are a harsh art director..."), which says nothing of what the work was for.

A BRIEF is a folder <briefs>/<task id>/ with brief.json and what it shows: a title in plain words, what the task is
(`about`), what it was part of (`part_of`), where it stands (`stands`), links (a handoff, a doc, a lane, a commit, a
page) and pictures or short films. It is made for the task as it is now (sig()): a task that changed is read again.
The folder is on the Drive both stations read, and holds a copy of every doc and picture it shows, so the other
station's board shows the same.

THE GATE (hold()). A task with no brief is not listed: it is counted ("3 more are being read first"). Listed
without one are only: a capture from the game (his own picture and words, shown at once and read after), a task he
already said something about, and a task that could not be read (TRIES runs failed, or it waited HOLD_MAX): that
one says "no context found", because a task nobody can see is a task lost, which is what the board is against.

THE BOARD STARTS THE AGENT (tick(), as ideas.py does for ideas). The watcher starts one headless session at a time
for up to BATCH tasks. It runs in its own scratch folder and may write there only; beyond that it reads, and runs
this tool. It starts no agent and searches no web. A station reads the agents and sessions whose logs it has
(LOCAL_KINDS); what both stations see (handoffs, units, the relay's, steps, captures) is read by whichever claims
it first. The numbers are LIMITS; every run is a line in spend.jsonl with its dollars and the host that ran it.

WHAT IS CHECKED (check()). A brief is refused, with every reason, when it is not short, when it repeats the prompt,
when a link's target is not there (a file that does not exist, a lane or commit origin does not have), when a picture
is not one the task's own records named (context lists them) or one the run made itself, and when it shows nothing or
links nothing without saying why. Whether the words are TRUE is not checked by anything here: the skill
(tw-task-context) holds the agent to what it read.
"""
import argparse
import datetime
import hashlib
import html
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
import src_tasks    # noqa: E402
import src_visuals  # noqa: E402

TITLE = 70              # characters of a brief's title
WORDS = dict(about=45, part_of=25, stands=45)       # words, at most
CAPTION_WORDS, LABEL_WORDS = 14, 6
MOST_LINKS, MOST_PICTURES = 6, 4
BATCH = 4               # tasks one run reads
TRIES = 2               # runs that may fail on a task before it is listed without a brief
HOLD_MAX = 24 * 3600    # seconds a task may wait to be read before it is listed without one
CLAIM = 30 * 60         # seconds another station's claim on a shared task is left alone
LOCAL_KINDS = ('agent', 'session', 'paused')        # their logs are on the station they ran on
LINK_KINDS = ('handoff', 'doc', 'lane', 'commit', 'page', 'board')
BOARD_PAGES = ('index.html', 'floor.html', 'house.html', 'graphs.html', 'decide.html', 'tasks.html')
DOC_TYPES = ('.md', '.txt', '.json', '.py', '.cs', '.js', '.css', '.uss', '.uxml', '.ps1', '.sh', '.yml', '.yaml', '.shader', '.hlsl')
DOC_MAX = 400000        # bytes of a doc a brief may link (it is copied)
SESSION_TAIL = 8 * 2 ** 20      # bytes of a session's transcript that are read for what it had in its hands
NEAR = (2 * 86400, 6 * 3600)    # a capture is the task's when it was made this long before, or this long after, it stopped
LANE = re.compile(r'lane/(?:show|sim)/[\w-]+(?:\.[\w-]+)*')         # not the dots of a range (lane/show/a..HEAD)
MADE = re.compile(r'\[([\w/.-]+) ([0-9a-f]{7,12})\]')               # git's line for a commit that was made: [branch sha]
SHA = re.compile(r'\b(?=[0-9a-f]*\d)(?=[0-9a-f]*[a-f])[0-9a-f]{7,12}\b')
URL = re.compile(r'https?://[^\s"\'<>)\]\\`]+')
FILE = re.compile(r'[A-Za-z]:[\\/](?:[^\\/:*?"<>|\r\n]+[\\/])*[^\\/:*?"<>|\r\n]+?\.(?:png|jpe?g|gif|webp|mp4|webm)', re.I)
PROMPTY = re.compile(r'^\s*(you are|your (task|job) is|read the brief)|follow it exactly|\bas an ai\b', re.I)

words, slug, one = briefs.words, briefs.slug, briefs.one


def folder():
    """Where the briefs are: TW_TASKBRIEFS, else tasks/briefs under the tasks' own folder (the Drive both stations read)."""
    if os.environ.get('TW_TASKBRIEFS'):
        return Path(os.environ['TW_TASKBRIEFS'])
    return src_tasks.root() / 'tasks' / 'briefs'


def state():
    """This station's own: the run that is going, the runs' scratch folders, what context() found. Never on the Drive
    (a session's scratch is not something to upload)."""
    if os.environ.get('TW_TASKBRIEFS'):
        return Path(os.environ['TW_TASKBRIEFS']) / '_station'
    return build.LOCAL / 'taskbrief'


def sig(r):
    """The task as it is now, in twelve characters: its own last change (the agent's log, the session's transcript,
    the handoff's file), not its parent moving or somebody opening it. Another sig is another task to read."""
    return hashlib.sha1(f'{r["kind"]}|{r["id"]}|{r.get("changed", r.get("touched", 0))}'.encode('utf-8')).hexdigest()[:12]


def load(path: Path):
    try:
        d = json.loads(path.read_text(encoding='utf-8'))
        return d if isinstance(d, dict) else None
    except (OSError, ValueError):
        return None


def put(path: Path, d):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + '.tmp')
    tmp.write_text(json.dumps(d, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    tmp.replace(path)


def read(where: Path, tid):
    return load(where / tid / 'brief.json')


def current(where: Path, r):
    """The brief of a task as it is now, or None."""
    b = read(where, r['id'])
    return b if b and b.get('sig') == sig(r) else None


def waits(where: Path, r, now=None):
    """What is written of a task that waits to be read: since when, and the runs that failed on it. Made on first ask."""
    w = load(where / r['id'] / 'wait.json')
    if not w or w.get('sig') != sig(r):
        w = dict(sig=sig(r), since=int(now or time.time()), n=0, why='')
        try:
            put(where / r['id'] / 'wait.json', w)
        except OSError:
            pass
    return w


def given_up(where: Path, r, now=None):
    """Why a task is listed with no brief ('' while it is still to be read): the runs that failed on it, or the wait."""
    now = now or time.time()
    w = waits(where, r, now)
    if w.get('n', 0) >= TRIES:
        return f'{w["n"]} readings of it failed' + (f' ({w["why"]})' if w.get('why') else '')
    if now - w.get('since', now) > HOLD_MAX:
        return f'nothing read it in {HOLD_MAX // 3600} hours'
    return ''


# ---- what the records say about a task (context) ---------------------------------------------------------------------

def git(*args, send=None):
    try:
        p = subprocess.run(['git', '-C', str(build.REPO), *args], capture_output=True, input=send, timeout=60)
    except (OSError, subprocess.TimeoutExpired):
        return ''
    return p.stdout.decode('utf-8', 'replace').strip() if p.returncode == 0 else ''


def origin():
    """The repo's page on GitHub, without the ending: what a lane's and a commit's link hang under ('' when unknown)."""
    url = git('remote', 'get-url', 'origin')
    url = re.sub(r'^git@github\.com:', 'https://github.com/', url)
    return re.sub(r'\.git$', '', url) if url.startswith('https://') else ''


def lane_facts(lane):
    """How a lane stands, from the refs this station last fetched: (on origin, commits not on integration)."""
    if not git('rev-parse', '--verify', '--quiet', f'origin/{lane}'):
        return False, 0
    ahead = git('rev-list', '--count', f'{src_tasks.INTEGRATION}..origin/{lane}')
    return True, int(ahead) if ahead.isdigit() else -1


def commits_of(cands):
    """Which of some hex words are commits of this repo: [(short, subject)], in the order given."""
    cands = list(dict.fromkeys(cands))[-40:]
    if not cands:
        return []
    got = git('cat-file', '--batch-check', send=''.join(c + '\n' for c in cands).encode('ascii'))
    out = []
    for c, line in zip(cands, got.splitlines()):
        part = line.split()
        if len(part) == 3 and part[1] == 'commit':
            out.append((part[0][:10], git('log', '-1', '--format=%s', part[0])[:110]))
    return out[-8:]


def in_text(text, found):
    """Add to `found` the lanes, hex words, links, handoffs and pictures a text names."""
    found['lanes'] += LANE.findall(text)
    found['shas'] += SHA.findall(text)
    found['urls'] += [u.rstrip('.,;') for u in URL.findall(text)]
    found['handoffs'] += src_tasks.HANDOFF.findall(text)
    return found


def scan(f: Path, side, cwd='', last=None):
    """One pass over a log: what it wrote, the docs it read, the pictures and films it looked at or named in a
    command (src_visuals.named), and the names in its talk. `last` reads only that many bytes of its end."""
    found = dict(wrote=[], read=[], seen={}, lanes=[], shas=[], made=[], urls=[], handoffs=[], start=None)
    try:
        size = f.stat().st_size
        with open(f, 'rb') as h:
            if last and size > last:
                h.seek(size - last)
                h.readline()
            for line in h:
                try:
                    d = json.loads(line)
                except ValueError:
                    continue
                if not isinstance(d, dict) or not src_tasks.talks(d, side):
                    continue
                when = src_tasks.stamp(d.get('timestamp')) or 0
                found['start'] = found['start'] or when
                cwd = d.get('cwd') or cwd
                c = (d.get('message') or {}).get('content')
                texts = [c] if isinstance(c, str) else []
                for x in c if isinstance(c, list) else []:
                    if not isinstance(x, dict):
                        continue
                    if x.get('type') == 'text':
                        texts.append(str(x.get('text') or ''))
                    elif x.get('type') == 'tool_use':
                        name, inp = x.get('name'), x.get('input') if isinstance(x.get('input'), dict) else {}
                        for p, how in src_visuals.named(name, inp, cwd):
                            found['seen'][p] = (int(when), how)
                        path = str(inp.get('file_path') or '')
                        if path and name in ('Write', 'Edit', 'NotebookEdit'):
                            found['wrote'].append(path)
                        elif path and name == 'Read' and path.lower().endswith(DOC_TYPES[:2]):
                            found['read'].append(path)
                        texts.append(json.dumps(inp))
                    elif x.get('type') == 'tool_result':
                        # of what a tool answered only the commits it made count: a log it listed names every lane there is
                        r = x.get('content')
                        for branch, sha in MADE.findall((r if isinstance(r, str) else ' '.join(str(y.get('text') or '') for y in r or [] if isinstance(y, dict)))[:20000]):
                            found['made'].append(sha)
                            found['lanes'] += LANE.findall(branch)
                for t in texts:
                    in_text(t, found)
    except OSError:
        pass
    return found


def asked_before(parent: Path, start):
    """What the owner last typed in a session before it started an agent (the request the agent's work was for), and
    the session's own last lines before it: (his words, [its lines]). A summary the session was continued from is
    not his request."""
    typed, said = '', []
    try:
        with open(parent, 'rb') as h:
            for line in h:
                try:
                    d = json.loads(line)
                except ValueError:
                    continue
                if not isinstance(d, dict) or d.get('type') not in ('user', 'assistant') or d.get('isSidechain'):
                    continue
                if start and (src_tasks.stamp(d.get('timestamp')) or 0) > start:
                    break
                t = src_tasks.text_of(d).strip()
                if d['type'] == 'assistant':
                    said = (said + [src_tasks.clip(t, 500)])[-3:] if t else said
                elif t and not t.startswith(('<', '[', 'This session is being continued')) and not d.get('isCompactSummary'):
                    typed, said = t, []
    except OSError:
        pass
    return typed[:1500], said


def siblings(log: Path, start):
    """The other agents the same session started, from an hour before this one on: what each was for and how it
    ended. A critic that failed and was started again shows here, and so does the round after it."""
    out = []
    for g in sorted(log.parent.glob('agent-*.jsonl')):
        if g == log:
            continue
        try:
            a = src_tasks.read_agent(g)
        except (OSError, ValueError):
            continue
        if (a.get('last') or 0) >= (start or 0) - 3600:
            out.append((a.get('last') or 0, dict(what=src_tasks.clip(a['desc'] or a['ask'], 90), ended='finished' if a['end'] == 'done' else 'cut off: ' + a['why'], when=src_tasks.when(a.get('last')),
                                                 task='agent-' + g.stem[len('agent-'):][:10])))
    return [d for _, d in sorted(out, key=lambda x: x[0])][:15]


def showable(path):
    try:
        f = Path(path)
        ext = f.suffix.lower()
        return f.is_file() and (ext in src_visuals.PICTURES or (ext in src_visuals.FILMS and f.stat().st_size <= src_visuals.FILM_MAX))
    except OSError:
        return False


def captures_near(cwd, at):
    """The pictures and films under the Captures folder of the checkout a task ran in, made around when it stopped."""
    here = Path(cwd) if cwd else None
    root = next((p / 'trench-warfare-3d' / 'Captures' for p in [here, *here.parents] if (p / 'trench-warfare-3d' / 'Captures').is_dir()), None) if here else None
    out = []
    try:
        for f in root.rglob('*') if root else []:
            if f.suffix.lower() in src_visuals.PICTURES + src_visuals.FILMS and f.is_file():
                t = f.stat().st_mtime
                if at - NEAR[0] <= t <= at + NEAR[1] and showable(f):
                    out.append((int(t), str(f)))
    except OSError:
        pass
    return sorted(out)[-8:]


def digest(r):
    """Everything the records say about a task, for the agent that writes its brief: the row, what its log wrote,
    read and looked at, the request behind it, the lanes and commits it names with how they stand, and the pictures
    it may show. Nothing here is judged; `pictures` is the list a brief's pictures must come from."""
    k, where = r['kind'], src_tasks.root()
    d = dict(id=r['id'], kind=k, title=r.get('title', ''), stopped=r.get('stopped', ''), why=r.get('why', ''), station=r.get('where', ''), started_by=r.get('by', ''),
             agent=r.get('agent', ''), asked=r.get('ask') or r.get('what') or '', last_lines=r.get('turns') or [], cwd=r.get('cwd', ''), branch=r.get('branch', ''),
             first_asked=r.get('first', ''), lane=r.get('lane', ''), report=r.get('report', ''), verdict=r.get('verdict', ''), notes_of_his=r.get('words') or [])
    found = dict(wrote=[], read=[], seen={}, lanes=[], shas=[], made=[], urls=[], handoffs=[], start=None)
    at = r.get('changed', r.get('touched', 0))
    if k in LOCAL_KINDS and r.get('log'):
        found = scan(Path(r['log']), side=k == 'agent', cwd=r.get('cwd', ''), last=None if k == 'agent' else SESSION_TAIL)
        d['log'] = r['log']
        if k == 'agent' and r.get('parent_log'):
            d['owner_asked_before'], d['session_said_before'] = asked_before(Path(r['parent_log']), found['start'])
            d['parent_log'] = r['parent_log']
        if k == 'agent':
            d['siblings'] = siblings(Path(r['log']), found['start'])
    elif k == 'handoff' and r.get('file'):
        d['file'] = str(where / r['file'])
        try:
            text = (where / r['file']).read_text(encoding='utf-8', errors='replace')
        except OSError:
            text = ''
        d['file_head'] = text[:6000]
        in_text(text, found)
        found['seen'].update({p: (int(at), 'named in the handoff') for p in FILE.findall(text)})
        found['handoffs'].append(r['file'])
    elif k == 'unit' and r.get('file'):
        d['file'] = str(where / 'units-for-master' / r['file'])
    elif k == 'capture' and r.get('picture'):
        found['seen'][r['picture']] = (int(at), 'the screen when he pressed F10')
        d['capture_lines'] = r.get('lines') or []
    in_text(' '.join([d['asked'], d['report'], d['lane'], d['branch']]), found)
    for p in FILE.findall(d['asked']):              # the stills a critic was pointed at and never opened are the task's too
        found['seen'].setdefault(p, (int(at), 'named in what it was asked'))
    pictures = [dict(path=os.path.normpath(p), when=src_tasks.when(t), how=how) for p, (t, how) in sorted(found['seen'].items(), key=lambda kv: kv[1][0]) if showable(p)][-16:]
    have = {p['path'] for p in pictures}
    pictures += [dict(path=os.path.normpath(p), when=src_tasks.when(t), how='in the checkout\'s Captures, made around when it stopped') for t, p in captures_near(r.get('cwd'), at) if os.path.normpath(p) not in have]
    index = (load(where / 'handoffs.json') or {}).get('handoffs') or {}
    d.update(wrote=list(dict.fromkeys(found['wrote']))[-30:], docs_read=list(dict.fromkeys(found['read']))[-15:], pictures=pictures,
             lanes=[dict(lane=l, on_origin=lane_facts(l)[0], not_landed=lane_facts(l)[1]) for l in list(dict.fromkeys(found['lanes']))[-10:]],
             commits_made=[dict(sha=s, subject=t) for s, t in commits_of(found['made'])], commits=[dict(sha=s, subject=t) for s, t in commits_of([s for s in found['shas'] if s not in found['made']])], links=[u for u in dict.fromkeys(found['urls']) if not any(h in u for h in ('anthropic.com', 'claude.com', 'claude.ai'))][-10:],
             handoffs=[dict(file=n, there=(where / n).is_file(), topic=(index.get(n) or {}).get('topic', ''), state=(index.get(n) or {}).get('state', 'not in the index')) for n in list(dict.fromkeys(found['handoffs']))[-8:]],
             handoff_folder=str(where), repo=str(build.REPO))
    return d


def digest_lines(d):
    """The digest as text for the agent."""
    out = [f'TASK {d["id"]}  ({d["kind"]})', f'  on the page now as: {d["title"]}', f'  stopped: {d["stopped"]}{" - " + d["why"] if d["why"] else ""}{"   station: " + d["station"] if d["station"] else ""}']
    if d['started_by']:
        out.append(f'  started by the session titled: {d["started_by"]}')
    if d.get('owner_asked_before'):
        out += ['', 'THE OWNER\'S LAST REQUEST in that session before the agent was started (what the work was for):', '  ' + d['owner_asked_before'].replace('\n', '\n  ')]
    if d.get('session_said_before'):
        out += ['', 'ITS SESSION\'S OWN LAST LINES before it started the agent:'] + ['  ' + t for t in d['session_said_before']]
    if d['first_asked']:
        out += ['', 'THE SESSION WAS FIRST ASKED:', '  ' + d['first_asked'].replace('\n', '\n  ')]
    out += ['', 'WHAT IT WAS ASKED (the prompt or last request; do not copy it, say what it is for):', '  ' + str(d['asked']).replace('\n', '\n  ')]
    if d['last_lines']:
        out += ['', 'ITS LAST LINES:'] + ['  ' + t for t in d['last_lines']]
    if d.get('siblings'):
        out += ['', 'THE OTHER AGENTS ITS SESSION STARTED AROUND AND AFTER IT (was this one started again, or its work done by a later one?):'] + [f'  {s["when"]}  {s["ended"]:38}  {s["what"]}   ({s["task"]})' for s in d['siblings']]
    if d['report']:
        out += ['', f'THE RELAY\'S LAST LEG SAID ({d["verdict"]}): {d["report"]}']
    if d['notes_of_his']:
        out += ['', 'THE OWNER SAID ABOUT IT:'] + ['  ' + w for w in d['notes_of_his']]
    for key, head in (('capture_lines', 'THE CAPTURE'), ):
        if d.get(key):
            out += ['', head + ':'] + ['  ' + t for t in d[key]]
    if d.get('file'):
        out += ['', f'ITS FILE (read it whole): {d["file"]}']
    if d.get('file_head'):
        out += ['  it starts:'] + ['  | ' + l for l in d['file_head'].splitlines()[:40]]
    where = [f'  folder {d["cwd"]}' if d['cwd'] else '', f'  branch then: {d["branch"]}' if d['branch'] else '', f'  its log: {d["log"]}' if d.get('log') else '', f'  its session\'s transcript: {d["parent_log"]}' if d.get('parent_log') else '']
    out += ['', 'WHERE IT RAN:'] + [w for w in where if w] if any(where) else []
    out += ['', 'LANES IT NAMES (as origin was last fetched):'] + [f'  {l["lane"]}: ' + (('landed (nothing of it is off integration)' if l['not_landed'] == 0 else f'on origin, {l["not_landed"]} commits not on integration') if l['on_origin'] else 'NOT on origin (cannot be a link)') for l in d['lanes']] if d['lanes'] else []
    out += ['', 'COMMITS IT MADE (git answered with them):'] + [f'  {c["sha"]}  {c["subject"]}' for c in d['commits_made']] if d.get('commits_made') else []
    out += ['', 'COMMITS IT NAMES IN ITS OWN WORDS OR COMMANDS:'] + [f'  {c["sha"]}  {c["subject"]}' for c in d['commits']] if d['commits'] else []
    out += ['', 'HANDOFFS IT NAMES (in ' + d['handoff_folder'] + '):'] + [f'  {h["file"]}: ' + (f'{h["state"]}{", topic " + h["topic"] if h["topic"] else ""}' if h['there'] else 'NOT THERE') for h in d['handoffs']] if d['handoffs'] else []
    out += ['', 'FILES IT WROTE OR EDITED (newest last):'] + ['  ' + p for p in d['wrote']] if d['wrote'] else []
    out += ['', 'DOCS IT READ:'] + ['  ' + p for p in d['docs_read']] if d['docs_read'] else []
    out += ['', 'LINKS IT NAMES:'] + ['  ' + u for u in d['links']] if d['links'] else []
    out += ['', 'PICTURES AND FILMS IT MAY SHOW (still there, and others in their folders; look at one with Read before you show it):'] + [f'  {p["path"]}   ({p["when"]}, {p["how"]})' for p in d['pictures']] if d['pictures'] else ['', 'PICTURES: none that its records name is still there.']
    out += ['', f'The repo is {d["repo"]} (docs/reference/tasks.md says what each system is; docs/reference/decisions.md what the owner decided).']
    return out


def remember(d):
    try:
        put(state() / 'digests' / f'{d["id"]}.json', d)
    except OSError:
        pass
    return d


# ---- a brief is written (check, add) ---------------------------------------------------------------------------------

def link_of(kind, label, target, bad):
    """One link, checked: what is kept of it, or None with the reason in `bad`."""
    kind, label, target = one(kind).lower(), one(label), str(target or '').strip()
    if kind not in LINK_KINDS:
        bad.append(f'a link of kind {kind!r}: it is one of {", ".join(LINK_KINDS)}')
        return None
    if not label or words(label) > LABEL_WORDS:
        bad.append(f'the link to {target} needs a label of at most {LABEL_WORDS} words that says what is behind it')
        return None
    if kind == 'handoff':
        name = Path(target.replace('\\', '/')).name
        f = src_tasks.root() / name
        if not src_tasks.HANDOFF.fullmatch(name) or not f.is_file():
            bad.append(f'the handoff {name} is not in {src_tasks.root()}')
            return None
        return dict(kind=kind, label=label, target=name, source=str(f))
    if kind == 'doc':
        f = Path(target) if Path(target).is_absolute() else build.REPO / target
        try:
            ok = f.is_file() and f.suffix.lower() in DOC_TYPES and f.stat().st_size <= DOC_MAX
        except OSError:
            ok = False
        if not ok:
            bad.append(f'the doc {target} is not a file that is there, of {" ".join(DOC_TYPES)} and at most {DOC_MAX // 1000} kB')
            return None
        try:
            shown = f.resolve().relative_to(build.REPO.resolve()).as_posix()
        except ValueError:
            shown = f.name
        return dict(kind=kind, label=label, target=shown, source=str(f))
    if kind == 'lane':
        base = origin()
        if not LANE.fullmatch(target) or not lane_facts(target)[0] or not base:
            bad.append(f'the lane {target} is not on origin as this station last fetched it: say it in --stands, it cannot be a link')
            return None
        return dict(kind=kind, label=label, target=target, href=f'{base}/tree/{target}')
    if kind == 'commit':
        full, base = git('rev-parse', '--verify', '--quiet', f'{target}^{{commit}}') if SHA.fullmatch(target) or re.fullmatch(r'[0-9a-f]{40}', target) else '', origin()
        if not full or not base or not git('branch', '-r', '--contains', full):
            bad.append(f'{target} is not a commit that origin has: say it in --stands, it cannot be a link')
            return None
        return dict(kind=kind, label=label, target=full[:10], href=f'{base}/commit/{full}')
    if kind == 'page':
        if not re.fullmatch(r'https?://\S+', target):
            bad.append(f'the page {target} is not a link (http or https)')
            return None
        return dict(kind=kind, label=label, target=target, href=target)
    if target not in BOARD_PAGES:
        bad.append(f'the board has no page {target}: it has {", ".join(BOARD_PAGES)}')
        return None
    return dict(kind=kind, label=label, target=target, href=target)


def check(r, title, about, part_of, stands, links, pictures, no_picture='', no_link='', may_show=(), scratch=None):
    """Why this is not a brief yet: every reason, as a list (empty when it is one), and the links as they are kept.
    `links` is [(kind, label, target)], `pictures` [(path, caption)], `may_show` the pictures context() listed."""
    bad, kept = [], []
    title, texts = one(title), dict(about=one(about), part_of=one(part_of), stands=one(stands))
    if not title or len(title) > TITLE:
        bad.append(f'the title is {len(title)} characters; 1 to {TITLE}')
    for key, text in texts.items():
        if not text:
            bad.append(f'--{key.replace("_", "-")} is empty')
        elif words(text) > WORDS[key]:
            bad.append(f'--{key.replace("_", "-")} takes {words(text)} words; {WORDS[key]} at most')
    asked = one(r.get('ask') or r.get('what') or '')
    for name, text in (('the title', title), ('--about', texts['about'])):
        if PROMPTY.search(text) or (len(asked) > 60 and asked[:60].lower() in text.lower()):
            bad.append(f'{name} repeats the prompt the agent was given: say in your own words what the work is and what it is for')
    if len(links) > MOST_LINKS:
        bad.append(f'{len(links)} links; {MOST_LINKS} at most, the ones he would open')
    for kind, label, target in links[:MOST_LINKS]:
        l = link_of(kind, label, target, bad)
        if l:
            kept.append(l)
    if not links and not one(no_link):
        bad.append('it links nothing: give a --link, or say with --no-link why there is nothing to point to')
    if len(pictures) > MOST_PICTURES:
        bad.append(f'{len(pictures)} pictures; {MOST_PICTURES} at most')
    allowed = {os.path.normcase(os.path.normpath(p)) for p in may_show}
    beside = {os.path.dirname(p) for p in allowed}  # "bu1.png, bu2.png ... (same folder)": a listed picture's neighbours are the task's too
    for path, caption in pictures[:MOST_PICTURES]:
        f = Path(path)
        mine = scratch and Path(scratch).resolve() in f.resolve().parents
        if not showable(f):
            bad.append(f'{path} is not a picture or a film that is there (films: {" ".join(src_visuals.FILMS)}, at most {src_visuals.FILM_MAX // 2 ** 20} MB)')
        elif not mine and os.path.normcase(os.path.normpath(str(f))) not in allowed and os.path.dirname(os.path.normcase(os.path.normpath(str(f)))) not in beside:
            bad.append(f'{f.name} is not one of the pictures `context` lists for this task (or in a folder of one), nor one this run made: a picture from elsewhere is not shown as the task\'s')
        if not one(caption) or words(caption) > CAPTION_WORDS:
            bad.append(f'the picture {f.name} needs a caption of at most {CAPTION_WORDS} words that says what it shows')
    if not pictures and not one(no_picture):
        bad.append('it shows nothing: give a --picture, or say with --no-picture why nothing can be shown')
    return bad, kept


def add(where: Path, r, title, about, part_of, stands, links=(), pictures=(), no_picture='', no_link='', may_show=(), scratch=None, run='', now=None):
    """Write the brief of a task and return it. A ValueError says every reason it is not one."""
    links, pictures = list(links), list(pictures)
    bad, kept = check(r, title, about, part_of, stands, links, pictures, no_picture, no_link, may_show, scratch)
    if bad:
        raise ValueError('not a brief yet: ' + '; '.join(bad))
    now = now or datetime.datetime.now()
    home = where / r['id']
    home.mkdir(parents=True, exist_ok=True)
    for f in home.iterdir():                        # the brief before this one: its pictures and docs go with it
        if f.name not in ('wait.json', 'claim.json') and f.is_file():
            f.unlink()
    shown = []
    for i, (path, caption) in enumerate(pictures, 1):
        src = Path(path)
        dst = briefs.keep(src, home / f'{i}-{slug(src.stem)[:40] or "picture"}{src.suffix.lower()}')
        shown.append(dict(file=dst.name, caption=one(caption), film=dst.suffix.lower() in src_visuals.FILMS))
    for i, l in enumerate(kept, 1):
        src = l.pop('source', '')
        if src:                                     # a copy: the other station, and a later day, show what was read
            l['file'] = f'doc-{i}-{slug(Path(src).stem)[:50] or "doc"}.txt'
            (home / l['file']).write_text(Path(src).read_text(encoding='utf-8', errors='replace'), encoding='utf-8')
    b = dict(id=r['id'], sig=sig(r), when=f'{now:%Y-%m-%d %H:%M}', host=socket.gethostname(), run=run, title=one(title), about=one(about), part_of=one(part_of), stands=one(stands),
             links=kept, pictures=shown, no_picture=one(no_picture), no_link=one(no_link))
    put(home / 'brief.json', b)
    return b


# ---- the brief on the page (attach, hold, dress) ---------------------------------------------------------------------

def attach(T, where: Path = None):
    """Give every task that has a brief for what it is now its `brief`. Returns how many have one."""
    where, n = where or folder(), 0
    for r in T['rows']:
        b = current(where, r)
        if b:
            r['brief'] = b
            n += 1
    return n


def hold(T, where: Path = None, now=None):
    """THE GATE: a task with no brief leaves the rows and is counted in T['reading']. See the head of this file for
    what is listed without one. Returns the rows held."""
    where, now, kept, held = where or folder(), now or time.time(), [], []
    for r in T['rows']:
        if r.get('brief') or r['state'] != 'left' or r['kind'] == 'capture':
            kept.append(r)
            continue
        why = given_up(where, r, now)
        if why:
            r['nocontext'] = why
            kept.append(r)
        else:
            held.append(r)
    T['rows'], T['reading'] = kept, len(held)
    return held


PAGE = ('<!doctype html><meta charset="utf-8"><title>%s</title><style>body{margin:0;background:#0b1220;color:#dbe4f5;font:15px/1.55 ui-sans-serif,system-ui,sans-serif}'
        'header{padding:18px 28px;border-bottom:1px solid #24304a}h1{font-size:18px;margin:0 0 4px}p{margin:0;color:#8fa0c0;font-size:13px}'
        'pre{margin:0;padding:22px 28px;white-space:pre-wrap;overflow-wrap:anywhere;font:13.5px/1.55 ui-monospace,Consolas,monospace}</style>'
        '<header><h1>%s</h1><p>%s</p></header><pre>%s</pre>\n')


def dress(r, where: Path, out: Path):
    """Put a task's brief on its row for the page, and what it shows in the site: pictures and films under
    img/task/<id>/, a linked doc as a page of its own under task/<id>/ (a page opened as a file cannot follow a path
    of this machine, and the other station has no such path). The row keeps what it said before as `told`."""
    b = r.pop('brief', None)
    if not b:
        return r
    home, tid = where / b['id'], b['id']
    r.update(was=r.get('title', ''), title=b['title'], about=b['about'], part_of=b['part_of'], stands=b['stands'], read_at=b.get('when', ''),
             told=src_tasks.clip(r.get('ask') or r.get('what') or '', 600))
    shots = list(r.get('shots') or [])
    for p in b.get('pictures') or []:
        src, dst = home / p['file'], out / 'img' / 'task' / tid / p['file']
        try:
            if not dst.exists() or dst.stat().st_size != src.stat().st_size:
                dst.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(src, dst)
            shots.append(dict(src=dst.relative_to(out).as_posix(), name=p['caption'], caption=p['caption'], film=bool(p.get('film'))))
        except OSError:
            continue
    r['shots'] = shots
    links = []
    for l in b.get('links') or []:
        href = l.get('href', '')
        if l.get('file'):
            page = out / 'task' / tid / (Path(l['file']).stem + '.html')
            try:
                text = PAGE % (html.escape(l['target']), html.escape(l['label']), html.escape(f'{l["target"]}, as it was read on {b.get("when", "")} for the task "{b["title"]}"'),
                               html.escape((home / l['file']).read_text(encoding='utf-8', errors='replace')))
                if not page.exists() or page.read_text(encoding='utf-8') != text:
                    page.parent.mkdir(parents=True, exist_ok=True)
                    page.write_text(text, encoding='utf-8')
                href = page.relative_to(out).as_posix()
            except OSError:
                continue
        if href:
            links.append(dict(label=l['label'], href=href, kind=l['kind'], title=l['target']))
    r['links'] = links
    return r


def sweep(out: Path, listed):
    """Remove from the site what the briefs of tasks no longer listed showed."""
    for root in (out / 'img' / 'task', out / 'task'):
        for d in root.iterdir() if root.is_dir() else []:
            if d.is_dir() and d.name not in listed:
                shutil.rmtree(d, ignore_errors=True)


# ---- the board starts the agent --------------------------------------------------------------------------------------

LIMITS = dict(runs_per_day=12, usd_per_run=3.0, minutes=20, model='sonnet')         # taskbrief.json beside this file overrules them
RUNS = {}               # the run this process started: {'p': Popen, 'run': name}
TOOL = Path(__file__).resolve().as_posix()
SKILL = (build.REPO / '.claude' / 'skills' / 'tw-task-context' / 'SKILL.md').as_posix()
IN_A_RUN = ('list', 'context', 'add', 'shoot')          # what a run may ask of this tool: never another run


def limits():
    got = load(HERE / 'taskbrief.json') or {}
    return {k: type(v)(got.get(k, v)) for k, v in LIMITS.items()}


def spent(where: Path, day):
    out = []
    try:
        for row in (where / 'spend.jsonl').read_text(encoding='utf-8').splitlines():
            r = json.loads(row)
            if str(r.get('when', '')).startswith(day):
                out.append(r)
    except (OSError, ValueError):
        pass
    return out


def wanted(T, where: Path, host, now):
    """The tasks to read next, this station's to read: a capture first (it is on the page already, bare), then the
    ones that wait off the page, the one that stopped last first, then the ones he already queued (listed, bare). Another station's agents and sessions are that station's; a shared task another station
    claimed lately is left to it."""
    out = []
    for r in T['rows']:
        if current(where, r) or given_up(where, r, now):
            continue
        if r['kind'] in LOCAL_KINDS and r.get('where') != host:
            continue
        c = load(where / r['id'] / 'claim.json') or {}
        if c.get('host') not in (None, host) and c.get('sig') == sig(r) and now - c.get('at', 0) < CLAIM:
            continue
        out.append(r)
    return sorted(out, key=lambda r: (r['kind'] != 'capture', r['state'] != 'left', r.get('idle', 0)))


def prompt_for(ids):
    return (f'You are the task-context agent of Trench Warfare 3D, started by the board. First read {SKILL} and follow it. '
            f'These tasks are about to be listed on the owner\'s Tasks page and he cannot tell what they are: {", ".join(ids)}. '
            f'For each one, in this order: `python {TOOL} context ID`, look at what it points to, then `python {TOOL} add ID ...`. Run the tool with that path, as written, one command a call. '
            'You are in your scratch folder: write there and nowhere else. Stop when every task has its brief. '
            'Nobody answers a question in this session: where you are unsure, say less.')


def command(ids, lim, exe=None):
    """The headless session that reads a batch of tasks (ideas.py command() says why it is started this way). It may
    read, and run this tool; it starts no agent and searches no web; it is cut off at the run's money."""
    exe = exe or shutil.which('claude')
    if not exe:
        raise ValueError('no claude on this station\'s path: the tasks cannot be read here')
    cmd = [exe, '-p', prompt_for(ids), '--output-format', 'json', '--permission-mode', 'acceptEdits', '--allowedTools', 'Read', 'Grep', 'Glob', f'Bash(python {TOOL} *)',
           '--disallowedTools', 'AskUserQuestion', 'Agent', 'WebSearch', 'WebFetch', '--max-budget-usd', str(lim['usd_per_run'])]
    return cmd + (['--model', lim['model']] if lim['model'] else [])


def start(where: Path, rows, now=None, launch=None, lim=None, host=None):
    """Start a run for some tasks and write down that it runs. `launch(cmd, cwd, env, out)` starts the process (the
    tests give their own). Returns the run's record."""
    now, lim, host = now or datetime.datetime.now(), lim or limits(), host or socket.gethostname()
    run, ids = f'{now:%Y%m%d-%H%M%S}', [r['id'] for r in rows]
    scratch = state() / 'runs' / run
    scratch.mkdir(parents=True, exist_ok=True)
    cmd = command(ids, lim) if launch is None else ['claude', '-p', prompt_for(ids)]
    given = state() / 'runs' / f'{run}.rows.json'       # beside the scratch folder, not in it: what a task is is not the run's to rewrite
    put(given, {r['id']: {k: v for k, v in r.items() if k != 'brief'} for r in rows})
    for r in rows:
        try:
            put(where / r['id'] / 'claim.json', dict(host=host, at=int(now.timestamp()), sig=sig(r)))
        except OSError:
            pass
    env = dict(os.environ, TW_TASKBRIEFS=str(where), TW_TASKBRIEF_RUN=run, TW_TASKBRIEF_ROWS=str(given), TW_TASKBRIEF_SCRATCH=str(scratch))
    out = state() / 'runs' / f'{run}.json'
    if launch is None:
        fh = open(out, 'wb')
        p = subprocess.Popen(cmd, cwd=str(scratch), env=env, stdin=subprocess.DEVNULL, stdout=fh, stderr=subprocess.STDOUT, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    else:
        p = launch(cmd, str(scratch), env, out)
    RUNS.update(p=p, run=run)
    rec = dict(run=run, since=f'{now:%Y-%m-%d %H:%M:%S}', ids=ids, pid=getattr(p, 'pid', 0))
    put(state() / 'running.json', rec)
    return rec


def finish(where: Path, rec, why='', now=None):
    """A run ended: its line in spend.jsonl, and for every task it was given and left without a brief one failed
    reading written down (TRIES of them and the task is listed bare)."""
    now = now or datetime.datetime.now()
    usd = 0.0
    try:
        raw = (state() / 'runs' / f'{rec["run"]}.json').read_text(encoding='utf-8', errors='replace')
        got = json.loads(raw[raw.index('{'):])
        usd = float(got.get('total_cost_usd') or 0)
        if got.get('is_error'):
            why = why or f'the session ended on an error: {one(got.get("result"))[:160]}'
    except (OSError, ValueError):
        why = why or 'the session left no result'
    rows = load(state() / 'runs' / f'{rec["run"]}.rows.json') or {}
    made = [i for i in rec['ids'] if i in rows and current(where, rows[i])]
    for i in rec['ids']:
        if i in rows and i not in made:
            w = waits(where, rows[i], now.timestamp())
            w.update(n=w.get('n', 0) + 1, why=why or 'the run wrote no brief for it')
            try:
                put(where / i / 'wait.json', w)
            except OSError:
                pass
    line = dict(when=f'{now:%Y-%m-%d %H:%M}', run=rec['run'], ids=rec['ids'], made=len(made), usd=round(usd, 2), why=why, host=socket.gethostname())
    where.mkdir(parents=True, exist_ok=True)
    with open(where / 'spend.jsonl', 'a', encoding='utf-8') as f:
        f.write(json.dumps(line, sort_keys=True) + '\n')
    try:
        (state() / 'running.json').unlink()
    except OSError:
        pass
    RUNS.clear()
    old = sorted(d for d in (state() / 'runs').iterdir() if d.is_dir())[:-20] if (state() / 'runs').is_dir() else []
    for d in old:                                   # the last twenty runs' folders are kept to look at
        shutil.rmtree(d, ignore_errors=True)
    return line


def tick(T, where: Path = None, now=None, launch=None, alive=None, lim=None, host=None, only=None):
    """What the watcher does on every read: look whether the run that is going has ended (or ran past its minutes:
    then it is stopped), and when none runs and tasks wait to be read, start one for the next BATCH. `only` reads
    just those ids. Returns what the page is told: {running, waiting, left, last, off}."""
    import ideas
    where, now, host = where or folder(), now or datetime.datetime.now(), host or socket.gethostname()
    lim, day = lim or limits(), f'{now:%Y-%m-%d}'
    rec, last, off = load(state() / 'running.json'), None, ''
    if rec:
        p = RUNS.get('p') if RUNS.get('run') == rec['run'] else None
        age = (now - datetime.datetime.strptime(rec['since'], '%Y-%m-%d %H:%M:%S')).total_seconds() / 60
        going = (p.poll() is None) if p is not None else (alive or ideas.pid_alive)(rec)
        if going and age >= lim['minutes']:
            ideas.stop(rec, p) if (p is not None or alive is None) else None
            last, rec = finish(where, rec, why=f'stopped after {lim["minutes"]} minutes', now=now), None
        elif not going:
            last, rec = finish(where, rec, now=now), None
    want = [r for r in wanted(T, where, host, now.timestamp()) if not only or r['id'] in only]
    left = max(0, lim['runs_per_day'] - len(spent(where, day)))
    if not rec and want:
        if os.environ.get('TW_TASKBRIEF_OFF') and launch is None:
            off = 'the reading is switched off on this station (TW_TASKBRIEF_OFF)'       # the tests of other tools: no session is ever started from one
        elif not left:
            off = f'the day\'s {lim["runs_per_day"]} readings are used'
        else:
            try:
                rec = start(where, want[:BATCH], now=now, launch=launch, lim=lim, host=host)
            except (ValueError, OSError) as e:
                off = str(e)
    return dict(running=dict(since=rec['since'], ids=rec['ids']) if rec else None, waiting=len(want), left=max(0, left - (1 if rec else 0)), last=last, off=off)


# ---- the command -----------------------------------------------------------------------------------------------------

def rows_given(path=''):
    """The tasks, whole: what a run was given (TW_TASKBRIEF_ROWS), a file named, else read now as a session reads them."""
    path = path or os.environ.get('TW_TASKBRIEF_ROWS', '')
    if path:
        return load(Path(path)) or {}
    import ops
    import tasks
    return {r['id']: r for r in tasks.read(board=ops.board_root(), live=False)['rows']}


def main(argv=None):
    ap = argparse.ArgumentParser(description='what a task on the board is about, read before it is listed')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'context', 'add', 'shoot', 'tick'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--title', default='', help=f'what the task is, plainly, {TITLE} characters at most')
    ap.add_argument('--about', default='', help=f'what the work is and what it is for, {WORDS["about"]} words at most')
    ap.add_argument('--part-of', default='', help=f'the owner\'s request or the larger work it belongs to, {WORDS["part_of"]} words at most')
    ap.add_argument('--stands', default='', help=f'what was done and what is left, {WORDS["stands"]} words at most')
    ap.add_argument('--link', action='append', default=[], help='KIND=LABEL=TARGET; kinds: ' + ', '.join(LINK_KINDS))
    ap.add_argument('--picture', action='append', default=[], help='PATH=what it shows; a picture or film `context` listed, or one made in the run\'s folder')
    ap.add_argument('--no-picture', default='', help='why nothing can be shown')
    ap.add_argument('--no-link', default='', help='why there is nothing to point to')
    ap.add_argument('--rows', default='', help='a file of the tasks, whole (a run is given one)')
    ap.add_argument('--only', action='append', default=[], help='tick: read just this task')
    ap.add_argument('--json', action='store_true')
    a = ap.parse_args(argv)
    where, run, scratch = folder(), os.environ.get('TW_TASKBRIEF_RUN', ''), os.environ.get('TW_TASKBRIEF_SCRATCH', '')
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(errors='replace')
    try:
        if run and a.what not in IN_A_RUN:
            raise ValueError(f'{a.what} is not for a run the board started ({", ".join(IN_A_RUN)})')
        if a.what in ('context', 'add'):
            if len(a.args) != 1:
                raise ValueError(f'{a.what} ID')
            rows = rows_given(a.rows)
            r = rows.get(a.args[0])
            if not r:
                raise ValueError(f'{a.args[0]} is not a task ' + ('this run was given: ' + ', '.join(rows) if run else 'on the board'))
            if a.what == 'context':
                d = remember(digest(r))
                print(json.dumps(d, indent=1, sort_keys=True) if a.json else '\n'.join(digest_lines(d)))
            else:
                d = load(state() / 'digests' / f'{r["id"]}.json') or remember(digest(r))
                links = [(l.split('=', 2) + ['', ''])[:3] for l in a.link]
                pictures = [(p.rsplit('=', 1) + [''])[:2] for p in a.picture]
                b = add(where, r, a.title, a.about, a.part_of, a.stands, links, pictures, a.no_picture, a.no_link, may_show=[p['path'] for p in d.get('pictures') or []], scratch=scratch, run=run)
                print(f'taskbrief: {b["id"]} has its brief ({len(b["links"])} links, {len(b["pictures"])} pictures), in {where / b["id"]}')
        elif a.what == 'shoot':
            if len(a.args) != 2:
                raise ValueError('shoot PAGE.html OUT.png')
            if scratch and Path(scratch).resolve() not in Path(a.args[1]).resolve().parents:
                raise ValueError(f'in a run a picture is made in the run\'s own folder, {scratch}')
            print(f'taskbrief: the picture is in {briefs.shoot(Path(a.args[0]), Path(a.args[1]))}; give it with --picture')
        elif a.what == 'tick':
            import ops
            import tasks
            T = tasks.read(board=ops.board_root(), live=False)
            s = tick(T, where, only=a.only)
            print(f'taskbrief: {"a run is going since " + s["running"]["since"] + " for " + ", ".join(s["running"]["ids"]) if s["running"] else "no run"}; {s["waiting"]} wait to be read; {s["left"]} runs left today'
                  + (f'; {s["off"]}' if s['off'] else '') + (f'; the last run made {s["last"]["made"]} of {len(s["last"]["ids"])} for ${s["last"]["usd"]}' if s['last'] else ''))
        else:
            every = [b for b in (read(where, d.name) for d in sorted(where.iterdir()) if d.is_dir()) if b] if where.is_dir() else []
            today = spent(where, f'{datetime.datetime.now():%Y-%m-%d}')
            print(f'taskbrief: {len(every)} briefs in {where}; today {len(today)} runs, ${sum(r.get("usd", 0) for r in today):.2f}, {sum(r.get("made", 0) for r in today)} briefs made')
            for b in every:
                print(f'  {b["id"]:44} {b["when"]}  {b["title"]}')
    except ValueError as e:
        print(f'taskbrief: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
