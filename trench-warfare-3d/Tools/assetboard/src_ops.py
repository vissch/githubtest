"""Who is working on what, right now: the floor page's data.

Read from this machine, nothing typed in:
- the branches (src_git.lanes): every lane with commits integration lacks, its checkout, its last commits;
- the checkouts: what is uncommitted in each, which is work going on in it now;
- the Claude sessions: their transcripts under ~/.claude/projects. A transcript written to in the last few minutes
  is a session at work, and so is one whose subagents are still running; its folder says which checkout (so which
  branch), or, started outside every checkout, the paths its last calls name; its title and last request say on
  what, its Skill calls say which skills it has taken up, and its subagents (subagents/*.meta.json) which agents
  it sent;
- the machines: the processes running against a checkout (a Unity editor or test run, the gate, Blender, a film);
- the pipeline board (Tools/pipeline): each item's lane, its stages, the role (skill) each stage wants, a claim;
- the relay (src_relay.py): a run that is going is a worker on the branch of its unit, with the leg it is on;
- everyone else (src_floor.py): Claude's own list of running sessions, the relay's legs, the second opinions, Codex
  and Grok, and what the other station wrote of its own floor.

Work in no checkout of this repo is on the floor too, in the lane src_floor.OTHER: nobody at work is left out.

The roster is the project's skills (.claude/skills) and agents (.claude/agents, and the user's ~/.claude/agents).
A skill or agent with nothing to do is on the bench.

For the house page every worker also says which room its work is in (`act`) and every roster entry which room it
works in when called (`home`); src_acts.py holds those rules. A session's room is where its last tool calls point,
the war room while it waits on the owner or is in plan mode, the bunkhouse when it rests.
"""
import collections
import datetime
import json
import os
import re
import subprocess
import time
from pathlib import Path

import src_acts
import src_floor
import src_git
import src_relay

WORKING = 10 * 60        # a transcript written to this recently is a session at work
RECENT = 3 * 3600        # ... this recently: it was here today, shown resting on its branch
LONG = 24 * 3600         # ... not for this long: it is not asked about its agents either
SKILL_FRESH = 90 * 60    # a skill a working session took up this recently is working with it
AGENT_FRESH = 4 * 60     # a subagent whose transcript moved this recently is still running
AGENT_QUIET = 15 * 60    # ... and one quiet for up to this long, if it has not said its last word and its session still runs
TAIL = 4 * 1024 * 1024
AGENT_TAIL = 512 * 1024  # of an agent's own log: enough for its last calls
LONG_PATH = 180          # a transcript's path from this length on is opened in its long form: its agents' files lie some 70 characters deeper, and Windows takes 260
POINTS = 40              # the last tool calls of a session that are asked: which room, and which checkout
ENOUGH = 3               # ... and how many of them must name a checkout for a session started outside every one
ASKS = ('AskUserQuestion', 'ExitPlanMode')     # a call that is not over until the owner answers
PLAN_MARKS = {'plan_mode': True, 'plan_mode_reentry': True, 'plan_mode_exit': False}    # the notes a transcript keeps of going in and out of plan mode
PROJECTS = Path(os.path.expanduser('~')) / '.claude' / 'projects'
DETACHED = 'detached: '  # how a checkout on no branch is named as a lane: this and its folder's name
MACHINES = [            # (process name, a pattern in its command line, what it is doing)
    ('Unity.exe', r'\b(-runTests|test)\b', 'Unity test run'),
    ('Unity.exe', r'-batchmode', 'Unity editor, no window'),
    ('Unity.exe', r'-projectpath', 'Unity editor'),
    ('powershell.exe', r'gate\.ps1', 'the gate'),
    ('blender.exe', r'film_blender', 'Blender, rendering films'),
    ('blender.exe', r'thumb_blender', 'Blender, rendering previews'),
    ('blender.exe', r'', 'Blender'),
    ('python.exe', r'gamefilm\.py', 'filming the game'),
    ('python.exe', r'otr\.py', 'tests outside Unity'),
    ('python.exe', r'assetboard[\\/]build\.py', 'building the asset board'),
]


def fix_text(s):
    """Transcripts sometimes hold UTF-8 read as cp1252 ('â€¦' for '…')."""
    if s and 'â€' in s:
        try:
            return s.encode('cp1252').decode('utf-8')
        except (UnicodeEncodeError, UnicodeDecodeError):
            pass
    return s or ''


def short(s, n=140):
    s = re.sub(r'\s+', ' ', fix_text(s)).strip()
    return s if len(s) <= n else s[:n - 1].rstrip() + '…'


def roster(repo: Path):
    """The skills and agents, each with what it is for (its description up to the first full stop) and the room it
    works in when it is called (`home`)."""
    out = []
    for f in sorted((repo / '.claude' / 'skills').glob('*/SKILL.md')):
        t = f.read_text(encoding='utf-8', errors='replace')
        d = re.search(r'^description:\s*(.+)$', t, re.M)
        text = fix_text(d.group(1)).strip().strip('"') if d else ''
        what = re.split(r'\s[—–-]\s', text, 1)[-1]
        does = short(what.split('. ')[0], 110)
        out.append(dict(id=f.parent.name, kind='skill', name=f.parent.name, does=does, home=src_acts.of_skill(f.parent.name, does)))
    for folder, where in ((repo / '.claude' / 'agents', 'project'), (Path(os.path.expanduser('~')) / '.claude' / 'agents', 'yours')):
        for f in sorted(folder.glob('*.md')):
            t = f.read_text(encoding='utf-8', errors='replace')
            d = re.search(r'^description:\s*(.+)$', t, re.M)
            does = short(fix_text(d.group(1)) if d else '', 110)
            out.append(dict(id='agent:' + f.stem, kind='agent', name=f.stem, does=does, where=where, home=src_acts.of_agent(f.stem, does)))
    return out


def skill_for(role, skills):
    """A board stage's role (balance-simulator, critic, master) to the skill that plays it (tw-balance-sim ...)."""
    if not role:
        return None
    head = role.split('-')[0]
    for s in skills:
        parts = s.replace('tw-', '').split('-')
        if parts[0] == head or s == role or s == 'tw-' + role:
            return s
    return None


def checkouts(repo: Path):
    """path -> branch for every worktree, and the number of files each has uncommitted."""
    out, cur = {}, None
    for line in src_git.git(repo, 'worktree', 'list', '--porcelain').split('\n'):
        if line.startswith('worktree '):
            cur = Path(line[9:])
        elif line.startswith('branch refs/heads/') and cur:
            out[cur] = dict(branch=line[18:], path=str(cur), name=cur.name)
        elif line.strip() == 'detached' and cur:        # on no branch (a pinned copy, a review's checkout): still a place work is done in
            out[cur] = dict(branch=DETACHED + cur.name, path=str(cur), name=cur.name)
    for p, w in out.items():
        if w['branch'].startswith(DETACHED):        # a pinned copy, or a checkout a check moves and resets by itself: no status is run in it
            w['dirty'] = []                         # (git status takes the index lock, and a reset there must not meet it)
            continue
        st = src_git.git(p, 'status', '--porcelain', '--untracked-files=normal').split('\n')
        w['dirty'] = [l[3:] for l in st if l.strip()]
    return out


def owner_of(path, trees):
    """The checkout a path is in (the longest checkout path that holds it)."""
    try:
        p = Path(path).resolve()
    except (OSError, ValueError):
        return None
    best = None
    for root in trees:
        try:
            p.relative_to(root.resolve())
        except ValueError:
            continue
        if best is None or len(str(root)) > len(str(best)):
            best = root
    return best


def spellings(trees):
    """Each checkout with the ways a command or a path may spell it, the longest path first: with backslashes or
    forward slashes (C:/Users/x/repo) and as Git Bash does (/c/Users/x/repo), in any case. A name counts only whole:
    followed by a separator, a quote, a space or nothing, so githubtest is not found in githubtest-pipe."""
    out = []
    for root in trees:
        p = re.sub(r'/+', '/', str(root).replace('\\', '/').lower()).rstrip('/')
        forms = sorted({p, re.sub(r'^([a-z]):/', r'/\1/', p)})
        out.append((root, re.compile('(?:' + '|'.join(re.escape(f) for f in forms) + r')(?=[/\s\'"`;)|&<>,]|$)')))
    return sorted(out, key=lambda x: -len(str(x[0])))


def pointed(inp, spell):
    """The checkout a tool call points into: the one its file, its folder or its command names, or the one the task
    it hands an agent names (a session that only sends agents names its checkout nowhere else). Of several the
    longest path, so a worktree inside a checkout is itself and not the checkout around it."""
    text = ' '.join(str(inp.get(k) or '') for k in ('file_path', 'path', 'notebook_path', 'command', 'prompt', 'description'))
    text = re.sub(r'[\\/]+', '/', text.lower())
    return next((root for root, rx in spell if rx.search(text)), None)


def home_of(cwd, named, trees):
    """The checkout a session works in. Its own folder decides when that is inside one. A session started outside
    every checkout (the laptop starts them in Documents/claude and reaches the checkouts by path) belongs to the one
    most of its last calls point into (`named`: the checkout each named, or None, in the order they were made),
    when at least ENOUGH do; of two named as often, the one named last."""
    tree = owner_of(cwd, trees) if cwd else None
    if tree is None:
        hits = collections.Counter(t for t in named if t is not None)
        best = max(hits, key=lambda t: (hits[t], max(i for i, n in enumerate(named) if n == t)), default=None)
        tree = best if best is not None and hits[best] >= ENOUGH else None
    return tree


def read_tail(f: Path, tail=TAIL):
    """The entries in the last `tail` bytes of a transcript (the first line of the cut is half a line and is skipped)."""
    with open(f, 'rb') as h:
        h.seek(max(0, f.stat().st_size - tail))
        data = h.read()
    for line in data.split(b'\n'):
        try:
            d = json.loads(line)
        except ValueError:
            continue
        if isinstance(d, dict):
            yield d


def uses(d):
    """The tool calls one entry of a transcript makes."""
    if d.get('type') != 'assistant':
        return []
    return [c for c in (d.get('message') or {}).get('content') or [] if isinstance(c, dict) and c.get('type') == 'tool_use']


def ts(s):
    try:
        return datetime.datetime.fromisoformat(s.replace('Z', '+00:00')).timestamp()
    except (ValueError, AttributeError):
        return None


def reach(f: Path):
    """A path the system can open: one longer than Windows takes gets the prefix that lifts the limit (a session
    run from a deep folder has a transcript like that, and it is at work like any other)."""
    s = str(f)
    return Path('\\\\?\\' + s) if os.name == 'nt' and len(s) >= LONG_PATH and not s.startswith('\\\\') else f


def closed(log: Path):
    """Whether an agent's log ends on its last word: an answer that ends the turn and calls no tool."""
    try:
        with open(log, 'rb') as h:
            h.seek(max(0, log.stat().st_size - AGENT_TAIL))
            lines = h.read().split(b'\n')
    except OSError:
        return True
    for line in reversed(lines):
        try:
            d = json.loads(line)
        except ValueError:
            continue
        if not isinstance(d, dict) or d.get('type') not in ('assistant', 'user'):
            continue
        return d['type'] == 'assistant' and (d.get('message') or {}).get('stop_reason') == 'end_turn' and not uses(d)
    return False


def running(f: Path, now, parent_lives=False):
    """The subagents of a session that are still running: (what it was sent as, its log). One whose log moved in the
    last AGENT_FRESH seconds is; so is one whose log is quiet for up to AGENT_QUIET seconds but does not end on its
    last word, when the session that sent it still runs (`parent_lives`): an agent inside one long call, or waiting
    on agents of its own, writes nothing for a long time. Agents an agent sent lie deeper in the same folder."""
    sub = f.with_suffix('') / 'subagents'
    out = []
    for meta in sorted(sub.rglob('*.meta.json')) if sub.is_dir() else []:
        log = meta.with_name(meta.name[:-len('.meta.json')] + '.jsonl')
        try:
            age = now - log.stat().st_mtime
        except OSError:
            continue
        if age <= AGENT_FRESH or (parent_lives and age <= AGENT_QUIET and not closed(log)):
            out.append((meta, log))
    return out


def agent_key(stem):
    return stem[len('agent-'):] if stem.startswith('agent-') else stem


def agent(meta: Path, log: Path):
    """A running subagent: its type, what it was last asked, and the room it is in: where the last calls in its own
    log point, else the room of its trade. Agents of one type share an id on the floor, so each also has a `uid` of
    its own, from the name of its log. `sent_by` is the agent that sent it, when an agent did; `model` what it runs on."""
    try:
        m = json.loads(meta.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        m = {}
    kind = m.get('agentType') or 'agent'
    what = agent_task(last_ask(log), m.get('description', ''))
    calls, model = [], m.get('model') or ''
    for d in read_tail(log, AGENT_TAIL):
        model = (d.get('message') or {}).get('model') or model if d.get('type') == 'assistant' else model
        calls += [(ts(d.get('timestamp')), src_acts.of_tool(c.get('name'), c.get('input'))) for c in uses(d)]
    try:
        since = log.stat().st_ctime
    except OSError:
        since = None
    return dict(type=kind, what=what, uid=f'agent:{kind}#{agent_key(log.stem)[:6]}', key=agent_key(log.stem), sent_by=agent_key(str(m.get('parentAgentId') or '')),
                act=src_acts.pick(calls) or src_acts.of_agent(kind, what), model=model, since=since)


HEADLESS = [            # a session the board or the relay started by itself, by the folder it runs in: (pattern, name, room)
    (r'[\\/]ideas[\\/]runs[\\/]', 'ideas agent', 'plan'),
    (r'[\\/]taskbrief[\\/]runs[\\/]', 'task brief agent', 'lab'),
    (r'[\\/]runreport[\\/]', 'run reader', 'lab'),
    (r'[\\/]relay\d*[\\/]runs[\\/][^\\/]+[\\/]desk[\\/]', 'relay critic', 'lab'),
]


def headless(cwd):
    """(name, room) of a session one of the tools started by itself, or None."""
    return next(((name, room) for rx, name, room in HEADLESS if re.search(rx, (cwd or '') + '/', re.I)), None)


def sessions(trees, now, live=None, skip=()):
    """Every Claude session written to in the last RECENT seconds, or with an agent still running, or that Claude
    lists as running and busy (`live`: src_floor.registry()). When Claude lists any session at all, one it does not
    list is over: it is not at work however lately it wrote (2026-10-10: six run readers that had ended were counted
    at work for ten minutes each), and one a tool started by itself is not listed at all. Each says what it is on (title, last request, the
    skills it took up, its running agents) and, from its last POINTS tool calls: the room each is work in (`calls`,
    for src_acts.pick), whether the last one still waits for the owner's answer (`wait`), and the checkout it works
    in (`tree`): its own folder's, else the one its calls point into, else None: work in no checkout of this repo is
    work all the same, and so are the agents it sent (2026-10-09: four of five sessions and both running agents were
    left off the floor for having no checkout). `plan` is whether it is in plan mode. It is at work (`working`) when
    its transcript moved in the last WORKING seconds, or an agent of its is still running, or Claude says it is busy:
    a parent waiting on its agents writes nothing for a long time. `skip` are session ids shown as something else (a
    relay leg is its leg, not a session beside it)."""
    out, spell, live = [], spellings(trees), live or {}
    for f in PROJECTS.glob('*/*.jsonl'):
        if f.stem in skip:
            continue
        f = reach(f)
        try:
            age = now - f.stat().st_mtime
        except OSError:
            # a transcript the system cannot open (2026-10-08: every read of the board failed on one for half an
            # hour): passed over, not fatal
            continue
        mine = live.get(f.stem)
        busy = bool(mine) and mine.get('status') == 'busy'
        ended = bool(live) and not mine         # Claude lists its running sessions and this is not one: its process is over
        logs = running(f, now, bool(mine)) if age <= LONG and not ended else []
        if age > RECENT and not logs and not busy:
            continue
        s = dict(id=f.stem, age=int(age), cwd=None, title='', prompt='', doing='', skills={}, agents=[], calls=[], wait='', plan=False,
                 model='', since=(mine or {}).get('since') or None)
        last, ask = collections.deque(maxlen=POINTS), None
        for d in read_tail(f):
            s['cwd'] = d.get('cwd') or s['cwd']
            if d.get('type') == 'ai-title':
                s['title'] = d.get('aiTitle', '')
            elif d.get('type') == 'last-prompt':
                s['prompt'] = d.get('lastPrompt', '')
            elif d.get('isSidechain'):
                continue
            elif d.get('type') == 'assistant':
                s['model'] = (d.get('message') or {}).get('model') or s['model']
                for c in uses(d):
                    inp = c.get('input') if isinstance(c.get('input'), dict) else {}
                    if c.get('name') == 'Skill' and inp.get('skill'):
                        s['skills'][inp['skill'].split(':')[-1]] = (ts(d.get('timestamp')), short(inp.get('args', ''), 90))
                    s['doing'] = short(inp.get('description') or inp.get('prompt') or c.get('name', ''), 110)
                    last.append((ts(d.get('timestamp')), c.get('name'), inp))
                    ask = c.get('id') if c.get('name') in ASKS else None
            elif d.get('type') == 'user':
                if 'permissionMode' in d:              # only what the owner typed carries it
                    s['plan'] = d['permissionMode'] == 'plan'
                for c in (d.get('message') or {}).get('content') or []:
                    if not isinstance(c, dict):
                        continue
                    if c.get('type') == 'text' and c.get('text', '').startswith('<command-name>/'):
                        name = re.search(r'<command-name>/([\w:-]+)', c['text']).group(1)
                        s['skills'][name.split(':')[-1]] = (ts(d.get('timestamp')), '')
                    elif c.get('type') == 'tool_result' and ask and c.get('tool_use_id') == ask:
                        ask = None                     # the owner answered
            elif d.get('type') == 'attachment':        # a plan approved, or the mode switched, with nothing typed since
                s['plan'] = PLAN_MARKS.get((d.get('attachment') or {}).get('type'), s['plan'])
        s['calls'] = [(when, src_acts.of_tool(name, inp)) for when, name, inp in last]
        s['wait'] = 'owner' if ask else ''
        if ended and headless(s['cwd']):
            continue                            # a run a tool started and that is over has left: it does not rest here for hours
        s['tree'] = home_of(s['cwd'], [pointed(inp, spell) for _, _, inp in last], trees)
        s['agents'] = [agent(meta, log) for meta, log in logs]
        s['working'] = not ended and (age <= WORKING or bool(logs) or busy)
        out.append(s)
    return out


def session_worker(s):
    """A session as a worker on its branch, with the room it is in (`act`): the war room while it waits on the owner
    (then it says so: `wait`) or is at work in plan mode; the bunkhouse when it rests; else where its last calls
    point. A question nobody answered goes on waiting after the transcript went quiet: that is when the owner has to
    see it, so a session that waits says so for as long as it is listed, at work or not."""
    working = s['working']
    auto = headless(s.get('cwd'))               # a session a tool started by itself has that tool's name and room
    act = 'plan' if s['wait'] or (working and s['plan']) else 'bunk' if not working else src_acts.pick(s['calls']) or (auto[1] if auto else 'work')
    w = dict(kind='session', vendor='claude', id='session:' + s['id'][:8], name=auto[0] if auto else 'Claude', title=short(s['title'], 60),
             what=short(s['prompt'], 160), doing=s['doing'] if working else '', state='working' if working else 'resting', age=s['age'], act=act)
    if s['wait']:
        w['wait'] = s['wait']
    if s.get('model'):
        w['model'] = s['model']
    if s.get('since'):
        w['since'] = datetime.datetime.fromtimestamp(s['since']).strftime('%H:%M')
    if s.get('tree') is None and s.get('cwd'):
        w['where'] = Path(s['cwd']).name          # in no checkout: the folder it was started in says what it is part of
    return w


def tidy(t: str, n=60):
    """A task as a short phrase: paths and what is in brackets go, the first clause stays, cut at a word."""
    t = re.sub(r'\s*\b(in|at|from|to|under)?\s*[A-Za-z]:[\\/]\S*', '', t)
    t = re.sub(r'\s*\S*/\S+/\S*', '', t)
    t = re.split(r'\s\(|:\s|;\s|\.\s|\s[\u2014\u2013-]\s', t)[0].strip(' ,.')
    if len(t) > n:
        t = t[:n].rsplit(' ', 1)[0] + '\u2026'
    return t


def agent_task(last: str, description: str):
    """What an agent is doing: its last ask as a phrase; when that is too short to say anything ('Round 23'), the
    description it was spawned with, its number brought up to date ('Score board UI/UX round 1' -> '... round 23')."""
    t = tidy(last) if last else ''
    if len(t.split()) >= 3 or not description:
        return t or tidy(description)
    n = re.findall(r'\d+', t)
    d = tidy(description)
    if n and re.search(r'\d+', d):
        d = re.sub(r'\d+(?!.*\d)', n[-1], d)
    return d


def last_ask(log: Path):
    """The first line of the last message an agent was given (a resumed agent's new task), read from the end of its
    log in growing steps (pictures it read make the log's tail megabytes of image data)."""
    try:
        size = log.stat().st_size
    except OSError:
        return ''
    for tail in (1 << 20, 8 << 20, 48 << 20):
        with open(log, 'rb') as fh:
            fh.seek(max(0, size - tail))
            lines = fh.read().decode('utf-8', 'replace').splitlines()
        for line in reversed(lines):
            if '"type":"user"' not in line.replace(' ', '')[:400]:
                continue
            try:
                d = json.loads(line)
            except ValueError:
                continue
            content = (d.get('message') or {}).get('content')
            texts = [content] if isinstance(content, str) else [c.get('text', '') for c in content or [] if isinstance(c, dict) and c.get('type') == 'text']
            for t in texts:
                t = re.sub(r'^The coordinator sent a message while you were working:\s*', '', fix_text(t).strip())
                if t and not t.startswith(('<', '[Image', '[Request', 'Base directory for this skill')):     # ... nor a skill's own text, loaded into it
                    return t.splitlines()[0]
        if tail >= size:
            break
    return ''


def machines(trees):
    """Processes working against a checkout, with what each is doing."""
    names = sorted({m[0] for m in MACHINES})
    flt = ' -or '.join(f"$_.Name -eq '{n}'" for n in names)
    ps = (f"Get-CimInstance Win32_Process | Where-Object {{ {flt} }} | Select-Object ProcessId, Name, CommandLine, "
          "@{n='Started';e={$_.CreationDate.ToString('s')}} | ConvertTo-Json -Compress")
    try:
        r = subprocess.run(['powershell', '-NoProfile', '-Command', ps], capture_output=True, text=True, timeout=60)
        rows = json.loads(r.stdout or '[]')
    except (subprocess.SubprocessError, ValueError):
        return []
    rows = [rows] if isinstance(rows, dict) else rows
    out, seen = [], set()
    for p in rows:
        cmd = p.get('CommandLine') or ''
        if 'AssetImportWorker' in cmd:
            continue
        what = next((w for n, rx, w in MACHINES if n.lower() == (p.get('Name') or '').lower() and re.search(rx, cmd, re.I)), None)
        if what is None:
            continue
        tree = None
        for root in trees:
            for form in (str(root), str(root).replace('\\', '/'), str(root).replace('/', '\\')):
                if form.lower() in cmd.lower():
                    tree = root if tree is None or len(str(root)) > len(str(tree)) else tree
        if tree is None or (str(tree), what) in seen:
            continue
        seen.add((str(tree), what))
        out.append(dict(tree=tree, what=what, pid=p.get('ProcessId'), started=p.get('Started')))
    return out


def board(root: Path, skills):
    import sys
    sys.path.insert(0, str(root / 'Tools' / 'pipeline'))
    try:
        import pipeline
        b = pipeline.Board()
        items = b.items()
    except (SystemExit, Exception):
        return [], {}
    out = []
    for iid, item in items.items():
        try:
            states = pipeline.evaluate(item, b)
        except (SystemExit, Exception):
            states = {}
        stages = [dict(id=s['id'], station=s.get('station', ''), role=s.get('role', ''), skill=skill_for(s.get('role'), skills),
                       state=states.get(s['id'], {}).get('state', 'UNKNOWN')) for s in item['stages']]
        out.append(dict(id=iid, title=item.get('title', ''), lane=item.get('lane', ''), stages=stages))
    claims = {}
    for st in ('laptop', 'desktop'):
        try:
            c = b.claim(st)
            if c and pipeline.claim_alive(c):
                claims[st] = c
        except Exception:
            pass
    return out, claims


def relay(root: Path, now):
    """The relay's runs, read with the pipeline's own test of "is this process still the one"."""
    import sys
    sys.path.insert(0, str(root / 'Tools' / 'pipeline'))
    try:
        import pipeline
        return src_relay.collect(pipeline.board_dir(), now, lambda pid, start: bool(pid) and pipeline.proc_start(pid) == start)
    except (SystemExit, Exception):
        return dict(runs=[], last=None, does='runs work as a chain of short sessions')


def floor(trees, now, home=None, rel=None):
    """Everyone at work on this station, each (lane, worker): the Claude sessions with the skills they took up and
    the agents they sent, the machines, the relay's runs and legs, the second opinions, Codex and Grok. The lane is
    the branch of the checkout the work is in, else src_floor.OTHER. `home` gives a skill its room (the roster's);
    `rel` is relay()."""
    home, rel, rows = home or {}, rel or dict(runs=[]), []

    def tried(what, read, none=()):
        try:
            return read()
        except Exception as e:      # noqa: BLE001  one reader that meets a file it does not understand must not empty the floor
            print(f'floor: {what} could not be read ({type(e).__name__}: {e})', flush=True)
            return list(none)
    def lane_of(folder):
        tree = owner_of(folder, trees) if folder else None
        return trees[tree]['branch'] if tree is not None else src_floor.OTHER

    for run in rel['runs']:
        going = sum(1 for l in run['legs'] if l.get('state') == 'RUNNING')
        rows.append((run['lane'] or src_floor.OTHER, dict(kind='agent', vendor='claude', id='agent:relay', name='relay', what=src_relay.leg_line(run), state='working',
                                                         legs=run['legs'], run=run['run'], minutes=run['minutes'], going=going)))
    legs = tried("the relay's legs", lambda: src_floor.legs(rel['runs']))
    rows += legs
    rows += tried('the second opinions', lambda: src_floor.seconds(src_floor.second_homes(src_relay.homes() or [src_relay.home()]), now,
                                                                  lanes={r['run']: r['lane'] for r in rel['runs'] if r.get('lane')}))
    live = tried("Claude's list of sessions", src_floor.registry, {})
    for s in tried('the sessions', lambda: sessions(list(trees), now, dict(live), skip={w['session'] for _, w in legs if w.get('session')})):
        branch = trees[s['tree']]['branch'] if s['tree'] is not None else src_floor.OTHER
        me = session_worker(s)
        rows.append((branch, me))
        if not s['working']:
            continue
        for name, (when, args) in s['skills'].items():
            if when and now - when <= SKILL_FRESH:
                rows.append((branch, dict(kind='skill', id=name, name=name, what=args or short(s['title'] or s['prompt'], 100), state='working',
                                          act=home.get(name) or src_acts.of_skill(name))))
        uids = {a['key']: a['uid'] for a in s['agents']}
        for a in s['agents']:
            w = dict(kind='agent', vendor='claude', id='agent:' + a['type'], uid=a['uid'], name=a['type'], what=a['what'], state='working', act=a['act'],
                     parent=uids.get(a['sent_by']) or me['id'])
            if a['model']:
                w['model'] = a['model']
            if a['since']:
                w['since'] = datetime.datetime.fromtimestamp(a['since']).strftime('%H:%M')
            rows.append((branch, w))
    for m in tried('the machines', lambda: machines(list(trees))):
        rows.append((trees[m['tree']]['branch'], dict(kind='machine', id=f'pid:{m["pid"]}', name=m['what'], what=f'since {(m["started"] or "")[11:16]}',
                                                    state='working', act=src_acts.of_machine(m['what']))))
    for folder, w in tried('Codex', lambda: src_floor.codex(now)) + tried('Grok', lambda: src_floor.grok(now)):
        lane = lane_of(folder)
        if lane == src_floor.OTHER and folder:
            w['where'] = Path(folder).name
        rows.append((lane, w))
    return rows


def collect(repo: Path, site: Path, share=False):
    """The floor and the branches. `share`: also write this station's floor where the other station reads it."""
    now = time.time()
    root = repo / 'trench-warfare-3d'
    people = roster(repo)
    skills = [r['id'] for r in people if r['kind'] == 'skill']
    trees = checkouts(repo)
    lanes = {l['branch']: l for l in src_git.lanes(repo)}
    by_tree = {str(p): w for p, w in trees.items()}
    for w in trees.values():                        # a checkout on a branch integration already has is still a place
        lanes.setdefault(w['branch'], dict(branch=w['branch'], ahead=0, tip='', live=True, newest=True, commits=[], family=w['branch']))
    for l in lanes.values():
        tree = next((w for w in trees.values() if w['branch'] == l['branch']), None)
        l.update(checkout=tree['name'] if tree else None, path=tree['path'] if tree else None,
                 dirty=len(tree['dirty']) if tree else 0, dirty_files=(tree['dirty'][:6] if tree else []),
                 workers=[], items=[], assets=[], last=[dict(sha=c[0], date=c[1], subject=short(c[2], 100)) for c in l.get('commits', [])[:3]])
        l.pop('commits', None)
    # the assets each lane touches, from the last build of the board (pictures included)
    data = site / 'data' / 'assets.json'
    if data.exists():
        for a in json.loads(data.read_text(encoding='utf-8'))['assets']:
            pic = next((f.get('poster') for m in a['models'] for f in m.get('films', [])), None) or next((m.get('thumb') for m in a['models'] if m.get('thumb')), None)
            for l in a.get('lanes', []):
                if l.get('touches_art') and l['branch'] in lanes:
                    lanes[l['branch']]['assets'].append(dict(id=a['id'], name=a['name'], pic=pic, status=a['status']))
    items, claims = board(root, skills)
    for it in items:
        if it['lane'] in lanes:
            lanes[it['lane']]['items'].append(it)
    busy = {}                                       # roster id -> [(lane, what)]

    def put(branch, worker):
        if branch not in lanes:         # no branch of this station's: the other station's lane, or work in no checkout (`made`: the page links to no branch for it)
            lanes[branch] = dict(branch=branch, ahead=0, tip='', live=True, newest=True, checkout=None, dirty=0, dirty_files=[], workers=[], items=[], assets=[], last=[], made=True)
        lanes[branch]['workers'].append(worker)

    home = {r['id']: r['home'] for r in people}     # a skill at work is in the room the roster gives it
    rel = relay(root, now)
    mine = floor(trees, now, home, rel)
    if share:
        src_floor.publish(mine, now)
    stations, theirs = src_floor.others(now)
    for lane, w in mine + theirs:
        put(lane, w)
        if w['kind'] in ('skill', 'agent') and w['state'] == 'working':
            busy.setdefault(w['id'], []).append(lane)
    for st, c in claims.items():
        it = next((i for i in items if c.get('job', '').startswith(i['id'])), None)
        stage = next((s for s in it['stages'] if s['id'] in c.get('job', '')), None) if it else None
        if it and stage:
            sk = stage['skill'] or stage['role']
            put(it['lane'], dict(kind='skill' if stage['skill'] else 'role', id=sk, name=sk, what=f'{it["id"]}: {stage["id"]} on the {st}', state='working',
                                 act=home.get(sk) or src_acts.of_skill(sk)))
            busy.setdefault(sk, []).append(it['lane'])
    people.append(dict(id='agent:relay', kind='agent', name='relay', does=rel['does'], where='project'))
    for r in people:
        r['busy'] = busy.get(r['id'], [])
    ordered = sorted(lanes.values(), key=lambda l: (-sum(w['state'] == 'working' for w in l['workers']), -len(l['workers']), -len(l['items']),
                                                     -(l['dirty'] > 0), '' if not l['tip'] else ''.join(chr(255 - ord(c)) for c in l['tip']), l['branch']))
    return dict(now=datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'), lanes=ordered, roster=people,
                integration=src_git.INTEGRATION[7:], relay=rel, host=src_floor.host(), stations=stations,
                floor=src_floor.count([w for l in ordered for w in l['workers']]), counts=dict(
                    sessions=sum(1 for l in ordered for w in l['workers'] if w['kind'] == 'session' and w['state'] == 'working'),
                    machines=sum(1 for l in ordered for w in l['workers'] if w['kind'] == 'machine'),
                    ready=sum(1 for l in ordered for it in l['items'] for s in it['stages'] if s['state'] == 'READY'),
                    lanes=sum(1 for l in ordered if not l.get('made')), idle=sum(1 for r in people if not r['busy'])))
