"""Who is working on what, right now: the floor page's data.

Read from this machine, nothing typed in:
- the branches (src_git.lanes): every lane with commits integration lacks, its checkout, its last commits;
- the checkouts: what is uncommitted in each, which is work going on in it now;
- the Claude sessions: their transcripts under ~/.claude/projects. A transcript written to in the last few minutes
  is a session at work; its folder says which checkout (so which branch), its title and last request say on what,
  its Skill calls say which skills it has taken up, and its subagents (subagents/*.meta.json) which agents it sent;
- the machines: the processes running against a checkout (a Unity editor or test run, the gate, Blender, a film);
- the pipeline board (Tools/pipeline): each item's lane, its stages, the role (skill) each stage wants, a claim.

The roster is the project's skills (.claude/skills) and agents (.claude/agents, and the user's ~/.claude/agents).
A skill or agent with nothing to do is on the bench.
"""
import datetime
import json
import os
import re
import subprocess
import time
from pathlib import Path

import src_git

WORKING = 10 * 60        # a transcript written to this recently is a session at work
RECENT = 3 * 3600        # ... this recently: it was here today, shown resting on its branch
SKILL_FRESH = 90 * 60    # a skill a working session took up this recently is working with it
AGENT_FRESH = 4 * 60     # a subagent whose transcript moved this recently is still running
TAIL = 4 * 1024 * 1024
PROJECTS = Path(os.path.expanduser('~')) / '.claude' / 'projects'
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
    """The skills and agents, each with what it is for (its description up to the first full stop)."""
    out = []
    for f in sorted((repo / '.claude' / 'skills').glob('*/SKILL.md')):
        t = f.read_text(encoding='utf-8', errors='replace')
        d = re.search(r'^description:\s*(.+)$', t, re.M)
        text = fix_text(d.group(1)).strip().strip('"') if d else ''
        what = re.split(r'\s[—–-]\s', text, 1)[-1]
        out.append(dict(id=f.parent.name, kind='skill', name=f.parent.name, does=short(what.split('. ')[0], 110)))
    for folder, where in ((repo / '.claude' / 'agents', 'project'), (Path(os.path.expanduser('~')) / '.claude' / 'agents', 'yours')):
        for f in sorted(folder.glob('*.md')):
            t = f.read_text(encoding='utf-8', errors='replace')
            d = re.search(r'^description:\s*(.+)$', t, re.M)
            out.append(dict(id='agent:' + f.stem, kind='agent', name=f.stem, does=short(fix_text(d.group(1)) if d else '', 110), where=where))
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
    for p, w in out.items():
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


def read_tail(f: Path):
    with open(f, 'rb') as h:
        h.seek(max(0, f.stat().st_size - TAIL))
        data = h.read()
    for line in data.split(b'\n'):
        try:
            yield json.loads(line)
        except ValueError:
            continue


def ts(s):
    try:
        return datetime.datetime.fromisoformat(s.replace('Z', '+00:00')).timestamp()
    except (ValueError, AttributeError):
        return None


def sessions(trees, now):
    """Every Claude session written to in the last RECENT seconds that works in one of these checkouts."""
    out = []
    for f in PROJECTS.glob('*/*.jsonl'):
        age = now - f.stat().st_mtime
        if age > RECENT:
            continue
        s = dict(id=f.stem, age=int(age), cwd=None, title='', prompt='', doing='', skills={}, agents=[])
        for d in read_tail(f):
            s['cwd'] = d.get('cwd') or s['cwd']
            if d.get('type') == 'ai-title':
                s['title'] = d.get('aiTitle', '')
            elif d.get('type') == 'last-prompt':
                s['prompt'] = d.get('lastPrompt', '')
            elif d.get('type') == 'assistant' and not d.get('isSidechain'):
                for c in (d.get('message') or {}).get('content') or []:
                    if not isinstance(c, dict) or c.get('type') != 'tool_use':
                        continue
                    inp = c.get('input') or {}
                    if c.get('name') == 'Skill' and inp.get('skill'):
                        s['skills'][inp['skill'].split(':')[-1]] = (ts(d.get('timestamp')), short(inp.get('args', ''), 90))
                    s['doing'] = short(inp.get('description') or inp.get('prompt') or c.get('name', ''), 110)
            elif d.get('type') == 'user' and not d.get('isSidechain'):
                for c in (d.get('message') or {}).get('content') or []:
                    if isinstance(c, dict) and c.get('type') == 'text' and c.get('text', '').startswith('<command-name>/'):
                        name = re.search(r'<command-name>/([\w:-]+)', c['text']).group(1)
                        s['skills'][name.split(':')[-1]] = (ts(d.get('timestamp')), '')
        tree = owner_of(s['cwd'], trees) if s['cwd'] else None
        if tree is None:
            continue
        s['tree'] = tree
        sub = f.with_suffix('') / 'subagents'
        for meta in sub.glob('*.meta.json') if sub.is_dir() else []:
            log = meta.with_name(meta.name[:-len('.meta.json')] + '.jsonl')
            if log.exists() and now - log.stat().st_mtime <= AGENT_FRESH:
                m = json.loads(meta.read_text(encoding='utf-8'))
                s['agents'].append(dict(type=m.get('agentType', 'agent'), what=short(last_ask(log) or m.get('description', ''), 90)))
        out.append(s)
    return out


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
                if t and not t.startswith(('<', '[Image', '[Request')):
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


def collect(repo: Path, site: Path):
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
        l.update(checkout=tree['name'] if tree else None, dirty=len(tree['dirty']) if tree else 0, dirty_files=(tree['dirty'][:6] if tree else []),
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
        if branch not in lanes:
            lanes[branch] = dict(branch=branch, ahead=0, tip='', live=True, newest=True, checkout=None, dirty=0, dirty_files=[], workers=[], items=[], assets=[], last=[])
        lanes[branch]['workers'].append(worker)

    for s in sessions(list(trees), now):
        branch = trees[s['tree']]['branch']
        working = s['age'] <= WORKING
        put(branch, dict(kind='session', id='session:' + s['id'][:8], name='Claude', title=short(s['title'], 60), what=short(s['prompt'], 160),
                         doing=s['doing'] if working else '', state='working' if working else 'resting', age=s['age']))
        if not working:
            continue
        for name, (when, args) in s['skills'].items():
            if when and now - when <= SKILL_FRESH:
                put(branch, dict(kind='skill', id=name, name=name, what=args or short(s['title'] or s['prompt'], 100), state='working'))
                busy.setdefault(name, []).append(branch)
        for a in s['agents']:
            aid = 'agent:' + a['type']
            put(branch, dict(kind='agent', id=aid, name=a['type'], what=a['what'], state='working'))
            busy.setdefault(aid, []).append(branch)
    for m in machines(list(trees)):
        put(trees[m['tree']]['branch'], dict(kind='machine', id=f'pid:{m["pid"]}', name=m['what'], what=f'since {(m["started"] or "")[11:16]}', state='working'))
    for st, c in claims.items():
        it = next((i for i in items if c.get('job', '').startswith(i['id'])), None)
        stage = next((s for s in it['stages'] if s['id'] in c.get('job', '')), None) if it else None
        if it and stage:
            sk = stage['skill'] or stage['role']
            put(it['lane'], dict(kind='skill' if stage['skill'] else 'role', id=sk, name=sk, what=f'{it["id"]}: {stage["id"]} on the {st}', state='working'))
            busy.setdefault(sk, []).append(it['lane'])
    for r in people:
        r['busy'] = busy.get(r['id'], [])
    ordered = sorted(lanes.values(), key=lambda l: (-sum(w['state'] == 'working' for w in l['workers']), -len(l['workers']), -len(l['items']),
                                                     -(l['dirty'] > 0), '' if not l['tip'] else ''.join(chr(255 - ord(c)) for c in l['tip']), l['branch']))
    return dict(now=datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'), lanes=ordered, roster=people,
                integration=src_git.INTEGRATION[7:], counts=dict(
                    sessions=sum(1 for l in ordered for w in l['workers'] if w['kind'] == 'session' and w['state'] == 'working'),
                    machines=sum(1 for l in ordered for w in l['workers'] if w['kind'] == 'machine'),
                    ready=sum(1 for l in ordered for it in l['items'] for s in it['stages'] if s['state'] == 'READY'),
                    lanes=len(ordered), idle=sum(1 for r in people if not r['busy'])))
