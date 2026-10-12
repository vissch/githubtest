"""What every worker on the floor is doing and what it touches: data/touch.js, the graph a click on a frog opens
(static/touch.js lays it out, static/touchdraw.js draws it), made again on every read of ops.py.

    window.TOUCH = {by: {<worker uid or id>: {nodes, links, log, counts, ...}}, at}

Read from this machine, nothing typed in:
- a Claude session or one of its agents: the tool calls in its transcript (~/.claude/projects). Each call is a touch:
  a file read or changed, a command run (and the files the command names), a search, an agent sent, a skill taken up,
  a page fetched, a message. The last CALLS calls are kept. A file in a checkout hangs under that checkout (its
  branch); a file anywhere else under the folder it is in. More than NODES things, and the oldest files are folded
  into their folder;
- a relay leg, and the relay itself: the run's own records as the Runs page has them (data/runs.js): the run, its
  units, each unit's lane, the commits its legs made and the files their reports name. The legs run on the other
  station, so what one call of a leg touched is not here; `relay()` says where a published digest would go.

A transcript is read once. A cache keeps, per file, how far it was read and the calls that count, and the next read
takes only what was written since (a session's transcript is hundreds of megabytes; on first sight only its tail is
read). Workers that left the floor stay for GONE seconds, and the agents a worker sent that are over stay reachable
from the node that stands for them.
"""
import hashlib
import json
import os
import posixpath
import re
import time
from pathlib import Path

import src_ops

VERSION = 4              # of the cache: a new number has every transcript read again (from its tail)
CALLS = 300              # the last tool calls of a worker that are kept
NODES = 120              # the most things a worker's graph shows: beyond it the oldest files fold into their folder
NODES_OVER = 48          # ... and for a worker that is over
LOG = 40                 # the lines of the running log
MARKS = 6                # the last touches a thing remembers (when, verb)
FIRST = 2 * 1024 * 1024  # of a transcript seen for the first time: its tail
FIRST_MORE = 16 * 1024 * 1024   # ... and this much when that tail held hardly a call (pictures fill a tail)
FEW = 60
ROW_MAX = 4 * 1024 * 1024
GONE = 30 * 60           # a worker that left the floor is kept this long
SENT_FRESH = 6 * 3600    # an agent that is over and whose log moved this recently can still be opened from its node
SENT_MOST = 8            # ... the newest of them, per worker
BUDGET = 272 * 1024      # of data/touch.js, in characters
HEAT = (30, 120)         # the heat strip: this many buckets of this many seconds, ending on the worker's last call
PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp', '.mp4', '.webm', '.mov', '.svg', '.psd', '.exr', '.tga')
CHANGES = ('Edit', 'MultiEdit', 'Write', 'NotebookEdit')
COUNTS = dict(Read='read', Edit='changed', MultiEdit='changed', Write='changed', NotebookEdit='changed', Bash='ran', PowerShell='ran',
              Grep='searched', Glob='searched', WebSearch='searched', ToolSearch='searched', WebFetch='read',
              Agent='sent', Task='sent', SendMessage='sent', Skill='skills')
FILES = ('read', 'changed', 'named', 'picture')     # the kinds of node that are a file
HOME = str(Path(os.path.expanduser('~'))).replace('\\', '/')
LOCALAPP = (os.environ.get('LOCALAPPDATA') or '').replace('\\', '/')
_TEMP = re.match(r'^(.+?~\d[^/]*)/', (os.environ.get('TEMP') or '').replace('\\', '/'))
SHORT = _TEMP.group(1) if _TEMP and _TEMP.group(1).rsplit('/', 1)[0].lower() == HOME.rsplit('/', 1)[0].lower() else ''

_ABS = r'(?:[A-Za-z]:[\\/]|/[a-z]/|~/)'
_OPEN = r'(?<![A-Za-z0-9_.~/\\-])'
PATHS = re.compile(r'"(' + _ABS + r'[^"\n]*)"|\'(' + _ABS + r'[^\'\n]*)\'|' + _OPEN + '(' + _ABS + r'[^\s"\'`;|&<>()*?,]+)')
EXTS = 'py|js|mjs|ts|css|html|cs|json|jsonl|md|ps1|sh|txt|yml|yaml|toml|shader|hlsl|asset|unity|prefab|meta|xml|csv|png|jpg|jpeg|gif|webp|mp4|webm|fbx|blend|asmdef'
RELS = re.compile(r'(?<![\w/\\.:~$%@-])((?:\.\.?/)*(?:[\w.@-]+/)+[\w.@-]+\.(?:' + EXTS + r'))(?![\w/])')
CD = re.compile(r'(?:^|[;&|\n(]\s*)(?:cd|pushd|Set-Location|sl)\s+(?:-\w+\s+)?(?:"([^"\n]+)"|\'([^\'\n]+)\'|([^\s;&|]+))')
SHELLS = ('python', 'python3', 'py', 'node', 'bash', 'sh', 'powershell', 'pwsh', 'blender', 'unity')
SUBS = ('git', 'gh', 'npm', 'npx', 'docker', 'ssh', 'dotnet', 'pip', 'cargo', 'ffmpeg', 'curl')
INLINE = re.compile(r'<<-?\s*[\'"]?\w+|\s-c\s+["\']|\s-e\s+["\']')
QUIET = ('cd', 'export', 'set', 'echo', 'sleep', 'source', 'pushd', 'popd', 'true', 'mkdir', 'for', 'while', 'until', 'done', 'fi')
BEFORE = ('time', 'timeout', 'env', 'nohup', 'sudo', 'exec', 'command', 'call', 'start', 'do', 'then', 'if', 'else', '!')     # words before the program that are not it


def nid(letter, key):
    """A thing's id: the same thing has the same id for every worker and on every read (the page keeps a node where
    it stood by it), in eight characters."""
    return letter + hashlib.sha1(str(key).lower().encode('utf-8', 'replace')).hexdigest()[:7]


def norm(p, cwd=''):
    """A path as tools spell it, in one form: forward slashes, the drive as a capital with a colon, a home as its
    folder, a relative one from `cwd` (empty when it cannot be placed)."""
    p = re.sub(r'/+', '/', str(p or '').strip().strip('"\'').replace('\\', '/'))
    if p.startswith('~/'):
        p = HOME + p[1:]
    m = re.match(r'^/([a-zA-Z])/(.*)', p)
    if m:
        p = m.group(1) + ':/' + m.group(2)
    if not re.match(r'^[A-Za-z]:/', p):
        if not cwd or p.startswith('/') or not p:
            return ''
        p = norm(cwd).rstrip('/') + '/' + p
    p = posixpath.normpath(p)
    p = p[0].upper() + p[1:]
    if SHORT and p.lower().startswith(SHORT.lower() + '/'):      # the home in its short spelling (THOMAS~1.VIS) is the home
        p = HOME + p[len(SHORT):]
    return p


def place(p, spell):
    """(checkout, path inside it) of a path in one of the checkouts; else ('', the path made short: the home as ~,
    this session's scratch folder as scratchpad, the station's cache by its name)."""
    low = p.lower()
    for root, rx in spell:
        m = rx.match(low)
        if m:
            return str(root), p[m.end():].lstrip('/')
    m = re.match(r'(?i).*/temp/claude/[^/]+/[^/]+/scratchpad(?:/(.*))?$', p)
    if m:
        return '', 'scratchpad/' + (m.group(1) or '')
    for head, name in ((LOCALAPP, '%LOCALAPPDATA%'), (HOME, '~')):
        if head and low.startswith(head.lower() + '/'):
            return '', name + p[len(head):]
    return '', p


def spot(text):
    """The folder a file outside every checkout is shown under: its first three folders, the scratch folder whole."""
    dirs = text.split('/')[:-1]
    return '/'.join(dirs[:1 if dirs[:1] == ['scratchpad'] else 3]) or text


def command_key(cmd):
    """A command by what it runs, so forty runs of one script are one thing: the program, with the script it is
    given (python test_touch.py) or its first word (git status)."""
    bare = re.sub(r'"[^"\n]*"|\'[^\'\n]*\'', lambda m: m.group(0)[0] + re.sub(r'[;|&]', '_', m.group(0)[1:-1]) + m.group(0)[0], cmd or '')      # a bar inside quotes splits nothing
    parts = [s.strip() for s in re.split(r'&&|\|\||[;\n|]', bare) if s.strip()]
    words = []
    for seg in parts:
        w = [t for t in re.findall(r'"[^"]*"|\'[^\']*\'|\S+', seg) if not re.match(r'^[A-Za-z_]\w*=', t)]
        while w and (w[0] == '&' or base(w[0]) in BEFORE or re.match(r'^[-$\d=(]', w[0])):
            w = w[1:]
        if w and base(w[0]) not in QUIET:
            words = w
            break
    if not words:
        words = re.findall(r'\S+', parts[0]) if parts else []
    if not words:
        return 'a command'
    prog = base(words[0])
    rest = [t.strip('"\'') for t in words[1:]]
    if prog in SHELLS:
        script = next((t for t in rest if not t.startswith('-')), '')
        flag = next((t for t in rest if t in ('-c', '-', '-Command', '-e')), '')
        nxt = base(script, keep=True) if script and not (flag and rest.index(flag) < rest.index(script)) else 'inline'
        return f'{prog} {nxt}'[:44]
    if prog in SUBS:
        sub = next((t for t in rest if not t.startswith('-')), '')
        return f'{prog} {sub}'.strip()[:44]
    return prog[:44]


def base(t, keep=False):
    t = re.split(r'[\\/]', t.strip('"\''))[-1]
    return t if keep else re.sub(r'\.(exe|cmd|bat)$', '', t, flags=re.I).lower()


def in_command(cmd, cwd, spell):
    """The files a command names, and the checkout it works in: paths written whole, and paths relative to the
    folder it changes into (else to the folder the call was made in). Most calls are commands, so a worker's files
    would be undercounted without them."""
    found, seen, where = [], set(), ''
    cmd = INLINE.split(cmd, 1)[0]                # code written into the command names files it does not touch
    here = norm(cwd) if cwd else ''
    moves = [(0, here)]                          # (from where in the command, the folder it is in from there on)
    for m in CD.finditer(cmd):
        to = norm(next(g for g in m.groups() if g), here)
        if to:
            here = to
            moves.append((m.end(), here))
    where = next((t for t in (place(d, spell)[0] for _, d in reversed(moves) if d) if t), '')
    def add(p):
        nonlocal where
        p = p.rstrip('.,:)]}')
        if not p or p.lower() in seen:
            return
        seen.add(p.lower())
        tree, rel = place(p, spell)
        where = where or tree
        if re.search(r'\.[A-Za-z0-9]{1,8}$', p.rsplit('/', 1)[-1]) and len(found) < 6:
            found.append((tree, rel))
    for m in PATHS.finditer(cmd):
        add(norm(next(g for g in m.groups() if g)))
    for m in RELS.finditer(cmd):
        at = [d for pos, d in moves if pos <= m.start()][-1]
        if at:
            add(norm(m.group(1), at))
    return found, where


def file_kind(rel):
    return 'picture' if rel.lower().endswith(PICTURES) else None


def things(name, inp, cwd, spell):
    """What one tool call touches: (verb, [(kind, checkout, text, more)]), the first the one the log names."""
    def a_file(key, kind, verb):
        p = norm(inp.get(key) or '', cwd)
        if not p:
            return verb, [('tool', '', name, '')]
        tree, rel = place(p, spell)
        return verb, [(file_kind(rel) or kind, tree, rel, '')]
    if name == 'Read':
        verb, out = a_file('file_path', 'read', 'read')
        return ('looked at' if out[0][0] == 'picture' else verb), out
    if name in ('Edit', 'MultiEdit'):
        return a_file('file_path', 'changed', 'edited')
    if name == 'NotebookEdit':
        return a_file('notebook_path', 'changed', 'edited')
    if name == 'Write':
        return a_file('file_path', 'changed', 'wrote')
    if name in ('Grep', 'Glob'):
        p = norm(inp.get('path') or '', cwd) or (norm(cwd) if cwd else '')
        tree, rel = place(p, spell) if p else ('', '')
        return 'searched', [('search', tree, str(inp.get('pattern') or '')[:80], rel or '')]
    if name in ('Bash', 'PowerShell'):
        cmd = str(inp.get('command') or '')
        found, where = in_command(cmd, cwd, spell)
        said = src_ops.short(str(inp.get('description') or ''), 110)
        return 'ran', [('command', where, command_key(cmd), said or src_ops.short(cmd, 110))] + [(file_kind(rel) or 'named', tree, rel, '') for tree, rel in found]
    if name in ('Agent', 'Task'):
        return 'sent', [('subagent', '', src_ops.short(str(inp.get('description') or inp.get('prompt') or 'an agent'), 70), str(inp.get('subagent_type') or 'general-purpose'))]
    if name == 'SendMessage':
        return 'messaged', [('message', '', str(inp.get('to') or inp.get('recipient') or 'an agent')[:40], src_ops.short(str(inp.get('message') or inp.get('content') or ''), 110))]
    if name == 'AskUserQuestion':
        return 'asked', [('message', '', 'the owner', '')]
    if name == 'Skill':
        return 'took up', [('skill', '', str(inp.get('skill') or 'a skill').split(':')[-1][:40], src_ops.short(str(inp.get('args') or ''), 110))]
    if name == 'WebFetch':
        url = str(inp.get('url') or '')
        return 'fetched', [('web', '', re.sub(r'^https?://(www\.)?', '', url).split('/')[0][:40] or 'the web', url[:160])]
    if name == 'WebSearch':
        return 'searched the web', [('web', '', str(inp.get('query') or '')[:60], '')]
    if name == 'ToolSearch':
        return 'searched', [('search', '', str(inp.get('query') or '')[:60], 'for a tool')]
    m = re.match(r'^mcp__(.+?)__(.+)$', name or '')
    return 'called', [('tool', '', f'{m.group(1)}: {m.group(2)}'[:44] if m else str(name or 'a tool')[:44], '')]


def take(f: Path, entry, spell, side=False, first=FIRST, more=FIRST_MORE):
    """Read what was written to a transcript since the last time, and add its tool calls to the file's entry: `ev`,
    the last CALLS calls, each {t, n, v, x: [(kind, checkout, text, more)], i}; `sent`, the agent each Agent call
    started (the call's id -> the agent's); `all`, how many calls were read in all. Seen for the first time, only its
    tail is read. A file that got shorter (it was rewritten) is read again. Says whether anything was read."""
    size = f.stat().st_size
    off = entry.get('off')
    if off is not None and size < off:
        entry.clear()
        off = None
    if off is not None and size == off:
        return False
    for tail in ((first, more) if off is None else (0,)):
        start = max(0, size - tail) if off is None else off
        with open(f, 'rb') as h:
            h.seek(start)
            data = h.read()
        if off is None and start:
            cut = data.find(b'\n') + 1           # the first line of the cut is half a line
            data, start = data[cut:], start + cut
        end = data.rfind(b'\n') + 1              # a last line still being written waits for the next read
        ev, sent, n = [], {}, 0
        for row in data[:end].split(b'\n'):
            use, res = b'"tool_use"' in row, b'"agentId"' in row
            if not (use or res) or len(row) > ROW_MAX:
                continue
            try:
                d = json.loads(row)
            except ValueError:
                continue
            if not isinstance(d, dict):
                continue
            if res and isinstance(d.get('toolUseResult'), dict) and d['toolUseResult'].get('agentId'):
                for c in (d.get('message') or {}).get('content') or []:
                    if isinstance(c, dict) and c.get('tool_use_id'):
                        sent[c['tool_use_id']] = str(d['toolUseResult']['agentId'])
            when = src_ops.ts(d.get('timestamp'))
            if when is None or (d.get('isSidechain') and not side):      # an agent's calls are in the agent's own log
                continue
            for c in src_ops.uses(d):
                inp = c.get('input') if isinstance(c.get('input'), dict) else {}
                verb, xs = things(c.get('name') or '', inp, d.get('cwd') or '', spell)
                e = dict(t=int(when), n=c.get('name') or '', v=verb, x=[list(x) for x in xs])
                if e['n'] in ('Agent', 'Task'):
                    e['i'] = c.get('id') or ''
                ev.append(e)
                n += 1
        if off is not None or len(ev) >= FEW or start == 0 or tail == more:
            break
    was = entry.get('ev') or []
    entry['more'] = bool(entry.get('more')) or (off is None and start > 0) or len(was) + len(ev) > CALLS
    entry['ev'] = (was + ev)[-CALLS:]
    entry['sent'] = dict(list({**(entry.get('sent') or {}), **sent}.items())[-CALLS:])
    entry['all'] = int(entry.get('all') or 0) + n
    entry['off'] = start + end
    return True


def branch_short(b):
    return re.sub(r'^lane/(show|sim)/', '', b or '')


def graph(entry, branches, cap=NODES, log=LOG, uid_of=None):
    """A file's calls as the page's graph: nodes, links, the log, the counts and the heat strip. `branches` is
    {checkout path: branch}; `uid_of` turns a started agent's id and type into the name it has on the floor."""
    ev, sent = entry.get('ev') or [], entry.get('sent') or {}
    nodes, counts = {}, dict(read=0, changed=0, ran=0, searched=0, sent=0, skills=0)
    verbs = {}           # node id -> the verbs it was touched with (a file's kind is told from them)

    def node(i, kind, label, full, when, verb, p='', **more):
        n = nodes.get(i)
        if n is None:
            n = nodes[i] = dict(id=i, kind=kind, label=label, full=full, n=0, last=0, t=[], **more)
            if p:
                n['p'] = p
        n['n'] += 1
        n['last'] = max(n['last'], when)
        if verb:
            n['t'] = (n['t'] + [[when, verb]])[-MARKS:]
            verbs.setdefault(i, set()).add(verb)
        return n

    def head(tree, rel, when):
        """The node a thing hangs under: its checkout, or the folder outside every checkout."""
        if tree:
            b = branches.get(tree) or ''
            name = Path(tree).name
            node(nid('c', tree), 'checkout', branch_short(b) if b and not b.startswith(src_ops.DETACHED) else name, (b + ' · ' if b else '') + name, when, '', ref=b)
            return nid('c', tree)
        if not rel or '/' not in rel:
            return ''
        s = spot(rel)
        node(nid('p', s), 'folder', s.split('/')[-1] if len(s) > 28 else s, s, when, '')
        return nid('p', s)

    lines = []
    for e in ev:
        when, verb = e['t'], e['v']
        tally = COUNTS.get(e['n'], 'other')
        counts[tally] = counts.get(tally, 0) + 1
        first = None
        for k, (kind, tree, text, more) in enumerate(e['x']):
            if kind in FILES:
                i = nid('f', tree + '|' + text)
                n = node(i, kind, text.rsplit('/', 1)[-1] or text, text, when, verb if k == 0 else 'named in a command', head(tree, text, when))
                n['_at'] = (tree, text)
            elif kind == 'command':
                i = nid('x', text)
                n = node(i, kind, text, more, when, verb)
                n['full'] = more or n['full']
                if tree:
                    head(tree, '', when)
            elif kind == 'search':
                i = nid('q', text + '|' + tree + '|' + more)
                where = more
                n = node(i, kind, text or '(everything)', (text + ' in ' + where) if where else text, when, verb, head(tree, '', when) if tree else '')
            elif kind == 'subagent':
                i = nid('s', e.get('i') or text)
                n = node(i, kind, text, more + ' · ' + text, when, verb)
                key = sent.get(e.get('i') or '')
                if key:
                    n['to'] = uid_of(key, more) if uid_of else f'agent:{more}#{key[:6]}'
            else:
                i = nid(kind[0] if kind != 'tool' else 't', kind + '|' + text)
                n = node(i, kind, text, more or text, when, verb)
                n['full'] = more or n['full']
            first = first or n
        lines.append([when, verb, first['id'] if first else '', first['label'] if first else e['n']])
    for i, n in nodes.items():              # a file is what was done to it: changed wins over read, read over named
        if n['kind'] in ('read', 'changed', 'named'):
            v = verbs.get(i, set())
            n['kind'] = 'changed' if v & {'edited', 'wrote'} else 'read' if 'read' in v else 'named'
    into = fold(nodes, cap)
    out = []
    for n in nodes.values():
        n.pop('_at', None)
        if not n['t']:
            n.pop('t')
        out.append(n)
    links = [dict(a=n.get('p') or 'core', b=n['id'], n=n['n'], last=n['last'], verb=n['t'][-1][1] if n.get('t') else '') for n in out]
    last = ev[-1]['t'] if ev else 0
    heat = [[0] * HEAT[0] for _ in range(5)]
    rows = dict(read=0, changed=1, ran=2, searched=3, sent=4)
    for e in ev:
        k, row = (last - e['t']) // HEAT[1], rows.get(COUNTS.get(e['n'], ''))
        if row is not None and 0 <= k < HEAT[0]:
            heat[row][HEAT[0] - 1 - k] += 1
    files = [n for n in out if n['kind'] in FILES]
    return dict(nodes=out, links=links, log=[dict(at=t, verb=v, thing=src_ops.short(what, 60), id=into.get(i, i)) for t, v, i, what in lines[-log:]],
                counts=counts, counters=[['read', counts['read']], ['changed', counts['changed']], ['ran', counts['ran']], ['sent out', counts['sent']]],
                heat=heat, first=ev[0]['t'] if ev else 0, last=last, calls=len(ev), more=bool(entry.get('more')),
                files=len(files) + sum(n.get('m', 0) for n in out if n['kind'] == 'folder'))


def fold(nodes, cap):
    """Keep a graph to `cap` things: the files touched longest ago fold into the folder they are in (a folder node
    says how many, `m`), then the oldest of every other kind into one node of that kind. Returns {id: the id it
    folded into}. The nodes that others hang under are never folded."""
    into = {}
    heads = {n['p'] for n in nodes.values() if n.get('p')}

    def merge(n, i, kind, label, full, p=''):
        d = nodes.get(i)
        if d is None:
            d = nodes[i] = dict(id=i, kind=kind, label=label, full=full, n=0, last=0, t=[], m=0)
            if p:
                d['p'] = p
        d['n'] += n['n']
        d['m'] += n.get('m', 0) or 1
        if n['last'] >= d['last']:
            d['last'], d['t'] = n['last'], (n.get('t') or [])[-2:]
        into[n['id']] = i
        for k, v in list(into.items()):
            if v == n['id']:
                into[k] = i
        del nodes[n['id']]

    if len(nodes) <= cap:
        return into
    for n in sorted((n for n in nodes.values() if n['kind'] in FILES and n['id'] not in heads), key=lambda n: (n['last'], n['id'])):
        if len(nodes) <= cap:
            return into
        tree, rel = n.get('_at') or ('', n['full'])
        folder = rel.rsplit('/', 1)[0] if '/' in rel else ''
        where = folder
        merge(n, nid('d', tree + '|' + folder), 'folder', '/'.join(folder.split('/')[-2:]) + '/' if folder else (Path(tree).name if tree else 'files'), where.rstrip('/') + '/', n.get('p', ''))
    for kind in ('command', 'search', 'tool', 'web', 'message', 'skill', 'subagent', 'folder'):
        for n in sorted((n for n in nodes.values() if n['kind'] == kind and n['id'] not in heads and not n['id'].startswith('o')), key=lambda n: (n['last'], n['id'])):
            if len(nodes) <= cap:
                return into
            words = dict(command='older commands', search='older searches', tool='other tools', web='older pages', message='older messages', skill='older skills', subagent='agents sent earlier', folder='older folders')
            merge(n, nid('o', kind), kind, words[kind], words[kind])
    return into


# ---- the relay: a run's own records ---------------------------------------------------------------------------------

NAMED = re.compile(r'(?<![\w/\\.-])((?:[\w.@-]+[/\\])*[\w@-][\w.@-]*\.(?:' + EXTS + r'))(?![\w/\\])')
FILES_MOST = 60


def runs_of(out: Path):
    """The runs as the Runs page was given them on this reading (data/runs.js)."""
    try:
        t = (out / 'data' / 'runs.js').read_text(encoding='utf-8')
        return json.loads(t[t.index('{'):].rstrip().rstrip(';')).get('runs') or []
    except (OSError, ValueError):
        return []


def relay(run, leg=0, line=''):
    """A run of the relay as a graph: the run, its units (the one it is on now marked `on`, one with no leg yet
    marked `q`: queued, drawn faint), each unit's lane, the commits its legs made and the files their reports name.
    The chain run -> unit -> lane -> commit -> file is in the links; a node's `p` is the unit it belongs to.

    The legs run on the other station, and a leg's record reaches this one when the leg is over: what the leg that
    runs now is reading and changing is not known here. A digest the relay published per call (its last tool calls,
    as `take` reads them from a transcript) would be turned into nodes here, hung under the unit in `on`."""
    nodes, links, log, seen = [], [], [], set()
    rid = nid('r', run['id'])
    state = 'going' if run.get('state') == 'going' else run.get('state') or ''
    nodes.append(dict(id=rid, kind='run', label='run ' + '-'.join(str(run['id']).split('-')[:2])[4:], full=f'relay run {run["id"]} · {state}' + (f' · on the {run["station"]}' if run.get('station') else ''),
                      n=int(run.get('legs') or 0), last=int(run.get('heard') or run.get('started') or 0), ref=run['id']))
    links.append(dict(a='core', b=rid, n=1, last=nodes[0]['last'], verb='runs'))
    commits = files = legs = 0
    for u in run.get('units') or []:
        uid = nid('u', run['id'] + '|' + u['id'])
        ls = u.get('legs') or []
        last = max([int(l.get('started') or 0) + int(l.get('seconds') or 0) for l in ls] or [0])
        un = dict(id=uid, kind='unit', label=u['id'], full=(u.get('verdict') or ('on it now' if u['id'] == run.get('now_on') else 'no verdict yet')) + ' · ' + (u.get('goal') or u['id'])[:200],
                  n=len(ls), last=last)
        if u['id'] == run.get('now_on') and run.get('state') == 'going':
            un['on'] = 1
        if not ls:
            un['q'] = 1
        if u.get('verdict'):
            un['verdict'] = u['verdict']
        nodes.append(un)
        links.append(dict(a=rid, b=uid, n=len(ls), last=last, verb=(u.get('verdict') or '').lower() or 'on'))
        at = uid
        if u.get('lane'):
            lid = nid('l', run['id'] + '|' + u['id'] + '|' + u['lane'])
            nodes.append(dict(id=lid, kind='lane', label=branch_short(u['lane']), full=u['lane'], n=len(ls), last=last, p=uid, ref=u['lane']))
            links.append(dict(a=uid, b=lid, n=len(ls), last=last, verb='on'))
            at = lid
        for l in ls:
            legs += 1
            when = int(l.get('started') or 0)
            done = when + int(l.get('seconds') or 0)
            log.append([when, l.get('phase') or 'leg', uid, f'leg {l.get("n")} · {u["id"]}' + (f' · {l["said"]}' if l.get('said') else '')])
            tip = at
            for c in l.get('commits') or []:
                cid = nid('k', c['sha'])
                if cid in seen:
                    continue
                seen.add(cid)
                commits += 1
                nodes.append(dict(id=cid, kind='commit', label=c['sha'][:7], full=c['sha'] + ' · ' + c.get('subject', ''), n=1, last=done, p=uid, t=[[done, 'committed']]))
                links.append(dict(a=at, b=cid, n=1, last=done, verb='committed'))
                log.append([done, 'commit', cid, c.get('subject') or c['sha']])
                tip = cid
            for name in sorted({m.group(1).replace('\\', '/') for line in l.get('changed') or [] for m in NAMED.finditer(str(line))}):
                fid = nid('f', run['id'] + '|' + u['id'] + '|' + name)
                if fid in seen or files >= FILES_MOST:
                    continue
                seen.add(fid)
                files += 1
                nodes.append(dict(id=fid, kind='reported', label=name.rsplit('/', 1)[-1], full=name, n=1, last=done, p=uid, t=[[done, 'named in the report of leg ' + str(l.get('n'))]]))
                links.append(dict(a=tip, b=fid, n=1, last=done, verb='named'))
    log.sort(key=lambda r: r[0])
    units = run.get('units') or []
    on = next((n for n in nodes if n.get('on')), None)
    known = max([int(l.get('n') or 0) for u in units for l in u.get('legs') or []] or [0])
    waits = bool(leg) and leg > known           # the leg that runs now has no record yet: the graph is the run so far, and says so
    if waits:
        phase = (line.split(':', 1)[1].split('(')[0].strip() if ':' in line else '') or 'leg'
        log.append([max([r[0] for r in log] or [nodes[0]['last']]), 'running', (on or nodes[0])['id'], f'leg {leg} \u00b7 {phase} \u00b7 no record of it yet'])
    nodes[0]['last'] = nodes[0]['last'] or max([n['last'] for n in nodes] or [0])       # a run with no word of its own yet: its newest record
    g = dict(kind='run', nodes=nodes, links=links, log=[dict(at=t, verb=v, id=i, thing=src_ops.short(what, 60), **(dict(now=1) if v == 'running' else {})) for t, v, i, what in log[-LOG:]],
             counts=dict(read=0, changed=files, ran=legs, searched=0, sent=0, skills=0),
             counters=[['units', len(units)], ['legs', legs], ['commits', commits], ['files', files]],
             first=int(run.get('started') or 0) or (log[0][0] if log else 0), last=nodes[0]['last'], calls=legs, more=False, files=files, run=run['id'],
             note=(f'This is the run so far: leg {leg} runs now and has no record yet (a leg\'s record comes when it ends). ' if waits else '')
             + 'From the run\'s records. What a leg touches call by call stays on the station it runs on.')
    if leg:
        g['leg'] = leg
    return g


# ---- everyone on the floor ------------------------------------------------------------------------------------------

STAMP = 120              # seconds between two notes in the cache that a file is still in use
AGAIN = 120              # ... and between two looks for a transcript that was not found
MISSED = {}              # worker -> when its transcript was last looked for and not found
BUILT = {}               # transcript path -> (how far it was read, the branches then, the cap, its graph): a watcher builds a graph once per change
LOADED = {}              # cache path -> (its time and size when this process last had it, the cache)


def load(path: Path):
    try:
        st = path.stat()
        have = LOADED.get(str(path))
        if have and have[0] == (st.st_mtime_ns, st.st_size):
            return have[1]
        text = path.read_text(encoding='utf-8')
        c = json.loads(text)
        LOADED[str(path)] = ((st.st_mtime_ns, st.st_size), c, text)
        return c if c.get('version') == VERSION else dict(version=VERSION, files={}, who={})
    except (OSError, ValueError):
        return dict(version=VERSION, files={}, who={})


def save(path: Path, cache):
    text = json.dumps(cache, separators=(',', ':'))
    have = LOADED.get(str(path))
    if have and have[2] == text and path.exists():
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + '.tmp')
    tmp.write_text(text, encoding='utf-8')
    tmp.replace(path)
    st = path.stat()
    LOADED[str(path)] = ((st.st_mtime_ns, st.st_size), cache, text)


def session_log(key8, projects):
    return next(iter(sorted(projects.glob(f'*/{key8}*.jsonl'))), None)


def agent_log(session: Path, key6):
    sub = src_ops.reach(session).with_suffix('') / 'subagents'
    return next(iter(sorted(sub.rglob(f'agent-{key6}*.jsonl'))), None) if sub.is_dir() else None


def collect(workers, trees, now, cache, projects=None, runs=()):
    """The graph of every worker: {key: graph}, the key a worker's uid, else its id. `workers` is [(lane, worker)] as
    the floor has them, `trees` {checkout path: {'branch': ...}}, `cache` kept between reads by the caller, `runs` the
    relay's runs (runs_of)."""
    projects = projects or src_ops.PROJECTS
    spell = src_ops.spellings(list(trees))
    branches = {str(p): (t.get('branch') or '') for p, t in trees.items()}
    bsig = json.dumps(branches, sort_keys=True)
    files, who = cache.setdefault('files', {}), cache.setdefault('who', {})
    by, floor, here = {}, {}, {}
    for _, w in workers:
        floor[w.get('uid') or w.get('id')] = w
    on6 = {k.split('#')[-1]: k for k, w in floor.items() if '#' in k and w.get('kind') == 'agent'}

    def uid_of(agent_id, kind):
        return on6.get(agent_id[:6]) or f'agent:{kind}#{agent_id[:6]}'

    def root(key, depth=0):
        """The session an agent's log lies under: the one that sent it, or that sent the agent that did."""
        if key.startswith('session:') or depth > 6:
            return key if key.startswith('session:') else ''
        return root(str((floor.get(key) or who.get(key) or {}).get('parent') or ''), depth + 1)

    def log_of(key, w):
        """Where a worker's transcript is, asked of the disk once and remembered."""
        had = (who.get(key) or {}).get('path')
        if had and os.path.exists(had):
            return had
        if w.get('vendor') not in (None, '', 'claude') or w.get('kind') not in ('session', 'agent') or now - MISSED.get(key, 0) < AGAIN:
            return ''
        f = None
        if w.get('session'):                              # a relay leg that runs on this station is a session
            f = session_log(str(w['session'])[:36], projects)
        elif key.startswith('session:'):
            f = session_log(key[len('session:'):], projects)
        elif '#' in key and w.get('kind') == 'agent':
            s = root(str(w.get('parent') or ''))
            sf = log_of(s, floor.get(s) or who.get(s) or {}) if s else None
            f = agent_log(Path(sf), key.split('#')[-1]) if sf else None
        if not f:
            MISSED[key] = now
        return str(f) if f else ''

    def built(path, cap, side):
        e = files.setdefault(path, {})
        f = src_ops.reach(Path(path))
        try:
            take(f, e, spell, side=side)
        except OSError:
            return None
        if now - e.get('used', 0) > STAMP:               # coarse, so a floor where nothing moved leaves the cache as it is
            e['used'] = int(now)
        have = BUILT.get(path)
        if have and have[:3] == (e.get('off'), bsig, cap) and have[4] == on6:
            return dict(have[3])
        g = graph(e, branches, cap=cap, uid_of=uid_of)
        BUILT[path] = (e.get('off'), bsig, cap, g, dict(on6))
        return dict(g)

    for key, w in floor.items():
        if w.get('host') and not w.get('session'):       # the other station's worker: its transcript is not on this one
            continue
        path = log_of(key, w)
        if not path:
            continue
        g = built(path, NODES, side=not (key.startswith('session:') or w.get('session')))
        if g is None:
            continue
        g['kind'] = 'session' if w.get('kind') == 'session' else 'agent'
        if w.get('parent'):
            g['parent'] = w['parent']
        by[key] = g
        here[key] = path
        was = (who.get(key) or {}).get('seen', 0)
        who[key] = dict(path=path, seen=int(now) if now - was > STAMP else was, kind=g['kind'], parent=str(w.get('parent') or ''),
                        title=str(w.get('title') or w.get('name') or '')[:60], what=str(w.get('what') or '')[:110])
    # the agents a worker sent that are over: each still opens from the node that stands for it
    for key in list(by):
        sent = [n for n in by[key]['nodes'] if n['kind'] == 'subagent' and n.get('to') and n['to'] not in by]
        for n in sorted(sent, key=lambda n: -n['last'])[:SENT_MOST]:
            s = root(key)
            f = agent_log(Path(here[s]), n['to'].split('#')[-1]) if s in here else None
            try:
                if not f or now - f.stat().st_mtime > SENT_FRESH:
                    continue
            except OSError:
                continue
            g = built(str(f), NODES_OVER, side=True)
            if g:
                g.update(kind='agent', parent=key, gone=True, title=n['label'], log=g['log'][-LOG // 2:])
                g.pop('heat', None)
                by[n['to']] = g
    # whoever left the floor a little while ago
    for key, m in list(who.items()):
        if key in floor:
            continue
        if now - m.get('seen', 0) > GONE or not m.get('path'):
            del who[key]
            continue
        if key in by:
            continue
        g = built(m['path'], NODES_OVER, side=m.get('kind') != 'session')
        if g:
            g.update(kind=m.get('kind') or 'agent', gone=True, title=m.get('title') or '', log=g['log'][-LOG // 2:])
            if m.get('parent'):
                g['parent'] = m['parent']
            by[key] = g
    for path in [p for p, e in files.items() if now - e.get('used', 0) > max(GONE, SENT_FRESH)]:
        del files[path]
        BUILT.pop(path, None)
    # the relay: every leg that runs is its run's graph, and so is the relay itself
    going = {str(r['id']): r for r in runs if not r.get('empty') or r.get('state') == 'going'}
    lane_of = {(w.get('uid') or w.get('id')): lane for lane, w in workers}
    legs = {lane_of.get(k): re.match(r'^leg:(.+)-(\d+)$', k).group(1) for k in floor if re.match(r'^leg:(.+)-(\d+)$', k)}       # lane -> the run of the leg on it

    def started(rid, key, w):
        """A run whose records have not reached this station: what the floor says of it (its unit, from the worker's
        line; its lane, where the worker stands), and that unit's legs from the last run that worked on it."""
        unit = str(w.get('what') or '').split(':')[0].strip()
        if w.get('id') == 'agent:relay' or not unit:
            unit = next((str(x.get('what') or '').split(':')[0].strip() for k, x in floor.items() if k.startswith(f'leg:{rid}-')), unit)
        lane = lane_of.get(key) or ''
        before = next(((r, u) for r in sorted(runs, key=lambda r: -(r.get('started') or 0)) for u in r.get('units') or [] if u.get('id') == unit and u.get('legs')), None)
        u = dict(before[1], verdict='', goal=f'earlier legs are from run {before[0]["id"]} · ' + str(before[1].get('goal') or '')) if before else dict(id=unit, lane=lane if lane.startswith('lane/') else '', legs=[], goal='')
        return dict(id=rid, state='going', station=str(w.get('host') or ''), started=0, heard=0, legs=len(u.get('legs') or []), now_on=unit, units=[u] if unit else [],
                    fresh=True)

    for key, w in floor.items():
        m = re.match(r'^leg:(.+)-(\d+)$', key)
        relay_itself = w.get('id') == 'agent:relay'
        if not m and not relay_itself:
            continue
        rid = m.group(1) if m else str(w.get('run') or legs.get(lane_of.get(key)) or '')
        run = going.get(rid)
        if run is None and relay_itself and not rid:
            mine = [r for r in going.values() if r.get('state') == 'going']
            run = max(mine, key=lambda r: r.get('started') or 0) if mine else None
        if run is None and rid:
            run = started(rid, key, w)
        if run is None:
            continue
        g = relay(run, int(m.group(2)) if m else 0, str(w.get('what') or ''))
        if run.get('fresh'):
            g['note'] = 'This run has only just started: its own records have not reached this station yet. ' + g['note']
        if key in by:                                    # a leg on this station: its own calls, with its run beside them
            keep = [n for n in g['nodes'] if n['kind'] == 'run' or n.get('on')]
            ids = {n['id'] for n in keep}
            by[key]['nodes'] = by[key]['nodes'] + keep
            by[key]['links'] = by[key]['links'] + [l for l in g['links'] if l['b'] in ids]
            by[key]['run'] = run['id']
        else:
            if w.get('parent'):
                g['parent'] = w['parent']
            by[key] = g
    return by


def slim(g, cap):
    """A graph again with at most `cap` things, from the graph itself: the things touched longest ago go, a node others
    hang under stays, and the graph says how many are not shown (`cut`)."""
    heads = {n['p'] for n in g['nodes'] if n.get('p')}
    order = sorted((n for n in g['nodes'] if n['id'] not in heads), key=lambda n: (-n['last'], n['id']))
    keep = heads | {n['id'] for n in order[:max(0, cap - len(heads))]}
    if len(keep) >= len(g['nodes']):
        return g
    return dict(g, nodes=[n for n in g['nodes'] if n['id'] in keep], links=[l for l in g['links'] if l['b'] in keep],
                log=g['log'][-LOG // 2:], cut=len(g['nodes']) - len(keep))


def size(by):
    return len(json.dumps(by, separators=(',', ':')))


def fit(by, state, budget=None):
    """The graphs in at most BUDGET characters: the page reads the file every 20 seconds. Over it, the workers that are
    over are shown smaller first, then the ones that rest, then everyone; who is at work keeps the most."""
    budget = budget or BUDGET
    sizes = {k: size(g) + len(k) + 6 for k, g in by.items()}      # each graph is measured once, and again only when it was made smaller
    if sum(sizes.values()) <= budget:
        return by
    out = dict(by)
    order = sorted(out, key=lambda k: (0 if out[k].get('gone') else 1 if state.get(k) != 'working' else 2, out[k].get('last') or 0))
    for cap in (48, 24, 12):
        for k in order:
            if out[k].get('kind') == 'run' or (cap > 24 and state.get(k) == 'working' and not out[k].get('gone')):
                continue
            out[k] = slim(out[k], cap)
            sizes[k] = size(out[k]) + len(k) + 6
            if sum(sizes.values()) <= budget:
                return out
    return out


def site(data, out: Path, now=None, cache_path=None, projects=None):
    """Put the graphs in the site: data/touch.js, written only when a graph changed (its `at` is the last touch, not
    the time of the reading). A reading that fails leaves the file of the reading before: graphs that do not read
    are no reason to have no floor."""
    try:
        import build
        now = now or time.time()
        cache_path = cache_path or build.LOCAL / 'touch-cache.json'
        cache = load(cache_path)
        trees = {Path(l['path']): dict(branch=l['branch']) for l in data['lanes'] if l.get('path')}
        workers = [(l['branch'], w) for l in data['lanes'] for w in l['workers']]
        by = collect(workers, trees, now, cache, projects=projects, runs=runs_of(out))
        save(cache_path, cache)
        state = {(w.get('uid') or w.get('id')): w.get('state') for _, w in workers}
        T = dict(by=fit(by, state), at=max([g.get('last') or 0 for g in by.values()] or [0]))
        text = f'window.TOUCH = {json.dumps(T, sort_keys=True, separators=(",", ":"))};\n'
        path = out / 'data' / 'touch.js'
        try:
            if path.read_text(encoding='utf-8') == text:
                return T
        except OSError:
            pass
        path.parent.mkdir(parents=True, exist_ok=True)
        tmp = path.with_name(path.name + '.tmp')
        tmp.write_text(text, encoding='utf-8')
        tmp.replace(path)
        return T
    except Exception as e:      # noqa: BLE001
        print(f'ops: what the workers touch was not read ({type(e).__name__}: {e})', flush=True)
        return None
