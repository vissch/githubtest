"""Which room of the house a piece of work belongs in: the rule book of the house page (house.html).

The house has a room for each kind of work, and every session, skill, agent and machine walks to the room of what
it is doing. This file says which. Nothing here reads a file or the clock, so every rule can be tried with one line
(test_assetboard.py, the cases named "house:"). src_ops.py asks it for each worker's `act` and each roster entry's
`home`.

    work    the workroom: writing code and docs, commits
    lab     reading, searching, reviews, tests and gates
    shop    the workshop: builds, Unity, tools, machines
    studio  animation, films, VFX, art
    plan    the war room: planning, briefing agents, waiting on the owner
    bunk    the bunkhouse: resting and idle (src_ops.py sends a resting session there; no call votes for it)
"""
import re

ROOMS = ('work', 'lab', 'shop', 'studio', 'plan', 'bunk')
ACT_WINDOW = 180    # a call this many seconds older than the newest one says nothing about now
ACT_CALLS = 12      # ... and no more than this many calls are asked

WRITES = {'Edit', 'MultiEdit', 'Write', 'NotebookEdit'}
LOOKS = {'Read', 'Grep', 'Glob', 'LS', 'WebFetch', 'WebSearch', 'ToolSearch', 'ListMcpResourcesTool', 'ReadMcpResourceTool'}
SHELLS = {'Bash', 'PowerShell'}
PLANS = {'Agent', 'Task', 'SendMessage', 'TodoWrite', 'EnterPlanMode', 'ExitPlanMode', 'AskUserQuestion', 'TaskStop', 'ListAgents', 'Workflow'}
PACING = {'ScheduleWakeup', 'CronCreate', 'CronDelete', 'CronList', 'Monitor'}    # waiting for the clock is no kind of work: no vote
ART = ('.png', '.jpg', '.jpeg', '.webp', '.gif', '.psd', '.blend', '.fbx', '.mp4', '.mov', '.aep', '.svg')
MCP_STUDIO = ('aftereffects', 'remotion', 'blender', 'comfy', 'fal')


def of_tool(name, inp):
    """The room a tool call is work in, or None for a call that is only pacing (a wakeup, a monitor)."""
    name, inp = name or '', inp if isinstance(inp, dict) else {}
    if name in WRITES:
        path = str(inp.get('file_path') or inp.get('notebook_path') or '').replace('\\', '/').lower()
        return 'plan' if re.search(r'(^|/)\.claude/plans/', path) else 'studio' if path.endswith(ART) else 'work'
    if name in LOOKS:
        return 'lab'
    if name in SHELLS:
        return of_command(str(inp.get('command') or ''))
    if name in PLANS:
        return 'plan'
    if name == 'Skill':
        return of_skill(str(inp.get('skill') or '').split(':')[-1])
    if name in PACING:
        return None
    if name.startswith('mcp__'):
        low = name.lower()
        return 'studio' if any(w in low for w in MCP_STUDIO) else 'shop' if 'unity' in low else 'work'
    return 'work'


# ---- a command line -----------------------------------------------------------------------------------------------

# The verbs that only look. The first row is the brief's. The rest are the filters and lookups the transcripts are
# full of: on the laptop's 44,000 shell calls `cut` alone kept a thousand plain looks out of the lab.
LOOK = set("""ls dir cat head tail rg grep find wc stat file type echo pwd which du df tree awk get-childitem get-content select-string test-path get-item measure-object
              egrep fgrep cut sort uniq tr nl column paste comm diff cmp od xxd strings basename dirname realpath readlink date printf seq jq
              true false test [ [[ read md5sum sha1sum sha256sum cygpath ps tasklist netstat whoami hostname uname printenv where
              gc gci sls select select-object sort-object where-object group-object format-table format-list out-string out-null get-date
              get-process get-location resolve-path get-filehash get-itemproperty get-ciminstance""".split())
GIT_LOOK = set("""log status diff show ls-tree ls-files rev-parse blame fetch
                  merge-base grep rev-list check-ignore ls-remote for-each-ref merge-tree count-objects cat-file check-attr shortlog patch-id
                  cherry range-diff describe show-ref name-rev diff-tree whatchanged version help""".split())
LISTING = re.compile(r'(-a|-r|-l|-v|-vv|-n|--all|--remotes|--verbose|--list|--show-current|--no-color|--(contains|merged|no-merged|points-at|sort|format)(=\S+)?)$')
QUOTED = r'"(?:\\.|[^"\\])*"|\'[^\']*\''
INSIDE = r'\$\(\([^()]*\)\)|[$<]\((?:[^()"\'\\]|\\.|' + QUOTED + r')*\)'      # $((a sum)); a $(command) or <(command) with no other inside it
TOKEN = re.compile(QUOTED + '|' + INSIDE + r"""|\\.|&&|\|\||[;|\n]|(?<!<)<<(?!<)-?\s*["']?(\w+)["']?|[^"'\\;|&\n<$]+|.""", re.S)
# what stands before a command without being one (`do`, `timeout 60`, `LC_ALL=C`, `xargs -n 1`, PowerShell's `$x =`),
# and a command that is no work at all: going somewhere, waiting, naming a value, the words that hold a loop together
WRAP = re.compile(r'((do|then|else|elif|if|while|until|!|time|nohup)\s+|timeout\s+\S+\s+|xargs(\s+-[nIPdLsE]\s*\S+|\s+-[\w-]+)*\s+'
                  r'|[A-Za-z_]\w*=(?:' + QUOTED + r'|[^\s"\'(`])*\s+|\$\w+\s*=\s*)+')
IDLE = re.compile(r'(cd|chdir|pushd|popd|set-location|sl|sleep|start-sleep|wait|break|continue|exit|done|fi|esac|else|then|do)(\s|$)|[{}]$'
                  r'|for\s+\w+\s+in\b|for\s*\(\(|(export\s+)?[A-Za-z_]\w*=\S*$|(""|\d+|\$[\w.]+)$', re.I)
HARMLESS = re.compile(r'\d?>&\d|[\d&]?>>?\s*(/dev/null|\$null|nul)\b', re.I)   # 2>&1, 2>/dev/null: a redirect that writes no file
FIND_ACTS = re.compile(r'\s-(delete|exec|execdir|ok|okdir|fprint|fprintf|fls)\b')          # a find that does something
TESTING = re.compile(r"""gate\.ps1|\bunity(\.exe)?["']?\s+test\b|-runTests\b|\b(otr|validate|selftest|toolcheck|health|assetgate)\.py\b"""
                     r"""|\btest_\w+\.py\b|\bpytest\b|\bnpm\s+(run\s+)?test\b|\bdotnet\s+test\b""", re.I)
FILMING = re.compile(r'\bblender(\.exe)?\b|\bff(mpeg|probe)\b|\b(gamefilm|films|firebooks|sprites)\.py\b|film_blender|thumb_blender'
                     r'|\bremotion\b|comfy|seedance|\baerender\b|\bafterfx\b|fal_client|fal-ai|\bfal\.run\b', re.I)   # "film" alone and --no-films are not it
BUILDING = re.compile(r'\bunity(\.exe)?\b|\bBuildWindows\b|\b(build|occ)\.py\b|\bcsc\.dll\b|\bdotnet\b|\bmsbuild\b|\bnpm\s+(install|ci|run)\b'
                      r'|\bpip3?\s+install\b|\btw\s+eval\b|\bTools[\\/]tw\b', re.I)
RULES = [(TESTING, 'lab'), (FILMING, 'studio'), (BUILDING, 'shop'), (re.compile(r'\bland\.py\b', re.I), 'work')]


def segments(cmd):
    """A command line as the commands it chains, each with what a here-document feeds it: cut at && || ; | and at a
    line's end, never inside quotes or a $(...). `python - <<EOF` and the lines up to EOF are one command and its
    input, not a command per line."""
    out, cur, feeds, pos = [], [], [], 0
    while pos < len(cmd):
        m = TOKEN.match(cmd, pos)
        t, pos = m.group(0), m.end()
        if m.group(1):
            feeds.append((len(out), re.compile(r'^[ \t]*' + re.escape(m.group(1)) + r'[ \t]*$', re.M)))
        if t == '\n' and feeds:                    # the line that opened them is over: each takes its lines
            out.append([''.join(cur), ''])
            cur = []
            for at, last in feeds:
                end = last.search(cmd, pos)
                stop = end.end() if end else len(cmd)
                out[at][1] += cmd[pos:stop]
                pos = stop
            feeds = []
        elif t in ('&&', '||', ';', '|', '\n'):
            out.append([''.join(cur), ''])
            cur = []
        else:
            cur.append(t)
    out.append([''.join(cur), ''])
    return [(line.strip(), feed) for line, feed in out if line.strip()]


def words(line):
    """A command's words without their quotes; its first word without its folder and .exe, in lower case."""
    ws = [w[1:-1] if len(w) > 1 and w[0] == w[-1] and w[0] in '"\'' else w for w in re.findall(QUOTED + r'|\S+', line)]
    return [re.sub(r'\.exe$', '', re.split(r'[\\/]', ws[0])[-1].lower())] + ws[1:] if ws else []


def git_looks(ws):
    """Whether a git command only looks. `git -C x --no-pager log -3` is log; a branch or a tag is a look when it lists."""
    i = 1
    while i < len(ws) and ws[i].startswith('-'):
        i += 2 if ws[i] in ('-C', '-c', '--git-dir', '--work-tree') else 1
    verb, rest = (ws[i].lower() if i < len(ws) else ''), ws[i + 1:]
    if verb in ('branch', 'tag'):
        return '--list' in rest or '-l' in rest or all(LISTING.match(w) or (n and re.match(r'--(contains|merged|no-merged|points-at)$', rest[n - 1])) for n, w in enumerate(rest))
    if verb == 'config':                           # asking for a value or the list, not setting one
        return bool({'--get', '--get-all', '--get-regexp', '--list', '-l'} & set(rest)) or len([w for w in rest if not w.startswith('-')]) == 1 and not {'--unset', '--unset-all', '--edit', '-e'} & set(rest)
    return verb in GIT_LOOK or (verb, rest[:1]) in (('worktree', ['list']), ('stash', ['list']), ('stash', ['show']), ('remote', []), ('remote', ['-v']),
                                                   ('remote', ['show']), ('remote', ['get-url']), ('reflog', []), ('reflog', ['show']))


def kind(line):
    """What one command is: 'look' (it only reads), 'idle' (it goes somewhere, waits or names a value), 'paper' (a git
    command that is not a look), 'put' (a looking verb whose output is kept in a file: `echo note >> x.md`) or 'do'.
    When it is unsure whether a command only looks, it does not."""
    s = line.replace('\\\n', ' ').strip()
    for a, b in ('()', '{}'):                      # half of a group: `(cd x && git log)` came apart at the &&
        s = s.lstrip(a + ' ') if s.count(a) > s.count(b) else s.rstrip(b + ' ') if s.count(b) > s.count(a) else s
    s = s[WRAP.match(s).end():] if WRAP.match(s) else s
    live = re.sub(QUOTED, lambda m: m.group(0) if m.group(0)[0] == '"' else "''", s)     # what the shell still reads: not '...'
    sure = all(i.startswith('$((') or not set(kinds(i[2:-1])) - {'look', 'idle'} for i in re.findall(INSIDE, live))   # what it runs inside itself
    live = re.sub(INSIDE, 'X', live)
    bare = re.sub(QUOTED, '""', live)
    sure = sure and not re.search(r'[$<]\(|`', live)                               # nothing left in it that it may run
    keeps = '>' in HARMLESS.sub(' ', bare)                                         # it writes a file
    ws = words(s)
    head = ws[0] if ws else ''
    if head == 'git':
        return 'look' if sure and not keeps and git_looks(ws) else 'paper'
    if not sure:
        return 'do'
    if IDLE.match(bare) and not keeps:
        return 'idle'
    if head == 'sed':                              # a sed that prints is a look, one that rewrites its file is not
        looks = not any(re.match(r'-[a-zE]*i|--in-place', w) for w in ws[1:])
    elif head == 'find':
        looks = not FIND_ACTS.search(bare)
    elif head == 'awk':
        looks = not re.search(r'\s-i\s', bare)
    else:                                          # and asking any program which version it is, or how it is used
        looks = head in LOOK or (len(ws) > 1 and all(w in ('--version', '-version', '-V', '--help') for w in ws[1:]))
    return 'do' if not looks else 'put' if keeps else 'look'


def kinds(cmd):
    return [kind(line) for line, _ in segments(cmd)]


def of_command(cmd):
    """The room a shell command is work in. The first of these that holds:
    1. it only looks (every command in it is a read-only verb or a read-only git): lab;
    2. it tests (the gate, a Unity test run, otr, validate, toolcheck, a test_*.py, pytest): lab;
    3. it films, animates or draws (Blender, ffmpeg, the film scripts, Remotion, ComfyUI, fal): studio;
    4. it builds or runs (Unity, build.py, dotnet, npm, pip, tw): shop;
    5. it is paperwork (git that is not a look: commit, push, add, rebase ...; land.py): work;
    6. anything else: shop.
    A `cd`, a `sleep`, a `NAME=value` and the words of a loop are no work of their own. Rules 2 to 4 read the
    commands that do something, with what a here-document feeds them and where they were sent (`cd ComfyUI`,
    `B=.../blender.exe`). A look (`grep ffmpeg x.py`), a note (`echo "Blender moved it" >> notes.md`) and a commit's
    own words (`git add build.py`, a message that names the gate) are not asked: what they name is not what is run."""
    segs = segments(cmd or '')
    ks = [kind(line) for line, _ in segs]
    if 'look' in ks and not {'do', 'put', 'paper'} & set(ks):
        return 'lab'
    rest = '\n'.join(line + feed for (line, feed), k in zip(segs, ks) if k in ('do', 'idle')) if 'do' in ks else ''
    return next((room for rx, room in RULES if rx.search(rest)), 'work' if 'paper' in ks else 'shop')


# ---- a skill, an agent, a machine ---------------------------------------------------------------------------------

SKILLS = {'tw-critic': 'lab', 'tw-bug-catcher': 'lab', 'tw-balance-sim': 'lab', 'tw-master': 'plan', 'pipeline': 'plan',
          'tw-vfx-sheets': 'studio', 'tw-destruction-vfx': 'studio', 'tw-character-sim': 'studio', 'tw-env-sim': 'studio',
          'tw-vehicle-sim': 'shop', 'tw-optimizer': 'shop', 'unity-pipeline': 'shop'}
AGENTS = {'explore': 'lab', 'claude-code-guide': 'lab', 'plan': 'plan', 'gamedesign': 'plan', 'relay': 'plan', 'statusline-setup': 'shop'}
# A trade by its words, for a name no table knows: the first group with a word in the name or in what it does. `anim*`
# is a stem (animation, animated); a bare word is that word only, so `art` is not found in "start" or "artifact", `rig`
# not in "right", `logo` not in "logon". The last group is also where everything else goes; it is listed so that a
# group added after it cannot take its words.
TRADES = [('lab', 'critic critics critiq* review* bug bugs test* audit* balanc* research* explor* verif* security'),
          ('studio', 'vfx anim* film* video* image* sprite* icon icons trailer* remotion logo logos art arts artwork*'),
          ('plan', 'master* plan plans planning planner planned pipeline* board boards relay* schedul* design* advis*'),
          ('shop', 'unity build builds builder builders optimi* deploy* install* config* vehicle* rig rigs rigging rigged'),
          ('work', 'doc docs docx document* write writes writer writing report* pdf xlsx pptx book books')]
_TRADES = [(room, {w for w in ws.split() if not w.endswith('*')}, tuple(w[:-1] for w in ws.split() if w.endswith('*'))) for room, ws in TRADES]
MACHINES = {'Unity test run': 'lab', 'the gate': 'lab', 'tests outside Unity': 'lab', 'Blender, rendering films': 'studio',
            'Blender, rendering previews': 'studio', 'Blender': 'studio', 'filming the game': 'studio'}     # the labels of src_ops.MACHINES


def trade(text):
    """The room of the first group of TRADES with a word in the text, or None."""
    found = re.findall(r'[a-z0-9]+', (text or '').lower())
    return next((room for room, exact, stems in _TRADES if any(w in exact or w.startswith(stems) for w in found)), None)


def of_skill(name, does=''):
    """The room a skill works in: the project's skills by name, any other by the words of its name and what it does."""
    return SKILLS.get((name or '').lower()) or trade(f'{name} {does}') or 'work'


def of_agent(type, text=''):
    """The room an agent works in: the built-in agents by name, any other by the words of its name and its task."""
    return AGENTS.get((type or '').lower()) or trade(f'{type} {text}') or 'work'


def of_machine(label):
    """The room a machine (a process against a checkout, src_ops.MACHINES) works in: a test run in the lab, Blender
    and the game's films in the studio, every other in the workshop."""
    return MACHINES.get(label, 'shop')


# ---- where a worker is now ----------------------------------------------------------------------------------------

def pick(calls):
    """The room a worker is in now, from its tool calls in the order it made them, each (when, room).
    Calls with no room are left out. Of the rest, those within ACT_WINDOW seconds of the newest count (a call with no
    time counts), the last ACT_CALLS of them at most, a later call for more than an earlier one: the i-th of n adds
    1 + i/n to its room. The room with the most wins, and of rooms level with each other the one called last.
    None when no call counts."""
    calls = [(t, r) for t, r in calls if r]
    times = [t for t, _ in calls if t is not None]
    kept = [r for t, r in calls if t is None or max(times) - t <= ACT_WINDOW][-ACT_CALLS:]
    score, last = {}, {}
    for i, r in enumerate(kept):
        score[r] = score.get(r, 0) + len(kept) + i      # 1 + i/n, times n: whole numbers, so level is exactly level
        last[r] = i
    return max(score, key=lambda r: (score[r], last[r])) if score else None
