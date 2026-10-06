#!/usr/bin/env python3
"""The owner queue: everything that waits on the owner, from where it is written down. The site's "Needs you".

    python Tools/assetboard/ops.py --queue                   from trench-warfare-3d/: the queue, as the site shows it
    python Tools/assetboard/src_queue.py --stranded          the decisions stranded on lanes
    python Tools/assetboard/src_queue.py --stranded --text   the same, each with its full line, ready to copy

WHY. Work was made faster than it landed and the owner could not see what waited on them: "Needs you" counted the
board's ready stages and nothing else (the process critique of 2026-10-04). collect() derives five groups and stores
nothing but the owner's approvals:
- decide: the bullets under "## Open" of decisions.md on integration, oldest first. A bullet's date is the one in
  its title, else the day the commit that first wrote it was made;
- land: a checkout whose last green full gate tested exactly its HEAD (gate.ps1's tw-gate-green), clean and ahead of
  integration: it lands on the owner's word;
- approved: the owner said land and it has not landed. approvals/<lane>.json on the board holds the lane, the date
  and the owner's words; it is listed, with what holds it up, until the lane is in integration;
- ready: the board's stages ready to take (what "Needs you" used to be);
- broken: the last checks run on integration when it is red, a run before a commit that took over 300 s, the
  decisions still stranded on a lane, and the answers he gave on the Decide page that no session has taken up for
  over two hours (one row for all of them; the caller hands them in, briefs.py answers()).

STRANDED DECISIONS. A decision is written into docs/reference/decisions.md in the turn it is made, on whatever lane that session is
on, and reaches the integration branch only when the lane lands. Until then no other lane can read it: on 2026-10-04
about fifteen unlanded lanes each held rows nobody else saw, one of them never pushed.

stranded(repo) reads decisions.md on every branch, origin's and this clone's (a local branch wins over origin's copy
of it, and a branch that was never pushed is read too), with git show: nothing is checked out and nothing is fetched.
An entry is a table row or a bullet under "## Open". It is stranded when the lane ADDED it (it is not in the file at
the lane's merge-base with integration) and integration does not have it. Entries are told apart by their date and
bold title, not their wording, so an older wording of a bullet integration has since changed is not reported. One
entry on several lanes is reported once, in the wording of the lane that touched the file last. An open bullet that
some branch has since taken out is not stranded either: it was answered there (that lane's row says how) or reworded,
and the lanes still showing it are only older.
"""
import datetime
import hashlib
import json
import re
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import src_git    # noqa: E402

DECISIONS = 'docs/reference/decisions.md'
GROUPS = ('broken', 'land', 'approved', 'ready', 'decide')     # the order the site lists them in


def slug(s):
    """A question's title however it is spelled: the rule of briefs.py slug() and decide.js."""
    return re.sub(r'[^a-z0-9]+', '-', str(s).lower()).strip('-')[:48] or 'decision'
EDIT_BUDGET = 300      # seconds a run before a commit may take: gate.ps1's $EditBudget
UNTAKEN_HOURS = 2      # an answer of his on the Decide page that no session took up in this long is broken (the agent's choice)
CI_EVERY = 600         # seconds between two questions to GitHub about the last checks run
ROW = re.compile(r'^\| *(\d{4}-\d{2}-\d{2}) *\| *(.*?) *\|? *$')
BOLD = re.compile(r'\*\*(.+?)\*\*', re.S)
DATE = re.compile(r'\d{4}-\d{2}-\d{2}')


def norm(s):
    return re.sub(r'\s+', ' ', re.sub(r'[`*]', '', s)).strip(' .:,;?').lower()


def entry(kind, section, date, body, text):
    bold = BOLD.search(body)
    title = re.sub(r'\s+', ' ', bold.group(1) if bold else body[:80]).strip()
    if kind == 'open' and not date:
        in_title = DATE.search(title)
        date = in_title.group(0) if in_title else ''
    return dict(kind=kind, section=section, date=date, title=title, text=text, key=(kind, date, norm(title)))


def parse(text):
    """The entries of a decisions.md: every dated table row, and every bullet of the Open section."""
    out, section, is_open, bullet = [], '', False, None

    def close():
        if bullet:
            out.append(entry('open', section, '', ' '.join(l.strip() for l in bullet)[2:], '\n'.join(bullet)))

    for line in text.replace('\r\n', '\n').split('\n'):
        if line.startswith('## '):
            close()
            section, is_open, bullet = line[3:].strip(), line.startswith('## Open'), None
        elif is_open:
            if line.startswith('- '):
                close()
                bullet = [line]
            elif bullet and line.startswith('  ') and line.strip():
                bullet.append(line)
            else:                       # a blank line, or a sentence between the bullets
                close()
                bullet = None
        else:
            m = ROW.match(line)
            if m:
                out.append(entry('row', section, m.group(1), m.group(2), line))
    close()
    return out


def taken_out(repo, e, path):
    for sha in src_git.git(repo, 'log', '--all', '--format=%H', '-S' + e['title'], '--', path).split():
        if e['title'] in src_git.git(repo, 'show', f'{sha}^:{path}') and e['title'] not in src_git.git(repo, 'show', f'{sha}:{path}'):
            return True
    return False


def stranded(repo, integration=None, path=DECISIONS):
    """[entry] oldest first; each also has lane (whose wording this is), lanes (every lane holding it) and ref."""
    integ = integration or src_git.INTEGRATION
    parsed = {}

    def entries(ref):
        blob = src_git.git(repo, 'rev-parse', '--verify', '--quiet', f'{ref}:{path}').strip()
        if blob and blob not in parsed:
            parsed[blob] = parse(src_git.git(repo, 'show', blob))
        return blob, parsed.get(blob, [])

    on_integ, landed = entries(integ)
    have = {e['key'] for e in landed}
    found = {}
    for name, (ref, _) in sorted(src_git.branch_refs(repo).items()):
        blob, theirs = entries(ref)
        if not blob or blob == on_integ:
            continue
        base = src_git.git(repo, 'merge-base', integ, ref).strip()
        at_base = {e['key'] for e in entries(base)[1]} if base else set()
        # whose wording: a lane before a merged play or test branch, then whoever touched the file last, then the
        # lane with the fewest commits (a lane stacked on another carries its rows and did not write them)
        rank = (name.startswith('lane/'), int(src_git.git(repo, 'log', '-1', '--format=%ct', ref, '--', path).strip() or 0),
                -int(src_git.git(repo, 'rev-list', '--count', f'{integ}..{ref}').strip() or 0))
        for e in theirs:
            if e['key'] in have or e['key'] in at_base:
                continue
            best = found.get(e['key'])
            lanes = (best['lanes'] if best else []) + [name]
            if best is None or rank > best['rank']:
                best = found[e['key']] = dict(e, lane=name, ref=ref, rank=rank)
            best['lanes'] = lanes
    rows = [e for e in found.values() if e['kind'] == 'row' or not taken_out(repo, e, path)]
    return sorted(rows, key=lambda e: (e['date'] or '9999', e['kind'], e['title']))


def headline(title):
    """The first phrase of a bullet's title: without its date, its "the agent's choices" and its colon."""
    return re.split(r'\s\(\d{4}-|\s\(|,\s|:$|\?\s', title.replace('`', '') + ' ')[0].strip(' :.') or title


def answered_here(repo, integ, path=DECISIONS):
    """The open bullets this checkout has taken out since it left integration, by key: answered here (its row says
    how), committed or not, and on integration only once the lane lands. A checkout that is only behind took none out."""
    try:
        here = {e['key'] for e in parse((Path(repo) / path).read_text(encoding='utf-8')) if e['kind'] == 'open'}
    except OSError:
        return set()
    base = src_git.git(repo, 'merge-base', integ, 'HEAD').strip()
    blob = src_git.git(repo, 'rev-parse', '--verify', '--quiet', f'{base}:{path}').strip() if base else ''
    return {e['key'] for e in parse(src_git.git(repo, 'show', blob)) if e['kind'] == 'open'} - here if blob else set()


def open_questions(repo, integ, cache, path=DECISIONS):
    """The open bullets on integration, oldest first. `answered` marks one this checkout has already taken out."""
    blob = src_git.git(repo, 'rev-parse', '--verify', '--quiet', f'{integ}:{path}').strip()
    dates = cache.setdefault('first_written', {})
    gone = answered_here(repo, integ, path)
    out = []
    for e in parse(src_git.git(repo, 'show', blob)) if blob else []:
        if e['kind'] != 'open':
            continue
        date = e['date'] or dates.get(e['title'])
        if not date:                # the day it was first written down
            log = src_git.git(repo, 'log', '--reverse', '--format=%ad', '--date=short', '-S' + e['title'], integ, '--', path).split()
            date = dates[e['title']] = log[0] if log else ''
        body = re.sub(r'\s+', ' ', re.sub(r'[`*]', '', e['text'][2:]))
        out.append(dict(title=headline(e['title']), date=date, text=body[:400], choice="agent's choice" in body[:260], answered=e['key'] in gone))
    return sorted(out, key=lambda q: (q['date'] or '9999', q['title']))


def gate_state(tree):
    """(HEAD's tree, the tree the last green full gate tested and when, the seconds of the last whole run before a commit)."""
    out = src_git.git(tree, 'rev-parse', '--path-format=absolute', '--git-path', 'tw-gate-green', 'HEAD^{tree}').split('\n')
    if len(out) < 2:
        return '', [], None
    marker = Path(out[0].strip())
    green = marker.read_text(encoding='utf-8', errors='replace').split() if marker.is_file() else []
    took = marker.with_name('tw-gate-edit-seconds')
    seconds = took.read_text(encoding='ascii', errors='replace').split() if took.is_file() else []
    return out[1].strip(), green, (int(seconds[0]), seconds[1] if len(seconds) > 1 else '') if seconds and seconds[0].isdigit() else None


def drift(repo, integ, ref):
    """(commits integration has and the ref lacks, commits the ref has and integration lacks)."""
    n = src_git.git(repo, 'rev-list', '--left-right', '--count', f'{integ}...{ref}').split()
    return (int(n[0]), int(n[1])) if len(n) == 2 else (0, 0)


def merged_into(repo, ref):
    """The lanes whose tip `ref` contains, by name without origin/."""
    out = src_git.git(repo, 'branch', '-a', '--merged', ref, '--format=%(refname:short)').split()
    return {n[7:] if n.startswith('origin/') else n for n in out}


def approvals(repo, integ, board, trees):
    out = []
    refs = src_git.branch_refs(repo)
    for f in sorted((Path(board) / 'approvals').glob('*.json')) if board else []:
        try:
            a = json.loads(f.read_text(encoding='utf-8'))
            lane, ref = a['lane'], refs.get(a['lane'], (None,))[0]
        except (OSError, ValueError, KeyError):
            continue
        if not ref or not int(src_git.git(repo, 'rev-list', '--count', '--cherry-pick', '--right-only', f'{integ}...{ref}').strip() or 0):
            continue                # landed (or gone): the approval has done its work
        behind, ahead = drift(repo, integ, ref)
        why = [f'{behind} behind'] if behind else []
        tree = next((t for t in trees if t['branch'] == lane), None)
        head, green, _ = gate_state(tree['path']) if tree else ('', [], None)
        if not tree:
            why.append('no checkout here')
        elif not green or green[0] != head:
            why.append('no green gate on its tip')
        # an unlanded lane this one contains: it lands first, or with it
        under = [n for n in merged_into(repo, ref) - merged_into(repo, integ)
                 if n.startswith('lane/') and src_git.family(n) != src_git.family(lane)]
        if under:
            why.append('sits on ' + ', '.join(sorted(under)[:2]))
        out.append(dict(lane=lane, date=a.get('date', ''), words=a.get('words', ''), ahead=ahead, why=why))
    return sorted(out, key=lambda a: a['date'])


def checks_run(repo, integ, cache, now):
    """The last checks run on integration, asked of GitHub at most every ten minutes: None when it cannot be known."""
    seen = cache.get('ci') or {}
    if now - seen.get('at', 0) < CI_EVERY:
        return seen.get('run')
    run = None
    try:
        p = subprocess.run(['gh', 'run', 'list', '--workflow', 'checks.yml', '--branch', integ.split('/', 1)[1], '--limit', '1',
                            '--json', 'conclusion,status,createdAt,url'], cwd=repo, capture_output=True, timeout=30)
        rows = json.loads(p.stdout.decode('utf-8', 'replace') or '[]') if p.returncode == 0 else []
        run = rows[0] if rows else None
    except (OSError, ValueError, subprocess.TimeoutExpired):
        pass
    cache['ci'] = dict(at=now, run=run)
    return run


def untaken(answers, now):
    """The row of Broken for what he answered on the Decide page and nobody took up, or None. Nothing wakes a session
    when he answers: an answer waits until he next talks to one, and past UNTAKEN_HOURS the board says so. One row for
    all that wait, however many: `answers` is briefs.py answers(), each with `when`, the time of his last note."""
    waits = []
    for a in answers or []:
        try:
            waits.append((now - time.mktime(time.strptime(str(a.get('when', ''))[:19], '%Y-%m-%d %H:%M:%S')), a))
        except ValueError:
            continue                            # a note with no time it can read: it cannot be said to be late
    if not waits or max(w for w, _ in waits) <= UNTAKEN_HOURS * 3600:
        return None
    longest, first = max(waits, key=lambda x: x[0])
    return dict(kind='untaken', title=f'{len(waits)} answer{"s" if len(waits) > 1 else ""} of yours nobody has taken up, the oldest {int(longest // 3600)} h ago',
                url='decide.html', date=str(first['when'])[:10], text='; '.join(a.get('title', a.get('id', '')) for _, a in sorted(waits, key=lambda x: -x[0]))[:400])


def collect(repo, ops, board=None, cache=None, now=None, integration=None, answers=None):
    """The queue, from the floor's data (src_ops.collect: its lanes carry each checkout's path, dirty count and board
    items). `cache` is a dict the caller keeps between reads (what does not change between two reads is not asked
    again). `answers` is what he answered on the Decide page that no session has taken up (briefs.py answers(); this
    function reads no folder itself). Returns the five groups and `count`, the number of entries: the site's "Needs you"."""
    integ = integration or src_git.INTEGRATION
    cache = cache if cache is not None else {}
    now = now or time.time()
    trees = [l for l in ops['lanes'] if l.get('path')]
    asked = open_questions(repo, integ, cache)
    # a question he has answered on its brief is decided (his answer is the decision): it waits on a session, not on him
    his = {slug(t): a for a in answers or [] for t in (a.get('about'), a.get('title')) if t}
    q = dict(decide=[d for d in asked if not d['answered'] and slug(d['title']) not in his], land=[], approved=approvals(repo, integ, board, trees), ready=[], broken=[])
    # answered in this checkout and not landed: no longer the owner's to decide, so not in the count; the page says where it is
    here = src_git.git(repo, 'rev-parse', '--abbrev-ref', 'HEAD').strip()
    q['answered'] = [dict(d, lane=here) for d in asked if d['answered']]
    q['decided'] = [dict(d, brief=his[slug(d['title'])].get('id', '')) for d in asked if not d['answered'] and slug(d['title']) in his]

    for t in trees:
        head, green, took = gate_state(t['path'])
        behind, ahead = drift(repo, integ, 'refs/heads/' + t['branch'])
        if green and green[0] == head and not t.get('dirty') and ahead:
            q['land'].append(dict(lane=t['branch'], checkout=t.get('checkout'), date=(green[1] if len(green) > 1 else '')[:10],
                                  ahead=ahead, behind=behind))
        if took and took[0] > EDIT_BUDGET:
            q['broken'].append(dict(kind='gate', title=f'The run before a commit took {took[0]} s', lane=t['branch'], date=took[1][:10]))
    q['land'].sort(key=lambda l: (l['behind'] > 0, l['date']))

    for l in ops['lanes']:
        for it in l.get('items', []):
            for st in it['stages']:
                if st.get('state') == 'READY':
                    q['ready'].append(dict(title=it.get('title') or it['id'], item=it['id'], stage=st['id'], lane=l['branch'],
                                           role=st.get('skill') or st.get('role') or '', date=(st.get('since') or '')[:10], since=st.get('since', '')))
    q['ready'].sort(key=lambda r: r['since'] or '~')

    run = checks_run(repo, integ, cache, now)
    if run and run.get('status') == 'completed' and run.get('conclusion') not in ('success', 'skipped', 'neutral'):
        q['broken'].append(dict(kind='ci', title='The checks run on integration is red', url=run.get('url', ''), date=(run.get('createdAt') or '')[:10]))
    q['ci'] = 'unknown' if not run else run.get('conclusion') or run.get('status') or 'unknown'

    refs = hashlib.sha1(src_git.git(repo, 'for-each-ref', '--format=%(objectname)', 'refs/heads', 'refs/remotes/origin').encode()).hexdigest()
    if (cache.get('stranded') or {}).get('refs') != refs:       # stranded() reads every branch: only when a branch moved
        by_lane = {}
        for e in stranded(repo, integ):
            by_lane.setdefault(e['lane'], []).append(e['title'])
        cache['stranded'] = dict(refs=refs, lanes=by_lane)
    for lane, titles in sorted(cache['stranded']['lanes'].items()):
        q['broken'].append(dict(kind='stranded', title=f'{len(titles)} decision{"s" if len(titles) > 1 else ""} written down only on this lane',
                                lane=lane, date='', text='; '.join(titles)[:400]))
    late = untaken(answers, now)
    if late:
        q['broken'].append(late)

    today = datetime.date.fromtimestamp(now)
    for g in GROUPS:
        for e in q[g]:
            try:
                e['days'] = (today - datetime.date.fromisoformat(e['date'])).days if e.get('date') else None
            except ValueError:
                e['days'] = None
    q['count'] = sum(len(q[g]) for g in GROUPS)
    return q


PICTURES = ('.png', '.jpg', '.jpeg', '.webp', '.gif')
MOST_SHOTS = 3


def newest_pictures(folder, most=MOST_SHOTS):
    """The newest pictures under a folder, newest first. A folder that is not there has none."""
    try:
        found = [(f.stat().st_mtime, str(f)) for f in Path(folder).rglob('*') if f.suffix.lower() in PICTURES and f.is_file()]
    except OSError:
        return []
    return [p for _, p in sorted(found, reverse=True)[:most]]


def clip(s, n=110):
    s = re.sub(r'\s+', ' ', str(s or '')).strip()
    return s if len(s) <= n else s[:n - 1].rstrip() + '…'


def details(q, repo, ops, board=None, integration=None):
    """What a click on a row of the queue opens (the owner, 2026-10-06: "once i click on it i see detailed information
    but compact and to the point with hopefully some visuals"). Every entry gets `detail`, at most five short lines that
    say what it is and what happens when he acts, and `pictures`, at most three files that show it: the evidence the
    board holds for the item, or for the items of the lane. The caller puts the pictures in the site. Returns q."""
    integ = integration or src_git.INTEGRATION
    board = Path(board) if board else None
    items = {}
    for f in sorted((board / 'items').glob('*.json')) if board and (board / 'items').is_dir() else []:
        try:
            items[f.stem] = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue

    def shown(ids):
        out = []
        for i in ids:
            out += newest_pictures(board / 'evidence' / i) if board else []
        return out[:MOST_SHOTS]

    def of_lane(lane):
        return [i for i, it in items.items() if isinstance(it, dict) and it.get('lane') == lane]

    def work(lane):
        """What a lane holds on top of integration: its newest commits in their own words, and how much it changes."""
        ref = next((r for r in ('refs/heads/' + lane, 'refs/remotes/origin/' + lane) if src_git.git(repo, 'rev-parse', '--verify', '--quiet', r).strip()), '')
        if not ref:
            return []
        subjects = [clip(s, 80) for s in src_git.git(repo, 'log', '--format=%s', '-n', '2', f'{integ}..{ref}').splitlines() if s.strip()]
        files = re.match(r'\s*(\d+) file', src_git.git(repo, 'diff', '--shortstat', f'{integ}...{ref}'))
        return ['· ' + s for s in subjects] + ([f'It changes {files.group(1)} file{"" if files.group(1) == "1" else "s"}.'] if files else [])

    for e in q.get('land', []):
        e['detail'] = [f'The full gate went green on its tip{", " + e["date"] if e.get("date") else ""}. It holds {e["ahead"]} commit{"" if e["ahead"] == 1 else "s"} the game does not have yet.'] + work(e['lane']) + (
            [f'It is {e["behind"]} commit{"" if e["behind"] == 1 else "s"} behind the integration branch: on your word it is rebased and gated again, then it lands.'] if e.get('behind') else ['On your word it lands as it is.'])
        e['pictures'] = shown(of_lane(e['lane']))
    for e in q.get('approved', []):
        e['detail'] = [f'You said land on {e.get("date", "")}: "{clip(e.get("words"), 60)}".'] + ([('It has not landed: ' + ', '.join(e['why']) + '.')] if e.get('why') else []) + work(e['lane'])
        e['pictures'] = shown(of_lane(e['lane']))
    for e in q.get('ready', []):
        it = items.get(e.get('item'), {})
        st = next((s for s in it.get('stages', []) if isinstance(s, dict) and s.get('id') == e.get('stage')), {})
        e['detail'] = [clip(it.get('title') or e.get('title'), 180) + '.',
                       f'Its step {e.get("stage")} is ready to be taken{" on the " + st["station"] if st.get("station") else ""}{" by the " + e["role"] + " role" if e.get("role") else ""}. Nobody has taken it.']
        if st.get('notes'):
            e['detail'].append('The step: ' + clip(st['notes'], 120))
        later = [s['id'] for s in it.get('stages', []) if isinstance(s, dict) and e.get('stage') in (s.get('after') or [])]
        if later:
            e['detail'].append('Waiting behind it: ' + ', '.join(later) + '.')
        e['pictures'] = shown([e['item']] if e.get('item') else [])
    for e in q.get('broken', []):
        if e.get('kind') == 'stranded':
            titles = [t.strip() for t in str(e.get('text', '')).split(';') if t.strip()]
            e['detail'] = ['These decisions of yours are written down on this lane only. The other lanes do not see them until it lands.'] + ['· ' + clip(t, 90) for t in titles[:4]]
            e['pictures'] = shown(of_lane(e.get('lane', '')))
        elif e.get('kind') == 'untaken':
            e['detail'] = ['You answered these on the Decide page and no session has taken them up. Nothing wakes a session: it happens when you next talk to one.'] + [
                '· ' + clip(t, 90) for t in str(e.get('text', '')).split(';')[:4] if t.strip()]
        elif e.get('kind') == 'ci':
            e['detail'] = ['The checks GitHub runs on the integration branch are red. The game itself is judged by the gate on the desktop, not by this run.']
        elif e.get('kind') == 'gate':
            e['detail'] = ['The run before a commit on this lane took longer than its budget. Slow checks get skipped: it wants a look.']
    for e in q.get('decide', []):
        e['detail'] = [clip(e.get('text'), 380)] if e.get('text') else []
    return q


def lines(q):
    """The queue as text, for a session (ops.py --queue)."""
    label = dict(broken='Broken', land='Say land (gate green on its tip)', approved='Approved, not landed', ready='Ready to take', decide='Decide')
    out = [f'{q["count"]} wait on the owner (checks on integration: {q["ci"]})']
    for g in GROUPS:
        if not q[g]:
            continue
        out.append(f'\n{label[g]}: {len(q[g])}')
        for e in q[g]:
            age = '' if e.get('days') is None else f'{e["days"]} d'
            if g == 'decide':
                what = e['title'] + (' (agent\'s choice)' if e['choice'] else '')
            elif g == 'land':
                what = f'{e["lane"]}  {e["ahead"]} commits' + (f', {e["behind"]} behind: rebase and gate again' if e['behind'] else '')
            elif g == 'approved':
                what = f'{e["lane"]}  "{e["words"]}"' + (f'  ({"; ".join(e["why"])})' if e['why'] else '')
            elif g == 'ready':
                what = f'{e["item"]}: {e["stage"]} on {e["lane"]}'
            else:
                what = e['title'] + (f' ({e["lane"]})' if e.get('lane') else '')
            out.append(f'  {age:>6}  {what}')
    return out


def main(argv=None):
    import argparse
    ap = argparse.ArgumentParser(description='what waits on the owner that the board does not hold')
    ap.add_argument('--stranded', action='store_true', help='the decisions on lanes that integration lacks')
    ap.add_argument('--text', action='store_true', help='print each entry in full')
    args = ap.parse_args(argv)
    if not args.stranded:
        ap.print_help()
        return 0
    rows = stranded(HERE.parents[2])
    by_lane = {}
    for e in rows:
        by_lane.setdefault(e['lane'], []).append(e)
    for lane, es in sorted(by_lane.items(), key=lambda kv: -len(kv[1])):
        print(f'{lane}: {len(es)}')
        for e in es:
            also = [l for l in e['lanes'] if l != lane]
            print(f'  {e["date"] or "undated":10}  {"open" if e["kind"] == "open" else e["section"][:24]:24}  {e["title"][:90]}'
                  + (f'  (also on {", ".join(also)})' if also else ''))
            if args.text:
                print(e['text'] + '\n')
    print(f'{len(rows)} stranded on {len(by_lane)} lanes')
    return 0


if __name__ == '__main__':
    sys.exit(main())
