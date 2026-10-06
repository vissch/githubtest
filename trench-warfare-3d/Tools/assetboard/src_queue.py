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
- broken: the last checks run on integration when it is red, a run before a commit that took over 300 s, and the
  decisions still stranded on a lane.

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
EDIT_BUDGET = 300      # seconds a run before a commit may take: gate.ps1's $EditBudget
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


def collect(repo, ops, board=None, cache=None, now=None, integration=None):
    """The queue, from the floor's data (src_ops.collect: its lanes carry each checkout's path, dirty count and board
    items). `cache` is a dict the caller keeps between reads (what does not change between two reads is not asked
    again). Returns the five groups and `count`, the number of entries: the site's "Needs you"."""
    integ = integration or src_git.INTEGRATION
    cache = cache if cache is not None else {}
    now = now or time.time()
    trees = [l for l in ops['lanes'] if l.get('path')]
    asked = open_questions(repo, integ, cache)
    q = dict(decide=[d for d in asked if not d['answered']], land=[], approved=approvals(repo, integ, board, trees), ready=[], broken=[])
    # answered in this checkout and not landed: no longer the owner's to decide, so not in the count; the page says where it is
    here = src_git.git(repo, 'rev-parse', '--abbrev-ref', 'HEAD').strip()
    q['answered'] = [dict(d, lane=here) for d in asked if d['answered']]

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

    today = datetime.date.fromtimestamp(now)
    for g in GROUPS:
        for e in q[g]:
            try:
                e['days'] = (today - datetime.date.fromisoformat(e['date'])).days if e.get('date') else None
            except ValueError:
                e['days'] = None
    q['count'] = sum(len(q[g]) for g in GROUPS)
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
