#!/usr/bin/env python3
"""What waits on the owner that the board does not hold. First part: the decisions stranded on lanes.

    python Tools/assetboard/src_queue.py --stranded          from trench-warfare-3d/: list them
    python Tools/assetboard/src_queue.py --stranded --text   the same, each with its full line, ready to copy

WHY. A decision is written into docs/reference/decisions.md in the turn it is made, on whatever lane that session is
on, and reaches the integration branch only when the lane lands. Until then no other lane can read it: on 2026-10-04
about fifteen unlanded lanes each held rows nobody else saw, one of them never pushed.

stranded(repo) reads decisions.md on every branch, origin's and this clone's (a local branch wins over origin's copy
of it, and a branch that was never pushed is read too), with git show: nothing is checked out and nothing is fetched.
An entry is a table row or a bullet under "## Open". It is stranded when the lane ADDED it (it is not in the file at
the lane's merge-base with integration) and integration does not have it. Entries are told apart by their date and
bold title, not their wording, so an older wording of a bullet integration has since changed is not reported. One
entry on several lanes is reported once, in the wording of the lane that touched the file last.
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import src_git    # noqa: E402

DECISIONS = 'docs/reference/decisions.md'
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
    return sorted(found.values(), key=lambda e: (e['date'] or '9999', e['kind'], e['title']))


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
