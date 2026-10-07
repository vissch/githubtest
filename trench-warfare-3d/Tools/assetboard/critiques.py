#!/usr/bin/env python3
"""Critiques: what a critic found, kept where the ideas agent reads it.

WHY. The owner, 2026-10-08: "save the critique loops and make sure theyre findable by the idea agent so he can make
ideas based off critique as well". A critic's paper used to end where it was written: beside a stage's evidence on
the board, in a session's scratch folder, in a handoff. What a critic keeps pointing at is the best hint of what to do
next, so every loop is kept in one place and `ideas.py context` lists them.

    python Tools/assetboard/critiques.py                    the kept loops, newest first: scores and what was asked
    python Tools/assetboard/critiques.py show ID            one loop: each paper's verdict and fixes, and where its files are
    python Tools/assetboard/critiques.py save PAPER[=label] [PAPER[=label] ...] --title "..." --subject "..." \
        [--by "..."] [--role ROLE] [--picture FILE=caption ...] [--note "..."] [--day 2026-10-08]
    python Tools/assetboard/critiques.py collect            keep every critic round the relay left on the board

A LOOP is a folder <day>-<slug>/ with critique.json, the papers as they were written and the pictures the critic
judged, in TW_CRITIQUES, else the folder critiques of the Drive's TW3D-pipeline, else this station's cache. A paper
in the tw-critic shape (a VERDICT line with n/100, numbered lines under TOP-3 MANDATED FIXES) is read for its score
and its fixes; any other paper is kept whole and listed without them. Saving a loop again under the same title and
day adds the papers and pictures it does not hold yet.

`collect` reads the board's origin/main as last fetched (evidence/<item>/<stage>/critic-r<n>.md, written there by the
relay after each critic round), never this station's checkout of the board: one loop for each stage, its pictures the
stage's evidence JPGs. ideas.py runs it before it starts a run, so a relay critique is there for the next ideas run.

A critique is a hint, not a decision: nothing here enters the ideas ledger, and an idea made from a finding is held
against that ledger like any other.
"""
import argparse
import datetime
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs     # noqa: E402
import build      # noqa: E402

VERDICT = re.compile(r'\s*VERDICT\b[^\n]*?(\d{1,3})\s*/\s*100\W*(.*)')
TOP3 = re.compile(r'\s*TOP-3 MANDATED FIXES')
FIX = re.compile(r'\s*\d+[.)]\s+(\S.*)')
HEADING = re.compile(r'[A-Z][A-Z0-9 \-]*:')
BOARD_PAPER = re.compile(r'evidence/([^/]+)/([^/]+)/critic-r(\d+)\.md\Z')
PICTURES = ('.jpg', '.jpeg', '.png')
MOST_IN_CONTEXT = 30        # loops the ideas agent is shown, newest first
FIX_CHARS = 260             # of a fix line in that list; the paper holds the rest

slug, one = briefs.slug, briefs.one


def folder():
    if os.environ.get('TW_CRITIQUES'):
        return Path(os.environ['TW_CRITIQUES'])
    return build.DRIVE / 'critiques' if build.DRIVE.is_dir() else build.LOCAL / 'critiques'


def read_paper(text):
    """What a paper says: dict(score, verdict, fixes). score is None and fixes empty for a paper in another shape."""
    lines = str(text).splitlines()
    score, verdict = None, ''
    for l in lines:
        if l.strip():
            m = VERDICT.match(l)
            if m and int(m.group(1)) <= 100:
                score, verdict = int(m.group(1)), one(m.group(2))
            break
    fixes, on = [], False
    for l in lines:
        if TOP3.match(l):
            on = True
        elif on and FIX.match(l):
            fixes.append(one(FIX.match(l).group(1)))
        elif on and HEADING.match(l):
            break
    return dict(score=score, verdict=verdict, fixes=fixes)


def read_all(where: Path):
    out = []
    for f in sorted(where.glob('*/critique.json')) if where.is_dir() else []:
        try:
            c = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(c, dict) and c.get('id') and isinstance(c.get('papers'), list):
            out.append(c)
    return sorted(out, key=lambda c: (str(c.get('when', '')), c['id']), reverse=True)


def find(where: Path, cid):
    got = [c for c in read_all(where) if c['id'] == cid]
    if not got:
        raise ValueError(f'no critique {cid} in {where}')
    return got[0]


def save(where: Path, papers, title, subject, by='', role='', pictures=(), note='', day=None, cid=None, source=''):
    """Keep one loop. `papers` is [(text or Path, label)], in the order they were written; `pictures` is
    [(Path or (name, bytes), caption)]. Returns the record. A ValueError says what is missing."""
    if not one(title) or not one(subject):
        raise ValueError('a critique is kept with a title and its subject: what was judged')
    if not papers:
        raise ValueError('a critique is kept with at least one paper')
    day = day or f'{datetime.datetime.now():%Y-%m-%d}'
    cid = cid or f'{day}-{slug(title)}'
    home = where / cid
    home.mkdir(parents=True, exist_ok=True)
    try:
        c = json.loads((home / 'critique.json').read_text(encoding='utf-8'))
    except (OSError, ValueError):
        c = dict(id=cid, papers=[], pictures=[])
    c.update(title=one(title), subject=one(subject), when=day, by=one(by), role=role, note=one(note), source=source)
    for p, label in papers:
        text = p.read_text(encoding='utf-8', errors='replace') if isinstance(p, Path) else str(p)
        if any(x.get('text_len') == len(text) and x['label'] == label for x in c['papers']):
            continue
        name = f'paper-{len(c["papers"]) + 1}.md'
        (home / name).write_text(text, encoding='utf-8', newline='\n')
        c['papers'].append(dict(file=name, label=one(label) or f'paper {len(c["papers"]) + 1}', text_len=len(text), **read_paper(text)))
    for p, caption in pictures:
        name, data = (p.name, p.read_bytes()) if isinstance(p, Path) else p
        if name not in [x['file'] for x in c['pictures']]:
            c['pictures'].append(dict(file=name, caption=one(caption)))
        (home / name).write_bytes(data)
    (home / 'critique.json').write_text(json.dumps(c, indent=1, sort_keys=True) + '\n', encoding='utf-8', newline='\n')
    return c


def _git(board, *args):
    return subprocess.run(['git', '-C', str(board), *args], capture_output=True).stdout


def collect(where: Path, board, ref='origin/main'):
    """Keep every critic round the relay left on the board, one loop for each stage. Returns the ids written or added
    to. Nothing is fetched and the board's checkout is not read: only what `ref` holds."""
    if not board or not Path(board).is_dir():
        return []
    names = _git(board, 'ls-tree', '-r', '--name-only', ref, 'evidence').decode('utf-8', 'replace').split('\n')
    loops = {}
    for n in names:
        m = BOARD_PAPER.match(n.strip())
        if m:
            loops.setdefault((m.group(1), m.group(2)), []).append((int(m.group(3)), n.strip()))
    done = []
    for (item, stage), rounds in sorted(loops.items()):
        cid = f'board-{slug(item)}-{slug(stage)}'
        have = {p['label']: p.get('text_len') for c in read_all(where) if c['id'] == cid for p in c['papers']}
        texts = [(_git(board, 'show', f'{ref}:{n}').decode('utf-8', 'replace'), f'round {r}') for r, n in sorted(rounds)]
        new = [(t, label) for t, label in texts if have.get(label) != len(t)]
        if not new:
            continue
        try:
            it = json.loads(_git(board, 'show', f'{ref}:items/{item}.json').decode('utf-8', 'replace'))
        except ValueError:
            it = {}
        st = next((s for s in it.get('stages', []) if isinstance(s, dict) and s.get('id') == stage), {})
        day = _git(board, 'log', '-1', '--format=%cs', ref, '--', rounds[-1][1]).decode().strip() or None
        pics = [(n.strip(), '') for n in names if n.strip().startswith(f'evidence/{item}/{stage}/') and n.strip().count('/') == 3
                and n.strip().lower().endswith(PICTURES)]
        if have:                                # a stage judged again: the loop starts over with the newer papers
            shutil.rmtree(where / cid, ignore_errors=True)
            new = texts
        save(where, new, f'{it.get("title") or item}: the stage {stage}', f'the evidence of stage {stage} of the board item {item}',
             by='the relay\'s critic', role=st.get('role', ''), day=day, cid=cid, source=f'board {ref}:evidence/{item}/{stage}',
             pictures=[((Path(n).name, _git(board, 'show', f'{ref}:{n}')), c) for n, c in pics])
        done.append(cid)
    return done


SIGNS = {'→': '->', '≥': '>=', '≤': '<=', '≈': 'about ', '—': '-', '–': '-', '…': '...', '×': 'x',
         '·': '.', '‘': "'", '’': "'", '“': '"', '”': '"'}


def plain(text):
    """A critic's words in ASCII: a run reads this list through a pipe that may not carry an arrow or a dash, and a
    list that cannot be printed is no list. The paper in the folder keeps every sign."""
    text = str(text)
    for sign, says in SIGNS.items():
        text = text.replace(sign, says)
    return text.encode('ascii', 'replace').decode('ascii')


def for_context(loops, where: Path, most=MOST_IN_CONTEXT):
    """The kept loops as the ideas agent is shown them: the newest paper's fixes, every paper's score, where it is."""
    out = []
    for c in loops[:most]:
        last = c['papers'][-1] if c['papers'] else {}
        out.append(dict(id=c['id'], when=c.get('when', ''), title=plain(c.get('title', '')), subject=plain(c.get('subject', '')), role=c.get('role', ''),
                        scores=[p.get('score') for p in c['papers']], verdict=plain(last.get('verdict', '')), fixes=[plain(f)[:FIX_CHARS] for f in last.get('fixes', [])],
                        note=plain(c.get('note', '')), folder=str(where / c['id']), pictures=[p['file'] for p in c.get('pictures', [])]))
    return out


def context_lines(rows, kept=None):
    kept = len(rows) if kept is None else kept
    out = ['', f'# What the critics found ({kept} loops kept, the newest {len(rows)} here; a finding nobody answered is a hint of what to do next)']
    for r in rows:
        scores = ', '.join('-' if s is None else str(s) for s in r['scores'])
        out += ['', f'## {r["when"]}  {r["title"]}   ({r["id"]})', f'judged: {r["subject"]}{" [" + r["role"] + "]" if r["role"] else ""}; scores of 100: {scores}']
        out += [f'said: {r["verdict"]}'] if r['verdict'] else []
        out += [f'note: {r["note"]}'] if r['note'] else []
        out += [f'- {f}' for f in r['fixes']]
        out += [f'papers{" and pictures (" + ", ".join(r["pictures"]) + ")" if r["pictures"] else ""} in {r["folder"]}']
    return out


def lines(loops):
    out = []
    for c in loops:
        scores = ', '.join('-' if p.get('score') is None else str(p['score']) for p in c['papers'])
        out.append(f'{c.get("when", ""):10}  {scores:14}  {c.get("title", "")}   ({c["id"]})')
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='what the critics found, kept for the ideas agent')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'show', 'save', 'collect'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--title', default='')
    ap.add_argument('--subject', default='', help='what was judged')
    ap.add_argument('--by', default='', help='who ran the critic: a relay run, a session, a trial')
    ap.add_argument('--role', default='', help='the role whose work was judged, when it has one')
    ap.add_argument('--picture', action='append', default=[], help='FILE=what it shows; a picture the critic judged')
    ap.add_argument('--note', default='', help='one line a later reader needs: what came of it, what not to trust')
    ap.add_argument('--day', default='', help='the day of the loop, when it is not today')
    ap.add_argument('--board', default='', help='collect: the board\'s checkout (default: the pipeline\'s)')
    a = ap.parse_args(argv)
    where = folder()
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(errors='replace')        # a paper's own signs must not stop the listing
    try:
        if a.what == 'save':
            papers = [(Path(x.split('=', 1)[0]), (x.split('=', 1) + [''])[1]) for x in a.args]
            pictures = [(Path(x.split('=', 1)[0]), (x.split('=', 1) + [''])[1]) for x in a.picture]
            for p, _ in papers + pictures:
                if not p.is_file():
                    raise ValueError(f'{p} is not a file')
            c = save(where, papers, a.title, a.subject, a.by, a.role, pictures, a.note, a.day or None)
            print(f'critiques: {c["id"]} is kept in {where}: {len(c["papers"])} papers, {len(c["pictures"])} pictures')
        elif a.what == 'collect':
            board = a.board
            if not board:
                import ops
                board = ops.board_root()
            got = collect(where, board)
            print(f'critiques: {len(got)} loops taken from the board at {board}' + ''.join(f'\n      {g}' for g in got))
        elif a.what == 'show':
            if len(a.args) != 1:
                raise ValueError('show ID')
            c = find(where, a.args[0])
            print(f'{c["title"]}\njudged: {c["subject"]}; by {c.get("by") or "nobody named"}; {c.get("when", "")}\nin {where / c["id"]}')
            for p in c['papers']:
                print(f'\n{p["file"]} ({p["label"]}): {"no score" if p.get("score") is None else str(p["score"]) + "/100"} {p.get("verdict", "")}')
                print(''.join(f'  - {f}\n' for f in p.get('fixes', [])), end='')
        else:
            got = read_all(where)
            print(f'critiques: {len(got)} loops kept in {where}')
            print('\n'.join(lines(got)))
    except ValueError as e:
        print(f'critiques: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
