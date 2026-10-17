#!/usr/bin/env python3
"""Checks that a `File.cs:NN` citation in the big skill references still points at the symbol it names.

    python Tools/skillcite.py                    from trench-warfare-3d/: check the four reference files
    python Tools/skillcite.py --files A.md B.md  check these instead
    python Tools/skillcite.py -v                 print every citation, clean ones too

WHY. The three deep references under .claude/skills/ are pinned snapshots (558c667 / c43b73f); integration is
hundreds of commits past them, so the line numbers rotted while the prose stayed true. `SimWorld.cs:424` claimed
`SimWorld.Hash` and is a roster comment; `CombatFx.cs:478` claimed `OnSimEvent` and is a SetInt blend line. Each
wrong line costs a leg a re-read of the real file, and a leg that trusts it edits the wrong place. Their own
banner says "re-grep the symbol", so the fix is to cite the symbol and drop the line number, and this is the
check that keeps it that way.

THE RULE, per backticked `<path>.cs` token (optionally `:N` or `:N-M`):
  * the path resolves the way Tools/codemap.py resolves one (path keys R\\ P\\ T\\ D\\ dropped, several bases
    tried, then a basename index over Assets/**/*.cs). Several hits: the citation is clean if any hit satisfies it.
  * the symbols are the other backticked identifiers on the same markdown line, `()` stripped, last dotted
    segment kept (`SimWorld.Hash` -> `Hash`).
  * with a line number: clean when some symbol matches \\bSymbol\\b within 5 lines of the (widened) span.
  * without one: clean when at least one symbol appears anywhere in the file.
  * no symbol named at all: a problem, so a bare line citation cannot hide.

Exit 0 and print the citation total when clean, exit 1 with one line per problem otherwise.
"""
from __future__ import annotations

import argparse
import functools
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent          # trench-warfare-3d
REPO = ROOT.parent
SKILLS = REPO / '.claude' / 'skills'

DEFAULT_FILES = [
    SKILLS / 'tw-destruction-vfx' / 'references' / 'architecture.md',
    SKILLS / 'tw-optimizer' / 'references' / 'aosa-and-perf.md',
    SKILLS / 'tw-balance-sim' / 'references' / 'balance.md',
    SKILLS / 'tw-gym' / 'references' / 'gym-design.md',
]

NEAR = 5                                               # how far from the cited line the symbol may have moved
SKIP_DIRS = {'github-test1'}
PATH_KEYS = ('R\\', 'P\\', 'T\\', 'D\\')               # the key table at the top of architecture.md

CITE = re.compile(r'`([^`\n]+\.cs(?::\d+(?:-\d+)?)?)`')
IDENT = re.compile(r'^[A-Za-z_][\w.]*(?:\(\))?$')


def read(p: Path) -> str:
    return p.read_text(encoding='utf-8-sig').replace('\r\n', '\n')


class Tree:
    """The repo as git sees it, with the bases and the basename index a citation is resolved against."""

    def __init__(self, repo: Path):
        self.repo = repo
        self.root = repo / 'trench-warfare-3d'
        self.bases = [repo, self.root, self.root / 'Assets' / '_Project']
        out = subprocess.run(['git', 'ls-files', '--cached', '--others', '--exclude-standard', '-z'], cwd=repo,
                             capture_output=True).stdout.decode('utf-8', 'replace')
        self.files = {f for f in out.split('\0') if f and f.split('/')[0] not in SKIP_DIRS and (repo / f).exists()}
        if not self.files:                             # not a git tree (a test fixture): walk it
            self.files = {p.relative_to(repo).as_posix() for p in repo.rglob('*')
                          if p.is_file() and p.parts[0] not in SKIP_DIRS}
        self.index: dict[str, list[Path]] = {}
        for rel in self.files:
            if rel.endswith('.cs'):
                self.index.setdefault(rel.rsplit('/', 1)[-1], []).append(repo / rel)

    def exists(self, p: Path) -> bool:
        try:
            return p.resolve().relative_to(self.repo.resolve()).as_posix() in self.files
        except ValueError:
            return False

    def resolve(self, token: str) -> list[Path]:
        path = token.split(':')[0]
        for key in PATH_KEYS:
            if path.startswith(key):
                path = path[len(key):]
                break
        path = path.replace('\\', '/').lstrip('/')
        hits = [b / path for b in self.bases if self.exists(b / path)]
        if not hits and '/' not in path:
            hits = list(self.index.get(path, []))
        if not hits and '/' in path:                   # relative to a folder we did not guess: match by suffix
            tail = path.split('/')
            hits = [p for p in self.index.get(tail[-1], []) if list(p.parts[-len(tail):]) == tail]
        return hits

    @functools.lru_cache(maxsize=None)
    def lines(self, p: Path) -> tuple:
        return tuple(read(p).split('\n'))


def symbols(line: str) -> list[str]:
    """Every other backticked identifier on the markdown line, `()` stripped, last dotted segment kept."""
    out = []
    for tok in re.findall(r'`([^`\n]+)`', line):
        tok = tok.strip()
        if '.cs' in tok or not IDENT.match(tok):
            continue
        name = tok[:-2] if tok.endswith('()') else tok
        name = name.rsplit('.', 1)[-1]
        if name:
            out.append(name)
    return out


def hit_near(tree: Tree, path: Path, names: list[str], lo: int, hi: int) -> bool:
    body = tree.lines(path)
    window = '\n'.join(body[max(0, lo - 1 - NEAR):min(len(body), hi + NEAR)])
    return any(re.search(rf'\b{re.escape(n)}\b', window) for n in names)


def hit_anywhere(tree: Tree, path: Path, names: list[str]) -> bool:
    body = '\n'.join(tree.lines(path))
    return any(re.search(rf'\b{re.escape(n)}\b', body) for n in names)


def check(doc: Path, tree: Tree, verbose: bool = False) -> list[str]:
    problems = []
    for ln, line in enumerate(read(doc).split('\n'), 1):
        for tok in CITE.findall(line):
            tok = tok.strip()
            where = f'{doc.name}:{ln}'
            hits = tree.resolve(tok)
            if not hits:
                problems.append(f'{where}: cites `{tok}`, which does not resolve to a file')
                continue
            names = symbols(line)
            m = re.search(r':(\d+)(?:-(\d+))?$', tok)
            if not names:
                problems.append(f'{where}: cites `{tok}` and names no symbol; cite the symbol it is about')
                continue
            if m:
                lo, hi = int(m.group(1)), int(m.group(2) or m.group(1))
                ok = any(h.is_file() and hit_near(tree, h, names, lo, hi) for h in hits)
                why = f'cites `{tok}`, {"/".join(names)} is not within {NEAR} lines'
            else:
                ok = any(h.is_file() and hit_anywhere(tree, h, names) for h in hits)
                why = f'cites `{tok}`, {"/".join(names)} is not in the file'
            if not ok:
                problems.append(f'{where}: {why}')
            elif verbose:
                print(f'ok    {where}: `{tok}` <- {"/".join(names)}')
    return problems


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description='are the skill references still pointing at their symbols?')
    ap.add_argument('--files', nargs='+', help='markdown files to check (default: the four big references)')
    ap.add_argument('-v', '--verbose', action='store_true', help='print every citation, clean ones too')
    ap.add_argument('--repo', default=str(REPO), help='repo root to resolve citations against')
    a = ap.parse_args(argv)

    docs = [Path(f).resolve() for f in a.files] if a.files else DEFAULT_FILES
    tree = Tree(Path(a.repo).resolve())
    problems, total = [], 0
    for doc in docs:
        if not doc.exists():
            problems.append(f'{doc}: no such file')
            continue
        total += sum(len(CITE.findall(l)) for l in read(doc).split('\n'))
        problems += check(doc, tree, a.verbose)
    if problems:
        print('\n'.join(problems))
        print(f'skillcite: {len(problems)} stale of {total} citations in {len(docs)} files')
        return 1
    print(f'skillcite OK: {total} citations in {len(docs)} files')
    return 0


if __name__ == '__main__':
    sys.exit(main())
