#!/usr/bin/env python3
"""Tests of Tools/skillcite.py, on fixtures only.

Self-contained by rule: Tools/toolcheck.py runs every Tools/**/test_*.py, so a test that read the real skill docs
would be red for as long as those docs still hold a stale line, and would make the gate red for the commit that
adds this tool. The fixtures write their own .cs file, so the expected line numbers come from the test itself.
"""
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import skillcite  # noqa: E402

STUB_LINE = 10                      # the line `void Hash()` is written on
NEAR_OK = skillcite.NEAR            # a line still inside the tolerance window


def fixture(tmp: Path, doc_body: str) -> tuple:
    """A tiny repo: Stub.cs with `void Hash()` at STUB_LINE, and a doc holding doc_body."""
    src = tmp / 'trench-warfare-3d' / 'Assets' / '_Project' / 'Sim'
    src.mkdir(parents=True)
    lines = ['// filler'] * (STUB_LINE - 1) + ['    void Hash() { }', '    void Other() { }']
    (src / 'Stub.cs').write_text('\n'.join(lines), encoding='utf-8')
    doc = tmp / 'doc.md'
    doc.write_text(doc_body, encoding='utf-8')
    return doc, skillcite.Tree(tmp)


def case(name: str, body: str, expected: int) -> bool:
    with tempfile.TemporaryDirectory() as d:
        doc, tree = fixture(Path(d), body)
        problems = skillcite.check(doc, tree)
    ok = len(problems) == expected
    print(f'{"ok  " if ok else "FAIL"}  {name}: {len(problems)} problems, expected {expected}')
    if not ok:
        print('\n'.join('      ' + p for p in problems))
    return ok


def main() -> int:
    results = [
        case('right line', f'`World.Hash` lives at `Stub.cs:{STUB_LINE}`.', 0),
        case('stale line', '`World.Hash` lives at `Stub.cs:40`.', 1),
        case('near enough', f'`World.Hash` lives at `Stub.cs:{STUB_LINE + NEAR_OK}`.', 0),
        case('symbol, no line', '`Hash` lives in `Stub.cs`.', 0),
        case('symbol not in file', '`Nope` lives in `Stub.cs`.', 1),
        case('no symbol at all', 'Look at `Stub.cs` some time.', 1),
        case('unknown file', '`Hash` lives in `Ghost.cs`.', 1),
    ]
    print(f'test_skillcite: {sum(results)}/{len(results)} passed')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
