#!/usr/bin/env python3
"""Carry edits made to a file before it was split into the files it was split into.

WHY. When a big file is cut into partials (CombatFx.cs into CombatFx.*.cs on 2026-09-25), git cannot follow a
branch's edits into the moved code: the merge reports a conflict in the old file and the edits have to be redone by
hand. A split that only moves whole blocks leaves every edit's context intact somewhere, so each diff hunk can be
re-applied wherever its context now lives. This does that, hunk by hunk, and says where each one went.

USAGE (from trench-warfare-3d/, during a merge that conflicts in the split file)
    python Tools/port_split.py FILE --from BASE --to REV [--dry-run]

    FILE   the file as it was before the split, e.g. Assets/_Project/Presentation/Camera/CombatFx.cs
    BASE   the merge base: git merge-base HEAD MERGE_HEAD
    REV    the side whose edits to FILE you are carrying over

  You are on the split side, merging a branch that edited the old file:
    git merge lane/show/x                                   # conflict in CombatFx.cs
    git checkout --ours -- Assets/_Project/Presentation/Camera/CombatFx.cs
    python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs \\
        --from $(git merge-base HEAD MERGE_HEAD) --to MERGE_HEAD
  You are on the old side, merging in the split (ours and theirs swap):
    git merge claude/trench-warfare-2d-3d-plan-idt7lf        # conflict in CombatFx.cs
    git checkout --theirs -- Assets/_Project/Presentation/Camera/CombatFx.cs
    python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs \\
        --from $(git merge-base HEAD MERGE_HEAD) --to HEAD

The targets are FILE and every sibling named like it (CombatFx.*.cs), plus any file given with --into. A hunk is
applied where its lines (context and removed) appear exactly once across the targets. If they do not, the outermost
context lines are dropped one at a time (a split boundary can cut a hunk's context) down to one line of context. A
hunk that still matches nowhere, or in more than one place, is written to FILE.port.rej and left for you.

Exit 0 when every hunk applied, 1 when some are in the .rej file. Then compile (Tools/tw build or the gate): this
moves text, it does not know C#. A new member lands next to its old neighbours, which is the partial that owns them.
"""
import argparse
import pathlib
import subprocess
import sys


def git(*args, cwd):
    return subprocess.run(['git', *args], cwd=cwd, capture_output=True, check=True).stdout.decode('utf-8', 'replace')


def hunks(diff: str):
    out, cur = [], None
    for line in diff.split('\n'):
        if line.startswith('@@'):
            cur = {'header': line, 'lines': []}
            out.append(cur)
        elif cur is not None and line[:1] in (' ', '-', '+'):
            cur['lines'].append((line[0], line[1:].rstrip('\r')))
        elif cur is not None and line.startswith('\\'):
            continue
    return out


def find(seq, lines):
    n = len(seq)
    return [i for i in range(len(lines) - n + 1) if lines[i:i + n] == seq] if n else []


def groups(body, ctx=3):
    """One hunk can hold several separate edits; split it so a conflict in one does not strand the others."""
    runs, i = [], 0
    while i < len(body):
        if body[i][0] == ' ':
            i += 1
            continue
        j = i
        while j < len(body) and body[j][0] != ' ':
            j += 1
        runs.append((i, j))
        i = j
    out = []
    for a, b in runs:
        lo = a
        while lo > 0 and a - lo < ctx and body[lo - 1][0] == ' ':
            lo -= 1
        hi = b
        while hi < len(body) and hi - b < ctx and body[hi][0] == ' ':
            hi += 1
        out.append(body[lo:hi])
    return out


def place(body, targets, text):
    """Where this edit applies: full context first, then shed outer context lines one at a time (a split boundary
    can cut them off). Removed lines anchor on their own; a pure addition keeps one context line to hang from."""
    changed = [i for i, (k, _) in enumerate(body) if k != ' ']
    lead, trail = changed[0], len(body) - 1 - changed[-1]
    keep = 0 if any(k == '-' for k, _ in body) else 1
    for cut in range(0, max(lead, trail) + 1):
        lo = min(cut, max(lead - keep, 0))
        hi = len(body) - min(cut, max(trail - keep, 0))
        part = body[lo:hi]
        before = [s for k, s in part if k != '+']
        after = [s for k, s in part if k != '-']
        hits = [(t, i) for t in targets for i in find(before, text[t])]
        if len(hits) == 1:
            return hits[0], before, after
    return None


def main():
    ap = argparse.ArgumentParser(description='carry edits to a file across its split')
    ap.add_argument('file')
    ap.add_argument('--from', dest='base', required=True)
    ap.add_argument('--to', dest='rev', required=True)
    ap.add_argument('--into', nargs='*', default=[])
    ap.add_argument('--dry-run', action='store_true')
    a = ap.parse_args()

    old = pathlib.Path(a.file)
    targets = [old] if old.exists() else []
    targets += sorted(p for p in old.parent.glob(old.stem + '.*' + old.suffix) if p != old)
    targets += [pathlib.Path(p) for p in a.into]
    if not targets:
        sys.exit(f'no target files next to {old}')

    diff = git('diff', '-U3', '--no-color', a.base, a.rev, '--', old.name, cwd=old.parent)
    hs = hunks(diff)
    if not hs:
        print(f'{a.rev} does not change {old.name} since {a.base}: nothing to port')
        return

    raw = {t: t.read_bytes().decode('utf-8-sig') for t in targets}
    bom = {t: t.read_bytes().startswith(b'\xef\xbb\xbf') for t in targets}
    nl = {t: '\r\n' if '\r\n' in raw[t] else '\n' for t in targets}
    text = {t: raw[t].replace('\r\n', '\n').split('\n') for t in targets}

    rejected, total = [], 0
    for h in hs:
        parts = groups(h['lines'])
        held = len(rejected)
        for n, body in enumerate(parts, 1):
            total += 1
            label = f'{h["header"].split(" @@")[0]} @@ edit {n}'
            spot = place(body, targets, text)
            added = sum(1 for k, _ in body if k == '+')
            removed = sum(1 for k, _ in body if k == '-')
            if not spot:
                rejected.append({'header': label, 'lines': body})
                print(f'NOT APPLIED {label}  (+{added} -{removed}): its lines are not in any target exactly once')
                continue
            (t, i), before, after = spot
            text[t][i:i + len(before)] = after
            print(f'applied     {label}  ->  {t.name}:{i + 1}  (+{added} -{removed})')
        if 0 < len(rejected) - held < len(parts):
            print(f'  CHECK: {h["header"].split(" @@")[0]} @@ was applied only in part; its applied edits may use '
                  f'something the rejected ones declare. Apply the .rej edits before judging a compile error.')

    if not a.dry_run:
        for t in targets:
            new = nl[t].join(text[t])
            if new != raw[t]:
                t.write_bytes(((b'\xef\xbb\xbf' if bom[t] else b'') + new.encode('utf-8')))
        if rejected:
            rej = old.with_name(old.name + '.port.rej')
            rej.write_text('\n'.join(h['header'] + '\n' + '\n'.join(k + s for k, s in h['lines']) for h in rejected) + '\n',
                           encoding='utf-8')
            print(f'{len(rejected)} of {total} edits left in {rej}: apply them by hand, then delete it')
    print(f'{total - len(rejected)} of {total} edits applied' + (' (dry run, nothing written)' if a.dry_run else ''))
    sys.exit(1 if rejected else 0)


if __name__ == '__main__':
    main()
