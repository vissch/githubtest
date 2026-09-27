#!/usr/bin/env python3
"""Carry edits made to a file before it was split into the files it was split into.

WHY. When a big file is cut into partials (CombatFx.cs into CombatFx.*.cs on 2026-09-25), git cannot follow a
branch's edits into the moved code: the merge reports a conflict in the old file and the edits have to be redone by
hand. A split that only moves whole blocks leaves every edit's context intact somewhere, so each diff hunk can be
re-applied wherever its context now lives. This does that, hunk by hunk, and says where each one went.

USAGE (from trench-warfare-3d/, when a rebase or merge stops on a conflict in the split file)
    python Tools/port_split.py FILE --rebase      mid-rebase onto a branch that has the split (the lane rule)
    python Tools/port_split.py FILE --merge       mid-merge, whichever side has the split
    python Tools/port_split.py FILE --from BASE --to REV     explicit revisions
    add --dry-run to see where each edit would go without writing

    FILE   the file as it was before the split, e.g. Assets/_Project/Presentation/Camera/CombatFx.cs

  Rebasing your lane onto the integration branch, which split the file:
    git rebase claude/trench-warfare-2d-3d-plan-idt7lf        # stops: conflict in CombatFx.cs
    python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs --rebase
    (compile, fix any .rej by hand, git add the files, git rebase --continue; repeat at the next stop)
  --rebase keeps the upstream file (mid-rebase that is --ours) and carries REBASE_HEAD's edits. --merge finds which
  side has the split, keeps that side's file and carries the other side's edits since the merge base.

The targets are FILE and every sibling named like it (CombatFx.*.cs), plus any file given with --into. An edit is
applied where its lines (context and removed) appear exactly once across the targets. If they do not, outer context
lines are shed one at a time, but only lines that still exist somewhere in the targets (the split moved them). A
context line that exists nowhere was changed by the other side: that is a conflict, and the edit goes to
FILE.port.rej for you, as it does when its lines match nowhere or in more than one place.

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


def place(body, targets, text, everywhere):
    """Where this edit applies: full context first, then shed outer context lines one at a time (a split boundary
    can cut them off). Removed lines anchor on their own; a pure addition keeps one context line to hang from.

    A context line may only be shed if it still exists somewhere in the targets: then the split moved it. If it
    exists nowhere, the other side changed it, and git would call that a conflict; so do we (the edit goes to .rej).
    """
    changed = [i for i, (k, _) in enumerate(body) if k != ' ']
    lead, trail = changed[0], len(body) - 1 - changed[-1]
    keep = 0 if any(k == '-' for k, _ in body) else 1
    for cut in range(0, max(lead, trail) + 1):
        lo = min(cut, max(lead - keep, 0))
        hi = len(body) - min(cut, max(trail - keep, 0))
        shed = body[:lo] + body[hi:]
        if any(s.strip() and s not in everywhere for _, s in shed):
            return None
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
    mode = ap.add_mutually_exclusive_group(required=True)
    mode.add_argument('--rebase', action='store_true', help='you are mid-rebase onto a branch that has the split')
    mode.add_argument('--merge', action='store_true', help='you are mid-merge; either side may have the split')
    mode.add_argument('--from', dest='base', help='explicit: the revision the edits start from')
    ap.add_argument('--to', dest='rev', help='explicit: the revision the edits end at (with --from)')
    ap.add_argument('--into', nargs='*', default=[])
    ap.add_argument('--dry-run', action='store_true')
    a = ap.parse_args()

    old = pathlib.Path(a.file)
    here = old.parent
    if a.rebase:
        # Mid-rebase HEAD is the upstream you are rebasing onto (it has the split) and REBASE_HEAD is your commit being
        # replayed. So keep HEAD's version of the file (in a rebase that is --ours) and carry your commit's edits.
        git('checkout', '--ours', '--', old.name, cwd=here)
        a.base, a.rev = 'REBASE_HEAD~1', 'REBASE_HEAD'
    elif a.merge:
        # Whichever side has the split keeps its version of the file; the other side's edits are carried across.
        split_here = any(n.startswith(old.stem + '.') and n.endswith(old.suffix) and n != old.name
                         for n in git('ls-tree', '--name-only', 'HEAD', './', cwd=here).split())
        base = git('merge-base', 'HEAD', 'MERGE_HEAD', cwd=here).strip()
        git('checkout', '--ours' if split_here else '--theirs', '--', old.name, cwd=here)
        a.base, a.rev = base, 'MERGE_HEAD' if split_here else 'HEAD'
        print(f'the split is on {"your side (HEAD)" if split_here else "the side you are merging in"}; '
              f'carrying the edits of {a.rev} since {base[:8]}')
    elif not a.rev:
        ap.error('--from needs --to')
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
    everywhere = {s for t in targets for s in text[t]}

    rejected, total = [], 0
    for h in hs:
        parts = groups(h['lines'])
        held = len(rejected)
        for n, body in enumerate(parts, 1):
            total += 1
            label = f'{h["header"].split(" @@")[0]} @@ edit {n}'
            spot = place(body, targets, text, everywhere)
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
