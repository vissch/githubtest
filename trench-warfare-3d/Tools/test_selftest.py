#!/usr/bin/env python3
"""Unit tests for Tools/selftest.py's own helpers (review findings H4-H7).

    python Tools/test_selftest.py        from trench-warfare-3d/; exit 0 = every case behaved

Run before and after fixing selftest.py: red on the old code proves each case catches the bug, green on the new
code proves the fix. See CODE_REVIEW_2026-10-04.md H4-H7.
"""
import os
import pathlib
import stat
import subprocess
import sys
import tempfile
import time

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import selftest

results = []


def case(name, ok, detail=''):
    results.append(ok)
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + detail.strip()[:600]))


def git(cmd, cwd):
    p = subprocess.run(['git', *cmd], cwd=cwd, capture_output=True)
    return p.returncode, (p.stdout + p.stderr).decode('utf-8', 'replace')


def h4_rmtree_hard_and_sweep_stale():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = pathlib.Path(tmp)
        d = tmp / 'readonly-holder'
        d.mkdir()
        f = d / 'locked.txt'
        f.write_text('x')
        f.chmod(stat.S_IREAD)
        selftest.rmtree_hard(d)
        case('[H4] rmtree_hard removes a folder holding a read-only file', not d.exists(), str(d))

        stale = tmp / 'tw-selftest-stale'
        fresh = tmp / 'tw-selftest-fresh'
        stale.mkdir()
        fresh.mkdir()
        old = time.time() - 7 * 3600
        os.utime(stale, (old, old))
        import pathlib as pl
        real_gettempdir = tempfile.gettempdir
        tempfile.gettempdir = lambda: str(tmp)
        try:
            repo = tmp / 'repo'
            repo.mkdir()
            git(['init', '-q'], repo)
            selftest.sweep_stale(repo)
        finally:
            tempfile.gettempdir = real_gettempdir
        case('[H4] sweep_stale deletes a back-dated tw-selftest-* folder and keeps a fresh one',
             not stale.exists() and fresh.exists(), f'stale exists={stale.exists()} fresh exists={fresh.exists()}')


def h5_run_scrubs_git_env():
    with tempfile.TemporaryDirectory() as tmp:
        notrepo = pathlib.Path(tmp) / 'notrepo'
        notrepo.mkdir()
        realrepo = HERE.parent  # trench-warfare-3d's repo
        env = dict(os.environ, GIT_DIR=str(realrepo / '.git'))
        old_environ = dict(os.environ)
        os.environ.clear()
        os.environ.update(env)
        try:
            code, out = selftest.run(['git', 'rev-parse', '--git-dir'], str(notrepo))
        finally:
            os.environ.clear()
            os.environ.update(old_environ)
        case('[H5] run() scrubs an inherited GIT_DIR, so a non-repo temp folder is not fooled into the real repo',
             code != 0 and 'not a git repository' in out.lower(), f'exit {code}: {out}')


def h6_must_exits():
    with tempfile.TemporaryDirectory() as tmp:
        notrepo = pathlib.Path(tmp) / 'notrepo'
        notrepo.mkdir()
        try:
            selftest.must(['git', 'rev-parse', 'HEAD'], str(notrepo))
            case('[H6] must() raises SystemExit on a failing git call', False, 'did not raise')
        except SystemExit:
            case('[H6] must() raises SystemExit on a failing git call', True)


def h7_changed_paths_rename():
    with tempfile.TemporaryDirectory() as tmp:
        repo = pathlib.Path(tmp) / 'rename-repo'
        repo.mkdir()
        git(['init', '-q', '-b', 'main'], repo)
        old = repo / 'Old.cs'
        old.write_text('class Old {\n    int a = 1;\n    int b = 2;\n    int c = 3;\n}\n')
        git(['-c', 'user.name=t', '-c', 'user.email=t@t', 'add', '.'], repo)
        git(['-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qm', 'base'], repo)
        new = repo / 'New.cs'
        old.rename(new)
        git(['add', '-A'], repo)
        copy, delete = selftest.changed_paths(str(repo))
        case('[H7] changed_paths lists the renamed-from path as a delete and the renamed-to path as a copy',
             'Old.cs' in delete and 'New.cs' in copy, f'copy={copy} delete={delete}')


def h20_snapshot_copies_skips_big_untracked():
    # [H20] selftest's snapshot copied and committed every untracked file, so large untracked art went into the
    # shared object store on every run. A tracked change is kept; an untracked file over the cap is skipped.
    with tempfile.TemporaryDirectory() as tmp:
        repo = pathlib.Path(tmp) / 'snapshot-repo'
        repo.mkdir()
        git(['init', '-q', '-b', 'main'], repo)
        code = repo / 'Code.cs'
        code.write_text('class C {}\n')
        git(['-c', 'user.name=t', '-c', 'user.email=t@t', 'add', '.'], repo)
        git(['-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-qm', 'base'], repo)
        code.write_text('class C { int a; }\n')
        art = repo / 'big.png'
        art.write_bytes(b'x' * 2_000_000)
        copy, delete = selftest.changed_paths(str(repo))
        keep, skipped = selftest.snapshot_copies(str(repo), copy)
        case('[H20] snapshot_copies skips a big untracked file and keeps a tracked one',
             'Code.cs' in keep and 'big.png' in skipped and 'big.png' not in keep,
             f'[H20] keep={keep} skipped={skipped}')


def main():
    h4_rmtree_hard_and_sweep_stale()
    h5_run_scrubs_git_env()
    h6_must_exits()
    h7_changed_paths_rename()
    h20_snapshot_copies_skips_big_untracked()
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)


if __name__ == '__main__':
    main()
