#!/usr/bin/env python3
"""The one check of the docs and the tools: validate.py, Tools/selftest.py and every tool's own tests. No Unity.

    python Tools/toolcheck.py            from trench-warfare-3d/: run every part; exit 0 = all green, 1 = a part failed
    python Tools/toolcheck.py --list     name the parts and run nothing
    python Tools/toolcheck.py --project DIR   check that project instead (Tools/land.py passes the lane's own)

WHY. The tools that keep the repo honest were themselves checked only by hand. A lane that changed code and a tool
landed with no tool check at all (land.py looked at the gate's marker and nothing else), no Tools/**/test_*.py ever
ran at a landing, and CI was a generated Unity workflow that was red on every push. This is the same check in three
places: Tools/land.py runs it and refuses a lane on red, gate.ps1 runs it in place of validate.py when the lane
changes a tool, and CI is to run it on every push as a witness (decisions.md, 2026-10-04).

The parts are validate.py, Tools/selftest.py, then each Tools/**/test_*.py in path order, each run as a script from
the project folder: its exit code is its verdict. Tools/checks/ is left out: those files are validate.py's checks (one
is named test_modules.py), not tests. A new tool's tests need no registration: name the file test_<tool>.py.
"""
import argparse
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path

PROJECT = Path(__file__).resolve().parent.parent
NOT_TESTS = ('Tools/checks/',)
PROGRESS_S = 30      # how often a running part's output is looked at (a dot when it grew)


def parts(project: Path):
    out = [p for p in ('validate.py', 'Tools/selftest.py') if (project / p).exists()]
    tests = sorted(p.relative_to(project).as_posix() for p in (project / 'Tools').rglob('test_*.py'))
    return out + [t for t in tests if not t.startswith(NOT_TESTS)]


def run_part(part, project, env, beat=PROGRESS_S):
    """(exit code, its output, dots printed). While the part runs, a dot goes out whenever its output has grown
    since the last look, so the log of a gate grows while a long test file makes progress and stops growing when
    it hangs: the relay's gate wait stops a gate whose log is quiet too long (Tools/relay/legcmd.py)."""
    with tempfile.TemporaryFile() as buf:
        p = subprocess.Popen([sys.executable, part], cwd=project, stdout=buf, stderr=subprocess.STDOUT,
                             stdin=subprocess.DEVNULL, env=env)
        seen, dots = 0, 0
        while True:
            try:
                p.wait(timeout=beat)
                break
            except subprocess.TimeoutExpired:
                size = os.fstat(buf.fileno()).st_size
                if size > seen:
                    seen, dots = size, dots + 1
                    print('.', end='', flush=True)
        buf.seek(0)
        return p.returncode, buf.read(), dots


def main(argv=None):
    ap = argparse.ArgumentParser(description='the docs-and-tools check')
    ap.add_argument('--list', action='store_true')
    ap.add_argument('--project', default=str(PROJECT))
    a = ap.parse_args(argv)
    project = Path(a.project)
    todo = parts(project)
    if a.list:
        print('\n'.join(todo))
        return 0
    failed, t0 = [], time.time()
    # no __pycache__ left in the tree; TW_IN_TOOLCHECK tells a gate.ps1 that selftest.py starts not to start this again
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', TW_IN_TOOLCHECK='1', PYTHONUNBUFFERED='1')
    for part in todo:
        t = time.time()
        code, out, dots = run_part(part, project, env)
        ok = code == 0
        print(f'{chr(10) if dots else ""}{"ok  " if ok else "FAIL"}  {part}  {time.time() - t:.0f} s',
              flush=True)
        if not ok:
            failed.append(part)
            tail = out.decode('utf-8', 'replace').strip().split('\n')[-25:]
            print('\n'.join('      ' + l.rstrip() for l in tail), flush=True)
    if failed:
        print(f'toolcheck FAILED: {", ".join(failed)} ({len(todo)} parts, {time.time() - t0:.0f} s)')
        return 1
    print(f'toolcheck OK: {len(todo)} parts, {time.time() - t0:.0f} s')
    return 0


if __name__ == '__main__':
    sys.exit(main())
